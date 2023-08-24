use std::{
    io::Write,
    path::PathBuf,
    process::{Command, Stdio},
};

use clap::*;
use parse::*;
use resvg::{
    tiny_skia::Pixmap,
    usvg::{self, Transform, TreeParsing},
    Tree,
};
use svg::{node::element::Element, Node};

use crate::analysis::Animation;

mod analysis;
mod ast;
mod lex;
mod parse;
mod tokens;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Path to animation script
    input: PathBuf,

    /// Path to output
    output: PathBuf,

    /// Width of the output
    #[arg(long, default_value = "320")]
    width: u32,

    /// Height of the output
    #[arg(long, default_value = "320")]
    height: u32,

    /// Framerate of the output
    #[arg(long, default_value = "60")]
    framerate: u32,
}

fn main() {
    let args = Cli::parse();

    let animation_source = std::fs::read_to_string(&args.input).unwrap();
    let anim = Animation::new(ast::Animation::parse_from_str(&animation_source).unwrap());

    let mut time = 0.0;

    let mut ffmpeg = Command::new("ffmpeg")
        .args([
            "-framerate",
            &args.framerate.to_string(),
            "-y",
            "-f",
            "rawvideo",
            "-pixel_format",
            "rgba",
            "-video_size",
            &format!("{}x{}", args.width, args.height),
            "-i",
            "-",
            &format!("{}", args.output.display()),
        ])
        .stdin(Stdio::piped())
        .spawn()
        .expect("ffmpeg command to work");

    let mut stdin = ffmpeg.stdin.take().unwrap();
    while time < anim.duration.as_secs_f32() {
        let frame = render_frame(time, &anim, &args);
        time += 1.0 / args.framerate as f32;

        let mut image = Pixmap::new(args.width, args.height).unwrap();
        let usvg = usvg::Tree::from_str(&frame.to_string(), &usvg::Options::default()).unwrap();
        let svg = Tree::from_usvg(&usvg);
        svg.render(Transform::default(), &mut image.as_mut());

        stdin.write_all(image.data()).unwrap();
    }

    drop(stdin);
    ffmpeg.wait().unwrap();
}

fn render_frame(current_time: f32, animation: &Animation, args: &Cli) -> Element {
    let frame = animation.render(current_time);
    let mut svg = Element::new("svg");
    svg.assign("width", args.width);
    svg.assign("height", args.height);
    svg.assign("xmlns", "http://www.w3.org/2000/svg");
    svg.assign("render_time", current_time);
    svg.append(frame);
    svg
}
