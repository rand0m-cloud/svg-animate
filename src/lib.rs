use std::{
    io::Write,
    path::PathBuf,
    process::{Command, Stdio},
};

use analysis::{Animation, AnimationConfig};
use clap::Parser;
use parse::Parse;
use resvg::{
    tiny_skia::Pixmap,
    usvg::{self, Transform, TreeParsing},
    Tree,
};
use svg::{node::element::Element, Node};

mod analysis;
mod ast;
mod lex;
mod parse;
mod tokens;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
pub struct Cli {
    /// Path to animation script
    pub input: PathBuf,

    /// Path to output
    pub output: PathBuf,

    /// Width of the output
    #[arg(long, default_value = "320")]
    pub width: u32,

    /// Height of the output
    #[arg(long, default_value = "320")]
    pub height: u32,

    /// Framerate of the output
    #[arg(long, default_value = "60")]
    pub framerate: u32,

    #[arg(short)]
    pub quiet: bool,
}

pub fn app_main(args: Cli) {
    let animation_source = std::fs::read_to_string(&args.input).unwrap();
    let anim = Animation::new(
        ast::Animation::parse_from_str(&animation_source).expect("animation didn't parse :("),
    );

    let framerate_str = args.framerate.to_string();
    let size_str = format!("{}x{}", args.width, args.height);
    let output_str = args.output.display().to_string();

    let mut ffmpeg_args = vec![
        "-framerate",
        &framerate_str,
        "-y",
        "-f",
        "rawvideo",
        "-pixel_format",
        "rgba",
        "-video_size",
        &size_str,
        "-i",
        "-",
        "-vf",
        "format=yuv420p",
        &output_str,
    ];

    if args.quiet {
        ffmpeg_args.extend(["-loglevel", "panic"]);
    }

    let mut ffmpeg = Command::new("ffmpeg")
        .args(ffmpeg_args)
        .stdin(Stdio::piped())
        .spawn()
        .expect("ffmpeg command to work");

    let mut stdin = ffmpeg.stdin.take().unwrap();

    let mut frame_index = 0;
    let mut time = 0.0;

    while time < anim.duration.as_secs_f32() {
        let frame = render_frame(time, &anim, &args);
        time += 1.0 / args.framerate as f32;

        let mut image = Pixmap::new(args.width, args.height).unwrap();
        let usvg = usvg::Tree::from_str(&frame.to_string(), &usvg::Options::default()).unwrap();
        let svg = Tree::from_usvg(&usvg);
        svg.render(Transform::default(), &mut image.as_mut());

        frame_index += 1;
        stdin.write_all(image.data()).unwrap();
    }

    if frame_index == 0 {
        render_frame(0.0, &anim, &args);
    }

    drop(stdin);
    ffmpeg.wait().unwrap();
}

pub fn render_frame(current_time: f32, animation: &Animation, args: &Cli) -> Element {
    let frame = animation.render(
        current_time,
        AnimationConfig {
            width: args.width,
            height: args.height,
        },
    );
    let mut svg = Element::new("svg");
    svg.assign("width", args.width);
    svg.assign("height", args.height);
    svg.assign("xmlns", "http://www.w3.org/2000/svg");
    svg.assign("render_time", current_time);
    svg.append(frame);
    svg
}
