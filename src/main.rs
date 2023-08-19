use std::{
    io::Write,
    process::{Command, Stdio},
};

use lex::lex;
use parse::*;
use resvg::{
    tiny_skia::Pixmap,
    usvg::{self, Transform, TreeParsing},
    Tree,
};
use svg::{node::element::Element, Node};

use crate::analysis::{Animation, AnimationContext};

mod analysis;
mod ast;
mod lex;
mod parse;
mod tokens;

fn main() {
    let animation_source = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();
    let lexed = lex(&animation_source);
    let anim = Animation::new(ast::Animation::parse_from_str(&animation_source).unwrap());

    let mut time = 0.0;
    let mut index = 0;

    let mut ffmpeg = Command::new("ffmpeg")
        .args([
            "-framerate",
            "60",
            "-y",
            "-f",
            "rawvideo",
            "-pixel_format",
            "rgba",
            "-video_size",
            "320x320",
            "-i",
            "-",
            "out.mp4",
        ])
        .stdin(Stdio::piped())
        .spawn()
        .expect("ffmpeg command to work");

    let mut stdin = ffmpeg.stdin.take().unwrap();
    while time < anim.duration {
        let frame = render_frame(time, &anim);
        time += 1.0 / 60.0;

        let mut image = Pixmap::new(320, 320).unwrap();
        let usvg = usvg::Tree::from_str(&frame.to_string(), &usvg::Options::default()).unwrap();
        let svg = Tree::from_usvg(&usvg);
        svg.render(Transform::default(), &mut image.as_mut());

        stdin.write_all(&image.data()).unwrap();

        index += 1;
    }

    drop(stdin);
    ffmpeg.wait().unwrap();
}

fn render_frame(current_time: f32, animation: &Animation) -> Element {
    let ctx = AnimationContext::render(current_time, animation.clone());
    let mut svg = Element::new("svg");
    svg.assign("width", "320");
    svg.assign("height", "320");
    svg.assign("xmlns", "http://www.w3.org/2000/svg");
    svg.assign("render_time", current_time);
    svg.append(ctx.root);
    svg
}
