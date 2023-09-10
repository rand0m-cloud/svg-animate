use std::{
    io::Write,
    process::{Command, Stdio},
};

use crate::{analysis::Animation, ast, parse::Parse, render_frame, Cli};
use resvg::{
    tiny_skia::{Color, Pixmap},
    usvg::{self, Transform, TreeParsing},
    Tree,
};

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

    let mut image = Pixmap::new(args.width, args.height).unwrap();

    while time < anim.duration.as_secs_f32() {
        let frame = render_frame(time, &anim, &args);

        if args.debug_svg {
            println!("{frame}");
        }

        time += 1.0 / args.framerate as f32;

        let usvg = usvg::Tree::from_str(&frame.to_string(), &usvg::Options::default()).unwrap();
        let svg = Tree::from_usvg(&usvg);
        svg.render(Transform::default(), &mut image.as_mut());

        frame_index += 1;
        stdin.write_all(image.data()).unwrap();

        image.fill(Color::TRANSPARENT);
    }

    if frame_index == 0 {
        render_frame(0.0, &anim, &args);
    }

    drop(stdin);
    ffmpeg.wait().unwrap();
}
