mod ast;
mod ir;
mod lex;
mod parse;
mod tokens;

#[derive(Parser, Default)]
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

    /// Makes ffmpeg quiet down
    #[arg(short)]
    pub quiet: bool,

    /// Prints out rendered svgs
    #[arg(long)]
    pub debug_svg: bool,

    /// Renders a specific frame at given time
    #[arg(long)]
    pub render_frame: Option<f32>,
}

use std::{
    io::Write,
    path::PathBuf,
    process::{Child, ChildStdin, Command, Stdio},
};

use ast::{convert_ast, Animation};
use clap::Parser;
use ir::IR;
use parse::Parse;
use resvg::tiny_skia::PixmapRef;

use crate::ir::IRContext;

pub fn app_main(args: Cli) {
    let anim_source = std::fs::read_to_string(&args.input).unwrap();
    let anim = Animation::parse_from_str(&anim_source).unwrap();
    let ir_source = convert_ast(&anim);
    println!("generated ir:\n {ir_source}");
    let ir = IR::parse_from_str(&ir_source).expect("IR source to parse");

    let mut ffmpeg = FfmpegHandle::new(&args);

    let mut ctx = IRContext::new(
        ir,
        args.framerate as f32,
        (args.width, args.height),
        move |buf| {
            ffmpeg.push_image(buf);
        },
    );
    ctx.vars.enter_scope();
    ctx.evaluate("main", &[]);
}

pub struct FfmpegHandle(Child, Option<ChildStdin>);

impl FfmpegHandle {
    pub fn new(args: &Cli) -> Self {
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

        let stdin = ffmpeg.stdin.take().unwrap();
        Self(ffmpeg, Some(stdin))
    }

    pub fn push_image(&mut self, buf: PixmapRef) {
        self.1.as_mut().unwrap().write_all(buf.data()).unwrap();
    }
}

impl Drop for FfmpegHandle {
    fn drop(&mut self) {
        drop(self.1.take().unwrap());
        self.0.wait().unwrap();
    }
}
