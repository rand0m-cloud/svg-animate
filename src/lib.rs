mod analysis;
mod ast;
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

use std::path::PathBuf;

use analysis::{Animation, AnimationConfig};
use clap::Parser;
use svg::{node::element::Element, Node};

#[cfg(target_arch = "wasm32")]
mod wasi;

#[cfg(target_arch = "wasm32")]
pub use wasi::app_main;

#[cfg(not(target_arch = "wasm32"))]
mod desktop;

#[cfg(not(target_arch = "wasm32"))]
pub use desktop::app_main;

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
