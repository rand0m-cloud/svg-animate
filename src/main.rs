use clap::Parser;
use svg_animate::{app_main, Cli};

fn main() {
    let args = Cli::parse();
    app_main(args);
}
