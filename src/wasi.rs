use crate::{analysis::Animation, ast, parse::Parse, render_frame, Cli};

pub fn app_main(args: Cli) {
    let animation_source = std::fs::read_to_string(&args.input).unwrap();
    let anim = Animation::new(
        ast::Animation::parse_from_str(&animation_source).expect("animation didn't parse :("),
    );
    let mut time = 0.0;

    while time < anim.duration.as_secs_f32() {
        let frame = render_frame(time, &anim, &args);
        println!("{frame}");

        time += 1.0 / args.framerate as f32;
    }
}
