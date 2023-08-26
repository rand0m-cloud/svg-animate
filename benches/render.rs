use criterion::{criterion_group, criterion_main, Criterion};
use svg_animate::*;

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("render 4k60 mp4", |b| {
        b.iter_batched(
            || {},
            |_| {
                app_main(Cli {
                    input: "benches/bench.anim".into(),
                    output: "benches/out/bench_4k60.mp4".into(),
                    width: 3840,
                    height: 2160,
                    framerate: 60,
                    quiet: true,
                })
            },
            criterion::BatchSize::PerIteration,
        )
    });
    c.bench_function("render 320x320, 30fps mp4", |b| {
        b.iter_batched(
            || {},
            |_| {
                app_main(Cli {
                    input: "benches/bench.anim".into(),
                    output: "benches/out/bench_320_30.mp4".into(),
                    width: 320,
                    height: 320,
                    framerate: 30,
                    quiet: true,
                })
            },
            criterion::BatchSize::PerIteration,
        )
    });
}

criterion_group!(name = benches; config = Criterion::default().sample_size(10); targets = criterion_benchmark);
criterion_main!(benches);
