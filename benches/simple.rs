#![allow(unused)]

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use orion_lib::run_contents;

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("simple", |b| {
        b.iter(|| {
            run_contents(
                r#"
        say("Hello!");

        say("Hi,", join("somename", "!"));
        
        if true {
            say("Possible");
        } else {
            say("Impossible");
        };
        "#,
            )
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
