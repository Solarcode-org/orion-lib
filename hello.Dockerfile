FROM lukemathwalker/cargo-chef:latest-rust-1 AS chef
WORKDIR /usr/src/orion-lib

FROM chef AS planner
COPY . .
RUN cargo chef prepare --recipe-path recipe.json

FROM chef AS builder 
COPY --from=planner /usr/src/orion-lib/recipe.json recipe.json
RUN cargo chef cook --release --recipe-path recipe.json

COPY . .
RUN echo 'use color_eyre::{eyre::{eyre,WrapErr},install,Result};\
    fn main()->Result<()>{install()?;\
    lalrpop::process_root().map_err(|e|eyre!(e.to_string())).with_context(||"Could not process all the files in the current directory.")?;\
    Ok(())}' > build.rs
RUN cargo build --release
RUN cargo build --release --example hello_world

FROM debian:bookworm-slim AS runtime
WORKDIR /usr/src/orion-lib

COPY --from=builder /usr/src/orion-lib/target/release/examples/hello_world /usr/local/bin
RUN mkdir examples
COPY --from=builder /usr/src/orion-lib/examples/hello.or ./examples

ENTRYPOINT ["/usr/local/bin/hello_world"]