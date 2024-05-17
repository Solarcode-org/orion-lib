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
RUN cargo test --release --no-fail-fast