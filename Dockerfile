FROM lukemathwalker/cargo-chef:latest-rust-1 AS chef
WORKDIR /usr/src/orion-lib

FROM chef AS planner
COPY . .
RUN cargo chef prepare --recipe-path recipe.json

FROM chef AS builder 
COPY --from=planner /usr/src/orion-lib/recipe.json recipe.json
RUN cargo chef cook --release --recipe-path recipe.json

COPY . .
RUN cargo build --release
RUN cargo test --release --no-fail-fast