use orion_lib::run_contents;
use std::fs::read_to_string;

fn main() {
    run_contents(read_to_string("./examples/hello.or").unwrap());
}
