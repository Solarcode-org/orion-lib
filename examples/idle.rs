use orion_lib::IdleRunner;

fn main() {
    let mut idle = IdleRunner::new(false).unwrap();

    idle.run_line("$say(\"hi\")").unwrap();
}
