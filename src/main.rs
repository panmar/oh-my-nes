use std::{env, path::Path, process::exit};

use emulator::Emulator;

pub mod emulator;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!(
            "Wrong number of arguments: expected {}, given {}",
            1,
            args.len() - 1
        );
        exit(1);
    }

    let rom_path = Path::new(&args[1]);
    let mut emulator = Emulator::new();
    emulator.run(rom_path);
}
