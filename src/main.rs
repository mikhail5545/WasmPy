use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Path to the input source file
    #[arg(short, long)]
    input: String,
    /// Target compilation strategy: type-checked static compilation to direct WASM bytecode, or interpreted execution in the VM environment. Allowed values: "wasm", "vm".
    #[arg(short, long, default_value = "wasm")]
    target: String,
}

fn main() {
    let args = Args::parse();

}
