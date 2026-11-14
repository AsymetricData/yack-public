use clap::Parser;

/// A CLI: `app --name Ada` prints a greeting.
#[derive(Parser)]
#[command(name = "app")]
struct Args {
    /// who to greet
    #[arg(long, default_value = "Yack")]
    name: String,
}

fn main() {
    let args = Args::parse();
    println!("Hello from {} !", args.name);
}
