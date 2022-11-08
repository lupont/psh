#[derive(clap::Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    #[arg(short, long, help("Run the specified command and exit"))]
    pub command: Option<String>,

    #[arg(long, requires("command"), help("Lex the command specified by --command, and exit"))]
    pub lex: bool,

    #[arg(long, requires("command"), help("Print the AST of the command specified by --command, and exit"))]
    pub ast: bool,
}
