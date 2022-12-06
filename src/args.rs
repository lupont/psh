#[derive(clap::Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    #[arg(short, long, help("Run the specified command and exit"))]
    pub command: Option<String>,

    #[arg(
        long,
        requires("command"),
        help("Only lex the command specified by --command")
    )]
    pub lex: bool,

    #[arg(long, requires("lex"), help("Include whitespace when lexing"))]
    pub include_space: bool,

    #[arg(
        long,
        requires("command"),
        help("Only produce the AST of the command specified by --command")
    )]
    pub ast: bool,
}
