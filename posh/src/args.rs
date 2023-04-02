#[derive(clap::Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    #[arg(short, long, help("Run the specified command and exit"))]
    pub command: Option<String>,

    #[arg(long, help("Only tokenize the command specified by --command"))]
    pub tokenize: bool,

    #[arg(long, help("Only lex the command specified by --command"))]
    pub lex: bool,

    #[arg(
        long,
        help("Only produce the AST of the command specified by --command")
    )]
    pub ast: bool,

    #[arg(help("Run the given file"))]
    pub file: Option<String>,
}
