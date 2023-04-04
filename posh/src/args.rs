#[derive(clap::Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    #[arg(
        short,
        long,
        help("Run the specified command and exit"),
        value_name("cmd")
    )]
    pub command: Option<String>,

    #[arg(long, help("Only tokenize the input"))]
    pub tokenize: bool,

    #[arg(long, help("Only lex the input"))]
    pub lex: bool,

    #[arg(long, help("Only produce the AST of the input"))]
    pub ast: bool,

    #[arg(help("Run the given file"), value_name("file"))]
    pub file: Option<String>,
}
