#[derive(clap::Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    #[arg(short, long, help("Run the specified command and exit"))]
    pub command: bool,

    #[arg(long, help("Only tokenize the input"))]
    pub tokenize: bool,

    #[arg(long, help("Only lex the input"))]
    pub lex: bool,

    #[arg(long, help("Only produce the AST of the input"))]
    pub ast: bool,

    #[cfg(feature = "serde")]
    #[arg(long, requires("ast"), help("Prints the AST in JSON format"))]
    pub json: bool,

    #[arg(help("The file or command (if `-c`) to run"), value_name("target"))]
    pub target: Option<String>,
}
