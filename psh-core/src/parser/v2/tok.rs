pub enum Token {
    Word,
    AssignmentWord,
    Name,
    Newline,
    IoNumber,
    Operator(Operator),
    Reserved(ReservedWord),
}

/// ```[no_run]
/// %token  AND_IF    OR_IF    DSEMI
/// /*      '&&'      '||'     ';;'    */
///
/// %token  DLESS  DGREAT  LESSAND  GREATAND  LESSGREAT  DLESSDASH
/// /*      '<<'   '>>'    '<&'     '>&'      '<>'       '<<-'   */
///
/// %token  CLOBBER
/// /*      '>|'   */
/// ```
pub enum Operator {
    /// ```[no_run]
    /// &&
    /// ```
    AndIf,

    /// ```[no_run]
    /// ||
    /// ```
    OrIf,

    /// ```[no_run]
    /// ;;
    /// ```
    DSemi,

    /// ```[no_run]
    /// <<
    /// ```
    DLess,

    /// ```[no_run]
    /// >>
    /// ```
    DGreat,

    /// ```[no_run]
    /// <&
    /// ```
    LessAnd,

    /// ```[no_run]
    /// >&
    /// ```
    GreatAnd,

    /// ```[no_run]
    /// <>
    /// ```
    LessGreat,

    /// ```[no_run]
    /// <<-
    /// ```
    DLessDash,

    /// ```[no_run]
    /// >|
    /// ```
    Clobber,
}

impl Operator {
    fn can_apply(operator: &str, c: char) -> bool {
        match operator {
            "" => "&|;<>".contains(c),
            "&" => c == '&',
            "|" => c == '|',
            ";" => c == ';',
            "<" => "<>&".contains(c),
            ">" => ">&|".contains(c),
            "<<" => c == '-',
            _ => false,
        }
    }
}

impl<'a> TryFrom<&'a str> for Operator {
    type Error = &'a str;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        match value {
            "&&" => Ok(Self::AndIf),
            "||" => Ok(Self::OrIf),
            ";;" => Ok(Self::DSemi),
            "<<" => Ok(Self::DLess),
            ">>" => Ok(Self::DGreat),
            "<&" => Ok(Self::LessAnd),
            ">&" => Ok(Self::GreatAnd),
            "<>" => Ok(Self::LessGreat),
            ">>-" => Ok(Self::DLessDash),
            ">|" => Ok(Self::Clobber),
            s => Err(s),
        }
    }
}

pub enum ReservedWord {
    /// ```[no_run]
    /// if
    /// ```
    If,

    /// ```[no_run]
    /// then
    /// ```
    Then,

    /// ```[no_run]
    /// else
    /// ```
    Else,

    /// ```[no_run]
    /// elif
    /// ```
    Elif,

    /// ```[no_run]
    /// fi
    /// ```
    Fi,

    /// ```[no_run]
    /// do
    /// ```
    Do,

    /// ```[no_run]
    /// done
    /// ```
    Done,

    /// ```[no_run]
    /// case
    /// ```
    Case,

    /// ```[no_run]
    /// esac
    /// ```
    Esac,

    /// ```[no_run]
    /// while
    /// ```
    While,

    /// ```[no_run]
    /// until
    /// ```
    Until,

    /// ```[no_run]
    /// for
    /// ```
    For,

    /// ```[no_run]
    /// {
    /// ```
    LBrace,

    /// ```[no_run]
    /// }
    /// ```
    RBrace,

    /// ```[no_run]
    /// !
    /// ```
    Bang,

    /// ```[no_run]
    /// in
    /// ```
    In,
}

impl<'a> TryFrom<&'a str> for ReservedWord {
    type Error = &'a str;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        match value {
            "if" => Ok(Self::If),
            "then" => Ok(Self::Then),
            "else" => Ok(Self::Else),
            "elif" => Ok(Self::Elif),
            "fi" => Ok(Self::Fi),
            "do" => Ok(Self::Do),
            "done" => Ok(Self::Done),
            "case" => Ok(Self::Case),
            "esac" => Ok(Self::Esac),
            "while" => Ok(Self::While),
            "until" => Ok(Self::Until),
            "for" => Ok(Self::For),
            "{" => Ok(Self::LBrace),
            "}" => Ok(Self::RBrace),
            "!" => Ok(Self::Bang),
            "in" => Ok(Self::In),

            s => Err(s),
        }
    }
}

#[allow(unused_mut, unused_variables)]
pub fn tokenize(input: String) -> Vec<Token> {
    let mut tokens = Vec::new();

    let mut chars = input.chars().peekable();

    let mut in_iohere = false;

    // When it is not processing an io_here, the shell shall break its input into tokens by applying
    // the first applicable rule below to the next character in its input. The token shall be from the
    // current position in the input until a token is delimited according to one of the rules below;
    // the characters forming the token are exactly those in the input, including any quoting
    // characters. If it is indicated that a token is delimited, and no characters have been included
    // in a token, processing shall continue until an actual token is delimited.

    let mut current_token = String::new();

    let mut in_operator_state = false;
    let mut is_quoted = false;

    // 1.  If the end of input is recognized, the current token (if any) shall be delimited.
    // Implicitly applied when `chars.next()` returns `None`
    for c in chars {
        if let Ok(operator) = Operator::try_from(current_token.as_str()) {
            if in_operator_state && !is_quoted {
                if Operator::can_apply(&current_token, c) {
                    // 2.  If the previous character was used as part of an operator and the current
                    //     character is not quoted and can be used with the previous characters to form
                    //     an operator, it shall be used as part of that (operator) token.
                    current_token.push(c);
                } else {
                    // 3.  If the previous character was used as part of an operator and the current
                    //     character cannot be used with the previous characters to form an operator,
                    //     the operator containing the previous character shall be delimited.
                    tokens.push(Token::Operator(operator));
                    current_token.truncate(0);
                }
            }
        }

        // 4.  If the current character is <backslash>, single-quote, or double-quote and it is not quoted,
        //     it shall affect quoting for subsequent characters up to the end of the quoted text. The
        //     rules for quoting are as described in Quoting . During token recognition no substitutions
        //     shall be actually performed, and the result token shall contain exactly the characters that
        //     appear in the input (except for <newline> joining), unmodified, including any embedded or
        //     enclosing quotes or substitution operators, between the <quotation-mark> and the end of the
        //     quoted text. The token shall not be delimited by the end of the quoted field.
        if (c == '\\' || c == '\'' || c == '"') && !is_quoted {
            is_quoted = true;
        }

        // 5.  If the current character is an unquoted '$' or '`', the shell shall identify the start of
        //     any candidates for parameter expansion (Parameter Expansion), command substitution (Command
        //     Substitution), or arithmetic expansion (Arithmetic Expansion) from their introductory
        //     unquoted character sequences: '$' or "${", "$(" or '`', and "$((", respectively. The shell
        //     shall read sufficient input to determine the end of the unit to be expanded (as explained in
        //     the cited sections). While processing the characters, if instances of expansions or quoting
        //     are found nested within the substitution, the shell shall recursively process them in the
        //     manner specified for the construct that is found. The characters found from the beginning of
        //     the substitution to its end, allowing for any recursion necessary to recognize embedded
        //     constructs, shall be included unmodified in the result token, including any embedded or
        //     enclosing substitution operators or quotes. The token shall not be delimited by the end of
        //     the substitution.

        // 6.  If the current character is not quoted and can be used as the first character of a new
        //     operator, the current token (if any) shall be delimited. The current character shall be used
        //     as the beginning of the next (operator) token.

        // 7.  If the current character is an unquoted <blank>, any token containing the previous character
        //     is delimited and the current character shall be discarded.

        // 8.  If the previous character was part of a word, the current character shall be appended to
        //     that word.

        // 9.  If the current character is a '#', it and all subsequent characters up to, but excluding,
        //     the next <newline> shall be discarded as a comment. The <newline> that ends the line is not
        //     considered part of the comment.

        // 10. The current character is used as the start of a new word.

        // Once a token is delimited, it is categorized as required by the grammar in Shell Grammar.
    }

    tokens
}
