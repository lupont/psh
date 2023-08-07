use crate::ast::nodes::*;

impl ToString for SyntaxTree {
    fn to_string(&self) -> String {
        let mut s = self.leading.to_string();
        if let Some((cmds, linebreak)) = &self.commands {
            s.push_str(&cmds.to_string());
            s.push_str(&linebreak.to_string());
        }
        s.push_str(&self.unparsed);
        s
    }
}

impl ToString for CompleteCommands {
    fn to_string(&self) -> String {
        let mut s = self.head.to_string();
        for (newlines, cmd) in &self.tail {
            s.push_str(&newlines.to_string());
            s.push_str(&cmd.to_string());
        }
        s
    }
}

impl ToString for CompleteCommand {
    fn to_string(&self) -> String {
        let mut s = String::new();

        match self {
            Self::List {
                list,
                separator_op: None,
                comment: None,
            } => s.push_str(&list.to_string()),
            Self::List {
                list,
                separator_op: Some(separator_op),
                comment: None,
            } => {
                s.push_str(&list.to_string());
                s.push_str(&separator_op.to_string());
            }
            Self::List {
                list,
                separator_op: None,
                comment: Some(comment),
            } => {
                s.push_str(&list.to_string());
                s.push_str(&comment.to_string());
            }
            Self::List {
                list,
                separator_op: Some(separator_op),
                comment: Some(comment),
            } => {
                s.push_str(&list.to_string());
                s.push_str(&separator_op.to_string());
                s.push_str(&comment.to_string());
            }
            Self::Comment { comment } => s.push_str(&comment.to_string()),
        }

        s
    }
}

impl ToString for Comment {
    fn to_string(&self) -> String {
        let mut s = self.whitespace.to_string();
        s.push('#');
        s.push_str(&self.content);
        s
    }
}

impl ToString for List {
    fn to_string(&self) -> String {
        let mut s = self.head.to_string();

        for (sep, list) in &self.tail {
            s.push_str(&sep.to_string());
            s.push_str(&list.to_string());
        }

        s
    }
}

impl ToString for AndOrList {
    fn to_string(&self) -> String {
        let mut s = self.head.to_string();
        for (op, linebreak, pipeline) in &self.tail {
            s.push_str(&op.to_string());
            s.push_str(&linebreak.to_string());
            s.push_str(&pipeline.to_string());
        }
        s
    }
}

impl ToString for Pipeline {
    fn to_string(&self) -> String {
        let mut s = match &self.bang {
            Some(bang) => bang.to_string(),
            None => "".to_string(),
        };
        s.push_str(&self.sequence.to_string());
        s
    }
}

impl ToString for Bang {
    fn to_string(&self) -> String {
        format!("{}!", self.whitespace)
    }
}

impl ToString for Pipe {
    fn to_string(&self) -> String {
        format!("{}|", self.whitespace)
    }
}

impl ToString for PipeSequence {
    fn to_string(&self) -> String {
        let mut s = self.head.to_string();
        for (pipe, linebreak, cmd) in &self.tail {
            s.push_str(&pipe.to_string());
            s.push_str(&linebreak.to_string());
            s.push_str(&cmd.to_string());
        }
        s
    }
}

impl ToString for Command {
    fn to_string(&self) -> String {
        match self {
            Self::Simple(cmd) => cmd.to_string(),
            Self::Compound(cmd, redirections) => {
                let mut s = cmd.to_string();
                for r in redirections {
                    s.push_str(&r.to_string());
                }
                s
            }
            Self::FunctionDefinition(func_def) => func_def.to_string(),
        }
    }
}

impl ToString for CompoundCommand {
    fn to_string(&self) -> String {
        match self {
            Self::Brace(brace_group) => brace_group.to_string(),
            Self::Subshell(subshell) => subshell.to_string(),
            Self::For(for_clause) => for_clause.to_string(),
            Self::Case(case_clause) => case_clause.to_string(),
            Self::If(if_clause) => if_clause.to_string(),
            Self::While(while_clause) => while_clause.to_string(),
            Self::Until(until_clause) => until_clause.to_string(),
        }
    }
}

impl ToString for Subshell {
    fn to_string(&self) -> String {
        let mut s = format!("{}(", &self.lparen_ws);
        s.push_str(&self.body.to_string());
        s.push_str(self.rparen_ws.as_ref());
        s.push(')');
        s
    }
}

impl ToString for CompoundList {
    fn to_string(&self) -> String {
        let mut s = format!("{}{}", self.linebreak.to_string(), self.term.to_string());
        if let Some(sep) = &self.separator {
            s.push_str(&sep.to_string());
        }
        s
    }
}

impl ToString for Term {
    fn to_string(&self) -> String {
        let mut s = self.head.to_string();
        for (sep, list) in &self.tail {
            s.push_str(&sep.to_string());
            s.push_str(&list.to_string());
        }
        s
    }
}

impl ToString for Name {
    fn to_string(&self) -> String {
        format!("{}{}", self.whitespace, self.name)
    }
}

impl ToString for ForClause {
    fn to_string(&self) -> String {
        match self {
            Self::Simple(name, do_group) => {
                format!("for{}{}", name.to_string(), do_group.to_string())
            }
            Self::Padded(name, seq_sep, do_group) => format!(
                "for{}{}{}",
                name.to_string(),
                seq_sep.to_string(),
                do_group.to_string()
            ),
            // FIXME
            Self::Full(name, linebreak, _wordlist, seq_sep, do_group) => format!(
                "for{}{}in{}{}",
                name.to_string(),
                linebreak.to_string(),
                seq_sep.to_string(),
                do_group.to_string()
            ),
        }
    }
}

impl ToString for CaseClause {
    fn to_string(&self) -> String {
        match self {
            Self::Normal(word, linebreak, linebreak2, case_list) => format!(
                "case{}{}in{}{}esac",
                word.to_string(),
                linebreak.to_string(),
                linebreak2.to_string(),
                case_list.to_string()
            ),
            Self::NoSeparator(word, linebreak, linebreak2, case_list) => format!(
                "case{}{}in{}{}esac",
                word.to_string(),
                linebreak.to_string(),
                linebreak2.to_string(),
                case_list.to_string()
            ),
            Self::Empty(word, linebreak, linebreak2) => format!(
                "case{}{}in{}",
                word.to_string(),
                linebreak.to_string(),
                linebreak2.to_string(),
            ),
        }
    }
}

impl ToString for CaseListNs {
    fn to_string(&self) -> String {
        let mut s = String::new();
        if let Some(list) = &self.case_list {
            s.push_str(&list.to_string());
        }
        s.push_str(&self.last.to_string());
        s
    }
}

impl ToString for CaseList {
    fn to_string(&self) -> String {
        let mut s = self.head.to_string();
        for part in &self.tail {
            s.push_str(&part.to_string());
        }
        s
    }
}

impl ToString for CaseItemNs {
    fn to_string(&self) -> String {
        match self {
            Self::Empty(false, pattern, linebreak) => {
                format!("{}){}", pattern.to_string(), linebreak.to_string(),)
            }
            Self::Empty(true, pattern, linebreak) => {
                format!("({}){}", pattern.to_string(), linebreak.to_string(),)
            }
            Self::List(false, pattern, list) => {
                format!("{}){}", pattern.to_string(), list.to_string(),)
            }
            Self::List(true, pattern, list) => {
                format!("({}){}", pattern.to_string(), list.to_string(),)
            }
        }
    }
}

impl ToString for CaseItem {
    fn to_string(&self) -> String {
        match self {
            Self::Empty(false, pattern, linebreak, end_linebreak) => format!(
                "{}){};;{}",
                pattern.to_string(),
                linebreak.to_string(),
                end_linebreak.to_string()
            ),
            Self::Empty(true, pattern, linebreak, end_linebreak) => format!(
                "({}){};;{}",
                pattern.to_string(),
                linebreak.to_string(),
                end_linebreak.to_string()
            ),
            Self::List(false, pattern, list, end_linebreak) => format!(
                "{}){};;{}",
                pattern.to_string(),
                list.to_string(),
                end_linebreak.to_string()
            ),
            Self::List(true, pattern, list, end_linebreak) => format!(
                "({}){};;{}",
                pattern.to_string(),
                list.to_string(),
                end_linebreak.to_string()
            ),
        }
    }
}

impl ToString for Pattern {
    fn to_string(&self) -> String {
        let mut s = self.head.to_string();
        for part in &self.tail {
            s.push('|');
            s.push_str(&part.to_string());
        }
        s
    }
}

impl ToString for IfClause {
    fn to_string(&self) -> String {
        let mut s = "if".to_string();
        s.push_str(&self.predicate.to_string());
        s.push_str("then");
        s.push_str(&self.body.to_string());
        if let Some(else_part) = &self.else_part {
            s.push_str(&else_part.to_string());
        }
        s.push_str("fi");
        s
    }
}

impl ToString for ElsePart {
    fn to_string(&self) -> String {
        let mut s = String::new();
        for (predicate, body) in &self.elseifs {
            s.push_str("elif");
            s.push_str(&predicate.to_string());
            s.push_str("then");
            s.push_str(&body.to_string());
        }
        if let Some(else_part) = &self.else_part {
            s.push_str("else");
            s.push_str(&else_part.to_string());
        }
        s
    }
}

impl ToString for WhileClause {
    fn to_string(&self) -> String {
        let mut s = "while".to_string();
        s.push_str(&self.predicate.to_string());
        s.push_str(&self.body.to_string());
        s
    }
}

impl ToString for UntilClause {
    fn to_string(&self) -> String {
        let mut s = "until".to_string();
        s.push_str(&self.predicate.to_string());
        s.push_str(&self.body.to_string());
        s
    }
}

impl ToString for FunctionDefinition {
    fn to_string(&self) -> String {
        let mut s = self.name.to_string();
        s.push_str(&self.parens);
        s.push_str(&self.linebreak.to_string());
        s.push_str(&self.body.to_string());
        s
    }
}

impl ToString for FunctionBody {
    fn to_string(&self) -> String {
        let mut s = self.command.to_string();
        for r in &self.redirections {
            s.push_str(&r.to_string());
        }
        s
    }
}

impl ToString for BraceGroup {
    fn to_string(&self) -> String {
        let mut s = self.lbrace_ws.clone();
        s.0.push('{');
        s.0.push_str(&self.body.to_string());
        s.0.push_str(self.rbrace_ws.as_ref());
        s.0.push('}');
        s.0
    }
}

impl ToString for DoGroup {
    fn to_string(&self) -> String {
        let mut s = "do".to_string();
        s.push_str(&self.body.to_string());
        s.push_str("done");
        s
    }
}

impl ToString for SimpleCommand {
    fn to_string(&self) -> String {
        let mut s = String::new();

        self.prefixes
            .iter()
            .for_each(|m| s.push_str(&m.to_string()));

        if let Some(name) = &self.name {
            s += &name.to_string();
        }

        self.suffixes
            .iter()
            .for_each(|m| s.push_str(&m.to_string()));
        s
    }
}

impl ToString for CmdPrefix {
    fn to_string(&self) -> String {
        match self {
            Self::Redirection(r) => r.to_string(),
            Self::Assignment(a) => a.to_string(),
        }
    }
}

impl ToString for CmdSuffix {
    fn to_string(&self) -> String {
        match self {
            Self::Word(w) => w.to_string(),
            Self::Redirection(r) => r.to_string(),
        }
    }
}

impl ToString for Redirection {
    fn to_string(&self) -> String {
        match self {
            Redirection::File {
                whitespace,
                input_fd,
                ty,
                target,
            } => {
                format!(
                    "{}{}{}{}",
                    whitespace,
                    if let Some(fd) = input_fd {
                        fd.to_string()
                    } else {
                        String::new()
                    },
                    ty.to_string(),
                    target.to_string()
                )
            }
            Redirection::Here {
                whitespace,
                input_fd,
                ty,
                content,
                end,
            } => format!(
                "{}{}{}{}{}",
                whitespace,
                if let Some(fd) = input_fd {
                    fd.to_string()
                } else {
                    String::new()
                },
                ty.to_string(),
                content.to_string(),
                end.to_string(),
            ),
        }
    }
}

impl ToString for RedirectionType {
    fn to_string(&self) -> String {
        match self {
            Self::Input => "<",
            Self::InputFd => "<&",
            Self::Output => ">",
            Self::OutputFd => ">&",
            Self::OutputAppend => ">>",
            Self::OutputClobber => ">|",
            Self::ReadWrite => "<>",
        }
        .to_string()
    }
}

impl ToString for HereDocType {
    fn to_string(&self) -> String {
        match self {
            Self::Normal => "<<",
            Self::StripTabs => "<<-",
        }
        .to_string()
    }
}

impl ToString for FileDescriptor {
    fn to_string(&self) -> String {
        match self {
            Self::Stdin => "0".to_string(),
            Self::Stdout => "1".to_string(),
            Self::Stderr => "2".to_string(),
            Self::Other(fd) => fd.to_string(),
        }
    }
}

impl ToString for VariableAssignment {
    fn to_string(&self) -> String {
        format!(
            "{}{}={}",
            self.whitespace,
            self.lhs.to_string(),
            match &self.rhs {
                Some(rhs) => rhs.to_string(),
                None => "".to_string(),
            }
        )
    }
}

impl ToString for Word {
    fn to_string(&self) -> String {
        format!("{}{}", self.whitespace, self.name)
    }
}

impl ToString for LogicalOp {
    fn to_string(&self) -> String {
        match self {
            Self::And(ws) => format!("{ws}&&"),
            Self::Or(ws) => format!("{ws}||"),
        }
    }
}

impl ToString for NewlineList {
    fn to_string(&self) -> String {
        self.whitespace.clone()
    }
}

impl ToString for Linebreak {
    fn to_string(&self) -> String {
        match &self.newlines {
            Some(newlines) => newlines.to_string(),
            None => "".to_string(),
        }
    }
}

impl ToString for SeparatorOp {
    fn to_string(&self) -> String {
        match self {
            Self::Sync(ws) => format!("{ws};"),
            Self::Async(ws) => format!("{ws}&"),
        }
    }
}

impl ToString for Separator {
    fn to_string(&self) -> String {
        match self {
            Self::Explicit(sep_op, linebreak) => {
                format!("{}{}", sep_op.to_string(), linebreak.to_string())
            }
            Self::Implicit(newlines) => newlines.to_string(),
        }
    }
}

impl ToString for SequentialSeparator {
    fn to_string(&self) -> String {
        match self {
            Self::Semi(linebreak) => format!(";{}", linebreak.to_string()),
            Self::Implicit(newlines) => newlines.to_string(),
        }
    }
}
