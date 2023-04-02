use crate::ast::prelude::*;

impl ToString for SyntaxTree {
    fn to_string(&self) -> String {
        // FIXME: save amount of new lines?
        self.program
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join("\n")
    }
}

impl ToString for CompleteCommand {
    fn to_string(&self) -> String {
        let mut s = self.list.to_string();

        if let Some(separator) = &self.separator {
            s.push_str(&separator.to_string());
        }

        s
    }
}

impl ToString for List {
    fn to_string(&self) -> String {
        let mut list = self.first.to_string();

        for (sep, and_or_list) in &self.rest {
            list.push_str(&sep.to_string());
            list.push_str(&and_or_list.to_string());
        }

        list
    }
}

impl ToString for AndOrList {
    fn to_string(&self) -> String {
        let mut list = self.first.to_string();
        for (op, pipeline) in &self.rest {
            list.push_str(&op.to_string());
            list.push_str(&pipeline.to_string());
        }
        list
    }
}

impl ToString for Pipeline {
    fn to_string(&self) -> String {
        let mut pipeline = self.first.to_string();

        for (ws, cmd) in &self.rest {
            let part = format!("{}|{}", ws, &cmd.to_string());
            pipeline.push_str(&part);
        }

        if let Some(ws) = &self.bang {
            let bang = format!("{ws}!");
            pipeline.insert_str(0, &bang);
        }

        pipeline
    }
}

impl ToString for Command {
    fn to_string(&self) -> String {
        match self {
            Command::Simple(s) => s.to_string(),
            Command::Compound(cmd, redirections) => {
                let mut s = cmd.to_string();
                for r in redirections {
                    s.push_str(&r.to_string());
                }
                s
            }
            Command::FunctionDefinition(f) => f.to_string(),
        }
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

impl ToString for CompoundCommand {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl ToString for FunctionDefinition {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl ToString for Separator {
    fn to_string(&self) -> String {
        match self {
            Separator::Sync(ws) => format!("{ws};"),
            Separator::Async(ws) => format!("{ws}&"),
        }
    }
}

impl ToString for LogicalOp {
    fn to_string(&self) -> String {
        match self {
            LogicalOp::And(ws) => format!("{ws}&&"),
            LogicalOp::Or(ws) => format!("{ws}||"),
        }
    }
}

impl ToString for Redirection {
    fn to_string(&self) -> String {
        match self {
            Redirection::Input {
                file_descriptor,
                target,
            } => format!("{}<{}", file_descriptor.to_string(), target.to_string()),
            Redirection::Output {
                file_descriptor,
                append,
                target,
            } => format!(
                "{}>{}{}",
                file_descriptor.to_string(),
                if *append { ">" } else { "" },
                target.to_string()
            ),
            Redirection::HereDocument {
                file_descriptor,
                delimiter,
            } => format!("{}<<{}", file_descriptor.to_string(), delimiter.to_string()),
        }
    }
}

impl ToString for VariableAssignment {
    fn to_string(&self) -> String {
        format!(
            "{}{}={}",
            self.whitespace,
            self.lhs,
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
