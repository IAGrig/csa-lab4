use csa_lab4::isa::{self, CodeWord, Instr, Slot};
use std::collections::{BTreeSet, HashMap};
use std::fs::{self, File};
use std::io;
use std::io::Write;

#[derive(Debug, Clone)]
enum Token {
    LParen,
    RParen,
    Number(i32),
    Identifier(String),
    StringLiteral(String),
}

#[derive(Debug, Clone)]
enum AstNode {
    NumberLiteral(i32),
    StringLiteral(String),
    Symbol(String),     // variable or function name
    List(Vec<AstNode>), // (head arg1 arg2 ...)
}

#[derive(Debug, Clone)]
struct Line {
    label: Option<String>,
    instr: Instr,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct DataItem {
    pub label: String,
    pub size: usize,            // number of words in data memory
    pub bytes: Option<Vec<u8>>, // for string literals
}

struct Compiler {
    code: Vec<Line>,
    data: BTreeSet<DataItem>,
    labels_count: usize,
    pending_labels: Vec<String>,
}

impl Compiler {
    fn new() -> Self {
        Compiler {
            code: Vec::new(),
            data: BTreeSet::new(),
            labels_count: 0,
            pending_labels: vec![],
        }
    }

    fn new_label(&mut self, suffix: &str) -> String {
        let label = format!("L{}_{}", self.labels_count, suffix);
        self.labels_count += 1;
        label
    }

    fn alloc_variable(&mut self, label: String) -> String {
        let label = label + "_var";
        self.data.insert(DataItem {
            label: label.clone(),
            size: 1,
            bytes: None,
        });
        label
    }

    fn alloc_buffer(&mut self, size: usize) -> String {
        let label = self.new_label("buf");
        self.data.insert(DataItem {
            label: label.clone(),
            size,
            bytes: None,
        });
        label
    }

    fn alloc_string(&mut self, s: &str) -> String {
        let label = self.new_label(s);
        let mut v = s.as_bytes().to_vec();
        v.push(0); // null terminator
        self.data.insert(DataItem {
            label: label.clone(),
            size: v.len(),
            bytes: Some(v),
        });
        label
    }

    fn line(&mut self, instr: Instr) {
        for _ in 1..self.pending_labels.len() {
            let label = Some(self.pending_labels.remove(0));
            self.code.push(Line {
                label,
                instr: Instr::Nop,
            });
        }

        let label = if !self.pending_labels.is_empty() {
            Some(self.pending_labels.remove(0))
        } else {
            None
        };

        self.code.push(Line { label, instr });
    }

    fn attach_label_to_next(&mut self, label: String) {
        self.pending_labels.push(label);
    }

    fn tokenize(&self, src: String) -> Vec<Token> {
        let mut tokens = Vec::new();
        let mut chars = src.chars().peekable();
        let mut parentheses = Vec::new();

        while let Some(&char) = chars.peek() {
            match char {
                c if c.is_whitespace() => {
                    chars.next();
                }

                '(' => {
                    tokens.push(Token::LParen);
                    chars.next();
                    parentheses.push("(");
                }

                ')' => {
                    tokens.push(Token::RParen);
                    chars.next();

                    if parentheses.is_empty() || parentheses[parentheses.len() - 1] != "(" {
                        panic!("parentheses mismathc: not enough )");
                    }
                    parentheses.pop();
                }

                // string literal
                '"' => {
                    let mut str_buf = String::new();
                    chars.next(); // skip opening "
                    while let Some(&c) = chars.peek() {
                        match c {
                            '"' => {
                                chars.next(); // skip closing "
                                break;
                            }
                            character => {
                                str_buf.push(character);
                                chars.next();
                            }
                        }
                    }
                    tokens.push(Token::StringLiteral(str_buf));
                }

                // number
                c if c.is_ascii_digit()
                    || (c == '-' && chars.peek().unwrap_or(&'!').is_ascii_digit()) =>
                {
                    let mut str_buf = String::new();
                    str_buf.push(c);
                    chars.next();

                    while let Some(&c) = chars.peek() {
                        if c.is_ascii_digit() {
                            str_buf.push(c);
                            chars.next();
                        } else {
                            break;
                        }
                    }

                    match str_buf.parse::<i32>() {
                        Ok(num) => tokens.push(Token::Number(num)),
                        Err(_) => panic!("Invalid number: {str_buf}"),
                    }
                }

                // identifiers
                _ => {
                    let mut str_buf = String::new();
                    while let Some(&c) = chars.peek() {
                        if c.is_whitespace() || c == '(' || c == ')' {
                            break;
                        }
                        str_buf.push(c);
                        chars.next();
                    }
                    tokens.push(Token::Identifier(str_buf));
                }
            }
        }

        if !parentheses.is_empty() {
            panic!("{} ( never closed", parentheses.len());
        }

        tokens
    }

    fn parse(&self, tokens: &[Token]) -> Vec<AstNode> {
        fn parse_expr(idx: &mut usize, tokens: &[Token]) -> AstNode {
            match &tokens[*idx] {
                Token::LParen => {
                    *idx += 1;
                    let mut items = Vec::new();
                    while *idx < tokens.len() {
                        if let Token::RParen = tokens[*idx] {
                            *idx += 1;
                            break;
                        }
                        items.push(parse_expr(idx, tokens));
                    }
                    AstNode::List(items)
                }
                Token::RParen => panic!("Unexpected ) at {idx}"),
                Token::Number(n) => {
                    *idx += 1;
                    AstNode::NumberLiteral(*n)
                }
                Token::StringLiteral(s) => {
                    *idx += 1;
                    AstNode::StringLiteral(s.clone())
                }
                Token::Identifier(id) => {
                    *idx += 1;
                    AstNode::Symbol(id.clone())
                }
            }
        }
        let mut idx = 0;
        let mut exprs = Vec::new();
        while idx < tokens.len() {
            exprs.push(parse_expr(&mut idx, tokens));
        }
        exprs
    }

    fn separate_defuns(&self, ast: &[AstNode]) -> (Vec<AstNode>, Vec<AstNode>) {
        let mut main = Vec::new();
        let mut defuns = Vec::new();
        for node in ast {
            if let AstNode::List(items) = node {
                if let Some(AstNode::Symbol(head)) = &items.first() {
                    if head == "defun" {
                        defuns.push(node.clone());
                        continue;
                    }
                }
            }
            main.push(node.clone());
        }
        (main, defuns)
    }

    fn compile(&mut self, source_code: String) -> (Vec<CodeWord>, Vec<i32>) {
        let tokens = self.tokenize(source_code);
        let ast = self.parse(&tokens);
        let (main_nodes, defun_nodes) = self.separate_defuns(&ast);

        let main_label = "__main_start".to_string();
        self.line(Instr::Jump(main_label.clone()));

        // interrupts handlers
        for node in &defun_nodes {
            if let AstNode::List(items) = node {
                if let AstNode::Symbol(func_name) = &items[1] {
                    if let Some(stripped) = func_name.strip_prefix("isr") {
                        if stripped.parse::<u8>().is_ok() {
                            self.line(Instr::Jump(func_name.clone()));
                            continue;
                        }
                    }
                }
            }
        }

        let mut main = main_nodes;
        main.insert(0, AstNode::Symbol("progn".to_string()));
        self.attach_label_to_next(main_label);
        self.compile_node(&AstNode::List(main), false);

        self.line(Instr::Halt);

        for node in &defun_nodes {
            self.compile_node(node, false);
        }

        let mut packed_code = self.pack_code();
        self.patch_labels(&mut packed_code);

        let (data_memory, _) = self.pack_data();

        (packed_code, data_memory)
    }

    fn compile_node(&mut self, node: &AstNode, in_tail: bool) {
        match node {
            AstNode::NumberLiteral(n) => self.line(Instr::PushNum(*n)),
            AstNode::StringLiteral(s) => {
                let label = self.alloc_string(s);
                self.line(Instr::PushAddr(label));
            }
            AstNode::Symbol(name) => self.line(Instr::Load(name.clone() + "_var")),
            AstNode::List(items) if !items.is_empty() => {
                if let AstNode::Symbol(head) = &items[0] {
                    match head.as_str() {
                        "defun" if items.len() >= 4 => {
                            let func_name = if let AstNode::Symbol(str) = &items[1] {
                                str
                            } else {
                                panic!("No functions name provided")
                            };

                            self.attach_label_to_next(func_name.clone());
                            // store params to use them just as loads in body
                            if let AstNode::List(params) = &items[2] {
                                for p in params.iter().rev() {
                                    // reverse order because of stack
                                    if let AstNode::Symbol(param) = p {
                                        let label = self.alloc_variable(param.clone());
                                        self.line(Instr::Store(label));
                                    }
                                }
                            }

                            let body_node = if items.len() == 4 {
                                &items[3]
                            } else {
                                let mut expressions = items[3..].to_vec();
                                expressions.insert(0, AstNode::Symbol("progn".to_string()));
                                &AstNode::List(expressions)
                            };

                            self.compile_node(body_node, true);
                            if func_name.starts_with("isr") && func_name[3..].parse::<u8>().is_ok()
                            {
                                self.line(Instr::Drop); // isr must not affect data stack
                            }
                            self.line(Instr::Return);
                        }
                        "setq" if items.len() == 3 => {
                            if let AstNode::Symbol(var) = &items[1] {
                                self.compile_node(&items[2], false);
                                let label = self.alloc_variable(var.clone());
                                self.line(Instr::Dup);
                                self.line(Instr::Store(label));
                            } else {
                                panic!(
                                    "setq requires a symbol for variable name, got {:?}",
                                    items[1]
                                )
                            }
                        }
                        "if" if items.len() == 4 => {
                            let else_branch = self.new_label("else");
                            let end_branch = self.new_label("endif");

                            self.compile_node(&items[1], false);
                            self.line(Instr::BranchZero(else_branch.clone()));
                            self.line(Instr::Drop); // drop predicate
                            self.compile_node(&items[2], in_tail);
                            self.line(Instr::Jump(end_branch.clone()));

                            self.attach_label_to_next(else_branch);
                            self.line(Instr::Drop); // drop predicate
                            self.compile_node(&items[3], in_tail);

                            self.attach_label_to_next(end_branch);
                        }
                        "if" if items.len() == 3 => {
                            let end_branch = self.new_label("endif");

                            self.compile_node(&items[1], false);
                            self.line(Instr::BranchZero(end_branch.clone()));
                            self.line(Instr::Drop); // drop predicate
                            self.compile_node(&items[2], in_tail);

                            self.attach_label_to_next(end_branch);
                        }
                        "progn" if items.len() >= 2 => {
                            for expr in &items[1..items.len() - 1] {
                                self.compile_node(expr, false);
                                self.line(Instr::Drop);
                            }
                            self.compile_node(&items[items.len() - 1], in_tail);
                        }
                        "allocate" if items.len() == 2 => {
                            if let AstNode::NumberLiteral(n) = items[1] {
                                let label = self.alloc_buffer(n as usize);
                                self.line(Instr::PushAddr(label));
                            } else {
                                panic!("alloc-bytes takes only literal arguments");
                            }
                        }
                        "load-byte" if items.len() == 2 => {
                            self.compile_node(&items[1], false);
                            self.line(Instr::LoadIndirect);
                        }
                        "store-byte" if items.len() == 3 => {
                            // (sb byte addr)
                            self.compile_node(&items[1], false);
                            self.line(Instr::Dup);
                            self.compile_node(&items[2], false);
                            self.line(Instr::StoreIndirect);
                        }
                        "read-char" if items.len() == 2 => {
                            if let AstNode::NumberLiteral(port) = &items[1] {
                                self.line(Instr::In(*port));
                            }
                        }
                        "write-char" if items.len() == 3 => {
                            // (wc port char)
                            if let AstNode::NumberLiteral(port) = &items[2] {
                                self.compile_node(&items[1], false);
                                self.line(Instr::Dup);
                                self.line(Instr::Out(*port));
                            }
                        }
                        "enable-interrupts" => {
                            self.line(Instr::EnableInterrupts);
                            self.line(Instr::PushNum(1));
                        }
                        "disable-interrupts" => {
                            self.line(Instr::DisableInterrupts);
                            self.line(Instr::PushNum(0));
                        }
                        "enable-eam" => {
                            self.line(Instr::EnableEam);
                            self.line(Instr::PushNum(1));
                        }
                        "disable-eam" => {
                            self.line(Instr::DisableEam);
                            self.line(Instr::PushNum(0));
                        }
                        "+" => {
                            match items.len() {
                                // (+) -> 0
                                0 | 1 => self.line(Instr::PushNum(0)),
                                // (+ x) -> x
                                2 => self.compile_node(&items[1], false),
                                // 2+ arguments
                                _ => {
                                    for idx in (1..items.len()).rev() {
                                        // reverse because of stack cpu
                                        self.compile_node(&items[idx], false);
                                    }
                                    for _idx in 0..items.len() - 2 {
                                        self.line(Instr::Add);
                                    }
                                }
                            }
                        }
                        "-" => {
                            match items.len() {
                                // (-) -> 0
                                1 => self.line(Instr::PushNum(0)),
                                // (- x) -> -x
                                2 => {
                                    self.compile_node(&items[1], false);
                                    self.line(Instr::Neg);
                                }
                                // (- x y) -> x - y
                                _ => {
                                    for idx in (1..items.len()).rev() {
                                        // reverse because of stack cpu
                                        self.compile_node(&items[idx], false);
                                    }
                                    for _idx in 0..items.len() - 2 {
                                        self.line(Instr::Sub);
                                    }
                                }
                            }
                        }
                        "*" => match items.len() {
                            0 | 1 => self.line(Instr::PushNum(1)),
                            2 => self.compile_node(&items[1], false),
                            _ => {
                                for idx in (1..items.len()).rev() {
                                    // reverse because of stack cpu
                                    self.compile_node(&items[idx], false);
                                }
                                for _idx in 0..items.len() - 2 {
                                    self.line(Instr::MultiplyLow);
                                }
                            }
                        },
                        "*^" => match items.len() {
                            0 | 1 => self.line(Instr::PushNum(1)),
                            2 => self.compile_node(&items[1], false),
                            _ => {
                                for idx in (1..items.len()).rev() {
                                    // reverse because of stack cpu
                                    self.compile_node(&items[idx], false);
                                }
                                for _idx in 0..items.len() - 2 {
                                    self.line(Instr::MultiplyHigh);
                                }
                            }
                        },
                        "/" => match items.len() {
                            0 | 1 => self.line(Instr::PushNum(1)),
                            2 => self.compile_node(&items[1], false),
                            _ => {
                                for idx in (1..items.len()).rev() {
                                    // reverse because of stack cpu
                                    self.compile_node(&items[idx], false);
                                }
                                for _idx in 0..items.len() - 2 {
                                    self.line(Instr::Divide);
                                }
                            }
                        },
                        "%" => match items.len() {
                            0 | 1 => self.line(Instr::PushNum(0)),
                            2 => self.compile_node(&items[1], false),
                            _ => {
                                for idx in (1..items.len()).rev() {
                                    // reverse because of stack cpu
                                    self.compile_node(&items[idx], false);
                                }
                                for _idx in 0..items.len() - 2 {
                                    self.line(Instr::Remainder);
                                }
                            }
                        },
                        "and" => match items.len() {
                            0 | 1 => self.line(Instr::PushNum(1)),
                            _ => {
                                let false_branch = self.new_label("and_false");
                                let end_branch = self.new_label("and_exit");

                                for node in &items[1..] {
                                    self.compile_node(node, false);
                                    self.line(Instr::BranchZero(false_branch.clone()));
                                    self.line(Instr::Drop);
                                }
                                self.line(Instr::PushNum(1));
                                self.line(Instr::Jump(end_branch.clone()));

                                self.attach_label_to_next(false_branch);
                                self.line(Instr::Drop);
                                self.line(Instr::PushNum(0));

                                self.attach_label_to_next(end_branch);
                            }
                        },
                        "=" => match items.len() {
                            0..=2 => self.line(Instr::PushNum(0)),
                            3 => {
                                let true_branch = self.new_label("eq_false");
                                let end_branch = self.new_label("eq_exit");

                                self.compile_node(&items[1], false);
                                self.compile_node(&items[2], false);
                                self.line(Instr::Xor);
                                self.line(Instr::BranchZero(true_branch.clone()));
                                self.line(Instr::Drop);
                                self.line(Instr::PushNum(0));
                                self.line(Instr::Jump(end_branch.clone()));

                                self.attach_label_to_next(true_branch);
                                self.line(Instr::Drop);
                                self.line(Instr::PushNum(1));

                                self.attach_label_to_next(end_branch);
                            }
                            _ => {
                                unimplemented!("= takes only 2 arguments");
                            }
                        },
                        ">" => match items.len() {
                            0..=2 => self.line(Instr::PushNum(0)),
                            3 => {
                                let pos_branch = self.new_label("gt_false");
                                let end_branch = self.new_label("gt_exit");

                                self.compile_node(&items[1], false);
                                self.compile_node(&items[2], false);
                                self.line(Instr::Sub);
                                self.line(Instr::BranchPositive(pos_branch.clone()));
                                self.line(Instr::BranchZero(pos_branch.clone()));
                                self.line(Instr::Drop);
                                self.line(Instr::PushNum(1));
                                self.line(Instr::Jump(end_branch.clone()));

                                self.attach_label_to_next(pos_branch);
                                self.line(Instr::Drop);
                                self.line(Instr::PushNum(0));

                                self.attach_label_to_next(end_branch);
                            }
                            _ => {
                                unimplemented!("> takes only 2 arguments");
                            }
                        },
                        "<" => match items.len() {
                            0..=2 => self.line(Instr::PushNum(0)),
                            3 => {
                                let pos_branch = self.new_label("lt_false");
                                let end_branch = self.new_label("lt_exit");

                                self.compile_node(&items[1], false);
                                self.compile_node(&items[2], false);
                                self.line(Instr::Sub);
                                self.line(Instr::Neg);
                                self.line(Instr::BranchPositive(pos_branch.clone()));
                                self.line(Instr::BranchZero(pos_branch.clone()));
                                self.line(Instr::Drop);
                                self.line(Instr::PushNum(1));
                                self.line(Instr::Jump(end_branch.clone()));

                                self.attach_label_to_next(pos_branch);
                                self.line(Instr::Drop);
                                self.line(Instr::PushNum(0));

                                self.attach_label_to_next(end_branch);
                            }
                            _ => {
                                unimplemented!("< takes only 2 arguments");
                            }
                        },
                        ">=" => {
                            // not <, so just changed push values
                            match items.len() {
                                0..=2 => self.line(Instr::PushNum(0)),
                                3 => {
                                    let pos_branch = self.new_label("gte_false");
                                    let end_branch = self.new_label("gte_exit");

                                    self.compile_node(&items[1], false);
                                    self.compile_node(&items[2], false);
                                    self.line(Instr::Sub);
                                    self.line(Instr::Neg);
                                    self.line(Instr::BranchPositive(pos_branch.clone()));
                                    self.line(Instr::BranchZero(pos_branch.clone()));
                                    self.line(Instr::Drop);
                                    self.line(Instr::PushNum(0));
                                    self.line(Instr::Jump(end_branch.clone()));

                                    self.attach_label_to_next(pos_branch);
                                    self.line(Instr::Drop);
                                    self.line(Instr::PushNum(1));

                                    self.attach_label_to_next(end_branch);
                                }
                                _ => {
                                    unimplemented!(">= takes only 2 arguments");
                                }
                            }
                        }
                        "<=" => match items.len() {
                            0..=2 => self.line(Instr::PushNum(0)),
                            3 => {
                                let pos_branch = self.new_label("lte_false");
                                let end_branch = self.new_label("lte_exit");

                                self.compile_node(&items[1], false);
                                self.compile_node(&items[2], false);
                                self.line(Instr::Sub);
                                self.line(Instr::BranchPositive(pos_branch.clone()));
                                self.line(Instr::BranchZero(pos_branch.clone()));
                                self.line(Instr::Drop);
                                self.line(Instr::PushNum(0));
                                self.line(Instr::Jump(end_branch.clone()));

                                self.attach_label_to_next(pos_branch);
                                self.line(Instr::Drop);
                                self.line(Instr::PushNum(1));

                                self.attach_label_to_next(end_branch);
                            }
                            _ => {
                                unimplemented!("<= takes only 2 arguments");
                            }
                        },
                        _ => {
                            // function call
                            for arg in &items[1..] {
                                self.compile_node(arg, false);
                            }
                            if in_tail {
                                self.line(Instr::Jump(head.clone()));
                            } else {
                                self.line(Instr::Call(head.clone()));
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }

    fn pack_code(&self) -> Vec<CodeWord> {
        let mut words: Vec<CodeWord> = Vec::new();

        let mut current = CodeWord {
            label: None,
            slots: Vec::new(),
        };

        for line in &self.code {
            if isa::is_arg_instr(&line.instr) || line.label.is_some() {
                // flush current partial word
                if !current.slots.is_empty() {
                    words.push(current);
                }
                current = CodeWord {
                    label: line.label.clone(),
                    slots: Vec::new(),
                };

                if isa::is_arg_instr(&line.instr) {
                    // extract placeholder arg
                    let arg = match &line.instr {
                        Instr::PushNum(n) => *n,
                        Instr::In(n) => *n,
                        Instr::Out(n) => *n,
                        _ => 0,
                    };
                    current.slots.push(Slot::Arg(line.instr.clone(), arg));
                } else {
                    current.slots.push(Slot::Basic(line.instr.clone()));
                }

                words.push(current);
                current = CodeWord {
                    label: None,
                    slots: Vec::new(),
                };
                continue;
            }
            // no operand and no label
            current.slots.push(Slot::Basic(line.instr.clone()));
            if current.slots.len() == isa::SLOTS_PER_WORD {
                words.push(current);
                // reset
                current = CodeWord {
                    label: None,
                    slots: Vec::new(),
                };
            }
        }
        // flush any remaining
        if !current.slots.is_empty() {
            words.push(current);
        }

        words
    }

    fn pack_data(&self) -> (Vec<i32>, HashMap<String, i32>) {
        let mut data_mem = Vec::new();
        let mut labels_map = HashMap::new();
        let mut addr = 0;

        for item in &self.data {
            labels_map.insert(item.label.clone(), addr);
            if let Some(bytes) = &item.bytes {
                // store each byte as a word
                for &b in bytes {
                    data_mem.push(b as i32);
                    addr += 1;
                }
                // pad remaining words
                data_mem.resize(data_mem.len() + item.size - bytes.len(), 0);
                addr += (item.size - bytes.len()) as i32;
            } else {
                data_mem.resize(data_mem.len() + item.size, 0);
                addr += item.size as i32;
            }
        }
        (data_mem, labels_map)
    }

    fn patch_labels(&self, words: &mut [CodeWord]) {
        // code labels
        let mut map: HashMap<String, i32> = HashMap::new();
        for (i, w) in words.iter().enumerate() {
            if let Some(lbl) = &w.label {
                map.insert(lbl.clone(), i as i32);
            }
        }
        // data labels
        let (_data_mem, data_map) = self.pack_data();
        for (lbl, &a) in &data_map {
            map.insert(lbl.clone(), a);
        }

        // patch args in slots
        for word in words.iter_mut() {
            for slot in word.slots.iter_mut() {
                if let Slot::Arg(
                    Instr::Jump(label)
                    | Instr::Call(label)
                    | Instr::BranchZero(label)
                    | Instr::BranchPositive(label)
                    | Instr::Load(label)
                    | Instr::Store(label)
                    | Instr::PushAddr(label),
                    arg,
                ) = slot
                {
                    if let Some(&addr) = map.get(label) {
                        *arg = addr;
                    }
                }
            }
        }
    }

    fn write_code_binary(&self, path: String, code: &[CodeWord]) -> std::io::Result<()> {
        let mut file = File::create(path)?;

        for cw in code.iter() {
            let mut word: u32 = 0;

            if let Some(Slot::Arg(instr, imm)) = cw.slots.first() {
                let opcode = isa::instr_to_u8(instr) as u32;
                word |= opcode << (isa::INSTR_WORD_BITS - 5);
                let n = isa::arg_bitcount(instr);
                let mask = (1u32 << n) - 1;
                word |= (*imm as u32) & mask;
            } else {
                // basic instructions without arguments
                for i in 0..isa::SLOTS_PER_WORD {
                    let opcode = match cw.slots.get(i) {
                        Some(Slot::Basic(instr)) => isa::instr_to_u8(instr) as u32,
                        _ => isa::instr_to_u8(&Instr::Nop) as u32, // empty slot -> NOP
                    };
                    let shift = (isa::INSTR_WORD_BITS - 5 * (i + 1)) as u32;
                    word |= opcode << shift;
                }
            }

            file.write_all(&word.to_be_bytes())?;
        }

        Ok(())
    }

    fn write_code_hex(&self, path: String, code: &[CodeWord]) -> std::io::Result<()> {
        let mut file = File::create(path)?;

        for (i, cw) in code.iter().enumerate() {
            let mut word: u32 = 0;

            if let Some(Slot::Arg(instr, imm)) = cw.slots.first() {
                let opcode = isa::instr_to_u8(instr) as u32;
                word |= opcode << (isa::INSTR_WORD_BITS - 5);
                let n = isa::arg_bitcount(instr);
                let mask = (1u32 << n) - 1;
                word |= (*imm as u32) & mask;
            } else {
                // basic instructions without arguments
                for i in 0..isa::SLOTS_PER_WORD {
                    let opcode = match cw.slots.get(i) {
                        Some(Slot::Basic(instr)) => isa::instr_to_u8(instr) as u32,
                        _ => isa::instr_to_u8(&Instr::Nop) as u32, // empty slot -> NOP
                    };
                    let shift = (isa::INSTR_WORD_BITS - 5 * (i + 1)) as u32;
                    word |= opcode << shift;
                }
            }

            let mut mnemonics = Vec::new();
            for slot in cw.slots.iter() {
                match slot {
                    Slot::Basic(instr) => mnemonics.push(format!("{instr:?}").to_lowercase()),
                    Slot::Arg(instr, arg) => {
                        let instr = format!("{instr:?}").to_lowercase();
                        mnemonics.push(format!("{instr} {arg}"));
                    }
                }
            }

            writeln!(
                file,
                "{:04} - {:#010X} - {}",
                i,
                word,
                mnemonics.join(" | ")
            )?;
        }

        Ok(())
    }

    fn write_data_binary(&self, path: String, data: &[i32]) -> io::Result<()> {
        let mut f = File::create(path)?;
        let mut buf = [0u8; 3];

        for &value in data {
            let truncated = (value << 8) >> 8; // truncate saving sign

            buf[0] = ((truncated >> 16) & 0xFF) as u8;
            buf[1] = ((truncated >> 8) & 0xFF) as u8;
            buf[2] = (truncated & 0xFF) as u8;

            f.write_all(&buf)?;
        }
        Ok(())
    }
}

fn main() {
    let code_filepath = std::env::args().nth(1).expect(
        "Usage: translator <source> <out> \n (Generate out.bin, out.data.bin and out.bin.hex)",
    );
    let out_filepath = std::env::args().nth(2).expect(
        "Usage: translator <source> <out> \n (Generate out.bin, out.data.bin and out.bin.hex)",
    );

    let source_code = fs::read_to_string(code_filepath).unwrap();

    let mut c = Compiler::new();
    let (packed_code, data_memory) = c.compile(source_code);

    c.write_code_binary(out_filepath.clone() + ".bin", &packed_code)
        .unwrap();
    c.write_data_binary(out_filepath.clone() + ".data.bin", &data_memory)
        .unwrap();
    c.write_code_hex(out_filepath.clone() + ".bin.hex", &packed_code)
        .unwrap();
}
