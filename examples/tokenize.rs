use no_xml::{Cursor, tokenize};

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} <file.xml>", args[0]);
        std::process::exit(1);
    }

    let file_path = &args[1];
    let content = match std::fs::read_to_string(file_path) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Error reading file {}: {}", file_path, e);
            std::process::exit(1);
        }
    };

    let mut cursor = Cursor::<()>::new(&content);

    println!("Tokenizing: {}\n", file_path);

    loop {
        match tokenize(cursor) {
            Ok((span, token, new_cursor)) => {
                println!(
                    "{:?} @ {}:{}-{}:{}",
                    token, span.start.line, span.start.col, span.end.line, span.end.col
                );

                if matches!(token, no_xml::Token::Eof) {
                    break;
                }

                cursor = new_cursor;
            }
            Err(e) => {
                eprintln!("\nError: {}", e);
                std::process::exit(1);
            }
        }
    }
}
