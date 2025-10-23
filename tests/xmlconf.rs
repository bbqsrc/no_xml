use console::Style;
use no_xml::{Error, Visitor, XmlWriter, stream_xml_events, stream_xml_events_fragment};
use similar::{ChangeTag, TextDiff};
use std::collections::HashMap;
use std::fmt::Write as _;

const GREEN: &str = "\x1b[32m";
const RED: &str = "\x1b[31m";
const RESET: &str = "\x1b[0m";

fn decode_utf16_be(bytes: &[u8]) -> Result<String, String> {
    if bytes.len() % 2 != 0 {
        return Err("Invalid UTF-16 BE: odd number of bytes".to_string());
    }
    let u16s: Vec<u16> = bytes
        .chunks_exact(2)
        .map(|chunk| u16::from_be_bytes([chunk[0], chunk[1]]))
        .collect();
    String::from_utf16(&u16s).map_err(|e| e.to_string())
}

fn decode_utf16_le(bytes: &[u8]) -> Result<String, String> {
    if bytes.len() % 2 != 0 {
        return Err("Invalid UTF-16 LE: odd number of bytes".to_string());
    }
    let u16s: Vec<u16> = bytes
        .chunks_exact(2)
        .map(|chunk| u16::from_le_bytes([chunk[0], chunk[1]]))
        .collect();
    String::from_utf16(&u16s).map_err(|e| e.to_string())
}

#[derive(Debug, Clone)]
pub struct Test {
    pub uri: String,
    pub id: String,
    pub test_type: String,
    pub version: Option<String>,
    pub sections: Option<String>,
    pub output: Option<String>,
    pub entities: Option<String>,
    pub description: String,
}

#[derive(Debug)]
pub enum TestResult {
    Pass,
    Skipped,
    Fail {
        expected: String,
        actual: String,
        error_msg: Option<String>,
        diff: Option<String>,
    },
}

struct FailureInfo {
    suite: String,
    id: String,
    file_path: String,
    test_type: String,
    description: String,
    result: TestResult,
    expected_output_path: Option<String>,
}

#[derive(Debug, Clone)]
pub enum ContentItem {
    EntityRef(String),
    Text(String),
    Comment(String),
}

#[derive(Debug, Clone)]
pub struct TestCases {
    pub profile: Option<String>,
    pub xml_base: Option<String>,
    pub content: Vec<ContentItem>,
}

#[derive(Debug, Clone)]
pub struct TestSuite {
    pub profile: String,
    pub test_cases: Vec<TestCases>,
    pub entities: HashMap<String, String>,
}

enum ParsingContext {
    Root,
    InTestSuite,
    InTestCases,
}

pub struct TestSuiteVisitor {
    context_stack: Vec<ParsingContext>,

    suite: Option<TestSuite>,
    current_test_cases: Option<TestCases>,

    current_doctype_content: Option<String>,
    current_entity_content: Option<String>,
    pending_entities: HashMap<String, String>,
}

impl TestSuiteVisitor {
    pub fn new() -> Self {
        Self {
            context_stack: vec![ParsingContext::Root],
            suite: None,
            current_test_cases: None,
            current_doctype_content: None,
            current_entity_content: None,
            pending_entities: HashMap::new(),
        }
    }

    pub fn into_suite(self) -> Option<TestSuite> {
        self.suite
    }
}

impl Visitor for TestSuiteVisitor {
    fn open_tag(&mut self, name: &str) -> Result<(), Error> {
        match name {
            "TESTSUITE" => {
                self.context_stack.push(ParsingContext::InTestSuite);
                let mut entities = HashMap::new();
                std::mem::swap(&mut entities, &mut self.pending_entities);
                self.suite = Some(TestSuite {
                    profile: String::new(),
                    test_cases: Vec::new(),
                    entities,
                });
            }
            "TESTCASES" => {
                self.context_stack.push(ParsingContext::InTestCases);
                self.current_test_cases = Some(TestCases {
                    profile: None,
                    xml_base: None,
                    content: Vec::new(),
                });
            }
            _ => {}
        }
        Ok(())
    }

    fn start_close_tag(&mut self, name: &str) -> Result<(), Error> {
        match name {
            "TESTSUITE" => {
                self.context_stack.pop();
            }
            "TESTCASES" => {
                self.context_stack.pop();
                if let Some(test_cases) = self.current_test_cases.take() {
                    if let Some(ref mut suite) = self.suite {
                        suite.test_cases.push(test_cases);
                    }
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn attribute(&mut self, key: &str, value: &str) -> Result<(), Error> {
        match key {
            "PROFILE" => match self.context_stack.last() {
                Some(ParsingContext::InTestSuite) => {
                    if let Some(ref mut suite) = self.suite {
                        suite.profile = value.to_string();
                    }
                }
                Some(ParsingContext::InTestCases) => {
                    if let Some(ref mut test_cases) = self.current_test_cases {
                        test_cases.profile = Some(value.to_string());
                    }
                }
                _ => {}
            },
            "xml:base" => {
                if let Some(ref mut test_cases) = self.current_test_cases {
                    test_cases.xml_base = Some(value.to_string());
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn text(&mut self, text: &str) -> Result<(), Error> {
        if let Some(ParsingContext::InTestCases) = self.context_stack.last() {
            if !text.trim().is_empty() {
                if let Some(ref mut test_cases) = self.current_test_cases {
                    test_cases.content.push(ContentItem::Text(text.to_string()));
                }
            }
        }
        Ok(())
    }

    fn comment(&mut self, text: &str) -> Result<(), Error> {
        if let Some(ParsingContext::InTestCases) = self.context_stack.last() {
            if let Some(ref mut test_cases) = self.current_test_cases {
                test_cases
                    .content
                    .push(ContentItem::Comment(text.to_string()));
            }
        }
        Ok(())
    }

    fn entity_ref(&mut self, entity: &str) -> Result<(), Error> {
        if let Some(ParsingContext::InTestCases) = self.context_stack.last() {
            if let Some(ref mut test_cases) = self.current_test_cases {
                test_cases
                    .content
                    .push(ContentItem::EntityRef(entity.to_string()));
            }
        }
        Ok(())
    }

    fn start_doctype(&mut self) -> Result<(), Error> {
        self.current_doctype_content = Some(String::new());
        Ok(())
    }

    fn doctype(&mut self, content: &str) -> Result<(), Error> {
        if let Some(ref mut doctype_content) = self.current_doctype_content {
            doctype_content.push_str(content);
        }
        Ok(())
    }

    fn end_doctype(&mut self) -> Result<(), Error> {
        self.current_doctype_content.take();
        Ok(())
    }

    fn entity_declaration(&mut self, content: &str) -> Result<(), Error> {
        if let Some(ref mut entity_content) = self.current_entity_content {
            entity_content.push_str(content);
            entity_content.push(' ');
        }
        Ok(())
    }

    fn end_entity_declaration(&mut self) -> Result<(), Error> {
        if let Some(content) = self.current_entity_content.take() {
            let trimmed = content.trim();

            let mut parts = trimmed.splitn(2, |c: char| c.is_whitespace());
            let entity_name = parts.next().unwrap_or("");
            let rest = parts.next().unwrap_or("");

            if !entity_name.is_empty() && !rest.is_empty() {
                let entity_value = if rest.trim().starts_with("SYSTEM") {
                    rest.trim()
                        .strip_prefix("SYSTEM")
                        .unwrap_or(rest)
                        .trim()
                        .trim_matches('"')
                        .to_string()
                } else {
                    rest.trim().trim_matches('"').to_string()
                };

                if let Some(ref mut suite) = self.suite {
                    suite.entities.insert(entity_name.to_string(), entity_value);
                } else {
                    self.pending_entities
                        .insert(entity_name.to_string(), entity_value);
                }
            }
        }
        Ok(())
    }

    fn start_entity_declaration(
        &mut self,
        _decl_type: no_xml::EntityDeclType,
    ) -> Result<(), Error> {
        self.current_entity_content = Some(String::new());
        Ok(())
    }
}

fn format_colored_diff(old: &str, new: &str) -> String {
    let mut output = String::new();
    let diff = TextDiff::from_lines(old, new);

    for (idx, group) in diff.grouped_ops(3).iter().enumerate() {
        if idx > 0 {
            let _ = writeln!(&mut output, "{:-^1$}", "-", 80);
        }
        for op in group {
            for change in diff.iter_inline_changes(op) {
                let (sign, style) = match change.tag() {
                    ChangeTag::Delete => ("-", Style::new().red()),
                    ChangeTag::Insert => ("+", Style::new().green()),
                    ChangeTag::Equal => (" ", Style::new().dim()),
                };
                let _ = write!(
                    &mut output,
                    "{} {} {} | ",
                    style.apply_to(line_num(change.old_index())).bold(),
                    style.apply_to(line_num(change.new_index())).bold(),
                    style.apply_to(sign).bold(),
                );
                for (emphasized, value) in change.iter_strings_lossy() {
                    if emphasized {
                        let _ = write!(
                            &mut output,
                            "{}",
                            style.clone().underlined().on_black().apply_to(value)
                        );
                    } else {
                        let _ = write!(&mut output, "{}", style.apply_to(value));
                    }
                }
                if change.missing_newline() {
                    let _ = writeln!(&mut output);
                }
            }
        }
    }

    output
}

fn line_num(idx: Option<usize>) -> String {
    match idx {
        Some(idx) => format!("{:4}", idx + 1),
        None => "    ".to_string(),
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().collect();

    // Check for -p flag to test a single file
    let mut path_to_test = None;
    let mut skip_output_tests = false;
    let mut i = 1;
    while i < args.len() {
        if args[i] == "-p" && i + 1 < args.len() {
            path_to_test = Some(args[i + 1].as_str());
            i += 2;
            continue;
        }
        if args[i] == "--no-output" {
            skip_output_tests = true;
        }
        i += 1;
    }

    if let Some(path) = path_to_test {
        println!("Testing file: {}\n", path);

        let buf = std::fs::read(path)?;
        let content = match if buf.starts_with(&[0xFE, 0xFF]) {
            decode_utf16_be(&buf[2..])
        } else if buf.starts_with(&[0xFF, 0xFE]) {
            decode_utf16_le(&buf[2..])
        } else {
            String::from_utf8(buf).map_err(|e| e.to_string())
        } {
            Ok(s) => s,
            Err(e) => {
                println!("FAIL: Failed to decode file: {}", e);
                std::process::exit(1);
            }
        };

        struct DummyVisitor;
        impl Visitor for DummyVisitor {}

        match stream_xml_events::<Vec<&'_ str>>(&content, &mut DummyVisitor) {
            Ok(_) => {
                println!("PASS: File parsed successfully");
                std::process::exit(0);
            }
            Err(e) => {
                println!("FAIL: {}:{} {}", path, e.span, e.kind);
                std::process::exit(1);
            }
        }
    }

    let filter_type = args.get(2).map(|s| s.as_str());
    let filter_id = args.get(3).map(|s| s.as_str());

    if let Some(filter) = filter_type {
        println!("Filtering tests by TYPE={:?}", filter);
    }
    if let Some(filter) = filter_id {
        println!("Filtering tests by ID={:?}", filter);
    }
    if filter_type.is_some() || filter_id.is_some() {
        println!();
    }

    let xml_content = std::fs::read_to_string("xmlconf/xmlconf.xml")?;

    let mut visitor = TestSuiteVisitor::new();
    stream_xml_events::<Vec<&'_ str>>(&xml_content, &mut visitor)
        .map_err(|e| format!("Error parsing xmlconf/xmlconf.xml: {}", e))?;

    let suite = visitor.into_suite().ok_or("Failed to parse test suite")?;

    // Run all tests
    println!("running tests\n");

    let mut total_pass = 0;
    let mut total_fail = 0;
    let mut total_skip = 0;
    let mut total_tests = 0;
    let mut failures: Vec<FailureInfo> = Vec::new();

    for test_cases in suite.test_cases.iter() {
        for item in &test_cases.content {
            if let ContentItem::EntityRef(entity) = item {
                if let Some(entity_path) = suite.entities.get(entity) {
                    // Entity path is already correct relative to xmlconf/
                    let full_path = format!("xmlconf/{}", entity_path);
                    let suite_name = entity.as_str();

                    match parse_test_file(&full_path) {
                        Ok(tests) => {
                            // xml:base is the base directory for test file URIs
                            let base_dir = if let Some(ref xml_base) = test_cases.xml_base {
                                format!("xmlconf/{}", xml_base)
                            } else {
                                "xmlconf/".to_string()
                            };

                            for test in &tests {
                                // Skip tests that don't match the type filter
                                if let Some(filter) = filter_type {
                                    if test.test_type != filter {
                                        continue;
                                    }
                                }

                                // Skip tests that don't match the ID filter
                                if let Some(filter) = filter_id {
                                    if test.id != filter {
                                        continue;
                                    }
                                }

                                // Skip non-1.0 XML tests
                                if let Some(ref version) = test.version {
                                    if version != "1.0" {
                                        continue;
                                    }
                                }

                                print!("test {}.{} ... ", suite_name, test.id);
                                let test_file_path = format!("{}{}", base_dir, test.uri);
                                let result = run_test(test, &base_dir, skip_output_tests);
                                match result {
                                    TestResult::Pass => {
                                        println!("{}ok{}", GREEN, RESET);
                                        total_pass += 1;
                                    }
                                    TestResult::Skipped => {
                                        println!("skipped");
                                        total_skip += 1;
                                    }
                                    TestResult::Fail { .. } => {
                                        println!("{}FAILED{}", RED, RESET);
                                        let expected_output_path =
                                            test.output.as_ref().map(|output_file| {
                                                format!("{}{}", base_dir, output_file)
                                            });
                                        failures.push(FailureInfo {
                                            suite: suite_name.to_string(),
                                            id: test.id.clone(),
                                            file_path: test_file_path,
                                            test_type: test.test_type.clone(),
                                            description: test.description.clone(),
                                            result,
                                            expected_output_path,
                                        });
                                        total_fail += 1;
                                    }
                                }
                                total_tests += 1;
                            }
                        }
                        Err(e) => {
                            println!("  {}", e);
                        }
                    }
                }
            }
        }
    }

    // Print failures
    if !failures.is_empty() {
        println!("\nfailures:\n");
        for failure in &failures {
            if let TestResult::Fail {
                expected,
                actual,
                error_msg,
                diff,
            } = &failure.result
            {
                println!("---- test {}.{} ----", failure.suite, failure.id);
                println!("File: {}", failure.file_path);
                println!("Description: {}", failure.description);
                if let Some(ref expected_output_path) = failure.expected_output_path {
                    println!("Expected output: {}", expected_output_path);
                }
                println!("{}Expected:{} {}", GREEN, RESET, expected);
                println!("{}Actual:{} {}", RED, RESET, actual);
                if let Some(err) = error_msg {
                    println!("Error: {}", err);
                }
                if let Some(d) = diff {
                    println!("{}", d);
                }
                println!();
            }
        }
    }

    // Count failures by type
    let mut failures_by_type: HashMap<&str, usize> = HashMap::new();
    for failure in &failures {
        *failures_by_type
            .entry(failure.test_type.as_str())
            .or_insert(0) += 1;
    }

    // Print summary
    if !failures_by_type.is_empty() {
        println!("failures by type:");
        let mut types: Vec<_> = failures_by_type.iter().collect();
        types.sort_by_key(|(k, _)| *k);
        for (test_type, count) in types {
            println!("  {}: {} failed", test_type, count);
        }
        println!();
    }

    println!(
        "test result: {}. {} passed; {} failed; {} skipped\n",
        if total_fail == 0 {
            format!("{}ok{}", GREEN, RESET)
        } else {
            format!("{}FAILED{}", RED, RESET)
        },
        total_pass,
        total_fail,
        total_skip
    );

    if total_fail > 0 {
        std::process::exit(1);
    }

    Ok(())
}

struct TestFileVisitor {
    tests: Vec<Test>,
    current_test: Option<Test>,
    current_text: String,
}

impl TestFileVisitor {
    fn new() -> Self {
        Self {
            tests: Vec::new(),
            current_test: None,
            current_text: String::new(),
        }
    }

    fn into_tests(self) -> Vec<Test> {
        self.tests
    }
}

impl Visitor for TestFileVisitor {
    fn open_tag(&mut self, name: &str) -> Result<(), Error> {
        if name == "TEST" {
            self.current_test = Some(Test {
                uri: String::new(),
                id: String::new(),
                test_type: String::new(),
                version: None,
                sections: None,
                output: None,
                entities: None,
                description: String::new(),
            });
            self.current_text.clear();
        }
        Ok(())
    }

    fn start_close_tag(&mut self, name: &str) -> Result<(), Error> {
        if name == "TEST" {
            if let Some(mut test) = self.current_test.take() {
                test.description = self.current_text.trim().to_string();
                self.tests.push(test);
                self.current_text.clear();
            }
        }
        Ok(())
    }

    fn attribute(&mut self, key: &str, value: &str) -> Result<(), Error> {
        if let Some(ref mut test) = self.current_test {
            match key {
                "URI" => test.uri = value.to_string(),
                "ID" => test.id = value.to_string(),
                "TYPE" => test.test_type = value.to_string(),
                "VERSION" => test.version = Some(value.to_string()),
                "SECTIONS" => test.sections = Some(value.to_string()),
                "OUTPUT" => test.output = Some(value.to_string()),
                "ENTITIES" => test.entities = Some(value.to_string()),
                _ => {}
            }
        }
        Ok(())
    }

    fn text(&mut self, text: &str) -> Result<(), Error> {
        if self.current_test.is_some() {
            self.current_text.push_str(text);
        }
        Ok(())
    }
}

fn parse_test_file(path: &str) -> Result<Vec<Test>, Box<dyn std::error::Error>> {
    let content = std::fs::read_to_string(path)?;
    let mut visitor = TestFileVisitor::new();
    stream_xml_events_fragment::<Vec<&'_ str>>(&content, &mut visitor)
        .map_err(|e| format!("Error parsing {}: {}", path, e))?;
    Ok(visitor.into_tests())
}

fn run_test(test: &Test, base_path: &str, skip_output: bool) -> TestResult {
    let test_file_path = format!("{}{}", base_path, test.uri);

    // Try to parse the test file
    let buf = match std::fs::read(&test_file_path) {
        Ok(b) => b,
        Err(e) => {
            return TestResult::Fail {
                error_msg: Some(format!("Failed to read test file: {}", e)),
                expected: "successful parse".to_string(),
                actual: format!("Failed to read test file: {test_file_path}"),
                diff: None,
            };
        }
    };

    // Check for LE and BE UTF-16 BOMs and convert to UTF-8
    let content = match if buf.starts_with(&[0xFE, 0xFF]) {
        decode_utf16_be(&buf[2..])
    } else if buf.starts_with(&[0xFF, 0xFE]) {
        decode_utf16_le(&buf[2..])
    } else {
        String::from_utf8(buf).map_err(|e| e.to_string())
    } {
        Ok(s) => s,
        Err(e) => {
            return TestResult::Fail {
                error_msg: Some(format!("Failed to decode test file: {}", e)),
                expected: "successful parse".to_string(),
                actual: format!("failed to decode test file: {test_file_path}"),
                diff: None,
            };
        }
    };

    // Check if this is an OUTPUT test
    if let Some(ref output_file) = test.output
        && !skip_output
    {
        let mut writer = XmlWriter::with_source_path(&test_file_path);
        match stream_xml_events::<Vec<&'_ str>>(&content, &mut writer) {
            Ok(_) => {
                let actual_output = writer.finish();
                let expected_path = format!("{}{}", base_path, output_file);

                match std::fs::read_to_string(&expected_path) {
                    Ok(expected) => {
                        if actual_output == expected {
                            return TestResult::Pass;
                        } else {
                            let colored_diff = format_colored_diff(&expected, &actual_output);

                            return TestResult::Fail {
                                expected: "output to match expected file".to_string(),
                                actual: "output mismatch".to_string(),
                                diff: Some(colored_diff),
                                error_msg: None,
                            };
                        }
                    }
                    Err(e) => {
                        return TestResult::Fail {
                            expected: "load output file".to_string(),
                            actual: format!("failed to load {}", expected_path),
                            error_msg: Some(e.to_string()),
                            diff: None,
                        };
                    }
                }
            }
            Err(e) => {
                return TestResult::Fail {
                    expected: "successful parse for output test".to_string(),
                    actual: format!("{}:{} {}", test_file_path, e.span, e.kind),
                    error_msg: Some(e.to_string()),
                    diff: None,
                };
            }
        }
    }

    // Regular valid/invalid test
    struct DummyVisitor;
    impl Visitor for DummyVisitor {}

    let parse_result = stream_xml_events::<Vec<&'_ str>>(&content, &mut DummyVisitor);

    let parse_succeeded = parse_result.is_ok();
    let should_succeed = test.test_type == "valid";

    if parse_succeeded == should_succeed {
        TestResult::Pass
    } else {
        TestResult::Fail {
            diff: None,
            error_msg: if let Err(ref e) = parse_result {
                Some(e.to_string())
            } else {
                None
            },
            expected: if should_succeed {
                "successful parse".to_string()
            } else {
                "parse failure".to_string()
            },
            actual: if parse_succeeded {
                "successful parse".to_string()
            } else {
                let err = parse_result.unwrap_err();
                format!("{}:{} {}", test_file_path, err.span, err.kind)
            },
        }
    }
}
