#![feature(str_from_utf16_endian)]

use no_xml::{Error, Visitor, stream_xml_events};
use std::collections::HashMap;

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
    Fail {
        expected: String,
        actual: String,
        error_msg: Option<String>,
    },
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
    fn start_tag(&mut self, name: &str) -> Result<(), Error> {
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

    fn end_tag(&mut self, name: &str) -> Result<(), Error> {
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

    fn start_entity_declaration(&mut self) -> Result<(), Error> {
        self.current_entity_content = Some(String::new());
        Ok(())
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().collect();
    let filter_type = if args.len() > 1 {
        Some(args[1].as_str())
    } else {
        None
    };

    if let Some(filter) = filter_type {
        println!("Filtering tests by TYPE={:?}\n", filter);
    }

    let xml_content = std::fs::read_to_string("xmlconf/xmlconf.xml")?;

    let mut visitor = TestSuiteVisitor::new();
    stream_xml_events::<Vec<&'_ str>>(&xml_content, &mut visitor)?;

    let suite = visitor.into_suite().ok_or("Failed to parse test suite")?;

    println!("Test Suite Profile: {}", suite.profile);
    println!("Number of test case groups: {}", suite.test_cases.len());
    println!("Number of entities defined: {}", suite.entities.len());

    // Run all tests
    println!("\n=== Running XML Conformance Tests ===\n");

    let mut total_pass = 0;
    let mut total_fail = 0;
    let mut total_tests = 0;

    for (i, test_cases) in suite.test_cases.iter().enumerate() {
        println!("Test Cases #{}", i + 1);
        if let Some(ref profile) = test_cases.profile {
            println!("  Profile: {}", profile);
        }

        let mut suite_pass = 0;
        let mut suite_fail = 0;

        for item in &test_cases.content {
            if let ContentItem::EntityRef(entity) = item {
                if let Some(entity_path) = suite.entities.get(entity) {
                    // Entity path is already correct relative to xmlconf/
                    let full_path = format!("xmlconf/{}", entity_path);

                    match parse_test_file(&full_path) {
                        Ok(tests) => {
                            // xml:base is the base directory for test file URIs
                            let base_dir = if let Some(ref xml_base) = test_cases.xml_base {
                                format!("xmlconf/{}", xml_base)
                            } else {
                                "xmlconf/".to_string()
                            };

                            for test in &tests {
                                // Skip tests that don't match the filter
                                if let Some(filter) = filter_type {
                                    if test.test_type != filter {
                                        continue;
                                    }
                                }

                                // Skip non-1.0 XML tests
                                if let Some(ref version) = test.version {
                                    if version != "1.0" {
                                        continue;
                                    }
                                }

                                let result = run_test(test, &base_dir);
                                match result {
                                    TestResult::Pass => {
                                        suite_pass += 1;
                                        total_pass += 1;
                                    }
                                    TestResult::Fail {
                                        ref expected,
                                        ref actual,
                                        ..
                                    } => {
                                        suite_fail += 1;
                                        total_fail += 1;
                                        println!("    FAIL: {} ({})", test.id, test.test_type);
                                        println!("      Expected: {}", expected);
                                        println!("      Actual: {}", actual);
                                    }
                                }
                                total_tests += 1;
                            }

                            let suite_total = suite_pass + suite_fail;
                            if suite_total > 0 {
                                println!(
                                    "  {} tests: {} passed, {} failed",
                                    suite_total, suite_pass, suite_fail
                                );
                            }
                        }
                        Err(e) => {
                            println!("  Error loading test file: {}", e);
                        }
                    }
                }
            }
        }
        println!();
    }

    println!("=== Summary ===");
    println!("Total tests: {}", total_tests);
    println!(
        "Passed: {} ({:.1}%)",
        total_pass,
        (total_pass as f64 / total_tests as f64) * 100.0
    );
    println!(
        "Failed: {} ({:.1}%)",
        total_fail,
        (total_fail as f64 / total_tests as f64) * 100.0
    );

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
    fn start_tag(&mut self, name: &str) -> Result<(), Error> {
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

    fn end_tag(&mut self, name: &str) -> Result<(), Error> {
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
    stream_xml_events::<Vec<&'_ str>>(&content, &mut visitor)?;
    Ok(visitor.into_tests())
}

fn run_test(test: &Test, base_path: &str) -> TestResult {
    let test_file_path = format!("{}{}", base_path, test.uri);

    // Try to parse the test file
    let buf = std::fs::read(&test_file_path).expect("Failed to read test file");

    // Check for LE and BE UTF-16 BOMs and convert to UTF-8
    let content = match if buf.starts_with(&[0xFE, 0xFF]) {
        String::from_utf16be(&buf).map_err(|e| e.to_string())
    } else if buf.starts_with(&[0xFF, 0xFE]) {
        String::from_utf16le(&buf).map_err(|e| e.to_string())
    } else {
        String::from_utf8(buf).map_err(|e| e.to_string())
    } {
        Ok(s) => s,
        Err(e) => {
            return TestResult::Fail {
                error_msg: Some(format!("Failed to decode test file: {}", e)),
                expected: "successful parse".to_string(),
                actual: "failed to decode test file".to_string(),
            };
        }
    };

    struct DummyVisitor;
    impl Visitor for DummyVisitor {}

    let parse_result = stream_xml_events::<Vec<&'_ str>>(&content, &mut DummyVisitor);

    let parse_succeeded = parse_result.is_ok();
    let should_succeed = test.test_type == "valid";

    if parse_succeeded == should_succeed {
        TestResult::Pass
    } else {
        TestResult::Fail {
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
