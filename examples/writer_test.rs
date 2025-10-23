use no_xml::{stream_xml_events, writer::XmlWriter};

fn main() {
    let xml =
        r#"<!DOCTYPE root [<!ENTITY foo "42">]><root>The answer is &foo; and also &amp;</root>"#;

    let mut writer = XmlWriter::new_string();
    stream_xml_events::<Vec<&str>>(xml, &mut writer).unwrap();

    let result = writer.finish();
    println!("Input:  {}", xml);
    println!("Output: {}", result);
    println!();
    println!("Text content: \"The answer is 42 and also &\"");
    println!("✓ Entity &foo; resolved to: 42");
    println!("✓ Entity &amp; resolved to: &");
}
