use core::fmt::Write as FmtWrite;

#[cfg(feature = "std")]
use std::collections::HashMap;
#[cfg(feature = "std")]
use std::path::PathBuf;

#[cfg(not(feature = "std"))]
use heapless::FnvIndexMap as HashMap;

use crate::{EntityDeclType, Error, Visitor};

#[derive(Debug, Clone)]
pub enum EntityValue {
    Internal(String),
    System(String),
}

#[cfg(not(feature = "std"))]
impl EntityValue {
    fn try_from_internal(s: &str) -> Result<Self, ()> {
        Ok(EntityValue::Internal(s.to_string()))
    }

    fn try_from_system(s: &str) -> Result<Self, ()> {
        Ok(EntityValue::System(s.to_string()))
    }
}

pub struct XmlWriter<W> {
    writer: W,
    #[cfg(feature = "std")]
    entities: HashMap<String, EntityValue>,
    #[cfg(not(feature = "std"))]
    entities: HashMap<heapless::String<64>, EntityValue, 32>,
    #[cfg(feature = "std")]
    current_entity_decl: String,
    #[cfg(not(feature = "std"))]
    current_entity_decl: heapless::String<128>,
    entity_decl_type: Option<EntityDeclType>,
    entity_decl_piece_count: usize,
    #[cfg(feature = "std")]
    source_file_path: Option<PathBuf>,
    #[cfg(not(feature = "std"))]
    source_file_path: Option<heapless::String<256>>,
}

impl XmlWriter<String> {
    pub fn new_string() -> Self {
        Self {
            writer: String::new(),
            entities: HashMap::new(),
            current_entity_decl: String::new(),
            entity_decl_type: None,
            entity_decl_piece_count: 0,
            source_file_path: None,
        }
    }

    #[cfg(feature = "std")]
    pub fn with_source_path(source_path: impl Into<PathBuf>) -> Self {
        Self {
            writer: String::new(),
            entities: HashMap::new(),
            current_entity_decl: String::new(),
            entity_decl_type: None,
            entity_decl_piece_count: 0,
            source_file_path: Some(source_path.into()),
        }
    }
}

#[cfg(feature = "std")]
impl XmlWriter<Vec<u8>> {
    pub fn new_vec() -> Self {
        Self {
            writer: Vec::new(),
            entities: HashMap::new(),
            current_entity_decl: String::new(),
            entity_decl_type: None,
            entity_decl_piece_count: 0,
            source_file_path: None,
        }
    }
}

#[cfg(feature = "std")]
impl XmlWriter<std::fs::File> {
    pub fn to_file(path: &str) -> Result<Self, std::io::Error> {
        Ok(Self {
            writer: std::fs::File::create(path)?,
            entities: HashMap::new(),
            current_entity_decl: String::new(),
            entity_decl_type: None,
            entity_decl_piece_count: 0,
            source_file_path: None,
        })
    }
}

impl<W> XmlWriter<W> {
    pub fn new(writer: W) -> Self {
        Self {
            writer,
            #[cfg(feature = "std")]
            entities: HashMap::new(),
            #[cfg(not(feature = "std"))]
            entities: HashMap::new(),
            #[cfg(feature = "std")]
            current_entity_decl: String::new(),
            #[cfg(not(feature = "std"))]
            current_entity_decl: heapless::String::new(),
            entity_decl_type: None,
            entity_decl_piece_count: 0,
            #[cfg(feature = "std")]
            source_file_path: None,
            #[cfg(not(feature = "std"))]
            source_file_path: None,
        }
    }

    pub fn finish(self) -> W {
        self.writer
    }
}

fn escape_text(text: &str) -> String {
    let mut result = String::with_capacity(text.len());
    for ch in text.chars() {
        match ch {
            '&' => result.push_str("&amp;"),
            '<' => result.push_str("&lt;"),
            '>' => result.push_str("&gt;"),
            _ => result.push(ch),
        }
    }
    result
}

fn escape_attr(text: &str) -> String {
    let mut result = String::with_capacity(text.len());
    for ch in text.chars() {
        match ch {
            '&' => result.push_str("&amp;"),
            '<' => result.push_str("&lt;"),
            '>' => result.push_str("&gt;"),
            '"' => result.push_str("&quot;"),
            _ => result.push(ch),
        }
    }
    result
}

#[cfg(feature = "std")]
fn decode_entity(entity: &str, entities: &HashMap<String, EntityValue>) -> String {
    match entity {
        "amp" => "&".to_string(),
        "lt" => "<".to_string(),
        "gt" => ">".to_string(),
        "quot" => "\"".to_string(),
        "apos" => "'".to_string(),
        _ => {
            if let Some(entity_value) = entities.get(entity) {
                if let EntityValue::Internal(value) = entity_value {
                    return value.clone();
                }
                // System entities are handled separately in entity_ref
            }

            if entity.starts_with('#') {
                let num_str = &entity[1..];
                if num_str.starts_with('x') {
                    if let Ok(code) = u32::from_str_radix(&num_str[1..], 16) {
                        if let Some(ch) = char::from_u32(code) {
                            return ch.to_string();
                        }
                    }
                } else {
                    if let Ok(code) = num_str.parse::<u32>() {
                        if let Some(ch) = char::from_u32(code) {
                            return ch.to_string();
                        }
                    }
                }
            }
            format!("&{};", entity)
        }
    }
}

#[cfg(not(feature = "std"))]
fn decode_entity(
    entity: &str,
    _entities: &HashMap<heapless::String<64>, EntityValue, 32>,
) -> String {
    match entity {
        "amp" => "&".to_string(),
        "lt" => "<".to_string(),
        "gt" => ">".to_string(),
        "quot" => "\"".to_string(),
        "apos" => "'".to_string(),
        _ => {
            if entity.starts_with('#') {
                let num_str = &entity[1..];
                if num_str.starts_with('x') {
                    if let Ok(code) = u32::from_str_radix(&num_str[1..], 16) {
                        if let Some(ch) = char::from_u32(code) {
                            return ch.to_string();
                        }
                    }
                } else {
                    if let Ok(code) = num_str.parse::<u32>() {
                        if let Some(ch) = char::from_u32(code) {
                            return ch.to_string();
                        }
                    }
                }
            }
            format!("&{};", entity)
        }
    }
}

impl<W: FmtWrite> Visitor for XmlWriter<W> {
    fn open_tag(&mut self, name: &str) -> Result<(), Error> {
        write!(self.writer, "<{}", name).map_err(|_| Error {
            span: crate::Span {
                start: crate::Position {
                    index: 0,
                    line: 0,
                    col: 0,
                },
                end: crate::Position {
                    index: 0,
                    line: 0,
                    col: 0,
                },
            },
            kind: crate::ErrorKind::UnexpectedToken,
        })?;
        Ok(())
    }

    fn close_open_tag(&mut self, _name: &str, self_closing: bool) -> Result<(), Error> {
        if self_closing {
            write!(self.writer, "/>").map_err(|_| Error {
                span: crate::Span {
                    start: crate::Position {
                        index: 0,
                        line: 0,
                        col: 0,
                    },
                    end: crate::Position {
                        index: 0,
                        line: 0,
                        col: 0,
                    },
                },
                kind: crate::ErrorKind::UnexpectedToken,
            })?;
        } else {
            write!(self.writer, ">").map_err(|_| Error {
                span: crate::Span {
                    start: crate::Position {
                        index: 0,
                        line: 0,
                        col: 0,
                    },
                    end: crate::Position {
                        index: 0,
                        line: 0,
                        col: 0,
                    },
                },
                kind: crate::ErrorKind::UnexpectedToken,
            })?;
        }
        Ok(())
    }

    fn start_close_tag(&mut self, name: &str) -> Result<(), Error> {
        write!(self.writer, "</{}", name).map_err(|_| Error {
            span: crate::Span {
                start: crate::Position {
                    index: 0,
                    line: 0,
                    col: 0,
                },
                end: crate::Position {
                    index: 0,
                    line: 0,
                    col: 0,
                },
            },
            kind: crate::ErrorKind::UnexpectedToken,
        })?;
        Ok(())
    }

    fn end_close_tag(&mut self, _name: &str) -> Result<(), Error> {
        write!(self.writer, ">").map_err(|_| Error {
            span: crate::Span {
                start: crate::Position {
                    index: 0,
                    line: 0,
                    col: 0,
                },
                end: crate::Position {
                    index: 0,
                    line: 0,
                    col: 0,
                },
            },
            kind: crate::ErrorKind::UnexpectedToken,
        })?;
        Ok(())
    }

    fn attribute(&mut self, key: &str, value: &str) -> Result<(), Error> {
        let escaped = escape_attr(value);
        write!(self.writer, " {}=\"{}\"", key, escaped).map_err(|_| Error {
            span: crate::Span {
                start: crate::Position {
                    index: 0,
                    line: 0,
                    col: 0,
                },
                end: crate::Position {
                    index: 0,
                    line: 0,
                    col: 0,
                },
            },
            kind: crate::ErrorKind::UnexpectedToken,
        })?;
        Ok(())
    }

    fn text(&mut self, text: &str) -> Result<(), Error> {
        let escaped = escape_text(text);
        write!(self.writer, "{}", escaped).map_err(|_| Error {
            span: crate::Span {
                start: crate::Position {
                    index: 0,
                    line: 0,
                    col: 0,
                },
                end: crate::Position {
                    index: 0,
                    line: 0,
                    col: 0,
                },
            },
            kind: crate::ErrorKind::UnexpectedToken,
        })?;
        Ok(())
    }

    fn start_comment(&mut self) -> Result<(), Error> {
        write!(self.writer, "<!--").map_err(|_| Error {
            span: crate::Span {
                start: crate::Position {
                    index: 0,
                    line: 0,
                    col: 0,
                },
                end: crate::Position {
                    index: 0,
                    line: 0,
                    col: 0,
                },
            },
            kind: crate::ErrorKind::UnexpectedToken,
        })?;
        Ok(())
    }

    fn comment(&mut self, text: &str) -> Result<(), Error> {
        write!(self.writer, "{}", text).map_err(|_| Error {
            span: crate::Span {
                start: crate::Position {
                    index: 0,
                    line: 0,
                    col: 0,
                },
                end: crate::Position {
                    index: 0,
                    line: 0,
                    col: 0,
                },
            },
            kind: crate::ErrorKind::UnexpectedToken,
        })?;
        Ok(())
    }

    fn end_comment(&mut self) -> Result<(), Error> {
        write!(self.writer, "-->").map_err(|_| Error {
            span: crate::Span {
                start: crate::Position {
                    index: 0,
                    line: 0,
                    col: 0,
                },
                end: crate::Position {
                    index: 0,
                    line: 0,
                    col: 0,
                },
            },
            kind: crate::ErrorKind::UnexpectedToken,
        })?;
        Ok(())
    }

    fn entity_ref(&mut self, entity: &str) -> Result<(), Error> {
        // Check if this is a SYSTEM entity
        #[cfg(feature = "std")]
        if let Some(EntityValue::System(system_path)) = self.entities.get(entity) {
            // Load and insert the file contents
            if let Some(ref source_path) = self.source_file_path {
                let source_dir = source_path.parent().unwrap_or(std::path::Path::new("."));
                let full_path = source_dir.join(system_path);

                match std::fs::read_to_string(&full_path) {
                    Ok(contents) => {
                        // Write the contents directly without escaping (it's already XML)
                        write!(self.writer, "{}", contents).map_err(|_| Error {
                            span: crate::Span {
                                start: crate::Position {
                                    index: 0,
                                    line: 0,
                                    col: 0,
                                },
                                end: crate::Position {
                                    index: 0,
                                    line: 0,
                                    col: 0,
                                },
                            },
                            kind: crate::ErrorKind::UnexpectedToken,
                        })?;
                        return Ok(());
                    }
                    Err(_) => {
                        // If file can't be loaded, fall through to write the entity ref as-is
                    }
                }
            }
        }

        // Handle internal entities and built-in entities
        let decoded = decode_entity(entity, &self.entities);
        let escaped = escape_text(&decoded);
        write!(self.writer, "{}", escaped).map_err(|_| Error {
            span: crate::Span {
                start: crate::Position {
                    index: 0,
                    line: 0,
                    col: 0,
                },
                end: crate::Position {
                    index: 0,
                    line: 0,
                    col: 0,
                },
            },
            kind: crate::ErrorKind::UnexpectedToken,
        })?;
        Ok(())
    }

    fn start_processing_instruction(&mut self) -> Result<(), Error> {
        write!(self.writer, "<?").map_err(|_| Error {
            span: crate::Span {
                start: crate::Position {
                    index: 0,
                    line: 0,
                    col: 0,
                },
                end: crate::Position {
                    index: 0,
                    line: 0,
                    col: 0,
                },
            },
            kind: crate::ErrorKind::UnexpectedToken,
        })?;
        Ok(())
    }

    fn processing_instruction(&mut self, content: &str) -> Result<(), Error> {
        write!(self.writer, "{}", content).map_err(|_| Error {
            span: crate::Span {
                start: crate::Position {
                    index: 0,
                    line: 0,
                    col: 0,
                },
                end: crate::Position {
                    index: 0,
                    line: 0,
                    col: 0,
                },
            },
            kind: crate::ErrorKind::UnexpectedToken,
        })?;
        Ok(())
    }

    fn end_processing_instruction(&mut self) -> Result<(), Error> {
        write!(self.writer, "?>").map_err(|_| Error {
            span: crate::Span {
                start: crate::Position {
                    index: 0,
                    line: 0,
                    col: 0,
                },
                end: crate::Position {
                    index: 0,
                    line: 0,
                    col: 0,
                },
            },
            kind: crate::ErrorKind::UnexpectedToken,
        })?;
        Ok(())
    }

    fn start_doctype(&mut self) -> Result<(), Error> {
        Ok(())
    }

    fn doctype(&mut self, _content: &str) -> Result<(), Error> {
        Ok(())
    }

    fn end_doctype(&mut self) -> Result<(), Error> {
        Ok(())
    }

    fn start_entity_declaration(&mut self, decl_type: EntityDeclType) -> Result<(), Error> {
        self.entity_decl_type = Some(decl_type);
        self.entity_decl_piece_count = 0;
        self.current_entity_decl.clear();
        Ok(())
    }

    fn entity_declaration(&mut self, content: &str) -> Result<(), Error> {
        // Track entity declaration content for parsing later
        #[cfg(feature = "std")]
        {
            if !self.current_entity_decl.is_empty() {
                self.current_entity_decl.push(' ');
            }
            self.current_entity_decl.push_str(content);
        }
        #[cfg(not(feature = "std"))]
        {
            use core::fmt::Write;
            if !self.current_entity_decl.is_empty() {
                let _ = write!(self.current_entity_decl, " ");
            }
            let _ = write!(self.current_entity_decl, "{}", content);
        }

        self.entity_decl_piece_count += 1;
        Ok(())
    }

    fn end_entity_declaration(&mut self) -> Result<(), Error> {
        let decl = &self.current_entity_decl;
        let parts: Vec<&str> = decl.split_whitespace().collect();

        if parts.len() >= 2 {
            let name = parts[0];

            let entity_value = if parts[1] == "SYSTEM" && parts.len() >= 3 {
                // SYSTEM entity: parts[2] is the file path
                EntityValue::System(parts[2].to_string())
            } else {
                // Internal entity: parts[1] is the value
                EntityValue::Internal(parts[1].to_string())
            };

            #[cfg(feature = "std")]
            self.entities.insert(name.to_string(), entity_value);
            #[cfg(not(feature = "std"))]
            {
                if let Ok(k) = heapless::String::try_from(name) {
                    let _ = self.entities.insert(k, entity_value);
                }
            }
        }

        Ok(())
    }

    fn start_element_declaration(&mut self) -> Result<(), Error> {
        Ok(())
    }

    fn element_declaration(&mut self, _content: &str) -> Result<(), Error> {
        Ok(())
    }

    fn end_element_declaration(&mut self) -> Result<(), Error> {
        Ok(())
    }
}
