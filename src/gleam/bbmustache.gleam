import gleam/string_builder.{StringBuilder}

// Template compilation
pub type Template

pub type CompileError {
  FileNotFound
  IncorrectSection(String)
  InvalidDelimiters
  UnclosedSection(String)
  UnclosedTag
  UnsupportedTag(String)
}

@external(erlang, "gleam_bbmustache_native", "try_catch")
fn try_catch(a: fn() -> a) -> Result(Template, CompileError)

@external(erlang, "bbmustache", "parse_binary")
fn throwing_compile(a: String) -> Template

@external(erlang, "bbmustache", "parse_file")
fn throwing_compile_file(a: String) -> Template

pub fn compile(tmpl: String) -> Result(Template, CompileError) {
  try_catch(fn() { throwing_compile(tmpl) })
}

pub fn compile_file(path: String) -> Result(Template, CompileError) {
  try_catch(fn() { throwing_compile_file(path) })
}

// Template rendering
pub type Argument

type KeyType {
  Binary
}

type RenderOption {
  KeyType(KeyType)
}

@external(erlang, "bbmustache", "compile")
fn do_render(
  a: Template,
  b: List(#(String, Argument)),
  c: List(RenderOption),
) -> String

pub fn render(template, injecting args: List(#(String, Argument))) -> String {
  do_render(template, args, [KeyType(Binary)])
}

@external(erlang, "gleam_bbmustache_native", "argument")
pub fn int(of of: Int) -> Argument

@external(erlang, "gleam_bbmustache_native", "argument")
pub fn string(of of: String) -> Argument

@external(erlang, "gleam_bbmustache_native", "argument")
pub fn builder(of of: StringBuilder) -> Argument

@external(erlang, "gleam_bbmustache_native", "argument")
pub fn object(of of: List(#(String, Argument))) -> Argument

@external(erlang, "gleam_bbmustache_native", "argument")
pub fn list(of of: List(Argument)) -> Argument
