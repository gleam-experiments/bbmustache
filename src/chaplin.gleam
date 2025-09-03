import gleam/string_tree.{type StringTree}

/// A template that be rendered with some arguments using the `render` function.
pub type Template

/// The error that can happen when compiling a template.
pub type CompileError {
  FileNotFound
  IncorrectSection(String)
  InvalidDelimiters
  UnclosedSection(String)
  UnclosedTag
  UnsupportedTag(String)
}

@external(erlang, "chaplin_ffi", "try_catch")
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

/// An argument that can be given to the template when rendering it.
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

@external(erlang, "chaplin_ffi", "argument")
pub fn int(of of: Int) -> Argument

@external(erlang, "chaplin_ffi", "argument")
pub fn float(of of: Int) -> Argument

@external(erlang, "chaplin_ffi", "argument")
pub fn string(of of: String) -> Argument

@external(erlang, "chaplin_ffi", "argument")
pub fn string_tree(of of: StringTree) -> Argument

@external(erlang, "chaplin_ffi", "argument")
pub fn object(of of: List(#(String, Argument))) -> Argument

@external(erlang, "chaplin_ffi", "argument")
pub fn list(of of: List(Argument)) -> Argument
