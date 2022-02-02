import gleam/string_builder.{StringBuilder}

// Template compilation
pub external type Template

pub external type CompileError

external fn try_catch(fn() -> a) -> Result(Template, CompileError) =
  "gleam_bbmustache_native" "try_catch"

external fn throwing_compile(String) -> Template =
  "bbmustache" "parse_binary"

external fn throwing_compile_file(String) -> Template =
  "bbmustache" "parse_file"

pub fn compile(tmpl: String) -> Result(Template, CompileError) {
  try_catch(fn() { throwing_compile(tmpl) })
}

pub fn compile_file(path: String) -> Result(Template, CompileError) {
  try_catch(fn() { throwing_compile_file(path) })
}

// Template rendering
pub external type Argument

type KeyType {
  Binary
}

type RenderOption {
  KeyType(KeyType)
}

external fn do_render(
  Template,
  List(#(String, Argument)),
  List(RenderOption),
) -> String =
  "bbmustache" "compile"

pub fn render(template, injecting args: List(#(String, Argument))) -> String {
  do_render(template, args, [KeyType(Binary)])
}

pub external fn int(of: Int) -> Argument =
  "gleam_bbmustache_native" "argument"

pub external fn string(of: String) -> Argument =
  "gleam_bbmustache_native" "argument"

pub external fn builder(of: StringBuilder) -> Argument =
  "gleam_bbmustache_native" "argument"

pub external fn object(of: List(#(String, Argument))) -> Argument =
  "gleam_bbmustache_native" "argument"

pub external fn list(of: List(Argument)) -> Argument =
  "gleam_bbmustache_native" "argument"
