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
pub external type Arg

type KeyType {
  Binary
}

type RenderOption {
  KeyType(KeyType)
}

external fn do_render(Template, List(Arg), List(RenderOption)) -> String =
  "bbmustache" "compile"

external fn unsafe_make_arg(a) -> Arg =
  "gleam_stdlib" "identity"

pub fn render(template, injecting args: List(Arg)) -> String {
  do_render(template, args, [KeyType(Binary)])
}

// Creating args to pass to render
pub fn string(named key: String, of value: String) -> Arg {
  unsafe_make_arg(tuple(key, value))
}

pub fn builder(named key: String, of value: StringBuilder) -> Arg {
  unsafe_make_arg(tuple(key, value))
}

pub fn object(named key: String, of value: List(Arg)) -> Arg {
  unsafe_make_arg(tuple(key, value))
}

pub fn strings(named key: String, of value: List(String)) -> Arg {
  unsafe_make_arg(tuple(key, value))
}

pub fn builders(named key: String, of value: List(StringBuilder)) -> Arg {
  unsafe_make_arg(tuple(key, value))
}

pub fn objects(named key: String, of value: List(List(Arg))) -> Arg {
  unsafe_make_arg(tuple(key, value))
}
