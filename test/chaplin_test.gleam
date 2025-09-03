import chaplin.{int, list, object, string}
import gleeunit

pub fn main() {
  gleeunit.main()
}

pub fn readme_test() {
  let assert Ok(template) = chaplin.compile("Hello, {{name}}!")
  let rendered = chaplin.render(template, [#("name", string("World"))])
  assert rendered == "Hello, World!"
}

pub fn render_test() {
  let assert Ok(template) =
    chaplin.compile(
      "# {{title}}

Here is a list of {{type}}:

{{#items}}
- {{.}}
{{/items}}
",
    )
  let rendered =
    chaplin.render(template, [
      #("title", string("Hello")),
      #("type", string("items")),
      #("items", list([string("one"), string("two"), string("three")])),
    ])
  assert rendered == "# Hello

Here is a list of items:

- one
- two
- three
"
}

pub fn object_test() {
  let assert Ok(template) =
    chaplin.compile(
      "
{{#items}}
- {{name}} ({{age}})
{{/items}}
",
    )
  let rendered =
    chaplin.render(template, [
      #(
        "items",
        list([
          object([#("name", string("Samsara")), #("age", int(42))]),
          object([#("name", string("Keith")), #("age", int(38))]),
        ]),
      ),
    ])

  assert rendered == "
- Samsara (42)
- Keith (38)
"
}

pub fn file_test() {
  let assert Ok(template) = chaplin.compile_file("test/test_template.mustache")
  let rendered = chaplin.render(template, [#("title", string("Hello!"))])
  assert rendered == "<h1>Hello!</h1>\n"
}

pub fn file_not_found_test() {
  assert chaplin.compile_file("nothing-here.jpeg")
    == Error(chaplin.FileNotFound)
}

pub fn unclosed_tag_test() {
  assert chaplin.compile("{{") == Error(chaplin.UnclosedTag)
}

pub fn incorrect_section_test() {
  assert chaplin.compile("{{/tag}}") == Error(chaplin.IncorrectSection("tag"))
}

pub fn unclosed_section_test() {
  assert chaplin.compile("{{#tag}}") == Error(chaplin.UnclosedSection("/tag"))
}

pub fn invalid_delimiters_equals_test() {
  assert chaplin.compile("{{=<= =>=}}<=n=>") == Error(chaplin.InvalidDelimiters)
}

pub fn invalid_delimiters_whitespace_test() {
  assert chaplin.compile("{{ = < < >> = }}< <n>>")
    == Error(chaplin.InvalidDelimiters)
}

pub fn unsupported_tag_test() {
  assert chaplin.compile("{{=<< >>}}")
    == Error(chaplin.UnsupportedTag("=<< >>"))
}
