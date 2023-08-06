import gleeunit
import gleeunit/should
import gleam/bbmustache.{int, list, object, string}

pub fn main() {
  gleeunit.main()
}

pub fn readme_test() {
  let assert Ok(template) = bbmustache.compile("Hello, {{name}}!")
  let rendered = bbmustache.render(template, [#("name", string("World"))])
  should.equal(rendered, "Hello, World!")
}

pub fn render_test() {
  let assert Ok(template) =
    bbmustache.compile(
      "# {{title}}

Here is a list of {{type}}:

{{#items}}
- {{.}}
{{/items}}
",
    )
  let rendered =
    bbmustache.render(
      template,
      [
        #("title", string("Hello")),
        #("type", string("items")),
        #("items", list([string("one"), string("two"), string("three")])),
      ],
    )
  should.equal(
    rendered,
    "# Hello

Here is a list of items:

- one
- two
- three
",
  )
}

pub fn object_test() {
  let assert Ok(template) =
    bbmustache.compile(
      "
{{#items}}
- {{name}} ({{age}})
{{/items}}
",
    )
  let rendered =
    bbmustache.render(
      template,
      [
        #(
          "items",
          list([
            object([#("name", string("Samsara")), #("age", int(42))]),
            object([#("name", string("Keith")), #("age", int(38))]),
          ]),
        ),
      ],
    )

  should.equal(
    rendered,
    "
- Samsara (42)
- Keith (38)
",
  )
}

pub fn file_test() {
  let assert Ok(template) =
    bbmustache.compile_file("test/test_template.mustache")
  let rendered = bbmustache.render(template, [#("title", string("Hello!"))])
  should.equal(rendered, "<h1>Hello!</h1>\n")
}

pub fn file_not_found_test() {
  bbmustache.compile_file("nothing-here.jpeg")
  |> should.equal(Error(bbmustache.FileNotFound))
}

pub fn unclosed_tag_test() {
  bbmustache.compile("{{")
  |> should.equal(Error(bbmustache.UnclosedTag))
}

pub fn incorrect_section_test() {
  bbmustache.compile("{{/tag}}")
  |> should.equal(Error(bbmustache.IncorrectSection("tag")))
}

pub fn unclosed_section_test() {
  bbmustache.compile("{{#tag}}")
  |> should.equal(Error(bbmustache.UnclosedSection("/tag")))
}

pub fn invalid_delimiters_equals_test() {
  bbmustache.compile("{{=<= =>=}}<=n=>")
  |> should.equal(Error(bbmustache.InvalidDelimiters))
}

pub fn invalid_delimiters_whitespace_test() {
  bbmustache.compile("{{ = < < >> = }}< <n>>")
  |> should.equal(Error(bbmustache.InvalidDelimiters))
}

pub fn unsupported_tag_test() {
  bbmustache.compile("{{=<< >>}}")
  |> should.equal(Error(bbmustache.UnsupportedTag("=<< >>")))
}
