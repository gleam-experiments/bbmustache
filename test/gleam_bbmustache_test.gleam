import gleeunit
import gleeunit/should
import gleam/bbmustache.{int, list, object, string}

pub fn main() {
  gleeunit.main()
}

pub fn readme_test() {
  assert Ok(template) = bbmustache.compile("Hello, {{name}}!")
  let rendered = bbmustache.render(template, [#("name", string("World"))])
  should.equal(rendered, "Hello, World!")
}

pub fn render_test() {
  assert Ok(template) =
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
  assert Ok(template) =
    bbmustache.compile("
{{#items}}
- {{name}} ({{age}})
{{/items}}
")
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

  should.equal(rendered, "
- Samsara (42)
- Keith (38)
")
}

pub fn file_test() {
  assert Ok(template) = bbmustache.compile_file("test/test_template.mustache")
  let rendered = bbmustache.render(template, [#("title", string("Hello!"))])
  should.equal(rendered, "<h1>Hello!</h1>\n")
}
