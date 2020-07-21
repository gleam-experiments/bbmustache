import gleam/bbmustache.{string, strings}
import gleam/should

pub fn readme_test() {
  let Ok(template) = bbmustache.compile("Hello, {{name}}!")
  let rendered = bbmustache.render(template, [string("name", "World")])
  should.equal(rendered, "Hello, World!")
}

pub fn render_test() {
  let Ok(
    template,
  ) = bbmustache.compile(
    "# {{title}}

Here is a list of {{type}}:

{{#items}}
- {{.}}
{{/items}}
",
  )
  let rendered = bbmustache.render(
    template,
    [
      string("title", "Hello"),
      string("type", "items"),
      strings("items", ["one", "two", "three"]),
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
