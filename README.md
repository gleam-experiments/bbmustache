# bbmustache

[Mustache][mustache] text templates using Erlang's [bbmustache][bbmustache] package.

[mustache]: https://mustache.github.io/
[bbmustache]: https://github.com/soranoba/bbmustache

```gleam
import chaplin

pub fn main() {
  let assert Ok(template) = chaplin.compile("Hello, {{name}}!")

  let rendered = chaplin.render(template, [
    chaplin.string("name", "World"),
  ])

  assert rendered == "Hello, World!"
}
```

Further documentation can be found at <https://hexdocs.pm/chaplin>.
