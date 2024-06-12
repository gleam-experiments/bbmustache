# bbmustache

Gleam bindings to the [bbmustache][bbmustache] templating library.

[bbmustache]: https://github.com/soranoba/bbmustache

```gleam
import gleam/bbmustache.{string}
import gleeunit/should

pub fn main() {
  let assert Ok(template) = bbmustache.compile("Hello, {{name}}!")

  let rendered = bbmustache.render(template, [#("name", string("World"))])

  should.equal(rendered, "Hello, World!")
}
```

## Quick start

```sh
# Build the project
rebar3 compile

# Run the eunit tests
rebar3 eunit

# Run the Erlang REPL
rebar3 shell
```
