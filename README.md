# bbmustache

Gleam bindings to the [bbmustache][bbmustache] templating library.

[bbmustache]: https://github.com/soranoba/bbmustache

```rust
import gleam/bbmustache.{string}
import gleam/expect

pub fn main() {
  assert Ok(template) = bbmustache.compile("Hello, {{name}}!")

  let rendered = bbmustache.render(template, [
    string("name", "World"),
  ])

  expect.equal(rendered, "Hello, World!")
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
