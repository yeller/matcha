# matcha

A clojure library for composable test assertions with readable error messages

## Installation

Grab from clojars (TODO)

## Getting Started

`matcha` is primarily designed for use with `clojure.test`, but it's easy to extend to other test frameworks as well.

Matchers are composable values - you can make complex, interesting assertions
about expected values and get great error messages out of them when the
assertion fails.

Here's what a basic assertion looks like with `clojure.test`:

```clojure
(require '[matcha :as m])
(deftest a-test
  (m/is (m/= 1) 2))
```

There are a *lot* of matchers included in this project. If you're working with
a core clojure or java data structure, chances are `matcha` supports working
with it. See the api docs for more: TODO

## License

Copyright © 2014 Tom Crayford and Reid Draper

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
