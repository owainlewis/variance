# Variance

A library of useful statistics functions.

## Use

You can include this library in your projects using leiningen

```
[variance "0.1.0-SNAPSHOT"]
```

## Examples

```clojure

(def chi-square-test (fn []
  (let [sample [[48 58] [35 34.5] [15 7] [3 0.5]]]
    (chi-square sample))))

(standard-deviation [600 470 170 430 300])

(variance [600 470 170 430 300])

```

## License

Copyright © 2012 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
