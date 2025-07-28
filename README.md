# `json-reader`

Reader macro for parsing JSON expressions within Common Lisp.

## Usage

To enable the reader macro with the `json-reader-enable` function and disable
it with the `json-reader-disable` function when you don't want to use it
anymore.

Enabling the reader mode when it is already enabled or disabling it when it is
not enabled will be a no-op.

```lisp
  (json-reader:json-reader-enable)
  (defun create-nice-json ()
     {"foo": 10})
  (json-reader:json-reader-disable)
```

You can check whether the macro is enabled through the variable
`*json-reader-enabled*`.

```lisp
  CL-USER> json-reader:*json-reader-enabled*
  NIL
  CL-USER> (json-reader:json-reader-enable)
  NIL
  CL-USER> json-reader:*json-reader-enabled*
  T
  CL-USER> (json-reader:json-reader-disable)
  NIL
  CL-USER> json-reader:*json-reader-enabled*
  NIL
```

## Syntax

`json-reader` tries to parse a JSON expression in them most natural and
intuitive way, but there are obvious limitations.

`json-reader` parses the JSON expressions in a way that is compatible with the
[`jzon`](https://github.com/Zulu-Inuoe/jzon) package.


```lisp
  CL-USER> true
  t
  CL-USER> false
  nil
  CL-USER> null ;; Returns the 'null symbol
  NULL
  CL-USER> [1, 2, 3]
  #(1 2 3)
  CL-USER> [1, 2, [3, 4, 5]]
  #(1 2 #(3 4 5))
  CL-USER> {"foo": "bar", "baz": 10}
  #<HASH-TABLE :TEST EQUAL :COUNT 2 {7007D40CD3}>
```

For integers, floats and strings, they are just returned as is, as they are
already perfectly compatible with `jzon`.

You can use Common Lisp expressions within the JSON expression with no problem,
as long as the evaluated value is a valid JSON value.

```lisp
CL-USER> [1, 2, (+ 1 2)]
#(1 2 3)
CL-USER> (defparameter *key* "foo")
*KEY*
CL-USER> {*key*: "bar"}
#<HASH-TABLE :TEST EQUAL :COUNT 1 {7005810833}>
CL-USER> (defparameter *key* 10)
*KEY*
CL-USER> {*key*: "bar"} ;; *key* is a number, which is an invalid JSON key
TODO: Error
```

When dealing with collections (lists or objects), `json-reader` will spread
lists.

```lisp
CL-USER> [1, 2, (loop for i from 10 to 13 collect i)]
#(1 2 3 10 11 12 13)
```

For objects, nested objects are merged together. This, combined with the list
spread, allow for powerful dynamic constructions.

```lisp
CL-USER> {"a": "b", {"c": "d", "e": "f"}}
;; Equivalent to {"a": "b", "c": "d", "e": "f"}
#<HASH-TABLE :TEST EQUAL :COUNT 3 {7008275FE3}>
CL-USER> {"foo": "bar", (loop for i from 10 to 13 collect {(write-to-string i): i})}
;; Equivalent to {"foo": "bar", "10": 10, "11": 11, "12": 12, "13": 13}
#<HASH-TABLE :TEST EQUAL :COUNT 5 {7008275FE3}>
```


## Installation

TODO: Quicklisp installation

## Author

+ Mateus Felipe Cordeiro Caetano Pinto (mateusfccp@gmail.com)

## Copyright

Copyright (c) 2025 Mateus Felipe Cordeiro Caetano Pinto (mateusfccp@gmail.com)

## License

Licensed under the MIT License.
