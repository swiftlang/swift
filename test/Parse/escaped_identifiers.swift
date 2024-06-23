// RUN: %target-typecheck-verify-swift

func `protocol`() {}

`protocol`()

class `Type` {}

var `class` = `Type`.self

func foo() {}

`foo`()

// Escaping suppresses identifier contextualization.
var get: (() -> ()) -> () = { $0() }

var applyGet: Int {
  `get` { }
  return 0
}

enum `switch` {}

typealias `Self` = Int

// Identifiers that must be escaped
func `method with space and .:/`() {}
`method with space and .:/`()

class `Class with space and .:/` {}
var `var with space and .:/` = `Class with space and .:/`.self

enum `enum with space and .:/` {
  case `space cases`
  case `case with payload`(`some label`: `Class with space and .:/`)
}

typealias `Typealias with space and .:/` = Int
func `+ start with operator`() {}

struct `Escaped Type` {}
func `escaped function`(`escaped label` `escaped arg`: `Escaped Type`) {}
`escaped function`(`escaped label`: `Escaped Type`())
let `escaped reference` = `escaped function`(`escaped label`:)
`escaped reference`(`Escaped Type`())

let `@atSign` = 0
let `#octothorpe` = 0

// Escaped identifiers cannot contain *only* operator characters.
let `+` = 0  // expected-error{{expected pattern}}
let `^*^` = 0  // expected-error{{expected pattern}}
let `.` = 0  // expected-error{{expected pattern}}
let `?` = 0  // expected-error{{expected pattern}}
func `+`(lhs: Int, rhs: Int) -> Int  // expected-error{{expected identifier in function declaration}}

@propertyWrapper
struct `@PoorlyNamedWrapper`<`The Value`> {
  var wrappedValue: `The Value`
}
struct WithWrappedProperty {
  @`@PoorlyNamedWrapper` var x: Int
}
