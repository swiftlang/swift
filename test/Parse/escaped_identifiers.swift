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

func `method with space and .:/`() {}

`method with space and .:/`()

class `Class with space and .:/` {}

var `var with space and .:/` = `Class with space and .:/`.self

enum `enum with space and .:/` {}

typealias `Typealias with space and .:/` = Int
