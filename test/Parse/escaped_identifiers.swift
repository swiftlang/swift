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
