// RUN: %target-typecheck-verify-swift

func `protocol`() {} // no-warning

`protocol`() // no-warning

class `Type` {} // expected-warning {{identifier 'Type' does not need to be escaped}}
// expected-warning@-1 {{identifier 'Type' does not need to be escaped}}

var `class` = `Type`.self // expected-warning {{identifier 'Type' does not need to be escaped}}

func foo() {} // no-warning

`foo`() // expected-warning {{identifier 'foo' does not need to be escaped}}

// Escaping suppresses identifier contextualization.
var get: (() -> ()) -> () = { $0() }

var applyGet: Int {
  `get` { } // no-warning
  return 0
}

enum `switch` {} // no-warning

typealias `Self` = Int // no-warning