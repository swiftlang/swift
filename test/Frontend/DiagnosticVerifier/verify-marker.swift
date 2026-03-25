// Tests for location markers in the Swift frontend's `-verify` mode.

// Positive tests: these should all pass verification.
// RUN: %target-typecheck-verify-swift

// Basic marker usage: marker on same line as diagnostic-producing code.
let x: Int = "hello" // #marker1
// expected-error@#marker1 {{cannot convert value of type 'String' to specified type 'Int'}}

// Marker referenced before definition (forward reference).
// expected-error@#marker2 {{cannot convert value of type 'String' to specified type 'Int'}}
let y: Int = "world" // #marker2

// Marker with column specifier and fix-it.
func fn() {}
fn(()) // #marker3
// expected-error@#marker3:4 {{argument passed to call that takes no arguments}} {{4-6=}}

// Multiple diagnostics referencing the same marker.
func bar(_ x: Int, _ y: Int) {}
bar("a", "b") // #marker4
// expected-error@#marker4 {{cannot convert value of type 'String' to expected argument type 'Int'}}
// expected-error@#marker4 {{cannot convert value of type 'String' to expected argument type 'Int'}}
