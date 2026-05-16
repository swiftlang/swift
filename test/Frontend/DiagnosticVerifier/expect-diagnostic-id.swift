// RUN: %target-typecheck-verify-swift

// Basic matching by diagnostic identifier
let basic = // expected-error [expected_init_value]

// Trailing directive on the same line as the diagnostic source
let inferenceFail: Double = "" // expected-error [cannot_convert_initializer_value]

// Line offsets
// expected-error@+1 [expected_init_value]
let nextLine =

let prevLine =
// expected-error@-1 [expected_init_value]

// Column constraint
let column = // expected-error@:13 [expected_init_value]

// Wildcard count
// expected-error * [expected_init_value]

// Location markers
let markerTest = // #marker1
// expected-error@#marker1 [expected_init_value]

// Continuation backslash
let a = , b = , c = // expected-error [expected_init_value] \
                    // expected-error [expected_init_value] \
                    // expected-error [expected_init_value]

// Count
let d = , e = , f = // expected-error 3 [expected_init_value]

// ID-mode and message-mode expectations on different lines
let bothModes1 = // expected-error [expected_init_value]
let bothModes2 = // expected-error {{expected initial value after '='}}

// Warning and note via ID on a single source construct
func returnsInt() -> Int {
  return
  42 // expected-warning [unindented_code_after_return] expected-note [indent_expression_to_silence]
}

// Mixed ID-mode warning and message-mode note on a single source construct
func returnsInt2() -> Int {
  return
  43 // expected-warning [unindented_code_after_return] expected-note {{indent the expression to silence this warning}}
}