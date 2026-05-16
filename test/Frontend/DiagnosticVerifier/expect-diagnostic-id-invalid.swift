// RUN: not %target-swift-frontend -typecheck -verify %s 2>&1 | %FileCheck %s

// CHECK: error: unknown diagnostic identifier 'nonexistent_diag_id'
let unknown = // expected-error [nonexistent_diag_id]

// CHECK: error: expected diagnostic identifier inside '[...]'
let empty = // expected-error []

// CHECK: error: invalid diagnostic identifier '123bad'
let badShape = // expected-error [123bad]

// CHECK: error: invalid diagnostic identifier 'has space'
let hasSpace = // expected-error [has space]

// CHECK: error: didn't find ']' to match '[' in expected-warning/note/error line
let unclosed = // expected-error [expected_init_value

// CHECK: error: diagnostic ID matching with '[...]' cannot be combined with message text matching
let combined = // expected-error [expected_init_value] {{expected initial value after '='}}

// CHECK: error: expected-expansion does not support matching by diagnostic identifier
let expansion = // expected-expansion@:1 [some_id]

// CHECK: error: incorrect diagnostic identifier found; actual: [expected_init_value]
let wrongID = // expected-error [cannot_convert_initializer_value]

// CHECK: error: expected error [expected_init_value] not produced
// expected-error [expected_init_value]