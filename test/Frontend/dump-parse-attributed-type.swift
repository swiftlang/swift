// RUN: %target-swift-frontend -dump-parse %s | %FileCheck %s

// https://github.com/swiftlang/swift/issues/91094
// The S-expression dumper used to drop the closing parenthesis of
// `type_attributed` nodes (`visitAttributedTypeRepr` never called
// `printFoot()`), so any dump containing an attributed type was
// unbalanced and consumers that match parentheses silently reparented
// every node printed after it. `type_error` and `using_decl` had the
// same omission.

let f: (@escaping (Int) -> Void) -> Void = { _ in }

// CHECK: (pattern_named "f"
// CHECK: (type_function
// CHECK-NEXT: (type_tuple
// CHECK-NEXT: (type_attributed attrs="@escaping "
// CHECK-NEXT: (type_function
// CHECK-NEXT: (type_tuple
// CHECK-NEXT: (type_unqualified_ident id="Int" unbound))
// The attributed node must close here: its own paren, after the inner
// function type's — four closes returns to the parameter tuple's level.
// CHECK-NEXT: (type_unqualified_ident id="Void" unbound)))){{$}}
// CHECK-NEXT: (type_unqualified_ident id="Void" unbound))
