// RUN: %target-typecheck-verify-swift -use-clang-function-types

let f: @convention(c, cType: "void (*)(void *)") (OpaquePointer?) -> () = { _ in }

let g: @convention(c, cType: "void (*)(int *)") (OpaquePointer?) -> () = f
// expected-error@-1{{cannot convert value of type '@convention(c) (Swift.OpaquePointer?) -> ()' to specified type '@convention(c) (Swift.OpaquePointer?) -> ()'}}
// expected-note@-2{{these values have identical Swift types, but different C types ('void (*)(void *)' vs. 'void (*)(int *)')}}
