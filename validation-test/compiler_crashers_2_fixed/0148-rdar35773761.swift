// RUN: %target-typecheck-verify-swift %s

let b: () -> Void = withoutActuallyEscaping({ print("hello crash") }, do: { $0() })
// expected-error@-1 {{cannot convert value of type '()' to specified type '() -> Void'}}
