// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -c -parse-sil %S/Inputs/unconditional_checked_cast_addr.sil -o %t/cast.o
// RUN: %target-build-swift %s %t/cast.o -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s --check-prefix=SUCCESS
// RUN: %target-not-crash %target-run %t/main fail 2>&1 | %FileCheck %s --check-prefix=FAILURE
// REQUIRES: executable_test

@_silgen_name("copyCast")
func copyCast<T, U>(_ source: borrowing T, _: U.Type) -> U

var destructions = 0
class Object {
  deinit { destructions += 1 }
}

@inline(never)
func checkLifetime() {
  let source: Any = Object()
  let result = copyCast(source, Object.self)
  precondition((source as! Object) === result)
  precondition(destructions == 0)
  withExtendedLifetime((source, result)) {}
}

checkLifetime()
precondition(destructions == 1)
// SUCCESS: source and result released exactly once
print("source and result released exactly once")

if CommandLine.arguments.count > 1 {
  let source: Any = 42
  let result = copyCast(source, String.self)
  print("unexpected success: \(result)")
}
// FAILURE: Could not cast value of type 'Swift.Int'
// FAILURE-SAME: to 'Swift.String'
// FAILURE-NOT: unexpected success
