// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -O %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

var global: Any = 1
func withValue(action: (Any) -> Void) {
  action(global)
}

@inline(never)
public func test_global_side_effect() {
  withValue { value in
      print(value)
      global = 24
      print(value)
  }
}

// CHECK:      1
// CHECK-NEXT: 1
test_global_side_effect()
