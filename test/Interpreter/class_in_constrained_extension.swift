// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test
// XFAIL: swift_test_mode_optimize_none_with_opaque_values

class Foo {}

extension Array where Element == Foo {
  class Bar { var foo = Foo() }
  
  init(withAThing: String) {
    self = [Bar(), Bar(), Bar()].map { $0.foo }
  }
}

// CHECK: [main.Foo, main.Foo, main.Foo]
let foos = [Foo](withAThing: "Hi")
print(foos)
