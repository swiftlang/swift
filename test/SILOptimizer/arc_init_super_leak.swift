// RUN: %target-run-simple-swift(-O) | %FileCheck %s

// REQUIRES: executable_test

// Test that delegating to super.init with an array argument inside a designated
// initializer after a conditional stored property initialization does not leak self.

class Base {
  init(ids: [Int]) {}
}

final class Sub: Base {
  let flag: Int?

  init(values: [Int]) {
    flag = Bool.random() ? 1 : nil
    super.init(ids: values.map { $0 })
  }

  deinit {
    print("deinit")
  }
}

@inline(never)
func make() {
  _ = Sub(values: [1])
}

// CHECK: deinit
// CHECK: done
make()
print("done")
