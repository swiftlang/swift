// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asan

func compute(_ cont: () -> Void) {}

func test() {
  compute {
    let x = 42
    compute {
      print(x)
      let v: UInt64 = UInt64((24 / UInt32(1)) + UInt32(0) - UInt32(0) - 24 / 42 - 42)
      // expected-error@-1 {{the compiler is unable to type-check this expression in reasonable time; try breaking up the expression into distinct sub-expressions}}
      print(v)
    }
  }
}
