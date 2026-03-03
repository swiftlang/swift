// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000
// REQUIRES: tools-release,no_asan

func compute(_ cont: () -> Void) {}

func test() {
  compute {
    let x = 42
    compute {
      print(x)
      let v: UInt64 = UInt64((24 / UInt32(1)) + UInt32(0) - UInt32(0) - 24 / 42 - 42)
      print(v)
    }
  }
}
