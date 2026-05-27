// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000
// REQUIRES: tools-release,no_asan

func rdar22249571() -> [UInt8] {
  return (0...10).map { _ in
    UInt8(Int(1) % Int(1))
  }
}
