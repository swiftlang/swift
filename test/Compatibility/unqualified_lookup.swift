// RUN: %target-typecheck-verify-swift -swift-version 3

// Stupid Swift 3 unqualified lookup quirk

func f3(_ x: Int, _ y: Int, z: Int) { } // expected-note{{did you mean 'f3'?}}

struct S0 {
  func testS0() {
    _ = f3(_:y:z:) // expected-error{{use of unresolved identifier 'f3(_:y:z:)'}}
  }

  static func f3(_ x: Int, y: Int, z: Int) -> S0 { return S0() }
}

extension Float {
  func isClose(to: Float, epiValue: Float = 1e-5) -> Bool {
    // Float.abs() and Swift.abs() are both visible here, but
    // Swift 3 drops 'Float.abs()'.
    return abs(self - to) < epiValue
  }
}
