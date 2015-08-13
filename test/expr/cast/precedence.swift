// RUN: %target-parse-verify-swift

let x: Bool = 3/4 as Float > 1/2 as Float

func testInIf(a: Any) {
  if a as? Float {} // expected-error {{cannot be used as a boolean}} {{6-6=((}} {{17-17=) != nil)}}
}
