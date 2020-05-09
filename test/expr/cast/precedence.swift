// RUN: %target-typecheck-verify-swift

let x: Bool = 3/4 as Float > 1/2 as Float

func testInIf(a: Any) {
  if a as? Float {} // expected-error {{cannot be used as a boolean}} {{6-6=((}} {{17-17=) != nil)}}
  let _: Float = a as? Float // expected-error {{value of optional type 'Float?' must be unwrapped}}
  // expected-note@-1{{coalesce}}
  // expected-note@-2{{force-unwrap}}
}
