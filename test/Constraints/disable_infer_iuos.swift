// RUN: %target-parse-verify-swift -disable-infer-iuos

func test_bind_iuo() {
  // Unannotated variable bound to IUO should produce optional.
  func f() -> Int! { return 5 }
  let x = f()
  let _ = x + 5 // expected-error{{value of optional type 'Int?' not unwrapped; did you mean to use '!' or '?'?}}
  let _ = f() + 5
}
