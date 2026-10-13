// RUN: %target-typecheck-verify-swift -enable-objc-interop -disable-objc-attr-requires-foundation-module -Wwarning ImplicitForceUnwrap

@objc class D {
  @objc func m() -> Int { 1 }
  @objc var p: Int = 0
  @objc subscript(i: Int) -> Int { 1 }
}

func testDynamicLookup(o: AnyObject) {
  let _: Int = o.m() // expected-warning{{implicit force unwrap of dynamic lookup result of type '(() -> Int)?'}}{{19-19=!}}
  let _: Int = o.p // expected-warning{{implicit force unwrap of dynamic lookup result of type 'Int?'}}{{19-19=!}}
  let _: Int = o[0] // expected-warning{{implicit force unwrap of dynamic lookup result of type 'Int?'}}{{20-20=!}}

  // No implicit unwrap.
  let _: Int = o.m!()
  let _: Int? = o.p
  let _ = o.m?()
}
