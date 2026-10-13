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

// Overrides and witnesses that take a non-optional value where the base
// declaration has an implicitly unwrapped optional.
@objc class Base {
  @objc func take(_ x: D!, _ y: D!) {}
  @objc init(d: D!) {}
}

class Derived: Base {
  override func take(_ x: D, _ y: D!) {} // expected-warning{{implicit force unwrap of implicitly unwrapped optional parameter of type 'D?' in override of instance method 'take'}}{{28-28=!}}
  override init(d: D) { super.init(d: d) } // expected-warning{{implicit force unwrap of implicitly unwrapped optional parameter of type 'D?' in override of initializer 'init(d:)'}}{{21-21=!}}
}

@objc protocol P {
  func take(_ x: (any AnyObject & P)!)
  var prop: D! { get set }
}

class Conformer: P {
  func take(_ x: any AnyObject & P) {} // expected-warning{{implicit force unwrap of implicitly unwrapped optional parameter of type '(any AnyObject & P)?' in witness for instance method 'take'}}{{18-18=(}}{{35-35=)!}}
  var prop: D = D() // expected-warning{{implicit force unwrap of implicitly unwrapped optional value of type 'D?' in witness for property 'prop'}}{{14-14=!}}
}
