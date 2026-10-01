// RUN: %target-typecheck-verify-swift -enable-experimental-feature CalledAttribute -verify-additional-prefix supported-
// RUN: %target-typecheck-verify-swift -verify-additional-prefix forbidden-

// REQUIRES: swift_feature_CalledAttribute

typealias FnType = @called(atMostOnce) () -> () // Ok
// expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}

func testInParameter(_: @called(atMostOnce) () -> ()) {} // Ok
// expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}

func testInParameterEscaping(_: @escaping @called(atMostOnce) () -> ()) {} // Ok
// expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}

func testInParameterAutoclosure(_: @autoclosure @called(atMostOnce) () -> ()) {} // Ok
// expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}

func testInParameterExplicitOwnership(_: borrowing @called(atMostOnce) () -> ()) {}
// expected-supported-error@-1 {{'@called(atMostOnce)' cannot be used together with 'borrowing'}}
// expected-forbidden-error@-2 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}

func testInParameterExplicitOwnership(_: inout @called(atMostOnce) () -> ()) {} // Ok
// expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}

func testInParameterConsuming(_: consuming @called(atMostOnce) () -> ()) {} // Ok
// expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}
// expected-forbidden-error@-2 {{'consuming' cannot be applied to nonescaping closure}}

func testInResultPosition(_: () -> @called(atMostOnce) () -> Void) {}
// expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}

func testInner() {
  let opt: (@called(atMostOnce) () -> ())? = nil
  // expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}
  _ = opt
}

struct Test : ~Copyable {
  let prop: @called(atMostOnce) () -> Void // Ok
  // expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}
}

func testWithConvention(_: @convention(block) @called(atMostOnce) () -> Void) {}
// expected-supported-error@-1 {{'@convention' attribute is not allowed on '@called' types}}
// expected-forbidden-error@-2 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}

func testInvalidResult() -> @called(atMostOnce) Int {
  // expected-error@-1 {{'@called' only applies to function types}}
}

func testClosure() {
  _ = { @called(atMostOnce) in 42 }
  // expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}
  _ = { @called(atMostOnce) (x: Int, y: String) -> Void in }
  // expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}

  @called(atMostOnce) func local() {}
  // expected-supported-error@-1 {{'@called(atMostOnce)' attribute cannot be applied to this declaration}}
  // expected-forbidden-error@-2 {{'called(atMostOnce)' attribute is only valid when experimental feature CalledAttribute is enabled}}

  @called(atMostOnce) let x: () -> Void = { }
  // expected-supported-error@-1 {{'@called(atMostOnce)' attribute cannot be applied to this declaration}}
  // expected-forbidden-error@-2 {{'called(atMostOnce)' attribute is only valid when experimental feature CalledAttribute is enabled}}
  _ = x
}

func testSendingCaptures() {
  class NS {
    func test() {
      _ = { @called(atMostOnce) [sending self] in
        // expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}
        // expected-forbidden-error@-2 {{expected 'weak', 'unowned', or no specifier in capture list}}
        _ = self
      }
    }
  }

  let ns = NS()
  _ = { @called(atMostOnce) [sending ns] in
    // expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}
    // expected-forbidden-error@-2 {{expected 'weak', 'unowned', or no specifier in capture list}}
    ns
  }
  _ = { @called(atMostOnce) [x = 42, sending ns = NS()] in
    // expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}
    // expected-forbidden-error@-2 {{expected 'weak', 'unowned', or no specifier in capture list}}
    _ = x
    _ = ns
  }
}
