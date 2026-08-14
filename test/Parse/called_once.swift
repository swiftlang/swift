// RUN: %target-typecheck-verify-swift -enable-experimental-feature CalledAttribute -verify-additional-prefix supported-
// RUN: %target-typecheck-verify-swift -verify-additional-prefix forbidden-

// REQUIRES: swift_feature_CalledAttribute

typealias FnType = @called(once) () -> () // Ok
// expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}

func testInParameter(_: @called(once) () -> ()) {} // Ok
// expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}

func testInParameterEscaping(_: @escaping @called(once) () -> ()) {} // Ok
// expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}

func testInParameterAutoclosure(_: @autoclosure @called(once) () -> ()) {} // Ok
// expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}

func testInParameterExplicitOwnership(_: borrowing @called(once) () -> ()) {}
// expected-supported-error@-1 {{'@called(once)' cannot be used together with 'borrowing'}}
// expected-forbidden-error@-2 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}

func testInParameterExplicitOwnership(_: inout @called(once) () -> ()) {} // Ok
// expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}

func testInParameterConsuming(_: consuming @called(once) () -> ()) {} // Ok
// expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}
// expected-forbidden-error@-2 {{'consuming' cannot be applied to nonescaping closure}}

func testInResultPosition(_: () -> @called(once) () -> Void) {}
// expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}

func testInner() {
  let opt: (@called(once) () -> ())? = nil
  // expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}
  _ = opt
}

struct Test : ~Copyable {
  let prop: @called(once) () -> Void // Ok
  // expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}
}

func testWithConvention(_: @convention(block) @called(once) () -> Void) {}
// expected-supported-error@-1 {{'@convention' attribute is not allowed on '@called' types}}
// expected-forbidden-error@-2 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}

func testInvalidResult() -> @called(once) Int {
  // expected-error@-1 {{'@called' only applies to function types}}
}

func testClosure() {
  _ = { @called(once) in 42 }
  // expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}
  _ = { @called(once) (x: Int, y: String) -> Void in }
  // expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}

  @called(once) func local() {}
  // expected-supported-error@-1 {{'@called(once)' attribute cannot be applied to this declaration}}
  // expected-forbidden-error@-2 {{'called(once)' attribute is only valid when experimental feature CalledAttribute is enabled}}

  @called(once) let x: () -> Void = { }
  // expected-supported-error@-1 {{'@called(once)' attribute cannot be applied to this declaration}}
  // expected-forbidden-error@-2 {{'called(once)' attribute is only valid when experimental feature CalledAttribute is enabled}}
  _ = x
}

func testSendingCaptures() {
  class NS {
    func test() {
      _ = { @called(once) [sending self] in
        // expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}
        // expected-forbidden-error@-2 {{expected 'weak', 'unowned', or no specifier in capture list}}
        _ = self
      }
    }
  }

  let ns = NS()
  _ = { @called(once) [sending ns] in
    // expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}
    // expected-forbidden-error@-2 {{expected 'weak', 'unowned', or no specifier in capture list}}
    ns
  }
  _ = { @called(once) [x = 42, sending ns = NS()] in
    // expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}
    // expected-forbidden-error@-2 {{expected 'weak', 'unowned', or no specifier in capture list}}
    _ = x
    _ = ns
  }
}
