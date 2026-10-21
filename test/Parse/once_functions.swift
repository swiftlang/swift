// RUN: %target-typecheck-verify-swift -enable-experimental-feature OnceFunctions -verify-additional-prefix supported-
// RUN: %target-typecheck-verify-swift -verify-additional-prefix forbidden-
// RUN: %if swift_feature_ParserASTGen %{ %target-typecheck-verify-swift -enable-experimental-feature OnceFunctions -enable-experimental-feature ParserASTGen -verify-additional-prefix supported- %}

// REQUIRES: swift_feature_OnceFunctions

typealias FnType = @callable(once) () -> () // Ok
// expected-forbidden-error@-1 {{'@callable(once)' attribute is only valid when experimental feature OnceFunctions is enabled}}

func testInParameter(_: @callable(once) () -> ()) {} // Ok
// expected-forbidden-error@-1 {{'@callable(once)' attribute is only valid when experimental feature OnceFunctions is enabled}}

func testInParameterEscaping(_: @escaping @callable(once) () -> ()) {} // Ok
// expected-forbidden-error@-1 {{'@callable(once)' attribute is only valid when experimental feature OnceFunctions is enabled}}

func testInParameterAutoclosure(_: @autoclosure @callable(once) () -> ()) {} // Ok
// expected-forbidden-error@-1 {{'@callable(once)' attribute is only valid when experimental feature OnceFunctions is enabled}}

func testInParameterExplicitOwnership(_: borrowing @callable(once) () -> ()) {}
// expected-supported-error@-1 {{'@callable(once)' cannot be used together with 'borrowing'}}
// expected-forbidden-error@-2 {{'@callable(once)' attribute is only valid when experimental feature OnceFunctions is enabled}}

func testInParameterExplicitOwnership(_: inout @callable(once) () -> ()) {} // Ok
// expected-forbidden-error@-1 {{'@callable(once)' attribute is only valid when experimental feature OnceFunctions is enabled}}

func testInParameterConsuming(_: consuming @callable(once) () -> ()) {} // Ok
// expected-forbidden-error@-1 {{'@callable(once)' attribute is only valid when experimental feature OnceFunctions is enabled}}
// expected-forbidden-error@-2 {{'consuming' cannot be applied to nonescaping closure}}

func testInResultPosition(_: () -> @callable(once) () -> Void) {}
// expected-forbidden-error@-1 {{'@callable(once)' attribute is only valid when experimental feature OnceFunctions is enabled}}

func testInner() {
  let opt: (@callable(once) () -> ())? = nil
  // expected-forbidden-error@-1 {{'@callable(once)' attribute is only valid when experimental feature OnceFunctions is enabled}}
  _ = opt
}

struct Test : ~Copyable {
  let prop: @callable(once) () -> Void // Ok
  // expected-forbidden-error@-1 {{'@callable(once)' attribute is only valid when experimental feature OnceFunctions is enabled}}
}

typealias ExactlyOnceFnType = @once () -> () // Ok
// expected-forbidden-error@-1 {{'@once' attribute is only valid when experimental feature OnceFunctions is enabled}}

func testExactlyOnceInParameter(_: @once () -> ()) {} // Ok
// expected-forbidden-error@-1 {{'@once' attribute is only valid when experimental feature OnceFunctions is enabled}}

func testExactlyOnceInParameterEscaping(_: @escaping @once () -> ()) {} // Ok
// expected-forbidden-error@-1 {{'@once' attribute is only valid when experimental feature OnceFunctions is enabled}}

func testExactlyOnceInParameterAutoclosure(_: @autoclosure @once () -> ()) {} // Ok
// expected-forbidden-error@-1 {{'@once' attribute is only valid when experimental feature OnceFunctions is enabled}}

func testExactlyOnceInParameterExplicitOwnership(_: borrowing @once () -> ()) {}
// expected-supported-error@-1 {{'@once' cannot be used together with 'borrowing'}}
// expected-forbidden-error@-2 {{'@once' attribute is only valid when experimental feature OnceFunctions is enabled}}

func testExactlyOnceInParameterExplicitOwnership(_: inout @once () -> ()) {} // Ok
// expected-forbidden-error@-1 {{'@once' attribute is only valid when experimental feature OnceFunctions is enabled}}

func testExactlyOnceInParameterConsuming(_: consuming @once () -> ()) {} // Ok
// expected-forbidden-error@-1 {{'@once' attribute is only valid when experimental feature OnceFunctions is enabled}}
// expected-forbidden-error@-2 {{'consuming' cannot be applied to nonescaping closure}}

func testExactlyOnceInResultPosition(_: () -> @once () -> Void) {}
// expected-forbidden-error@-1 {{'@once' attribute is only valid when experimental feature OnceFunctions is enabled}}

func testInvalidArgument(_: @callable(twice) () -> Void) {}
// expected-error@-1 {{expected 'once' in '@callable' attribute}}

func testMissingArgument(_: @callable() () -> Void) {}
// expected-error@-1 {{expected 'once' in '@callable' attribute}}

func testMissingParentheses(_: @callable Int) {}
// expected-error@-1 {{expected '(' in 'callable' attribute}}

func testBothAttributes(_: @once @callable(once) () -> Void) {}
// expected-error@-1 {{'@callable' contradicts previous '@once'}}
// expected-forbidden-error@-2 {{'@once' attribute is only valid when experimental feature OnceFunctions is enabled}}

func testOldSpelling(_: @called(atMostOnce) () -> Void) {}
// expected-error@-1 {{unknown attribute 'called'}}

func testInvalidResult() -> @callable(once) Int {
  // expected-error@-1 {{'@callable' only applies to function types}}
}

func testClosure() {
  _ = { @callable(once) in 42 }
  // expected-forbidden-error@-1 {{'@callable(once)' attribute is only valid when experimental feature OnceFunctions is enabled}}
  _ = { @callable(once) (x: Int, y: String) -> Void in }
  // expected-forbidden-error@-1 {{'@callable(once)' attribute is only valid when experimental feature OnceFunctions is enabled}}

  _ = { @once in 42 }
  // expected-forbidden-error@-1 {{'@once' attribute is only valid when experimental feature OnceFunctions is enabled}}
  _ = { @once (x: Int, y: String) -> Void in }
  // expected-forbidden-error@-1 {{'@once' attribute is only valid when experimental feature OnceFunctions is enabled}}

  _ = { @callable(twice) in }
  // expected-error@-1 {{expected 'once' in '@callable' attribute}}

  _ = { @callable in }
  // expected-error@-1 {{expected '(' in 'callable' attribute}}

  _ = { @once @callable(once) in }
  // expected-supported-error@-1 {{'@callable(once)' contradicts previous attribute '@once'}}
  // expected-forbidden-error@-2 {{'@once' attribute is only valid when experimental feature OnceFunctions is enabled}}
  // expected-forbidden-error@-3 {{'@callable(once)' attribute is only valid when experimental feature OnceFunctions is enabled}}

  @callable(once) func local() {}
  // expected-supported-error@-1 {{'@callable(once)' attribute cannot be applied to this declaration}}
  // expected-forbidden-error@-2 {{'@callable(once)' attribute is only valid when experimental feature OnceFunctions is enabled}}

  @callable(once) let x: () -> Void = { }
  // expected-supported-error@-1 {{'@callable(once)' attribute cannot be applied to this declaration}}
  // expected-forbidden-error@-2 {{'@callable(once)' attribute is only valid when experimental feature OnceFunctions is enabled}}
  _ = x

  @once func exactlyOnceLocal() {}
  // expected-supported-error@-1 {{'@once' attribute cannot be applied to this declaration}}
  // expected-forbidden-error@-2 {{'@once' attribute is only valid when experimental feature OnceFunctions is enabled}}
}

func testSendingCaptures() {
  class NS {
    func test() {
      _ = { @callable(once) [sending self] in
        // expected-forbidden-error@-1 {{'@callable(once)' attribute is only valid when experimental feature OnceFunctions is enabled}}
        // expected-forbidden-error@-2 {{expected 'weak', 'unowned', or no specifier in capture list}}
        _ = self
      }
    }
  }

  let ns = NS()
  _ = { @callable(once) [sending ns] in
    // expected-forbidden-error@-1 {{'@callable(once)' attribute is only valid when experimental feature OnceFunctions is enabled}}
    // expected-forbidden-error@-2 {{expected 'weak', 'unowned', or no specifier in capture list}}
    ns
  }
  _ = { @callable(once) [x = 42, sending ns = NS()] in
    // expected-forbidden-error@-1 {{'@callable(once)' attribute is only valid when experimental feature OnceFunctions is enabled}}
    // expected-forbidden-error@-2 {{expected 'weak', 'unowned', or no specifier in capture list}}
    _ = x
    _ = ns
  }
}
