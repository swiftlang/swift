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

func testInParameterExplicitOwnership(_: borrowing @called(once) () -> ()) {} // Ok
// expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}

func testInResultPosition(_: () -> @called(once) () -> Void) {}
// expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}

func testInner() {
  let array = [@called(once) () -> ()]() // Ok
  // expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}
  _ = array
}

struct Test {
  let prop: @called(once) () -> Void // Ok
  // expected-forbidden-error@-1 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}
}

func testWithConvention(_: @convention(block) @called(once) () -> Void) {}
// expected-supported-error@-1 {{'@convention' attribute is not allowed on '@called' types}}
// expected-forbidden-error@-2 {{'@called' attribute is only valid when experimental feature CalledAttribute is enabled}}

func testInvalidResult() -> @called(once) Int {
  // expected-error@-1 {{'@called' only applies to function types}}
}

// TODO: add closure support `_ = { @called(once) in }`
