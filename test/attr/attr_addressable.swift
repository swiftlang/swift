// RUN: %target-swift-frontend -enable-experimental-feature AddressableParameters -typecheck %s -verify
// REQUIRES: swift_feature_AddressableParameters

func foo(_: @_addressable String) {}

func bar(_: (@_addressable String) -> Void) {}

func bas(_: @convention(c) (@_addressable String) -> Void) {} // expected-error{{_addressable is not allowed in C function conventions}} expected-error{{not representable in Objective-C}}

@_addressableSelf func doesNotHaveSelf() {} // expected-error{{cannot be applied to non-member declarations}}

struct Foo {
    @_addressableSelf func doesHaveSelf() {}
}
