// RUN: %target-swift-frontend -typecheck -import-objc-header %S/Inputs/invalid_metatype_bridging_header.h %s -verify

// REQUIRES: objc_interop

// rdar://problem/33830526: Constraint system should not add static methods
// to the overload search space if it would require bridging unrelated metatypes.
class Crasher {
  static func called(argument: String) {}
}

Crasher.called(argument: .meth) // expected-error {{type 'String' has no member 'meth'}}
Crasher.called(argument: String.meth) // expected-error {{type 'String' has no member 'meth'}}
