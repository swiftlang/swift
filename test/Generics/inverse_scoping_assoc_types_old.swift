// RUN: %target-typecheck-verify-swift  -enable-experimental-feature SuppressedAssociatedTypes

// REQUIRES: swift_feature_SuppressedAssociatedTypes

protocol NoCopyReq: ~Copyable {}

protocol P {
  associatedtype AT where Self: ~Copyable // expected-error {{constraint with subject type of 'Self' is not supported; consider adding requirement to protocol inheritance clause instead}}

  associatedtype Bob where Alice: NoCopyReq & ~Copyable
  associatedtype Alice where Bob: ~Copyable
}

protocol Q<Primary>: ~Copyable {
  associatedtype Primary: ~Copyable
  associatedtype Secondary: ~Copyable
}

extension Q {
  func testCopyability(_ a: Self.Primary, // expected-error {{parameter of noncopyable type 'Self.Primary}} // expected-note 3{{}}
                       _ b: Self.Secondary) {} // expected-error {{parameter of noncopyable type 'Self.Secondary}} // expected-note 3{{}}

  func testCopyability2(_ s: Self) {}
}

func genericFunc<T: Q>(_ t: T,
                       _ a: T.Primary, // expected-error {{parameter of noncopyable type 'T.Primary}} // expected-note 3{{}}
                       _ b: T.Secondary) {} // expected-error {{parameter of noncopyable type 'T.Secondary}} // expected-note 3{{}}
