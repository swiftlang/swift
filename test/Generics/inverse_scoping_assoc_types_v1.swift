// RUN: %target-typecheck-verify-swift  -enable-experimental-feature SuppressedAssociatedTypes

// REQUIRES: swift_feature_SuppressedAssociatedTypes

protocol NoCopyReq: ~Copyable {}

protocol P<Primary> {
  associatedtype AT where Self: ~Copyable // expected-error {{constraint with subject type of 'Self' is not supported; consider adding requirement to protocol inheritance clause instead}}

  // expected-error@+1 {{cannot suppress '~Copyable' on generic parameter 'Self.Alice' defined in outer scope}}
  associatedtype Bob where Alice: NoCopyReq & ~Copyable
  associatedtype Alice where Bob: ~Copyable
  // expected-error@-1 {{cannot suppress '~Copyable' on generic parameter 'Self.Bob' defined in outer scope}}

  associatedtype Primary: ~Copyable
  associatedtype Secondary: ~Copyable
}

extension P {
  func testCopyability(_ a: Self.Primary, // expected-error {{parameter of noncopyable type 'Self.Primary}} // expected-note 3{{}}
                       _ b: Self.Secondary) {} // expected-error {{parameter of noncopyable type 'Self.Secondary}} // expected-note 3{{}}
}