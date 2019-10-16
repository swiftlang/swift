// RUN: %target-typecheck-verify-swift

// With a bit of effort, we could make this work -- but for now, let's just
// not crash.

protocol P {
  var x: Int { get set }
}

struct S : P {
  static var x = 0
  var x = S.x // expected-error {{circular reference}}
  // expected-note@-1 {{through reference here}}
}

// FIXME: Lousy diagnostics on this case.
protocol SR9224_Foo: SR9224_Foobar {} // expected-error 3 {{protocol 'SR9224_Foo' refines itself}}
protocol SR9224_Bar: SR9224_Foobar {} // expected-error {{protocol 'SR9224_Bar' refines itself}} expected-note 2 {{protocol 'SR9224_Bar' declared here}}
typealias SR9224_Foobar = SR9224_Foo & SR9224_Bar
