// RUN: %target-typecheck-verify-swift

// SR-11420 / #53821: when a requirement nearly matches an *inherited* member
// (rename near-miss), still emit a stub for that requirement so the "add stubs"
// fix-it covers every missing requirement.

protocol RenamedStubProto {
  func foo(x: Int)
  // expected-note@-1 {{protocol requires function 'foo(x:)' with type '(Int) -> ()'}}
  // expected-note@-2 {{requirement 'foo(x:)' declared here}}
  func bar()
  // expected-note@-1 {{protocol requires function 'bar()' with type '() -> ()'}}
}

class RenamedStubBase {
  func foo(y: Int) {}
  // expected-note@-1 {{rename to 'foo(x:)' to satisfy this requirement}}
}

class RenamedStubDerived: RenamedStubBase, RenamedStubProto {
  // expected-error@-1 {{type 'RenamedStubDerived' does not conform to protocol 'RenamedStubProto'}}
  // expected-note@-2 {{add stubs for conformance}} {{67-67=\n    func foo(x: Int) {\n        <#code#>\n    \}\n\n    func bar() {\n        <#code#>\n    \}\n}}
}
