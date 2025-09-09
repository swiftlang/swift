// RUN: %target-typecheck-verify-swift %s

@attached(member) @attached(peer)
public macro Foo(_: any P) = #externalMacro(module: "FooMacros", type: "FooMacro")
// expected-warning@-1 {{external macro implementation type 'FooMacros.FooMacro' could not be found for macro 'Foo'; plugin for module 'FooMacros' not found}}
// expected-note@-2 2 {{'Foo' declared here}}

public protocol P {}

@Foo(S.s)
struct A {
// expected-error@-1 2 {{external macro implementation type 'FooMacros.FooMacro' could not be found for macro 'Foo'; plugin for module 'FooMacros' not found}}
  func a() {}
}

extension A {
  struct Nested {}
}

// Binding this extension must not trigger macro expansion, because that
// performs a qualified lookup of S.s, which fails because the extension
// of P declared below has not been bound yet.
extension A.Nested {}

struct S: P {}

extension P where Self == S {
  static var s: Self { Self() }
}
