// RUN: %target-swift-frontend -typecheck -verify %s

struct Bar {}

struct Foo {
  func registerCallback(_ callback: @escaping ([Bar]) -> Void) {}
  func registerCallback(_ callback: @escaping ([String: Bar]) -> Void) {}
  func registerCallback(_ callback: @escaping (Bar?) -> Void) {}
}

Foo().registerCallback { ([Bar]) in } // expected-warning {{unnamed parameters must be written with the empty name '_'}}
Foo().registerCallback { ([String: Bar]) in } // expected-warning {{unnamed parameters must be written with the empty name '_'}}
Foo().registerCallback { (Bar?) in } // expected-error {{unnamed parameters must be written with the empty name '_'}}
Foo().registerCallback { (Bar!) in } // expected-error {{unnamed parameters must be written with the empty name '_'}}
