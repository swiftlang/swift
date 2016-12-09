// RUN: %target-swift-frontend -import-objc-header %S/Inputs/extension-inheritance-conformance-objc.h -typecheck -verify %s

// REQUIRES: objc_interop

extension View {}

protocol Foo {}

extension Foo {
  func bar() -> Self { return self }
}

extension Object: Foo {}

_ = Object().bar()
_ = Responder().bar()
_ = View().bar()

