// RUN: %target-swift-frontend -typecheck -verify %s

class Object {}
class Responder: Object {}
class View: Responder {}

extension View {}

protocol Foo {}

extension Foo {
  func bar() -> Self { return self }
}

extension Object: Foo {}

_ = Object().bar()
_ = Responder().bar()
_ = View().bar()

