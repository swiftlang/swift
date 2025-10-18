// RUN: not %target-swift-frontend %s -typecheck

class C {}
protocol Foo {
  associatedtype X where C: X
}
