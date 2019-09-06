// RUN: not %target-swift-frontend -typecheck %s

struct Foo: ExpressibleByStringLiteral {
  init(stringLiteral: String) {}
}

enum Bar: Foo {
  case some
  typealias RawValue = Foo
}
