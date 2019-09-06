// RUN: not %target-swift-frontend -typecheck %s

struct Foo: ExpressibleByStringLiteral {
  init(stringLiteral: String) {}
}

enum Bar1: Foo {
  case some1
  typealias RawValue = Foo
}

enum Bar2: Foo {
  case some2
  typealias RawValue = Foo
  init?(rawValue: Int) { self = .some2 }
  var rawValue: Int { 0 }
}