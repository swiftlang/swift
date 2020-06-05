// RUN: not %target-swift-frontend -typecheck %s

struct Foo: OptionSet {
  let rawValue: Int
  static let none = Foo(rawValue: 1 << 0)
}

extension Foo: ExpressibleByIntegerLiteral { }
