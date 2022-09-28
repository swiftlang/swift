// RUN: not %target-swift-frontend -typecheck %s

// https://github.com/apple/swift/issues/49550

struct Foo: OptionSet {
  let rawValue: Int
  static let none = Foo(rawValue: 1 << 0)
}

extension Foo: ExpressibleByIntegerLiteral { }
