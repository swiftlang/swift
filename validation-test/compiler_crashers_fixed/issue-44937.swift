// RUN: not %target-swift-frontend -typecheck %s

struct X: ExpressibleByStringLiteral {
  let i: Int
}

enum E: X {
  case fooBar = "A"
}

print(E.fooBar)
