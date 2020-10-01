// RUN: not %target-swift-frontend -typecheck %s

// rdar://problem/45590743 used to crash
class Base {
  var x = 0
}

class Derived : Base {
  override var x = Derived().x
}
