// RUN: %target-swift-frontend -typecheck %s

func doSomething(_: Int) {}

struct S {
  var x: Int {
    didSet {
      doSomething(y)
      doSomething(self.y)
    }
  }

  var y: Int {
    didSet {
      doSomething(x)
      doSomething(self.x)
    }
  }
}
