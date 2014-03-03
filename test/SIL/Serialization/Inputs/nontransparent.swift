
struct B {
  func amIConfused() {}
}

struct A {
  var b : B

  init() {
    b = B()
  }

  func isBConfused() {
    b.amIConfused()
  }
}

func doSomething() -> A {
  var a = A()
  return a
}
