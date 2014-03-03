
struct Y {}

struct X<U> {
  var a : U

  init(_a : U) {
    a = _a
  }

  func doneSomething() {}
}

class A {
  var y : Y
  var x : X<Y>

  init() {
    y = Y()
    x = X<Y>(y)
  }

  func doSomething() {
    x.doneSomething()
  }
}
