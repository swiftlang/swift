
struct Y {}

protocol P {
  func doSomething() -> Y
}

struct X : P {
  func doSomething() -> Y {
    return Y()
  }
}
