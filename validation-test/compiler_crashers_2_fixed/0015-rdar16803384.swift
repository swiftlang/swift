// RUN: not %target-swift-frontend %s -parse

protocol P {
  func doSomething()

  typealias Y
  func doSomething2(Y)
}

protocol P2 : P {
  func doSomething3()
}

struct X {}

class B<T> : P2 {
  func doSomething() { }

  typealias Y = T

  func doSomething2<T2>(t : T) { }

  func doSomething3() { }
}

func DoSomething<T : P, T2 where T2 == T.Y>(t : T, t2 : T2) {
  t.doSomething()
  t.doSomething2(t2)
}

func DoSomething2(p : P) {
  p.doSomething()
}

func DoSomething3<T : P2>(t : T) {
  t.doSomething3()
}

func DoSomething3(p2 : P2) {
  p2.doSomething3()
}

var b = B<X>()
var x = X()
DoSomething(b, t2: x)
DoSomething2(b)
DoSomething3(b)
