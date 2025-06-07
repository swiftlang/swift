// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple

// These test cases exercise variants of rdar://problem/112785081
// and https://github.com/apple/swift/issues/68160.

protocol P {}

protocol Q {
  associatedtype A: Q

  var a: A { get }
}

struct S1<each T: Q>: P {
  init(_: repeat each T) {}
}

func foo1a<each T: Q>(_ t: repeat each T) -> some P {
  return S1(repeat each t)
}

func foo2a<each T: Q>(_ t: repeat each T) -> S1<repeat each T> {
  return S1(repeat each t)
}

func foo3a<each T: Q>(_ t: repeat each T) -> some P {
  return S1(repeat (each t).a)
}

func foo4a<each T: Q>(_ t: repeat each T) -> S1<repeat (each T).A> {
  return S1(repeat (each t).a)
}

func foo1b<each T: Q>(_ t: repeat each T) -> some P {
  return S1.init(repeat each t)
}

func foo2b<each T: Q>(_ t: repeat each T) -> S1<repeat each T> {
  return S1.init(repeat each t)
}

func foo3b<each T: Q>(_ t: repeat each T) -> some P {
  return S1.init(repeat (each t).a)
}

func foo4b<each T: Q>(_ t: repeat each T) -> S1<repeat (each T).A> {
  return S1.init(repeat (each t).a)
}

struct S2<each T: Q>: P {
  init(arg: (repeat each T)) {}
}

func bar1a<each T: Q>(_ t: repeat each T) -> some P {
  return S2(arg: (repeat each t))
}

func bar2a<each T: Q>(_ t: repeat each T) -> S2<repeat each T> {
  return S2(arg: (repeat each t))
}

func bar3a<each T: Q>(_ t: repeat each T) -> some P {
  return S2(arg: (repeat (each t).a))
}

func bar4a<each T: Q>(_ t: repeat each T) -> S2<repeat (each T).A> {
  return S2(arg: (repeat (each t).a))
}

func bar1b<each T: Q>(_ t: repeat each T) -> some P {
  return S2.init(arg: (repeat each t))
}

func bar2b<each T: Q>(_ t: repeat each T) -> S2<repeat each T> {
  return S2.init(arg: (repeat each t))
}

func bar3b<each T: Q>(_ t: repeat each T) -> some P {
  return S2.init(arg: (repeat (each t).a))
}

func bar4b<each T: Q>(_ t: repeat each T) -> S2<repeat (each T).A> {
  return S2.init(arg: (repeat (each t).a))
}
