// RUN: not %target-swift-frontend -typecheck %s

protocol P {
  associatedtype T
}

class G<T : P> {
  init(_: T.T) {}
}

class Sub : G<S> {}

struct S : P {}

Sub()
