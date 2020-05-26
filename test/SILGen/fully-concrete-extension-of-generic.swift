// RUN: %target-swift-emit-silgen -verify %s 

class C<T> {
  init() {}
}

extension C where T == Int {
  convenience init(forInt _: ()) {
    self.init()
  }
}

func exerciseInits(which: Bool) -> C<Int> {
  if which {
    return C()
  } else {
    return C(forInt: ())
  }
}

protocol P {
  associatedtype T
}

struct S : P {
  typealias T = Int
}

struct G<T : P> {}

extension G where T == S {
  func foo(_: T.T) {}
}