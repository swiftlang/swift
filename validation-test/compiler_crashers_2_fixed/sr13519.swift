// RUN: %target-swift-frontend -typecheck %s

// https://bugs.swift.org/browse/SR-13519

class Class: P {
  typealias A = Bool
}

protocol P {
  associatedtype A
}

protocol Q : P {
  func takesA(arg: A)
}

func test<T: Class & Q>(arg: T) {
  arg.takesA(arg: true)
}
