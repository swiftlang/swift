// RUN: %target-swift-frontend -typecheck %s

// https://github.com/apple/swift/issues/55955

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
