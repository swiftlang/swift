// RUN: %target-typecheck-verify-swift

extension Collection {
  func myMap<T>(_: (Self.Element) -> T) -> [T] { fatalError() }
}

protocol SignedInteger {}

extension SignedInteger {
  init<T: BinaryFloatingPoint>(_: T) { fatalError() }
  init<T: BinaryInteger>(_: T) { fatalError() }
}

struct Int32: SignedInteger {
  init(_: String) {}
}

func test() {
  let _: [(Int32, Float)] = (0..<1).myMap { (Int32($0), 0.0) }
}
