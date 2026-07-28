// RUN: %target-typecheck-verify-swift

// https://github.com/swiftlang/swift/issues/90628

protocol P {}

@resultBuilder
struct Builder {
  static func buildBlock<T: P>(_: T) {}
}

func overloaded<T>(_: @autoclosure () -> T) {}
func overloaded<T>(@Builder _: () -> T) {}

struct S : P {
}

struct Test {
  func test() {
    overloaded { [self] in
      getS()
    }
  }

  func getS() -> S { .init() }
}
