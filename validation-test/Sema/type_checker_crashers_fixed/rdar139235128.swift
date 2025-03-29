// RUN: %target-typecheck-verify-swift

@resultBuilder
struct Builder {
  static func buildExpression<T: BinaryInteger>(_: T) {}
  static func buildBlock<T>(_: T) {}
}

enum E {
  case a(Int)
}

protocol Test {
  associatedtype V: BinaryInteger
  @Builder var prop: V { get }
}

struct S : Test {
  var flag: E

  var prop: some BinaryInteger {
    switch flag {
    case .a:
      test()
      return 42
    }
  }

  func test() {}
}
