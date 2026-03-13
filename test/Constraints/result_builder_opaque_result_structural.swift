// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple

@resultBuilder
struct TupleBuilder {
  static func buildBlock<T1, T2>(_ t1: T1, _ t2: T2) -> (T1, T2) {
    return (t1, t2)
  }
}

protocol Tupled {
  associatedtype TupleType
  
  @TupleBuilder var tuple: TupleType { get }
}

struct TupleMeStructural: Tupled {
  var tuple: (some Any, Int) {
    "hello"
    0
  }
}
