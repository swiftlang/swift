// RUN: %target-typecheck-verify-swift -swift-version 3

public protocol P {
  associatedtype Element

  func f() -> Element
}

struct S<T> : P {
  func f() -> T { while true {} }
}

public struct G<T> {
  typealias A = S<T>

  public func foo<U : P>(u: U) where U.Element == A.Element {}
}

public final class ReplayableGenerator<S: Sequence> : IteratorProtocol {
    typealias Sequence = S
    public typealias Element = Sequence.Iterator.Element

    public func next() -> Element? {
      return nil
    }
}

struct Generic<T> {
  fileprivate typealias Dependent = T
}

var x: Generic<Int>.Dependent = 3

func internalFuncWithFileprivateAlias() -> Generic<Int>.Dependent {
  return 3
}

private func privateFuncWithFileprivateAlias() -> Generic<Int>.Dependent {
  return 3
}

var y = privateFuncWithFileprivateAlias()
