// RUN: %target-typecheck-verify-swift -swift-version 4

public protocol P {
  associatedtype Element

  func f() -> Element
}

struct S<T> : P {
  func f() -> T { while true {} }
}

public struct G<T> {
  typealias A = S<T> // expected-note {{type declared here}}

  public func foo<U : P>(u: U) where U.Element == A.Element {}
  // expected-error@-1 {{instance method cannot be declared public because its generic requirement uses an internal type}}
}

public final class ReplayableGenerator<S: Sequence> : IteratorProtocol {
    typealias Sequence = S // expected-note {{type declared here}}
    public typealias Element = Sequence.Iterator.Element // expected-error {{type alias cannot be declared public because its underlying type uses an internal type}}

    public func next() -> Element? {
      return nil
    }
}

// FIXME: Dependent member lookup of typealiases is not subject
// to accessibility checking.
struct Generic<T> {
  fileprivate typealias Dependent = T
}

var x: Generic<Int>.Dependent = 3 // expected-error {{variable must be declared private or fileprivate because its type uses a fileprivate type}}

func internalFuncWithFileprivateAlias() -> Generic<Int>.Dependent { // expected-error {{function must be declared private or fileprivate because its result uses a fileprivate type}}
  return 3
}

private func privateFuncWithFileprivateAlias() -> Generic<Int>.Dependent {
  return 3
}

// FIXME: No error here
var y = privateFuncWithFileprivateAlias()
