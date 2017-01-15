// RUN: %target-typecheck-verify-swift -swift-version 3 -module-name main

public protocol P {
  associatedtype Element

  func f() -> Element
}

struct S<T> : P {
  func f() -> T { while true {} }
}

public struct G<T> {
  typealias A = S<T> // expected-note {{declared here}}

  public func foo<U : P>(u: U) where U.Element == A.Element {} // expected-warning {{instance method should not be declared public because its generic requirement uses an internal type}}
}

public final class ReplayableGenerator<S: Sequence> : IteratorProtocol {
    typealias Sequence = S // expected-note {{declared here}}
    public typealias Element = Sequence.Iterator.Element // expected-warning {{type alias should not be declared public because its underlying type uses an internal type}}

    public func next() -> Element? {
      return nil
    }
}

struct Generic<T> {
  fileprivate typealias Dependent = T
}

var x: Generic<Int>.Dependent = 3 // expected-warning {{variable should be declared fileprivate because its type uses a fileprivate type}}

func internalFuncWithFileprivateAlias() -> Generic<Int>.Dependent { // expected-warning {{function should be declared fileprivate because its result uses a fileprivate type}}
  return 3
}

private func privateFuncWithFileprivateAlias() -> Generic<Int>.Dependent {
  return 3
}

var y = privateFuncWithFileprivateAlias()


private typealias FnType = (_ x: Int) -> Void // expected-note * {{type declared here}}

public var fn1: (FnType) -> Void = { _ in } // expected-warning {{should not be declared public}}
public var fn2: (_ x: FnType) -> Void = { _ in } // expected-warning {{should not be declared public}}
public var fn3: (main.FnType) -> Void = { _ in } // expected-warning {{should not be declared public}}
public var fn4: (_ x: main.FnType) -> Void = { _ in } // expected-warning {{should not be declared public}}
public var nested1: (_ x: (FnType) -> Void) -> Void = { _ in } // expected-warning {{should not be declared public}}
public var nested2: (_ x: (main.FnType) -> Void) -> Void = { _ in } // expected-warning {{should not be declared public}}
public func test1(x: FnType) {} // expected-warning {{should not be declared public}}
public func test2(x: main.FnType) {} // expected-warning {{should not be declared public}}


public func reject1(x: FnType?) {} // expected-error {{cannot be declared public}}
public func reject2(x: main.FnType?) {} // expected-error {{cannot be declared public}}
public func reject3() -> FnType { fatalError() } // expected-error {{cannot be declared public}}
public func reject4() -> main.FnType { fatalError() } // expected-error {{cannot be declared public}}
public var rejectVar1: FnType = {_ in } // expected-error {{cannot be declared public}}
public var rejectVar2: main.FnType = {_ in } // expected-error {{cannot be declared public}}
public var rejectVar3: FnType? // expected-error {{cannot be declared public}}
public var rejectVar4: main.FnType? // expected-error {{cannot be declared public}}
public var escaping1: (@escaping FnType) -> Void = { _ in } // expected-error {{cannot be declared public}}
public var escaping2: (_ x: @escaping FnType) -> Void = { _ in } // expected-error {{cannot be declared public}}
public var escaping3: (@escaping main.FnType) -> Void = { _ in } // expected-error {{cannot be declared public}}
public var escaping4: (_ x: @escaping main.FnType) -> Void = { _ in } // expected-error {{cannot be declared public}}

public struct SubscriptTest {
  public subscript(test1 x: FnType) -> () { return } // expected-warning {{should not be declared public}}
  public subscript(test2 x: main.FnType) -> () { return } // expected-warning {{should not be declared public}}

  public subscript(reject1 x: FnType?) -> () { return } // expected-error {{cannot be declared public}}
  public subscript(reject2 x: main.FnType?) -> () { return } // expected-error {{cannot be declared public}}
  public subscript(reject3 x: Int) -> FnType { fatalError() } // expected-error {{cannot be declared public}}
  public subscript(reject4 x: Int) -> main.FnType { fatalError() } // expected-error {{cannot be declared public}}
}

private struct ActuallyPrivate {} // expected-note * {{declared here}}
private typealias ActuallyPrivateAlias = ActuallyPrivate

public var failFn: (ActuallyPrivate) -> Void = { _ in } // expected-error {{cannot be declared public}}
public var failFn2: (_ x: ActuallyPrivate) -> Void = { _ in } // expected-error {{cannot be declared public}}
public var failFn3: (main.ActuallyPrivate) -> Void = { _ in } // expected-error {{cannot be declared public}}
public var failFn4: (_ x: main.ActuallyPrivate) -> Void = { _ in } // expected-error {{cannot be declared public}}
public var failNested1: (_ x: (ActuallyPrivate) -> Void) -> Void = { _ in } // expected-error {{cannot be declared public}}
public var failNested2: (_ x: (main.ActuallyPrivate) -> Void) -> Void = { _ in } // expected-error {{cannot be declared public}}
public func failTest(x: ActuallyPrivate) {} // expected-error {{cannot be declared public}}
public func failTest2(x: main.ActuallyPrivate) {} // expected-error {{cannot be declared public}}

