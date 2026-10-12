// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name MyModule
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name MyModule
// RUN: %FileCheck %s < %t.swiftinterface

public struct MyStruct<T> {
// CHECK-LABEL: public struct MyStruct<T> {
  public typealias AliasT = T
  public typealias AliasInt = Int

  public func foo(x: AliasInt) -> AliasT { fatalError() }
// CHECK:  public func foo(x: MyModule::MyStruct<T>.MyModule::AliasInt) -> MyModule::MyStruct<T>.MyModule::AliasT
}

public class MyBase<U> {
  public typealias AliasU = U 
  public typealias AliasInt = Int
}

public class MyDerived<X>: MyBase<X> {
// CHECK-LABEL: public class MyDerived<X> : MyModule::MyBase<X> {
  public func bar(x: AliasU) -> AliasInt { fatalError() }
// CHECK:  public func bar(x: MyModule::MyDerived<X>.MyModule::AliasU) -> MyModule::MyDerived<X>.MyModule::AliasInt
}

// The signature of `useMySequence` is checked first and requires the type
// witnesses for `MySequence.Iterator`'s conformance to IteratorProtocol, which
// synthesizes the implicit typealias `Element` in `MySequence.Iterator`.
// References to the implicit typealias, which is not printed, must be printed
// as the generic parameter it aliases.
func useMySequence(_: MySequence<Int>.Iterator.Element) {}

public struct MySequence<Element>: Sequence {
// CHECK-LABEL: public struct MySequence<Element> : Swift::Sequence {
  public struct Iterator: IteratorProtocol {
// CHECK-LABEL: public struct Iterator : Swift::IteratorProtocol {
    public init(_ element: Element) {}
// CHECK: public init(_ element: Element)

    public func next() -> Element? { nil }
  }

  public func makeIterator() -> Iterator { fatalError() }
}

public struct MyLaterSequence<Element> {}

extension MyLaterSequence {
// CHECK-LABEL: extension MyModule::MyLaterSequence {
  public func baz() -> Self.Element { fatalError() }
// CHECK: public func baz() -> Element
}

// The conformance comes after the extension above, so the implicit typealias
// `Element` is not available to it when the interface is typechecked.
extension MyLaterSequence: Sequence {
  public struct Iterator: IteratorProtocol {
    public func next() -> Element? { nil }
  }

  public func makeIterator() -> Iterator { fatalError() }
}
