// RUN: %target-swift-frontend -typecheck -emit-module-interface-path - %s -enable-library-evolution -module-name MyModule | %FileCheck %s --check-prefix CHECK

public struct MyStruct<T> {
// CHECK-LABEL: public struct MyStruct<T> {
  public typealias AliasT = T
  public typealias AliasInt = Int

  public func foo(x: AliasInt) -> AliasT { fatalError() }
// CHECK:  public func foo(x: MyModule.MyStruct<T>.AliasInt) -> MyModule.MyStruct<T>.AliasT
}

public class MyBase<U> {
  public typealias AliasU = U 
  public typealias AliasInt = Int
}

public class MyDerived<X>: MyBase<X> {
// CHECK-LABEL: public class MyDerived<X> : MyModule.MyBase<X> {
  public func bar(x: AliasU) -> AliasInt { fatalError() }
// CHECK:  public func bar(x: MyModule.MyDerived<X>.AliasU) -> MyModule.MyDerived<X>.AliasInt
}
