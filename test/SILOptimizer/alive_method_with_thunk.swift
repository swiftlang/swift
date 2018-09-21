// RUN: %target-swift-frontend -O  %s -parse-as-library -emit-sil | %FileCheck %s

public class BaseClass<T> {
  func doSomething(_ value: T) -> Int {
    return 1
  }
}

public class DerivedClass: BaseClass<Double> {

  // Don't eliminate this public method, which is called via a thunk
  public override func doSomething(_ value: Double) -> Int {
    return 1
  }
}

// CHECK: sil_vtable BaseClass {
// CHECK:  #BaseClass.doSomething!1: <T> (BaseClass<T>) -> (T) -> Int : @$s23alive_method_with_thunk9BaseClassC11doSomethingySixF // BaseClass.doSomething(_:)
// CHECK: }

// CHECK: sil_vtable DerivedClass {
// CHECK:  #BaseClass.doSomething!1: <T> (BaseClass<T>) -> (T) -> Int : public @$s23alive_method_with_thunk12DerivedClassC11doSomethingySiSdFAA04BaseF0CADySixFTV [override]  // vtable thunk for BaseClass.doSomething(_:) dispatching to DerivedClass.doSomething(_:)
// CHECK: }

