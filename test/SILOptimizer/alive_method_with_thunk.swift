// RUN: %target-swift-frontend -O  %s -parse-as-library -emit-sil | %FileCheck %s

public class BaseClass<T> {
  func doSomething(_ value: T) -> Int {
    return 1
  }
}

public class DerivedClass: BaseClass<Double> {
  // This method is more visible than its override, so it gets a new
  // vtable entry, and the base class vtable entry is replaced with a
  // thunk that re-dispatches to the derived method.
  //
  // The base class method is dead, but the derived method is not.
  public override func doSomething(_ value: Double) -> Int {
    return 1
  }
}

// CHECK-LABEL: sil_vtable BaseClass {
// CHECK-NEXT:  #BaseClass.deinit!deallocator: @$s23alive_method_with_thunk9BaseClassCfD
// CHECK-NEXT: }

// CHECK-LABEL: sil_vtable DerivedClass {
// CHECK-NEXT:  #DerivedClass.doSomething: (DerivedClass) -> (Double) -> Int : @$s23alive_method_with_thunk12DerivedClassC11doSomethingySiSdF
// CHECK-NEXT:  #DerivedClass.deinit!deallocator: @$s23alive_method_with_thunk12DerivedClassCfD
// CHECK-NEXT: }

