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
// CHECK:  #BaseClass.doSomething!1: {{.*}} : _{{.*}}// BaseClass.doSomething(A) -> Int
// CHECK: }

// CHECK: sil_vtable DerivedClass {
// CHECK:  #BaseClass.doSomething!1: {{.*}} : public _{{.*}}// override DerivedClass.doSomething(Double) -> Int
// CHECK: }

