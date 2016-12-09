// RUN: %target-swift-frontend %s -module-name test -emit-sil -o - -verify | %FileCheck %s


// Constructor calls are dispatched dynamically for open classes, even if
// the constructor itself is not "open".

open class OpenClass {
  // CHECK-LABEL: sil @_TFC4test9OpenClasscfT1xSi_S0_ : $@convention(method) (Int, @owned OpenClass) -> @owned OpenClass
  // CHECK: [[M:%[0-9]+]] = class_method %1 : $OpenClass, #OpenClass.init!initializer.1 : (OpenClass.Type) -> (Int, Int) -> OpenClass
  // CHECK: apply [[M]]
  // CHECK: return
  public convenience init(x: Int) {
    self.init(x: x, y: 27)
  }
  public init(x: Int, y: Int) {
  }
}

// Static dispatch for not-open class (we are compiling with -wmo).

public class PublicClass {
  // CHECK-LABEL: sil @_TFC4test11PublicClasscfT1xSi_S0_ : $@convention(method) (Int, @owned PublicClass) -> @owned PublicClass
  // CHECK: [[M:%[0-9]+]] = function_ref @_TFC4test11PublicClasscfT1xSi1ySi_S0_ : $@convention(method) (Int, Int, @owned PublicClass) -> @owned PublicClass
  // CHECK: apply [[M]]
  // CHECK: return
  public convenience init(x: Int) {
    self.init(x: x, y: 27)
  }
  public init(x: Int, y: Int) {
  }
}

