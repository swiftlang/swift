// RUN: %target-swift-frontend -enable-sil-ownership -sil-verify-all %s -module-name test -emit-sil -o - -verify | %FileCheck %s


// Constructor calls are dispatched dynamically for open classes, even if
// the constructor itself is not "open".

open class OpenClass {
  // CHECK-LABEL: sil @$S4test9OpenClassC1xACSi_tcfc : $@convention(method) (Int, @owned OpenClass) -> @owned OpenClass
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
  // CHECK-LABEL: sil @$S4test11PublicClassC1xACSi_tcfc : $@convention(method) (Int, @owned PublicClass) -> @owned PublicClass
  // CHECK: [[M:%[0-9]+]] = function_ref @$S4test11PublicClassC1x1yACSi_Sitcfc : $@convention(method) (Int, Int, @owned PublicClass) -> @owned PublicClass
  // CHECK: apply [[M]]
  // CHECK: return
  public convenience init(x: Int) {
    self.init(x: x, y: 27)
  }
  public init(x: Int, y: Int) {
  }
}

public protocol Thrower {
  func fail() throws
}

@_transparent public func callee<T : Thrower>(_ t: T) throws {
  try t.fail()
}

public struct Concrete : Thrower {
  public func fail() throws {}
}

// CHECK-LABEL: sil @$S4test6calleryyAA8ConcreteVKF : $@convention(thin) (Concrete) -> @error Error
public func caller(_ c: Concrete) throws {
  // CHECK: [[ARG:%.*]] = struct $Concrete ()
  // CHECK: [[FN:%.*]] = function_ref @$S4test8ConcreteV4failyyKF : $@convention(method) (Concrete) -> @error Error
  // CHECK: try_apply [[FN]]([[ARG]]) : $@convention(method) (Concrete) -> @error Error
  try callee(c)
}
