// RUN: %target-swift-frontend -g -Xllvm -sil-print-types -emit-sil %s -parse-as-library -module-name a | %FileCheck %s
public class C {}
public enum MyError : Error {
  init() { self.init() }
}
public class S {
  private var c = [Int : C?]()
  public func f(_ i: Int) throws  -> C {
    guard let x = c[i], let x else {
      // CHECK: sil_scope [[P:[0-9]+]] { loc "{{.*}}":9:5
      // CHECK: sil_scope [[X1_RHS:[0-9]+]] { loc "{{.*}}":9:19 parent [[P]]
      // CHECK: sil_scope [[X1:[0-9]+]] { loc "{{.*}}":9:19 parent [[P]]
      // CHECK: sil_scope [[X2:[0-9]+]] { loc "{{.*}}":9:29 parent [[X1]]
      // CHECK: sil_scope [[X3:[0-9]+]] { loc "{{.*}}":9:29 parent [[X1]]
      // CHECK: sil_scope [[GUARD:[0-9]+]] { loc "{{.*}}":9:36 parent [[P]]
      // CHECK: debug_value {{.*}} : $Optional<C>, let, name "x", {{.*}}, scope [[X1]]
      // CHECK: debug_value {{.*}} : $C, let, name "x", {{.*}}, scope [[X3]]
      // FIXME: This source location is a little wild.
      // CHECK-NEXT: release_value{{.*}}:[[@LINE+5]]:3, scope [[X3]]
      throw MyError()
      // CHECK: function_ref {{.*}}MyError{{.*}}:[[@LINE-1]]:13, scope [[GUARD]]
    }
    return x
  }
}
