// RUN: %target-swift-frontend -g -emit-sil %s -parse-as-library -module-name a | %FileCheck %s
public class C {}
public enum MyError : Error {
  init() { self.init() }
}
public class S {
  private var c = [Int : C?]()
  public func f(_ i: Int) throws  -> C {
    guard let x = c[i], let x else {
    // CHECK: sil_scope [[X1:[0-9]+]] { loc "{{.*}}":[[@LINE-1]]:5
    // CHECK: sil_scope [[X2:[0-9]+]] { loc "{{.*}}":[[@LINE-2]]:29
    // CHECK: debug_value {{.*}} : $Optional<C>, let, name "x", {{.*}}, scope [[X1]]
    // CHECK: debug_value {{.*}} : $C, let, name "x", {{.*}}, scope [[X2]]
    // CHECK-NEXT:  scope [[X2]]
      throw MyError()
    }
    return x
  }
}
