// RUN: %target-swift-frontend -emit-sil -Xllvm -sil-print-debuginfo %s \
// RUN:  | %FileCheck %s
func f(c: AnyObject?) {
  let x = c
  // CHECK: sil_scope [[S1:[0-9]+]] { loc "{{.*}}":[[@LINE-2]]:23 parent 
  // CHECK: sil_scope [[S2:[0-9]+]] { loc "{{.*}}":[[@LINE+3]]:3 parent [[S1]] }
  // CHECK: debug_value %0 : $Optional<AnyObject>, let, name "x"{{.*}} scope [[S1]]
  // CHECK: debug_value %6 : $AnyObject, let, name "x", {{.*}} scope [[S2]]
  guard let x = x else {
    fatalError(".")
  }
  print(x)
}
