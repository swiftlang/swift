// RUN: %target-swift-frontend -emit-sil -Xllvm -sil-print-debuginfo %s \
// RUN:  | %FileCheck %s
func f(c: AnyObject?) {
  let x = c
  // CHECK: sil_scope [[S1:[0-9]+]] { {{.*}} parent @{{.*}}1f
  // CHECK: sil_scope [[S2:[0-9]+]] { loc "{{.*}}":[[@LINE+3]]:3 parent [[S1]] }
  // CHECK: debug_value %{{.*}} : $Optional<AnyObject>, let, name "x"{{.*}} scope [[S1]]
  // CHECK: debug_value %{{.*}} : $AnyObject, let, name "x", {{.*}} scope [[S2]]
  guard let x = x else {
    fatalError(".")
  }
  print(x)
}

// Check that we don't crash with a verifier error on this.
protocol P {}

public func testit(_ x: AnyObject) -> Bool {
  guard let _ = x as? P else {
    return false
  }
  fatalError()
}
