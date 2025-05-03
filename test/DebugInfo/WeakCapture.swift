// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s
// RUN: %target-swift-frontend %s -enable-experimental-feature WeakLet -emit-ir -g -o - | %FileCheck %s

// REQUIRES: swift_feature_WeakLet

class A {
    init(handler: (() -> ())) { }
}

class B { }

// CHECK: define {{.*}} @"$s11WeakCapture8functionyyF"()
func function() {
    let b = B()

  // Ensure that the local b and its weak copy are distinct local variables.
  // CHECK: #dbg_{{.*}}(ptr [[B:.*]],
  // CHECK: #dbg_{{.*}}(ptr 
  // CHECK-NOT: [[B]]
  // CHECK: call
    A(handler: { [weak b] in
            if b != nil { }
        })
}

function()
