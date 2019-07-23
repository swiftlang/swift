// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s
class A {
    init(handler: (() -> ())) { }
}

class B { }

// CHECK: define {{.*}} @"$s11WeakCapture8functionyyF"()
func function() {
    let b = B()

  // Ensure that the local b and its weak copy are distinct local variables.
  // CHECK: call void @llvm.dbg.{{.*}}(metadata %T11WeakCapture1BC*
  // CHECK-SAME:                       metadata [[B:.*]], metadata
  // CHECK: call void @llvm.dbg.{{.*}}(metadata %swift.weak*
  // CHECK-NOT:                        metadata [[B]]
  // CHECK: call
    A(handler: { [weak b] in
            if b != nil { }
        })
}

function()
