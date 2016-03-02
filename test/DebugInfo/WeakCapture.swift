// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s
class A {
    init(handler: (() -> ())) { }
}

class B { }

// CHECK: define {{.*}} @_TF11WeakCapture8functionFT_T_()
func function() {
    let b = B()

  // Ensure that the local b and its weak copy are distinct local variables.
  // CHECK:  %[[B:.*]] = alloca %C11WeakCapture1B*
  // CHECK:  %[[BWEAK:.*]] = alloca %swift.weak*
  // CHECK: call void @llvm.dbg.declare({{.*}}[[B]]
  // CHECK: call void @llvm.dbg.declare({{.*}}[[BWEAK]]
    A(handler: { [weak b] _ in
            if b != nil { }
        })
}

function()
