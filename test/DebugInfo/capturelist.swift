// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s
class C {
  func withClosure(_ : () -> ()) -> () {}

  func f() {
    // CHECK: define{{.*}}_TFFC11capturelist1C1fFT_T_U_FT_T_
    // There should not be a local weak variable "self" shadowing the
    // implicit self argument.
    // let self
    // CHECK: call void @llvm.dbg
    // let s
    // CHECK: call void @llvm.dbg
    // var weak self
    // CHECK-NOT: call void @llvm.dbg
    // CHECK: ret void
    withClosure { [weak self] in
      guard let s = self else { return }
    }
  }
}
