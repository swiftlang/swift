// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s -emit-sil | FileCheck %s
import gizmo

// Super calls in objc require dynamic dispatch, represented in SIL as
// a super_method instruction.
class Hoozit : Gizmo {

  // CHECK: sil @constructor.initializer.1 : $[sil_cc=method] (Hoozit)() -> Hoozit
  constructor() {
    // CHECK: super_method {{%.*}}, @constructor.initializer.1
    super.constructor()
  }

  // CHECK: sil @runce.1 : $(Hoozit.metatype)() -> ()
  static func runce() {
    // CHECK: super_method {{%.*}}, @runce.1
    super.runce()
  }

  // CHECK: sil @frob.1 : $[sil_cc=method] (Hoozit)() -> ()
  func frob() {
    // CHECK: super_method {{%.*}}, @frob.1
    super.frob()
  }
}
