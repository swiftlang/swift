// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s -emit-silgen | FileCheck %s
import gizmo

// Super calls in objc require dynamic dispatch, represented in SIL as
// a super_method instruction.
class Hoozit : Gizmo {

  // CHECK: sil @_TCSo6Hoozitc{{.*}} : $[cc(method), thin] ((), Hoozit) -> Hoozit
  constructor() {
    // CHECK: super_method [volatile] {{%.*}} : $Hoozit, #Gizmo.constructor!initializer.1.objc
    super.constructor()
  }

  // CHECK: sil @_TCSo6Hoozit5runcefMS_FT_T_ : $[thin] ((), Hoozit.metatype) -> ()
  static func runce() {
    // CHECK: super_method [volatile] {{%.*}} : $Hoozit.metatype, #Gizmo.runce!1.objc
    super.runce()
  }

  // CHECK: sil @_TCSo6Hoozit4frobfS_FT_T_ : $[cc(method), thin] ((), Hoozit) -> ()
  func frob() {
    // CHECK: super_method [volatile] {{%.*}} : $Hoozit, #Gizmo.frob!1.objc
    super.frob()
  }
}

class Wotsit : Hoozit {
  // -- funge() is declared on Gizmo, the grandparent class
  func funge() {
    // CHECK: super_method [volatile] {{%.*}} : $Wotsit, #Gizmo.funge!1.objc
    super.funge()
  }
}
