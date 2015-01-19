// RUN: %target-swift-frontend -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -emit-silgen | FileCheck %s

// REQUIRES: objc_interop

import gizmo

// Super calls in objc require dynamic dispatch, represented in SIL as
// a super_method instruction.
class Hoozit : Gizmo {

  // CHECK-LABEL: sil hidden  @_TFC10objc_super6HoozitcfMS0_FT_S0_{{.*}} : $@cc(method) @thin (@owned Hoozit) -> @owned Hoozit
  override init() {
    // CHECK: super_method [volatile] {{%.*}} : $Hoozit, #Gizmo.init!initializer.1.foreign
    super.init()
  }

  // CHECK-LABEL: sil hidden  @_TFC10objc_super6Hoozit5runcefMS0_FT_T_ : $@thin (@thick Hoozit.Type) -> ()
  override class func runce() {
    // CHECK: super_method [volatile] {{%.*}} : $@thick Hoozit.Type, #Gizmo.runce!1.foreign
    super.runce()
  }

  // CHECK-LABEL: sil hidden  @_TFC10objc_super6Hoozit4frobfS0_FT_T_ : $@cc(method) @thin (@owned Hoozit) -> ()
  override func frob() {
    // CHECK: super_method [volatile] {{%.*}} : $Hoozit, #Gizmo.frob!1.foreign
    super.frob()
  }
}

struct NotInObjC<T> { }

class Wotsit : Hoozit {
  // -- funge() is declared on Gizmo, the grandparent class
  override func funge() {
    // CHECK: super_method [volatile] {{%.*}} : $Wotsit, #Gizmo.funge!1.foreign
    super.funge()
  }

  init (nope: NotInObjC<Int>) { }
}

class NonObjCSuperInit : Wotsit {
  // CHECK-LABEL: sil hidden @_TFC10objc_super16NonObjCSuperInitcfMS0_FT_S0_ : $@cc(method) @thin (@owned NonObjCSuperInit) -> @owned NonObjCSuperInit
  init() {
    // CHECK: function_ref @_TFC10objc_super6WotsitcfMS0_FT4nopeGVS_9NotInObjCSi__S0_ : $@cc(method) @thin (NotInObjC<Int>, @owned Wotsit) -> @owned Wotsit
    super.init(nope: NotInObjC<Int>())
  }
}
