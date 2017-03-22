// RUN: %target-swift-frontend -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -emit-silgen | %FileCheck %s

// REQUIRES: objc_interop

import gizmo

// Super calls in objc require dynamic dispatch, represented in SIL as
// a super_method instruction.
class Hoozit : Gizmo {

  // CHECK-LABEL: sil hidden  @_T010objc_super6HoozitC{{[_0-9a-zA-Z]*}}fc : $@convention(method) (@owned Hoozit) -> @owned Hoozit
  override init() {
    // CHECK: super_method [volatile] {{%.*}} : $Hoozit, #Gizmo.init!initializer.1.foreign
    super.init()
  }

  // CHECK-LABEL: sil hidden  @_T010objc_super6HoozitC5runce{{[_0-9a-zA-Z]*}}FZ : $@convention(method) (@thick Hoozit.Type) -> ()
  override class func runce() {
    // CHECK: super_method [volatile] {{%.*}} : $@thick Hoozit.Type, #Gizmo.runce!1.foreign
    super.runce()
  }

  // CHECK-LABEL: sil hidden  @_T010objc_super6HoozitC4frob{{[_0-9a-zA-Z]*}}F : $@convention(method) (@guaranteed Hoozit) -> ()
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
  // CHECK-LABEL: sil hidden @_T010objc_super16NonObjCSuperInitC{{[_0-9a-zA-Z]*}}fc : $@convention(method) (@owned NonObjCSuperInit) -> @owned NonObjCSuperInit
  init() {
    // CHECK: function_ref @_T010objc_super9NotInObjCVACyxGycfC : $@convention(method) <τ_0_0> (@thin NotInObjC<τ_0_0>.Type) -> NotInObjC<τ_0_0>
    super.init(nope: NotInObjC<Int>())
  }
}
