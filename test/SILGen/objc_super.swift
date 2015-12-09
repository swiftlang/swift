// RUN: %target-swift-frontend -use-native-super-method -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -emit-silgen | FileCheck %s

// REQUIRES: objc_interop

import gizmo

// Super calls in objc require dynamic dispatch, represented in SIL as
// a super_method instruction.
class Hoozit : Gizmo {

  // CHECK-LABEL: sil hidden  @_TFC10objc_super6Hoozitc{{.*}} : $@convention(method) (@owned Hoozit) -> @owned Hoozit
  override init() {
    // CHECK: super_method [volatile] {{%.*}} : $Hoozit, #Gizmo.init!initializer.1.foreign
    super.init()
  }

  // CHECK-LABEL: sil hidden  @_TZFC10objc_super6Hoozit5runce{{.*}} : $@convention(thin) (@thick Hoozit.Type) -> ()
  override class func runce() {
    // CHECK: super_method [volatile] {{%.*}} : $@thick Hoozit.Type, #Gizmo.runce!1.foreign
    super.runce()
  }

  // CHECK-LABEL: sil hidden  @_TFC10objc_super6Hoozit4frob{{.*}} : $@convention(method) (@guaranteed Hoozit) -> ()
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
  // CHECK-LABEL: sil hidden @_TFC10objc_super16NonObjCSuperInitc{{.*}} : $@convention(method) (@owned NonObjCSuperInit) -> @owned NonObjCSuperInit
  init() {
    // CHECK: super_method {{%[0-9]+}} : $NonObjCSuperInit, #Wotsit.init!initializer.1 : Wotsit.Type -> (nope: NotInObjC<Int>) -> Wotsit , $@convention(method) (NotInObjC<Int>, @owned Wotsit) -> @owned Wotsit
    super.init(nope: NotInObjC<Int>())
  }
}
