// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -emit-module -o %t %S/Inputs/accessibility_vtables_helper.swift
// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -emit-silgen -primary-file %s %S/Inputs/accessibility_vtables_other.swift -I %t -module-name accessibility_vtables | %FileCheck %s

import accessibility_vtables_helper

class Sub : Base {
  func internalMethod() {}
  override var prop: Int {
    get { return 42 }
    set {}
  }
}

// CHECK-LABEL: sil hidden @_T021accessibility_vtables3SubCACycfc : $@convention(method) (@owned Sub) -> @owned Sub
// CHECK:       bb0(%0 : $Sub):
// CHECK:         function_ref @_T0s25_unimplementedInitializers5NeverOs12StaticStringV9className_AE04initG0AE4fileSu4lineSu6columntF

// CHECK-LABEL: sil_vtable Sub {
// CHECK-NEXT:  #Base.internalMethod!1: {{.*}} : _T028accessibility_vtables_helper4BaseC14internalMethodyyF
// CHECK-NEXT:  #Base.prop!getter.1: {{.*}} : _T021accessibility_vtables3SubC4propSifg  // accessibility_vtables.Sub.prop.getter : Swift.Int
// CHECK-NEXT:  #Base.prop!setter.1: {{.*}} : _T028accessibility_vtables_helper4BaseC4propSifs  // accessibility_vtables_helper.Base.prop.setter : Swift.Int
// CHECK-NEXT:  #Base.prop!materializeForSet.1: {{.*}} : _T028accessibility_vtables_helper4BaseC4propSifm  // accessibility_vtables_helper.Base.prop.materializeForSet : Swift.Int
// CHECK-NEXT:  #Base.init!initializer.1: {{.*}} : _T021accessibility_vtables3SubCACycfc  // accessibility_vtables.Sub.init () -> accessibility_vtables.Sub
// CHECK-NEXT: #Sub.internalMethod!1: {{.*}} : _T021accessibility_vtables3SubC14internalMethodyyF
// CHECK-NEXT: #Sub.prop!setter.1: {{.*}} : _T021accessibility_vtables3SubC4propSifs   // accessibility_vtables.Sub.prop.setter : Swift.Int
// CHECK-NEXT: #Sub.prop!materializeForSet.1: {{.*}} : _T021accessibility_vtables3SubC4propSifm  // accessibility_vtables.Sub.prop.materializeForSet : Swift.Int
// CHECK-NEXT: #Sub.deinit
// CHECK-NEXT: }

class InternalSub : InternalBase {
  func method() {}
  override var prop: Int {
    get { return 42 }
    set {}
  }
}

// CHECK-LABEL: sil_vtable InternalSub {
// CHECK-NEXT:  #InternalBase.method!1: {{.*}} : _T021accessibility_vtables12InternalBaseC6method{{[0-9]+}}[[DISCRIMINATOR:_.+]]
// CHECK-NEXT:  #InternalBase.prop!getter.1: {{.*}} : _T021accessibility_vtables11InternalSubC4propSifg // accessibility_vtables.InternalSub.prop.getter : Swift.Int
// CHECK-NEXT:  #InternalBase.prop!setter.1: {{.*}} : _T021accessibility_vtables12InternalBaseC4propSifs        // accessibility_vtables.InternalBase.prop.setter : Swift.Int
// CHECK-NEXT:  #InternalBase.prop!materializeForSet.1: {{.*}} : _T021accessibility_vtables12InternalBaseC4propSifm // accessibility_vtables.InternalBase.prop.materializeForSet : Swift.Int
// CHECK-NEXT:  #InternalBase.init!initializer.1: {{.*}} : _T021accessibility_vtables11InternalSubCACycfc
// CHECK-NEXT:  #InternalSub.method!1: {{.*}} : _T021accessibility_vtables11InternalSubC6methodyyF
// CHECK-NEXT:  #InternalSub.prop!setter.1: {{.*}} : _T021accessibility_vtables11InternalSubC4propSifs  // accessibility_vtables.InternalSub.prop.setter : Swift.Int
// CHECK-NEXT:  #InternalSub.prop!materializeForSet.1: {{.*}} : _T021accessibility_vtables11InternalSubC4propSifm // accessibility_vtables.InternalSub.prop.materializeForSet : Swift.Int
// CHECK-NEXT:  #InternalSub.deinit
// CHECK-NEXT: }

