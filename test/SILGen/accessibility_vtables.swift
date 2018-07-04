// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -emit-module -o %t %S/Inputs/accessibility_vtables_helper.swift
// RUN: %target-swift-emit-silgen -Xllvm -sil-full-demangle -primary-file %s %S/Inputs/accessibility_vtables_other.swift -I %t -module-name accessibility_vtables | %FileCheck %s

import accessibility_vtables_helper

class Sub : Base {
  func internalMethod() {}
  override var prop: Int {
    get { return 42 }
    set {}
  }
}

// CHECK-LABEL: sil hidden @$S21accessibility_vtables3SubCACycfc : $@convention(method) (@owned Sub) -> @owned Sub
// CHECK:       bb0(%0 : $Sub):
// CHECK:         function_ref @$Ss25_unimplementedInitializer9className04initD04file4line6columns5NeverOs12StaticStringV_A2JS2utF

// CHECK-LABEL: sil_vtable Sub {
// CHECK-NEXT:  #Base.internalMethod!1: {{.*}} : @$S28accessibility_vtables_helper4BaseC14internalMethodyyF [inherited]
// CHECK-NEXT:  #Base.prop!getter.1: {{.*}} : @$S21accessibility_vtables3SubC4propSivg [override]  // accessibility_vtables.Sub.prop.getter : Swift.Int
// CHECK-NEXT:  #Base.prop!setter.1: {{.*}} : @$S28accessibility_vtables_helper4BaseC4propSivs [inherited]  // accessibility_vtables_helper.Base.prop.setter : Swift.Int
// CHECK-NEXT:  #Base.prop!materializeForSet.1: {{.*}} : @$S28accessibility_vtables_helper4BaseC4propSivm [inherited]  // accessibility_vtables_helper.Base.prop.materializeForSet : Swift.Int
// CHECK-NEXT:  #Base.init!initializer.1: {{.*}} : @$S21accessibility_vtables3SubCACycfc [override]  // accessibility_vtables.Sub.init() -> accessibility_vtables.Sub
// CHECK-NEXT: #Sub.internalMethod!1: {{.*}} : @$S21accessibility_vtables3SubC14internalMethodyyF
// CHECK-NEXT: #Sub.prop!setter.1: {{.*}} : @$S21accessibility_vtables3SubC4propSivs   // accessibility_vtables.Sub.prop.setter : Swift.Int
// CHECK-NEXT: #Sub.prop!materializeForSet.1: {{.*}} : @$S21accessibility_vtables3SubC4propSivm  // accessibility_vtables.Sub.prop.materializeForSet : Swift.Int
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
// CHECK-NEXT:  #InternalBase.method!1: {{.*}} : @$S21accessibility_vtables12InternalBaseC6method{{[0-9]+}}[[DISCRIMINATOR:_.+]] [inherited]
// CHECK-NEXT:  #InternalBase.prop!getter.1: {{.*}} : @$S21accessibility_vtables11InternalSubC4propSivg [override] // accessibility_vtables.InternalSub.prop.getter : Swift.Int
// CHECK-NEXT:  #InternalBase.prop!setter.1: {{.*}} : @$S21accessibility_vtables12InternalBaseC4propSivs [inherited]        // accessibility_vtables.InternalBase.prop.setter : Swift.Int
// CHECK-NEXT:  #InternalBase.prop!materializeForSet.1: {{.*}} : @$S21accessibility_vtables12InternalBaseC4propSivm [inherited] // accessibility_vtables.InternalBase.prop.materializeForSet : Swift.Int
// CHECK-NEXT:  #InternalBase.init!initializer.1: {{.*}} : @$S21accessibility_vtables11InternalSubCACycfc [override]
// CHECK-NEXT:  #InternalSub.method!1: {{.*}} : @$S21accessibility_vtables11InternalSubC6methodyyF
// CHECK-NEXT:  #InternalSub.prop!setter.1: {{.*}} : @$S21accessibility_vtables11InternalSubC4propSivs  // accessibility_vtables.InternalSub.prop.setter : Swift.Int
// CHECK-NEXT:  #InternalSub.prop!materializeForSet.1: {{.*}} : @$S21accessibility_vtables11InternalSubC4propSivm // accessibility_vtables.InternalSub.prop.materializeForSet : Swift.Int
// CHECK-NEXT:  #InternalSub.deinit
// CHECK-NEXT: }

