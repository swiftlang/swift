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

// CHECK-LABEL: sil_vtable Sub {
// CHECK-NEXT:  #Base.internalMethod: {{.*}} : @$s28accessibility_vtables_helper4BaseC14internalMethodyyF [inherited]
// CHECK-NEXT:  #Base.prop!getter: {{.*}} : @$s21accessibility_vtables3SubC4propSivg [override]  // accessibility_vtables.Sub.prop.getter : Swift.Int
// CHECK-NEXT:  #Base.prop!setter: {{.*}} : @$s28accessibility_vtables_helper4BaseC4propSivs [inherited]  // accessibility_vtables_helper.Base.prop.setter : Swift.Int
// CHECK-NEXT:  #Base.prop!modify: {{.*}} : @$s28accessibility_vtables_helper4BaseC4propSivM [inherited]  // accessibility_vtables_helper.Base.prop.modify : Swift.Int
// CHECK-NEXT:  #Base.init!allocator: (Base.Type) -> () -> Base : @$s28accessibility_vtables_helper4BaseCACycfC [inherited] // accessibility_vtables_helper.Base.__allocating_init() -> accessibility_vtables_helper.Base
// CHECK-NEXT:  #Sub.internalMethod: {{.*}} : @$s21accessibility_vtables3SubC14internalMethodyyF
// CHECK-NEXT:  #Sub.prop!setter: {{.*}} : @$s21accessibility_vtables3SubC4propSivs   // accessibility_vtables.Sub.prop.setter : Swift.Int
// CHECK-NEXT:  #Sub.prop!modify: {{.*}} : @$s21accessibility_vtables3SubC4propSivM  // accessibility_vtables.Sub.prop.modify : Swift.Int
// CHECK-NEXT:  #Sub.deinit
// CHECK-NEXT: }

class InternalSub : InternalBase {
  func method() {}
  override var prop: Int {
    get { return 42 }
    set {}
  }
}

// CHECK-LABEL: sil_vtable InternalSub {
// CHECK-NEXT:  #InternalBase.method: {{.*}} : @$s21accessibility_vtables12InternalBaseC6method{{[0-9]+}}[[DISCRIMINATOR:_.+]] [inherited]
// CHECK-NEXT:  #InternalBase.prop!getter: {{.*}} : @$s21accessibility_vtables11InternalSubC4propSivg [override] // accessibility_vtables.InternalSub.prop.getter : Swift.Int
// CHECK-NEXT:  #InternalBase.prop!setter: {{.*}} : @$s21accessibility_vtables12InternalBaseC4propSivs [inherited]        // accessibility_vtables.InternalBase.prop.setter : Swift.Int
// CHECK-NEXT:  #InternalBase.prop!modify: {{.*}} : @$s21accessibility_vtables12InternalBaseC4propSivM [inherited] // accessibility_vtables.InternalBase.prop.modify : Swift.Int
// CHECK-NEXT:  #InternalBase.init!allocator: {{.*}} : @$s21accessibility_vtables11InternalSubCACycfC [override]
// CHECK-NEXT:  #InternalSub.method: {{.*}} : @$s21accessibility_vtables11InternalSubC6methodyyF
// CHECK-NEXT:  #InternalSub.prop!setter: {{.*}} : @$s21accessibility_vtables11InternalSubC4propSivs  // accessibility_vtables.InternalSub.prop.setter : Swift.Int
// CHECK-NEXT:  #InternalSub.prop!modify: {{.*}} : @$s21accessibility_vtables11InternalSubC4propSivM // accessibility_vtables.InternalSub.prop.modify : Swift.Int
// CHECK-NEXT:  #InternalSub.deinit
// CHECK-NEXT: }

