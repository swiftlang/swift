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

// CHECK-LABEL: sil hidden @_TFC21accessibility_vtables3SubcfT_S0_ : $@convention(method) (@owned Sub) -> @owned Sub
// CHECK:       bb0(%0 : $Sub):
// CHECK:         function_ref @_TFs25_unimplementedInitializerFT9classNameVs12StaticString8initNameS_4fileS_4lineSu6columnSu_Os5Never

// CHECK-LABEL: sil_vtable Sub {
// CHECK-NEXT:  #Base.internalMethod!1: {{.*}} : _TFC28accessibility_vtables_helper4Base14internalMethod
// CHECK-NEXT:  #Base.prop!getter.1: {{.*}} : _TFC21accessibility_vtables3Subg4propSi  // accessibility_vtables.Sub.prop.getter : Swift.Int
// CHECK-NEXT:  #Base.prop!setter.1: {{.*}} : _TFC28accessibility_vtables_helper4Bases4propSi  // accessibility_vtables_helper.Base.prop.setter : Swift.Int
// CHECK-NEXT:  #Base.prop!materializeForSet.1: {{.*}} : _TFC28accessibility_vtables_helper4Basem4propSi  // accessibility_vtables_helper.Base.prop.materializeForSet : Swift.Int
// CHECK-NEXT:  #Base.init!initializer.1: {{.*}} : _TFC21accessibility_vtables3SubcfT_S0_  // accessibility_vtables.Sub.init () -> accessibility_vtables.Sub
// CHECK-NEXT: #Sub.internalMethod!1: {{.*}} : _TFC21accessibility_vtables3Sub14internalMethod
// CHECK-NEXT: #Sub.prop!setter.1: {{.*}} : _TFC21accessibility_vtables3Subs4propSi   // accessibility_vtables.Sub.prop.setter : Swift.Int
// CHECK-NEXT: #Sub.prop!materializeForSet.1: {{.*}} : _TFC21accessibility_vtables3Subm4propSi  // accessibility_vtables.Sub.prop.materializeForSet : Swift.Int
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
// CHECK-NEXT:  #InternalBase.method!1: {{.*}} : _TFC21accessibility_vtables12InternalBaseP{{[0-9]+}}[[DISCRIMINATOR:_.+]]6method
// CHECK-NEXT:  #InternalBase.prop!getter.1: {{.*}} : _TFC21accessibility_vtables11InternalSubg4propSi // accessibility_vtables.InternalSub.prop.getter : Swift.Int
// CHECK-NEXT:  #InternalBase.prop!setter.1: {{.*}} : _TFC21accessibility_vtables12InternalBases4propSi        // accessibility_vtables.InternalBase.prop.setter : Swift.Int
// CHECK-NEXT:  #InternalBase.prop!materializeForSet.1: {{.*}} : _TFC21accessibility_vtables12InternalBasem4propSi // accessibility_vtables.InternalBase.prop.materializeForSet : Swift.Int
// CHECK-NEXT:  #InternalBase.init!initializer.1: {{.*}} : _TFC21accessibility_vtables11InternalSubc
// CHECK-NEXT:  #InternalSub.method!1: {{.*}} : _TFC21accessibility_vtables11InternalSub6method
// CHECK-NEXT:  #InternalSub.prop!setter.1: {{.*}} : _TFC21accessibility_vtables11InternalSubs4propSi  // accessibility_vtables.InternalSub.prop.setter : Swift.Int
// CHECK-NEXT:  #InternalSub.prop!materializeForSet.1: {{.*}} : _TFC21accessibility_vtables11InternalSubm4propSi // accessibility_vtables.InternalSub.prop.materializeForSet : Swift.Int
// CHECK-NEXT:  #InternalSub.deinit
// CHECK-NEXT: }

