// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/accessibility_vtables_helper.swift
// RUN: %target-swift-frontend -emit-silgen -primary-file %s %S/Inputs/accessibility_vtables_other.swift -I %t -module-name accessibility_vtables | FileCheck %s

import accessibility_vtables_helper

class Sub : Base {
  func internalMethod() {}
  override var prop: Int {
    get { return 42 }
    set {}
  }
}

// CHECK-LABEL: sil_vtable Sub {
// CHECK-NEXT:  #Base.internalMethod!1: _TFC28accessibility_vtables_helper4Base14internalMethodfS0_FT_T_
// CHECK-NEXT:  #Base.prop!getter.1: _TFC21accessibility_vtables3Subg4propSi  // accessibility_vtables.Sub.prop.getter : Swift.Int
// CHECK-NEXT:  #Base.prop!setter.1: _TFC28accessibility_vtables_helper4Bases4propSi  // accessibility_vtables_helper.Base.prop.setter : Swift.Int
// CHECK-NEXT:  #Base.prop!materializeForSet.1: _TFC28accessibility_vtables_helper4Basem4propSi  // accessibility_vtables_helper.Base.prop.materializeForSet : Swift.Int
// CHECK-NEXT:  #Base.init!initializer.1: _TFC28accessibility_vtables_helper4BasecfMS0_FT_S0_ // accessibility_vtables_helper.Base.init (accessibility_vtables_helper.Base.Type)() -> accessibility_vtables_helper.Base
// CHECK-NEXT: #Sub.internalMethod!1: _TFC21accessibility_vtables3Sub14internalMethodfS0_FT_T_       // accessibility_vtables.Sub.internalMethod (accessibility_vtables.Sub)() -> ()
// CHECK-NEXT: #Sub.prop!setter.1: _TFC21accessibility_vtables3Subs4propSi   // accessibility_vtables.Sub.prop.setter : Swift.Int
// CHECK-NEXT: #Sub.prop!materializeForSet.1: _TFC21accessibility_vtables3Subm4propSi  // accessibility_vtables.Sub.prop.materializeForSet : Swift.Int
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
// CHECK-NEXT:  #InternalBase.method!1: _TFC21accessibility_vtables12InternalBaseP{{[0-9]+}}[[DISCRIMINATOR:_.+]]6methodfS0_FT_T_     // accessibility_vtables.InternalBase.(method in [[DISCRIMINATOR]]) (accessibility_vtables.InternalBase)() -> ()
// CHECK-NEXT:  #InternalBase.init!initializer.1: _TFC21accessibility_vtables11InternalSubcfMS0_FT_S0_        // accessibility_vtables.InternalSub.init (accessibility_vtables.InternalSub.Type)() -> accessibility_vtables.InternalSub
// CHECK-NEXT:  #InternalBase.prop!getter.1: _TFC21accessibility_vtables11InternalSubg4propSi // accessibility_vtables.InternalSub.prop.getter : Swift.Int
// CHECK-NEXT:  #InternalBase.prop!setter.1: _TFC21accessibility_vtables12InternalBases4propSi        // accessibility_vtables.InternalBase.prop.setter : Swift.Int
// CHECK-NEXT:  #InternalBase.prop!materializeForSet.1: _TFC21accessibility_vtables12InternalBasem4propSi // accessibility_vtables.InternalBase.prop.materializeForSet : Swift.Int
// CHECK-NEXT:  #InternalSub.method!1: _TFC21accessibility_vtables11InternalSub6methodfS0_FT_T_       // accessibility_vtables.InternalSub.method (accessibility_vtables.InternalSub)() -> ()
// CHECK-NEXT:  #InternalSub.prop!setter.1: _TFC21accessibility_vtables11InternalSubs4propSi  // accessibility_vtables.InternalSub.prop.setter : Swift.Int
// CHECK-NEXT:  #InternalSub.prop!materializeForSet.1: _TFC21accessibility_vtables11InternalSubm4propSi // accessibility_vtables.InternalSub.prop.materializeForSet : Swift.Int
// CHECK-NEXT:  #InternalSub.deinit
// CHECK-NEXT: }

