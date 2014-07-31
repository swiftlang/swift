// RUN: rm -rf %t && mkdir -p %t
// RUN: %swift -emit-module -o %t %S/Inputs/accessibility_vtables_helper.swift
// RUN: %swift -emit-silgen %s -I %t | FileCheck %s

import accessibility_vtables_helper

class Sub : Base {
  func internalMethod() {}
  override var prop: Int {
    get { return 42 }
    set {}
  }
}

// CHECK-LABEL: sil_vtable Sub {
// CHECK-NEXT:  #Base.prop!getter.1: _TFC21accessibility_vtables3Subg4propSi  // accessibility_vtables.Sub.prop.getter : Swift.Int
// CHECK-NEXT:  #Base.prop!setter.1: _TFC28accessibility_vtables_helper4Bases4propSi  // accessibility_vtables_helper.Base.prop.setter : Swift.Int
// CHECK-NEXT:  #Base.init!initializer.1: _TFC28accessibility_vtables_helper4BasecfMS0_FT_S0_ // accessibility_vtables_helper.Base.init (accessibility_vtables_helper.Base.Type)() -> accessibility_vtables_helper.Base
// CHECK-NEXT: }

