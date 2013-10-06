// RUN: %swift -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs -emit-silgen %s | FileCheck %s

import gizmo

// TODO: Generic base classes

// Test for compilation order independence
class C : B {
  // foo inherited from B
  func bar() {}
  // bas inherited from A
  func qux() {}

  // zim inherited from B
  func zang() {}

  func flopsy() {}
  func mopsy() {}
}
// CHECK: sil_vtable C {
// CHECK:   #B.foo!1: _TC7vtables1B3foofS0_FT_T_
// CHECK:   #C.bar!1: _TC7vtables1C3barfS0_FT_T_
// CHECK:   #A.bas!1: _TC7vtables1A3basfS0_FT_T_
// CHECK:   #C.qux!1: _TC7vtables1C3quxfS0_FT_T_
// CHECK:   #B.zim!1: _TC7vtables1B3zimfS0_FT_T_
// CHECK:   #C.zang!1: _TC7vtables1C4zangfS0_FT_T_
// CHECK:   #C.flopsy!1: _TC7vtables1C6flopsyfS0_FT_T_
// CHECK:   #C.mopsy!1: _TC7vtables1C5mopsyfS0_FT_T_
// CHECK: }

class A {
  func foo() {}
  func bar() {}
  func bas() {}
  func qux() {}
}

// CHECK: sil_vtable A {
// CHECK:   #A.foo!1: _TC7vtables1A3foofS0_FT_T_
// CHECK:   #A.bar!1: _TC7vtables1A3barfS0_FT_T_
// CHECK:   #A.bas!1: _TC7vtables1A3basfS0_FT_T_
// CHECK:   #A.qux!1: _TC7vtables1A3quxfS0_FT_T_
// CHECK: }

class B : A {
  func foo() {}
  // bar inherited from A
  // bas inherited from A
  func qux() {}

  func zim() {}
  func zang() {}
}

// CHECK: sil_vtable B {
// CHECK:   #B.foo!1: _TC7vtables1B3foofS0_FT_T_
// CHECK:   #A.bar!1: _TC7vtables1A3barfS0_FT_T_
// CHECK:   #A.bas!1: _TC7vtables1A3basfS0_FT_T_
// CHECK:   #B.qux!1: _TC7vtables1B3quxfS0_FT_T_
// CHECK:   #B.zim!1: _TC7vtables1B3zimfS0_FT_T_
// CHECK:   #B.zang!1: _TC7vtables1B4zangfS0_FT_T_
// CHECK: }

// Test ObjC base class

class Hoozit : Gizmo {
  // Overrides Gizmo.frob
  func frob() {}
  // Overrides Gizmo.funge
  func funge() {}

  func anse() {}
  func incorrige() {}
}

// Entries only exist for native Swift methods

// CHECK: sil_vtable Hoozit {
// CHECK:   #Hoozit.frob!1: _TCSo6Hoozit4frobfS_FT_T_
// CHECK:   #Hoozit.funge!1: _TCSo6Hoozit5fungefS_FT_T_
// CHECK:   #Hoozit.anse!1: _TCSo6Hoozit4ansefS_FT_T_
// CHECK:   #Hoozit.incorrige!1: _TCSo6Hoozit9incorrigefS_FT_T_
// CHECK: }

class Wotsit : Hoozit {
  func funge() {}
  func incorrige() {}
}

// CHECK: sil_vtable Wotsit {
// CHECK:   #Hoozit.frob!1: _TCSo6Hoozit4frobfS_FT_T_
// CHECK:   #Wotsit.funge!1: _TCSo6Wotsit5fungefS_FT_T_
// CHECK:   #Hoozit.anse!1: _TCSo6Hoozit4ansefS_FT_T_
// CHECK:   #Wotsit.incorrige!1: _TCSo6Wotsit9incorrigefS_FT_T_
// CHECK: }
