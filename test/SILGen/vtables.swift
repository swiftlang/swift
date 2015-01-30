// RUN: %target-swift-frontend -sdk %S/Inputs -emit-silgen -I %S/Inputs -enable-source-import %s -disable-objc-attr-requires-foundation-module | FileCheck %s

// FIXME: rdar://problem/19648117 Needs splitting objc parts out
// XFAIL: linux

import gizmo

// TODO: Generic base classes

// Test for compilation order independence
class C : B {
  // foo inherited from B
  override func bar() {}
  // bas inherited from A
  override func qux() {}

  // zim inherited from B
  override func zang() {}

  required init(int i: Int) { }

  func flopsy() {}
  func mopsy() {}
}
// CHECK: sil_vtable C {
// CHECK:   #A.foo!1: _TFC7vtables1B3foofS0_FT_T_
// CHECK:   #A.bar!1: _TFC7vtables1C3barfS0_FT_T_
// CHECK:   #A.bas!1: _TFC7vtables1A3basfS0_FT_T_
// CHECK:   #A.qux!1: _TFC7vtables1C3quxfS0_FT_T_
// CHECK:   #B.init!allocator.1: _TFC7vtables1CCfMS0_FT3intSi_S0_
// CHECK:   #B.init!initializer.1: _TFC7vtables1CcfMS0_FT3intSi_S0_
// CHECK:   #B.zim!1: _TFC7vtables1B3zimfS0_FT_T_
// CHECK:   #B.zang!1: _TFC7vtables1C4zangfS0_FT_T_
// CHECK:   #C.flopsy!1: _TFC7vtables1C6flopsyfS0_FT_T_
// CHECK:   #C.mopsy!1: _TFC7vtables1C5mopsyfS0_FT_T_
// CHECK: }

class A {
  func foo() {}
  func bar() {}
  func bas() {}
  func qux() {}
}

// CHECK: sil_vtable A {
// CHECK:   #A.foo!1: _TFC7vtables1A3foofS0_FT_T_
// CHECK:   #A.bar!1: _TFC7vtables1A3barfS0_FT_T_
// CHECK:   #A.bas!1: _TFC7vtables1A3basfS0_FT_T_
// CHECK:   #A.qux!1: _TFC7vtables1A3quxfS0_FT_T_
// CHECK:   #A.init!initializer.1: _TFC7vtables1AcfMS0_FT_S0_
// CHECK: }

class B : A {
  required init(int i: Int) { }

  override func foo() {}
  // bar inherited from A
  // bas inherited from A
  override func qux() {}

  func zim() {}
  func zang() {}
}

// CHECK: sil_vtable B {
// CHECK:   #A.foo!1: _TFC7vtables1B3foofS0_FT_T_
// CHECK:   #A.bar!1: _TFC7vtables1A3barfS0_FT_T_
// CHECK:   #A.bas!1: _TFC7vtables1A3basfS0_FT_T_
// CHECK:   #A.qux!1: _TFC7vtables1B3quxfS0_FT_T_
// CHECK:   #B.init!allocator.1: _TFC7vtables1BCfMS0_FT3intSi_S0_
// CHECK:   #B.init!initializer.1: _TFC7vtables1BcfMS0_FT3intSi_S0_
// CHECK:   #B.zim!1: _TFC7vtables1B3zimfS0_FT_T_
// CHECK:   #B.zang!1: _TFC7vtables1B4zangfS0_FT_T_
// CHECK: }

// Test ObjC base class

class Hoozit : Gizmo {
  // Overrides Gizmo.frob
  override func frob() {}
  // Overrides Gizmo.funge
  override func funge() {}

  func anse() {}
  func incorrige() {}
}

// Entries only exist for native Swift methods

// CHECK: sil_vtable Hoozit {
// CHECK:   #Hoozit.frob!1: _TFC7vtables6Hoozit4frobfS0_FT_T_
// CHECK:   #Hoozit.funge!1: _TFC7vtables6Hoozit5fungefS0_FT_T_
// CHECK:   #Hoozit.anse!1: _TFC7vtables6Hoozit4ansefS0_FT_T_
// CHECK:   #Hoozit.incorrige!1: _TFC7vtables6Hoozit9incorrigefS0_FT_T_
// CHECK: }

class Wotsit : Hoozit {
  override func funge() {}
  override func incorrige() {}
}

// CHECK: sil_vtable Wotsit {
// CHECK:   #Hoozit.frob!1: _TFC7vtables6Hoozit4frobfS0_FT_T_
// CHECK:   #Hoozit.funge!1: _TFC7vtables6Wotsit5fungefS0_FT_T_
// CHECK:   #Hoozit.anse!1: _TFC7vtables6Hoozit4ansefS0_FT_T_
// CHECK:   #Hoozit.incorrige!1: _TFC7vtables6Wotsit9incorrigefS0_FT_T_
// CHECK: }

// <rdar://problem/15282548>
// CHECK: sil_vtable Base {
// CHECK:   #Base.init!initializer.1: _TFC7vtables4BasecfMS0_FT_S0_
// CHECK: }
// CHECK: sil_vtable Derived {
// CHECK:   #Base.init!initializer.1: _TFC7vtables7DerivedcfMS0_FT_S0_
// CHECK: }
@objc class Base {}

extension Base {
  // note: does not have a vtable slot, because it is from an extension
  func identify() -> Int {
    return 0
  }
}

class Derived : Base {
  override func identify() -> Int {
    return 1
  }
}


// CHECK: sil_vtable RequiredInitDerived {
// CHECK-NEXT: #SimpleInitBase.init!initializer.1: _TFC7vtables19RequiredInitDerivedcfMS0_FT_S0_     // vtables.RequiredInitDerived.init (vtables.RequiredInitDerived.Type)() -> vtables.RequiredInitDerived
// CHECK-NEXT  #RequiredInitDerived.init!allocator.1: _TFC7vtables19RequiredInitDerivedCfMS0_FT_S0_  // vtables.RequiredInitDerived.__allocating_init (vtables.RequiredInitDerived.Type)() -> vtables.RequiredInitDerived
// CHECK-NEXT}

class SimpleInitBase { }

class RequiredInitDerived : SimpleInitBase {
  required override init() { }
}
