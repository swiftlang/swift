// RUN: %target-swift-frontend -sdk %S/Inputs -emit-silgen -I %S/Inputs -enable-source-import %s -disable-objc-attr-requires-foundation-module | %FileCheck %s

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
// CHECK:   #A.foo!1: {{.*}} : _T07vtables1BC3foo{{[_0-9a-zA-Z]*}}F
// CHECK:   #A.bar!1: {{.*}} : _T07vtables1CC3bar{{[_0-9a-zA-Z]*}}F
// CHECK:   #A.bas!1: {{.*}} : _T07vtables1AC3bas{{[_0-9a-zA-Z]*}}F
// CHECK:   #A.qux!1: {{.*}} : _T07vtables1CC3qux{{[_0-9a-zA-Z]*}}F
// CHECK:   #B.init!allocator.1: {{.*}} : _T07vtables1CC{{[_0-9a-zA-Z]*}}fC
// CHECK:   #B.init!initializer.1: {{.*}} : _T07vtables1CC{{[_0-9a-zA-Z]*}}fc
// CHECK:   #B.zim!1: {{.*}} : _T07vtables1BC3zim{{[_0-9a-zA-Z]*}}F
// CHECK:   #B.zang!1: {{.*}} : _T07vtables1CC4zang{{[_0-9a-zA-Z]*}}F
// CHECK:   #C.flopsy!1: {{.*}} : _T07vtables1CC6flopsy{{[_0-9a-zA-Z]*}}F
// CHECK:   #C.mopsy!1: {{.*}} : _T07vtables1CC5mopsy{{[_0-9a-zA-Z]*}}F
// CHECK: }

class A {
  func foo() {}
  func bar() {}
  func bas() {}
  func qux() {}
}

// CHECK: sil_vtable A {
// CHECK:   #A.foo!1: {{.*}} : _T07vtables1AC3foo{{[_0-9a-zA-Z]*}}F
// CHECK:   #A.bar!1: {{.*}} : _T07vtables1AC3bar{{[_0-9a-zA-Z]*}}F
// CHECK:   #A.bas!1: {{.*}} : _T07vtables1AC3bas{{[_0-9a-zA-Z]*}}F
// CHECK:   #A.qux!1: {{.*}} : _T07vtables1AC3qux{{[_0-9a-zA-Z]*}}F
// CHECK:   #A.init!initializer.1: {{.*}} : _T07vtables1AC{{[_0-9a-zA-Z]*}}fc
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
// CHECK:   #A.foo!1: {{.*}} : _T07vtables1BC3foo{{[_0-9a-zA-Z]*}}F
// CHECK:   #A.bar!1: {{.*}} : _T07vtables1AC3bar{{[_0-9a-zA-Z]*}}F
// CHECK:   #A.bas!1: {{.*}} : _T07vtables1AC3bas{{[_0-9a-zA-Z]*}}F
// CHECK:   #A.qux!1: {{.*}} : _T07vtables1BC3qux{{[_0-9a-zA-Z]*}}F
// CHECK:   #B.init!allocator.1: {{.*}} : _T07vtables1BC{{[_0-9a-zA-Z]*}}fC
// CHECK:   #B.init!initializer.1: {{.*}} : _T07vtables1BC{{[_0-9a-zA-Z]*}}fc
// CHECK:   #B.zim!1: {{.*}} : _T07vtables1BC3zim{{[_0-9a-zA-Z]*}}F
// CHECK:   #B.zang!1: {{.*}} : _T07vtables1BC4zang{{[_0-9a-zA-Z]*}}F
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
// CHECK:   #Hoozit.frob!1: {{.*}} : _T07vtables6HoozitC4frob{{[_0-9a-zA-Z]*}}F
// CHECK:   #Hoozit.funge!1: {{.*}} : _T07vtables6HoozitC5funge{{[_0-9a-zA-Z]*}}F
// CHECK:   #Hoozit.anse!1: {{.*}} : _T07vtables6HoozitC4anse{{[_0-9a-zA-Z]*}}F
// CHECK:   #Hoozit.incorrige!1: {{.*}} : _T07vtables6HoozitC9incorrige{{[_0-9a-zA-Z]*}}F
// CHECK: }

class Wotsit : Hoozit {
  override func funge() {}
  override func incorrige() {}
}

// CHECK: sil_vtable Wotsit {
// CHECK:   #Hoozit.frob!1: {{.*}} : _T07vtables6HoozitC4frob{{[_0-9a-zA-Z]*}}F
// CHECK:   #Hoozit.funge!1: {{.*}} : _T07vtables6WotsitC5funge{{[_0-9a-zA-Z]*}}F
// CHECK:   #Hoozit.anse!1: {{.*}} : _T07vtables6HoozitC4anse{{[_0-9a-zA-Z]*}}F
// CHECK:   #Hoozit.incorrige!1: {{.*}} : _T07vtables6WotsitC9incorrige{{[_0-9a-zA-Z]*}}F
// CHECK: }

// <rdar://problem/15282548>
// CHECK: sil_vtable Base {
// CHECK:   #Base.init!initializer.1: {{.*}} : _T07vtables4BaseC{{[_0-9a-zA-Z]*}}fc
// CHECK: }
// CHECK: sil_vtable Derived {
// CHECK:   #Base.init!initializer.1: {{.*}} : _T07vtables7DerivedC{{[_0-9a-zA-Z]*}}fc
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
// CHECK-NEXT: #SimpleInitBase.init!initializer.1: {{.*}} : _T07vtables19RequiredInitDerivedC{{[_0-9a-zA-Z]*}}fc
// CHECK-NEXT  #RequiredInitDerived.init!allocator.1: {{.*}} : _TFC7vtables19RequiredInitDerivedC
// CHECK-NEXT}

class SimpleInitBase { }

class RequiredInitDerived : SimpleInitBase {
  required override init() { }
}

class Observed {
  var x: Int = 0 {
    didSet {
    }
    willSet {
    }
  }
}

// rdar://problem/21298214
class BaseWithDefaults {
   func a(_ object: AnyObject? = nil) {}
}

class DerivedWithoutDefaults : BaseWithDefaults {
   override func a(_ object: AnyObject?) { 
     super.a(object)   
   }
}


// CHECK-LABEL: sil_vtable Observed {
// CHECK-NOT:     #Observed.x!didSet
// CHECK-NOT:     #Observed.x!willSet
// CHECK:         #Observed.x!getter
// CHECK:         #Observed.x!setter

// CHECK-LABEL: sil_vtable DerivedWithoutDefaults {
// CHECK:         #BaseWithDefaults.a!1: {{.*}} : _T07vtables22DerivedWithoutDefaultsC1a{{[_0-9a-zA-Z]*}}F
