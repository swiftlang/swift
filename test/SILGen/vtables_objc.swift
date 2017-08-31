// RUN: %target-swift-frontend -sdk %S/Inputs -emit-silgen -I %S/Inputs -enable-source-import %s -disable-objc-attr-requires-foundation-module | %FileCheck %s

// REQUIRES: objc_interop

import gizmo

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
// CHECK-NEXT:   #Hoozit.anse!1: {{.*}} : _T012vtables_objc6HoozitC4anse{{[_0-9a-zA-Z]*}}F
// CHECK-NEXT:   #Hoozit.incorrige!1: {{.*}} : _T012vtables_objc6HoozitC9incorrige{{[_0-9a-zA-Z]*}}F
// CHECK-NEXT:   #Hoozit.init!initializer.1: (Hoozit.Type) -> () -> Hoozit! : _T012vtables_objc6HoozitCSQyACGycfc
// CHECK-NEXT:   #Hoozit.init!initializer.1: (Hoozit.Type) -> (Int) -> Hoozit! : _T012vtables_objc6HoozitCSQyACGSi7bellsOn_tcfc
// CHECK-NEXT:   #Hoozit.deinit!deallocator: _T012vtables_objc6HoozitCfD
// CHECK-NEXT: }

class Wotsit : Hoozit {
  override func funge() {}
  override func incorrige() {}
}

// CHECK: sil_vtable Wotsit {
// CHECK-NEXT:   #Hoozit.anse!1: {{.*}} : _T012vtables_objc6HoozitC4anse{{[_0-9a-zA-Z]*}}F
// CHECK-NEXT:   #Hoozit.incorrige!1: {{.*}} : _T012vtables_objc6WotsitC9incorrige{{[_0-9a-zA-Z]*}}F
// CHECK-NEXT:   #Hoozit.init!initializer.1: (Hoozit.Type) -> () -> Hoozit! : _T012vtables_objc6WotsitCSQyACGycfc
// CHECK-NEXT:   #Hoozit.init!initializer.1: (Hoozit.Type) -> (Int) -> Hoozit! : _T012vtables_objc6WotsitCSQyACGSi7bellsOn_tcfc
// CHECK-NEXT:   #Wotsit.deinit!deallocator: _T012vtables_objc6WotsitCfD
// CHECK-NEXT: }

// <rdar://problem/15282548>
// CHECK: sil_vtable Base {
// CHECK:   #Base.init!initializer.1: {{.*}} : _T012vtables_objc4BaseC{{[_0-9a-zA-Z]*}}fc
// CHECK: }
// CHECK: sil_vtable Derived {
// CHECK:   #Base.init!initializer.1: {{.*}} : _T012vtables_objc7DerivedC{{[_0-9a-zA-Z]*}}fc
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
