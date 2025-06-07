
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name vtables_objc -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -disable-objc-attr-requires-foundation-module | %FileCheck %s

// REQUIRES: objc_interop

import gizmo

// Test ObjC base class

class Hoozit : Gizmo {
  // Overrides Gizmo.frob
  override func frob() {}

  // Overrides Gizmo.funge
  override func funge() {}

  // Overrides Gizmo.foo
  final override func foo() {}

  func anse() {}
  func incorrige() {}
}

// CHECK-LABEL: sil hidden [ossa] @$s12vtables_objc10callHoozityyAA0D0CF : $@convention(thin) (@guaranteed Hoozit) -> ()
func callHoozit(_ h: Hoozit) {
  // CHECK: objc_method {{.*}} : $Hoozit, #Hoozit.frob!foreign
  h.frob()
  // CHECK: function_ref @$s12vtables_objc6HoozitC3fooyyF
  h.foo()
  // CHECK: class_method {{.*}} : $Hoozit, #Hoozit.anse :
  h.anse()
  // CHECK: return
}

class Wotsit : Hoozit {
  // Overrides Gizmo.funge
  override func funge() {}

  // Overrides Hoozit.incorrige
  override func incorrige() {}

  // Overrides Gizmo.frob
  final override func frob() {}
}

// CHECK-LABEL: sil hidden [ossa] @$s12vtables_objc10callWotsityyAA0D0CF : $@convention(thin) (@guaranteed Wotsit) -> ()
func callWotsit(_ w: Wotsit) {
  // CHECK: objc_method {{.*}} : $Wotsit, #Wotsit.funge!foreign
  w.funge()
  // CHECK: class_method {{.*}} : $Wotsit, #Wotsit.incorrige :
  w.incorrige()
  // CHECK: function_ref @$s12vtables_objc6WotsitC4frobyyF
  w.frob()
  // CHECK: return
}

// Entries only exist for native Swift methods

// CHECK: sil_vtable Hoozit {
// CHECK-NEXT:   #Hoozit.anse: {{.*}} : @$s12vtables_objc6HoozitC4anse{{[_0-9a-zA-Z]*}}F
// CHECK-NEXT:   #Hoozit.incorrige: {{.*}} : @$s12vtables_objc6HoozitC9incorrige{{[_0-9a-zA-Z]*}}F
// CHECK-NEXT:   #Hoozit.deinit!deallocator: @$s12vtables_objc6HoozitCfD
// CHECK-NEXT: }

// CHECK: sil_vtable Wotsit {
// CHECK-NEXT:   #Hoozit.anse: {{.*}} : @$s12vtables_objc6HoozitC4anse{{[_0-9a-zA-Z]*}}F
// CHECK-NEXT:   #Hoozit.incorrige: {{.*}} : @$s12vtables_objc6WotsitC9incorrige{{[_0-9a-zA-Z]*}}F
// CHECK-NEXT:   #Wotsit.deinit!deallocator: @$s12vtables_objc6WotsitCfD
// CHECK-NEXT: }

// <rdar://problem/15282548>
// CHECK: sil_vtable Base {
// CHECK:   #Base.init!allocator: {{.*}} : @$s12vtables_objc4BaseC{{[_0-9a-zA-Z]*}}fC
// CHECK: }
// CHECK: sil_vtable Derived {
// CHECK:   #Base.init!allocator: {{.*}} : @$s12vtables_objc7DerivedC{{[_0-9a-zA-Z]*}}fC
// CHECK: }
@objc class Base {}

extension Base {
  // note: does not have a vtable slot, because it is from an extension
  @objc func identify() -> Int {
    return 0
  }
}

class Derived : Base {
  override func identify() -> Int {
    return 1
  }
}
