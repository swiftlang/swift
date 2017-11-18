// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-sil-ownership %S/Inputs/objc_required_designated_init_2.swift -module-name Booms -o %t/Booms.swiftmodule -import-objc-header %S/Inputs/objc_required_designated_init.h
// RUN: %target-swift-frontend -I %t -emit-silgen -enable-sil-ownership -verify %s -import-objc-header %S/Inputs/objc_required_designated_init.h | %FileCheck %s
// RUN: %target-swift-frontend -I %t -emit-ir %s -import-objc-header %S/Inputs/objc_required_designated_init.h

// REQUIRES: objc_interop

import Booms

class Baboom : Boom {
  @objc dynamic required init() {
    super.init()
  }
}

class BigBadaBoom<V> : Badaboom<V> {
  required init() {
    super.init()
  }
}

class Root {
  @objc dynamic required init() {}
}

// CHECK-LABEL: sil_vtable Baboom {
// CHECK:   #Boom.init!allocator.1: (Boom.Type) -> () -> Boom : _T029objc_required_designated_init6BaboomCACycfC [override]
// CHECK:   #Baboom.deinit!deallocator: _T029objc_required_designated_init6BaboomCfD
// CHECK: }

// CHECK-LABEL: sil_vtable BigBadaBoom {
// CHECK:   #Badaboom.init!allocator.1: <U> (Badaboom<U>.Type) -> () -> Badaboom<U> : _T029objc_required_designated_init11BigBadaBoomCACyxGycfC [override]
// CHECK:   #BigBadaBoom.deinit!deallocator: _T029objc_required_designated_init11BigBadaBoomCfD
// CHECK: }

// CHECK-LABEL: sil_vtable Root {
// CHECK:   #Root.init!allocator.1: (Root.Type) -> () -> Root : _T029objc_required_designated_init4RootCACycfC
// CHECK:   #Root.deinit!deallocator: _T029objc_required_designated_init4RootCfD
// CHECK: }
