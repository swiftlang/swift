// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-sil-ownership %S/Inputs/objc_required_designated_init_2.swift -module-name Booms -o %t/Booms.swiftmodule -import-objc-header %S/Inputs/objc_required_designated_init.h
// RUN: %target-swift-emit-silgen -I %t -enable-sil-ownership -verify %s -import-objc-header %S/Inputs/objc_required_designated_init.h | %FileCheck %s
// RUN: %target-swift-emit-ir -I %t %s -import-objc-header %S/Inputs/objc_required_designated_init.h

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

// Because these init() hierarchies are all rooted in overriding
// -[NSObject init], they're all dynamic whether explicitly declared so or not,
// so do not appear in the vtable.

// CHECK-LABEL: sil_vtable Baboom {
// CHECK-NOT: #Boom.init!allocator.1: (Boom.Type) -> () -> Boom : @$s29objc_required_designated_init6BaboomCACycfC [override]
// CHECK:     #Baboom.deinit!deallocator.1: @$s29objc_required_designated_init6BaboomCfD
// CHECK: }

// CHECK-LABEL: sil_vtable BigBadaBoom {
// CHECK-NOT: #Badaboom.init!allocator.1
// CHECK:     #BigBadaBoom.deinit!deallocator.1: @$s29objc_required_designated_init11BigBadaBoomCfD
// CHECK: }

// CHECK-LABEL: sil_vtable Root {
// CHECK-NOT: #Root.init!allocator.1
// CHECK:     #Root.deinit!deallocator.1: @$s29objc_required_designated_init4RootCfD
// CHECK: }
