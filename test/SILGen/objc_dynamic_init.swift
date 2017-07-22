// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen %s | %FileCheck %s
// REQUIRES: objc_interop

import Foundation

protocol Person {
    init()
}

class Driver: NSObject, Person {
    required override init() {
        super.init()
    }
}

// CHECK-LABEL: sil private [transparent] [thunk] @_T{{.*}}DriverC{{.*}}CTW
// CHECK:         class_method {{%.*}} : $@thick Driver.Type, #Driver.init!allocator.1 :

// CHECK-LABEL: sil_vtable Driver {
// CHECK:         #Driver.init!allocator.1: (Driver.Type) -> () -> Driver : _T{{.*}}DriverC{{.*}}C {{ *}}//
