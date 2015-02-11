// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk -I %S/Inputs/custom-modules) -emit-sil %s -verify
// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk -I %S/Inputs/custom-modules) -emit-sil %s > %t.log 2>&1
// RUN: FileCheck %s < %t.log

// REQUIRES: objc_interop

import Foundation

// rdar://problem/17687082
extension NSObject {
  convenience init() { self.init() } // expected-error{{initializer 'init()' redeclares Objective-C method 'init'}}
}

extension NSObject {
  @objc(class) func foo() { } // expected-error{{method 'foo()' redeclares Objective-C method 'class'}}
}

// CHECK: objc_init_redundant.swift:11:15: error: initializer 'init()' redeclares
// CHECK: ObjectiveC.NSObject{{.*}}note: Objective-C method 'init' previously 
