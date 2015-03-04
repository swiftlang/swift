// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk -I %S/Inputs/custom-modules) -emit-sil %s -verify
// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk -I %S/Inputs/custom-modules) -emit-sil %s > %t.log 2>&1
// RUN: FileCheck %s < %t.log

// REQUIRES: objc_interop

import Foundation

// rdar://problem/17687082
extension NSObject {
  convenience init() { self.init() } // expected-error{{initializer 'init()' with Objective-C selector 'init' conflicts with previous declaration with the same Objective-C selector}}
// CHECK: objc_init_redundant.swift:[[@LINE-1]]:15: error: initializer 'init()' with Objective-C selector 'init' conflicts
// CHECK: ObjectiveC.NSObject{{.*}}note: 'init' previously declared here
}

extension NSObject {
  @objc(class) func foo() { } // expected-error{{method 'foo()' with Objective-C selector 'class' conflicts with method 'class()' with the same Objective-C selector}}
// CHECK: objc_init_redundant.swift:[[@LINE-1]]:21: error: method 'foo()' with Objective-C selector 'class' conflicts
// CHECK: ObjectiveC.NSObject{{.*}}note: method 'class()' declared here
}

