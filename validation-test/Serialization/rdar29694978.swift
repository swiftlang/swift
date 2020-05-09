// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -import-objc-header %S/Inputs/rdar29694978.h -emit-module -o %t/Library.swiftmodule
// RUN: %target-swift-ide-test -print-module -module-to-print=Library -source-filename=x -I %S/Inputs/ -I %t | %FileCheck %s

// REQUIRES: objc_interop

// This would be wonderful to fold into a larger test, but I'm worried that
// trying to do so would perturb the original conditions that cause it to
// crash: deserializing a typealias (1) to an imported Objective-C type (2)
// that has a protocol (3) that refines another protocol (4) which causes us
// to load all extensions, including one defined in this file (5) in terms of
// the original typealias (1).
//
// It's probably best to just leave this test about that.

import Foundation

// CHECK-DAG: typealias MyNonGenericType = NonGenericType
typealias MyNonGenericType = NonGenericType
// CHECK-DAG: extension NonGenericType
extension MyNonGenericType {}

// CHECK-DAG: typealias MyGenericType<T> = GenericType<T>
typealias MyGenericType<T: NSObject> = GenericType<T>
// CHECK-DAG: extension MyGenericType where Element : NSObject
extension MyGenericType {}
// CHECK-DAG: extension MyGenericType where Element == NSObject
extension MyGenericType where Element == NSObject {}
