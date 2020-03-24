// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir %s | %FileCheck %s

// REQUIRES: OS=macosx


// Make sure we don't emit duplicate method descriptors

// CHECK-NOT: _PROTOCOL_INSTANCE_METHODS_NSObject{{.*}}"\01L_selector_data(conformsToProtocol:)"{{.*}}"\01L_selector_data(conformsToProtocol:)"

// Make sure that extended method types are in sync with entries in method list.
// CHECK: @_PROTOCOL_INSTANCE_METHODS_NSObject = private constant { i32, i32, [5 x
// CHECK: @_PROTOCOL_METHOD_TYPES_NSObject = private constant [5
import Foundation

@objc protocol P: NSObjectProtocol {}
class C: NSObject, P {}
