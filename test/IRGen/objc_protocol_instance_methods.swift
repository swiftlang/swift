// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir %s | %FileCheck %s

// REQUIRES: OS=macosx


// Make sure we don't emit duplicate method descriptors

// CHECK-NOT: _PROTOCOL_INSTANCE_METHODS_NSObject{{.*}}"\01L_selector_data(conformsToProtocol:)"{{.*}}"\01L_selector_data(conformsToProtocol:)"

import Foundation

@objc protocol P: NSObjectProtocol {}
class C: NSObject, P {}
