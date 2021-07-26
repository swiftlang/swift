// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/Test.swiftmodule -emit-module-interface-path %t/Test.swiftinterface -module-name Test  %s
// RUN: %FileCheck %s < %t/Test.swiftinterface
// RUN: %target-swift-frontend -typecheck-module-from-interface -module-name Test %t/Test.swiftinterface 

// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/Test.swiftmodule -disable-objc-attr-requires-foundation-module -emit-module-interface-path %t/TestFromModule.swiftinterface -module-name Test 
// RUN: %FileCheck %s < %t/TestFromModule.swiftinterface
// RUN: %target-swift-frontend -typecheck-module-from-interface -module-name Test %t/TestFromModule.swiftinterface

// REQUIRES: concurrency
// REQUIRES: objc_interop

import Foundation

// CHECK-LABEL: @objc @_inheritsConvenienceInitializers
// CHECK: public actor SomeActor : ObjectiveC.NSObject {
// CHECK: @objc override public init()
@available(macOS 12.0, iOS 15.0, watchOS 8.0, tvOS 15.0, *)
public actor SomeActor: NSObject {
}
