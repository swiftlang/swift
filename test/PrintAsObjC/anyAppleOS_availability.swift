// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-experimental-feature AnyAppleOSAvailability -typecheck %s -emit-objc-header-path %t/anyAppleOS.h -target arm64-apple-macos26 -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck %s < %t/anyAppleOS.h

// REQUIRES: objc_interop
// REQUIRES: swift_feature_AnyAppleOSAvailability
// REQUIRES: asserts

import Foundation

// CHECK-LABEL: SWIFT_CLASS("{{.*}}AvailableOnAnyAppleOS26{{.*}}")
// CHECK-SAME: SWIFT_AVAILABILITY(macos,introduced=26)
// CHECK: @end

// CHECK-LABEL: SWIFT_CLASS("{{.*}}availability13PropertyClass{{.*}}")
// CHECK: @property{{.*}}propertyWithAvailability
// CHECK-SAME: SWIFT_AVAILABILITY(macos,introduced=26)
// CHECK: @end

// CHECK-LABEL: SWIFT_CLASS("{{.*}}SomeClass{{.*}}")
// CHECK: - (void)methodWithAnyAppleOSAvailability
// CHECK-SAME: SWIFT_AVAILABILITY(macos,introduced=26)
// CHECK: - (void)methodWithMinorVersion
// CHECK-SAME: SWIFT_AVAILABILITY(macos,introduced=26.5)
// CHECK: @end

@objc
@available(anyAppleOS 26, *)
public class AvailableOnAnyAppleOS26: NSObject {
    @objc
    public func someMethod() {
        print("This method is available on anyAppleOS 26+")
    }

    @objc
    public var someProperty: Int = 0
}

@objc
public class SomeClass: NSObject {
    @objc
    @available(anyAppleOS 26, *)
    public func methodWithAnyAppleOSAvailability() {
        print("Method available on anyAppleOS 26+")
    }
    @objc
    @available(anyAppleOS 26.5, *)
    public func methodWithMinorVersion() {
        print("Method available on anyAppleOS 26.5+")
    }
}

@objc
public class PropertyClass: NSObject {
    @objc
    @available(anyAppleOS 26, *)
    public var propertyWithAvailability: String = "test"
}
