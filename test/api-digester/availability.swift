// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -o %t/Foo.swiftmodule -emit-abi-descriptor-path %t/abi-before.json %S/../Inputs/empty.swift -enable-library-evolution -emit-tbd-path %t/abi-before.tbd -tbd-install_name Foo
// RUN: %target-swift-frontend -emit-module -o %t/Foo.swiftmodule -emit-abi-descriptor-path %t/abi-after.json %s -enable-library-evolution -emit-tbd-path %t/abi-after.tbd -tbd-install_name Foo
// RUN: %api-digester -diagnose-sdk --input-paths %t/abi-before.json -input-paths %t/abi-after.json -abi -o %t/result.txt
// RUN: %FileCheck %s < %t/result.txt

// REQUIRES: OS=macosx

public func noAvailability() { }

@available(macOS, unavailable)
public func unavailableOnMacOS() { }

@available(macOS 10.15, *)
public func onlyMacOSAvailability() { }

@available(macOS 15.0, iOS 18.0, watchOS 11.0, tvOS 18.0, visionOS 2.0, *)
public func allAppleOSAvailability() { }

@available(macOS 9999, iOS 9999, watchOS 999, tvOS 9999, visionOS 9999, *)
public func appleOSPlaceholderAvailability() { }

@available(anyAppleOS 26.0, *)
public func anyAppleOSAvailability() { }

@available(anyAppleOS, unavailable)
public func unavailableOnAnyAppleOS() { }

@available(anyAppleOS 9999, *)
public func anyAppleOSPlaceHolderAvailability() { }

// CHECK-LABEL: /* Decl Attribute changes */
// CHECK-NEXT: Func noAvailability() is a new API without '@available'
// CHECK-NOT: is a new API without
