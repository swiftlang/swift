/// Check that @objc on a top-level function (SE-0495) is printed into the
/// Objective-C compatibility header, using Objective-C types. Bridged types
/// (String, Array, Dictionary) are printed as their Objective-C counterparts.

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) \
// RUN:   %s -emit-module -o %t -emit-objc-header-path %t/objc_global_func.h

// RUN: %FileCheck %s --input-file %t/objc_global_func.h
// RUN: %check-in-clang %t/objc_global_func.h

// REQUIRES: objc_interop

import Foundation

// CHECK-DAG: SWIFT_EXTERN void basic(void) SWIFT_NOEXCEPT;
@objc func basic() {}

// The C symbol name comes from the @objc(name) argument.
// CHECK-DAG: SWIFT_EXTERN NSInteger customName(void) SWIFT_NOEXCEPT SWIFT_WARN_UNUSED_RESULT;
@objc(customName) func named() -> Int { 0 }

// Objective-C class types are printed with their Objective-C spelling.
// CHECK-DAG: SWIFT_EXTERN NSObject * _Nonnull takesObject(NSObject * _Nonnull x) SWIFT_NOEXCEPT SWIFT_WARN_UNUSED_RESULT;
@objc(takesObject) func takesObject(_ x: NSObject) -> NSObject { x }

// Bridged types: String prints as NSString.
// CHECK-DAG: SWIFT_EXTERN NSString * _Nonnull greet(NSString * _Nonnull name) SWIFT_NOEXCEPT SWIFT_WARN_UNUSED_RESULT;
@objc(greet) func greet(name: String) -> String { "" }

// Bridged collection: [NSObject] prints as NSArray<NSObject *>.
// CHECK-DAG: SWIFT_EXTERN NSInteger countItems(NSArray<NSObject *> * _Nonnull items) SWIFT_NOEXCEPT SWIFT_WARN_UNUSED_RESULT;
@objc(countItems) func countItems(items: [NSObject]) -> Int { 0 }

// Bridged dictionary: [String: NSObject] prints as NSDictionary<NSString *, NSObject *>.
// CHECK-DAG: SWIFT_EXTERN NSObject * _Nullable lookup(NSDictionary<NSString *, NSObject *> * _Nonnull dict) SWIFT_NOEXCEPT SWIFT_WARN_UNUSED_RESULT;
@objc(lookup) func lookup(dict: [String: NSObject]) -> NSObject? { nil }
