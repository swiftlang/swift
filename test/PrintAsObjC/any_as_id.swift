// Please keep this file in alphabetical order!

// REQUIRES: objc_interop

// RUN: rm -rf %t && mkdir -p %t

// FIXME: BEGIN -enable-source-import hackaround
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/CoreGraphics.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift
// FIXME: END -enable-source-import hackaround

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t %s
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -parse-as-library %t/any_as_id.swiftmodule -typecheck -emit-objc-header-path %t/any_as_id.h

// RUN: %FileCheck %s < %t/any_as_id.h

// RUN: %check-in-clang %t/any_as_id.h

import Foundation


// CHECK-LABEL: SWIFT_CLASS("_TtC9any_as_id11AnyAsIdTest")
// CHECK-NEXT:  @interface AnyAsIdTest : NSObject
class AnyAsIdTest : NSObject {

// CHECK-NEXT:  - (NSArray * _Nonnull)arrayOfAny:(NSArray * _Nonnull)x SWIFT_WARN_UNUSED_RESULT;
  @objc func arrayOfAny(_ x: [Any]) -> [Any] { return x }
// CHECK-NEXT:  - (NSArray * _Nullable)arrayOfAnyPerhaps:(NSArray * _Nonnull)x SWIFT_WARN_UNUSED_RESULT;
  @objc func arrayOfAnyPerhaps(_ x: [Any]) -> [Any]? { return x }

// CHECK-NEXT:  - (NSDictionary * _Nonnull)dictionaryOfAny:(NSDictionary * _Nonnull)x SWIFT_WARN_UNUSED_RESULT;
  @objc func dictionaryOfAny(_ x: [AnyHashable: Any]) -> [AnyHashable: Any] { return x }
// CHECK-NEXT:  - (void)dictionaryOfAnyKeys:(NSDictionary<id <NSCopying>, NSString *> * _Nonnull)x;
  @objc func dictionaryOfAnyKeys(_ x: [AnyHashable: String]) {}
// CHECK-NEXT:  - (NSDictionary * _Nullable)dictionaryOfAnyMayhap:(NSDictionary * _Nonnull)x SWIFT_WARN_UNUSED_RESULT;
  @objc func dictionaryOfAnyMayhap(_ x: [AnyHashable: Any]) -> [AnyHashable: Any]? { return x }
// CHECK-NEXT:  - (void)dictionaryOfAnyValues:(NSDictionary<NSString *, id> * _Nonnull)x;
  @objc func dictionaryOfAnyValues(_ x: [String: Any]) {}

// CHECK-NEXT:  - (id _Nonnull)getAny SWIFT_WARN_UNUSED_RESULT;
  @objc func getAny() -> Any { return 1 as Any }
// CHECK-NEXT: - (id _Nullable)getAnyConstructively SWIFT_WARN_UNUSED_RESULT;
  @objc func getAnyConstructively() -> Any? { return Optional<Any>(1 as Any) }
// CHECK-NEXT: - (id _Nullable)getAnyMaybe SWIFT_WARN_UNUSED_RESULT;
  @objc func getAnyMaybe() -> Any? { return nil }
// CHECK-NEXT: - (id _Nullable)getAnyProbably SWIFT_WARN_UNUSED_RESULT;
  @objc func getAnyProbably() -> Any? { return 1 as Any }

// CHECK-NEXT:  - (id _Nonnull)passThroughAny:(id _Nonnull)x SWIFT_WARN_UNUSED_RESULT;
  @objc func passThroughAny(_ x: Any) -> Any { return x }
// CHECK-NEXT: - (id _Nullable)passThroughAnyMaybe:(id _Nullable)x SWIFT_WARN_UNUSED_RESULT;
  @objc func passThroughAnyMaybe(_ x: Any?) -> Any? { return x }

// CHECK-NEXT: - (void)setOfAny:(NSSet * _Nonnull)x;
  @objc func setOfAny(_ x: Set<AnyHashable>) {}

// CHECK-NEXT:  - (void)takesId:(id _Nonnull)x;
  @objc func takesId(_ x: Any) {}

// CHECK-NEXT: - (id _Nonnull)unwrapAny:(id _Nullable)x SWIFT_WARN_UNUSED_RESULT;
  @objc func unwrapAny(_ x : Any?) -> Any { return x! }

// CHECK-NEXT: - (id _Nullable)wrapAny:(id _Nonnull)x SWIFT_WARN_UNUSED_RESULT;
  @objc func wrapAny(_ x : Any) -> Any? { return x }

// CHECK-NEXT:  - (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
  /* implicit inherited init() */

}
// CHECK-NEXT:  @end

extension NSArray { @objc func forceToExist() {} }

