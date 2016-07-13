// Please keep this file in alphabetical order!

// REQUIRES: objc_interop

// RUN: rm -rf %t && mkdir %t

// FIXME: BEGIN -enable-source-import hackaround
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/CoreGraphics.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift
// FIXME: END -enable-source-import hackaround

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -enable-id-as-any -emit-module -o %t %s
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -enable-id-as-any -parse-as-library %t/any_as_id.swiftmodule -parse -emit-objc-header-path %t/any_as_id.h

// RUN: FileCheck %s < %t/any_as_id.h

// RUN: %check-in-clang %t/any_as_id.h
// RUN: %check-in-clang -fno-modules -Qunused-arguments %t/any_as_id.h

import Foundation


// CHECK-LABEL: SWIFT_CLASS("_TtC9any_as_id11AnyAsIdTest")
// CHECK-NEXT:  @interface AnyAsIdTest : NSObject
class AnyAsIdTest : NSObject {

// CHECK-NEXT:  - (void)takesId:(id _Nonnull)x;
	func takesId(_ x: Any) {}

// CHECK-NEXT:  - (id _Nonnull)getAny;
  func getAny() -> Any { return 1 as Any }

// CHECK-NEXT:  - (id _Nonnull)passThroughAny:(id _Nonnull)x;
  func passThroughAny(_ x: Any) -> Any { return x }

// CHECK-NEXT: - (id _Nonnull)unwrapAny:(id _Nullable)x;
  func unwrapAny(_ x : Any?) -> Any { return x! }

  // FIXME: the below asserts Lowering::SILGenFunction::emitNativeToForeignThunk
  //   with (indirectResult.getType()->isAny() && 
  //        "Should not be trying to bridge anything except for Any here")
  //
  // func getAnyMaybe() -> Any? { return nil }
  // func getAnyProbably() -> Any? { return 1 as Any }
  // func passThroughAnyMaybe(_ x: Any?) -> Any? { return x }
  // func getAnyConstructively() -> Any? { return Optional<Any>(1 as Any) }
  // func wrapAny(_ x : Any) -> Any? { return x }

// CHECK-NEXT:  - (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
	/* implicit inherited init() */
}
// CHECK-NEXT:  @end



