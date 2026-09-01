// REQUIRES: objc_interop

// RUN: %empty-directory(%t)

// FIXME: BEGIN -enable-source-import hackaround
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift -disable-objc-attr-requires-foundation-module
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/CoreGraphics.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift
// FIXME: END -enable-source-import hackaround

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -I %S/Inputs/custom-modules -o %t %s -Xcc -fobjc-direct-precondition-thunk -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -parse-as-library %t/objc_direct.swiftmodule -typecheck -Xcc -fobjc-direct-precondition-thunk -emit-objc-header-path %t/objc_direct.h -import-objc-header %S/../Inputs/empty.h -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck %s --input-file %t/objc_direct.h

import ObjectiveC

// The SWIFT_OBJC_DIRECT macro is defined in the header preamble, guarded on
// __has_attribute so the header stays usable with a compiler that lacks
// objc_direct. This has to be checked before the @interface: FileCheck's plain
// CHECK directives match in order, and the preamble precedes the classes.
// CHECK: # if __has_attribute(objc_direct)
// CHECK-NEXT: #  define SWIFT_OBJC_DIRECT __attribute__((objc_direct))
// CHECK-NEXT: # else
// CHECK-NEXT: #  define SWIFT_OBJC_DIRECT
// CHECK-NEXT: # endif

// CHECK-LABEL: @interface DirectMethodClass
// CHECK: - (NSInteger)directMethod SWIFT_WARN_UNUSED_RESULT SWIFT_OBJC_DIRECT;
// Only the direct method is annotated; the plain @objc one is not.
// CHECK-NOT: SWIFT_OBJC_DIRECT
// CHECK: - (void)normalMethod;
// CHECK: @end
public class DirectMethodClass: NSObject {
  @objcDirect public final func directMethod() -> Int { return 42 }
  @objc public func normalMethod() {}
}
