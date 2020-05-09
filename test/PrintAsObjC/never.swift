// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %s -module-name never -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %t/never.swiftmodule -typecheck -emit-objc-header-path %t/never.h -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck %s < %t/never.h
// RUN: %check-in-clang %t/never.h

// REQUIRES: objc_interop

@objc public class NeverClass {
  @objc func doesNotReturn() -> Never { while true {} }
}

// CHECK-LABEL: @interface NeverClass
// CHECK:         - (void)doesNotReturn SWIFT_NORETURN;
// CHECK:       @end
