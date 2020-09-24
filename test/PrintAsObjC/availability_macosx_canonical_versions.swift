// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %s -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %t/availability_macosx_canonical_versions.swiftmodule -typecheck -emit-objc-header-path %t/availability.h -import-objc-header %S/../Inputs/empty.h -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck %s < %t/availability.h
// RUN: %check-in-clang %t/availability.h

// REQUIRES: objc_interop

// CHECK-LABEL: @interface Availability{{$}}
// CHECK-NEXT: - (void)alwaysAvailable;
// CHECK-NEXT: - (void)introducedOn10_16
// CHECK-DAG: SWIFT_AVAILABILITY(macos,introduced=11.0)
// CHECK-DAG: SWIFT_AVAILABILITY(ios,introduced=10.16)

@objc class Availability {
  @objc func alwaysAvailable() {}
  @available(macOS 10.16, *)
  @available(iOS, introduced: 10.16)
  @objc func introducedOn10_16() {}
}
