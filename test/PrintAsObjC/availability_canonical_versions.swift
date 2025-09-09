// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %s -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %t/availability_canonical_versions.swiftmodule -typecheck -verify -emit-objc-header-path %t/availability.h -import-objc-header %S/../Inputs/empty.h -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck %s < %t/availability.h
// RUN: %check-in-clang %t/availability.h

// REQUIRES: objc_interop

// CHECK-LABEL: @interface Availability{{$}}
@objc class Availability {
  // CHECK-NEXT: - (void)alwaysAvailable;
  @objc func alwaysAvailable() {}

  // CHECK-NEXT: - (void)introducedOn10_16
  // CHECK-SAME: SWIFT_AVAILABILITY(ios,introduced=10.16)
  // CHECK-SAME: SWIFT_AVAILABILITY(macos,introduced=11.0)
  @available(macOS 10.16, *)
  @available(iOS, introduced: 10.16)
  @objc func introducedOn10_16() {}

  // CHECK-NEXT: - (void)introducedOn11_0
  // CHECK-SAME: SWIFT_AVAILABILITY(ios,introduced=11.0)
  // CHECK-SAME: SWIFT_AVAILABILITY(macos,introduced=11.0)
  @available(macOS 11.0, *)
  @available(iOS, introduced: 11.0)
  @objc func introducedOn11_0() {}

  // CHECK-NEXT: - (void)introducedOnVersionsMappingTo26_0
  // CHECK-SAME: SWIFT_AVAILABILITY(visionos,introduced=26.0)
  // CHECK-SAME: SWIFT_AVAILABILITY(tvos,introduced=26.0)
  // CHECK-SAME: SWIFT_AVAILABILITY(watchos,introduced=26.0)
  // CHECK-SAME: SWIFT_AVAILABILITY(maccatalyst,introduced=26.0)
  // CHECK-SAME: SWIFT_AVAILABILITY(ios,introduced=26.0)
  // CHECK-SAME: SWIFT_AVAILABILITY(macos,introduced=26.0)
  @available(macOS 16.0, iOS 19.0, macCatalyst 19.0, watchOS 12.0, tvOS 19.0, visionOS 3.0, *)
  @objc func introducedOnVersionsMappingTo26_0() {}

  // CHECK-NEXT: - (void)introducedOn26_0
  // CHECK-SAME: SWIFT_AVAILABILITY(visionos,introduced=26.0)
  // CHECK-SAME: SWIFT_AVAILABILITY(watchos,introduced=26.0)
  // CHECK-SAME: SWIFT_AVAILABILITY(tvos,introduced=26.0)
  // CHECK-SAME: SWIFT_AVAILABILITY(maccatalyst,introduced=26.0)
  // CHECK-SAME: SWIFT_AVAILABILITY(ios,introduced=26.0)
  // CHECK-SAME: SWIFT_AVAILABILITY(macos,introduced=26.0)
  @available(macOS 26.0, iOS 26.0, macCatalyst 26.0, tvOS 26.0, watchOS 26.0, visionOS 26.0, *)
  @objc func introducedOn26_0() {}
}
