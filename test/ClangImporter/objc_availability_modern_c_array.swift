// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated %clang-importer-sdk -verify-ignore-unknown -enable-experimental-feature ModernImportedCArraysOnly -enable-objc-interop

// REQUIRES: objc_interop
// REQUIRES: swift_feature_ModernImportedCArraysOnly

import Foundation

// Check that we attach `@available` attributes to modern projections of
// inherited ObjC initializers as needed.

func testHugeInheritedInitWithoutAvailability(
    // expected-note@-1 2 {{add '@available' attribute to enclosing global function}}
    _ huge: InlineArray<5000, Int32>
    // expected-error@-1 {{'InlineArray' is only available in}}
) {
  var huge2 = huge
  _ = InheritsNestedArrayInit(hugeGrid: &huge2)
  // expected-error@-1 {{'init(hugeGrid:)' is only available in}}
  // expected-note@-2 {{add 'if #available' version check}}
}

@available(anyAppleOS 26, *)
func testHugeInheritedInitWithAvailability(_ huge: InlineArray<5000, Int32>) {
  var huge2 = huge
  _ = InheritsNestedArrayInit(hugeGrid: &huge2)
}

func testSmallInheritedInitWithoutAvailability(
    // expected-note@-1 2 {{add '@available' attribute to enclosing global function}}
    _ small: InlineArray<4, Int32>
    // expected-error@-1 {{'InlineArray' is only available in}}
) {
  var small2 = small
  _ = InheritsNestedArrayInit(grid: &small2)
  // expected-error@-1 {{'init(grid:)' is only available in}}
  // expected-note@-2 {{add 'if #available' version check}}
}

@available(anyAppleOS 26, *)
func testSmallInheritedInitWithAvailability(_ small: InlineArray<4, Int32>) {
  var small2 = small
  _ = InheritsNestedArrayInit(grid: &small2)
}

