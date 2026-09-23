// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated %clang-importer-sdk -verify-ignore-unknown -enable-experimental-feature ModernImportedCArraysOnly

// REQUIRES: swift_feature_ModernImportedCArraysOnly

import ctypes

// Check that we attach `@available` attributes to modern projections as needed.
// We can only tell by using experimental feature `ModernImportedCArraysOnly`,
// since the normal one disables itself when the target is too low.

func testStructWithAllHugeArrayFieldsWithoutAvailability(
    // expected-note@-1 4 {{add '@available' attribute to enclosing global function}}
    _ s: StructWithAllHugeArrayFields,
    _ huge1: InlineArray<5000, Int32>,
    // expected-error@-1 {{'InlineArray' is only available in}}
    _ huge2: InlineArray<6000, Int32>
    // expected-error@-1 {{'InlineArray' is only available in}}
) {
  _ = StructWithAllHugeArrayFields(huge1: huge1, huge2: huge2)
  // expected-error@-1 {{'init(huge1:huge2:)' is only available in}}
  // expected-note@-2 {{add 'if #available' version check}}

  let _ = s.huge1
  // expected-error@-1 {{'huge1' is only available in}}
  // expected-note@-2 {{add 'if #available' version check}}
}

@available(anyAppleOS 26, *)
func testStructWithAllHugeArrayFieldsWithAvailability(
    _ s: StructWithAllHugeArrayFields, _ huge1: InlineArray<5000, Int32>, _ huge2: InlineArray<6000, Int32>
) {
  _ = StructWithAllHugeArrayFields(huge1: huge1, huge2: huge2)
  let _ = s.huge1
}

func testUnionWithSmallAndHugeArrayFieldsWithoutAvailability(
    // expected-note@-1 3 {{add '@available' attribute to enclosing global function}}
    _ u: UnionWithSmallAndHugeArrayFields,
    _ huge: InlineArray<5000, Int32>
    // expected-error@-1 {{'InlineArray' is only available in}}
) {
  _ = UnionWithSmallAndHugeArrayFields(huge: huge)
  // expected-error@-1 {{'init(huge:)' is only available in}}
  // expected-note@-2 {{add 'if #available' version check}}

  let _ = u.huge
  // expected-error@-1 {{'huge' is only available in}}
  // expected-note@-2 {{add 'if #available' version check}}
}

@available(anyAppleOS 26, *)
func testUnionWithSmallAndHugeArrayFieldsWithAvailability(
    _ u: UnionWithSmallAndHugeArrayFields, _ huge: InlineArray<5000, Int32>
) {
  _ = UnionWithSmallAndHugeArrayFields(huge: huge)
  let _ = u.huge
}

func testGlobalArrayWithoutAvailability() {
  // expected-note@-1 {{add '@available' attribute to enclosing global function}}
  let _ = hugeGlobalArray
  // expected-error@-1 {{'hugeGlobalArray' is only available in}}
  // expected-note@-2 {{add 'if #available' version check}}
}

@available(anyAppleOS 26, *)
func testGlobalArrayWithAvailability() {
  let _ = hugeGlobalArray
}

func testStructWithNestedHugeArrayFieldWithoutAvailability(
    // expected-note@-1 3 {{add '@available' attribute to enclosing global function}}
    _ s: StructWithNestedHugeArrayField,
    _ elems: InlineArray<2, InlineArray<5000, Int32>>
    // expected-error@-1 {{'InlineArray' is only available in}}
) {
  _ = StructWithNestedHugeArrayField(elems: elems)
  // expected-error@-1 {{'init(elems:)' is only available in}}
  // expected-note@-2 {{add 'if #available' version check}}

  let _ = s.elems
  // expected-error@-1 {{'elems' is only available in}}
  // expected-note@-2 {{add 'if #available' version check}}
}

@available(anyAppleOS 26, *)
func testStructWithNestedHugeArrayFieldWithAvailability(
    _ s: StructWithNestedHugeArrayField, _ elems: InlineArray<2, InlineArray<5000, Int32>>
) {
  _ = StructWithNestedHugeArrayField(elems: elems)
  let _ = s.elems
}

// C arrays decay to pointers when used as function parameters and results, but
// with 2D (or higher) arrays, the inner arrays don't decay.

func testBigArray2dWithoutAvailability(
    // expected-note@-1 3 {{add '@available' attribute to enclosing global function}}
    _ maxSize: UnsafeMutablePointer<InlineArray<4096, CChar>>?,
    // expected-error@-1 {{'InlineArray' is only available in}}
    _ maxSizePlusOne: UnsafeMutablePointer<InlineArray<4097, CChar>>?
    // expected-error@-1 {{'InlineArray' is only available in}}
) {
  useBigArray2d(maxSize, maxSizePlusOne)
  // expected-error@-1 {{'useBigArray2d' is only available in}}
  // expected-note@-2 {{add 'if #available' version check}}
}

@available(anyAppleOS 26, *)
func testBigArray2dWithAvailability(
    _ maxSize: UnsafeMutablePointer<InlineArray<4096, CChar>>?,
    _ maxSizePlusOne: UnsafeMutablePointer<InlineArray<4097, CChar>>?
) {
  useBigArray2d(maxSize, maxSizePlusOne)
}
