// RUN: %target-typecheck-verify-swift -disable-availability-checking

// https://github.com/swiftlang/swift/issues/88549
//
// Diagnose an `InlineArray<N, T>` whose literal element count `N` exceeds
// `UINT32_MAX` at type-check time. IRGen's `FixedTypeInfo` stores byte size
// in a 32-bit field, so once `N` exceeds that, no element type (minimum
// stride 1) can make it fit. Catching this in Sema gives us a real source
// location and covers the case where `T` is generic or opaque — the IRGen
// overflow guard fires only on the fixed-layout path and otherwise falls
// back to `NonFixedArrayTypeInfo` without diagnosing.

func tooLargeConcrete() {
  _ = InlineArray<9_999_999_999, Int64>(repeating: 0) // expected-error {{'InlineArray' element count '9999999999' exceeds the maximum supported value of 4294967295}}
}

func tooLargeGenericElement<T>(_: T.Type) {
  _ = InlineArray<9_999_999_999, T>(repeating: fatalError()) // expected-error {{'InlineArray' element count '9999999999' exceeds the maximum supported value of 4294967295}}
}

func tooLargeSugar() -> [9_999_999_999 of Int64].Type { // expected-error {{'InlineArray' element count '9999999999' exceeds the maximum supported value of 4294967295}}
  fatalError()
}

// Boundary cases.

// Exactly UINT32_MAX is the largest accepted count.
typealias AtLimit = InlineArray<4_294_967_295, Int8>

// UINT32_MAX + 1 is rejected.
typealias OverByOne = InlineArray<4_294_967_296, Int8> // expected-error {{'InlineArray' element count '4294967296' exceeds the maximum supported value of 4294967295}}

// Counts that fit but produce a byte size > 4 GiB (element stride × count)
// are still left to IRGen's fixed-layout check, since Sema doesn't know
// stride here.
typealias LargeButCountFits = InlineArray<1_000_000_000, Int64>
