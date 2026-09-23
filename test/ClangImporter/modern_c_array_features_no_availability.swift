// Test the modern C array flags on platforms without availability checking.
// See modern_c_array_features.swift for the Apple platform version.

// On these platforms, the standard library always has InlineArray, so the
// feature is never disabled and nothing is ever partially available.

// RUN: %target-typecheck-verify-swift %clang-importer-sdk -enable-experimental-feature ModernImportedCArrays
// RUN: %target-typecheck-verify-swift %clang-importer-sdk -enable-experimental-feature ModernImportedCArraysOnly

// REQUIRES: swift_feature_ModernImportedCArrays
// REQUIRES: swift_feature_ModernImportedCArraysOnly
// UNSUPPORTED: VENDOR=apple

import ctypes

func fn(_ s: StructWithPlainAndArrayFields) {
  let _: (CInt, CInt, CInt, CInt) = s.small
  // expected-error@-1 {{cannot convert value of type '[4 of CInt]' (aka 'InlineArray<4, Int32>') to specified type '(CInt, CInt, CInt, CInt)'}}

  let _: [4 of CInt] = s.small

  use(s.small)
}

func use<T>(_: T) {}
