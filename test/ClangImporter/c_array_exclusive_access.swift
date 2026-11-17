// Test that C array projections of stored properties only need exclusive access
// to the one property, not all of `self`. Reproducer for rdar://190152763.

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil -verify %s -o /dev/null
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil -verify %s -o /dev/null -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple

// REQUIRES: swift_feature_ModernImportedCArrays

// expected-warning@<unknown> * {{libc not found for }}

import ctypes

func accessArrayAndSibling(_ s: inout StructWithPlainAndArrayFields) -> Int32 {
  return withUnsafeBytes(of: &s.small) { _ in s.plain }
}

func accessNestedArrayAndSibling(
    _ s: inout StructWithNestedPlainAndArrayFields) -> Int32 {
  return withUnsafeBytes(of: &s.inner.small) { _ in s.inner.plain }
}
