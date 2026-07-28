// Test the various modes of the modern C array flag

// ModernImportedCArrays, default target
// C arrays are imported as tuples, as though the feature flag was not used.
// RUN: %target-typecheck-verify-swift %clang-importer-sdk -enable-experimental-feature ModernImportedCArrays -verify-additional-prefix not-an-inline-array-

// ModernImportedCArrays, high enough target
// C arrays are imported as InlineArrays.
// RUN: %target-typecheck-verify-swift %clang-importer-sdk -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple -verify-additional-prefix not-a-tuple-

// ModernImportedCArraysOnly, default target
// C arrays are imported as InlineArrays, but are only partially available.
// RUN: %target-typecheck-verify-swift %clang-importer-sdk -enable-experimental-feature ModernImportedCArraysOnly -verify-additional-prefix not-a-tuple- -verify-additional-prefix not-available-

// ModernImportedCArraysOnly, high enough target
// C arrays are imported as InlineArrays and are fully available.
// RUN: %target-typecheck-verify-swift %clang-importer-sdk -enable-experimental-feature ModernImportedCArraysOnly -target %target-has-inline-array-triple -verify-additional-prefix not-a-tuple-

// REQUIRES: swift_feature_ModernImportedCArrays
// REQUIRES: swift_feature_ModernImportedCArraysOnly

import Foundation

// expected-not-available-note@+1 {{add '@available' attribute to enclosing global function}}
func fn(_ state: NSFastEnumerationState) {
  let _: (UInt, UInt, UInt, UInt, UInt) = state.extra
  // expected-not-a-tuple-error@-1 {{cannot convert value of type '[5 of CUnsignedLong]' (aka 'InlineArray<5, UInt>') to specified type '(UInt /* ... repeated 5 times ... */)'}}

  if #available(anyAppleOS 26, *) {
    let _: [5 of UInt] = state.extra
    // expected-not-an-inline-array-error@-1 {{cannot convert value of type '(CUnsignedLong /* ... repeated 5 times ... */)' (aka '(UInt /* ... repeated 5 times ... */)') to specified type '[5 of UInt]'}}
  }

  use(state.extra)
  // expected-not-available-error@-1 {{'extra' is only available in}}
  // expected-not-available-note@-2 {{add 'if #available' version check}}
}

func use<T>(_: T) {}
