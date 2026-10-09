// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated %clang-importer-sdk -verify-ignore-unknown -verify-additional-prefix legacy-
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated %clang-importer-sdk -verify-ignore-unknown -verify-additional-prefix modern- -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple

// REQUIRES: swift_feature_ModernImportedCArrays

import ctypes

// Check that a `swift_newtype` typedef to a C array is imported correctly
// (which is a bit tricky since types can't have legacy projections, only
// their members can).

func acceptSwiftNewtypeWrapper<T: _SwiftNewtypeWrapper>(_: T) {}

#if hasFeature(ModernImportedCArrays)

let smallValue = SmallArrayNewtype(rawValue: InlineArray<4, Int32>(repeating: 0))
acceptSwiftNewtypeWrapper(smallValue)

let hugeValue = HugeArrayNewtype(rawValue: InlineArray<9000, Int32>(repeating: 0))
acceptSwiftNewtypeWrapper(hugeValue)

#else

let smallValue = SmallArrayNewtype(rawValue: (0, 0, 0, 0))
acceptSwiftNewtypeWrapper(smallValue)

// expected-legacy-error@+1 {{cannot find 'HugeArrayNewtype' in scope}}
_ = HugeArrayNewtype.self

#endif
