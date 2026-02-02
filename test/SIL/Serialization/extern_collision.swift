// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -DUSE_EXTERN -enable-experimental-feature Extern -o %t %S/Inputs/extern_with_nullable.swift
// RUN: %target-swift-frontend -emit-module -DUSE_EXTERN -enable-experimental-feature Extern -o %t %S/Inputs/extern_with_nonnull.swift
// RUN: %target-swift-frontend -emit-sil -o %t -I %t -primary-file %s -module-name main -O

// REQUIRES: swift_feature_Extern

// Don't crash or otherwise fail when inlining multiple functions that reference
// @_extern(c) declarations of the same name but different types at the SIL
// level.

import extern_with_nullable
import extern_with_nonnull

public func main() {
  callWithNullable()
  callWithNonNull()
}
