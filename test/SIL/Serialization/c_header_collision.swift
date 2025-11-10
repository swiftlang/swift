// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -DUSE_C_MODULE -I %S/Inputs -o %t %S/Inputs/extern_with_nullable.swift -module-name c_with_nullable -enable-experimental-feature Extern
// RUN: %target-swift-frontend -emit-module -DUSE_C_MODULE -I %S/Inputs -o %t %S/Inputs/extern_with_nonnull.swift -module-name c_with_nonnull -enable-experimental-feature Extern
// RUN: %target-swift-frontend -emit-sil -o %t -I %t -primary-file %s -module-name main -O -enable-experimental-feature Extern

// RUN: %target-swift-frontend -emit-ir -o %t -I %t -primary-file %s -module-name main -O -enable-experimental-feature Extern

// Don't crash or otherwise fail when inlining multiple functions that reference
// C declarations of the same name but different types at the SIL level.

// REQUIRES: swift_feature_Extern

import c_with_nullable
import c_with_nonnull

public func main() {
  callWithNullable()
  callWithNonNull()
}
