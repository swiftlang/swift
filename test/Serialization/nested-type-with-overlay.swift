// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/HasOverlay.swiftmodule %S/Inputs/nested-type-with-overlay/overlay.swift -I %S/Inputs/nested-type-with-overlay -module-name HasOverlay

// RUN: %target-swift-frontend -emit-module -o %t/main~partial.swiftmodule -primary-file %s -module-name main -I %t -I %S/Inputs/nested-type-helper
// RUN: %target-swift-frontend -merge-modules -emit-module -o %t/main.swiftmodule %t/main~partial.swiftmodule -print-stats -module-name main -I %t -I %S/Inputs/nested-type-with-overlay 2>&1 | %FileCheck %s

// RUN: %target-swift-frontend -emit-silgen %S/Inputs/nested-type-with-overlay/verify.swift -I %t -I %S/Inputs/nested-type-with-overlay -verify

// REQUIRES: asserts

// CHECK: 3 Serialization - # of nested types resolved without full lookup
// Unfortunately this isn't 4 because of the shadowed nested type from Clang.

import HasOverlay

public func resolveNestedTypes(
  _: Base.NestedFromClang,
  _: Base.NestedFromSwift
) {}

public var shadowedFromClang = getShadowedFromClang()
public var shadowedFromSwift = HasOverlay.shadowedFromSwift
