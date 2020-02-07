// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name IncludeInternal -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name IncludeInternal -I %t -pretty-print -output-dir %t -minimum-access-level internal
// RUN: %FileCheck %s --input-file %t/IncludeInternal.symbols.json

public struct ShouldAppear {
  public var x: Int
}

internal struct ShouldAlsoAppear {
  internal var x: Int
}

private struct ShouldntAppear {
  var x: Int
}

// CHECK: ShouldAppear
// CHECK: ShouldAlsoAppear
// CHECK-NOT: ShouldntAppear 
