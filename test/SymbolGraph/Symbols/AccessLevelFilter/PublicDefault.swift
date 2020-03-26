// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name PublicDefault -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name PublicDefault -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/PublicDefault.symbols.json

public struct ShouldAppear {
  public var x: Int
}

internal struct ShouldntAppear {
  internal var x: Int
}

// CHECK: ShouldAppear
// CHECK-NOT: ShouldntAppear
