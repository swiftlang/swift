// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name SkipsPublicUnderscore -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name SkipsPublicUnderscore -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/SkipsPublicUnderscore.symbols.json

public struct _ShouldntAppear {
  public var shouldntAppear: Int
}

// CHECK-NOT: _ShouldntAppear
// CHECK-NOT: shouldntAppear 
