// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name SkipsPublicUnderscore -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name SkipsPublicUnderscore -I %t -pretty-print -o %t/SymbolGraphModule.symbols.json
// RUN: %FileCheck %s --input-file %t/SymbolGraphModule.symbols.json

public struct _ShouldntAppear {
  public var shouldntAppear: Int
}

// CHECK-NOT: _ShouldntAppear
// CHECK-NOT: shouldntAppear 
