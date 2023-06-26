// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -resolve-imports -emit-tbd -emit-tbd-path %t/resolve_imports.tbd %s -disable-availability-checking
// RUN: %FileCheck %s < %t/resolve_imports.tbd

// REQUIRES: OS=macosx 

@_alwaysEmitIntoClient public var x: some Any {
  get {
  if #available(macOS 20, *) {
    return 3
  } else {
    return "hi"
  }
  }
}

public class C {}

// CHECK: symbols: [ '_$s15resolve_imports1CCMa', '_$s15resolve_imports1CCMm', 
// CHECK-NEXT:       '_$s15resolve_imports1CCMn', '_$s15resolve_imports1CCN', '_$s15resolve_imports1CCfD', 
// CHECK-NEXT:       '_$s15resolve_imports1CCfd', _main ]