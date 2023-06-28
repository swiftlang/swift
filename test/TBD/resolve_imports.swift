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

// CHECK: symbols: [ _main ]