// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -resolve-imports -emit-tbd -emit-tbd-path %t/resolve_imports.tbd %s -disable-availability-checking
// RUN: %FileCheck %s < %t/resolve_imports.tbd

// REQUIRES: OS=macosx 

// Correct linkage for opaque type descriptor.
@_alwaysEmitIntoClient public var x: some Any {
  get {
  if #available(macOS 20, *) {
    return 3
  } else {
    return "hi"
  }
  }
}

// Make sure we emit all ABI members.
public class C {}

// Edge case where protocol witness thunk is public.
protocol PrivateProto: Equatable {}

extension PrivateProto {
  public static func ==(lhs: Self, rhs: Self) -> Bool { return false }
}

public struct S: PrivateProto {}

// CHECK: symbols: [
// CHECK: '_$s15resolve_imports1CCMa',
// CHECK: '_$s15resolve_imports1CCMm',
// CHECK: '_$s15resolve_imports1CCMn',
// CHECK: '_$s15resolve_imports1CCN',
// CHECK: '_$s15resolve_imports1CCfD',
// CHECK: '_$s15resolve_imports1CCfd',
// CHECK: '_$s15resolve_imports1SVMa',
// CHECK: '_$s15resolve_imports1SVMn',
// CHECK: '_$s15resolve_imports1SVN',
// CHECK: '_$s15resolve_imports1SVSQAAMc',
// CHECK: '_$s15resolve_imports1SVSQAASQ2eeoiySbx_xtFZTW',
// CHECK: _main
// CHECK: ]
