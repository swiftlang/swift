// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -resolve-imports -emit-tbd -emit-tbd-path %t/resolve_imports.tbd %s -disable-availability-checking
// RUN: %target-swift-frontend -experimental-lazy-typecheck -emit-tbd -emit-tbd-path %t/lazy_typecheck.tbd %s -disable-availability-checking
// RUN: %FileCheck %s < %t/resolve_imports.tbd
// RUN: %FileCheck %s < %t/lazy_typecheck.tbd

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
// CHECK: '_$s14lazy_typecheck1CCMa',
// CHECK: '_$s14lazy_typecheck1CCMm',
// CHECK: '_$s14lazy_typecheck1CCMn',
// CHECK: '_$s14lazy_typecheck1CCN',
// CHECK: '_$s14lazy_typecheck1CCfD',
// CHECK: '_$s14lazy_typecheck1CCfd',
// CHECK: '_$s14lazy_typecheck1SVMa',
// CHECK: '_$s14lazy_typecheck1SVMn',
// CHECK: '_$s14lazy_typecheck1SVN',
// CHECK: '_$s14lazy_typecheck1SVSQAAMc',
// CHECK: '_$s14lazy_typecheck1SVSQAASQ2eeoiySbx_xtFZTW',
// CHECK: _main
// CHECK: ]
