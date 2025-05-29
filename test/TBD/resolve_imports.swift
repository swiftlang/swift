// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -resolve-imports -emit-tbd -emit-tbd-path %t/resolve_imports.tbd %s -disable-availability-checking -tbd-install_name resolve_imports
// RUN: %llvm-nm %t/resolve_imports.tbd |  %FileCheck %s 

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

// CHECK: T _$s15resolve_imports1CCMa
// CHECK-NEXT: D _$s15resolve_imports1CCMm
// CHECK-NEXT: T _$s15resolve_imports1CCMn
// CHECK-NEXT: D _$s15resolve_imports1CCN
// CHECK-NEXT: T _$s15resolve_imports1CCfD
// CHECK-NEXT: T _$s15resolve_imports1CCfd
// CHECK-NEXT: T _$s15resolve_imports1SVMa
// CHECK-NEXT: T _$s15resolve_imports1SVMn
// CHECK-NEXT: D _$s15resolve_imports1SVN
// CHECK-NEXT: T _$s15resolve_imports1SVSQAAMc
// CHECK-NEXT: T _$s15resolve_imports1SVSQAASQ2eeoiySbx_xtFZTW
// CHECK-NEXT: T _main
