// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name SkipsPublicUnderscore -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name SkipsPublicUnderscore -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/SkipsPublicUnderscore.symbols.json

public protocol PublicProtocol {}

// CHECK-NOT: precise:{{.*}}_ProtocolShouldntAppear
// CHECK-NOT: precise:{{.*}}PublicProtocol
@_show_in_interface
public protocol _ProtocolShouldntAppear {}

// CHECK-NOT: _ShouldntAppear

public struct _ShouldntAppear: PublicProtocol, _ProtocolShouldntAppear {
  // Although these are public and not underscored,
  // they are inside an underscored type,
  // so shouldn't be allowed through.

  // CHECK-NOT: shouldntAppear
  public var shouldntAppear: Int

  // CHECK-NOT: InnerShouldntAppear
  public struct InnerShouldntAppear {

  // CHECK-NOT: InnerInnerShouldntAppear
  public struct InnerInnerShouldntAppear {}
  }
}

// A public type's relationship to an "internal" protocol
// shouldn't cause it to be included.
public struct ShouldAppear: _ProtocolShouldntAppear {}

public struct PublicOuter {
  // Nor should an "internal" type's relationship to a "public" protocol.
  // CHECK-NOT: _InnerShouldntAppear
  public struct _InnerShouldntAppear: PublicProtocol {}
}

extension PublicOuter {
  // CHECK-NOT: _FromExtension
  public struct _FromExtension: PublicProtocol {
    // CHECK-NOT: shouldntAppear
    public var shouldntAppear: Int
  }
}

extension _ShouldntAppear {
  // CHECK-NOT: FromExtension
  public struct FromExtension: PublicProtocol {
    // CHECK-NOT: shouldntAppear
    public var shouldntAppear: Int
  }
}

extension _ShouldntAppear.InnerShouldntAppear {
  public struct ShouldntAppear {}
}

extension _ShouldntAppear.InnerShouldntAppear: Equatable {}

// CHECK: "relationships": []
