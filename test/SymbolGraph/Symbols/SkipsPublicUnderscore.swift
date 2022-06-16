// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name SkipsPublicUnderscore -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name SkipsPublicUnderscore -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/SkipsPublicUnderscore.symbols.json --check-prefix PUBLIC

// RUN: %target-swift-symbolgraph-extract -module-name SkipsPublicUnderscore -I %t -pretty-print -output-dir %t -minimum-access-level internal
// RUN: %FileCheck %s --input-file %t/SkipsPublicUnderscore.symbols.json --check-prefix INTERNAL

// RUN: %target-swift-symbolgraph-extract -module-name SkipsPublicUnderscore -I %t -pretty-print -output-dir %t -minimum-access-level private
// RUN: %FileCheck %s --input-file %t/SkipsPublicUnderscore.symbols.json --check-prefix PRIVATE

public protocol PublicProtocol {}

public class SomeClass {
  // underscored names marked `internal` or tighter should be considered `private`

  // PUBLIC-NOT: "precise": "s:21SkipsPublicUnderscore9SomeClassC12_InternalVarSSvp"
  // INTERNAL-NOT: "precise": "s:21SkipsPublicUnderscore9SomeClassC12_InternalVarSSvp"
  // PRIVATE: "precise": "s:21SkipsPublicUnderscore9SomeClassC12_InternalVarSSvp"
  internal var _InternalVar: String = ""
}

// PUBLIC-NOT: precise:{{.*}}_ProtocolShouldntAppear
// PUBLIC-NOT: precise:{{.*}}PublicProtocol
// PUBLIC-NOT: precise:{{.*}}someHiddenVar
@_show_in_interface
public protocol _ProtocolShouldntAppear {
  static var someHiddenVar: String { get }
}

// PUBLIC-NOT: _ShouldntAppear
// INTERNAL-DAG: _ShouldntAppear

// INTERNAL-DAG: "precise": "s:21SkipsPublicUnderscore23_ProtocolShouldntAppearP"
public struct _ShouldntAppear: PublicProtocol, _ProtocolShouldntAppear {
  // Although these are public and not underscored,
  // they are inside an underscored type,
  // so shouldn't be allowed through.

  // PUBLIC-NOT: shouldntAppear
  // INTERNAL-DAG: shouldntAppear
  public var shouldntAppear: Int

  // PUBLIC-NOT: InnerShouldntAppear
  // INTERNAL-DAG: InnerShouldntAppear
  public struct InnerShouldntAppear {

  // PUBLIC-NOT: InnerInnerShouldntAppear
  // INTERNAL-DAG: InnerInnerShouldntAppear
  public struct InnerInnerShouldntAppear {}
  }

  // INTERNAL-DAG: "precise": "s:21SkipsPublicUnderscore15_ShouldntAppearV13someHiddenVarSSvpZ"
  public static var someHiddenVar: String { "someHiddenVar" }
}

// A public type's relationship to an "internal" protocol
// shouldn't cause it to be included.
public struct ShouldAppear {}

extension ShouldAppear: _ProtocolShouldntAppear {
  public static var someHiddenVar: String { "someHiddenVar" }
}

public struct PublicOuter {
  // Nor should an "internal" type's relationship to a "public" protocol.
  // PUBLIC-NOT: _InnerShouldntAppear
  // INTERNAL-DAG: _InnerShouldntAppear
  public struct _InnerShouldntAppear: PublicProtocol {}
}

extension PublicOuter {
  // PUBLIC-NOT: _FromExtension
  // INTERNAL-DAG: _FromExtension
  public struct _FromExtension: PublicProtocol {
    // PUBLIC-NOT: shouldntAppear
    // INTERNAL-DAG: shouldntAppear
    public var shouldntAppear: Int
  }
}

extension _ShouldntAppear {
  // PUBLIC-NOT: FromExtension
  // INTERNAL-DAG: FromExtension
  public struct FromExtension: PublicProtocol {
    // PUBLIC-NOT: shouldntAppear
    // INTERNAL-DAG: shouldntAppear
    public var shouldntAppear: Int
  }
}

extension _ShouldntAppear.InnerShouldntAppear {
  public struct ShouldntAppear {}
}

extension _ShouldntAppear.InnerShouldntAppear: Equatable {}

// PUBLIC: "relationships": []
