// RUN: %empty-directory(%t)
// RUN: %target-typecheck-verify-swift -enable-experimental-feature ExtendedSpecializedAttribute -verify-additional-prefix flag- -D EMITTED_INTO_CLIENT
// RUN: %target-typecheck-verify-swift -enable-experimental-feature ExtendedSpecializedAttribute -enable-library-evolution -verify-additional-prefix evolution-
// RUN: %target-swift-frontend -typecheck %s -parse-as-library -module-name M -enable-experimental-feature ExtendedSpecializedAttribute -emit-module-interface-path %t/M.swiftinterface
// RUN: %FileCheck %s --input-file %t/M.swiftinterface

// REQUIRES: swift_feature_ExtendedSpecializedAttribute

// expected-evolution-error@+1 {{'@specialized(exported: true)' is not supported in a module built with library evolution}}
@specialized(exported: true, where T == Int)
public func sum<T: FixedWidthInteger>(_ a: T, _ b: T) -> T { a &+ b }

// CHECK: #if compiler(>=5.3) && $ExtendedSpecializedAttribute
// CHECK-NEXT: @specialized(exported: true, where T == Swift::Int)
// CHECK-NEXT: public func sum<T>(_ a: T, _ b: T) -> T where T : Swift::FixedWidthInteger

#if EMITTED_INTO_CLIENT
// Code emitted into clients can't have a specialized body owned by the
// library.
// expected-flag-error@+1 {{'@specialized(exported: true)' cannot be combined with '@export(implementation)'}}
@specialized(exported: true, where T == Int)
@export(implementation)
public func viaExport<T: FixedWidthInteger>(_ a: T) -> T { a }

// expected-flag-error@+1 {{'@specialized(exported: true)' cannot be combined with '@_alwaysEmitIntoClient'}}
@specialized(exported: true, where T == Int)
@_alwaysEmitIntoClient
public func viaAEIC<T: FixedWidthInteger>(_ a: T) -> T { a }

// expected-flag-error@+1 {{'@specialized(exported: true)' cannot be combined with '@_transparent'}}
@specialized(exported: true, where T == Int)
@_transparent
public func viaTransparent<T: FixedWidthInteger>(_ a: T) -> T { a }

public struct Wrapper<T> {
  public var t: T
  @_alwaysEmitIntoClient public var value: T {
    // expected-flag-error@+1 {{'@specialized(exported: true)' cannot be combined with '@_alwaysEmitIntoClient'}}
    @specialized(exported: true, where T == Int)
    get { t }
  }
}
#endif

