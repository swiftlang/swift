// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %s -module-name Library -target %target-swift-5.1-abi-triple -enable-experimental-feature AllowIsolatedGlobalActorAttribute
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -module-name Library -target %target-swift-5.1-abi-triple
// RUN: %FileCheck %s < %t/Library.swiftinterface

// REQUIRES: concurrency
// REQUIRES: swift_feature_AllowIsolatedGlobalActorAttribute

// A concrete global actor prints as the '@G' custom attribute and doesn't
// need the feature

// CHECK-NOT: #if compiler(>=5.3) && $AllowIsolatedGlobalActorAttribute
// CHECK: public func concrete(_ fn: @_Concurrency::MainActor () -> Swift::Void)
public func concrete(_ fn: @isolated(MainActor) () -> Void) {}

// A generic global actor can only be spelled with '@isolated(G)'

// CHECK: #if compiler(>=5.3) && $AllowIsolatedGlobalActorAttribute
// CHECK-NEXT: public func generic<G>(_ fn: @isolated(G) () -> Swift::Void) where G : _Concurrency::GlobalActor
// CHECK-NEXT: #endif
public func generic<G: GlobalActor>(_ fn: @isolated(G) () -> Void) {}

// The feature guard is printed around the member, not the extension
// CHECK: extension _Concurrency::GlobalActor {
// CHECK-NEXT: #if compiler(>=5.3) && $AllowIsolatedGlobalActorAttribute
// CHECK-NEXT: public static func selfIsolated(_ fn: @isolated(Self) () -> Swift::Void)
// CHECK-NEXT: #endif
extension GlobalActor {
  public static func selfIsolated(_ fn: @isolated(Self) () -> Void) {}
}

// An isolated conformance spelled with '@isolated(G)' prints as '@G'

public protocol P {}

// CHECK-NOT: $AllowIsolatedGlobalActorAttribute
// CHECK: public struct IsolatedConformance : @_Concurrency::MainActor Library::P {
public struct IsolatedConformance: @isolated(MainActor) P {}
