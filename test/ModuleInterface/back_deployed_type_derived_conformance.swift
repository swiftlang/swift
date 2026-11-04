// RUN: %empty-directory(%t)

// The interface of a back-deployed type omits the members that the compiler
// derives, because clients emit the type's members themselves and a printed
// declaration has no body. A compiler that reads the interface derives them
// again.
// RUN: %target-swift-emit-module-interface(%t/Lib.swiftinterface) %s -module-name Lib -swift-version 5 -parse-as-library -enable-library-evolution -enable-experimental-feature BackDeployFrozenValueTypes
// RUN: %FileCheck %s --check-prefix=INTERFACE < %t/Lib.swiftinterface

// A client builds the module from its interface and emits its own copy of the
// derived members.
// RUN: %target-swift-frontend -compile-module-from-interface %t/Lib.swiftinterface -module-name Lib -o %t/Lib.swiftmodule
// RUN: %target-swift-frontend -emit-ir -parse-as-library -module-name Client %S/Inputs/back_deployed_type_derived_client.swift -I %t -target %target-cpu-apple-macosx10.15 | %FileCheck %s --check-prefix=CLIENT

// REQUIRES: swift_feature_BackDeployFrozenValueTypes
// REQUIRES: OS=macosx
// REQUIRES: CPU=arm64 || CPU=x86_64

// INTERFACE: #if compiler(>=5.3) && $BackDeployFrozenValueTypes
// INTERFACE: @backDeployed(before: macOS 99)
// INTERFACE-NEXT: @frozen public struct Derived : Swift::Hashable {
// INTERFACE-NEXT:   public var x: Swift::Int
// INTERFACE-NEXT:   public init(x: Swift::Int) { self.x = x }
// INTERFACE-NEXT: }
// INTERFACE-NEXT: #else
// INTERFACE: @frozen public struct Derived : Swift::Hashable {
// INTERFACE: public static func == (a: Lib::Derived, b: Lib::Derived) -> Swift::Bool
// INTERFACE: public func hash(into hasher: inout Swift::Hasher)
// INTERFACE: public var hashValue: Swift::Int {
// INTERFACE: #if compiler(>=5.3) && $BackDeployFrozenValueTypes
// INTERFACE: @frozen public enum Kind {
// INTERFACE-NEXT:   case a, b
// INTERFACE-NEXT: }
// INTERFACE-NEXT: #else
// INTERFACE: public static func == (a: Lib::Kind, b: Lib::Kind) -> Swift::Bool

// CLIENT-DAG: define {{.*}} @"$s3Lib7DerivedV2eeoiySbAC_ACtFZTwB"
// CLIENT-DAG: define {{.*}} @"$s3Lib7DerivedV4hash4intoys6HasherVz_tFTwB"
// CLIENT-DAG: define {{.*}} @"$s3Lib7DerivedV9hashValueSivgTwB"
// CLIENT-DAG: define {{.*}} @"$s3Lib4KindO2eeoiySbAC_ACtFZTwB"

@available(macOS 10.15, *)
@backDeployed(before: macOS 99)
@frozen public struct Derived: Hashable {
  public var x: Int
  public init(x: Int) { self.x = x }
}

@available(macOS 10.15, *)
@backDeployed(before: macOS 99)
@frozen public enum Kind {
  case a, b
}
