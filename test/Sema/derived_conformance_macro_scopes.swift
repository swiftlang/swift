// RUN: %target-swift-frontend -emit-silgen -compiler-assertions -enable-experimental-feature DeriveConformancesViaMacros -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-sil -parse-as-library -enable-library-evolution -compiler-assertions -enable-experimental-feature DeriveConformancesViaMacros -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) %s | %FileCheck %s

// REQUIRES: swift_feature_DeriveConformancesViaMacros

// CHECK-LABEL: struct InInheritanceClause : Equatable {
struct InInheritanceClause: Equatable {
  var hasSeen = false
}

// CHECK-LABEL: struct InExtension {
struct InExtension {
  var hasSeen = false
}

extension InExtension: Equatable {}

// CHECK-LABEL: enum EnumInInheritanceClause : Equatable {
enum EnumInInheritanceClause: Equatable {
  case a
  case b(Int)
}

// CHECK-LABEL: struct EmptyBraces : Equatable {
struct EmptyBraces: Equatable {}
