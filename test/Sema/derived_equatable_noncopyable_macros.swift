// RUN: %target-swift-frontend -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -typecheck -verify %s
// RUN: %target-swift-frontend -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -typecheck -dump-macro-expansions %s 2>&1 | %FileCheck %s

// REQUIRES: swift_feature_DeriveConformancesViaMacros

struct Unconditional<T: ~Copyable & Equatable>: ~Copyable, Equatable {
  var unconditional: T
}

// CHECK: static func __derived_struct_equals(_ lhs: borrowing Self, _ rhs: borrowing Self) -> Swift::Bool {
// CHECK: lhs.unconditional == rhs.unconditional

struct Conditional<T: ~Copyable & Equatable>: ~Copyable, Equatable {
  var conditional: T
}

extension Conditional: Copyable where T: Copyable {}

// CHECK: static func __derived_struct_equals(_ lhs: borrowing Self, _ rhs: borrowing Self) -> Swift::Bool {
// CHECK: lhs.conditional == rhs.conditional

struct ConditionalInExtension<T: ~Copyable>: ~Copyable {
  var conditionalInExtension: T
}

extension ConditionalInExtension: Copyable where T: Copyable {}
extension ConditionalInExtension: Equatable where T: Copyable & Equatable {}

// CHECK: static func __derived_struct_equals(_ lhs: Self, _ rhs: Self) -> Swift::Bool {
// CHECK: lhs.conditionalInExtension == rhs.conditionalInExtension

struct AlwaysCopyable: Equatable {
  var alwaysCopyable: Int
}

// CHECK: static func __derived_struct_equals(_ lhs: Self, _ rhs: Self) -> Swift::Bool {
// CHECK: lhs.alwaysCopyable == rhs.alwaysCopyable
