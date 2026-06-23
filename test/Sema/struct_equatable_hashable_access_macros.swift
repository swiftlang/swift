// RUN: %target-swift-frontend -print-ast-decl %s -enable-experimental-feature DeriveConformancesViaMacros -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) 2>&1 | %FileCheck %s

// REQUIRES: swift_feature_DeriveConformancesViaMacros

// Check that synthesized members show up as 'fileprivate', not 'private.

// CHECK-LABEL: private struct PrivateConformer : Hashable {
private struct PrivateConformer: Hashable {
  var value: Int
  // CHECK-DAG: fileprivate var hashValue: Int { get }
  // CHECK-DAG: @_implements(Equatable, ==(_:_:)) fileprivate static func __derived_struct_equals
}
