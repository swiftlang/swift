// RUN: %target-swift-frontend -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -typecheck -dump-macro-expansions %s 2>&1 | %FileCheck %s

// REQUIRES: swift_feature_DeriveConformancesViaMacros

enum Simple: CaseIterable {
  case a
  case b
}

// CHECK: nonisolated static var allCases: [Self] {
// CHECK:   return [.a, .b]
// CHECK: }

enum Uninhabited: CaseIterable {}

// CHECK: nonisolated static var allCases: [Self] {
// CHECK:   return []
// CHECK: }

enum WithRawIdentifiers: CaseIterable {
  case `foo bar`
  case `default`
}

// CHECK: nonisolated static var allCases: [Self] {
// CHECK:   return [.`foo bar`, .`default`]
// CHECK: }

enum WithRawValue: Int, CaseIterable {
  case one = 1
  case two = 2
}

// CHECK: nonisolated static var allCases: [Self] {
// CHECK:   return [.one, .two]
// CHECK: }

enum Generic<T>: CaseIterable {
  case x
  case y
}

// CHECK: nonisolated static var allCases: [Self] {
// CHECK:   return [.x, .y]
// CHECK: }
