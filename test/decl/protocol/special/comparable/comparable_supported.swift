// RUN: %target-typecheck-verify-swift -verify-ignore-unknown
// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -enable-experimental-feature DeriveConformancesViaMacros -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros)

// REQUIRES: swift_feature_DeriveConformancesViaMacros

// Comparable enum synthesis
enum Angel: Comparable {
  case lily, elsa, karlie 
}

func pit(_ a: Angel, against b: Angel) -> Bool {
  return a < b // Okay
}

// An enum with no cases should also derive conformance to Comparable.

enum NoCasesEnum: Comparable {} // Okay

// Comparable enum conformance through extension 
enum Birthyear {
  case eighties(Int)
  case nineties(Int)
  case twothousands(Int)
}

extension Birthyear: Comparable {}

func canEatHotChip(_ birthyear:Birthyear) -> Bool {
  return birthyear > .nineties(3) // Okay
}
