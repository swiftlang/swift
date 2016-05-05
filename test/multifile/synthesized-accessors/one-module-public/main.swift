// Try with and without whole module optimization

// RUN: %target-build-swift %S/library.swift %S/main.swift
// RUN: %target-build-swift -whole-module-optimization %S/library.swift %S/main.swift

// REQUIRES: executable_test

protocol Takeaway {
  var costPounds: Float { get set }
  var costEuros: Float { get set }
  var costDollars: Float { get set }
}

extension FishAndChips : Takeaway {}

protocol Beverage {
  var abv: Int { get set }
}

extension Beer : Beverage {}

protocol PurrExtractor {
  var purrs: Int { get set }
}

extension LazyCat : PurrExtractor {}

// Dummy statement
_ = ()
