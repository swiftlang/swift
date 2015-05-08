//===--- Statistics.swift -------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// For a given p-value, returns the critical chi-square value for
/// a distribution with 1 degree of freedom.
func _chiSquaredUniform1DFCritical(pValue: Double) -> Double {
  if abs(pValue - 0.05) < 0.00001 { return 0.00393214 }
  if abs(pValue - 0.02) < 0.00001 { return 0.000628450 }
  if abs(pValue - 0.01) < 0.00001 { return 0.000157088 }
  if abs(pValue - 0.007) < 0.00001 { return 0.000076971 }
  if abs(pValue - 0.005) < 0.00001 { return 0.0000392704 }
  if abs(pValue - 0.003) < 0.00001 { return 0.0000141372 }
  if abs(pValue - 0.002) < 0.00001 { return 6.2832e-6 }
  if abs(pValue - 0.001) < 0.00001 { return 1.5708e-6 }
  fatalError("unknown value")
}

/// Perform chi-squared test for a discrete uniform distribution with
/// 2 outcomes.
public func chiSquaredUniform2(
  trials: Int, _ observedACount: Int, _ pValue: Double
) -> Bool {

  func square(x: Double) -> Double {
    return x * x
  }

  let expectedA = 0.5
  let expectedB = 0.5
  let observedA = Double(observedACount) / Double(trials)
  let observedB = 1.0 - observedA
  let chiSq =
    square(observedA - expectedA) / expectedA +
    square(observedB - expectedB) / expectedB
  if chiSq > _chiSquaredUniform1DFCritical(pValue) {
    print("chi-squared test failed: \(trials) \(observedACount) \(chiSq)")
    return false
  }
  return true
}


