
import Darwin

public func rand32() -> UInt32 {
  return arc4random()
}

public func rand32(#exclusiveUpperBound: UInt32) -> UInt32 {
  return arc4random_uniform(exclusiveUpperBound)
}

public func rand64() -> UInt64 {
  return (UInt64(arc4random()) << 32) | UInt64(arc4random())
}

public func randInt() -> Int {
#if arch(i386) || arch(arm)
  return Int(Int32(bitPattern: rand32()))
#elseif arch(x86_64) || arch(arm64)
  return Int(Int64(bitPattern: rand64()))
#else
  fatalError("unimplemented")
#endif
}

public func randArray64(count: Int) -> ContiguousArray<UInt64> {
  var result = ContiguousArray<UInt64>(count: count, repeatedValue: 0)
  for i in indices(result) {
    result[i] = rand64()
  }
  return result
}

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
  trials: Int, observedACount: Int, pValue: Double
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
    println("chi-squared test failed: \(trials) \(observedACount) \(chiSq)")
    return false
  }
  return true
}


