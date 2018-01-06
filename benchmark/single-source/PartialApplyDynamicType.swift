// PartialApplyDynamicType benchmark
//
// Description: Create and call a generic partially applied dynamic type method.
// Source: https://gist.github.com/airspeedswift/f14f13960270004fbe64

import TestsUtils

public var PartialApplyDynamicType = BenchmarkInfo(
  name: "PartialApplyDynamicType",
  runFunction: run_PartialApplyDynamicType,
  tags: [.validation, .abstraction]
)

@inline(never)
public func run_PartialApplyDynamicType(_ N: Int) {
  let expectedResult = 2
  var result = 0
  for _ in 1...100000*N {
    result = g(c)(c)(1)
    if result != expectedResult {
      break
    }
  }
  CheckResults(result == expectedResult)
}

let c = C()

protocol P {
  func f(_ i: Int) -> Int
}

func g<T: P>(_ t: T) -> ((T) -> (Int) -> Int) {
  let f = { type(of: t).f($0) }
  return f
}

class C: P {
  func f(_ i: Int) -> Int {
    return i + 1
  }
}
