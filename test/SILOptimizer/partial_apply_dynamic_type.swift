// RUN: %target-swift-frontend -O -emit-ir  %s

// Test that this does not crash

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

let c = C()

public func test_PartialApplyDynamicType() -> Int {
  var result = 0
  for _ in 1...100000 {
    result += g(c)(c)(1)
  }
  return result
}

