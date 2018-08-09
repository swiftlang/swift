// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test
protocol P {
  func foo() -> Int
}

struct X : P {
  func foo() -> Int { return 1 }
}

struct Y<T> {
  var wrapped: T
}

extension Y: P where T: P {
  func foo() -> Int { return wrapped.foo() + 10 }
}

func tryAsP(_ value: Any) -> Int {
  if let p = value as? P {
    return p.foo()
  }

  return 0
}

extension Dictionary: P where Value == (Key) -> Bool {
  func foo() -> Int { return 2 }
}


let yx = Y(wrapped: X())
assert(tryAsP(yx) == 11)

let dict: [Int : (Int) -> Bool] = [:]
assert(tryAsP(dict) == 2)

let yDict = Y(wrapped: dict)
assert(tryAsP(yDict) == 12)

