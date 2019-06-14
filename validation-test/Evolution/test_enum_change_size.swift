// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import enum_change_size


var EnumChangeSizeTest = TestSuite("EnumChangeSize")

public func getMySingletonEnumValues(_ c: ChangeSize)
    -> [SingletonEnum?] {
  return [.X(c), nil]
}

EnumChangeSizeTest.test("SingletonEnum") {
  do {
    let c = ChangeSize(value: 123)
    for e in [getMySingletonEnumValues(c), getSingletonEnumValues(c)] {
      let b: [Int] = e.map {
        switch $0 {
        case .some(.X(let cc)):
          expectEqual(c.value, cc.value)
          return 0
        case .none:
          return 1
        }
      }
      expectEqual(b, [0, 1])
    }
  }
}

public func getMySinglePayloadEnumValues(_ c: ChangeSize)
    -> [SinglePayloadEnum?] {
  return [.X(c), .Y, .Z, nil]
}

EnumChangeSizeTest.test("SinglePayloadEnum") {
  do {
    let c = ChangeSize(value: 123)
    for e in [getMySinglePayloadEnumValues(c), getSinglePayloadEnumValues(c)] {
      let b: [Int] = e.map {
        switch $0 {
        case .some(.X(let cc)):
          expectEqual(c.value, cc.value)
          return 0
        case .some(.Y):
          return 1
        case .some(.Z):
          return 2
        case .none:
          return 3
        }
      }
      expectEqual(b, [0, 1, 2, 3])
    }
  }
}

public func getMyMultiPayloadEnumValues(_ c: ChangeSize, _ d: ChangeSize)
    -> [MultiPayloadEnum?] {
  return [.X(c), .Y(d), .Z, nil]
}

EnumChangeSizeTest.test("MultiPayloadEnum") {
  do {
    let c = ChangeSize(value: 123)
    let d = ChangeSize(value: 321)
    for e in [getMyMultiPayloadEnumValues(c, d), getMultiPayloadEnumValues(c, d)] {
      let b: [Int] = e.map {
        switch $0 {
        case .some(.X(let cc)):
          expectEqual(c.value, cc.value)
          return 0
        case .some(.Y(let dd)):
          expectEqual(d.value, dd.value)
          return 1
        case .some(.Z):
          return 2
        case .none:
          return 3
        }
      }
      expectEqual(b, [0, 1, 2, 3])
    }
  }
}

runAllTests()

