// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import enum_change_size

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
import SwiftPrivatePthreadExtras
#if _runtime(_ObjC)
import ObjectiveC
#endif

var EnumChangeSizeTest = TestSuite("EnumChangeSize")

public func getMySingletonEnumValues(c: ChangeSize)
    -> [SingletonEnum?] {
  return [.X(c), nil]
}

EnumChangeSizeTest.test("SingletonEnum") {
  do {
    let c = ChangeSize(value: 123)
    for e in [getMySingletonEnumValues(c), getSingletonEnumValues(c)] {
      let b: [Int] = e.map {
        switch $0 {
        case .Some(.X(let cc)):
          expectEqual(c.value, cc.value)
          return 0
        case .None:
          return 1
        }
      }
      expectEqual(b, [0, 1])
    }
  }
}

public func getMySinglePayloadEnumValues(c: ChangeSize)
    -> [SinglePayloadEnum?] {
  return [.X(c), .Y, .Z, nil]
}

EnumChangeSizeTest.test("SinglePayloadEnum") {
  do {
    let c = ChangeSize(value: 123)
    for e in [getMySinglePayloadEnumValues(c), getSinglePayloadEnumValues(c)] {
      let b: [Int] = e.map {
        switch $0 {
        case .Some(.X(let cc)):
          expectEqual(c.value, cc.value)
          return 0
        case .Some(.Y):
          return 1
        case .Some(.Z):
          return 2
        case .None:
          return 3
        }
      }
      expectEqual(b, [0, 1, 2, 3])
    }
  }
}

public func getMyMultiPayloadEnumValues(c: ChangeSize, _ d: ChangeSize)
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
        case .Some(.X(let cc)):
          expectEqual(c.value, cc.value)
          return 0
        case .Some(.Y(let dd)):
          expectEqual(d.value, dd.value)
          return 1
        case .Some(.Z):
          return 2
        case .None:
          return 3
        }
      }
      expectEqual(b, [0, 1, 2, 3])
    }
  }
}

runAllTests()

