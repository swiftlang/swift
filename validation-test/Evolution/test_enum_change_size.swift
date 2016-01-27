// RUN: rm -rf %t && mkdir -p %t/before && mkdir -p %t/after

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -D BEFORE -c %S/Inputs/enum_change_size.swift -o %t/before/enum_change_size.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -D BEFORE -c %S/Inputs/enum_change_size.swift -o %t/before/enum_change_size.o

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -D AFTER -c %S/Inputs/enum_change_size.swift -o %t/after/enum_change_size.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -D AFTER -c %S/Inputs/enum_change_size.swift -o %t/after/enum_change_size.o

// RUN: %target-build-swift -D BEFORE -c %s -I %t/before -o %t/before/main.o
// RUN: %target-build-swift -D AFTER -c %s -I %t/after -o %t/after/main.o

// RUN: %target-build-swift %t/before/enum_change_size.o %t/before/main.o -o %t/before_before
// RUN: %target-build-swift %t/before/enum_change_size.o %t/after/main.o -o %t/before_after
// RUN: %target-build-swift %t/after/enum_change_size.o %t/before/main.o -o %t/after_before
// RUN: %target-build-swift %t/after/enum_change_size.o %t/after/main.o -o %t/after_after

// RUN: %target-run %t/before_before
// RUN: %target-run %t/before_after
// RUN: %target-run %t/after_before
// RUN: %target-run %t/after_after

import StdlibUnittest
import enum_change_size

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

