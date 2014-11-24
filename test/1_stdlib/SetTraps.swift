// These tests should crash.
// RUN: mkdir -p %t
// RUN: xcrun -sdk %target-sdk-name clang++ -arch %target-cpu %S/Inputs/CatchCrashes.cpp -c -o %t/CatchCrashes.o
// RUN: %target-build-swift %s -Xlinker %t/CatchCrashes.o -o %t/a.out
//
// RUN: %target-run %t/a.out DuplicateKeys1 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out DuplicateKeys2 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out DuplicateKeys3 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out RemoveInvalidIndex1 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out RemoveInvalidIndex2 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out RemoveInvalidIndex3 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out RemoveInvalidIndex4 2>&1 | FileCheck %s -check-prefix=CHECK

// FIXME: <rdar://problem/18853078> Implement Set<T> up and downcasting
// R/UN: %target-run %t/a.out BridgedKeyIsNotNSCopyable1 2>&1 | FileCheck %s -check-prefix=CHECK-UNRECOGNIZED-SELECTOR
// R/UN: %target-run %t/a.out Downcast1 2>&1 | FileCheck %s -check-prefix=CHECK
// R/UN: %target-run %t/a.out Downcast2 2>&1 | FileCheck %s -check-prefix=CHECK

// CHECK: OK
// CHECK: CRASHED: SIG{{ILL|TRAP|ABRT}}

// CHECK-UNRECOGNIZED-SELECTOR: OK
// CHECK-UNRECOGNIZED-SELECTOR: unrecognized selector sent to instance
// CHECK-UNRECOGNIZED-SELECTOR: CRASHED: SIGABRT

import Foundation

// Interpret the command line arguments.
var arg = Process.arguments[1]

struct NotBridgedKeyTy : Equatable, Hashable {
  init(_ value: Int) {
    self.value = value
  }
  var hashValue: Int {
    return value
  }
  var value: Int
}

func == (lhs: NotBridgedKeyTy, rhs: NotBridgedKeyTy) -> Bool {
  return lhs.value == rhs.value
}

assert(!_isBridgedToObjectiveC(NotBridgedKeyTy.self))

struct NotBridgedValueTy {}

assert(!_isBridgedToObjectiveC(NotBridgedValueTy.self))

class BridgedVerbatimRefTy : Equatable, Hashable {
  init(_ value: Int) {
    self.value = value
  }
  var hashValue: Int {
    return value
  }
  var value: Int
}

func == (lhs: BridgedVerbatimRefTy, rhs: BridgedVerbatimRefTy) -> Bool {
  return lhs.value == rhs.value
}

assert(_isBridgedToObjectiveC(BridgedVerbatimRefTy.self))
assert(_isBridgedVerbatimToObjectiveC(BridgedVerbatimRefTy.self))

if true {
  // Sanity checks.  This code should not trap.
  var s = _Set<BridgedVerbatimRefTy>()
  var nss: NSSet = s
}

if arg == "DuplicateKeys1" {
  println("OK")
  let s: _Set<Int> = [10, 20, 30, 10]
}

if arg == "DuplicateKeys2" {
  println("OK")
  let s: _Set<Int> = [10, 20, 30, 10]
}

if arg == "DuplicateKeys3" {
  println("OK")
  var s: _Set<Int> = [10, 10]
}

if arg == "RemoveInvalidIndex1" {
  var s = _Set<Int>()
  let index = s.startIndex
  println("OK")
  s.removeAtIndex(index)
}

if arg == "RemoveInvalidIndex2" {
  var s = _Set<Int>()
  let index = s.endIndex
  println("OK")
  s.removeAtIndex(index)
}

if arg == "RemoveInvalidIndex3" {
  var s: _Set<Int> = [ 10, 20, 30 ]
  let index = s.endIndex
  println("OK")
  s.removeAtIndex(index)
}

if arg == "RemoveInvalidIndex4" {
  var s: _Set<Int> = [ 10 ]
  let index = s.indexForMember(10)!
  s.removeAtIndex(index)
  assert(!s.contains(10))
  println("OK")
  s.removeAtIndex(index)
}

class TestObjCKeyTy : NSObject {
  init(_ value: Int) {
    self.value = value
  }

  override func isEqual(object: AnyObject!) -> Bool {
    if let other: AnyObject = object {
      if let otherObjcKey = other as? TestObjCKeyTy {
        return self.value == otherObjcKey.value
      }
    }
    return false
  }

  override var hash : Int {
    return value
  }

  var value: Int
}

struct TestBridgedKeyTy : Hashable, _ObjectiveCBridgeable {
  static func _isBridgedToObjectiveC() -> Bool {
    return true
  }

  init(_ value: Int) { self.value = value }

  var hashValue: Int { return value }

  static func _getObjectiveCType() -> Any.Type {
    return TestObjCKeyTy.self
  }

  func _bridgeToObjectiveC() -> TestObjCKeyTy {
    return TestObjCKeyTy(value)
  }

  static func _forceBridgeFromObjectiveC(
    x: TestObjCKeyTy,
    inout result: TestBridgedKeyTy?
  ) {
    result = TestBridgedKeyTy(x.value)
  }

  static func _conditionallyBridgeFromObjectiveC(
    x: TestObjCKeyTy,
    inout result: TestBridgedKeyTy?
  ) -> Bool {
    result = TestBridgedKeyTy(x.value)
    return true
  }

  var value: Int
}

func ==(x: TestBridgedKeyTy, y: TestBridgedKeyTy) -> Bool {
  return x.value == y.value
}

// FIXME: <rdar://problem/18853078> Implement Set<T> up and downcasting
//if arg == "BridgedKeyIsNotNSCopyable1" {
//  // This Set is bridged in O(1).
//  var s = Set([ TestObjCKeyTy(10) ])
//  var nss: NSSet = s
//  println("OK")
//  nss.mutableCopy()
//}
//
//if arg == "Downcast1" {
//  let s: Set<NSObject> = [ NSObject(), NSObject() ]
//  let s2: Set<TestObjCKeyTy> = _setDownCast(s)
//  println("OK")
//  let v1 = s2.member(TestObjCKeyTy(10))
//  let v2 = s2.member(TestObjCKeyTy(20))
//
//  // This triggers failure.
//  for m in s2 { }
//}
//
//if arg == "Downcast2" {
//  let s: Set<NSObject> = [ TestObjCKeyTy(10), NSObject() ]
//  println("OK")
//  let s2: Set<TestBridgedKeyTy> = _setBridgeFromObjectiveC(s)
//  let v1 = s2.member(TestBridgedKeyTy(10))
//}
//
//println("BUSTED: should have crashed already")
//exit(1)

