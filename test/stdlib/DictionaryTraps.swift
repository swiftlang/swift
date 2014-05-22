// These tests should crash.
// RUN: mkdir -p %t
// RUN: xcrun -sdk %target-sdk-name clang++ -arch %target-cpu %S/Inputs/CatchCrashes.cpp -c -o %t/CatchCrashes.o
// RUN: %target-build-swift %s -Xlinker %t/CatchCrashes.o -o %t/a.out
//
// RUN: %target-run %t/a.out KeyTypeNotBridged 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out ValueTypeNotBridged 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out DuplicateKeys1 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out DuplicateKeys2 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out DuplicateKeys3 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out RemoveInvalidIndex1 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out RemoveInvalidIndex2 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out RemoveInvalidIndex3 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out RemoveInvalidIndex4 2>&1 | FileCheck %s -check-prefix=CHECK
// RUN: %target-run %t/a.out BridgedKeyIsNotNSCopyable1 2>&1 | FileCheck %s -check-prefix=CHECK-UNRECOGNIZED-SELECTOR
// RUN: %target-run %t/a.out BridgedKeyIsNotNSCopyable2 2>&1 | FileCheck %s -check-prefix=CHECK

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

assert(!isBridgedToObjectiveC(NotBridgedKeyTy.self))

struct NotBridgedValueTy {}

assert(!isBridgedToObjectiveC(NotBridgedValueTy.self))

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

assert(isBridgedToObjectiveC(BridgedVerbatimRefTy.self))
assert(isBridgedVerbatimToObjectiveC(BridgedVerbatimRefTy.self))

if true {
  // Sanity checks.  This code should not trap.
  var d = Dictionary<BridgedVerbatimRefTy, BridgedVerbatimRefTy>()
  var nsd: NSDictionary = d
}

if arg == "KeyTypeNotBridged" {
  var d = Dictionary<NotBridgedKeyTy, BridgedVerbatimRefTy>()
  println("OK")
  var nsd: NSDictionary = d
}

if arg == "ValueTypeNotBridged" {
  var d = Dictionary<BridgedVerbatimRefTy, NotBridgedValueTy>()
  println("OK")
  var nsd: NSDictionary = d
}

if arg == "DuplicateKeys1" {
  println("OK")
  Dictionary.convertFromDictionaryLiteral(
    (10, 1010), (20, 1020), (30, 1030), (10, 0))
}

if arg == "DuplicateKeys2" {
  println("OK")
  Dictionary.convertFromDictionaryLiteral(
    (10, 1010), (20, 1020), (30, 1030), (10, 1010))
}

if arg == "DuplicateKeys3" {
  println("OK")
  var d = [ 10: 1010, 10: 0 ]
}

if arg == "RemoveInvalidIndex1" {
  var d = Dictionary<Int, Int>()
  let index = d.startIndex
  println("OK")
  d.removeAtIndex(index)
}

if arg == "RemoveInvalidIndex2" {
  var d = Dictionary<Int, Int>()
  let index = d.endIndex
  println("OK")
  d.removeAtIndex(index)
}

if arg == "RemoveInvalidIndex3" {
  var d = [ 10: 1010, 20: 1020, 30: 1030 ]
  let index = d.endIndex
  println("OK")
  d.removeAtIndex(index)
}

if arg == "RemoveInvalidIndex4" {
  var d = [ 10: 1010 ]
  let index = d.indexForKey(10)!
  d.removeAtIndex(index)
  assert(!d[10])
  println("OK")
  d.removeAtIndex(index)
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

if arg == "BridgedKeyIsNotNSCopyable1" {
  // This Dictionary is bridged in O(1).
  var d = [ TestObjCKeyTy(10): NSObject() ]
  var nsd: NSDictionary = d
  println("OK")
  nsd.mutableCopy()
}

if arg == "BridgedKeyIsNotNSCopyable2" {
  // This Dictionary is bridged in O(N).
  var d = [ TestObjCKeyTy(10): 10 ]
  println("OK")
  var nsd: NSDictionary = d
}

println("BUSTED: should have crashed already")
exit(1)

