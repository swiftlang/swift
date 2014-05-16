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

// CHECK: OK
// CHECK: CRASHED: SIG{{ILL|TRAP}}

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

println("BUSTED: should have crashed already")
exit(1)

