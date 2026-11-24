// RUN: %target-run-simple-swift(-O) | %FileCheck %s
// RUN: %target-run-simple-swift(-Onone) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

// Check that `@_noMetadata Element : _Class` pre-specializations of the stdlib's
// array functions are not applied to non-class types which happen to have the
// same layout as a class reference.

import Foundation

final class Storage {
  let value: Int

  init(_ value: Int) {
    self.value = value
  }
}

struct Single {
  let storage: Storage
}

@inline(never)
func roundTrip(_ array: [Single]) -> Bool {
  let object: AnyObject = array as AnyObject
  return (object as Any) as? [Single] != nil
}

@inline(never)
func singleViaArrayMap(_ ids: [Int]) -> [Single] {
  ids.map {
    Single(storage: Storage($0))
  }
}

@inline(never)
func singleViaReserveCapacity(_ n: Int) -> [Single] {
  var a = [Single]()
  a.reserveCapacity(n)
  for i in 0..<n {
    a.append(Single(storage: Storage(i)))
  }
  return a
}

@inline(never)
func optionalsViaReserveCapacity(_ n: Int) -> [Storage?] {
  var a = [Storage?]()
  a.reserveCapacity(n)
  for _ in 0..<n {
    a.append(nil)
  }
  return a
}

func storageClassName<T>(_ a: [T]) -> String {
  return String(cString: object_getClassName(unsafeBitCast(a, to: AnyObject.self)))
}

// CHECK: true
print(roundTrip(singleViaArrayMap([1])))

// CHECK-NEXT: Single
print(storageClassName(singleViaArrayMap([1])).contains("Single") ? "Single" : "wrong storage")

// CHECK-NEXT: Single
print(storageClassName(singleViaReserveCapacity(1)).contains("Single") ? "Single" : "wrong storage")

// CHECK-NEXT: Optional
print(storageClassName(optionalsViaReserveCapacity(1)).hasPrefix("_TtGCs23_ContiguousArrayStorageGSq") ? "Optional" : "wrong storage")

// CHECK-NEXT: 2 true
let ns = optionalsViaReserveCapacity(2) as NSArray
print(ns.count, ns[0] is NSNull)
