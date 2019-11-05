// RUN: %target-run-simple-swift(-Onone) | %FileCheck %s
// RUN: %target-run-simple-swift(-O) | %FileCheck -check-prefix=CHECK-OPT %s

// NOTE: We use FileCheck for the crashing test cases to make sure we crash for
// the correct reason in the test. We want to separate a memory management error
// from a cast error which prints a nice error message.

// FIXME: we should run this test if the OS-provided stdlib is recent enough.
// UNSUPPORTED: use_os_stdlib

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import StdlibUnittest

// At the end of the file, run all of the tests.
var Tests = TestSuite("BridgedCastFolding")
defer {
  runAllTests()
}

public func forcedCast<NS, T>(_ ns: NS) -> T {
  return ns as! T
}

public func condCast<NS, T>(_ ns: NS) -> T? {
  return ns as? T
}

// Check optimizations of casts from NSString to String
let nsString: NSString = "string🍕"
let swiftString: String = "string🍕"
let cfString: CFString = "string🍕" as CFString

Tests.test("NSString => String") {
  do {
    let o: String = forcedCast(nsString)
    expectEqual(o, swiftString)
  }

  do {
    let o: String? = condCast(nsString)
    expectEqual(o!, swiftString)
  }
}

Tests.test("NSString => Array<Int>. Crashing test case") {
  do {
    let o: Array<Int>? = condCast(nsString)
    expectNil(o)
  }

  // CHECK-LABEL: [ RUN      ] BridgedCastFolding.NSString => Array<Int>. Crashing test case
  // CHECK: stderr>>> Could not cast value of type '{{.*}}' (0x{{[0-9a-f]*}}) to 'NSArray' (0x{{[0-9a-f]*}}).
  // CHECK: stderr>>> OK: saw expected "crashed: sigabrt"
  // CHECK: [       OK ] BridgedCastFolding.NSString => Array<Int>. Crashing test case

  // CHECK-OPT-LABEL: [ RUN      ] BridgedCastFolding.NSString => Array<Int>. Crashing test case
  // CHECK-OPT: stderr>>> OK: saw expected "crashed: sigill"
  // CHECK-OPT: [       OK ] BridgedCastFolding.NSString => Array<Int>. Crashing test case
  expectCrashLater()
  do {
    let o: Array<Int> = forcedCast(nsString)
    expectEqual(o.count, 0)
  }
}

// Check optimizations of casts from NSNumber to Int

let nsIntNumber = NSNumber(value: 1)
let swiftIntNumber: Int = 1
let cfIntNumber: CFNumber = 1 as CFNumber

Tests.test("NSNumber => Int") {
  do {
    let o: Int = forcedCast(nsIntNumber)
    expectEqual(o, swiftIntNumber)
  }

  do {
    let o: Int? = condCast(nsIntNumber)
    expectEqual(o!, swiftIntNumber)
  }
}

// Check optimizations of casts from NSNumber to Double

let nsDoubleNumber = NSNumber(value: 1.234)
let swiftDoubleNumber: Double = 1.234
let swiftDoubleNumberWithInt: Double = 1

Tests.test("NSNumber => Double") {
  do {
    let o: Double = forcedCast(nsDoubleNumber)
    expectEqual(o, swiftDoubleNumber)
  }

  do {
    let o: Double? = condCast(nsDoubleNumber)
    expectEqual(o!, swiftDoubleNumber)
  }
}

// Check optimizations from NSNumber (Int) -> Double

Tests.test("NSNumber (Int) -> Double") {
  do {
    let o: Double = forcedCast(nsIntNumber)
    expectEqual(o, swiftDoubleNumberWithInt)
  }

  do {
    let o: Double? = condCast(nsIntNumber)
    expectEqual(o, swiftDoubleNumberWithInt)
  }
}

// Check that we fail when casting an NSNumber -> String
Tests.test("NSNumber (Int) -> String. Crashing test.") {
  do {
    let o: String? = condCast(nsIntNumber)
    expectNil(o)
  }

  // CHECK-LABEL: [ RUN      ] BridgedCastFolding.NSNumber (Int) -> String. Crashing test.
  // CHECK: stderr>>> Could not cast value of type '{{.*}}' (0x{{[0-9a-f]*}}) to 'NSString' (0x{{[0-9a-f]*}}).
  // CHECK: stderr>>> OK: saw expected "crashed: sigabrt"
  // CHECK: [       OK ] BridgedCastFolding.NSNumber (Int) -> String. Crashing test.

  // CHECK-OPT-LABEL: [ RUN      ] BridgedCastFolding.NSNumber (Int) -> String. Crashing test.
  // CHECK-OPT: stderr>>> OK: saw expected "crashed: sigill"
  // CHECK-OPT: [       OK ] BridgedCastFolding.NSNumber (Int) -> String. Crashing test.
  expectCrashLater()
  do {
    let o: String = forcedCast(nsIntNumber)
    expectEqual(o.count, 5)
  }
}

// Check optimization of casts from NSArray to Swift Array

let nsArrInt: NSArray = [1, 2, 3, 4]
let nsArrDouble: NSArray = [1.1, 2.2, 3.3, 4.4]
let nsArrString: NSArray = ["One🍕", "Two🍕", "Three🍕", "Four🍕"]
let swiftArrInt: [Int] = [1, 2, 3, 4]
let swiftArrDouble: [Double] = [1.1, 2.2, 3.3, 4.4]
let swiftArrString: [String] = ["One🍕", "Two🍕", "Three🍕", "Four🍕"]
let cfArrInt: CFArray = [1, 2, 3, 4] as CFArray
let cfArrDouble: CFArray = [1.1, 2.2, 3.3, 4.4] as CFArray
let cfArrString: CFArray = ["One🍕", "Two🍕", "Three🍕", "Four🍕"] as CFArray

Tests.test("NSArray -> Swift Array") {
  do {
    let arr: [Int] = forcedCast(nsArrInt)
    expectEqual(arr, swiftArrInt)
  }

  do {
    let arrOpt: [Int]? = condCast(nsArrInt)
    expectEqual(arrOpt!, swiftArrInt)
  }

  do {
    let arr: [Double] = forcedCast(nsArrDouble)
    expectEqual(arr, swiftArrDouble)
  }

  do {
    let arrOpt: [Double]? = condCast(nsArrDouble)
    expectEqual(arrOpt!, swiftArrDouble)
  }

  do {
    let arr: [String] = forcedCast(nsArrString)
    expectEqual(arr, swiftArrString)
  }

  do {
    let arrOpt: [String]? = condCast(nsArrString)
    expectEqual(arrOpt!, swiftArrString)
  }
}

Tests.test("NSArray (String) -> Swift Array (Int). Crashing.") {
  do {
    let arrOpt: [Int]? = condCast(nsArrString)
    expectNil(arrOpt)
  }

  // CHECK-LABEL: [ RUN      ] BridgedCastFolding.NSArray (String) -> Swift Array (Int). Crashing.
  // CHECK: stderr>>> Could not cast value of type '{{.*}}' (0x{{[0-9a-f]*}}) to 'NSNumber' (0x{{[0-9a-f]*}}).
  // CHECK: stderr>>> OK: saw expected "crashed: sigabrt"
  // CHECK: [       OK ] BridgedCastFolding.NSArray (String) -> Swift Array (Int). Crashing.

  // CHECK-OPT-LABEL: [ RUN      ] BridgedCastFolding.NSArray (String) -> Swift Array (Int). Crashing.
  // CHECK-OPT: stderr>>> Could not cast value of type '{{.*}}' (0x{{[0-9a-f]*}}) to 'NSNumber' (0x{{[0-9a-f]*}}).
  // CHECK-OPT: stderr>>> OK: saw expected "crashed: sigabrt"
  // CHECK-OPT: [       OK ] BridgedCastFolding.NSArray (String) -> Swift Array (Int). Crashing.
  expectCrashLater()
  do {
    let arr: [Int] = forcedCast(nsArrString)
    expectEqual(arr, swiftArrInt)
  }
}

Tests.test("NSArray (String) -> Swift Array (Double). Crashing.") {
  do {
    let arrOpt: [Double]? = condCast(nsArrString)
    expectNil(arrOpt)
  }

  // CHECK-LABEL: [ RUN      ] BridgedCastFolding.NSArray (String) -> Swift Array (Double). Crashing.
  // CHECK: stderr>>> Could not cast value of type '{{.*}}' (0x{{[0-9a-f]*}}) to 'NSNumber' (0x{{[0-9a-f]*}}).
  // CHECK: stderr>>> OK: saw expected "crashed: sigabrt"
  // CHECK: [       OK ] BridgedCastFolding.NSArray (String) -> Swift Array (Double). Crashing.
  // CHECK-OPT-LABEL: [ RUN      ] BridgedCastFolding.NSArray (String) -> Swift Array (Double). Crashing.
  // CHECK-OPT: stderr>>> Could not cast value of type '{{.*}}' (0x{{[0-9a-f]*}}) to 'NSNumber' (0x{{[0-9a-f]*}}).
  // CHECK-OPT: stderr>>> OK: saw expected "crashed: sigabrt"
  // CHECK-OPT: [       OK ] BridgedCastFolding.NSArray (String) -> Swift Array (Double). Crashing.
  expectCrashLater()
  do {
    let arr: [Double] = forcedCast(nsArrString)
    expectEqual(arr, swiftArrDouble)
  }
}

Tests.test("NSArray (Int) -> Swift Array (String). Crashing.") {
  do {
    let arrOpt: [String]? = condCast(nsArrInt)
    expectNil(arrOpt)
  }

  // CHECK-LABEL: [ RUN      ] BridgedCastFolding.NSArray (Int) -> Swift Array (String). Crashing.
  // CHECK: stderr>>> Could not cast value of type '{{.*}}' (0x{{[0-9a-f]*}}) to 'NSString' (0x{{[0-9a-f]*}}).
  // CHECK: stderr>>> OK: saw expected "crashed: sigabrt"
  // CHECK: [       OK ] BridgedCastFolding.NSArray (Int) -> Swift Array (String). Crashing.

  // CHECK-OPT-LABEL: [ RUN      ] BridgedCastFolding.NSArray (Int) -> Swift Array (String). Crashing.
  // CHECK-OPT: stderr>>> Could not cast value of type '{{.*}}' (0x{{[0-9a-f]*}}) to 'NSString' (0x{{[0-9a-f]*}}).
  // CHECK-OPT: stderr>>> OK: saw expected "crashed: sigabrt"
  // CHECK-OPT: [       OK ] BridgedCastFolding.NSArray (Int) -> Swift Array (String). Crashing.
  expectCrashLater()
  do {
    let arr: [String] = forcedCast(nsArrInt)
    expectEqual(arr, swiftArrString)
  }
}

// Check optimization of casts from NSDictionary to Swift Dictionary

let swiftDictInt: [Int: Int] = [1:1, 2:2, 3:3, 4:4]
let swiftDictDouble: [Double: Double] = [1.1 : 1.1, 2.2 : 2.2, 3.3 : 3.3, 4.4 : 4.4]
let swiftDictString: [String: String] = ["One🍕":"One🍕", "Two":"Two", "Three":"Three", "Four":"Four"]
let nsDictInt: NSDictionary = [1:1, 2:2, 3:3, 4:4]
let nsDictDouble: NSDictionary = [1.1 : 1.1, 2.2 : 2.2, 3.3 : 3.3, 4.4 : 4.4]
let nsDictString: NSDictionary = ["One🍕":"One🍕", "Two":"Two", "Three":"Three", "Four":"Four"]
let cfDictInt: CFDictionary = [1:1, 2:2, 3:3, 4:4] as CFDictionary
let cfDictDouble: CFDictionary = [1.1 : 1.1, 2.2 : 2.2, 3.3 : 3.3, 4.4 : 4.4] as CFDictionary
let cfDictString: CFDictionary = ["One🍕":"One🍕", "Two":"Two", "Three":"Three", "Four":"Four"] as CFDictionary

Tests.test("NSDictionary -> Swift (Dictionary)") {
  do {
    let dict: [Int: Int] = forcedCast(nsDictInt)
    expectEqual(dict, swiftDictInt)
  }

  do {
    let dictOpt: [Int: Int]? = condCast(nsDictInt)
    expectEqual(dictOpt!, swiftDictInt)
  }

  do {
    let dict: [Double: Double] = forcedCast(nsDictDouble)
    expectEqual(dict, swiftDictDouble)
  }

  do {
    let dictOpt: [Double: Double]? = condCast(nsDictDouble)
    expectEqual(dictOpt!, swiftDictDouble)
  }

  do {
    let dict: [String: String] = forcedCast(nsDictString)
    expectEqual(dict, swiftDictString)
  }

  do {
    let dictOpt: [String: String]? = condCast(nsDictString)
    expectEqual(dictOpt!, swiftDictString)
  }

  do {
    let dictOpt: [Int: Int]? = condCast(nsDictString)
    expectNil(dictOpt)
  }
}

Tests.test("NSDictionary -> Swift (Dictionary). Crashing Test Cases") {
  do {
    // Will this crash?
    let dictOpt: [Int: Int]? = condCast(nsDictString)
    expectNil(dictOpt)
  }

  // CHECK-LABEL: [ RUN      ] BridgedCastFolding.NSDictionary -> Swift (Dictionary). Crashing Test Cases
  // CHECK: stderr>>> Could not cast value of type '{{.*}}' (0x{{[0-9a-f]*}}) to 'NSNumber' (0x{{[0-9a-f]*}}).
  // CHECK: stderr>>> OK: saw expected "crashed: sigabrt"
  // CHECK: [       OK ] BridgedCastFolding.NSDictionary -> Swift (Dictionary). Crashing Test Cases
  //
  // CHECK-OPT-LABEL: [ RUN      ] BridgedCastFolding.NSDictionary -> Swift (Dictionary). Crashing Test Cases
  // CHECK-OPT: stderr>>> Could not cast value of type '{{.*}}' (0x{{[0-9a-f]*}}) to 'NSNumber' (0x{{[0-9a-f]*}}).
  // CHECK-OPT: stderr>>> OK: saw expected "crashed: sigabrt"
  // CHECK-OPT: [       OK ] BridgedCastFolding.NSDictionary -> Swift (Dictionary). Crashing Test Cases
  expectCrashLater()
  do {
    // Will this crash?
    let dictOpt: [Int: Int] = forcedCast(nsDictString)
    expectEqual(dictOpt.count, 4)
  }
}

// Check optimization of casts from NSSet to Swift Set

let swiftSetInt: Set<Int> = [1, 2, 3, 4]
let swiftSetDouble: Set<Double> = [1.1, 2.2, 3.3, 4.4]
let swiftSetString: Set<String> = ["One🍕", "Two🍕", "Three🍕", "Four🍕"]
let nsSetInt: NSSet = [1, 2, 3, 4]
let nsSetDouble: NSSet = [1.1, 2.2, 3.3, 4.4]
let nsSetString: NSSet = ["One🍕", "Two🍕", "Three🍕", "Four🍕"]
let cfSetInt: CFSet = [1, 2, 3, 4] as NSSet
let cfSetDouble: CFSet = [1.1, 2.2, 3.3, 4.4] as NSSet
let cfSetString: CFSet = ["One🍕", "Two🍕", "Three🍕", "Four🍕"] as NSSet

Tests.test("NSSet -> Swift Set") {
  do {
    let s: Set<Int> = forcedCast(nsSetInt)
    expectEqual(s, swiftSetInt)
  }

  do {
    let s: Set<Int>? = condCast(nsSetInt)
    expectEqual(s!, swiftSetInt)
  }

  do {
    let s: Set<Double> = forcedCast(nsSetDouble)
    expectEqual(s, swiftSetDouble)
  }

  do {
    let s: Set<Double>? = condCast(nsSetDouble)
    expectEqual(s!, swiftSetDouble)
  }

  do {
    let s: Set<String> = forcedCast(nsSetString)
    expectEqual(s, swiftSetString)
  }

  do {
    let s: Set<String>? = condCast(nsSetString)
    expectEqual(s, swiftSetString)
  }
}

// Check optimizations of casts from String to NSString

Tests.test("String -> NSString") {
  do {
    let o: NSString = forcedCast(swiftString)
    expectEqual(o, nsString)
  }

  do {
    let o: NSString? = condCast(swiftString)
    expectEqual(o!, nsString)
  }
}

// Check crashing case from String -> NSNumber

Tests.test("String -> NSNumber. Crashing Test Case") {
  do {
    let o: NSNumber! = condCast(swiftString)
    expectNil(o)
  }

  // CHECK-LABEL: [ RUN      ] BridgedCastFolding.String -> NSNumber. Crashing Test Case
  // CHECK: stderr>>> Could not cast value of type '{{.*}}' (0x{{[0-9a-f]*}}) to 'NSNumber' (0x{{[0-9a-f]*}}).
  // CHECK: stderr>>> OK: saw expected "crashed: sigabrt"
  // CHECK: [       OK ] BridgedCastFolding.String -> NSNumber. Crashing Test Case

  // CHECK-OPT-LABEL: [ RUN      ] BridgedCastFolding.String -> NSNumber. Crashing Test Case
  // CHECK-OPT: stderr>>> OK: saw expected "crashed: sigill"
  // CHECK-OPT: [       OK ] BridgedCastFolding.String -> NSNumber. Crashing Test Case
  expectCrashLater()
  do {
    let o: NSNumber = forcedCast(swiftString)
    expectEqual(o, 123)
  }
}

// Check optimizations of casts from Int to NSNumber

Tests.test("Int -> NSNumber") {
  do {
    let o: NSNumber = forcedCast(swiftIntNumber)
    expectEqual(o, nsIntNumber)
  }

  do {
    let o: NSNumber? = condCast(swiftIntNumber)
    expectEqual(o!, nsIntNumber)
  }
}

// Check optimizations of casts from Double to NSNumber

Tests.test("Double -> NSNumber") {
  do {
    let o: NSNumber = forcedCast(swiftDoubleNumber)
    expectEqual(o, nsDoubleNumber)
  }

  do {
    let o: NSNumber? = condCast(swiftDoubleNumber)
    expectEqual(o!, nsDoubleNumber)
  }
}

// Check optimization of casts from Swift Array to NSArray

Tests.test("Swift<Int> -> NSArray (NSNumber)") {
  do {
    let arr: NSArray = forcedCast(swiftArrInt)
    expectEqual(arr, nsArrInt)
  }

  do {
    let arrOpt: NSArray? = condCast(swiftArrInt)
    expectEqual(arrOpt!, nsArrInt)
  }

  do {
    let arr: NSArray = forcedCast(swiftArrDouble)
    expectEqual(arr, nsArrDouble)
  }

  do {
    let arrOpt: NSArray? = condCast(swiftArrDouble)
    expectEqual(arrOpt!, nsArrDouble)
  }

  do {
    let arr: NSArray = forcedCast(swiftArrString)
    expectEqual(arr, nsArrString)
  }

  do {
    let arrOpt: NSArray? = condCast(swiftArrString)
    expectEqual(arrOpt!, nsArrString)
  }
}

// Check optimization of casts from Swift Dict to NSDict

Tests.test("Swift Dict -> NSDict.") {
  do {
    let dict: NSDictionary = forcedCast(swiftDictInt)
    expectEqual(dict, nsDictInt)
  }

  do {
    let dictOpt: NSDictionary? = condCast(swiftDictInt)
    expectEqual(dictOpt!, nsDictInt)
  }

  do {
    let dict: NSDictionary = forcedCast(swiftDictDouble)
    expectEqual(dict, nsDictDouble)
  }

  do {
    let dictOpt: NSDictionary? = condCast(swiftDictDouble)
    expectEqual(dictOpt!, nsDictDouble)
  }

  do {
    let dict: NSDictionary = forcedCast(swiftDictString)
    expectEqual(dict, nsDictString)
  }

  do {
    let dictOpt: NSDictionary? = condCast(swiftDictString)
    expectEqual(dictOpt!, nsDictString)
  }
}

// Check optimization of casts from Swift Set to NSSet

Tests.test("Swift Set -> NSSet") {
  do {
    let d: NSSet = forcedCast(swiftSetInt)
    expectEqual(d, nsSetInt)
  }

  do {
    let setOpt: NSSet? = condCast(swiftSetInt)
    expectEqual(setOpt!, nsSetInt)
  }

  do {
    let set: NSSet = forcedCast(swiftSetDouble)
    expectEqual(set, nsSetDouble)
  }

  do {
    let setOpt: NSSet? = condCast(swiftSetDouble)
    expectEqual(setOpt!, nsSetDouble)
  }

  do {
    let set: NSSet = forcedCast(swiftSetString)
    expectEqual(set, nsSetString)
  }

  do {
    let setOpt: NSSet? = condCast(swiftSetString)
    expectEqual(setOpt!, nsSetString)
  }
}

// Check optimizations of casts from String to CFString

Tests.test("String -> CFString") {
  do {
    let o: CFString = forcedCast(swiftString)
    expectEqual(o, cfString)
  }

  do {
    let o: CFString? = condCast(swiftString)
    expectEqual(o!, cfString)
  }
}

// Check optimizations of casts from Int to CFNumber

Tests.test("Int -> CFNumber") {
  do {
    let o: CFNumber = forcedCast(swiftIntNumber)
    expectEqual(o, cfIntNumber)
  }

  do {
    let o: CFNumber? = condCast(swiftIntNumber)
    expectEqual(o!, cfIntNumber)
  }
}

// Check optimization of casts from Swift Array to CFArray

Tests.test("Swift Array -> CFArray") {
  do {
    let arr: CFArray = forcedCast(swiftArrInt)
    expectEqual(arr, cfArrInt)
  }

  do {
    let arrOpt: CFArray? = condCast(swiftArrInt)
    expectEqual(arrOpt!, cfArrInt)
  }
}

// Check optimization of casts from Swift Dict to CFDictionary

Tests.test("Swift Dict -> CFDictionary") {
  do {
    let dict: CFDictionary = forcedCast(swiftDictInt)
    expectEqual(dict, cfDictInt)
  }

  do {
    let dictOpt: CFDictionary? = condCast(swiftDictInt)
    expectEqual(dictOpt!, cfDictInt)
  }
}

// Check optimization of casts from Swift Set to CFSet

Tests.test("Swift Set -> CFSet") {
  do {
    let set: CFSet = forcedCast(swiftSetInt)
    expectEqual(set, cfSetInt)
  }

  do {
    let setOpt: CFSet? = condCast(swiftSetInt)
    expectEqual(setOpt! as NSSet, swiftSetInt as NSSet)
  }
}

// Check AnyHashable. We do not support this today... so just make sure we do
// not miscompile.

public class NSObjectSubclass : NSObject { }

let anyHashable: AnyHashable = 0


class MyThing: Hashable {
    let name: String

    init(name: String) {
        self.name = name
    }

    deinit {
        Swift.print("Deinit \(name)")
    }

    func hash(into hasher: inout Hasher) {}

    static func ==(lhs: MyThing, rhs: MyThing) -> Bool {
        return false
    }
}

@inline(never)
func doSomethingWithAnyHashable(_ item: AnyHashable) -> MyThing? {
  return item as? MyThing
}

Tests.test("AnyHashable") {
  do {
    let x = MyThing(name: "B")
    let r = doSomethingWithAnyHashable(x)
    expectEqual(r!.name, x.name)
  }
}

