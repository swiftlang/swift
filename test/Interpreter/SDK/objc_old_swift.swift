// RUN: %empty-directory(%t)
//
// RUN: cp %s %t/main.swift
// RUN: %target-clang %S/Inputs/OldABI/OldABI.mm -g -c -o %t/OldABI.o
// RUN: %target-build-swift %t/main.swift -framework Foundation -I %S/Inputs/OldABI/ -Xlinker %t/OldABI.o -o %t/objc_old_swift -Xfrontend -disable-access-control
// RUN: %target-codesign %t/objc_old_swift
// RUN: %target-run %t/objc_old_swift

// REQUIRES: executable_test
// REQUIRES: objc_interop

// Verify that objects that appear to be from the pre-stable Swift ABI
// are correctly ignored by stable Swift's entry points.

import Foundation
import OldABI
import StdlibUnittest

var tests = TestSuite("objc_old_swift")

tests.test("description")
  .skip(.custom({ !CanTestOldABI() },
                reason: "not using stable ABI's is-Swift bit yet"))
  .code {
  let obj = AllocOldABIObject()
  expectEqual(String(describing:obj), "OldABI.Subclass")
  expectEqual((obj as AnyObject).description!, "FakeSwiftObject instance")
}

tests.test("casts")
  .skip(.custom({ !CanTestOldABI() },
                reason: "not using stable ABI's is-Swift bit yet"))
  .code {
  let obj = AllocOldABIObject()
  expectNil(obj as? String)
  expectNotNil(obj as Any)
  expectNotNil(obj as AnyObject)
}

tests.test("array")
  .skip(.custom({ !CanTestOldABI() },
                reason: "not using stable ABI's is-Swift bit yet"))
  .code {
  let array = Array(repeating: AllocOldABIObject(), count:5)
  expectEqual(String(describing: array), "[OldABI.Subclass, OldABI.Subclass, OldABI.Subclass, OldABI.Subclass, OldABI.Subclass]")

  var array2 = Array(repeating: AllocOldABIObject(), count:0)
  for i in 0..<array.count {
    expectNotNil(array[i])
    array2.append(i as NSNumber)
    array2.append(array[i]);
  }
  expectEqual(String(describing: array2), "[0, OldABI.Subclass, 1, OldABI.Subclass, 2, OldABI.Subclass, 3, OldABI.Subclass, 4, OldABI.Subclass]")

  // Bridge an array of pre-stable objects to NSArray
  let nsarray = NSMutableArray(array: array2)
  expectEqual(nsarray.description, #"""
    (
        0,
        "FakeSwiftObject instance",
        1,
        "FakeSwiftObject instance",
        2,
        "FakeSwiftObject instance",
        3,
        "FakeSwiftObject instance",
        4,
        "FakeSwiftObject instance"
    )
    """#)

  nsarray.add(5 as NSNumber)

  // Bridge back from NSArray
  let array3 = nsarray as [AnyObject]
  expectEqual(String(describing: array3), "[0, OldABI.Subclass, 1, OldABI.Subclass, 2, OldABI.Subclass, 3, OldABI.Subclass, 4, OldABI.Subclass, 5]")
}

// FIXME: add coverage of more Swift runtime entrypoints

runAllTests()
