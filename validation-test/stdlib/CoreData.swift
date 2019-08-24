// RUN: %empty-directory(%t)

// RUN: %target-clang %S/Inputs/CoreDataHelper/CoreDataHelper.m -c -o %t/CoreDataHelper.o -g -fmodules
// RUN: %target-build-swift %s -import-objc-header %S/Inputs/CoreDataHelper/CoreDataHelper.h -Xlinker %t/CoreDataHelper.o -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main

// REQUIRES: executable_test
// REQUIRES: objc_interop

import CoreData
import StdlibUnittest

var CoreDataTests = TestSuite("CoreData")

CoreDataTests.test("conformsToProtocol") {
  expectTrue(NSDictionary.conforms(to: NSFetchRequestResult.self))
  expectTrue(NSManagedObject.conforms(to: NSFetchRequestResult.self))
}

CoreDataTests.test("downcasting") {
  var dictionaries = NSFetchRequest<NSFetchRequestResult>.testGettingSomeDictionaries()
  expectType([NSFetchRequestResult].self, &dictionaries)

  let casted = dictionaries as? [[NSObject: AnyObject]]
  expectNotNil(casted)
  expectEqual([[:] as NSDictionary, [:] as NSDictionary] as NSArray, casted! as NSArray)
  expectEqual([[:] as NSDictionary, [:] as NSDictionary] as NSArray, dictionaries as! [[NSObject: AnyObject]] as NSArray)
}

CoreDataTests.test("bridging") {
  var dictionaries = NSFetchRequest<NSDictionary>.testGettingSomeDictionaries()
  expectType([NSDictionary].self, &dictionaries)
  expectEqual([[:], [:]], dictionaries)

  let casted = dictionaries as? [[NSObject: AnyObject]]
  expectNotNil(casted)
  expectEqual([[:] as NSDictionary, [:] as NSDictionary] as NSArray, casted! as NSArray)
  expectEqual([[:] as NSDictionary, [:] as NSDictionary] as NSArray, dictionaries as! [[NSObject: AnyObject]] as NSArray)
}

runAllTests()
