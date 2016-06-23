// RUN: rm -rf %t && mkdir -p %t

// RUN: %target-clang %S/Inputs/CoreDataHelper/CoreDataHelper.m -c -o %t/CoreDataHelper.o -g -fmodules
// RUN: %target-build-swift %s -import-objc-header %S/Inputs/CoreDataHelper/CoreDataHelper.h -Xlinker %t/CoreDataHelper.o -o %t/main
// RUN: %target-run %t/main

// REQUIRES: executable_test
// REQUIRES: objc_interop

import CoreData
import StdlibUnittest

var CoreDataTests = TestSuite("CoreData")

CoreDataTests.test("conformsToProtocol") {
  expectTrue(NSDictionary.conformsToProtocol(NSFetchRequestResult.self))
  expectTrue(NSManagedObject.conformsToProtocol(NSFetchRequestResult.self))
}

CoreDataTests.test("downcasting") {
  var dictionaries = NSFetchRequest.testGettingSomeDictionaries()
  expectType([NSFetchRequestResult].self, &dictionaries)

  let casted = dictionaries as? [[NSObject: AnyObject]]
  expectNotEmpty(casted)
  expectEqual([[:], [:]], casted!)
  expectEqual([[:], [:]], dictionaries as! [[NSObject: AnyObject]])
}

runAllTests()
