// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/main -import-objc-header %S/Inputs/objc_bridge_cast_newtype.h
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main

// REQUIRES: executable_test
// REQUIRES: objc_interop

// Test dynamic casts that bridge value types through the runtime.

import Foundation
import StdlibUnittest

var importSuite = TestSuite("objc_bridge_cast_newtype.import")
var exportSuite = TestSuite("objc_bridge_cast_newtype.export")

importSuite.test("NSArrayToArrayOfStringWrappers") {
  expectEqual(["abc", "def"],
              (getUntypedNSArrayOfStrings() as? [StringWrapper])?.map { $0.rawValue })
  expectEqual(["abc", "def"],
              (getOpaqueNSArrayOfStrings() as? [StringWrapper])?.map { $0.rawValue })

  expectEqual(["abc", "def"],
              getTypedNSArrayOfStringWrappers().map { $0.rawValue })

  expectEqual(["abc", "def"], getTypedNSArrayOfStringWrappers() as [NSString])
}

importSuite.test("NSArrayToArrayOfCopyingWrappers") {
  expectEqual(["abc" as NSString, [] as NSArray],
              (getUntypedNSArrayOfCopyings() as? [CopyingWrapper])?.map { $0.rawValue } as! [NSObject]?)
  expectEqual(["abc" as NSString, [] as NSArray],
              (getOpaqueNSArrayOfCopyings() as? [CopyingWrapper])?.map { $0.rawValue } as! [NSObject]?)

  expectEqual(["abc" as NSString, [] as NSArray],
              getTypedNSArrayOfCopyingWrappers().map { $0.rawValue } as! [NSObject])
}

importSuite.test("NSArrayToArrayOfCopyingAndCodingWrappers") {
  expectEqual(["abc" as NSString, [] as NSArray],
              (getUntypedNSArrayOfCopyingAndCodings() as? [CopyingAndCodingWrapper])?.map { $0.rawValue } as! [NSObject]?)
  expectEqual(["abc" as NSString, [] as NSArray],
              (getOpaqueNSArrayOfCopyingAndCodings() as? [CopyingAndCodingWrapper])?.map { $0.rawValue } as! [NSObject]?)

  expectEqual(["abc" as NSString, [] as NSArray],
              getTypedNSArrayOfCopyingAndCodingWrappers().map { $0.rawValue } as! [NSObject])
}

importSuite.test("NSArrayToArrayOfObjectWrappers") {
  expectEqual(["abc" as NSString, [] as NSArray],
              (getUntypedNSArrayOfObjects() as? [ObjectWrapper])?.map { $0.rawValue })
  expectEqual(["abc" as NSString, [] as NSArray],
              (getOpaqueNSArrayOfObjects() as? [ObjectWrapper])?.map { $0.rawValue })

  expectEqual(["abc" as NSString, [] as NSArray],
              getTypedNSArrayOfObjectWrappers().map { $0.rawValue })
}

importSuite.test("NSArrayToArrayOfErrorWrappers") {
  expectEqual([11, 22],
              (getUntypedNSArrayOfErrors() as? [ErrorWrapper])?.map { $0.rawValue._code })
  expectEqual([11, 22],
              (getOpaqueNSArrayOfErrors() as? [ErrorWrapper])?.map { $0.rawValue._code })

  expectEqual([11, 22],
              getTypedNSArrayOfErrorWrappers().map { $0.rawValue._code })
}

exportSuite.test("ArrayOfStringWrappersToNSArray") {
  let array = [StringWrapper("abc"), StringWrapper("def")]
  expectTrue(checkUntypedNSArrayOfStrings(array))
  expectTrue(checkOpaqueNSArrayOfStrings(array))
  expectTrue(checkTypedNSArrayOfStringWrappers(array))
}

exportSuite.test("ArrayOfCopyingAndCodingWrappersToNSArray") {
  let array = [CopyingAndCodingWrapper("abc" as NSString),
               CopyingAndCodingWrapper([] as NSArray)]
  expectTrue(checkUntypedNSArrayOfCopyingAndCodings(array))
  expectTrue(checkOpaqueNSArrayOfCopyingAndCodings(array))
  expectTrue(checkTypedNSArrayOfCopyingAndCodingWrappers(array))
}

exportSuite.test("ArrayOfCopyingWrappersToNSArray") {
  let array = [CopyingWrapper("abc" as NSString), CopyingWrapper([] as NSArray)]
  expectTrue(checkUntypedNSArrayOfCopyings(array))
  expectTrue(checkOpaqueNSArrayOfCopyings(array))
  expectTrue(checkTypedNSArrayOfCopyingWrappers(array))
}

exportSuite.test("ArrayOfObjectWrappersToNSArray") {
  let array = [ObjectWrapper("abc" as NSString), ObjectWrapper([] as NSArray)]
  expectTrue(checkUntypedNSArrayOfObjects(array))
  expectTrue(checkOpaqueNSArrayOfObjects(array))
  expectTrue(checkTypedNSArrayOfObjectWrappers(array))
}

enum CustomError: Int, Error {
  case twentyTwo = 22
}

exportSuite.test("ArrayOfErrorWrappersToNSArray") {
  let array = [ErrorWrapper(NSError(domain: "Swift", code: 11, userInfo: nil)),
               ErrorWrapper(CustomError.twentyTwo)]
  expectTrue(checkUntypedNSArrayOfErrors(array))
  expectTrue(checkOpaqueNSArrayOfErrors(array))
  expectTrue(checkTypedNSArrayOfErrorWrappers(array))
}


runAllTests()
