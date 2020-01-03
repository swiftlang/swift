// RUN: %empty-directory(%t)
//
// RUN: %target-clang -fobjc-arc %S/../Inputs/ObjCClasses/ObjCClasses.m -c -o %t/ObjCClasses.o
// RUN: %target-build-swift -I %S/../Inputs/ObjCClasses/ %t/ObjCClasses.o %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: objc_interop

import ObjCClasses
import Foundation
import StdlibUnittest

var ErrorHandlingTests = TestSuite("ErrorHandling")

ErrorHandlingTests.test("nil") {
  do {
    try TestingNSError.throwNilError()
    expectUnreachable()
  } catch {
    expectEqual("Foundation._GenericObjCError.nilError", String(reflecting: error))
  }
}

ErrorHandlingTests.test("pointerFailure") {
  do {
    _ = try TestingNSError.maybeThrow(true)
    expectUnreachable()
  } catch let error as NSError {
    expectEqual("pointer error", error.domain)
  }
}

ErrorHandlingTests.test("pointerSuccess") {
  do {
    var pointer = try TestingNSError.maybeThrow(false)
    expectType(UnsafeMutableRawPointer.self, &pointer)
    expectEqual(UnsafeMutablePointer(bitPattern: 42)!, pointer)
  } catch {
    expectUnreachableCatch(error)
  }
}

ErrorHandlingTests.test("blockFailure") {
  do {
    _ = try TestingNSError.blockThrowError()
    expectUnreachable()
  } catch let error as NSError {
    expectEqual("block error", error.domain)
  }
}

runAllTests()

