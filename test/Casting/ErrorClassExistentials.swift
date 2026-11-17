// Tests runtime metadata and casts for class-constrained existentials that
// include Error, such as `any Error & AnyObject` and `any Base & Error`.
// RUN: %empty-directory(%t)
//
// RUN: %target-build-swift -swift-version 5 -g -Onone -module-name a %s -o %t/a.Onone.out
// RUN: %target-codesign %t/a.Onone.out
// RUN: %target-run %t/a.Onone.out
//
// RUN: %target-build-swift -swift-version 5 -g -O -module-name a %s -o %t/a.O.out
// RUN: %target-codesign %t/a.O.out
// RUN: %target-run %t/a.O.out
//
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest
#if _runtime(_ObjC)
import Foundation
#endif

let ErrorClassExistentials = TestSuite("ErrorClassExistentials")

class ErrorClass: Error {
  var payload = 0x4141_4141_4140
}

class Base {}
class ErrorSubclass: Base, Error {
  var payload = 0x4141_4141_4140
}

typealias ErrorAnyObject = any Error & AnyObject
typealias BaseError = any Base & Error

@inline(never)
func opaque<T>(_ value: T) -> T { value }

@inline(never)
func eraseToAny<T>(_ value: T) -> Any { value }

@inline(never)
func runtimeSize(_ type: Any.Type) -> Int {
  func size<T>(_: T.Type) -> Int { MemoryLayout<T>.size }
  return _openExistential(type, do: size)
}

@inline(never)
func isErrorType(_ type: Any.Type) -> Bool {
  return type is Error.Type
}

ErrorClassExistentials.test("any Error is still special") {
  expectEqual(MemoryLayout<any Error>.size, runtimeSize(opaque((any Error).self)))
  expectTrue(isErrorType(opaque((any Error).self)))
}

ErrorClassExistentials.test("any Error & AnyObject/metadata") {
  expectEqual(MemoryLayout<ErrorAnyObject>.size,
              runtimeSize(opaque(ErrorAnyObject.self)))
  expectEqual(2 * MemoryLayout<Int>.size,
              runtimeSize(opaque(ErrorAnyObject.self)))
  expectFalse(isErrorType(opaque(ErrorAnyObject.self)))
}

ErrorClassExistentials.test("any Error & AnyObject/cast from Any") {
  let object = ErrorClass()
  let any: Any = opaque(object)
  guard let e = any as? ErrorAnyObject else {
    expectUnreachable()
    return
  }
  expectTrue(e === object)
  expectEqual(1, e._code)
  expectEqual("a.ErrorClass", e._domain)
}

ErrorClassExistentials.test("any Error & AnyObject/round trip through Any") {
  let object = ErrorClass()
  let e: ErrorAnyObject = object
  let any = eraseToAny(e)
  expectTrue((any as? ErrorClass) === object)
  expectTrue((any as? ErrorAnyObject) === object)
  expectNil(any as? Int)
  guard let error = any as? Error else {
    expectUnreachable()
    return
  }
  expectTrue((error as? ErrorClass) === object)
  expectEqual(1, error._code)
}

ErrorClassExistentials.test("any Base & Error/metadata") {
  expectEqual(MemoryLayout<BaseError>.size,
              runtimeSize(opaque(BaseError.self)))
  expectEqual(2 * MemoryLayout<Int>.size,
              runtimeSize(opaque(BaseError.self)))
  expectFalse(isErrorType(opaque(BaseError.self)))
}

ErrorClassExistentials.test("any Base & Error/cast from Any") {
  let object = ErrorSubclass()
  let any: Any = opaque(object)
  guard let e = any as? BaseError else {
    expectUnreachable()
    return
  }
  expectTrue(e === object)
  expectEqual(1, e._code)
  expectEqual("a.ErrorSubclass", e._domain)
  expectNil(eraseToAny(Base()) as? BaseError)
}

ErrorClassExistentials.test("any Base & Error/round trip through Any") {
  let object = ErrorSubclass()
  let e: BaseError = object
  let any = eraseToAny(e)
  expectTrue((any as? ErrorSubclass) === object)
  expectTrue((any as? BaseError) === object)
  expectTrue((any as? Base) === object)
  guard let error = any as? Error else {
    expectUnreachable()
    return
  }
  expectTrue((error as? ErrorSubclass) === object)
  expectEqual(1, error._code)
}

#if _runtime(_ObjC)
class ErrorObject: NSObject, Error {
  var payload = 0x4141_4141_4140
}

typealias NSObjectError = any NSObject & Error

ErrorClassExistentials.test("any NSObject & Error/metadata") {
  expectEqual(MemoryLayout<NSObjectError>.size,
              runtimeSize(opaque(NSObjectError.self)))
  expectEqual(2 * MemoryLayout<Int>.size,
              runtimeSize(opaque(NSObjectError.self)))
  expectFalse(isErrorType(opaque(NSObjectError.self)))
}

ErrorClassExistentials.test("any NSObject & Error/cast from Any") {
  let object = ErrorObject()
  let any: Any = opaque(object)
  guard let e = any as? NSObjectError else {
    expectUnreachable()
    return
  }
  expectTrue(e === object)
  expectEqual(1, e._code)
  let roundTrip = eraseToAny(e)
  expectTrue((roundTrip as? ErrorObject) === object)
  expectTrue((roundTrip as? NSObjectError) === object)
  expectNil(eraseToAny(NSObject()) as? NSObjectError)
}
#endif

runAllTests()
