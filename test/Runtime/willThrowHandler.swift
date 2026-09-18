// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// These tests crash on WASI.
// UNSUPPORTED: OS=wasip1

import StdlibUnittest

// Error isn't allowed in a @convention(c) function when ObjC interop is not
// available, so pass it through an UnsafeRawPointer.
typealias WillThrowHandler = @convention(c) (UnsafeRawPointer) -> Void
typealias WillThrowTypedHandler =
  @convention(c) (UnsafeRawPointer, UnsafeRawPointer, UnsafeRawPointer) -> Void

typealias WillThrowOldHandlerCallback =
  @convention(c) (WillThrowHandler?) -> Void
typealias WillThrowTypedOldHandlerCallback =
  @convention(c) (WillThrowTypedHandler?) -> Void

@_silgen_name("_swift_setWillThrowHandler")
func setWillThrowHandler(
  _ handler: WillThrowHandler?, _ saveOldHandler: WillThrowOldHandlerCallback?
)

@_silgen_name("_swift_setWillThrowTypedHandler")
func setWillThrowTypedHandler(
  _ handler: WillThrowTypedHandler?,
  _ saveOldHandler: WillThrowTypedOldHandlerCallback?
)

enum SillyError: Error { case JazzHands }

@inline(never)
func throwJazzHands() throws {
  throw SillyError.JazzHands
}

@inline(never)
func throwJazzHandsTyped() throws(SillyError) {
  throw .JazzHands
}

var errors: [Error] = []
let appendingHandler: WillThrowHandler = {
  errors.append(unsafeBitCast($0, to: Error.self))
}

var typedCalls = 0
let countingTypedHandler: WillThrowTypedHandler = { _, _, _ in typedCalls += 1 }

var savedHandler: WillThrowHandler?
let saveHandler: WillThrowOldHandlerCallback = { savedHandler = $0 }

var savedTypedHandler: WillThrowTypedHandler?
let saveTypedHandler: WillThrowTypedOldHandlerCallback = {
  savedTypedHandler = $0
}

func rawPointer(_ handler: WillThrowHandler?) -> UnsafeRawPointer? {
  unsafeBitCast(handler, to: UnsafeRawPointer.self)
}

func rawPointer(_ handler: WillThrowTypedHandler?) -> UnsafeRawPointer? {
  unsafeBitCast(handler, to: UnsafeRawPointer.self)
}

let WillThrowTests = TestSuite("WillThrowHandler")

WillThrowTests.test("untyped") {
  savedHandler = appendingHandler
  setWillThrowHandler(appendingHandler, saveHandler)
  // Nothing was installed before, so the saved handler is null.
  expectNil(savedHandler)
  defer {
    setWillThrowHandler(nil, saveHandler)
    expectEqual(rawPointer(appendingHandler), rawPointer(savedHandler))
  }

  expectTrue(errors.isEmpty)
  do {
    try throwJazzHands()
  } catch {}
  expectEqual(1, errors.count)
  expectEqual(SillyError.self, type(of: errors.last!))

  // A typed throw with no typed handler installed boxes the error and reaches
  // the untyped handler.
  do {
    try throwJazzHandsTyped()
  } catch {}
  expectEqual(2, errors.count)
  expectEqual(SillyError.self, type(of: errors.last!))
}

WillThrowTests.test("typed") {
  // A caller that does not chain passes no callback.
  setWillThrowTypedHandler(countingTypedHandler, nil)

  do {
    try throwJazzHandsTyped()
  } catch {}
  expectEqual(1, typedCalls)

  setWillThrowTypedHandler(nil, saveTypedHandler)
  expectEqual(rawPointer(countingTypedHandler), rawPointer(savedTypedHandler))

  do {
    try throwJazzHandsTyped()
  } catch {}
  expectEqual(1, typedCalls)
}

runAllTests()
