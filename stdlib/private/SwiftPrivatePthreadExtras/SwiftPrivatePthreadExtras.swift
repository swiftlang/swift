//===--- SwiftPrivatePthreadExtras.swift ----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file contains wrappers for pthread APIs that are less painful to use
// than the C APIs.
//
//===----------------------------------------------------------------------===//

#if os(OSX) || os(iOS) || os(watchOS) || os(tvOS)
import Darwin
#elseif os(Linux)
import Glibc
#endif

@asmname("swift_stdlib_getExecuteBlockFunctionPtr_VoidPtr_VoidPtr")
func _stdlib_getExecuteBlockFunctionPtr_VoidPtr_VoidPtr()
  -> CFunctionPointer<((UnsafeMutablePointer<()>) -> UnsafeMutablePointer<()>)>

@asmname("swift_stdlib_getTypeMetadata")
func _stdlib_getTypeMetadata<T>(_: T.Type) -> UnsafePointer<Void>

public struct ExecuteSwiftClosureContext<Argument, Result> {
  let argumentTypeMetadata: UnsafePointer<Void>
  let resultTypeMetadata: UnsafePointer<Void>
  let closure: ((Argument) -> Result)
  let arg: Argument
}

/// Execute a Swift closure with a given argument.
///
/// :param: closureAndArg closure and its argument.  The function takes
///   ownership of this pointer.
@asmname("swift_stdlib_executeSwiftClosure")
public // COMPILER_INTRINSIC
func _stdlib_executeSwiftClosure<Param, Result>(
  context: UnsafeMutablePointer<ExecuteSwiftClosureContext<Param, Result>>
) -> UnsafeMutablePointer<Result> {
  let closure = context.memory.closure
  let arg = context.memory.arg
  context.destroy()
  context.dealloc(1)

  var result = UnsafeMutablePointer<Result>.alloc(1)
  result.initialize(closure(arg))
  return result
}

/// Block-based wrapper for `pthread_create`.
public func _stdlib_pthread_create_block<Argument, Result>(
  attr: UnsafePointer<pthread_attr_t>,
  start_routine: (Argument) -> Result,
  arg: Argument
) -> (CInt, pthread_t?) {

  let thunk = _stdlib_getExecuteBlockFunctionPtr_VoidPtr_VoidPtr()
  var thunkArg =
    UnsafeMutablePointer<ExecuteSwiftClosureContext<Argument, Result>>.alloc(1)
  thunkArg.initialize(ExecuteSwiftClosureContext(
    argumentTypeMetadata: _stdlib_getTypeMetadata(Argument.self),
    resultTypeMetadata: _stdlib_getTypeMetadata(Result.self),
    closure: start_routine,
    arg: arg))

  var threadID = pthread_t()
  let result = pthread_create(&threadID, attr, thunk, thunkArg)
  if result == 0 {
    return (result, threadID)
  } else {
    return (result, nil)
  }
}

/// Block-based wrapper for `pthread_join`.
public func _stdlib_pthread_join<Result>(
  thread: pthread_t,
  resultType: Result.Type
) -> (CInt, Result?) {
  var threadResultPtr = UnsafeMutablePointer<Void>()
  let result = pthread_join(thread, &threadResultPtr)
  if result == 0 {
    let threadResult = UnsafeMutablePointer<Result>(threadResultPtr).memory
    threadResultPtr.destroy()
    threadResultPtr.dealloc(1)
    return (result, threadResult)
  } else {
    return (result, nil)
  }
}

public class _stdlib_Barrier {
  var _pthreadBarrier: _stdlib_pthread_barrier_t

  var _pthreadBarrierPtr: UnsafeMutablePointer<_stdlib_pthread_barrier_t> {
    return UnsafeMutablePointer(_getUnsafePointerToStoredProperties(self))
  }

  public init(threadCount: Int) {
    self._pthreadBarrier = _stdlib_pthread_barrier_t()
    let ret = _stdlib_pthread_barrier_init(
      _pthreadBarrierPtr, nil, CUnsignedInt(threadCount))
    if ret != 0 {
      fatalError("_stdlib_pthread_barrier_init() failed")
    }
  }

  deinit {
    _stdlib_pthread_barrier_destroy(_pthreadBarrierPtr)
  }

  public func wait() {
    let ret = _stdlib_pthread_barrier_wait(_pthreadBarrierPtr)
    if !(ret == 0 || ret == _stdlib_PTHREAD_BARRIER_SERIAL_THREAD) {
      fatalError("_stdlib_pthread_barrier_wait() failed")
    }
  }
}
