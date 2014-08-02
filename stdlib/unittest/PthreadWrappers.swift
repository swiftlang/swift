//===--- PthreadWrappers.swift --------------------------------------------===//
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

import Darwin

//
// This file contains wrappers for functions in Darwin module.
//

@asmname("swift_stdlib_getExecuteBlockFunctionPtr_VoidPtr_VoidPtr")
func _stdlib_getExecuteBlockFunctionPtr_VoidPtr_VoidPtr()
  -> CFunctionPointer<((UnsafeMutablePointer<()>) -> UnsafeMutablePointer<()>)>

@asmname("swift_stdlib_getTypeMetadata")
func _stdlib_getTypeMetadata<T>(_: T.Type) -> UnsafePointer<Void>

struct ExecuteSwiftClosureContext<Argument, Result> {
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

extension pthread_attr_t {
  public init() {
#if arch(i386) || arch(arm)
    self = pthread_attr_t(
      __sig: 0,
      __opaque: (
        0, 0, 0, 0, 0,   0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,   0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,   0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,   0))
#elseif arch(x86_64) || arch(arm64)
    self = pthread_attr_t(
      __sig: 0,
      __opaque: (
        0, 0, 0, 0, 0,   0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,   0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,   0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,   0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,   0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,   0))
#endif
  }
}

extension pthread_mutex_t {
  public init() {
#if arch(i386) || arch(arm)
    self = pthread_mutex_t(
      __sig: 0,
      __opaque: (
        0, 0, 0, 0, 0,   0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,   0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,   0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,   0, 0, 0, 0, 0))
#elseif arch(x86_64) || arch(arm64)
    self = pthread_mutex_t(
      __sig: 0,
      __opaque: (
        0, 0, 0, 0, 0,   0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,   0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,   0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,   0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,   0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,   0))
#endif
  }
}

extension pthread_cond_t {
  public init() {
#if arch(i386) || arch(arm)
    self = pthread_cond_t(
      __sig: 0,
      __opaque: (
        0, 0, 0, 0, 0,   0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,   0, 0, 0, 0, 0,
        0, 0, 0, 0))
#elseif arch(x86_64) || arch(arm64)
    self = pthread_cond_t(
      __sig: 0,
      __opaque: (
        0, 0, 0, 0, 0,   0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,   0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,   0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,   0, 0, 0, 0, 0))
#endif
  }
}

/// Block-based wrapper `pthread_create`.
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

/// Block-based wrapper `pthread_join`.
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

