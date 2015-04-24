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

/// An abstract base class to encapsulate the context necessary to invoke
/// a block from pthread_create.
internal class PthreadBlockContext {
  /// Execute the block, and return an `UnsafeMutablePointer` to memory
  /// allocated with `UnsafeMutablePointer.alloc` containing the result of the
  /// block.
  func run() -> UnsafeMutablePointer<Void> { fatalError("abstract") }
}

internal class PthreadBlockContextImpl<Argument, Result>: PthreadBlockContext {
  let block: (Argument) -> Result
  let arg: Argument

  init(block: (Argument) -> Result, arg: Argument) {
    self.block = block
    self.arg = arg
    super.init()
  }

  override func run() -> UnsafeMutablePointer<Void> {
    let result = UnsafeMutablePointer<Result>.alloc(1)
    result.initialize(block(arg))
    return UnsafeMutablePointer(result)
  }
}

/// Entry point for `pthread_create` that invokes a block context.
internal func invokeBlockContext(
  contextAsVoidPointer: UnsafeMutablePointer<Void>
) -> UnsafeMutablePointer<Void> {
  // The context is passed in +1; we're responsible for releasing it.
  let contextAsOpaque = COpaquePointer(contextAsVoidPointer)
  let context = Unmanaged<PthreadBlockContext>.fromOpaque(contextAsOpaque)
    .takeRetainedValue()

  return context.run()
}

/// Block-based wrapper for `pthread_create`.
public func _stdlib_pthread_create_block<Argument, Result>(
  attr: UnsafePointer<pthread_attr_t>,
  _ start_routine: (Argument) -> Result,
  _ arg: Argument
) -> (CInt, pthread_t?) {
  let context = PthreadBlockContextImpl(block: start_routine, arg: arg)
  // We hand ownership off to `invokeBlockContext` through its void context
  // argument.
  let contextAsOpaque = Unmanaged.passRetained(context)
    .toOpaque()
  let contextAsVoidPointer = UnsafeMutablePointer<Void>(contextAsOpaque)

  var threadID = pthread_t()
  let result = pthread_create(&threadID, attr,
    invokeBlockContext, contextAsVoidPointer)
  if result == 0 {
    return (result, threadID)
  } else {
    return (result, nil)
  }
}

/// Block-based wrapper for `pthread_join`.
public func _stdlib_pthread_join<Result>(
  thread: pthread_t,
  _ resultType: Result.Type
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
