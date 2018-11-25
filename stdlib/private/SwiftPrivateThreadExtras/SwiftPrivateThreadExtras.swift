//===--- SwiftPrivateThreadExtras.swift ----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file contains wrappers for pthread APIs that are less painful to use
// than the C APIs.
//
//===----------------------------------------------------------------------===//

#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
import Darwin
#elseif os(Linux) || os(FreeBSD) || os(PS4) || os(Android) || os(Cygwin) || os(Haiku)
import Glibc
#endif

/// An abstract base class to encapsulate the context necessary to invoke
/// a block from pthread_create.
internal class ThreadBlockContext {
  /// Execute the block, and return an `UnsafeMutablePointer` to memory
  /// allocated with `UnsafeMutablePointer.alloc` containing the result of the
  /// block.
  func run() -> UnsafeMutableRawPointer { fatalError("abstract") }
}

internal class ThreadBlockContextImpl<Argument, Result>: ThreadBlockContext {
  let block: (Argument) -> Result
  let arg: Argument

  init(block: @escaping (Argument) -> Result, arg: Argument) {
    self.block = block
    self.arg = arg
    super.init()
  }

  override func run() -> UnsafeMutableRawPointer {
    let result = UnsafeMutablePointer<Result>.allocate(capacity: 1)
    result.initialize(to: block(arg))
    return UnsafeMutableRawPointer(result)
  }
}

/// Entry point for `pthread_create` that invokes a block context.
internal func invokeBlockContext(
  _ contextAsVoidPointer: UnsafeMutableRawPointer?
) -> UnsafeMutableRawPointer! {
  // The context is passed in +1; we're responsible for releasing it.
  let context = Unmanaged<ThreadBlockContext>
    .fromOpaque(contextAsVoidPointer!)
    .takeRetainedValue()

  return context.run()
}

#if os(Cygwin) || os(FreeBSD) || os(Haiku)
public typealias _stdlib_pthread_attr_t = UnsafePointer<pthread_attr_t?>
#else
public typealias _stdlib_pthread_attr_t = UnsafePointer<pthread_attr_t>
#endif

/// Block-based wrapper for `pthread_create`.
public func _stdlib_pthread_create_block<Argument, Result>(
  _ attr: _stdlib_pthread_attr_t?,
  _ start_routine: @escaping (Argument) -> Result,
  _ arg: Argument
) -> (CInt, pthread_t?) {
  let context = ThreadBlockContextImpl(block: start_routine, arg: arg)
  // We hand ownership off to `invokeBlockContext` through its void context
  // argument.
  let contextAsVoidPointer = Unmanaged.passRetained(context).toOpaque()

  var threadID = _make_pthread_t()
  let result = pthread_create(&threadID, attr,
    { invokeBlockContext($0) }, contextAsVoidPointer)
  if result == 0 {
    return (result, threadID)
  } else {
    return (result, nil)
  }
}

#if os(Linux) || os(Android)
internal func _make_pthread_t() -> pthread_t {
  return pthread_t()
}
#else
internal func _make_pthread_t() -> pthread_t? {
  return nil
}
#endif

/// Block-based wrapper for `pthread_join`.
public func _stdlib_pthread_join<Result>(
  _ thread: pthread_t,
  _ resultType: Result.Type
) -> (CInt, Result?) {
  var threadResultRawPtr: UnsafeMutableRawPointer?
  let result = pthread_join(thread, &threadResultRawPtr)
  if result == 0 {
    let threadResultPtr = threadResultRawPtr!.assumingMemoryBound(
      to: Result.self)
    let threadResult = threadResultPtr.pointee
    threadResultPtr.deinitialize(count: 1)
    threadResultPtr.deallocate()
    return (result, threadResult)
  } else {
    return (result, nil)
  }
}

public class _stdlib_Barrier {
  var _pthreadBarrier: _stdlib_pthread_barrier_t

  var _pthreadBarrierPtr: UnsafeMutablePointer<_stdlib_pthread_barrier_t> {
    return _getUnsafePointerToStoredProperties(self)
      .assumingMemoryBound(to: _stdlib_pthread_barrier_t.self)
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
    let ret = _stdlib_pthread_barrier_destroy(_pthreadBarrierPtr)
    if ret != 0 {
      fatalError("_stdlib_pthread_barrier_destroy() failed")
    }
  }

  public func wait() {
    let ret = _stdlib_pthread_barrier_wait(_pthreadBarrierPtr)
    if !(ret == 0 || ret == _stdlib_PTHREAD_BARRIER_SERIAL_THREAD) {
      fatalError("_stdlib_pthread_barrier_wait() failed")
    }
  }
}
