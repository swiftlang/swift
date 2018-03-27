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
// This file contains wrappers for thread APIs that are less painful to use.
//
//===----------------------------------------------------------------------===//

#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
import Darwin
#elseif os(Linux) || os(FreeBSD) || os(PS4) || os(Android) || os(Cygwin) || os(Haiku)
import Glibc
#elseif os(Windows)
import MSVCRT
#endif

#if os(Windows)
public typealias _stdlib_thread_t = UnsafeRawPointer
#else
public typealias _stdlib_thread_t = pthread_t
#endif

/// An abstract base class to encapsulate the context necessary to invoke
/// a block from thread_create.
internal class ThreadBlockContextType {
  /// Execute the block, and return an `UnsafeMutablePointer` to memory
  /// allocated with `UnsafeMutablePointer.alloc` containing the result of the
  /// block.
  func run() -> UnsafeMutableRawPointer { fatalError("abstract") }
}

internal class ThreadBlockContext<Argument, Result> : ThreadBlockContextType {
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

internal func invokeBlockContext(_ context: UnsafeMutableRawPointer?)
    -> UnsafeMutableRawPointer! {
  // The context is passed in +1; we're responsible for releasing it.
  let context =
      Unmanaged<ThreadBlockContextType>.fromOpaque(context!).takeRetainedValue()
  return context.run()
}

public func _stdlib_thread_create_block<Argument, Result>(
    _ routine: @escaping (Argument) -> Result,
    _ arg: Argument) -> (CInt, _stdlib_thread_t?) {
  let context = ThreadBlockContext(block: routine, arg: arg)
  let contextPointer = Unmanaged.passRetained(context).toOpaque()

#if os(Windows)
  let result = 1
#else
#if os(Linux) || os(Android)
  var thread : pthread_t = pthread_t()
#else
  var thread : pthread_t? = nil
#endif
  let result = pthread_create(&thread, nil, { invokeBlockContext($0) },
                              contextPointer)
#endif
  if (result == 0) {
    return (result, thread)
  }
  return (result, nil)
}

public func _stdlib_thread_join<Result>(_ thread: _stdlib_thread_t,
                                        _ resultType: Result.Type)
    -> (CInt, Result?) {
  var threadResultRawPtr: UnsafeMutableRawPointer?

#if os(Windows)
  let result = 1
#else
  let result = pthread_join(thread, &threadResultRawPtr)
#endif
  if (result != 0) {
    return (result, nil)
  }

  let threadResultPtr = threadResultRawPtr!.assumingMemoryBound(to: Result.self)
  let threadResult = threadResultPtr.pointee
  threadResultPtr.deinitialize(count: 1)
  threadResultPtr.deallocate()
  return (result, threadResult)
}

