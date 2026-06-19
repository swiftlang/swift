//===----------- EmbeddedPlatformSingleThreaded.swift ---------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_extern(c, "_swift_exit")
func _swift_exit(_: Int)

func _swift_single_threaded_trap() -> Never {
  _swift_exit(1)
  while true {}
}

fileprivate struct SingleThreadedMutex {
  var checked: Bool
  var locked: Bool
}

@implementation @c
public func _swift_mutex_init(
  _ mutex: UnsafeMutableRawPointer,
  _ checked: Int
) {
  let storage = mutex.assumingMemoryBound(to: SingleThreadedMutex.self)
  storage.pointee = SingleThreadedMutex(
    checked: checked != 0,
    locked: false)
}

@implementation @c
public func _swift_mutex_destroy(_ mutex: UnsafeMutableRawPointer) {
  let storage = mutex.assumingMemoryBound(to: SingleThreadedMutex.self)
  if storage.pointee.checked && storage.pointee.locked {
    _swift_single_threaded_trap()
  }

  storage.pointee = SingleThreadedMutex(checked: false, locked: false)
}

@implementation @c
public func _swift_mutex_lock(_ mutex: UnsafeMutableRawPointer) {
  let storage = mutex.assumingMemoryBound(to: SingleThreadedMutex.self)
  if storage.pointee.checked {
    if storage.pointee.locked {
      _swift_single_threaded_trap()
    }
    storage.pointee.locked = true
  }
}

@implementation @c
public func _swift_mutex_unlock(_ mutex: UnsafeMutableRawPointer) {
  let storage = mutex.assumingMemoryBound(to: SingleThreadedMutex.self)
  if storage.pointee.checked {
    if !storage.pointee.locked {
      _swift_single_threaded_trap()
    }
    storage.pointee.locked = false
  }
}

@implementation @c
public func _swift_mutex_tryLock(_ mutex: UnsafeMutableRawPointer) -> Int {
  let storage = mutex.assumingMemoryBound(to: SingleThreadedMutex.self)
  if storage.pointee.checked {
    if storage.pointee.locked {
      return 0
    }
    storage.pointee.locked = true
  }
  return 1
}
