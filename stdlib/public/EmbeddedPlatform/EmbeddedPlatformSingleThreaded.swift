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

@implementation @c
public func _swift_mutex_init(
  _ mutex: UnsafeMutableRawPointer,
  _ checked: Int
) {
  let storage = mutex.assumingMemoryBound(to: UInt.self)
  storage.pointee = checked == 0 ? 0 : 1
  (storage + 1).pointee = 0
}

@implementation @c
public func _swift_mutex_destroy(_ mutex: UnsafeMutableRawPointer) {
  let storage = mutex.assumingMemoryBound(to: UInt.self)
  let checked = storage.pointee != 0
  let state = storage + 1
  if checked && state.pointee != 0 {
    _swift_single_threaded_trap()
  }

  storage.pointee = 0
  state.pointee = 0
}

@implementation @c
public func _swift_mutex_lock(_ mutex: UnsafeMutableRawPointer) {
  let storage = mutex.assumingMemoryBound(to: UInt.self)
  if storage.pointee != 0 {
    let state = storage + 1
    if state.pointee != 0 {
      _swift_single_threaded_trap()
    }
    state.pointee = 1
  }
}

@implementation @c
public func _swift_mutex_unlock(_ mutex: UnsafeMutableRawPointer) {
  let storage = mutex.assumingMemoryBound(to: UInt.self)
  if storage.pointee != 0 {
    let state = storage + 1
    if state.pointee == 0 {
      _swift_single_threaded_trap()
    }
    state.pointee = 0
  }
}

@implementation @c
public func _swift_mutex_tryLock(_ mutex: UnsafeMutableRawPointer) -> Int {
  let storage = mutex.assumingMemoryBound(to: UInt.self)
  if storage.pointee != 0 {
    let state = storage + 1
    if state.pointee != 0 {
      return 0
    }
    state.pointee = 1
  }
  return 1
}
