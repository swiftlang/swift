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

fileprivate struct SingleThreadedMutex {
  var checked: Bool
  var recursive: Bool
  var lockCount: UInt
}

@implementation @c
public func _swift_mutex_init(
  _ mutex: UnsafeMutableRawPointer,
  _ flags: SwiftMutexFlags
) {
  let storage = mutex.assumingMemoryBound(to: SingleThreadedMutex.self)
  storage.pointee = SingleThreadedMutex(
    checked: flags.contains(.checked),
    recursive: flags.contains(.recursive),
    lockCount: 0)
}

@implementation @c
public func _swift_mutex_destroy(_ mutex: UnsafeMutableRawPointer) {
  let storage = mutex.assumingMemoryBound(to: SingleThreadedMutex.self)
  if storage.pointee.checked && storage.pointee.lockCount != 0 {
    fatalError("destroying a locked mutex")
  }

  storage.pointee = SingleThreadedMutex(
    checked: false,
    recursive: false,
    lockCount: 0)
}

@implementation @c
public func _swift_mutex_lock(_ mutex: UnsafeMutableRawPointer) {
  let storage = mutex.assumingMemoryBound(to: SingleThreadedMutex.self)
  if storage.pointee.checked {
    if storage.pointee.lockCount != 0 && !storage.pointee.recursive {
      fatalError("locking an already locked mutex")
    }
    storage.pointee.lockCount += 1
  }
}

@implementation @c
public func _swift_mutex_unlock(_ mutex: UnsafeMutableRawPointer) {
  let storage = mutex.assumingMemoryBound(to: SingleThreadedMutex.self)
  if storage.pointee.checked {
    if storage.pointee.lockCount == 0 {
      fatalError("unlocking an unlocked mutex")
    }
    storage.pointee.lockCount -= 1
  }
}

@implementation @c
public func _swift_mutex_tryLock(_ mutex: UnsafeMutableRawPointer) -> Int {
  let storage = mutex.assumingMemoryBound(to: SingleThreadedMutex.self)
  if storage.pointee.checked {
    if storage.pointee.lockCount != 0 && !storage.pointee.recursive {
      return 0
    }
    storage.pointee.lockCount += 1
  }
  return 1
}

fileprivate struct SingleThreadedTLS {
  static let keyCount = Int(SWIFT_TLS_KEY_COUNT)
  static var values = [8 of UnsafeMutableRawPointer?](repeating: nil)
}

@implementation @c
public func _swift_tls_init(
  _ key: Int,
  _ destructor: (@convention(c) (UnsafeMutableRawPointer?) -> Void)?
) {
}

@implementation @c
public func _swift_tls_get(_ key: Int) -> UnsafeMutableRawPointer? {
  precondition(key >= 0 && key < SingleThreadedTLS.keyCount)
  return SingleThreadedTLS.values[key]
}

@implementation @c
public func _swift_tls_set(_ key: Int, _ value: UnsafeMutableRawPointer?) {
  precondition(key >= 0 && key < SingleThreadedTLS.keyCount)
  SingleThreadedTLS.values[key] = value
}

@implementation @c
public func _swift_thread_isMain() -> Int {
  1
}
