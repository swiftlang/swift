//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

// NOTE: this cant use Synchronization because it is deployed before that was
// introduced and availability wont let it be hidden behind an internal type.
// The Swift internal runtime locking cannot be used since that emits dependent
// symbols that are not provided by this library - so instead it has to re-implement 
// all of this on its own... 

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#elseif canImport(Musl)
import Musl
#elseif canImport(WinSDK)
import WinSDK
#elseif canImport(Bionic)
import Bionic
#elseif arch(wasm32)
#else
#error("Unsupported platform")
#endif

internal struct Lock {
  #if canImport(Darwin)
  typealias Primitive = os_unfair_lock
  #elseif canImport(Glibc) || canImport(Musl) || canImport(Bionic)
  #if os(FreeBSD) || os(OpenBSD)
  // BSD libc does not annotate the nullability of pthread APIs.
  // We should replace this with the appropriate API note in the platform
  // overlay.
  // https://github.com/swiftlang/swift/issues/81407
  typealias Primitive = pthread_mutex_t?
  #else
  typealias Primitive = pthread_mutex_t
  #endif
  #elseif canImport(WinSDK)
  typealias Primitive = SRWLOCK
  #elseif arch(wasm32)
  typealias Primitive = Int
  #else
  #error("Unsupported platform")
  #endif

  typealias PlatformLock = UnsafeMutablePointer<Primitive>
  let platformLock: PlatformLock

  private init(_ platformLock: PlatformLock) {
    self.platformLock = platformLock
  }

  fileprivate static func initialize(_ platformLock: PlatformLock) {
    #if canImport(Darwin)
    platformLock.initialize(to: os_unfair_lock())
    #elseif canImport(Glibc) || canImport(Musl) || canImport(Bionic)
    let result = pthread_mutex_init(platformLock, nil)
    precondition(result == 0, "pthread_mutex_init failed")
    #elseif canImport(WinSDK)
    InitializeSRWLock(platformLock)
    #elseif arch(wasm32)
    platformLock.initialize(to: 0)
    #else
    #error("Unsupported platform")
    #endif
  }

  fileprivate static func deinitialize(_ platformLock: PlatformLock) {
    #if canImport(Glibc) || canImport(Musl) || canImport(Bionic)
    let result = pthread_mutex_destroy(platformLock)
    precondition(result == 0, "pthread_mutex_destroy failed")
    #endif
    platformLock.deinitialize(count: 1)
  }

  fileprivate static func lock(_ platformLock: PlatformLock) {
    #if canImport(Darwin)
    os_unfair_lock_lock(platformLock)
    #elseif canImport(Glibc) || canImport(Musl) || canImport(Bionic)
    pthread_mutex_lock(platformLock)
    #elseif canImport(WinSDK)
    AcquireSRWLockExclusive(platformLock)
    #elseif arch(wasm32)
    #else
    #error("Unsupported platform")
    #endif
  }

  fileprivate static func unlock(_ platformLock: PlatformLock) {
    #if canImport(Darwin)
    os_unfair_lock_unlock(platformLock)
    #elseif canImport(Glibc) || canImport(Musl) || canImport(Bionic)
    let result = pthread_mutex_unlock(platformLock)
    precondition(result == 0, "pthread_mutex_unlock failed")
    #elseif canImport(WinSDK)
    ReleaseSRWLockExclusive(platformLock)
    #elseif arch(wasm32)
    #else
    #error("Unsupported platform")
    #endif
  }

  static func allocate() -> Lock {
    let platformLock = PlatformLock.allocate(capacity: 1)
    initialize(platformLock)
    return Lock(platformLock)
  }

  func deinitialize() {
    Lock.deinitialize(platformLock)
    platformLock.deallocate()
  }

  func lock() {
    Lock.lock(platformLock)
  }

  func unlock() {
    Lock.unlock(platformLock)
  }

  /// Acquire the lock for the duration of the given block.
  ///
  /// This convenience method should be preferred to `lock` and `unlock` in
  /// most situations, as it ensures that the lock will be released regardless
  /// of how `body` exits.
  ///
  /// - Parameter body: The block to execute while holding the lock.
  /// - Returns: The value returned by the block.
  func withLock<T>(_ body: () throws -> T) rethrows -> T {
    self.lock()
    defer {
      self.unlock()
    }
    return try body()
  }

  // specialise Void return (for performance)
  func withLockVoid(_ body: () throws -> Void) rethrows {
    try self.withLock(body)
  }
}

struct _ManagedCriticalState<State> {
  private final class LockedBuffer: ManagedBuffer<State, Lock.Primitive> {
    deinit {
      withUnsafeMutablePointerToElements { Lock.deinitialize($0) }
    }
  }

  private let buffer: ManagedBuffer<State, Lock.Primitive>

  init(_ initial: State) {
    buffer = LockedBuffer.create(minimumCapacity: 1) { buffer in
      buffer.withUnsafeMutablePointerToElements { Lock.initialize($0) }
      return initial
    }
  }

  func withCriticalRegion<R>(_ critical: (inout State) throws -> R) rethrows -> R {
    try buffer.withUnsafeMutablePointers { header, lock in
      Lock.lock(lock)
      defer { Lock.unlock(lock) }
      return try critical(&header.pointee)
    }
  }
}

extension _ManagedCriticalState: @unchecked Sendable where State: Sendable {}


@available(SwiftStdlib 5.9, *)
extension _ManagedCriticalState: Identifiable {
  internal var id: ObjectIdentifier {
    ObjectIdentifier(buffer)
  }
}
