//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// Only support 64bit
#if !(os(iOS) && (arch(i386) || arch(arm)))

import Darwin

@available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
extension UnsafeMutablePointer where Pointee == os_unfair_lock_s {
    internal init() {
        let l = UnsafeMutablePointer.allocate(capacity: 1)
        l.initialize(to: os_unfair_lock())
        self = l
    }
    
    internal func cleanupLock() {
        deinitialize(count: 1)
        deallocate()
    }
    
    internal func lock() {
        os_unfair_lock_lock(self)
    }
    
    internal func tryLock() -> Bool {
        let result = os_unfair_lock_trylock(self)
        return result
    }
    
    internal func unlock() {
        os_unfair_lock_unlock(self)
    }
}

@available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
typealias Lock = os_unfair_lock_t

#if canImport(DarwinPrivate)

@_implementationOnly import DarwinPrivate

@available(macOS 10.14, iOS 12.0, tvOS 12.0, watchOS 5.0, *)
extension UnsafeMutablePointer where Pointee == os_unfair_recursive_lock_s {
    internal init() {
        let l = UnsafeMutablePointer.allocate(capacity: 1)
        l.initialize(to: os_unfair_recursive_lock_s())
        self = l
    }
    
    internal func cleanupLock() {
        deinitialize(count: 1)
        deallocate()
    }
    
    internal func lock() {
        os_unfair_recursive_lock_lock(self)
    }
    
    internal func tryLock() -> Bool {
        let result = os_unfair_recursive_lock_trylock(self)
        return result
    }

    internal func unlock() {
        os_unfair_recursive_lock_unlock(self)
    }
}

@available(macOS 10.14, iOS 12.0, tvOS 12.0, watchOS 5.0, *)
typealias RecursiveLock = os_unfair_recursive_lock_t

#else

// Kept in overlay since some builds may not have `DarwinPrivate` but we should have the availability the same
@available(macOS 10.14, iOS 12.0, tvOS 12.0, watchOS 5.0, *)
internal struct RecursiveLock {
    private let lockPtr: UnsafeMutablePointer<pthread_mutex_t>
    
    internal init() {
        lockPtr = UnsafeMutablePointer<pthread_mutex_t>.allocate(capacity: 1)
        var attr = pthread_mutexattr_t()
        pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE)
        pthread_mutex_init(lockPtr, &attr)
    }
    
    internal func cleanupLock() {
        pthread_mutex_destroy(lockPtr)
        lockPtr.deinitialize(count: 1)
        lockPtr.deallocate()
    }
    
    internal func lock() {
        pthread_mutex_lock(lockPtr)
    }
    
    internal func tryLock() -> Bool {
        return pthread_mutex_trylock(lockPtr) == 0
    }

    internal func unlock() {
        pthread_mutex_unlock(lockPtr)
    }
}

#endif

#endif /* !(os(iOS) && (arch(i386) || arch(arm))) */
