// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library -enable-experimental-feature Embedded -disable-availability-checking -wmo %s -c -o %t/main.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/main.o %target-embedded-posix-shim -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s --implicit-check-not=unexpected

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: synchronization
// REQUIRES: swift_feature_Embedded
// UNSUPPORTED: OS=wasip1

import Synchronization

@inline(__always)
func mutexWord(
  _ mutex: UnsafeMutableRawPointer
) -> UnsafeMutablePointer<UInt> {
  mutex.assumingMemoryBound(to: UInt.self)
}

@_cdecl("_swift_mutex_init")
public func test_swift_mutex_init(
  _ mutex: UnsafeMutableRawPointer,
  _ flags: CUnsignedLongLong
) {
  print("mutex.init")
  if flags != 0 {
    print("unexpected mutex flags")
  }
  mutexWord(mutex).pointee = 0x51
}

@_cdecl("_swift_mutex_destroy")
public func test_swift_mutex_destroy(
  _ mutex: UnsafeMutableRawPointer
) {
  print("mutex.destroy")
  if mutexWord(mutex).pointee != 0x51 {
    print("unexpected destroy handle")
  }
  mutexWord(mutex).pointee = 0
}

@_cdecl("_swift_mutex_lock")
public func test_swift_mutex_lock(
  _ mutex: UnsafeMutableRawPointer
) {
  print("mutex.lock")
  if mutexWord(mutex).pointee != 0x51 {
    print("unexpected lock handle")
  }
}

@_cdecl("_swift_mutex_unlock")
public func test_swift_mutex_unlock(
  _ mutex: UnsafeMutableRawPointer
) {
  print("mutex.unlock")
  if mutexWord(mutex).pointee != 0x51 {
    print("unexpected unlock handle")
  }
}

@_cdecl("_swift_mutex_tryLock")
public func test_swift_mutex_tryLock(
  _ mutex: UnsafeMutableRawPointer
) -> Int {
  print("mutex.tryLock")
  if mutexWord(mutex).pointee != 0x51 {
    print("unexpected tryLock handle")
  }
  return 1
}

@main
struct Main {
  static func main() {
    do {
      let mutex = Mutex(0)
      // CHECK: mutex.init

      mutex.withLock {
        $0 += 1
      }
      // CHECK: mutex.lock
      // CHECK: mutex.unlock

      let value = mutex.withLockIfAvailable {
        print("mutex.body")
        $0 += 1
        return $0
      }
      // CHECK: mutex.tryLock
      // CHECK: mutex.body
      // CHECK: mutex.unlock

      print(value!)
      // CHECK: 2
    }
    // CHECK: mutex.destroy

    print("done")
    // CHECK: done
  }
}
