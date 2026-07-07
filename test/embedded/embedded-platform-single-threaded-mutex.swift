// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature Extern -wmo %s -c -o %t/main.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/main.o %target-embedded-single-threaded-shim %target-embedded-posix-shim -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out

// RUN: %target-swift-frontend -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature Extern -D LOCK_LOCKED -module-name lock_locked -wmo %s -c -o %t/lock-locked.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/lock-locked.o %target-embedded-single-threaded-shim %target-embedded-posix-shim -o %t/lock-locked.out -dead_strip
// RUN: %target-not-crash %target-run %t/lock-locked.out

// RUN: %target-swift-frontend -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature Extern -D UNLOCK_UNLOCKED -module-name unlock_unlocked -wmo %s -c -o %t/unlock-unlocked.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/unlock-unlocked.o %target-embedded-single-threaded-shim %target-embedded-posix-shim -o %t/unlock-unlocked.out -dead_strip
// RUN: %target-not-crash %target-run %t/unlock-unlocked.out

// RUN: %target-swift-frontend -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature Extern -D DESTROY_LOCKED -module-name destroy_locked -wmo %s -c -o %t/destroy-locked.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/destroy-locked.o %target-embedded-single-threaded-shim %target-embedded-posix-shim -o %t/destroy-locked.out -dead_strip
// RUN: %target-not-crash %target-run %t/destroy-locked.out

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern

@_extern(c, "_swift_mutex_init")
func _swift_mutex_init(_: UnsafeMutableRawPointer, _: CUnsignedLongLong)

@_extern(c, "_swift_mutex_destroy")
func _swift_mutex_destroy(_: UnsafeMutableRawPointer)

@_extern(c, "_swift_mutex_lock")
func _swift_mutex_lock(_: UnsafeMutableRawPointer)

@_extern(c, "_swift_mutex_unlock")
func _swift_mutex_unlock(_: UnsafeMutableRawPointer)

@_extern(c, "_swift_mutex_tryLock")
func _swift_mutex_tryLock(_: UnsafeMutableRawPointer) -> Int

func check(_ condition: Bool) {
  if !condition {
    fatalError("unexpected single-threaded mutex behavior")
  }
}

func withMutexStorage(_ body: (UnsafeMutableRawPointer) -> Void) {
  var storage: [6 of UInt] = [0, 0, 0, 0, 0, 0]
  withUnsafeMutablePointer(to: &storage) {
    body(UnsafeMutableRawPointer($0))
  }
}

let checked: CUnsignedLongLong = 0x01
let recursive: CUnsignedLongLong = 0x02

@main
struct Main {
  static func main() {
#if LOCK_LOCKED
    withMutexStorage { mutex in
      _swift_mutex_init(mutex, checked)
      _swift_mutex_lock(mutex)
      _swift_mutex_lock(mutex)
    }
#elseif UNLOCK_UNLOCKED
    withMutexStorage { mutex in
      _swift_mutex_init(mutex, checked)
      _swift_mutex_unlock(mutex)
    }
#elseif DESTROY_LOCKED
    withMutexStorage { mutex in
      _swift_mutex_init(mutex, checked)
      _swift_mutex_lock(mutex)
      _swift_mutex_destroy(mutex)
    }
#else
    withMutexStorage { mutex in
      _swift_mutex_init(mutex, checked)
      check(_swift_mutex_tryLock(mutex) != 0)
      check(_swift_mutex_tryLock(mutex) == 0)
      _swift_mutex_unlock(mutex)

      _swift_mutex_lock(mutex)
      check(_swift_mutex_tryLock(mutex) == 0)
      _swift_mutex_unlock(mutex)

      _swift_mutex_destroy(mutex)

      _swift_mutex_init(mutex, checked)
      check(_swift_mutex_tryLock(mutex) != 0)
      _swift_mutex_unlock(mutex)
      _swift_mutex_destroy(mutex)
    }

    withMutexStorage { mutex in
      _swift_mutex_init(mutex, 0)
      _swift_mutex_lock(mutex)
      check(_swift_mutex_tryLock(mutex) != 0)
      _swift_mutex_unlock(mutex)
      _swift_mutex_unlock(mutex)
      _swift_mutex_destroy(mutex)
    }

    withMutexStorage { mutex in
      _swift_mutex_init(mutex, checked | recursive)
      _swift_mutex_lock(mutex)
      _swift_mutex_lock(mutex)
      check(_swift_mutex_tryLock(mutex) != 0)
      _swift_mutex_unlock(mutex)
      _swift_mutex_unlock(mutex)
      _swift_mutex_unlock(mutex)
      _swift_mutex_destroy(mutex)
    }
#endif
  }
}
