// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature Extern -wmo %s -c -o %t/main.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/main.o %target-embedded-single-threaded-shim -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern

@_extern(c, "_swift_mutex_init")
func _swift_mutex_init(_: UnsafeMutableRawPointer, _: Int)

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

@main
struct Main {
  static func main() {
    withMutexStorage { mutex in
      _swift_mutex_init(mutex, 1)
      check(_swift_mutex_tryLock(mutex) != 0)
      check(_swift_mutex_tryLock(mutex) == 0)
      _swift_mutex_unlock(mutex)

      _swift_mutex_lock(mutex)
      check(_swift_mutex_tryLock(mutex) == 0)
      _swift_mutex_unlock(mutex)

      _swift_mutex_destroy(mutex)

      _swift_mutex_init(mutex, 1)
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
  }
}
