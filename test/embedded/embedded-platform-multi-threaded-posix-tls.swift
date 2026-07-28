// RUN: %target-swift-frontend -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature Extern -wmo %s -c -o %t/main.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/main.o %target-embedded-multi-threaded-posix-shim %target-embedded-posix-shim -o %t/a.out -dead_strip -pthreads
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern

typealias pthread_t = UnsafeMutableRawPointer

@_extern(c, "pthread_create")
func pthread_create(
  _ thread: UnsafeMutablePointer<pthread_t?>,
  _ attr: UnsafeRawPointer?,
  _ start: @convention(c) (UnsafeMutableRawPointer?) -> UnsafeMutableRawPointer?,
  _ arg: UnsafeMutableRawPointer?
) -> Int

@_extern(c, "pthread_join")
func pthread_join(
  _ thread: pthread_t,
  _ result: UnsafeMutablePointer<UnsafeMutableRawPointer?>?
) -> Int

@_extern(c, "_swift_tls_init")
func _swift_tls_init(
  _ key: Int,
  _ destructor: (@convention(c) (UnsafeMutableRawPointer?) -> Void)?
)

@_extern(c, "_swift_tls_get")
func _swift_tls_get(_ key: Int) -> UnsafeMutableRawPointer?

@_extern(c, "_swift_tls_set")
func _swift_tls_set(_ key: Int, _ value: UnsafeMutableRawPointer?)

@_extern(c, "_swift_thread_isMain")
func _swift_thread_isMain() -> Int

func check(_ condition: Bool) {
  if !condition {
    fatalError("unexpected multi-threaded POSIX platform behavior")
  }
}

func embeddedPlatformWorkerIsMain() -> Bool {
  var thread: pthread_t? = nil
  guard pthread_create(&thread, nil, { _ in
    _swift_thread_isMain() != 0
      ? UnsafeMutableRawPointer(bitPattern: 1)
      : nil
  }, nil) == 0, let thread else {
    return true
  }

  var result: UnsafeMutableRawPointer?
  guard pthread_join(thread, &result) == 0 else {
    return true
  }
  return result != nil
}

@main
struct Main {
  static func main() {
    check(_swift_thread_isMain() != 0)
    check(!embeddedPlatformWorkerIsMain())

    check(_swift_tls_get(0) == nil)
    let lazyValue = UnsafeMutableRawPointer(bitPattern: 1)
    _swift_tls_set(0, lazyValue)
    check(_swift_tls_get(0) == lazyValue)
    _swift_tls_set(0, nil)

    for key in 1..<8 {
      _swift_tls_init(key, nil)
      check(_swift_tls_get(key) == nil)

      let value = UnsafeMutableRawPointer(bitPattern: key + 1)
      _swift_tls_set(key, value)
      check(_swift_tls_get(key) == value)
    }
  }
}
