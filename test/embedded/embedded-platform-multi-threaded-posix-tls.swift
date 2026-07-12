// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature Extern -wmo %s -c -o %t/main.o
// RUN: %target-clang -x c -std=c11 -I %swift_obj_root/include -c %S/Inputs/embedded-platform-threading.c -o %t/threading.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/main.o %t/threading.o %target-embedded-multi-threaded-posix-shim %target-embedded-posix-shim -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern

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

@_extern(c, "embedded_platform_worker_is_main")
func embedded_platform_worker_is_main() -> Int

func check(_ condition: Bool) {
  if !condition {
    fatalError("unexpected multi-threaded POSIX platform behavior")
  }
}

@main
struct Main {
  static func main() {
    check(_swift_thread_isMain() != 0)
    check(embedded_platform_worker_is_main() == 0)

    for key in 0..<8 {
      _swift_tls_init(key, nil)
      check(_swift_tls_get(key) == nil)

      let value = UnsafeMutableRawPointer(bitPattern: key + 1)
      _swift_tls_set(key, value)
      check(_swift_tls_get(key) == value)
    }
  }
}
