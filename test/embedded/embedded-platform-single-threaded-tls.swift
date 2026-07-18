// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature Extern -wmo %s -c -o %t/main.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/main.o %target-embedded-single-threaded-shim %target-embedded-posix-shim -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
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

func check(_ condition: Bool) {
  if !condition {
    fatalError("unexpected single-threaded TLS behavior")
  }
}

@main
struct Main {
  static func main() {
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

      _swift_tls_set(key, nil)
      check(_swift_tls_get(key) == nil)
    }
  }
}
