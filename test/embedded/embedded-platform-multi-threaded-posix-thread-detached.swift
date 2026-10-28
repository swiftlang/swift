// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -parse-as-library -enable-experimental-feature Embedded -wmo -import-bridging-header %S/../../stdlib/public/EmbeddedPlatform/swift/EmbeddedPlatform.h %s -c -o %t/main.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/main.o %target-embedded-multi-threaded-posix-shim %target-embedded-posix-shim -o %t/a.out -dead_strip -pthreads
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: wasi_threads
// REQUIRES: swift_feature_Embedded

@main
struct Main {
  static func main() {
    let returned = UnsafeMutableRawPointer.allocate(
      byteCount: Int(EMBEDDED_SWIFT_SEMAPHORE_NUM_WORDS)
        * MemoryLayout<UnsafeRawPointer>.size,
      alignment: MemoryLayout<UnsafeRawPointer>.alignment)
    _swift_sem_init(returned, 0)
    // A returned thread that is not detached keeps its stack, and the test's
    // 1 GiB memory holds about 16000 of them.
    let count = 20_000
    var created = 0
    while created < count {
      let started = _swift_thread_create({ returned in
        _swift_sem_post(returned!)
        return nil
      }, returned)
      if started == 0 {
        break
      }
      _swift_sem_wait(returned)
      created += 1
    }
    print("created: \(created)")
    // CHECK: created: 20000
  }
}
