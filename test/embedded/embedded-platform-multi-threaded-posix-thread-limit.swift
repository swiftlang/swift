// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -parse-as-library -enable-experimental-feature Embedded -wmo -import-bridging-header %S/../../stdlib/public/EmbeddedPlatform/swift/EmbeddedPlatform.h %s -c -o %t/main.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/main.o %target-embedded-multi-threaded-posix-shim %target-embedded-posix-shim -o %t/a.out -dead_strip -pthreads
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: wasi_threads
// REQUIRES: swift_feature_Embedded

func allocateSemaphore() -> UnsafeMutableRawPointer {
  UnsafeMutableRawPointer.allocate(
    byteCount: Int(EMBEDDED_SWIFT_SEMAPHORE_NUM_WORDS)
      * MemoryLayout<UnsafeRawPointer>.size,
    alignment: MemoryLayout<UnsafeRawPointer>.alignment)
}

@main
struct Main {
  static func main() {
    // WasmKit allows 64 live guest threads unless --wasi-threads-max raises it.
    let cap = 256
    let gate = allocateSemaphore()
    _swift_sem_init(gate, 0)
    let threads = UnsafeMutablePointer<__swift_thread_t?>.allocate(capacity: cap)
    var created = 0
    while created < cap {
      (threads + created).initialize(to: nil)
      let started = _swift_thread_create(threads + created, { gate in
        _swift_sem_wait(gate!)
        return nil
      }, gate)
      if started == 0 {
        break
      }
      created += 1
    }
    print(created > 0 ? "created some: yes" : "created some: no")
    // CHECK: created some: yes
    print(created < cap ? "refused before the cap: yes" : "refused before the cap: no")
    // CHECK: refused before the cap: yes

    for _ in 0..<created {
      _swift_sem_post(gate)
    }
    for i in 0..<created {
      _swift_thread_join(threads[i]!)
    }
    _swift_sem_destroy(gate)
    gate.deallocate()
    threads.deallocate()
    print("joined all")
    // CHECK: joined all
  }
}
