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

struct Gate {
  var open: UnsafeMutableRawPointer
  var returned: UnsafeMutableRawPointer
}

@main
struct Main {
  static func main() {
    // WasmKit allows 64 live guest threads unless --wasi-threads-max raises it.
    let cap = 256
    let gate = UnsafeMutablePointer<Gate>.allocate(capacity: 1)
    gate.initialize(to: Gate(
      open: allocateSemaphore(), returned: allocateSemaphore()))
    _swift_sem_init(gate.pointee.open, 0)
    _swift_sem_init(gate.pointee.returned, 0)
    var created = 0
    while created < cap {
      let started = _swift_thread_create({ context in
        let gate = context!.assumingMemoryBound(to: Gate.self)
        _swift_sem_wait(gate.pointee.open)
        _swift_sem_post(gate.pointee.returned)
        return nil
      }, UnsafeMutableRawPointer(gate))
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
      _swift_sem_post(gate.pointee.open)
    }
    for _ in 0..<created {
      _swift_sem_wait(gate.pointee.returned)
    }
    print("returned all")
    // CHECK: returned all
  }
}
