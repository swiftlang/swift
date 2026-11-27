// RUN: %empty-directory(%t)
// RUN: %target-clang -x c %S/Inputs/tsan_invisible_barrier.c -c -o %t/tsan_invisible_barrier.o
// RUN: %target-swiftc_driver %s %t/tsan_invisible_barrier.o -import-objc-header %S/Inputs/tsan_invisible_barrier.h -Onone -parse-as-library -g -sanitize=thread %import-libdispatch -o %t/test-Onone
// RUN: %target-codesign %t/test-Onone
// RUN: env %env-TSAN_OPTIONS=abort_on_error=0 %target-run %t/test-Onone 2>&1 | %FileCheck %s --dump-input=fail --implicit-check-not='ThreadSanitizer'
// RUN: %target-swiftc_driver %s %t/tsan_invisible_barrier.o -import-objc-header %S/Inputs/tsan_invisible_barrier.h -O -parse-as-library -g -sanitize=thread %import-libdispatch -o %t/test-O
// RUN: %target-codesign %t/test-O
// RUN: env %env-TSAN_OPTIONS=abort_on_error=0 %target-run %t/test-O 2>&1 | %FileCheck %s --dump-input=fail --implicit-check-not='ThreadSanitizer'
// REQUIRES: executable_test
// REQUIRES: tsan_runtime
// REQUIRES: libdispatch
// REQUIRES: synchronization

// Not verified on these platforms
// UNSUPPORTED: OS=ios
// UNSUPPORTED: OS=freebsd

// tsan_runtime doesn't say whether a remote device's OS supports TSan.
// UNSUPPORTED: remote_run

// The Synchronization runtime on Linux must annotate slow-path futex lock
// acquisitions for TSan; an older OS/back-deployed runtime does not.
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// XFAIL: OS=linux-{{.*}}

// Verify that ThreadSanitizer correctly observes happens-before edges across
// Synchronization.Mutex.withLock

import Dispatch
import Synchronization
#if canImport(Darwin)
import Darwin
#elseif canImport(Android)
import Android
#elseif canImport(Glibc)
import Glibc
#elseif canImport(Musl)
import Musl
#endif

let parkedMutex = Mutex(0)
let stressMutex = Mutex(0)

@main struct Test {
  static func main() {
    // 1. Force a contender onto the slow path:
    //    Thread A holds `parkedMutex` across an invisible barrier and sleeps
    //    long enough for Thread B to exhaust the spin loop and park.
    //    Thread A writes to the protected value and unlocks - if the
    //    happens-before edge is missed, we will get a false positive race 
    //    report on `value`.
    swift_tsan_invisible_barrier_init(2)
    let done = DispatchSemaphore(value: 0)

    DispatchQueue.global().async {
      parkedMutex.withLock { value in
        swift_tsan_invisible_barrier_wait()
        usleep(20_000)
        value = 42
      }
      done.signal()
    }

    swift_tsan_invisible_barrier_wait()
    parkedMutex.withLock { value in
      value += 1
    }
    done.wait()
    print(parkedMutex.withLock { $0 })
    // CHECK: 43

    // 2. High-contention concurrent increments exercising fast-path and spin
    let iterations = 1_000
    DispatchQueue.global().async {
      swift_tsan_invisible_barrier_wait()
      for _ in 0..<iterations {
        stressMutex.withLock { $0 += 1 }
      }
      done.signal()
    }
    swift_tsan_invisible_barrier_wait()
    for _ in 0..<iterations {
      stressMutex.withLock { $0 += 1 }
    }
    done.wait()
    print(stressMutex.withLock { $0 })
    // CHECK: 2000

    print("Done.")
  }
}

// CHECK: Done.
