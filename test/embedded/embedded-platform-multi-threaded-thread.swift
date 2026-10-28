// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature Extern -wmo %s -c -o %t/main.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/main.o %target-embedded-multi-threaded-posix-shim %target-embedded-posix-shim -o %t/posix.out -dead_strip -pthreads
// RUN: %target-run %t/posix.out | %FileCheck %s

// RUN: %if OS=macosx %{ %target-embedded-link %target-clang-resource-dir-opt %t/main.o %target-embedded-multi-threaded-darwin-shim %target-embedded-posix-shim -o %t/darwin.out -dead_strip %}
// RUN: %if OS=macosx %{ %target-run %t/darwin.out | %FileCheck %s --check-prefixes=CHECK,DARWIN %}

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu || wasi_threads
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern

import SwiftPlatform
#if canImport(Darwin)
import Darwin
#endif

@_extern(c, "usleep")
func usleep(_ microseconds: UInt32) -> Int32

struct Handoff {
  var go: UnsafeMutableRawPointer
  var done: UnsafeMutableRawPointer
  var value: Int
  var workerIsMain: Int
}

struct Contended {
  var work: UnsafeMutableRawPointer
  var done: UnsafeMutableRawPointer
  var exited: UnsafeMutableRawPointer
  var stop: Bool
}

struct Worker {
  var shared: UnsafeMutablePointer<Contended>
  var tokens: Int
}

func allocateSemaphore() -> UnsafeMutableRawPointer {
  UnsafeMutableRawPointer.allocate(
    byteCount: Int(EMBEDDED_SWIFT_SEMAPHORE_NUM_WORDS)
      * MemoryLayout<UnsafeRawPointer>.size,
    alignment: MemoryLayout<UnsafeRawPointer>.alignment)
}

#if canImport(Darwin)
func machPortNameCount() -> Int {
  var names: mach_port_name_array_t? = nil
  var nameCount: mach_msg_type_number_t = 0
  var types: mach_port_type_array_t? = nil
  var typeCount: mach_msg_type_number_t = 0
  precondition(mach_port_names(mach_task_self_, &names, &nameCount, &types,
                               &typeCount) == KERN_SUCCESS)
  precondition(vm_deallocate(
    mach_task_self_, vm_address_t(UInt(bitPattern: names)),
    vm_size_t(nameCount) * vm_size_t(MemoryLayout<mach_port_name_t>.size))
    == KERN_SUCCESS)
  precondition(vm_deallocate(
    mach_task_self_, vm_address_t(UInt(bitPattern: types)),
    vm_size_t(typeCount) * vm_size_t(MemoryLayout<mach_port_type_t>.size))
    == KERN_SUCCESS)
  return Int(nameCount)
}
#endif

@main
struct Main {
  static func main() {
    let counted = allocateSemaphore()
    _swift_semaphore_init(counted, 2)
    _swift_semaphore_wait(counted)
    _swift_semaphore_wait(counted)
    _swift_semaphore_destroy(counted)
    print("initial count: ok")
    // CHECK: initial count: ok

#if canImport(Darwin)
    // Only the Darwin shim's semaphores hold Mach ports.
    let portNames = machPortNameCount()
    for _ in 0..<100 {
      _swift_semaphore_init(counted, 0)
      _swift_semaphore_destroy(counted)
    }
    print("Mach port names added: \(machPortNameCount() - portNames)")
    // DARWIN: Mach port names added: 0
#endif
    counted.deallocate()

    let handoff = UnsafeMutablePointer<Handoff>.allocate(capacity: 1)
    handoff.initialize(to: Handoff(
      go: allocateSemaphore(), done: allocateSemaphore(),
      value: 0, workerIsMain: -1))
    _swift_semaphore_init(handoff.pointee.go, 0)
    _swift_semaphore_init(handoff.pointee.done, 0)

    let created = _swift_thread_create({ context in
      let handoff = context!.assumingMemoryBound(to: Handoff.self)
      _swift_semaphore_wait(handoff.pointee.go)
      // Delay the write so that a _swift_semaphore_wait(done) returning early
      // reads 0.
      _ = usleep(50_000)
      handoff.pointee.workerIsMain = _swift_thread_isMain()
      handoff.pointee.value = 42
      _swift_semaphore_post(handoff.pointee.done)
      return nil
    }, UnsafeMutableRawPointer(handoff))
    guard created != 0 else {
      print("thread: not created")
      return
    }
    print("thread: created")
    // CHECK: thread: created

    _swift_semaphore_post(handoff.pointee.go)
    _swift_semaphore_wait(handoff.pointee.done)
    print(handoff.pointee.value == 42 ? "value after wait: 42" : "value after wait: missing")
    // CHECK: value after wait: 42
    print(handoff.pointee.workerIsMain == 0 ? "worker is main: no" : "worker is main: yes")
    // CHECK: worker is main: no
    _swift_semaphore_destroy(handoff.pointee.go)
    _swift_semaphore_destroy(handoff.pointee.done)
    handoff.pointee.go.deallocate()
    handoff.pointee.done.deallocate()
    handoff.deallocate()

    let workerCount = 4
    let rounds = 1000
    let shared = UnsafeMutablePointer<Contended>.allocate(capacity: 1)
    shared.initialize(to: Contended(
      work: allocateSemaphore(), done: allocateSemaphore(),
      exited: allocateSemaphore(), stop: false))
    _swift_semaphore_init(shared.pointee.work, 0)
    _swift_semaphore_init(shared.pointee.done, 0)
    _swift_semaphore_init(shared.pointee.exited, 0)
    let workers = UnsafeMutablePointer<Worker>.allocate(capacity: workerCount)
    for i in 0..<workerCount {
      (workers + i).initialize(to: Worker(shared: shared, tokens: 0))
      let started = _swift_thread_create({ context in
        let worker = context!.assumingMemoryBound(to: Worker.self)
        let shared = worker.pointee.shared
        while true {
          _swift_semaphore_wait(shared.pointee.work)
          if shared.pointee.stop {
            _swift_semaphore_post(shared.pointee.exited)
            return nil
          }
          worker.pointee.tokens += 1
          _swift_semaphore_post(shared.pointee.done)
        }
      }, UnsafeMutableRawPointer(workers + i))
      precondition(started != 0)
    }
    for _ in 0..<rounds {
      for _ in 0..<workerCount {
        _swift_semaphore_post(shared.pointee.work)
      }
      for _ in 0..<workerCount {
        _swift_semaphore_wait(shared.pointee.done)
      }
    }
    shared.pointee.stop = true
    for _ in 0..<workerCount {
      _swift_semaphore_post(shared.pointee.work)
    }
    for _ in 0..<workerCount {
      _swift_semaphore_wait(shared.pointee.exited)
    }
    var tokens = 0
    for i in 0..<workerCount {
      tokens += workers[i].tokens
    }
    // A wait that returns without a matching post hands out an extra token.
    print("contended tokens: \(tokens)")
    // CHECK: contended tokens: 4000
    _swift_semaphore_destroy(shared.pointee.work)
    _swift_semaphore_destroy(shared.pointee.done)
    _swift_semaphore_destroy(shared.pointee.exited)
    shared.pointee.work.deallocate()
    shared.pointee.done.deallocate()
    shared.pointee.exited.deallocate()
    shared.deallocate()
    workers.deallocate()

    // Returned joinable threads exhaust WasmKit's 1 GiB of memory after about
    // 16300.
    let returned = allocateSemaphore()
    _swift_semaphore_init(returned, 0)
    var returnedThreads = 0
    while returnedThreads < 20_000 {
      let spawned = _swift_thread_create({ context in
        _swift_semaphore_post(context!)
        return nil
      }, returned)
      if spawned == 0 {
        break
      }
      _swift_semaphore_wait(returned)
      returnedThreads += 1
    }
    print("returned threads: \(returnedThreads)")
    // CHECK: returned threads: 20000
    _swift_semaphore_destroy(returned)
    returned.deallocate()
  }
}
