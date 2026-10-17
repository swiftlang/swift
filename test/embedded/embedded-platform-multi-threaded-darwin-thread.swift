// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature Extern -wmo -import-bridging-header %S/../../stdlib/public/EmbeddedPlatform/swift/EmbeddedPlatform.h %s -c -o %t/main.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/main.o %target-embedded-multi-threaded-darwin-shim %target-embedded-posix-shim -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern

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

@main
struct Main {
  static func main() {
    let counted = allocateSemaphore()
    _swift_sem_init(counted, 2)
    _swift_sem_wait(counted)
    _swift_sem_wait(counted)
    _swift_sem_destroy(counted)
    counted.deallocate()
    print("initial count: ok")
    // CHECK: initial count: ok

    let handoff = UnsafeMutablePointer<Handoff>.allocate(capacity: 1)
    handoff.initialize(to: Handoff(
      go: allocateSemaphore(), done: allocateSemaphore(),
      value: 0, workerIsMain: -1))
    _swift_sem_init(handoff.pointee.go, 0)
    _swift_sem_init(handoff.pointee.done, 0)

    var thread: __swift_thread_t? = nil
    let created = _swift_thread_create(&thread, { context in
      let handoff = context!.assumingMemoryBound(to: Handoff.self)
      _swift_sem_wait(handoff.pointee.go)
      // Delay the write so that a _swift_sem_wait(done) returning early reads 0.
      _ = usleep(50_000)
      handoff.pointee.workerIsMain = _swift_thread_isMain()
      handoff.pointee.value = 42
      _swift_sem_post(handoff.pointee.done)
      return nil
    }, UnsafeMutableRawPointer(handoff))
    guard created != 0 else {
      print("thread: not created")
      return
    }
    print("thread: created")
    // CHECK: thread: created

    _swift_sem_post(handoff.pointee.go)
    _swift_sem_wait(handoff.pointee.done)
    print(handoff.pointee.value == 42 ? "value after wait: 42" : "value after wait: missing")
    // CHECK: value after wait: 42
    print(handoff.pointee.workerIsMain == 0 ? "worker is main: no" : "worker is main: yes")
    // CHECK: worker is main: no

    _swift_thread_join(thread!)
    _swift_sem_destroy(handoff.pointee.go)
    _swift_sem_destroy(handoff.pointee.done)
    handoff.pointee.go.deallocate()
    handoff.pointee.done.deallocate()
    handoff.deallocate()
    print("joined")
    // CHECK: joined

    let workerCount = 4
    let rounds = 1000
    let shared = UnsafeMutablePointer<Contended>.allocate(capacity: 1)
    shared.initialize(to: Contended(
      work: allocateSemaphore(), done: allocateSemaphore(), stop: false))
    _swift_sem_init(shared.pointee.work, 0)
    _swift_sem_init(shared.pointee.done, 0)
    let workers = UnsafeMutablePointer<Worker>.allocate(capacity: workerCount)
    let threads = UnsafeMutablePointer<__swift_thread_t?>.allocate(
      capacity: workerCount)
    for i in 0..<workerCount {
      (workers + i).initialize(to: Worker(shared: shared, tokens: 0))
      (threads + i).initialize(to: nil)
      let started = _swift_thread_create(threads + i, { context in
        let worker = context!.assumingMemoryBound(to: Worker.self)
        let shared = worker.pointee.shared
        while true {
          _swift_sem_wait(shared.pointee.work)
          if shared.pointee.stop {
            return nil
          }
          worker.pointee.tokens += 1
          _swift_sem_post(shared.pointee.done)
        }
      }, UnsafeMutableRawPointer(workers + i))
      precondition(started != 0)
    }
    for _ in 0..<rounds {
      for _ in 0..<workerCount {
        _swift_sem_post(shared.pointee.work)
      }
      for _ in 0..<workerCount {
        _swift_sem_wait(shared.pointee.done)
      }
    }
    shared.pointee.stop = true
    for _ in 0..<workerCount {
      _swift_sem_post(shared.pointee.work)
    }
    var tokens = 0
    for i in 0..<workerCount {
      _swift_thread_join(threads[i]!)
      tokens += workers[i].tokens
    }
    _swift_sem_destroy(shared.pointee.work)
    _swift_sem_destroy(shared.pointee.done)
    shared.pointee.work.deallocate()
    shared.pointee.done.deallocate()
    shared.deallocate()
    workers.deallocate()
    threads.deallocate()
    // A wait that returns without a matching post hands out an extra token.
    print("contended tokens: \(tokens)")
    // CHECK: contended tokens: 4000
  }
}
