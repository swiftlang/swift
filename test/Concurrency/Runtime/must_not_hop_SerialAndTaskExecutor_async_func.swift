// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple %import-libdispatch) | %FileCheck %s --dump-input=always

// REQUIRES: concurrency
// REQUIRES: executable_test

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding
// REQUIRES: libdispatch
// REQUIRES: synchronization

import Synchronization
import Dispatch

if #available(SwiftStdlib 6.3, *) {
  await Task.detached {
    print("\n\n\n=====================================================================================")
    print("=== MyActorSerial.call() async -- sync closure")
    print("------------------------------------------------------------------------------------")
    // CHECK: === MyActorSerial.call() async -- sync closure
    // CHECK: [executor][MySerialExecutor] Enqueue (1)
    // CHECK: [MyActorSerial] inside actor; before f()
    
    // This enqueue is bad
    // CHECK-NOT: [executor][MySerialExecutor] Enqueue (2)
    
    // CHECK: [f] inside async func (isolation = Optional(main.MyActorSerial))
    // CHECK: [MyActorSerial] inside f - inside closure
    // CHECK: [MyActorSerial] inside actor; after f()
    // No more enqueues;
    // CHECK-NOT: [executor][MySerialExecutor] Enqueue
    let actorSerial = MyActorSerial()
    _ = await actorSerial.call(x: 10)
    dispatchPrecondition(condition: .notOnQueue(actorSerial.executor.queue))


    // print("\n\n\n=====================================================================================")
    // print("=== MyActorSerialAndTask.call() async -- sync closure")
    // print("------------------------------------------------------------------------------------")
    // let actorSerialAndTaskExecutor = MyActorSerialAndTask()
    // _ = await actorSerialAndTaskExecutor.call(x: 10)

    // print("\n\n\n=====================================================================================")
    // print("=====================================================================================")
    // print("=====================================================================================")

    // print("==== call() async -- sync closure")
    // print("------------------------------------------------------------------------------------")
    // _ = await call(x: 10)

    print("\n\n\n=====================================================================================")
    print("==== call() async - sync closure -- withTaskExecutor")
    print("Before withTaskExecutorPreference")
    let serialAndTaskExecutor = SerialAndTaskExecutor(name: "SerialAndTaskExecutor")
    await withTaskExecutorPreference(serialAndTaskExecutor) {
      print("Inside withTaskExecutorPreference")
      _ = await call(x: 10)
    }
    print("After withTaskExecutorPreference")
    dispatchPrecondition(condition: .notOnQueue(serialAndTaskExecutor.queue))
    // CHECK: ==== call() async - sync closure -- withTaskExecutor
    // CHECK:     Before withTaskExecutorPreference
    // CHECK:     [executor][SerialAndTaskExecutor] Enqueue (1)
    // CHECK:     Inside withTaskExecutorPreference
    // CHECK-NOT: [executor][SerialAndTaskExecutor] Enqueue
    // CHECK:     [func call() async] inside 'call' async func; before f()
    // CHECK-NOT: [executor][SerialAndTaskExecutor] Enqueue
    // CHECK:     [f] inside async func (isolation = nil)
    // CHECK:     [func call() async] --- inside 'f' - inside closure
    // CHECK-NOT: [executor][SerialAndTaskExecutor] Enqueue
    // CHECK:     [func call() async] inside 'call' async func; after f()
    // CHECK-NOT: [executor][SerialAndTaskExecutor] Enqueue
    // CHECK:     After withTaskExecutorPreference

    // print("\n\n\n=====================================================================================")
    // print("==== foo() async - sync closure -- withTaskExecutor ")
    // print("Before withTaskExecutorPreference")
    // await withTaskExecutorPreference(SerialAndTaskExecutor(name: "SerialAndTaskExecutor")) {
    //   print("Inside withTaskExecutorPreference")
    //   _ = await call(x: 10)
    // }
    // print("After withTaskExecutorPreference")

    // print("\n\n\n=====================================================================================")
    // print("==== foo() async - sync closure -- withTaskExecutor ")
    // print("Before withTaskExecutorPreference")
    // await withTaskExecutorPreference(SerialAndTaskExecutor(name: "SerialAndTaskExecutor")) {
    //   print("Inside withTaskExecutorPreference")
    //   _ = await call(x: 10)
    // }
    // print("After withTaskExecutorPreference")

    print("\n\n\n=====================================================================================")
    print("=====================================================================================")
    print("=====================================================================================")

    // print("==== defaultActor.call() async -- sync closure")
    // print("------------------------------------------------------------------------------------")
    // await MyDefaultActor().call(x: 10)

    // print("\n\n\n=====================================================================================")
    // print("==== defaultActor.call() async - sync closure -- withTaskExecutor ")
    // print("Before withTaskExecutorPreference")
    // await withTaskExecutorPreference(SerialAndTaskExecutor(name: "SerialAndTaskExecutor")) {
    //   print("Inside withTaskExecutorPreference")
    //   await MyDefaultActor().call(x: 10)
    // }
    print("After withTaskExecutorPreference")
  }.value
}

@available(SwiftStdlib 6.3, *)
func call(x: Int) async -> Int {
  print("[func call() async] inside 'call' async func; before f()")
  let x2 = await f() {
  print("[func call() async] --- inside 'f' - inside closure")
    return x + 1
  }
  print("[func call() async] inside 'call' async func; after f()")
  return x2
}



// NOTES: But that's the absolute happy case:

@available(SwiftStdlib 6.3, *)
func f(_ iso: isolated (any Actor)? = #isolation, closure: () -> Int) async -> Int {
  print("[f] inside async func (isolation = \(iso))")
  return closure()
}

@available(SwiftStdlib 6.3, *)
actor MyDefaultActor {
  func call(x: Int) async -> Int {
    print("[\(Self.self)] inside actor; before f()")
    let x2 = await f() {
      print("[\(Self.self)] inside f - inside closure")
      self.preconditionIsolated("Must be on the actor.")
      return x + 1
    }
    print("[\(Self.self)] inside actor; after f()")
    return x2
  }
}

@available(SwiftStdlib 6.3, *)
actor MyActorSerial {

  let executor = JustSerialExecutor(name: "MySerialExecutor")

  nonisolated var unownedExecutor: UnownedSerialExecutor {
    executor.asUnownedSerialExecutor()
  }

  func call(x: Int) async -> Int {
    print("[\(Self.self)] inside actor; before f()")
    let x2 = await f() {
      print("[\(Self.self)] inside f - inside closure")
      return x + 1
    }
    print("[\(Self.self)] inside actor; after f()")
    return x2
  }
}

@available(SwiftStdlib 6.3, *)
actor MyActorSerialAndTask {

  let executor: SerialAndTaskExecutor

  nonisolated var unownedExecutor: UnownedSerialExecutor {
    executor.asUnownedSerialExecutor()
  }

  init() {
    self.executor = SerialAndTaskExecutor(name: "SerialAndTaskExecutor")
  }

  func call(x: Int) async -> Int {
    print("[\(Self.self)] inside actor; before f()")
    let x2 = await f() {
      print("[\(Self.self)] inside f - inside closure")
      return x + 1
    }
    print("[\(Self.self)] inside actor; after f()")
    return x2
  }
}

@available(SwiftStdlib 6.3, *)
final class JustSerialExecutor: SerialExecutor {
  let enqueueCount: Atomic<Int>
  let queue: DispatchSerialQueue 
  let name: String

  init(name: String) {
    self.enqueueCount = .init(0)
    self.queue = DispatchSerialQueue(label: name)
    self.name = name
  }

  public func enqueue(_ job: consuming ExecutorJob) {
    let job = UnownedJob(job)
    self.queue.async {
      let newEnqueueValue = self.enqueueCount.add(1, ordering: .relaxed).newValue
      print("[executor][\(self.name)] Enqueue (\(newEnqueueValue))")
      job.runSynchronously(on: self.asUnownedSerialExecutor())
    }
  }
}

@available(SwiftStdlib 6.3, *)
final class SerialAndTaskExecutor: TaskExecutor, SerialExecutor {
  let enqueueCount: Atomic<Int>
  let queue: DispatchSerialQueue 
  let name: String

  init(name: String) {
    self.enqueueCount = .init(0)
    self.queue = DispatchSerialQueue(label: name)
    self.name = name
  }

  public func enqueue(_ job: consuming ExecutorJob) {
    let job = UnownedJob(job)
    self.queue.async {
      let newEnqueueValue = self.enqueueCount.add(1, ordering: .relaxed).newValue
      print("[executor][\(self.name)] Enqueue (\(newEnqueueValue))")
      job.runSynchronously(
        isolatedTo: self.asUnownedSerialExecutor(), 
        taskExecutor: self.asUnownedTaskExecutor())
    }
  }

}

print("done") // CHECK: done