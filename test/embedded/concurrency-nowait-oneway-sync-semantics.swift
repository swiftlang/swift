// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -enable-experimental-feature OnewayNowait -disable-experimental-parser-round-trip -parse-as-library -swift-version 6 %s -c -o %t/a.o -plugin-path %swift-plugin-dir
// RUN: %target-embedded-link %t/a.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-clang-resource-dir-opt -lc++abi -lswift_Concurrency %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s
// RUN: %target-swift-frontend -O -enable-experimental-feature Embedded -enable-experimental-feature OnewayNowait -disable-experimental-parser-round-trip -parse-as-library -swift-version 6 %s -c -o %t/aopt.o -plugin-path %swift-plugin-dir
// RUN: %target-embedded-link %t/aopt.o -o %t/aopt.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-clang-resource-dir-opt -lc++abi -lswift_Concurrency %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/aopt.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: concurrency
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_OnewayNowait
// REQUIRES: concurrency_runtime

// The runtime semantics of 'nowait' on a synchronous 'oneway' method of a
// plain actor or of a global actor in Embedded Swift, which is lowered to a
// synchronous enqueue:
// - the receiver and the arguments (including default arguments and variadic
//   arguments) are evaluated at the call site, in order, exactly once
// - the calls run in FIFO order with other work enqueued on the actor, and a
//   call from the actor's own isolation runs after the current job
// - the same for a call through a protocol requirement
// - the actor is kept alive until the call ran
// - the call runs in a new task, isolated to the actor, which copies the
//   caller's task locals, inherits its priority and is not cancelled with it

import _Concurrency

// Polls until 'done' returns true, and fails instead of hanging forever if the
// fire-and-forget calls never run
func waitUntil(_ done: () async throws -> Bool) async rethrows {
  for _ in 0 ..< 1_000_000 {
    if try await done() {
      return
    }
    await Task.yield()
  }
  fatalError("timed out waiting for the 'nowait' calls to run")
}

enum Probe {
  @TaskLocal static var tag: Int = 0
  @TaskLocal static var inner: Int = 0
}

func log(_ s: String) { print("[swift] \(s)") }

func arg(_ n: Int) -> Int { log("arg \(n)"); return n }
func defaultValue() -> Int { log("default"); return 99 }
func auto() -> Int { log("autoclosure"); return 5 }

nonisolated(unsafe) var deinits = 0

final class Tracker {
  let name: String
  init(_ name: String) { self.name = name }
  deinit {
    log("deinit \(name)")
    deinits += 1
  }
}

protocol Tellable: Actor {
  func tell(_ a: Int, _ b: Int) oneway
}

actor Mailbox: Tellable {
  let tracker: Tracker?
  var count = 0
  init(_ tracker: Tracker? = nil) { self.tracker = tracker }

  func tell(_ a: Int, _ b: Int) oneway {
    count += 1
    log("tell \(a) \(b)")
  }
  func tellDefault(_ a: Int, d: Int = defaultValue()) oneway {
    count += 1
    log("tellDefault \(a) \(d)")
  }
  func tellVariadic(_ xs: Int...) oneway {
    count += 1
    log("tellVariadic \(xs.count) \(xs[0]) \(xs[1])")
  }
  func tellAuto(_ x: @autoclosure () -> Int) oneway {
    count += 1
    log("tellAuto before")
    log("tellAuto \(x())")
  }
  func tellAsync(_ n: Int) async oneway {
    count += 1
    log("tellAsync \(n)")
  }
  func probe() oneway {
    count += 1
    log("probe tag \(Probe.tag) inner \(Probe.inner)")
    log("probe cancelled \(Task.isCancelled)")
    withUnsafeCurrentTask { t in log("probe has task \(t != nil)") }
    log("probe priority \(Task.currentPriority.rawValue)")
  }

  func g() { log("g") }
  func getCount() -> Int { count }

  func selfSend() {
    nowait self.tell(1, 1)
    nowait tell(2, 2)
    log("selfSend end")
  }
}

// Wait until 'm' ran 'n' calls in total
func waitFor(_ m: Mailbox, _ n: Int) async {
  await waitUntil { await m.getCount() >= n }
}

@globalActor actor Background {
  static let shared = Background()
}

@Background var backgroundCount = 0

@Background func note(_ n: Int) oneway {
  backgroundCount += 1
  log("note \(n)")
}

@Background func backgroundDefault() -> Int {
  log("backgroundDefault")
  return 7
}

// The default argument is isolated to 'Background' (SE-0411), so it is
// evaluated in the enqueued call, just as it is only evaluated after the hop
// for an 'await'
@Background func noteIsolatedDefault(_ n: Int = backgroundDefault()) oneway {
  backgroundCount += 1
  log("noteIsolatedDefault \(n)")
}

@Background func backgroundSelfSend() {
  nowait note(100)
  log("backgroundSelfSend end")
}

@Background func getBackgroundCount() -> Int { backgroundCount }

func waitForBackground(_ n: Int) async {
  await waitUntil { await getBackgroundCount() >= n }
}

struct Receiver {
  @Background func note(_ n: Int) oneway {
    backgroundCount += 1
    log("Receiver.note \(n)")
  }
}

func makeMailbox(_ m: Mailbox) -> Mailbox { log("receiver"); return m }
func makeReceiver() -> Receiver { log("receiver"); return Receiver() }

func runAutoclosure(_ body: @autoclosure () -> Void) { body() }

// 'nowait' from a 'defer', a synchronous closure and an autoclosure
func sendFromContexts(_ m: Mailbox) {
  defer { nowait m.tell(30, 30) }
  let closure = { nowait m.tell(31, 31) }
  closure()
  runAutoclosure(nowait m.tell(32, 32))
  log("sendFromContexts end")
}

// Through a protocol requirement
func sendGeneric<T: Tellable>(_ t: T, _ n: Int) { nowait t.tell(n, n) }
func sendOpaque(_ t: some Tellable, _ n: Int) { nowait t.tell(n, n) }

// The async-bodied 'oneway' method keeps the task-based lowering
func sendAsync(_ m: Mailbox) {
  nowait m.tellAsync(40)
}

@main struct Main {
  static func main() async {
    let m = Mailbox()

    print("[swift] test_evaluation")
    nowait makeMailbox(m).tell(arg(1), arg(2))
    log("after nowait")
    await m.g()
    // CHECK-LABEL: [swift] test_evaluation
    // CHECK-NEXT: [swift] receiver
    // CHECK-NEXT: [swift] arg 1
    // CHECK-NEXT: [swift] arg 2
    // CHECK-NEXT: [swift] after nowait
    // CHECK-NEXT: [swift] tell 1 2
    // CHECK-NEXT: [swift] g

    print("[swift] test_arguments")
    nowait m.tellDefault(arg(3))
    nowait m.tellVariadic(arg(4), arg(5))
    nowait m.tellAuto(auto())
    log("after nowait")
    await m.g()
    // CHECK-LABEL: [swift] test_arguments
    // CHECK-NEXT: [swift] arg 3
    // CHECK-NEXT: [swift] default
    // CHECK-NEXT: [swift] arg 4
    // CHECK-NEXT: [swift] arg 5
    // CHECK-NEXT: [swift] after nowait
    // CHECK-NEXT: [swift] tellDefault 3 99
    // CHECK-NEXT: [swift] tellVariadic 2 4 5
    // The autoclosure is evaluated by the callee, as for an 'await'
    // CHECK-NEXT: [swift] tellAuto before
    // CHECK-NEXT: [swift] autoclosure
    // CHECK-NEXT: [swift] tellAuto 5
    // CHECK-NEXT: [swift] g

    print("[swift] test_fifo")
    for i in 10 ..< 14 {
      nowait m.tell(i, i)
    }
    await m.g()
    // CHECK-LABEL: [swift] test_fifo
    // CHECK-NEXT: [swift] tell 10 10
    // CHECK-NEXT: [swift] tell 11 11
    // CHECK-NEXT: [swift] tell 12 12
    // CHECK-NEXT: [swift] tell 13 13
    // CHECK-NEXT: [swift] g

    print("[swift] test_self")
    var expected = await m.getCount() + 2
    await m.selfSend()
    await waitFor(m, expected)
    // Enqueued, so they only run after the current method returned
    // CHECK-LABEL: [swift] test_self
    // CHECK-NEXT: [swift] selfSend end
    // CHECK-NEXT: [swift] tell 1 1
    // CHECK-NEXT: [swift] tell 2 2

    print("[swift] test_contexts")
    expected = await m.getCount() + 4
    sendFromContexts(m)
    sendAsync(m)
    await waitFor(m, expected)
    // CHECK-LABEL: [swift] test_contexts
    // CHECK-NEXT: [swift] sendFromContexts end
    // CHECK-DAG: [swift] tell 30 30
    // CHECK-DAG: [swift] tell 31 31
    // CHECK-DAG: [swift] tell 32 32
    // CHECK-DAG: [swift] tellAsync 40

    print("[swift] test_protocols")
    sendGeneric(m, 41)
    sendOpaque(m, 42)
    await m.g()
    // CHECK-LABEL: [swift] test_protocols
    // CHECK-NEXT: [swift] tell 41 41
    // CHECK-NEXT: [swift] tell 42 42
    // CHECK-NEXT: [swift] g

    print("[swift] test_lifetime")
    do {
      let t = Mailbox(Tracker("t"))
      nowait t.tell(50, 50)
    }
    log("scope exited")
    await waitUntil { deinits >= 1 }
    // The enqueued call keeps the actor alive, and it is released after
    // the call ran
    // CHECK-LABEL: [swift] test_lifetime
    // CHECK-NEXT: [swift] scope exited
    // CHECK-NEXT: [swift] tell 50 50
    // CHECK-NEXT: [swift] deinit t

    print("[swift] test_task")
    expected = await m.getCount() + 1
    await Probe.$tag.withValue(1) {
      await Probe.$inner.withValue(2) {
        await Probe.$tag.withValue(3) {
          let caller = Task(priority: .high) {
            withUnsafeCurrentTask { $0?.cancel() }
            log("caller cancelled \(Task.isCancelled)")
            log("caller priority \(Task.currentPriority.rawValue)")
            nowait m.probe()
          }
          await caller.value
        }
      }
    }
    await waitFor(m, expected)
    // CHECK-LABEL: [swift] test_task
    // CHECK-NEXT: [swift] caller cancelled true
    // CHECK-NEXT: [swift] caller priority 25
    // The innermost binding of each task local
    // CHECK-NEXT: [swift] probe tag 3 inner 2
    // CHECK-NEXT: [swift] probe cancelled false
    // CHECK-NEXT: [swift] probe has task true
    // CHECK-NEXT: [swift] probe priority 25

    print("[swift] test_global_actor")
    nowait makeReceiver().note(arg(60))
    nowait note(arg(61))
    nowait noteIsolatedDefault()
    log("after nowait")
    await waitForBackground(3)
    // CHECK-LABEL: [swift] test_global_actor
    // CHECK-NEXT: [swift] receiver
    // CHECK-NEXT: [swift] arg 60
    // CHECK-NEXT: [swift] arg 61
    // CHECK-NEXT: [swift] after nowait
    // CHECK-NEXT: [swift] Receiver.note 60
    // CHECK-NEXT: [swift] note 61
    // CHECK-NEXT: [swift] backgroundDefault
    // CHECK-NEXT: [swift] noteIsolatedDefault 7

    print("[swift] test_global_actor_self")
    await backgroundSelfSend()
    await waitForBackground(4)
    // CHECK-LABEL: [swift] test_global_actor_self
    // CHECK-NEXT: [swift] backgroundSelfSend end
    // CHECK-NEXT: [swift] note 100

    print("[swift] done")
    // CHECK-LABEL: [swift] done
  }
}
