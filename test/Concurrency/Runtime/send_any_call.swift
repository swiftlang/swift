// RUN: %target-run-simple-swift(-parse-as-library -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls) | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: concurrency_runtime
// REQUIRES: swift_feature_SendCalls

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: use_os_stdlib

// 'send' can be applied to any Void, non-throwing call of an actor isolated
// function. For a synchronous or 'async' actor method which is not
// '@oneway' the call is enqueued on the actor in program order, like
// for a '@oneway' one. A global actor isolated function runs on its
// global actor, and a function with an 'isolated' parameter on that actor

actor Mailbox {
  var received: [String] = []
  func tell(_ n: Int) {
    received.append("tell \(n)")
  }
  func tellAsync(_ n: Int) async {
    received.append("tellAsync \(n)")
  }
  nonisolated func log(_ line: String) {
    print("[swift] Mailbox.log(\(line))")
    send record(line)
  }
  func record(_ line: String) {
    received.append(line)
  }
  func drain() -> [String] { received }
}

actor Collector {
  var lines: [String] = []
  func add(_ line: String) { lines.append(line) }
  func all() -> [String] { lines }
}

let collector = Collector()

@MainActor func mainActorFunction(_ n: Int) {
  print("[swift] mainActorFunction(\(n))")
  send collector.add("mainActorFunction \(n)")
}

func isolatedParameter(_ c: isolated Collector, _ line: String) {
  print("[swift] isolatedParameter(\(line))")
  c.add(line)
}

// Polls until 'done' returns true, and fails instead of hanging forever if the
// fire-and-forget calls never run
func waitUntil(_ done: () async -> Bool) async {
  for _ in 0 ..< 1_000_000 {
    if await done() {
      return
    }
    await Task.yield()
  }
  fatalError("timed out waiting for the 'send' calls to run")
}

@main struct Main {
  static func main() async {
    let m = Mailbox()
    for i in 1 ... 4 {
      send m.tell(i)
    }
    // FIFO: the awaited call is enqueued after the sent calls
    print("[swift] \(await m.drain())")
    // CHECK: [swift] ["tell 1", "tell 2", "tell 3", "tell 4"]

    let m2 = Mailbox()
    for i in 1 ... 4 {
      send m2.tellAsync(i)
    }
    await waitUntil { await m2.drain().count == 4 }
    print("[swift] \(await m2.drain().sorted())")
    // CHECK: [swift] ["tellAsync 1", "tellAsync 2", "tellAsync 3", "tellAsync 4"]

    send mainActorFunction(7)
    await waitUntil { await collector.all().count == 1 }
    // CHECK: [swift] mainActorFunction(7)
    print("[swift] \(await collector.all())")
    // CHECK: [swift] ["mainActorFunction 7"]

    // A 'nonisolated' method can send a call to its own actor
    let m3 = Mailbox()
    m3.log("hello")
    await waitUntil { await m3.drain().count == 1 }
    // CHECK: [swift] Mailbox.log(hello)
    print("[swift] \(await m3.drain())")
    // CHECK: [swift] ["hello"]

    send isolatedParameter(collector, "isolated parameter")
    await waitUntil { await collector.all().count == 2 }
    // CHECK: [swift] isolatedParameter(isolated parameter)
    print("[swift] \(await collector.all())")
    // CHECK: [swift] ["mainActorFunction 7", "isolated parameter"]
  }
}
