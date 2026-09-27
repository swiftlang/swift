// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -enable-experimental-feature OnewayNowait -disable-experimental-parser-round-trip -parse-as-library %s -c -o %t/a.o -plugin-path %swift-plugin-dir
// RUN: %target-embedded-link %t/a.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-clang-resource-dir-opt -lc++abi -lswift_Concurrency %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: concurrency
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_OnewayNowait
// REQUIRES: concurrency_runtime

// In Embedded Swift 'nowait' on a synchronous 'oneway' method of a plain actor
// or of a global actor enqueues the call on the actor directly: no task body
// with suspension points, callable from synchronous code, FIFO with other
// work enqueued on the same actor, and the caller's task locals are visible
// in the method body

import _Concurrency

enum Probe {
  @TaskLocal static var tag: Int = 0
}

actor Mailbox {
  var received: [Int] = []

  func tell(_ n: Int) oneway {
    received.append(n)
    print("[swift] Mailbox.tell(\(n)) tag: \(Probe.tag)")
  }

  func drain() -> [Int] { received }
}

@globalActor actor Background {
  static let shared = Background()
}

@Background var backgroundLog: [Int] = []

@Background func note(_ n: Int) oneway {
  backgroundLog.append(n)
  print("[swift] note(\(n)) tag: \(Probe.tag)")
}

@Background func drainBackground() -> [Int] { backgroundLog }

// Synchronous senders
func sendToMailbox(_ m: Mailbox) {
  Probe.$tag.withValue(3) {
    for i in 1 ... 3 {
      nowait m.tell(i)
    }
  }
}

func sendToBackground() {
  Probe.$tag.withValue(5) {
    for i in 1 ... 3 {
      nowait note(i * 10)
    }
  }
}

@main struct Main {
  static func main() async {
    print("[swift] test_actor")
    let m = Mailbox()
    sendToMailbox(m)
    // FIFO: the awaited call is enqueued after the three oneway calls
    let all = await m.drain()
    print("[swift] mailbox: \(all.count) \(all[0]) \(all[1]) \(all[2])")

    print("[swift] test_global_actor")
    sendToBackground()
    let log = await drainBackground()
    print("[swift] background: \(log.count) \(log[0]) \(log[1]) \(log[2])")
  }
}

// CHECK-LABEL: [swift] test_actor
// CHECK-NEXT: [swift] Mailbox.tell(1) tag: 3
// CHECK-NEXT: [swift] Mailbox.tell(2) tag: 3
// CHECK-NEXT: [swift] Mailbox.tell(3) tag: 3
// CHECK-NEXT: [swift] mailbox: 3 1 2 3

// CHECK-LABEL: [swift] test_global_actor
// CHECK-NEXT: [swift] note(10) tag: 5
// CHECK-NEXT: [swift] note(20) tag: 5
// CHECK-NEXT: [swift] note(30) tag: 5
// CHECK-NEXT: [swift] background: 3 10 20 30
