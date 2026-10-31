// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -enable-experimental-feature SendCalls -parse-as-library %s -c -o %t/a.o -plugin-path %swift-plugin-dir
// RUN: %target-embedded-link %t/a.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-clang-resource-dir-opt -lc++abi -lswift_Concurrency %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: concurrency
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_SendCalls
// REQUIRES: concurrency_runtime

// In Embedded Swift 'send' can be applied to any Void, non-throwing call of an
// actor isolated function: a synchronous actor method is enqueued on the
// actor, FIFO with other work enqueued on
// it, and a function isolated through an 'isolated' parameter runs in a new
// task, whose operation the task consumes

import _Concurrency

actor Mailbox {
  var received: [Int] = []
  func tell(_ n: Int) {
    received.append(n)
    print("[swift] Mailbox.tell(\(n))")
  }
  func drain() -> [Int] { received }
}

actor Signal {
  var continuation: CheckedContinuation<Void, Never>?
  var fired = false
  func wait() async {
    if fired { return }
    await withCheckedContinuation { continuation = $0 }
  }
  func fire() {
    fired = true
    continuation?.resume()
    continuation = nil
  }
}

let signal = Signal()

func isolatedParameter(_ s: isolated Signal, _ n: Int) {
  print("[swift] isolatedParameter(\(n))")
  s.fire()
}

func sendToMailbox(_ m: Mailbox) {
  for i in 1 ... 3 {
    send m.tell(i)
  }
}

@main struct Main {
  static func main() async {
    print("[swift] test_actor")
    let m = Mailbox()
    sendToMailbox(m)
    // FIFO: the awaited call is enqueued after the three sent calls
    let all = await m.drain()
    print("[swift] mailbox: \(all.count) \(all[0]) \(all[1]) \(all[2])")

    print("[swift] test_isolated_parameter")
    send isolatedParameter(signal, 7)
    await signal.wait()
    print("[swift] done")
  }
}

// CHECK-LABEL: [swift] test_actor
// CHECK-NEXT: [swift] Mailbox.tell(1)
// CHECK-NEXT: [swift] Mailbox.tell(2)
// CHECK-NEXT: [swift] Mailbox.tell(3)
// CHECK-NEXT: [swift] mailbox: 3 1 2 3
// CHECK-LABEL: [swift] test_isolated_parameter
// CHECK-NEXT: [swift] isolatedParameter(7)
// CHECK-NEXT: [swift] done
