// RUN: %target-run-simple-swift(-parse-as-library -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls) | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: concurrency_runtime
// REQUIRES: swift_feature_SendCalls

// UNSUPPORTED: back_deployment_runtime

// 'send' delivers to an actor in FIFO order: successive 'send a.tell(i)'
// calls are enqueued on the actor's serial executor in program order, so they
// run in exactly that order. A final awaited call drains the mailbox and, by
// FIFO, observes every prior message

actor Mailbox {
  var received: [Int] = []
  @oneway func tell(_ n: Int) {
    received.append(n)
    print("got \(n)")
  }
  func drain() -> [Int] { received }
}

@main struct Main {
  static func main() async {
    let m = Mailbox()
    for i in 1 ... 8 {
      send m.tell(i)
    }
    let all = await m.drain()
    print("order: \(all)")
  }
}

// CHECK: got 1
// CHECK-NEXT: got 2
// CHECK-NEXT: got 3
// CHECK-NEXT: got 4
// CHECK-NEXT: got 5
// CHECK-NEXT: got 6
// CHECK-NEXT: got 7
// CHECK-NEXT: got 8
// CHECK-NEXT: order: [1, 2, 3, 4, 5, 6, 7, 8]
