// RUN: %target-run-simple-swift(-parse-as-library -enable-experimental-feature OnewayNowait -disable-experimental-parser-round-trip) | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: concurrency_runtime
// REQUIRES: swift_feature_OnewayNowait

// UNSUPPORTED: back_deployment_runtime

// 'nowait' delivers to an actor in FIFO order: successive 'nowait a.tell(i)'
// calls are enqueued on the actor's serial executor in program order, so they
// run in exactly that order. A final awaited call drains the mailbox and, by
// FIFO, observes every prior message

actor Mailbox {
  var received: [Int] = []
  func tell(_ n: Int) {
    received.append(n)
    print("got \(n)")
  }
  func drain() -> [Int] { received }
}

@main struct Main {
  static func main() async {
    let m = Mailbox()
    for i in 1 ... 8 {
      nowait m.tell(i)
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
