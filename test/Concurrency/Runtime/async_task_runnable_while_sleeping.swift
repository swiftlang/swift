// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// UNSUPPORTED: freestanding

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

// Regression test for: https://github.com/swiftlang/swift/issues/92461
//
// Tasks that become runnable while another task is sleeping must run without
// waiting for that sleep's deadline.

let sleepDuration: Duration = .seconds(30)

func scenario(_ name: String, yields: Int) async {
  let clock = ContinuousClock()
  let start = clock.now

  let sleeper = Task {
    try? await Task.sleep(for: sleepDuration)
    print("\(name): sleeper woke, cancelled=\(Task.isCancelled)")
  }
  let worker = Task {
    for _ in 0 ..< yields {
      await Task.yield()
    }
    print("\(name): worker done")
  }
  await worker.value
  sleeper.cancel()
  await sleeper.value
  print("\(name): before deadline=\(clock.now - start < sleepDuration)")
}

@main struct Main {
  static func main() async {
    // CHECK: no-yield: worker done
    // CHECK-NEXT: no-yield: sleeper woke, cancelled=true
    // CHECK-NEXT: no-yield: before deadline=true
    await scenario("no-yield", yields: 0)

    // CHECK-NEXT: yield: worker done
    // CHECK-NEXT: yield: sleeper woke, cancelled=true
    // CHECK-NEXT: yield: before deadline=true
    await scenario("yield", yields: 50)
  }
}
