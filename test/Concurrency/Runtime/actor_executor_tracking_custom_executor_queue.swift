// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library -Xfrontend -disable-availability-checking) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: concurrency_runtime
// REQUIRES: libdispatch

// REQUIRES: OS=macosx

// Regression test for rdar://150213107

import Darwin
import Dispatch

actor TestActor {
    private let queue = DispatchSerialQueue(label: "MYQUEUE")

    nonisolated var unownedExecutor: UnownedSerialExecutor {
        queue.asUnownedSerialExecutor()
    }

    init() {
        dispatchPrecondition(condition: .notOnQueue(self.queue))

        Task.detached {
            dispatchPrecondition(condition: .notOnQueue(self.queue))
            sleep(1)
            await self.start()
        }
    }

    private var startupCompleted: Bool = false
    private var startupContinuations: [CheckedContinuation<Void, Never>] = []

    func waitForStartup() async {
        self.assertIsolated()
        guard !startupCompleted else {
            print("waitForStartup - completed, bailing")
            return
        }

        print("waitForStartup - adding continuation")
        sleep(2)
        print("waitForStartup - slept...")

        print("waitForStartup - suspend")
        await withCheckedContinuation { continuation in
            print("waitForStartup - suspend inside")
            dispatchPrecondition(condition: .onQueue(self.queue))
            startupContinuations.append(continuation)
            print("waitForStartup - added")
        }
    }

    private func start() {
        dispatchPrecondition(condition: .onQueue(self.queue))
        print("start")

        startupCompleted = true
        for continuation in startupContinuations {
            print("start - resuming")
            continuation.resume()
        }
        startupContinuations = []
        print("start - FINISHED")
    }
}


@main struct Main {
  static func main() async {
    // CHECK: waitForStartup - adding continuation
    // CHECK: waitForStartup - added
    // CHECK: start
    // CHECK: start - FINISHED
    // CHECK: DONE
    let test = TestActor()
    await test.waitForStartup()
    print("DONE")
  }
}