// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency) 2>&1 | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency

import _Concurrency

struct TestError: Error {}

runAsyncAndBlock {
    let x: Int = await withCheckedContinuation { c in
        c.resume(returning: 17)
        c.resume(returning: 38)
    }
    // CHECK: main tried to resume its continuation more than once, returning 38!

    do {
        let x: Int = await try withCheckedThrowingContinuation { c in
            c.resume(throwing: TestError())
            c.resume(returning: 679)
            c.resume(throwing: TestError())
        }
    } catch {
        // CHECK-NEXT: main tried to resume its continuation more than once, returning 679!
        // CHECK-NEXT: main tried to resume its continuation more than once, throwing TestError()!
    }
    
    _ = Task.runDetached {
        let _: Void = await withCheckedContinuation { _ in
            /*do nothing, leaking the task*/
        }
        // TODO: Whether the detached task gets a chance to run or not before
        // the process exits is currently platform-dependent, and we don't yet
        // have the API for yielding to the task runtime.
        // C/HECK-NEXT: main leaked its continuation!
    }
}
