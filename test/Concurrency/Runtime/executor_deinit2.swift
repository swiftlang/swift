// RUN: %target-run-simple-swift(-parse-as-library  -target %target-swift-5.1-abi-triple) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// UNSUPPORTED: freestanding

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

// this needs to match with the check count below.
let NUM_TASKS : Int = 100

@available(SwiftStdlib 5.1, *)
final class Capture : Sendable {
    func doSomething() { }
    deinit {
        // CHECK-COUNT-100: deinit was called!
        print("deinit was called!")
    }
}

@available(SwiftStdlib 5.1, *)
@main
struct App {
    static func main() async {
        var n = 0
        for _ in 1...NUM_TASKS {
            let c = Capture()
            let r = detach {
                c.doSomething()
            }
            await r.get()
            n += 1
        }
        print("test complete")
    }
}
