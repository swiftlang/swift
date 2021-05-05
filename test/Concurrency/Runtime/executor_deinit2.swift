// RUN: %target-run-simple-swift(-parse-as-library -Xfrontend -enable-experimental-concurrency %import-libdispatch) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// this needs to match with the check count below.
let NUM_TASKS : Int = 100

@available(SwiftStdlib 5.5, *)
final class Capture : Sendable {
    func doSomething() { }
    deinit {
        // CHECK-COUNT-100: deinit was called!
        print("deinit was called!")
    }
}

@available(SwiftStdlib 5.5, *)
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
