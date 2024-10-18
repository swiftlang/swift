// RUN: %target-run-simple-swift(%import-libdispatch -target %target-swift-5.1-abi-triple -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

@available(SwiftStdlib 5.1, *)
actor Number {
    var val: Int
    var task: Task<Void, Never>?

    func increment() {
        print("did increment")
        val += 1
    }

    func fib(n: Int) -> Int {
        if n < 2 {
            return n
        }
        return fib(n: n-1) + fib(n: n-2)
    }

    init() async {
        val = 0

        task = Task.detached(priority: .high) { await self.increment() }

        // do some synchronous work
        let ans = fib(n: 37)
        guard ans == 24157817 else {
            fatalError("got ans = \(ans). miscomputation?")
        }

        // make sure task didn't modify me!
        guard val == 0 else {
            fatalError("race!")
        }

        print("finished init()")
    }

    init(iterations: Int) async {
        var iter = iterations
        repeat {
            val = iter
            iter -= 1
        } while iter > 0
    }
}

@available(SwiftStdlib 5.1, *)
@main struct Main {
    static func main() async {

        // CHECK:       finished init()
        // CHECK-NEXT:  did increment

        let n1 = await Number()
        await n1.task!.value

        let n2 = await Number(iterations: 1000)
        let val = await n2.val
        guard val == 1 else {
            fatalError("wrong val setting! got \(val)")
        }
    }
}

