// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o
// RUN: %target-clang %t/a.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%target-cpu-apple-macos -lswift_Concurrency -lswift_ConcurrencyDefaultExecutor -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

import _Concurrency

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
            fatalError("miscomputation?")
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

@main struct Main {
    static func main() async {

        // CHECK:       finished init()
        // CHECK-NEXT:  did increment

        let n1 = await Number()
        await n1.task!.value

        let n2 = await Number(iterations: 1000)
        let val = await n2.val
        guard val == 1 else {
            fatalError("wrong val setting")
        }
    }
}
