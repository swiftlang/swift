// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o
// RUN: %target-clang %t/a.o %target-embedded-posix-shim -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-clang-resource-dir-opt -lswift_Concurrency %target-swift-default-executor-opt -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded

import _Concurrency

enum NumberError: Error {
case isMultipleOfSeven(Int)
}

actor Number {
    var val: Int
    var task: Task<Void, any Error>?

    func increment() throws {
        if val > 7 && val % 7 == 0 {
            throw NumberError.isMultipleOfSeven(val)
        }
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

        task = Task.detached(priority: .high) {
            for _ in 0..<100 {
                try await self.increment()
            }
        }
 
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
        // CHECK:       Found thrown error
        do {
            let n1 = await Number()
            try await n1.task!.value
            print("Error wasn't thrown!?")
            fatalError("Error wasn't thrown??")
        } catch let e as NumberError {
            switch e {
            case .isMultipleOfSeven(let value):
                print("Found thrown error")
                precondition(value % 7 == 0)
            }
        } catch {
            fatalError("Not thrown")
        }
    }
}
