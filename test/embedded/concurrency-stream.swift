// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -target %target-cpu-apple-macos14 -parse-as-library %s -c -o %t/a.o
// RUN: %target-clang %t/a.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%target-cpu-apple-macos -lswift_Concurrency -lswift_ConcurrencyDefaultExecutor -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

import _Concurrency

@main struct Main {
    static func main() async {
        let (stream, continuation) = AsyncStream.makeStream(of: String.self)
        Task {
            for i in 0 ..< 10 {
                continuation.yield("\(i)")
            }
            continuation.finish()
        }
        for await value in stream {
            print(value)
        }
        print("All done!")
        // CHECK: 0
        // CHECK: 1
        // CHECK: 2
        // CHECK: 3
        // CHECK: 4
        // CHECK: 5
        // CHECK: 6
        // CHECK: 7
        // CHECK: 8
        // CHECK: 9
        // CHECK: All done!
    }
}
