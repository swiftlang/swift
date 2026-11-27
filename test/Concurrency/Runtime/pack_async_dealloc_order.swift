// RUN: %target-run-simple-swift(-parse-as-library -Xfrontend -disable-availability-checking)

// REQUIRES: executable_test
// REQUIRES: concurrency

// Test that parameter pack temporaries in async contexts are deallocated in LIFO order.
// https://github.com/swiftlang/swift/issues/XXXXX

protocol Wrapper: Sendable {
    associatedtype Value: Sendable
}

struct DefaultWrapper<T: Sendable>: Wrapper {
    typealias Value = T
    init() {}
}

func innerAsync<each Input: Sendable, each W: Wrapper>(
    wrappers: repeat each W,
    test: @escaping @Sendable () async -> Void
) async where (repeat (each W).Value) == (repeat each Input) {
    await test()
}

func outerAsync<each Input: Sendable>(
    seeds: [(repeat each Input)] = [],
    test: @escaping @Sendable () async -> Void
) async {
    // This pattern creates pack temporaries that must be deallocated in LIFO order
    await innerAsync(
        wrappers: repeat DefaultWrapper<each Input>(),
        test: test
    )
}

func runTest() async {
    for _ in 0..<1000 {
        await outerAsync(seeds: [] as [(Int, Int, Int)]) {
            // Empty test body
        }
    }
}

@main
struct Main {
    static func main() async {
        await withTaskGroup(of: Void.self) { group in
            for _ in 0..<100 {
                group.addTask {
                    await runTest()
                }
            }
        }
        print("Done")
    }
}
