// RUN: %target-run-simple-swift(-O -swift-version 6 -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// The opaque-values test mode hits an unrelated AddressLowering verifier failure.
// XFAIL: swift_test_mode_optimize_none_with_opaque_values

// Check that ARCSequenceOpts does not remove the retain/release which keeps a
// Span's owner alive.
// https://github.com/swiftlang/swift/issues/94047

@MainActor var hook: (@MainActor () -> Void)?

@available(SwiftStdlib 6.2, *)
@MainActor @inline(never) func read(_ values: Span<Int>) -> Int {
    let before = values[0]
    hook?()
    return before &+ values[0]
}

@MainActor final class Owner {
    var values: [Int] = []

    @inline(never) func add(_ value: Int) {
        values.append(value)
    }

    @inline(never) func clear() {
        values = []
    }

    @available(SwiftStdlib 6.2, *)
    @inline(never) func run() -> Int {
        let snapshot = values
        return read(snapshot.span)
    }
}

@main struct Probe {
    @MainActor static func main() {
        if #available(SwiftStdlib 6.2, *) {
            let owner = Owner()
            owner.add(7)
            hook = { owner.clear() }
            // CHECK: result: 14
            print("result:", owner.run())
        } else {
            print("result: 14")
        }
    }
}
