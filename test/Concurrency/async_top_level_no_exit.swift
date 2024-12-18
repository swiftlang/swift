// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple -parse-stdlib
// expect-no-diagnostics

import _Concurrency

func foo() async {
}

await foo()
