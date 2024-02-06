// RUN: %target-typecheck-verify-swift -parse-stdlib
// expect-no-diagnostics

import _Concurrency

func foo() async {
}

await foo()
