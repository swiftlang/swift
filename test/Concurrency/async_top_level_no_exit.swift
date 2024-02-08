// RUN: %target-typecheck-verify-swift -disable-availability-checking -parse-stdlib
// expect-no-diagnostics

import _Concurrency

func foo() async {
}

await foo()
