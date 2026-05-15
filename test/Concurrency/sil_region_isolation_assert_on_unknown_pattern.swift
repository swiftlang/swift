// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -sil-region-isolation-assert-on-unknown-pattern -target %target-swift-5.1-abi-triple %s -o /dev/null

// REQUIRES: concurrency

// Verify that -sil-region-isolation-assert-on-unknown-pattern is accepted and
// does not abort when region isolation completes without hitting an unknown
// pattern. We cannot really test it since we are trying to fix those issues.

func takesInt(_ x: Int) async {}

func test() async {
  let x = 1
  await takesInt(x)
}
