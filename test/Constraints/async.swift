// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency

func doAsynchronously() async { }
func doSynchronously() { }

func testNonConversions() async {
  let _: () -> Void = doAsynchronously // expected-error{{cannot convert value of type '() async -> ()' to specified type '() -> Void'}}
  let _: () async -> Void = doSynchronously // expected-error{{cannot convert value of type '() -> ()' to specified type '() async -> Void'}}
}
