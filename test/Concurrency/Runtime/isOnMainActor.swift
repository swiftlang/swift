// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -disable-availability-checking %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN:  %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: asserts
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

@usableFromInline
@_silgen_name("swift_task_isOnMainActor")
internal func _isOnMainActor() -> Bool

@MainActor
func main() async {
  precondition(_isOnMainActor())

  nonisolated func test() async {
    precondition(!_isOnMainActor())
  }

  await test()

  precondition(_isOnMainActor())
}

await main()
