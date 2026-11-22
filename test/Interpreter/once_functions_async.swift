// RUN: %target-run-simple-swift(-enable-experimental-feature OnceFunctions) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature OnceFunctions -O -Xfrontend -sil-verify-all) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: swift_feature_OnceFunctions

// UNSUPPORTED: back_deployment_runtime

// Executable tests of exactly-once and at-most-once functions that need
// concurrency.

// A closure with no captures, passed as a `nonisolated(nonsending)`
// exactly-once or at-most-once argument, keeps its execution semantics when it
// is thickened.

@inline(never)
nonisolated(nonsending) func runExactlyOnce<Return, Failure>(
  _ operation: nonisolated(nonsending) @once () async throws(Failure) -> Return
) async throws(Failure) -> Return {
  try await operation()
}

@inline(never)
nonisolated(nonsending) func runAtMostOnce<Return, Failure>(
  _ operation:
    nonisolated(nonsending) @callable(once) () async throws(Failure) -> Return
) async throws(Failure) -> Return {
  try await operation()
}

@inline(never)
func printOperation(_ name: String) { print("\(name) operation") }

@inline(never)
func testExactlyOnce() async {
  await runExactlyOnce { printOperation("exactly-once") }
}

@inline(never)
func testAtMostOnce() async {
  await runAtMostOnce { printOperation("at-most-once") }
}

// CHECK: exactly-once operation
await testExactlyOnce()
// CHECK-NEXT: at-most-once operation
await testAtMostOnce()
