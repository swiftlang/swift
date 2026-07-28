// RUN: %empty-directory(%t)
// RUN: %target-build-swift -language-mode 5 -strict-concurrency=complete -target %target-swift-5.1-abi-triple %s -parse-as-library -module-name main -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main

// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: executable_test

// UNSUPPORTED: back_deployment_runtime

// https://github.com/swiftlang/swift/issues/88993

actor MyActor {
  nonisolated func perform<E: Error>(_ work: @escaping @Sendable (isolated MyActor) async throws(E) -> ()) async throws(E) {
    try await work(self)
  }
}

// The bug only happens if this closure is `nonisolated(nonsending)`
typealias MigrationWorkBlock = nonisolated(nonsending) () async -> Void

nonisolated struct MyOperation {
  let actor = MyActor()
  let migrationWork: MigrationWorkBlock

  nonisolated func run() async {
    await self.actor.perform { actor in
      actor.assertIsolated()

      await migrationWork() // nonisolated(nonsending) should hop back to the context were it was executed

      actor.assertIsolated() // Ok
    }
  }
}

@main struct Main {
  static func main() async throws {
    let operation = MyOperation(migrationWork: { }) // closure here is @MainActor isolated
    await operation.run()
  }
}
