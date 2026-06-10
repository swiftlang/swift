// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-irgen -O -g -module-name test -primary-file %s

// REQUIRES: concurrency

public nonisolated(nonsending) func withHandler<Return, Failure>(
  operation: nonisolated(nonsending) () async throws(Failure) -> Return
) async throws(Failure) -> Return {
  try await operation()
}

public nonisolated(nonsending) func compute<T>(
  _ fn: () -> Void
) async -> T {
  fatalError()
}

struct Test {
  static func wait() async -> (@Sendable () async -> Void)? {
    await withHandler {
      await compute {
      }
    }
  }
}
