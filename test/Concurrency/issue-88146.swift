// RUN: %target-swift-frontend -disable-availability-checking -language-mode 5 -strict-concurrency=complete %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend -disable-availability-checking -language-mode 6 %s -emit-sil -o /dev/null -verify

// REQUIRES: concurrency

// https://github.com/swiftlang/swift/issues/88146

@MainActor
final class App {
  var age = 1

  func main() {
    run0 {
      self.age = 1 // Ok
    }

    runVariadic {
      self.age = 1 // Ok
    }

    runVariadic { @MainActor in
      self.age = 1 // Ok
    }

    runVariadic(values: 1) {
      self.age = 1 // Ok
    }
  }

  nonisolated func run0(
    @_inheritActorContext action: @escaping @Sendable () async -> Swift.Void
  )  {
  }

  nonisolated func runVariadic<each T>(
    values: repeat each T,
    @_inheritActorContext action: @escaping @Sendable () async -> Swift.Void
  ) {
  }
}
