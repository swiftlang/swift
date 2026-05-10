// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-module -emit-module-path %t/Transitive.swiftmodule -parse-as-library %t/Transitive.swift
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-module -emit-module-path %t/Direct.swiftmodule -I %t/ -parse-as-library %t/Direct.swift

// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -typecheck -verify -I %t/ %t/Client.swift -enable-upcoming-feature MemberImportVisibility
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -typecheck -verify -I %t/ %t/Client.swift -enable-upcoming-feature MemberImportVisibility -language-mode 6

// REQUIRES: swift_feature_MemberImportVisibility
// REQUIRES: concurrency, OS=macosx

//--- Transitive.swift
public struct Countdown: Sequence, IteratorProtocol {
    public init(count: Int) {
    }

    public mutating func next() -> Int? {
      nil
    }

    // This is needed to make sure that `makeIterator` error is
    // downgrade as well, even though it's not strictly necessary
    // here because Sequence has a default implementation.
    public func makeIterator() -> Self {
      self
    }
}

public struct AsyncCountdown<Element>: AsyncSequence {
  public init(count: Int) {
  }

  public struct AsyncIterator: AsyncIteratorProtocol {
    public mutating func next() async throws -> Element? {
      nil
    }
  }

  public func makeAsyncIterator() -> Self.AsyncIterator {
    AsyncIterator()
  }
}

//--- Direct.swift
import Transitive

public func getCounter(upTo: Int) -> Countdown {
  return Countdown(count: upTo)
}

public func getAsyncCounter(upTo: Int) -> AsyncCountdown<Int> {
  return AsyncCountdown(count: upTo)
}

//--- Client.swift
import Direct
// expected-note 10 {{add import of module 'Transitive'}}

func test() {
  for _ in getCounter(upTo: 3) {
    // expected-error@-1 {{instance method 'next()' is not available due to missing import of defining module 'Transitive'}}
    // expected-error@-2 {{instance method 'makeIterator()' is not available due to missing import of defining module 'Transitive'}}
  }

  let _ = {
    for _ in getCounter(upTo: 3) {
      // expected-warning@-1 {{instance method 'next()' is not available due to missing import of defining module 'Transitive'}}
      // expected-warning@-2 {{instance method 'makeIterator()' is not available due to missing import of defining module 'Transitive'}}
    }
  }
}

func testAsync() async throws {
  for try await _ in getAsyncCounter(upTo: 3) {
    // expected-error@-1 {{instance method 'next()' is not available due to missing import of defining module 'Transitive'}}
    // expected-error@-2 {{instance method 'makeAsyncIterator()' is not available due to missing import of defining module 'Transitive'}}
  }

  let _ = {
      for try await _ in getAsyncCounter(upTo: 3) {
        // expected-warning@-1 {{instance method 'next()' is not available due to missing import of defining module 'Transitive'}}
        // expected-warning@-2 {{instance method 'makeAsyncIterator()' is not available due to missing import of defining module 'Transitive'}}
      }
  }

  _ = Task {
    while !Task.isCancelled {
      do {
        for try await _ in getAsyncCounter(upTo: 3) {
          // expected-warning@-1 {{instance method 'next()' is not available due to missing import of defining module 'Transitive'}}
          // expected-warning@-2 {{instance method 'makeAsyncIterator()' is not available due to missing import of defining module 'Transitive'}}
        }
      }
    }
  }
}
