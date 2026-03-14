// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

/// Build the library
// RUN: %target-swift-frontend -emit-module %t/src/Lib.swift \
// RUN:   -target %target-swift-5.1-abi-triple \
// RUN:   -module-name Lib -swift-version 5 -enable-library-evolution \
// RUN:   -emit-module-path %t/Lib.swiftmodule \
// RUN:   -emit-module-interface-path %t/Lib.swiftinterface

// RUN: rm %t/Lib.swiftmodule

// Build the client using interface
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.1-abi-triple -swift-version 5 -I %t %t/src/Test.swift -enable-upcoming-feature NonisolatedNonsendingByDefault:migrate

// REQUIRES: asserts
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

//--- Lib.swift
public struct Counter: AsyncSequence {
  public typealias Element = Int

  public init(howHigh: Int) {
  }

  public struct AsyncIterator: AsyncIteratorProtocol {
    public mutating func next() async -> Int? {
      nil
    }
  }
  
  public func makeAsyncIterator() -> AsyncIterator {
    AsyncIterator()
  }
}

//--- Test.swift
import Lib

func count(n: Int) async {
  // expected-warning@-1 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async global function 'count' to run on the caller's actor; use '@concurrent' to preserve behavior}}
  for await _ in Counter(howHigh: n) {
  }
}
