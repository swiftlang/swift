// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-ir -o - -disable-implicit-concurrency-module-import %s -verify

// REQUIRES: concurrency

final class NC<A> {
  struct State { }

  // expected-error@+1{{'_Concurrency' module not imported, required for @isolated(any)}}
  @discardableResult public func doIt(_ handler: (@escaping @Sendable @isolated(any) (_ connection: NC, _ state: State) -> Void)) -> Self {
    return self
  }
}

extension NC {
  func blah() {
    doIt { conn, state in
      
    }
  }
}
