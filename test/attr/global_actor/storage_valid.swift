// RUN: %target-swift-frontend -typecheck -verify -swift-version 5 %s

// REQUIRES: concurrency

actor SomeActor {}

@globalActor
struct GA1 {
  static let shared = SomeActor()
}

// Global actor on global var/let.

@GA1 let isolatedGlobalStoredLet: Int = 0
@GA1 var isolatedGlobalStoredVar: Int = 0
@GA1 var isolatedGlobalComputed: Int {0}

// Global actor on properties/subscripts.
do {
  struct VarLet {
    @GA1 let stored: Int
    @GA1 var computed: Int {0}
    @GA1 subscript(_: Int) -> Int {0}
  }
  @GA1 struct VarLetRedundantIsolation {
    @GA1 let stored: Int
    @GA1 var computed: Int {0}
    @GA1 subscript(_: Int) -> Int {0}
  }
}
