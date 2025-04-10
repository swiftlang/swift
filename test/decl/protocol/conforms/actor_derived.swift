// RUN: %target-typecheck-verify-swift
// REQUIRES: concurrency

@available(SwiftStdlib 5.1, *)
actor A1: Hashable {
  nonisolated func hash(into hasher: inout Hasher) { }
  static func ==(lhs: A1, rhs: A1) -> Bool { true }
}

@available(SwiftStdlib 5.1, *)
actor A2: Hashable {
  nonisolated var hashValue: Int { 0 } // expected-warning{{'Hashable.hashValue' is deprecated as a protocol requirement; conform type 'A2' to 'Hashable' by implementing 'hash(into:)' instead}}{{documentation-file=deprecated-declaration}}
  static func ==(lhs: A2, rhs: A2) -> Bool { true }
}


@available(SwiftStdlib 5.1, *)
@MainActor
class C1: Hashable {
  nonisolated func hash(into hasher: inout Hasher) { }
  nonisolated static func ==(lhs: C1, rhs: C1) -> Bool { true }
}

@available(SwiftStdlib 5.1, *)
@MainActor
class C2: Hashable {
  nonisolated var hashValue: Int { 0 } // expected-warning{{'Hashable.hashValue' is deprecated as a protocol requirement; conform type 'C2' to 'Hashable' by implementing 'hash(into:)' instead}}{{documentation-file=deprecated-declaration}}
  nonisolated static func ==(lhs: C2, rhs: C2) -> Bool { true }
}

