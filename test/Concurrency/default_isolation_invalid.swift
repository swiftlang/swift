// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// REQUIRES: concurrency
// REQUIRES: swift_feature_DefaultIsolationTypealias


//--- invalid_access.swift

// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationTypealias -c -verify -swift-version 6 -disable-availability-checking %t/invalid_access.swift 

typealias DefaultIsolation = MainActor
// expected-error@-1 {{default isolation can only be set per file}}


//--- invalid_actor.swift

// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationTypealias -c -verify -swift-version 6 -disable-availability-checking %t/invalid_actor.swift

@globalActor
actor CustomGlobalActor {
  static let shared = CustomGlobalActor()
}

private typealias DefaultIsolation = CustomGlobalActor
// expected-error@-1 {{default isolation can only be set to 'MainActor' or 'nonisolated'}}
