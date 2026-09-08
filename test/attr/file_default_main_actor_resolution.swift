// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -verify -disable-implicit-concurrency-module-import -enable-experimental-feature DefaultIsolationPerFile %t/module_absent.swift
// RUN: %target-swift-frontend -typecheck -verify -disable-implicit-concurrency-module-import -enable-experimental-feature DefaultIsolationPerFile %t/module_absent_shadowed.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature DefaultIsolationPerFile %t/shadowed_by_type.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature DefaultIsolationPerFile %t/shadowed_by_global_actor.swift

// REQUIRES: swift_feature_DefaultIsolationPerFile

//--- module_absent.swift

using @MainActor
// expected-error@-1:8 {{cannot find type 'MainActor' in scope}}
// expected-note@-2:7 {{'using' supports '@MainActor', 'nonisolated', '@available', and '@diagnose'}}

struct NotAnActor {} // expected-note@:8 {{'NotAnActor' declared here}}

using @NotAnActor
// expected-error@-1:7 {{'@NotAnActor' is not valid in a 'using' declaration}}
// expected-note@-2:7 {{'using' supports '@MainActor', 'nonisolated', '@available', and '@diagnose'}}

using nonisolated
using @available(*, deprecated, message: "legacy")

//--- module_absent_shadowed.swift

struct MainActor {} // expected-note@:8 {{'MainActor' declared here}}

using @MainActor
// expected-error@-1:7 {{'@MainActor' is not valid in a 'using' declaration}}
// expected-note@-2:7 {{'using' supports '@MainActor', 'nonisolated', '@available', and '@diagnose'}}

//--- shadowed_by_type.swift

struct MainActor {} // expected-note@:8 {{'MainActor' declared here}}

using @MainActor
// expected-error@-1:7 {{'@MainActor' is not valid in a 'using' declaration}}
// expected-note@-2:7 {{'using' supports '@MainActor', 'nonisolated', '@available', and '@diagnose'}}

//--- shadowed_by_global_actor.swift

@globalActor
actor MainActor { // expected-note@:7 {{'MainActor' declared here}}
  static let shared = MainActor()
}

using @MainActor
// expected-error@-1:7 {{global actor 'MainActor' is not valid in a 'using' declaration}}
// expected-note@-2:7 {{file-level default isolation must be '@MainActor' or 'nonisolated'}}
