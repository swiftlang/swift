// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -verify -disable-implicit-concurrency-module-import -enable-experimental-feature DefaultIsolationPerFile %t/module_absent.swift
// RUN: %target-swift-frontend -typecheck -verify -disable-implicit-concurrency-module-import -enable-experimental-feature DefaultIsolationPerFile %t/module_absent_shadowed.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature DefaultIsolationPerFile %t/shadowed_by_type.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature DefaultIsolationPerFile %t/shadowed_by_global_actor.swift

// REQUIRES: swift_feature_DefaultIsolationPerFile

//--- module_absent.swift

default @MainActor
// expected-error@-1:10 {{cannot find type 'MainActor' in scope}}
// expected-note@-2:9 {{a file-level default must be '@MainActor', 'nonisolated', '@available', or '@diagnose'}}

struct NotAnActor {} // expected-note@:8 {{'NotAnActor' declared here}}

default @NotAnActor
// expected-error@-1:9 {{'@NotAnActor' is not valid in a 'default' declaration}}
// expected-note@-2:9 {{a file-level default must be '@MainActor', 'nonisolated', '@available', or '@diagnose'}}

default nonisolated
default @available(*, deprecated, message: "legacy")

//--- module_absent_shadowed.swift

struct MainActor {} // expected-note@:8 {{'MainActor' declared here}}

default @MainActor
// expected-error@-1:9 {{'@MainActor' is not valid in a 'default' declaration}}
// expected-note@-2:9 {{a file-level default must be '@MainActor', 'nonisolated', '@available', or '@diagnose'}}

//--- shadowed_by_type.swift

struct MainActor {} // expected-note@:8 {{'MainActor' declared here}}

default @MainActor
// expected-error@-1:9 {{'@MainActor' is not valid in a 'default' declaration}}
// expected-note@-2:9 {{a file-level default must be '@MainActor', 'nonisolated', '@available', or '@diagnose'}}

//--- shadowed_by_global_actor.swift

@globalActor
actor MainActor { // expected-note@:7 {{'MainActor' declared here}}
  static let shared = MainActor()
}

default @MainActor
// expected-error@-1:9 {{global actor 'MainActor' is not valid in a 'default' declaration}}
// expected-note@-2:9 {{file-level default isolation must be '@MainActor' or 'nonisolated'}}
