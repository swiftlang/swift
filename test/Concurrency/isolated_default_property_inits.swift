// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -emit-module-path %t/OtherActors.swiftmodule -module-name OtherActors %S/Inputs/OtherActors.swift -target %target-swift-5.1-abi-triple

// RUN: %target-swift-frontend -I %t  -target %target-swift-5.1-abi-triple -strict-concurrency=complete -enable-upcoming-feature IsolatedDefaultValues -parse-as-library -emit-sil -o /dev/null -verify %s
// RUN: %target-swift-frontend -I %t  -target %target-swift-5.1-abi-triple -strict-concurrency=complete -parse-as-library -emit-sil -o /dev/null -verify -enable-upcoming-feature IsolatedDefaultValues %s

// REQUIRES: concurrency
// REQUIRES: swift_feature_IsolatedDefaultValues

// This tests errors emitted in definite initialization; this test file cannot
// have any type checker errors. Type checker errors for IsolatedDefaultValues
// are tested in isolated_default_arguments.swift

@globalActor
actor SomeGlobalActor {
  static let shared = SomeGlobalActor()
}

@MainActor
func requiresMainActor() -> Int { 0 }

@SomeGlobalActor
func requiresSomeGlobalActor() -> Int { 0 }

class C1 {
  // expected-note@+2 {{main actor-isolated default value of 'self.x' cannot be used in a nonisolated initalizer}}
  // expected-note@+1 {{main actor-isolated default value of 'self.x' cannot be used in a global actor 'SomeGlobalActor'-isolated initalizer}}
  @MainActor var x = requiresMainActor()
  // expected-note@+2 {{global actor 'SomeGlobalActor'-isolated default value of 'self.y' cannot be used in a nonisolated initalizer}}
  // expected-note@+1 {{global actor 'SomeGlobalActor'-isolated default value of 'self.y' cannot be used in a main actor-isolated initalizer}}
  @SomeGlobalActor var y = requiresSomeGlobalActor()
  var z = 10

  // expected-error@+1 {{return from initializer without initializing all stored properties}}
  nonisolated init(a: Int) {}

  // expected-error@+1 {{return from initializer without initializing all stored properties}}
  @MainActor init(b: Int) {}

  // expected-error@+1 {{return from initializer without initializing all stored properties}}
  @SomeGlobalActor init(c: Int) {}
}

class C2 {
  @MainActor var x = requiresMainActor()
  @SomeGlobalActor var y = requiresSomeGlobalActor()
  var z = 10

  nonisolated init(x: Int, y: Int) {
    self.x = x
    self.y = y
  }

  @MainActor init(y: Int) {
    self.y = y
  }

  @SomeGlobalActor init(x: Int) {
    self.x = x
  }
}

