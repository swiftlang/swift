// RUN: %target-swift-frontend -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -typecheck -verify -verify-ignore-unrelated %s

// The macro-based derivation must accept and reject exactly what the legacy
// AST-building path does, so run the same expectations without the feature.
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated %s

// REQUIRES: swift_feature_DeriveConformancesViaMacros
// REQUIRES: concurrency

enum Simple: CaseIterable {
  case a, b
}

enum Uninhabited: CaseIterable {}

enum Escaped: CaseIterable {
  case `default`, `class`, `self`, `Self`, `Type`
}

enum RawIdentifiers: CaseIterable {
  case `foo bar`, `if let`
}

enum WithRawValue: String, CaseIterable {
  case one, two
}

enum Generic<T>: CaseIterable {
  case x, y
}

struct Outer {
  enum Nested: CaseIterable {
    case a
  }
}

enum InExtension {
  case a, b
}

extension InExtension: CaseIterable {}

@MainActor
enum GlobalActorIsolated: CaseIterable {
  case a, b
}

actor SomeActor {
  enum Nested: CaseIterable {
    case a
  }
}

func useThem() {
  enum Local: CaseIterable {
    case a, b
  }

  let _: Local.AllCases = Local.allCases
  let _: [Local] = Local.allCases
  let _: [Simple] = Simple.allCases
  let _: Simple.AllCases = Simple.allCases
  let _: [Escaped] = Escaped.allCases
  let _: [Generic<Int>] = Generic<Int>.allCases
  let _: [Outer.Nested] = Outer.Nested.allCases
  let _: [InExtension] = InExtension.allCases
  let _: [Uninhabited] = Uninhabited.allCases
}

func useIsolated() {
  let _: [GlobalActorIsolated] = GlobalActorIsolated.allCases
  let _: [SomeActor.Nested] = SomeActor.Nested.allCases
}

enum HasAssociatedValues: CaseIterable { // expected-error {{type 'HasAssociatedValues' does not conform to protocol 'CaseIterable'}}
  // expected-note@-1 {{add stubs for conformance}}
  case a(Int)
}

enum PotentiallyUnavailableCase: CaseIterable { // expected-error {{type 'PotentiallyUnavailableCase' does not conform to protocol 'CaseIterable'}}
  // expected-note@-1 {{add stubs for conformance}}
  @available(macOS 99, *)
  case a
  case b
}
