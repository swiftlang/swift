// RUN: %target-typecheck-verify-swift -swift-version 6 -enable-upcoming-feature NonisolatedNonsendingByDefault %s

// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

func testCasts() {
  let defaultedType = (() async -> ()).self
  _ = defaultedType as (@concurrent () async -> ()).Type
  // expected-error@-1 {{cannot convert value of type '(nonisolated(nonsending) () async -> ()).Type' to type '(@concurrent () async -> ()).Type' in coercion}}
  _ = defaultedType as (nonisolated(nonsending) () async -> ()).Type // Ok
}

protocol TestWitnessFixIts {
  func test(_: @concurrent () async -> Void)
  // expected-note@-1 {{protocol requires function 'test' with type '(@concurrent () async -> Void) -> ()'}}
}

do {
  struct Test: TestWitnessFixIts { // expected-error {{type 'Test' does not conform to protocol 'TestWitnessFixIts'}}
    // expected-note@-1 {{add stubs for conformance}} {{35-35=\n      func test(_: @concurrent () async -> Void) {\n          <#code#>\n      \}\n}}
    func test(_: () async -> Void) {}
    // expected-note@-1 {{candidate has non-matching type '(nonisolated(nonsending) () async -> Void) -> ()'}}
  }
}
