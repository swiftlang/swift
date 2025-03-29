// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency -target %target-swift-5.1-abi-triple

// REQUIRES: concurrency

// We don't want 'reasync' overloads to have a higher score in the
// case of a sync vs reasync mismatch the way 'async' overloads do,
// since this would change solver performance characteristics when
// using the reasync '&&', '||' and '??' operators.

func asyncOverload(_: () async -> (), _: Int) async {}
func asyncOverload(_: () -> (), _: String) {}

func referencesAsyncOverload() {
  _ = asyncOverload // we prefer the sync overload
}

func referencesAsyncOverloadAsync() async {
  _ = asyncOverload // we prefer the async overload
}

func reasyncOverload(_: () async -> (), _: Int) reasync {} // expected-note {{found this candidate}}
func reasyncOverload(_: () -> (), _: String) {} // expected-note {{found this candidate}}

func referencesReasyncOverload() {
  _ = reasyncOverload // expected-error {{ambiguous use of 'reasyncOverload'}}
}

func referencesReasyncOverloadAsync() async {
  // we prefer the async overload because the sync overload scores higher
  // due to a sync-vs-async mismatch.
  _ = reasyncOverload
}
