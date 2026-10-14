// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple
// REQUIRES: concurrency

// Without the experimental feature only '@isolated(any)' is accepted

func concrete(_ fn: @isolated(MainActor) () -> Void) {} // expected-error{{expected 'any' as the isolation kind}}{{31-40=any}}

func generic<G: GlobalActor>(_: G.Type, _ fn: @isolated(G) () -> Void) {} // expected-error{{expected 'any' as the isolation kind}}{{57-58=any}}
