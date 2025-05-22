// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.1-abi-triple -swift-version 6 -enable-upcoming-feature InferIsolatedConformances:migrate %s

// REQUIRES: concurrency
// REQUIRES: swift_feature_InferIsolatedConformances

protocol P { }
protocol Q: P { }

struct S: P { }

struct S2: Q { }

@MainActor
struct MA1: P { }
// expected-warning@-1{{conformance of 'MA1' to 'P' should be marked 'nonisolated' to retain its behavior with upcoming feature 'InferIsolatedConformances'}}{{13-13=nonisolated }}

@MainActor
struct MA2: Q { }
// expected-warning@-1{{conformance of 'MA2' to 'Q' should be marked 'nonisolated' to retain its behavior with upcoming feature 'InferIsolatedConformances'}}{{13-13=nonisolated }}

@MainActor
struct MA3 { }

extension MA3: P, Q { }
// expected-warning@-1{{conformance of 'MA3' to 'P' should be marked 'nonisolated' to retain its behavior with upcoming feature 'InferIsolatedConformances'}}{{16-16=nonisolated }}
// expected-warning@-2{{conformance of 'MA3' to 'Q' should be marked 'nonisolated' to retain its behavior with upcoming feature 'InferIsolatedConformances'}}{{19-19=nonisolated }}

@MainActor
struct MA4: @MainActor P { }

@MainActor
struct MA5: nonisolated P { }

