// REQUIRES: concurrency
// RUN: %target-typecheck-verify-swift -verify-ignore-unknown %s

@MainActor
@available(SwiftStdlib 5.5, *)
public class SomeMainActorClass {
    @available(*, deprecated, renamed: "request(at:completion:)")
    public func request(at: Int) async throws {}
}

@available(SwiftStdlib 5.5, *)
func asyncStuff() async throws {
    let foo = await SomeMainActorClass()
    try await foo.request(at: 11) // expected-warning{{'request(at:)' is deprecated: renamed to 'request(at:completion:)'}}
    //expected-note@-1{{use 'request(at:completion:)' instead}}
}
