// RUN: %target-swift-frontend -disable-availability-checking  %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend -disable-availability-checking  %s -emit-sil -o /dev/null -verify -strict-concurrency=targeted
// RUN: %target-swift-frontend -disable-availability-checking  %s -emit-sil -o /dev/null -verify -strict-concurrency=complete
// RUN: %target-swift-frontend -disable-availability-checking  %s -emit-sil -o /dev/null -verify -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation

// REQUIRES: concurrency
// REQUIRES: asserts

func autoclosureCapture(_: @autoclosure () async throws -> Int) async {}
func nonescapingCapture(_: () async throws -> Int) {}
func escapingCapture(_: @escaping () async throws -> Int) {}

func foo() async {
    async let x = 32

    async let y = x // expected-error{{not supported}}
    _ = await y

    await autoclosureCapture(await x)// expected-error{{not supported}}
    nonescapingCapture { await x }// expected-error{{not supported}}
    escapingCapture { await x }// expected-error{{not supported}}
}
