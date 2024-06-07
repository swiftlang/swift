// RUN: %target-swift-frontend  -disable-availability-checking %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend  -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=targeted
// RUN: %target-swift-frontend  -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=complete
// RUN: %target-swift-frontend  -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation

// REQUIRES: concurrency
// REQUIRES: asserts
// REQUIRES: libdispatch

@available(SwiftStdlib 5.1, *)
@rethrows
protocol TGP: AsyncSequence, AsyncIteratorProtocol { }

@available(SwiftStdlib 5.1, *)
extension TaskGroup: TGP { }
// expected-warning@-1{{extension declares a conformance of imported type 'TaskGroup' to imported protocol 'AsyncIteratorProtocol'}}
// expected-note@-2{{add '@retroactive' to silence this warning}}

@available(SwiftStdlib 5.1, *)
extension ThrowingTaskGroup: TGP { }
// expected-warning@-1{{extension declares a conformance of imported type 'ThrowingTaskGroup' to imported protocol 'AsyncIteratorProtocol'}}
// expected-note@-2{{add '@retroactive' to silence this warning}}
