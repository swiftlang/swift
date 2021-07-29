// RUN: %target-typecheck-verify-swift -enable-experimental-distributed
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

@available(SwiftStdlib 5.5, *)
distributed actor Capybara { }

//@available(SwiftStdlib 5.5, *)
//protocol Wheeker: DistributedActor { }
//@available(SwiftStdlib 5.5, *)
//distributed actor GuineaPing: Wheeker { }

@available(SwiftStdlib 5.5, *)
func test<Identity: ActorIdentity>(identity: Identity, transport: ActorTransport) async throws {
  let _: Capybara = try Capybara.resolve(identity, using: transport)

// TODO: implement resolve being able to be called on a distributed actor protocol
//       (yes, normally it is not allowed to call such function... so we need to
//        discuss and figure out how we want to expose the resolve of a protocol)
//  let c: Wheeker = try Wheeker.resolve(.init(identity), using: transport)
}