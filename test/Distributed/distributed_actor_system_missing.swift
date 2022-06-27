// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-distributed -disable-availability-checking -I %t 2>&1 %s

// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed

distributed actor DA {
  // expected-error@-1{{distributed actor 'DA' does not declare ActorSystem it can be used with.}}

  // Since synthesis would have failed due to the missing ActorSystem:
  // expected-error@-4{{type 'DA' does not conform to protocol 'Encodable'}}
  // expected-error@-5{{type 'DA' does not conform to protocol 'Decodable'}}

  // expected-note@-7{{you can provide a module-wide default actor system by declaring:}}

  // Note to add the typealias is diagnosed on the protocol:
  // _Distributed.DistributedActor:3:20: note: diagnostic produced elsewhere: protocol requires nested type 'ActorSystem'; do you want to add it?
}

// When an actor declares a typealias for a system, but that system does not exist,
// we need to fail gracefully, rather than crash trying to use the non-existing type.
//
// Test case for: https://github.com/apple/swift/issues/58663
distributed actor Server { // expected-error 2 {{distributed actor 'Server' does not declare ActorSystem it can be used with.}}
  // expected-note@-1{{you can provide a module-wide default actor system by declaring:}}
  typealias ActorSystem = DoesNotExistDataSystem
  // expected-error@-1{{cannot find type 'DoesNotExistDataSystem' in scope}}
  typealias SerializationRequirement = any Codable
  distributed func send() { }
}
