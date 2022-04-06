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
