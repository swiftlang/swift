// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.7-abi-triple %s
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed

func acceptEscaping(_ fn: @escaping () -> Void) {}

distributed actor D {
  typealias ActorSystem = LocalTestingDistributedActorSystem
  var x = 0

  func f() {
    acceptEscaping { [unowned self] in self.x += 1 }
    // expected-error@-1 {{actor-isolated closure cannot capture 'self' as 'unowned'}}
    // expected-note@-2 {{capture 'self' as 'weak' instead}} {{23-30=weak}}
  }
}
