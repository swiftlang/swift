// RUN: %target-typecheck-verify-swift -strict-concurrency=complete -disable-availability-checking
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed

actor Charlie {
    // should not cause sendable warnings, Worker is Sendable as implied by DA
    func two<Worker>() -> Set<Worker> where Worker: DistributedActor {
        []
    }
}
