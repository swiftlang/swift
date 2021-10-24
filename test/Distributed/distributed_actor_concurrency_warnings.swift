// RUN: %target-typecheck-verify-swift -warn-concurrency -enable-experimental-distributed -disable-availability-checking
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

actor Charlie {
    // should not cause sendable warnings, Worker is Sendable as implied by DA
    func two<Worker>() -> Set<Worker> where Worker: DistributedActor {
        []
    }
}