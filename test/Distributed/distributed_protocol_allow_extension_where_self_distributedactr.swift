// RUN: %target-typecheck-verify-swift -disable-availability-checking
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed

/// This adds test coverage for rdar://91857262 in which the extension below would cause a crash.
protocol GamePlayer: Sendable, Identifiable where ID == LocalTestingDistributedActorSystem.ActorID {}

extension GamePlayer where Self: DistributedActor, ActorSystem == LocalTestingDistributedActorSystem {
  distributed func startGame(with opponent: String) -> String { opponent }
}

distributed actor FishPlayer: GamePlayer {
  typealias ActorSystem = LocalTestingDistributedActorSystem
}