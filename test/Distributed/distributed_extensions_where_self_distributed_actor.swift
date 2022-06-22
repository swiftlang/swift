// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unknown -disable-availability-checking -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

@available(SwiftStdlib 5.7, *)
typealias DefaultDistributedActorSystem = FakeActorSystem

// Reproducer for: rdar://91857262
// Being able to use protocols and extend them conditionally with
// `where Self: DistributedActor` and adding distributed methods onto them then.
protocol GamePlayer: Sendable, Identifiable where ID == FakeActorSystem.ActorID {
    func makeMove() async throws -> String
}
extension GamePlayer where Self: DistributedActor, ActorSystem == FakeActorSystem {
    distributed func startGame(with opponent: String) {}
}

distributed actor FishPlayer: GamePlayer {
    typealias ActorSystem = FakeActorSystem
    distributed func makeMove() async throws -> String {
        "move"
    }
}

func test(player: FishPlayer) async throws {
    try await player.startGame(with: "Caplin")
}
