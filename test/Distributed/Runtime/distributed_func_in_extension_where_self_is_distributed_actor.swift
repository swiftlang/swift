// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main  -Xfrontend -disable-availability-checking -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s --color

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Distributed
import FakeDistributedActorSystems

protocol GamePlayer: Sendable, Identifiable where ID == FakeActorSystem.ActorID {
  func makeMove() async throws -> String
}
extension GamePlayer where Self: DistributedActor, ActorSystem == FakeActorSystem {
  distributed func startGame(with opponent: String) -> String {
    "game with \(opponent)"
  }
}

distributed actor FishPlayer: GamePlayer {
  typealias ActorSystem = FakeActorSystem
  distributed func makeMove() async throws -> String {
    "move"
  }
}

typealias DefaultDistributedActorSystem = FakeActorSystem

// ==== Execute ----------------------------------------------------------------

func test() async throws {
  let system = DefaultDistributedActorSystem()

  let fish = FishPlayer(actorSystem: system)

  let opponentName = "Caplin"
  let game = try await fish.startGame(with: opponentName)
  print("game: game with \(opponentName)") // CHECK: game: game with Caplin

  print("done") // CHECK: done
}

@main struct Main {
  static func main() async {
    try! await test()
  }
}

