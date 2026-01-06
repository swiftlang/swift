// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.0-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift -plugin-path %swift-plugin-dir
// RUN: %target-build-swift -module-name main  -target %target-swift-6.0-abi-triple -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -plugin-path %swift-plugin-dir -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s --color

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// FIXME(distributed): Distributed actors currently have some issues on windows, isRemote always returns false. rdar://82593574
// UNSUPPORTED: OS=windows-msvc

import Distributed
import FakeDistributedActorSystems

@Resolvable
protocol GreeterProtocol: DistributedActor where ActorSystem: DistributedActorSystem<any Codable> {
  distributed func greet() -> String
}

protocol FruitProtocol: Codable {}
struct Watermelon: FruitProtocol {}

protocol AnimalProtocol: Codable {}
struct Capybara: AnimalProtocol {}

@Resolvable
protocol FeederProtocol<Fruit, Animal>: DistributedActor where ActorSystem: DistributedActorSystem<any Codable> {
  associatedtype Fruit: FruitProtocol
  associatedtype Animal: AnimalProtocol
  distributed func give(fruit: Fruit, to animal: Animal)
}

// ==== ------------------------------------------------------------------------

distributed actor DAFR: GreeterProtocol {
  typealias ActorSystem = FakeRoundtripActorSystem
  distributed func greet() -> String { "\(Self.self)" }
}

distributed actor DAFL: GreeterProtocol {
  typealias ActorSystem = LocalTestingDistributedActorSystem
  distributed func greet() -> String { "\(Self.self)" }
}

distributed actor Feeder: FeederProtocol {
  typealias ActorSystem = LocalTestingDistributedActorSystem
  distributed func give(fruit: Watermelon, to animal: Capybara) {
    // fake impl
  }
}

@main struct Main {
  static func main() async throws {
    let fakeRoundtripSystem = FakeRoundtripActorSystem()

    let fr = DAFR(actorSystem: fakeRoundtripSystem)
    let frid = fr.id
    _ = DAFL(actorSystem: .init())

    let gfr: any GreeterProtocol = try $GreeterProtocol.resolve(id: frid, using: fakeRoundtripSystem)

    print("resolved on \(fakeRoundtripSystem): \(type(of: gfr))")
    // CHECK: resolved on main.FakeRoundtripActorSystem: $GreeterProtocol<FakeRoundtripActorSystem>
    // CHECK-NEXT: > encode generic sub: main.$GreeterProtocol<main.FakeRoundtripActorSystem>
    // CHECK-NEXT: > encode return type: Swift.String
    // CHECK-NEXT: > done recording
    // CHECK-NEXT: >> remoteCall: on:main.$GreeterProtocol<main.FakeRoundtripActorSystem>, target:main.$GreeterProtocol.greet(), invocation:FakeInvocationEncoder(genericSubs: [main.$GreeterProtocol<main.FakeRoundtripActorSystem>], arguments: [], returnType: Optional(Swift.String), errorType: nil), throwing:Swift.Never, returning:Swift.String
    // CHECK-NEXT: > execute distributed target: main.$GreeterProtocol.greet(), identifier: $s4main16$GreeterProtocolC5greetSSyYaKFTE
    // FIXME: was: > execute distributed target: main.$GreeterProtocol.greet(), identifier: $s4main16$GreeterProtocolC5greetSSyYaKAA0bC0RzlFTE
    // Notes:
    // - The call is made on the stub: $GreeterProtocol
    // - the record is name is 'HF' for the accessible function

    let got = try await gfr.greet()
    print("got: \(got)")
  }
}


