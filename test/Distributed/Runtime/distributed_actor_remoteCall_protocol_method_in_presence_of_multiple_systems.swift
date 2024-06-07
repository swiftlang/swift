// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift -plugin-path %swift-plugin-dir
// RUN: %target-build-swift -module-name main  -Xfrontend -disable-availability-checking -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -plugin-path %swift-plugin-dir -o %t/a.out
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

// ==== ------------------------------------------------------------------------

distributed actor DAFR: GreeterProtocol {
  typealias ActorSystem = FakeRoundtripActorSystem
  distributed func greet() -> String { "\(Self.self)" }
}

distributed actor DAFL: GreeterProtocol {
  typealias ActorSystem = LocalTestingDistributedActorSystem
  distributed func greet() -> String { "\(Self.self)" }
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

    // CHECK: > execute distributed target: main.$GreeterProtocol.greet(), identifier: $s4main16$GreeterProtocolC5greetSSyYaKFTE
    // Notes:
    // - The call is made on the stub: $GreeterProtocol
    // - the record is name is 'HF' for the accessible function

    let got = try await gfr.greet()
    print("got: \(got)")

    print("ok") // CHECK: ok
  }
}


