// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature Embedded -enable-experimental-feature SendCalls -parse-as-library -wmo -module-name main -target %target-cpu-apple-macos14 %s %S/Runtime/Inputs/EmbeddedFakeActorSystem.swift -o %t/out.sil
// RUN: %FileCheck %s < %t/out.sil

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_SendCalls
// REQUIRES: optimized_stdlib

// A '@oneway' requirement that a plain function witnesses is witnessed by the
// synchronous 'oneway witness adapter' of the function, which makes remote
// calls with the identifier of the thunk of the function itself

import _Concurrency
import Distributed

typealias DefaultDistributedActorSystem = EmbeddedFakeRoundtripActorSystem

protocol Greetable: DistributedActor where ActorSystem == EmbeddedFakeRoundtripActorSystem {
  @oneway distributed func greet(_ n: Int)
}

distributed actor Greeter: Greetable {
  distributed func greet(_ n: Int) {
    _ = n
  }
}

func sendIt(_ g: any Greetable) throws {
  try send g.greet(1)
}

@main struct Main {
  static func main() async {
    let system = EmbeddedFakeRoundtripActorSystem()
    let greeter = Greeter(actorSystem: system)
    try? sendIt(greeter)
  }
}

// CHECK-LABEL: sil {{.*}}@$e4main7GreeterC20$onewayWitness$greetyySiKFTE : $@convention(method) ({{.*}}) -> @error any Error {
// CHECK: string_literal utf8 "$e4main7GreeterC5greetyySiYaKFTE"
// CHECK: remoteCallOneway
// CHECK: end sil function '$e4main7GreeterC20$onewayWitness$greetyySiKFTE'
