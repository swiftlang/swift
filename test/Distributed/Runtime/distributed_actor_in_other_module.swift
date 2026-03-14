// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/EchoActorModule.swiftmodule -module-name EchoActorModule -target %target-swift-5.7-abi-triple %S/../Inputs/EchoActor.swift
// RUN: %target-build-swift -module-name main -Xfrontend -enable-experimental-distributed -target %target-swift-5.7-abi-triple -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift %S/../Inputs/EchoActor.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// FIXME(distributed): Distributed actors currently have some issues on windows, isRemote always returns false. rdar://82593574
// UNSUPPORTED: OS=windows-msvc

import Distributed
import EchoActorModule
import FakeDistributedActorSystems

func test() async {
  let system = LocalTestingDistributedActorSystem()

  let echo = Echo(actorSystem: system)
  let reply = try! await echo.echo("in the mirror")
  // CHECK: reply: echo: in the mirror
  print("reply: \(reply)")
}

@main struct Main {
  static func main() async {
    await test()
  }
}
