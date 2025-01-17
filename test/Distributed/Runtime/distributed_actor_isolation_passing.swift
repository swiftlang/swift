// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.9-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -parse-as-library -swift-version 6 -target %target-swift-5.9-abi-triple -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift %S/../Inputs/CustomSerialExecutorAvailability.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

//import StdlibUnittest
import Distributed

@available(SwiftStdlib 5.9, *)
distributed actor Tester {

  typealias ActorSystem = LocalTestingDistributedActorSystem

  distributed func check() async {
    // should correctly infer not to be crossing isolation boundary:
    pass()
    passOptional()

    pass(isolatedToActor: self.asLocalActor)
    passOptional(isolatedToActor: self.asLocalActor)

    // Not supported: the parameter must be exactly the 'self.asLocalActor'
    // as otherwise we don't know where the value came from and if it's really
    // self we're calling from; We'd need more sophisticated analysis to make it work.
    //    let myself = self.asLocalActor
    //    pass(isolatedToActor: myself)
  }
}

@available(SwiftStdlib 5.9, *)
func passOptional(isolatedToActor: isolated (any Actor)? = #isolation) {
  isolatedToActor!.preconditionIsolated("Expected to be executing on actor \(isolatedToActor!)")
}

@available(SwiftStdlib 5.9, *)
func pass(isolatedToActor: isolated (any Actor) = #isolation) {
  isolatedToActor.preconditionIsolated("Expected to be executing on actor \(isolatedToActor)")
}

@main struct Main {
  static func main() async {
    if #available(SwiftStdlib 5.9, *) {
      let system = LocalTestingDistributedActorSystem()
      let tester = Tester(actorSystem: system)

      try! await tester.check()
    }
  }
}
