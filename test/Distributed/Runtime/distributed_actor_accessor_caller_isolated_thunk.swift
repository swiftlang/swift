// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.0-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeNonsendingActorSystems.swiftmodule -module-name FakeNonsendingActorSystems -target %target-swift-6.0-abi-triple -I %t %S/../Inputs/FakeNonsendingActorSystems.swift
// RUN: %target-build-swift -module-name main -target %target-swift-6.0-abi-triple -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift %S/../Inputs/FakeNonsendingActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: OS=windows-msvc

// Exercise the `executeDistributedTarget` path when accessible func is nonisolated(nonsending):
// -- executeDistributedTarget  
// -> compiler-generated distributed accessor 
// -> nonisolated(nonsending) distributed thunk
// -> the actual `distributed func`

import Distributed
import FakeDistributedActorSystems
import FakeNonsendingActorSystems

// ==== ------------------------------------------------------------------------
// MARK: Actors
//
// Members are declared both in the actor body and in an extension: the thunk
// for a body member is synthesized at a different point in type checking than
// one for an extension member, and both must end up with the same ABI.

@available(SwiftStdlib 6.0, *)
distributed actor Hopper {
  typealias ActorSystem = FakeRoundtripActorSystem
  var count = 0

  distributed func hello(_ s: String) -> String {
    self.preconditionIsolated("not isolated to self")
    count += 1
    return "hopper-body-\(s)-\(count)"
  }
  distributed func helloVoid() {
    self.preconditionIsolated("not isolated to self")
    count += 1
  }
}

@available(SwiftStdlib 6.0, *)
extension Hopper {
  distributed func helloInExtension(_ s: String) -> String {
    self.preconditionIsolated("not isolated to self")
    return "hopper-ext-\(s)"
  }
}

@available(SwiftStdlib 6.0, *)
distributed actor LessHopper {
  typealias ActorSystem = FakeNonsendingRoundtripActorSystem
  var count = 0

  distributed func hello(_ s: String) -> String {
    self.preconditionIsolated("not isolated to self")
    count += 1
    return "lesshopper-body-\(s)-\(count)"
  }
  distributed func helloVoid() {
    self.preconditionIsolated("not isolated to self")
    count += 1
  }
  distributed func twoArgs(_ a: String, _ b: Int) -> String {
    self.preconditionIsolated("not isolated to self")
    return "lesshopper-two-\(a)-\(b)"
  }
  distributed var computed: String {
    self.preconditionIsolated("not isolated to self")
    return "lesshopper-computed"
  }
}

@available(SwiftStdlib 6.0, *)
extension LessHopper {
  distributed func helloInExtension(_ s: String) -> String {
    self.preconditionIsolated("not isolated to self")
    return "lesshopper-ext-\(s)"
  }
}

// ==== ------------------------------------------------------------------------

@available(SwiftStdlib 6.0, *)
@main struct Main {
  @MainActor
  static func main() async throws {
    // ---- hopping system: unchanged ABI ----------------------------------
    let hopSys = FakeRoundtripActorSystem()
    let hopLocal = Hopper(actorSystem: hopSys)
    let hop = try Hopper.resolve(id: hopLocal.id, using: hopSys)
    // CHECK: hopper-body-a-1
    print(try await hop.hello("a"))
    // CHECK: hopper-ext-b
    print(try await hop.helloInExtension("b"))
    try await hop.helloVoid()
    // CHECK: hop void ok
    print("hop void ok")

    // ---- non-sending system: caller-isolated thunk ABI ------------------
    let nsSys = FakeNonsendingRoundtripActorSystem()
    nsSys.onRemoteCall = { _ in
      // The sending side must not have hopped away from the caller
      MainActor.preconditionIsolated("remoteCall lost the caller's isolation")
    }
    let nsLocal = LessHopper(actorSystem: nsSys)
    let ns = try LessHopper.resolve(id: nsLocal.id, using: nsSys)

    // zero-argument, non-void: the implicit actor parameter must not be
    // decoded as if it were an argument
    // CHECK: lesshopper-body-a-1
    print(try await ns.hello("a"))

    // zero-argument, void
    try await ns.helloVoid()
    // CHECK: ns void ok
    print("ns void ok")

    // more than one real argument, to catch off-by-one argument shifting
    // CHECK: lesshopper-two-x-7
    print(try await ns.twoArgs("x", 7))

    // computed property accessor thunk
    // CHECK: lesshopper-computed
    print(try await ns.computed)

    // extension member: must have the same ABI as the body members
    // CHECK: lesshopper-ext-b
    print(try await ns.helloInExtension("b"))

    // CHECK: OK
    print("OK")
  }
}
