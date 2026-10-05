// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -module-name main -emit-ir -O -target %target-swift-5.7-abi-triple -I %t %s | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: CPU=x86_64 || CPU=arm64
// The CHECK lines match the Darwin ObjC-runtime metadata accessor (object_getClass);
// on non-ObjC targets the accessor loads the isa directly and -O erases the derivation
// REQUIRES: objc_interop
// UNSUPPORTED: OS=windows-msvc
// UNSUPPORTED: back_deploy_concurrency

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

protocol Greeter: DistributedActor where ActorSystem == FakeActorSystem {
  distributed func greet() -> String
}

distributed actor Impl: Greeter {
  distributed func greet() -> String { "hi" }
}

// The accessor for the bare protocol requirement `Greeter.greet`.
// CHECK-LABEL: define{{.*}}@"$s4main7GreeterP5greetSSyYaKFTETF"

// The actor's type metadata is derived from the actor instance via object_getClass.
// CHECK: [[META:%.*]] = {{(tail )?}}call ptr @object_getClass(ptr [[SELF:%[0-9]+]])

// The witness_method call must pass that metadata (and not just plain self) in the SelfMetadata slot:
// CHECK: musttail call swifttailcc void {{%[0-9]+}}(ptr {{(nonnull )?}}swiftasync {{%[0-9]+}}, ptr swiftself [[SELF]], ptr [[META]], ptr
