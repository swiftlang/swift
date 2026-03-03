// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -module-name no_to_arg_leaks -emit-irgen -target %target-swift-5.7-abi-triple -I %t 2>&1 %s | %IRGenFileCheck %s -check-prefix CHECK-%target-import-type

// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed

// REQUIRES: CPU=x86_64 || CPU=arm64

// UNSUPPORTED: OS=windows-msvc

import Distributed
import FakeDistributedActorSystems

@available(SwiftStdlib 5.5, *)
typealias DefaultDistributedActorSystem = FakeActorSystem

final class SomeClass<T>: Sendable, Codable {
  init() {}
}

struct S<T> : Codable {
  var data: SomeClass<T>
}


class Sentinel {
  let str: String = ""
}

struct InnerStruct1 {
  let sentinel: Sentinel
  let innerStruct2: InnerStruct2

  init() {
    self.sentinel = Sentinel()
    self.innerStruct2 = InnerStruct2()
  }
}

struct InnerStruct2 {
  let sentinel: Sentinel

  init() {
    self.sentinel = Sentinel()
  }
}

enum InnerEnum {
  case v1(String)
  case v2(InnerStruct1)
}

struct ArgumentType: Codable {
  let sentinel: Sentinel
  let value: Int
  let innerEnum: InnerEnum

  init() {
    self.sentinel = Sentinel()
    self.value = 100
    self.innerEnum = .v2(InnerStruct1())
  }

  init(from decoder: Decoder) throws {
    fatalError("Not implemented")
  }

  func encode(to encoder: Encoder) throws {}
}

distributed actor Greeter {
  // CHECK-LABEL: define linkonce_odr hidden swifttailcc void @"$s15no_to_arg_leaks7GreeterC5test1yyAA9SomeClassCyxGYaKlFTETF"
  // CHECK: call void {{.*}}(ptr noalias [[PARAM:%.*]], ptr %arg_type)
  // CHECK-NEXT: call swiftcc void @swift_task_dealloc(ptr [[PARAM]])
  distributed func test1<T>(_: SomeClass<T>) {
  }

  // CHECK-LABEL: define linkonce_odr hidden swifttailcc void @"$s15no_to_arg_leaks7GreeterC5test2yyAA1SVyxGYaKlFTETF"
  // CHECK: call void {{.*}}(ptr noalias [[PARAM:%.*]], ptr %arg_type)
  // CHECK-NEXT: call swiftcc void @swift_task_dealloc(ptr [[PARAM]])
  distributed func test2<T>(_: S<T>) {}

  // CHECK-LABEL: define linkonce_odr hidden swifttailcc void @"$s15no_to_arg_leaks7GreeterC5test3yyAA12ArgumentTypeVYaKFTETF"
  // CHECK: call void {{.*}}(ptr noalias [[PARAM:%.*]], ptr %arg_type)
  // CHECK-NEXT: call swiftcc void @swift_task_dealloc(ptr [[PARAM]])
  distributed func test3(_: ArgumentType) {}
}

func test() async throws {
  let system = DefaultDistributedActorSystem()

  let local = Greeter(actorSystem: system)
  let ref = try Greeter.resolve(id: local.id, using: system)

  try await ref.test1(SomeClass<Int>())
  try await ref.test2(S(data: SomeClass<Int>()))
  try await ref.test3(.init())
}
