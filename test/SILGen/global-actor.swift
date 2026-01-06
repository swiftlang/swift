// RUN: %empty-directory(%t)
// REQUIRES: concurrency

import MeowActor

@available(SwiftStdlib 5.1, *)
@MeowActor func doMeow() {}

@available(SwiftStdlib 5.1, *)
@HissActor func doHiss() {}

// RUN: %target-swift-frontend  -target %target-swift-5.1-abi-triple -enable-library-evolution -emit-module -o %t/MeowActor.swiftmodule %S/Inputs/MeowActor.swift
// RUN: %target-swift-frontend  -target %target-swift-5.1-abi-triple -emit-silgen %s -I %t | %FileCheck --check-prefix CHECK-RESILIENT %s
// CHECK-RESILIENT: metatype $@thick MeowActor.Type
// CHECK-RESILIENT: metatype $@thick HissActor.Type

// RUN: %target-swift-frontend  -target %target-swift-5.1-abi-triple -emit-module -o %t/MeowActor.swiftmodule %S/Inputs/MeowActor.swift
// RUN: %target-swift-frontend  -target %target-swift-5.1-abi-triple -emit-silgen %s -I %t | %FileCheck --check-prefix CHECK-FRAGILE %s
// CHECK-FRAGILE: metatype $@thin MeowActor.Type
// CHECK-FRAGILE: metatype $@thin HissActor.Type

@available(SwiftStdlib 5.1, *)
func someFunc() async {
  await doMeow()
  await doHiss()
}
