// RUN: %empty-directory(%t)
// REQUIRES: concurrency

import MeowActor

@available(SwiftStdlib 5.5, *)
@MeowActor func doMeow() {}

// RUN: %target-swift-frontend  -disable-availability-checking -enable-library-evolution -emit-module -o %t/MeowActor.swiftmodule %S/Inputs/MeowActor.swift
// RUN: %target-swift-frontend  -disable-availability-checking -emit-silgen %s -I %t | %FileCheck --check-prefix CHECK-RESILIENT %s
// CHECK-RESILIENT: metatype $@thick MeowActor.Type

// RUN: %target-swift-frontend  -disable-availability-checking -emit-module -o %t/MeowActor.swiftmodule %S/Inputs/MeowActor.swift
// RUN: %target-swift-frontend  -disable-availability-checking -emit-silgen %s -I %t | %FileCheck --check-prefix CHECK-FRAGILE %s
// CHECK-FRAGILE: metatype $@thin MeowActor.Type

@available(SwiftStdlib 5.5, *)
func someFunc() async {
  await doMeow()
}