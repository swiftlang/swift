// RUN: %empty-directory(%t)
// REQUIRES: concurrency

import MeowActor

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@MeowActor func doMeow() {}

// RUN: %target-swift-frontend -enable-experimental-concurrency -enable-library-evolution -emit-module -o %t/MeowActor.swiftmodule %S/Inputs/MeowActor.swift
// RUN: %target-swift-frontend -enable-experimental-concurrency -emit-silgen %s -I %t | %FileCheck --check-prefix CHECK-RESILIENT %s
// CHECK-RESILIENT: metatype $@thick MeowActor.Type

// RUN: %target-swift-frontend -enable-experimental-concurrency -emit-module -o %t/MeowActor.swiftmodule %S/Inputs/MeowActor.swift
// RUN: %target-swift-frontend -enable-experimental-concurrency -emit-silgen %s -I %t | %FileCheck --check-prefix CHECK-FRAGILE %s
// CHECK-FRAGILE: metatype $@thin MeowActor.Type

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func someFunc() async {
  await doMeow()
}