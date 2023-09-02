// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/StrictModule.swiftmodule -module-name StrictModule -warn-concurrency %S/Inputs/StrictModule.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/NonStrictModule.swiftmodule -module-name NonStrictModule %S/Inputs/NonStrictModule.swift
// RUN: %target-swift-frontend -disable-availability-checking -I %t -verify -emit-sil %s -o /dev/null
// RUN: %target-swift-frontend -disable-availability-checking -I %t -verify -emit-sil %s -o /dev/null -strict-concurrency=targeted
// RUN: %target-swift-frontend -disable-availability-checking -I %t -verify -emit-sil %s -o /dev/null -strict-concurrency=complete
// RUN: %target-swift-frontend -disable-availability-checking -I %t -verify -emit-sil %s -o /dev/null -strict-concurrency=complete -enable-experimental-feature SendNonSendable

// REQUIRES: concurrency
// REQUIRES: asserts

@preconcurrency import NonStrictModule

struct MyType {
  var nsc: NonStrictClass
}

func test(mt: MyType, nsc: NonStrictClass) {
  Task {
    print(mt)
  }
}
