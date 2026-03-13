// REQUIRES: swift_swift_parser, asserts
//
// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed
//
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t-scratch)

// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: not %target-swift-frontend -typecheck -target %target-swift-5.7-abi-triple -plugin-path %swift-plugin-dir -parse-as-library -I %t %S/../Inputs/FakeDistributedActorSystems.swift -dump-macro-expansions %s 2>&1 | %FileCheck %s

import Distributed

// These tests check the error output inside of a "bad" expansion;
//
// Since we cannot nicely diagnose these problems from the macro itself,
// as it would need to inspect the type and its extensions and all involved
// protocols., as at least

// CHECK: macro expansion @Resolvable:1:[[COL:[0-9]+]]: error: distributed actor '$Fail' does not declare ActorSystem it can be used with
@Resolvable
protocol Fail: DistributedActor {
  distributed func method() -> String
}
