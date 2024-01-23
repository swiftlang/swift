// REQUIRES: swift_swift_parser, asserts
//
// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed
//
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t-scratch)

// RUN: not %target-swift-frontend -typecheck -verify -disable-availability-checking -plugin-path %swift-plugin-dir -I %t -dump-macro-expansions %s 2>&1 | %FileCheck %s

import Distributed

protocol EmptyBase {}

// CHECK: error: unexpected note produced: in expansion of macro 'DistributedProtocol' on protocol 'Fail' here
@DistributedProtocol // TODO: attach automatically
protocol Fail: DistributedActor, EmptyBase where SerializationRequirement == any Codable {
  distributed func get() -> String
  distributed func greet(name: String) -> String
}

// CHECK: error: diagnostic produced elsewhere: no macro named '_distributed_stubs_EmptyBase'
