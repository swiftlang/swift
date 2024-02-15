// REQUIRES: swift_swift_parser, asserts
//
// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed
//
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t-scratch)

// RUN: not %target-swift-frontend -typecheck -verify -disable-availability-checking -plugin-path %swift-plugin-dir -I %t -dump-macro-expansions %s 2>&1 | %FileCheck %s

// FIXME: rdar://123012943 witnesses of get() and greet(name:) are not found
// After that, this test SHOULD fail, but only because of the notStubbed
// XFAIL: *

import Distributed

protocol EmptyBase {
  func notStubbed()
}

@_DistributedProtocol
protocol Fail: DistributedActor, EmptyBase where SerializationRequirement == any Codable {
  distributed func get() -> String
  distributed func greet(name: String) -> String
}

// CHECK: FAIL