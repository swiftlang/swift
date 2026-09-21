// REQUIRES: swift_swift_parser, asserts
//
// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed
//
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// The @Resolvable macro must properly handle #if ... blocks.
//
// Feature is disabled:
// RUN: %target-swift-frontend -typecheck -target %target-swift-6.0-abi-triple -plugin-path %swift-plugin-dir -dump-macro-expansions %t/Protocol.swift %t/Server.swift 2>&1 | %FileCheck %s
//
// Feature OUTER is enabled:
// RUN: %target-swift-frontend -typecheck -target %target-swift-6.0-abi-triple -plugin-path %swift-plugin-dir -D OUTER %t/Protocol.swift %t/Server.swift
//
// Nested #if branches (OUTER, INNER):
// RUN: %target-swift-frontend -typecheck -target %target-swift-6.0-abi-triple -plugin-path %swift-plugin-dir -D OUTER %t/Protocol.swift %t/Server.swift
// RUN: %target-swift-frontend -typecheck -target %target-swift-6.0-abi-triple -plugin-path %swift-plugin-dir -D OUTER -D INNER %t/Protocol.swift %t/Server.swift

//--- Protocol.swift
import Distributed

@Resolvable
public protocol MyService: DistributedActor
  where ActorSystem: DistributedActorSystem<any Codable> {
  distributed func always() async -> String
#if OUTER
  distributed func outer() async -> String
#endif
}

// CHECK: extension MyService where Self: Distributed._DistributedActorStub {
// CHECK:   public distributed func always() async -> String {
// CHECK:   }
// CHECK:   #if OUTER
// CHECK:   public distributed func outer() async -> String {
// CHECK:   }
// CHECK:   #endif
// CHECK: }


@Resolvable
public protocol MyNestedService: DistributedActor
  where ActorSystem: DistributedActorSystem<any Codable> {
  distributed func always() async -> String
#if OUTER
  distributed func outer() async -> String
  #if INNER
  distributed func inner() async -> String
  #endif
#endif
}

// CHECK: extension MyNestedService where Self: Distributed._DistributedActorStub {
// CHECK:   public distributed func always() async -> String {
// CHECK:   }
// CHECK:   #if OUTER
// CHECK:   public distributed func outer() async -> String {
// CHECK:   }
// CHECK:   #if INNER
// CHECK:   public distributed func inner() async -> String {
// CHECK:   }
// CHECK:   #endif
// CHECK:   #endif
// CHECK: }
//--- Server.swift
import Distributed

public distributed actor MyServer: MyService {
  public typealias ActorSystem = LocalTestingDistributedActorSystem

  public distributed func always() async -> String { "always" }
#if OUTER
  public distributed func outer() async -> String { "outer" }
#endif
}

public distributed actor MyNestedServer: MyNestedService {
  public typealias ActorSystem = LocalTestingDistributedActorSystem

  public distributed func always() async -> String { "always" }
#if OUTER
  public distributed func outer() async -> String { "outer" }
  #if INNER
  public distributed func inner() async -> String { "inner" }
  #endif
#endif
}
