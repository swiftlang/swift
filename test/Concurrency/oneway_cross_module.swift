// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// The library, as a binary module and as a module interface
// RUN: %target-swift-frontend -emit-module -parse-as-library -language-mode 6 -enable-library-evolution -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls -module-name Lib -emit-module-path %t/binary/Lib.swiftmodule -emit-module-interface-path %t/textual/Lib.swiftinterface %t/Lib.swift
// RUN: %target-swift-typecheck-module-from-interface(%t/textual/Lib.swiftinterface) -module-name Lib -enable-experimental-feature SendCalls

// The client, against the binary module and against a module built from the
// module interface
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls -I %t/binary %t/Client.swift
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls -I %t/textual -module-cache-path %t/mcp %t/Client.swift
// RUN: %target-swift-frontend -emit-silgen -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls -D NO_ERRORS -I %t/binary -module-name Client %t/Client.swift | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls -D NO_ERRORS -I %t/textual -module-cache-path %t/mcp -module-name Client %t/Client.swift | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: swift_feature_SendCalls

// '@oneway' and '@oneway(never)' are declaration attributes, which both a
// binary module and a module interface keep, so that a client gets the same
// diagnostics and overload resolution as in the library's own module

//--- Lib.swift

public struct Summary: Sendable {
  public init() {}
}

public actor Worker {
  public init() {}
  @oneway public func tell() {}
  @oneway(never) public func avoid() {}
}

// The proposal's example: overloads which differ in more than '@oneway'
public actor Queue {
  public init() {}
  public func stop() async -> Summary { Summary() }
  @oneway public func stop() {}
}

public protocol Tellable: Actor {
  @oneway func ping()
  func pong()
}

public actor Examples {
  public init() {}
  @oneway public func example(a: Int = 32) {}
  public func example() {}
}

@MainActor @oneway public func example(a: Int = 32) {}
@MainActor public func example() {}

//--- Client.swift

import Lib

#if !NO_ERRORS
func warnings(_ worker: Worker) async {
  // expected-warning@+1:16{{'@oneway' instance method 'tell()' is not intended to be awaited on, use 'send' instead}}
  await worker.tell()
  // expected-warning@+1:15{{'@oneway(never)' instance method 'avoid()' is not intended to be called with 'send', use 'await' instead}}
  send worker.avoid()
}

// A '@oneway' requirement may be witnessed by a plain method
actor PlainWitness: Tellable {
  func ping() {}
  func pong() {}
}

// A requirement which is not '@oneway' cannot be witnessed by a '@oneway'
// method
// expected-error@+2:7{{type 'OnewayWitness' does not conform to protocol 'Tellable'}}
// expected-note@+1:7{{add stubs for conformance}}
actor OnewayWitness: Tellable {
  func ping() {}
  @oneway func pong() {} // expected-note@:16{{candidate is '@oneway', but protocol requirement is not}}
}

// Functions which only differ in '@oneway' redeclare each other
@MainActor func redeclared() {} // expected-note@:17{{'redeclared()' previously declared here}}
@MainActor @oneway func redeclared() {}
// expected-error@-1:25{{invalid redeclaration of 'redeclared()'}}
#endif

func stops(_ q: Queue) async {
  // 'send' picks the '@oneway' 'stop()', 'await' the one returning 'Summary'
  send q.stop()
  let summary: Summary = await q.stop()
  _ = summary
}

func examples(_ e: Examples) async {
  send e.example()
  await e.example()
  send e.example(a: 1)
}

@MainActor func mainActorExamples() {
  send example()
  example()
  send example(a: 1)
}

func nonisolatedExamples() async {
  await example()
}

// CHECK-LABEL: sil hidden [ossa] @$s6Client5stopsyy3Lib5QueueCYaF :
// CHECK: #Queue.stop : (isolated Queue) -> () async -> Summary
// CHECK-LABEL: sil private [ossa] @$s6Client5stopsyy3Lib5QueueCYaF{{.*}}fU_ :
// CHECK: #Queue.stop : (isolated Queue) -> () -> ()

// CHECK-LABEL: sil hidden [ossa] @$s6Client8examplesyy3Lib8ExamplesCYaF :
// CHECK: function_ref @$s3Lib8ExamplesC7example1aySi_tFfA_ :
// CHECK: #Examples.example : (isolated Examples) -> () -> ()
// CHECK-LABEL: sil private [ossa] @$s6Client8examplesyy3Lib8ExamplesCYaF{{.*}}fU_ :
// CHECK: #Examples.example : (isolated Examples) -> (Int) -> ()
// CHECK-LABEL: sil private [ossa] @$s6Client8examplesyy3Lib8ExamplesCYaF{{.*}}fU0_ :
// CHECK: #Examples.example : (isolated Examples) -> (Int) -> ()

// CHECK-LABEL: sil hidden [ossa] @$s6Client17mainActorExamplesyyF :
// CHECK: function_ref @$s3Lib7example1aySi_tFfA_ :
// CHECK: function_ref @$s3Lib7exampleyyF :
// CHECK-LABEL: sil private [ossa] @$s6Client17mainActorExamplesyyF{{.*}}fU_ :
// CHECK: function_ref @$s3Lib7example1aySi_tF :
// CHECK-LABEL: sil private [ossa] @$s6Client17mainActorExamplesyyF{{.*}}fU0_ :
// CHECK: function_ref @$s3Lib7example1aySi_tF :

// CHECK-LABEL: sil hidden [ossa] @$s6Client19nonisolatedExamplesyyYaF :
// CHECK: function_ref @$s3Lib7exampleyyF :
