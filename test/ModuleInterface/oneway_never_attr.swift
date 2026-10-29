// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -parse-as-library -language-mode 5 -enable-library-evolution -enable-experimental-feature SendCalls -module-name AwaitLib -emit-module-interface-path %t/AwaitLib.swiftinterface -emit-module-path %t/AwaitLib.swiftmodule %s
// RUN: %target-swift-frontend -typecheck-module-from-interface %t/AwaitLib.swiftinterface -module-name AwaitLib
// RUN: %FileCheck %s --input-file %t/AwaitLib.swiftinterface

// Both the module and the module interface keep '@oneway(never)', so a
// client still cannot call the functions with 'send'
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls -D CLIENT -I %t %s
// RUN: rm %t/AwaitLib.swiftmodule
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls -D CLIENT -I %t %s

// REQUIRES: concurrency
// REQUIRES: swift_feature_SendCalls

#if !CLIENT

// CHECK: public actor Worker {
public actor Worker {
  public init() {}

  // CHECK: #if compiler(>=5.3) && $SendCalls
  // CHECK-NEXT: @oneway(never) public func drain()
  // CHECK-NEXT: #endif
  @oneway(never) public func drain() {}
}

// CHECK: #if compiler(>=5.3) && $SendCalls
// CHECK-NEXT: {{^}}@{{.*}}MainActor @oneway(never) public func flush()
// CHECK-NEXT: #endif
@MainActor @oneway(never) public func flush() {}

// CHECK: @{{.*}}MainActor public protocol Drainable {
@MainActor public protocol Drainable {
  // CHECK: #if compiler(>=5.3) && $SendCalls
  // CHECK-NEXT: @oneway(never) @{{.*}}MainActor func drain()
  // CHECK-NEXT: #endif
  @oneway(never) func drain()
}

// CHECK: @{{.*}}MainActor open class Base {
@MainActor open class Base {
  public init() {}
  // CHECK: #if compiler(>=5.3) && $SendCalls
  // CHECK-NEXT: @oneway(never) @{{.*}}MainActor open func close()
  // CHECK-NEXT: #endif
  @oneway(never) open func close() {}
}

#else

import AwaitLib

class Derived: Base {
  override func close() {}
}

func client(_ worker: Worker, _ drainer: some Drainable, _ derived: Derived) {
  send flush() // expected-warning{{'@oneway(never)' global function 'flush()' is not intended to be called with 'send', use 'await' instead}}
  send worker.drain() // expected-warning{{'@oneway(never)' instance method 'drain()' is not intended to be called with 'send', use 'await' instead}}
  send drainer.drain() // expected-warning{{'@oneway(never)' instance method 'drain()' is not intended to be called with 'send', use 'await' instead}}
  send derived.close() // expected-warning{{'@oneway(never)' instance method 'close()' is not intended to be called with 'send', use 'await' instead}}
}

#endif
