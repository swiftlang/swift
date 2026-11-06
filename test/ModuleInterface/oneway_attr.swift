// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -parse-as-library -language-mode 5 -enable-library-evolution -enable-experimental-feature SendCalls -module-name OnewayLib -emit-module-interface-path %t/OnewayLib.swiftinterface -emit-module-path %t/OnewayLib.swiftmodule %s
// RUN: %target-swift-frontend -typecheck-module-from-interface %t/OnewayLib.swiftinterface -module-name OnewayLib
// RUN: %FileCheck %s --input-file %t/OnewayLib.swiftinterface

// Both the binary module and the module interface keep '@oneway', which is a
// declaration attribute, so a client still sees it
// RUN: %target-swift-frontend -typecheck -verify -disable-availability-checking -enable-experimental-feature SendCalls -D CLIENT -I %t %s
// RUN: rm %t/OnewayLib.swiftmodule
// RUN: %target-swift-frontend -typecheck -verify -disable-availability-checking -enable-experimental-feature SendCalls -D CLIENT -I %t %s

// REQUIRES: concurrency
// REQUIRES: swift_feature_SendCalls

#if !CLIENT

// CHECK: public actor Worker {
public actor Worker {
  public init() {}

  // CHECK: #if compiler(>=5.3) && $SendCalls
  // CHECK-NEXT: @oneway public func tick()
  // CHECK-NEXT: #endif
  @oneway public func tick() {}

  // CHECK: #if compiler(>=5.3) && $SendCalls
  // CHECK-NEXT: @oneway public func log(_ line: Swift::String) async
  // CHECK-NEXT: #endif
  @oneway public func log(_ line: String) async {}

  // An overload which is not '@oneway'
  // CHECK: public func tick() async
  public func tick() async {}
}

// CHECK: #if compiler(>=5.3) && $SendCalls
// CHECK-NEXT: @_Concurrency::MainActor @oneway public func ping()
// CHECK-NEXT: #endif
@MainActor @oneway public func ping() {}

#else

import OnewayLib

@MainActor
func client(_ worker: Worker) async {
  send worker.tick()
  send worker.log("line")
  send ping()
  await worker.tick()

  // A call from within the function's own isolation is an ordinary call
  ping()

  // A '@oneway' function is an ordinary function value
  let plain: @MainActor () -> Void = ping
  _ = plain
}

#endif
