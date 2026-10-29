// RUN: %target-swift-frontend -typecheck -diagnostic-style llvm -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls %s 2>&1 | %FileCheck %s --check-prefix=CHECK-DEFAULT
// RUN: not %target-swift-frontend -typecheck -diagnostic-style llvm -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls -Werror AwaitedOnewayCall %s 2>&1 | %FileCheck %s --check-prefix=CHECK-WE-GROUP
// RUN: not %target-swift-frontend -typecheck -diagnostic-style llvm -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls -warnings-as-errors %s 2>&1 | %FileCheck %s --check-prefix=CHECK-WAE
// RUN: %target-swift-frontend -typecheck -diagnostic-style llvm -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls -warnings-as-errors -Wwarning AwaitedOnewayCall %s 2>&1 | %FileCheck %s --check-prefix=CHECK-WAE-WW-GROUP

// The warning prints its group, which links to its documentation
// RUN: %target-swift-frontend -typecheck -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls %s 2>&1 | %FileCheck %s --check-prefix=CHECK-GROUP

// REQUIRES: concurrency
// REQUIRES: swift_feature_SendCalls
// REQUIRES: swift_swift_parser

// An 'await' of a '@oneway' function from outside of its isolation
// domain is a warning in the 'AwaitedOnewayCall' group, which
// '-Werror AwaitedOnewayCall' turns into an error. A call from within the
// function's own isolation domain is an ordinary call, and is not diagnosed

actor Logger {
  @oneway func log(_ message: String) {}

  func plainCall() {
    log("plain")
  }

  func sendToSelf() {
    send log("send to self")
  }
}

func process(_ logger: Logger) async {
  send logger.log("send")

  // CHECK-DEFAULT: warning: '@oneway' instance method 'log' is not intended to be awaited on, use 'send' instead
  // CHECK-DEFAULT-NOT: error:
  // CHECK-WE-GROUP: error: '@oneway' instance method 'log' is not intended to be awaited on, use 'send' instead
  // CHECK-WE-GROUP-NOT: warning: call to '@oneway'
  // CHECK-WAE: error: '@oneway' instance method 'log' is not intended to be awaited on, use 'send' instead
  // CHECK-WAE-NOT: warning: call to '@oneway'
  // CHECK-WAE-WW-GROUP: warning: '@oneway' instance method 'log' is not intended to be awaited on, use 'send' instead
  // CHECK-WAE-WW-GROUP-NOT: error:
  // CHECK-GROUP: warning: '@oneway' instance method 'log' is not intended to be awaited on, use 'send' instead [#AwaitedOnewayCall]{{$}}
  await logger.log("await")
}

// CHECK-GROUP: [#AwaitedOnewayCall]: <{{.*}}awaited-oneway-call{{(\.md)?}}>
