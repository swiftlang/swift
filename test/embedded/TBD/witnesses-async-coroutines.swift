// A witness that a serialized witness thunk refers to by symbol is public,
// and so are the async and coroutine function pointers that refer to it.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-ir -o /dev/null %s -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -enable-experimental-feature CoroutineAccessors -emit-tbd-path %t/Lib.tbd -tbd-install_name Lib -validate-tbd-against-ir=all
// RUN: %FileCheck %s < %t/Lib.tbd
// RUN: %target-swift-frontend -emit-ir -o /dev/null %s -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -enable-experimental-feature CoroutineAccessors -validate-tbd-against-ir=all -O
// RUN: %target-swift-frontend -emit-ir -o /dev/null %s -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -enable-experimental-feature CoroutineAccessors -validate-tbd-against-ir=all -disable-callee-allocated-coro-abi

// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_CoroutineAccessors
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=wasip1

import _Concurrency

public protocol AsyncRequirement {
  func requirement() async -> Int
}

// CHECK-DAG: "_$e3Lib13InternalAsyncV11requirementSiyYaF"
// CHECK-DAG: "_$e3Lib13InternalAsyncV11requirementSiyYaFTu"
struct InternalAsync: AsyncRequirement {
  func requirement() async -> Int { 7 }
}

@inline(never)
func makeInternalAsync() -> any AsyncRequirement { InternalAsync() }

public func useAsync() async -> Int { await makeInternalAsync().requirement() }

public protocol YieldingRequirement {
  var value: Int { yielding borrow yielding mutate }
}

// CHECK-DAG: "_$e3Lib16InternalYieldingV5valueSivy"
// CHECK-DAG: "_$e3Lib16InternalYieldingV5valueSivyTwc"
// CHECK-DAG: "_$e3Lib16InternalYieldingV5valueSivx"
// CHECK-DAG: "_$e3Lib16InternalYieldingV5valueSivxTwc"
struct InternalYielding: YieldingRequirement {
  var _value = 0
  var value: Int {
    yielding borrow { yield _value }
    yielding mutate { yield &_value }
  }
}

@inline(never)
func makeInternalYielding() -> any YieldingRequirement { InternalYielding() }

public func useYielding() -> Int { makeInternalYielding().value }
