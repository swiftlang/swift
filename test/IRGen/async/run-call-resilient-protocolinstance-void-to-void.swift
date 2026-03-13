// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(PrintShims)) %S/../../Inputs/print-shims.swift -module-name PrintShims -emit-module -emit-module-path %t/PrintShims.swiftmodule
// RUN: %target-codesign %t/%target-library-name(PrintShims)
// RUN: %target-build-swift-dylib(%t/%target-library-name(ResilientProtocol)) %S/Inputs/protocol-1instance-void_to_void.swift  -target %target-swift-5.1-abi-triple -g -module-name ResilientProtocol -emit-module -emit-module-path %t/ResilientProtocol.swiftmodule
// RUN: %target-codesign %t/%target-library-name(ResilientProtocol)
// RUN: %target-build-swift  -target %target-swift-5.1-abi-triple -g %s -emit-ir -I %t -L %t -lPrintShim -lResilientProtocol -parse-as-library -module-name main | %FileCheck %s --check-prefix=CHECK-LL
// RUN: %target-build-swift  -target %target-swift-5.1-abi-triple -g %s -parse-as-library -module-name main -o %t/main -I %t -L %t -lPrintShims -lResilientProtocol %target-rpath(%t) 
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main %t/%target-library-name(PrintShims) %t/%target-library-name(ResilientProtocol) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize_none
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import _Concurrency
import ResilientProtocol

func call<T : Protokol>(_ t: T) async {
  await t.protocolinstanceVoidToVoid()
}

// CHECK-LL: define hidden swift{{(tail)?}}cc void @"$s4main4callyyxYa17ResilientProtocol8ProtokolRzlF"(
func test_case() async {
  let impl = Impl()
  await call(impl) // CHECK: Impl()
}

@main struct Main {
  static func main() async {
    try await test_case()
  }
}
