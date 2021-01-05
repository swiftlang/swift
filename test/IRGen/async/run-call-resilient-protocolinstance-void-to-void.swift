// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(PrintShims)) %S/../../Inputs/print-shims.swift -module-name PrintShims -emit-module -emit-module-path %t/PrintShims.swiftmodule
// RUN: %target-codesign %t/%target-library-name(PrintShims)
// RUN: %target-build-swift-dylib(%t/%target-library-name(ResilientProtocol)) %S/Inputs/protocol-1instance-void_to_void.swift -Xfrontend -enable-experimental-concurrency -module-name ResilientProtocol -emit-module -emit-module-path %t/ResilientProtocol.swiftmodule
// RUN: %target-codesign %t/%target-library-name(ResilientProtocol)
// RUN: %target-build-swift -Xfrontend -enable-experimental-concurrency %s -emit-ir -I %t -L %t -lPrintShim -lResilientProtocol | %FileCheck %s --check-prefix=CHECK-LL
// RUN: %target-build-swift -Xfrontend -enable-experimental-concurrency %s -module-name main -o %t/main -I %t -L %t -lPrintShims -lResilientProtocol %target-rpath(%t) 
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main %t/%target-library-name(PrintShims) %t/%target-library-name(ResilientProtocol) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize_none
// REQUIRES: concurrency
// UNSUPPORTED: use_os_stdlib

import _Concurrency
import ResilientProtocol

func call<T : Protokol>(_ t: T) async {
  await t.protocolinstanceVoidToVoid()
}

// CHECK-LL: define hidden swiftcc void @"$s4main4callyyxY17ResilientProtocol8ProtokolRzlF"(%swift.task* {{%[0-9]+}}, %swift.executor* {{%[0-9]+}}, %swift.context* {{%[0-9]+}}) {{#[0-9]*}} {
func test_case() async {
  let impl = Impl()
  await call(impl) // CHECK: Impl()
}

_Concurrency.runAsyncAndBlock(test_case)
