// RUN: %swift -triple x86_64-apple-darwin10 -emit-llvm %s | FileCheck %s

func arch<F>(f: F) {}

// CHECK: define void @_T17function_metadata9test_archFT_T_()
func test_arch() {
  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata
  arch( func()->(){} )
}
