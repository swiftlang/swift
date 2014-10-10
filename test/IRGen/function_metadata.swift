// RUN: %swift -target x86_64-apple-macosx10.9 -emit-ir -primary-file %s | FileCheck %s

func arch<F>(f: F) {}

// CHECK: define hidden void @_TF17function_metadata9test_archFT_T_()
func test_arch() {
  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata
  arch( {() -> () in } )
}
