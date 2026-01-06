// RUN: %target-swift-emit-silgen -enable-experimental-feature Extern %s | %FileCheck %s

// REQUIRES: swift_feature_Extern

// CHECK-DAG: sil hidden_external [asmname "my_c_name"] @$s8extern_c9withCNameyS2iFTo : $@convention(c) (Int) -> Int
@_extern(c, "my_c_name")
func withCName(_ x: Int) -> Int

// CHECK-DAG: sil hidden_external [asmname "take_c_func_ptr"] @$s8extern_c12takeCFuncPtryyS2iXCFTo : $@convention(c) (@convention(c) (Int) -> Int) -> ()
@_extern(c, "take_c_func_ptr")
func takeCFuncPtr(_ f: @convention(c) (Int) -> Int)

// CHECK-DAG: sil [asmname "public_visible"] @$s8extern_c16publicVisibilityyS2iFTo : $@convention(c) (Int) -> Int
@_extern(c, "public_visible")
public func publicVisibility(_ x: Int) -> Int

// CHECK-DAG: sil [asmname "private_visible"] @$s8extern_c17privateVisibility{{.*}} : $@convention(c) (Int) -> Int
@_extern(c, "private_visible")
private func privateVisibility(_ x: Int) -> Int

// CHECK-DAG: sil hidden_external [asmname "withoutCName"] @$s8extern_c12withoutCNameSiyFTo : $@convention(c) () -> Int
@_extern(c)
func withoutCName() -> Int

// CHECK-DAG: sil hidden [ossa] @$s8extern_c10defaultArgyySiFfA_ : $@convention(thin) () -> Int {
// CHECK-DAG: sil hidden_external [asmname "default_arg"] @$s8extern_c10defaultArgyySiFTo : $@convention(c) (Int) -> ()
@_extern(c, "default_arg")
func defaultArg(_ x: Int = 42)

func callMe(body: (Int) -> Int) -> Int {
  return body(17)
}

func main() {
  // CHECK-DAG: [[F1:%.+]] = function_ref @$s8extern_c9withCNameyS2iFTo : $@convention(c) (Int) -> Int
  // CHECK-DAG: [[F2:%.+]] = function_ref @$s8extern_c12takeCFuncPtryyS2iXCFTo : $@convention(c) (@convention(c) (Int) -> Int) -> ()
  // CHECK-DAG: apply [[F2]]([[F1]]) : $@convention(c) (@convention(c) (Int) -> Int) -> ()
  takeCFuncPtr(withCName)
  // CHECK-DAG: [[F3:%.+]] = function_ref @$s8extern_c16publicVisibilityyS2iFTo : $@convention(c) (Int) -> Int
  // CHECK-DAG: apply [[F3]]({{.*}}) : $@convention(c) (Int) -> Int
  _ = publicVisibility(42)
  // CHECK-DAG: [[F4:%.+]] = function_ref @$s8extern_c17privateVisibility{{.*}} : $@convention(c) (Int) -> Int
  // CHECK-DAG: apply [[F4]]({{.*}}) : $@convention(c) (Int) -> Int
  _ = privateVisibility(24)
  // CHECK-DAG: [[F5:%.+]] = function_ref @$s8extern_c12withoutCNameSiyFTo : $@convention(c) () -> Int
  // CHECK-DAG: apply [[F5]]() : $@convention(c) () -> Int
  _ = withoutCName()
  // CHECK-DAG: [[F6:%.+]] = function_ref @$s8extern_c10defaultArgyySiFfA_ : $@convention(thin) () -> Int
  // CHECK-DAG: [[DEFAULT_V:%.+]] = apply [[F6]]() : $@convention(thin) () -> Int
  // CHECK-DAG: [[F7:%.+]] = function_ref @$s8extern_c10defaultArgyySiFTo : $@convention(c) (Int) -> ()
  // CHECK-DAG: apply [[F7]]([[DEFAULT_V]]) : $@convention(c) (Int) -> ()
  defaultArg()

  // CHECK-DAG: [[CREF:%[0-9]+]] = function_ref @$s8extern_c16publicVisibilityyS2iFTO : $@convention(thin) (Int) -> Int
  // CHECK-DAG: [[THICK_CREF:%[0-9]+]] = thin_to_thick_function [[CREF]] to $@callee_guaranteed (Int) -> Int
  // CHECK-DAG: [[NOESCAPE_CREF:%[0-9]+]] = convert_escape_to_noescape [not_guaranteed] [[THICK_CREF]] to $@noescape @callee_guaranteed (Int) -> Int
  // CHECK-DAG: [[CALL_ME:%[0-9]+]] = function_ref @$s8extern_c6callMe4bodyS3iXE_tF
  // CHECK-DAG: apply [[CALL_ME]]([[NOESCAPE_CREF]]) : $@convention(thin) (@guaranteed @noescape @callee_guaranteed (Int) -> Int) -> Int
  _ = callMe(body: publicVisibility)
}

main()
