// RUN: %target-swift-frontend -emit-silgen -enable-c-function-pointers %s | FileCheck %s

func values(arg: @convention(c) Int -> Int) -> @convention(c) Int -> Int {
  return arg
}
// CHECK-LABEL: sil hidden @_TF19c_function_pointers6valuesFcSiSicSiSi
// CHECK:       bb0(%0 : $@convention(c) (Int) -> Int):
// CHECK:         return %0 : $@convention(c) (Int) -> Int

func calls(arg: @convention(c) Int -> Int, x: Int) -> Int {
  return arg(x)
}
// CHECK-LABEL: sil hidden @_TF19c_function_pointers5callsFTcSiSiSi_Si
// CHECK:       bb0(%0 : $@convention(c) (Int) -> Int, %1 : $Int):
// CHECK:         [[RESULT:%.*]] = apply %0(%1)
// CHECK:         return [[RESULT]]

func calls_no_args(arg: @convention(c) () -> Int) -> Int {
  return arg()
}

func global(x: Int) -> Int { return x }

func no_args() -> Int { return 42 }

// CHECK-LABEL: sil hidden @_TF19c_function_pointers27pointers_to_swift_functionsFSiT_
func pointers_to_swift_functions(x: Int) {
// CHECK: bb0([[X:%.*]] : $Int):

  func local(y: Int) -> Int { return y }

  // CHECK:   [[GLOBAL_C:%.*]] = function_ref @_TToF19c_function_pointers6globalFSiSi
  // CHECK:   apply {{.*}}([[GLOBAL_C]], [[X]])
  calls(global, x)

  // CHECK:   [[LOCAL_C:%.*]] = function_ref @_TToFF19c_function_pointers27pointers_to_swift_functionsFSiT_L_5localFSiSi
  // CHECK:   apply {{.*}}([[LOCAL_C]], [[X]])
  calls(local, x)

  // CHECK:   [[CLOSURE_C:%.*]] = function_ref @_TToFF19c_function_pointers27pointers_to_swift_functionsFSiT_U_FSiSi
  // CHECK:   apply {{.*}}([[CLOSURE_C]], [[X]])
  calls({ $0 + 1 }, x)

  calls_no_args(no_args)
  // CHECK:   [[NO_ARGS_C:%.*]] = function_ref @_TToF19c_function_pointers7no_argsFT_Si
  // CHECK:   apply {{.*}}([[NO_ARGS_C]])
}

