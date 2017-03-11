// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen -verify %s | %FileCheck %s

func values(_ arg: @escaping @convention(c) (Int) -> Int) -> @convention(c) (Int) -> Int {
  return arg
}
// CHECK-LABEL: sil hidden @_T019c_function_pointers6valuesS2iXCS2iXCF
// CHECK:       bb0(%0 : $@convention(c) (Int) -> Int):
// CHECK:         return %0 : $@convention(c) (Int) -> Int

@discardableResult
func calls(_ arg: @convention(c) (Int) -> Int, _ x: Int) -> Int {
  return arg(x)
}
// CHECK-LABEL: sil hidden @_T019c_function_pointers5callsS3iXC_SitF
// CHECK:       bb0(%0 : $@convention(c) (Int) -> Int, %1 : $Int):
// CHECK:         [[RESULT:%.*]] = apply %0(%1)
// CHECK:         return [[RESULT]]

@discardableResult
func calls_no_args(_ arg: @convention(c) () -> Int) -> Int {
  return arg()
}

func global(_ x: Int) -> Int { return x }

func no_args() -> Int { return 42 }

// CHECK-LABEL: sil hidden @_T019c_function_pointers0B19_to_swift_functionsySiF
func pointers_to_swift_functions(_ x: Int) {
// CHECK: bb0([[X:%.*]] : $Int):

  func local(_ y: Int) -> Int { return y }

  // CHECK:   [[GLOBAL_C:%.*]] = function_ref @_T019c_function_pointers6globalS2iFTo
  // CHECK:   apply {{.*}}([[GLOBAL_C]], [[X]])
  calls(global, x)

  // CHECK:   [[LOCAL_C:%.*]] = function_ref @_T019c_function_pointers0B19_to_swift_functionsySiF5localL_S2iFTo
  // CHECK:   apply {{.*}}([[LOCAL_C]], [[X]])
  calls(local, x)

  // CHECK:   [[CLOSURE_C:%.*]] = function_ref @_T019c_function_pointers0B19_to_swift_functionsySiFS2icfU_To
  // CHECK:   apply {{.*}}([[CLOSURE_C]], [[X]])
  calls({ $0 + 1 }, x)

  calls_no_args(no_args)
  // CHECK:   [[NO_ARGS_C:%.*]] = function_ref @_T019c_function_pointers7no_argsSiyFTo
  // CHECK:   apply {{.*}}([[NO_ARGS_C]])
}

func unsupported(_ a: Any) -> Int { return 0 }

func pointers_to_bad_swift_functions(_ x: Int) {
  calls(unsupported, x) // expected-error{{C function pointer signature '(Any) -> Int' is not compatible with expected type '@convention(c) (Int) -> Int'}}
}

// CHECK-LABEL: sil shared @_T019c_function_pointers22StructWithInitializersV3fn1yyXCvfiyycfU_ : $@convention(thin) () -> () {
// CHECK-LABEL: sil shared [thunk] @_T019c_function_pointers22StructWithInitializersV3fn1yyXCvfiyycfU_To : $@convention(c) () -> () {

struct StructWithInitializers {
  let fn1: @convention(c) () -> () = {}

  init(a: ()) {}
  init(b: ()) {}
}
