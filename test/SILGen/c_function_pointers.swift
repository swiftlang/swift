// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -verify %s | %FileCheck %s

if true {
  var x = 0 // expected-warning {{variable 'x' was never mutated; consider changing to 'let' constant}}
  func local() -> Int { return 0 }
  func localWithContext() -> Int { return x }
  func transitiveWithoutContext() -> Int { return local() }

  // Can't convert a closure with context to a C function pointer
  let _: @convention(c) () -> Int = { x } // expected-error{{cannot be formed from a closure that captures context}}
  let _: @convention(c) () -> Int = { [x] in x } // expected-error{{cannot be formed from a closure that captures context}}
  let _: @convention(c) () -> Int = localWithContext // expected-error{{cannot be formed from a local function that captures context}}

  let _: @convention(c) () -> Int = local
  let _: @convention(c) () -> Int = transitiveWithoutContext
}

class C {
  static func staticMethod() -> Int { return 0 }
  class func classMethod() -> Int { return 0 }
}

class Generic<X : C> {
  func f<Y : C>(_ y: Y) {
    let _: @convention(c) () -> Int = { return 0 }
    let _: @convention(c) () -> Int = { return X.staticMethod() } // expected-error{{cannot be formed from a closure that captures generic parameters}}
    let _: @convention(c) () -> Int = { return Y.staticMethod() } // expected-error{{cannot be formed from a closure that captures generic parameters}}
  }
}

func values(_ arg: @escaping @convention(c) (Int) -> Int) -> @convention(c) (Int) -> Int {
  return arg
}
// CHECK-LABEL: sil hidden [ossa] @$s19c_function_pointers6valuesyS2iXCS2iXCF
// CHECK:       bb0(%0 : $@convention(c) (Int) -> Int):
// CHECK:         return %0 : $@convention(c) (Int) -> Int

@discardableResult
func calls(_ arg: @convention(c) (Int) -> Int, _ x: Int) -> Int {
  return arg(x)
}
// CHECK-LABEL: sil hidden [ossa] @$s19c_function_pointers5callsyS3iXC_SitF
// CHECK:       bb0(%0 : $@convention(c) (Int) -> Int, %1 : $Int):
// CHECK:         [[RESULT:%.*]] = apply %0(%1)
// CHECK:         return [[RESULT]]

@discardableResult
func calls_no_args(_ arg: @convention(c) () -> Int) -> Int {
  return arg()
}

func global(_ x: Int) -> Int { return x }

func no_args() -> Int { return 42 }

// CHECK-LABEL: sil hidden [ossa] @$s19c_function_pointers0B19_to_swift_functionsyySiF
func pointers_to_swift_functions(_ x: Int) {
// CHECK: bb0([[X:%.*]] : $Int):

  func local(_ y: Int) -> Int { return y }

  // CHECK:   [[GLOBAL_C:%.*]] = function_ref @$s19c_function_pointers6globalyS2iFTo
  // CHECK:   apply {{.*}}([[GLOBAL_C]], [[X]])
  calls(global, x)

  // CHECK:   [[LOCAL_C:%.*]] = function_ref @$s19c_function_pointers0B19_to_swift_functionsyySiF5localL_yS2iFTo
  // CHECK:   apply {{.*}}([[LOCAL_C]], [[X]])
  calls(local, x)

  // CHECK:   [[CLOSURE_C:%.*]] = function_ref @$s19c_function_pointers0B19_to_swift_functionsyySiFS2icfU_To
  // CHECK:   apply {{.*}}([[CLOSURE_C]], [[X]])
  calls({ $0 + 1 }, x)

  calls_no_args(no_args)
  // CHECK:   [[NO_ARGS_C:%.*]] = function_ref @$s19c_function_pointers7no_argsSiyFTo
  // CHECK:   apply {{.*}}([[NO_ARGS_C]])
}

func unsupported(_ a: Any) -> Int { return 0 }

func pointers_to_bad_swift_functions(_ x: Int) {
  calls(unsupported, x) // expected-error{{C function pointer signature '(Any) -> Int' is not compatible with expected type '@convention(c) (Int) -> Int'}}
}

// CHECK-LABEL: sil private [ossa] @$s19c_function_pointers22StructWithInitializersV3fn1yyXCvpfiyycfU_ : $@convention(thin) () -> () {
// CHECK-LABEL: sil private [thunk] [ossa] @$s19c_function_pointers22StructWithInitializersV3fn1yyXCvpfiyycfU_To : $@convention(c) () -> () {

struct StructWithInitializers {
  let fn1: @convention(c) () -> () = {}

  init(a: ()) {}
  init(b: ()) {}
}

func pointers_to_nested_local_functions_in_generics<T>(x: T) -> Int{
  func foo(y: Int) -> Int { return y }

  return calls(foo, 0)
}

func capture_list_no_captures(x: Int) {
  calls({ [x] in $0 }, 0) // expected-warning {{capture 'x' was never used}}
}

class Selfless {
  func capture_dynamic_self() {
    calls_no_args { _ = Self.self; return 0 }
    // expected-error@-1 {{a C function pointer cannot be formed from a closure that captures dynamic Self type}}
  }
}
