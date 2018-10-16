// RUN: %target-swift-emit-silgen -parse-as-library -enable-sil-ownership %s | %FileCheck %s

// CHECK: sil private @globalinit_[[T:.*]]_func0 : $@convention(c) () -> () {
// CHECK:   alloc_global @$s12lazy_globals1xSiv
// CHECK:   [[XADDR:%.*]] = global_addr @$s12lazy_globals1xSivp : $*Int
// CHECK:   store {{%.*}} to [trivial] [[XADDR]] : $*Int

// CHECK: sil hidden [global_init] @$s12lazy_globals1xSivau : $@convention(thin) () -> Builtin.RawPointer {
// CHECK:   [[TOKEN_ADDR:%.*]] = global_addr @globalinit_[[T]]_token0 : $*Builtin.Word
// CHECK:   [[TOKEN_PTR:%.*]] = address_to_pointer [[TOKEN_ADDR]] : $*Builtin.Word to $Builtin.RawPointer
// CHECK:   [[INIT_FUNC:%.*]] = function_ref @globalinit_[[T]]_func0 : $@convention(c) () -> ()
// CHECK:   builtin "once"([[TOKEN_PTR]] : $Builtin.RawPointer, [[INIT_FUNC]] : $@convention(c) () -> ()) : $()
// CHECK:   [[GLOBAL_ADDR:%.*]] = global_addr @$s12lazy_globals1xSivp : $*Int
// CHECK:   [[GLOBAL_PTR:%.*]] = address_to_pointer [[GLOBAL_ADDR]] : $*Int to $Builtin.RawPointer
// CHECK:   return [[GLOBAL_PTR]] : $Builtin.RawPointer
// CHECK: }
var x: Int = 0

// CHECK: sil private @globalinit_[[T:.*]]_func1 : $@convention(c) () -> () {
// CHECK:   alloc_global @$s12lazy_globals3FooV3fooSivpZ
// CHECK:   [[XADDR:%.*]] = global_addr @$s12lazy_globals3FooV3fooSivpZ : $*Int
// CHECK:   store {{.*}} to [trivial] [[XADDR]] : $*Int
// CHECK:   return

struct Foo {
// CHECK: sil hidden [global_init] @$s12lazy_globals3FooV3fooSivau : $@convention(thin) () -> Builtin.RawPointer {
// CHECK:   [[TOKEN_ADDR:%.*]] = global_addr @globalinit_[[T]]_token1 : $*Builtin.Word
// CHECK:   [[TOKEN_PTR:%.*]] = address_to_pointer [[TOKEN_ADDR]] : $*Builtin.Word to $Builtin.RawPointer
// CHECK:   [[INIT_FUNC:%.*]] = function_ref @globalinit_[[T]]_func1 : $@convention(c) () -> ()
// CHECK:   builtin "once"([[TOKEN_PTR]] : $Builtin.RawPointer, [[INIT_FUNC]] : $@convention(c) () -> ()) : $()
// CHECK:   [[GLOBAL_ADDR:%.*]] = global_addr @$s12lazy_globals3FooV3fooSivpZ : $*Int
// CHECK:   [[GLOBAL_PTR:%.*]] = address_to_pointer [[GLOBAL_ADDR]] : $*Int to $Builtin.RawPointer
// CHECK:   return [[GLOBAL_PTR]] : $Builtin.RawPointer
  static var foo: Int = 22

  static var computed: Int {
    return 33
  }

  static var initialized: Int = 57
}

// CHECK: sil private @globalinit_[[T:.*]]_func3 : $@convention(c) () -> () {
// CHECK:   alloc_global @$s12lazy_globals3BarO3barSivpZ
// CHECK:   [[XADDR:%.*]] = global_addr @$s12lazy_globals3BarO3barSivpZ : $*Int
// CHECK:   store {{.*}} to [trivial] [[XADDR]] : $*Int
// CHECK:   return

enum Bar {
// CHECK: sil hidden [global_init] @$s12lazy_globals3BarO3barSivau : $@convention(thin) () -> Builtin.RawPointer {
// CHECK:   [[TOKEN_ADDR:%.*]] = global_addr @globalinit_[[T]]_token3 : $*Builtin.Word
// CHECK:   [[TOKEN_PTR:%.*]] = address_to_pointer [[TOKEN_ADDR]] : $*Builtin.Word to $Builtin.RawPointer
// CHECK:   [[INIT_FUNC:%.*]] = function_ref @globalinit_[[T]]_func3 : $@convention(c) () -> ()
// CHECK:   builtin "once"([[TOKEN_PTR]] : $Builtin.RawPointer, [[INIT_FUNC]] : $@convention(c) () -> ()) : $()
// CHECK:   [[GLOBAL_ADDR:%.*]] = global_addr @$s12lazy_globals3BarO3barSivpZ : $*Int
// CHECK:   [[GLOBAL_PTR:%.*]] = address_to_pointer [[GLOBAL_ADDR]] : $*Int to $Builtin.RawPointer
// CHECK:   return [[GLOBAL_PTR]] : $Builtin.RawPointer
  static var bar: Int = 33
}

// We only emit one initializer function per pattern binding, which initializes
// all of the bound variables.

func f() -> (Int, Int) { return (1, 2) }

// CHECK: sil private @globalinit_[[T]]_func4 : $@convention(c) () -> () {
// CHECK:   function_ref @$s12lazy_globals1fSi_SityF : $@convention(thin) () -> (Int, Int)
// CHECK: sil hidden [global_init] @$s12lazy_globals2a1Sivau : $@convention(thin) () -> Builtin.RawPointer
// CHECK:   function_ref @globalinit_[[T]]_func4 : $@convention(c) () -> ()
// CHECK:   global_addr @$s12lazy_globals2a1Sivp : $*Int
// CHECK: sil hidden [global_init] @$s12lazy_globals2b1Sivau : $@convention(thin) () -> Builtin.RawPointer {
// CHECK:   function_ref @globalinit_[[T]]_func4 : $@convention(c) () -> ()
// CHECK:   global_addr @$s12lazy_globals2b1Sivp : $*Int
var (a1, b1) = f()

var computed: Int {
  return 44
}

var initialized: Int = 57

