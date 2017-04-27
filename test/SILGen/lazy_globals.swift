// RUN: %target-swift-frontend -parse-as-library -emit-silgen %s | %FileCheck %s

// CHECK: sil private @globalinit_[[T:.*]]_func0 : $@convention(thin) () -> () {
// CHECK:   alloc_global @_T012lazy_globals1xSiv
// CHECK:   [[XADDR:%.*]] = global_addr @_T012lazy_globals1xSiv : $*Int
// CHECK:   store {{%.*}} to [trivial] [[XADDR]] : $*Int

// CHECK: sil hidden [global_init] @_T012lazy_globals1xSifau : $@convention(thin) () -> Builtin.RawPointer {
// CHECK:   [[TOKEN_ADDR:%.*]] = global_addr @globalinit_[[T]]_token0 : $*Builtin.Word
// CHECK:   [[TOKEN_PTR:%.*]] = address_to_pointer [[TOKEN_ADDR]] : $*Builtin.Word to $Builtin.RawPointer
// CHECK:   [[INIT_FUNC:%.*]] = function_ref @globalinit_[[T]]_func0 : $@convention(thin) () -> ()
// CHECK:   builtin "once"([[TOKEN_PTR]] : $Builtin.RawPointer, [[INIT_FUNC]] : $@convention(thin) () -> ()) : $()
// CHECK:   [[GLOBAL_ADDR:%.*]] = global_addr @_T012lazy_globals1xSiv : $*Int
// CHECK:   [[GLOBAL_PTR:%.*]] = address_to_pointer [[GLOBAL_ADDR]] : $*Int to $Builtin.RawPointer
// CHECK:   return [[GLOBAL_PTR]] : $Builtin.RawPointer
// CHECK: }
var x: Int = 0

// CHECK: sil private @globalinit_[[T:.*]]_func1 : $@convention(thin) () -> () {
// CHECK:   alloc_global @_T012lazy_globals3FooV3fooSivZ
// CHECK:   [[XADDR:%.*]] = global_addr @_T012lazy_globals3FooV3fooSivZ : $*Int
// CHECK:   store {{.*}} to [trivial] [[XADDR]] : $*Int
// CHECK:   return

struct Foo {
// CHECK: sil hidden [global_init] @_T012lazy_globals3FooV3fooSifau : $@convention(thin) () -> Builtin.RawPointer {
// CHECK:   [[TOKEN_ADDR:%.*]] = global_addr @globalinit_[[T]]_token1 : $*Builtin.Word
// CHECK:   [[TOKEN_PTR:%.*]] = address_to_pointer [[TOKEN_ADDR]] : $*Builtin.Word to $Builtin.RawPointer
// CHECK:   [[INIT_FUNC:%.*]] = function_ref @globalinit_[[T]]_func1 : $@convention(thin) () -> ()
// CHECK:   builtin "once"([[TOKEN_PTR]] : $Builtin.RawPointer, [[INIT_FUNC]] : $@convention(thin) () -> ()) : $()
// CHECK:   [[GLOBAL_ADDR:%.*]] = global_addr @_T012lazy_globals3FooV3fooSivZ : $*Int
// CHECK:   [[GLOBAL_PTR:%.*]] = address_to_pointer [[GLOBAL_ADDR]] : $*Int to $Builtin.RawPointer
// CHECK:   return [[GLOBAL_PTR]] : $Builtin.RawPointer
  static var foo: Int = 22

  static var computed: Int {
    return 33
  }

  static var initialized: Int = 57
}

// CHECK: sil private @globalinit_[[T:.*]]_func3 : $@convention(thin) () -> () {
// CHECK:   alloc_global @_T012lazy_globals3BarO3barSivZ
// CHECK:   [[XADDR:%.*]] = global_addr @_T012lazy_globals3BarO3barSivZ : $*Int
// CHECK:   store {{.*}} to [trivial] [[XADDR]] : $*Int
// CHECK:   return

enum Bar {
// CHECK: sil hidden [global_init] @_T012lazy_globals3BarO3barSifau : $@convention(thin) () -> Builtin.RawPointer {
// CHECK:   [[TOKEN_ADDR:%.*]] = global_addr @globalinit_[[T]]_token3 : $*Builtin.Word
// CHECK:   [[TOKEN_PTR:%.*]] = address_to_pointer [[TOKEN_ADDR]] : $*Builtin.Word to $Builtin.RawPointer
// CHECK:   [[INIT_FUNC:%.*]] = function_ref @globalinit_[[T]]_func3 : $@convention(thin) () -> ()
// CHECK:   builtin "once"([[TOKEN_PTR]] : $Builtin.RawPointer, [[INIT_FUNC]] : $@convention(thin) () -> ()) : $()
// CHECK:   [[GLOBAL_ADDR:%.*]] = global_addr @_T012lazy_globals3BarO3barSivZ : $*Int
// CHECK:   [[GLOBAL_PTR:%.*]] = address_to_pointer [[GLOBAL_ADDR]] : $*Int to $Builtin.RawPointer
// CHECK:   return [[GLOBAL_PTR]] : $Builtin.RawPointer
  static var bar: Int = 33
}

// We only emit one initializer function per pattern binding, which initializes
// all of the bound variables.

func f() -> (Int, Int) { return (1, 2) }

// CHECK: sil private @globalinit_[[T]]_func4 : $@convention(thin) () -> () {
// CHECK:   function_ref @_T012lazy_globals1fSi_SityF : $@convention(thin) () -> (Int, Int)
// CHECK: sil hidden [global_init] @_T012lazy_globals2a1Sifau : $@convention(thin) () -> Builtin.RawPointer
// CHECK:   function_ref @globalinit_[[T]]_func4 : $@convention(thin) () -> ()
// CHECK:   global_addr @_T012lazy_globals2a1Siv : $*Int
// CHECK: sil hidden [global_init] @_T012lazy_globals2b1Sifau : $@convention(thin) () -> Builtin.RawPointer {
// CHECK:   function_ref @globalinit_[[T]]_func4 : $@convention(thin) () -> ()
// CHECK:   global_addr @_T012lazy_globals2b1Siv : $*Int
var (a1, b1) = f()

var computed: Int {
  return 44
}

var initialized: Int = 57

