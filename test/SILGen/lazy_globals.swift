// RUN: %swift -parse-as-library -emit-silgen %s | FileCheck %s

// CHECK: sil private @globalinit_func0 : $@thin () -> () {
// CHECK:   [[XADDR:%.*]] = global_addr #x : $*Int
// CHECK:   store {{%.*}} to [[XADDR]] : $*Int
// CHECK: sil [global_init] @_TF12lazy_globalsa1xSi : $@thin () -> Builtin.RawPointer {
// CHECK:   %0 = builtin_function_ref "once" : $@thin (Builtin.RawPointer, @owned @callee_owned () -> ()) -> ()
// CHECK:   %1 = sil_global_addr @globalinit_token0 : $*Builtin.Word
// CHECK:   %2 = address_to_pointer %1 : $*Builtin.Word to $Builtin.RawPointer
// CHECK:   %3 = function_ref @globalinit_func0 : $@thin () -> ()
// CHECK:   %4 = thin_to_thick_function %3 : $@thin () -> () to $@callee_owned () -> ()
// CHECK:   %5 = apply %0(%2, %4) : $@thin (Builtin.RawPointer, @owned @callee_owned () -> ()) -> ()
// CHECK:   %6 = global_addr #x : $*Int
// CHECK:   %7 = address_to_pointer %6 : $*Int to $Builtin.RawPointer
// CHECK:   return %7 : $Builtin.RawPointer
// CHECK: }
var x: Int = 0

struct Foo {
// CHECK: sil [global_init] @_TFV12lazy_globals3Fooa3fooSi : $@thin () -> Builtin.RawPointer {
  static var foo: Int = 22

  static var computed: Int {
    return 33
  }

  static var initialized: Int = 57
}

enum Bar {
// CHECK: sil [global_init] @_TFO12lazy_globals3Bara3barSi : $@thin () -> Builtin.RawPointer {
  static var bar: Int = 33
}

// We only emit one initializer function per pattern binding, which initializes
// all of the bound variables.

func f() -> (Int, Int) { return (1, 2) }

// CHECK: sil private @globalinit_func4 : $@thin () -> () {
// CHECK:   function_ref @_TF12lazy_globals1fFT_TSiSi_ : $@thin () -> (Int, Int)
// CHECK: sil [global_init] @_TF12lazy_globalsa2a1Si : $@thin () -> Builtin.RawPointer
// CHECK:   function_ref @globalinit_func4 : $@thin () -> ()
// CHECK:   global_addr #a1 : $*Int
// CHECK: sil [global_init] @_TF12lazy_globalsa2b1Si : $@thin () -> Builtin.RawPointer {
// CHECK:   function_ref @globalinit_func4 : $@thin () -> ()
// CHECK:   global_addr #b1 : $*Int
var (a1, b1) = f()

var computed: Int {
  return 44
}

var initialized: Int = 57

