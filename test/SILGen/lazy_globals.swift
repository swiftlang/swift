// RUN: %swift -emit-lazy-global-initializers -parse-as-library -emit-silgen %s | FileCheck %s

// CHECK: sil internal @globalinit_func0 : $@thin () -> () {
// CHECK:   %0 = global_addr #x : $*Int64
// CHECK:   %1 = mark_uninitialized %0 : $*Int64
// CHECK:   store %5 to %1 : $*Int64
// CHECK: sil @_T12lazy_globals1xSia : $@thin () -> Builtin.RawPointer {
// CHECK:   %0 = builtin_function_ref #Builtin.once : $@thin (Builtin.RawPointer, () -> ()) -> ()
// CHECK:   %1 = sil_global_addr @globalinit_token0 : $*Builtin.Word
// CHECK:   %2 = address_to_pointer %1 : $*Builtin.Word to $Builtin.RawPointer
// CHECK:   %3 = function_ref @globalinit_func0 : $@thin () -> ()
// CHECK:   %4 = thin_to_thick_function %3 : $@thin () -> () to $() -> ()
// CHECK:   %5 = apply %0(%2, %4) : $@thin (Builtin.RawPointer, () -> ()) -> ()
// CHECK:   %6 = global_addr #x : $*Int64
// CHECK:   %7 = address_to_pointer %6 : $*Int64 to $Builtin.RawPointer
// CHECK:   return %7 : $Builtin.RawPointer
// CHECK: }
var x : Int = 0

struct Foo {
// CHECK: sil @_TV12lazy_globals3Foo3fooSia : $@thin () -> Builtin.RawPointer {
  static var foo: Int = 22
}

enum Bar {
// CHECK: sil @_TO12lazy_globals3Bar3barSia : $@thin () -> Builtin.RawPointer {
  static var bar: Int = 33
}

// TODO: Access globals through accessor functions.
def use_globals() -> (Int, Int, Int) {
  return (x, Foo.foo, Bar.bar)
}

// We only emit one initializer function per pattern binding, which initializes
// all of the bound variables.

def f() -> (Int, Int) { return (1, 2) }

// CHECK: sil internal @globalinit_func3 : $@thin () -> () {
// CHECK:   function_ref @_T12lazy_globals1fFT_TSiSi_ : $@thin () -> (Int64, Int64)
// CHECK: sil @_T12lazy_globals2a1Sia : $@thin () -> Builtin.RawPointer
// CHECK:   function_ref @globalinit_func3 : $@thin () -> ()
// CHECK:   global_addr #a1 : $*Int64
// CHECK: sil @_T12lazy_globals2b1Sia : $@thin () -> Builtin.RawPointer {
// CHECK:   function_ref @globalinit_func3 : $@thin () -> ()
// CHECK:   global_addr #b1 : $*Int64
var (a1, b1) = f()
