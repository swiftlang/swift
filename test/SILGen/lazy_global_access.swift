// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s -check-prefix=MAIN
// RUN: %target-swift-frontend -parse-as-library -emit-silgen %s | FileCheck %s -check-prefix=LIBRARY

// The following code is valid as a library or as a main source file. Script
// variables should be accessed directly, whereas library global variables
// should use the accessor function. Static properties always go through an
// accessor.

var globalProp = 0

struct Fooo {
  static var staticProp = 0
}

// MAIN: sil hidden @_TF18lazy_global_access8usePropsFT_TSiSi_ : $@convention(thin) () -> (Int, Int) {
// MAIN:   global_addr @_Tv18lazy_global_access10globalPropSi : $*Int
// MAIN:   function_ref @_TFV18lazy_global_access4Foooau10staticPropSi : $@convention(thin) () -> Builtin.RawPointer
// LIBRARY: sil hidden @_TF18lazy_global_access8usePropsFT_TSiSi_ : $@convention(thin) () -> (Int, Int) {
// LIBRARY:   function_ref @_TF18lazy_global_accessau10globalPropSi : $@convention(thin) () -> Builtin.RawPointer
// LIRBARY:   function_ref @_TFV18lazy_global_access4Foooau10staticPropSi : $@convention(thin) () -> Builtin.RawPointer
func useProps() -> (Int, Int) {
  return (globalProp, Fooo.staticProp)
}

