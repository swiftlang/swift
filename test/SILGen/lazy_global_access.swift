// RUN: %swift -emit-lazy-global-initializers -emit-silgen %s | FileCheck %s -check-prefix=MAIN
// RUN: %swift -emit-lazy-global-initializers -parse-as-library -emit-silgen %s | FileCheck %s -check-prefix=LIBRARY

// The following code is valid as a library or as a main source file. Script
// variables should be accessed directly, whereas library global variables
// should use the accessor function. Static properties always go through an
// accessor.

var globalProp = 0

struct Type {
  static var staticProp = 0
}

// MAIN: sil @_T18lazy_global_access8usePropsFT_TSiSi_ : $@thin () -> (Int64, Int64) {
// MAIN:   global_addr #globalProp : $*Int64
// MAIN:   function_ref @_TV18lazy_global_access4Type10staticPropSia : $@thin () -> Builtin.RawPointer
// LIBRARY: sil @_T18lazy_global_access8usePropsFT_TSiSi_ : $@thin () -> (Int64, Int64) {
// LIBRARY:   function_ref @_T18lazy_global_access10globalPropSia : $@thin () -> Builtin.RawPointer
// LIRBARY:   function_ref @_TV18lazy_global_access4Type10staticPropSia : $@thin () -> Builtin.RawPointer
def useProps() -> (Int, Int) {
  return (globalProp, Type.staticProp)
}

