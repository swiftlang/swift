// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s -check-prefix=MAIN
// RUN: %target-swift-frontend -parse-as-library -emit-silgen %s | %FileCheck %s -check-prefix=LIBRARY

// The following code is valid as a library or as a main source file. Script
// variables should be accessed directly, whereas library global variables
// should use the accessor function. Static properties always go through an
// accessor.

var globalProp = 0

struct Fooo {
  static var staticProp = 0
}

// MAIN: sil hidden @_T018lazy_global_access8usePropsSi_SityF : $@convention(thin) () -> (Int, Int) {
// MAIN:   global_addr @_T018lazy_global_access0B4PropSiv : $*Int
// MAIN:   function_ref @_T018lazy_global_access4FoooV10staticPropSifau : $@convention(thin) () -> Builtin.RawPointer
// LIBRARY: sil hidden @_T018lazy_global_access8usePropsSi_SityF : $@convention(thin) () -> (Int, Int) {
// LIBRARY:   function_ref @_T018lazy_global_access0B4PropSifau : $@convention(thin) () -> Builtin.RawPointer
// LIBRARY:   function_ref @_T018lazy_global_access4FoooV10staticPropSifau : $@convention(thin) () -> Builtin.RawPointer
func useProps() -> (Int, Int) {
  return (globalProp, Fooo.staticProp)
}

