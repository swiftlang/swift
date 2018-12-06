// RUN: %target-swift-emit-silgen -enable-sil-ownership %s | %FileCheck %s -check-prefix=MAIN
// RUN: %target-swift-emit-silgen -parse-as-library -enable-sil-ownership %s | %FileCheck %s -check-prefix=LIBRARY

// The following code is valid as a library or as a main source file. Script
// variables should be accessed directly, whereas library global variables
// should use the accessor function. Static properties always go through an
// accessor.

var globalProp = 0

struct Fooo {
  static var staticProp = 0
}

// MAIN: sil hidden @$s18lazy_global_access8usePropsSi_SityF : $@convention(thin) () -> (Int, Int) {
// MAIN:   global_addr @$s18lazy_global_access0B4PropSivp : $*Int
// MAIN:   function_ref @$s18lazy_global_access4FoooV10staticPropSivau : $@convention(thin) () -> Builtin.RawPointer
// LIBRARY: sil hidden @$s18lazy_global_access8usePropsSi_SityF : $@convention(thin) () -> (Int, Int) {
// LIBRARY:   function_ref @$s18lazy_global_access0B4PropSivau : $@convention(thin) () -> Builtin.RawPointer
// LIBRARY:   function_ref @$s18lazy_global_access4FoooV10staticPropSivau : $@convention(thin) () -> Builtin.RawPointer
func useProps() -> (Int, Int) {
  return (globalProp, Fooo.staticProp)
}

// rdar://46472759
// We used to crash tying to double-emit the setter.
struct Bar {
  mutating func mutate() {}
}
func useGlobalBar() -> Bar {
  globalBar = Bar()
  globalBar.mutate()
  return globalBar
}
private var globalBar = Bar() {
  willSet {}
}
