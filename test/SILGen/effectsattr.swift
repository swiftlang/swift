// RUN: %target-swift-emit-silgen -enable-sil-ownership -parse-stdlib %s | %FileCheck %s


//CHECK: [readonly] @func1
@_effects(readonly) @_silgen_name("func1") func func1() { }

//CHECK: [readnone] @func2
@_effects(readnone) @_silgen_name("func2") func func2() { }

//CHECK: [readwrite] @func3
@_effects(readwrite) @_silgen_name("func3") func func3() { }

//CHECK: [releasenone] @func4
@_effects(releasenone) @_silgen_name("func4") func func4() { }
