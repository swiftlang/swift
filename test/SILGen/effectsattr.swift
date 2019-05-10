// RUN: %target-swift-emit-silgen -parse-stdlib %s | %FileCheck %s


//CHECK: [readonly] [ossa] @func1
@_effects(readonly) @_silgen_name("func1") func func1() { }

//CHECK: [readnone] [ossa] @func2
@_effects(readnone) @_silgen_name("func2") func func2() { }

//CHECK: [readwrite] [ossa] @func3
@_effects(readwrite) @_silgen_name("func3") func func3() { }

//CHECK: [releasenone] [ossa] @func4
@_effects(releasenone) @_silgen_name("func4") func func4() { }
