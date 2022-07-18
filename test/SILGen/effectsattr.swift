// RUN: %target-swift-emit-silgen -parse-stdlib %s | %FileCheck %s

// REQUIRES: swift_in_compiler

//CHECK: [readonly] [ossa] @func1
@_effects(readonly) @_silgen_name("func1") func func1() { }

//CHECK: [readnone] [ossa] @func2
@_effects(readnone) @_silgen_name("func2") func func2() { }

//CHECK: [readwrite] [ossa] @func3
@_effects(readwrite) @_silgen_name("func3") func func3() { }

//CHECK: [releasenone] [ossa] @func4
@_effects(releasenone) @_silgen_name("func4") func func4() { }

//CHECK: [defined_escapes !%0.**] [ossa] @func5
@_effects(notEscaping t.**) @_silgen_name("func5") func func5<T>(_ t: T) { }

//CHECK: [defined_escapes %1.v**.c* => %0.v**] [ossa] @func6
@_effects(escaping t.value**.class* => return.value**) @_silgen_name("func6") func func6<T>(_ t: T) -> T { }

struct Mystr<T> {
  var sf: T

  //CHECK: [defined_escapes !%3.**, %2.v** -> %1.s0.v**] [ossa] @func7
  @_effects(notEscaping self.**)
  @_effects(escaping s.value** -> t.sf.value**)
  @_silgen_name("func7") func func7<T>(_ t: inout Mystr<T>, _ s: T) -> T { }
}

