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

//CHECK-LABEL: sil hidden [ossa] @func5
//CHECK-NEXT:  [%0: noescape! **]
//CHECK-NEXT:  {{^[^[]}}
@_effects(notEscaping t.**) @_silgen_name("func5") func func5<T>(_ t: T) { }

//CHECK-LABEL: sil hidden [ossa] @func6
//CHECK-NEXT:  [%1: escape! v**.c* -> %0.v**]
//CHECK-NEXT:  {{^[^[]}}
@_effects(escaping t.value**.class* => return.value**) @_silgen_name("func6") func func6<T>(_ t: T) -> T { }

struct Mystr<T> {
  var sf: T

  //CHECK-LABEL: sil hidden [ossa] @func7
  //CHECK-NEXT:  [%2: escape! v** -> %1.s0.v**]
  //CHECK-NEXT:  [%3: noescape! **]
  //CHECK-NEXT:  {{^[^[]}}
  @_effects(notEscaping self.**)
  @_effects(escaping s.value** -> t.sf.value**)
  @_silgen_name("func7") func func7<T>(_ t: inout Mystr<T>, _ s: T) -> T { }
}

