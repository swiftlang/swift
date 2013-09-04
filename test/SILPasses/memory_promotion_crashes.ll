// RUN: %sil-opt %s -memory-promotion -verify | FileCheck %s

// These are all regression tests to ensure that the memory promotion pass
// doesn't crash.

import Builtin
import swift


// Mixed combination of aggregate load/stores and elements.
struct Triple {
  var a, b, c : Int
}

// CHECK-LABEL: sil @TripleTest
sil @TripleTest : $[cc(method), thin] ((s : Int64), [byref] Triple) -> Triple {
bb0(%0 : $Int64, %1 : $*Triple):
  %4 = alloc_box $Triple
  %5 = load %1 : $*Triple
  store %5 to %4#1 : $*Triple
  %8 = struct_element_addr %4#1 : $*Triple, #b
  store %0 to %8 : $*Int64
  %10 = load %4#1 : $*Triple
  strong_release %4#0 : $Builtin.ObjectPointer
  return %10 : $Triple
}


struct Single {
  var a : Int
}

// CHECK-LABEL: sil @SingleTest
sil @SingleTest : $[cc(method), thin] (s : [byref]  Single, a : Int64) -> Single {
bb0(%0 : $*Single, %1 : $Int64):
  %4 = alloc_box $Single
  %5 = load %0 : $*Single
  store %5 to %4#1 : $*Single
  
  %8 = struct_element_addr %4#1 : $*Single, #a
  store %1 to %8 : $*Int64

  %10 = load %4#1 : $*Single
  strong_release %4#0 : $Builtin.ObjectPointer
  return %10 : $Single
}


class SomeClass {}

union SomeUnion {
  case x(Int)
  case y(SomeClass)
}

sil @getSomeClass : $[thin] ((), SomeClass.metatype) -> SomeClass
sil @getSomeUnion : $[thin] (SomeClass, SomeUnion.metatype) -> SomeUnion


// CHECK-LABEL: sil @test_union_release
sil @test_union_release : $[thin] () -> () {
bb0:
  %0 = tuple ()
  %1 = alloc_box $SomeUnion                       // users: %9, %8
  %2 = function_ref @getSomeUnion : $[thin] (SomeClass, SomeUnion.metatype) -> SomeUnion // user: %7
  %3 = metatype $SomeUnion.metatype               // user: %7
  %4 = function_ref @getSomeClass : $[thin] ((), SomeClass.metatype) -> SomeClass // user: %6
  %5 = metatype $SomeClass.metatype               // user: %6
  %6 = apply %4(%5) : $[thin] ((), SomeClass.metatype) -> SomeClass // user: %7
  %7 = apply %2(%6, %3) : $[thin] (SomeClass, SomeUnion.metatype) -> SomeUnion // user: %8
  assign %7 to %1#1 : $*SomeUnion
  strong_release %1#0 : $Builtin.ObjectPointer
  %10 = tuple ()                                  // user: %11
  return %10 : $()
}


