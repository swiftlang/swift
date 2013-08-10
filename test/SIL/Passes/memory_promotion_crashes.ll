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
  release %4#0 : $Builtin.ObjectPointer
  return %10 : $Triple
}

