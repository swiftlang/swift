// Test that accessing a legacy projection acts the way a tuple normally does:
// each element is tracked separately by exclusivity, so you can simultaneously
// take exclusive access of several elements. Reproducer for rdar://190152541.

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil -verify %s -o /dev/null

// expected-warning@<unknown> * {{libc not found for }}

import ctypes

func swapElements(_ s: inout StructWithPlainAndArrayFields) {
  swap(&s.small.0, &s.small.1)
}

func swapFieldsOfElements(_ s: inout StructWithArrayOfStructs) {
  swap(&s.elems.0.first, &s.elems.1.first)
}

func swapGlobalElements() {
  swap(&smallGlobalArray.0, &smallGlobalArray.1)
}

// The array is reached through a computed property of an enclosing type.
struct Wrapper {
  var storage = StructWithPlainAndArrayFields()
  var computed: StructWithPlainAndArrayFields {
    get { storage }
    set { storage = newValue }
  }
}

func assignThroughComputed(_ w: inout Wrapper) {
  w.computed.small = (1, 2, 3, 4)
  w.computed.small.0 = 5
}

// The conflict is between the two writebacks to 'computed', not the array.
func swapThroughComputed(_ w: inout Wrapper) {
  swap(&w.computed.small.0, &w.computed.small.1)
  // expected-error@-1 {{inout writeback to computed property 'computed' occurs in multiple arguments to call, introducing invalid aliasing}}
  // expected-note@-2 {{concurrent writeback occurred here}}
}
