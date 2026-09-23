// RUN: %target-swift-emit-silgen %s -target %target-future-triple -enable-experimental-feature NoncopyableCasting -verify | %FileCheck %s

// REQUIRES: swift_feature_NoncopyableCasting

// A `case is T` subject does not have to arrive as an address. A loadable
// non-`Copyable` type -- a concrete struct, not an existential -- reaches the
// pattern emitter as a value, and the ordinary lowering would answer the cast
// by materialising that value into a destination buffer, which copies. For a
// copyable subject that is fine and the ordinary lowering keeps it; for this
// one the copy is illegal, so the subject is borrowed into a temporary and
// read in place by the non-consuming test.
//
// What that buys is visible in the instruction itself: `test_only` takes no
// destination operand, so there is one `alloc_stack` (the borrow temporary)
// instead of two (source buffer plus cast destination).
//
// Without this, the three spellings of the same question disagreed: `switch`
// and `if case` both produced "copy of noncopyable typed value", while the
// expression form `x is Big` is rejected outright by Sema.

protocol P: ~Copyable {}
protocol Q {}

struct Small: P, ~Copyable { var a: Int }
struct Big: P, ~Copyable { var a, b, c: Int }

// CHECK-LABEL: sil hidden [ossa] @$s31noncopyable_loadable_is_pattern9viaSwitchySbAA5SmallVF :
// CHECK:       bb0(%0 : @guaranteed $Small):
// CHECK:         [[TEMP:%[0-9]+]] = alloc_stack $Small
// CHECK:         [[BORROW:%[0-9]+]] = store_borrow {{%[0-9]+}} to [[TEMP]]
// The test reads the borrow in place: no destination operand, so no second
// buffer for the cast to write a copy of the subject into.
// CHECK-NOT:     alloc_stack
// CHECK:         checked_cast_addr_br test_only Small in [[BORROW]] to Big, bb1, bb2
// CHECK:       } // end sil function
func viaSwitch(_ x: borrowing Small) -> Bool {
  switch x {
  case is Big: return true // expected-warning {{cast from 'Small' to unrelated type 'Big' always fails}}
  default: return false
  }
}

// The same question in `if case` form reaches the same lowering.
// CHECK-LABEL: sil hidden [ossa] @$s31noncopyable_loadable_is_pattern9viaIfCaseySbAA5SmallVF :
// CHECK:         [[BORROW:%[0-9]+]] = store_borrow
// CHECK:         checked_cast_addr_br test_only Small in [[BORROW]] to Big, bb1, bb2
// CHECK:       } // end sil function
func viaIfCase(_ x: borrowing Small) -> Bool {
  if case is Big = x { return true } // expected-warning {{cast from 'Small' to unrelated type 'Big' always fails}}
  return false
}

// A consuming subject arrives as a value too, and must not be consumed merely
// to be asked about.
// CHECK-LABEL: sil hidden [ossa] @$s31noncopyable_loadable_is_pattern12viaConsumingySbAA5SmallVnF :
// CHECK:       bb0(%0 : @owned $Small):
// CHECK:         [[BORROW:%[0-9]+]] = store_borrow
// CHECK:         checked_cast_addr_br test_only Small in [[BORROW]] to Big, bb1, bb2
// CHECK:       } // end sil function
func viaConsuming(_ x: consuming Small) -> Bool {
  switch x {
  case is Big: return true // expected-warning {{cast from 'Small' to unrelated type 'Big' always fails}}
  default: return false
  }
}

struct CopyableSmall: Q { var a: Int }
struct CopyableBig: Q { var a, b, c: Int }

// A copyable subject keeps the ordinary lowering -- a destination buffer and a
// copying cast. Nothing here widens the non-consuming test to subjects that
// never needed it.
// CHECK-LABEL: sil hidden [ossa] @$s31noncopyable_loadable_is_pattern8copyableySbAA13CopyableSmallVF :
// CHECK-NOT:     test_only
// CHECK:         [[SRC:%[0-9]+]] = alloc_stack $CopyableSmall
// CHECK:         [[DEST:%[0-9]+]] = alloc_stack $CopyableBig
// CHECK:         checked_cast_addr_br copy_on_success CopyableSmall in [[SRC]] to CopyableBig in [[DEST]], bb1, bb2
// CHECK:       } // end sil function
func copyable(_ x: CopyableSmall) -> Bool {
  switch x {
  case is CopyableBig: return true // expected-warning {{cast from 'CopyableSmall' to unrelated type 'CopyableBig' always fails}}
  default: return false
  }
}
