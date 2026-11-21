// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s
// RUN: %target-swift-emit-silgen %s | %FileCheck %s

// https://github.com/swiftlang/swift/issues/86208
// https://github.com/swiftlang/swift/issues/87074
//
// 'copy' and 'consume' of a tuple used to build a single-element RValue for
// the whole tuple and crash with "creating rvalue with wrong number of
// pre-exploded elements".

class C {}

// CHECK-LABEL: sil hidden [ossa] @$s18copy_consume_tuple0A10Borrowing13fooySi_Sit_tF :
// CHECK:         [[BORROW:%.*]] = begin_borrow
// CHECK:         ([[A:%.*]], [[B:%.*]]) = destructure_tuple [[BORROW]]
// CHECK:         [[COPY:%.*]] = tuple ([[A]], [[B]])
// CHECK:         destructure_tuple [[COPY]]
// CHECK:       } // end sil function '$s18copy_consume_tuple0A10Borrowing13fooySi_Sit_tF'
func copyBorrowing1(foo: borrowing (Int, Int)) {
  switch copy foo {
  default: return
  }
}

func copyBorrowingTrivial(foo: borrowing (Int, Int)) -> (Int, Int) {
  return copy foo
}

func copyBorrowingGeneric<T>(foo: borrowing (T, T)) -> (T, T) {
  return copy foo
}

// CHECK-LABEL: sil hidden [ossa] @$s18copy_consume_tuple0A8LocalLetSS_AA1CCtyF :
// CHECK:         [[COPY:%.*]] = explicit_copy_value
// CHECK:         destructure_tuple [[COPY]]
// CHECK:       } // end sil function '$s18copy_consume_tuple0A8LocalLetSS_AA1CCtyF'
func copyLocalLet() -> (String, C) {
  let x = ("a", C())
  return copy x
}

func copyLocalVar() -> (String, Int) {
  var x = ("a", 1)
  x.1 = 2
  return copy x
}

func copyLocalVarTrivial() -> (Int, Int) {
  var x = (1, 1)
  x.1 = 2
  return copy x
}

func copyNested(x: (Int, (String, C))) -> (Int, (String, C)) {
  return copy x
}

func copyConsuming(x: consuming (String, Int)) -> (String, Int) {
  return copy x
}

// CHECK-LABEL: sil hidden [ossa] @$s18copy_consume_tuple0A7Generic1xx_Sitx_Sit_tlF :
// CHECK:         explicit_copy_addr {{%.*}} to [init] [[TMP:%[0-9]+]]
// CHECK:         tuple_element_addr [[TMP]], 0
// CHECK:         tuple_element_addr [[TMP]], 1
// CHECK:       } // end sil function '$s18copy_consume_tuple0A7Generic1xx_Sitx_Sit_tlF'
func copyGeneric<T>(x: (T, Int)) -> (T, Int) {
  return copy x
}

func copyGenericVar<T>(x: T) -> (T, T) {
  var y = (x, x)
  y = (x, x)
  return copy y
}

// CHECK-LABEL: sil hidden [ossa] @$s18copy_consume_tuple0B9Consuming1xSS_SitSS_Sitn_tF :
// CHECK:         [[VALUE:%.*]] = load [take]
// CHECK:         destructure_tuple [[VALUE]]
// CHECK:       } // end sil function '$s18copy_consume_tuple0B9Consuming1xSS_SitSS_Sitn_tF'
func consumeConsuming(x: consuming (String, Int)) -> (String, Int) {
  return consume x
}

func consumeConsumingGeneric<T>(x: consuming (T, Int)) -> (T, Int) {
  return consume x
}

func consumeLocalLet() -> (String, C) {
  let x = ("a", C())
  return consume x
}

func consumeLocalVar() -> (String, Int) {
  var x = ("a", 1)
  x.1 = 2
  return consume x
}

struct S {
  var ex: Any
}

func consumeTupleOfAddressOnly(_ s: S) {
  let tup = (s, 0)
  _ = consume tup
}
