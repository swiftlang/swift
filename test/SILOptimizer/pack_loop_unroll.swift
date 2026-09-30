// RUN: %target-swift-frontend -module-name test -O -emit-sil %s | %FileCheck %s

// A loop over the elements of a pack, with an early exit, is unrolled. This
// allows the body to be specialized for each element.
//
// Reduced from the example in issue #84819.

@inline(never)
public func pred<T: CustomStringConvertible>(_ t1: T) -> Bool {
  return t1.description == "a"
}

// CHECK-LABEL: sil shared [noinline] @$s4test5checkySbxxQpRvzs23CustomStringConvertibleRzlFs4Int8V_s5Int16Vs5Int32VQP_Tg5Tf8x_n : $@convention(thin) (Int8, Int16, Int32) -> Bool {
// CHECK:         function_ref @$s4test4predySbxs23CustomStringConvertibleRzlFs4Int8V_Tg5
// CHECK:         function_ref @$s4test4predySbxs23CustomStringConvertibleRzlFs5Int16V_Tg5
// CHECK:         function_ref @$s4test4predySbxs23CustomStringConvertibleRzlFs5Int32V_Tg5
// CHECK-LABEL: } // end sil function '$s4test5checkySbxxQpRvzs23CustomStringConvertibleRzlFs4Int8V_s5Int16Vs5Int32VQP_Tg5Tf8x_n'
@inline(never)
public func check<each T: CustomStringConvertible>(_ xs: repeat each T) -> Bool {
  for x in repeat each xs {
    if pred(x) {
      return false
    }
  }

  return true
}

public func less(a: Int8, b: Int16, c: Int32) -> Bool {
  check(a, b, c)
}

@inline(never)
public func use<T: CustomStringConvertible>(_ t1: T) {
  print(t1)
}

// The early exit destroys the element, which the rest of the loop body still
// uses.
//
// CHECK-LABEL: sil shared [noinline] @$s4test6check2ySbxxQpRvzs23CustomStringConvertibleRzlFs4Int8V_s5Int16Vs5Int32VQP_Tg5Tf8x_n : $@convention(thin) (Int8, Int16, Int32) -> Bool {
// CHECK:         function_ref @$s4test4predySbxs23CustomStringConvertibleRzlFs4Int8V_Tg5
// CHECK:         function_ref @$s4test3useyyxs23CustomStringConvertibleRzlFs4Int8V_Tg5
// CHECK:         function_ref @$s4test4predySbxs23CustomStringConvertibleRzlFs5Int16V_Tg5
// CHECK:         function_ref @$s4test3useyyxs23CustomStringConvertibleRzlFs5Int16V_Tg5
// CHECK:         function_ref @$s4test4predySbxs23CustomStringConvertibleRzlFs5Int32V_Tg5
// CHECK:         function_ref @$s4test3useyyxs23CustomStringConvertibleRzlFs5Int32V_Tg5
// CHECK-LABEL: } // end sil function '$s4test6check2ySbxxQpRvzs23CustomStringConvertibleRzlFs4Int8V_s5Int16Vs5Int32VQP_Tg5Tf8x_n'
@inline(never)
public func check2<each T: CustomStringConvertible>(_ xs: repeat each T) -> Bool {
  for x in repeat each xs {
    if pred(x) {
      return false
    }
    use(x)
  }

  return true
}

public func less2(a: Int8, b: Int16, c: Int32) -> Bool {
  check2(a, b, c)
}
