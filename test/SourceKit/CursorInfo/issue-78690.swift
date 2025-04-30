// https://github.com/swiftlang/swift/issues/78690
// rdar://143077965

protocol P {
  associatedtype R
}

func foo<each T>(_ xs: repeat each T) {
  for x in repeat each xs {
    // RUN: %sourcekitd-test -req=cursor -pos=%(line-1):7 %s -- %s | %FileCheck %s --check-prefix=BASIC
    // BASIC: s:4main3fooyyxxQpRvzlF1xL_qd__vp
    for x2 in repeat each xs {
      // RUN: %sourcekitd-test -req=cursor -pos=%(line-1):9 %s -- %s | %FileCheck %s --check-prefix=NESTED
      // NESTED: s:4main3fooyyxxQpRvzlF2x2L_qd__vp
    }
  }
}

func bar<each T, each U>(xs: repeat each T, ys: repeat each U) {
  for x1 in repeat each xs {
    for y1 in repeat each ys {
      func localFn() {
        for x2 in repeat each xs {
          for y2 in repeat each ys {
            let k = (x1, y1, x2, y2)
            // RUN: %sourcekitd-test -req=cursor -pos=%(line-1):17 %s -- %s | %FileCheck %s --check-prefix=DOUBLENESTED
            // DOUBLENESTED: s:4main3bar2xs2ysyxxQp_q_q_QptRvzRv_r0_lF7localFnL_yyRvzRv_r0_lF1kL_qd___qd0__qd1__qd2__tvp
            // -> (A1, A2, A3, A4)
          }
        }
      }
      _ = {
        for x2 in repeat each xs {
          for y2 in repeat each ys {
            let k = (x1, y1, x2, y2)
            // RUN: %sourcekitd-test -req=cursor -pos=%(line-1):17 %s -- %s | %FileCheck %s --check-prefix=DOUBLENESTED-CLOSURE
            // DOUBLENESTED-CLOSURE: s:4main3bar2xs2ysyxxQp_q_q_QptRvzRv_r0_lFyycfU_1kL_qd___qd0__qd1__qd2__tvp
            // -> (A1, A2, A3, A4)
          }
        }
      }
    }
  }
}

func baz<each T, each U, each V>(
  ts: repeat each T, us: repeat each U, urs: repeat (each U).R, vs: repeat each V
) where repeat each U: P, (repeat (each U, each V)): Any {
  for t in repeat each ts {
    func localFn() {
      for y in repeat each us {
        func genericLocalFn<A>(_ a: A) {
          for (u, ur, v) in repeat (each us, each urs, each vs) {
            let k = (a, t, y, u, ur, v)
            // RUN: %sourcekitd-test -req=cursor -pos=%(line-1):17 %s -- %s | %FileCheck %s --check-prefix=TUPLE1
            // TUPLE1: s:4main3baz2ts2us3urs2vsyxxQp_q_q_Qp1RQy_q_Qpq0_q_QptRvzRv_Rv0_AA1PR_q0_Rh_r1_lF7localFnL_yyRvzRv_Rv0_AaIR_q0_Rh_r1_lF012genericLocalH0L_yyqd__RvzRv_Rv0_AaIR_q0_Rh_r1__lF1kL_qd___qd0__qd1__qd2__AgaIPQyd2__qd2_0_tvp
            // -> (A1, A2, A3, A4, A4.main.P.R, B4)

            _ = {
              let k = (a, t, y, u, ur, v)
              // RUN: %sourcekitd-test -req=cursor -pos=%(line-1):19 %s -- %s | %FileCheck %s --check-prefix=TUPLE2
              // TUPLE2: s:4main3baz2ts2us3urs2vsyxxQp_q_q_Qp1RQy_q_Qpq0_q_QptRvzRv_Rv0_AA1PR_q0_Rh_r1_lF7localFnL_yyRvzRv_Rv0_AaIR_q0_Rh_r1_lF012genericLocalH0L_yyqd__RvzRv_Rv0_AaIR_q0_Rh_r1__lFyycfU_1kL_qd___qd0__qd1__qd2__AgaIPQyd2__qd2_0_tvp
              // -> (A1, A2, A3, A4, A4.main.P.R, B4)
            }
          }
        }
      }
    }
  }
}
