// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Onone %s -o %t/a.out.Onone
// RUN: %target-codesign %t/a.out.Onone
// RUN: %target-run %t/a.out.Onone | %FileCheck %s
//
// RUN: %target-build-swift -O %s -o %t/a.out.O
// RUN: %target-codesign %t/a.out.O
// RUN: %target-run %t/a.out.O | %FileCheck %s
//
// REQUIRES: executable_test

// A `modify` accessor coroutine holds a dynamic access open across the yield. In the caller that
// access is only bounded by begin_apply/end_apply, so MandatoryDestroyHoisting must not hoist the
// destroy of `D` into that scope: `D.deinit` opens a second modify access on the same storage and
// the runtime would trap with "Simultaneous accesses ... Fatal access conflict detected".
//
// rdar://185777259

protocol P: AnyObject {
  var p: Int { get set }
}

final class K: P {
  var p: Int = 0
}

final class D {
  let k: K
  let v: Int
  init(k: K, v: Int) { self.k = k; self.v = v }
  deinit { k.p = 99 }
}

@inline(never)
func combine(_ x: inout Int, _ d: D) {
  x = d.v
}

@inline(never)
func test(_ p: P, _ k: K) {
  combine(&p.p, D(k: k, v: 7))
}

let k = K()
test(k, k)

// CHECK: k.p=99
print("k.p=\(k.p)")
