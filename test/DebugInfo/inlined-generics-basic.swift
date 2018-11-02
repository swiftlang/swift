// RUN: %target-swift-frontend -parse-as-library -module-name A -Xllvm -sil-print-debuginfo %s -g -O -o - -emit-sil | %FileCheck %s --check-prefix=SIL

@inline(never)
func yes() -> Bool { return true }

#sourceLocation(file: "use.swift", line: 1)
@inline(never) func use<V>(_ v: V) {}

#sourceLocation(file: "h.swift", line: 1)
@inline(__always) func h<U>(_ u: U) {
  yes()
  use(u)
}

#sourceLocation(file: "g.swift", line: 1)
@inline(__always) func g<T>(_ t: T) {
  if (yes()) {
    h(t)
  }
}

// SIL: sil_scope [[F:.*]] { {{.*}}parent @$S1A1CC1fyyqd__lF
// SIL: sil_scope [[F1:.*]] { loc "f.swift":1:29 parent [[F]] }
// SIL: sil_scope [[F1G:.*]] { loc "f.swift":5:5 parent [[F1]] }
// SIL: sil_scope [[F1G1:.*]] { loc "g.swift":2:3 {{.*}}inlined_at [[F1G]] }
// SIL: sil_scope [[F1G3:.*]] { loc "g.swift":3:5 {{.*}}inlined_at [[F1G]] }
// SIL: sil_scope [[F1G3H:.*]] { loc "h.swift":1:24
// SIL-SAME:                     parent @{{.*}}1h{{.*}} inlined_at [[F1G3]] }
// SIL: sil_scope [[F1G3H1:.*]] { loc "h.swift":1:37
// SIL-SAME:                      parent [[F1G3H]] inlined_at [[F1G3]] }
// SIL: sil_scope [[F1G3H2:.*]] { loc "h.swift":3:3
// SIL-SAME:                      parent [[F1G3H1]] inlined_at [[F1G3]] }
// SIL: sil_scope [[F1G3H2_THUNK:.*]] { loc "use.swift":1:21
// SIL-SAME:                            inlined_at [[F1G3H2]] }

#sourceLocation(file: "C.swift", line: 1)
public class C<R> {
  let r : R
  init(_ _r: R) { r = _r }

  // SIL: // C.f<A>(_:)
#sourceLocation(file: "f.swift", line: 1)
  public func f<S> (_ s: S) {
    // SIL: debug_value_addr %0 : $*S, let, name "s", argno 1,{{.*}} scope [[F]]
    // SIL: function_ref {{.*}}yes{{.*}} scope [[F1G1]]
    // SIL: function_ref {{.*}}use{{.*}}:0:0, scope [[F1G3H2_THUNK]]
    g(s)
    g(r)
    g((s, s))
  }
}
