// RUN: %target-swift-frontend -parse-as-library -module-name A -Xllvm -sil-print-debuginfo %s -g -O -o - -emit-sil | %FileCheck %s --check-prefix=SIL

@inline(never)
func yes() -> Bool { return true }

@inline(never)
func use<V>(_ v: V) {}

@inline(__always)
func h<U>(_ u: U) {
  yes()
  use(u)
}

#sourceLocation(file: "g.swift", line: 1)
@inline(__always) func g<T>(_ t: T) {
  if (yes()) {
    use(t)
  }
}

// SIL: sil_scope [[F:.*]] { {{.*}}parent @$S1A1CC1fyyqd__lF
// SIL: sil_scope [[F0:.*]] { loc "f.swift":1:29 parent [[F]] }
// SIL: sil_scope [[F_G_S:.*]] { loc "f.swift":5:5 parent [[F0]] }
// SIL: sil_scope [[G_S:.*]] { loc "g.swift":2:3 {{.*}} inlined_at [[F_G_S]] }

#sourceLocation(file: "C.swift", line: 1)
public class C<R> {
  let r : R
  init(_ _r: R) { r = _r }

  // SIL: // C.f<A>(_:)
#sourceLocation(file: "f.swift", line: 1)
  public func f<S> (_ s: S) {
    // SIL: debug_value_addr %0 : $*S, let, name "s", argno 1,
    // SIL-SAME:             scope [[F]]
    // SIL: function_ref {{.*}}yes{{.*}} scope [[G_S]]
    g(s)
    g(r)
    g((s, s))
  }
}
