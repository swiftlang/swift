// SIL.
// RUN: %target-swift-frontend -parse-as-library -module-name A \
// RUN:      -Xllvm -sil-print-debuginfo %s -g -O -o - -emit-sil \
// RUN:    | %FileCheck %s --check-prefix=SIL
// IR.
// RUN: %target-swift-frontend -parse-as-library -module-name A \
// RUN:      %s -g -O -o - -emit-ir \
// RUN:      | %FileCheck %s --check-prefix=IR

import StdlibUnittest

@inline(never)
func yes() -> Bool { return true }

#sourceLocation(file: "use.swift", line: 1)
@inline(never) func use<V>(_ v: V) { _blackHole(v) }

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

// SIL: sil_scope [[F:.*]] { {{.*}}parent @$s1A1CC1fyyqd__lF
// SIL: sil_scope [[F1:.*]] { loc "f.swift":1:28 parent [[F]] }
// SIL: sil_scope [[F1G:.*]] { loc "f.swift":2:5 parent [[F1]] }
// SIL: sil_scope [[F1G1:.*]] { loc "g.swift":2:3 {{.*}}inlined_at [[F1G]] }
// SIL: sil_scope [[F1G3:.*]] { loc "g.swift":3:5 {{.*}}inlined_at [[F1G]] }
// SIL: sil_scope [[F1G3H:.*]] { loc "h.swift":1:24
// SIL-SAME:                     parent @{{.*}}1h{{.*}} inlined_at [[F1G3]] }
// SIL: sil_scope [[F1G3H1:.*]] { loc "h.swift":1:37
// SIL-SAME:                      parent [[F1G3H]] inlined_at [[F1G3]] }

#sourceLocation(file: "C.swift", line: 1)
public class C<R> {
  let r : R
  init(_ _r: R) { r = _r }

  // SIL-LABEL: // C.f<A>(_:)
  // IR-LABEL: define {{.*}} @"$s1A1CC1fyyqd__lF"
#sourceLocation(file: "f.swift", line: 1)
  public func f<S>(_ s: S) {
    // SIL: debug_value_addr %0 : $*S, let, name "s", argno 1,{{.*}} scope [[F]]
    // SIL: function_ref {{.*}}yes{{.*}} scope [[F1G1]]
    // SIL: function_ref {{.*}}use{{.*}} scope [[F1G3H1]]
    // IR: dbg.value(metadata %swift.type* %S, metadata ![[MD_1_0:[0-9]+]]
    // IR: dbg.value(metadata %swift.opaque* %0, metadata ![[S:[0-9]+]]
    // IR: dbg.value(metadata %swift.opaque* %0, metadata ![[GS_T:[0-9]+]]
    // IR: dbg.value(metadata %swift.opaque* %0, metadata ![[GS_U:[0-9]+]]
    // IR: call {{.*}}3use
#sourceLocation(file: "f.swift", line: 2)
    g(s)
    // Jump-threading removes the basic block containing debug_value
    // "t" before the second call to `g(r)`. When this happens, the
    // ref_element_addr in that removed block is left with a single
    // debug_value use, so they are both deleted. This means we have
    // no debug value for "t" in the call to `g(r)`.
    //   dbg.value({{.*}}, metadata ![[GR_T:[0-9]+]]

    // IR: dbg.value({{.*}}, metadata ![[GR_U:[0-9]+]]
    // IR: call {{.*}}3use
#sourceLocation(file: "f.swift", line: 3)
    g(r)
    // IR: dbg.value({{.*}}, metadata ![[GRS_T:[0-9]+]]
    // IR: dbg.value({{.*}}, metadata ![[GRS_U:[0-9]+]]
    // IR: call {{.*}}3use
#sourceLocation(file: "f.swift", line: 4)
    g((r, s))
    // Note to maintainers: the relative order of the constant dbg.values here
    // seem to flip back and forth.
    // IR: dbg.value({{.*}}, metadata ![[GI_U:[0-9]+]]
    // IR: dbg.value({{.*}}, metadata ![[GI_T:[0-9]+]]
    // IR: call {{.*}}3use
#sourceLocation(file: "f.swift", line: 5)
    g(Int(0))
    // IR: dbg.value({{.*}}, metadata ![[GB_U:[0-9]+]]
    // IR: dbg.value({{.*}}, metadata ![[GB_T:[0-9]+]]
    // IR: call {{.*}}3use
#sourceLocation(file: "f.swift", line: 6)
    g(false)
  }
}
// SIL-LABEL: } // end sil function '$s1A1CC1fyyqd__lF'
// IR-LABEL: ret void

// IR: ![[BOOL:[0-9]+]] = !DICompositeType({{.*}}name: "Bool"
// IR: ![[LET_BOOL:[0-9]+]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[BOOL]])
// IR: ![[INT:[0-9]+]] = !DICompositeType({{.*}}name: "Int"
// IR: ![[LET_INT:[0-9]+]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[INT]])
// IR: ![[TAU_0_0:[0-9]+]] = {{.*}}DW_TAG_structure_type, name: "$sxD",
// IR: ![[LET_TAU_0_0:[0-9]+]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[TAU_0_0]])
// IR: ![[TAU_1_0:[0-9]+]] = {{.*}}DW_TAG_structure_type, name: "$sqd__D",
// IR: ![[MD_1_0]] = !DILocalVariable(name: "$\CF\84_1_0"
// IR: ![[S]] = !DILocalVariable(name: "s", {{.*}} type: ![[LET_TAU_1_0:[0-9]+]]
// IR: ![[LET_TAU_1_0]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[TAU_1_0]])
// IR: ![[GS_T]] = !DILocalVariable(name: "t", {{.*}} scope: ![[SP_GS_T:[0-9]+]], {{.*}} type: ![[LET_TAU_1_0]])
// IR: ![[SP_GS_T]] = {{.*}}linkageName: "$s1A1gyyxlFqd___Ti5"
// IR: ![[GS_U]] = !DILocalVariable(name: "u", {{.*}} scope: ![[SP_GS_U:[0-9]+]], {{.*}} type: ![[LET_TAU_1_0]])
// IR: ![[SP_GS_U]] = {{.*}}linkageName: "$s1A1hyyxlFqd___Ti5"

// Debug info for this variable is removed. See the note above the call to g(r).
//   ![[GR_T]] = !DILocalVariable(name: "t", {{.*}} scope: ![[SP_GR_T:[0-9]+]], {{.*}}type: ![[LET_TAU_0_0]])
// S has the same generic parameter numbering s T and U.
//   ![[SP_GR_T]] = {{.*}}linkageName: "$s1A1gyyxlF"

// IR: ![[GR_U]] = !DILocalVariable(name: "u", {{.*}} scope: ![[SP_GR_U:[0-9]+]], {{.*}}type: ![[LET_TAU_0_0]])
// IR: ![[SP_GR_U]] = {{.*}}linkageName: "$s1A1hyyxlF"
// IR: ![[GRS_T]] = !DILocalVariable(name: "t", {{.*}} scope: ![[SP_GRS_T:[0-9]+]], {{.*}}type: ![[LET_TUPLE:[0-9]+]]
// IR: ![[SP_GRS_T]] = {{.*}}linkageName: "$s1A1gyyxlFx_qd__t_Ti5"
// IR: ![[LET_TUPLE]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[TUPLE:[0-9]+]])
// IR: ![[TUPLE]] = {{.*}}DW_TAG_structure_type, name: "$sx_qd__tD"
// IR: ![[GRS_U]] = !DILocalVariable(name: "u", {{.*}} scope: ![[SP_GRS_U:[0-9]+]], {{.*}}type: ![[LET_TUPLE]]
// IR: ![[SP_GRS_U]] = {{.*}}linkageName: "$s1A1hyyxlFx_qd__t_Ti5"
// IR-DAG: ![[GI_T]] = !DILocalVariable(name: "t", {{.*}} scope: ![[SP_GI_G:[0-9]+]], {{.*}}type: ![[LET_INT]])
// IR-DAG: ![[SP_GI_G]] = {{.*}}linkageName: "$s1A1gyyxlFSi_Tg5"
// IR-DAG: ![[GI_U]] = !DILocalVariable(name: "u", {{.*}} scope: ![[SP_GI_U:[0-9]+]], {{.*}}type: ![[LET_INT]])
// IR-DAG: ![[SP_GI_U]] = {{.*}}linkageName: "$s1A1hyyxlFSi_TG5"
// IR-DAG: ![[GB_T]] = !DILocalVariable(name: "t", {{.*}} scope: ![[SP_GB_G:[0-9]+]], {{.*}}type: ![[LET_BOOL]])
// IR-DAG: ![[SP_GB_G]] = {{.*}}linkageName: "$s1A1gyyxlFSb_Tg5"
// IR-DAG: ![[GB_U]] = !DILocalVariable(name: "u", {{.*}} scope: ![[SP_GB_U:[0-9]+]], {{.*}}type: ![[LET_BOOL]])
// IR-DAG: ![[SP_GB_U]] = {{.*}}linkageName: "$s1A1hyyxlFSb_TG5"
