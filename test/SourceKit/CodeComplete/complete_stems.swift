// XFAIL: broken_std_regex

func aaaBbb() {}
func aaaaBbb() {}
func aaaCcc() {}
func aaaCccDdd() {}
func abc(def def: Int) {}
func abc(ghi ghi: Int) {}
func abc(ghi ghi: Int, jkl: Int) {}
func DEF() {}
func DEFG() {}
func HIJ() {}
func HIJ_KLM() {}
func HIJ_KLM_NOP() {}
func HIJ_KLM_QRS() {}
func HIJ_KLM(NOP NOP: Int) {}
func HIJ_KLM(NOP_QRS NOP_QRS: Int) {}
func HIJZ() {}

func test001() {
  #^GLOBAL_FUNC_0,a,aaaC,abc,def,hij^#
}
// RUN: %complete-test %s -no-fuzz -group=stems -tok=GLOBAL_FUNC_0 | %FileCheck %s -check-prefix=GLOBAL_FUNC_0
// GLOBAL_FUNC_0-LABEL: Results for filterText: a [
// GLOBAL_FUNC_0-NEXT: aaa:
// GLOBAL_FUNC_0-NEXT:     aaaBbb()
// GLOBAL_FUNC_0-NEXT:     aaaCcc:
// GLOBAL_FUNC_0-NEXT:         aaaCcc()
// GLOBAL_FUNC_0-NEXT:         aaaCccDdd()
// GLOBAL_FUNC_0-NEXT: aaaaBbb()
// GLOBAL_FUNC_0-NEXT: abc(:
// GLOBAL_FUNC_0-NEXT:     abc(def: Int)
// GLOBAL_FUNC_0-NEXT:     abc(ghi:
// GLOBAL_FUNC_0-NEXT:         abc(ghi: Int)
// GLOBAL_FUNC_0-NEXT:         abc(ghi: Int, jkl: Int)
// GLOBAL_FUNC_0: ]

// GLOBAL_FUNC_0-LABEL: Results for filterText: aaaC [
// GLOBAL_FUNC_0-NEXT:    aaaCcc()
// GLOBAL_FUNC_0-NEXT:    aaaCccDdd()
// GLOBAL_FUNC_0-NEXT: ]

// GLOBAL_FUNC_0-LABEL: Results for filterText: abc [
// GLOBAL_FUNC_0-NEXT:     abc(def: Int)

// FIXME: rdar://22062152 Missing sub-groups when there is a common stem to all
// results.
// DISABLED:     abc(ghi:
// GLOBAL_FUNC_0-NEXT:         abc(ghi: Int)
// GLOBAL_FUNC_0-NEXT:         abc(ghi: Int, jkl: Int)
// GLOBAL_FUNC_0-NEXT: ]

// GLOBAL_FUNC_0-LABEL: Results for filterText: def [
// GLOBAL_FUNC_0-NEXT:     DEF()
// GLOBAL_FUNC_0-NEXT:     DEFG()
// GLOBAL_FUNC_0: ]

// GLOBAL_FUNC_0-LABEL: Results for filterText: hij [

// FIXME: rdar://22062152 Missing sub-groups when there is a common stem to all
// results.  Should be -NEXT, and should remove HIJZ() above.
// GLOBAL_FUNC_0: HIJ()
// GLOBAL_FUNC_0-NEXT: HIJ_KLM:
// GLOBAL_FUNC_0-NEXT:     HIJ_KLM(:
// GLOBAL_FUNC_0-NEXT:         HIJ_KLM()
// GLOBAL_FUNC_0-NEXT:         HIJ_KLM(NOP:
// GLOBAL_FUNC_0-NEXT:             HIJ_KLM(NOP: Int)
// GLOBAL_FUNC_0-NEXT:             HIJ_KLM(NOP_QRS: Int)
// GLOBAL_FUNC_0-NEXT:     HIJ_KLM_:
// GLOBAL_FUNC_0-NEXT:         HIJ_KLM_NOP()
// GLOBAL_FUNC_0-NEXT:         HIJ_KLM_QRS()
// GLOBAL_FUNC_0: ]

struct S {
  func aaaBbb() {}
  func aaaaBbb() {}
  func aaaCcc() {}
  func aaaCccDdd() {}
  func abc(def def: Int) {}
  func abc(ghi ghi: Int) {}
  func abc(ghi ghi: Int, jkl: Int) {}
  func DEF() {}
  func DEFG() {}
  func HIJ() {}
  func HIJ_KLM() {}
  func HIJ_KLM_NOP() {}
  func HIJ_KLM_QRS() {}
  func HIJ_KLM(NOP NOP: Int) {}
  func HIJ_KLM(NOP_QRS NOP_QRS: Int) {}
}
func test002(x: S) {
  x.#^S_QUALIFIED_0^#
}
// RUN: %complete-test %s -no-fuzz -group=stems -tok=S_QUALIFIED_0 | %FileCheck %s -check-prefix=S_QUALIFIED_0
// S_QUALIFIED_0:      aaa:
// S_QUALIFIED_0-NEXT:     aaaBbb()
// S_QUALIFIED_0-NEXT:     aaaCcc:
// S_QUALIFIED_0-NEXT:         aaaCcc()
// S_QUALIFIED_0-NEXT:         aaaCccDdd()
// S_QUALIFIED_0-NEXT: aaaaBbb()
// S_QUALIFIED_0-NEXT: abc(:
// S_QUALIFIED_0-NEXT:     abc(def: Int)
// S_QUALIFIED_0-NEXT:     abc(ghi::
// S_QUALIFIED_0-NEXT:         abc(ghi: Int)
// S_QUALIFIED_0-NEXT:         abc(ghi: Int, jkl: Int)
// S_QUALIFIED_0-NEXT: DEF()
// S_QUALIFIED_0-NEXT: DEFG()
// S_QUALIFIED_0-NEXT: HIJ:
// S_QUALIFIED_0-NEXT:     HIJ()
// S_QUALIFIED_0-NEXT:     HIJ_KLM:
// S_QUALIFIED_0-NEXT:         HIJ_KLM(:
// S_QUALIFIED_0-NEXT:             HIJ_KLM()
// S_QUALIFIED_0-NEXT:             HIJ_KLM(NOP:
// S_QUALIFIED_0-NEXT:                 HIJ_KLM(NOP: Int)
// S_QUALIFIED_0-NEXT:                 HIJ_KLM(NOP_QRS: Int)
// S_QUALIFIED_0-NEXT:         HIJ_KLM_:
// S_QUALIFIED_0-NEXT:             HIJ_KLM_NOP()
// S_QUALIFIED_0-NEXT:             HIJ_KLM_QRS()

struct T {
  init() {}
  init(abc: Int) {}
  init(abcDef: Int) {}
  init(abcDefGhi: Int) {}
  func foo() {}
  subscript(x x: Int) -> Int {}
  subscript(x_y x: Int) -> Int {}
  subscript(x_y_z x: Int) -> Int {}
}
// RUN: %complete-test %s -no-fuzz -add-inner-results -no-inner-operators -group=stems -tok=T_POSTFIX_0 | %FileCheck %s -check-prefix=T_POSTFIX_0
func test003() {
  T#^T_POSTFIX_0^#
}
// T_POSTFIX_0: (:
// T_POSTFIX_0-NEXT:   ()
// T_POSTFIX_0-NEXT:   (abc:
// T_POSTFIX_0-NEXT:       (abc: Int)
// T_POSTFIX_0-NEXT:       (abcDef:
// T_POSTFIX_0-NEXT:           (abcDef: Int)
// T_POSTFIX_0-NEXT:           (abcDefGhi: Int)
// T_POSTFIX_0-NEXT: foo(self: T)

// RUN: %complete-test %s -no-fuzz -add-inner-results -no-inner-operators -group=stems -tok=T_POSTFIX_1 | %FileCheck %s -check-prefix=T_POSTFIX_1
func test004(x: T) {
  x#^T_POSTFIX_1^#
}
// T_POSTFIX_1: [x:
// T_POSTFIX_1-NEXT:    [x: Int]
// T_POSTFIX_1-NEXT:    [x_y:
// T_POSTFIX_1-NEXT:        [x_y: Int]
// T_POSTFIX_1-NEXT:        [x_y_z: Int]
// T_POSTFIX_1-NEXT:foo()


struct MyAnyA {}
struct MyAnyB {}
func myanyFunc() {}
struct MyAnyGenerator {}
struct MyAnyGenerationalGarbageCollector {}

// There is only one Any group rdar://problem/21550130
// RUN: %complete-test %s -no-fuzz -group=stems -tok=GLOBAL_FUNC_1 | %FileCheck %s -check-prefix=GLOBAL_FUNC_1
func test005() {
  #^GLOBAL_FUNC_1,my^#
}
// GLOBAL_FUNC_1: Results for filterText: my [
// GLOBAL_FUNC_1-NEXT:     MyAny:
// GLOBAL_FUNC_1-NEXT:         MyAnyA
// GLOBAL_FUNC_1-NEXT:         MyAnyB
// GLOBAL_FUNC_1-NEXT:         MyAnyGenerationalGarbageCollector
// GLOBAL_FUNC_1-NEXT:         MyAnyGenerator
// GLOBAL_FUNC_1-NEXT:     myanyFunc()
// GLOBAL_FUNC_1-NEXT: ]
