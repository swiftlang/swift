// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=POSTFIX_1 | FileCheck %s -check-prefix=POSTFIX_1
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=POSTFIX_2 | FileCheck %s -check-prefix=POSTFIX_2
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=POSTFIX_3 | FileCheck %s -check-prefix=POSTFIX_3
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=POSTFIX_4 | FileCheck %s -check-prefix=POSTFIX_4
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=POSTFIX_5 | FileCheck %s -check-prefix=POSTFIX_5
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=POSTFIX_6 | FileCheck %s -check-prefix=POSTFIX_6
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=POSTFIX_7 | FileCheck %s -check-prefix=POSTFIX_7
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=POSTFIX_8 | FileCheck %s -check-prefix=POSTFIX_8
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=POSTFIX_9 | FileCheck %s -check-prefix=POSTFIX_9
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=POSTFIX_10 | FileCheck %s -check-prefix=POSTFIX_10

struct S {}
postfix operator ++ {}
postfix func ++(inout x: S) -> S { return x }

func testPostfix1(x: S) {
  x#^POSTFIX_1^#
}
// POSTFIX_1-NOT: ++

func testPostfix2(var x: S) {
  x#^POSTFIX_2^#
}
// POSTFIX_2: Begin completions
// POSTFIX_2-DAG: Decl[OperatorFunction]/CurrModule:  ++[#S#]
// POSTFIX_2-DAG-NOT: --
// POSTFIX_2: End completions


postfix operator +- {}
postfix func +-(x: S) -> S? { return x }
func testPostfix3(x: S) {
  x#^POSTFIX_3^#
}
// POSTFIX_3: Decl[OperatorFunction]/CurrModule:  +-[#S?#]

func testPostfix4(x: S?) {
  x#^POSTFIX_4^#
}
// POSTFIX_4: Pattern/None:  ![#S#]

struct T {}
postfix func +-<G>(x: G?) -> G { return x! }
func testPostfix5(x: T?) {
  x#^POSTFIX_5^#
}
// POSTFIX_5: Decl[OperatorFunction]/CurrModule:  +-[#T#]

protocol Fooable {}
extension Int : Fooable {}
extension Double : Fooable {}

postfix operator *** {}
postfix func ***<G: Fooable>(x: G) -> G { return x }
func testPostfix6() {
  1 + 2 * 3#^POSTFIX_6^#
}
// POSTFIX_6: Decl[OperatorFunction]/CurrModule:  ***[#Int#]

func testPostfix7() {
  1 + 2 * 3.0#^POSTFIX_7^#
}
// POSTFIX_7: Decl[OperatorFunction]/CurrModule:  ***[#Double#]

func testPostfix8(x: S) {
  x#^POSTFIX_8^#
}
// POSTFIX_8-NOT: ***

protocol P {
  typealias T
  func foo() -> T
}

func testPostfix9<G: P where G.T == Int>(x: G) {
  x.foo()#^POSTFIX_9^#
}
// POSTFIX_9: Decl[OperatorFunction]/CurrModule: ***[#Int#]

func testPostfix10<G: P where G.T : Fooable>(x: G) {
  x.foo()#^POSTFIX_10^#
}
// POSTFIX_10: Decl[OperatorFunction]/CurrModule: ***[#G.T#]
