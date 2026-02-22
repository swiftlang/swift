// Complex literal expressions in integer generic parameter values with AST verification
// REQUIRES: swift_feature_LiteralExpressions
// RUN: %target-swift-frontend -typecheck -dump-ast %s -disable-availability-checking -enable-experimental-feature LiteralExpressions -disable-experimental-parser-round-trip | %FileCheck %s

let foldAdd: InlineArray<(2 + 3), Int> = [1, 2, 3, 4, 5]
// CHECK-LABEL: (pattern_named type="InlineArray<5, Int>" "foldAdd")

let foldMul: InlineArray<(2 * 3), Int> = [1, 2, 3, 4, 5, 6]
// CHECK-LABEL: (pattern_named type="InlineArray<6, Int>" "foldMul")

let foldShift: InlineArray<(1 << 4), Int> = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
// CHECK-LABEL: (pattern_named type="InlineArray<16, Int>" "foldShift")

let foldComplex: InlineArray<((2 + 3) * 2), Int> = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
// CHECK-LABEL: (pattern_named type="InlineArray<10, Int>" "foldComplex")

let foldOr: InlineArray<(4 | 3), Int> = [1, 2, 3, 4, 5, 6, 7]
// CHECK-LABEL: (pattern_named type="InlineArray<7, Int>" "foldOr")

let nest1: InlineArray<((1 + 1) * (2 + 1)), Int> = [1, 2, 3, 4, 5, 6]
// CHECK-LABEL: (pattern_named type="InlineArray<6, Int>" "nest1")

let nest2: InlineArray<(((1 << 2) + 1) - 2), Int> = [1, 2, 3]
// CHECK-LABEL: (pattern_named type="InlineArray<3, Int>" "nest2")

let sugarFold1: [(2 + 2) of Int] = [1, 2, 3, 4]
// CHECK-LABEL: (pattern_named type="[4 of Int]" "sugarFold1")

let sugarFold2: [(1 << 3) of String] = ["a", "b", "c", "d", "e", "f", "g", "h"]
// CHECK-LABEL: (pattern_named type="[8 of String]" "sugarFold2")

typealias PowerOfTwo = InlineArray<(1 << 4), Int>
let aliasCheck: PowerOfTwo = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
// CHECK-LABEL: (pattern_named type="PowerOfTwo" "aliasCheck")

typealias ComputedSize = InlineArray<((3 + 2) * (4 - 2)), Int>
let aliasComputed: ComputedSize = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
// CHECK-LABEL: (pattern_named type="ComputedSize" "aliasComputed")

let nestedExpr: InlineArray<(1 + 1), InlineArray<(2 + 1), Int>> = [[1, 2, 3], [4, 5, 6]]
// CHECK-LABEL: (pattern_named type="InlineArray<2, InlineArray<3, Int>>" "nestedExpr")

let nestedSugar: [(2 * 1) of [(1 + 2) of Int]] = [[1, 2, 3], [4, 5, 6]]
// CHECK-LABEL: (pattern_named type="[2 of [3 of Int]]" "nestedSugar")


func process(_ arr: InlineArray<(2 * 4), Int>) -> Int { return 42 }
// CHECK-LABEL: (func_decl decl_context={{.*}} range=[{{.*}}] "process(_:)" interface_type="(InlineArray<8, Int>) -> Int"

let processResult = process([1, 2, 3, 4, 5, 6, 7, 8])
// CHECK-LABEL: (pattern_named type="Int" "processResult")

let base = 2
let variableRef: InlineArray<(base + 1), Int> = [1, 2, 3]
// CHECK-LABEL: (pattern_named type="InlineArray<3, Int>" "variableRef")

let negativeBase: InlineArray<-3, Int>
// CHECK-LABEL: (pattern_named type="InlineArray<-3, Int>" "negativeBase")
let negative: InlineArray<(-3), Int>
// CHECK-LABEL: (pattern_named type="InlineArray<-3, Int>" "negative")
let negativeFold: InlineArray<(2-5), Int>
// CHECK-LABEL: (pattern_named type="InlineArray<-3, Int>" "negativeFold")

struct W<let N: Int, let M: Int> {}
extension W where N == 2 {
  func nIsTwo() -> Int { N }
}
extension W where M == 6 {
  func mIsSix() -> Int { M }
}
extension W where N == 2, M == 6 {
  func bothMatch() -> Int { N + M }
}
// Verify that folded expressions in requirements produce the correct types.
func makeW() -> W<(1+1), (2*3)> { W() }
// CHECK-LABEL: (func_decl decl_context={{.*}} range=[{{.*}}] "makeW()" interface_type="() -> W<2, 6>"
let wResult = makeW().nIsTwo()
// CHECK-LABEL: (pattern_named type="Int" "wResult")
let wResult2 = makeW().mIsSix()
// CHECK-LABEL: (pattern_named type="Int" "wResult2")
let wResult3 = makeW().bothMatch()
// CHECK-LABEL: (pattern_named type="Int" "wResult3")

// Expression argument in type position should be equivalent to the folded literal.
let exprArg: W<(3 - 1), (2 + 4)> = W()
// CHECK-LABEL: (pattern_named type="W<2, 6>" "exprArg")
let exprArgResult = exprArg.nIsTwo()
// CHECK-LABEL: (pattern_named type="Int" "exprArgResult")
let exprArgResult2 = exprArg.bothMatch()
// CHECK-LABEL: (pattern_named type="Int" "exprArgResult2")

struct V<let N: Int> {}
let letNumWithInit = 12
struct Foo {
  let foo: V<(letNumWithInit)>
  // CHECK-LABEL: (pattern_named type="V<12>" "foo")
}
