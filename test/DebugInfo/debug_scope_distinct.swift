// REQUIRES: differentiable_programming

// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver -DM -emit-module -emit-module-path %t/M.swiftmodule %s -module-name M
// RUN: %target-swiftc_driver -O -g -I %t -c %s -emit-ir -o - | %FileCheck %s
// RUN: %target-swiftc_driver -O -g -I %t -c %s -o /dev/null

// REQUIRES: CPU=arm64 || CPU=x86_64 || CPU=arm64e

// CHECK: define {{.*}} void @"$s4main1TV4move2byyAC13TangentVectorV_tF"
// CHECK-SAME: ptr {{.*}} %[[ARG_PTR:.*]], ptr
//
// CHECK: %[[ARG0:.*]] = load {{.*}} %[[ARG_PTR]]
// CHECK: #dbg_value({{.*}} %[[ARG0]], ![[VAR1:[0-9]+]], !DIExpression(DW_OP_LLVM_fragment, 0, 64), ![[LOC1:[0-9]+]]
// CHECK: %[[ARG1_GEP:.*]] = getelementptr inbounds{{.*}} i8, ptr %[[ARG_PTR]], i64 8
// CHECK: %[[ARG1:.*]] = load {{.*}} %[[ARG1_GEP]]
// CHECK: #dbg_value({{.*}} %[[ARG1]], ![[VAR1]], !DIExpression(DW_OP_LLVM_fragment, 64, 8), ![[LOC1]]
//
// CHECK: %[[ARG2_GEP:.*]] = getelementptr inbounds{{.*}} i8, ptr %[[ARG_PTR]], i64 32
// CHECK: %[[ARG2:.*]] = load {{.*}} %[[ARG2_GEP]]
// CHECK: #dbg_value({{.*}} %[[ARG2]], ![[VAR1]], !DIExpression(DW_OP_LLVM_fragment, 0, 64), ![[LOC2:[0-9]+]]
// CHECK: %[[ARG3_GEP:.*]] = getelementptr inbounds{{.*}} i8, ptr %[[ARG_PTR]], i64 40
// CHECK: %[[ARG3:.*]] = load {{.*}} %[[ARG3_GEP]]
// CHECK: #dbg_value({{.*}} %[[ARG3]], ![[VAR1]], !DIExpression(DW_OP_LLVM_fragment, 64, 8), ![[LOC2]]

// CHECK-DAG: ![[VAR1]] = !DILocalVariable(name: "offset", arg: 1, scope: ![[SCOPE:[0-9]+]]

// CHECK-DAG: ![[LOC1]] = !DILocation(line: 0, scope: ![[SCOPE]], inlinedAt: ![[LOCINL1:[0-9]+]])
// CHECK-DAG: ![[LOCINL1]] = distinct !DILocation(line: 0, scope: ![[SUBPROG:[0-9]+]])
// CHECK-DAG: ![[SUBPROG]] = distinct !DISubprogram(name: "move", linkageName: "$s4main1TV4move2byyAC13TangentVectorV_tF"

// CHECK-DAG: ![[LOC2]] = !DILocation(line: 0, scope: ![[SCOPE]], inlinedAt: ![[LOCINL2:[0-9]+]])
// CHECK-DAG: ![[LOCINL2]] = distinct !DILocation(line: 0, scope: ![[SUBPROG]])

#if M
import _Differentiation

public struct S<T> {
  class C {}
  let c: C
  internal var b: Int8
  init(_ initb: Int8) {
    b = initb;
    c = C();
  }
}

extension S: AdditiveArithmetic {
  public static var zero: S { let ret = S(0); return ret; }
  public static func == (_ lhs: S, _ rhs: S) -> Bool { return lhs.b == rhs.b; }
  public static func + (_ lhs: S, _ rhs: S) -> S { var ret = lhs; ret.b += rhs.b; return ret; }
  public static func - (_ lhs: S, _ rhs: S) -> S { var ret = lhs; ret.b -= rhs.b; return ret; }
}

extension S: Differentiable {
  public typealias TangentVector = S
}

#else
import _Differentiation
import M

struct T: Differentiable {
  var u1: U
  var u2: U
}

struct U: Differentiable {
  var s: S<Float>
  var v: V
}

struct V: Differentiable {
  var s: S<Float>
}
#endif
