// RUN: %{python} %S/../Inputs/timeout.py 10 %target-swift-frontend -O -parse-as-library -experimental-swift-based-closure-specialization %s -emit-sil | %FileCheck %s
// XFAIL: *
public func callit() {
    testit { false }
}

// Check if the compiler terminates and does not full into an infinite optimization
// loop between the ClosureSpecializer and CapturePropagation.

// CHECK-LABEL: sil @$s23closure_specialize_loop6testit1cySbyc_tF
public func testit(c: @escaping () -> Bool) {
  if c() {
    testit { !c() }
  }
}

// PR: https://github.com/apple/swift/pull/61956
// Optimizing Expression.contains(where:) should not timeout.
//
// Repeated capture propagation leads to:
//   func contains$termPred@arg0$[termPred$falsePred@arg1]@arg1(expr) {
//     closure = termPred$[termPred$falsePred@arg1]@arg1
//     falsePred(expr)
//     contains$termPred@arg0$termPred$[termPred$falsePred@arg1]@arg1(expr)
//   }
//
//   func contains$termPred@arg0$termPred$[termPred$falsePred@arg1]@arg1(expr) {
//   closure = [termPred(termPred$[termPred$falsePred@arg1]@arg1)]
//   closure(expr)
//   contains$termPred@arg0(expr, closure)
// }
// The Demangled type tree looks like:
//   kind=FunctionSignatureSpecialization
//     kind=SpecializationPassID, index=3
//     kind=FunctionSignatureSpecializationParam
//     kind=FunctionSignatureSpecializationParam
//       kind=FunctionSignatureSpecializationParamKind, index=0
//       kind=FunctionSignatureSpecializationParamPayload, text="$s4test10ExpressionO8contains5whereS3bXE_tFSbACXEfU_S2bXEfU_36$s4test12IndirectEnumVACycfcS2bXEfU_Tf3npf_n"
//
// CHECK-LABEL: $s23closure_specialize_loop10ExpressionO8contains5whereS3bXE_tFSbACXEfU_S2bXEfU_012$s23closure_b7_loop10d44O8contains5whereS3bXE_tFSbACXEfU_S2bXEfU_012g13_b7_loop10d44ijk2_tlm2U_no52U_012g30_B34_loop12IndirectEnumVACycfcnO10U_Tf3npf_nY2_nTf3npf_n
// ---> function signature specialization
//     <Arg[1] = [Constant Propagated Function : function signature specialization
//                <Arg[1] = [Constant Propagated Function : function signature specialization
//                           <Arg[1] = [Constant Propagated Function : closure #1 (Swift.Bool) -> Swift.Bool
//                                      in closure_specialize_loop.IndirectEnum.init() -> closure_specialize_loop.IndirectEnum]>
//                           of closure #1 (Swift.Bool) -> Swift.Bool
//                           in closure #1 (closure_specialize_loop.Expression) -> Swift.Bool
//                           in closure_specialize_loop.Expression.contains(where: (Swift.Bool) -> Swift.Bool) -> Swift.Bool]>
//                of closure #1 (Swift.Bool) -> Swift.Bool
//                in closure #1 (closure_specialize_loop.Expression) -> Swift.Bool
//                in closure_specialize_loop.Expression.contains(where: (Swift.Bool) -> Swift.Bool) -> Swift.Bool]>
//     of closure #1 (Swift.Bool) -> Swift.Bool
//     in closure #1 (closure_specialize_loop.Expression) -> Swift.Bool
//     in closure_specialize_loop.Expression.contains(where: (Swift.Bool) -> Swift.Bool) -> Swift.Bool
//
public indirect enum Expression {
    case term(Bool)
    case list(_ expressions: [Expression])

    public func contains(where predicate: (Bool) -> Bool) -> Bool {
        switch self {
        case let .term(term):
            return predicate(term)
        case let .list(expressions):
            return expressions.contains { expression in
                expression.contains { term in
                    predicate(term)
                }
            }
        }
    }
}

public struct IndirectEnum {
    public init() {
        let containsFalse = Expression.list([.list([.term(true), .term(false)]), .term(true)]).contains { term in
            term == false
        }
        print(containsFalse)
    }
}
