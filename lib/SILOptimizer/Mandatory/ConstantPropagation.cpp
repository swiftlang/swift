//===--- ConstantPropagation.cpp - Constant fold and diagnose overflows ---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "constant-propagation"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/AST/Expr.h"
#include "swift/SIL/PatternMatch.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SILOptimizer/Utils/ConstantFolding.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"
using namespace swift;
using namespace swift::PatternMatch;

STATISTIC(NumInstFolded, "Number of constant folded instructions");

template<typename...T, typename...U>
static InFlightDiagnostic
diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag, U &&...args) {
  return Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}

namespace {

class ConstantPropagation : public SILFunctionTransform {
  SILFunction *SignedIntegerInitFn;
  SILFunction *UnsignedIntegerInitFn;
  bool EnableDiagnostics;

  bool checkIntegerInitializer(ApplyInst *AI,
                               llvm::SetVector<SILInstruction *> &WorkList);
  void prepare(SILFunction &F);
  bool isApplyOfIntegerInit(SILInstruction &I);
  void initializeWorklist(SILFunction &F, bool InstantiateAssertConfiguration,
                          llvm::SetVector<SILInstruction *> &WorkList);

  SILAnalysis::InvalidationKind processFunction(SILFunction &F,
                                                bool EnableDiagnostics,
                                                unsigned AssertConfiguration);

public:
  ConstantPropagation(bool EnableDiagnostics)
      : SignedIntegerInitFn(nullptr), UnsignedIntegerInitFn(nullptr),
        EnableDiagnostics(EnableDiagnostics) {}

private:
  /// The entry point to the transformation.
  void run() override {
    prepare(*getFunction());
    auto Invalidation = processFunction(*getFunction(), EnableDiagnostics,
                                        getOptions().AssertConfig);

    if (Invalidation != SILAnalysis::InvalidationKind::Nothing) {
      invalidateAnalysis(Invalidation);
    }
  }
};

} // end anonymous namespace

/// \brief Construct (int, overflow) result tuple.
static SILInstruction *constructResultWithOverflowTuple(BuiltinInst *BI,
                                                        APInt Res,
                                                        bool Overflow) {
  // Get the SIL subtypes of the returned tuple type.
  SILType FuncResType = BI->getType();
  assert(FuncResType.castTo<TupleType>()->getNumElements() == 2);
  SILType ResTy1 = FuncResType.getTupleElementType(0);
  SILType ResTy2 = FuncResType.getTupleElementType(1);

  // Construct the folded instruction - a tuple of two literals, the
  // result and overflow.
  SILBuilderWithScope B(BI);
  SILLocation Loc = BI->getLoc();
  SILValue Result[] = {
    B.createIntegerLiteral(Loc, ResTy1, Res),
    B.createIntegerLiteral(Loc, ResTy2, Overflow)
  };
  return B.createTuple(Loc, FuncResType, Result);
}

/// \brief Fold arithmetic intrinsics with overflow.
static SILInstruction *
constantFoldBinaryWithOverflow(BuiltinInst *BI, llvm::Intrinsic::ID ID,
                               bool ReportOverflow,
                               Optional<bool> &ResultsInError) {
  OperandValueArrayRef Args = BI->getArguments();
  assert(Args.size() >= 2);

  auto *Op1 = dyn_cast<IntegerLiteralInst>(Args[0]);
  auto *Op2 = dyn_cast<IntegerLiteralInst>(Args[1]);

  // If either Op1 or Op2 is not a literal, we cannot do anything.
  if (!Op1 || !Op2)
    return nullptr;

  // Calculate the result.
  APInt LHSInt = Op1->getValue();
  APInt RHSInt = Op2->getValue();
  bool Overflow;
  APInt Res = constantFoldBinaryWithOverflow(LHSInt, RHSInt, Overflow, ID);

  // If we can statically determine that the operation overflows,
  // warn about it if warnings are not disabled by ResultsInError being null.
  if (ResultsInError.hasValue() && Overflow && ReportOverflow) {
    // Try to infer the type of the constant expression that the user operates
    // on. If the intrinsic was lowered from a call to a function that takes
    // two arguments of the same type, use the type of the LHS argument.
    // This would detect '+'/'+=' and such.
    Type OpType;
    SILLocation Loc = BI->getLoc();
    const ApplyExpr *CE = Loc.getAsASTNode<ApplyExpr>();
    SourceRange LHSRange, RHSRange;
    if (CE) {
      const auto *Args = dyn_cast_or_null<TupleExpr>(CE->getArg());
      if (Args && Args->getNumElements() == 2) {
        // Look through inout types in order to handle += well.
        CanType LHSTy = Args->getElement(0)->getType()->getInOutObjectType()->
                         getCanonicalType();
        CanType RHSTy = Args->getElement(1)->getType()->getCanonicalType();
        if (LHSTy == RHSTy)
          OpType = Args->getElement(1)->getType();
        
        LHSRange = Args->getElement(0)->getSourceRange();
        RHSRange = Args->getElement(1)->getSourceRange();
      }
    }

    bool Signed = false;
    StringRef Operator = "+";
    
    switch (ID) {
      default: llvm_unreachable("Invalid case");
      case llvm::Intrinsic::sadd_with_overflow:
        Signed = true;
        break;
      case llvm::Intrinsic::uadd_with_overflow:
        break;
      case llvm::Intrinsic::ssub_with_overflow:
        Operator = "-";
        Signed = true;
        break;
      case llvm::Intrinsic::usub_with_overflow:
        Operator = "-";
        break;
      case llvm::Intrinsic::smul_with_overflow:
        Operator = "*";
        Signed = true;
        break;
      case llvm::Intrinsic::umul_with_overflow:
        Operator = "*";
        break;
    }

    if (!OpType.isNull()) {
      diagnose(BI->getModule().getASTContext(),
               Loc.getSourceLoc(),
               diag::arithmetic_operation_overflow,
               LHSInt.toString(/*Radix*/ 10, Signed),
               Operator,
               RHSInt.toString(/*Radix*/ 10, Signed),
               OpType).highlight(LHSRange).highlight(RHSRange);
    } else {
      // If we cannot get the type info in an expected way, describe the type.
      diagnose(BI->getModule().getASTContext(),
               Loc.getSourceLoc(),
               diag::arithmetic_operation_overflow_generic_type,
               LHSInt.toString(/*Radix*/ 10, Signed),
               Operator,
               RHSInt.toString(/*Radix*/ 10, Signed),
               Signed,
               LHSInt.getBitWidth()).highlight(LHSRange).highlight(RHSRange);
    }
    ResultsInError = Optional<bool>(true);
  }

  return constructResultWithOverflowTuple(BI, Res, Overflow);
}

static SILInstruction *
constantFoldBinaryWithOverflow(BuiltinInst *BI, BuiltinValueKind ID,
                               Optional<bool> &ResultsInError) {
  OperandValueArrayRef Args = BI->getArguments();
  auto *ShouldReportFlag = dyn_cast<IntegerLiteralInst>(Args[2]);
  return constantFoldBinaryWithOverflow(BI,
           getLLVMIntrinsicIDForBuiltinWithOverflow(ID),
           ShouldReportFlag && (ShouldReportFlag->getValue() == 1),
           ResultsInError);
}

static SILInstruction *constantFoldIntrinsic(BuiltinInst *BI,
                                             llvm::Intrinsic::ID ID,
                                             Optional<bool> &ResultsInError) {
  switch (ID) {
  default: break;
  case llvm::Intrinsic::expect: {
    // An expect of an integral constant is the constant itself.
    assert(BI->getArguments().size() == 2 && "Expect should have 2 args.");
    auto *Op1 = dyn_cast<IntegerLiteralInst>(BI->getArguments()[0]);
    if (!Op1)
      return nullptr;
    return Op1;
  }

  case llvm::Intrinsic::ctlz: {
    assert(BI->getArguments().size() == 2 && "Ctlz should have 2 args.");
    OperandValueArrayRef Args = BI->getArguments();

    // Fold for integer constant arguments.
    auto *LHS = dyn_cast<IntegerLiteralInst>(Args[0]);
    if (!LHS) {
      return nullptr;
    }
    APInt LHSI = LHS->getValue();
    unsigned LZ = 0;
    // Check corner-case of source == zero
    if (LHSI == 0) {
      auto *RHS = dyn_cast<IntegerLiteralInst>(Args[1]);
      if (!RHS || RHS->getValue() != 0) {
        // Undefined
        return nullptr;
      }
      LZ = LHSI.getBitWidth();
    } else {
      LZ = LHSI.countLeadingZeros();
    }
    APInt LZAsAPInt = APInt(LHSI.getBitWidth(), LZ);
    SILBuilderWithScope B(BI);
    return B.createIntegerLiteral(BI->getLoc(), LHS->getType(), LZAsAPInt);
  }

  case llvm::Intrinsic::sadd_with_overflow:
  case llvm::Intrinsic::uadd_with_overflow:
  case llvm::Intrinsic::ssub_with_overflow:
  case llvm::Intrinsic::usub_with_overflow:
  case llvm::Intrinsic::smul_with_overflow:
  case llvm::Intrinsic::umul_with_overflow:
    return constantFoldBinaryWithOverflow(BI, ID,
                                          /* ReportOverflow */ false,
                                          ResultsInError);
  }
  return nullptr;
}

static SILInstruction *constantFoldCompare(BuiltinInst *BI,
                                           BuiltinValueKind ID) {
  OperandValueArrayRef Args = BI->getArguments();

  // Fold for integer constant arguments.
  auto *LHS = dyn_cast<IntegerLiteralInst>(Args[0]);
  auto *RHS = dyn_cast<IntegerLiteralInst>(Args[1]);
  if (LHS && RHS) {
    APInt Res = constantFoldComparison(LHS->getValue(), RHS->getValue(), ID);
    SILBuilderWithScope B(BI);
    return B.createIntegerLiteral(BI->getLoc(), BI->getType(), Res);
  }

  // Comparisons of an unsigned value with 0.
  SILValue Other;
  auto MatchNonNegative =
      m_BuiltinInst(BuiltinValueKind::AssumeNonNegative, m_ValueBase());
  if (match(BI, m_CombineOr(m_BuiltinInst(BuiltinValueKind::ICMP_ULT,
                                          m_SILValue(Other), m_Zero()),
                            m_BuiltinInst(BuiltinValueKind::ICMP_UGT, m_Zero(),
                                          m_SILValue(Other)))) ||
      match(BI, m_CombineOr(m_BuiltinInst(BuiltinValueKind::ICMP_SLT,
                                          MatchNonNegative, m_Zero()),
                            m_BuiltinInst(BuiltinValueKind::ICMP_SGT, m_Zero(),
                                          MatchNonNegative)))) {
    SILBuilderWithScope B(BI);
    return B.createIntegerLiteral(BI->getLoc(), BI->getType(), APInt());
  }

  if (match(BI, m_CombineOr(m_BuiltinInst(BuiltinValueKind::ICMP_UGE,
                                          m_SILValue(Other), m_Zero()),
                            m_BuiltinInst(BuiltinValueKind::ICMP_ULE, m_Zero(),
                                          m_SILValue(Other)))) ||
      match(BI, m_CombineOr(m_BuiltinInst(BuiltinValueKind::ICMP_SGE,
                                          MatchNonNegative, m_Zero()),
                            m_BuiltinInst(BuiltinValueKind::ICMP_SLE, m_Zero(),
                                          MatchNonNegative)))) {
    SILBuilderWithScope B(BI);
    return B.createIntegerLiteral(BI->getLoc(), BI->getType(), APInt(1, 1));
  }

  // Comparisons with Int.Max.
  IntegerLiteralInst *IntMax;

  // Check signed comparisons.
  if (match(BI,
            m_CombineOr(
                // Int.max < x
                m_BuiltinInst(BuiltinValueKind::ICMP_SLT,
                              m_IntegerLiteralInst(IntMax), m_SILValue(Other)),
                // x > Int.max
                m_BuiltinInst(BuiltinValueKind::ICMP_SGT, m_SILValue(Other),
                              m_IntegerLiteralInst(IntMax)))) &&
      IntMax->getValue().isMaxSignedValue()) {
    // Any signed number should be <= then IntMax.
    SILBuilderWithScope B(BI);
    return B.createIntegerLiteral(BI->getLoc(), BI->getType(), APInt());
  }

  if (match(BI,
            m_CombineOr(
                m_BuiltinInst(BuiltinValueKind::ICMP_SGE,
                              m_IntegerLiteralInst(IntMax), m_SILValue(Other)),
                m_BuiltinInst(BuiltinValueKind::ICMP_SLE, m_SILValue(Other),
                              m_IntegerLiteralInst(IntMax)))) &&
      IntMax->getValue().isMaxSignedValue()) {
    // Any signed number should be <= then IntMax.
    SILBuilderWithScope B(BI);
    return B.createIntegerLiteral(BI->getLoc(), BI->getType(), APInt(1, 1));
  }

  // For any x of the same size as Int.max and n>=1 , (x>>n) is always <= Int.max,
  // that is (x>>n) <= Int.max and Int.max >= (x>>n) are true.
  if (match(BI,
            m_CombineOr(
                // Int.max >= x
                m_BuiltinInst(BuiltinValueKind::ICMP_UGE,
                              m_IntegerLiteralInst(IntMax), m_SILValue(Other)),
                // x <= Int.max
                m_BuiltinInst(BuiltinValueKind::ICMP_ULE, m_SILValue(Other),
                              m_IntegerLiteralInst(IntMax)),
                // Int.max >= x
                m_BuiltinInst(BuiltinValueKind::ICMP_SGE,
                              m_IntegerLiteralInst(IntMax), m_SILValue(Other)),
                // x <= Int.max
                m_BuiltinInst(BuiltinValueKind::ICMP_SLE, m_SILValue(Other),
                              m_IntegerLiteralInst(IntMax)))) &&
      IntMax->getValue().isMaxSignedValue()) {
    // Check if other is a result of a logical shift right by a strictly
    // positive number of bits.
    IntegerLiteralInst *ShiftCount;
    if (match(Other, m_BuiltinInst(BuiltinValueKind::LShr, m_ValueBase(),
                                   m_IntegerLiteralInst(ShiftCount))) &&
        ShiftCount->getValue().isStrictlyPositive()) {
      SILBuilderWithScope B(BI);
      return B.createIntegerLiteral(BI->getLoc(), BI->getType(), APInt(1, 1));
    }
  }

  // At the same time (x>>n) > Int.max and Int.max < (x>>n) is false.
  if (match(BI,
            m_CombineOr(
                // Int.max < x
                m_BuiltinInst(BuiltinValueKind::ICMP_ULT,
                              m_IntegerLiteralInst(IntMax), m_SILValue(Other)),
                // x > Int.max
                m_BuiltinInst(BuiltinValueKind::ICMP_UGT, m_SILValue(Other),
                              m_IntegerLiteralInst(IntMax)),
                // Int.max < x
                m_BuiltinInst(BuiltinValueKind::ICMP_SLT,
                              m_IntegerLiteralInst(IntMax), m_SILValue(Other)),
                // x > Int.max
                m_BuiltinInst(BuiltinValueKind::ICMP_SGT, m_SILValue(Other),
                              m_IntegerLiteralInst(IntMax)))) &&
      IntMax->getValue().isMaxSignedValue()) {
    // Check if other is a result of a logical shift right by a strictly
    // positive number of bits.
    IntegerLiteralInst *ShiftCount;
    if (match(Other, m_BuiltinInst(BuiltinValueKind::LShr, m_ValueBase(),
                                   m_IntegerLiteralInst(ShiftCount))) &&
        ShiftCount->getValue().isStrictlyPositive()) {
      SILBuilderWithScope B(BI);
      return B.createIntegerLiteral(BI->getLoc(), BI->getType(), APInt());
    }
  }

  // Fold x < 0 into false, if x is known to be a result of an unsigned
  // operation with overflow checks enabled.
  BuiltinInst *BIOp;
  if (match(BI, m_BuiltinInst(BuiltinValueKind::ICMP_SLT,
                              m_TupleExtractInst(m_BuiltinInst(BIOp), 0),
                              m_Zero()))) {
    // Check if Other is a result of an unsigned operation with overflow.
    switch (BIOp->getBuiltinInfo().ID) {
    default:
      break;
    case BuiltinValueKind::UAddOver:
    case BuiltinValueKind::USubOver:
    case BuiltinValueKind::UMulOver:
      // Was it an operation with an overflow check?
      if (match(BIOp->getOperand(2), m_One())) {
        SILBuilderWithScope B(BI);
        return B.createIntegerLiteral(BI->getLoc(), BI->getType(), APInt());
      }
    }
  }

  // Fold x >= 0 into true, if x is known to be a result of an unsigned
  // operation with overflow checks enabled.
  if (match(BI, m_BuiltinInst(BuiltinValueKind::ICMP_SGE,
                              m_TupleExtractInst(m_BuiltinInst(BIOp), 0),
                              m_Zero()))) {
    // Check if Other is a result of an unsigned operation with overflow.
    switch (BIOp->getBuiltinInfo().ID) {
    default:
      break;
    case BuiltinValueKind::UAddOver:
    case BuiltinValueKind::USubOver:
    case BuiltinValueKind::UMulOver:
      // Was it an operation with an overflow check?
      if (match(BIOp->getOperand(2), m_One())) {
        SILBuilderWithScope B(BI);
        return B.createIntegerLiteral(BI->getLoc(), BI->getType(), APInt(1, 1));
      }
    }
  }

  return nullptr;
}

static SILInstruction *
constantFoldAndCheckDivision(BuiltinInst *BI, BuiltinValueKind ID,
                             Optional<bool> &ResultsInError) {
  assert(ID == BuiltinValueKind::SDiv ||
         ID == BuiltinValueKind::SRem ||
         ID == BuiltinValueKind::UDiv ||
         ID == BuiltinValueKind::URem);

  OperandValueArrayRef Args = BI->getArguments();
  SILModule &M = BI->getModule();

  // Get the denominator.
  auto *Denom = dyn_cast<IntegerLiteralInst>(Args[1]);
  if (!Denom)
    return nullptr;
  APInt DenomVal = Denom->getValue();

  // If the denominator is zero...
  if (DenomVal == 0) {
    // And if we are not asked to report errors, just return nullptr.
    if (!ResultsInError.hasValue())
      return nullptr;

    // Otherwise emit a diagnosis error and set ResultsInError to true.
    diagnose(M.getASTContext(), BI->getLoc().getSourceLoc(),
             diag::division_by_zero);
    ResultsInError = Optional<bool>(true);
    return nullptr;
  }

  // Get the numerator.
  auto *Num = dyn_cast<IntegerLiteralInst>(Args[0]);
  if (!Num)
    return nullptr;
  APInt NumVal = Num->getValue();

  bool Overflowed;
  APInt ResVal = constantFoldDiv(NumVal, DenomVal, Overflowed, ID);
  
  // If we overflowed...
  if (Overflowed) {
    // And we are not asked to produce diagnostics, just return nullptr...
    if (!ResultsInError.hasValue())
      return nullptr;

    bool IsRem = ID == BuiltinValueKind::SRem || ID == BuiltinValueKind::URem;

    // Otherwise emit the diagnostic, set ResultsInError to be true, and return
    // nullptr.
    diagnose(M.getASTContext(),
             BI->getLoc().getSourceLoc(),
             diag::division_overflow,
             NumVal.toString(/*Radix*/ 10, /*Signed*/true),
             IsRem ? "%" : "/",
             DenomVal.toString(/*Radix*/ 10, /*Signed*/true));
    ResultsInError = Optional<bool>(true);
    return nullptr;
  }

  // Add the literal instruction to represent the result of the division.
  SILBuilderWithScope B(BI);
  return B.createIntegerLiteral(BI->getLoc(), BI->getType(), ResVal);
}

/// \brief Fold binary operations.
///
/// The list of operations we constant fold might not be complete. Start with
/// folding the operations used by the standard library.
static SILInstruction *constantFoldBinary(BuiltinInst *BI,
                                          BuiltinValueKind ID,
                                          Optional<bool> &ResultsInError) {
  switch (ID) {
  default:
    llvm_unreachable("Not all BUILTIN_BINARY_OPERATIONs are covered!");

  // Not supported yet (not easily computable for APInt).
  case BuiltinValueKind::ExactSDiv:
  case BuiltinValueKind::ExactUDiv:
    return nullptr;

  // Not supported now.
  case BuiltinValueKind::FRem:
    return nullptr;

  // Fold constant division operations and report div by zero.
  case BuiltinValueKind::SDiv:
  case BuiltinValueKind::SRem:
  case BuiltinValueKind::UDiv:
  case BuiltinValueKind::URem: {
    return constantFoldAndCheckDivision(BI, ID, ResultsInError);
  }

  // Are there valid uses for these in stdlib?
  case BuiltinValueKind::Add:
  case BuiltinValueKind::Mul:
  case BuiltinValueKind::Sub:
    return nullptr;

  case BuiltinValueKind::And:
  case BuiltinValueKind::AShr:
  case BuiltinValueKind::LShr:
  case BuiltinValueKind::Or:
  case BuiltinValueKind::Shl:
  case BuiltinValueKind::Xor: {
    OperandValueArrayRef Args = BI->getArguments();
    auto *LHS = dyn_cast<IntegerLiteralInst>(Args[0]);
    auto *RHS = dyn_cast<IntegerLiteralInst>(Args[1]);
    if (!RHS || !LHS)
      return nullptr;
    APInt LHSI = LHS->getValue();
    APInt RHSI = RHS->getValue();

    bool IsShift = ID == BuiltinValueKind::AShr ||
                   ID == BuiltinValueKind::LShr ||
                   ID == BuiltinValueKind::Shl;

    // Reject shifting all significant bits
    if (IsShift && RHSI.getZExtValue() >= LHSI.getBitWidth()) {
      diagnose(BI->getModule().getASTContext(),
               RHS->getLoc().getSourceLoc(),
               diag::shifting_all_significant_bits);

      ResultsInError = Optional<bool>(true);
      return nullptr;
    }

    APInt ResI = constantFoldBitOperation(LHSI, RHSI, ID);
    // Add the literal instruction to represent the result.
    SILBuilderWithScope B(BI);
    return B.createIntegerLiteral(BI->getLoc(), BI->getType(), ResI);
  }
  case BuiltinValueKind::FAdd:
  case BuiltinValueKind::FDiv:
  case BuiltinValueKind::FMul:
  case BuiltinValueKind::FSub: {
    OperandValueArrayRef Args = BI->getArguments();
    auto *LHS = dyn_cast<FloatLiteralInst>(Args[0]);
    auto *RHS = dyn_cast<FloatLiteralInst>(Args[1]);
    if (!RHS || !LHS)
      return nullptr;
    APFloat LHSF = LHS->getValue();
    APFloat RHSF = RHS->getValue();
    switch (ID) {
    default: llvm_unreachable("Not all cases are covered!");
    case BuiltinValueKind::FAdd:
      LHSF.add(RHSF, APFloat::rmNearestTiesToEven);
      break;
    case BuiltinValueKind::FDiv:
      LHSF.divide(RHSF, APFloat::rmNearestTiesToEven);
      break;
    case BuiltinValueKind::FMul:
      LHSF.multiply(RHSF, APFloat::rmNearestTiesToEven);
      break;
    case BuiltinValueKind::FSub:
      LHSF.subtract(RHSF, APFloat::rmNearestTiesToEven);
      break;
    }

    // Add the literal instruction to represent the result.
    SILBuilderWithScope B(BI);
    return B.createFloatLiteral(BI->getLoc(), BI->getType(), LHSF);
  }
  }
}

static std::pair<bool, bool> getTypeSignedness(const BuiltinInfo &Builtin) {
  bool SrcTySigned =
  (Builtin.ID == BuiltinValueKind::SToSCheckedTrunc ||
   Builtin.ID == BuiltinValueKind::SToUCheckedTrunc ||
   Builtin.ID == BuiltinValueKind::SUCheckedConversion);

  bool DstTySigned =
  (Builtin.ID == BuiltinValueKind::SToSCheckedTrunc ||
   Builtin.ID == BuiltinValueKind::UToSCheckedTrunc ||
   Builtin.ID == BuiltinValueKind::USCheckedConversion);

  return std::pair<bool, bool>(SrcTySigned, DstTySigned);
}

static SILInstruction *
constantFoldAndCheckIntegerConversions(BuiltinInst *BI,
                                       const BuiltinInfo &Builtin,
                                       Optional<bool> &ResultsInError) {
  assert(Builtin.ID == BuiltinValueKind::SToSCheckedTrunc ||
         Builtin.ID == BuiltinValueKind::UToUCheckedTrunc ||
         Builtin.ID == BuiltinValueKind::SToUCheckedTrunc ||
         Builtin.ID == BuiltinValueKind::UToSCheckedTrunc ||
         Builtin.ID == BuiltinValueKind::SUCheckedConversion ||
         Builtin.ID == BuiltinValueKind::USCheckedConversion);

  // Check if we are converting a constant integer.
  OperandValueArrayRef Args = BI->getArguments();
  auto *V = dyn_cast<IntegerLiteralInst>(Args[0]);
  if (!V)
    return nullptr;
  APInt SrcVal = V->getValue();

  // Get source type and bit width.
  Type SrcTy = Builtin.Types[0];
  uint32_t SrcBitWidth =
    Builtin.Types[0]->castTo<BuiltinIntegerType>()->getGreatestWidth();

  // Compute the destination (for SrcBitWidth < DestBitWidth) and enough info
  // to check for overflow.
  APInt Result;
  bool OverflowError;
  Type DstTy;

  // Process conversions signed <-> unsigned for same size integers.
  if (Builtin.ID == BuiltinValueKind::SUCheckedConversion ||
      Builtin.ID == BuiltinValueKind::USCheckedConversion) {
    DstTy = SrcTy;
    Result = SrcVal;
    // Report an error if the sign bit is set.
    OverflowError = SrcVal.isNegative();

  // Process truncation from unsigned to signed.
  } else if (Builtin.ID != BuiltinValueKind::UToSCheckedTrunc) {
    assert(Builtin.Types.size() == 2);
    DstTy = Builtin.Types[1];
    uint32_t DstBitWidth =
      DstTy->castTo<BuiltinIntegerType>()->getGreatestWidth();
    //     Result = trunc_IntFrom_IntTo(Val)
    //   For signed destination:
    //     sext_IntFrom(Result) == Val ? Result : overflow_error
    //   For signed destination:
    //     zext_IntFrom(Result) == Val ? Result : overflow_error
    Result = SrcVal.trunc(DstBitWidth);
    // Get the signedness of the destination.
    bool Signed = (Builtin.ID == BuiltinValueKind::SToSCheckedTrunc);
    APInt Ext = Signed ? Result.sext(SrcBitWidth) : Result.zext(SrcBitWidth);
    OverflowError = (SrcVal != Ext);

  // Process the rest of truncations.
  } else {
    assert(Builtin.Types.size() == 2);
    DstTy = Builtin.Types[1];
    uint32_t DstBitWidth =
      Builtin.Types[1]->castTo<BuiltinIntegerType>()->getGreatestWidth();
    // Compute the destination (for SrcBitWidth < DestBitWidth):
    //   Result = trunc_IntTo(Val)
    //   Trunc  = trunc_'IntTo-1bit'(Val)
    //   zext_IntFrom(Trunc) == Val ? Result : overflow_error
    Result = SrcVal.trunc(DstBitWidth);
    APInt TruncVal = SrcVal.trunc(DstBitWidth - 1);
    OverflowError = (SrcVal != TruncVal.zext(SrcBitWidth));
  }

  // Check for overflow.
  if (OverflowError) {
    // If we are not asked to emit overflow diagnostics, just return nullptr on
    // overflow.
    if (!ResultsInError.hasValue())
      return nullptr;

    SILLocation Loc = BI->getLoc();
    SILModule &M = BI->getModule();
    const ApplyExpr *CE = Loc.getAsASTNode<ApplyExpr>();
    Type UserSrcTy;
    Type UserDstTy;
    // Primitive heuristics to get the user-written type.
    // Eventually we might be able to use SILLocation (when it contains info
    // about inlined call chains).
    if (CE) {
      if (const TupleType *RTy = CE->getArg()->getType()->getAs<TupleType>()) {
        if (RTy->getNumElements() == 1) {
          UserSrcTy = RTy->getElementType(0);
          UserDstTy = CE->getType();
        }
      } else {
        UserSrcTy = CE->getArg()->getType();
        UserDstTy = CE->getType();
      }
    }
    
 
    // Assume that we are converting from a literal if the Source size is
    // 2048. Is there a better way to identify conversions from literals?
    bool Literal = (SrcBitWidth == 2048);

    // FIXME: This will prevent hard error in cases the error is coming
    // from ObjC interoperability code. Currently, we treat NSUInteger as
    // Int.
    if (Loc.getSourceLoc().isInvalid()) {
      // Otherwise emit the appropriate diagnostic and set ResultsInError.
      if (Literal)
        diagnose(M.getASTContext(), Loc.getSourceLoc(),
                 diag::integer_literal_overflow_warn,
                 UserDstTy.isNull() ? DstTy : UserDstTy);
      else
        diagnose(M.getASTContext(), Loc.getSourceLoc(),
                 diag::integer_conversion_overflow_warn,
                 UserSrcTy.isNull() ? SrcTy : UserSrcTy,
                 UserDstTy.isNull() ? DstTy : UserDstTy);

      ResultsInError = Optional<bool>(true);
      return nullptr;
    }

    // Otherwise report the overflow error.
    if (Literal) {
      bool SrcTySigned, DstTySigned;
      std::tie(SrcTySigned, DstTySigned) = getTypeSignedness(Builtin);
      SmallString<10> SrcAsString;
      SrcVal.toString(SrcAsString, /*radix*/10, SrcTySigned);

      // Try to print user-visible types if they are available.
      if (!UserDstTy.isNull()) {
        auto diagID = diag::integer_literal_overflow;
        
        // If this is a negative literal in an unsigned type, use a specific
        // diagnostic.
        if (SrcTySigned && !DstTySigned && SrcVal.isNegative())
          diagID = diag::negative_integer_literal_overflow_unsigned;
        
        diagnose(M.getASTContext(), Loc.getSourceLoc(),
                 diagID, UserDstTy, SrcAsString);
      // Otherwise, print the Builtin Types.
      } else {
        bool SrcTySigned, DstTySigned;
        std::tie(SrcTySigned, DstTySigned) = getTypeSignedness(Builtin);
        diagnose(M.getASTContext(), Loc.getSourceLoc(),
                 diag::integer_literal_overflow_builtin_types,
                 DstTySigned, DstTy, SrcAsString);
      }
    } else {
      if (Builtin.ID == BuiltinValueKind::SUCheckedConversion) {
        diagnose(M.getASTContext(), Loc.getSourceLoc(),
                 diag::integer_conversion_sign_error,
                 UserDstTy.isNull() ? DstTy : UserDstTy);
      } else {
        // Try to print user-visible types if they are available.
        if (!UserSrcTy.isNull()) {
          diagnose(M.getASTContext(), Loc.getSourceLoc(),
                   diag::integer_conversion_overflow,
                   UserSrcTy, UserDstTy);

        // Otherwise, print the Builtin Types.
        } else {
          // Since builtin types are sign-agnostic, print the signedness
          // separately.
          bool SrcTySigned, DstTySigned;
          std::tie(SrcTySigned, DstTySigned) = getTypeSignedness(Builtin);
          diagnose(M.getASTContext(), Loc.getSourceLoc(),
                   diag::integer_conversion_overflow_builtin_types,
                   SrcTySigned, SrcTy, DstTySigned, DstTy);
        }
      }
    }

    ResultsInError = Optional<bool>(true);
    return nullptr;
  }

  // The call to the builtin should be replaced with the constant value.
  return constructResultWithOverflowTuple(BI, Result, false);

}

static SILInstruction *constantFoldBuiltin(BuiltinInst *BI,
                                           Optional<bool> &ResultsInError) {
  const IntrinsicInfo &Intrinsic = BI->getIntrinsicInfo();
  SILModule &M = BI->getModule();

  // If it's an llvm intrinsic, fold the intrinsic.
  if (Intrinsic.ID != llvm::Intrinsic::not_intrinsic)
    return constantFoldIntrinsic(BI, Intrinsic.ID, ResultsInError);

  // Otherwise, it should be one of the builtin functions.
  OperandValueArrayRef Args = BI->getArguments();
  const BuiltinInfo &Builtin = BI->getBuiltinInfo();

  switch (Builtin.ID) {
  default: break;

// Check and fold binary arithmetic with overflow.
#define BUILTIN(id, name, Attrs)
#define BUILTIN_BINARY_OPERATION_WITH_OVERFLOW(id, name, _, attrs, overload) \
  case BuiltinValueKind::id:
#include "swift/AST/Builtins.def"
    return constantFoldBinaryWithOverflow(BI, Builtin.ID, ResultsInError);

#define BUILTIN(id, name, Attrs)
#define BUILTIN_BINARY_OPERATION(id, name, attrs, overload) \
case BuiltinValueKind::id:
#include "swift/AST/Builtins.def"
      return constantFoldBinary(BI, Builtin.ID, ResultsInError);

// Fold comparison predicates.
#define BUILTIN(id, name, Attrs)
#define BUILTIN_BINARY_PREDICATE(id, name, attrs, overload) \
case BuiltinValueKind::id:
#include "swift/AST/Builtins.def"
      return constantFoldCompare(BI, Builtin.ID);

  case BuiltinValueKind::Trunc:
  case BuiltinValueKind::ZExt:
  case BuiltinValueKind::SExt:
  case BuiltinValueKind::TruncOrBitCast:
  case BuiltinValueKind::ZExtOrBitCast:
  case BuiltinValueKind::SExtOrBitCast: {

    // We can fold if the value being cast is a constant.
    auto *V = dyn_cast<IntegerLiteralInst>(Args[0]);
    if (!V)
      return nullptr;

    APInt CastResV = constantFoldCast(V->getValue(), Builtin);

    // Add the literal instruction to represent the result of the cast.
    SILBuilderWithScope B(BI);
    return B.createIntegerLiteral(BI->getLoc(), BI->getType(), CastResV);
  }

  // Process special builtins that are designed to check for overflows in
  // integer conversions.
  case BuiltinValueKind::SToSCheckedTrunc:
  case BuiltinValueKind::UToUCheckedTrunc:
  case BuiltinValueKind::SToUCheckedTrunc:
  case BuiltinValueKind::UToSCheckedTrunc:
  case BuiltinValueKind::SUCheckedConversion:
  case BuiltinValueKind::USCheckedConversion: {
    return constantFoldAndCheckIntegerConversions(BI, Builtin, ResultsInError);
  }

  case BuiltinValueKind::IntToFPWithOverflow: {
    // Get the value. It should be a constant in most cases.
    // Note, this will not always be a constant, for example, when analyzing
    // _convertFromBuiltinIntegerLiteral function itself.
    auto *V = dyn_cast<IntegerLiteralInst>(Args[0]);
    if (!V)
      return nullptr;
    APInt SrcVal = V->getValue();
    Type DestTy = Builtin.Types[1];

    APFloat TruncVal(
        DestTy->castTo<BuiltinFloatType>()->getAPFloatSemantics());
    APFloat::opStatus ConversionStatus = TruncVal.convertFromAPInt(
        SrcVal, /*IsSigned=*/true, APFloat::rmNearestTiesToEven);

    SILLocation Loc = BI->getLoc();
    const ApplyExpr *CE = Loc.getAsASTNode<ApplyExpr>();

    // Check for overflow.
    if (ConversionStatus & APFloat::opOverflow) {
      // If we overflow and are not asked for diagnostics, just return nullptr.
      if (!ResultsInError.hasValue())
        return nullptr;

      SmallString<10> SrcAsString;
      SrcVal.toString(SrcAsString, /*radix*/10, true /*isSigned*/);
      
      // Otherwise emit our diagnostics and then return nullptr.
      diagnose(M.getASTContext(), Loc.getSourceLoc(),
               diag::integer_literal_overflow,
               CE ? CE->getType() : DestTy, SrcAsString);
      ResultsInError = Optional<bool>(true);
      return nullptr;
    }

    // The call to the builtin should be replaced with the constant value.
    SILBuilderWithScope B(BI);
    return B.createFloatLiteral(Loc, BI->getType(), TruncVal);
  }

  case BuiltinValueKind::FPTrunc: {
    // Get the value. It should be a constant in most cases.
    auto *V = dyn_cast<FloatLiteralInst>(Args[0]);
    if (!V)
      return nullptr;
    APFloat TruncVal = V->getValue();
    Type DestTy = Builtin.Types[1];
    bool losesInfo;
    APFloat::opStatus ConversionStatus = TruncVal.convert(
        DestTy->castTo<BuiltinFloatType>()->getAPFloatSemantics(),
        APFloat::rmNearestTiesToEven, &losesInfo);
    SILLocation Loc = BI->getLoc();

    // Check if conversion was successful.
    if (ConversionStatus != APFloat::opStatus::opOK &&
        ConversionStatus != APFloat::opStatus::opInexact) {
      return nullptr;
    }

    // The call to the builtin should be replaced with the constant value.
    SILBuilderWithScope B(BI);
    return B.createFloatLiteral(Loc, BI->getType(), TruncVal);
  }

  case BuiltinValueKind::AssumeNonNegative: {
    auto *V = dyn_cast<IntegerLiteralInst>(Args[0]);
    if (!V)
      return nullptr;

    APInt VInt = V->getValue();
    if (VInt.isNegative() && ResultsInError.hasValue()) {
      diagnose(M.getASTContext(), BI->getLoc().getSourceLoc(),
               diag::wrong_non_negative_assumption,
               VInt.toString(/*Radix*/ 10, /*Signed*/ true));
      ResultsInError = Optional<bool>(true);
    }
    return V;
  }
  }
  return nullptr;
}

static SILValue constantFoldInstruction(SILInstruction &I,
                                        Optional<bool> &ResultsInError) {
  // Constant fold function calls.
  if (auto *BI = dyn_cast<BuiltinInst>(&I)) {
    return constantFoldBuiltin(BI, ResultsInError);
  }

  // Constant fold extraction of a constant element.
  if (auto *TEI = dyn_cast<TupleExtractInst>(&I)) {
    if (auto *TheTuple = dyn_cast<TupleInst>(TEI->getOperand()))
      return TheTuple->getElement(TEI->getFieldNo());
  }

  // Constant fold extraction of a constant struct element.
  if (auto *SEI = dyn_cast<StructExtractInst>(&I)) {
    if (auto *Struct = dyn_cast<StructInst>(SEI->getOperand()))
      return Struct->getOperandForField(SEI->getField())->get();
  }

  // Constant fold indexing insts of a 0 integer literal.
  if (auto *II = dyn_cast<IndexingInst>(&I))
    if (auto *IntLiteral = dyn_cast<IntegerLiteralInst>(II->getIndex()))
      if (!IntLiteral->getValue())
        return II->getBase();

  return SILValue();
}

static bool isApplyOfBuiltin(SILInstruction &I, BuiltinValueKind kind) {
  if (auto *BI = dyn_cast<BuiltinInst>(&I))
    if (BI->getBuiltinInfo().ID == kind)
      return true;
  return false;
}

static bool isApplyOfStringConcat(SILInstruction &I) {
  if (auto *AI = dyn_cast<ApplyInst>(&I))
    if (auto *Fn = AI->getReferencedFunction())
      if (Fn->hasSemanticsAttr("string.concat"))
        return true;
  return false;
}

bool ConstantPropagation::isApplyOfIntegerInit(SILInstruction &I) {
  if (auto *AI = dyn_cast<ApplyInst>(&I))
    if (auto *Fn = AI->getReferencedFunction())
      if (Fn == SignedIntegerInitFn || Fn == UnsignedIntegerInitFn)
        return true;
  return false;
}

static bool isFoldable(SILInstruction *I) {
  return isa<IntegerLiteralInst>(I) || isa<FloatLiteralInst>(I);
}

static bool
constantFoldStringConcatenation(ApplyInst *AI,
                                llvm::SetVector<SILInstruction *> &WorkList) {
  SILBuilder B(AI);
  // Try to apply the string literal concatenation optimization.
  auto *Concatenated = tryToConcatenateStrings(AI, B);
  // Bail if string literal concatenation could not be performed.
  if (!Concatenated)
    return false;

  // Replace all uses of the old instruction by a new instruction.
  AI->replaceAllUsesWith(Concatenated);

  auto RemoveCallback = [&](SILInstruction *DeadI) { WorkList.remove(DeadI); };
  // Remove operands that are not used anymore.
  // Even if they are apply_inst, it is safe to
  // do so, because they can only be applies
  // of functions annotated as string.utf16
  // or string.utf16.
  for (auto &Op : AI->getAllOperands()) {
    SILValue Val = Op.get();
    Op.drop();
    if (Val->use_empty()) {
      auto *DeadI = dyn_cast<SILInstruction>(Val);
      recursivelyDeleteTriviallyDeadInstructions(DeadI, /*force*/ true,
                                                 RemoveCallback);
      WorkList.remove(DeadI);
    }
  }
  // Schedule users of the new instruction for constant folding.
  // We only need to schedule the string.concat invocations.
  for (auto AIUse : Concatenated->getUses()) {
    if (isApplyOfStringConcat(*AIUse->getUser())) {
      WorkList.insert(AIUse->getUser());
    }
  }
  // Delete the old apply instruction.
  recursivelyDeleteTriviallyDeadInstructions(AI, /*force*/ true,
                                             RemoveCallback);
  return true;
}

static IntegerLiteralInst *getIntegerLiteralFromConstructorCall(ApplyInst *AI) {
  auto Op = AI->getArgument(1);
  // This operand is suppsed to be an alloc_stack instruction.
  if (!isa<AllocStackInst>(Op))
    return nullptr;
  // Check that this is an alloc_stack, which is assigned only once.
  StoreInst *SI = nullptr;
  for (auto U : getNonDebugUses(Op)) {
    auto User = U->getUser();
    if (isa<DeallocStackInst>(User))
      continue;
    if (User == AI)
      continue;
    if (!SI) {
      // This is something we cannot handle.
      SI = dyn_cast<StoreInst>(User);
      if (SI && SI->getDest() == Op)
        continue;
    }
    // This is something we cannot handle.
    return nullptr;
  }
  // Bail if there was no assignment.
  if (!SI)
    return nullptr;
  // Bail if it was not an assignment of an integer literal.
  auto StructI = dyn_cast<StructInst>(SI->getSrc());
  if (!StructI)
    return nullptr;
  return dyn_cast<IntegerLiteralInst>(StructI->getOperand(0));
}

static IntegerLiteralExpr *
getIntegerLiteralExprFromConstructorCall(ApplyInst *AI) {
  auto Loc = AI->getLoc();
  const ApplyExpr *Apply = Loc.getAsASTNode<ApplyExpr>();
  const Expr *ValueExpr = Apply->getArg();
  if (auto *Paren = dyn_cast<ParenExpr>(ValueExpr))
    ValueExpr = Paren->getSubExpr();
  auto *CallExp = dyn_cast<CallExpr>(ValueExpr);
  if (CallExp) {
    auto Callee = CallExp->getFn();
    if (isa<ConstructorRefCallExpr>(Callee) &&
        CallExp->getNumArguments() == 1) {
      // Check if takes an integer literal as parameter.
      auto Arg = CallExp->getArg();
      auto ArgLabels = CallExp->getArgumentLabels();
      if (ArgLabels.size() == 1 &&
          ArgLabels[0].str() == "_builtinIntegerLiteral") {
        if (auto TupleExp = dyn_cast<TupleExpr>(Arg)) {
          return dyn_cast<IntegerLiteralExpr>(TupleExp->getElement(0));
        }
      }
    }
  }
  return nullptr;
}

/// Special handling of the generic initializers provided by
/// UnsignedInteger and SignedInteger protocols when they are
/// invoked for fixed size builtin integer types and constant
/// arguments.
///
/// Check a call of an initializer for one of the builtin integer types.
/// Try to determine if the initializer takes an integer constant
/// as an argument and if this constant overflows/underflows the values
/// representable by this type.
///
/// The initializer in question is one of:
/// UnsignedInteger.init<T : BinaryInteger>(_ source: T), or
/// SignedInteger.init<T : BinaryInteger>(_ source: T)
///
/// A typical invocation of such an initializer looks in SIL like this:
///
///   // function_ref UnsignedInteger<A>.init<A>(_:)
///   %init = function_ref @init_name: $@convention(method)
///     <τ_0_0 where τ_0_0 : FixedWidthInteger, τ_0_0 : UnsignedInteger>
///     <τ_1_0 where τ_1_0 : BinaryInteger>
///     (@in τ_1_0, @thick τ_0_0.Type) -> @out τ_0_0
///   %int_lit = integer_literal $Builtin.Int64, 400
///   %int_val = struct $Int (%int_lit : $Builtin.Int64)
///   %result = alloc_stack $UInt8
///   %tmp = alloc_stack $Int
///   %ty = metatype $@thick UInt8.Type
///   store %int_val to %tmp : $*Int
///   apply %init<UInt8, Int>(%result, %tmp, %ty) : ...
///   dealloc_stack %tmp : $*Int
bool ConstantPropagation::checkIntegerInitializer(
    ApplyInst *AI, llvm::SetVector<SILInstruction *> &WorkList) {
  auto &Ctx = AI->getModule().getASTContext();

  assert(AI->getSubstitutions().size() == 2);


  // First check source and destination types.
  auto GetStdlibIntTypeInfo = [](CanType Ty, unsigned &TySize,
                                 bool &TySigned) -> bool {
    auto NominalTy = Ty->getAnyNominal();
    if (!NominalTy)
      return false;
    // The DstTy should be from the standard library.
    if (!NominalTy->getParentModule()->isStdlibModule())
      return false;

    StringRef TyName = NominalTy->getNameStr();
    assert(TyName.startswith("Int") || TyName.startswith("UInt"));
    TySigned = TyName.startswith("Int");

    // Find the size of the target type.
    StringRef SizeStr = TyName.drop_until(
      [](char c) -> bool { return (c >= '0' && c <= '9'); });
    unsigned long long Size = 0;
    if (!SizeStr.empty() && llvm::getAsUnsignedInteger(SizeStr, 10, Size))
      return false;
    TySize = (unsigned)Size;
    return true;
  };

  auto DstTy =
      AI->getSubstitutions()[0].getReplacement()->getCanonicalType();
  unsigned DstTySize = 0;
  bool DstTySigned = false;
  if (!GetStdlibIntTypeInfo(DstTy, DstTySize, DstTySigned) || !DstTySize)
    return false;


  auto SrcTy =
    AI->getSubstitutions()[1].getReplacement()->getCanonicalType();
  unsigned SrcTySize = 0;
  bool SrcTySigned = false;
  if (!GetStdlibIntTypeInfo(SrcTy, SrcTySize, SrcTySigned))
    return false;

  // Try to get the actual integer literal.
  auto LI = getIntegerLiteralFromConstructorCall(AI);
  if (!LI)
    return false;
  APInt V = LI->getValue();

  // If no explicit size is known from the type, use the size
  // of the constant value to determine it.
  if (SrcTySize == 0)
    SrcTySize = V.getBitWidth();

  assert(SrcTySize == V.getBitWidth());

  // If the source type is unsigned, extend the value of the literal,
  // so that it becomes a non-negative in its APInt representation.
  if (!SrcTySigned) {
    V = V.zext(SrcTySize + 1);
  }

  // Try to find if the argument of the integer constructor is an integer
  // literal or an expression.
  auto *IntLitExp = getIntegerLiteralExprFromConstructorCall(AI);

  // Conversion of negative values to unsigned types is not allowed.
  if (!DstTySigned && V.isNegative()) {
    SmallString<10> SrcAsString;
    V.toString(SrcAsString, /*radix*/10, true);

    if (IntLitExp && IntLitExp->getLoc().isValid()) {
      diagnose(Ctx, IntLitExp->getLoc(),
               diag::negative_integer_literal_overflow_unsigned,
               AI->getSubstitutions()[0].getReplacement(), SrcAsString);
      return false;
    }

    diagnose(Ctx, AI->getLoc().getSourceLoc(),
             diag::integer_conversion_sign_error,
             AI->getSubstitutions()[0].getReplacement());
    return false;
  }

  // OK, now we know the literal value.
  // Check if it is within a range allowed by the type.
  if ((DstTySigned && !V.isSignedIntN(DstTySize)) ||
      (!DstTySigned && !V.isIntN(DstTySize))) {

    SmallString<10> SrcAsString;
    V.toString(SrcAsString, /*radix*/10, true);

    if (IntLitExp && IntLitExp->getLoc().isValid()) {
      diagnose(Ctx, IntLitExp->getLoc(),
               diag::integer_literal_overflow_builtin_types, DstTySigned,
               AI->getSubstitutions()[0].getReplacement(), SrcAsString);
      return false;
    }

    diagnose(Ctx, AI->getLoc().getSourceLoc(),
             diag::integer_conversion_overflow,
             SrcTy, AI->getSubstitutions()[0].getReplacement());
    return false;
  }

  // The constant value is in a domain allowed by the DstTy.

  // This is where the initialized value will be stored. This can
  // be not only the alloc_stack but e.g. an element inside of the array, etc.
  auto ResultAddr = AI->getArgument(0);

  // Check that this memory location is assigned just once (as an @out parameter
  // of this initializer call).
  bool OptimizeLoadsOfConst = isa<AllocStackInst>(ResultAddr);
  if (OptimizeLoadsOfConst) {
    for (auto Use : ResultAddr->getUses()) {
      auto User = Use->getUser();
      if (User == AI)
        continue;
      if (isa<StoreInst>(User) || isa<ApplyInst>(User)) {
        OptimizeLoadsOfConst = false;
        break;
      }
    }
  }

  // Replace this generic constructor call with a simple integer literal of a
  // proper size.
  SILBuilderWithScope B(LI);
  auto NewIL = B.createIntegerLiteral(
      AI->getLoc(), SILType::getBuiltinIntegerType(
                        DstTySize, AI->getModule().getASTContext()),
      DstTySigned ? V.sextOrTrunc(DstTySize) : V.zextOrTrunc(DstTySize));
  auto NewStructI = B.createStruct(
      AI->getLoc(), SILType::getPrimitiveObjectType(DstTy), {NewIL});

  // Store into the ResultAddr.
  B.setInsertionPoint(AI);
  B.createStore(AI->getLoc(), NewStructI, ResultAddr,
                StoreOwnershipQualifier::Unqualified);

  auto RemoveCallback = [&](SILInstruction *DeadI) { WorkList.remove(DeadI); };

  if (OptimizeLoadsOfConst) {
    // Replace all uses of the original result.
    for (auto U = ResultAddr->use_begin(), E = ResultAddr->use_end(); U != E;) {
      auto *User = U->getUser();
      auto *LoadI = dyn_cast<LoadInst>(User);
      ++U;
      if (!LoadI)
        continue;
      LoadI->replaceAllUsesWith(NewStructI);
      RemoveCallback(LoadI);
      LoadI->eraseFromParent();
    }
  }

  // Replace all instructions reading from the struct conatining the integer by
  // the actual value.
  for (auto U = NewStructI->use_begin(), E = NewStructI->use_end(); U != E;) {
    auto *User = U->getUser();
    ++U;
    auto *SEI = dyn_cast<StructExtractInst>(User);
    if (!SEI)
      continue;
    SEI->replaceAllUsesWith(NewIL);
  }

  // Schedule all uses of the new integer struct for re-scanning.
  for (auto Use : NewStructI->getUses()) {
    WorkList.insert(Use->getUser());
  }

  // Schedule all uses of the new constant for re-scanning.
  for (auto Use : NewIL->getUses()) {
    WorkList.insert(Use->getUser());
  }

  auto Op = AI->getArgument(1);

  // Delete the old apply instruction and other unused instructions.
  recursivelyDeleteTriviallyDeadInstructions(AI, /*force*/ true,
                                             RemoveCallback);

  // Delete any dead uses of the original constructor argument now.
  for (auto U = Op->use_begin(), E = Op->use_end(); U != E;) {
    auto *User = U->getUser();
    ++U;
    if (User == NewStructI)
      continue;
    recursivelyDeleteTriviallyDeadInstructions(User, /*force*/ true,
                                               RemoveCallback);
  }

  return true;
}

/// Initialize the worklist to all of the constant instructions.
void ConstantPropagation::initializeWorklist(
    SILFunction &F, bool InstantiateAssertConfiguration,
    llvm::SetVector<SILInstruction *> &WorkList) {
  for (auto &BB : F) {
    for (auto &I : BB) {
      if (isFoldable(&I) && !I.use_empty()) {
        WorkList.insert(&I);
        continue;
      }

      if (InstantiateAssertConfiguration &&
          (isApplyOfBuiltin(I, BuiltinValueKind::AssertConf) ||
           isApplyOfBuiltin(I, BuiltinValueKind::CondUnreachable))) {
        WorkList.insert(&I);
        continue;
      }

      if (isa<CheckedCastBranchInst>(&I) ||
          isa<CheckedCastAddrBranchInst>(&I) ||
          isa<UnconditionalCheckedCastInst>(&I) ||
          isa<UnconditionalCheckedCastAddrInst>(&I)) {
        WorkList.insert(&I);
        continue;
      }

      if (!isApplyOfStringConcat(I) && !isApplyOfIntegerInit(I)) {
        continue;
      }
      WorkList.insert(&I);
    }
  }
}

SILAnalysis::InvalidationKind
ConstantPropagation::processFunction(SILFunction &F, bool EnableDiagnostics,
                                     unsigned AssertConfiguration) {
  DEBUG(llvm::dbgs() << "*** ConstPropagation processing: " << F.getName()
        << "\n");

  // This is the list of traits that this transformation might preserve.
  bool InvalidateBranches = false;
  bool InvalidateCalls = false;
  bool InvalidateInstructions = false;

  // Should we replace calls to assert_configuration by the assert
  // configuration.
  bool InstantiateAssertConfiguration =
      (AssertConfiguration != SILOptions::DisableReplacement);

  // The list of instructions whose evaluation resulted in error or warning.
  // This is used to avoid duplicate error reporting in case we reach the same
  // instruction from different entry points in the WorkList.
  llvm::DenseSet<SILInstruction *> ErrorSet;

  // The worklist of the constants that could be folded into their users.
  llvm::SetVector<SILInstruction *> WorkList;
  initializeWorklist(F, InstantiateAssertConfiguration, WorkList);

  llvm::SetVector<SILInstruction *> FoldedUsers;
  CastOptimizer CastOpt(
      [&](SILInstruction *I, ValueBase *V) { /* ReplaceInstUsesAction */

        InvalidateInstructions = true;
        I->replaceAllUsesWith(V);
      },
      [&](SILInstruction *I) { /* EraseAction */
        auto *TI = dyn_cast<TermInst>(I);

        if (TI) {
          // Invalidate analysis information related to branches. Replacing
          // unconditional_check_branch type instructions by a trap will also
          // invalidate branches/the CFG.
          InvalidateBranches = true;
        }

        InvalidateInstructions = true;

        WorkList.remove(I);
        I->eraseFromParent();
      });

  while (!WorkList.empty()) {
    SILInstruction *I = WorkList.pop_back_val();
    assert(I->getParent() && "SILInstruction must have parent.");

    DEBUG(llvm::dbgs() << "Visiting: " << *I);

    // Replace assert_configuration instructions by their constant value. We
    // want them to be replace even if we can't fully propagate the constant.
    if (InstantiateAssertConfiguration)
      if (auto *BI = dyn_cast<BuiltinInst>(I)) {
        if (isApplyOfBuiltin(*BI, BuiltinValueKind::AssertConf)) {
          // Instantiate the constant.
          SILBuilderWithScope B(BI);
          auto AssertConfInt = B.createIntegerLiteral(
            BI->getLoc(), BI->getType(), AssertConfiguration);
          BI->replaceAllUsesWith(AssertConfInt);
          // Schedule users for constant folding.
          WorkList.insert(AssertConfInt);
          // Delete the call.
          recursivelyDeleteTriviallyDeadInstructions(BI);

          InvalidateInstructions = true;
          continue;
        }

        // Kill calls to conditionallyUnreachable if we've folded assert
        // configuration calls.
        if (isApplyOfBuiltin(*BI, BuiltinValueKind::CondUnreachable)) {
          assert(BI->use_empty() && "use of conditionallyUnreachable?!");
          recursivelyDeleteTriviallyDeadInstructions(BI, /*force*/ true);
          InvalidateInstructions = true;
          continue;
        }
      }

    if (auto *AI = dyn_cast<ApplyInst>(I)) {
      // An apply may come from a string.concat invocation or from integer
      // initializer.
      if (isApplyOfIntegerInit(*I)) {
        if (checkIntegerInitializer(AI, WorkList)) {
          // Invalidate all analysis that's related to the call graph.
          InvalidateInstructions = true;
        }
        continue;
      }

      if (isApplyOfStringConcat(*I)) {
        if (constantFoldStringConcatenation(AI, WorkList)) {
          // Invalidate all analysis that's related to the call graph.
          InvalidateInstructions = true;
        }
        continue;
      }
    }

    if (isa<CheckedCastBranchInst>(I) || isa<CheckedCastAddrBranchInst>(I) ||
        isa<UnconditionalCheckedCastInst>(I) ||
        isa<UnconditionalCheckedCastAddrInst>(I)) {
      // Try to perform cast optimizations. Invalidation is handled by a
      // callback inside the cast optimizer.
      ValueBase *Result = nullptr;
      switch(I->getKind()) {
      default:
        llvm_unreachable("Unexpected instruction for cast optimizations");
      case ValueKind::CheckedCastBranchInst:
        Result = CastOpt.simplifyCheckedCastBranchInst(cast<CheckedCastBranchInst>(I));
        break;
      case ValueKind::CheckedCastAddrBranchInst:
        Result = CastOpt.simplifyCheckedCastAddrBranchInst(cast<CheckedCastAddrBranchInst>(I));
        break;
      case ValueKind::UnconditionalCheckedCastInst:
        Result = CastOpt.optimizeUnconditionalCheckedCastInst(cast<UnconditionalCheckedCastInst>(I));
        break;
      case ValueKind::UnconditionalCheckedCastAddrInst:
        Result = CastOpt.optimizeUnconditionalCheckedCastAddrInst(cast<UnconditionalCheckedCastAddrInst>(I));
        break;
      }

      if (Result) {
        if (isa<CheckedCastBranchInst>(Result) ||
            isa<CheckedCastAddrBranchInst>(Result) ||
            isa<UnconditionalCheckedCastInst>(Result) ||
            isa<UnconditionalCheckedCastAddrInst>(Result))
          WorkList.insert(cast<SILInstruction>(Result));
      }
      continue;
    }


    // Go through all users of the constant and try to fold them.
    FoldedUsers.clear();
    for (auto Use : I->getUses()) {
      SILInstruction *User = Use->getUser();
      DEBUG(llvm::dbgs() << "    User: " << *User);

      // It is possible that we had processed this user already. Do not try
      // to fold it again if we had previously produced an error while folding
      // it.  It is not always possible to fold an instruction in case of error.
      if (ErrorSet.count(User))
        continue;

      // Some constant users may indirectly cause folding of their users.
      if (isa<StructInst>(User) || isa<TupleInst>(User)) {
        WorkList.insert(User);
        continue;
      }

      // Some initializers of integer types may need to be revisited.
      if (isa<ApplyInst>(User) && isApplyOfIntegerInit(*User)) {
        WorkList.insert(User);
        continue;
      }

      // Stores of a constant into a memory slot may be
      // used later to constant fold an integer type initializer.
      if (auto *SI = dyn_cast<StoreInst>(User)) {
        if (SI->getSrc() != I)
          continue;
        auto Dest = dyn_cast<SILInstruction>(SI->getDest());
        if (Dest)
          WorkList.insert(Dest);
        continue;
      }

      // Always consider cond_fail instructions as potential for DCE.  If the
      // expression feeding them is false, they are dead.  We can't handle this
      // as part of the constant folding logic, because there is no value
      // they can produce (other than empty tuple, which is wasteful).
      if (isa<CondFailInst>(User))
        FoldedUsers.insert(User);

      // Initialize ResultsInError as a None optional.
      //
      // We are essentially using this optional to represent 3 states: true,
      // false, and n/a.
      Optional<bool> ResultsInError;

      // If we are asked to emit diagnostics, override ResultsInError with a
      // Some optional initialized to false.
      if (EnableDiagnostics)
        ResultsInError = false;

      // Try to fold the user. If ResultsInError is None, we do not emit any
      // diagnostics. If ResultsInError is some, we use it as our return value.
      SILValue C = constantFoldInstruction(*User, ResultsInError);

      // If we did not pass in a None and the optional is set to true, add the
      // user to our error set.
      if (ResultsInError.hasValue() && ResultsInError.getValue())
        ErrorSet.insert(User);

      // We failed to constant propagate... continue...
      if (!C)
        continue;

      // Ok, we have succeeded. Add user to the FoldedUsers list and perform the
      // necessary cleanups, RAUWs, etc.
      FoldedUsers.insert(User);
      ++NumInstFolded;

      InvalidateInstructions = true;

      // If the constant produced a tuple, be smarter than RAUW: explicitly nuke
      // any tuple_extract instructions using the apply.  This is a common case
      // for functions returning multiple values.
      if (auto *TI = dyn_cast<TupleInst>(C)) {
        for (auto UI = User->use_begin(), E = User->use_end(); UI != E;) {
          Operand *O = *UI++;

          // If the user is a tuple_extract, just substitute the right value in.
          if (auto *TEI = dyn_cast<TupleExtractInst>(O->getUser())) {
            SILValue NewVal = TI->getOperand(TEI->getFieldNo());
            TEI->replaceAllUsesWith(NewVal);
            TEI->dropAllReferences();
            FoldedUsers.insert(TEI);
            if (auto *Inst = dyn_cast<SILInstruction>(NewVal))
              WorkList.insert(Inst);
          }
        }

        if (User->use_empty())
          FoldedUsers.insert(TI);
      }


      // We were able to fold, so all users should use the new folded value.
      User->replaceAllUsesWith(C);

      // The new constant could be further folded now, add it to the worklist.
      if (auto *Inst = dyn_cast<SILInstruction>(C))
        WorkList.insert(Inst);
    }

    // Eagerly DCE. We do this after visiting all users to ensure we don't
    // invalidate the uses iterator.
    ArrayRef<SILInstruction *> UserArray = FoldedUsers.getArrayRef();
    if (!UserArray.empty()) {
      InvalidateInstructions = true;
    }

    recursivelyDeleteTriviallyDeadInstructions(UserArray, false,
                                               [&](SILInstruction *DeadI) {
                                                 WorkList.remove(DeadI);
                                               });
  }

  // TODO: refactor this code outside of the method. Passes should not merge
  // invalidation kinds themselves.
  using InvalidationKind = SILAnalysis::InvalidationKind;

  unsigned Inv = InvalidationKind::Nothing;
  if (InvalidateInstructions) Inv |= (unsigned) InvalidationKind::Instructions;
  if (InvalidateCalls)        Inv |= (unsigned) InvalidationKind::Calls;
  if (InvalidateBranches)     Inv |= (unsigned) InvalidationKind::Branches;
  return InvalidationKind(Inv);
}

void ConstantPropagation::prepare(SILFunction &F) {
  auto &M = F.getModule();
  auto SignedIntegerInitDecl = F.getASTContext().getSignedIntegerInitDecl();
  auto UnsignedIntegerInitDecl = F.getASTContext().getUnsignedIntegerInitDecl();

  if (SignedIntegerInitDecl) {
    auto SignedIntegerInitMangledName =
        SILDeclRef(SignedIntegerInitDecl, SILDeclRef::Kind::Allocator).mangle();
    SignedIntegerInitFn = M.findFunction(SignedIntegerInitMangledName,
                                         SILLinkage::PublicExternal);
    // assert(SignedIntegerInitFn);
  }
  if (UnsignedIntegerInitDecl) {
    auto UnsignedIntegerInitMangledName =
        SILDeclRef(UnsignedIntegerInitDecl, SILDeclRef::Kind::Allocator)
            .mangle();
    UnsignedIntegerInitFn = M.findFunction(UnsignedIntegerInitMangledName,
                                           SILLinkage::PublicExternal);
    // assert(UnsignedIntegerInitFn);
  }
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

SILTransform *swift::createDiagnosticConstantPropagation() {
  return new ConstantPropagation(true /*enable diagnostics*/);
}

SILTransform *swift::createPerformanceConstantPropagation() {
  return new ConstantPropagation(false /*disable diagnostics*/);
}
