//===--- ConstantPropagation.cpp - Constant fold and diagnose overflows ---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "constant-propagation"
#include "swift/SILPasses/Passes.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILPasses/Utils/ConstantFolding.h"
#include "swift/SILPasses/Transforms.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"
using namespace swift;

STATISTIC(NumInstFolded, "Number of constant folded instructions");

template<typename...T, typename...U>
static InFlightDiagnostic
diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag, U &&...args) {
  return Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}

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
  SILBuilderWithScope<4> B(BI);
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

  IntegerLiteralInst *Op1 = dyn_cast<IntegerLiteralInst>(Args[0]);
  IntegerLiteralInst *Op2 = dyn_cast<IntegerLiteralInst>(Args[1]);

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
      const TupleExpr *Args = dyn_cast_or_null<TupleExpr>(CE->getArg());
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
  IntegerLiteralInst *ShouldReportFlag = dyn_cast<IntegerLiteralInst>(Args[2]);
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
  IntegerLiteralInst *LHS = dyn_cast<IntegerLiteralInst>(Args[0]);
  IntegerLiteralInst *RHS = dyn_cast<IntegerLiteralInst>(Args[1]);
  if (LHS && RHS) {
    APInt Res = constantFoldComparison(LHS->getValue(), RHS->getValue(), ID);
    SILBuilderWithScope<1> B(BI);
    return B.createIntegerLiteral(BI->getLoc(), BI->getType(), Res);
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
  IntegerLiteralInst *Denom = dyn_cast<IntegerLiteralInst>(Args[1]);
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
  IntegerLiteralInst *Num = dyn_cast<IntegerLiteralInst>(Args[0]);
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

  // Add the literal instruction to represnet the result of the division.
  SILBuilderWithScope<1> B(BI);
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
    IntegerLiteralInst *LHS = dyn_cast<IntegerLiteralInst>(Args[0]);
    IntegerLiteralInst *RHS = dyn_cast<IntegerLiteralInst>(Args[1]);
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
    SILBuilderWithScope<1> B(BI);
    return B.createIntegerLiteral(BI->getLoc(), BI->getType(), ResI);
  }
  case BuiltinValueKind::FAdd:
  case BuiltinValueKind::FDiv:
  case BuiltinValueKind::FMul:
  case BuiltinValueKind::FSub: {
    OperandValueArrayRef Args = BI->getArguments();
    FloatLiteralInst *LHS = dyn_cast<FloatLiteralInst>(Args[0]);
    FloatLiteralInst *RHS = dyn_cast<FloatLiteralInst>(Args[1]);
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
    SILBuilderWithScope<1> B(BI);
    return B.createFloatLiteral(BI->getLoc(), BI->getType(), LHSF);
  }
  }
}

static std::pair<bool, bool> getTypeSigndness(const BuiltinInfo &Builtin) {
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
  IntegerLiteralInst *V = dyn_cast<IntegerLiteralInst>(Args[0]);
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

    // FIXME: This will prevent hard error in cases the error is comming
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
      // Try to print user-visible types if they are available.
      if (!UserDstTy.isNull()) {
        diagnose(M.getASTContext(), Loc.getSourceLoc(),
                 diag::integer_literal_overflow, UserDstTy);
      // Otherwise, print the Builtin Types.
      } else {
        bool SrcTySigned, DstTySigned;
        std::tie(SrcTySigned, DstTySigned) = getTypeSigndness(Builtin);
        diagnose(M.getASTContext(), Loc.getSourceLoc(),
                 diag::integer_literal_overflow_builtin_types,
                 DstTySigned, DstTy);
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
          // Since builtin types are sign-agnostic, print the signdness
          // separately.
          bool SrcTySigned, DstTySigned;
          std::tie(SrcTySigned, DstTySigned) = getTypeSigndness(Builtin);
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
    IntegerLiteralInst *V = dyn_cast<IntegerLiteralInst>(Args[0]);
    if (!V)
      return nullptr;

    APInt CastResV = constantFoldCast(V->getValue(), Builtin);

    // Add the literal instruction to represent the result of the cast.
    SILBuilderWithScope<1> B(BI);
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
    IntegerLiteralInst *V = dyn_cast<IntegerLiteralInst>(Args[0]);
    if (!V)
      return nullptr;
    APInt SrcVal = V->getValue();
    Type DestTy = Builtin.Types[1];

    APFloat TruncVal(
        DestTy->castTo<BuiltinFloatType>()->getAPFloatSemantics());
    APFloat::opStatus ConversionStatus = TruncVal.convertFromAPInt(
        SrcVal, /*isSigned=*/true, APFloat::rmNearestTiesToEven);

    SILLocation Loc = BI->getLoc();
    const ApplyExpr *CE = Loc.getAsASTNode<ApplyExpr>();

    // Check for overflow.
    if (ConversionStatus & APFloat::opOverflow) {
      // If we overflow and are not asked for diagnostics, just return nullptr.
      if (!ResultsInError.hasValue())
        return nullptr;

      // Otherwise emit our diagnostics and then return nullptr.
      diagnose(M.getASTContext(), Loc.getSourceLoc(),
               diag::integer_literal_overflow,
               CE ? CE->getType() : DestTy);
      ResultsInError = Optional<bool>(true);
      return nullptr;
    }

    // The call to the builtin should be replaced with the constant value.
    SILBuilderWithScope<1> B(BI);
    return B.createFloatLiteral(Loc, BI->getType(), TruncVal);
  }
      
  case BuiltinValueKind::AssumeNonNegative: {
    IntegerLiteralInst *V = dyn_cast<IntegerLiteralInst>(Args[0]);
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
  if (BuiltinInst *BI = dyn_cast<BuiltinInst>(&I)) {
    return constantFoldBuiltin(BI, ResultsInError);
  }

  // Constant fold extraction of a constant element.
  if (TupleExtractInst *TEI = dyn_cast<TupleExtractInst>(&I)) {
    if (TupleInst *TheTuple = dyn_cast<TupleInst>(TEI->getOperand()))
      return TheTuple->getElements()[TEI->getFieldNo()];
  }

  // Constant fold extraction of a constant struct element.
  if (StructExtractInst *SEI = dyn_cast<StructExtractInst>(&I)) {
    if (StructInst *Struct = dyn_cast<StructInst>(SEI->getOperand()))
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
    if (auto *FRI = dyn_cast<FunctionRefInst>(AI->getCallee()))
      if (FRI->getReferencedFunction()->hasSemanticsString("string.concat"))
        return true;
  return false;
}

static bool isFoldable(SILInstruction *I) {
  return isa<IntegerLiteralInst>(I) || isa<FloatLiteralInst>(I);
}

static void
constantFoldStringConcatenation(ApplyInst *AI,
                                llvm::SetVector<SILInstruction *> &WorkList) {
  SILBuilder B(AI);
  // Try to apply the string literal concatenation optimization.
  auto *Concatenated = tryToConcatenateStrings(AI, B);
  // Bail if string literal concatenation could not be performed.
  if (!Concatenated)
    return;

  // Add the newly created instruction to the BB.
  AI->getParent()->getInstList().insert(AI,
                                        dyn_cast<SILInstruction>(Concatenated));

  // Replace all uses of the old instruction by a new instruction.
  SILValue(AI).replaceAllUsesWith(Concatenated);

  auto RemoveCallback = [&](SILInstruction *DeadI) { WorkList.remove(DeadI); };
  // Remove operands that are not used anymore.
  // Even if they are apply_inst, it is safe to
  // do so, because they can only be applies
  // of functions annotated as string.utf16
  // or string.utf16.
  for (auto &Op : AI->getAllOperands()) {
    SILValue Val = Op.get();
    Op.drop();
    if (Val.use_empty()) {
      SILInstruction *DeadI = dyn_cast<SILInstruction>(Val);
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
}

static bool CCPFunctionBody(SILFunction &F, bool EnableDiagnostics,
                            unsigned AssertConfiguration) {
  DEBUG(llvm::dbgs() << "*** ConstPropagation processing: " << F.getName()
        << "\n");
  bool Changed = false;

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
  // Initialize the worklist to all of the constant instructions.
  for (auto &BB : F) {
    for (auto &I : BB) {
      if (isFoldable(&I) && !I.use_empty())
        WorkList.insert(&I);
      else if (InstantiateAssertConfiguration
               && (isApplyOfBuiltin(I, BuiltinValueKind::AssertConf)
                   || isApplyOfBuiltin(I, BuiltinValueKind::CondUnreachable)))
        WorkList.insert(&I);
      else if (isApplyOfStringConcat(I))
        WorkList.insert(&I);
    }
  }

  llvm::SetVector<SILInstruction *> FoldedUsers;

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
          SILBuilderWithScope<1> B(BI);
          auto AssertConfInt = B.createIntegerLiteral(
            BI->getLoc(), BI->getType(), AssertConfiguration);
          BI->replaceAllUsesWith(AssertConfInt);
          // Schedule users for constant folding.
          WorkList.insert(AssertConfInt);
          // Delete the call.
          recursivelyDeleteTriviallyDeadInstructions(BI);
          continue;
        }
        
        // Kill calls to conditionallyUnreachable if we've folded assert
        // configuration calls.
        if (isApplyOfBuiltin(*BI, BuiltinValueKind::CondUnreachable)) {
          assert(BI->use_empty() && "use of conditionallyUnreachable?!");
          recursivelyDeleteTriviallyDeadInstructions(BI, /*force*/ true);
          continue;
        }
      }

    if (auto *AI = dyn_cast<ApplyInst>(I)) {
      // Apply may only come from a string.concat invocation.
      constantFoldStringConcatenation(AI, WorkList);
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

      // We failed to constant propogate... continue...
      if (!C)
        continue;

      // Ok, we have succeeded. Add user to the FoldedUsers list and perform the
      // necessary cleanups, RAUWs, etc.
      FoldedUsers.insert(User);
      ++NumInstFolded;
      Changed = true;

      // If the constant produced a tuple, be smarter than RAUW: explicitly nuke
      // any tuple_extract instructions using the apply.  This is a common case
      // for functions returning multiple values.
      if (auto *TI = dyn_cast<TupleInst>(C)) {
        for (auto UI = User->use_begin(), E = User->use_end(); UI != E;) {
          Operand *O = *UI++;

          // If the user is a tuple_extract, just substitute the right value in.
          if (auto *TEI = dyn_cast<TupleExtractInst>(O->getUser())) {
            SILValue NewVal = TI->getOperand(TEI->getFieldNo());
            assert(TEI->getTypes().size() == 1 &&
                   "Currently, we only support single result instructions.");
            SILValue(TEI, 0).replaceAllUsesWith(NewVal);
            TEI->dropAllReferences();
            FoldedUsers.insert(TEI);
            if (auto *Inst = dyn_cast<SILInstruction>(NewVal.getDef()))
              WorkList.insert(Inst);
          }
        }

        if (User->use_empty())
          FoldedUsers.insert(TI);
      }


      // We were able to fold, so all users should use the new folded value.
      assert(User->getTypes().size() == 1 &&
             "Currently, we only support single result instructions");
      SILValue(User).replaceAllUsesWith(C);

      // The new constant could be further folded now, add it to the worklist.
      if (auto *Inst = dyn_cast<SILInstruction>(C.getDef()))
        WorkList.insert(Inst);
    }

    // Eagerly DCE. We do this after visiting all users to ensure we don't
    // invalidate the uses iterator.
    auto UserArray = ArrayRef<SILInstruction *>(&*FoldedUsers.begin(),
                                                FoldedUsers.size());
    recursivelyDeleteTriviallyDeadInstructions(UserArray, false,
                                               [&](SILInstruction *DeadI) {
                                                 WorkList.remove(DeadI);
                                               });
  }

  return Changed;
}

namespace {
class ConstantPropagation : public SILFunctionTransform {
  bool EnableDiagnostics;

public:
  ConstantPropagation(bool EnableDiagnostics) :
    EnableDiagnostics(EnableDiagnostics) {}

private:
  /// The entry point to the transformation.
  void run() override {
    if (CCPFunctionBody(*getFunction(), EnableDiagnostics,
                        getOptions().AssertConfig))
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }

  StringRef getName() override { return "Constant Propagation"; }
};
} // end anonymous namespace

SILTransform *swift::createDiagnosticConstantPropagation() {
  return new ConstantPropagation(true /*enable diagnostics*/);
}

SILTransform *swift::createPerformanceConstantPropagation() {
  return new ConstantPropagation(false /*disable diagnostics*/);
}
