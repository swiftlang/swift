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
#include "swift/Subsystems.h"
#include "swift/AST/Diagnostics.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILPasses/Utils/Local.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/Debug.h"
using namespace swift;

STATISTIC(NumInstFolded, "Number of constant folded instructions");

template<typename...T, typename...U>
static void diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag,
                     U &&...args) {
  Context.Diags.diagnose(loc,
                         diag, std::forward<U>(args)...);
}

/// \brief Fold arithmetic intrinsics with overflow.
static SILInstruction *constantFoldBinaryWithOverflow(ApplyInst *AI,
                                                      llvm::Intrinsic::ID ID,
                                                      bool ReportOverflow,
                                                      bool &ResultsInError) {
  OperandValueArrayRef Args = AI->getArguments();
  assert(Args.size() >= 2);

  // Check if both arguments are literals.
  IntegerLiteralInst *Op1 = dyn_cast<IntegerLiteralInst>(Args[0]);
  IntegerLiteralInst *Op2 = dyn_cast<IntegerLiteralInst>(Args[1]);

  // We cannot fold a builtin if one of the arguments is not a constant.
  if (!Op1 || !Op2)
    return nullptr;

  // Calculate the result.
  APInt LHSInt = Op1->getValue();
  APInt RHSInt = Op2->getValue();
  APInt Res;
  bool Overflow;
  bool Signed = false;
  StringRef Operator = "+";

  switch (ID) {
  default: llvm_unreachable("Invalid case");
  case llvm::Intrinsic::sadd_with_overflow:
    Res = LHSInt.sadd_ov(RHSInt, Overflow);
    Signed = true;
    break;
  case llvm::Intrinsic::uadd_with_overflow:
    Res = LHSInt.uadd_ov(RHSInt, Overflow);
    break;
  case llvm::Intrinsic::ssub_with_overflow:
    Res = LHSInt.ssub_ov(RHSInt, Overflow);
    Operator = "-";
    Signed = true;
    break;
  case llvm::Intrinsic::usub_with_overflow:
    Res = LHSInt.usub_ov(RHSInt, Overflow);
    Operator = "-";
    break;
  case llvm::Intrinsic::smul_with_overflow:
    Res = LHSInt.smul_ov(RHSInt, Overflow);
    Operator = "*";
    Signed = true;
    break;
  case llvm::Intrinsic::umul_with_overflow:
    Res = LHSInt.umul_ov(RHSInt, Overflow);
    Operator = "*";
    break;
  }

  // Get the SIL subtypes of the returned tuple type.
  SILType FuncResType = AI->getFunctionTypeInfo()->getResult().getSILType();
  TupleType *T = FuncResType.castTo<TupleType>();
  assert(T->getNumElements() == 2);
  SILType ResTy1 =
    SILType::getPrimitiveType(CanType(T->getElementType(0)),
                              SILValueCategory::Object);
  SILType ResTy2 =
    SILType::getPrimitiveType(CanType(T->getElementType(1)),
                              SILValueCategory::Object);

  // Construct the folded instruction - a tuple of two literals, the
  // result and overflow.
  SILBuilder B(AI);
  SILLocation Loc = AI->getLoc();
  SILValue Result[] = {
    B.createIntegerLiteral(Loc, ResTy1, Res),
    B.createIntegerLiteral(Loc, ResTy2, Overflow)
  };

  // If we can statically determine that the operation overflows,
  // warn about it.
  if (Overflow && ReportOverflow) {
    // Try to infer the type of the constant expression that the user operates
    // on. If the intrinsic was lowered from a call to a function that takes
    // two arguments of the same type, use the type of the LHS argument.
    // This would detect '+'/'+=' and such.
    Type OpType;
    const ApplyExpr *CE = Loc.getAsASTNode<ApplyExpr>();
    if (CE) {
      const TupleExpr *Args = dyn_cast_or_null<TupleExpr>(CE->getArg());
      if (Args && Args->getNumElements() == 2) {
        CanType LHSTy = Args->getElement(0)->getType()->getCanonicalType();
        CanType RHSTy = Args->getElement(0)->getType()->getCanonicalType();
        if (LHSTy == RHSTy)
          OpType = Args->getElement(1)->getType();
      }
    }

    if (!OpType.isNull()) {
      diagnose(AI->getModule().getASTContext(),
               Loc.getSourceLoc(),
               diag::arithmetic_operation_overflow,
               LHSInt.toString(/*Radix*/ 10, Signed),
               Operator,
               RHSInt.toString(/*Radix*/ 10, Signed),
               OpType);
      ResultsInError = true;
    } else {
      // If we cannot get the type info in an expected way, describe the type.
      diagnose(AI->getModule().getASTContext(),
               Loc.getSourceLoc(),
               diag::arithmetic_operation_overflow_generic_type,
               LHSInt.toString(/*Radix*/ 10, Signed),
               Operator,
               RHSInt.toString(/*Radix*/ 10, Signed),
               Signed,
               LHSInt.getBitWidth());

      ResultsInError = true;
    }
  }

  return B.createTuple(Loc, FuncResType, Result);
}

static SILInstruction *constantFoldOverflowBuiltin(ApplyInst *AI,
                                                   BuiltinValueKind ID,
                                                   bool &ResultsInError) {
  OperandValueArrayRef Args = AI->getArguments();
  IntegerLiteralInst *ShouldReportFlag = dyn_cast<IntegerLiteralInst>(Args[2]);
  return constantFoldBinaryWithOverflow(AI,
           getLLVMIntrinsicIDForBuiltinWithOverflow(ID),
           ShouldReportFlag && (ShouldReportFlag->getValue() == 1),
           ResultsInError);
}

static SILInstruction *constantFoldIntrinsic(ApplyInst *AI,
                                             llvm::Intrinsic::ID ID,
                                             bool &ResultsInError) {
  switch (ID) {
  default: break;
  case llvm::Intrinsic::sadd_with_overflow:
  case llvm::Intrinsic::uadd_with_overflow:
  case llvm::Intrinsic::ssub_with_overflow:
  case llvm::Intrinsic::usub_with_overflow:
  case llvm::Intrinsic::smul_with_overflow:
  case llvm::Intrinsic::umul_with_overflow:
    return constantFoldBinaryWithOverflow(AI, ID,
                                          /* ReportOverflow */ false,
                                          ResultsInError);
  }
  return nullptr;
}

static SILInstruction *constantFoldCompareBuiltin(ApplyInst *AI,
                                                  BuiltinValueKind ID) {
  OperandValueArrayRef Args = AI->getArguments();

  // Fold for integer constant arguments.
  IntegerLiteralInst *LHS = dyn_cast<IntegerLiteralInst>(Args[0]);
  IntegerLiteralInst *RHS = dyn_cast<IntegerLiteralInst>(Args[1]);
  if (LHS && RHS) {
    APInt V1 = LHS->getValue();
    APInt V2 = RHS->getValue();
    APInt Res;
    switch (ID) {
    default: llvm_unreachable("Invalid integer compare kind");
    case BuiltinValueKind::ICMP_EQ:  Res = V1 == V2; break;
    case BuiltinValueKind::ICMP_NE:  Res = V1 != V2; break;
    case BuiltinValueKind::ICMP_SLT: Res = V1.slt(V2); break;
    case BuiltinValueKind::ICMP_SGT: Res = V1.sgt(V2); break;
    case BuiltinValueKind::ICMP_SLE: Res = V1.sle(V2); break;
    case BuiltinValueKind::ICMP_SGE: Res = V1.sge(V2); break;
    case BuiltinValueKind::ICMP_ULT: Res = V1.ult(V2); break;
    case BuiltinValueKind::ICMP_UGT: Res = V1.ugt(V2); break;
    case BuiltinValueKind::ICMP_ULE: Res = V1.ule(V2); break;
    case BuiltinValueKind::ICMP_UGE: Res = V1.uge(V2); break;
    }
    SILBuilder B(AI);
    SILLocation Loc = AI->getLoc();
    SILType ResType = AI->getFunctionTypeInfo()->getResult().getSILType();
    return B.createIntegerLiteral(Loc, ResType, Res);
  }

  return nullptr;
}

static SILInstruction *constantFoldBuiltin(ApplyInst *AI,
                                           BuiltinFunctionRefInst *FR,
                                           bool &ResultsInError) {
  const IntrinsicInfo &Intrinsic = FR->getIntrinsicInfo();
  SILModule &M = AI->getModule();

  // If it's an llvm intrinsic, fold the intrinsic.
  if (Intrinsic.ID != llvm::Intrinsic::not_intrinsic)
    return constantFoldIntrinsic(AI, Intrinsic.ID, ResultsInError);

  // Otherwise, it should be one of the builin functions.
  OperandValueArrayRef Args = AI->getArguments();
  const BuiltinInfo &Builtin = M.getBuiltinInfo(FR->getReferencedFunction());

  switch (Builtin.ID) {
  default: break;

// Check and fold binary arithmetic with overflow.
#define BUILTIN(id, name, Attrs)
#define BUILTIN_BINARY_OPERATION_WITH_OVERFLOW(id, name, attrs, overload) \
  case BuiltinValueKind::id:
#include "swift/AST/Builtins.def"
    return constantFoldOverflowBuiltin(AI, Builtin.ID, ResultsInError);

// Fold comparison predicates.
#define BUILTIN(id, name, Attrs)
#define BUILTIN_BINARY_PREDICATE(id, name, attrs, overload) \
case BuiltinValueKind::id:
#include "swift/AST/Builtins.def"
      return constantFoldCompareBuiltin(AI, Builtin.ID);

  case BuiltinValueKind::Trunc:
  case BuiltinValueKind::ZExt:
  case BuiltinValueKind::SExt: {

    // We can fold if the value being cast is a constant.
    IntegerLiteralInst *V = dyn_cast<IntegerLiteralInst>(Args[0]);
    if (!V)
      return nullptr;

    // Get the cast result.
    APInt CastResV;
    Type DestTy = Builtin.Types.size() == 2 ? Builtin.Types[1] : Type();
    uint32_t DestBitWidth =
      DestTy->castTo<BuiltinIntegerType>()->getBitWidth();
    switch (Builtin.ID) {
    default : llvm_unreachable("Invalid case.");
    case BuiltinValueKind::Trunc:
      CastResV = V->getValue().trunc(DestBitWidth);
      break;
    case BuiltinValueKind::ZExt:
      CastResV = V->getValue().zext(DestBitWidth);
      break;
    case BuiltinValueKind::SExt:
      CastResV = V->getValue().sext(DestBitWidth);
      break;
    }

    // Add the literal instruction to represnet the result of the cast.
    SILBuilder B(AI);
    return B.createIntegerLiteral(AI->getLoc(), AI->getType(), CastResV);
  }

  // Fold constant division operations and report div by zero.
  case BuiltinValueKind::SDiv:
  case BuiltinValueKind::ExactSDiv:
  case BuiltinValueKind::SRem:
  case BuiltinValueKind::UDiv:
  case BuiltinValueKind::ExactUDiv:
  case BuiltinValueKind::URem: {
    // Get the denominator.
    IntegerLiteralInst *Denom = dyn_cast<IntegerLiteralInst>(Args[1]);
    if (!Denom)
      return nullptr;
    APInt DenomVal = Denom->getValue();

    // Reoprt an error if the denominator is zero.
    if (DenomVal == 0) {
      diagnose(M.getASTContext(),
               AI->getLoc().getSourceLoc(),
               diag::division_by_zero);
      ResultsInError = true;
      return nullptr;
    }

    // Get the numerator.
    IntegerLiteralInst *Num = dyn_cast<IntegerLiteralInst>(Args[0]);
    if (!Num)
      return nullptr;
    APInt NumVal = Num->getValue();

    APInt ResVal;
    bool Overflowed = false;
    switch (Builtin.ID) {
    // We do not cover all the cases below - only the ones that are easily
    // computable for APInt.
    default : return nullptr;
    case BuiltinValueKind::SDiv:
      ResVal = NumVal.sdiv_ov(DenomVal, Overflowed);
      break;
    case BuiltinValueKind::SRem:
      ResVal = NumVal.srem(DenomVal);
      break;
    case BuiltinValueKind::UDiv:
      ResVal = NumVal.udiv(DenomVal);
      break;
    case BuiltinValueKind::URem:
      ResVal = NumVal.urem(DenomVal);
      break;
    }

    if (Overflowed) {
      diagnose(M.getASTContext(),
               AI->getLoc().getSourceLoc(),
               diag::division_overflow,
               NumVal.toString(/*Radix*/ 10, /*Signed*/true),
               "/",
               DenomVal.toString(/*Radix*/ 10, /*Signed*/true));
      ResultsInError = true;
      return nullptr;
    }

    // Add the literal instruction to represnet the result of the division.
    SILBuilder B(AI);
    return B.createIntegerLiteral(AI->getLoc(), AI->getType(), ResVal);
  }

  // Deal with special builtins that are designed to check overflows on
  // integer literals.
  case BuiltinValueKind::STruncWithOverflow:
  case BuiltinValueKind::UTruncWithOverflow: {
    // Get the value. It should be a constant in most cases.
    // Note, this will not always be a constant, for example, when analyzing
    // _convertFromBuiltinIntegerLiteral function itself.
    IntegerLiteralInst *V = dyn_cast<IntegerLiteralInst>(Args[0]);
    if (!V)
      return nullptr;
    APInt SrcVal = V->getValue();

    // Get the signedness of the destination.
    bool Signed = (Builtin.ID == BuiltinValueKind::STruncWithOverflow);

    // Get the source and destination bit width.
    assert(Builtin.Types.size() == 2);
    uint32_t SrcBitWidth =
      Builtin.Types[0]->castTo<BuiltinIntegerType>()->getBitWidth();
    Type DestTy = Builtin.Types[1];
    uint32_t DestBitWidth =
      DestTy->castTo<BuiltinIntegerType>()->getBitWidth();

    // Compute the destination:
    //   truncVal = trunc_IntFrom_IntTo(val)
    //   strunc_IntFrom_IntTo(val) =
    //     sext_IntFrom(truncVal) == val ? truncVal : overflow_error
    //   utrunc_IntFrom_IntTo(val) =
    //     zext_IntFrom(truncVal) == val ? truncVal : overflow_error
    APInt TruncVal = SrcVal.trunc(DestBitWidth);
    APInt T = Signed ? TruncVal.sext(SrcBitWidth) : TruncVal.zext(SrcBitWidth);

    SILLocation Loc = AI->getLoc();
    const ApplyExpr *CE = Loc.getAsASTNode<ApplyExpr>();

    // Check for overflow.
    if (SrcVal != T) {
      // FIXME: This will prevent hard error in cases the error is comming
      // from ObjC interoperability code. Currently, we treat NSUInteger as
      // Int.
      if (Loc.getSourceLoc().isInvalid()) {
        diagnose(M.getASTContext(), Loc.getSourceLoc(),
                 diag::integer_literal_overflow_warn,
                 CE ? CE->getType() : DestTy);
        ResultsInError = true;
        return nullptr;
      }
      diagnose(M.getASTContext(), Loc.getSourceLoc(),
               diag::integer_literal_overflow,
               CE ? CE->getType() : DestTy);
      ResultsInError = true;
      return nullptr;
    }

    // The call to the builtin should be replaced with the constant value.
    SILBuilder B(AI);
    return B.createIntegerLiteral(Loc, AI->getType(), TruncVal);
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

    SILLocation Loc = AI->getLoc();
    const ApplyExpr *CE = Loc.getAsASTNode<ApplyExpr>();

    // Check for overflow.
    if (ConversionStatus & APFloat::opOverflow) {
      diagnose(M.getASTContext(), Loc.getSourceLoc(),
               diag::integer_literal_overflow,
               CE ? CE->getType() : DestTy);
      ResultsInError = true;
      return nullptr;
    }

    // The call to the builtin should be replaced with the constant value.
    SILBuilder B(AI);
    return B.createFloatLiteral(Loc, AI->getType(), TruncVal);
  }
  }
  return nullptr;
}

static SILValue constantFoldInstruction(SILInstruction &I,
                                        bool &ResultsInError) {
  // Constant fold function calls.
  if (ApplyInst *AI = dyn_cast<ApplyInst>(&I)) {
    // Constant fold calls to builtins.
    if (BuiltinFunctionRefInst *FR =
          dyn_cast<BuiltinFunctionRefInst>(AI->getCallee().getDef())) {
      return constantFoldBuiltin(AI, FR, ResultsInError);
    }
    return SILValue();
  }

  // Constant fold extraction of a constant element.
  if (TupleExtractInst *TEI = dyn_cast<TupleExtractInst>(&I)) {
    if (TupleInst *TheTuple = dyn_cast<TupleInst>(TEI->getOperand().getDef()))
      return TheTuple->getElements()[TEI->getFieldNo()];
  }

  // Constant fold extraction of a constant struct element.
  if (StructExtractInst *SEI = dyn_cast<StructExtractInst>(&I)) {
    if (StructInst *Struct = dyn_cast<StructInst>(SEI->getOperand().getDef()))
      return Struct->getOperandForField(SEI->getField())->get();
  }

  return SILValue();
}

static bool isFoldable(SILInstruction *I) {
  return isa<IntegerLiteralInst>(I);
}

static bool CCPFunctionBody(SILFunction &F) {
  DEBUG(llvm::errs() << "*** ConstPropagation processing: " << F.getName()
        << "\n");

  // The list of instructions whose evaluation resulted in errror or warning.
  // This is used to avoid duplicate error reporting in case we reach the same
  // instruction from different entry points in the WorkList.
  llvm::DenseSet<SILInstruction*> ErrorSet;

  // The worklist of the constants that could be folded into their users.
  llvm::SetVector<ValueBase*> WorkList;
  // Initialize the worklist to all of the constant instructions.
  for (auto &BB : F) {
    for (auto &I : BB) {
      if (isFoldable(&I) && !I.use_empty())
        WorkList.insert(&I);
    }
  }

  while (!WorkList.empty()) {
    ValueBase *I = *WorkList.begin();
    WorkList.remove(I);

    // Go through all users of the constant and try to fold them.
    llvm::DenseSet<SILInstruction*> FoldedUsers;
    for (auto Use : I->getUses()) {
      SILInstruction *User = cast<SILInstruction>(Use->getUser());

      // It is possible that we had processed this user already. Do not try
      // to fold it again if we had previously produced an error while folding
      // it.
      // (It is not always possible to fold an instruction in case of error.)
      if (ErrorSet.count(User))
        continue;

      // TODO: This is handling folding of tupleelement/tuple and
      // structelement/structs inline with constant folding.  This should
      // probably handle them in the prepass, instead of handling them in the
      // worklist loop.  They are conceptually very different operations and
      // are technically not constant folding.
      //
      // Some constant users may indirectly cause folding of their users.
      if (isa<StructInst>(User) || isa<TupleInst>(User)) {
        WorkList.insert(User);
        continue;
      }

      // Try to fold the user.
      bool ResultsInError = false;
      SILValue C = constantFoldInstruction(*User, ResultsInError);
      if (ResultsInError)
        ErrorSet.insert(User);

      if (!C) continue;

      FoldedUsers.insert(User);
      ++NumInstFolded;

      // We were able to fold, so all users should use the new folded value.
      assert(User->getTypes().size() == 1 &&
             "Currently, we only support single result instructions");
      SILValue(User).replaceAllUsesWith(C);

      // The new constant could be further folded now, add it to the worklist.
      WorkList.insert(C.getDef());
    }

    // Eagerly DCE. We do this after visiting all users to ensure we don't
    // invalidate the uses iterator.
    for (auto U : FoldedUsers)
      recursivelyDeleteTriviallyDeadInstructions(U);

  }

  return false;
}

//===----------------------------------------------------------------------===//
//                          Top Level Driver
//===----------------------------------------------------------------------===//
void swift::performSILConstantPropagation(SILModule *M) {
  for (auto &Fn : *M)
    CCPFunctionBody(Fn);
}
