//===-- ValueTracking.h - SIL Value Tracking Analysis ----------*- C++ -*--===//
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

#define DEBUG_TYPE "sil-value-tracking"
#include "swift/SILAnalysis/ValueTracking.h"
#include "swift/SILAnalysis/SimplifyInstruction.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SIL/PatternMatch.h"
#include "llvm/Support/Debug.h"
using namespace swift;
using namespace swift::PatternMatch;

/// Strip off casts/indexing insts/address projections from V until there is
/// nothing left to strip.
/// FIXME: Maybe put this on SILValue?
SILValue swift::getUnderlyingObject(SILValue V) {
  while (true) {
    SILValue V2 = V.stripCasts().stripAddressProjections().stripIndexingInsts();
    if (V2 == V)
      return V2;
    V = V2;
  }
}

/// Returns true if the ValueBase inside V is an apply whose callee is a no read
/// builtin_function_ref.
static bool isNoReadApplyInst(SILValue V) {
  auto *AI = dyn_cast<ApplyInst>(V);
  if (!AI)
    return false;

  auto *BI = dyn_cast<BuiltinFunctionRefInst>(AI->getCallee());

  return BI && isReadNone(BI);
}

/// Is Inst an instruction which escapes if and only if one of its results
/// escape?
static bool isTransitiveEscapeInst(SILInstruction *Inst) {
  switch (Inst->getKind()) {
  case ValueKind::AllocBoxInst:
  case ValueKind::AllocRefInst:
  case ValueKind::AllocRefDynamicInst:
  case ValueKind::AllocStackInst:
  case ValueKind::BuiltinInst:
  case ValueKind::ApplyInst:
  case ValueKind::WitnessMethodInst:
  case ValueKind::BuiltinFunctionRefInst:
  case ValueKind::CopyAddrInst:
  case ValueKind::RetainValueInst:
  case ValueKind::DeallocBoxInst:
  case ValueKind::DeallocRefInst:
  case ValueKind::DeallocStackInst:
  case ValueKind::DebugValueAddrInst:
  case ValueKind::DebugValueInst:
  case ValueKind::DestroyAddrInst:
  case ValueKind::ReleaseValueInst:
  case ValueKind::AutoreleaseValueInst:
  case ValueKind::FloatLiteralInst:
  case ValueKind::FunctionRefInst:
  case ValueKind::GlobalAddrInst:
  case ValueKind::IntegerLiteralInst:
  case ValueKind::LoadInst:
  case ValueKind::LoadWeakInst:
  case ValueKind::MetatypeInst:
  case ValueKind::ObjCProtocolInst:
  case ValueKind::SILGlobalAddrInst:
  case ValueKind::StoreInst:
  case ValueKind::StoreWeakInst:
  case ValueKind::StringLiteralInst:
  case ValueKind::CopyBlockInst:
  case ValueKind::StrongReleaseInst:
  case ValueKind::StrongRetainAutoreleasedInst:
  case ValueKind::StrongRetainInst:
  case ValueKind::StrongRetainUnownedInst:
  case ValueKind::UnownedReleaseInst:
  case ValueKind::UnownedRetainInst:
  case ValueKind::InjectEnumAddrInst:
  case ValueKind::DeinitExistentialInst:
  case ValueKind::UnreachableInst:
  case ValueKind::IsNonnullInst:
  case ValueKind::CondFailInst:
  case ValueKind::DynamicMethodBranchInst:
  case ValueKind::ReturnInst:
  case ValueKind::AutoreleaseReturnInst:
  case ValueKind::FixLifetimeInst:
    return false;

  case ValueKind::AddressToPointerInst:
  case ValueKind::ValueMetatypeInst:
  case ValueKind::BranchInst:
  case ValueKind::CheckedCastBranchInst:
  case ValueKind::CheckedCastAddrBranchInst:
  case ValueKind::ClassMethodInst:
  case ValueKind::CondBranchInst:
  case ValueKind::ConvertFunctionInst:
  case ValueKind::DynamicMethodInst:
  case ValueKind::EnumInst:
  case ValueKind::IndexAddrInst:
  case ValueKind::IndexRawPointerInst:
  case ValueKind::InitBlockStorageHeaderInst:
  case ValueKind::InitEnumDataAddrInst:
  case ValueKind::InitExistentialInst:
  case ValueKind::InitExistentialMetatypeInst:
  case ValueKind::InitExistentialRefInst:
  case ValueKind::ObjCExistentialMetatypeToObjectInst:
  case ValueKind::ObjCMetatypeToObjectInst:
  case ValueKind::ObjCToThickMetatypeInst:
  case ValueKind::UncheckedRefCastInst:
  case ValueKind::UncheckedAddrCastInst:
  case ValueKind::UncheckedTrivialBitCastInst:
  case ValueKind::UncheckedRefBitCastInst:
  case ValueKind::OpenExistentialInst:
  case ValueKind::OpenExistentialMetatypeInst:
  case ValueKind::OpenExistentialRefInst:
  case ValueKind::PartialApplyInst:
  case ValueKind::PointerToAddressInst:
  case ValueKind::ProjectBlockStorageInst:
  case ValueKind::ExistentialMetatypeInst:
  case ValueKind::RawPointerToRefInst:
  case ValueKind::RefElementAddrInst:
  case ValueKind::RefToRawPointerInst:
  case ValueKind::RefToUnmanagedInst:
  case ValueKind::RefToUnownedInst:
  case ValueKind::SelectEnumInst:
  case ValueKind::SelectEnumAddrInst:
  case ValueKind::StructElementAddrInst:
  case ValueKind::StructExtractInst:
  case ValueKind::StructInst:
  case ValueKind::SuperMethodInst:
  case ValueKind::SwitchEnumAddrInst:
  case ValueKind::SwitchEnumInst:
  case ValueKind::SwitchIntInst:
  case ValueKind::UncheckedEnumDataInst:
  case ValueKind::UncheckedTakeEnumDataAddrInst:
  case ValueKind::ThickToObjCMetatypeInst:
  case ValueKind::ThinToThickFunctionInst:
  case ValueKind::TupleElementAddrInst:
  case ValueKind::TupleExtractInst:
  case ValueKind::TupleInst:
  case ValueKind::UnconditionalCheckedCastInst:
  case ValueKind::UnconditionalCheckedCastAddrInst:
  case ValueKind::UnmanagedToRefInst:
  case ValueKind::UnownedToRefInst:
  case ValueKind::UpcastInst:
    return true;

  case ValueKind::AssignInst:
  case ValueKind::MarkFunctionEscapeInst:
  case ValueKind::MarkUninitializedInst:
    llvm_unreachable("Invalid in canonical SIL.");

  case ValueKind::SILArgument:
  case ValueKind::SILUndef:
    llvm_unreachable("These do not use other values.");
  }
}

/// Maximum amount of ValueCapture queries.
static unsigned const Threshold = 32;

namespace {

/// Are there any uses that should be ignored as capture uses.
///
/// TODO: Expand this if we ever do the store of pointer analysis mentioned in
/// Basic AA.
enum CaptureException : unsigned {
  None=0,
  ReturnsCannotCapture=1,
};

} // end anonymous namespace

/// Returns true if V is a value that is used in a manner such that we know its
/// captured or we don't understand whether or not it was captured. In such a
/// case to be conservative, we must assume it is captured.
/// FIXME: Maybe put this on SILValue?
static bool valueMayBeCaptured(SILValue V, CaptureException Exception) {
  llvm::SmallVector<Operand *, Threshold> Worklist;
  llvm::SmallPtrSet<Operand *, Threshold> Visited;
  unsigned Count = 0;

  DEBUG(llvm::dbgs() << "        Checking for capture.\n");
  

  // All all uses of V to the worklist.
  for (auto *UI : V.getUses()) {
    // If we have more uses than the threshold, be conservative and bail so we
    // don't use too much compile time.
    if (Count++ >= Threshold)
      return true;
    Visited.insert(UI);
    Worklist.push_back(UI);
  }

  // Until the worklist is empty...
  while (!Worklist.empty()) {
    // Pop off an operand and grab the operand's user...
    Operand *Op = Worklist.pop_back_val();
    SILInstruction *Inst = Op->getUser();

    DEBUG(llvm::dbgs() << "            Visiting: " << *Inst);

    // If Inst is an instruction with the transitive escape property, V escapes
    // if and only if the results of Inst escape as well.
    if (isTransitiveEscapeInst(Inst)) {
      DEBUG(llvm::dbgs() << "                Found transitive escape "
            "instruction!\n");
      for (auto *UI : Inst->getUses()) {
        // If we have more uses than the threshold, be conservative and bail
        // so we don't use too much compile time.
        if (Count++ >= Threshold)
          return true;

        if (Visited.insert(UI)) {
          Worklist.push_back(UI);
        }
      }
      continue;
    }

    // An apply of a builtin that does not read memory can not capture a value.
    //
    // TODO: Use analysis of the other function perhaps to see if it captures
    // memory in some manner?
    // TODO: Add in knowledge about how parameters work on swift to make this
    // more aggressive.
    if (isNoReadApplyInst(Inst))
      continue;

    // Loading from a pointer does not cause it to be captured.
    if (isa<LoadInst>(Inst))
      continue;

    // If we have a store and are storing into the pointer, this is not a
    // capture. Otherwise it is safe.
    if (auto *SI = dyn_cast<StoreInst>(Inst)) {
      if (SI->getDest() == Op->get()) {
        continue;
      } else {
        return true;
      }
    }

    // Deallocation instructions don't capture.
    if (isa<DeallocationInst>(Inst))
      continue;

    // Debug instructions don't capture.
    if (isa<DebugValueInst>(Inst) || isa<DebugValueAddrInst>(Inst))
      continue;

    // RefCountOperations don't capture.
    //
    // The release case is true since Swift does not allow destructors to
    // resurrent objects. This is enforced via a runtime failure.
    if (isa<RefCountingInst>(Inst))
      continue;

    // If we have a return instruction and we are assuming that returns don't
    // capture, we are safe.
    if (Exception == CaptureException::ReturnsCannotCapture &&
        (isa<ReturnInst>(Inst) || isa<AutoreleaseReturnInst>(Inst)))
      continue;

    // We could not prove that Inst does not capture V. Be conservative and
    // return true.
    DEBUG(llvm::dbgs() << "        Could not prove that inst does not capture "
          "V!\n");
    return true;
  }

  // We successfully proved that V is not captured. Return false.
  DEBUG(llvm::dbgs() << "        V was not captured!\n");
  return false;
}

static bool isNoAliasArgument(SILValue V) {
  auto *Arg = dyn_cast<SILArgument>(V);
  if (!Arg)
    return false;

  return Arg->isFunctionArg() && V.getType().isAddress();
}

/// Return true if the pointer is to a function-local object that never escapes
/// from the function.
bool swift::isNonEscapingLocalObject(SILValue V) {
  // If this is a local allocation, or the result of a no read apply inst (which
  // can not affect memory in the caller), check to see if the allocation
  // escapes.
  if (isa<AllocationInst>(*V) || isNoReadApplyInst(V))
    return !valueMayBeCaptured(V, CaptureException::ReturnsCannotCapture);

  // If this is a no alias argument then it has not escaped before entering the
  // function. Check if it escapes inside the function.
  if (isNoAliasArgument(V))
      return !valueMayBeCaptured(V, CaptureException::ReturnsCannotCapture);

  // If this is an enum value. If it or its operand does not escape, it is
  // local.
  if (auto *EI = dyn_cast<EnumInst>(V))
    return !EI->hasOperand() ||
           !valueMayBeCaptured(EI->getOperand(),
                               CaptureException::ReturnsCannotCapture);

  // Otherwise we could not prove that V is a non escaping local object. Be
  // conservative and return false.
  return false;
}

/// Check if the value \p Value is known to be zero, non-zero or unknown.
IsZeroKind swift::isZeroValue(SILValue Value) {
  // Inspect integer literals.
  if (auto *L = dyn_cast<IntegerLiteralInst>(Value.getDef())) {
    if (L->getValue().getZExtValue() == 0)
      return IsZeroKind::Zero;
    return IsZeroKind::NotZero;
  }

  // Inspect Structs.
  switch (Value.getDef()->getKind()) {
    // Bitcast of zero is zero.
    case ValueKind::UncheckedTrivialBitCastInst:
    // Extracting from a zero class returns a zero.
    case ValueKind::StructExtractInst:
      return isZeroValue(cast<SILInstruction>(Value.getDef())->getOperand(0));
    default:
      break;
  }

  // Inspect casts.
  if (auto *AI = dyn_cast<ApplyInst>(Value.getDef())) {
    auto *FR = dyn_cast<BuiltinFunctionRefInst>(AI->getCallee());
    if (!FR)
      return IsZeroKind::Unknown;
    switch (FR->getBuiltinInfo().ID) {
      case BuiltinValueKind::IntToPtr:
      case BuiltinValueKind::PtrToInt:
      case BuiltinValueKind::ZExt:
        return isZeroValue(AI->getArgument(0));
      case BuiltinValueKind::UDiv:
      case BuiltinValueKind::SDiv: {
        if (IsZeroKind::Zero == isZeroValue(AI->getArgument(0)))
          return IsZeroKind::Zero;
        return IsZeroKind::Unknown;
      }
      case BuiltinValueKind::Mul:
      case BuiltinValueKind::SMulOver:
      case BuiltinValueKind::UMulOver: {
        IsZeroKind LHS = isZeroValue(AI->getArgument(0));
        IsZeroKind RHS = isZeroValue(AI->getArgument(1));
        if (LHS == IsZeroKind::Zero || RHS == IsZeroKind::Zero)
          return IsZeroKind::Zero;

        return IsZeroKind::Unknown;
      }
      default:
        return IsZeroKind::Unknown;
    }
  }

  // Handle results of XXX_with_overflow arithmetic.
  if (auto *T = dyn_cast<TupleExtractInst>(Value.getDef())) {
    // Make sure we are extracting the number value and not
    // the overflow flag.
    if (T->getFieldNo() != 0)
      return IsZeroKind::Unknown;

    ApplyInst *CAI = dyn_cast<ApplyInst>(T->getOperand());
    if (!CAI)
      return IsZeroKind::Unknown;

    // Check that this is a builtin function.
    if (!isa<BuiltinFunctionRefInst>(CAI->getCallee()))
      return IsZeroKind::Unknown;

    return isZeroValue(T->getOperand());
  }

  //Inspect allocations and pointer literals.
  if (isa<StringLiteralInst>(Value.getDef()) ||
      isa<AllocationInst>(Value.getDef()) ||
      isa<SILGlobalAddrInst>(Value.getDef()))
    return IsZeroKind::NotZero;

  return IsZeroKind::Unknown;
}

/// Check if the sign bit of the value \p V is known to be:
/// set (true), not set (false) or unknown (None).
Optional<bool> swift::computeSignBit(SILValue V) {
  SILValue Value = V;
  while (true) {
    auto *Def = Value.getDef();
    // Inspect integer literals.
    if (auto *L = dyn_cast<IntegerLiteralInst>(Def)) {
      if (L->getValue().isNonNegative())
        return false;
      return true;
    }

    switch (Def->getKind()) {
    // Bitcast of non-negative is non-negative
    case ValueKind::UncheckedTrivialBitCastInst:
      Value = cast<SILInstruction>(Def)->getOperand(0);
      continue;
    default:
      break;
    }

    if (auto *AI = dyn_cast<ApplyInst>(Def)) {
      auto *FR = dyn_cast<BuiltinFunctionRefInst>(AI->getCallee());
      if (!FR)
        return None;
      switch (FR->getBuiltinInfo().ID) {
      // Sizeof always returns non-negative results.
      case BuiltinValueKind::Sizeof:
        return false;
      // Strideof always returns non-negative results.
      case BuiltinValueKind::Strideof:
        return false;
      // StrideofNonZero always returns positive results.
      case BuiltinValueKind::StrideofNonZero:
        return false;
      // Alignof always returns non-negative results.
      case BuiltinValueKind::Alignof:
        return false;
      case BuiltinValueKind::LShr: {
        // If count is provably >= 1, then top bit is not set.
        auto *ILShiftCount = dyn_cast<IntegerLiteralInst>(AI->getArgument(1));
        if (ILShiftCount) {
          if (ILShiftCount->getValue().isStrictlyPositive()) {
            return false;
          }
        }
        // May be top bit is not set in the value being shifted.
        Value = AI->getArgument(0);
        continue;
      }

      // Source and target type sizes are the same.
      // S->U conversion can only succeed if
      // the sign bit of its operand is 0, i.e. it is >= 0.
      // The sign bit of a result is 0 only if the sign
      // bit of a source operand is 0.
      case BuiltinValueKind::SUCheckedConversion:
        Value = AI->getArgument(0);
        continue;

      // Source and target type sizes are the same.
      // U->S conversion can only succeed if
      // the top bit of its operand is 0, i.e.
      // it is representable as a signed integer >=0.
      // The sign bit of a result is 0 only if the sign
      // bit of a source operand is 0.
      case BuiltinValueKind::USCheckedConversion:
        Value = AI->getArgument(0);
        continue;

      // Sign bit of the operand is promoted.
      case BuiltinValueKind::SExt:
        Value = AI->getArgument(0);
        continue;

      // Source type is always smaller than the target type.
      // Therefore the sign bit of a result is always 0.
      case BuiltinValueKind::ZExt:
        return false;

      // Sign bit of the operand is promoted.
      case BuiltinValueKind::SExtOrBitCast:
        Value = AI->getArgument(0);
        continue;

      // TODO: If source type size is smaller than the target type
      // the result will be always false.
      case BuiltinValueKind::ZExtOrBitCast:
        Value = AI->getArgument(0);
        continue;

      // Inspect casts.
      case BuiltinValueKind::IntToPtr:
      case BuiltinValueKind::PtrToInt:
        Value = AI->getArgument(0);
        continue;
      default:
        return None;
      }
    }

    return None;
  }
}

/// Check if a checked trunc instruction can overflow.
/// Returns false if it can be proven that no overflow can happen.
/// Otherwise returns true.
static bool checkTruncOverflow(ApplyInst *AI, BuiltinFunctionRefInst *BFRI) {
  SILValue Left, Right;
  if (match(AI, m_CheckedTrunc(m_And(m_SILValue(Left),
                               m_SILValue(Right))))) {
    // [US]ToSCheckedTrunc(And(x, mask)) cannot overflow
    // if mask has the following properties:
    // Only the first (N-1) bits are allowed to be set, where N is the width
    // of the trunc result type.
    //
    // [US]ToUCheckedTrunc(And(x, mask)) cannot overflow
    // if mask has the following properties:
    // Only the first N bits are allowed to be set, where N is the width
    // of the trunc result type.
    if (auto BITy = AI->getType().
                        getTupleElementType(0).
                        getAs<BuiltinIntegerType>()) {
      unsigned Width = BITy->getFixedWidth();

      switch (BFRI->getBuiltinInfo().ID) {
      case BuiltinValueKind::SToSCheckedTrunc:
      case BuiltinValueKind::UToSCheckedTrunc:
        // If it is a trunc to a signed value
        // then sign bit should not be set to avoid overflows.
        --Width;
        break;
      default:
        break;
      }

      if (auto *ILLeft = dyn_cast<IntegerLiteralInst>(Left)) {
        APInt Value = ILLeft->getValue();
        if (Value.isIntN(Width)) {
          return false;
        }
      }

      if (auto *ILRight = dyn_cast<IntegerLiteralInst>(Right)) {
        APInt Value = ILRight->getValue();
        if (Value.isIntN(Width)) {
          return false;
        }
      }
    }
  }
  return true;
}

/// Check if execution of a given Apply instruction can result in overflows.
/// Returns true if an overflow can happen. Otherwise returns false.
bool swift::canOverflow(ApplyInst *AI) {
  if (simplifyOverflowBuiltinInstruction(AI) != SILValue())
    return false;

  if (auto *BFRI = dyn_cast<BuiltinFunctionRefInst>(AI->getCallee())) {
    if (!checkTruncOverflow(AI, BFRI))
      return false;
  }
  // Conservatively assume that an overflow can happen
  return true;
}

