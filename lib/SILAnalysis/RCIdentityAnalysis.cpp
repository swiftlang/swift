//===--- RCIdentityAnalysis.cpp -------------------------------------------===//
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

#include "swift/SILAnalysis/RCIdentityAnalysis.h"
#include "swift/SILAnalysis/DominanceAnalysis.h"
#include "swift/SIL/SILInstruction.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                                  Utility
//===----------------------------------------------------------------------===//

/// Returns true if V is an enum without a payload.
///
/// We perform this computation by checking if V is an enum instruction without
/// an argument. I am using a helper here in case I find more cases where I need
/// to expand it.
static bool isNoPayloadEnum(SILValue V) {
  auto *EI = dyn_cast<EnumInst>(V);
  if (!EI)
    return false;

  return !EI->hasOperand();
}

static bool isRCIdentityPreservingCast(ValueKind Kind) {
  switch (Kind) {
  case ValueKind::UpcastInst:
  case ValueKind::UncheckedRefCastInst:
  case ValueKind::UncheckedAddrCastInst:
  case ValueKind::UnconditionalCheckedCastInst:
  case ValueKind::UncheckedRefBitCastInst:
  case ValueKind::InitExistentialRefInst:
  case ValueKind::OpenExistentialRefInst:
  case ValueKind::RefToBridgeObjectInst:
  case ValueKind::BridgeObjectToRefInst:
    return true;
  default:
    return false;
  }
}

//===----------------------------------------------------------------------===//
//                                  Analysis
//===----------------------------------------------------------------------===//

/// Returns true if FirstIV is a SILArgument or SILInstruction in a BB that
/// dominates the BB of A.
static bool dominatesArgument(DominanceInfo *DI, SILArgument *A,
                              SILValue FirstIV) {
  SILBasicBlock *OtherBB = FirstIV->getParentBB();
  if (!OtherBB || OtherBB == A->getParent())
    return false;
  return DI->dominates(OtherBB, A->getParent());
}

static SILValue stripRCIdentityPreservingInsts(SILValue V) {
  // First strip off RC identity preserving casts.
  if (isRCIdentityPreservingCast(V->getKind()))
    return cast<SILInstruction>(V.getDef())->getOperand(0);

  // Then if we have a struct_extract that is extracting a non-trivial member
  // from a struct with no other non-trivial members, a ref count operation on
  // the struct is equivalent to a ref count operation on the extracted
  // member. Strip off the extract.
  if (auto *SEI = dyn_cast<StructExtractInst>(V))
    if (SEI->isFieldOnlyNonTrivialField())
      return SEI->getOperand();

  // If we have a struct instruction with only one non-trivial stored field, the
  // only reference count that can be modified is the non-trivial field. Return
  // the non-trivial field.
  if (auto *SI = dyn_cast<StructInst>(V))
    if (SILValue NewValue = SI->getUniqueNonTrivialFieldValue())
      return NewValue;

  // If we have an unchecked_enum_data, strip off the unchecked_enum_data.
  if (auto *UEDI = dyn_cast<UncheckedEnumDataInst>(V))
    return UEDI->getOperand();

  // If we have an enum instruction with a payload, strip off the enum to
  // expose the enum's payload.
  if (auto *EI = dyn_cast<EnumInst>(V))
    if (EI->hasOperand())
      return EI->getOperand();

  // If we have a tuple_extract that is extracting the only non trivial member
  // of a tuple, a retain_value on the tuple is equivalent to a retain_value on
  // the extracted value.
  if (auto *TEI = dyn_cast<TupleExtractInst>(V))
    if (TEI->isEltOnlyNonTrivialElt())
      return TEI->getOperand();

  // If we are forming a tuple and the tuple only has one element with reference
  // semantics, a retain_value on the tuple is equivalent to a retain value on
  // the tuple operand.
  if (auto *TI = dyn_cast<TupleInst>(V))
    if (SILValue NewValue = TI->getUniqueNonTrivialElt())
      return NewValue;

  return SILValue();
}

/// Return the underlying SILValue after stripping off SILArguments that can not
/// affect RC identity if our BB has only one predecessor.
SILValue
RCIdentityAnalysis::
stripRCIdentityPreservingArgs(SILValue V, unsigned RecursionDepth) {
  auto *A = dyn_cast<SILArgument>(V);
  if (!A) {
    return SILValue();
  }

  // If we already visited this BB, don't reprocess it since we have a cycle.
  if (!VisitedArgs.insert(A).second) {
    return SILValue();
  }

  // Ok, this is the first time that we have visited this BB. Get the
  // SILArgument's incoming values. If we don't have an incoming value for each
  // one of our predecessors, just return SILValue().
  llvm::SmallVector<SILValue, 8> IncomingValues;
  if (!A->getIncomingValues(IncomingValues) || IncomingValues.empty()) {
    return SILValue();
  }

  unsigned IVListSize = IncomingValues.size();

  // If we only have one incoming value, just return the identity root of that
  // incoming value. There can be no loop problems.
  if (IVListSize == 1) {
    return getRCIdentityRootInner(IncomingValues[0], RecursionDepth+1);
  }

  // Ok, we have multiple predecessors. First find the first non-payloaded enum.
  unsigned i = 0;
  for (; i < IVListSize && isNoPayloadEnum(IncomingValues[i]); ++i) {}

  // If we did not find any non-payloaded enum, there is no RC associated with
  // this Phi node. Just return SILValue().
  if (i == IVListSize)
    return SILValue();

  SILValue FirstIV = IncomingValues[i];

  // Strip off any non-argument instructions from IV. We know that 
  while (SILValue NewIV = stripRCIdentityPreservingInsts(FirstIV))
    FirstIV = NewIV;

  // Then make sure that this incoming value is from a BB which is different
  // from our BB and dominates our BB. Otherwise, just return SILValue() there
  // is nothing we can do hter.
  DominanceInfo *DI = DA->getDomInfo(A->getFunction());
  if (!dominatesArgument(DI, A, FirstIV))
    return SILValue();

  // Ok, it is safe to continue stripping from this argument. Perform the
  // recursive stripping.
  FirstIV = getRCIdentityRootInner(FirstIV, RecursionDepth+1);
  while (i < IVListSize) {
    SILValue IV = IncomingValues[i++];

    // If IV is a no payload enum, we don't care about it. Skip it.
    if (isNoPayloadEnum(IV))
      continue;

    // Strip off any non-argument instructions from IV. We know that 
    while (SILValue NewIV = stripRCIdentityPreservingInsts(IV))
      IV = NewIV;

    // Then make sure that this incoming value is from a BB which is different
    // from our BB and dominates our BB. Otherwise, just return SILValue() there
    // is nothing we can do hter.
    if (!dominatesArgument(DI, A, IV))
      return SILValue();

    // Ok, it is safe to continue stripping incoming values. Perform the
    // stripping and check if the stripped value equals FirstIV. If it is not
    // so, then bail. We found a conflicting value in our merging.
    if (getRCIdentityRootInner(IV, RecursionDepth+1) != FirstIV)
      return SILValue();
  }

  // Ok all our values match! Return FirstIV.
  return FirstIV;
}

llvm::cl::opt<bool> StripOffArgs(
    "enable-rc-identity-arg-strip", llvm::cl::init(true),
    llvm::cl::desc("Should RC identity try to strip off arguments"));

SILValue
RCIdentityAnalysis::
stripRCIdentityPreservingOps(SILValue V, unsigned RecursionDepth) {
  while (true) {
    // First strip off any RC identity preserving instructions. This is cheap.
    if (SILValue NewV = stripRCIdentityPreservingInsts(V)) {
      V = NewV;
      continue;
    }

    if (!StripOffArgs)
      break;

    // Once we have done all of the easy work, try to see if we can strip off
    // any RCIdentityPreserving args. This is potentially expensive since we
    // need to perform additional stripping on the argument provided to this
    // argument from each predecessor BB. There is a counter in
    // getRCIdentityRootInner that ensures we don't do too many.
    SILValue NewV = stripRCIdentityPreservingArgs(V, RecursionDepth);
    if (!NewV)
      break;

    V = NewV;
  }

  return V;
}

//===----------------------------------------------------------------------===//
//                              Main Entry Point
//===----------------------------------------------------------------------===//

SILValue RCIdentityAnalysis::getRCIdentityRootInner(SILValue V,
                                                    unsigned RecursionDepth) {
  // Only allow this method to be recursed on for a limited number of times to
  // make sure we don't explode compile time.
  if (RecursionDepth >= MaxRecursionDepth)
    return SILValue();

  // First check the cache.
  auto Pair = Cache.find(V);
  if (Pair != Cache.end())
    return Pair->second;

  SILValue NewValue = stripRCIdentityPreservingOps(V, RecursionDepth);
  if (!NewValue)
    return SILValue();

  // We can get back V if our analysis completely fails. There is no point in
  // storing this value into the cache so just return it.
  if (NewValue == V)
    return V;

  return Cache[V] = NewValue;
}

SILValue RCIdentityAnalysis::getRCIdentityRoot(SILValue V) {
  SILValue Root = getRCIdentityRootInner(V, 0);
  VisitedArgs.clear();

  // If we fail to find a root, return V.
  if (!Root)
    return V;

  return Root;
}

SILAnalysis *swift::createRCIdentityAnalysis(SILModule *M, SILPassManager *PM) {
  return new RCIdentityAnalysis(M, PM);
}
