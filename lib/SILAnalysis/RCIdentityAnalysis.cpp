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
#include "swift/SIL/SILInstruction.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

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

/// Return the underlying SILValue after stripping off SILArguments that can not
/// affect RC identity if our BB has only one predecessor.
SILValue
RCIdentityAnalysis::
stripRCIdentityPreservingArgs(SILValue V, unsigned RecursionDepth) {
  auto *A = dyn_cast<SILArgument>(V);
  if (!A)
    return SILValue();

  // If we already visited this BB, don't reprocess it since we have a cycle.
  if (!VisitedArgs.insert(A).second)
    return SILValue();

  // Ok, this is the first time that we have visited this BB. Get the
  // SILArgument's incoming values. If we don't have an incoming value for each
  // one of our predecessors, just return SILValue().
  llvm::SmallVector<SILValue, 8> IncomingValues;
  if (!A->getIncomingValues(IncomingValues) || IncomingValues.empty())
    return SILValue();

  // Ok, we have incoming values to process. Grab the first incoming SILValue
  // and get its identity root. We do not increment the RecursionDepth here
  // since in the case with a single predecessor, we want to strip off all
  // arguments.
  SILValue FirstIV = getRCIdentityRootInner(IncomingValues[0],
                                            RecursionDepth+1);

  // If we did not find a FirstIV, return an empty SILValue.
  if (!FirstIV)
    return SILValue();

  unsigned IVListSize = IncomingValues.size();

  // If we only have one predecessor, just return FirstIV.
  if (IVListSize == 1)
    return FirstIV;

  // Ok, we have multiple predecessors. If ignoring non-payloaded enums all of
  // the incoming arguments match FirstIV, we can strip off the argument and
  // return FirstIV. This first means that we need to ensure that FirstIV is not
  // a no payload enum. Go through the list of incoming values until we find the
  // first value that is not a no payload enum. Assign it to FirstIV so we can
  // compare the rest of the no payload enum arguments with it.
  unsigned i = 1;
  while (isNoPayloadEnum(FirstIV) && i < IVListSize) {
    // We do not increment the RecursionDepth here to ensure that if we have a
    // single payload enum that we can strip off all such arguments. We use
    // RecursionDepth just to make sure that we do not run into compile time
    // problems when processing multiple Phis.
    FirstIV = getRCIdentityRootInner(IncomingValues[i++], RecursionDepth+1);

    // If we fail to find the root of FirstIV. Return SILValue().
    if (!FirstIV)
      return SILValue();
  }

  // Then compare the rest of the arguments with FirstIV now that we know that
  // FirstIV is not a no payload enum.
  while (i < IVListSize) {
    // We increment the recursion depth here
    SILValue IV = getRCIdentityRootInner(IncomingValues[i++], RecursionDepth+1);

    // If we fail to find the root of this incoming value, bail and return
    // SILValue().
    if (!IV)
      return SILValue();

    // If IV is a no payload enum, we don't care about it. Skip it.
    if (isNoPayloadEnum(IV))
      continue;

    // Otherwise, if IV does not equal FirstIV, we have a conflicting value so
    // return SILValue().
    if (IV != FirstIV)
      return SILValue();
  }

  // Ok all our values match! Return FirstIV.
  return FirstIV;
}

static bool isRCIdentityPreservingCast(ValueKind Kind) {
  switch (Kind) {
  case ValueKind::UpcastInst:
  case ValueKind::AddressToPointerInst:
  case ValueKind::PointerToAddressInst:
  case ValueKind::UncheckedRefCastInst:
  case ValueKind::UncheckedAddrCastInst:
  case ValueKind::RefToRawPointerInst:
  case ValueKind::RawPointerToRefInst:
  case ValueKind::UnconditionalCheckedCastInst:
  case ValueKind::UncheckedRefBitCastInst:
    return true;
  default:
    return false;
  }
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

  // If we have an unchecked_enum_data, strip off the unchecked_enum_data.
  if (auto *UEDI = dyn_cast<UncheckedEnumDataInst>(V))
    return UEDI->getOperand();

  // If we have an enum instruction with a payload, strip off the enum to
  // expose the enum's payload.
  if (auto *EI = dyn_cast<EnumInst>(V))
    if (EI->hasOperand())
      return EI->getOperand();

  return SILValue();
}

SILValue
RCIdentityAnalysis::
stripRCIdentityPreservingOps(SILValue V, unsigned RecursionDepth) {
  while (true) {
    // First strip off any RC identity preserving instructions. This is cheap.
    if (SILValue NewV = stripRCIdentityPreservingInsts(V)) {
      V = NewV;
      continue;
    }

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

static llvm::cl::opt<bool>
EnableRCIdentityAnalysis("enable-rc-id-analysis", llvm::cl::init(false));

SILValue RCIdentityAnalysis::getRCIdentityRoot(SILValue V) {
  // If the analysis is not enabled, just call the old SILValue method. We don't
  // want this risk for OzU.
  if (!EnableRCIdentityAnalysis)
    return V.stripRCIdentityPreservingOps();

  SILValue Root = getRCIdentityRootInner(V, 0);
  VisitedArgs.clear();

  // If we fail to find a root, return V.
  if (!Root)
    return V;

  return Root;
}

SILAnalysis *swift::createRCIdentityAnalysis(SILModule *M) {
  return new RCIdentityAnalysis(M);
}
