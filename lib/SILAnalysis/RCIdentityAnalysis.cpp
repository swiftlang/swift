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
//                          RCIdentityRoot Analysis
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

/// V is the incoming value for the SILArgument A on at least one path.  Find a
/// value that is trivially RC-identical to V and dominates the argument's
/// block. If such a value exists, it is a candidate for RC-indentity with the
/// argument itself--the caller must verify this after evaluating all paths.
SILValue RCIdentityAnalysis::stripOneRCIdentityIncomingValue(SILArgument *A,
                                                             SILValue V) {
  // Strip off any non-argument instructions from IV. We know that this will
  // always result in RCIdentical values without additional analysis.
  while (SILValue NewIV = stripRCIdentityPreservingInsts(V))
    V = NewIV;

  // Then make sure that this incoming value is from a BB which is different
  // from our BB and dominates our BB. Otherwise, return SILValue() to bail.
  DominanceInfo *DI = DA->getDomInfo(A->getFunction());
  if (!dominatesArgument(DI, A, V))
    return SILValue();

  // In the future attempt to recursively strip here. We are being more
  // conservative than most likely necessary.
  return V;
}

/// Returns true if we proved that RCIdentity has a non-payloaded enum case,
/// false if RCIdentity has a payloaded enum case, and None if we failed to find
/// anything.
static llvm::Optional<bool> proveNonPayloadedEnumCase(SILBasicBlock *BB,
                                                      SILValue RCIdentity) {
  // Then see if BB has one predecessor... if it does not, return None so we
  // keep searching up the domtree.
  SILBasicBlock *SinglePred = BB->getSinglePredecessor();
  if (!SinglePred)
    return None;

  // Check if SinglePred has a switch_enum terminator switching on
  // RCIdentity... If it does not, return None so we keep searching up the
  // domtree.
  auto *SEI = dyn_cast<SwitchEnumInst>(SinglePred->getTerminator());
  if (!SEI || SEI->getOperand() != RCIdentity)
    return None;

  // Then return true if along the edge from the SEI to BB, RCIdentity has a
  // non-payloaded enum value.
  NullablePtr<EnumElementDecl> Decl = SEI->getUniqueCaseForDestination(BB);
  if (Decl.isNull())
    return None;
  return !Decl.get()->hasArgumentType();
}

bool RCIdentityAnalysis::
findDominatingNonPayloadedEdge(SILBasicBlock *IncomingEdgeBB,
                               SILValue RCIdentity) {
  // First grab the NonPayloadedEnumBB and RCIdentityBB. If we can not find
  // either of them, return false.
  SILBasicBlock *RCIdentityBB = RCIdentity->getParentBB();
  if (!RCIdentityBB)
    return false;

  // Make sure that the incoming edge bb is not the RCIdentityBB. We are not
  // trying to handle this case here, so simplify by just bailing if we detect
  // it.
  //
  // I think the only way this can happen is if we have a switch_enum of some
  // sort with multiple incoming values going into the destination BB. We are
  // not interested in handling that case anyways.
  //
  // FIXME: If we ever split all critical edges, this should be relooked at.
  if (IncomingEdgeBB == RCIdentityBB)
    return false;

  // Now we know that RCIdentityBB and IncomingEdgeBB are different. Prove that
  // RCIdentityBB dominates IncomingEdgeBB.
  SILFunction *F = RCIdentityBB->getParent();

  // First make sure that IncomingEdgeBB dominates NonPayloadedEnumBB. If not,
  // return false.
  DominanceInfo *DI = DA->getDomInfo(F);
  if (!DI->dominates(RCIdentityBB, IncomingEdgeBB))
    return false;

  // Now walk up the dominator tree from IncomingEdgeBB to RCIdentityBB and see
  // if we can find a use of RCIdentity that dominates IncomingEdgeBB and
  // enables us to know that RCIdentity must be a no-payload enum along
  // IncomingEdge. We don't care if the case or enum of RCIdentity match the
  // case or enum along RCIdentityBB since a pairing of retain, release of two
  // non-payloaded enums can always be eliminated (since we can always eliminate
  // ref count operations on non-payloaded enums).

  // RCIdentityBB must have a valid dominator tree node.
  auto *EndDomNode = DI->getNode(RCIdentityBB);
  if (!EndDomNode)
    return false;

  for (auto *Node = DI->getNode(IncomingEdgeBB); Node; Node = Node->getIDom()) {
    // Search for uses of RCIdentity in Node->getBlock() that will enable us to
    // know that it has a non-payloaded enum case.
    SILBasicBlock *DominatingBB = Node->getBlock();
    llvm::Optional<bool> Result =
        proveNonPayloadedEnumCase(DominatingBB, RCIdentity);

    // If we found either a signal of a payloaded or a non-payloaded enum,
    // return that value.
    if (Result.hasValue())
      return Result.getValue();

    // If we didn't reach RCIdentityBB, keep processing up the DomTree.
    if (DominatingBB != RCIdentityBB)
      continue;

    // Otherwise, we failed to find any interesting information, return false.
    return false;
  }

  return false;
}

/// Return the underlying SILValue after stripping off SILArguments that can not
/// affect RC identity.
///
/// This code is meant to enable RCIdentity to be ascertained in the following
/// cases:
///
/// 1. Where we have an unneeded phi node (i.e. all incoming values are the same
/// argument). This helps to avoid phase ordering issues (simplify-cfg *should*
/// catch this).
///
/// 2. Cases where we break apart an enum and then reform it from its individual
/// cases. The main problem here is when the non-payloaded cases are created
/// with new enum instructions (which happens when casting sometimes):
///
///   bb9:
///     ...
///     switch_enum %0 : $Optional<T>, #Optional.None: bb10,
///                                    #Optional.Some: bb11
///
///   bb10:
///     %1 = enum $Optional<U>, #Optional.None
///     br bb12(%1 : $Optional<U>)
///
///   bb11:
///     %2 = some_cast_to_u %0 : ...
///     %3 = enum $Optional<U>, #Optional.Some, %2 : $U
///     br bb12(%3 : $Optional<U>)
///
///   bb12(%4 : $Optional<U>):
///     ...
///
/// In this case, we want to be able to infer that %0 and %4 have the same ref
/// count identity. The key thing we have to be careful of is that %0 must have
/// the same enum case as %1 along the edge from bb10 to bb12. Otherwise, we can
/// potentially mismatch
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
  llvm::SmallVector<std::pair<SILBasicBlock *, SILValue>, 8> IncomingValues;
  if (!A->getIncomingValues(IncomingValues) || IncomingValues.empty()) {
    return SILValue();
  }

  unsigned IVListSize = IncomingValues.size();

  // If we only have one incoming value, just return the identity root of that
  // incoming value. There can be no loop problems.
  if (IVListSize == 1) {
    return IncomingValues[0].second;
  }

  // Ok, we have multiple predecessors. First find the first non-payloaded enum.
  llvm::SmallVector<SILBasicBlock *, 8> NoPayloadEnumBBs;
  unsigned i = 0;
  for (; i < IVListSize && isNoPayloadEnum(IncomingValues[i].second); ++i) {
    NoPayloadEnumBBs.push_back(IncomingValues[i].first);
  }

  // If we did not find any non-payloaded enum, there is no RC associated with
  // this Phi node. Just return SILValue().
  if (i == IVListSize)
    return SILValue();

  SILValue FirstIV =
      stripOneRCIdentityIncomingValue(A, IncomingValues[i].second);
  if (!FirstIV)
    return SILValue();

  while (i < IVListSize) {
    SILBasicBlock *IVBB;
    SILValue IV;
    std::tie(IVBB, IV) = IncomingValues[i++];

    // If IV is a no payload enum, we don't care about it. Skip it.
    if (isNoPayloadEnum(IV)) {
      NoPayloadEnumBBs.push_back(IVBB);
      continue;
    }

    // Try to strip off the RCIdentityPresrvingArg for IV. If it matches
    // FirstIV, we may be able to succeed here.
    if (FirstIV == stripOneRCIdentityIncomingValue(A, IV))
      continue;

    // Otherwise, just return SILValue().
    return SILValue();
  }

  // We now know that all incoming values, other than NoPayloadEnums, are
  // FirstIV after trivially stripping RCIdentical instructions. If we have no
  // NoPayloadEnums, then we know that this Arg's RCIdentity must be FirstIV.
  if (NoPayloadEnumBBs.empty())
    return FirstIV;

  // At this point, we know that we have *some* NoPayloadEnums. If FirstIV is
  // not an enum, then we must bail. We do not try to analyze this case.
  if (!FirstIV.getType().getEnumOrBoundGenericEnum())
    return SILValue();

  // Now we know that FirstIV is an enum and that all payloaded enum cases after
  // just stripping off instructions are FirstIV. Now we need to make sure that
  // each non-payloaded enum value is safe to ignore.
  //
  // Let IVE be the edge for the non-payloaded enum. It is only safe to perform
  // this operation when there exists a dominating edge E' of IVE for which
  // FirstIV also takes on a non-payloaded enum value.
  if (std::any_of(NoPayloadEnumBBs.begin(), NoPayloadEnumBBs.end(),
                  [&](SILBasicBlock *BB) -> bool {
                    return !findDominatingNonPayloadedEdge(BB, FirstIV);
                  }))
    return SILValue();

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
//                              RCUser Analysis
//===----------------------------------------------------------------------===//

/// Is this a user that represents an escape of user from ARC control. This
/// means that from an RC use perspective, the object can be ignored since it is
/// up to the frontend to communicate via fix_lifetime and mark_dependence these
/// dependencies.
static bool isNonOverlappingTrivialAccess(SILInstruction *User) {
  if (auto *TEI = dyn_cast<TupleExtractInst>(User)) {
    // If the tuple we are extracting from only has one non trivial element and
    // we are not extracting from that element, this is an ARC escape.
    return TEI->isTrivialEltOfOneRCIDTuple();
  }

  if (auto *SEI = dyn_cast<StructExtractInst>(User)) {
    // If the struct we are extracting from only has one non trivial element and
    // we are not extracting from that element, this is an ARC escape.
    return SEI->isTrivialFieldOfOneRCIDStruct();
  }

  return false;
}

/// Return all recursive users of V, looking through users which propagate
/// RCIdentity. *NOTE* This ignores obvious ARC escapes where the a potential
/// user of the RC is not managed by ARC.
///
/// We only use the instruction analysis here.
void RCIdentityAnalysis::getRCUsers(
    SILValue InputValue, llvm::SmallVectorImpl<SILInstruction *> &Users) {
  // Add V to the worklist.
  llvm::SmallVector<SILValue, 8> Worklist;
  Worklist.push_back(InputValue);

  // A set used to ensure we only visit users once.
  llvm::SmallPtrSet<SILInstruction *, 8> VisitedInsts;

  // Then until we finish the worklist...
  while (!Worklist.empty()) {
    // Pop off the top value.
    SILValue V = Worklist.pop_back_val();

    // For each user of V...
    for (auto *Op : V.getUses()) {
      SILInstruction *User = Op->getUser();

      // If we have already visited this user, continue.
      if (!VisitedInsts.insert(User).second)
        continue;

      // Otherwise attempt to strip off one layer of RC identical instructions
      // from User.
      SILValue StrippedRCID = stripRCIdentityPreservingInsts(User);

      // If StrippedRCID is not V, then we know that User's result is
      // conservatively not RCIdentical to V.
      if (StrippedRCID != V) {
        // If the user is extracting a trivial field of an aggregate structure
        // that does not overlap with the ref counted part of the aggregate, we
        // can ignore it.
        if (isNonOverlappingTrivialAccess(User))
          continue;

        // Otherwise, it is an RC user that our user wants.
        Users.push_back(User);
        continue;
      }

      // Otherwise, add all of User's uses to our list to continue searching.
      for (unsigned i = 0, e = User->getNumTypes(); i != e; ++i) {
        Worklist.push_back(SILValue(User, i));
      }
    }
  }
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
