//===--- PartitionUtils.cpp -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SILOptimizer/Utils/PartitionUtils.h"
#include "swift/AST/Expr.h"
#include "swift/SIL/ApplySite.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;
using namespace swift::PartitionPrimitives;

//===----------------------------------------------------------------------===//
//                               MARK: Logging
//===----------------------------------------------------------------------===//

#ifndef NDEBUG

bool swift::PartitionPrimitives::REGIONBASEDISOLATION_ENABLE_VERBOSE_LOGGING;

static llvm::cl::opt<bool, true> // The parser
    RegionBasedIsolationVerboseLog(
        "sil-regionbasedisolation-verbose-log",
        llvm::cl::desc("Enable verbose logging for SIL region based isolation "
                       "diagnostics"),
        llvm::cl::Hidden,
        llvm::cl::location(swift::PartitionPrimitives::
                               REGIONBASEDISOLATION_ENABLE_VERBOSE_LOGGING));

#endif

//===----------------------------------------------------------------------===//
//                           MARK: SILIsolationInfo
//===----------------------------------------------------------------------===//

SILIsolationInfo SILIsolationInfo::get(SILInstruction *inst) {
  if (ApplyExpr *apply = inst->getLoc().getAsASTNode<ApplyExpr>()) {
    if (auto crossing = apply->getIsolationCrossing()) {
      if (crossing->getCalleeIsolation().isActorIsolated())
        return SILIsolationInfo::getActorIsolated(
            crossing->getCalleeIsolation());
    }
  }

  if (auto fas = FullApplySite::isa(inst)) {
    if (auto crossing = fas.getIsolationCrossing()) {
      if (crossing->getCalleeIsolation().isActorIsolated()) {
        return SILIsolationInfo::getActorIsolated(
            crossing->getCalleeIsolation());
      }
    }

    if (fas.hasSelfArgument()) {
      auto &self = fas.getSelfArgumentOperand();
      if (fas.getArgumentParameterInfo(self).hasOption(
              SILParameterInfo::Isolated)) {
        if (auto *nomDecl =
                self.get()->getType().getNominalOrBoundGenericNominal()) {
          // TODO: We should be doing this off of the instance... what if we
          // have two instances of the same class?
          return SILIsolationInfo::getActorIsolated(nomDecl);
        }
      }
    }
  }

  if (auto *pai = dyn_cast<PartialApplyInst>(inst)) {
    if (auto *ace = pai->getLoc().getAsASTNode<AbstractClosureExpr>()) {
      auto actorIsolation = ace->getActorIsolation();
      if (actorIsolation.isActorIsolated()) {
        return SILIsolationInfo::getActorIsolated(actorIsolation);
      }
    }
  }

  // We assume that any instruction that does not correspond to an ApplyExpr
  // cannot cross an isolation domain.
  return SILIsolationInfo();
}

SILIsolationInfo SILIsolationInfo::get(SILFunctionArgument *arg) {
  // If we have self and our function is actor isolated, all of our arguments
  // should be marked as actor isolated.
  if (auto *self = arg->getFunction()->maybeGetSelfArgument()) {
    if (auto functionIsolation = arg->getFunction()->getActorIsolation()) {
      if (functionIsolation.isActorIsolated()) {
        if (auto *nomDecl = self->getType().getNominalOrBoundGenericNominal()) {
          if (auto isolationInfo =
                  SILIsolationInfo::getActorIsolated(nomDecl)) {
            return isolationInfo;
          }
        }
      }
    }
  }

  if (auto *decl = arg->getDecl()) {
    auto isolation = swift::getActorIsolation(const_cast<ValueDecl *>(decl));
    if (!bool(isolation)) {
      if (auto *dc = decl->getDeclContext()) {
        isolation = swift::getActorIsolationOfContext(dc);
      }
    }

    if (isolation.isActorIsolated()) {
      return SILIsolationInfo::getActorIsolated(isolation);
    }
  }

  return SILIsolationInfo::getTaskIsolated(arg);
}

void SILIsolationInfo::print(llvm::raw_ostream &os) const {
  switch (Kind(*this)) {
  case Unknown:
    os << "unknown";
    return;
  case Disconnected:
    os << "disconnected";
    return;
  case Actor:
    os << "actor";
    return;
  case Task:
    os << "task";
    return;
  }
}

NominalTypeDecl *SILIsolationInfo::tryInferActorDecl() const {
  if (hasActorIsolation()) {
    auto actorIsolation = getActorIsolation();
    if (auto *actor = actorIsolation->getActorOrNullPtr()) {
      return actor;
    }
    return nullptr;
  }

  if (hasActorInstance()) {
    auto actorDecl = getActorInstance();
    return actorDecl;
  }

  return nullptr;
}

SILIsolationInfo SILIsolationInfo::merge(SILIsolationInfo other) const {
  // If we are greater than the other kind, then we are further along the
  // lattice. We ignore the change.
  if (unsigned(other.kind) < unsigned(kind))
    return *this;

  // TODO: Make this failing mean that we emit an unknown SIL error instead of
  // asserting.
  assert((!other.isActorIsolated() || !isActorIsolated() || *this == other) &&
         "Actor can only be merged with the same actor");

  // Otherwise, take the other value.
  return other;
}

bool SILIsolationInfo::operator==(const SILIsolationInfo &other) const {
  if (getKind() != other.getKind())
    return false;

  switch (getKind()) {
  case Unknown:
  case Disconnected:
    return true;
  case Task:
    return getTaskIsolatedValue() == other.getTaskIsolatedValue();
  case Actor:
    // First try to use actor isolation if we have them.
    if (hasActorIsolation() && other.hasActorIsolation()) {
      auto lhsIsolation = getActorIsolation();
      auto rhsIsolation = other.getActorIsolation();
      if (lhsIsolation && rhsIsolation)
        return *lhsIsolation == *rhsIsolation;
    }

    // Otherwise, try to use the inferred actor decl.
    auto *lhsDecl = tryInferActorDecl();
    auto *rhsDecl = other.tryInferActorDecl();
    if (lhsDecl && rhsDecl)
      return lhsDecl == rhsDecl;

    // Otherwise, false, they are not equal.
    return false;
  }
}

void SILIsolationInfo::Profile(llvm::FoldingSetNodeID &id) const {
  id.AddInteger(getKind());
  switch (getKind()) {
  case Unknown:
  case Disconnected:
    return;
  case Task:
    id.AddPointer(getTaskIsolatedValue());
    return;
  case Actor:
    // We profile in integer cases here so that we can always distinguish in
    // between the various cases and the non-case. Just being paranoid.
    if (hasActorIsolation()) {
      if (auto isolation = getActorIsolation()) {
        id.AddInteger(1);
        return isolation->Profile(id);
      }
    }

    if (hasActorInstance()) {
      id.AddInteger(2);
      return id.AddPointer(getActorInstance());
    }

    id.AddInteger(3);
    break;
  }
}

//===----------------------------------------------------------------------===//
//                             MARK: PartitionOp
//===----------------------------------------------------------------------===//

void PartitionOp::print(llvm::raw_ostream &os, bool extraSpace) const {
  switch (opKind) {
  case PartitionOpKind::Assign: {
    constexpr static char extraSpaceLiteral[10] = "      ";
    os << "assign ";
    if (extraSpace)
      os << extraSpaceLiteral;
    os << "%%" << opArgs[0] << " = %%" << opArgs[1];
    break;
  }
  case PartitionOpKind::AssignFresh:
    os << "assign_fresh %%" << opArgs[0];
    break;
  case PartitionOpKind::Transfer: {
    constexpr static char extraSpaceLiteral[10] = "    ";
    os << "transfer ";
    if (extraSpace)
      os << extraSpaceLiteral;
    os << "%%" << opArgs[0];
    break;
  }
  case PartitionOpKind::UndoTransfer: {
    constexpr static char extraSpaceLiteral[10] = "    ";
    os << "undo_transfer ";
    if (extraSpace)
      os << extraSpaceLiteral;
    os << "%%" << opArgs[0];
    break;
  }
  case PartitionOpKind::Merge: {
    constexpr static char extraSpaceLiteral[10] = "       ";
    os << "merge ";
    if (extraSpace)
      os << extraSpaceLiteral;
    os << "%%" << opArgs[0] << " with %%" << opArgs[1];
    break;
  }
  case PartitionOpKind::Require: {
    constexpr static char extraSpaceLiteral[10] = "     ";
    os << "require ";
    if (extraSpace)
      os << extraSpaceLiteral;
    os << "%%" << opArgs[0];
    break;
  }
  }
  os << ": " << *getSourceInst();
}

//===----------------------------------------------------------------------===//
//                              MARK: Partition
//===----------------------------------------------------------------------===//

Partition Partition::singleRegion(ArrayRef<Element> indices) {
  Partition p;
  if (!indices.empty()) {
    Region min_index =
        Region(*std::min_element(indices.begin(), indices.end()));
    p.fresh_label = Region(min_index + 1);
    for (Element index : indices) {
      p.elementToRegionMap.insert_or_assign(index, min_index);
    }
  }

  assert(p.is_canonical_correct());
  return p;
}

Partition Partition::separateRegions(ArrayRef<Element> indices) {
  Partition p;
  if (indices.empty())
    return p;

  auto maxIndex = Element(0);
  for (Element index : indices) {
    p.elementToRegionMap.insert_or_assign(index, Region(index));
    maxIndex = Element(std::max(maxIndex, index));
  }
  p.fresh_label = Region(maxIndex + 1);
  assert(p.is_canonical_correct());
  return p;
}

void Partition::markTransferred(Element val,
                                TransferringOperandSet *transferredOperandSet) {
  // First see if our val is tracked. If it is not tracked, insert it and mark
  // its new region as transferred.
  if (!isTrackingElement(val)) {
    elementToRegionMap.insert_or_assign(val, fresh_label);
    regionToTransferredOpMap.insert({fresh_label, transferredOperandSet});
    fresh_label = Region(fresh_label + 1);
    canonical = false;
    return;
  }

  // Otherwise, we already have this value in the map. Try to insert it.
  auto iter1 = elementToRegionMap.find(val);
  assert(iter1 != elementToRegionMap.end());
  auto iter2 = regionToTransferredOpMap.try_emplace(iter1->second,
                                                    transferredOperandSet);

  // If we did insert, just return. We were not tracking any state.
  if (iter2.second)
    return;

  // Otherwise, we need to merge the sets.
  iter2.first->getSecond() = iter2.first->second->merge(transferredOperandSet);
}

bool Partition::undoTransfer(Element val) {
  // First see if our val is tracked. If it is not tracked, insert it.
  if (!isTrackingElement(val)) {
    elementToRegionMap.insert_or_assign(val, fresh_label);
    fresh_label = Region(fresh_label + 1);
    canonical = false;
    return true;
  }

  // Otherwise, we already have this value in the map. Remove it from the
  // transferred map.
  auto iter1 = elementToRegionMap.find(val);
  assert(iter1 != elementToRegionMap.end());
  return regionToTransferredOpMap.erase(iter1->second);
}

void Partition::trackNewElement(Element newElt) {
  SWIFT_DEFER { validateRegionToTransferredOpMapRegions(); };

  // First try to emplace newElt with fresh_label.
  auto iter = elementToRegionMap.try_emplace(newElt, fresh_label);

  // If we did insert, then we know that the value is completely new. We can
  // just update the fresh_label, set canonical to false, and return.
  if (iter.second) {
    // Increment the fresh label so it remains fresh.
    fresh_label = Region(fresh_label + 1);
    canonical = false;
    return;
  }

  // Otherwise, we have a bit more work that we need to perform:
  //
  // 1. We of course need to update iter to point at fresh_label.
  //
  // 2. We need to see if this value was the last element in its current
  // region. If so, then we need to remove the region from the transferred op
  // map.
  //
  // This is important to ensure that every region in the transferredOpMap is
  // also in elementToRegionMap.
  auto oldRegion = iter.first->second;
  iter.first->second = fresh_label;

  if (llvm::none_of(elementToRegionMap, [&](std::pair<Element, Region> value) {
        return value.second == oldRegion;
      })) {
    regionToTransferredOpMap.erase(oldRegion);
  }

  // Increment the fresh label so it remains fresh.
  fresh_label = Region(fresh_label + 1);
  canonical = false;
}

/// Assigns \p oldElt to the region associated with \p newElt.
void Partition::assignElement(Element oldElt, Element newElt) {
  SWIFT_DEFER { validateRegionToTransferredOpMapRegions(); };

  // First try to emplace oldElt with the newRegion.
  auto newRegion = elementToRegionMap.at(newElt);
  auto iter = elementToRegionMap.try_emplace(oldElt, newRegion);

  // If we did an insert, then we know that the value is new and we can just
  // set canonical to false and return.
  if (iter.second) {
    canonical = false;
    return;
  }

  // Otherwise, we did an assign. In such a case, we need to see if oldElt was
  // the last element in oldRegion. If so, we need to erase the oldRegion from
  // regionToTransferredOpMap.
  auto oldRegion = iter.first->second;
  iter.first->second = newRegion;

  if (llvm::none_of(elementToRegionMap, [&](std::pair<Element, Region> value) {
        return value.second == oldRegion;
      })) {
    regionToTransferredOpMap.erase(oldRegion);
  }

  canonical = false;
}

Partition Partition::join(const Partition &fst, const Partition &snd) {
  // First copy and canonicalize our inputs.
  Partition fstReduced = fst;
  Partition sndReduced = snd;

  fstReduced.canonicalize();
  sndReduced.canonicalize();

  // For each (sndEltNumber, sndRegionNumber) in snd_reduced...
  for (auto pair : sndReduced.elementToRegionMap) {
    auto sndEltNumber = pair.first;
    auto sndRegionNumber = pair.second;

    // Check if fstReduced has sndEltNumber within it...
    if (fstReduced.elementToRegionMap.count(sndEltNumber)) {
      // If we do, we just merge sndEltNumber into fstRegion.
      auto mergedRegion =
          fstReduced.merge(sndEltNumber, Element(sndRegionNumber));

      // Then if sndRegionNumber is transferred in sndReduced, make sure
      // mergedRegion is transferred in fstReduced.
      auto sndIter = sndReduced.regionToTransferredOpMap.find(sndRegionNumber);
      if (sndIter != sndReduced.regionToTransferredOpMap.end()) {
        auto fstIter = fstReduced.regionToTransferredOpMap.try_emplace(
            mergedRegion, sndIter->second);
        if (!fstIter.second) {
          fstIter.first->getSecond() =
              fstIter.first->getSecond()->merge(sndIter->second);
        }
      }
      continue;
    }

    // Then check if the representative element number for this element in snd
    // is in fst. In that case, we know that we visited it before we visited
    // this elt number (since we are processing in order) so what ever is
    // mapped to that number in snd must be the correct number for this
    // element as well since this number is guaranteed to be greater than our
    // representative and the number mapped to our representative in fst must
    // be <= our representative.
    //
    // In this case, we do not need to propagate transfer into fstRegion since
    // we would have handled that already when we visited our earlier
    // representative element number.
    {
      auto iter = fstReduced.elementToRegionMap.find(Element(sndRegionNumber));
      if (iter != fstReduced.elementToRegionMap.end()) {
        fstReduced.elementToRegionMap.insert({sndEltNumber, iter->second});
        // We want fresh_label to always be one element larger than our
        // maximum element.
        if (fstReduced.fresh_label <= Region(sndEltNumber))
          fstReduced.fresh_label = Region(sndEltNumber + 1);
        continue;
      }
    }

    // Otherwise, we have an element that is not in fst and its representative
    // is not in fst. This means that we must be our representative in snd
    // since we should have visited our representative earlier if we were not
    // due to our traversal being in order. Thus just add this to fst_reduced.
    assert(sndEltNumber == Element(sndRegionNumber));
    fstReduced.elementToRegionMap.insert({sndEltNumber, sndRegionNumber});
    auto sndIter = sndReduced.regionToTransferredOpMap.find(sndRegionNumber);
    if (sndIter != sndReduced.regionToTransferredOpMap.end()) {
      auto fstIter = fstReduced.regionToTransferredOpMap.try_emplace(
          sndRegionNumber, sndIter->second);
      if (!fstIter.second)
        fstIter.first->getSecond() =
            fstIter.first->second->merge(sndIter->second);
    }
    if (fstReduced.fresh_label <= sndRegionNumber)
      fstReduced.fresh_label = Region(sndEltNumber + 1);
  }

  assert(fstReduced.is_canonical_correct());

  // fst_reduced is now the join
  return fstReduced;
}

void Partition::print(llvm::raw_ostream &os) const {
  SmallFrozenMultiMap<Region, Element, 8> multimap;

  for (auto [eltNo, regionNo] : elementToRegionMap)
    multimap.insert(regionNo, eltNo);

  multimap.setFrozen();

  os << "[";
  for (auto [regionNo, elementNumbers] : multimap.getRange()) {
    auto iter = regionToTransferredOpMap.find(regionNo);
    bool isTransferred = iter != regionToTransferredOpMap.end();
    bool isClosureCaptured = false;
    if (isTransferred) {
      isClosureCaptured = llvm::any_of(iter->getSecond()->range(),
                                       [](const TransferringOperand *operand) {
                                         return operand->isClosureCaptured();
                                       });
    }

    if (isTransferred) {
      os << '{';
      if (isClosureCaptured)
        os << '*';
    } else {
      os << '(';
    }

    int j = 0;
    for (Element i : elementNumbers) {
      os << (j++ ? " " : "") << i;
    }
    if (isTransferred) {
      if (isClosureCaptured)
        os << '*';
      os << '}';
    } else {
      os << ')';
    }
  }
  os << "]\n";
}

void Partition::printVerbose(llvm::raw_ostream &os) const {
  SmallFrozenMultiMap<Region, Element, 8> multimap;

  for (auto [eltNo, regionNo] : elementToRegionMap)
    multimap.insert(regionNo, eltNo);

  multimap.setFrozen();

  for (auto [regionNo, elementNumbers] : multimap.getRange()) {
    auto iter = regionToTransferredOpMap.find(regionNo);
    bool isTransferred = iter != regionToTransferredOpMap.end();
    bool isClosureCaptured = false;
    if (isTransferred) {
      isClosureCaptured = llvm::any_of(iter->getSecond()->range(),
                                       [](const TransferringOperand *operand) {
                                         return operand->isClosureCaptured();
                                       });
    }

    os << "Region: " << regionNo << ". ";
    if (isTransferred) {
      os << '{';
      if (isClosureCaptured)
        os << '*';
    } else {
      os << '(';
    }

    int j = 0;
    for (Element i : elementNumbers) {
      os << (j++ ? " " : "") << i;
    }
    if (isTransferred) {
      if (isClosureCaptured)
        os << '*';
      os << '}';
    } else {
      os << ')';
    }
    os << "\n";
    os << "TransferInsts:\n";
    if (isTransferred) {
      for (auto op : iter->getSecond()->data()) {
        os << "    ";
        op->print(os);
      }
    } else {
      os << "None.\n";
    }
  }
}

bool Partition::is_canonical_correct() const {
#ifdef NDEBUG
  return true;
#else
  if (!canonical)
    return true; // vacuously correct

  auto fail = [&](Element i, int type) {
    llvm::errs() << "FAIL(i=" << i << "; type=" << type << "): ";
    print(llvm::errs());
    return false;
  };

  for (auto &[eltNo, regionNo] : elementToRegionMap) {
    // Labels should not exceed fresh_label.
    if (regionNo >= fresh_label)
      return fail(eltNo, 0);

    // The label of a region should be at most as large as each index in it.
    if ((unsigned)regionNo > eltNo)
      return fail(eltNo, 1);

    // Each region label should also be an element of the partition.
    if (!elementToRegionMap.count(Element(regionNo)))
      return fail(eltNo, 2);

    // Each element that is also a region label should be mapped to itself.
    if (elementToRegionMap.at(Element(regionNo)) != regionNo)
      return fail(eltNo, 3);
  }

  // Before we do anything, validate region to transferred op map.
  validateRegionToTransferredOpMapRegions();

  return true;
#endif
}

Region Partition::merge(Element fst, Element snd) {
  assert(elementToRegionMap.count(fst) && elementToRegionMap.count(snd));

  auto fstRegion = elementToRegionMap.at(fst);
  auto sndRegion = elementToRegionMap.at(snd);

  if (fstRegion == sndRegion)
    return fstRegion;

  // Maintain canonicality by renaming the greater-numbered region to the
  // smaller region.
  std::optional<Region> result;
  if (fstRegion < sndRegion) {
    result = fstRegion;

    // Rename snd to use first region.
    horizontalUpdate(elementToRegionMap, snd, fstRegion);
    auto iter = regionToTransferredOpMap.find(sndRegion);
    if (iter != regionToTransferredOpMap.end()) {
      auto operand = iter->second;
      regionToTransferredOpMap.erase(iter);
      regionToTransferredOpMap.try_emplace(fstRegion, operand);
    }
  } else {
    result = sndRegion;

    horizontalUpdate(elementToRegionMap, fst, sndRegion);
    auto iter = regionToTransferredOpMap.find(fstRegion);
    if (iter != regionToTransferredOpMap.end()) {
      auto operand = iter->second;
      regionToTransferredOpMap.erase(iter);
      regionToTransferredOpMap.try_emplace(sndRegion, operand);
    }
  }

  assert(is_canonical_correct());
  assert(elementToRegionMap.at(fst) == elementToRegionMap.at(snd));
  return *result;
}

void Partition::canonicalize() {
  if (canonical)
    return;
  canonical = true;

  validateRegionToTransferredOpMapRegions();
  std::map<Region, Region> oldRegionToRelabeledMap;

  // We rely on in-order traversal of labels to ensure that we always take the
  // lowest eltNumber.
  for (auto &[eltNo, regionNo] : elementToRegionMap) {
    if (!oldRegionToRelabeledMap.count(regionNo)) {
      // if this is the first time encountering this region label,
      // then this region label should be relabelled to this index,
      // so enter that into the map
      oldRegionToRelabeledMap.insert_or_assign(regionNo, Region(eltNo));
    }

    // Update this label with either its own index, or a prior index that
    // shared a region with it.
    regionNo = oldRegionToRelabeledMap.at(regionNo);

    // The maximum index iterated over will be used here to appropriately
    // set fresh_label.
    fresh_label = Region(eltNo + 1);
  }

  // Then relabel our regionToTransferredInst map if we need to by swapping
  // out the old map and updating.
  //
  // TODO: If we just used an array for this, we could just rewrite and
  // re-sort and not have to deal with potential allocations.
  decltype(regionToTransferredOpMap) oldMap =
      std::move(regionToTransferredOpMap);
  for (auto &[oldReg, op] : oldMap) {
    auto iter = oldRegionToRelabeledMap.find(oldReg);
    assert(iter != oldRegionToRelabeledMap.end());
    regionToTransferredOpMap[iter->second] = op;
  }

  assert(is_canonical_correct());
}

void Partition::horizontalUpdate(std::map<Element, Region> &map, Element key,
                                 Region val) {
  if (!map.count(key)) {
    map.insert({key, val});
    return;
  }

  Region oldVal = map.at(key);
  if (val == oldVal)
    return;

  for (auto [otherKey, otherVal] : map)
    if (otherVal == oldVal)
      map.insert_or_assign(otherKey, val);
}
