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

// We only use this so we can implement print on our type erased errors.
#include "swift/SILOptimizer/Analysis/RegionAnalysis.h"
#include "swift/SILOptimizer/Utils/VariableNameUtils.h"

using namespace swift;
using namespace swift::PartitionPrimitives;

//===----------------------------------------------------------------------===//
//                           MARK: PartitionOpError
//===----------------------------------------------------------------------===//

void PartitionOpError::UnknownCodePatternError::print(
    llvm::raw_ostream &os, RegionAnalysisValueMap &valueMap) const {
  os << "    Emitting Error. Kind: Unknown Code Pattern Error\n"
     << "        Inst: " << *op->getSourceInst();
}

void PartitionOpError::LocalUseAfterSendError::print(
    llvm::raw_ostream &os, RegionAnalysisValueMap &valueMap) const {
  os << "    Emitting Error. Kind: Use After Send\n"
     << "        Sending Inst: " << *sendingOp->getUser()
     << "        Sending Op Value: " << sendingOp->get()
     << "        Require Inst: " << *op->getSourceInst() << "        ID:  %%"
     << sentElement << "\n"
     << "        Rep: " << valueMap.getRepresentativeValue(sentElement)
     << "        Sending Op Num: " << sendingOp->getOperandNumber() << '\n';
}

void PartitionOpError::SentNeverSendableError::print(
    llvm::raw_ostream &os, RegionAnalysisValueMap &info) const {
  os << "    Emitting Error. Kind: Sent Non Sendable\n"
     << "        ID:  %%" << sentElement << "\n"
     << "        Rep: " << *info.getRepresentative(sentElement)
     << "        Dynamic Isolation Region: ";
  isolationRegionInfo.printForOneLineLogging(info.getFunction(), os);
  os << '\n';
  if (auto isolatedValue = isolationRegionInfo->maybeGetIsolatedValue()) {
    os << "        Isolated Value: " << isolatedValue;
    auto name = VariableNameInferrer::inferName(isolatedValue);
    os << "        Isolated Value Name: "
       << (name.has_value() ? name->get() : "none") << '\n';
  } else {
    os << "        Isolated Value: none\n";
  };
}

void PartitionOpError::AssignNeverSendableIntoSendingResultError::print(
    llvm::raw_ostream &os, RegionAnalysisValueMap &valueMap) const {
  os << "    Emitting Error. Kind: Assign Isolated Into Sending Result!\n"
     << "        Assign Inst: " << *op->getSourceInst()
     << "        Dest Value: " << *destValue
     << "        Dest Element: " << destElement << '\n'
     << "        Src Value: " << srcValue
     << "        Src Element: " << srcElement << '\n'
     << "        Src Rep: " << valueMap.getRepresentativeValue(srcElement)
     << "        Src Isolation: " << srcIsolationRegionInfo << '\n';
}

void PartitionOpError::InOutSendingNotInitializedAtExitError::print(
    llvm::raw_ostream &os, RegionAnalysisValueMap &valueMap) const {
  os << "    Emitting Error. Kind: InOut Not Reinitialized At End Of "
        "Function\n"
     << "        Sending Inst: " << *sendingOp->getUser()
     << "        Sending Op Value: " << sendingOp->get()
     << "        Require Inst: " << *op->getSourceInst() << "        ID:  %%"
     << sentElement << "\n"
     << "        Rep: " << valueMap.getRepresentativeValue(sentElement)
     << "        Sending Op Num: " << sendingOp->getOperandNumber() << '\n';
}

void PartitionOpError::InOutSendingNotDisconnectedAtExitError::print(
    llvm::raw_ostream &os, RegionAnalysisValueMap &valueMap) const {
  os << "    Emitting Error. Kind: InOut Sending ActorIsolated "
        "at end of "
        "Function Error!\n"
     << "        ID:  %%" << inoutSendingElement << "\n"
     << "        Rep: " << valueMap.getRepresentativeValue(inoutSendingElement)
     << "        Dynamic Isolation Region: ";
  isolationInfo.printForOneLineLogging(valueMap.getFunction(), os);
  os << '\n';
}

void PartitionOpError::NonSendableIsolationCrossingResultError::print(
    llvm::raw_ostream &os, RegionAnalysisValueMap &valueMap) const {
  os << "    Emitting Error. Kind: NonSendableIsolationCrossingResultError\n"
        "        Inst: "
     << *op->getSourceInst() << "        Result ID: %%" << returnValueElement
     << '\n';
}

//===----------------------------------------------------------------------===//
//                             MARK: PartitionOp
//===----------------------------------------------------------------------===//

void PartitionOp::print(llvm::raw_ostream &os, bool extraSpace) const {
  constexpr static char extraSpaceLiteral[10] = "     ";
  switch (opKind) {
  case PartitionOpKind::Assign: {
    os << "assign ";
    if (extraSpace)
      os << extraSpaceLiteral;
    os << "%%" << getOpArg1() << " = %%" << getOpArg2();
    break;
  }
  case PartitionOpKind::AssignFresh:
    os << "assign_fresh %%" << getOpArg1();
    break;
  case PartitionOpKind::Send: {
    os << "send ";
    if (extraSpace)
      os << extraSpaceLiteral;
    os << "%%" << getOpArg1();
    break;
  }
  case PartitionOpKind::UndoSend: {
    os << "undo_send ";
    if (extraSpace)
      os << extraSpaceLiteral;
    os << "%%" << getOpArg1();
    break;
  }
  case PartitionOpKind::Merge: {
    os << "merge ";
    if (extraSpace)
      os << extraSpaceLiteral;
    os << "%%" << getOpArg1() << " with %%" << getOpArg2();
    break;
  }
  case PartitionOpKind::Require: {
    os << "require ";
    if (getOptions().containsOnly(
            PartitionOp::Flag::RequireOfMutableBaseOfSendableValue))
      os << "[mutable_base_of_sendable_val] ";
    if (extraSpace)
      os << extraSpaceLiteral;
    os << "%%" << getOpArg1();
    break;
  }
  case PartitionOpKind::UnknownPatternError:
    os << "unknown pattern error ";
    os << "%%" << getOpArg1();
    break;
  case PartitionOpKind::InOutSendingAtFunctionExit:
    os << "inout_sending_at_function_exit ";
    if (extraSpace)
      os << extraSpaceLiteral;
    os << "%%" << getOpArg1();
    break;
  case PartitionOpKind::NonSendableIsolationCrossingResult:
    os << "nonsendable_isolationcrossing_result ";
    if (extraSpace)
      os << extraSpaceLiteral;
    os << "%%" << getOpArg1();
    break;
  case PartitionOpKind::AssignFreshAssign:
    os << "assign_fresh_assign ";
    if (extraSpace)
      os << extraSpaceLiteral;
    os << "%%" << getOpArg1() << " = %%" << getOpArg2();
    break;
  }
  os << ": " << *getSourceInst();
}

//===----------------------------------------------------------------------===//
//                              MARK: Partition
//===----------------------------------------------------------------------===//

Partition Partition::singleRegion(SILLocation loc, ArrayRef<Element> indices,
                                  IsolationHistory inputHistory) {
  Partition p(inputHistory);
  if (!indices.empty()) {
    // Lowest element is our region representative and the value that our
    // region takes.
    Element repElement = *std::min_element(indices.begin(), indices.end());
    Region repElementRegion = Region(repElement);
    p.freshLabel = Region(repElementRegion + 1);

    // Place all of the operations until end of scope into one history
    // sequence.
    p.pushHistorySequenceBoundary(loc);

    // First create a region for repElement. We are going to merge all other
    // regions into its region.
    p.pushNewElementRegion(repElement);
    llvm::SmallVector<Element, 32> nonRepElts;
    for (Element index : indices) {
      p.elementToRegionMap.insert_or_assign(index, repElementRegion);
      if (index != repElement) {
        p.pushNewElementRegion(index);
        nonRepElts.push_back(index);
      }
      p.pushMergeElementRegions(repElement, nonRepElts);
    }
  }

  assert(p.is_canonical_correct());
  return p;
}

Partition Partition::separateRegions(SILLocation loc, ArrayRef<Element> indices,
                                     IsolationHistory inputHistory) {
  Partition p(inputHistory);
  if (indices.empty())
    return p;

  // Place all operations in one history sequence.
  p.pushHistorySequenceBoundary(loc);

  auto maxIndex = Element(0);
  for (Element index : indices) {
    p.elementToRegionMap.insert_or_assign(index, Region(index));
    p.pushNewElementRegion(index);
    maxIndex = Element(std::max(maxIndex, index));
  }
  p.freshLabel = Region(maxIndex + 1);
  assert(p.is_canonical_correct());
  return p;
}

void Partition::markSent(Element val, SendingOperandSet *sendingOperandSet) {
  // First see if our val is tracked. If it is not tracked, insert it and mark
  // its new region as sent.
  if (!isTrackingElement(val)) {
    elementToRegionMap.insert_or_assign(val, freshLabel);
    pushNewElementRegion(val);
    regionToSendingOpMap.insert({freshLabel, sendingOperandSet});
    freshLabel = Region(freshLabel + 1);
    canonical = false;
    return;
  }

  // Otherwise, we already have this value in the map. Try to insert it.
  auto iter1 = elementToRegionMap.find(val);
  assert(iter1 != elementToRegionMap.end());
  auto iter2 = regionToSendingOpMap.insert({iter1->second, sendingOperandSet});

  // If we did insert, just return. We were not tracking any state.
  if (iter2.second)
    return;

  // Otherwise, we need to merge the sets.
  iter2.first->second = iter2.first->second->merge(sendingOperandSet);
}

bool Partition::undoSend(Element val) {
  // First see if our val is tracked. If it is not tracked, insert it.
  if (!isTrackingElement(val)) {
    elementToRegionMap.insert_or_assign(val, freshLabel);
    pushNewElementRegion(val);
    freshLabel = Region(freshLabel + 1);
    canonical = false;
    return true;
  }

  // Otherwise, we already have this value in the map. Remove it from the
  // "sending operand" map.
  auto iter1 = elementToRegionMap.find(val);
  assert(iter1 != elementToRegionMap.end());
  return regionToSendingOpMap.erase(iter1->second);
}

void Partition::trackNewElement(Element newElt, bool updateHistory) {
  SWIFT_DEFER { validateRegionToSendingOpMapRegions(); };

  // First try to emplace newElt with fresh_label.
  auto iter = elementToRegionMap.try_emplace(newElt, freshLabel);

  // If we did insert, then we know that the value is completely new. We can
  // just update the fresh_label, set canonical to false, and return.
  if (iter.second) {
    // Since the value is completely new, add a completely new history node to
    // the history.
    if (updateHistory)
      pushNewElementRegion(newElt);

    // Increment the fresh label so it remains fresh.
    freshLabel = Region(freshLabel + 1);
    canonical = false;
    return;
  }

  // Otherwise, we have a bit more work that we need to perform:
  //
  // 1. We of course need to update iter to point at fresh_label.
  //
  // 2. We need to see if this value was the last element in its current
  // region. If so, then we need to remove the region from the sending op
  // map.
  //
  // This is important to ensure that every region in the sendingOpMap is
  // also in elementToRegionMap.
  auto oldRegion = iter.first->second;
  iter.first->second = freshLabel;

  auto getValueFromOtherRegion = [&]() -> std::optional<Element> {
    for (auto pair : elementToRegionMap) {
      if (pair.second == oldRegion)
        return pair.first;
    }
    return {};
  };

  if (auto matchingElt = getValueFromOtherRegion()) {
    if (updateHistory)
      pushRemoveElementFromRegion(*matchingElt, newElt);
  } else {
    regionToSendingOpMap.erase(oldRegion);
    if (updateHistory)
      pushRemoveLastElementFromRegion(newElt);
  }

  if (updateHistory)
    pushNewElementRegion(newElt);

  // Increment the fresh label so it remains fresh.
  freshLabel = Region(freshLabel + 1);
  canonical = false;
}

/// Assigns \p oldElt to the region associated with \p newElt.
void Partition::assignElement(Element oldElt, Element newElt,
                              bool updateHistory) {
  // If the old/new elt at the same, just return.
  if (oldElt == newElt)
    return;

  SWIFT_DEFER { validateRegionToSendingOpMapRegions(); };

  // First try to emplace oldElt with the newRegion.
  auto newRegion = elementToRegionMap.at(newElt);
  auto iter = elementToRegionMap.try_emplace(oldElt, newRegion);

  // If we did an insert, then we know that oldElt was new to this
  // partition. This means that we update our history for a completely new
  // value in newElt's region. We also set canonical to false to ensure when
  // ever we do a merge/etc, we renumber indices as appropriate.
  if (iter.second) {
    if (updateHistory) {
      pushNewElementRegion(oldElt);
      pushMergeElementRegions(newElt, oldElt);
    }
    canonical = false;
    return;
  }

  // Otherwise, we did an assign.
  auto oldRegion = iter.first->second;

  // First check if oldRegion and newRegion are the same. In such a case, just
  // return.
  if (oldRegion == newRegion)
    return;

  // Otherwise, we need to actually assign. In such a case, we need to see if
  // oldElt was the last element in oldRegion. If so, we need to erase the
  // oldRegion from regionToSendingOpMap.
  iter.first->second = newRegion;

  auto getValueFromOtherRegion = [&]() -> std::optional<Element> {
    for (auto pair : elementToRegionMap) {
      if (pair.second == oldRegion)
        return pair.first;
    }
    return {};
  };

  if (auto otherElt = getValueFromOtherRegion()) {
    if (updateHistory)
      pushRemoveElementFromRegion(*otherElt, oldElt);
  } else {
    regionToSendingOpMap.erase(oldRegion);
    if (updateHistory)
      pushRemoveLastElementFromRegion(oldElt);
  }

  if (updateHistory) {
    pushNewElementRegion(oldElt);
    pushMergeElementRegions(newElt, oldElt);
  }

  canonical = false;
}

Partition Partition::join(const Partition &fst, Partition &mutableSnd) {
  // READ THIS! Remember, we cannot touch mutableSnd after this point. We just
  // use it to canonicalize to avoid having to copy snd. After this point,
  // please use the const reference snd to keep each other honest.
  mutableSnd.canonicalize();
  const auto &snd = mutableSnd;

  // First copy fst into result and canonicalize the result.and canonicalize
  // fst.
  Partition result = fst;
  result.canonicalize();

  // Push a history join so when processing, we know the next element to
  // process.
  result.pushCFGHistoryJoin(snd.history);

  // For each (sndEltNumber, sndRegionNumber) in snd_reduced...
  for (auto pair : snd.elementToRegionMap) {
    auto sndEltNumber = pair.first;
    auto sndRegionNumber = pair.second;

    // Check if result has sndEltNumber already within it...
    {
      auto resultIter = result.elementToRegionMap.find(sndEltNumber);
      if (result.elementToRegionMap.end() != resultIter) {
        auto resultRegion = resultIter->second;

        // If we do and Element(sndRegionNumber) isn't the same element as
        // sndEltNumber, then we know that sndEltNumber isn't the
        // representative element of its region in sndReduced. We need to
        // ensure that in result, that representative and our current
        // value are in the same region. If they are the same value, we can
        // just reuse sndEltNumber's region in result for the sending
        // check.
        if (sndEltNumber != Element(sndRegionNumber)) {
          // NOTE: History is updated by Partition::merge(...).
          resultRegion = result.merge(sndEltNumber, Element(sndRegionNumber));
        }

        // Then if sndRegionNumber is sent in sndReduced, make sure mergedRegion
        // is sent in result.
        auto sndIter = snd.regionToSendingOpMap.find(sndRegionNumber);
        if (sndIter != snd.regionToSendingOpMap.end()) {
          auto resultIter = result.regionToSendingOpMap.insert(
              {resultRegion, sndIter->second});
          if (!resultIter.second) {
            resultIter.first->second =
                resultIter.first->second->merge(sndIter->second);
          }
        }
        continue;
      }
    }

    // At this point, we know that sndEltNumber is not in result.
    //
    // Check if the representative element number
    // (i.e. Element(sndRegionNumber)) for this element in snd is in result. In
    // that case, we know that we visited the representative number before we
    // visited this elt number (since we are processing in order) so what ever
    // is mapped to that number in snd must be the correct region for this
    // element as well since this number is guaranteed to be greater than our
    // representative and the number mapped to our representative in result must
    // be
    // <= our representative.
    //
    // In this case, we do not need to propagate 'send' into resultRegion
    // since we would have handled that already when we visited our earlier
    // representative element number.
    {
      auto iter = result.elementToRegionMap.find(Element(sndRegionNumber));
      if (iter != result.elementToRegionMap.end()) {
        result.elementToRegionMap.insert({sndEltNumber, iter->second});
        result.pushMergeElementRegions(sndEltNumber, Element(sndRegionNumber));
        // We want fresh_label to always be one element larger than our
        // maximum element.
        if (result.freshLabel <= Region(sndEltNumber))
          result.freshLabel = Region(sndEltNumber + 1);
        continue;
      }
    }

    // Otherwise, we have an element that is not in result and its
    // representative is not in result. This means that we must be our
    // representative in snd since we should have visited our representative
    // earlier if we were not due to our traversal being in order. Thus just add
    // this to result.
    assert(sndEltNumber == Element(sndRegionNumber));
    result.elementToRegionMap.insert({sndEltNumber, sndRegionNumber});
    result.pushNewElementRegion(sndEltNumber);
    auto sndIter = snd.regionToSendingOpMap.find(sndRegionNumber);
    if (sndIter != snd.regionToSendingOpMap.end()) {
      auto fstIter = result.regionToSendingOpMap.insert(
          {sndRegionNumber, sndIter->second});
      if (!fstIter.second)
        fstIter.first->second = fstIter.first->second->merge(sndIter->second);
    }
    if (result.freshLabel <= sndRegionNumber)
      result.freshLabel = Region(sndEltNumber + 1);
  }

  // We should have preserved canonicality during the computation above. It
  // would be wasteful to need to canonicalize twice.
  assert(result.is_canonical_correct());

  // result is now the join.
  return result;
}

bool Partition::popHistory(
    SmallVectorImpl<IsolationHistory> &foundJoinedHistories) {
  // We only allow for history rewinding if we are not tracking any
  // sending operands. This is because the history rewinding does not
  // care about sending. One can either construct a new Partition from
  // the current Partition using Partition::removeSendingOperandSet or clear
  // the sending information using Partition::clearSendingOperandState().
  assert(regionToSendingOpMap.empty() &&
         "Can only rewind history if not tracking any sending operands");

  if (!history.getHead())
    return false;

  // Just put in a continue here to ensure that clang-format doesn't do weird
  // things with the semicolon.
  while (popHistoryOnce(foundJoinedHistories))
    continue;

  // Return if our history head is non-null so our user knows if there are more
  // things to pop.
  return history.getHead();
}

void Partition::print(llvm::raw_ostream &os) const {
  SmallFrozenMultiMap<Region, Element, 8> multimap;

  for (auto [eltNo, regionNo] : elementToRegionMap)
    multimap.insert(regionNo, eltNo);

  multimap.setFrozen();

  os << "[";
  for (auto [regionNo, elementNumbers] : multimap.getRange()) {
    auto iter = regionToSendingOpMap.find(regionNo);
    bool wasSent = iter != regionToSendingOpMap.end();
    if (wasSent) {
      os << '{';
    } else {
      os << '(';
    }

    int j = 0;
    for (Element i : elementNumbers) {
      os << (j++ ? " " : "") << i;
    }
    if (wasSent) {
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
    auto iter = regionToSendingOpMap.find(regionNo);
    bool wasSent = iter != regionToSendingOpMap.end();

    os << "Region: " << regionNo << ". ";
    if (wasSent) {
      os << '{';
    } else {
      os << '(';
    }

    int j = 0;
    for (Element i : elementNumbers) {
      os << (j++ ? " " : "") << i;
    }
    if (wasSent) {
      os << '}';
    } else {
      os << ')';
    }
    os << "\n";
    os << "SentInsts:\n";
    if (wasSent) {
      for (auto op : iter->second->data()) {
        os << "    ";
        op->print(os);
      }
    } else {
      os << "None.\n";
    }
  }
}

void Partition::printHistory(llvm::raw_ostream &os) const {
  llvm::dbgs() << "History Dump!\n";
  const auto *head = history.head;

  if (!head)
    return;

  do {
    switch (head->getKind()) {
    case IsolationHistory::Node::AddNewRegionForElement:
      os << "AddNewRegionForElement: " << head->getFirstArgAsElement();
      break;
    case IsolationHistory::Node::RemoveLastElementFromRegion:
      os << "RemoveLastElementFromRegion: " << head->getFirstArgAsElement();
      break;
    case IsolationHistory::Node::RemoveElementFromRegion: {
      os << "RemoveElementFromRegion: " << head->getFirstArgAsElement();
      auto extraArgs = head->getAdditionalElementArgs();
      if (extraArgs.empty())
        break;
      llvm::interleave(extraArgs, os, ", ");
      break;
    }
    case IsolationHistory::Node::MergeElementRegions: {
      os << "MergeElementRegions: " << head->getFirstArgAsElement();
      auto extraArgs = head->getAdditionalElementArgs();
      if (extraArgs.empty())
        break;
      os << ", ";
      llvm::interleave(extraArgs, os, ", ");
      break;
    }
    case IsolationHistory::Node::CFGHistoryJoin:
      os << "CFGHistoryJoin";
      break;
    case IsolationHistory::Node::SequenceBoundary:
      os << "SequenceBoundary";
      break;
    }
    os << "\n";

  } while ((head = head->getParent()));
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
    if (regionNo >= freshLabel)
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

  // Before we do anything, validate region to region to sending op map.
  validateRegionToSendingOpMapRegions();

  return true;
#endif
}

Region Partition::merge(Element fst, Element snd, bool updateHistory) {
  assert(elementToRegionMap.count(fst) && elementToRegionMap.count(snd));

  // Remember: fstRegion and sndRegion are actually elements in
  // elementToRegionMap... they are just the representative of the region
  // (which is the smallest element number).
  auto fstRegion = elementToRegionMap.at(fst);
  auto sndRegion = elementToRegionMap.at(snd);

  // Our value reps are the same... we can return either. Just return fstRegion.
  if (fstRegion == sndRegion)
    return fstRegion;

  // To maintain canonicality, we require that fstRegion is always less than
  // sndRegion. If we do not have that, swap first and second state.
  if (fstRegion > sndRegion) {
    std::swap(fst, snd);
    std::swap(fstRegion, sndRegion);
  }

  Region result = fstRegion;

  // Rename snd and snd's entire region to fst's region.
  SmallVector<Element, 32> mergedElements;
  horizontalUpdate(snd, fstRegion, mergedElements);
  auto iter = regionToSendingOpMap.find(sndRegion);
  if (iter != regionToSendingOpMap.end()) {
    auto operand = iter->second;
    regionToSendingOpMap.erase(iter);
    regionToSendingOpMap.insert({fstRegion, operand});
  }

  assert(is_canonical_correct());
  assert(elementToRegionMap.at(fst) == elementToRegionMap.at(snd));

  // Now that we are correct/canonicalized, add the merge to our history.
  if (updateHistory)
    pushMergeElementRegions(fst, mergedElements);
  return result;
}

void Partition::canonicalize() {
  if (canonical)
    return;
  canonical = true;

  validateRegionToSendingOpMapRegions();
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
    freshLabel = Region(eltNo + 1);
  }

  // Then relabel our regionToSendingOpMap map if we need to by swapping out the
  // old map and updating.
  //
  // TODO: If we just used an array for this, we could just rewrite and
  // re-sort and not have to deal with potential allocations.
  decltype(regionToSendingOpMap) oldMap = std::move(regionToSendingOpMap);
  for (auto &[oldReg, op] : oldMap) {
    auto iter = oldRegionToRelabeledMap.find(oldReg);
    assert(iter != oldRegionToRelabeledMap.end());
    regionToSendingOpMap[iter->second] = op;
  }

  assert(is_canonical_correct());
}

void Partition::horizontalUpdate(
    Element targetElement, Region newRegion,
    llvm::SmallVectorImpl<Element> &mergedElements) {
  // It is on our caller to make sure a value is in elementToRegionMap.
  Region oldRegion = elementToRegionMap.at(targetElement);

  // If our old region is the same as our new region, we do not have anything
  // to do.
  if (oldRegion == newRegion)
    return;

  for (auto [element, region] : elementToRegionMap) {
    if (region == oldRegion) {
      elementToRegionMap.insert_or_assign(element, newRegion);
      mergedElements.push_back(element);
    }
  }
}

bool Partition::popHistoryOnce(
    SmallVectorImpl<IsolationHistory> &foundJoinedHistoryNodes) {
  const auto *head = history.pop();
  if (!head)
    return false;

  // When popping, we /always/ want to canonicalize.
  canonicalize();

  switch (head->getKind()) {
  case IsolationHistory::Node::SequenceBoundary:
    return false;

  case IsolationHistory::Node::AddNewRegionForElement: {
    // We added an element to its own region... so we should remove it and it
    // should be the last element in the region.
    auto iter = elementToRegionMap.find(head->getFirstArgAsElement());
    assert(iter != elementToRegionMap.end());
    Region oldRegion = iter->second;
    regionToSendingOpMap.erase(oldRegion);
    elementToRegionMap.erase(iter);
    assert(llvm::none_of(elementToRegionMap,
                         [&](std::pair<Element, Region> pair) {
                           return pair.second == oldRegion;
                         }) &&
           "Should have been last element?!");
    return true;
  }
  case IsolationHistory::Node::RemoveLastElementFromRegion:
    // We removed an element from a region and it was the last element. Just
    // add new.
    trackNewElement(head->getFirstArgAsElement(), false /*update history*/);
    return true;
  case IsolationHistory::Node::RemoveElementFromRegion:
    // We removed an element from a specific region. So, we need to add it
    // back.
    assignElement(head->getFirstArgAsElement(),
                  head->getAdditionalElementArgs()[1],
                  false /*update history*/);
    return true;

  case IsolationHistory::Node::MergeElementRegions: {
    // We merged two regions together. We need to remove all elements from the
    // previous region into their own new region.
    auto elementsToExtract = head->getAdditionalElementArgs();
    assert(elementsToExtract.size());

    removeElement(elementsToExtract[0]);
    trackNewElement(elementsToExtract[0], false /*update history*/);

    for (auto e : elementsToExtract.drop_front()) {
      assert(head->getFirstArgAsElement() != e &&
             "We assume that we are never removing all values when undoing "
             "merging");
      removeElement(e);
      trackNewElement(e, false /*update history*/);
      merge(e, elementsToExtract[0], false /*update history*/);
    }

    return true;
  }
  case IsolationHistory::Node::CFGHistoryJoin:
    // When we have a CFG History Merge, we cannot simply pop. Instead, we need
    // to signal to the user that they need to visit each history node in turn
    // by returning it in the out parameter.
    auto newHistory = IsolationHistory(history.factory);
    newHistory.head = head->getFirstArgAsNode();
    foundJoinedHistoryNodes.push_back(newHistory);
    return true;
  }
}

//===----------------------------------------------------------------------===//
//                           MARK: IsolationHistory
//===----------------------------------------------------------------------===//

// Push onto the history list that \p value should be added into its own
// independent region.
IsolationHistory::Node *
IsolationHistory::pushNewElementRegion(Element element) {
  unsigned size = Node::totalSizeToAlloc<Element>(0);
  void *mem = factory->allocator.Allocate(size, alignof(Node));
  head = new (mem) Node(Node::AddNewRegionForElement, head, element);
  return getHead();
}

IsolationHistory::Node *
IsolationHistory::pushHistorySequenceBoundary(SILLocation loc) {
  unsigned size = Node::totalSizeToAlloc<Element>(0);
  void *mem = factory->allocator.Allocate(size, alignof(Node));
  head = new (mem) Node(Node::SequenceBoundary, head, loc);
  return getHead();
}

// Push onto the history that \p value should be removed from any region that it
// is apart of and placed within its own separate region.
void IsolationHistory::pushRemoveLastElementFromRegion(Element element) {
  unsigned size = Node::totalSizeToAlloc<Element>(0);
  void *mem = factory->allocator.Allocate(size, alignof(Node));
  head = new (mem) Node(Node::RemoveLastElementFromRegion, head, element);
}

void IsolationHistory::pushRemoveElementFromRegion(
    Element otherElementInOldRegion, Element element) {
  unsigned size = Node::totalSizeToAlloc<Element>(1);
  void *mem = factory->allocator.Allocate(size, alignof(Node));
  head = new (mem) Node(Node::RemoveElementFromRegion, head, element,
                        {otherElementInOldRegion});
}

void IsolationHistory::pushMergeElementRegions(Element elementToMergeInto,
                                               ArrayRef<Element> eltsToMerge) {
  assert(llvm::none_of(eltsToMerge,
                       [&](Element elt) { return elt == elementToMergeInto; }));
  unsigned size = Node::totalSizeToAlloc<Element>(eltsToMerge.size());
  void *mem = factory->allocator.Allocate(size, alignof(Node));
  head = new (mem)
      Node(Node::MergeElementRegions, head, elementToMergeInto, eltsToMerge);
}

// Push that \p other should be merged into this region.
void IsolationHistory::pushCFGHistoryJoin(Node *otherNode) {
  // If otherNode is nullptr or represents our same history, do not merge.
  if (!otherNode || otherNode == head)
    return;

  // If we do not have any history, just take on the history of otherNode. We
  // are going to merge our contents.
  if (!head) {
    head = otherNode;
    return;
  }

  // Otherwise, create a node that joins our true head and other node as a side
  // path we can follow.
  unsigned size = Node::totalSizeToAlloc<Element>(0);
  void *mem = factory->allocator.Allocate(size, alignof(Node));
  head = new (mem) Node(Node(Node::CFGHistoryJoin, head, otherNode));
}

IsolationHistory::Node *IsolationHistory::pop() {
  if (!head)
    return nullptr;

  auto *result = head;
  head = head->parent;
  return result;
}
