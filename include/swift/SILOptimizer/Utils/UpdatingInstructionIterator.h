//===--- UpdatingInstructionIterator.h --------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// Classes for tracking instruction iterators to be updated when instructions
/// are added or deleted.
///
/// UpdatingInstructionIteratorRegistry typically resides in the currently
/// active InstructionDeleter object.
///
/// UpdatingListIterator is the iterator adapter.
/// It is produced by: UpdatingInstructionIteratorRegistry::makeIterator()
///
/// Iterators are typically encapsulated in a range returned by
/// UpdatingInstructionIteratorRegistry::iteratorRange() for use in
/// range-based for loops:
///
///   for (SILInstruction *inst : registry.iteratorRange(bb)) ...
///
/// Or more commonly, directly from the InstructionDeleter:
///
///   for (SILInstruction *inst : deleter.updatingRange(bb)) ...
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_UPDATINGINSTRUCTIONITERATOR_H
#define SWIFT_SILOPTIMIZER_UTILS_UPDATINGINSTRUCTIONITERATOR_H

#include "swift/SIL/SILInstruction.h"

namespace swift {

/// Safely iterate over list elements while deleting elements and inserting new
/// elements. Advancing the iterator moves to the element following most
/// recently visited element. This holds even if the most recent element has
/// been deleted and new instructions have been added after its original
/// position.
///
/// Iterator copies are expensive because each copy registers
/// itself in a central data structure. Postorder increment is currently
/// unavailable to avoid excessive copies.
///
/// This adapter converts the base iterator's value_type into a pointer to the
/// same type. When the element has been deleted, dereferencing the iterator
/// returns a nullptr. This works with any bidirectional base iterator in which
/// the storage of each element is stable. Typically an llvm::ilist. The
/// implementation assumes that both forward and reverse iterators point to the
/// storage of the current element.
template <typename IteratorBase, bool IsReverse, typename Registry>
class UpdatingListIterator {
  using Self = UpdatingListIterator<IteratorBase, IsReverse, Registry>;

  Registry *registry;
  IteratorBase base;
  bool isDeleted = false;

public:
  using value_type = typename IteratorBase::pointer;
  using difference_type = ptrdiff_t;
  using pointer = value_type *;
  using iterator_category = std::bidirectional_iterator_tag;

  UpdatingListIterator(Registry &registry, IteratorBase base)
      : registry(&registry), base(base) {
    registry.registerIterator(this);
  }

  ~UpdatingListIterator() {
    if (registry)
      registry->destroyIterator(this);
  }

  // Copying forces a new entry in the registry.
  UpdatingListIterator(const Self &rhs)
      : registry(rhs.registry), base(rhs.base), isDeleted(rhs.isDeleted) {
    registry->registerIterator(this);
  }

  Self &operator=(const Self &rhs) {
    this->regsitry = rhs.registry;
    this->base = rhs.base;
    this->advanced = rhs.advanced;
    registry->registerIterator(this);
    return *this;
  }

  /// Explicit conversion between forward/reverse iterators.
  ///
  /// Note that this does not produce the same result as getting the iterator's
  /// instruction and producing a new iterator in the opposite
  /// direction. Instead, the new iterator points to the previous instruction in
  /// the original iteration order. This ensures that forward and reverse ranges
  /// enclose the same set of instructions.
  template <typename OtherIteratorBase>
  explicit UpdatingListIterator(
      const UpdatingListIterator<OtherIteratorBase, !IsReverse, Registry> &rhs)
      : UpdatingListIterator(IteratorBase(rhs.base)) {}

  /// This returns nullptr for deleted instructions.
  value_type operator*() const { return isDeleted ? nullptr : &*base; }

  SILInstruction *operator->() const { return operator*(); }

  Self &operator++() {
    advance();
    return *this;
  }

  bool operator==(const Self &rhs) const {
    return this->registry == rhs.registry && this->base == rhs.base
           && this->isDeleted == rhs.isDeleted;
  }
  bool operator!=(const Self &rhs) const { return !(*this == rhs); }

  void advance() {
    if (isDeleted)
      isDeleted = false;
    else
      ++base;
  }

  void notifyDelete(IteratorBase positionToDelete) {
    if (base == positionToDelete) {
      isDeleted = true;
      ++base;
    }
  }

  void notifyNew(IteratorBase newPosition) {
    if (isDeleted && std::prev(base) == newPosition) {
      // The deleted forward iterator was already advanced. Move it back to the
      // position of the new element.
      --base;
    }
  }
};

class UpdatingInstructionIteratorRegistry;

using UpdatingInstructionIterator =
    UpdatingListIterator<SILBasicBlock::iterator, false,
                         UpdatingInstructionIteratorRegistry>;
using UpdatingReverseInstructionIterator =
    UpdatingListIterator<SILBasicBlock::reverse_iterator, true,
                         UpdatingInstructionIteratorRegistry>;

/// Track instruction iterators that need updating when intructions are added or
/// deleted. Iterators can be tracked across multiple levels of the call
/// stack. This registry object must outlive any iterators that it vends.
///
/// While the registry is active, all instruction modificaiton must go through
/// its callbacks.
class UpdatingInstructionIteratorRegistry {

  /// Track iterators that need updating. Seldom expect to have more than 4
  /// (making a single range creates 4 but immediately discards 2). It is
  /// possible for iterators to be copied and destroyed on each iteration of a
  /// loop (although we should try hard to avoid that), so this does need to
  /// immediately reuse old slots.
  SmallVector<UpdatingInstructionIterator *, 4> forwardIterators;
  SmallVector<UpdatingReverseInstructionIterator *, 4> reverseIterators;

  /// Callbacks used when adding/deleting instructions.
  InstModCallbacks callbacks;

public:
  UpdatingInstructionIteratorRegistry(
      InstModCallbacks chainedCallbacks = InstModCallbacks()) {
    rechainCallbacks(chainedCallbacks);
  }

  // The callbacks capture 'this'. So copying is invalid.
  UpdatingInstructionIteratorRegistry(
      const UpdatingInstructionIteratorRegistry &) = delete;

  UpdatingInstructionIteratorRegistry &
  operator=(const UpdatingInstructionIteratorRegistry &) = delete;

  InstModCallbacks &getCallbacks() { return callbacks; }

  void rechainCallbacks(InstModCallbacks chainedCallbacks) {
    // Copy the two std::functions that we need. The rest of the callbacks are
    // copied implicitly by assignment.
    auto chainedDelete = chainedCallbacks.deleteInstFunc;
    auto chainedNew = chainedCallbacks.createdNewInstFunc;
    callbacks = chainedCallbacks
                    .onDelete([this, chainedDelete](SILInstruction *toDelete) {
                      notifyDelete(toDelete);
                      if (chainedDelete) {
                        chainedDelete(toDelete);
                        return;
                      }
                      toDelete->eraseFromParent();
                    })
                    .onCreateNewInst(
                        [this, chainedNew](SILInstruction *newlyCreatedInst) {
                          notifyNew(newlyCreatedInst);
                          if (chainedNew) {
                            chainedNew(newlyCreatedInst);
                          }
                        });
  }

  void registerIterator(UpdatingInstructionIterator *i) {
    forwardIterators.push_back(i);
  }

  void registerIterator(UpdatingReverseInstructionIterator *i) {
    reverseIterators.push_back(i);
  }

  void destroyIterator(UpdatingInstructionIterator *i) {
    auto pos = std::find(forwardIterators.begin(), forwardIterators.end(), i);
    assert(pos != forwardIterators.end() && "unregistered iterator");
    forwardIterators.erase(pos);
  }
  void destroyIterator(UpdatingReverseInstructionIterator *i) {
    auto pos = std::find(reverseIterators.begin(), reverseIterators.end(), i);
    assert(pos != reverseIterators.end() && "unregistered iterator");
    reverseIterators.erase(pos);
  }

  UpdatingInstructionIterator makeIterator(SILBasicBlock::iterator i) {
    return {*this, i};
  }

  UpdatingInstructionIterator makeIterator(SILInstruction *inst) {
    return makeIterator(inst->getIterator());
  }

  UpdatingReverseInstructionIterator
  makeReverseIterator(SILBasicBlock::reverse_iterator i) {
    return {*this, i};
  }

  UpdatingReverseInstructionIterator
  makeReverseIterator(SILInstruction *inst) {
    return makeReverseIterator(inst->getReverseIterator());
  }

  llvm::iterator_range<UpdatingInstructionIterator>
  makeIteratorRange(SILBasicBlock *bb) {
    return {makeIterator(bb->begin()), makeIterator(bb->end())};
  }

  llvm::iterator_range<UpdatingReverseInstructionIterator>
  makeReverseIteratorRange(SILBasicBlock *bb) {
    return {makeReverseIterator(bb->rbegin()), makeReverseIterator(bb->rend())};
  }

protected:
  void notifyDelete(SILInstruction *toDelete) {
    for (auto *iterator : forwardIterators) {
      iterator->notifyDelete(toDelete->getIterator());
    }
    for (auto *iterator : reverseIterators) {
      iterator->notifyDelete(toDelete->getReverseIterator());
    }
  }

  void notifyNew(SILInstruction *newlyCreatedInst) {
    for (auto *iterator : forwardIterators) {
      iterator->notifyNew(newlyCreatedInst->getIterator());
    }
    for (auto *iterator : reverseIterators) {
      iterator->notifyNew(newlyCreatedInst->getReverseIterator());
    }
  }
};

} // end namespace swift

#endif
