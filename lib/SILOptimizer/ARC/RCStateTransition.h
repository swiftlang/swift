//===--- RCStateTransition.h ------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_SILOPTIMIZER_PASSMANAGER_ARC_RCSTATETRANSITION_H
#define SWIFT_SILOPTIMIZER_PASSMANAGER_ARC_RCSTATETRANSITION_H

#include "swift/Basic/type_traits.h"
#include "swift/Basic/ImmutablePointerSet.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILInstruction.h"
#include "llvm/ADT/SmallPtrSet.h"
#include <cstdint>

namespace swift {

class RCIdentityFunctionInfo;
class ConsumedArgToEpilogueReleaseMatcher;

} // end swift namespace

//===----------------------------------------------------------------------===//
//                           RCStateTransitionKind
//===----------------------------------------------------------------------===//

namespace swift {

/// The kind of an RCStateTransition.
enum class RCStateTransitionKind : uint8_t {
#define KIND(K) K,
#define ABSTRACT_VALUE(Name, StartKind, EndKind) \
  Name ## _Start = StartKind, Name ## _End = EndKind,
#include "RCStateTransition.def"
};

/// \returns the RCStateTransitionKind corresponding to \p V.
RCStateTransitionKind getRCStateTransitionKind(ValueBase *V);

/// Define predicates to test for RCStateTransition abstract value kinds.
#define ABSTRACT_VALUE(Name, Start, End)                              \
  bool isRCStateTransition ## Name(RCStateTransitionKind Kind);       \
  static inline bool isRCStateTransition ## Name(ValueBase *V) {      \
    return isRCStateTransition ## Name(getRCStateTransitionKind(V));  \
  }
#define KIND(Name)                                                      \
  static inline bool isRCStateTransition ## Name(ValueBase *V) {        \
    return RCStateTransitionKind::Name == getRCStateTransitionKind(V);  \
  }
#include "RCStateTransition.def"

//===----------------------------------------------------------------------===//
//                             RCStateTransition
//===----------------------------------------------------------------------===//

class RefCountState;
class BottomUpRefCountState;
class TopDownRefCountState;

/// Represents a transition in the RC history of a ref count.
class RCStateTransition {
  friend class RefCountState;
  friend class BottomUpRefCountState;
  friend class TopDownRefCountState;

  /// An RCStateTransition can represent either an RC end point (i.e. an initial
  /// or terminal RC transition) or a ptr set of Mutators.
  ValueBase *EndPoint;
  ImmutablePointerSet<SILInstruction> *Mutators =
      ImmutablePointerSetFactory<SILInstruction>::getEmptySet();
  RCStateTransitionKind Kind;

  // Should only be constructed be default RefCountState.
  RCStateTransition() = default;

public:
  ~RCStateTransition() = default;
  RCStateTransition(const RCStateTransition &R) = default;

  RCStateTransition(ImmutablePointerSet<SILInstruction> *I) {
    assert(I->size() == 1);
    SILInstruction *Inst = *I->begin();
    Kind = getRCStateTransitionKind(Inst);
    if (isRCStateTransitionEndPoint(Kind)) {
      EndPoint = Inst;
      return;
    }

    if (isRCStateTransitionMutator(Kind)) {
      Mutators = I;
      return;
    }

    // Unknown kind.
  }

  RCStateTransition(SILFunctionArgument *A)
      : EndPoint(A), Kind(RCStateTransitionKind::StrongEntrance) {
    assert(A->hasConvention(SILArgumentConvention::Direct_Owned) &&
           "Expected owned argument");
  }

  RCStateTransitionKind getKind() const { return Kind; }

/// Define test functions for the various abstract categorizations we have.
#define ABSTRACT_VALUE(Name, StartKind, EndKind) bool is ## Name() const;
#include "RCStateTransition.def"

  /// Return true if this Transition is a mutator transition that contains I.
  bool containsMutator(SILInstruction *I) const {
    assert(isMutator() && "This should only be called if we are of mutator "
                          "kind");
    return Mutators->count(I);
  }

  using mutator_range =
      iterator_range<std::remove_pointer<decltype(Mutators)>::type::iterator>;

  /// Returns a Range of Mutators. Asserts if this transition is not a mutator
  /// transition.
  mutator_range getMutators() const {
    assert(isMutator() && "This should never be called given mutators");
    return {Mutators->begin(), Mutators->end()};
  }

  /// Return true if Inst is an instruction that causes a transition that can be
  /// paired with this transition.
  bool matchingInst(SILInstruction *Inst) const;

  /// Attempt to merge \p Other into \p this. Returns true if we succeeded,
  /// false otherwise.
  bool merge(const RCStateTransition &Other);

  /// Return true if the kind of this RCStateTransition is not 'Invalid'.
  bool isValid() const { return getKind() != RCStateTransitionKind::Invalid; }
};

// These static assert checks are here for performance reasons.
static_assert(IsTriviallyCopyable<RCStateTransition>::value,
              "RCStateTransitions must be trivially copyable");

} // end swift namespace

namespace llvm {
raw_ostream &operator<<(raw_ostream &os, swift::RCStateTransitionKind Kind);
} // end llvm namespace

#endif
