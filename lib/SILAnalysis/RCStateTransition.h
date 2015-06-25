//===--- RCStateTransition.h -------------------------------*- C++ -*------===//
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

#ifndef SWIFT_SILANALYSIS_RCSTATETRANSITION_H
#define SWIFT_SILANALYSIS_RCSTATETRANSITION_H

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

/// The kind of a RCStateTransition.
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

template <typename T> struct RefCountState;

/// Represents a transition in the RC history of a ref count.
class RCStateTransition {
  template <typename T> friend struct RefCountState;

  /// An RCStateTransition can represent either an RC end point (i.e. an initial
  /// or terminal RC transition) or a ptr set of Mutators.
  ValueBase *EndPoint;
  llvm::SmallPtrSet<SILInstruction *, 2> Mutators;
  RCStateTransitionKind Kind;

  // Should only be constructed be default RefCountState.
  RCStateTransition() {}

public:
  ~RCStateTransition() {}
  RCStateTransition(const RCStateTransition &R);

  RCStateTransition(SILInstruction *I) {
    Kind = getRCStateTransitionKind(I);
    if (isRCStateTransitionEndPoint(Kind)) {
      EndPoint = I;
      return;
    }

    if (isRCStateTransitionMutator(Kind)) {
      Mutators.insert(I);
      return;
    }

    // Unknown kind.
  }

  RCStateTransition(SILArgument *A)
      : EndPoint(A), Kind(RCStateTransitionKind::StrongEntrance) {
    assert(A->hasConvention(ParameterConvention::Direct_Owned) &&
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
    return Mutators.count(I);
  }

  using mutator_range = iterator_range<decltype(Mutators)::iterator>;
  using const_mutator_range = iterator_range<decltype(Mutators)::const_iterator>;

  /// Returns a Range of Mutators. Asserts if this transition is not a mutator
  /// transition.
  mutator_range getMutators() const {
    assert(isMutator() && "This should never be called given mutators");
    return {Mutators.begin(), Mutators.end()};
  }

  /// Return true if Inst is an instruction that causes a transition that can be
  /// paired with this transition.
  bool matchingInst(SILInstruction *Inst) const;

  /// Attempt to merge \p Other into \p this. Returns true if we succeeded,
  /// false otherwise.
  bool merge(const RCStateTransition &Other);
};

} // end swift namespace

namespace llvm {
raw_ostream &operator<<(raw_ostream &os, swift::RCStateTransitionKind Kind);
} // end llvm namespace

#endif
