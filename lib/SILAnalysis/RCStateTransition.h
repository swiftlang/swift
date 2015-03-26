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

//===----------------------------------------------------------------------===//
//                           RCStateTransitionKind
//===----------------------------------------------------------------------===//

/// The kind of a RCStateTransition.
///
/// TODO: Add in StrongExit.
enum class RCStateTransitionKind : uint8_t {
  /// An unknown RCStateTransitionKind.
  Unknown,

  // The introduction of a strong reference count. This can go on SILArguments
  // and non-terminator instructions.
  StrongEntrance,

  // The increment of a strong reference count. This can only represent
  // non-terminator instructions.
  StrongIncrement,

  // The decrement of a strong reference count. This can only represent
  // non-terminator instructions.
  StrongDecrement,
};

RCStateTransitionKind getRCStateTransitionKind(ValueBase *V);

/// Returns true if Kind is an initial or terminating transition kind.
bool isRCStateTransitionEndPoint(RCStateTransitionKind Kind);

/// Returns true if Kind mutates an RCIdentity that already has been introduced.
bool isRCStateTransitionMutator(RCStateTransitionKind Kind);

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

  /// Returns true if RCStateTransition act as an initial or terminal value to a
  /// string of RC operations.
  bool isEndPoint() const;

  /// Returns true if RCStateTransition acts as a mutator on an RC that was
  /// already created.
  bool isMutator() const;

  /// Return true if this Transition is a mutator transition that contains I.
  bool containsMutator(SILInstruction *I) const {
    assert(isMutator() && "This should only be called if we are of mutator "
                          "kind");
    return Mutators.count(I);
  }

  using mutator_range = Range<decltype(Mutators)::iterator>;
  using const_mutator_range = Range<decltype(Mutators)::const_iterator>;

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

#endif
