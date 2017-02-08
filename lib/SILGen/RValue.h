//===--- RValue.h - Exploded RValue Representation --------------*- C++ -*-===//
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
//
// A storage structure for holding a destructured rvalue with an optional
// cleanup(s).
// Ownership of the rvalue can be "forwarded" to disable the associated
// cleanup(s).
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_LOWERING_RVALUE_H
#define SWIFT_LOWERING_RVALUE_H

#include "ManagedValue.h"
#include "SILGenFunction.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {
namespace Lowering {
  class Initialization;

/// An "exploded" SIL rvalue, in which tuple values are recursively
/// destructured. (In SILGen we don't try to explode structs, because doing so
/// would require considering resilience, a job we want to delegate to IRGen).
class RValue {
  std::vector<ManagedValue> values;
  CanType type;
  unsigned elementsToBeAdded;
  
  /// Flag value used to mark an rvalue as invalid, because it was
  /// consumed or it was default-initialized.
  enum : unsigned { Used = ~0U };
  
  // Don't copy.
  RValue(const RValue &) = delete;
  RValue &operator=(const RValue &) = delete;
  
  void makeUsed() {
    elementsToBeAdded = Used;
    values = {};
  }
  
  /// Private constructor used by copy().
  RValue(const RValue &copied, SILGenFunction &gen, SILLocation l);
  
  /// Construct an RValue from a pre-exploded set of
  /// ManagedValues. Used to implement the extractElement* methods.
  RValue(ArrayRef<ManagedValue> values, CanType type);

public:
  /// Creates an invalid RValue object, in a "used" state.
  RValue() : elementsToBeAdded(Used) {}
  
  RValue(RValue &&rv)
    : values(std::move(rv.values)),
      type(rv.type),
      elementsToBeAdded(rv.elementsToBeAdded)
  {
    assert((rv.isComplete() || rv.isUsed())
           && "moving rvalue that wasn't complete?!");
    rv.elementsToBeAdded = Used;
  }
  RValue &operator=(RValue &&rv) {
    assert(isUsed() && "reassigning an unused rvalue?!");
    
    assert((rv.isComplete() || rv.isUsed())
           && "moving rvalue that wasn't complete?!");
    values = std::move(rv.values);
    type = rv.type;
    elementsToBeAdded = rv.elementsToBeAdded;
    rv.elementsToBeAdded = Used;
    return *this;
  }
  
  /// Create an RValue from a single value. If the value is of tuple type, it
  /// will be exploded.
  ///
  /// \param expr - the expression which yielded this r-value; its type
  ///   will become the substituted formal type of this r-value
  RValue(SILGenFunction &gen, Expr *expr, ManagedValue v);

  /// Create an RValue from a single value. If the value is of tuple type, it
  /// will be exploded.
  RValue(SILGenFunction &gen, SILLocation l, CanType type, ManagedValue v);

  /// Construct an RValue from a pre-exploded set of
  /// ManagedValues. Used to implement the extractElement* methods.
  static RValue withPreExplodedElements(ArrayRef<ManagedValue> values,
                                        CanType type);
  
  /// Create an RValue to which values will be subsequently added using
  /// addElement(), with the level of tuple expansion in the input specified
  /// by the abstraction pattern. The RValue will not be complete until all
  /// the elements have been added.
  explicit RValue(AbstractionPattern pattern, CanType type);
  
  /// Create an RValue to which values will be subsequently added using
  /// addElement(). The RValue will not be complete until all the elements have
  /// been added.
  explicit RValue(CanType type);
  
  /// True if the rvalue has been completely initialized by adding all its
  /// elements.
  bool isComplete() const & { return elementsToBeAdded == 0; }
  
  /// True if this rvalue has been used.
  bool isUsed() const & { return elementsToBeAdded == Used; }
  explicit operator bool() const & { return !isUsed(); }

  /// True if this rvalue was emitted into context.
  bool isInContext() const & { return isUsed(); }
  
  /// True if this represents an lvalue.
  bool isLValue() const & {
    return isa<InOutType>(type);
  }
  
  /// Add an element to the rvalue. The rvalue must not yet be complete.
  void addElement(RValue &&element) &;
  
  /// Add a ManagedValue element to the rvalue, exploding tuples if necessary.
  /// The rvalue must not yet be complete.
  void addElement(SILGenFunction &gen, ManagedValue element,
                  CanType formalType, SILLocation l) &;
  
  /// Forward an rvalue into a single value, imploding tuples if necessary.
  SILValue forwardAsSingleValue(SILGenFunction &gen, SILLocation l) &&;

  /// Forward an rvalue into a single value, imploding tuples if necessary, and
  /// introducing a potential conversion from semantic type to storage type.
  SILValue forwardAsSingleStorageValue(SILGenFunction &gen,
                                       SILType storageType,
                                       SILLocation l) &&;

  /// Get the rvalue as a single value, imploding tuples if necessary.
  ManagedValue getAsSingleValue(SILGenFunction &gen, SILLocation l) &&;
  
  /// Get the rvalue as a single unmanaged value, imploding tuples if necessary.
  /// The values must not require any cleanups.
  SILValue getUnmanagedSingleValue(SILGenFunction &gen, SILLocation l) const &;
  
  /// Peek at the single scalar value backing this rvalue without consuming it.
  /// The rvalue must not be of a tuple type.
  SILValue peekScalarValue() const & {
    assert(!isa<TupleType>(type) && "peekScalarValue of a tuple rvalue");
    assert(values.size() == 1 && "exploded scalar value?!");
    return values[0].getValue();
  }

  /// Peek at the single ManagedValue backing this rvalue without consuming it
  /// and return true if the value is not at +1.
  bool peekIsPlusZeroRValueOrTrivial() const & {
    assert(!isa<TupleType>(type) && "peekScalarValue of a tuple rvalue");
    assert(values.size() == 1 && "exploded scalar value?!");
    return values[0].isPlusZeroRValueOrTrivial();
  }

  ManagedValue getScalarValue() && {
    assert(!isa<TupleType>(type) && "getScalarValue of a tuple rvalue");
    assert(values.size() == 1);
    auto value = values[0];
    makeUsed();
    return value;
  }

  /// Use this rvalue to initialize an Initialization.
  void forwardInto(SILGenFunction &gen, SILLocation loc, Initialization *I) &&;

  /// Copy this rvalue to initialize an Initialization without consuming the
  /// rvalue.
  void copyInto(SILGenFunction &gen, SILLocation loc, Initialization *I) const&;

  /// Assign this r-value into the destination.
  void assignInto(SILGenFunction &gen, SILLocation loc, SILValue destAddr) &&;
  
  /// Forward the exploded SILValues into a SmallVector.
  void forwardAll(SILGenFunction &gen,
                  SmallVectorImpl<SILValue> &values) &&;

  ManagedValue materialize(SILGenFunction &gen, SILLocation loc) &&;
  
  /// Take the ManagedValues from this RValue into a SmallVector.
  void getAll(SmallVectorImpl<ManagedValue> &values) &&;
  
  /// Store the unmanaged SILValues into a SmallVector. The values must not
  /// require any cleanups.
  void getAllUnmanaged(SmallVectorImpl<SILValue> &values) const &;
  
  /// Extract a single tuple element from the rvalue.
  RValue extractElement(unsigned element) &&;
  
  /// Extract the tuple elements from the rvalue.
  void extractElements(SmallVectorImpl<RValue> &elements) &&;
  
  CanType getType() const & { return type; }

  /// Rewrite the type of this r-value.
  void rewriteType(CanType newType) & {
    // We only allow a very modest set of changes to a type.
    assert(newType == type ||
           (isa<TupleType>(newType) &&
            cast<TupleType>(newType)->getNumElements() == 1 &&
            cast<TupleType>(newType).getElementType(0) == type));
    type = newType;
  }
  
  /// Emit an equivalent value with independent ownership.
  RValue copy(SILGenFunction &gen, SILLocation l) const & {
    return RValue(*this, gen, l);
  }

  bool isObviouslyEqual(const RValue &rhs) const {
    assert(isComplete() && rhs.isComplete() && "Comparing incomplete rvalues");

    // Compare the count of elements instead of the type.
    if (values.size() != rhs.values.size())
      return false;

    return std::equal(values.begin(), values.end(), rhs.values.begin(),
                  [](const ManagedValue &lhs, const ManagedValue &rhs) -> bool {
                    return lhs.getValue() == rhs.getValue() &&
                    lhs.getCleanup() == rhs.getCleanup();
                  });
  }
};

} // end namespace Lowering
} // end namespace swift

#endif
