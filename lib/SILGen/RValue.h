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
///
/// \file
///
/// A storage structure for holding a destructured rvalue with an optional
/// cleanup(s).
///
/// Ownership of the rvalue can be "forwarded" to disable the associated
/// cleanup(s).
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_LOWERING_RVALUE_H
#define SWIFT_LOWERING_RVALUE_H

#include "ManagedValue.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {
namespace Lowering {

class Initialization;
class SILGenFunction;

/// An "exploded" SIL rvalue, in which tuple values are recursively
/// destructured.
///
/// *NOTE* In SILGen we don't try to explode structs, because doing so would
/// require considering resilience, a job we want to delegate to IRGen.
class RValue {
  std::vector<ManagedValue> values;
  CanType type;
  unsigned elementsToBeAdded;
  
  /// Flag value used to mark an rvalue as invalid, because it was
  /// consumed or it was default-initialized.
  enum : unsigned {
    Null = ~0U,
    Used = Null - 1,
    InContext = Used - 1,
  };

  bool isInSpecialState() const {
    return elementsToBeAdded >= InContext;
  }
  
  // Don't copy.
  RValue(const RValue &) = delete;
  RValue &operator=(const RValue &) = delete;
  
  void makeUsed() {
    elementsToBeAdded = Used;
    values = {};
  }

  /// Private constructor used by copy() and borrow().
  RValue(std::vector<ManagedValue> &&values, CanType type,
         unsigned elementsToBeAdded)
      : values(std::move(values)), type(type),
        elementsToBeAdded(elementsToBeAdded) {}

  /// Construct an RValue from a pre-exploded set of ManagedValues.
  ///
  /// Used to implement the extractElement* methods. *NOTE* This constructor
  /// assumes that the constructed RValue is fully formed and thus has
  /// elementsToBeAdded set to zero.
  RValue(ArrayRef<ManagedValue> values, CanType type);

  RValue(unsigned state) : elementsToBeAdded(state) {
    assert(isInSpecialState());
  }

public:
  RValue() : elementsToBeAdded(Null) {}
  
  RValue(RValue &&rv) : values(std::move(rv.values)),
                        type(rv.type),
                        elementsToBeAdded(rv.elementsToBeAdded) {
    assert((rv.isComplete() || rv.isInSpecialState())
           && "moving rvalue that wasn't complete?!");
    rv.elementsToBeAdded = Used;
  }

  RValue &operator=(RValue &&rv) {
    assert((isNull() || isUsed()) && "reassigning an valid rvalue?!");
    
    assert((rv.isComplete() || rv.isInSpecialState())
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
  RValue(SILGenFunction &SGF, Expr *expr, ManagedValue v);

  /// Create an RValue from a single value. If the value is of tuple type, it
  /// will be exploded.
  RValue(SILGenFunction &SGF, SILLocation l, CanType type, ManagedValue v);

  /// Construct an RValue from a pre-exploded set of ManagedValues.
  ///
  /// This is used to implement the extractElement* methods.
  static RValue withPreExplodedElements(ArrayRef<ManagedValue> values,
                                        CanType type);

  /// Creates an invalid RValue object, in an "in-context" state.
  static RValue forInContext() {
    return RValue(InContext);
  }
  
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

  /// True if the rvalue was null-initialized.
  bool isNull() const & { return elementsToBeAdded == Null; }
  
  /// True if this rvalue has been used.
  bool isUsed() const & { return elementsToBeAdded == Used; }

  /// True if this rvalue was emitted into context.
  bool isInContext() const & { return elementsToBeAdded == InContext; }
  
  /// True if this represents an lvalue.
  bool isLValue() const & {
    return isa<InOutType>(type);
  }
  
  /// Add an element to the rvalue. The rvalue must not yet be complete.
  void addElement(RValue &&element) &;
  
  /// Add a ManagedValue element to the rvalue, exploding tuples if necessary.
  /// The rvalue must not yet be complete.
  void addElement(SILGenFunction &SGF, ManagedValue element,
                  CanType formalType, SILLocation l) &;
  
  /// Forward an rvalue into a single value, imploding tuples if necessary.
  SILValue forwardAsSingleValue(SILGenFunction &SGF, SILLocation l) &&;

  /// Forward an rvalue into a single value, imploding tuples if necessary, and
  /// introducing a potential conversion from semantic type to storage type.
  SILValue forwardAsSingleStorageValue(SILGenFunction &SGF,
                                       SILType storageType,
                                       SILLocation l) &&;

  /// Get the rvalue as a single value, imploding tuples if necessary.
  ManagedValue getAsSingleValue(SILGenFunction &SGF, SILLocation l) &&;
  
  /// Get the rvalue as a single unmanaged value, imploding tuples if necessary.
  /// The values must not require any cleanups.
  SILValue getUnmanagedSingleValue(SILGenFunction &SGF, SILLocation l) const &;
  
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
  void forwardInto(SILGenFunction &SGF, SILLocation loc, Initialization *I) &&;

  /// Copy this rvalue to initialize an Initialization without consuming the
  /// rvalue.
  void copyInto(SILGenFunction &SGF, SILLocation loc, Initialization *I) const&;

  /// Assign this r-value into the destination.
  void assignInto(SILGenFunction &SGF, SILLocation loc, SILValue destAddr) &&;
  
  /// Forward the exploded SILValues into a SmallVector.
  void forwardAll(SILGenFunction &SGF,
                  SmallVectorImpl<SILValue> &values) &&;

  ManagedValue materialize(SILGenFunction &SGF, SILLocation loc) &&;
  
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
#ifndef NDEBUG
    static const auto areSimilarTypes = [](CanType l, CanType r) {
      if (l == r) return true;

      // Allow function types to disagree about 'noescape'.
      if (auto lf = dyn_cast<FunctionType>(l)) {
        if (auto rf = dyn_cast<FunctionType>(r)) {
          return lf.getInput() == rf.getInput()
              && lf.getResult() == rf.getResult()
              && lf->getExtInfo().withNoEscape(false) ==
                 lf->getExtInfo().withNoEscape(false);
        }
      }
      return false;
    };

    static const auto isSingleElementTuple = [](CanType type, CanType eltType) {
      if (auto tupleType = dyn_cast<TupleType>(type)) {
        return tupleType->getNumElements() == 1 &&
               areSimilarTypes(tupleType.getElementType(0), eltType);
      }
      return false;
    };

    // We only allow a very modest set of changes to a type.
    assert(areSimilarTypes(newType, type) ||
           isSingleElementTuple(newType, type) ||
           isSingleElementTuple(type, newType));
#endif
    type = newType;
  }
  
  /// Emit an equivalent value with independent ownership.
  RValue copy(SILGenFunction &SGF, SILLocation loc) const &;

  /// Borrow all subvalues of the rvalue.
  RValue borrow(SILGenFunction &SGF, SILLocation loc) const &;

  static bool areObviouslySameValue(SILValue lhs, SILValue rhs);
  bool isObviouslyEqual(const RValue &rhs) const;

  void dump() const;
  void dump(raw_ostream &OS, unsigned indent = 0) const;
};

} // end namespace Lowering
} // end namespace swift

#endif
