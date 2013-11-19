//===--- RValue.h - Exploded RValue Representation --------------*- C++ -*-===//
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
//
// A storage structure for holding a destructured rvalue with an optional
// cleanup(s).
// Ownership of the rvalue can be "forwarded" to disable the associated
// cleanup(s).
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_LOWERING_RVALUE_H
#define SWIFT_LOWERING_RVALUE_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "SILGen.h"

namespace swift {
  class SILValue;
  
namespace Lowering {
  class Initialization;

/// ManagedValue - represents a SIL rvalue. It consists of a SILValue and an
/// optional cleanup. Ownership of the ManagedValue can be "forwarded" to
/// disable its cleanup when the rvalue is consumed. A ManagedValue can also
/// represent an LValue used as a value, such as a [inout] function argument.
class ManagedValue {
  /// The value (or address of an address-only value) being managed, and
  /// whether it represents an lvalue.
  llvm::PointerIntPair<SILValue, 1, bool> valueAndIsLValue;
  /// A handle to the cleanup that destroys this value, or
  /// CleanupHandle::invalid() if the value has no cleanup.
  CleanupHandle cleanup;

public:
  enum Unmanaged_t { Unmanaged };
  enum LValue_t { LValue };
  
  ManagedValue() = default;
  explicit ManagedValue(SILValue value, LValue_t)
    : valueAndIsLValue(value, true),
      cleanup(CleanupHandle::invalid())
  {}
  explicit ManagedValue(SILValue value, Unmanaged_t)
    : valueAndIsLValue(value, false),
      cleanup(CleanupHandle::invalid())
  {}
  ManagedValue(SILValue value, CleanupHandle cleanup)
    : valueAndIsLValue(value, false),
      cleanup(cleanup)
  {}

  static ManagedValue forUnmanaged(SILValue value) {
    return ManagedValue(value, Unmanaged);
  }

  SILValue getUnmanagedValue() const {
    assert(!hasCleanup());
    return getValue();
  }
  SILValue getValue() const { return valueAndIsLValue.getPointer(); }
  
  SILType getType() const { return getValue().getType(); }
  
  bool isLValue() const { return valueAndIsLValue.getInt(); }

  CanType getSwiftType() const {
    return isLValue()
      ? getType().getSwiftType()
      : getType().getSwiftRValueType();
  }
  
  /// Emit a copy of this value with independent ownership.
  ManagedValue copy(SILGenFunction &gen, SILLocation l) {
    if (!cleanup.isValid()) {
      assert(gen.getTypeLowering(getType()).isTrivial());
      return *this;
    }
    
    auto &lowering = gen.getTypeLowering(getType());
    assert(!lowering.isTrivial() && "trivial value has cleanup?");

    if (lowering.isAddressOnly()) {
      SILValue buf = gen.emitTemporaryAllocation(l, getType());
      gen.B.createCopyAddr(l, getValue(), buf,
                           IsNotTake, IsInitialization);
      return gen.emitManagedRValueWithCleanup(buf, lowering);
    }
    auto result = lowering.emitCopyValue(gen.B, l, getValue());
    return gen.emitManagedRValueWithCleanup(result, lowering);
  }
  
  /// Store a copy of this value with independent ownership into the given
  /// uninitialized address.
  void copyInto(SILGenFunction &gen, SILValue dest, SILLocation L) {
    auto &lowering = gen.getTypeLowering(getType());
    if (lowering.isAddressOnly()) {
      gen.B.createCopyAddr(L, getValue(), dest,
                           IsNotTake, IsInitialization);
      return;
    }
    auto result = lowering.emitCopyValue(gen.B, L, getValue());
    gen.B.createStore(L, result, dest);
  }
  
  bool hasCleanup() const { return cleanup.isValid(); }
  CleanupHandle getCleanup() const { return cleanup; }

  /// Disable the cleanup for this value.
  void forwardCleanup(SILGenFunction &gen) const {
    assert(hasCleanup() && "value doesn't have cleanup!");
    gen.Cleanups.setCleanupState(getCleanup(), CleanupState::Dead);
  }
  
  /// Forward this value, deactivating the cleanup and returning the
  /// underlying value.
  SILValue forward(SILGenFunction &gen) const {
    if (hasCleanup())
      forwardCleanup(gen);
    return getValue();
  }
  
  /// Forward this value as an argument.
  SILValue forwardArgument(SILGenFunction &gen, SILLocation loc,
                           AbstractCC cc, CanType origNativeTy,
                           CanType substNativeTy, CanType bridgedTy) {
    // Bridge the value to the current calling convention.
    ManagedValue v = gen.emitNativeToBridgedValue(loc, *this, cc,
                                                  origNativeTy, substNativeTy,
                                                  bridgedTy);
    return v.forward(gen);
  }
  
  /// Get this value as an argument without consuming it.
  SILValue getArgumentValue(SILGenFunction &gen, SILLocation loc,
                            AbstractCC cc,
                            CanType origNativeTy, CanType substNativeTy,
                            CanType bridgedTy) {
    // Bridge the value to the current calling convention.
    ManagedValue v = gen.emitNativeToBridgedValue(loc, *this, cc,
                                                  origNativeTy, substNativeTy,
                                                  bridgedTy);
    return v.getValue();
  }
  
  /// Forward this value into memory by storing it to the given address.
  ///
  /// \param gen - The SILGenFunction.
  /// \param loc - the AST location to associate with emitted instructions.
  /// \param address - the address to assign to.
  void forwardInto(SILGenFunction &gen, SILLocation loc, SILValue address);
  
  /// Assign this value into memory, destroying the existing
  /// value at the destination address.
  ///
  /// \param gen - The SILGenFunction.
  /// \param loc - the AST location to associate with emitted instructions.
  /// \param address - the address to assign to.
  void assignInto(SILGenFunction &gen, SILLocation loc, SILValue address);
  
  explicit operator bool() const {
    return bool(getValue());
  }
};
  
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
  
  /// Create a RValue from a single value. If the value is of tuple type, it
  /// will be exploded.
  ///
  /// \param expr - the expression which yielded this r-value; its type
  ///   will become the substituted formal type of this r-value
  RValue(SILGenFunction &gen, Expr *expr, ManagedValue v);

  /// Create a RValue from a single value. If the value is of tuple type, it
  /// will be exploded.
  RValue(SILGenFunction &gen, SILLocation l, CanType type, ManagedValue v);

  /// Construct an RValue from a pre-exploded set of
  /// ManagedValues. Used to implement the extractElement* methods.
  RValue(ArrayRef<ManagedValue> values, CanType type);
  
  /// Create an RValue to which values will be subsequently added using
  /// addElement(). The RValue will not be complete until all the elements have
  /// been added.
  explicit RValue(CanType type);
  
  /// Create an RValue by emitting destructured arguments into a basic block.
  static RValue emitBBArguments(CanType type,
                                SILGenFunction &gen,
                                SILBasicBlock *parent,
                                SILLocation L);
  
  /// True if the rvalue has been completely initialized by adding all its
  /// elements.
  bool isComplete() const & { return elementsToBeAdded == 0; }
  
  /// True if this rvalue has been used.
  bool isUsed() const & { return elementsToBeAdded == Used; }
  explicit operator bool() const & { return !isUsed(); }
  
  /// True if this represents an lvalue.
  bool isLValue() const & { return isa<LValueType>(type); }
  
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

  ManagedValue getScalarValue() && {
    assert(!isa<TupleType>(type) && "getScalarValue of a tuple rvalue");
    assert(values.size() == 1);
    auto value = values[0];
    makeUsed();
    return value;
  }

  /// Use this rvalue to initialize an Initialization.
  void forwardInto(SILGenFunction &gen, Initialization *I, SILLocation Loc) &&;

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
};

/// A means of generating an r-value.
///
/// This is useful as a way to pass r-values around without either:
///   - requiring them to have already been evaluated or
///   - requiring them to come from an identifiable expression.
///
/// Being able to propagate RValues is important because there are a
/// number of cases (involving, say, property accessors) where values
/// are implicitly generated.  However, being able to propagate Expr*s
/// is also important because there are several kinds of expressions
/// (such as closures) which can be emitted more efficiently with a
/// known target abstraction level.
///
/// Because an RValueSource might contain an unevaluated expression,
/// care must be taken when dealing with multiple RValueSources to
/// preserve the original evaluation order of the program.  APIs
/// working with multiple RValueSources should document the order in
/// which they plan to evaluate them.
class RValueSource {
  union Storage {
    struct {
      RValue Value;
      SILLocation Loc;
    } TheRV;
    Expr *TheExpr;

    Storage() {}
    ~Storage() {}
  } Storage;
  bool IsRValue;

  void initRV(SILLocation loc, RValue &&value) {
    assert(IsRValue);
    Storage.TheRV.Loc = loc;
    new (&Storage.TheRV.Value) RValue(std::move(value));
  }

public:
  RValueSource() : IsRValue(false) {
    Storage.TheExpr = nullptr;
  }
  RValueSource(SILLocation loc, RValue &&value) : IsRValue(true) {
    initRV(loc, std::move(value));
  }
  RValueSource(Expr *e) : IsRValue(false) {
    assert(e && "initializing RValueSource with null expression");
    Storage.TheExpr = e;
  }

  // Cannot be copied.
  RValueSource(const RValueSource &other) = delete;
  RValueSource &operator=(const RValueSource &other) = delete;

  // Can be moved.
  RValueSource(RValueSource &&other) : IsRValue(other.IsRValue) {
    if (IsRValue) {
      initRV(other.getKnownRValueLocation(), std::move(other).asKnownRValue());
    } else {
      Storage.TheExpr = std::move(other).asKnownExpr();
    }
  }
  RValueSource &operator=(RValueSource &&other) {
    // Try to move RValue objects in-place.
    if (IsRValue && other.IsRValue) {
      Storage.TheRV.Value = std::move(other).asKnownRValue();
      Storage.TheRV.Loc = other.getKnownRValueLocation();

    // Otherwise, do what's necessary.
    } else {
      this->~RValueSource();
      new (this) RValueSource(std::move(other));
    }
    return *this;
  }

  ~RValueSource() {
    if (IsRValue) {
      asKnownRValue().~RValue();
    }
  }

  explicit operator bool() const & {
    if (isRValue()) {
      return bool(asKnownRValue());
    } else {
      return asKnownExpr() != nullptr;
    }
  }

  CanType getSubstType() const & {
    if (isRValue()) {
      return asKnownRValue().getType();
    } else {
      return asKnownExpr()->getType()->getCanonicalType();
    }
  }

  SILLocation getLocation() const & {
    if (isRValue()) {
      return getKnownRValueLocation();
    } else {
      return asKnownExpr();
    }
  }

  bool isRValue() const & { return IsRValue; }

  /// Given that this source is an expression, extract and clear
  /// that r-value.
  RValue &&asKnownRValue() && {
    assert(isRValue());
    return std::move(Storage.TheRV.Value);
  }
  SILLocation getKnownRValueLocation() const & {
    assert(isRValue());
    return Storage.TheRV.Loc;
  }

  /// Given that this source is an expression, extract and clear
  /// that expression.
  Expr *asKnownExpr() && {
    assert(!isRValue());
    Expr *result = Storage.TheExpr;
    Storage.TheExpr = nullptr;
    return result;
  }

  /// Force this source to become an r-value, then return an unmoved
  /// handle to that r-value.
  RValue &forceAndPeekRValue(SILGenFunction &gen) &;

  RValue getAsRValue(SILGenFunction &gen) &&;
  ManagedValue getAsSingleValue(SILGenFunction &gen) &&;

  void forwardInto(SILGenFunction &gen, Initialization *dest) &&;
  void forwardInto(SILGenFunction &gen, AbstractionPattern origFormalType,
                   Initialization *dest, const TypeLowering &destTL) &&;

  ManagedValue materialize(SILGenFunction &gen) &&;

  /// Emit this value to memory so that it follows the abstraction
  /// patterns of the original formal type.
  ///
  /// \param expectedType - the lowering of getSubstType() under the
  ///   abstractions of origFormalType
  ManagedValue materialize(SILGenFunction &gen,
                           AbstractionPattern origFormalType,
                           SILType expectedType = SILType()) &&;

  // This is a hack and should be avoided.
  void rewriteType(CanType newType) &;

private:
  // Make the non-move accessors private to make it more difficult
  // to accidentally re-emit values.
  const RValue &asKnownRValue() const & {
    assert(isRValue());
    return Storage.TheRV.Value;
  }

  Expr *asKnownExpr() const & {
    assert(!isRValue());
    return Storage.TheExpr;
  }
};

} // end namespace Lowering
} // end namespace swift

#endif
