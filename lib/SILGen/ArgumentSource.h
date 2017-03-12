//===--- ArgumentSource.h - Abstracted source of an argument ----*- C++ -*-===//
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
// A structure for holding an abstracted source of a call argument.
// It's either:
//
//  - an expression, yielding an l-value or r-value
//  - an RValue
//  - an LValue
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_LOWERING_ARGUMENTSOURCE_H
#define SWIFT_LOWERING_ARGUMENTSOURCE_H

#include "RValue.h"
#include "LValue.h"

namespace swift {
namespace Lowering {

/// A means of generating an argument.
///
/// This is useful as a way to pass values around without either:
///   - requiring them to have already been evaluated or
///   - requiring them to come from an identifiable expression.
///
/// Being able to propagate values is important because there are a
/// number of cases (involving, say, property accessors) where values
/// are implicitly or previously generated.  However, being able to
/// propagate Expr*s is also important because there are several kinds
/// of expressions (such as closures) which can be emitted more
/// efficiently with a known target abstraction level.
///
/// Because an ArgumentSource might contain an unevaluated expression,
/// care must be taken when dealing with multiple ArgumentSources to
/// preserve the original evaluation order of the program.  APIs
/// working with multiple ArgumentSources should document the order in
/// which they plan to evaluate them.
class ArgumentSource {
  union Storage {
    struct {
      RValue Value;
      SILLocation Loc;
    } TheRV;
    struct {
      LValue Value;
      SILLocation Loc;
    } TheLV;
    Expr *TheExpr;

    Storage() {}
    ~Storage() {}
  } Storage;

  enum class Kind : unsigned char {
    RValue,
    LValue,
    Expr,
  } StoredKind;

  void initRV(SILLocation loc, RValue &&value) {
    assert(StoredKind == Kind::RValue);
    Storage.TheRV.Loc = loc;
    new (&Storage.TheRV.Value) RValue(std::move(value));
  }

  void initLV(SILLocation loc, LValue &&value) {
    assert(StoredKind == Kind::LValue);
    Storage.TheLV.Loc = loc;
    new (&Storage.TheLV.Value) LValue(std::move(value));
  }

public:
  ArgumentSource() : StoredKind(Kind::Expr) {
    Storage.TheExpr = nullptr;
  }
  ArgumentSource(SILLocation loc, RValue &&value) : StoredKind(Kind::RValue) {
    initRV(loc, std::move(value));
  }
  ArgumentSource(SILLocation loc, LValue &&value) : StoredKind(Kind::LValue) {
    initLV(loc, std::move(value));
  }
  ArgumentSource(Expr *e) : StoredKind(Kind::Expr) {
    assert(e && "initializing ArgumentSource with null expression");
    Storage.TheExpr = e;
  }

  // Cannot be copied.
  ArgumentSource(const ArgumentSource &other) = delete;
  ArgumentSource &operator=(const ArgumentSource &other) = delete;

  // Can be moved.
  ArgumentSource(ArgumentSource &&other) : StoredKind(other.StoredKind) {
    switch (StoredKind) {
    case Kind::RValue:
      initRV(other.getKnownRValueLocation(), std::move(other).asKnownRValue());
      return;
    case Kind::LValue:
      initLV(other.getKnownLValueLocation(), std::move(other).asKnownLValue());
      return;
    case Kind::Expr:
      Storage.TheExpr = std::move(other).asKnownExpr();
      return;
    }
    llvm_unreachable("bad kind");
  }

  ArgumentSource &operator=(ArgumentSource &&other) {
    // If the kinds don't align, just move the other object over this.
    if (StoredKind != other.StoredKind) {
      this->~ArgumentSource();
      new (this) ArgumentSource(std::move(other));
      return *this;
    }

    // Otherwise, move RValue and LValue objects in-place.
    switch (StoredKind) {
    case Kind::RValue:
      Storage.TheRV.Value = std::move(other).asKnownRValue();
      Storage.TheRV.Loc = other.getKnownRValueLocation();
      return *this;
    case Kind::LValue:
      Storage.TheLV.Value = std::move(other).asKnownLValue();
      Storage.TheLV.Loc = other.getKnownLValueLocation();
      return *this;
    case Kind::Expr:
      Storage.TheExpr = std::move(other).asKnownExpr();
      return *this;
    }
    llvm_unreachable("bad kind");
  }

  ~ArgumentSource() {
    switch (StoredKind) {
    case Kind::RValue:
      asKnownRValue().~RValue();
      return;
    case Kind::LValue:
      asKnownLValue().~LValue();
      return;
    case Kind::Expr:
      return;
    }
    llvm_unreachable("bad kind");
  }

  explicit operator bool() const & {
    switch (StoredKind) {
    case Kind::RValue:
      return bool(asKnownRValue());
    case Kind::LValue:
      return asKnownLValue().isValid();
    case Kind::Expr:
      return asKnownExpr() != nullptr;
    }
    llvm_unreachable("bad kind");
  }

  CanType getSubstType() const & {
    switch (StoredKind) {
    case Kind::RValue:
      return asKnownRValue().getType();
    case Kind::LValue:
      return CanInOutType::get(asKnownLValue().getSubstFormalType());
    case Kind::Expr:
      return asKnownExpr()->getType()->getCanonicalType();
    }
    llvm_unreachable("bad kind");
  }

  CanType getSubstRValueType() const & {
    switch (StoredKind) {
    case Kind::RValue:
      return asKnownRValue().getType();
    case Kind::LValue:
      return asKnownLValue().getSubstFormalType();
    case Kind::Expr:
      return asKnownExpr()->getType()->getInOutObjectType()->getCanonicalType();
    }
    llvm_unreachable("bad kind");
  }

  bool hasLValueType() const & {
    switch (StoredKind) {
    case Kind::RValue: return false;
    case Kind::LValue: return true;
    case Kind::Expr: return asKnownExpr()->getType()->is<InOutType>();
    }
    llvm_unreachable("bad kind");    
  }

  SILLocation getLocation() const & {
    switch (StoredKind) {
    case Kind::RValue:
      return getKnownRValueLocation();
    case Kind::LValue:
      return getKnownLValueLocation();
    case Kind::Expr:
      return asKnownExpr();
    }
    llvm_unreachable("bad kind");
  }

  bool isRValue() const & { return StoredKind == Kind::RValue; }
  bool isLValue() const & { return StoredKind == Kind::LValue; }

  /// Given that this source is storing an RValue, extract and clear
  /// that value.
  RValue &&asKnownRValue() && {
    assert(isRValue());
    return std::move(Storage.TheRV.Value);
  }
  SILLocation getKnownRValueLocation() const & {
    assert(isRValue());
    return Storage.TheRV.Loc;
  }

  /// Given that this source is storing an LValue, extract and clear
  /// that value.
  LValue &&asKnownLValue() && {
    assert(isLValue());
    return std::move(Storage.TheLV.Value);
  }
  SILLocation getKnownLValueLocation() const & {
    assert(isLValue());
    return Storage.TheLV.Loc;
  }

  /// Given that this source is an expression, extract and clear
  /// that expression.
  Expr *asKnownExpr() && {
    assert(StoredKind == Kind::Expr);
    Expr *result = Storage.TheExpr;
    Storage.TheExpr = nullptr;
    return result;
  }

  /// Force this source to become an r-value, then return an unmoved
  /// handle to that r-value.
  RValue &forceAndPeekRValue(SILGenFunction &SGF) &;

  /// Return an unowned handle to the r-value stored in this source. Undefined
  /// if this ArgumentSource is not an rvalue.
  RValue &peekRValue() &;

  RValue getAsRValue(SILGenFunction &SGF, SGFContext C = SGFContext()) &&;
  ManagedValue getAsSingleValue(SILGenFunction &SGF,
                                SGFContext C = SGFContext()) &&;
  ManagedValue getAsSingleValue(SILGenFunction &SGF,
                                AbstractionPattern origFormalType,
                                SGFContext C = SGFContext()) &&;

  void forwardInto(SILGenFunction &SGF, Initialization *dest) &&;
  void forwardInto(SILGenFunction &SGF, AbstractionPattern origFormalType,
                   Initialization *dest, const TypeLowering &destTL) &&;

  ManagedValue materialize(SILGenFunction &SGF) &&;

  /// Emit this value to memory so that it follows the abstraction
  /// patterns of the original formal type.
  ///
  /// \param expectedType - the lowering of getSubstType() under the
  ///   abstractions of origFormalType
  ManagedValue materialize(SILGenFunction &SGF,
                           AbstractionPattern origFormalType,
                           SILType expectedType = SILType()) &&;

  // This is a hack and should be avoided.
  void rewriteType(CanType newType) &;

  /// Whether this argument source requires the callee to evaluate.
  bool requiresCalleeToEvaluate();

private:
  // Make the non-move accessors private to make it more difficult
  // to accidentally re-emit values.
  const RValue &asKnownRValue() const & {
    assert(isRValue());
    return Storage.TheRV.Value;
  }

  // Make the non-move accessors private to make it more difficult
  // to accidentally re-emit values.
  const LValue &asKnownLValue() const & {
    assert(isLValue());
    return Storage.TheLV.Value;
  }

  Expr *asKnownExpr() const & {
    assert(StoredKind == Kind::Expr);
    return Storage.TheExpr;
  }
};

} // end namespace Lowering
} // end namespace swift

#endif
