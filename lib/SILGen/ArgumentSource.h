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

#include "swift/Basic/ExternalUnion.h"
#include "RValue.h"
#include "LValue.h"

namespace swift {
namespace Lowering {
class Conversion;

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
  enum class Kind : uint8_t {
    Invalid,
    RValue,
    LValue,
    Expr,
    Tuple,
  };

  struct RValueStorage {
    RValue Value;
    SILLocation Loc;
  };
  struct LValueStorage {
    LValue Value;
    SILLocation Loc;
  };
  struct TupleStorage {
    CanTupleType SubstType;
    SILLocation Loc;
    std::vector<ArgumentSource> Elements;

    TupleStorage(CanTupleType type, SILLocation loc,
                 MutableArrayRef<ArgumentSource> elements)
        : SubstType(type), Loc(loc) {
      assert(type->getNumElements() == elements.size());
      Elements.reserve(elements.size());
      for (auto i : indices(elements)) {
        Elements.push_back(std::move(elements[i]));
      }
    }
  };

  using StorageMembers =
    ExternalUnionMembers<void, RValueStorage, LValueStorage,
                         Expr*, TupleStorage>;

  static StorageMembers::Index getStorageIndexForKind(Kind kind) {
    switch (kind) {
    case Kind::Invalid: return StorageMembers::indexOf<void>();
    case Kind::RValue:
      return StorageMembers::indexOf<RValueStorage>();
    case Kind::LValue: return StorageMembers::indexOf<LValueStorage>();
    case Kind::Expr: return StorageMembers::indexOf<Expr*>();
    case Kind::Tuple: return StorageMembers::indexOf<TupleStorage>();
    }
    llvm_unreachable("bad kind");
  }

  ExternalUnion<Kind, StorageMembers, getStorageIndexForKind> Storage;
  Kind StoredKind;

public:
  ArgumentSource() : StoredKind(Kind::Invalid) {}
  ArgumentSource(SILLocation loc, RValue &&value) : StoredKind(Kind::RValue) {
    Storage.emplaceAggregate<RValueStorage>(StoredKind, std::move(value), loc);
  }
  ArgumentSource(SILLocation loc, LValue &&value) : StoredKind(Kind::LValue) {
    Storage.emplaceAggregate<LValueStorage>(StoredKind, std::move(value), loc);
  }
  ArgumentSource(Expr *e) : StoredKind(Kind::Expr) {
    assert(e && "initializing ArgumentSource with null expression");
    Storage.emplace<Expr*>(StoredKind, e);
  }
  ArgumentSource(SILLocation loc, CanTupleType type,
                 MutableArrayRef<ArgumentSource> elements)
      : StoredKind(Kind::Tuple) {
    Storage.emplace<TupleStorage>(StoredKind, type, loc, elements);
  }

  // Cannot be copied.
  ArgumentSource(const ArgumentSource &other) = delete;
  ArgumentSource &operator=(const ArgumentSource &other) = delete;

  // Can be moved.
  ArgumentSource(ArgumentSource &&other) : StoredKind(other.StoredKind) {
    Storage.moveConstruct(StoredKind, std::move(other.Storage));
  }

  ArgumentSource &operator=(ArgumentSource &&other) {
    Storage.moveAssign(StoredKind, other.StoredKind, std::move(other.Storage));
    StoredKind = other.StoredKind;
    other.Storage.destruct(other.StoredKind);
    other.StoredKind = Kind::Invalid;
    return *this;
  }

  ~ArgumentSource() {
    Storage.destruct(StoredKind);
  }

  explicit operator bool() const & {
    switch (StoredKind) {
    case Kind::Invalid:
      return false;
    case Kind::RValue:
      return !asKnownRValue().isNull();
    case Kind::LValue:
      return asKnownLValue().isValid();
    case Kind::Expr:
      return asKnownExpr() != nullptr;
    case Kind::Tuple:
      return true;
    }
    llvm_unreachable("bad kind");
  }

  CanType getSubstType() const & {
    switch (StoredKind) {
    case Kind::Invalid:
      llvm_unreachable("argument source is invalid");
    case Kind::RValue:
      return asKnownRValue().getType();
    case Kind::LValue:
      return CanInOutType::get(asKnownLValue().getSubstFormalType());
    case Kind::Expr:
      return asKnownExpr()->getType()->getCanonicalType();
    case Kind::Tuple:
      return Storage.get<TupleStorage>(StoredKind).SubstType;
    }
    llvm_unreachable("bad kind");
  }

  SILType getSILSubstType(SILGenFunction &SGF) const &;

  CanType getSubstRValueType() const & {
    switch (StoredKind) {
    case Kind::Invalid:
      llvm_unreachable("argument source is invalid");
    case Kind::RValue:
      return asKnownRValue().getType();
    case Kind::LValue:
      return asKnownLValue().getSubstFormalType();
    case Kind::Expr:
      return asKnownExpr()->getType()->getInOutObjectType()->getCanonicalType();
    case Kind::Tuple:
      return Storage.get<TupleStorage>(StoredKind).SubstType;
    }
    llvm_unreachable("bad kind");
  }

  SILType getSILSubstRValueType(SILGenFunction &SGF) const &;

  bool hasLValueType() const & {
    switch (StoredKind) {
    case Kind::Invalid: llvm_unreachable("argument source is invalid");
    case Kind::RValue:
      return false;
    case Kind::LValue: return true;
    case Kind::Expr: return asKnownExpr()->isSemanticallyInOutExpr();
    case Kind::Tuple: return false;
    }
    llvm_unreachable("bad kind");    
  }

  SILLocation getLocation() const & {
    switch (StoredKind) {
    case Kind::Invalid:
      llvm_unreachable("argument source is invalid");
    case Kind::RValue:
      return getKnownRValueLocation();
    case Kind::LValue:
      return getKnownLValueLocation();
    case Kind::Expr:
      return asKnownExpr();
    case Kind::Tuple:
      return getKnownTupleLocation();
    }
    llvm_unreachable("bad kind");
  }

  bool isExpr() const & { return StoredKind == Kind::Expr; }
  bool isRValue() const & { return StoredKind == Kind::RValue; }
  bool isLValue() const & { return StoredKind == Kind::LValue; }
  bool isTuple() const & { return StoredKind == Kind::Tuple; }

  /// Given that this source is storing an RValue, extract and clear
  /// that value.
  RValue &&asKnownRValue(SILGenFunction &SGF) && {
    return std::move(Storage.get<RValueStorage>(StoredKind).Value);
  }

  SILLocation getKnownRValueLocation() const & {
    return Storage.get<RValueStorage>(StoredKind).Loc;
  }

  /// Given that this source is storing an LValue, extract and clear
  /// that value.
  LValue &&asKnownLValue() && {
    return std::move(Storage.get<LValueStorage>(StoredKind).Value);
  }
  SILLocation getKnownLValueLocation() const & {
    return Storage.get<LValueStorage>(StoredKind).Loc;
  }

  /// Given that this source is an expression, extract and clear
  /// that expression.
  Expr *asKnownExpr() && {
    Expr *result = Storage.get<Expr*>(StoredKind);
    Storage.resetToEmpty<Expr*>(StoredKind, Kind::Invalid);
    StoredKind = Kind::Invalid;
    return result;
  }

  SILLocation getKnownTupleLocation() const & {
    return Storage.get<TupleStorage>(StoredKind).Loc;
  }

  template <class ResultType>
  ResultType withKnownTupleElementSources(
    llvm::function_ref<ResultType(SILLocation loc, CanTupleType type,
                         MutableArrayRef<ArgumentSource> elts)> callback) && {
    auto &tuple = Storage.get<TupleStorage>(StoredKind);

    auto result = callback(tuple.Loc, tuple.SubstType, tuple.Elements);

    // We've consumed the tuple.
    Storage.resetToEmpty<TupleStorage>(StoredKind, Kind::Invalid);
    StoredKind = Kind::Invalid;

    return result;
  }

  /// Return an unowned handle to the r-value stored in this source. Undefined
  /// if this ArgumentSource is not an rvalue.
  RValue &peekRValue() &;

  RValue getAsRValue(SILGenFunction &SGF, SGFContext C = SGFContext()) &&;
  ManagedValue getAsSingleValue(SILGenFunction &SGF,
                                SGFContext C = SGFContext()) &&;
  ManagedValue getAsSingleValue(SILGenFunction &SGF,
                                AbstractionPattern origFormalType,
                                SGFContext C = SGFContext()) &&;

  ManagedValue getConverted(SILGenFunction &SGF, const Conversion &conversion,
                            SGFContext C = SGFContext()) &&;

  void forwardInto(SILGenFunction &SGF, Initialization *dest) &&;
  void forwardInto(SILGenFunction &SGF, AbstractionPattern origFormalType,
                   Initialization *dest, const TypeLowering &destTL) &&;

  /// If we have an rvalue, borrow the rvalue into a new ArgumentSource and
  /// return the ArgumentSource. Otherwise, assert.
  ArgumentSource borrow(SILGenFunction &SGF) const &;

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
  bool requiresCalleeToEvaluate() const;

  void dump() const;
  void dump(raw_ostream &os, unsigned indent = 0) const;

private:
  /// Private helper constructor for delayed borrowed rvalues.
  ArgumentSource(SILLocation loc, RValue &&rv, Kind kind);

  // Make the non-move accessors private to make it more difficult
  // to accidentally re-emit values.
  const RValue &asKnownRValue() const & {
    return Storage.get<RValueStorage>(StoredKind).Value;
  }

  // Make the non-move accessors private to make it more difficult
  // to accidentally re-emit values.
  const LValue &asKnownLValue() const & {
    return Storage.get<LValueStorage>(StoredKind).Value;
  }

  Expr *asKnownExpr() const & {
    return Storage.get<Expr*>(StoredKind);
  }

  RValue getKnownTupleAsRValue(SILGenFunction &SGF, SGFContext C) &&;
};

} // end namespace Lowering
} // end namespace swift

#endif
