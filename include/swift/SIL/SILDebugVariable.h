//===--- SILDebugVariable.h -----------------------------------------------===//
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

#ifndef SWIFT_SIL_SILDEBUGVARIABLE_H
#define SWIFT_SIL_SILDEBUGVARIABLE_H

#include "swift/Basic/LLVM.h"
#include "swift/SIL/SILDebugInfoExpression.h"
#include "swift/SIL/SILLocation.h"
#include "swift/SIL/SILType.h"
#include "llvm/ADT/StringRef.h"

namespace swift {

class AllocationInst;

/// Holds common debug information about local variables and function
/// arguments that are needed by DebugValueInst, AllocStackInst,
/// and AllocBoxInst.
struct SILDebugVariable;
inline llvm::hash_code hash_value(const SILDebugVariable &P);

/// Holds common debug information about local variables and function
/// arguments that are needed by DebugValueInst, AllocStackInst,
/// and AllocBoxInst.
struct SILDebugVariable {
  friend llvm::hash_code hash_value(const SILDebugVariable &P);

  enum class Kind : uint8_t {
    /// A SILDebugVariable that was default initialized and is invalid.
    Invalid,

    /// Describes a SILArgument via an argument number.
    Argument,

    /// Describes a SIL instruction created variable with a name.
    Name,

    /// Is the empty dense map singleton.
    EmptyDenseMapSingleton,

    /// The dense map tombstone singleton.
    TombstoneDenseMapSingleton,
  };

  Identifier Name;
  Optional<SILType> Type;
  Optional<SILLocation> Loc;
  const SILDebugScope *Scope;
  SILDebugInfoExpression *DIExpr;
  unsigned ArgNo : 16;
  unsigned Constant : 1;
  unsigned Implicit : 1;
  unsigned kind : 8;

  // Use vanilla copy ctor / operator
  SILDebugVariable(const SILDebugVariable &) = default;
  SILDebugVariable &operator=(const SILDebugVariable &) = default;

  enum class IsDenseMapSingleton { No, IsEmpty, IsTombstone };
  SILDebugVariable(IsDenseMapSingleton inputIsDenseMapSingleton)
      : SILDebugVariable() {
    assert(inputIsDenseMapSingleton != IsDenseMapSingleton::No &&
           "Should only pass IsEmpty or IsTombstone");
    switch (inputIsDenseMapSingleton) {
    case IsDenseMapSingleton::No:
      llvm_unreachable("Should only pass IsEmpty or IsTombstone");
      break;
    case IsDenseMapSingleton::IsEmpty:
      kind = unsigned(Kind::EmptyDenseMapSingleton);
      break;
    case IsDenseMapSingleton::IsTombstone:
      kind = unsigned(Kind::TombstoneDenseMapSingleton);
      break;
    }
  }

  SILDebugVariable()
      : Scope(nullptr), DIExpr(nullptr), ArgNo(0), Constant(false),
        Implicit(false), kind(unsigned(Kind::Invalid)) {}
  SILDebugVariable(bool Constant, uint16_t ArgNo)
      : Scope(nullptr), DIExpr(nullptr), ArgNo(ArgNo), Constant(Constant),
        Implicit(false), kind(unsigned(Kind::Argument)) {}
  SILDebugVariable(SILModule &mod, Identifier Name, bool Constant,
                   unsigned ArgNo, bool IsImplicit = false,
                   Optional<SILType> AuxType = {},
                   Optional<SILLocation> DeclLoc = {},
                   const SILDebugScope *DeclScope = nullptr,
                   llvm::ArrayRef<SILDIExprElement *> ExprElements = {})
      : Name(Name), Type(AuxType), Loc(DeclLoc), Scope(DeclScope),
        DIExpr(SILDebugInfoExpression::get(mod, ExprElements)), ArgNo(ArgNo),
        Constant(Constant), Implicit(IsImplicit), kind(unsigned(Kind::Name)) {}

  /// Created from either AllocStack or AllocBox instruction
  static Optional<SILDebugVariable>
  createFromAllocation(const AllocationInst *AI);

  // We're not comparing DIExpr here because strictly speaking,
  // DIExpr is not part of the debug variable. We simply piggyback
  // it in this class so that's it's easier to carry DIExpr around.
  bool operator==(const SILDebugVariable &V) const {
    return ArgNo == V.ArgNo && Constant == V.Constant && Name == V.Name &&
           Implicit == V.Implicit && Type == V.Type && Loc == V.Loc &&
           Scope == V.Scope && kind == V.kind && DIExpr == V.DIExpr;
  }

  Kind getKind() const { return Kind(kind); }

  operator bool() const { return bool(getKind()); }

  SILDebugVariable withoutDIExpr() const {
    auto result = *this;
    result.DIExpr = {};
    return result;
  }

  IsDenseMapSingleton isDenseMapSingleton() const {
    switch (getKind()) {
    case Kind::Invalid:
    case Kind::Argument:
    case Kind::Name:
      return IsDenseMapSingleton::No;
    case Kind::EmptyDenseMapSingleton:
      return IsDenseMapSingleton::IsEmpty;
    case Kind::TombstoneDenseMapSingleton:
      return IsDenseMapSingleton::IsTombstone;
    }
  }

  StringRef getName() const { return Name.str(); }

  bool isLet() const { return bool(*this) && getName().size() && Constant; }

  bool isVar() const { return bool(*this) && getName().size() && !Constant; }

  void appendDIExprElements(SILModule &mod, SILDebugInfoExpression *other) {
    appendDIExprElements(mod, other->getElements());
  }

  void appendDIExprElements(SILModule &mod,
                            ArrayRef<SILDIExprElement *> newElements) {
    if (DIExpr) {
      DIExpr = DIExpr->append(mod, newElements);
      return;
    }
    DIExpr = SILDebugInfoExpression::get(mod, newElements);
  }

  void prependDIExprElements(SILModule &mod,
                             ArrayRef<SILDIExprElement *> newElements) {
    if (DIExpr) {
      DIExpr = DIExpr->prepend(mod, newElements);
      return;
    }
    DIExpr = SILDebugInfoExpression::get(mod, newElements);
  }

  void unconditionallyDropDIExprDeref(SILModule &mod) {
    if (!DIExpr)
      return;
    DIExpr = DIExpr->dropDeref(mod);
  }

  void verify() const {
    if (DIExpr)
      DIExpr->verify();
  }
};

/// Returns the hashcode for the new projection path.
inline llvm::hash_code hash_value(const SILDebugVariable &P) {
  return llvm::hash_combine(P.ArgNo, P.Constant, P.Name, P.Implicit, P.kind,
                            P.Type, P.Loc, P.Scope, P.DIExpr);
}

} // namespace swift

#endif
