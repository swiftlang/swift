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

  Identifier Name;
  unsigned ArgNo : 16;
  unsigned Constant : 1;
  unsigned Implicit : 1;
  unsigned isDenseMapSingleton : 2;
  Optional<SILType> Type;
  Optional<SILLocation> Loc;
  const SILDebugScope *Scope;
  const SILDebugInfoExpression *DIExpr;

  // Use vanilla copy ctor / operator
  SILDebugVariable(const SILDebugVariable &) = default;
  SILDebugVariable &operator=(const SILDebugVariable &) = default;

  enum class IsDenseMapSingleton { No, IsEmpty, IsTombstone };
  SILDebugVariable(IsDenseMapSingleton inputIsDenseMapSingleton)
      : SILDebugVariable() {
    assert(inputIsDenseMapSingleton != IsDenseMapSingleton::No &&
           "Should only pass IsEmpty or IsTombstone");
    isDenseMapSingleton = unsigned(inputIsDenseMapSingleton);
  }

  SILDebugVariable()
      : ArgNo(0), Constant(false), Implicit(false), isDenseMapSingleton(0),
        Scope(nullptr), DIExpr(nullptr) {}
  SILDebugVariable(bool Constant, uint16_t ArgNo)
      : ArgNo(ArgNo), Constant(Constant), Implicit(false),
        isDenseMapSingleton(0), Scope(nullptr), DIExpr(nullptr) {}
  SILDebugVariable(SILModule &mod, Identifier Name, bool Constant,
                   unsigned ArgNo, bool IsImplicit = false,
                   Optional<SILType> AuxType = {},
                   Optional<SILLocation> DeclLoc = {},
                   const SILDebugScope *DeclScope = nullptr,
                   const SILDebugInfoExpression *DbgInfoExpr = nullptr)
      : Name(Name), ArgNo(ArgNo), Constant(Constant), Implicit(IsImplicit),
        isDenseMapSingleton(0), Type(AuxType), Loc(DeclLoc), Scope(DeclScope),
        DIExpr(DbgInfoExpr) {}

  /// Created from either AllocStack or AllocBox instruction
  static Optional<SILDebugVariable>
  createFromAllocation(const AllocationInst *AI);

  // We're not comparing DIExpr here because strictly speaking,
  // DIExpr is not part of the debug variable. We simply piggyback
  // it in this class so that's it's easier to carry DIExpr around.
  bool operator==(const SILDebugVariable &V) const {
    return ArgNo == V.ArgNo && Constant == V.Constant && Name == V.Name &&
           Implicit == V.Implicit && Type == V.Type && Loc == V.Loc &&
           Scope == V.Scope && isDenseMapSingleton == V.isDenseMapSingleton &&
           DIExpr == V.DIExpr;
  }

  SILDebugVariable withoutDIExpr() const {
    auto result = *this;
    result.DIExpr = {};
    return result;
  }

  void appendingElements(SILModule &mod,
                         ArrayRef<const SILDIExprElement *> newElts) {
    if (DIExpr) {
      DIExpr = DIExpr->append(mod, newElts);
      return;
    }
    DIExpr = SILDebugInfoExpression::get(mod, newElts);
  }

  void appendingElements(SILModule &mod,
                         const SILDebugInfoExpression *newDIExpr) {
    if (DIExpr) {
      DIExpr = DIExpr->append(mod, newDIExpr);
      return;
    }
    DIExpr = newDIExpr;
  }

  void prependingElements(SILModule &mod,
                          ArrayRef<const SILDIExprElement *> newElts) {
    if (DIExpr) {
      DIExpr = DIExpr->prepend(mod, newElts);
      return;
    }
    DIExpr = SILDebugInfoExpression::get(mod, newElts);
  }

  bool isLet() const { return Name.str().size() && Constant; }

  bool isVar() const { return Name.str().size() && !Constant; }

  StringRef getName() const { return Name.str(); }

  bool hasFragment() const { return DIExpr ? DIExpr->hasFragment() : false; }

  bool exprStartsWithDeref() const {
    return DIExpr ? DIExpr->startsWithDeref() : false;
  }

  void eraseDeref(SILModule &mod) {
    if (!DIExpr)
      return;
    DIExpr = DIExpr->dropDeref(mod);
  }
};

/// Returns the hashcode for the new projection path.
inline llvm::hash_code hash_value(const SILDebugVariable &P) {
  return llvm::hash_combine(P.ArgNo, P.Constant, P.Name, P.Implicit,
                            P.isDenseMapSingleton, P.Type, P.Loc, P.Scope,
                            P.DIExpr);
}

} // namespace swift

#endif
