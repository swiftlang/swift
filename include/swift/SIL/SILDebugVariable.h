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
#include <cstdint>

namespace swift {

class AllocationInst;

//===----------------------------------------------------------------------===//
//                              SILDebugVariable
//===----------------------------------------------------------------------===//

class SILDebugVariable;
llvm::hash_code hash_value(const SILDebugVariable &P);

/// Holds common debug information about local variables and function
/// arguments that are needed by DebugValueInst, AllocStackInst,
/// and AllocBoxInst.
///
/// Design
/// ------
///
/// SILDebugVariable is a wrapper type around a storage class that is stored
/// within the SILModule. It is retrived by calling
/// SILModule::getOrCreateSILDebugVariable(). The reason why we do not use the
/// pointer directly is that SILDebugVariable as an API relies on passing around
/// Optional SILDebugVariables that are value types. So we can keep the same
/// overall API but improve performance. As an additional benefit, we are able
/// to guarantee that pointer equality means value equality and hash equality.
class SILDebugVariable {
  friend class SILModule;
  friend llvm::hash_code hash_value(const SILDebugVariable &P);

  /// State struct that SILDebugVariable's inner points to. Uniqued in SILModule
  /// when calling SILModule::getOrCreateSILDebugVariable().
  struct Storage : public llvm::FoldingSetNode {
    StringRef Name;
    unsigned ArgNo : 16;
    unsigned Constant : 1;
    unsigned Implicit : 1;
    Optional<SILType> Type = None;
    Optional<SILLocation> Loc = None;
    const SILDebugScope *Scope = nullptr;
    SILDebugInfoExpression DIExpr;

    Storage()
        : Name(), ArgNo(0), Constant(false), Implicit(false), Type(), Loc(),
          Scope(nullptr) {}
    Storage(StringRef Name, bool Constant, unsigned ArgNo, bool IsImplicit,
            Optional<SILType> AuxType, Optional<SILLocation> DeclLoc,
            const SILDebugScope *DeclScope,
            llvm::ArrayRef<SILDIExprElement> ExprElements)
        : Name(Name), ArgNo(ArgNo), Constant(Constant), Implicit(IsImplicit),
          Type(AuxType), Loc(DeclLoc), Scope(DeclScope), DIExpr(ExprElements) {}

    // We use the copy constructor only in SILDebugVariable::Builder to copy
    // into a temporay. This is not usable by any caller beyond that.
    Storage(const Storage &other)
        : Name(other.Name), ArgNo(other.ArgNo), Constant(other.Constant),
          Implicit(other.Implicit), Type(other.Type), Loc(other.Loc),
          Scope(other.Scope), DIExpr(other.DIExpr) {}

    void Profile(llvm::FoldingSetNodeID &id);
    static void Profile(llvm::FoldingSetNodeID &id, StringRef name,
                        bool constant, unsigned argNo, bool isImplicit,
                        Optional<SILType> type, Optional<SILLocation> declLoc,
                        const SILDebugScope *declScope,
                        ArrayRef<SILDIExprElement> exprElements);
    LLVM_ATTRIBUTE_DEPRECATED(void dump() LLVM_ATTRIBUTE_USED,
                              "Only for use in the debugger");
  };

  Storage *storage;

  SILDebugVariable(Storage *storage) : storage(storage) { assert(storage); }

public:
  /// A builder struct used to create new SILDebugVariable.
  class Builder;

  // Can be copied/assigned directly. This is a value type.
  SILDebugVariable(const SILDebugVariable &) = default;
  SILDebugVariable &operator=(const SILDebugVariable &) = default;

  /// Global sentinel value for "empty" debug variables.
  ///
  /// Used for supporting DenseMapInfo.
  static SILDebugVariable getEmptySentinel() {
    return SILDebugVariable(reinterpret_cast<Storage *>(UINT_MAX));
  }

  /// Global sentinel value for "tombstone" sentinel values.
  ///
  /// Used for supporting DenseMapInfo.
  static SILDebugVariable getTombstoneSentinel() {
    return SILDebugVariable(reinterpret_cast<Storage *>(UINT_MAX - 1));
  }

  /// Global sentinel value for "tombstone" debug variables.
  static SILDebugVariable TombstoneSentinel;

  bool isTombstoneSentinel() const { return *this == getTombstoneSentinel(); }

  bool isEmptySentinel() const { return *this == getEmptySentinel(); }

  bool isNormal() const { return !isTombstoneSentinel() && !isEmptySentinel(); }

  /// Created from either AllocStack or AllocBox instruction
  static Optional<SILDebugVariable>
  createFromAllocation(const AllocationInst *AI);

  /// Return if \p other is the same SILDebugVariable as this.
  ///
  /// NOTE: Since we unique SILDebugVariable based off of content in our Module,
  /// this is just a pointer comparison.
  bool operator==(const SILDebugVariable other) const {
    return storage == other.storage;
  }

  /// Return the current SILDebugVariable removing DIExpr.
  SILDebugVariable withoutDIExpr(SILModule &mod) const;

  /// Return the current SILDebugVariable changing name to newName.
  SILDebugVariable withName(SILModule &mod, StringRef newName) const;

  /// Return the current SILDebugVariable prepending
  /// SILDIExprOperator::Dereference.
  SILDebugVariable withDereference(SILModule &mod) const;

  /// Returnj the current SILDebugVariable setting the diexpr to be the passed
  /// in expression.
  SILDebugVariable withDebugInfoExpr(SILModule &mod,
                                     const SILDebugInfoExpression &value) const;

  SILDebugVariable removingFirstDIExprElement(SILModule &mod) const;

  bool isLet() const { return storage->Name.size() && storage->Constant; }

  bool isVar() const { return storage->Name.size() && !storage->Constant; }

  // Getters for storage fields.
  StringRef getName() const { return storage->Name; }
  unsigned getArgNo() const { return storage->ArgNo; };
  bool isConstant() const { return storage->Constant; }
  bool isImplicit() const { return storage->Implicit; }
  Optional<SILType> getType() const { return storage->Type; }
  Optional<SILLocation> getLoc() const { return storage->Loc; }
  const SILDebugScope *getScope() const { return storage->Scope; }

  /// TODO: SILDebugInfoExpression is a SmallVector<T, 2> that we are passing
  /// around like a value type!
  SILDebugInfoExpression getDIExpr() const { return storage->DIExpr; }

  const SILDebugInfoExpression &getDIExprRef() const { return storage->DIExpr; }

  bool hasFragment() const { return storage->DIExpr.hasFragment(); }

  static SILDebugVariable get(SILModule &mod) { return get(mod, false, 0); }

  static SILDebugVariable get(SILModule &mod, bool constant, uint16_t argNo) {
    return get(mod, StringRef(), constant, argNo, false, None, None, nullptr,
               {});
  }
  static SILDebugVariable
  get(SILModule &mod, StringRef name, bool constant, unsigned argNo,
      bool isImplicit = false, Optional<SILType> auxType = {},
      Optional<SILLocation> declLoc = {},
      const SILDebugScope *declScope = nullptr,
      llvm::ArrayRef<SILDIExprElement> exprElements = {});

private:
  /// Helper method used to access directly fields in SILDebugVariable.
  SILDebugVariable::Storage *operator->() const {
    assert(isNormal() &&
           "Can not access the storage of a tombstone or empty sentinel value");
    return storage;
  }

  static SILDebugVariable::Storage *
  create(SILModule &mod, StringRef name, bool constant, unsigned argNo,
         bool isImplicit, Optional<SILType> auxType,
         Optional<SILLocation> declLoc, const SILDebugScope *declScope,
         llvm::ArrayRef<SILDIExprElement> exprElements);
};

/// Returns the hashcode for a SILDebugVariable.
inline llvm::hash_code hash_value(const SILDebugVariable &P) {
  // First hash in the state around empty sentinel/tombstone sentinel.
  auto initialHash =
      llvm::hash_combine(P.isEmptySentinel(), P.isTombstoneSentinel());

  // Then if we only have one of these, just return that hash mixed with default
  // values.
  if (!P.isNormal()) {
    return llvm::hash_combine(initialHash, 0, false, StringRef(), false, None,
                              None, (const swift::SILDebugScope *)nullptr,
                              ArrayRef<SILDIExprElement>());
  }

  // Otherwise, mix in the rest of the bits of the type.
  return llvm::hash_combine(initialHash, P->ArgNo, P->Constant, P->Name,
                            P->Implicit, P->Type, P->Loc, P->Scope,
                            P->DIExpr.getElementArray());
}

} // namespace swift

//===----------------------------------------------------------------------===//
//                                  Builder
//===----------------------------------------------------------------------===//

namespace swift {

class SILDebugVariable::Builder {
  /// The module we will create the SILDebugVariable in.
  SILModule &mod;

  /// We just reuse the uniqued storage struct in our builder while we build up
  /// and then we invoke the SILModule constructor with its state. This just
  /// ensures that when we initialize a SILDebugVariable we can use the State
  /// copy constructor and not worry about future changes not updating
  /// everything.
  Storage base;

public:
  Builder(SILModule &mod) : mod(mod), base() {}

  /// Initialize the builder such that it will build a copy of \p baseVariable
  /// if finalized. Useful for producing a new SILDebugVariable by tweaking an
  /// existing debug variable.
  Builder(SILModule &mod, SILDebugVariable baseVariable)
      : mod(mod), base(*baseVariable.storage) {}

  //===---
  // Builder can not be copied constructor or move constructed. We only allow
  // for it to be destroyed by using Builder::finalize.
  Builder(const Builder &) = delete;
  Builder &operator=(const Builder &) = delete;
  Builder(Builder &&) = delete;
  Builder &operator=(Builder &&) = delete;

  /// Generate a new SILDebugVariable based off of the current state of the
  /// builder.
  SILDebugVariable finalize() && {
    return SILDebugVariable::get(mod, base.Name, base.Constant, base.ArgNo,
                                 base.Implicit, base.Type, base.Loc, base.Scope,
                                 base.DIExpr.getElementArray());
  }

  //===---
  // Builder "withX" constructs
  //

  Builder &withName(StringRef newName) {
    base.Name = newName;
    return *this;
  }

  Builder &withArgNo(uint16_t newArgNo) {
    base.ArgNo = newArgNo;
    return *this;
  }

  Builder &withConstant(bool newVal) {
    base.Constant = newVal;
    return *this;
  }

  Builder &withImplicit(bool newVal) {
    base.Implicit = newVal;
    return *this;
  }

  Builder &withType(Optional<SILType> newVal) {
    base.Type = newVal;
    return *this;
  }

  Builder &withLoc(Optional<SILLocation> newVal) {
    base.Loc = newVal;
    return *this;
  }

  Builder &withScope(const SILDebugScope *newVal) {
    base.Scope = newVal;
    return *this;
  }

  Builder &withDIExpr(SILDebugInfoExpression newVal) {
    base.DIExpr = newVal;
    return *this;
  }

  Builder &
  appendingDIExprElements(llvm::ArrayRef<SILDIExprElement> newElements) {
    base.DIExpr.appendElements(newElements);
    return *this;
  }

  Builder &
  prependingDIExprElements(llvm::ArrayRef<SILDIExprElement> newElements) {
    base.DIExpr.prependElements(newElements);
    return *this;
  }

  Builder &removingFirstDIExprElt() {
    base.DIExpr.eraseElement(base.DIExpr.element_begin());
    return *this;
  }
};

} // namespace swift

//===----------------------------------------------------------------------===//
//                Out of line Impls due to Forward Decl Issues
//===----------------------------------------------------------------------===//

namespace swift {

inline SILDebugVariable SILDebugVariable::withoutDIExpr(SILModule &mod) const {
  Builder b(mod, *this);
  b.withDIExpr({});
  return std::move(b).finalize();
}

inline SILDebugVariable SILDebugVariable::withName(SILModule &mod,
                                                   StringRef newName) const {
  Builder b(mod, *this);
  b.withName(newName);
  return std::move(b).finalize();
}

inline SILDebugVariable
SILDebugVariable::withDereference(SILModule &mod) const {
  Builder b(mod, *this);
  b.prependingDIExprElements(
      {SILDIExprElement::createOperator(SILDIExprOperator::Dereference)});
  return std::move(b).finalize();
}

inline SILDebugVariable
SILDebugVariable::withDebugInfoExpr(SILModule &mod,
                                    const SILDebugInfoExpression &value) const {
  Builder b(mod, *this);
  b.withDIExpr(value);
  return std::move(b).finalize();
}

inline SILDebugVariable
SILDebugVariable::removingFirstDIExprElement(SILModule &mod) const {
  Builder b(mod, *this);
  b.removingFirstDIExprElt();
  return std::move(b).finalize();
}

} // namespace swift

//===----------------------------------------------------------------------===//
// Dense Map Info for SILDebugVariable
//===----------------------------------------------------------------------===//

namespace llvm {

template <>
struct DenseMapInfo<swift::SILDebugVariable> {
  using KeyTy = swift::SILDebugVariable;
  static inline KeyTy getEmptyKey() {
    return swift::SILDebugVariable::getEmptySentinel();
  }
  static inline KeyTy getTombstoneKey() {
    return swift::SILDebugVariable::getTombstoneSentinel();
  }
  static unsigned getHashValue(const KeyTy &Val) { return hash_value(Val); }
  static bool isEqual(const KeyTy &LHS, const KeyTy &RHS) { return LHS == RHS; }
};

} // namespace llvm

#endif
