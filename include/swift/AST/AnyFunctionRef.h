//===--- AnyFunctionRef.h - A Universal Function Reference ------*- C++ -*-===//
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

#ifndef SWIFT_AST_ANY_FUNCTION_REF_H
#define SWIFT_AST_ANY_FUNCTION_REF_H

#include "swift/Basic/Compiler.h"
#include "swift/Basic/LLVM.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PointerUnion.h"

namespace swift {
class CaptureInfo;

/// A universal function reference -- can wrap all AST nodes that
/// represent functions and exposes a common interface to them.
class AnyFunctionRef {
  PointerUnion<AbstractFunctionDecl *, AbstractClosureExpr *> TheFunction;

  friend struct llvm::DenseMapInfo<AnyFunctionRef>;
  
  AnyFunctionRef(decltype(TheFunction) TheFunction)
    : TheFunction(TheFunction) {}

public:
  AnyFunctionRef(AbstractFunctionDecl *AFD) : TheFunction(AFD) {
    assert(AFD && "should have a function");
  }
  AnyFunctionRef(AbstractClosureExpr *ACE) : TheFunction(ACE) {
    assert(ACE && "should have a closure");
  }

  /// Construct an AnyFunctionRef from a decl context that's known to
  /// be some sort of function.
  static AnyFunctionRef fromFunctionDeclContext(DeclContext *dc) {
    if (auto fn = dyn_cast<AbstractFunctionDecl>(dc)) {
      return fn;
    } else {
      return cast<AbstractClosureExpr>(dc);
    }
  }

  const CaptureInfo &getCaptureInfo() const {
    if (auto *AFD = TheFunction.dyn_cast<AbstractFunctionDecl *>())
      return AFD->getCaptureInfo();
    return TheFunction.get<AbstractClosureExpr *>()->getCaptureInfo();
  }

  void setCaptureInfo(const CaptureInfo &captures) const {
    if (auto *AFD = TheFunction.dyn_cast<AbstractFunctionDecl *>()) {
      AFD->setCaptureInfo(captures);
      return;
    }
    TheFunction.get<AbstractClosureExpr *>()->setCaptureInfo(captures);
  }

  void getLocalCaptures(SmallVectorImpl<CapturedValue> &Result) const {
    getCaptureInfo().getLocalCaptures(Result);
  }

  bool hasType() const {
    if (auto *AFD = TheFunction.dyn_cast<AbstractFunctionDecl *>())
      return AFD->hasInterfaceType();
    return !TheFunction.get<AbstractClosureExpr *>()->getType().isNull();
  }

  bool hasSingleExpressionBody() const {
    if (auto *AFD = TheFunction.dyn_cast<AbstractFunctionDecl *>())
      return AFD->hasSingleExpressionBody();
    return TheFunction.get<AbstractClosureExpr *>()->hasSingleExpressionBody();
  }

  Expr *getSingleExpressionBody() const {
    if (auto *AFD = TheFunction.dyn_cast<AbstractFunctionDecl *>())
      return AFD->getSingleExpressionBody();
    return TheFunction.get<AbstractClosureExpr *>()->getSingleExpressionBody();
  }

  Type getType() const {
    if (auto *AFD = TheFunction.dyn_cast<AbstractFunctionDecl *>())
      return AFD->getInterfaceType();
    return TheFunction.get<AbstractClosureExpr *>()->getType();
  }

  Type getBodyResultType() const {
    if (auto *AFD = TheFunction.dyn_cast<AbstractFunctionDecl *>()) {
      if (auto *FD = dyn_cast<FuncDecl>(AFD))
        return FD->mapTypeIntoContext(FD->getResultInterfaceType());
      return TupleType::getEmpty(AFD->getASTContext());
    }
    return TheFunction.get<AbstractClosureExpr *>()->getResultType();
  }

  ArrayRef<AnyFunctionType::Yield>
  getYieldResults(SmallVectorImpl<AnyFunctionType::Yield> &buffer) const {
    return getYieldResultsImpl(buffer, /*mapIntoContext*/ false);
  }

  ArrayRef<AnyFunctionType::Yield>
  getBodyYieldResults(SmallVectorImpl<AnyFunctionType::Yield> &buffer) const {
    return getYieldResultsImpl(buffer, /*mapIntoContext*/ true);
  }

  BraceStmt *getBody() const {
    if (auto *AFD = TheFunction.dyn_cast<AbstractFunctionDecl *>())
      return AFD->getBody();
    auto *ACE = TheFunction.get<AbstractClosureExpr *>();
    if (auto *CE = dyn_cast<ClosureExpr>(ACE))
      return CE->getBody();
    return cast<AutoClosureExpr>(ACE)->getBody();
  }

  DeclContext *getAsDeclContext() const {
    if (auto *AFD = TheFunction.dyn_cast<AbstractFunctionDecl *>())
      return AFD;
    return TheFunction.get<AbstractClosureExpr *>();
  }
  
  AbstractFunctionDecl *getAbstractFunctionDecl() const {
    return TheFunction.dyn_cast<AbstractFunctionDecl*>();
  }
  
  AbstractClosureExpr *getAbstractClosureExpr() const {
    return TheFunction.dyn_cast<AbstractClosureExpr*>();
  }

  bool isDeferBody() const {
    if (auto *fd = dyn_cast_or_null<FuncDecl>(getAbstractFunctionDecl()))
      return fd->isDeferBody();
    return false;
  }

  /// Return true if this closure is passed as an argument to a function and is
  /// known not to escape from that function.  In this case, captures can be
  /// more efficient.
  bool isKnownNoEscape() const {
    if (hasType() && !getType()->hasError())
      return getType()->castTo<AnyFunctionType>()->isNoEscape();
    return false;
  }

  bool isObjC() const {
    if (auto afd = TheFunction.dyn_cast<AbstractFunctionDecl *>()) {
      return afd->isObjC();
    }
    if (TheFunction.dyn_cast<AbstractClosureExpr *>()) {
      // Closures are never @objc.
      return false;
    }
    llvm_unreachable("unexpected AnyFunctionRef representation");
  }
  
  SourceLoc getLoc() const {
    if (auto afd = TheFunction.dyn_cast<AbstractFunctionDecl *>()) {
      return afd->getLoc();
    }
    if (auto ce = TheFunction.dyn_cast<AbstractClosureExpr *>()) {
      return ce->getLoc();
    }
    llvm_unreachable("unexpected AnyFunctionRef representation");
  }

// Disable "only for use within the debugger" warning.
#if SWIFT_COMPILER_IS_MSVC
#pragma warning(push)
#pragma warning(disable: 4996)
#endif
  LLVM_ATTRIBUTE_DEPRECATED(void dump() const LLVM_ATTRIBUTE_USED,
                            "only for use within the debugger") {
    if (auto afd = TheFunction.dyn_cast<AbstractFunctionDecl *>()) {
      return afd->dump();
    }
    if (auto ce = TheFunction.dyn_cast<AbstractClosureExpr *>()) {
      return ce->dump();
    }
    llvm_unreachable("unexpected AnyFunctionRef representation");
  }
  
  GenericEnvironment *getGenericEnvironment() const {
    if (auto afd = TheFunction.dyn_cast<AbstractFunctionDecl *>()) {
      return afd->getGenericEnvironment();
    }
    if (auto ce = TheFunction.dyn_cast<AbstractClosureExpr *>()) {
      return ce->getGenericEnvironmentOfContext();
    }
    llvm_unreachable("unexpected AnyFunctionRef representation");
  }

  GenericSignature getGenericSignature() const {
    if (auto afd = TheFunction.dyn_cast<AbstractFunctionDecl *>()) {
      return afd->getGenericSignature();
    }
    if (auto ce = TheFunction.dyn_cast<AbstractClosureExpr *>()) {
      return ce->getGenericSignatureOfContext();
    }
    llvm_unreachable("unexpected AnyFunctionRef representation");
  }

private:
  ArrayRef<AnyFunctionType::Yield>
  getYieldResultsImpl(SmallVectorImpl<AnyFunctionType::Yield> &buffer,
                      bool mapIntoContext) const {
    assert(buffer.empty());
    if (auto *AFD = TheFunction.dyn_cast<AbstractFunctionDecl *>()) {
      if (auto *AD = dyn_cast<AccessorDecl>(AFD)) {
        if (AD->isCoroutine()) {
          auto valueTy = AD->getStorage()->getValueInterfaceType()
                                         ->getReferenceStorageReferent();
          if (mapIntoContext)
            valueTy = AD->mapTypeIntoContext(valueTy);
          YieldTypeFlags flags(AD->getAccessorKind() == AccessorKind::Modify
                                 ? ValueOwnership::InOut
                                 : ValueOwnership::Shared);
          buffer.push_back(AnyFunctionType::Yield(valueTy, flags));
          return buffer;
        }
      }
    }
    return {};
  }
};
#if SWIFT_COMPILER_IS_MSVC
#pragma warning(pop)
#endif

} // namespace swift

namespace llvm {

template<>
struct DenseMapInfo<swift::AnyFunctionRef> {
  using PointerUnion = decltype(swift::AnyFunctionRef::TheFunction);
  using PointerUnionTraits = DenseMapInfo<PointerUnion>;
  using AnyFunctionRef = swift::AnyFunctionRef;

  static inline AnyFunctionRef getEmptyKey() {
    return AnyFunctionRef(PointerUnionTraits::getEmptyKey());
  }
  static inline AnyFunctionRef getTombstoneKey() {
    return AnyFunctionRef(PointerUnionTraits::getTombstoneKey());
  }
  static inline unsigned getHashValue(AnyFunctionRef ref) {
    return PointerUnionTraits::getHashValue(ref.TheFunction);
  }
  static bool isEqual(AnyFunctionRef a, AnyFunctionRef b) {
    return a.TheFunction == b.TheFunction;
  }
};

}

#endif // LLVM_SWIFT_AST_ANY_FUNCTION_REF_H

