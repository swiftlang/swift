//===--- AnyFunctionRef.h - A Universal Function Reference ------*- C++ -*-===//
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

#ifndef SWIFT_AST_ANY_FUNCTION_REF_H
#define SWIFT_AST_ANY_FUNCTION_REF_H

#include "swift/Basic/LLVM.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PointerUnion.h"

namespace swift {
class CaptureInfo;

/// \brief A universal function reference -- can wrap all AST nodes that
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

  CaptureInfo &getCaptureInfo() const {
    if (auto *AFD = TheFunction.dyn_cast<AbstractFunctionDecl *>())
      return AFD->getCaptureInfo();
    return TheFunction.get<AbstractClosureExpr *>()->getCaptureInfo();
  }

  void getLocalCaptures(SmallVectorImpl<CaptureInfo::
                        LocalCaptureTy> &Result) const {
    auto FD = dyn_cast_or_null<FuncDecl>(
                                TheFunction.dyn_cast<AbstractFunctionDecl *>());
    getCaptureInfo().getLocalCaptures(FD, Result);
  }

  ArrayRef<Pattern *> getBodyParamPatterns() const {
    if (auto *AFD = TheFunction.dyn_cast<AbstractFunctionDecl *>())
      return AFD->getBodyParamPatterns();
    return TheFunction.get<AbstractClosureExpr *>()->getParamPatterns();
  }

  Type getType() const {
    if (auto *AFD = TheFunction.dyn_cast<AbstractFunctionDecl *>())
      return AFD->getType();
    return TheFunction.get<AbstractClosureExpr *>()->getType();
  }
  
  /// FIXME: This should just be getType() when interface types take over in
  /// the AST.
  Type getInterfaceType() const {
    if (auto *AFD = TheFunction.dyn_cast<AbstractFunctionDecl *>())
      return AFD->getInterfaceType();
    return TheFunction.get<AbstractClosureExpr *>()->getType();
  }

  Type getBodyResultType() const {
    if (auto *AFD = TheFunction.dyn_cast<AbstractFunctionDecl *>()) {
      if (auto *FD = dyn_cast<FuncDecl>(AFD))
        return FD->getBodyResultType();
      return TupleType::getEmpty(AFD->getASTContext());
    }
    return TheFunction.get<AbstractClosureExpr *>()->getResultType();
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

  /// Return true if this closure is passed as an argument to a function and is
  /// known not to escape from that function.  In this case, captures can be
  /// more efficient.
  bool isKnownNoEscape() const {
    if (TheFunction.is<AbstractFunctionDecl *>())
      return false;

    auto *CE = TheFunction.get<AbstractClosureExpr *>();
    if (!CE->getType() || CE->getType()->is<ErrorType>())
      return false;
    return CE->getType()->castTo<FunctionType>()->isNoEscape();
  }

};

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

