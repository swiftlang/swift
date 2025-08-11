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

#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Compiler.h"
#include "swift/Basic/Debug.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PointerUnion.h"
#include <optional>

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

  /// Construct an AnyFunctionRef from a decl context that might be
  /// some sort of function.
  static std::optional<AnyFunctionRef> fromDeclContext(DeclContext *dc) {
    if (auto fn = dyn_cast<AbstractFunctionDecl>(dc)) {
      return AnyFunctionRef(fn);
    }

    if (auto ace = dyn_cast<AbstractClosureExpr>(dc)) {
      return AnyFunctionRef(ace);
    }

    return std::nullopt;
  }

  CaptureInfo getCaptureInfo() const {
    if (auto *AFD = TheFunction.dyn_cast<AbstractFunctionDecl *>())
      return AFD->getCaptureInfo();
    return cast<AbstractClosureExpr *>(TheFunction)->getCaptureInfo();
  }

  ParameterList *getParameters() const {
    if (auto *AFD = TheFunction.dyn_cast<AbstractFunctionDecl *>())
      return AFD->getParameters();
    return cast<AbstractClosureExpr *>(TheFunction)->getParameters();
  }

  bool hasExternalPropertyWrapperParameters() const {
    return llvm::any_of(*getParameters(), [](const ParamDecl *param) {
      return param->hasExternalPropertyWrapper();
    });
  }

  Type getType() const {
    if (auto *AFD = TheFunction.dyn_cast<AbstractFunctionDecl *>())
      return AFD->getInterfaceType();
    return cast<AbstractClosureExpr *>(TheFunction)->getType();
  }

  Type getBodyResultType() const {
    if (auto *AFD = TheFunction.dyn_cast<AbstractFunctionDecl *>()) {
      if (auto *FD = dyn_cast<FuncDecl>(AFD))
        return FD->mapTypeIntoContext(FD->getResultInterfaceType());
      return TupleType::getEmpty(AFD->getASTContext());
    }
    return cast<AbstractClosureExpr *>(TheFunction)->getResultType();
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
    auto *ACE = cast<AbstractClosureExpr *>(TheFunction);
    if (auto *CE = dyn_cast<ClosureExpr>(ACE))
      return CE->getBody();
    return cast<AutoClosureExpr>(ACE)->getBody();
  }

  void setParsedBody(BraceStmt *stmt) {
    if (auto *AFD = TheFunction.dyn_cast<AbstractFunctionDecl *>()) {
      AFD->setBody(stmt, AbstractFunctionDecl::BodyKind::Parsed);
      return;
    }

    auto *ACE = cast<AbstractClosureExpr *>(TheFunction);
    if (auto *CE = dyn_cast<ClosureExpr>(ACE)) {
      CE->setBody(stmt);
      CE->setBodyState(ClosureExpr::BodyState::Parsed);
      return;
    }

    llvm_unreachable("autoclosures don't have statement bodies");
  }

  void setTypecheckedBody(BraceStmt *stmt) {
    if (auto *AFD = TheFunction.dyn_cast<AbstractFunctionDecl *>()) {
      AFD->setBody(stmt, AbstractFunctionDecl::BodyKind::TypeChecked);
      return;
    }

    auto *ACE = cast<AbstractClosureExpr *>(TheFunction);
    if (auto *CE = dyn_cast<ClosureExpr>(ACE)) {
      CE->setBody(stmt);
      CE->setBodyState(ClosureExpr::BodyState::TypeChecked);
      return;
    }

    llvm_unreachable("autoclosures don't have statement bodies");
  }

  /// Returns a boolean value indicating whether the body, if any, contains
  /// an explicit `return` statement.
  ///
  /// \returns `true` if the body contains an explicit `return` statement,
  /// `false` otherwise.
  bool bodyHasExplicitReturnStmt() const;

  /// Finds occurrences of explicit `return` statements within the body, if any.
  ///
  /// \param results An out container to which the results are added.
  void getExplicitReturnStmts(SmallVectorImpl<ReturnStmt *> &results) const;

  DeclContext *getAsDeclContext() const {
    if (auto *AFD = TheFunction.dyn_cast<AbstractFunctionDecl *>())
      return AFD;
    return cast<AbstractClosureExpr *>(TheFunction);
  }
  
  AbstractFunctionDecl *getAbstractFunctionDecl() const {
    return TheFunction.dyn_cast<AbstractFunctionDecl*>();
  }
  
  AbstractClosureExpr *getAbstractClosureExpr() const {
    return TheFunction.dyn_cast<AbstractClosureExpr*>();
  }

  /// Whether this function is @Sendable.
  bool isSendable() const {
    if (auto *fnType = getType()->getAs<AnyFunctionType>())
      return fnType->isSendable();

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
  
  SourceLoc getLoc(bool SerializedOK = true) const {
    if (auto afd = TheFunction.dyn_cast<AbstractFunctionDecl *>()) {
      return afd->getLoc(SerializedOK);
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
  SWIFT_DEBUG_DUMP {
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

  DeclAttributes getDeclAttributes() const {
    if (auto afd = TheFunction.dyn_cast<AbstractFunctionDecl *>()) {
      return afd->getExpandedAttrs();
    }

    if (auto ace = TheFunction.dyn_cast<AbstractClosureExpr *>()) {
      if (auto *ce = dyn_cast<ClosureExpr>(ace)) {
        return ce->getAttrs();
      }
    }

    return DeclAttributes();
  }

  MacroDecl *getResolvedMacro(CustomAttr *attr) const {
    if (auto afd = TheFunction.dyn_cast<AbstractFunctionDecl *>()) {
      return afd->getResolvedMacro(attr);
    }

    if (auto ace = TheFunction.dyn_cast<AbstractClosureExpr *>()) {
      if (auto *ce = dyn_cast<ClosureExpr>(ace)) {
        return ce->getResolvedMacro(attr);
      }
    }

    return nullptr;
  }

  using MacroCallback = llvm::function_ref<void(CustomAttr *, MacroDecl *)>;

  void
  forEachAttachedMacro(MacroRole role,
                       MacroCallback macroCallback) const {
    auto attrs = getDeclAttributes();
    for (auto customAttrConst : attrs.getAttributes<CustomAttr>()) {
      auto customAttr = const_cast<CustomAttr *>(customAttrConst);
      auto *macroDecl = getResolvedMacro(customAttr);

      if (!macroDecl)
        continue;

      if (!macroDecl->getMacroRoles().contains(role))
        continue;

      macroCallback(customAttr, macroDecl);
    }
  }

  friend bool operator==(AnyFunctionRef lhs, AnyFunctionRef rhs) {
     return lhs.TheFunction == rhs.TheFunction;
   }

   friend bool operator!=(AnyFunctionRef lhs, AnyFunctionRef rhs) {
     return lhs.TheFunction != rhs.TheFunction;
   }

  friend llvm::hash_code hash_value(AnyFunctionRef fn) {
    using llvm::hash_value;
    return hash_value(fn.TheFunction.getOpaqueValue());
  }

  friend SourceLoc extractNearestSourceLoc(AnyFunctionRef fn) {
    return fn.getLoc(/*SerializedOK=*/false);
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
          YieldTypeFlags flags(isYieldingMutableAccessor(AD->getAccessorKind())
                                   ? ParamSpecifier::InOut
                                   : ParamSpecifier::LegacyShared);
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

void simple_display(llvm::raw_ostream &out, AnyFunctionRef fn);

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
