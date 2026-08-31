//===--- TypeCheckDeclInterface.h - Decl Interface Type Walk -------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines DeclInterfaceTypeChecker, a CRTP base class that walks
// every type that appears in a declaration's interface (parameter types,
// generic requirements, inherited types, pattern types, etc).
//
// `Derived` must provide:
//   void checkType(Type type, const TypeRepr *typeRepr, const Decl *context);
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_TYPE_CHECK_DECL_INTERFACE_H
#define SWIFT_SEMA_TYPE_CHECK_DECL_INTERFACE_H

#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DeclContext.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/Range.h"
#include "llvm/ADT/DenseSet.h"

namespace swift {

template <typename Derived>
class DeclInterfaceTypeChecker : public DeclVisitor<Derived> {
  Derived &asDerived() { return *static_cast<Derived *>(this); }

protected:
  /// Whether the trailing where clause of a generic context should be
  /// skipped entirely.
  bool shouldSkipGenericRequirements(const GenericContext *ownerCtx) {
    return false;
  }

  void checkGenericParams(const GenericContext *ownerCtx,
                          const ValueDecl *ownerDecl) {
    if (!ownerCtx->isGenericContext())
      return;

    if (auto params = ownerCtx->getGenericParams()) {
      for (auto param : *params) {
        auto inheritedEntries = param->getInherited().getEntries();
        if (inheritedEntries.empty())
          continue;
        assert(inheritedEntries.size() == 1);
        auto inherited = inheritedEntries.front();
        asDerived().checkGenericRequirementType(
            inherited.getType(), inherited.getTypeRepr(), ownerDecl);
      }
    }

    if (ownerCtx->getTrailingWhereClause()) {
      if (asDerived().shouldSkipGenericRequirements(ownerCtx))
        return;

      WhereClauseOwner(const_cast<GenericContext *>(ownerCtx))
          .forAllRequirementTypes([&](Type type, TypeRepr *typeRepr) {
            asDerived().checkGenericRequirementType(type, typeRepr, ownerDecl);
          });
    }
  }

  /// \see visitPatternBindingDecl
  void checkNamedPattern(const NamedPattern *NP,
                         const llvm::DenseSet<const VarDecl *> &seenVars) {
    const VarDecl *theVar = NP->getDecl();

    // Only check the type of individual variables if we didn't check an
    // enclosing TypedPattern.
    if (seenVars.count(theVar))
      return;

    asDerived().checkPatternVarType(theVar->getValueInterfaceType(),
                                    /*typeRepr*/ nullptr, theVar, theVar);

    for (auto attr : theVar->getAttachedPropertyWrappers()) {
      asDerived().checkStoredPropertyWrapperType(attr->getType(),
                                                 attr->getTypeRepr(), theVar);
    }
  }

  /// \see visitPatternBindingDecl
  void checkTypedPattern(PatternBindingDecl *PBD, const TypedPattern *TP,
                         llvm::DenseSet<const VarDecl *> &seenVars) {
    // FIXME: We need to figure out if this is a stored or computed property,
    // so we pull out some random VarDecl in the pattern. They're all going to
    // be the same, but still, ick.
    const VarDecl *anyVar = nullptr;
    TP->forEachVariable([&](VarDecl *V) {
      seenVars.insert(V);
      anyVar = V;
    });

    asDerived().checkPatternVarType(
        TP->hasType() ? TP->getType() : Type(), TP->getTypeRepr(),
        anyVar ? (Decl *)anyVar : (Decl *)PBD, anyVar);

    // Check the property wrapper types.
    if (anyVar) {
      for (auto attr : anyVar->getAttachedPropertyWrappers()) {
        asDerived().checkStoredPropertyWrapperType(attr->getType(),
                                                   attr->getTypeRepr(), anyVar);
      }

      if (auto attr = anyVar->getAttachedResultBuilder()) {
        asDerived().checkResultBuilderType(anyVar->getResultBuilderType(),
                                           attr->getTypeRepr(), anyVar);
      }
    }
  }

public:
  void visit(Decl *D) {
    DeclVisitor<Derived>::visit(D);
    asDerived().checkGlobalActor(D);
    asDerived().checkAdditional(D);
  }

  // Force all kinds to be handled at a lower level.
  void visitDecl(Decl *D) = delete;
  void visitValueDecl(ValueDecl *D) = delete;

#define UNREACHABLE(KIND, REASON)                                              \
  void visit##KIND##Decl(KIND##Decl *D) { llvm_unreachable(REASON); }
  UNREACHABLE(Import, "not applicable")
  UNREACHABLE(TopLevelCode, "not applicable")
  UNREACHABLE(Module, "not applicable")
  UNREACHABLE(Missing, "not applicable")
  UNREACHABLE(Using, "not applicable")

  UNREACHABLE(Param, "handled by the enclosing declaration")
  UNREACHABLE(GenericTypeParam, "handled by the enclosing declaration")
  UNREACHABLE(MissingMember, "handled by the enclosing declaration")
  UNREACHABLE(MacroExpansion, "handled by the enclosing declaration")
#undef UNREACHABLE

#define UNINTERESTING(KIND)                                                    \
  void visit##KIND##Decl(KIND##Decl *D) {}

  UNINTERESTING(PrefixOperator)
  UNINTERESTING(PostfixOperator)
  UNINTERESTING(InfixOperator)
  UNINTERESTING(EnumCase)
  UNINTERESTING(Destructor)
  UNINTERESTING(Accessor) // Handled by the Var or Subscript.
  UNINTERESTING(OpaqueType)
  UNINTERESTING(PrecedenceGroup)

  // Handled at the PatternBinding level; if the pattern has a simple
  // "name: TheType" form, we can get better results by diagnosing the
  // TypeRepr.
  UNINTERESTING(Var)
#undef UNINTERESTING

  void visitPatternBindingDecl(PatternBindingDecl *PBD) {
    llvm::DenseSet<const VarDecl *> seenVars;
    for (auto idx : range(PBD->getNumPatternEntries())) {
      PBD->getPattern(idx)->forEachNode([&](const Pattern *P) {
        if (auto *NP = dyn_cast<NamedPattern>(P)) {
          checkNamedPattern(NP, seenVars);
          return;
        }

        auto *TP = dyn_cast<TypedPattern>(P);
        if (!TP)
          return;
        checkTypedPattern(PBD, TP, seenVars);
      });
      seenVars.clear();
    }
  }

  void visitTypeAliasDecl(TypeAliasDecl *TAD) {
    checkGenericParams(TAD, TAD);
    asDerived().checkType(TAD->getUnderlyingType(),
                          TAD->getUnderlyingTypeRepr(), TAD);
  }

  void visitAssociatedTypeDecl(AssociatedTypeDecl *assocType) {
    for (TypeLoc requirement : assocType->getInherited().getEntries()) {
      asDerived().checkType(requirement.getType(), requirement.getTypeRepr(),
                            assocType);
    }
    asDerived().checkType(assocType->getDefaultDefinitionType(),
                          assocType->getDefaultDefinitionTypeRepr(), assocType);

    if (assocType->getTrailingWhereClause()) {
      WhereClauseOwner(assocType).forAllRequirementTypes(
          [&](Type type, TypeRepr *typeRepr) {
            asDerived().checkGenericRequirementType(type, typeRepr, assocType);
          });
    }
  }

  void visitNominalTypeDecl(const NominalTypeDecl *nominal) {
    checkGenericParams(nominal, nominal);

    for (TypeLoc inherited : nominal->getInherited().getEntries())
      asDerived().checkNominalInheritedType(nominal, inherited);
  }

  void visitProtocolDecl(ProtocolDecl *proto) {
    for (TypeLoc requirement : proto->getInherited().getEntries())
      asDerived().checkProtocolInheritedType(proto, requirement);

    if (proto->getTrailingWhereClause()) {
      WhereClauseOwner(proto).forAllRequirementTypes(
          [&](Type type, TypeRepr *typeRepr) {
            asDerived().checkGenericRequirementType(type, typeRepr, proto);
          });
    }
  }

  void visitSubscriptDecl(SubscriptDecl *SD) {
    checkGenericParams(SD, SD);

    for (auto &P : *SD->getIndices()) {
      asDerived().checkType(P->getInterfaceType(), P->getTypeRepr(), SD);
    }
    asDerived().checkType(SD->getElementInterfaceType(),
                          SD->getElementTypeRepr(), SD);
  }

  void visitAbstractFunctionDecl(AbstractFunctionDecl *fn) {
    checkGenericParams(fn, fn);

    for (auto *P : *fn->getParameters()) {
      auto wrapperAttrs = P->getAttachedPropertyWrappers();
      for (auto index : indices(wrapperAttrs)) {
        auto wrapperType = P->getAttachedPropertyWrapperType(index);
        asDerived().checkType(wrapperType, wrapperAttrs[index]->getTypeRepr(),
                              fn);
      }

      if (auto attr = P->getAttachedResultBuilder())
        asDerived().checkType(P->getResultBuilderType(), attr->getTypeRepr(),
                              fn);

      asDerived().checkType(P->getInterfaceType(), P->getTypeRepr(), fn);
    }

    if (auto thrownTypeRepr = fn->getThrownTypeRepr()) {
      asDerived().checkType(fn->getThrownInterfaceType(), thrownTypeRepr, fn);
    }
  }

  void visitFuncDecl(FuncDecl *FD) {
    asDerived().visitAbstractFunctionDecl(FD);
    asDerived().checkType(FD->getResultInterfaceType(), FD->getResultTypeRepr(),
                          FD);

    if (auto attr = FD->getAttachedResultBuilder()) {
      asDerived().checkResultBuilderType(FD->getResultBuilderType(),
                                         attr->getTypeRepr(), FD);
    }
  }

  void visitEnumElementDecl(EnumElementDecl *EED) {
    if (!EED->hasAssociatedValues())
      return;

    for (auto &P : *EED->getParameterList())
      asDerived().checkEnumElementAssociatedValueType(P->getInterfaceType(),
                                                      P->getTypeRepr(), EED);
  }

  void visitMacroDecl(MacroDecl *MD) {
    checkGenericParams(MD, MD);

    if (MD->parameterList) {
      for (auto P : *MD->parameterList) {
        asDerived().checkType(P->getInterfaceType(), P->getTypeRepr(), MD);
      }
    }
    asDerived().checkType(MD->getResultInterfaceType(),
                          MD->resultType.getTypeRepr(), MD);
  }

  void checkGlobalActor(Decl *D) {
    auto globalActor = D->getGlobalActorAttr();
    if (!globalActor)
      return;

    // Skip `@MainActor` since it doesn't have any generic parameters / carry
    // an inherent ABI impact.
    if (globalActor->second->isMainActor())
      return;

    auto customAttr = globalActor->first;
    asDerived().checkType(customAttr->getType(), customAttr->getTypeRepr(), D);
  }

  /// Called at the end of `visit(Decl *)` to allow derived types to perform
  /// any extra checking they require.
  void checkAdditional(Decl *D) {}

  /// A type appearing in a generic parameter's inheritance clause or a
  /// generic context's trailing where clause.
  void checkGenericRequirementType(Type type, const TypeRepr *typeRepr,
                                   const Decl *context) {
    asDerived().checkType(type, typeRepr, context);
  }

  /// The type of a variable introduced by a pattern (a `NamedPattern` or the
  /// var(s) bound by a `TypedPattern`). \p reasonSource is the specific
  /// `VarDecl` to consult when deciding how to treat the check (it may be
  /// null when a `TypedPattern` binds no variables).
  void checkPatternVarType(Type type, const TypeRepr *typeRepr,
                           const Decl *context, const VarDecl *reasonSource) {
    asDerived().checkType(type, typeRepr, context);
  }

  /// A property wrapper type attached to a stored property.
  void checkStoredPropertyWrapperType(Type type, const TypeRepr *typeRepr,
                                      const Decl *context) {
    asDerived().checkType(type, typeRepr, context);
  }

  /// A result builder type attached to a stored property or a function.
  void checkResultBuilderType(Type type, const TypeRepr *typeRepr,
                              const Decl *context) {
    asDerived().checkType(type, typeRepr, context);
  }

  /// The type of an enum element's associated value.
  void checkEnumElementAssociatedValueType(Type type, const TypeRepr *typeRepr,
                                           const Decl *context) {
    asDerived().checkType(type, typeRepr, context);
  }

  void checkNominalInheritedType(const NominalTypeDecl *nominal,
                                 TypeLoc inherited) {
    asDerived().checkType(inherited.getType(), inherited.getTypeRepr(),
                          nominal);
  }

  void checkProtocolInheritedType(ProtocolDecl *proto, TypeLoc inherited) {
    asDerived().checkType(inherited.getType(), inherited.getTypeRepr(), proto);
  }
};

} // end namespace swift

#endif
