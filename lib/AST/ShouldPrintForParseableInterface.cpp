//=== ShouldPrintForParseableInterface.cpp - Parseable Interface filtering ===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "ShouldPrintForParseableInterface.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeWalker.h"

using namespace swift;

bool swift::isPublicOrUsableFromInline(const ValueDecl *VD) {
  AccessScope scope =
  VD->getFormalAccessScope(/*useDC*/nullptr,
                           /*treatUsableFromInlineAsPublic*/true);
  return scope.isPublic();
}

bool swift::contributesToParentTypeStorage(const AbstractStorageDecl *ASD) {
  auto *DC = ASD->getDeclContext()->getAsDecl();
  if (!DC) return false;
  auto *ND = dyn_cast<NominalTypeDecl>(DC);
  if (!ND) return false;
  return !ND->isResilient() && ASD->hasStorage() && !ASD->isStatic();
}

/// Finds type decls that need to be printed to ensure the layout of public
/// types are correct for non-resilient types.
///
/// For example:
///
/// ```
/// private struct B {}
/// public struct A {
///   internal let b: B
/// }
/// ```
/// Since `A`'s size depends on `B`, we need to print `B` even though it's
/// private. `B`'s children will also be checked for types that contribute to
/// its layout, and so on.
class FindReferencedNonPublicTypeDecls: public ASTWalker, public TypeWalker {
  SmallPtrSetImpl<TypeDecl *> &foundDecls;
  SmallVector<Decl *, 8> workList;

  /// If the provided decl is non-public-or-usableFromInline, add it to the
  /// set of found decls and add it to the worklist.
  void addIfNonPublic(TypeDecl *typeDecl) {
    if (isPublicOrUsableFromInline(typeDecl))
      return;
    if (foundDecls.insert(typeDecl).second)
      workList.push_back(typeDecl);
  }

  /// Walk into the type of this var if it contributes to the parent's layout.
  void visitAbstractStorageDecl(AbstractStorageDecl *decl) {
    if (!contributesToParentTypeStorage(decl))
      return;
    decl->getInterfaceType().walk(*this);
  }

  /// Walk into each associated type.
  void visitEnumElementDecl(EnumElementDecl *decl) {
    auto params = decl->getParameterList();
    if (!params) return;
    for (auto param : *params) {
      param->getInterfaceType().walk(*this);
    }
  }

  /// Walk into inherited types, generic requirements, and extensions.
  void visitNominalTypeDecl(NominalTypeDecl *decl) {
    for (auto inherited : decl->getInherited())
      inherited.getType().walk(*this);
    for (auto extension : decl->getExtensions())
      workList.push_back(extension);
  }

  /// Walk into generic requirements.
  void visitRequirements(const GenericContext *decl) {
    for (auto requirement : decl->getGenericRequirements()) {
      requirement.getFirstType().walk(*this);
      if (requirement.getKind() != RequirementKind::Layout)
        requirement.getSecondType().walk(*this);
    }
  }

  /// Walk into the underlying type.
  void visitTypeAliasDecl(TypeAliasDecl *decl) {
    decl->getUnderlyingTypeLoc().getType().walk(*this);
  }

  /// Walk into the inherited conformances.
  void visitExtensionDecl(ExtensionDecl *decl) {
    for (auto inherited : decl->getInherited())
      inherited.getType().walk(*this);
  }

  /// Walk the provided type, finding all referenced type declarations and
  /// walking them as well.
  Action walkToTypePre(Type type) override {
    if (auto ty = dyn_cast<NominalOrBoundGenericNominalType>(type.getPointer()))
      addIfNonPublic(ty->getDecl());

    if (auto enumType = dyn_cast<EnumType>(type.getPointer()))
      addIfNonPublic(enumType->getDecl());

    if (auto alias = dyn_cast<NameAliasType>(type.getPointer()))
      addIfNonPublic(alias->getDecl());

    return Action::Continue;
  }

  bool walkToDeclPre(Decl *decl) override {
    if (auto genericContext = decl->getAsGenericContext()) {
      visitRequirements(genericContext);
      // Don't return, as we still want to visit these decls.
    }

    // If we see an abstract storage decl that contributes to
    // type layout, walk into its interface type.
    if (auto storage = dyn_cast<AbstractStorageDecl>(decl)) {
      visitAbstractStorageDecl(storage);
      return false;
    }

    // If we see an enum element, walk into its associated values.
    if (auto enumElement = dyn_cast<EnumElementDecl>(decl)) {
      visitEnumElementDecl(enumElement);
      return false;
    }

    // If we see a nominal type decl, walk its inherited types and generic
    // requirements, and continue walking into its children.
    if (auto nominal = dyn_cast<NominalTypeDecl>(decl)) {
      visitNominalTypeDecl(nominal);
      return true;
    }

    // If we see a typealias, walk into the RHS and collect the referenced
    // types.
    if (auto alias = dyn_cast<TypeAliasDecl>(decl)) {
      visitTypeAliasDecl(alias);
      return false;
    }

    // If we see an extension, it's of a type that's newly-exposed. Walk into
    // it.
    if (auto extension = dyn_cast<ExtensionDecl>(decl)) {
      visitExtensionDecl(extension);
      return true;
    }

    // Don't walk into function bodies.
    if (isa<AbstractFunctionDecl>(decl))
      return false;

    return true;
  }
public:
  FindReferencedNonPublicTypeDecls(SmallPtrSetImpl<TypeDecl *> &foundDecls)
    : foundDecls(foundDecls) {}

  /// Searches through the module, walking through all declared types and
  /// finding/exposing the layout-relevant bits.
  void search(ModuleDecl *module) {
    SmallVector<Decl *, 8> topLevelDecls;
    module->getTopLevelDecls(topLevelDecls);
    for (auto decl : topLevelDecls) {
      auto vd = dyn_cast<ValueDecl>(decl);
      if (!vd || !isPublicOrUsableFromInline(vd))
        continue;
      workList.push_back(vd);
    }

    while (!workList.empty()) {
      Decl *d = workList.pop_back_val();
      d->walk(*this);
    }
  }
};

std::shared_ptr<ShouldPrintForParseableInterface>
ShouldPrintForParseableInterface::create(ModuleDecl *module) {
  auto shouldPrint = std::make_shared<ShouldPrintForParseableInterface>();

  // Walk the module to find stored properties whose type is non-public but
  // required to reconstruct the layout of their containing type.
  // We need to keep track of these type declarations, so we make sure to
  // print them.
  auto findTypeDecls = FindReferencedNonPublicTypeDecls(
    shouldPrint->referencedNonPublicTypeDecls);
  findTypeDecls.search(module);
  return shouldPrint;
}

bool ShouldPrintForParseableInterface::shouldPrint(
  const Decl *D, const PrintOptions &options) {
  // Skip anything that isn't 'public' or '@usableFromInline'.
  if (auto *VD = dyn_cast<ValueDecl>(D)) {

    // Always print enum elements if the enum itself is printed.
    if (auto enumElement = dyn_cast<EnumElementDecl>(VD))
      return shouldPrint(enumElement->getParentEnum(), options);

    if (!isPublicOrUsableFromInline(VD)) {
      // We do want to print private stored properties, without their
      // original names present.
      if (auto *ASD = dyn_cast<AbstractStorageDecl>(VD))
        if (contributesToParentTypeStorage(ASD))
          return true;

      // If this is a type that contributes to the layout of any
      // types in this module, then we should print it.
      if (auto typeDecl = dyn_cast<TypeDecl>(VD))
        return referencedNonPublicTypeDecls.count(typeDecl) != 0;

      return false;
    }
  }

  // Skip enum cases, since we're always printing their elements.
  if (isa<EnumCaseDecl>(D))
    return false;

  // Skip extensions that extend things we wouldn't print.
  if (auto *ED = dyn_cast<ExtensionDecl>(D)) {
    if (!shouldPrint(ED->getExtendedNominal(), options))
      return false;
    // FIXME: We also need to check the generic signature for constraints
    // that we can't reference.
  }

  // Skip typealiases that just redeclare generic parameters.
  if (auto *alias = dyn_cast<TypeAliasDecl>(D)) {
    if (alias->isImplicit()) {
      const Decl *parent =
      D->getDeclContext()->getAsDecl();
      if (auto *genericCtx = parent->getAsGenericContext()) {
        bool matchesGenericParam =
        llvm::any_of(genericCtx->getInnermostGenericParamTypes(),
                     [alias](const GenericTypeParamType *param) {
                       return param->getName() == alias->getName();
                     });
        if (matchesGenericParam)
          return false;
      }
    }
  }

  return ShouldPrintChecker::shouldPrint(D, options);
}
