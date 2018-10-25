//=== ShouldPrintForParseableInterface.cpp - Parseable Interface filtering ===//
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

#include "ShouldPrintForParseableInterface.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
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
class FindReferencedNonPublicTypeDecls: public ASTWalker {
  using TypeDecls = SmallPtrSetImpl<TypeDecl *>;
  SmallVector<ValueDecl *, 8> workList;

  /// Finds all nominal types or typealiases referenced in the type being
  /// walked.
  class FindTypeDecls: public TypeWalker {
    TypeDecls &foundDecls;
    SmallVectorImpl<ValueDecl *> &workList;
  public:
    FindTypeDecls(TypeDecls &foundDecls, SmallVectorImpl<ValueDecl *> &workList)
      : foundDecls(foundDecls), workList(workList) {}

    void addIfNonPublic(TypeDecl *typeDecl) {
      if (isPublicOrUsableFromInline(typeDecl))
        return;
      if (foundDecls.insert(typeDecl).second)
        workList.push_back(typeDecl);
    }

    /// Finds all nominal types or typealiases referenced by the
    Action walkToTypePre(Type type) override {
      if (auto nominal =
          dyn_cast<NominalOrBoundGenericNominalType>(type.getPointer()))
        addIfNonPublic(nominal->getDecl());
      if (auto enumType = dyn_cast<EnumType>(type.getPointer()))
        addIfNonPublic(enumType->getDecl());
      if (auto protocol = dyn_cast<ProtocolType>(type.getPointer()))
        addIfNonPublic(protocol->getDecl());
      if (auto alias = dyn_cast<NameAliasType>(type.getPointer())) {
        // Add the typealias to the found decls
        auto decl = alias->getDecl();
        addIfNonPublic(decl);

        // Also walk into the RHS of the typealias to find extra types we need.
        decl->getUnderlyingTypeLoc().getType().walk(*this);
      }
      return Action::Continue;
    }
  };
  FindTypeDecls findTypeDecls;
public:
  FindReferencedNonPublicTypeDecls(TypeDecls &foundDecls):
    findTypeDecls(foundDecls, workList) {}

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

  void visitAbstractStorageDecl(AbstractStorageDecl *decl) {
    if (!contributesToParentTypeStorage(decl))
      return;
    decl->getInterfaceType().walk(findTypeDecls);
  }

  void visitEnumElementDecl(EnumElementDecl *decl) {
    auto params = decl->getParameterList();
    if (!params) return;
    for (auto param : params->getArray()) {
      param->getInterfaceType().walk(findTypeDecls);
    }
  }

  void visitProtocolDecl(ProtocolDecl *decl) {
    for (auto inherited : decl->getInheritedProtocols())
      inherited->getInterfaceType().walk(findTypeDecls);
  }

  bool walkToDeclPre(Decl *decl) {
    if (auto storage = dyn_cast<AbstractStorageDecl>(decl)) {
      visitAbstractStorageDecl(storage);
      return false;
    }

    if (auto enumElement = dyn_cast<EnumElementDecl>(decl)) {
      visitEnumElementDecl(enumElement);
      return false;
    }

    if (auto protocol = dyn_cast<ProtocolDecl>(decl)) {
      visitProtocolDecl(protocol);
      return false;
    }

    // Don't walk into function bodies
    if (isa<AbstractFunctionDecl>(decl))
      return false;

    return true;
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
