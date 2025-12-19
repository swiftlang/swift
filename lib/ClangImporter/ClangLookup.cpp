//===--- ClangLookup.cpp - Lookup in entities imported from Clang ---------===//
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
//
// This file contains facilities for name lookup in entites imported from Clang
//
//===----------------------------------------------------------------------===//

#include "ImporterImpl.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Builtins.h"
#include "swift/AST/ClangModuleLoader.h"
#include "swift/AST/ConcreteDeclRef.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Evaluator.h"
#include "swift/AST/ImportCache.h"
#include "swift/AST/Module.h"
#include "swift/AST/ModuleNameLookup.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/Types.h"
#include "swift/Basic/LLVM.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/ClangImporter/ClangImporterRequests.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Subsystems.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclBase.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/Mangle.h"
#include "clang/AST/Type.h"
#include "clang/Index/IndexingAction.h"
#include "clang/Serialization/ASTReader.h"
#include "clang/Serialization/ASTWriter.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include <utility>

using namespace swift;

namespace {
/// Collects name lookup results into the given tiny vector, for use in the
/// various ClangImporter lookup routines.
class CollectLookupResults {
  DeclName name;
  TinyPtrVector<ValueDecl *> &result;

public:
  CollectLookupResults(DeclName name, TinyPtrVector<ValueDecl *> &result)
      : name(name), result(result) {}

  void add(ValueDecl *imported) {
    result.push_back(imported);

    // Expand any macros introduced by the Clang importer.
    imported->visitAuxiliaryDecls([&](Decl *decl) {
      auto valueDecl = dyn_cast<ValueDecl>(decl);
      if (!valueDecl)
        return;

      // Bail out if the auxiliary decl was not produced by a macro.
      auto module = decl->getDeclContext()->getParentModule();
      auto *sf = module->getSourceFileContainingLocation(decl->getLoc());
      if (!sf || sf->Kind != SourceFileKind::MacroExpansion)
        return;

      // Only produce results that match the requested name.
      if (!valueDecl->getName().matchesRef(name))
        return;

      result.push_back(valueDecl);
    });
  }
};
} // anonymous namespace

static SmallVector<SwiftLookupTable::SingleEntry, 4>
lookupInClassTemplateSpecialization(
    ASTContext &ctx, const clang::ClassTemplateSpecializationDecl *clangDecl,
    DeclName name) {
  // TODO: we could make this faster if we can cache class templates in the
  // lookup table as well.
  // Import all the names to figure out which ones we're looking for.
  SmallVector<SwiftLookupTable::SingleEntry, 4> found;
  for (auto member : clangDecl->decls()) {
    auto *namedDecl = dyn_cast<clang::NamedDecl>(member);
    if (!namedDecl)
      continue;

    auto memberName = ctx.getClangModuleLoader()->importName(namedDecl);
    if (!memberName)
      continue;

    // Use the base names here because *sometimes* our input name won't have
    // any arguments.
    if (name.getBaseName().compare(memberName.getBaseName()) == 0)
      found.push_back(namedDecl);
  }

  return found;
}

static bool isDirectLookupMemberContext(const clang::Decl *foundClangDecl,
                                        const clang::Decl *memberContext,
                                        const clang::Decl *parent) {
  if (memberContext->getCanonicalDecl() == parent->getCanonicalDecl())
    return true;
  if (auto *namespaceDecl = dyn_cast<clang::NamespaceDecl>(memberContext)) {
    if (namespaceDecl->isInline()) {
      if (auto *memberCtxParent =
              dyn_cast<clang::Decl>(namespaceDecl->getParent()))
        return isDirectLookupMemberContext(foundClangDecl, memberCtxParent,
                                           parent);
    }
  }
  // Enum constant decl can be found in the parent context of the enum decl.
  if (auto *ED = dyn_cast<clang::EnumDecl>(memberContext)) {
    if (isa<clang::EnumConstantDecl>(foundClangDecl)) {
      if (auto *firstDecl = dyn_cast<clang::Decl>(ED->getDeclContext()))
        return firstDecl->getCanonicalDecl() == parent->getCanonicalDecl();
    }
  }
  return false;
}

SmallVector<SwiftLookupTable::SingleEntry, 4>
ClangDirectLookupRequest::evaluate(Evaluator &evaluator,
                                   ClangDirectLookupDescriptor desc) const {
  auto &ctx = desc.decl->getASTContext();
  auto *clangDecl = desc.clangDecl;
  // Class templates aren't in the lookup table.
  if (auto *spec = dyn_cast<clang::ClassTemplateSpecializationDecl>(clangDecl))
    return lookupInClassTemplateSpecialization(ctx, spec, desc.name);

  SwiftLookupTable *lookupTable;
  if (isa<clang::NamespaceDecl>(clangDecl)) {
    // DeclContext of a namespace imported into Swift is the __ObjC module.
    lookupTable = ctx.getClangModuleLoader()->findLookupTable(nullptr);
  } else {
    auto *clangModule =
        importer::getClangOwningModule(clangDecl, clangDecl->getASTContext());
    lookupTable = ctx.getClangModuleLoader()->findLookupTable(clangModule);
  }

  auto foundDecls = lookupTable->lookup(
      SerializedSwiftName(desc.name.getBaseName()), EffectiveClangContext());
  // Make sure that `clangDecl` is the parent of all the members we found.
  SmallVector<SwiftLookupTable::SingleEntry, 4> filteredDecls;
  llvm::copy_if(foundDecls, std::back_inserter(filteredDecls),
                [clangDecl](SwiftLookupTable::SingleEntry decl) {
                  auto *foundClangDecl = decl.dyn_cast<clang::NamedDecl *>();
                  if (!foundClangDecl)
                    return false;
                  auto *first = foundClangDecl->getDeclContext();
                  auto *second = cast<clang::DeclContext>(clangDecl);
                  if (auto *firstDecl = dyn_cast<clang::Decl>(first)) {
                    if (auto *secondDecl = dyn_cast<clang::Decl>(second)) {
                      return isDirectLookupMemberContext(foundClangDecl,
                                                         firstDecl, secondDecl);
                    }
                    return false;
                  }
                  return first == second;
                });
  return filteredDecls;
}

TinyPtrVector<ValueDecl *> CXXNamespaceMemberLookup::evaluate(
    Evaluator &evaluator, CXXNamespaceMemberLookupDescriptor desc) const {
  EnumDecl *namespaceDecl = desc.namespaceDecl;
  DeclName name = desc.name;
  auto *clangNamespaceDecl =
      cast<clang::NamespaceDecl>(namespaceDecl->getClangDecl());
  auto &ctx = namespaceDecl->getASTContext();

  TinyPtrVector<ValueDecl *> result;
  CollectLookupResults collector(name, result);

  llvm::SmallPtrSet<clang::NamedDecl *, 8> importedDecls;
  for (auto redecl : clangNamespaceDecl->redecls()) {
    auto allResults = evaluateOrDefault(
        ctx.evaluator, ClangDirectLookupRequest({namespaceDecl, redecl, name}),
        {});

    for (auto found : allResults) {
      auto clangMember = cast<clang::NamedDecl *>(found);
      auto it = importedDecls.insert(clangMember);
      // Skip over members already found during lookup in prior redeclarations.
      if (!it.second)
        continue;
      if (auto import =
              ctx.getClangModuleLoader()->importDeclDirectly(clangMember))
        collector.add(cast<ValueDecl>(import));
    }
  }

  return result;
}
