//===--- ImportDecl.cpp - Import Clang Declarations -----------------------===//
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
//
// This file implements support for importing Clang declarations into Swift.
//
//===----------------------------------------------------------------------===//

#include "ImporterImpl.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Types.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclVisitor.h"
#include "llvm/ADT/StringExtras.h"

using namespace swift;

namespace {
  /// \brief Convert Clang declarations into the corresponding Swift
  /// declarations.
  class SwiftDeclConverter
    : public clang::DeclVisitor<SwiftDeclConverter, ValueDecl *>
  {
    ClangImporter::Implementation &Impl;

  public:
    explicit SwiftDeclConverter(ClangImporter::Implementation &impl)
      : Impl(impl) { }

    ValueDecl *VisitDecl(clang::Decl *decl) {
      return nullptr;
    }

    ValueDecl *VisitTranslationUnitDecl(clang::TranslationUnitDecl *decl) {
      // Note: translation units are handled specially by importDeclContext.
      return nullptr;
    }

    ValueDecl *VisitNamespaceDecl(clang::NamespaceDecl *decl) {
      // FIXME: Implement once Swift has namespaces.
      return nullptr;
    }

    ValueDecl *VisitUsingDirectiveDecl(clang::UsingDirectiveDecl *decl) {
      // Never imported.
      return nullptr;
    }

    ValueDecl *VisitNamespaceAliasDecl(clang::NamespaceAliasDecl *decl) {
      // FIXME: Implement once Swift has namespaces.
      return nullptr;
    }

    ValueDecl *VisitLabelDecl(clang::LabelDecl *decl) {
      // Labels are function-local, and therefore never imported.
      return nullptr;
    }

    TypeDecl *VisitTypedefNameDecl(clang::TypedefNameDecl *decl){
      auto name = Impl.importName(decl->getDeclName());
      if (name.empty())
        return nullptr;

      auto dc = Impl.importDeclContext(decl->getDeclContext());
      if (!dc)
        return nullptr;

      auto type = Impl.importType(decl->getUnderlyingType());
      if (!type)
        return nullptr;


      return new (Impl.SwiftContext) TypeAliasDecl(
                                      Impl.importSourceLoc(decl->getLocStart()),
                                      name,
                                      Impl.importSourceLoc(decl->getLocation()),
                                      TypeLoc::withoutLoc(type),
                                      dc,
                                      { });
    }

    ValueDecl *VisitFunctionDecl(clang::FunctionDecl *decl) {
      // Import the function type. If we have parameters, make sure their names
      // get into the resulting function type.
      Type type;
      if (decl->param_size())
        type = Impl.importFunctionType(
                 decl->getType()->getAs<clang::FunctionType>()->getResultType(),
                 { decl->param_begin(), decl->param_size() },
                 decl->isVariadic());
      else
        type = Impl.importType(decl->getType());

      if (!type)
        return nullptr;

      auto name = Impl.importName(decl->getDeclName());
      if (name.empty())
        return nullptr;

      // FIXME: Map source locations!
      return new (Impl.SwiftContext) FuncDecl(SourceLoc(),
                                              SourceLoc(),
                                              name,
                                              SourceLoc(),
                                              /*GenericParams=*/0,
                                              type,
                                              /*Body=*/nullptr,
                                              Impl.firstClangModule);
    }
  };
}

ValueDecl *ClangImporter::Implementation::importDecl(clang::NamedDecl *decl) {
  auto known = ImportedDecls.find(decl);
  if (known != ImportedDecls.end())
    return known->second;

  SwiftDeclConverter converter(*this);
  auto result = converter.Visit(decl);
  return ImportedDecls[decl] = result;
}

DeclContext *
ClangImporter::Implementation::importDeclContext(clang::DeclContext *dc) {
  // FIXME: Should map to the module we want to import into (?).
  if (dc->isTranslationUnit())
    return firstClangModule;
  
  auto decl = dyn_cast<clang::NamedDecl>(dc);
  if (!decl)
    return nullptr;

  auto swiftDecl = importDecl(decl);
  if (!swiftDecl)
    return nullptr;

  if (auto nominal = dyn_cast<NominalTypeDecl>(swiftDecl))
    return nominal;
  if (auto extension = dyn_cast<ExtensionDecl>(swiftDecl))
    return extension;
  if (auto constructor = dyn_cast<ConstructorDecl>(swiftDecl))
    return constructor;
  if (auto destructor = dyn_cast<DestructorDecl>(swiftDecl))
    return destructor;
  return nullptr;
}
