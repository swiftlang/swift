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

    ValueDecl *VisitTypedefNameDecl(clang::TypedefNameDecl *decl){
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

    ValueDecl *
    VisitUnresolvedUsingTypenameDecl(clang::UnresolvedUsingTypenameDecl *decl) {
      // Note: only occurs in templates.
      return nullptr;
    }

    ValueDecl *VisitEnumDecl(clang::EnumDecl *decl) {
      decl = decl->getDefinition();
      if (!decl)
        return nullptr;
      
      Identifier name;
      if (decl->getDeclName())
        name = Impl.importName(decl->getDeclName());
      else if (decl->getTypedefNameForAnonDecl())
        name =Impl.importName(decl->getTypedefNameForAnonDecl()->getDeclName());

      if (name.empty())
        return nullptr;

      auto dc = Impl.importDeclContext(decl->getDeclContext());
      if (!dc)
        return nullptr;

      // Create the oneof declaration and record it.
      auto result = new (Impl.SwiftContext)
                      OneOfDecl(Impl.importSourceLoc(decl->getLocStart()),
                                name,
                                Impl.importSourceLoc(decl->getLocation()),
                                { }, nullptr, dc);
      Impl.ImportedDecls[decl->getCanonicalDecl()] = result;

      // Import each of the enumerators.
      SmallVector<Decl *, 4> members;
      for (auto ec = decl->enumerator_begin(), ecEnd = decl->enumerator_end();
           ec != ecEnd; ++ec) {
        auto ood = Impl.importDecl(*ec);
        if (!ood)
          continue;

        members.push_back(ood);
      }

      // FIXME: Source range isn't totally accurate because Clang lacks the
      // location of the '{'.
      result->setMembers(Impl.SwiftContext.AllocateCopy(members),
                         Impl.importSourceRange(clang::SourceRange(
                                                  decl->getLocation(),
                                                  decl->getRBraceLoc())));

      return result;
    }

    ValueDecl *VisitRecordDecl(clang::RecordDecl *decl) {
      // FIXME: Map structs and classes to structs.
      // FIXME: Unions will have to be dropped.
      return nullptr;
    }

    ValueDecl *VisitClassTemplateSpecializationDecl(
                 clang::ClassTemplateSpecializationDecl *decl) {
      // FIXME: We could import specializations, but perhaps only as unnamed
      // structural types. That's probably not useful.
      return nullptr;
    }

    ValueDecl *VisitClassTemplatePartialSpecializationDecl(
                 clang::ClassTemplatePartialSpecializationDecl *decl) {
      // Note: templates are not imported.
      return nullptr;
    }

    ValueDecl *VisitTemplateTypeParmDecl(clang::TemplateTypeParmDecl *decl) {
      // Note: templates are not imported.
      return nullptr;
    }

    ValueDecl *VisitEnumConstantDecl(clang::EnumConstantDecl *decl) {
      auto name = Impl.importName(decl->getDeclName());
      if (name.empty())
        return nullptr;

      auto dc = Impl.importDeclContext(decl->getDeclContext());
      if (!dc)
        return nullptr;

      auto result
        = new (Impl.SwiftContext)
            OneOfElementDecl(Impl.importSourceLoc(decl->getLocation()),
                             name, TypeLoc(), dc);

      // Give the oneof element the appropriate type.
      auto oneof = cast<OneOfDecl>(dc);
      auto argTy = MetaTypeType::get(oneof->getDeclaredType(),
                                     Impl.SwiftContext);
      result->overwriteType(FunctionType::get(argTy, oneof->getDeclaredType(),
                                              Impl.SwiftContext));
      return result;
    }

    ValueDecl *VisitFunctionDecl(clang::FunctionDecl *decl) {
      auto dc = Impl.importDeclContext(decl->getDeclContext());
      if (!dc)
        return nullptr;

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

      return new (Impl.SwiftContext)
               FuncDecl(SourceLoc(),
                        Impl.importSourceLoc(decl->getLocStart()),
                        name,
                        Impl.importSourceLoc(decl->getLocation()),
                        /*GenericParams=*/0,
                        type,
                        /*Body=*/nullptr,
                        dc);
    }
  };
}

ValueDecl *ClangImporter::Implementation::importDecl(clang::NamedDecl *decl) {
  auto known = ImportedDecls.find(decl->getCanonicalDecl());
  if (known != ImportedDecls.end())
    return known->second;

  SwiftDeclConverter converter(*this);
  auto result = converter.Visit(decl);
  return ImportedDecls[decl->getCanonicalDecl()] = result;
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
