//===--- ImporterImpl.h - Import Clang Modules - Implementation------------===//
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
// This file provides the implementation class definitions for the Clang
// module loader.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_CLANG_IMPORTER_IMPL_H
#define SWIFT_CLANG_IMPORTER_IMPL_H

#include "swift/ClangImporter/ClangImporter.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Basic/IdentifierTable.h"

namespace clang {
class CompilerInvocation;
class Decl;
class DeclarationName;
class NamedDecl;
class ParmVarDecl;
class QualType;
}

namespace swift {

class ASTContext;
class ClassDecl;
class ExtensionDecl;
class Identifier;
class Pattern;
class Type;
class ValueDecl;
class ClassDecl;

/// \brief Implementation of the Clang importer.
struct ClangImporter::Implementation {
  Implementation(ASTContext &ctx) : SwiftContext(ctx) { }

  /// \brief Swift AST context.
  ASTContext &SwiftContext;

  /// \brief Clang compiler invocation.
  llvm::IntrusiveRefCntPtr<clang::CompilerInvocation> Invocation;

  /// \brief Clang compiler instance, which is used to actually load Clang
  /// modules.
  std::unique_ptr<clang::CompilerInstance> Instance;

  /// \brief Clang compiler action, which is used to actually run the
  /// parser.
  std::unique_ptr<clang::SyntaxOnlyAction> Action;

  /// \brief Mapping of already-imported declarations.
  llvm::DenseMap<clang::Decl *, Decl *> ImportedDecls;

  /// \brief Generation number that is used for crude versioning.
  ///
  /// This value is incremented every time a new module is imported.
  unsigned Generation = 1;

  /// \brief A cached set of extensions for a particular Objective-C class.
  struct CachedExtensions {
    CachedExtensions()
      : Extensions(nullptr), Generation(0) { }

    CachedExtensions(const CachedExtensions &) = delete;
    CachedExtensions &operator=(const CachedExtensions &) = delete;

    CachedExtensions(CachedExtensions &&other)
      : Extensions(other.Extensions), Generation(other.Generation)
    {
      other.Extensions = nullptr;
      other.Generation = 0;
    }

    CachedExtensions &operator=(CachedExtensions &&other) {
      delete Extensions;
      Extensions = other.Extensions;
      Generation = other.Generation;
      other.Extensions = nullptr;
      other.Generation = 0;
      return *this;
    }

    ~CachedExtensions() { delete Extensions; }

    /// \brief The cached extensions.
    SmallVector<ExtensionDecl *, 4> *Extensions;

    /// \brief Generation number used to tell when this cache has gone stale.
    unsigned Generation;
  };

  /// \brief Cache of the class extensions.
  llvm::DenseMap<ClassDecl *, CachedExtensions> ClassExtensions;

private:
  /// \brief NSObject, imported into Swift.
  Type NSObjectTy;

public:
  
  ///\ brief The Swift standard library module.
  Module *swiftModule = nullptr;

  /// \brief The first Clang module we loaded.
  ///
  /// FIXME: This horrible hack is used because we don't have a nice way to
  /// map from a Decl in the tree back to the appropriate Clang module.
  /// It also means building ClangModules for all of the dependencies of a
  /// Clang module.
  ClangModule *firstClangModule = nullptr;

  /// \brief Retrieve the Clang AST context.
  clang::ASTContext &getClangASTContext() const {
    return Instance->getASTContext();
  }

  /// \brief Import the given Swift identifier into Clang.
  clang::DeclarationName importName(Identifier name);
  
  /// \brief Import the given Clang name into Swift.
  Identifier importName(clang::DeclarationName name);

  /// \brief Import the given Swift source location into Clang.
  clang::SourceLocation importSourceLoc(SourceLoc loc);

  /// \brief Import the given Clang source location into Swift.
  SourceLoc importSourceLoc(clang::SourceLocation loc);

  /// \brief Import the given Clang source range into Swift.
  SourceRange importSourceRange(clang::SourceRange loc);

  /// \brief Import the given Clang declaration into Swift.
  ///
  /// \returns The imported declaration, or null if this declaration could
  /// not be represented in Swift.
  Decl *importDecl(clang::NamedDecl *decl);

  /// \brief Import the given Clang declaration context into Swift.
  ///
  /// \returns The imported declaration context, or null if it could not
  /// be converted.
  DeclContext *importDeclContext(clang::DeclContext *dc);

  /// \brief Retrieve the named Swift type, e.g., Int32.
  ///
  /// \returns The named type, or null if the type could not be found.
  Type getNamedSwiftType(StringRef name);

  /// \brief Retrieve the NSObject type.
  Type getNSObjectType();

  /// \brief Import the given Clang type into Swift.
  ///
  /// \returns The imported type, or null if this type could
  /// not be represented in Swift.
  Type importType(clang::QualType type);

  /// \brief Import the given function type.
  ///
  /// This routine should be preferred when importing function types for
  /// which we have actual function parameters, e.g., when dealing with a
  /// function declaration, because it produces a function type whose input
  /// tuple has argument names.
  ///
  /// \param resultType The result type of the function.
  /// \param params The parameter types to the function.
  /// \param isVariadic Whether the function is variadic.
  /// \param argPatterns The externally-visible patterns for the parameters.
  /// \param bodyPatterns The patterns visible inside the function body.
  /// \param selector The Objective-C method selector to use for the names.
  ///
  /// \returns the imported function type, or null if the type cannot be
  /// imported.
  Type importFunctionType(clang::QualType resultType,
                          ArrayRef<clang::ParmVarDecl *> params,
                          bool isVariadic,
                          SmallVectorImpl<Pattern*> &argPatterns,
                          SmallVectorImpl<Pattern*> &bodyPatterns,
                          clang::Selector selector = clang::Selector());
};

}

#endif
