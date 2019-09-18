//===--- ClangModuleLoader.h - Clang Module Loader Interface ----*- C++ -*-===//
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

#ifndef SWIFT_AST_CLANG_MODULE_LOADER_H
#define SWIFT_AST_CLANG_MODULE_LOADER_H

#include "swift/AST/ModuleLoader.h"

namespace clang {
class ASTContext;
class CompilerInstance;
class Preprocessor;
class Sema;
} // namespace clang

namespace swift {

class DeclContext;
class VisibleDeclConsumer;

/// Represents the different namespaces for types in C.
///
/// A simplified version of clang::Sema::LookupKind.
enum class ClangTypeKind {
  Typedef,
  ObjCClass = Typedef,
  /// Structs, enums, and unions.
  Tag,
  ObjCProtocol,
};

class ClangModuleLoader : public ModuleLoader {
private:
  virtual void anchor();

protected:
  using ModuleLoader::ModuleLoader;

public:
  virtual clang::ASTContext &getClangASTContext() const = 0;
  virtual clang::Preprocessor &getClangPreprocessor() const = 0;
  virtual clang::Sema &getClangSema() const = 0;
  virtual const clang::CompilerInstance &getClangInstance() const = 0;
  virtual void printStatistics() const = 0;

  /// Returns the module that contains imports and declarations from all loaded
  /// Objective-C header files.
  virtual ModuleDecl *getImportedHeaderModule() const = 0;

  /// Adds a new search path to the Clang CompilerInstance, as if specified with
  /// -I or -F.
  ///
  /// \returns true if there was an error adding the search path.
  virtual bool addSearchPath(StringRef newSearchPath, bool isFramework,
                             bool isSystem) = 0;

  /// Determine whether \c overlayDC is within an overlay module for the
  /// imported context enclosing \c importedDC.
  ///
  /// This routine is used for various hacks that are only permitted within
  /// overlays of imported modules, e.g., Objective-C bridging conformances.
  virtual bool
  isInOverlayModuleForImportedModule(const DeclContext *overlayDC,
                                     const DeclContext *importedDC) = 0;

  /// Look for declarations associated with the given name.
  ///
  /// \param name The name we're searching for.
  virtual void lookupValue(DeclName name, VisibleDeclConsumer &consumer) = 0;

  /// Look up a type declaration by its Clang name.
  ///
  /// Note that this method does no filtering. If it finds the type in a loaded
  /// module, it returns it. This is intended for use in reflection / debugging
  /// contexts where access is not a problem.
  virtual void
  lookupTypeDecl(StringRef clangName, ClangTypeKind kind,
                 llvm::function_ref<void(TypeDecl *)> receiver) = 0;

  /// Look up type a declaration synthesized by the Clang importer itself, using
  /// a "related entity kind" to determine which type it should be. For example,
  /// this can be used to find the synthesized error struct for an
  /// NS_ERROR_ENUM.
  ///
  /// Note that this method does no filtering. If it finds the type in a loaded
  /// module, it returns it. This is intended for use in reflection / debugging
  /// contexts where access is not a problem.
  virtual void
  lookupRelatedEntity(StringRef clangName, ClangTypeKind kind,
                      StringRef relatedEntityKind,
                      llvm::function_ref<void(TypeDecl *)> receiver) = 0;
};

} // namespace swift

#endif // LLVM_SWIFT_AST_CLANG_MODULE_LOADER_H
