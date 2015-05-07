//===--- ClangModuleLoader.h - Clang Module Loader Interface --*- C++ -*- -===//
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

#ifndef SWIFT_AST_CLANG_MODULE_LOADER_H
#define SWIFT_AST_CLANG_MODULE_LOADER_H

#include "swift/AST/ModuleLoader.h"

namespace clang {
class ASTContext;
class Preprocessor;
} // namespace clang

namespace swift {

class ClangModuleLoader : public ModuleLoader {
private:
  virtual void anchor();
protected:
  using ModuleLoader::ModuleLoader;
public:
  virtual clang::ASTContext &getClangASTContext() const = 0;
  virtual clang::Preprocessor &getClangPreprocessor() const = 0;
  virtual void printStatistics() const = 0;

  /// Returns the module that contains imports and declarations from all loaded
  /// Objective-C header files.
  virtual ModuleDecl *getImportedHeaderModule() const = 0;

  /// Adds a new search path to the Clang CompilerInstance, as if specified with
  /// -I or -F.
  ///
  /// \returns true if there was an error adding the search path.
  virtual bool addSearchPath(StringRef newSearchPath, bool isFramework) = 0;
};

} // namespace swift

#endif // LLVM_SWIFT_AST_CLANG_MODULE_LOADER_H

