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
public:
  virtual clang::ASTContext &getClangASTContext() const = 0;
  virtual clang::Preprocessor &getClangPreprocessor() const = 0;
};

} // namespace swift

#endif // LLVM_SWIFT_AST_CLANG_MODULE_LOADER_H

