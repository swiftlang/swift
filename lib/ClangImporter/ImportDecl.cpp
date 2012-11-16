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
#include "clang/AST/TypeVisitor.h"
#include "llvm/ADT/StringExtras.h"

using namespace swift;

ValueDecl *ClangImporter::Implementation::importDecl(clang::NamedDecl *decl) {
  auto known = ImportedDecls.find(decl);
  if (known != ImportedDecls.end())
    return known->second;

  if (decl->getKind() == clang::Decl::Function) {
    auto func = cast<clang::FunctionDecl>(decl);

    // Import the function type. If we have parameters, make sure their names
    // get into the resulting function type.
    Type type;
    if (func->param_size())
      type = importFunctionType(
                                func->getType()->getAs<clang::FunctionType>()->getResultType(),
                                { func->param_begin(), func->param_size() },
                                func->isVariadic());
    else
      type = importType(func->getType());

    if (!type)
      return nullptr;

    auto name = importName(decl->getDeclName());

    // FIXME: Map source locations!
    auto result = new (SwiftContext) FuncDecl(SourceLoc(),
                                              SourceLoc(),
                                              name,
                                              SourceLoc(),
                                              /*GenericParams=*/0,
                                              type,
                                              /*Body=*/nullptr,
                                              firstClangModule);
    return ImportedDecls[decl] = result;
  }
  
  return nullptr;
}
