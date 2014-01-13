//===--- GenClangType.cpp - Swift IR Generation For Types -----------------===//
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
//  This file implements generation of Clang AST types from Swift AST types
//  for types that are representable in Objective-C interfaces.
//
//===----------------------------------------------------------------------===//

#include "GenClangType.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Type.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/CanonicalType.h"
#include "clang/AST/Decl.h"
#include "clang/AST/Type.h"

using namespace swift;
using namespace irgen;

clang::CanQualType GenClangType::visitStructType(CanStructType type) {
  if (auto *clangDecl = type->getDecl()->getClangDecl()) {
    auto *typeDecl = cast<clang::TypeDecl>(clangDecl);
    return typeDecl->getTypeForDecl()->getCanonicalTypeUnqualified();
  }

  // FIXME: Handle structs resulting from non-struct Clang types.
  return clang::CanQualType();
}

clang::CanQualType GenClangType::visitTupleType(CanTupleType type) {
  if (type->getNumElements() == 0) {
    auto &swiftCtx = type->getASTContext();
    auto *CI = static_cast<ClangImporter*>(&*swiftCtx.getClangModuleLoader());
    auto &clangCtx = CI->getClangASTContext();
    return clangCtx.VoidTy;
  }

  llvm_unreachable("Unexpected tuple type in Clang type generation!");
  return clang::CanQualType();
}

clang::CanQualType GenClangType::visitType(CanType type) {
  return clang::CanQualType();
}
