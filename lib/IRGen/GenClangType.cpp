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

#include "swift/AST/Decl.h"
#include "swift/AST/Type.h"
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

  // FIXME: For parameters, we need to be able to generate a Clang
  // type for all Swift types that can appear in an @objc parameter
  // list.
  return clang::CanQualType();
}

clang::CanQualType GenClangType::visitType(CanType type) {
  return clang::CanQualType();
}
