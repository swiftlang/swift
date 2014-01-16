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
#include "swift/AST/NameLookup.h"
#include "swift/AST/Type.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/CanonicalType.h"
#include "clang/AST/Decl.h"
#include "clang/AST/Type.h"

using namespace swift;
using namespace irgen;

static Type getNamedSwiftType(DeclContext *DC, StringRef name) {
  assert(DC && "Unexpected null declaration context!");

  auto &astContext = DC->getASTContext();
  UnqualifiedLookup lookup(astContext.getIdentifier(name), DC, nullptr);
  if (auto type = lookup.getSingleTypeResult())
    return type->getDeclaredType();

  return Type();
}

static const clang::CanQualType getClangBuiltinTypeFromKind(
  const clang::ASTContext &context,
  clang::BuiltinType::Kind kind) {
  switch (kind) {
#define BUILTIN_TYPE(Id, SingletonId) \
  case clang::BuiltinType::Id: return context.SingletonId;
#include "clang/AST/BuiltinTypes.def"
  }
}

clang::CanQualType GenClangType::visitStructType(CanStructType type) {
  // First attempt a lookup in our map of imported structs.
  auto *decl = type->getDecl();
  if (auto *clangDecl = decl->getClangDecl()) {
    auto *typeDecl = cast<clang::TypeDecl>(clangDecl);
    return typeDecl->getTypeForDecl()->getCanonicalTypeUnqualified();
  }

  auto &swiftCtx = type->getASTContext();
  auto *CI = static_cast<ClangImporter*>(&*swiftCtx.getClangModuleLoader());
  auto const &clangCtx = CI->getClangASTContext();
  
  // Special casing for those for which there are no type aliases.
  if (type->getDecl()->getName().str().equals("COpaquePointer"))
    return clangCtx.VoidPtrTy;
  if (type->getDecl()->getName().str().equals("UnicodeScalar"))
    return clangCtx.IntTy;
  if (type->getDecl()->getName().str().equals("Bool"))
    return clangCtx.SignedCharTy;\
  if (type->getDecl()->getName().str().equals("CBool"))
    return clangCtx.BoolTy;

#define MAP_BUILTIN_TYPE(CLANG_BUILTIN_KIND, SWIFT_TYPE_NAME) {                \
    auto lookupTy = getNamedSwiftType(decl->getDeclContext(),                  \
                                      #SWIFT_TYPE_NAME);                       \
    if (lookupTy && lookupTy->isEqual(type))                                   \
      return getClangBuiltinTypeFromKind(clangCtx,                             \
                                      clang::BuiltinType::CLANG_BUILTIN_KIND); \
  }
#include "swift/ClangImporter/BuiltinMappedTypes.def"
#undef MAP_BUILTIN_TYPE

  // FIXME: Handle other structs resulting from imported non-struct Clang types.
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
