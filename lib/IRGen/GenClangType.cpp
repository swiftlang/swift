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

static clang::CanQualType getClangSelectorType(
  const clang::ASTContext &clangCtx) {
  return clangCtx.getPointerType(clangCtx.ObjCBuiltinSelTy);
}

static clang::CanQualType getClangIdType(
  const clang::ASTContext &clangCtx) {
  clang::QualType clangType =
    clangCtx.getObjCObjectType(clangCtx.ObjCBuiltinIdTy, 0, 0);
  clangType = clangCtx.getObjCObjectPointerType(clangType);
  return clangCtx.getCanonicalType(clangType);
}

clang::CanQualType GenClangType::visitStructType(CanStructType type) {
  // First attempt a lookup in our map of imported structs.
  auto *decl = type->getDecl();
  if (auto *clangDecl = decl->getClangDecl()) {
    auto *typeDecl = cast<clang::TypeDecl>(clangDecl);
    return typeDecl->getTypeForDecl()->getCanonicalTypeUnqualified();
  }

  auto const &clangCtx = getClangASTContext();

#define MAP_BUILTIN_TYPE(CLANG_BUILTIN_KIND, SWIFT_TYPE_NAME) {                \
    auto lookupTy = getNamedSwiftType(decl->getDeclContext(),                  \
                                      #SWIFT_TYPE_NAME);                       \
    if (lookupTy && lookupTy->isEqual(type))                                   \
      return getClangBuiltinTypeFromKind(clangCtx,                             \
                                      clang::BuiltinType::CLANG_BUILTIN_KIND); \
  }
#include "swift/ClangImporter/BuiltinMappedTypes.def"
#undef MAP_BUILTIN_TYPE

  // Handle other imported types.

#define CHECK_CLANG_TYPE_MATCH(TYPE, NAME, RESULT)                           \
  if (auto lookupTy = getNamedSwiftType((TYPE)->getDecl()->getDeclContext(), \
                                        (NAME)))                             \
    if (lookupTy->isEqual(type))                                             \
      return (RESULT);

  CHECK_CLANG_TYPE_MATCH(type, "COpaquePointer", clangCtx.VoidPtrTy);
  CHECK_CLANG_TYPE_MATCH(type, "ObjCBool", clangCtx.ObjCBuiltinBoolTy);
  // FIXME: This is sufficient for ABI type generation, but should
  //        probably be const char* for type encoding.
  CHECK_CLANG_TYPE_MATCH(type, "CString", clangCtx.VoidPtrTy);
  CHECK_CLANG_TYPE_MATCH(type, "Selector", getClangSelectorType(clangCtx));
#undef CHECK_CLANG_TYPE_MATCH
  // FIXME: Handle other structs resulting from imported non-struct Clang types.
  return clang::CanQualType();
}

clang::CanQualType GenClangType::visitTupleType(CanTupleType type) {
  if (type->getNumElements() == 0)
    return getClangASTContext().VoidTy;

  llvm_unreachable("Unexpected tuple type in Clang type generation!");
  return clang::CanQualType();
}

clang::CanQualType GenClangType::visitProtocolType(CanProtocolType type) {
  if (auto lookupTy = getNamedSwiftType(type->getDecl()->getDeclContext(),
                                        "DynamicLookup"))
    if (lookupTy->isEqual(type))
      return getClangIdType(getClangASTContext());
  return clang::CanQualType();
}

clang::CanQualType GenClangType::visitMetatypeType(CanMetatypeType type) {
  auto const &clangCtx = getClangASTContext();
  clang::QualType clangType =
    clangCtx.getObjCObjectType(clangCtx.ObjCBuiltinClassTy, 0, 0);
  clangType = clangCtx.getObjCObjectPointerType(clangType);
  return clangCtx.getCanonicalType(clangType);
}

clang::CanQualType GenClangType::visitClassType(CanClassType type) {
  return clang::CanQualType();
}

clang::CanQualType GenClangType::visitBoundGenericStructType(
  CanBoundGenericStructType type) {
  return clang::CanQualType();
}

clang::CanQualType GenClangType::visitEnumType(CanEnumType type) {
  return clang::CanQualType();
}

clang::CanQualType GenClangType::visitFunctionType(CanFunctionType type) {
  return clang::CanQualType();
}

clang::CanQualType GenClangType::visitProtocolCompositionType(
  CanProtocolCompositionType type) {
  // Any protocol composition type in Swift that shows up in an @objc method maps 1-1 to
  // "id <SomeProto>"; with clang's encoding ignoring the protocol list.
  return getClangIdType(getClangASTContext());
}

clang::CanQualType GenClangType::visitType(CanType type) {
  llvm_unreachable("Unhandled type in Clang type generation.");

  return clang::CanQualType();
}

const clang::ASTContext &GenClangType::getClangASTContext() const {
  auto *CI = static_cast<ClangImporter*>(&*Context.getClangModuleLoader());
  return CI->getClangASTContext();
}
