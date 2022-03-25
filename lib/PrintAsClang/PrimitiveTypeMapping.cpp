//===--- PrimitiveTypeMapping.cpp - Mapping primitive types -----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "PrimitiveTypeMapping.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/ClangImporter/ClangImporter.h"

using namespace swift;

void PrimitiveTypeMapping::initialize(ASTContext &ctx) {
  assert(mappedTypeNames.empty() && "expected empty type map");
#define MAP(SWIFT_NAME, CLANG_REPR, NEEDS_NULLABILITY)                         \
  mappedTypeNames[{ctx.StdlibModuleName, ctx.getIdentifier(#SWIFT_NAME)}] = {  \
    CLANG_REPR,                                                                \
    Optional<StringRef>(CLANG_REPR),                                           \
    NEEDS_NULLABILITY                                                          \
  }
#define MAP_C(SWIFT_NAME, OBJC_REPR, C_REPR, NEEDS_NULLABILITY)                \
  mappedTypeNames[{ctx.StdlibModuleName, ctx.getIdentifier(#SWIFT_NAME)}] = {  \
      OBJC_REPR, Optional<StringRef>(C_REPR), NEEDS_NULLABILITY}

  MAP(CBool, "bool", false);

  MAP(CChar, "char", false);
  MAP(CWideChar, "wchar_t", false);
  MAP(CChar16, "char16_t", false);
  MAP(CChar32, "char32_t", false);

  MAP(CSignedChar, "signed char", false);
  MAP(CShort, "short", false);
  MAP(CInt, "int", false);
  MAP(CLong, "long", false);
  MAP(CLongLong, "long long", false);

  MAP(CUnsignedChar, "unsigned char", false);
  MAP(CUnsignedShort, "unsigned short", false);
  MAP(CUnsignedInt, "unsigned int", false);
  MAP(CUnsignedLong, "unsigned long", false);
  MAP(CUnsignedLongLong, "unsigned long long", false);

  MAP(CFloat, "float", false);
  MAP(CDouble, "double", false);

  MAP(Int8, "int8_t", false);
  MAP(Int16, "int16_t", false);
  MAP(Int32, "int32_t", false);
  MAP(Int64, "int64_t", false);
  MAP(UInt8, "uint8_t", false);
  MAP(UInt16, "uint16_t", false);
  MAP(UInt32, "uint32_t", false);
  MAP(UInt64, "uint64_t", false);

  MAP(Float, "float", false);
  MAP(Double, "double", false);
  MAP(Float32, "float", false);
  MAP(Float64, "double", false);

  MAP_C(Int, "NSInteger", "ptrdiff_t", false);
  MAP_C(UInt, "NSUInteger", "size_t", false);
  MAP_C(Bool, "BOOL", "bool", false);

  MAP(OpaquePointer, "void *", true);
  MAP(UnsafeRawPointer, "void const *", true);
  MAP(UnsafeMutableRawPointer, "void *", true);

  Identifier ID_ObjectiveC = ctx.Id_ObjectiveC;
  mappedTypeNames[{ID_ObjectiveC, ctx.getIdentifier("ObjCBool")}] = {
      "BOOL", None, false};
  mappedTypeNames[{ID_ObjectiveC, ctx.getIdentifier("Selector")}] = {
      "SEL", None, true};
  mappedTypeNames[{ID_ObjectiveC, ctx.getIdentifier(ctx.getSwiftName(
                                      KnownFoundationEntity::NSZone))}] = {
      "struct _NSZone *", None, true};

  mappedTypeNames[{ctx.Id_Darwin, ctx.getIdentifier("DarwinBoolean")}] = {
      "Boolean", None, false};

  mappedTypeNames[{ctx.Id_CoreGraphics, ctx.Id_CGFloat}] = {"CGFloat", None,
                                                            false};

  mappedTypeNames[{ctx.Id_CoreFoundation, ctx.Id_CGFloat}] = {"CGFloat", None,
                                                              false};

  // Use typedefs we set up for SIMD vector types.
#define MAP_SIMD_TYPE(BASENAME, _, __)                                         \
  mappedTypeNames[{ctx.Id_simd, ctx.getIdentifier(#BASENAME "2")}] = {         \
      "swift_" #BASENAME "2", Optional<StringRef>("swift_" #BASENAME "2"),     \
      false};                                                                  \
  mappedTypeNames[{ctx.Id_simd, ctx.getIdentifier(#BASENAME "3")}] = {         \
      "swift_" #BASENAME "3", Optional<StringRef>("swift_" #BASENAME "3"),     \
      false};                                                                  \
  mappedTypeNames[{ctx.Id_simd, ctx.getIdentifier(#BASENAME "4")}] = {         \
      "swift_" #BASENAME "4", Optional<StringRef>("swift_" #BASENAME "4"),     \
      false};
#include "swift/ClangImporter/SIMDMappedTypes.def"
  static_assert(SWIFT_MAX_IMPORTED_SIMD_ELEMENTS == 4,
                "must add or remove special name mappings if max number of "
                "SIMD elements is changed");
}

PrimitiveTypeMapping::ClangTypeInfo *
PrimitiveTypeMapping::getMappedTypeInfoOrNull(const TypeDecl *typeDecl) {
  if (mappedTypeNames.empty())
    initialize(typeDecl->getASTContext());

  Identifier moduleName = typeDecl->getModuleContext()->getName();
  Identifier name = typeDecl->getName();
  auto iter = mappedTypeNames.find({moduleName, name});
  if (iter == mappedTypeNames.end())
    return nullptr;
  return &iter->second;
}

Optional<PrimitiveTypeMapping::ObjCClangTypeInfo>
PrimitiveTypeMapping::getKnownObjCTypeInfo(const TypeDecl *typeDecl) {
  if (auto *typeInfo = getMappedTypeInfoOrNull(typeDecl))
    return ObjCClangTypeInfo{typeInfo->objcName, typeInfo->canBeNullable};
  return None;
}

Optional<PrimitiveTypeMapping::CClangTypeInfo>
PrimitiveTypeMapping::getKnownCTypeInfo(const TypeDecl *typeDecl) {
  if (auto *typeInfo = getMappedTypeInfoOrNull(typeDecl)) {
    if (typeInfo->cName)
      return CClangTypeInfo{*typeInfo->cName, typeInfo->canBeNullable};
  }
  return None;
}
