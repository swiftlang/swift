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
#include "swift/Basic/Assertions.h"
#include "swift/ClangImporter/ClangImporter.h"

using namespace swift;

/// Find the implementation of the named type in the named module if loaded.
static TypeDecl *findTypeInModuleByName(ASTContext &ctx,
                                        Identifier moduleName,
                                        Identifier typeName) {
  auto module = ctx.getLoadedModule(moduleName);
  if (!module)
    return nullptr;

  // Find all of the declarations with this name in the Swift module.
  SmallVector<ValueDecl *, 1> results;
  module->lookupValue(typeName, NLKind::UnqualifiedLookup, results);
  for (auto result : results) {
    if (auto nominal = dyn_cast<NominalTypeDecl>(result))
      return nominal;

    if (auto typealias = dyn_cast<TypeAliasDecl>(result))
      return typealias;
  }

  return nullptr;
}

void PrimitiveTypeMapping::initialize(ASTContext &ctx) {
  assert(mappedTypeNames.empty() && "expected empty type map");

  auto addMappedType = [&](Identifier moduleName, Identifier typeName,
                           FullClangTypeInfo info) {
    auto decl = findTypeInModuleByName(ctx, moduleName, typeName);
    if (!decl)
      return;

    // Always map a direct definition match. Either the nominal decl or the
    // typealias itself.
    mappedTypeNames[decl] = info;

    // If the underlying type of a typealias doesn't have a type, set it here.
    // This aims to reproduce the typealias behavior from BuiltinMappedTypes.
    if (auto typealias = dyn_cast<TypeAliasDecl>(decl)) {
      auto underlying = typealias->getDeclaredInterfaceType()->getAnyNominal();
      if (underlying && !mappedTypeNames.contains(underlying))
        mappedTypeNames[underlying] = info;
    }
  };

  // Map stdlib types.
#define MAP(SWIFT_NAME, CLANG_REPR, NEEDS_NULLABILITY)                         \
  addMappedType(ctx.StdlibModuleName,                                          \
                ctx.getIdentifier(#SWIFT_NAME),                                \
                {CLANG_REPR, std::optional<StringRef>(CLANG_REPR),             \
                 std::optional<StringRef>(CLANG_REPR), NEEDS_NULLABILITY})
#define MAP_C(SWIFT_NAME, OBJC_REPR, C_REPR, NEEDS_NULLABILITY)                \
  addMappedType(ctx.StdlibModuleName,                                          \
                ctx.getIdentifier(#SWIFT_NAME),                                \
                {OBJC_REPR, std::optional<StringRef>(C_REPR),                  \
                 std::optional<StringRef>(C_REPR), NEEDS_NULLABILITY})
#define MAP_CXX(SWIFT_NAME, OBJC_REPR, C_REPR, CXX_REPR, NEEDS_NULLABILITY)    \
  addMappedType(ctx.StdlibModuleName,                                          \
                ctx.getIdentifier(#SWIFT_NAME),                                \
                {OBJC_REPR, std::optional<StringRef>(C_REPR),                  \
                 std::optional<StringRef>(CXX_REPR), NEEDS_NULLABILITY})

  MAP(CBool, "bool", false);

  MAP(CChar, "char", false);
  MAP(CChar8, "char8_t", false);
  MAP(CChar16, "char16_t", false);
  MAP(CChar32, "char32_t", false);

  // Set after CChar32 to prefer char32_t for the shared underlying
  // Unicode.Scalar. char32_t is stable across platforms.
  MAP(CWideChar, "wchar_t", false);

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

  MAP(Float16, "_Float16", false);

  MAP_CXX(Int, "NSInteger", "ptrdiff_t", "swift::Int", false);
  MAP_CXX(UInt, "NSUInteger", "size_t", "swift::UInt", false);
  MAP_C(Bool, "BOOL", "bool", false);

  MAP(OpaquePointer, "void *", true);
  MAP(UnsafeRawPointer, "void const *", true);
  MAP(UnsafeMutableRawPointer, "void *", true);

  // Map other module types.

  addMappedType(ctx.Id_ObjectiveC, ctx.getIdentifier("ObjCBool"),
                {"BOOL", std::nullopt, std::nullopt, false});
  addMappedType(ctx.Id_ObjectiveC, ctx.getIdentifier("Selector"),
                {"SEL", std::nullopt, std::nullopt, true});
  addMappedType(ctx.Id_ObjectiveC,
                ctx.getIdentifier(swift::getSwiftName(
                                      KnownFoundationEntity::NSZone)),
                {"struct _NSZone *", std::nullopt, std::nullopt, true});

  addMappedType(ctx.Id_Darwin, ctx.getIdentifier("DarwinBoolean"),
                {"Boolean", std::nullopt, std::nullopt, false});

  addMappedType(ctx.Id_CoreGraphics, ctx.Id_CGFloat,
                {"CGFloat", std::nullopt, std::nullopt, false});

  addMappedType(ctx.Id_CoreFoundation, ctx.Id_CGFloat,
                {"CGFloat", std::nullopt, std::nullopt, false});

  // Use typedefs we set up for SIMD vector types.
#define MAP_SIMD_TYPE(BASENAME, _, __)                                         \
  StringRef simd2##BASENAME = "swift_" #BASENAME "2";                          \
  addMappedType(ctx.Id_simd, ctx.getIdentifier(#BASENAME "2"),                 \
      {simd2##BASENAME, simd2##BASENAME, simd2##BASENAME, false});             \
  StringRef simd3##BASENAME = "swift_" #BASENAME "3";                          \
  addMappedType(ctx.Id_simd, ctx.getIdentifier(#BASENAME "3"),                 \
      {simd3##BASENAME, simd3##BASENAME, simd3##BASENAME, false});             \
  StringRef simd4##BASENAME = "swift_" #BASENAME "4";                          \
  addMappedType(ctx.Id_simd, ctx.getIdentifier(#BASENAME "4"),                 \
      {simd4##BASENAME, simd4##BASENAME, simd4##BASENAME, false});
#include "swift/ClangImporter/SIMDMappedTypes.def"
  static_assert(SWIFT_MAX_IMPORTED_SIMD_ELEMENTS == 4,
                "must add or remove special name mappings if max number of "
                "SIMD elements is changed");
}

PrimitiveTypeMapping::FullClangTypeInfo *
PrimitiveTypeMapping::getMappedTypeInfoOrNull(const TypeDecl *typeDecl) {
  if (mappedTypeNames.empty())
    initialize(typeDecl->getASTContext());

  auto nominal = dyn_cast<TypeDecl>(typeDecl);
  if (!nominal)
    return nullptr;

  auto iter = mappedTypeNames.find(nominal);
  if (iter == mappedTypeNames.end())
    return nullptr;
  return &iter->second;
}

std::optional<PrimitiveTypeMapping::ClangTypeInfo>
PrimitiveTypeMapping::getKnownObjCTypeInfo(const TypeDecl *typeDecl) {
  if (auto *typeInfo = getMappedTypeInfoOrNull(typeDecl))
    return ClangTypeInfo{typeInfo->objcName, typeInfo->canBeNullable};
  return std::nullopt;
}

std::optional<PrimitiveTypeMapping::ClangTypeInfo>
PrimitiveTypeMapping::getKnownCTypeInfo(const TypeDecl *typeDecl) {
  if (auto *typeInfo = getMappedTypeInfoOrNull(typeDecl)) {
    if (typeInfo->cName)
      return ClangTypeInfo{*typeInfo->cName, typeInfo->canBeNullable};
  }
  return std::nullopt;
}

std::optional<PrimitiveTypeMapping::ClangTypeInfo>
PrimitiveTypeMapping::getKnownCxxTypeInfo(const TypeDecl *typeDecl) {
  if (auto *typeInfo = getMappedTypeInfoOrNull(typeDecl)) {
    if (typeInfo->cxxName)
      return ClangTypeInfo{*typeInfo->cxxName, typeInfo->canBeNullable};
  }
  return std::nullopt;
}
