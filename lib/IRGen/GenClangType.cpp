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

#include "llvm/ADT/StringSwitch.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/CanTypeVisitor.h"
#include "swift/AST/Decl.h"
#include "swift/AST/NameLookup.h"
#include "swift/SIL/SILType.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/CanonicalType.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/Type.h"
#include "IRGenModule.h"

using namespace swift;
using namespace irgen;

/// Global information about importing clang types.
class swift::irgen::ClangTypeConverter {
  llvm::DenseMap<CanType, clang::CanQualType> Cache;

  ClangTypeConverter(const ClangTypeConverter &) = delete;
  ClangTypeConverter &operator=(const ClangTypeConverter &) = delete;

public:
  ClangTypeConverter() = default;
  clang::CanQualType convert(IRGenModule &IGM, CanType type);

private:
  void fillSpeciallyImportedTypeCache(IRGenModule &IGM);
};

static CanType getNamedSwiftType(Module *stdlib, StringRef name) {
  auto &ctx = stdlib->getASTContext();
  SmallVector<ValueDecl*, 1> results;
  stdlib->lookupValue({}, ctx.getIdentifier(name), NLKind::QualifiedLookup,
                      results);

  // If we have one single type decl, and that decl has been
  // type-checked, return its declared type.
  //
  // ...non-type-checked types should only ever show up here because
  // of test cases using -enable-source-import, but unfortunately
  // that's a real thing.
  if (results.size() == 1) {
    if (auto typeDecl = dyn_cast<TypeDecl>(results[0]))
      if (typeDecl->hasType())
        return typeDecl->getDeclaredType()->getCanonicalType();
  }
  return CanType();
}

static clang::CanQualType getClangBuiltinTypeFromKind(
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

static clang::CanQualType getClangMetatypeType(
  const clang::ASTContext &clangCtx) {
  clang::QualType clangType =
    clangCtx.getObjCObjectType(clangCtx.ObjCBuiltinClassTy, 0, 0);
  clangType = clangCtx.getObjCObjectPointerType(clangType);
  return clangCtx.getCanonicalType(clangType);
}

static clang::CanQualType getClangIdType(
  const clang::ASTContext &clangCtx) {
  clang::QualType clangType =
    clangCtx.getObjCObjectType(clangCtx.ObjCBuiltinIdTy, 0, 0);
  clangType = clangCtx.getObjCObjectPointerType(clangType);
  return clangCtx.getCanonicalType(clangType);
}

static clang::CanQualType getClangDecayedVaListType(
  const clang::ASTContext &clangCtx) {
  clang::QualType clangType =
    clangCtx.getCanonicalType(clangCtx.getBuiltinVaListType());
  if (clangType->isConstantArrayType())
    clangType = clangCtx.getDecayedType(clangType);
  return clangCtx.getCanonicalType(clangType);
}

namespace {
/// Given a Swift type, attempt to return an appropriate Clang
/// CanQualType for the purpose of generating correct code for the
/// ABI.
class GenClangType : public CanTypeVisitor<GenClangType, clang::CanQualType> {
  IRGenModule &IGM;
  ClangTypeConverter &Converter;
public:
  GenClangType(IRGenModule &IGM, ClangTypeConverter &converter)
    : IGM(IGM), Converter(converter) {}

  const clang::ASTContext &getClangASTContext() const {
    return IGM.getClangASTContext();
  }

  /// Return the Clang struct type which was imported and resulted in
  /// this Swift struct type. We do not currently handle generating a
  /// new Clang struct type for Swift struct types that are created
  /// independently of importing a Clang module.
  clang::CanQualType visitStructType(CanStructType type);
  clang::CanQualType visitTupleType(CanTupleType type);
  clang::CanQualType visitMetatypeType(CanMetatypeType type);
  clang::CanQualType visitExistentialMetatypeType(CanExistentialMetatypeType type);
  clang::CanQualType visitProtocolType(CanProtocolType type);
  clang::CanQualType visitClassType(CanClassType type);
  clang::CanQualType visitBoundGenericClassType(CanBoundGenericClassType type);
  clang::CanQualType visitBoundGenericType(CanBoundGenericType type);
  clang::CanQualType visitEnumType(CanEnumType type);
  clang::CanQualType visitFunctionType(CanFunctionType type);
  clang::CanQualType visitProtocolCompositionType(
                                               CanProtocolCompositionType type);
  clang::CanQualType visitBuiltinRawPointerType(CanBuiltinRawPointerType type);
  clang::CanQualType visitBuiltinUnknownObjectType(
                                                CanBuiltinUnknownObjectType type);
  clang::CanQualType visitArchetypeType(CanArchetypeType type);
  clang::CanQualType visitSILFunctionType(CanSILFunctionType type);
  clang::CanQualType visitGenericTypeParamType(CanGenericTypeParamType type);
  clang::CanQualType visitDynamicSelfType(CanDynamicSelfType type);
  
  clang::CanQualType visitSILBlockStorageType(CanSILBlockStorageType type);
  
  clang::CanQualType visitType(CanType type);

  clang::CanQualType getCanonicalType(clang::QualType type) {
    return getClangASTContext().getCanonicalType(type);
  }
};
}

clang::CanQualType GenClangType::visitStructType(CanStructType type) {
  // Everything else should have been handled as an imported type
  // or an importer-primitive type.
  llvm_unreachable("Unhandled struct type in Clang type generation");
}

clang::CanQualType GenClangType::visitTupleType(CanTupleType type) {
  if (type->getNumElements() == 0)
    return getClangASTContext().VoidTy;

  llvm_unreachable("Unexpected tuple type in Clang type generation!");
}

clang::CanQualType GenClangType::visitProtocolType(CanProtocolType type) {
  return getClangIdType(getClangASTContext());
}

clang::CanQualType GenClangType::visitMetatypeType(CanMetatypeType type) {
  return getClangMetatypeType(getClangASTContext());
}

clang::CanQualType
GenClangType::visitExistentialMetatypeType(CanExistentialMetatypeType type) {
  return getClangMetatypeType(getClangASTContext());
}

clang::CanQualType GenClangType::visitClassType(CanClassType type) {
  auto &clangCtx = getClangASTContext();
  // produce the clang type INTF * if it is imported ObjC object.
  auto swiftDecl = type->getDecl();
  if (swiftDecl->isObjC()) {
    clang::IdentifierInfo *ForwardClassId =
      &clangCtx.Idents.get(swiftDecl->getName().get());
    auto *CDecl = clang::ObjCInterfaceDecl::Create(
                          clangCtx, clangCtx.getTranslationUnitDecl(),
                          clang::SourceLocation(), ForwardClassId,
                          0/*PrevIDecl*/, clang::SourceLocation());
    auto clangType  = clangCtx.getObjCInterfaceType(CDecl);
    auto ptrTy = clangCtx.getObjCObjectPointerType(clangType);
    return clangCtx.getCanonicalType(ptrTy);
  }
  return getClangIdType(clangCtx);
}

clang::CanQualType GenClangType::visitBoundGenericClassType(
                                                CanBoundGenericClassType type) {
  // Any @objc class type in Swift that shows up in an @objc method maps 1-1 to
  // "id <SomeProto>"; with clang's encoding ignoring the protocol list.
  return getClangIdType(getClangASTContext());
}

clang::CanQualType
GenClangType::visitBoundGenericType(CanBoundGenericType type) {
  // We only expect *Pointer<T>, ImplicitlyUnwrappedOptional<T>, and Optional<T>.
  // The first two are structs; the last is an enum.
  if (auto underlyingTy = type.getAnyOptionalObjectType()) {
    // The underlying type could be a bridged type, which makes any
    // sort of casual assertion here difficult.
    return Converter.convert(IGM, underlyingTy);
  }

  auto swiftStructDecl = type->getDecl();
  
  enum class StructKind {
    Invalid,
    UnsafePointer,
    AutoreleasingUnsafePointer,
    CMutablePointer,
    CConstPointer,
    Array,
    Dictionary,
    Unmanaged,
  } kind = llvm::StringSwitch<StructKind>(swiftStructDecl->getName().str())
    .Case("UnsafePointer", StructKind::UnsafePointer)
    .Case("AutoreleasingUnsafePointer", StructKind::AutoreleasingUnsafePointer)
    .Case("CMutablePointer", StructKind::CMutablePointer)
    .Case("CConstPointer", StructKind::CConstPointer)
    .Case("Array", StructKind::Array)
    .Case("Dictionary", StructKind::Dictionary)
    .Case("Unmanaged", StructKind::Unmanaged)
    .Default(StructKind::Invalid);
  
  auto args = type.getGenericArgs();

  switch (kind) {
  case StructKind::Invalid:
    llvm_unreachable("Unexpected non-pointer generic struct type in imported"
                     " Clang module!");
      
  case StructKind::UnsafePointer: // Assume UnsafePointer is mutable
  case StructKind::Unmanaged:
  case StructKind::AutoreleasingUnsafePointer:
  case StructKind::CMutablePointer: {
    assert(args.size() == 1 &&
           "*Pointer<T> should have a single generic argument!");
    auto clangCanTy = Converter.convert(IGM, args.front());
    if (!clangCanTy) return clang::CanQualType();
    return getClangASTContext().getPointerType(clangCanTy);
  }
      
  case StructKind::CConstPointer: {
    clang::QualType clangTy
      = Converter.convert(IGM, args.front()).withConst();
    return getCanonicalType(getClangASTContext().getPointerType(clangTy));
  }

  case StructKind::Array:
  case StructKind::Dictionary:
    return getClangIdType(getClangASTContext());
  }
}

clang::CanQualType GenClangType::visitEnumType(CanEnumType type) {
  llvm_unreachable("Imported enum without Clang decl!");
}

// FIXME: We hit this building Foundation, with a call on the type
//        encoding path. It seems like we shouldn't see FunctionType
//        at that point.
clang::CanQualType GenClangType::visitFunctionType(CanFunctionType type) {
  auto &clangCtx = getClangASTContext();
  SmallVector<clang::QualType, 16> paramTypes;
  CanType result = type.getResult();
  CanType input = type.getInput();
  auto resultType = Converter.convert(IGM, result);
  if (!resultType.isNull())
    if (auto tuple = dyn_cast<TupleType>(input)) {
      bool failed = false;
      for (auto argType: tuple.getElementTypes()) {
        auto clangType = Converter.convert(IGM, argType);
        if (clangType.isNull()) {
          failed = true;
          break;
        }
        paramTypes.push_back(clangType);
      }
      if (!failed) {
        clang::FunctionProtoType::ExtProtoInfo DefaultEPI;
        auto fnTy = clangCtx.getFunctionType(resultType, paramTypes, DefaultEPI);
        auto blockTy = clangCtx.getBlockPointerType(fnTy);
        return clangCtx.getCanonicalType(blockTy);
      }
    }
  
  // We'll select (void)(^)() for function types. As long as it's a
  // pointer type it doesn't matter exactly which for either ABI type
  // generation or Obj-C type encoding.
  auto fnTy = clangCtx.getFunctionNoProtoType(clangCtx.VoidTy);
  auto blockTy = clangCtx.getBlockPointerType(fnTy);
  return clangCtx.getCanonicalType(blockTy);
}

clang::CanQualType GenClangType::visitSILFunctionType(CanSILFunctionType type) {
  // We'll select (void)(^)() for function types. As long as it's a
  // pointer type it doesn't matter exactly which for either ABI type
  // generation or Obj-C type encoding.
  //
  // FIXME: This isn't strictly true because ObjC extended method encoding
  // encodes the parameter and return types of blocks.
  auto &clangCtx = getClangASTContext();
  auto fnTy = clangCtx.getFunctionNoProtoType(clangCtx.VoidTy);
  auto blockTy = clangCtx.getBlockPointerType(fnTy);
  return clangCtx.getCanonicalType(blockTy);
}

clang::CanQualType GenClangType::visitSILBlockStorageType(CanSILBlockStorageType type) {
  // We'll select (void)(^)() for function types. As long as it's a
  // pointer type it doesn't matter exactly which for either ABI type
  // generation or Obj-C type encoding.
  auto &clangCtx = getClangASTContext();
  auto fnTy = clangCtx.getFunctionNoProtoType(clangCtx.VoidTy);
  auto blockTy = clangCtx.getBlockPointerType(fnTy);
  return clangCtx.getCanonicalType(blockTy);
}

clang::CanQualType GenClangType::visitProtocolCompositionType(
  CanProtocolCompositionType type) {
  // FIXME. Eventually, this will have its own helper routine.
  SmallVector<const clang::ObjCProtocolDecl *, 4> Protocols;
  for (Type t : type->getProtocols()) {
    ProtocolDecl *protocol = t->castTo<ProtocolType>()->getDecl();
    if (auto *clangDecl = protocol->getClangDecl())
      if (auto *PDecl = dyn_cast<clang::ObjCProtocolDecl>(clangDecl))
        Protocols.push_back(PDecl);
  }
  auto &clangCtx = getClangASTContext();
  if (Protocols.empty())
    return getClangIdType(clangCtx);
  // id<protocol-list>
  clang::ObjCProtocolDecl **ProtoQuals = new(clangCtx) clang::ObjCProtocolDecl*[Protocols.size()];
  memcpy(ProtoQuals, Protocols.data(), sizeof(clang::ObjCProtocolDecl*)*Protocols.size());
  auto clangType = clangCtx.getObjCObjectType(clangCtx.ObjCBuiltinIdTy,
                     ProtoQuals,
                     Protocols.size());
  auto ptrTy = clangCtx.getObjCObjectPointerType(clangType);
  return clangCtx.getCanonicalType(ptrTy);
}

clang::CanQualType GenClangType::visitBuiltinRawPointerType(
  CanBuiltinRawPointerType type) {
  return getClangASTContext().VoidPtrTy;
}

clang::CanQualType GenClangType::visitBuiltinUnknownObjectType(
  CanBuiltinUnknownObjectType type) {
  auto &clangCtx = getClangASTContext();
  auto ptrTy = clangCtx.getObjCObjectPointerType(clangCtx.VoidTy);
  return clangCtx.getCanonicalType(ptrTy);
}

clang::CanQualType GenClangType::visitArchetypeType(CanArchetypeType type) {
  // We see these in the case where we invoke an @objc function
  // through a protocol.
  return getClangIdType(getClangASTContext());
}

clang::CanQualType GenClangType::visitDynamicSelfType(CanDynamicSelfType type) {
  // Dynamic Self is equivalent to 'instancetype', which is treated as
  // 'id' within the Objective-C type system.
  return getClangIdType(getClangASTContext());
}

clang::CanQualType GenClangType::visitGenericTypeParamType(
  CanGenericTypeParamType type) {
  // We see these in the case where we invoke an @objc function
  // through a protocol argument that is a generic type.
  return getClangIdType(getClangASTContext());
}

clang::CanQualType GenClangType::visitType(CanType type) {
  llvm_unreachable("Unexpected type in Clang type generation.");
}

clang::CanQualType ClangTypeConverter::convert(IRGenModule &IGM, CanType type) {
  // Try to do this without making cache entries for obvious cases.
  if (auto nominal = dyn_cast<NominalType>(type)) {
    if (auto clangDecl = nominal->getDecl()->getClangDecl()) {
      if (auto clangTypeDecl = dyn_cast<clang::TypeDecl>(clangDecl)) {
        auto &ctx = IGM.getClangASTContext();
        return ctx.getCanonicalType(ctx.getTypeDeclType(clangTypeDecl));
      } else if (auto ifaceDecl = dyn_cast<clang::ObjCInterfaceDecl>(clangDecl)) {
        auto &ctx = IGM.getClangASTContext();
        auto clangType  = ctx.getObjCInterfaceType(ifaceDecl);
        auto ptrTy = ctx.getObjCObjectPointerType(clangType);
        return ctx.getCanonicalType(ptrTy);
      }
    }
  }

  // If the cache is empty, fill it the builtin cases.
  if (Cache.empty()) {
    fillSpeciallyImportedTypeCache(IGM);
  }

  // Look in the cache.
  auto it = Cache.find(type);
  if (it != Cache.end()) {
    return it->second;
  }

  // If that failed, convert the type, cache, and return.
  clang::CanQualType result = GenClangType(IGM, *this).visit(type);
  Cache.insert({type, result});
  return result;
}

/// Fill the cache with entries for the fundamental types that are
/// special-cased by the importer.
void ClangTypeConverter::fillSpeciallyImportedTypeCache(IRGenModule &IGM) {
  // Do nothing if there isn't a stdlib module.
  auto stdlib = IGM.Context.getStdlibModule();
  if (!stdlib) return;

  auto &ctx = IGM.getClangASTContext();

#define CACHE_TYPE(MODULE, NAME, CLANG_TYPE)                            \
  do {                                                                  \
    if (CanType type = getNamedSwiftType(MODULE, NAME)) {               \
      Cache.insert({type, CLANG_TYPE});                                 \
    }                                                                   \
  } while (0)
#define CACHE_STDLIB_TYPE(NAME, CLANG_TYPE)                             \
  CACHE_TYPE(stdlib, NAME, CLANG_TYPE)

  // Handle all the builtin types.  This can be many-to-one; let the
  // first entry that corresponds to a type win.
#define MAP_BUILTIN_TYPE(CLANG_BUILTIN_KIND, SWIFT_TYPE_NAME)           \
  CACHE_STDLIB_TYPE(#SWIFT_TYPE_NAME, getClangBuiltinTypeFromKind(ctx,  \
                              clang::BuiltinType::CLANG_BUILTIN_KIND));
#include "swift/ClangImporter/BuiltinMappedTypes.def"

  CACHE_STDLIB_TYPE("COpaquePointer", ctx.VoidPtrTy);
  CACHE_STDLIB_TYPE("CConstVoidPointer",
             ctx.getCanonicalType(ctx.VoidPtrTy.withConst()));
  CACHE_STDLIB_TYPE("CMutableVoidPointer", ctx.VoidPtrTy);

  // FIXME: This is sufficient for ABI type generation, but should
  //        probably be const char* for type encoding.
  CACHE_STDLIB_TYPE("CString", ctx.VoidPtrTy);

  // We import NSString* (an Obj-C object pointer) as String.
  CACHE_STDLIB_TYPE("String", getClangIdType(ctx));

  CACHE_STDLIB_TYPE("CVaListPointer", getClangDecayedVaListType(ctx));

  // These types come from the ObjectiveC module.
  if (auto objcModule =
        IGM.Context.getLoadedModule(IGM.Context.getIdentifier("ObjectiveC"))) {
    CACHE_TYPE(objcModule, "ObjCBool", ctx.ObjCBuiltinBoolTy);
    CACHE_TYPE(objcModule, "Selector", getClangSelectorType(ctx));
  }

  // These types come from the Foundation module.
  if (auto foundationModule =
        IGM.Context.getLoadedModule(IGM.Context.getIdentifier("Foundation"))) {
    // We import NSZone* (a struct pointer) as NSZone.
    CACHE_TYPE(foundationModule, "NSZone", ctx.VoidPtrTy);
  }

#undef CACHE_STDLIB_TYPE
#undef CACHE_TYPE
}

clang::CanQualType IRGenModule::getClangType(SILType type) {
  return getClangType(type.getSwiftRValueType());
}

clang::CanQualType IRGenModule::getClangType(CanType type) {
  return ClangTypes->convert(*this, type);
}

void IRGenModule::initClangTypeConverter() {
  if (auto loader = Context.getClangModuleLoader()) {
    auto importer = static_cast<ClangImporter*>(loader);
    ClangASTContext = &importer->getClangASTContext();
    ClangTypes = new ClangTypeConverter();
  } else {
    ClangASTContext = nullptr;
    ClangTypes = nullptr;
  }
}

void IRGenModule::destroyClangTypeConverter() {
  delete ClangTypes;
}
