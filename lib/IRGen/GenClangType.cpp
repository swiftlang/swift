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
#include "clang/AST/Attr.h"
#include "clang/AST/CanonicalType.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/Type.h"
#include "clang/Basic/TargetInfo.h"
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
  clang::CanQualType reverseBuiltinTypeMapping(IRGenModule &IGM,
                                               CanStructType type);
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

  clang::QualType convertFunctionType(CanFunctionType type);

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
  clang::CanQualType visitBuiltinIntegerType(CanBuiltinIntegerType type);
  clang::CanQualType visitBuiltinFloatType(CanBuiltinFloatType type);
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

  clang::CanQualType convertMemberType(NominalTypeDecl *DC,
                                       StringRef memberName);
};
}

clang::CanQualType
GenClangType::convertMemberType(NominalTypeDecl *DC, StringRef memberName) {
  auto memberTypeDecl = cast<TypeDecl>(
    DC->lookupDirect(IGM.Context.getIdentifier(memberName))[0]);
  auto memberType = memberTypeDecl->getDeclaredType()->getCanonicalType();
  return Converter.convert(IGM, memberType);
}

static clang::CanQualType getClangVectorType(const clang::ASTContext &ctx,
                                             clang::BuiltinType::Kind eltKind,
                                        clang::VectorType::VectorKind vecKind,
                                             StringRef numEltsString) {
  unsigned numElts;
  bool failedParse = numEltsString.getAsInteger<unsigned>(10, numElts);
  assert(!failedParse && "vector type name didn't end in count?");
  (void) failedParse;

  auto eltTy = getClangBuiltinTypeFromKind(ctx, eltKind);
  auto vecTy = ctx.getVectorType(eltTy, numElts, vecKind);
  return ctx.getCanonicalType(vecTy);
}

clang::CanQualType GenClangType::visitStructType(CanStructType type) {
  auto &ctx = IGM.getClangASTContext();

  auto swiftDecl = type->getDecl();
  StringRef name = swiftDecl->getName().str();

  // We assume that the importer translates all of the following types
  // directly to structs in the standard library.

  // We want to recognize most of these types by name.
#define CHECK_NAMED_TYPE(NAME, CLANG_TYPE) do {                         \
    if (name == (NAME)) return CLANG_TYPE;                              \
  } while (false)

  CHECK_NAMED_TYPE("CGFloat", convertMemberType(swiftDecl, "NativeType"));
  CHECK_NAMED_TYPE("COpaquePointer", ctx.VoidPtrTy);
  CHECK_NAMED_TYPE("CVaListPointer", getClangDecayedVaListType(ctx));
  CHECK_NAMED_TYPE("NSZone", ctx.VoidPtrTy);
  CHECK_NAMED_TYPE("ObjCBool", ctx.ObjCBuiltinBoolTy);
  CHECK_NAMED_TYPE("Selector", getClangSelectorType(ctx));
#undef CHECK_NAMED_TYPE

  // Map vector types to the corresponding C vectors.
#define MAP_SIMD_TYPE(TYPE_NAME, CLANG_KIND)                           \
  if (name.startswith(#TYPE_NAME)) {                                   \
    return getClangVectorType(ctx, clang::BuiltinType::CLANG_KIND,     \
                              clang::VectorType::GenericVector,        \
                              name.drop_front(sizeof(#TYPE_NAME)-1));  \
  }
#include "swift/ClangImporter/SIMDMappedTypes.def"

  // Everything else we see here ought to be a translation of a builtin.
  return Converter.reverseBuiltinTypeMapping(IGM, type);
}

clang::CanQualType
ClangTypeConverter::reverseBuiltinTypeMapping(IRGenModule &IGM,
                                              CanStructType type) {
  // Handle builtin types by adding entries to the cache that reverse
  // the mapping done by the importer.  We could try to look at the
  // members of the struct instead, but even if that's ABI-equivalent
  // (which it had better be!), it might erase interesting semantic
  // differences like integers vs. characters.  This is important
  // because CC lowering isn't the only purpose of this conversion.
  //
  // The importer maps builtin types like 'int' to named types like
  // 'CInt', which are generally typealiases.  So what we do here is
  // map the underlying types of those typealiases back to the builtin
  // type.  These typealiases frequently create a many-to-one mapping,
  // so just use the first type that mapped to a particular underlying
  // type.
  //
  // This is the last thing that happens before asserting that the
  // struct type doesn't have a mapping.  Furthermore, all of the
  // builtin types are pre-built in the clang ASTContext.  So it's not
  // really a significant performance problem to just cache all them
  // right here; it makes making a few more entries in the cache than
  // we really need, but it also means we won't end up repeating these
  // stdlib lookups multiple times, and we have to perform multiple
  // lookups anyway because the MAP_BUILTIN_TYPE database uses
  // typealias names (like 'CInt') that aren't obviously associated
  // with the underlying C library type.

  auto stdlib = IGM.Context.getStdlibModule();
  assert(stdlib && "translating stdlib type to C without stdlib module?");
  auto &ctx = IGM.getClangASTContext();
  auto cacheStdlibType = [&](StringRef swiftName,
                             clang::BuiltinType::Kind builtinKind) {
    CanType swiftType = getNamedSwiftType(stdlib, swiftName);
    if (!swiftType) return;

    Cache.insert({swiftType, getClangBuiltinTypeFromKind(ctx, builtinKind)});
  };

#define MAP_BUILTIN_TYPE(CLANG_BUILTIN_KIND, SWIFT_TYPE_NAME)          \
  cacheStdlibType(#SWIFT_TYPE_NAME, clang::BuiltinType::CLANG_BUILTIN_KIND);
#include "swift/ClangImporter/BuiltinMappedTypes.def"

  // The above code sets up a bunch of mappings in the cache; just
  // assume that we hit one of them.
  auto it = Cache.find(type);
  assert(it != Cache.end() &&
         "cannot translate Swift type to C! type is not specially known");
  return it->second;
}

clang::CanQualType GenClangType::visitTupleType(CanTupleType type) {
  if (type->getNumElements() == 0)
    return getClangASTContext().VoidTy;

  llvm_unreachable("Unexpected tuple type in Clang type generation!");
}

clang::CanQualType GenClangType::visitProtocolType(CanProtocolType type) {
  auto proto = type->getDecl();

  // AnyObject -> id.
  if (proto->isSpecificProtocol(KnownProtocolKind::AnyObject))
    return getClangIdType(getClangASTContext());

  // Single protocol -> id<Proto>
  if (proto->isObjC()) {
    auto &clangCtx = getClangASTContext();
    clang::IdentifierInfo *name = &clangCtx.Idents.get(proto->getName().get());
    auto *PDecl = clang::ObjCProtocolDecl::Create(
                    const_cast<clang::ASTContext &>(clangCtx),
                    clangCtx.getTranslationUnitDecl(), name,
                    clang::SourceLocation(), clang::SourceLocation(), nullptr);

    auto clangType  = clangCtx.getObjCObjectType(clangCtx.ObjCBuiltinIdTy,
                                                 &PDecl, 1);
    auto ptrTy = clangCtx.getObjCObjectPointerType(clangType);
    return clangCtx.getCanonicalType(ptrTy);
  }

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
                          /*typeParamList*/nullptr, /*PrevDecl=*/nullptr,
                          clang::SourceLocation());
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
    UnsafeMutablePointer,
    UnsafePointer,
    AutoreleasingUnsafeMutablePointer,
    Unmanaged,
    CFunctionPointer,
  } kind = llvm::StringSwitch<StructKind>(swiftStructDecl->getName().str())
    .Case("UnsafeMutablePointer", StructKind::UnsafeMutablePointer)
    .Case("UnsafePointer", StructKind::UnsafePointer)
    .Case(
      "AutoreleasingUnsafeMutablePointer",
        StructKind::AutoreleasingUnsafeMutablePointer)
    .Case("Unmanaged", StructKind::Unmanaged)
    .Case("CFunctionPointer", StructKind::CFunctionPointer)
    .Default(StructKind::Invalid);
  
  auto args = type.getGenericArgs();

  switch (kind) {
  case StructKind::Invalid:
    llvm_unreachable("Unexpected non-pointer generic struct type in imported"
                     " Clang module!");
      
  case StructKind::UnsafeMutablePointer:
  case StructKind::Unmanaged:
  case StructKind::AutoreleasingUnsafeMutablePointer: {
    assert(args.size() == 1 &&
           "*Pointer<T> should have a single generic argument!");
    auto clangCanTy = Converter.convert(IGM, args.front());
    if (!clangCanTy) return clang::CanQualType();
    return getClangASTContext().getPointerType(clangCanTy);
  }
  case StructKind::UnsafePointer: {
    assert(args.size() == 1 &&
           "*Pointer<T> should have a single generic argument!");
    clang::QualType clangTy
      = Converter.convert(IGM, args.front()).withConst();
    return getCanonicalType(getClangASTContext().getPointerType(clangTy));
  }

  case StructKind::CFunctionPointer: {
    auto &clangCtx = getClangASTContext();

    assert(args.size() == 1 &&
           "CFunctionPointer should have a single generic argument!");
    clang::QualType functionTy;
    if (auto ft = dyn_cast<FunctionType>(args[0])) {
      functionTy = convertFunctionType(ft);
    } else {
      // Fall back to void().
      functionTy = clangCtx.getFunctionNoProtoType(clangCtx.VoidTy);
    }
    auto fnPtrTy = clangCtx.getPointerType(functionTy);
    return getCanonicalType(fnPtrTy);
  }
  }
}

clang::CanQualType GenClangType::visitEnumType(CanEnumType type) {
  assert(type->getDecl()->isObjC() && "not an @objc enum?!");
  
  // @objc enums lower to their raw types.
  return Converter.convert(IGM,
                           type->getDecl()->getRawType()->getCanonicalType());
}

clang::QualType GenClangType::convertFunctionType(CanFunctionType type) {
  auto &clangCtx = getClangASTContext();
  SmallVector<clang::QualType, 16> paramTypes;
  CanType result = type.getResult();
  CanType input = type.getInput();
  auto resultType = Converter.convert(IGM, result);
  {
    if (resultType.isNull())
      goto no_clang_type;
    
    if (auto tuple = dyn_cast<TupleType>(input)) {
      for (auto argType: tuple.getElementTypes()) {
        auto clangType = Converter.convert(IGM, argType);
        if (clangType.isNull())
          goto no_clang_type;
        paramTypes.push_back(clangType);
      }
    } else {
      auto clangType = Converter.convert(IGM, input);
      if (clangType.isNull())
        goto no_clang_type;
      paramTypes.push_back(clangType);
    }
    clang::FunctionProtoType::ExtProtoInfo DefaultEPI;
    return clangCtx.getFunctionType(resultType, paramTypes, DefaultEPI);
  }
no_clang_type:
  // Fall back to void(^)() for block types we can't convert otherwise. As long
  // as it's a pointer type it doesn't matter exactly which for either ABI type
  // generation or standard Obj-C type encoding, but protocol extended method
  // encodings will break.
  return clangCtx.getFunctionNoProtoType(clangCtx.VoidTy);
}

// FIXME: We hit this building Foundation, with a call on the type
//        encoding path. It seems like we shouldn't see FunctionType
//        at that point.
clang::CanQualType GenClangType::visitFunctionType(CanFunctionType type) {
  auto &clangCtx = getClangASTContext();

  // Convert to a Clang function type.
  auto fnTy = convertFunctionType(type);
  // Turn it into a block pointer.
  auto blockTy = clangCtx.getBlockPointerType(fnTy);
  return clangCtx.getCanonicalType(blockTy);
}

clang::CanQualType GenClangType::visitSILFunctionType(CanSILFunctionType type) {
  auto &clangCtx = getClangASTContext();

  enum FunctionPointerKind {
    Block, CFunctionPointer,
  };
  
  FunctionPointerKind kind;

  switch (type->getRepresentation()) {
  case SILFunctionType::Representation::Block:
    kind = Block;
    break;
  
  case SILFunctionType::Representation::CFunctionPointer:
    kind = CFunctionPointer;
    break;
  
  case SILFunctionType::Representation::Thick:
  case SILFunctionType::Representation::Thin:
  case SILFunctionType::Representation::Method:
  case SILFunctionType::Representation::ObjCMethod:
  case SILFunctionType::Representation::WitnessMethod:
    llvm_unreachable("not an ObjC-compatible function");
  }
  
  // Convert the return and parameter types.
  auto resultType = Converter.convert(IGM,
                type->getSemanticResultSILType().getSwiftRValueType());
  if (resultType.isNull())
    return clang::CanQualType();
  
  SmallVector<clang::QualType, 4> paramTypes;
  for (auto paramTy : type->getParametersWithoutIndirectResult()) {
    // Blocks should only take direct +0 parameters.
    switch (paramTy.getConvention()) {
    case ParameterConvention::Direct_Guaranteed:
    case ParameterConvention::Direct_Unowned:
    case ParameterConvention::Direct_Deallocating:
      // OK.
      break;

    case ParameterConvention::Direct_Owned:
      llvm_unreachable("block takes owned parameter");
    case ParameterConvention::Indirect_In:
    case ParameterConvention::Indirect_Inout:
    case ParameterConvention::Indirect_Out:
    case ParameterConvention::Indirect_In_Guaranteed:
      llvm_unreachable("block takes indirect parameter");
    }
    auto param = Converter.convert(IGM, paramTy.getType());
    if (param.isNull())
      return clang::CanQualType();
    paramTypes.push_back(param);
  }
  
  // Build the Clang function type.
  clang::FunctionProtoType::ExtProtoInfo defaultEPI;
  auto fnTy = clangCtx.getFunctionType(resultType, paramTypes, defaultEPI);
  clang::QualType ptrTy;
  
  switch (kind) {
  case Block:
    ptrTy = clangCtx.getBlockPointerType(fnTy);
    break;
  case CFunctionPointer:
    ptrTy = clangCtx.getPointerType(fnTy);
  }
  return clangCtx.getCanonicalType(ptrTy);
}

clang::CanQualType GenClangType::visitSILBlockStorageType(CanSILBlockStorageType type) {
  // We'll select (void)(^)(). This isn't correct for all blocks, but block
  // storage type should only be converted for function signature lowering,
  // where the parameter types do not matter.
  auto &clangCtx = getClangASTContext();
  auto fnTy = clangCtx.getFunctionNoProtoType(clangCtx.VoidTy);
  auto blockTy = clangCtx.getBlockPointerType(fnTy);
  return clangCtx.getCanonicalType(blockTy);
}

clang::CanQualType GenClangType::visitProtocolCompositionType(
  CanProtocolCompositionType type) {
  // FIXME. Eventually, this will have its own helper routine.
  SmallVector<const clang::ObjCProtocolDecl *, 4> Protocols;
  for (CanType t : type.getProtocols()) {
    auto opt = cast<clang::ObjCObjectPointerType>(Converter.convert(IGM, t));
    for (auto p : opt->quals())
      Protocols.push_back(p);
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

clang::CanQualType GenClangType::visitBuiltinIntegerType(
                                                   CanBuiltinIntegerType type) {
  auto &ctx = getClangASTContext();
  if (type->getWidth().isPointerWidth()) {
    return ctx.getCanonicalType(ctx.getUIntPtrType());
  }
  if (type->getWidth().isFixedWidth()) {
    auto width = type->getWidth().getFixedWidth();
    if (width == 1) return ctx.BoolTy;
    return ctx.getCanonicalType(ctx.getIntTypeForBitwidth(width, /*signed*/ 0));
  }
  llvm_unreachable("");
}

clang::CanQualType GenClangType::visitBuiltinFloatType(
                                                     CanBuiltinFloatType type) {
  auto &ctx = getClangASTContext();
  auto &clangTargetInfo = ctx.getTargetInfo();
  const llvm::fltSemantics *format = &type->getAPFloatSemantics();
  if (format == &clangTargetInfo.getHalfFormat()) return ctx.HalfTy;
  if (format == &clangTargetInfo.getFloatFormat()) return ctx.FloatTy;
  if (format == &clangTargetInfo.getDoubleFormat()) return ctx.DoubleTy;
  if (format == &clangTargetInfo.getLongDoubleFormat()) return ctx.LongDoubleTy;
  llvm_unreachable("cannot translate floating-point format to C");
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
    auto decl = nominal->getDecl();
    if (auto clangDecl = decl->getClangDecl()) {
      if (auto clangTypeDecl = dyn_cast<clang::TypeDecl>(clangDecl)) {
        auto &ctx = IGM.getClangASTContext();
        return ctx.getCanonicalType(ctx.getTypeDeclType(clangTypeDecl));
      } else if (auto ifaceDecl = dyn_cast<clang::ObjCInterfaceDecl>(clangDecl)) {
        auto &ctx = IGM.getClangASTContext();
        auto clangType  = ctx.getObjCInterfaceType(ifaceDecl);
        auto ptrTy = ctx.getObjCObjectPointerType(clangType);
        return ctx.getCanonicalType(ptrTy);
      } else if (auto protoDecl = dyn_cast<clang::ObjCProtocolDecl>(clangDecl)){
        auto &ctx = IGM.getClangASTContext();
        auto clangType  = ctx.getObjCObjectType(
                            ctx.ObjCBuiltinIdTy,
                            const_cast<clang::ObjCProtocolDecl **>(&protoDecl),
                            1);
        auto ptrTy = ctx.getObjCObjectPointerType(clangType);
        return ctx.getCanonicalType(ptrTy);
      }
    } else if (decl == IGM.Context.getBoolDecl()) {
      // FIXME: rdar://problem/21131808
      auto &ctx = IGM.getClangASTContext();
      auto &TI = ctx.getTargetInfo();
      if (TI.useSignedCharForObjCBool()) {
        return ctx.SignedCharTy;
      }
    }
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
