//===--- ClangTypeConverter.cpp - Convert Swift types to C types ----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements generation of Clang AST types from Swift AST types for
// types that are representable in Objective-C interfaces.
//
// The usage of ClangTypeConverter at the AST level means that we may
// encounter ill-formed types and/or sugared types. To avoid crashing and
// keeping sugar as much as possible (in case the generated Clang type needs
// to be surfaced to the user):
//
// 1. We fail gracefully instead of asserting/UB.
// 2. We try to keep clang sugar instead of discarding it.
//
//===----------------------------------------------------------------------===//

#include "ClangTypeConverter.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/ClangModuleLoader.h"
#include "swift/AST/ClangSwiftTypeCorrespondence.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/Module.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeVisitor.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/LLVM.h"

#include "clang/AST/ASTContext.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Sema/Sema.h"

#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/Compiler.h"

using namespace swift;

namespace {

static Type getNamedSwiftType(ModuleDecl *stdlib, StringRef name) {
  return stdlib->getASTContext().getNamedSwiftType(stdlib, name);
}

static clang::QualType
getClangBuiltinTypeFromKind(const clang::ASTContext &context,
                            clang::BuiltinType::Kind kind) {
  switch (kind) {
#define BUILTIN_TYPE(Id, SingletonId)                                          \
  case clang::BuiltinType::Id:                                                 \
    return context.SingletonId;
#include "clang/AST/BuiltinTypes.def"
#define IMAGE_TYPE(ImgType, Id, SingletonId, Access, Suffix)                   \
  case clang::BuiltinType::Id:                                                 \
    return context.SingletonId;
#include "clang/Basic/OpenCLImageTypes.def"
#define EXT_OPAQUE_TYPE(ExtType, Id, Ext)                                      \
  case clang::BuiltinType::Id:                                                 \
    return context.Id##Ty;
#include "clang/Basic/OpenCLExtensionTypes.def"
#define SVE_TYPE(Name, Id, SingletonId)                                        \
  case clang::BuiltinType::Id:                                                 \
    return context.SingletonId;
#include "clang/Basic/AArch64ACLETypes.def"
#define PPC_VECTOR_TYPE(Name, Id, Size)                                        \
  case clang::BuiltinType::Id:                                                 \
    return context.Id##Ty;
#include "clang/Basic/PPCTypes.def"
#define RVV_TYPE(Name, Id, SingletonId)                                        \
  case clang::BuiltinType::Id:                                                 \
    return context.Id##Ty;
#include "clang/Basic/RISCVVTypes.def"
#define WASM_REF_TYPE(Name, MangledNameBase, Id, SingletonId, AS)              \
  case clang::BuiltinType::Id:                                                 \
    return context.SingletonId;
#include "clang/Basic/WebAssemblyReferenceTypes.def"
#define AMDGPU_TYPE(Name, Id, SingletonId, Width, Align)                       \
  case clang::BuiltinType::Id:                                                 \
    return context.SingletonId;
#include "clang/Basic/AMDGPUTypes.def"
#define HLSL_INTANGIBLE_TYPE(Name, Id, SingletonId)                            \
  case clang::BuiltinType::Id:                                                 \
    return context.SingletonId;
#include "clang/Basic/HLSLIntangibleTypes.def"
  }

  // Not a valid BuiltinType.
  return clang::QualType();
}

static clang::QualType getClangSelectorType(
  const clang::ASTContext &clangCtx) {
  return clangCtx.getPointerType(clangCtx.ObjCBuiltinSelTy);
}

static clang::QualType getClangMetatypeType(
  const clang::ASTContext &clangCtx) {
  clang::QualType clangType =
      clangCtx.getObjCObjectType(clangCtx.ObjCBuiltinClassTy, nullptr, 0);
  return clangCtx.getObjCObjectPointerType(clangType);
}

static clang::QualType getClangIdType(
  const clang::ASTContext &clangCtx) {
  clang::QualType clangType =
      clangCtx.getObjCObjectType(clangCtx.ObjCBuiltinIdTy, nullptr, 0);
  return clangCtx.getObjCObjectPointerType(clangType);
}

static clang::QualType getClangDecayedVaListType(
const clang::ASTContext &clangCtx) {
  clang::QualType clangType = clangCtx.getBuiltinVaListType();
  if (clangType->isConstantArrayType())
    clangType = clangCtx.getDecayedType(clangType);
  return clangType;
}

} // end anonymous namespace

template <bool templateArgument>
const clang::Type *
ClangTypeConverter::getFunctionType(ArrayRef<AnyFunctionType::Param> params,
                                    Type resultTy,
                                    AnyFunctionType::Representation repr) {
  auto resultClangTy =
      templateArgument ? convertTemplateArgument(resultTy) : convert(resultTy);
  if (resultClangTy.isNull())
    return nullptr;

  SmallVector<clang::FunctionProtoType::ExtParameterInfo, 4> extParamInfos;
  SmallVector<clang::QualType, 4> paramsClangTy;
  bool someParamIsConsumed = false;
  for (auto p : params) {
    auto pc = templateArgument ? convertTemplateArgument(p.getPlainType())
                               : convert(p.getPlainType());
    if (pc.isNull())
      return nullptr;
    clang::FunctionProtoType::ExtParameterInfo extParamInfo;
    if (p.getParameterFlags().isOwned()) {
      someParamIsConsumed = true;
      extParamInfo = extParamInfo.withIsConsumed(true);
    }
    extParamInfos.push_back(extParamInfo);
    paramsClangTy.push_back(pc);
  }

  clang::FunctionProtoType::ExtProtoInfo info(clang::CallingConv::CC_C);
  if (someParamIsConsumed)
    info.ExtParameterInfos = extParamInfos.begin();
  auto fn = ClangASTContext.getFunctionType(resultClangTy, paramsClangTy, info);
  if (fn.isNull())
    return nullptr;

  switch (repr) {
  case AnyFunctionType::Representation::CFunctionPointer:
    return ClangASTContext.getPointerType(fn).getTypePtr();
  case AnyFunctionType::Representation::Block:
    return ClangASTContext.getBlockPointerType(fn).getTypePtr();
  case AnyFunctionType::Representation::Swift:
  case AnyFunctionType::Representation::Thin:
    llvm_unreachable("Expected a C-compatible representation.");
  }
  llvm_unreachable("invalid representation");
}

template <bool templateArgument>
const clang::Type *
ClangTypeConverter::getFunctionType(ArrayRef<SILParameterInfo> params,
                                    std::optional<SILResultInfo> result,
                                    SILFunctionType::Representation repr) {
  clang::QualType resultClangTy = ClangASTContext.VoidTy;
  if (result) {
    // Using the interface type is sufficient as type parameters get mapped to
    // `id`, since ObjC lightweight generics use type erasure.
    //
    // (See also: SE-0057)
    auto interfaceType = result->getInterfaceType();
    resultClangTy = templateArgument ? convertTemplateArgument(interfaceType)
                                     : convert(interfaceType);
  }

  if (resultClangTy.isNull())
    return nullptr;

  SmallVector<clang::FunctionProtoType::ExtParameterInfo, 4> extParamInfos;
  SmallVector<clang::QualType, 4> paramsClangTy;
  bool someParamIsConsumed = false;
  for (auto &p : params) {
    auto pc = templateArgument ? convertTemplateArgument(p.getInterfaceType())
                               : convert(p.getInterfaceType());
    if (pc.isNull())
      return nullptr;
    clang::FunctionProtoType::ExtParameterInfo extParamInfo;
    if (p.isConsumedInCallee()) {
      someParamIsConsumed = true;
      extParamInfo = extParamInfo.withIsConsumed(true);
    }
    extParamInfos.push_back(extParamInfo);
    paramsClangTy.push_back(pc);
  }

  clang::FunctionProtoType::ExtProtoInfo info(clang::CallingConv::CC_C);
  if (someParamIsConsumed)
    info.ExtParameterInfos = extParamInfos.begin();
  auto fn = ClangASTContext.getFunctionType(resultClangTy, paramsClangTy, info);
  if (fn.isNull())
    return nullptr;

  switch (repr) {
  case SILFunctionType::Representation::CXXMethod:
  case SILFunctionType::Representation::CFunctionPointer:
    return ClangASTContext.getPointerType(fn).getTypePtr();
  case SILFunctionType::Representation::Block:
    return ClangASTContext.getBlockPointerType(fn).getTypePtr();
  case SILFunctionType::Representation::Thick:
  case SILFunctionType::Representation::Thin:
  case SILFunctionType::Representation::Method:
  case SILFunctionType::Representation::ObjCMethod:
  case SILFunctionType::Representation::WitnessMethod:
  case SILFunctionType::Representation::Closure:
  case SILFunctionType::Representation::KeyPathAccessorGetter:
  case SILFunctionType::Representation::KeyPathAccessorSetter:
  case SILFunctionType::Representation::KeyPathAccessorEquals:
  case SILFunctionType::Representation::KeyPathAccessorHash:
    llvm_unreachable("Expected a C-compatible representation.");
  }
  llvm_unreachable("unhandled representation!");
}

clang::QualType ClangTypeConverter::convertMemberType(NominalTypeDecl *DC,
                                                      StringRef memberName) {
  auto memberTypeDecl = cast<TypeDecl>(
    DC->lookupDirect(Context.getIdentifier(memberName))[0]);
  auto memberType = memberTypeDecl->getDeclaredInterfaceType();
  return convert(memberType);
}

// TODO: It is unfortunate that we parse the name of a public library type
// in order to break it down into a vector component and length that in theory
// we could recover in some other way.
static clang::QualType getClangVectorType(const clang::ASTContext &ctx,
                                          clang::BuiltinType::Kind eltKind,
                                          clang::VectorKind vecKind,
                                          StringRef numEltsString) {
  unsigned numElts;
  bool failedParse = numEltsString.getAsInteger<unsigned>(10, numElts);
  if (failedParse)
    return clang::QualType();
  auto eltTy = getClangBuiltinTypeFromKind(ctx, eltKind);
  if (eltTy.isNull())
    return clang::QualType();
  return ctx.getVectorType(eltTy, numElts, vecKind);
}

clang::QualType
ClangTypeConverter::reverseImportedTypeMapping(StructType *type) {
  auto &ctx = ClangASTContext;

  auto swiftDecl = type->getDecl();
  StringRef name = swiftDecl->getName().str();

  // We assume that the importer translates all of the following types
  // directly to structs in the standard library.

  // We want to recognize most of these types by name.
#define CHECK_NAMED_TYPE(NAME, CLANG_TYPE) do {                         \
    if (name == (NAME)) return CLANG_TYPE;                              \
  } while (false)

  CHECK_NAMED_TYPE("CGFloat", convertMemberType(swiftDecl, "NativeType"));
  CHECK_NAMED_TYPE("OpaquePointer", ctx.VoidPtrTy);
  CHECK_NAMED_TYPE("CVaListPointer", getClangDecayedVaListType(ctx));
  CHECK_NAMED_TYPE("DarwinBoolean", ctx.UnsignedCharTy);
  CHECK_NAMED_TYPE(swift::getSwiftName(
                     KnownFoundationEntity::NSZone),
                   ctx.VoidPtrTy);
  CHECK_NAMED_TYPE("WindowsBool", ctx.IntTy);
  CHECK_NAMED_TYPE("ObjCBool", ctx.ObjCBuiltinBoolTy);
  CHECK_NAMED_TYPE("Selector", getClangSelectorType(ctx));
  CHECK_NAMED_TYPE("UnsafeRawPointer", ctx.VoidPtrTy);
  CHECK_NAMED_TYPE("UnsafeMutableRawPointer", ctx.VoidPtrTy);
#undef CHECK_NAMED_TYPE

  // Map vector types to the corresponding C vectors.
#define MAP_SIMD_TYPE(TYPE_NAME, _, BUILTIN_KIND)                              \
  if (name.starts_with(#TYPE_NAME)) {                                          \
    return getClangVectorType(ctx, clang::BuiltinType::BUILTIN_KIND,           \
                              clang::VectorKind::Generic,                      \
                              name.drop_front(sizeof(#TYPE_NAME) - 1));        \
  }
#include "swift/ClangImporter/SIMDMappedTypes.def"

  // This is not an imported type (according to the name)
  return clang::QualType();
}

clang::QualType ClangTypeConverter::visitStructType(StructType *type) {
  auto importedType = reverseImportedTypeMapping(type);
  if (!importedType.isNull())
    return importedType;

  // We might be looking at a builtin
  auto builtinType = reverseBuiltinTypeMapping(type);
  if (!builtinType.isNull())
    return builtinType;

  if (type->isPotentiallyBridgedValueType())
    if (auto t = Context.getBridgedToObjC(type->getDecl(), type))
      return convert(t);

  // Out of ideas, there must've been some error. :(
  return clang::QualType();
}

static clang::QualType
getClangBuiltinTypeFromTypedef(clang::Sema &sema, StringRef typedefName) {
  auto &context = sema.getASTContext();
  auto identifier = &context.Idents.get(typedefName);
  auto found = sema.LookupSingleName(sema.TUScope, identifier,
                                     clang::SourceLocation(),
                                     clang::Sema::LookupOrdinaryName);
  auto typedefDecl = dyn_cast_or_null<clang::TypedefDecl>(found);
  if (!typedefDecl)
    return clang::QualType();

  auto underlyingTy =
    context.getCanonicalType(typedefDecl->getUnderlyingType());

  if (underlyingTy->getAs<clang::BuiltinType>())
    return underlyingTy;
  return clang::QualType();
}

clang::QualType
ClangTypeConverter::reverseBuiltinTypeMapping(StructType *type) {
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

  auto stdlib = Context.getStdlibModule();
  assert(stdlib && "translating stdlib type to C without stdlib module?");
  auto &ctx = ClangASTContext;

  if (!StdlibTypesAreCached) {
    auto cacheStdlibType = [&](StringRef swiftName,
                               clang::BuiltinType::Kind builtinKind) {
      Type swiftType = getNamedSwiftType(stdlib, swiftName);
      if (!swiftType) return;

      auto &sema = Context.getClangModuleLoader()->getClangSema();

      if (Context.LangOpts.EnableObjCInterop) {
        // Handle Int and UInt specially. On Apple platforms, these map to
        // the NSInteger and NSUInteger typedefs. So try that if the typedefs
        // are available, to ensure we get consistent ObjC @encode strings.
        if (swiftType->isInt()) {
          auto NSIntegerTy = getClangBuiltinTypeFromTypedef(sema, "NSInteger");
          if (!NSIntegerTy.isNull()) {
            Cache.insert({swiftType->getCanonicalType(), NSIntegerTy});
            return;
          }
        } else if (swiftType->isUInt()) {
          auto NSUIntegerTy = getClangBuiltinTypeFromTypedef(sema, "NSUInteger");
          if (!NSUIntegerTy.isNull()) {
            Cache.insert({swiftType->getCanonicalType(), NSUIntegerTy});
            return;
          }
        }
      }

      // For something like `typealias CInt = Int32`, reverseBuiltinTypeMapping
      // will get Int32 as the input, so we need to record the desugared type.
      Cache.insert({swiftType->getCanonicalType(),
                    getClangBuiltinTypeFromKind(ctx, builtinKind)});
    };

#define MAP_BUILTIN_TYPE(CLANG_BUILTIN_KIND, SWIFT_TYPE_NAME)          \
    cacheStdlibType(#SWIFT_TYPE_NAME, clang::BuiltinType::CLANG_BUILTIN_KIND);
#include "swift/ClangImporter/BuiltinMappedTypes.def"

    // On 64-bit Windows, no C type is imported as an Int or UInt; CLong is
    // imported as an Int32 and CLongLong as an Int64. Therefore, manually
    // add mappings to C for Int and UInt.
    // On 64-bit Cygwin, no manual mapping is required.
    if (Triple.isOSWindows() && Triple.isArch64Bit()
        && !Triple.isWindowsCygwinEnvironment()) {
      // Map UInt to uintptr_t
      auto swiftUIntType = getNamedSwiftType(stdlib, "UInt");
      auto clangUIntPtrType = ctx.getCanonicalType(ctx.getUIntPtrType());
      Cache.insert({swiftUIntType, clangUIntPtrType});

      // Map Int to intptr_t
      auto swiftIntType = getNamedSwiftType(stdlib, "Int");
      auto clangIntPtrType = ctx.getCanonicalType(ctx.getIntPtrType());
      Cache.insert({swiftIntType, clangIntPtrType});
    }
    StdlibTypesAreCached = true;
  }

  auto it = Cache.find(Type(type));
  if (it != Cache.end())
    return it->second;

  it = Cache.find(type->getCanonicalType());
  if (it != Cache.end()) {
    Cache.insert({Type(type), it->second});
    return it->second;
  }

  return clang::QualType();
}

clang::QualType ClangTypeConverter::visitTupleType(TupleType *type) {
  unsigned tupleNumElements = type->getNumElements();
  if (tupleNumElements == 0)
    return ClangASTContext.VoidTy;

  Type eltTy = type->getElementType(0);
  for (unsigned i = 1; i < tupleNumElements; ++i) {
    if (!eltTy->isEqual(type->getElementType(i)))
      // Only tuples where all element types are equal map to fixed-size
      // arrays.
      return clang::QualType();
  }

  auto clangEltTy = convert(eltTy);
  if (clangEltTy.isNull())
    return clang::QualType();

  APInt size(32, tupleNumElements);
  return ClangASTContext.getConstantArrayType(
      clangEltTy, size, nullptr, clang::ArraySizeModifier::Normal, 0);
}

clang::QualType ClangTypeConverter::visitProtocolType(ProtocolType *type) {
  auto proto = type->getDecl();
  auto &clangCtx = ClangASTContext;

  // Strip 'Sendable'.
  auto strippedType = type->stripConcurrency(false, false);
  if (strippedType.getPointer() != type)
    return convert(strippedType);

  if (!proto->isObjC())
    return clang::QualType();

  assert(!cast_or_null<clang::ObjCProtocolDecl>(proto->getClangDecl())
         && "We shouldn't be creating duplicate decls; see `convert`");

  // Single protocol -> id<Proto>
  clang::IdentifierInfo *name = &clangCtx.Idents.get(proto->getName().get());
  auto *PDecl = clang::ObjCProtocolDecl::Create(
                  const_cast<clang::ASTContext &>(clangCtx),
                  clangCtx.getTranslationUnitDecl(), name,
                  clang::SourceLocation(), clang::SourceLocation(), nullptr);

  // Attach an objc_runtime_name attribute with the Objective-C name to use
  // for this protocol.
  SmallString<64> runtimeNameBuffer;
  PDecl->addAttr(clang::ObjCRuntimeNameAttr::CreateImplicit(
                   PDecl->getASTContext(),
                   proto->getObjCRuntimeName(runtimeNameBuffer)));

  registerExportedClangDecl(proto, PDecl);

  auto clangType  = clangCtx.getObjCObjectType(clangCtx.ObjCBuiltinIdTy,
                                               &PDecl, 1);
  return clangCtx.getObjCObjectPointerType(clangType);
}

// TODO: [stronger-checking-in-clang-type-conversion]
// Metatypes can be converted to Class when they are metatypes for concrete
// classes. https://github.com/apple/swift/pull/27479#discussion_r344418131
clang::QualType ClangTypeConverter::visitMetatypeType(MetatypeType *type) {
  return getClangMetatypeType(ClangASTContext);
}

// TODO: [stronger-checking-in-clang-type-conversion]
// Existential metatypes where the base is a non-metatype existential can be
// converted to Class<P, Q, ...> when the protocols are all ObjC.
// https://github.com/apple/swift/pull/27479#discussion_r344418131
clang::QualType
ClangTypeConverter::visitExistentialMetatypeType(ExistentialMetatypeType *type) {
  return getClangMetatypeType(ClangASTContext);
}

clang::QualType ClangTypeConverter::visitClassType(ClassType *type) {
  auto &clangCtx = ClangASTContext;
  auto swiftDecl = type->getDecl();

  // TODO: [non-objc-class-clang-type-conversion]
  // See the corresponding note in GenClangType.cpp
  if (!swiftDecl->isObjC())
    return getClangIdType(clangCtx);

  assert(!cast_or_null<clang::ObjCInterfaceDecl>(swiftDecl->getClangDecl())
         && "We shouldn't be creating duplicate decls; see `convert`");

  // produce the clang type INTF * if it is imported ObjC object.
  clang::IdentifierInfo *ForwardClassId =
    &clangCtx.Idents.get(swiftDecl->getName().get());
  auto *CDecl = clang::ObjCInterfaceDecl::Create(
                        clangCtx, clangCtx.getTranslationUnitDecl(),
                        clang::SourceLocation(), ForwardClassId,
                        /*typeParamList*/nullptr, /*PrevDecl=*/nullptr,
                        clang::SourceLocation());

  // Attach an objc_runtime_name attribute with the Objective-C name to use
  // for this class.
  SmallString<64> runtimeNameBuffer;
  CDecl->addAttr(clang::ObjCRuntimeNameAttr::CreateImplicit(
                   CDecl->getASTContext(),
                   swiftDecl->getObjCRuntimeName(runtimeNameBuffer)));

  registerExportedClangDecl(swiftDecl, CDecl);

  auto clangType  = clangCtx.getObjCInterfaceType(CDecl);
  return clangCtx.getObjCObjectPointerType(clangType);
}

// TODO: We should try to preserve type arguments on imported ObjC generic
// classes, instead of relying on our knowledge of clang's encoding.
// This would entail extracting the type arguments, calling `convert` to
// create clang types, extracting the ObjCProtocolDecls and then using
// getObjCObjectType with `id` as the base.
clang::QualType
ClangTypeConverter::visitBoundGenericClassType(BoundGenericClassType *type) {
  // Any @objc class type in Swift that shows up in an @objc method maps 1-1 to
  // "id <SomeProto>"; with clang's encoding ignoring the protocol list.
  return getClangIdType(ClangASTContext);
}

clang::QualType
ClangTypeConverter::visitBoundGenericType(BoundGenericType *type) {
  // The only supported conversions are for T?, SIMD*<T>, and *Pointer<T>,
  // so there should only be a single generic type argument.
  if (type->getGenericArgs().size() != 1)
    return clang::QualType();

  auto argType = type->getGenericArgs()[0]->getCanonicalType();

  if (type->getDecl()->isOptionalDecl()) {
    auto innerTy = convert(argType);
    if (swift::canImportAsOptional(innerTy.getTypePtrOrNull()) ||
        argType->isForeignReferenceType())
      return innerTy;
    return clang::QualType();
  }

  if (auto kind = classifyPointer(type))
    return convertPointerType</*templateArgument=*/false>(argType,
                                                          kind.value());

  if (auto width = classifySIMD(type))
    return convertSIMDType</*templateArgument=*/false>(argType, width.value());

  return clang::QualType();
}

template <bool templateArgument>
clang::QualType ClangTypeConverter::convertSIMDType(CanType scalarType,
                                                    unsigned width) {
  clang::QualType scalarTy = templateArgument
                                 ? convertTemplateArgument(scalarType)
                                 : convert(scalarType);
  if (scalarTy.isNull())
    return clang::QualType();

  auto vectorTy = ClangASTContext.getVectorType(scalarTy, width,
                                                clang::VectorKind::Generic);
  return vectorTy;
}

template <bool templateArgument>
clang::QualType ClangTypeConverter::convertPointerType(CanType pointeeType,
                                                       PointerKind kind) {
  switch (kind) {
  case PointerKind::Unmanaged:
    return templateArgument ? clang::QualType() : convert(pointeeType);

  case PointerKind::AutoreleasingUnsafeMutablePointer:
    if (templateArgument)
      return clang::QualType();
    LLVM_FALLTHROUGH;

  case PointerKind::UnsafeMutablePointer: {
    auto clangTy = templateArgument ? convertTemplateArgument(pointeeType)
                                    : convert(pointeeType);
    if (clangTy.isNull())
      return clang::QualType();
    return ClangASTContext.getPointerType(clangTy);
  }
  case PointerKind::UnsafePointer: {
    auto clangTy = templateArgument ? convertTemplateArgument(pointeeType)
                                    : convert(pointeeType);
    if (clangTy.isNull())
      return clang::QualType();
    return ClangASTContext.getPointerType(clangTy.withConst());
  }

  case PointerKind::CFunctionPointer: {
    if (templateArgument)
      return clang::QualType();

    auto &clangCtx = ClangASTContext;

    clang::QualType functionTy;
    if (isa<SILFunctionType>(pointeeType->getCanonicalType())) {
      functionTy = convert(pointeeType);
      if (functionTy.isNull())
        return clang::QualType();
    } else {
      // Fall back to void().
      functionTy = clangCtx.getFunctionNoProtoType(clangCtx.VoidTy);
    }
    return clangCtx.getPointerType(functionTy);
  }
  }

  llvm_unreachable("Not a valid StructKind.");
}

clang::QualType ClangTypeConverter::visitEnumType(EnumType *type) {
  // Special case: Uninhabited enums are not @objc, so we don't
  // know what to do below, but we can just convert to 'void'.
  if (type->isUninhabited())
    return convert(Context.TheEmptyTupleType);

  if (!type->getDecl()->isObjC())
    // Can't translate something not marked with @objc
    return clang::QualType();

  // @objc enums lower to their raw types.
  return convert(type->getDecl()->getRawType());
}

template <bool templateArgument>
clang::QualType ClangTypeConverter::visitFunctionType(FunctionType *type) {
  const clang::Type *clangTy = nullptr;
  auto repr = type->getRepresentation();
  bool useClangTypes = type->getASTContext().LangOpts.UseClangFunctionTypes;
  if (useClangTypes && (getSILFunctionLanguage(convertRepresentation(repr)) ==
                        SILFunctionLanguage::C)) {
    clangTy = type->getClangTypeInfo().getType();
  } else if (!useClangTypes || repr == FunctionTypeRepresentation::Swift) {
    // C function pointer types themselves are not bridged but their components
    // can be. If a component is an @convention(block) function, it may be
    // bridged to a Swift function type.
    auto newRepr = (repr == FunctionTypeRepresentation::Swift
                        ? FunctionTypeRepresentation::Block
                        : repr);
    clangTy = getFunctionType<templateArgument>(type->getParams(),
                                                type->getResult(), newRepr);
  }
  return clang::QualType(clangTy, 0);
}

template <bool templateArgument>
clang::QualType
ClangTypeConverter::visitSILFunctionType(SILFunctionType *type) {
  const clang::Type *clangTy = nullptr;
  auto repr = type->getRepresentation();
  bool useClangTypes = type->getASTContext().LangOpts.UseClangFunctionTypes;
  if (useClangTypes &&
      (getSILFunctionLanguage(repr) == SILFunctionLanguage::C)) {
    clangTy = type->getClangTypeInfo().getType();
  } else if (!useClangTypes || repr == SILFunctionTypeRepresentation::Thick) {
    // C function pointer types themselves are not bridged but their components
    // can be. If a component is an @convention(block) function, it may be
    // bridged to a Swift function type.
    auto newRepr = (repr == SILFunctionTypeRepresentation::Thick
                        ? SILFunctionTypeRepresentation::Block
                        : repr);
    auto results = type->getResults();
    auto optionalResult = results.empty()
                              ? std::nullopt
                              : std::optional<SILResultInfo>(results[0]);
    clangTy = getFunctionType<templateArgument>(type->getParameters(),
                                                optionalResult, newRepr);
  }
  return clang::QualType(clangTy, 0);
}

clang::QualType
ClangTypeConverter::visitSILBlockStorageType(SILBlockStorageType *type) {
  // We'll select (void)(^)(). This isn't correct for all blocks, but block
  // storage type should only be converted for function signature lowering,
  // where the parameter types do not matter.
  auto &clangCtx = ClangASTContext;
  auto fnTy = clangCtx.getFunctionNoProtoType(clangCtx.VoidTy);
  auto blockTy = clangCtx.getBlockPointerType(fnTy);
  return clangCtx.getCanonicalType(blockTy);
}

clang::QualType
ClangTypeConverter::visitProtocolCompositionType(ProtocolCompositionType *type) {
  // Strip 'Sendable'.
  auto strippedType = type->stripConcurrency(false, false);
  if (strippedType.getPointer() != type)
    return convert(strippedType);

  // Any will be lowered to AnyObject, so we return the same result.
  if (type->isAny())
    return getClangIdType(ClangASTContext);

  auto &clangCtx = ClangASTContext;

  // FIXME. Eventually, this will have its own helper routine.
  SmallVector<const clang::ObjCProtocolDecl *, 4> Protocols;
  auto layout = type->getExistentialLayout();
  if (!layout.isObjC())
    // Cannot represent opaque existential in Clang
    return clang::QualType();

  // AnyObject -> id.
  if (layout.isAnyObject())
    return getClangIdType(ClangASTContext);

  auto superclassTy = clangCtx.ObjCBuiltinIdTy;
  if (auto layoutSuperclassTy = layout.getSuperclass()) {
    auto clangTy = convert(layoutSuperclassTy);
    if (clangTy.isNull())
      return clang::QualType();
    superclassTy = clangCtx.getCanonicalType(
      clangTy->getAs<clang::ObjCObjectPointerType>()->getPointeeType());
  }

  for (ProtocolDecl *proto : layout.getProtocols()) {
    auto clangTy = convert(proto->getDeclaredInterfaceType());
    if (clangTy.isNull())
      return clang::QualType();
    for (auto p : clangTy->getAs<clang::ObjCObjectPointerType>()->quals())
      Protocols.push_back(p);
  }

  if (Protocols.empty())
    return superclassTy;

  // id<protocol-list>
  clang::ObjCProtocolDecl **ProtoQuals =
    new(clangCtx) clang::ObjCProtocolDecl*[Protocols.size()];
  memcpy(ProtoQuals, Protocols.data(),
         sizeof(clang::ObjCProtocolDecl*)*Protocols.size());
  auto clangType = clangCtx.getObjCObjectType(superclassTy,
                                              ProtoQuals,
                                              Protocols.size());
  return clangCtx.getObjCObjectPointerType(clangType);
}

clang::QualType
ClangTypeConverter::visitExistentialType(ExistentialType *type) {
  return visit(type->getConstraintType());
}

clang::QualType
ClangTypeConverter::visitBuiltinRawPointerType(BuiltinRawPointerType *type) {
  return ClangASTContext.VoidPtrTy;
}

clang::QualType
ClangTypeConverter::visitBuiltinIntegerType(BuiltinIntegerType *type) {
  auto &clangCtx = ClangASTContext;
  if (type->getWidth().isPointerWidth()) {
    return clangCtx.getUIntPtrType();
  }
  assert(type->getWidth().isFixedWidth());
  auto width = type->getWidth().getFixedWidth();
  if (width == 1)
    return clangCtx.BoolTy;
  return clangCtx.getIntTypeForBitwidth(width, /*signed*/ 0);
}

clang::QualType
ClangTypeConverter::visitBuiltinFloatType(BuiltinFloatType *type) {
  auto &clangCtx = ClangASTContext;
  auto &clangTargetInfo = clangCtx.getTargetInfo();
  const llvm::fltSemantics *format = &type->getAPFloatSemantics();
  if (format == &clangTargetInfo.getHalfFormat())
    return clangCtx.HalfTy;
  if (format == &clangTargetInfo.getFloatFormat())
    return clangCtx.FloatTy;
  if (format == &clangTargetInfo.getDoubleFormat())
    return clangCtx.DoubleTy;
  if (format == &clangTargetInfo.getLongDoubleFormat())
    return clangCtx.LongDoubleTy;
  llvm_unreachable("cannot translate floating-point format to C");
}

clang::QualType ClangTypeConverter::visitArchetypeType(ArchetypeType *type) {
  // We see these in the case where we invoke an @objc function
  // through a protocol.
  return getClangIdType(ClangASTContext);
}

clang::QualType ClangTypeConverter::visitDependentMemberType(DependentMemberType *type) {
  return convert(type->getBase());
}

clang::QualType ClangTypeConverter::visitDynamicSelfType(DynamicSelfType *type) {
  // Dynamic Self is equivalent to 'instancetype', which is treated as
  // 'id' within the Objective-C type system.
  return getClangIdType(ClangASTContext);
}

clang::QualType
ClangTypeConverter::visitGenericTypeParamType(GenericTypeParamType *type) {
  // We see these in the case where we invoke an @objc function
  // through a protocol argument that is a generic type.
  return getClangIdType(ClangASTContext);
}

clang::QualType
ClangTypeConverter::visitSugarType(SugarType *type) {
  return convert(Type(type->getDesugaredType()));
}

clang::QualType
ClangTypeConverter::visitType(TypeBase *type) {
  // We only convert specific types.
  return clang::QualType();
}

clang::QualType ClangTypeConverter::visit(Type type) {
  return static_cast<super *>(this)->visit(type);
}

clang::QualType ClangTypeConverter::convert(Type type) {
  auto it = Cache.find(type);
  if (it != Cache.end())
    return it->second;

  if (auto existential = type->getAs<ExistentialType>())
    type = existential->getConstraintType();

  // Try to do this without making cache entries for obvious cases.
  if (auto nominal = type->getAs<NominalType>()) {
    auto decl = nominal->getDecl();
    if (auto clangDecl = decl->getClangDecl()) {
      auto qualType = convertClangDecl(type, clangDecl);
      if (!qualType.isNull())
        return qualType;
    }
  }

  // If that failed, convert the type, cache, and return.
  clang::QualType result = visit(type);
  Cache.insert({type, result});
  return result;
}

clang::QualType
ClangTypeConverter::convertClangDecl(Type type, const clang::Decl *clangDecl) {
  auto &ctx = ClangASTContext;

  if (auto clangTypeDecl = dyn_cast<clang::TypeDecl>(clangDecl)) {
    auto qualType = ctx.getTypeDeclType(clangTypeDecl);
    if (type->isForeignReferenceType())
      qualType = ctx.getPointerType(qualType);

    return qualType.getUnqualifiedType();
  }

  if (auto ifaceDecl = dyn_cast<clang::ObjCInterfaceDecl>(clangDecl)) {
    auto clangType = ctx.getObjCInterfaceType(ifaceDecl);
    return ctx.getObjCObjectPointerType(clangType);
  }

  if (auto protoDecl = dyn_cast<clang::ObjCProtocolDecl>(clangDecl)) {
    auto clangType = ctx.getObjCObjectType(
        ctx.ObjCBuiltinIdTy, const_cast<clang::ObjCProtocolDecl **>(&protoDecl),
        1);
    return ctx.getObjCObjectPointerType(clangType);
  }

  // Unable to convert this ClangDecl; give up
  return clang::QualType();
}

void ClangTypeConverter::registerExportedClangDecl(Decl *swiftDecl,
                                             const clang::Decl *clangDecl) {
  assert(clangDecl->isCanonicalDecl() &&
         "generated Clang declaration for Swift declaration should not "
         "have multiple declarations");
  ReversedExportMap.insert({clangDecl, swiftDecl});
}

Decl *ClangTypeConverter::getSwiftDeclForExportedClangDecl(
                                             const clang::Decl *decl) const {
  // We don't need to canonicalize the declaration because these exported
  // declarations are never redeclarations.
  auto it = ReversedExportMap.find(decl);
  return (it != ReversedExportMap.end() ? it->second : nullptr);
}

clang::QualType ClangTypeConverter::convertTemplateArgument(Type type) {
  auto withCache = [&](auto conversion) {
    auto cached = Cache.find(type);
    if (cached != Cache.end())
      return cached->second;

    // Cache miss; perform the conversion and cache successful results
    auto result = conversion();

    if (!result.isNull())
      Cache.insert({type, result});
    return result;
  };

  // This type was imported from Clang, so we can convert it back by retrieving
  // ClangDecl stored in the imported type decl (without making a cache entry.)
  if (auto nominal = type->getAs<NominalType>())
    if (auto clangDecl = nominal->getDecl()->getClangDecl())
      return convertClangDecl(type, clangDecl);

  if (auto pointerType = type->getAs<BuiltinRawPointerType>())
    return withCache([&]() { return visitBuiltinRawPointerType(pointerType); });

  if (auto integerType = type->getAs<BuiltinIntegerType>())
    return withCache([&]() { return visitBuiltinIntegerType(integerType); });

  if (auto floatType = type->getAs<BuiltinFloatType>())
    return withCache([&]() { return visitBuiltinFloatType(floatType); });

  if (auto tupleType = type->getAs<TupleType>()) {
    // We do not call visitTupleType() because we cannot yet handle tuples with
    // a non-zero number of elements.
    if (tupleType->getNumElements() == 0)
      return ClangASTContext.VoidTy;
  }

  if (auto structType = type->getAs<StructType>()) {
    // Swift structs are not supported in general, but some foreign types are
    // imported as Swift structs. We reverse that mapping here.
    auto decl = structType->getDecl();

    // Ban ObjCBool type from being substituted into C++ templates (#74790)
    if (decl->getName().is("ObjCBool") &&
        decl->getModuleContext()->getName() ==
            decl->getASTContext().Id_ObjectiveC)
      return clang::QualType();

    auto importedType =
        withCache([&]() { return reverseImportedTypeMapping(structType); });

    if (!importedType.isNull())
      return importedType;

    return withCache([&]() { return reverseBuiltinTypeMapping(structType); });
  }

  if (auto boundGenericType = type->getAs<BoundGenericType>()) {
    if (boundGenericType->getGenericArgs().size() != 1)
      // Must've got something other than a T?, *Pointer<T>, or SIMD*<T>
      return clang::QualType();

    auto argType = boundGenericType->getGenericArgs()[0]->getCanonicalType();

    if (boundGenericType->getDecl()->isOptionalDecl()) {
      if (auto kind = classifyPointer(argType))
        return withCache([&]() {
          auto pointeeType = argType->getAs<BoundGenericType>()
                                 ->getGenericArgs()[0]
                                 ->getCanonicalType();
          return convertPointerType</*templateArgument=*/true>(pointeeType,
                                                               kind.value());
        });

      // Arbitrary optional types are not (yet) supported
      return clang::QualType();
    }

    if (auto kind = classifyPointer(boundGenericType))
      return withCache([&]() {
        return convertPointerType</*templateArgument=*/true>(argType,
                                                             kind.value());
      });

    if (auto width = classifySIMD(boundGenericType))
      return withCache([&]() {
        return convertSIMDType</*templateArgument=*/true>(argType,
                                                          width.value());
      });

    return clang::QualType();
  }

  if (auto functionType = type->getAs<FunctionType>()) {
    return withCache([&]() {
      return visitFunctionType</*templateArgument=*/true>(functionType);
    });
  }

  if (auto functionType = type->getAs<SILFunctionType>()) {
    return withCache([&]() {
      return visitSILFunctionType</*templateArgument=*/true>(functionType);
    });
  }

  // Most types cannot be used to instantiate C++ function templates; give up.
  return clang::QualType();
}

std::unique_ptr<TemplateInstantiationError>
ClangTypeConverter::getClangTemplateArguments(
    const clang::TemplateParameterList *templateParams,
    ArrayRef<Type> genericArgs,
    SmallVectorImpl<clang::TemplateArgument> &templateArgs) {
  assert(templateArgs.size() == 0);

  // Keep track of the types we failed to convert so we can return a useful
  // error.
  SmallVector<Type, 2> failedTypes;
  for (clang::NamedDecl *param : *templateParams) {
    // Note: all template parameters must be template type parameters. This is
    // verified when we import the Clang decl.
    auto templateParam = cast<clang::TemplateTypeParmDecl>(param);
    // We must have found a defaulted parameter at the end of the list.
    if (templateParam->getIndex() >= genericArgs.size()) {
      templateArgs.push_back(clang::TemplateArgument(
          templateParam->getDefaultArgument().getArgument()));
      continue;
    }

    auto replacement = genericArgs[templateParam->getIndex()];

    auto qualType = convertTemplateArgument(replacement);

    if (qualType.isNull())
      failedTypes.push_back(replacement);
    else
      templateArgs.push_back(clang::TemplateArgument(qualType));
  }
  if (failedTypes.empty())
    return nullptr;
  // Clear "templateArgs" to prevent the clients from accidentally reading a
  // partially converted set of template arguments.
  templateArgs.clear();
  auto errorInfo = std::make_unique<TemplateInstantiationError>();
  llvm::for_each(failedTypes, [&errorInfo](auto type) {
    errorInfo->failedTypes.push_back(type);
  });
  return errorInfo;
}

std::optional<ClangTypeConverter::PointerKind>
ClangTypeConverter::classifyPointer(Type type) {
  auto generic = type->getAs<BoundGenericType>();
  if (!generic || generic->getGenericArgs().size() != 1)
    // Must have got something other than a *Pointer<T>
    return std::nullopt;

  return llvm::StringSwitch<std::optional<PointerKind>>(
             generic->getDecl()->getName().str())
      .Case("UnsafeMutablePointer", PointerKind::UnsafeMutablePointer)
      .Case("UnsafePointer", PointerKind::UnsafePointer)
      .Case("AutoreleasingUnsafeMutablePointer",
            PointerKind::AutoreleasingUnsafeMutablePointer)
      .Case("Unmanaged", PointerKind::Unmanaged)
      .Case("CFunctionPointer", PointerKind::CFunctionPointer)
      .Default(std::nullopt);
}

std::optional<unsigned> ClangTypeConverter::classifySIMD(Type type) {
  auto generic = type->getAs<BoundGenericType>();
  if (!generic || generic->getGenericArgs().size() != 1)
    // Must have got something other than a SIMD*<T>
    return std::nullopt;

  auto name = generic->getDecl()->getName().str();
  if (!name.starts_with("SIMD"))
    return std::nullopt;
  name.consume_front("SIMD");

  unsigned width;
  if (/*failed to*/ name.getAsInteger<unsigned>(10, width))
    return std::nullopt;
  return width;
}
