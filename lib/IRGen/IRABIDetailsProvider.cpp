//===--- IRABIDetailsProvider.cpp - Get ABI details for decls ---*- C++ -*-===//
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

#include "swift/IRGen/IRABIDetailsProvider.h"
#include "Callee.h"
#include "FixedTypeInfo.h"
#include "GenEnum.h"
#include "GenType.h"
#include "GenericRequirement.h"
#include "IRGen.h"
#include "IRGenModule.h"
#include "NativeConventionSchema.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/Types.h"
#include "swift/SIL/SILFunctionBuilder.h"
#include "swift/SIL/SILModule.h"
#include "swift/Subsystems.h"
#include "clang/CodeGen/ModuleBuilder.h"
#include "clang/CodeGen/SwiftCallingConv.h"
#include "llvm/IR/DerivedTypes.h"

using namespace swift;
using namespace irgen;

static Optional<Type> getPrimitiveTypeFromLLVMType(ASTContext &ctx,
                                                   const llvm::Type *type) {
  if (const auto *intType = dyn_cast<llvm::IntegerType>(type)) {
    switch (intType->getBitWidth()) {
    case 8:
      return ctx.getUInt8Type();
    case 16:
      return ctx.getUInt16Type();
    case 32:
      return ctx.getUInt32Type();
    case 64:
      return ctx.getUInt64Type();
    default:
      return None;
    }
  } else if (type->isFloatTy()) {
    return ctx.getFloatType();
  } else if (type->isDoubleTy()) {
    return ctx.getDoubleType();
  } else if (type->isPointerTy()) {
    return ctx.getOpaquePointerType();
  }
  // FIXME: Handle vector type.
  return None;
}

namespace swift {

class IRABIDetailsProviderImpl {
public:
  IRABIDetailsProviderImpl(ModuleDecl &mod, const IRGenOptions &opts)
      : typeConverter(mod),
        silMod(SILModule::createEmptyModule(&mod, typeConverter, silOpts)),
        IRGen(opts, *silMod), IGM(IRGen, IRGen.createTargetMachine()) {}

  llvm::Optional<IRABIDetailsProvider::SizeAndAlignment>
  getTypeSizeAlignment(const NominalTypeDecl *TD) {
    auto *TI = &IGM.getTypeInfoForUnlowered(TD->getDeclaredTypeInContext());
    auto *fixedTI = dyn_cast<FixedTypeInfo>(TI);
    if (!fixedTI)
      return None;
    return IRABIDetailsProvider::SizeAndAlignment{
        fixedTI->getFixedSize().getValue(),
        fixedTI->getFixedAlignment().getValue()};
  }

  bool shouldPassIndirectly(Type type) {
    auto *TI = &IGM.getTypeInfoForUnlowered(type);
    NativeConventionSchema schema(IGM, TI, /*isResult=*/false);
    return schema.requiresIndirect();
  }

  bool shouldReturnIndirectly(Type type) {
    if (type->isVoid())
      return false;
    auto *TI = &IGM.getTypeInfoForUnlowered(type);
    NativeConventionSchema schema(IGM, TI, /*isResult=*/true);
    return schema.requiresIndirect();
  }

  bool enumerateDirectPassingRecordMembers(
      Type t, llvm::function_ref<void(clang::CharUnits, clang::CharUnits, Type)>
                  callback) {
    auto *TI = &IGM.getTypeInfoForUnlowered(t);
    NativeConventionSchema schema(IGM, TI, /*isResult=*/false);
    bool hasError = false;
    schema.enumerateComponents(
        [&](clang::CharUnits offset, clang::CharUnits end, llvm::Type *type) {
          auto primitiveType = getPrimitiveTypeFromLLVMType(
              IGM.getSwiftModule()->getASTContext(), type);
          if (!primitiveType) {
            hasError = true;
            return;
          }
          callback(offset, end, *primitiveType);
        });
    return hasError;
  }

  IRABIDetailsProvider::FunctionABISignature
  getTypeMetadataAccessFunctionSignature() {
    auto &ctx = IGM.getSwiftModule()->getASTContext();
    llvm::StructType *responseTy = IGM.getTypeMetadataResponseTy();
    IRABIDetailsProvider::TypeRecordABIRepresentation::MemberVectorTy members;
    for (auto *elementTy : responseTy->elements())
      members.push_back(*getPrimitiveTypeFromLLVMType(ctx, elementTy));
    auto returnTy =
        IRABIDetailsProvider::TypeRecordABIRepresentation(std::move(members));
    auto paramTy = IRABIDetailsProvider::TypeRecordABIRepresentation(
        {*getPrimitiveTypeFromLLVMType(ctx,
                                       IGM.getTypeMetadataRequestParamTy())});
    return {returnTy, {paramTy}};
  }

  SmallVector<GenericRequirement, 2>
  getTypeMetadataAccessFunctionGenericRequirementParameters(
      NominalTypeDecl *nominal) {
    GenericTypeRequirements requirements(IGM, nominal);
    SmallVector<GenericRequirement, 2> result;
    for (const auto &req : requirements.getRequirements())
      result.push_back(req);
    return result;
  }

  llvm::MapVector<EnumElementDecl *, IRABIDetailsProvider::EnumElementInfo>
  getEnumTagMapping(const EnumDecl *ED) {
    llvm::MapVector<EnumElementDecl *, IRABIDetailsProvider::EnumElementInfo>
        elements;
    auto &enumImplStrat =
        getEnumImplStrategy(IGM, ED->getDeclaredType()->getCanonicalType());

    for (auto *element : ED->getAllElements()) {
      auto tagIdx = enumImplStrat.getTagIndex(element);
      auto *global = cast<llvm::GlobalVariable>(
          IGM.getAddrOfEnumCase(element, NotForDefinition).getAddress());
      elements.insert({element, {tagIdx, global->getName()}});
    }

    return elements;
  }

  llvm::SmallVector<IRABIDetailsProvider::ABIAdditionalParam, 1>
  getFunctionABIAdditionalParams(AbstractFunctionDecl *afd) {
    llvm::SmallVector<IRABIDetailsProvider::ABIAdditionalParam, 1> params;

    auto function = SILFunction::getFunction(SILDeclRef(afd), *silMod);

    auto silFuncType = function->getLoweredFunctionType();
    auto funcPointerKind =
        FunctionPointerKind(FunctionPointerKind::BasicKind::Function);

    auto signature = Signature::getUncached(IGM, silFuncType, funcPointerKind,
                                            /*shouldComputeABIDetail=*/true);

    using ABIAdditionalParam = IRABIDetailsProvider::ABIAdditionalParam;
    using ParamRole = ABIAdditionalParam::ABIParameterRole;
    for (const auto &typeSource :
         signature.getABIDetails().polymorphicSignatureExpandedTypeSources) {
      typeSource.visit(
          [&](const GenericRequirement &reqt) {
            params.push_back(ABIAdditionalParam(ParamRole::GenericRequirement,
                                                reqt, CanType()));
          },
          [&](const MetadataSource &metadataSource) {
            auto index = metadataSource.getParamIndex();
            auto canType =
                silFuncType->getParameters()[index].getInterfaceType();
            params.push_back(ABIAdditionalParam(
                ParamRole::GenericTypeMetadataSource, llvm::None, canType));
          });
    }
    for (auto attrSet : signature.getAttributes()) {
      if (attrSet.hasAttribute(llvm::Attribute::AttrKind::SwiftSelf))
        params.push_back(
            ABIAdditionalParam(ParamRole::Self, llvm::None, CanType()));
      if (attrSet.hasAttribute(llvm::Attribute::AttrKind::SwiftError))
        params.push_back(
            ABIAdditionalParam(ParamRole::Error, llvm::None, CanType()));
    }
    return params;
  }

private:
  Lowering::TypeConverter typeConverter;
  // Default silOptions are sufficient, as we don't need to generated SIL.
  SILOptions silOpts;
  std::unique_ptr<SILModule> silMod;
  IRGenerator IRGen;
  IRGenModule IGM;
};

} // namespace swift

IRABIDetailsProvider::IRABIDetailsProvider(ModuleDecl &mod,
                                           const IRGenOptions &opts)
    : impl(std::make_unique<IRABIDetailsProviderImpl>(mod, opts)) {}

IRABIDetailsProvider::~IRABIDetailsProvider() {}

llvm::Optional<IRABIDetailsProvider::SizeAndAlignment>
IRABIDetailsProvider::getTypeSizeAlignment(const NominalTypeDecl *TD) {
  return impl->getTypeSizeAlignment(TD);
}

llvm::SmallVector<IRABIDetailsProvider::ABIAdditionalParam, 1>
IRABIDetailsProvider::getFunctionABIAdditionalParams(
    AbstractFunctionDecl *afd) {
  return impl->getFunctionABIAdditionalParams(afd);
}

bool IRABIDetailsProvider::shouldPassIndirectly(Type t) {
  return impl->shouldPassIndirectly(t);
}

bool IRABIDetailsProvider::shouldReturnIndirectly(Type t) {
  return impl->shouldReturnIndirectly(t);
}

bool IRABIDetailsProvider::enumerateDirectPassingRecordMembers(
    Type t, llvm::function_ref<void(clang::CharUnits, clang::CharUnits, Type)>
                callback) {
  return impl->enumerateDirectPassingRecordMembers(t, callback);
}

IRABIDetailsProvider::FunctionABISignature
IRABIDetailsProvider::getTypeMetadataAccessFunctionSignature() {
  return impl->getTypeMetadataAccessFunctionSignature();
}

SmallVector<GenericRequirement, 2>
IRABIDetailsProvider::getTypeMetadataAccessFunctionGenericRequirementParameters(
    NominalTypeDecl *nominal) {
  return impl->getTypeMetadataAccessFunctionGenericRequirementParameters(
      nominal);
}

llvm::MapVector<EnumElementDecl *, IRABIDetailsProvider::EnumElementInfo>
IRABIDetailsProvider::getEnumTagMapping(const EnumDecl *ED) {
  return impl->getEnumTagMapping(ED);
}
