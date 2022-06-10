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
#include "FixedTypeInfo.h"
#include "GenType.h"
#include "IRGen.h"
#include "IRGenModule.h"
#include "NativeConventionSchema.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/Types.h"
#include "swift/SIL/SILModule.h"
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
