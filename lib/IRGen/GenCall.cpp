//===--- GenCall.cpp - Swift IR Generation for Function Calls -------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements IR generation for function signature lowering
//  in Swift.  This includes creating the IR type, collecting IR attributes,
//  performing calls, and supporting prologue and epilogue emission.
//
//===----------------------------------------------------------------------===//

#include "swift/ABI/MetadataValues.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ClangModuleLoader.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/Basic/Assertions.h"
#include "swift/IRGen/Linking.h"
#include "swift/Runtime/Config.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILType.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/GlobalDecl.h"
#include "clang/AST/RecordLayout.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/CodeGen/CodeGenABITypes.h"
#include "clang/CodeGen/ModuleBuilder.h"
#include "clang/Sema/Sema.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/IR/GlobalPtrAuthInfo.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/Support/Compiler.h"
#include <optional>

#include "CallEmission.h"
#include "EntryPointArgumentEmission.h"
#include "Explosion.h"
#include "GenCall.h"
#include "GenFunc.h"
#include "GenHeap.h"
#include "GenKeyPath.h"
#include "GenObjC.h"
#include "GenPointerAuth.h"
#include "GenPoly.h"
#include "GenProto.h"
#include "GenType.h"
#include "IRGenDebugInfo.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "IRGenMangler.h"
#include "LoadableTypeInfo.h"
#include "NativeConventionSchema.h"
#include "Signature.h"
#include "StructLayout.h"

using namespace swift;
using namespace irgen;

static Size getYieldOnceCoroutineBufferSize(IRGenModule &IGM) {
  return NumWords_YieldOnceBuffer * IGM.getPointerSize();
}
static Alignment getYieldOnceCoroutineBufferAlignment(IRGenModule &IGM) {
  return IGM.getPointerAlignment();
}

static Size getYieldManyCoroutineBufferSize(IRGenModule &IGM) {
  return NumWords_YieldManyBuffer * IGM.getPointerSize();
}
static Alignment getYieldManyCoroutineBufferAlignment(IRGenModule &IGM) {
  return IGM.getPointerAlignment();
}

static Size getCoroutineContextSize(IRGenModule &IGM,
                                    CanSILFunctionType fnType) {
  switch (fnType->getCoroutineKind()) {
  case SILCoroutineKind::None:
    llvm_unreachable("expand a coroutine");
  case SILCoroutineKind::YieldOnce2:
    if (IGM.IRGen.Opts.EmitYieldOnce2AsYieldOnce) {
      LLVM_FALLTHROUGH;
    } else {
      llvm::report_fatal_error(
          "callee allocated coroutines do not have fixed-size buffers");
    }
  case SILCoroutineKind::YieldOnce:
    return getYieldOnceCoroutineBufferSize(IGM);
  case SILCoroutineKind::YieldMany:
    return getYieldManyCoroutineBufferSize(IGM);
  }
  llvm_unreachable("bad kind");
}

AsyncContextLayout irgen::getAsyncContextLayout(IRGenModule &IGM,
                                                SILFunction *function) {
  SubstitutionMap forwardingSubstitutionMap =
      function->getForwardingSubstitutionMap();
  CanSILFunctionType originalType = function->getLoweredFunctionType();
  CanSILFunctionType substitutedType = originalType->substGenericArgs(
      IGM.getSILModule(), forwardingSubstitutionMap,
      IGM.getMaximalTypeExpansionContext());
  auto layout = getAsyncContextLayout(
      IGM, originalType, substitutedType, forwardingSubstitutionMap);
  return layout;
}

static Size getAsyncContextHeaderSize(IRGenModule &IGM) {
  return 2 * IGM.getPointerSize();
}

AsyncContextLayout irgen::getAsyncContextLayout(
    IRGenModule &IGM, CanSILFunctionType originalType,
    CanSILFunctionType substitutedType, SubstitutionMap substitutionMap) {
  // FIXME: everything about this type is way more complicated than it
  // needs to be now that we no longer pass and return things in memory
  // in the async context and therefore the layout is totally static.

  SmallVector<const TypeInfo *, 4> typeInfos;
  SmallVector<SILType, 4> valTypes;

  // AsyncContext * __ptrauth_swift_async_context_parent Parent;
  {
    auto ty = SILType();
    auto &ti = IGM.getSwiftContextPtrTypeInfo();
    valTypes.push_back(ty);
    typeInfos.push_back(&ti);
  }

  // TaskContinuationFunction * __ptrauth_swift_async_context_resume
  //     ResumeParent;
  {
    auto ty = SILType();
    auto &ti = IGM.getTaskContinuationFunctionPtrTypeInfo();
    valTypes.push_back(ty);
    typeInfos.push_back(&ti);
  }

  return AsyncContextLayout(IGM, LayoutStrategy::Optimal, valTypes, typeInfos,
                            originalType, substitutedType, substitutionMap);
}

AsyncContextLayout::AsyncContextLayout(
    IRGenModule &IGM, LayoutStrategy strategy, ArrayRef<SILType> fieldTypes,
    ArrayRef<const TypeInfo *> fieldTypeInfos, CanSILFunctionType originalType,
    CanSILFunctionType substitutedType, SubstitutionMap substitutionMap)
    : StructLayout(IGM, /*type=*/std::nullopt, LayoutKind::NonHeapObject,
                   strategy, fieldTypeInfos, /*typeToFill*/ nullptr),
      originalType(originalType), substitutedType(substitutedType),
      substitutionMap(substitutionMap) {
  assert(fieldTypeInfos.size() == fieldTypes.size() &&
         "type infos don't match types");
  assert(this->isFixedLayout());
  assert(this->getSize() == getAsyncContextHeaderSize(IGM));
}

Alignment IRGenModule::getAsyncContextAlignment() const {
  return Alignment(MaximumAlignment);
}

std::optional<Size>
FunctionPointerKind::getStaticAsyncContextSize(IRGenModule &IGM) const {
  if (!isSpecial())
    return std::nullopt;

  auto headerSize = getAsyncContextHeaderSize(IGM);
  headerSize = headerSize.roundUpToAlignment(IGM.getPointerAlignment());

  switch (getSpecialKind()) {
  case SpecialKind::TaskFutureWaitThrowing:
  case SpecialKind::TaskFutureWait:
  case SpecialKind::AsyncLetWait:
  case SpecialKind::AsyncLetWaitThrowing:
  case SpecialKind::AsyncLetGet:
  case SpecialKind::AsyncLetGetThrowing:
  case SpecialKind::AsyncLetFinish:
  case SpecialKind::TaskGroupWaitNext:
  case SpecialKind::TaskGroupWaitAll:
  case SpecialKind::DistributedExecuteTarget:
    // The current guarantee for all of these functions is the same.
    // See TaskFutureWaitAsyncContext.
    //
    // If you add a new special runtime function, it is highly recommended
    // that you make calls to it allocate a little more memory than this!
    // These frames being this small is very arguably a mistake.
    return headerSize + 3 * IGM.getPointerSize();
  case SpecialKind::KeyPathAccessor:
    return std::nullopt;
  }
  llvm_unreachable("covered switch");
}

void IRGenFunction::setupAsync(unsigned asyncContextIndex) {
  llvm::Value *c = CurFn->getArg(asyncContextIndex);
  asyncContextLocation = createAlloca(c->getType(), IGM.getPointerAlignment());

  IRBuilder builder(IGM.getLLVMContext(), IGM.DebugInfo != nullptr);
  // Insert the stores after the coro.begin.
  builder.SetInsertPoint(getEarliestInsertionPoint()->getParent(),
                         getEarliestInsertionPoint()->getIterator());
  builder.CreateStore(c, asyncContextLocation);
}

llvm::Value *IRGenFunction::getAsyncTask() {
  auto call = Builder.CreateCall(IGM.getGetCurrentTaskFunctionPointer(), {});
  call->setDoesNotThrow();
  call->setCallingConv(IGM.SwiftCC);
  return call;
}

llvm::Value *IRGenFunction::getAsyncContext() {
  assert(isAsync());
  return Builder.CreateLoad(asyncContextLocation);
}

void IRGenFunction::storeCurrentAsyncContext(llvm::Value *context) {
  context = Builder.CreateBitCast(context, IGM.SwiftContextPtrTy);
  Builder.CreateStore(context, asyncContextLocation);
}

llvm::CallInst *IRGenFunction::emitSuspendAsyncCall(
    unsigned asyncContextIndex, llvm::StructType *resultTy,
    ArrayRef<llvm::Value *> args, bool restoreCurrentContext) {
  auto *id = Builder.CreateIntrinsicCall(llvm::Intrinsic::coro_suspend_async,
                                         {resultTy}, args);
  if (restoreCurrentContext) {
    // This is setup code after the split point. Don't associate any line
    // numbers to it.
    irgen::PrologueLocation LocRAII(IGM.DebugInfo.get(), Builder);
    llvm::Value *calleeContext =
        Builder.CreateExtractValue(id, asyncContextIndex);
    calleeContext =
        Builder.CreateBitOrPointerCast(calleeContext, IGM.Int8PtrTy);
    llvm::Function *projectFn = cast<llvm::Function>(
        (cast<llvm::Constant>(args[2])->stripPointerCasts()));
    auto *fnTy = projectFn->getFunctionType();

    llvm::Value *context =
        Builder.CreateCallWithoutDbgLoc(fnTy, projectFn, {calleeContext});
    storeCurrentAsyncContext(context);
  }

  return id;
}

llvm::Type *ExplosionSchema::getScalarResultType(IRGenModule &IGM) const {
  if (size() == 0) {
    return IGM.VoidTy;
  } else if (size() == 1) {
    return begin()->getScalarType();
  } else {
    SmallVector<llvm::Type*, 16> elts;
    for (auto &elt : *this) elts.push_back(elt.getScalarType());
    return llvm::StructType::get(IGM.getLLVMContext(), elts);
  }
}

static void addDereferenceableAttributeToBuilder(IRGenModule &IGM,
                                                 llvm::AttrBuilder &b,
                                                 const TypeInfo &ti) {
  // The addresses of empty values are undefined, so we can't safely mark them
  // dereferenceable.
  if (ti.isKnownEmpty(ResilienceExpansion::Maximal))
    return;
  
  // If we know the type to have a fixed nonempty size, then the pointer is
  // dereferenceable to at least that size.
  // TODO: Would be nice to have a "getMinimumKnownSize" on TypeInfo for
  // dynamic-layout aggregates.
  if (auto fixedTI = dyn_cast<FixedTypeInfo>(&ti)) {
    b.addAttribute(
      llvm::Attribute::getWithDereferenceableBytes(IGM.getLLVMContext(),
                                         fixedTI->getFixedSize().getValue()));
  }
}

static void addIndirectValueParameterAttributes(IRGenModule &IGM,
                                                llvm::AttributeList &attrs,
                                                const TypeInfo &ti,
                                                unsigned argIndex) {
  llvm::AttrBuilder b(IGM.getLLVMContext());
  // Value parameter pointers can't alias or be captured.
  b.addAttribute(llvm::Attribute::NoAlias);
  // Bitwise takable value types are guaranteed not to capture
  // a pointer into itself.
  if (ti.isBitwiseTakable(ResilienceExpansion::Maximal))
    b.addAttribute(llvm::Attribute::NoCapture);
  // The parameter must reference dereferenceable memory of the type.
  addDereferenceableAttributeToBuilder(IGM, b, ti);

  attrs = attrs.addParamAttributes(IGM.getLLVMContext(), argIndex, b);
}

static void addPackParameterAttributes(IRGenModule &IGM,
                                       SILType paramSILType,
                                       llvm::AttributeList &attrs,
                                       unsigned argIndex) {
  llvm::AttrBuilder b(IGM.getLLVMContext());
  // Pack parameter pointers can't alias.
  // Note: they are not marked `nocapture` as one
  // pack parameter could be a value type (e.g. a C++ type)
  // that captures its own pointer in itself.
  b.addAttribute(llvm::Attribute::NoAlias);
  // TODO: we could mark this dereferenceable when the pack has fixed
  // components.
  // TODO: add an alignment attribute
  // TODO: add a nonnull attribute

  attrs = attrs.addParamAttributes(IGM.getLLVMContext(), argIndex, b);
}

static void addInoutParameterAttributes(IRGenModule &IGM, SILType paramSILType,
                                        llvm::AttributeList &attrs,
                                        const TypeInfo &ti, unsigned argIndex,
                                        bool aliasable) {
  llvm::AttrBuilder b(IGM.getLLVMContext());
  // Thanks to exclusivity checking, it is not possible to alias inouts except
  // those that are inout_aliasable.
  if (!aliasable && paramSILType.getASTType()->getAnyPointerElementType()) {
    // To ward against issues with LLVM's alias analysis, for now, only add the
    // attribute if it's a pointer being passed inout.
    b.addAttribute(llvm::Attribute::NoAlias);
  }
  // Bitwise takable value types are guaranteed not to capture
  // a pointer into itself.
  if (ti.isBitwiseTakable(ResilienceExpansion::Maximal))
    b.addAttribute(llvm::Attribute::NoCapture);
  // The inout must reference dereferenceable memory of the type.
  addDereferenceableAttributeToBuilder(IGM, b, ti);

  attrs = attrs.addParamAttributes(IGM.getLLVMContext(), argIndex, b);
}

static llvm::CallingConv::ID getFreestandingConvention(IRGenModule &IGM) {
  // TODO: use a custom CC that returns three scalars efficiently
  return IGM.SwiftCC;
}

/// Expand the requirements of the given abstract calling convention
/// into a "physical" calling convention.
llvm::CallingConv::ID irgen::expandCallingConv(IRGenModule &IGM,
                                    SILFunctionTypeRepresentation convention,
                                    bool isAsync) {
  switch (convention) {
  case SILFunctionTypeRepresentation::CFunctionPointer:
  case SILFunctionTypeRepresentation::ObjCMethod:
  case SILFunctionTypeRepresentation::CXXMethod:
  case SILFunctionTypeRepresentation::Block:
    return IGM.getOptions().PlatformCCallingConvention;

  case SILFunctionTypeRepresentation::Method:
  case SILFunctionTypeRepresentation::WitnessMethod:
  case SILFunctionTypeRepresentation::Closure:
  case SILFunctionTypeRepresentation::Thin:
  case SILFunctionTypeRepresentation::Thick:
  case SILFunctionTypeRepresentation::KeyPathAccessorGetter:
  case SILFunctionTypeRepresentation::KeyPathAccessorSetter:
  case SILFunctionTypeRepresentation::KeyPathAccessorEquals:
  case SILFunctionTypeRepresentation::KeyPathAccessorHash:
    if (isAsync)
      return IGM.SwiftAsyncCC;
    return getFreestandingConvention(IGM);
  }
  llvm_unreachable("bad calling convention!");
}

static void addIndirectResultAttributes(IRGenModule &IGM,
                                        llvm::AttributeList &attrs,
                                        unsigned paramIndex, bool allowSRet,
                                        llvm::Type *storageType,
                                        const TypeInfo &typeInfo,
                                        bool useInReg = false) {
  llvm::AttrBuilder b(IGM.getLLVMContext());
  b.addAttribute(llvm::Attribute::NoAlias);
  // Bitwise takable value types are guaranteed not to capture
  // a pointer into itself.
  if (typeInfo.isBitwiseTakable(ResilienceExpansion::Maximal))
    b.addAttribute(llvm::Attribute::NoCapture);
  if (allowSRet) {
    assert(storageType);
    b.addStructRetAttr(storageType);
    if (useInReg)
      b.addAttribute(llvm::Attribute::InReg);
  }
  attrs = attrs.addParamAttributes(IGM.getLLVMContext(), paramIndex, b);
}

// This function should only be called with directly returnable
// result and error types. Errors can only be returned directly if
// they consists solely of int and ptr values.
CombinedResultAndErrorType irgen::combineResultAndTypedErrorType(
    const IRGenModule &IGM, const NativeConventionSchema &resultSchema,
    const NativeConventionSchema &errorSchema) {
  assert(!resultSchema.requiresIndirect());
  assert(!errorSchema.shouldReturnTypedErrorIndirectly());

  CombinedResultAndErrorType result;
  SmallVector<llvm::Type *, 8> elts;
  resultSchema.enumerateComponents(
      [&](clang::CharUnits offset, clang::CharUnits end, llvm::Type *type) {
        elts.push_back(type);
      });

  SmallVector<llvm::Type *, 8> errorElts;
  errorSchema.enumerateComponents(
      [&](clang::CharUnits offset, clang::CharUnits end, llvm::Type *type) {
        errorElts.push_back(type);
      });

  llvm::SmallVector<llvm::Type *, 4> combined;

  auto resIt = elts.begin();
  auto errorIt = errorElts.begin();

  while (resIt < elts.end() && errorIt < errorElts.end()) {
    auto *res = *resIt;
    if (!res->isIntOrPtrTy()) {
      combined.push_back(res);
      ++resIt;
      continue;
    }

    auto *error = *errorIt;
    assert(error->isIntOrPtrTy() &&
           "Direct errors must only consist of int or ptr values");
    result.errorValueMapping.push_back(combined.size());

    if (res == error) {
      combined.push_back(res);
    } else {
      auto maxSize = std::max(IGM.DataLayout.getTypeSizeInBits(res),
                              IGM.DataLayout.getTypeSizeInBits(error));
      combined.push_back(llvm::IntegerType::get(IGM.getLLVMContext(), maxSize));
    }

    ++resIt;
    ++errorIt;
  }

  while (resIt < elts.end()) {
    combined.push_back(*resIt);
    ++resIt;
  }

  while (errorIt < errorElts.end()) {
    result.errorValueMapping.push_back(combined.size());
    combined.push_back(*errorIt);
    ++errorIt;
  }

  if (combined.empty()) {
    result.combinedTy = llvm::Type::getVoidTy(IGM.getLLVMContext());
  } else if (combined.size() == 1) {
    result.combinedTy = combined[0];
  } else {
    result.combinedTy =
        llvm::StructType::get(IGM.getLLVMContext(), combined, /*packed*/ false);
  }

  return result;
}

void IRGenModule::addSwiftAsyncContextAttributes(llvm::AttributeList &attrs,
                                                 unsigned argIndex) {
  llvm::AttrBuilder b(getLLVMContext());
  b.addAttribute(llvm::Attribute::SwiftAsync);
  attrs = attrs.addParamAttributes(this->getLLVMContext(), argIndex, b);
}

void IRGenModule::addSwiftSelfAttributes(llvm::AttributeList &attrs,
                                         unsigned argIndex) {
  llvm::AttrBuilder b(getLLVMContext());
  b.addAttribute(llvm::Attribute::SwiftSelf);
  attrs = attrs.addParamAttributes(this->getLLVMContext(), argIndex, b);
}

void IRGenModule::addSwiftErrorAttributes(llvm::AttributeList &attrs,
                                          unsigned argIndex) {
  llvm::AttrBuilder b(getLLVMContext());
  // Don't add the swifterror attribute on ABIs that don't pass it in a register.
  // We create a shadow stack location of the swifterror parameter for the
  // debugger on such platforms and so we can't mark the parameter with a
  // swifterror attribute.
  if (ShouldUseSwiftError)
    b.addAttribute(llvm::Attribute::SwiftError);
  
  // The error result should not be aliased, captured, or pointed at invalid
  // addresses regardless.
  b.addAttribute(llvm::Attribute::NoAlias);
  b.addAttribute(llvm::Attribute::NoCapture);
  b.addDereferenceableAttr(getPointerSize().getValue());

  attrs = attrs.addParamAttributes(this->getLLVMContext(), argIndex, b);
}

void irgen::addByvalArgumentAttributes(IRGenModule &IGM,
                                       llvm::AttributeList &attrs,
                                       unsigned argIndex, Alignment align,
                                       llvm::Type *storageType) {
  llvm::AttrBuilder b(IGM.getLLVMContext());
  b.addByValAttr(storageType);
  b.addAttribute(llvm::Attribute::getWithAlignment(
      IGM.getLLVMContext(), llvm::Align(align.getValue())));
  attrs = attrs.addParamAttributes(IGM.getLLVMContext(), argIndex, b);
}

static llvm::Attribute::AttrKind attrKindForExtending(bool signExtend) {
  if (signExtend)
    return llvm::Attribute::SExt;
  return llvm::Attribute::ZExt;
}

namespace swift {
namespace irgen {
namespace {
  class SignatureExpansion {
    IRGenModule &IGM;
    CanSILFunctionType FnType;
    bool forStaticCall = false; // Used for objc_method (direct call or not).

    // Indicates this is a c++ constructor call.
    const clang::CXXConstructorDecl *cxxCtorDecl = nullptr;

  public:
    SmallVector<llvm::Type*, 8> ParamIRTypes;
    llvm::Type *ResultIRType = nullptr;
    llvm::AttributeList Attrs;
    ForeignFunctionInfo ForeignInfo;
    CoroutineInfo CoroInfo;
    bool CanUseSRet = true;
    bool CanUseError = true;
    bool CanUseSelf = true;
    unsigned AsyncContextIdx;
    unsigned AsyncResumeFunctionSwiftSelfIdx = 0;
    FunctionPointerKind FnKind;

    SignatureExpansion(IRGenModule &IGM, CanSILFunctionType fnType,
                       FunctionPointerKind fnKind, bool forStaticCall = false,
                       const clang::CXXConstructorDecl *cxxCtorDecl = nullptr)
        : IGM(IGM), FnType(fnType), forStaticCall(forStaticCall),
          cxxCtorDecl(cxxCtorDecl), FnKind(fnKind) {}

    /// Expand the components of the primary entrypoint of the function type.
    void expandFunctionType(
        SignatureExpansionABIDetails *recordedABIDetails = nullptr);

    /// Expand the components of the continuation entrypoint of the
    /// function type.
    void expandCoroutineContinuationType();

    // Expand the components for the async continuation entrypoint of the
    // function type (the function to be called on returning).
    void expandAsyncReturnType();

    // Expand the components for the async suspend call of the function type.
    void expandAsyncAwaitType();

    // Expand the components for the primary entrypoint of the async function
    // type.
    void expandAsyncEntryType();

    Signature getSignature();

  private:
    const TypeInfo &expand(SILParameterInfo param);
    llvm::Type *addIndirectResult(SILType resultType, bool useInReg = false);

    SILFunctionConventions getSILFuncConventions() const {
      return SILFunctionConventions(FnType, IGM.getSILModule());
    }

    unsigned getCurParamIndex() {
      return ParamIRTypes.size();
    }

    bool claimSRet() {
      bool result = CanUseSRet;
      CanUseSRet = false;
      return result;
    }

    bool claimSelf() {
      auto Ret = CanUseSelf;
      assert(CanUseSelf && "Multiple self parameters?!");
      CanUseSelf = false;
      return Ret;
    }

    bool claimError() {
      auto Ret = CanUseError;
      assert(CanUseError && "Multiple error parameters?!");
      CanUseError = false;
      return Ret;
    }

    /// Add a pointer to the given type as the next parameter.
    void addPointerParameter(llvm::Type *storageType) {
      ParamIRTypes.push_back(storageType->getPointerTo());
    }

    void addCoroutineContextParameter();
    void addAsyncParameters();

    void expandResult(SignatureExpansionABIDetails *recordedABIDetails);
    /// Returns the LLVM type pointer and its type info for
    /// the direct result of this function. If the result is passed indirectly,
    /// a void type is returned instead, with a \c null type info.
    std::pair<llvm::Type *, const TypeInfo *> expandDirectResult();
    std::pair<llvm::Type *, const TypeInfo *> expandDirectErrorType();
    void expandIndirectResults();
    void expandParameters(SignatureExpansionABIDetails *recordedABIDetails);
    void expandKeyPathAccessorParameters();
    void expandExternalSignatureTypes();

    void expandCoroutineResult(bool forContinuation);
    void expandCoroutineContinuationParameters();

    void addIndirectThrowingResult();
    llvm::Type *getErrorRegisterType();
  };
} // end anonymous namespace
} // end namespace irgen
} // end namespace swift

llvm::Type *SignatureExpansion::addIndirectResult(SILType resultType,
                                                  bool useInReg) {
  const TypeInfo &resultTI = IGM.getTypeInfo(resultType);
  auto storageTy = resultTI.getStorageType();
  addIndirectResultAttributes(IGM, Attrs, ParamIRTypes.size(), claimSRet(),
                              storageTy, resultTI, useInReg);
  addPointerParameter(storageTy);
  return IGM.VoidTy;
}

/// Expand all of the direct and indirect result types.
void SignatureExpansion::expandResult(
    SignatureExpansionABIDetails *recordedABIDetails) {
  if (FnType->isAsync()) {
    // The result will be stored within the SwiftContext that is passed to async
    // functions.
    ResultIRType = IGM.VoidTy;
    return;
  }
  if (FnType->isCoroutine()) {
    // This should be easy enough to support if we need to: use the
    // same algorithm but add the direct results to the results as if
    // they were unioned in.
    return expandCoroutineResult(/*for continuation*/ false);
  }

  auto fnConv = getSILFuncConventions();

  // Disable the use of sret if we have multiple indirect results.
  if (fnConv.getNumIndirectSILResults() > 1)
    CanUseSRet = false;

  // Ensure that no parameters were added before to correctly record their ABI
  // details.
  assert(ParamIRTypes.empty());
  // Expand the direct result.
  const TypeInfo *directResultTypeInfo;
  std::tie(ResultIRType, directResultTypeInfo) = expandDirectResult();

  if (!fnConv.hasIndirectSILResults() && !fnConv.hasIndirectSILErrorResults()) {
    llvm::Type *directErrorType;
    const TypeInfo *directErrorTypeInfo;
    std::tie(directErrorType, directErrorTypeInfo) = expandDirectErrorType();
    if ((directResultTypeInfo || ResultIRType->isVoidTy()) &&
        directErrorTypeInfo) {
      ResultIRType = directErrorType;
      directResultTypeInfo = directErrorTypeInfo;
    }
  }

  // Expand the indirect results.
  expandIndirectResults();

  // Record ABI details if asked.
  if (!recordedABIDetails)
    return;
  if (directResultTypeInfo)
    recordedABIDetails->directResult =
        SignatureExpansionABIDetails::DirectResult{*directResultTypeInfo};
  for (unsigned i = 0; i < ParamIRTypes.size(); ++i) {
    bool hasSRet = Attrs.hasParamAttr(i, llvm::Attribute::StructRet);
    recordedABIDetails->indirectResults.push_back(
        SignatureExpansionABIDetails::IndirectResult{hasSRet});
  }
}

void SignatureExpansion::expandIndirectResults() {
  auto fnConv = getSILFuncConventions();
  // Expand the indirect results.
  for (auto indirectResultType :
       fnConv.getIndirectSILResultTypes(IGM.getMaximalTypeExpansionContext())) {
    auto storageTy = IGM.getStorageType(indirectResultType);
    auto useSRet = claimSRet();
    // We need to use opaque types or non fixed size storage types because llvm
    // does type based analysis based on the type of sret arguments.
    const TypeInfo &typeInfo = IGM.getTypeInfo(indirectResultType);
    if (useSRet && !isa<FixedTypeInfo>(typeInfo)) {
      storageTy = IGM.OpaqueTy;
    }
    addIndirectResultAttributes(IGM, Attrs, ParamIRTypes.size(), useSRet,
                                storageTy, typeInfo);
    addPointerParameter(storageTy);
  }
}

namespace {
  class YieldSchema {
    SILType YieldTy;
    const TypeInfo &YieldTI;
    std::optional<NativeConventionSchema> NativeSchema;
    bool IsIndirect;
  public:
    YieldSchema(IRGenModule &IGM, SILFunctionConventions fnConv,
                SILYieldInfo yield)
        : YieldTy(
              fnConv.getSILType(yield, IGM.getMaximalTypeExpansionContext())),
          YieldTI(IGM.getTypeInfo(YieldTy)) {
      if (isFormalIndirect()) {
        IsIndirect = true;
      } else {
        NativeSchema.emplace(IGM, &YieldTI, /*result*/ true);
        IsIndirect = NativeSchema->requiresIndirect();
      }
    }

    SILType getSILType() const {
      return YieldTy;
    }

    const TypeInfo &getTypeInfo() const {
      return YieldTI;
    }

    /// Should the yielded value be yielded as a pointer?
    bool isIndirect() const { return IsIndirect; }

    /// Is the yielded value formally indirect?
    bool isFormalIndirect() const { return YieldTy.isAddress(); }

    llvm::PointerType *getIndirectPointerType() const {
      assert(isIndirect());
      return YieldTI.getStorageType()->getPointerTo();
    }

    const NativeConventionSchema &getDirectSchema() const {
      assert(!isIndirect());
      return *NativeSchema;
    }

    void enumerateDirectComponents(llvm::function_ref<void(llvm::Type*)> fn) {
      getDirectSchema().enumerateComponents([&](clang::CharUnits begin,
                                                clang::CharUnits end,
                                                llvm::Type *componentTy) {
        fn(componentTy);
      });
    }
  };
}

void SignatureExpansion::expandCoroutineResult(bool forContinuation) {
  // The return type may be different for the ramp function vs. the
  // continuations.
  if (forContinuation) {
    switch (FnType->getCoroutineKind()) {
    case SILCoroutineKind::None:
      llvm_unreachable("should have been filtered out before here");

    // Yield-once coroutines may optionaly return a value from the continuation.
    case SILCoroutineKind::YieldOnce:
    case SILCoroutineKind::YieldOnce2: {
      // Ensure that no parameters were added before to correctly record their ABI
      // details.
      assert(ParamIRTypes.empty());

      // Expand the direct result.
      const TypeInfo *directResultTypeInfo;
      std::tie(ResultIRType, directResultTypeInfo) = expandDirectResult();

      return;
    }

    // Yield-many coroutines yield the same types from the continuation
    // as they do from the ramp function.
    case SILCoroutineKind::YieldMany:
      assert(FnType->getNumResults() == 0 &&
             "having both normal and yield results is currently unsupported");
      break;
    }
  }

  SmallVector<llvm::Type*, 8> components;

  // The continuation pointer.
  components.push_back(IGM.Int8PtrTy);

  auto fnConv = getSILFuncConventions();
  for (auto yield : FnType->getYields()) {
    YieldSchema schema(IGM, fnConv, yield);

    // If the individual value must be yielded indirectly, add a pointer.
    if (schema.isIndirect()) {
      components.push_back(schema.getIndirectPointerType());
      continue;
    }

    // Otherwise, collect all the component types.
    schema.enumerateDirectComponents([&](llvm::Type *type) {
      components.push_back(type);
    });
  }

  // Find the maximal sequence of the component types that we can
  // convince the ABI to pass directly.
  // When counting components, ignore the continuation pointer.
  unsigned numDirectComponents = components.size() - 1;
  SmallVector<llvm::Type*, 8> overflowTypes;
  while (clang::CodeGen::swiftcall::
                shouldPassIndirectly(IGM.ClangCodeGen->CGM(), components,
                                     /*asReturnValue*/ true)) {
    // If we added a pointer to the end of components, remove it.
    if (!overflowTypes.empty()) components.pop_back();

    // Remove the last component and add it as an overflow type.
    overflowTypes.push_back(components.pop_back_val());
    --numDirectComponents;

    // Add a pointer to the end of components.
    components.push_back(IGM.Int8PtrTy);
  }

  // We'd better have been able to pass at least two pointers.
  assert(components.size() >= 2 || overflowTypes.empty());
  CoroInfo.NumDirectYieldComponents = numDirectComponents;

  // Replace the pointer type we added to components with the real
  // pointer-to-overflow type.
  if (!overflowTypes.empty()) {
    std::reverse(overflowTypes.begin(), overflowTypes.end());

    // TODO: should we use some sort of real layout here instead of
    // trusting LLVM's?
    CoroInfo.indirectResultsType =
        llvm::StructType::get(IGM.getLLVMContext(), overflowTypes);
    components.back() = CoroInfo.indirectResultsType->getPointerTo();
  }

  ResultIRType = components.size() == 1
                   ? components.front()
                   : llvm::StructType::get(IGM.getLLVMContext(), components);
}

void SignatureExpansion::expandCoroutineContinuationParameters() {
  // The coroutine context.
  addCoroutineContextParameter();

  // Whether this is an unwind resumption.
  ParamIRTypes.push_back(IGM.Int1Ty);
}

void SignatureExpansion::addAsyncParameters() {
  // using TaskContinuationFunction =
  //   SWIFT_CC(swift)
  //   void (SWIFT_ASYNC_CONTEXT AsyncContext *);
  AsyncContextIdx = getCurParamIndex();
  Attrs = Attrs.addParamAttribute(IGM.getLLVMContext(), AsyncContextIdx,
                                  llvm::Attribute::SwiftAsync);
  ParamIRTypes.push_back(IGM.SwiftContextPtrTy);
}

void SignatureExpansion::addCoroutineContextParameter() {
  // Flag that the context is dereferenceable and unaliased.
  auto contextSize = getCoroutineContextSize(IGM, FnType);
  Attrs = Attrs.addDereferenceableParamAttr(IGM.getLLVMContext(),
                                            getCurParamIndex(),
                                            contextSize.getValue());
  Attrs = Attrs.addParamAttribute(IGM.getLLVMContext(),
                                  getCurParamIndex(),
                                  llvm::Attribute::NoAlias);

  ParamIRTypes.push_back(IGM.Int8PtrTy);
}

NativeConventionSchema::NativeConventionSchema(IRGenModule &IGM,
                                               const TypeInfo *ti,
                                               bool IsResult)
    : Lowering(IGM.ClangCodeGen->CGM()) {
  if (auto *loadable = dyn_cast<LoadableTypeInfo>(ti)) {
    // Lower the type according to the Swift ABI.
    loadable->addToAggLowering(IGM, Lowering, Size(0));
    Lowering.finish();
    // Should we pass indirectly according to the ABI?
    RequiresIndirect = Lowering.shouldPassIndirectly(IsResult);
  } else {
    Lowering.finish();
    RequiresIndirect = true;
  }
}

llvm::Type *NativeConventionSchema::getExpandedType(IRGenModule &IGM) const {
  if (empty())
    return IGM.VoidTy;
  SmallVector<llvm::Type *, 8> elts;
  enumerateComponents([&](clang::CharUnits offset, clang::CharUnits end,
                          llvm::Type *type) { elts.push_back(type); });

  if (elts.size() == 1)
    return elts[0];

  auto &ctx = IGM.getLLVMContext();
  return llvm::StructType::get(ctx, elts, /*packed*/ false);
}

std::pair<llvm::StructType *, llvm::StructType *>
NativeConventionSchema::getCoercionTypes(
    IRGenModule &IGM, SmallVectorImpl<unsigned> &expandedTyIndicesMap) const {
  auto &ctx = IGM.getLLVMContext();

  if (empty()) {
    auto type = llvm::StructType::get(ctx);
    return {type, type};
  }

  clang::CharUnits lastEnd = clang::CharUnits::Zero();
  llvm::SmallSet<unsigned, 8> overlappedWithSuccessor;
  unsigned idx = 0;

  // Mark overlapping ranges.
  enumerateComponents(
      [&](clang::CharUnits offset, clang::CharUnits end, llvm::Type *type) {
        if (offset < lastEnd) {
          overlappedWithSuccessor.insert(idx);
        }
        lastEnd = end;
        ++idx;
      });

  // Create the coercion struct with only the integer portion of overlapped
  // components and non-overlapped components.
  idx = 0;
  lastEnd = clang::CharUnits::Zero();
  SmallVector<llvm::Type *, 8> elts;
  bool packed = false;
  enumerateComponents(
      [&](clang::CharUnits begin, clang::CharUnits end, llvm::Type *type) {
        bool overlapped = overlappedWithSuccessor.count(idx) ||
                          (idx && overlappedWithSuccessor.count(idx - 1));
        ++idx;
        if (overlapped && !isa<llvm::IntegerType>(type)) {
          // keep the old lastEnd for padding.
          return;
        }
        // Add padding (which may include padding for overlapped non-integer
        // components).
        if (begin != lastEnd) {
          auto paddingSize = begin - lastEnd;
          assert(!paddingSize.isNegative());

          auto padding = llvm::ArrayType::get(llvm::Type::getInt8Ty(ctx),
                                              paddingSize.getQuantity());
          elts.push_back(padding);
        }

        if (!packed && !begin.isMultipleOf(clang::CharUnits::fromQuantity(
                           IGM.DataLayout.getABITypeAlign(type))))
          packed = true;
        elts.push_back(type);
        expandedTyIndicesMap.push_back(idx - 1);
        lastEnd = begin + clang::CharUnits::fromQuantity(
                              IGM.DataLayout.getTypeAllocSize(type));
        assert(end <= lastEnd);
      });

  auto *coercionType = llvm::StructType::get(ctx, elts, packed);
  if (overlappedWithSuccessor.empty())
    return {coercionType, llvm::StructType::get(ctx)};

  // Create the coercion struct with only the non-integer overlapped
  // components.
  idx = 0;
  lastEnd = clang::CharUnits::Zero();
  elts.clear();
  packed = false;
  enumerateComponents(
      [&](clang::CharUnits begin, clang::CharUnits end, llvm::Type *type) {
        bool overlapped = overlappedWithSuccessor.count(idx) ||
                          (idx && overlappedWithSuccessor.count(idx - 1));
        ++idx;
        if (!overlapped || (overlapped && isa<llvm::IntegerType>(type))) {
          // Ignore and keep the old lastEnd for padding.
          return;
        }
        // Add padding.
        if (begin != lastEnd) {
          auto paddingSize = begin - lastEnd;
          assert(!paddingSize.isNegative());

          auto padding = llvm::ArrayType::get(llvm::Type::getInt8Ty(ctx),
                                              paddingSize.getQuantity());
          elts.push_back(padding);
        }
        if (!packed &&
            !begin.isMultipleOf(clang::CharUnits::fromQuantity(
                IGM.DataLayout.getABITypeAlign(type))))
          packed = true;
        elts.push_back(type);
        expandedTyIndicesMap.push_back(idx - 1);
        lastEnd = begin + clang::CharUnits::fromQuantity(
                              IGM.DataLayout.getTypeAllocSize(type));
        assert(end <= lastEnd);
      });
  auto *overlappedCoercionType = llvm::StructType::get(ctx, elts, packed);
  return {coercionType, overlappedCoercionType};
}

// TODO: Direct to Indirect result conversion could be handled in a SIL
// AddressLowering pass.
std::pair<llvm::Type *, const TypeInfo *>
SignatureExpansion::expandDirectResult() {
  // Handle the direct result type, checking for supposedly scalar
  // result types that we actually want to return indirectly.
  auto resultType = getSILFuncConventions().getSILResultType(
      IGM.getMaximalTypeExpansionContext());

  // Fast-path the empty tuple type.
  if (auto tuple = resultType.getAs<TupleType>())
    if (tuple->getNumElements() == 0)
      return std::make_pair(IGM.VoidTy, nullptr);

  switch (FnType->getLanguage()) {
  case SILFunctionLanguage::C:
    llvm_unreachable("Expanding C/ObjC parameters in the wrong place!");
    break;
  case SILFunctionLanguage::Swift: {
    auto &ti = IGM.getTypeInfo(resultType);
    auto &native = ti.nativeReturnValueSchema(IGM);
    if (native.requiresIndirect())
      return std::make_pair(addIndirectResult(resultType), nullptr);

    // Disable the use of sret if we have a non-trivial direct result.
    if (!native.empty()) CanUseSRet = false;
    return std::make_pair(native.getExpandedType(IGM), &ti);
  }
  }

  llvm_unreachable("Not a valid SILFunctionLanguage.");
}

std::pair<llvm::Type *, const TypeInfo *>
SignatureExpansion::expandDirectErrorType() {
  if (!getSILFuncConventions().funcTy->hasErrorResult() ||
      !getSILFuncConventions().isTypedError()) {
    return std::make_pair(nullptr, nullptr);
  }

  switch (FnType->getLanguage()) {
  case SILFunctionLanguage::C:
    llvm_unreachable("Expanding C/ObjC parameters in the wrong place!");
    break;
  case SILFunctionLanguage::Swift: {
    auto resultType = getSILFuncConventions().getSILResultType(
        IGM.getMaximalTypeExpansionContext());
    auto errorType = getSILFuncConventions().getSILErrorType(
        IGM.getMaximalTypeExpansionContext());
    const auto &ti = IGM.getTypeInfo(resultType);
    auto &native = ti.nativeReturnValueSchema(IGM);
    const auto &errorTI = IGM.getTypeInfo(errorType);
    auto &errorNative = errorTI.nativeReturnValueSchema(IGM);
    if (native.requiresIndirect() ||
        errorNative.shouldReturnTypedErrorIndirectly()) {
      return std::make_pair(nullptr, nullptr);
    }

    auto combined = combineResultAndTypedErrorType(IGM, native, errorNative);

    return std::make_pair(combined.combinedTy, &errorTI);
  }
  }
}

static const clang::FieldDecl *
getLargestUnionField(const clang::RecordDecl *record,
                     const clang::ASTContext &ctx) {
  const clang::FieldDecl *largestField = nullptr;
  clang::CharUnits unionSize = clang::CharUnits::Zero();

  for (auto field : record->fields()) {
    assert(!field->isBitField());
    clang::CharUnits fieldSize = ctx.getTypeSizeInChars(field->getType());
    if (unionSize < fieldSize) {
      unionSize = fieldSize;
      largestField = field;
    }
  }
  assert(largestField && "empty union?");
  return largestField;
}

namespace {
  /// A CRTP class for working with Clang's ABIArgInfo::Expand
  /// argument type expansions.
  template <class Impl, class... Args> struct ClangExpand {
    IRGenModule &IGM;
    const clang::ASTContext &Ctx;
    ClangExpand(IRGenModule &IGM) : IGM(IGM), Ctx(IGM.getClangASTContext()) {}

    Impl &asImpl() { return *static_cast<Impl*>(this); }

    void visit(clang::CanQualType type, Args... args) {
      switch (type->getTypeClass()) {
#define TYPE(Class, Base)
#define NON_CANONICAL_TYPE(Class, Base) \
      case clang::Type::Class:
#define DEPENDENT_TYPE(Class, Base) \
      case clang::Type::Class:
#define NON_CANONICAL_UNLESS_DEPENDENT_TYPE(Class, Base) \
      case clang::Type::Class:
#include "clang/AST/TypeNodes.inc"
        llvm_unreachable("canonical or dependent type in ABI lowering");

      // These shouldn't occur in expandable struct types.
      case clang::Type::IncompleteArray:
      case clang::Type::VariableArray:
        llvm_unreachable("variable-sized or incomplete array in ABI lowering");

      // We should only ever get ObjC pointers, not underlying objects.
      case clang::Type::ObjCInterface:
      case clang::Type::ObjCObject:
        llvm_unreachable("ObjC object type in ABI lowering");

      // We should only ever get function pointers.
      case clang::Type::FunctionProto:
      case clang::Type::FunctionNoProto:
        llvm_unreachable("non-pointer function type in ABI lowering");

      // We currently never import C++ code, and we should be able to
      // kill Expand before we do.
      case clang::Type::LValueReference:
      case clang::Type::RValueReference:
      case clang::Type::MemberPointer:
      case clang::Type::Auto:
      case clang::Type::DeducedTemplateSpecialization:
        llvm_unreachable("C++ type in ABI lowering?");

      case clang::Type::Pipe:
        llvm_unreachable("OpenCL type in ABI lowering?");

      case clang::Type::BitInt:
        llvm_unreachable("BitInt type in ABI lowering?");

      case clang::Type::ConstantMatrix: {
        llvm_unreachable("ConstantMatrix type in ABI lowering?");
      }

      case clang::Type::ArrayParameter:
        llvm_unreachable("HLSL type in ABI lowering");


      case clang::Type::ConstantArray: {
        auto array = Ctx.getAsConstantArrayType(type);
        auto elt = Ctx.getCanonicalType(array->getElementType());
        auto &&context = asImpl().beginArrayElements(elt);
        uint64_t n = array->getSize().getZExtValue();
        for (uint64_t i = 0; i != n; ++i) {
          asImpl().visitArrayElement(elt, i, context, args...);
        }
        return;
      }

      case clang::Type::Record: {
        auto record = cast<clang::RecordType>(type)->getDecl();
        if (record->isUnion()) {
          auto largest = getLargestUnionField(record, Ctx);
          asImpl().visitUnionField(record, largest, args...);
        } else {
          auto &&context = asImpl().beginStructFields(record);
          for (auto field : record->fields()) {
            asImpl().visitStructField(record, field, context, args...);
          }
        }
        return;
      }

      case clang::Type::Complex: {
        auto elt = type.castAs<clang::ComplexType>().getElementType();
        asImpl().visitComplexElement(elt, 0, args...);
        asImpl().visitComplexElement(elt, 1, args...);
        return;
      }

      // Just handle this types as opaque integers.
      case clang::Type::Enum:
      case clang::Type::Atomic:
        asImpl().visitScalar(convertTypeAsInteger(type), args...);
        return;

      case clang::Type::Builtin:
        asImpl().visitScalar(
                      convertBuiltinType(type.castAs<clang::BuiltinType>()),
                             args...);
        return;

      case clang::Type::Vector:
      case clang::Type::ExtVector:
        asImpl().visitScalar(
                      convertVectorType(type.castAs<clang::VectorType>()),
                             args...);
        return;

      case clang::Type::Pointer:
      case clang::Type::BlockPointer:
      case clang::Type::ObjCObjectPointer:
        asImpl().visitScalar(IGM.Int8PtrTy, args...);
        return;
      }
      llvm_unreachable("bad type kind");
    }
    
    Size getSizeOfType(clang::QualType type) {
      auto clangSize = Ctx.getTypeSizeInChars(type);
      return Size(clangSize.getQuantity());
    }

  private:
    llvm::Type *convertVectorType(clang::CanQual<clang::VectorType> type) {
      auto eltTy =
        convertBuiltinType(type->getElementType().castAs<clang::BuiltinType>());
      return llvm::FixedVectorType::get(eltTy, type->getNumElements());
    }

    llvm::Type *convertBuiltinType(clang::CanQual<clang::BuiltinType> type) {
      switch (type.getTypePtr()->getKind()) {
#define BUILTIN_TYPE(Id, SingletonId)
#define PLACEHOLDER_TYPE(Id, SingletonId) \
      case clang::BuiltinType::Id:
#include "clang/AST/BuiltinTypes.def"
      case clang::BuiltinType::Dependent:
        llvm_unreachable("placeholder type in ABI lowering");

      // We should never see these unadorned.
      case clang::BuiltinType::ObjCId:
      case clang::BuiltinType::ObjCClass:
      case clang::BuiltinType::ObjCSel:
        llvm_unreachable("bare Objective-C object type in ABI lowering");

      // This should never be the type of an argument or field.
      case clang::BuiltinType::Void:
        llvm_unreachable("bare void type in ABI lowering");

      // We should never see the OpenCL builtin types at all.
      case clang::BuiltinType::OCLClkEvent:
      case clang::BuiltinType::OCLEvent:
      case clang::BuiltinType::OCLSampler:
      case clang::BuiltinType::OCLQueue:
      case clang::BuiltinType::OCLReserveID:
#define IMAGE_TYPE(Name, Id, ...) case clang::BuiltinType::Id:
#include "clang/Basic/OpenCLImageTypes.def"
#define EXT_OPAQUE_TYPE(Name, Id, ...) case clang::BuiltinType::Id:
#include "clang/Basic/OpenCLExtensionTypes.def"
        llvm_unreachable("OpenCL type in ABI lowering");

      // We should never see ARM SVE types at all.
#define SVE_TYPE(Name, Id, ...) case clang::BuiltinType::Id:
#include "clang/Basic/AArch64SVEACLETypes.def"
        llvm_unreachable("ARM SVE type in ABI lowering");

      // We should never see PPC MMA types at all.
#define PPC_VECTOR_TYPE(Name, Id, Size) case clang::BuiltinType::Id:
#include "clang/Basic/PPCTypes.def"
        llvm_unreachable("PPC MMA type in ABI lowering");

      // We should never see RISC-V V types at all.
#define RVV_TYPE(Name, Id, Size) case clang::BuiltinType::Id:
#include "clang/Basic/RISCVVTypes.def"
        llvm_unreachable("RISC-V V type in ABI lowering");

#define WASM_TYPE(Name, Id, Size) case clang::BuiltinType::Id:
#include "clang/Basic/WebAssemblyReferenceTypes.def"
        llvm_unreachable("WASM type in ABI lowering");

      // We should never see AMDGPU types at all.
#define AMDGPU_TYPE(Name, Id, ...) case clang::BuiltinType::Id:
#include "clang/Basic/AMDGPUTypes.def"
        llvm_unreachable("AMDGPU type in ABI lowering");

      // Handle all the integer types as opaque values.
#define BUILTIN_TYPE(Id, SingletonId)
#define SIGNED_TYPE(Id, SingletonId) \
      case clang::BuiltinType::Id:
#define UNSIGNED_TYPE(Id, SingletonId) \
      case clang::BuiltinType::Id:
#include "clang/AST/BuiltinTypes.def"
        return convertTypeAsInteger(type);

      // Lower all the floating-point values by their semantics.
      case clang::BuiltinType::Half:
        return convertFloatingType(Ctx.getTargetInfo().getHalfFormat());
      case clang::BuiltinType::Float:
        return convertFloatingType(Ctx.getTargetInfo().getFloatFormat());
      case clang::BuiltinType::Double:
        return convertFloatingType(Ctx.getTargetInfo().getDoubleFormat());
      case clang::BuiltinType::LongDouble:
        return convertFloatingType(Ctx.getTargetInfo().getLongDoubleFormat());
      case clang::BuiltinType::Float16:
        llvm_unreachable("When upstream support is added for Float16 in "
                         "clang::TargetInfo, use the implementation here");
      case clang::BuiltinType::BFloat16:
        return convertFloatingType(Ctx.getTargetInfo().getBFloat16Format());
      case clang::BuiltinType::Float128:
        return convertFloatingType(Ctx.getTargetInfo().getFloat128Format());
      case clang::BuiltinType::Ibm128:
        return convertFloatingType(Ctx.getTargetInfo().getIbm128Format());

      // nullptr_t -> void*
      case clang::BuiltinType::NullPtr:
        return IGM.Int8PtrTy;
      }
      llvm_unreachable("bad builtin type");
    }

    llvm::Type *convertFloatingType(const llvm::fltSemantics &format) {
      if (&format == &llvm::APFloat::IEEEhalf())
        return llvm::Type::getHalfTy(IGM.getLLVMContext());
      if (&format == &llvm::APFloat::IEEEsingle())
        return llvm::Type::getFloatTy(IGM.getLLVMContext());
      if (&format == &llvm::APFloat::IEEEdouble())
        return llvm::Type::getDoubleTy(IGM.getLLVMContext());
      if (&format == &llvm::APFloat::IEEEquad())
        return llvm::Type::getFP128Ty(IGM.getLLVMContext());
      if (&format == &llvm::APFloat::PPCDoubleDouble())
        return llvm::Type::getPPC_FP128Ty(IGM.getLLVMContext());
      if (&format == &llvm::APFloat::x87DoubleExtended())
        return llvm::Type::getX86_FP80Ty(IGM.getLLVMContext());
      llvm_unreachable("bad float format");
    }

    llvm::Type *convertTypeAsInteger(clang::QualType type) {
      auto size = getSizeOfType(type);
      return llvm::IntegerType::get(IGM.getLLVMContext(),
                                    size.getValueInBits());
    }
  };

  /// A CRTP specialization of ClangExpand which projects down to
  /// various aggregate elements of an address.
  ///
  /// Subclasses should only have to define visitScalar.
  template <class Impl>
  class ClangExpandProjection : public ClangExpand<Impl, Address> {
    using super = ClangExpand<Impl, Address>;
    using super::asImpl;
    using super::IGM;
    using super::Ctx;
    using super::getSizeOfType;

  protected:
    IRGenFunction &IGF;
    ClangExpandProjection(IRGenFunction &IGF)
      : super(IGF.IGM), IGF(IGF) {}

  public:
    void visit(clang::CanQualType type, Address addr) {
      assert(addr.getType() == IGM.Int8PtrTy);
      super::visit(type, addr);
    }
    
    Size beginArrayElements(clang::CanQualType element) {
      return getSizeOfType(element);
    }
    void visitArrayElement(clang::CanQualType element, unsigned i,
                           Size elementSize, Address arrayAddr) {
      asImpl().visit(element, createGEPAtOffset(arrayAddr, elementSize * i));
    }

    void visitComplexElement(clang::CanQualType element, unsigned i,
                             Address complexAddr) {
      Address addr = complexAddr;
      if (i) { addr = createGEPAtOffset(complexAddr, getSizeOfType(element)); }
      asImpl().visit(element, addr);
    }

    void visitUnionField(const clang::RecordDecl *record,
                         const clang::FieldDecl *field,
                         Address structAddr) {
      asImpl().visit(Ctx.getCanonicalType(field->getType()), structAddr);
    }

    const clang::ASTRecordLayout &
    beginStructFields(const clang::RecordDecl *record) {
      return Ctx.getASTRecordLayout(record);
    }
    void visitStructField(const clang::RecordDecl *record,
                          const clang::FieldDecl *field,
                          const clang::ASTRecordLayout &layout,
                          Address structAddr) {
      auto fieldIndex = field->getFieldIndex();
      assert(!field->isBitField());
      auto fieldOffset = Size(layout.getFieldOffset(fieldIndex) / 8);
      asImpl().visit(Ctx.getCanonicalType(field->getType()),
                     createGEPAtOffset(structAddr, fieldOffset));
    }

  private:
    Address createGEPAtOffset(Address addr, Size offset) {
      if (offset.isZero()) {
        return addr;
      } else {
        return IGF.Builder.CreateConstByteArrayGEP(addr, offset);
      }
    }
  };

  /// A class for collecting the types of a Clang ABIArgInfo::Expand
  /// argument expansion.
  struct ClangExpandTypeCollector : ClangExpand<ClangExpandTypeCollector> {
    SmallVectorImpl<llvm::Type*> &Types;
    ClangExpandTypeCollector(IRGenModule &IGM,
                             SmallVectorImpl<llvm::Type*> &types)
      : ClangExpand(IGM), Types(types) {}

    bool beginArrayElements(clang::CanQualType element) { return true; }
    void visitArrayElement(clang::CanQualType element, unsigned i, bool _) {
      visit(element);
    }

    void visitComplexElement(clang::CanQualType element, unsigned i) {
      visit(element);
    }

    void visitUnionField(const clang::RecordDecl *record,
                         const clang::FieldDecl *field) {
      visit(Ctx.getCanonicalType(field->getType()));
    }

    bool beginStructFields(const clang::RecordDecl *record) { return true; }
    void visitStructField(const clang::RecordDecl *record,
                          const clang::FieldDecl *field,
                          bool _) {
      visit(Ctx.getCanonicalType(field->getType()));
    }

    void visitScalar(llvm::Type *type) {
      Types.push_back(type);
    }
  };
} // end anonymous namespace

static bool doesClangExpansionMatchSchema(IRGenModule &IGM,
                                          clang::CanQualType type,
                                          const ExplosionSchema &schema) {
  assert(!schema.containsAggregate());
  SmallVector<llvm::Type *, 4> expansion;
  ClangExpandTypeCollector(IGM, expansion).visit(type);

  if (expansion.size() != schema.size())
    return false;

  for (size_t i = 0, e = schema.size(); i != e; ++i) {
    if (schema[i].getScalarType() != expansion[i])
      return false;
  }

  return true;
}

/// Expand the result and parameter types to the appropriate LLVM IR
/// types for C, C++ and Objective-C signatures.
void SignatureExpansion::expandExternalSignatureTypes() {
  assert(FnType->getLanguage() == SILFunctionLanguage::C);

  auto SILResultTy = [&]() {
    if (FnType->getNumResults() == 0)
      return SILType::getPrimitiveObjectType(IGM.Context.TheEmptyTupleType);

    return SILType::getPrimitiveObjectType(
        FnType->getSingleResult().getReturnValueType(
            IGM.getSILModule(), FnType, TypeExpansionContext::minimal()));
  }();

  // Convert the SIL result type to a Clang type. If this is for a c++
  // constructor, use 'void' as the return type to arrange the function type.
  auto clangResultTy = IGM.getClangType(
      cxxCtorDecl
          ? SILType::getPrimitiveObjectType(IGM.Context.TheEmptyTupleType)
          : SILResultTy);

  // Now convert the parameters to Clang types.
  auto params = FnType->getParameters();

  SmallVector<clang::CanQualType,4> paramTys;
  auto const &clangCtx = IGM.getClangASTContext();

  switch (FnType->getRepresentation()) {
  case SILFunctionTypeRepresentation::ObjCMethod: {
    // ObjC methods take their 'self' argument first, followed by an
    // implicit _cmd argument.
    auto &self = params.back();
    auto clangTy = IGM.getClangType(self, FnType);
    paramTys.push_back(clangTy);
    if (!forStaticCall) // objc_direct methods don't have the _cmd argumment.
      paramTys.push_back(clangCtx.VoidPtrTy);
    params = params.drop_back();
    break;
  }

  case SILFunctionTypeRepresentation::Block:
    // Blocks take their context argument first.
    paramTys.push_back(clangCtx.VoidPtrTy);
    break;

  case SILFunctionTypeRepresentation::CXXMethod: {
    // Cxx methods take their 'self' argument first.
    auto &self = params.back();
    auto clangTy = IGM.getClangType(self, FnType);
    paramTys.push_back(clangTy);
    params = params.drop_back();
    break;
  }

  case SILFunctionTypeRepresentation::CFunctionPointer:
    if (cxxCtorDecl) {
      auto clangTy = IGM.getClangASTContext().getPointerType(
          IGM.getClangType(SILResultTy));
      paramTys.push_back(clangTy);
    }
    break;

  case SILFunctionTypeRepresentation::Thin:
  case SILFunctionTypeRepresentation::Thick:
  case SILFunctionTypeRepresentation::Method:
  case SILFunctionTypeRepresentation::WitnessMethod:
  case SILFunctionTypeRepresentation::Closure:
  case SILFunctionTypeRepresentation::KeyPathAccessorGetter:
  case SILFunctionTypeRepresentation::KeyPathAccessorSetter:
  case SILFunctionTypeRepresentation::KeyPathAccessorEquals:
  case SILFunctionTypeRepresentation::KeyPathAccessorHash:
    llvm_unreachable("not a C representation");
  }

  // Given an index within the clang parameters list, what do we need
  // to subtract from it to get to the corresponding index within the
  // Swift parameters list?
  size_t clangToSwiftParamOffset = paramTys.size();

  // Convert each parameter to a Clang type.
  for (auto param : params) {
    auto clangTy = IGM.getClangType(param, FnType);
    paramTys.push_back(clangTy);
  }

  // Generate function info for this signature.
  auto extInfo = clang::FunctionType::ExtInfo();

  bool isCXXMethod =
      FnType->getRepresentation() == SILFunctionTypeRepresentation::CXXMethod;
  auto &FI = isCXXMethod ?
      clang::CodeGen::arrangeCXXMethodCall(IGM.ClangCodeGen->CGM(),
          clangResultTy, paramTys, extInfo, {},
          clang::CodeGen::RequiredArgs::All) :
      clang::CodeGen::arrangeFreeFunctionCall(IGM.ClangCodeGen->CGM(),
          clangResultTy, paramTys, extInfo, {},
          clang::CodeGen::RequiredArgs::All);
  ForeignInfo.ClangInfo = &FI;

  assert(FI.arg_size() == paramTys.size() &&
         "Expected one ArgInfo for each parameter type!");

  auto &returnInfo = FI.getReturnInfo();

#ifndef NDEBUG
  bool formalIndirectResult = FnType->getNumResults() > 0 &&
                              FnType->getSingleResult().isFormalIndirect();
  assert(
      (cxxCtorDecl || !formalIndirectResult || returnInfo.isIndirect() || SILResultTy.isSensitive()) &&
      "swift and clang disagree on whether the result is returned indirectly");
#endif

  // Does the result need an extension attribute?
  if (returnInfo.isExtend()) {
    bool signExt = clangResultTy->hasSignedIntegerRepresentation();
    assert((signExt || clangResultTy->hasUnsignedIntegerRepresentation()) &&
           "Invalid attempt to add extension attribute to argument!");
    Attrs = Attrs.addRetAttribute(IGM.getLLVMContext(),
                                  attrKindForExtending(signExt));
  }

  auto emitArg = [&](size_t i) {
    auto &AI = FI.arg_begin()[i].info;

    // Add a padding argument if required.
    if (auto *padType = AI.getPaddingType())
      ParamIRTypes.push_back(padType);

    switch (AI.getKind()) {
    case clang::CodeGen::ABIArgInfo::Extend: {
      bool signExt = paramTys[i]->hasSignedIntegerRepresentation();
      assert((signExt || paramTys[i]->hasUnsignedIntegerRepresentation()) &&
             "Invalid attempt to add extension attribute to argument!");
      Attrs = Attrs.addParamAttribute(IGM.getLLVMContext(), getCurParamIndex(),
                                      attrKindForExtending(signExt));
      LLVM_FALLTHROUGH;
    }
    case clang::CodeGen::ABIArgInfo::Direct: {
      switch (FI.getExtParameterInfo(i).getABI()) {
      case clang::ParameterABI::Ordinary:
        break;
      case clang::ParameterABI::SwiftAsyncContext:
        IGM.addSwiftAsyncContextAttributes(Attrs, getCurParamIndex());
        break;
      case clang::ParameterABI::SwiftContext:
        IGM.addSwiftSelfAttributes(Attrs, getCurParamIndex());
        break;
      case clang::ParameterABI::SwiftErrorResult:
        IGM.addSwiftErrorAttributes(Attrs, getCurParamIndex());
        break;
      case clang::ParameterABI::SwiftIndirectResult: {
        auto &param = params[i - clangToSwiftParamOffset];
        auto paramTy = getSILFuncConventions().getSILType(
            param, IGM.getMaximalTypeExpansionContext());
        auto &paramTI = cast<FixedTypeInfo>(IGM.getTypeInfo(paramTy));
        addIndirectResultAttributes(IGM, Attrs, getCurParamIndex(), claimSRet(),
                                    paramTI.getStorageType(), paramTI);
        break;
      }
      }

      // If the coercion type is a struct which can be flattened, we need to
      // expand it.
      auto *coercedTy = AI.getCoerceToType();
      if (AI.isDirect() && AI.getCanBeFlattened() &&
          isa<llvm::StructType>(coercedTy)) {
        const auto *ST = cast<llvm::StructType>(coercedTy);
        for (unsigned EI : range(ST->getNumElements()))
          ParamIRTypes.push_back(ST->getElementType(EI));
      } else {
        ParamIRTypes.push_back(coercedTy);
      }
      break;
    }
    case clang::CodeGen::ABIArgInfo::CoerceAndExpand: {
      auto types = AI.getCoerceAndExpandTypeSequence();
      ParamIRTypes.append(types.begin(), types.end());
      break;
    }
    case clang::CodeGen::ABIArgInfo::IndirectAliased:
      llvm_unreachable("not implemented");
    case clang::CodeGen::ABIArgInfo::Indirect: {
      // When `i` is 0, if the clang offset is 1, that means we mapped the last
      // Swift parameter (self) to the first Clang parameter (this). In this
      // case, the corresponding Swift param is the last function parameter.
      assert((i >= clangToSwiftParamOffset || clangToSwiftParamOffset == 1) &&
             "Unexpected index for indirect byval argument");
      auto &param = i < clangToSwiftParamOffset
                        ? FnType->getParameters().back()
                        : params[i - clangToSwiftParamOffset];
      auto paramTy = getSILFuncConventions().getSILType(
          param, IGM.getMaximalTypeExpansionContext());
      auto &paramTI = cast<FixedTypeInfo>(IGM.getTypeInfo(paramTy));
      if (AI.getIndirectByVal() && !paramTy.isForeignReferenceType()) {
        addByvalArgumentAttributes(
            IGM, Attrs, getCurParamIndex(),
            Alignment(AI.getIndirectAlign().getQuantity()),
            paramTI.getStorageType());
      }
      addPointerParameter(paramTI.getStorageType());
      break;
    }
    case clang::CodeGen::ABIArgInfo::Expand:
      ClangExpandTypeCollector(IGM, ParamIRTypes).visit(paramTys[i]);
      break;
    case clang::CodeGen::ABIArgInfo::Ignore:
      break;
    case clang::CodeGen::ABIArgInfo::InAlloca:
      llvm_unreachable("Need to handle InAlloca during signature expansion");
    }
  };

  size_t firstParamToLowerNormally = 0;

  // If we return indirectly, that is the first parameter type.
  if (returnInfo.isIndirect()) {
    auto resultType = getSILFuncConventions().getSingleSILResultType(
        IGM.getMaximalTypeExpansionContext());
    if (returnInfo.isSRetAfterThis()) {
      // Windows ABI places `this` before the
      // returned indirect values.
      emitArg(0);
      firstParamToLowerNormally = 1;
      addIndirectResult(resultType, returnInfo.getInReg());
    } else
      addIndirectResult(resultType, returnInfo.getInReg());
  }

  // Use a special IR type for passing block pointers.
  if (FnType->getRepresentation() == SILFunctionTypeRepresentation::Block) {
    assert(FI.arg_begin()[0].info.isDirect() &&
           "block pointer not passed directly?");
    ParamIRTypes.push_back(IGM.ObjCBlockPtrTy);
    firstParamToLowerNormally = 1;
  }

  for (auto i : indices(paramTys).slice(firstParamToLowerNormally))
    emitArg(i);

  if (cxxCtorDecl) {
    ResultIRType = cast<llvm::Function>(IGM.getAddrOfClangGlobalDecl(
                                            {cxxCtorDecl, clang::Ctor_Complete},
                                            (ForDefinition_t) false))
                       ->getReturnType();
  } else if (returnInfo.isIndirect() || returnInfo.isIgnore()) {
    ResultIRType = IGM.VoidTy;
  } else {
    ResultIRType = returnInfo.getCoerceToType();
  }
}

static ArrayRef<llvm::Type *> expandScalarOrStructTypeToArray(llvm::Type *&ty) {
  ArrayRef<llvm::Type*> expandedTys;
  if (auto expansionTy = dyn_cast<llvm::StructType>(ty)) {
    // Is there any good reason this isn't public API of llvm::StructType?
    expandedTys = llvm::ArrayRef(expansionTy->element_begin(),
                                 expansionTy->getNumElements());
  } else {
    expandedTys = ty;
  }
  return expandedTys;
}

const TypeInfo &SignatureExpansion::expand(SILParameterInfo param) {
  auto paramSILType = getSILFuncConventions().getSILType(
      param, IGM.getMaximalTypeExpansionContext());
  auto &ti = IGM.getTypeInfo(paramSILType);
  switch (auto conv = param.getConvention()) {
  case ParameterConvention::Indirect_In:
  case ParameterConvention::Indirect_In_Guaranteed:
  case ParameterConvention::Indirect_In_CXX:
    addIndirectValueParameterAttributes(IGM, Attrs, ti, ParamIRTypes.size());
    addPointerParameter(IGM.getStorageType(getSILFuncConventions().getSILType(
        param, IGM.getMaximalTypeExpansionContext())));
    return ti;

  case ParameterConvention::Indirect_Inout:
  case ParameterConvention::Indirect_InoutAliasable:
    addInoutParameterAttributes(
        IGM, paramSILType, Attrs, ti, ParamIRTypes.size(),
        conv == ParameterConvention::Indirect_InoutAliasable);
    addPointerParameter(IGM.getStorageType(getSILFuncConventions().getSILType(
        param, IGM.getMaximalTypeExpansionContext())));
    return ti;

  case ParameterConvention::Pack_Guaranteed:
  case ParameterConvention::Pack_Owned:
  case ParameterConvention::Pack_Inout:
    addPackParameterAttributes(IGM, paramSILType, Attrs, ParamIRTypes.size());
    addPointerParameter(ti.getStorageType());
    return ti;

  case ParameterConvention::Direct_Owned:
  case ParameterConvention::Direct_Unowned:
  case ParameterConvention::Direct_Guaranteed:
    switch (FnType->getLanguage()) {
    case SILFunctionLanguage::C: {
      llvm_unreachable("Unexpected C/ObjC method in parameter expansion!");
      return ti;
    }
    case SILFunctionLanguage::Swift: {
      auto &nativeSchema = ti.nativeParameterValueSchema(IGM);
      if (nativeSchema.requiresIndirect()) {
        addIndirectValueParameterAttributes(IGM, Attrs, ti,
                                            ParamIRTypes.size());
        ParamIRTypes.push_back(ti.getStorageType()->getPointerTo());
        return ti;
      }
      if (nativeSchema.empty()) {
        assert(ti.getSchema().empty());
        return ti;
      }
      auto expandedTy = nativeSchema.getExpandedType(IGM);
      auto expandedTysArray = expandScalarOrStructTypeToArray(expandedTy);
      for (auto *Ty : expandedTysArray)
        ParamIRTypes.push_back(Ty);
      return ti;
    }
    }
    llvm_unreachable("bad abstract CC");
  }
  llvm_unreachable("bad parameter convention");
}

/// Does the given function type have a self parameter that should be
/// given the special treatment for self parameters?
///
/// It's important that this only return true for things that are
/// passed as a single pointer.
bool irgen::hasSelfContextParameter(CanSILFunctionType fnType) {
  if (!fnType->hasSelfParam())
    return false;

  SILParameterInfo param = fnType->getSelfParameter();

  // All the indirect conventions pass a single pointer.
  if (param.isFormalIndirect()) {
    return true;
  }

  // Direct conventions depend on the type.
  CanType type = param.getInterfaceType();

  // Thick or @objc metatypes (but not existential metatypes).
  if (auto metatype = dyn_cast<MetatypeType>(type)) {
    return metatype->getRepresentation() != MetatypeRepresentation::Thin;
  }

  // Classes and class-bounded archetypes or ObjC existentials.
  // No need to apply this to existentials.
  // The direct check for SubstitutableType works because only
  // class-bounded generic types can be passed directly.
  if (type->mayHaveSuperclass() || isa<SubstitutableType>(type) ||
      type->isObjCExistentialType()) {
    return true;
  }

  return false;
}

static void addParamInfo(SignatureExpansionABIDetails *details,
                         const TypeInfo &ti, ParameterConvention convention) {
  if (!details)
    return;
  details->parameters.push_back(
      SignatureExpansionABIDetails::Parameter(ti, convention));
}

void SignatureExpansion::expandKeyPathAccessorParameters() {
  auto params = FnType->getParameters();
  unsigned numArgsToExpand;
  SmallVector<llvm::Type *, 4> tailParams;

  switch (FnType->getRepresentation()) {
  case SILFunctionTypeRepresentation::KeyPathAccessorGetter:
    // from: (base: CurValue, indices: (X, Y, ...))
    // to:   (base: CurValue, argument: UnsafeRawPointer, size: Int)
    numArgsToExpand = 1;
    tailParams.push_back(IGM.Int8PtrTy);
    tailParams.push_back(IGM.SizeTy);
    break;
  case SILFunctionTypeRepresentation::KeyPathAccessorSetter:
    // from: (value: NewValue, base: CurValue, indices: (X, Y, ...))
    // to:   (value: NewValue, base: CurValue, argument: UnsafeRawPointer, size: Int)
    numArgsToExpand = 2;
    tailParams.push_back(IGM.Int8PtrTy);
    tailParams.push_back(IGM.SizeTy);
    break;
  case SILFunctionTypeRepresentation::KeyPathAccessorEquals:
    // from: (lhsIndices: (X, Y, ...), rhsIndices: (X, Y, ...))
    // to:   (lhsArguments: UnsafeRawPointer, rhsArguments: UnsafeRawPointer, size: Int)
    numArgsToExpand = 0;
    tailParams.push_back(IGM.Int8PtrTy);
    tailParams.push_back(IGM.Int8PtrTy);
    tailParams.push_back(IGM.SizeTy);
    break;
  case SILFunctionTypeRepresentation::KeyPathAccessorHash:
    // from: (indices: (X, Y, ...))
    // to:   (instanceArguments: UnsafeRawPointer, size: Int)
    numArgsToExpand = 0;
    tailParams.push_back(IGM.Int8PtrTy);
    tailParams.push_back(IGM.SizeTy);
    break;
  case SILFunctionTypeRepresentation::Thick:
  case SILFunctionTypeRepresentation::Block:
  case SILFunctionTypeRepresentation::Thin:
  case SILFunctionTypeRepresentation::Method:
  case SILFunctionTypeRepresentation::ObjCMethod:
  case SILFunctionTypeRepresentation::WitnessMethod:
  case SILFunctionTypeRepresentation::CFunctionPointer:
  case SILFunctionTypeRepresentation::Closure:
  case SILFunctionTypeRepresentation::CXXMethod:
    llvm_unreachable("non keypath accessor convention");
  }
  for (unsigned i = 0; i < numArgsToExpand; i++) {
    expand(params[i]);
  }
  for (auto tailParam : tailParams) {
    ParamIRTypes.push_back(tailParam);
  }
}

/// Expand the abstract parameters of a SIL function type into the physical
/// parameters of an LLVM function type (results have already been expanded).
void SignatureExpansion::expandParameters(
    SignatureExpansionABIDetails *recordedABIDetails) {
  assert(FnType->getRepresentation() != SILFunctionTypeRepresentation::Block
         && "block with non-C calling conv?!");

  if (FnType->isAsync()) {
    assert(false && "Should not use expandParameters for async functions");
    return;
  }

  // First, if this is a coroutine, add the coroutine-context parameter.
  switch (FnType->getCoroutineKind()) {
  case SILCoroutineKind::None:
    break;
  case SILCoroutineKind::YieldOnce2:
    if (IGM.IRGen.Opts.EmitYieldOnce2AsYieldOnce) {
      LLVM_FALLTHROUGH;
    } else {
      break;
    }

  case SILCoroutineKind::YieldOnce:
  case SILCoroutineKind::YieldMany:
    addCoroutineContextParameter();

    // Add indirect results as parameters. Similar to
    // expandIndirectResults, but it doesn't add sret attribute,
    // because the function has direct results (a continuation pointer
    // and yield results).
    auto fnConv = getSILFuncConventions();
    for (auto indirectResultType : fnConv.getIndirectSILResultTypes(
           IGM.getMaximalTypeExpansionContext())) {
      auto storageTy = IGM.getStorageType(indirectResultType);
      addPointerParameter(storageTy);
    }
    break;
  }

  // Next, the formal parameters.  But 'self' is treated as the
  // context if it has pointer representation.
  auto params = FnType->getParameters();
  bool hasSelfContext = false;
  if (hasSelfContextParameter(FnType)) {
    hasSelfContext = true;
    params = params.drop_back();
  }

  for (auto param : params) {
    const TypeInfo &ti = expand(param);
    addParamInfo(recordedABIDetails, ti, param.getConvention());
  }
  if (recordedABIDetails && FnType->hasSelfParam() && !hasSelfContext)
    recordedABIDetails->parameters.back().isSelf = true;

  // Next, the generic signature.
  if (hasPolymorphicParameters(FnType) &&
      !FnKind.shouldSuppressPolymorphicArguments())
    expandPolymorphicSignature(
        IGM, FnType, ParamIRTypes,
        recordedABIDetails
            ? &recordedABIDetails->polymorphicSignatureExpandedTypeSources
            : nullptr);

  // Certain special functions are passed the continuation directly.
  if (FnKind.shouldPassContinuationDirectly()) {
    ParamIRTypes.push_back(IGM.Int8PtrTy);
    ParamIRTypes.push_back(IGM.SwiftContextPtrTy);
  }

  // Context is next.
  if (hasSelfContext) {
    auto curLength = ParamIRTypes.size(); (void) curLength;

    if (claimSelf())
      IGM.addSwiftSelfAttributes(Attrs, curLength);
    expand(FnType->getSelfParameter());
    if (recordedABIDetails)
      recordedABIDetails->hasTrailingSelfParam = true;
    assert(ParamIRTypes.size() == curLength + 1 &&
           "adding 'self' added unexpected number of parameters");
  } else {
    auto needsContext = [=]() -> bool {
      switch (FnType->getRepresentation()) {
      case SILFunctionType::Representation::Block:
        llvm_unreachable("adding block parameter in Swift CC expansion?");

      // Always leave space for a context argument if we have an error result.
      case SILFunctionType::Representation::CFunctionPointer:
      case SILFunctionType::Representation::Method:
      case SILFunctionType::Representation::WitnessMethod:
      case SILFunctionType::Representation::ObjCMethod:
      case SILFunctionType::Representation::CXXMethod:
      case SILFunctionType::Representation::Thin:
      case SILFunctionType::Representation::Closure:
        return FnType->hasErrorResult();

      // KeyPath accessor always has no context.
      case SILFunctionType::Representation::KeyPathAccessorGetter:
      case SILFunctionType::Representation::KeyPathAccessorSetter:
      case SILFunctionType::Representation::KeyPathAccessorEquals:
      case SILFunctionType::Representation::KeyPathAccessorHash:
        return false;
      case SILFunctionType::Representation::Thick:
        return true;
      }
      llvm_unreachable("bad representation kind");
    };
    if (needsContext()) {
      if (claimSelf())
        IGM.addSwiftSelfAttributes(Attrs, ParamIRTypes.size());
      ParamIRTypes.push_back(IGM.RefCountedPtrTy);
      if (recordedABIDetails)
        recordedABIDetails->hasContextParam = true;
    }
  }

  // Error results are last.  We always pass them as a pointer to the
  // formal error type; LLVM will magically turn this into a non-pointer
  // if we set the right attribute.
  if (FnType->hasErrorResult()) {
    if (claimError())
      IGM.addSwiftErrorAttributes(Attrs, ParamIRTypes.size());
    llvm::Type *errorType = getErrorRegisterType();
    ParamIRTypes.push_back(errorType->getPointerTo());
    if (recordedABIDetails)
      recordedABIDetails->hasErrorResult = true;
    if (getSILFuncConventions().isTypedError()) {

      auto resultType = getSILFuncConventions().getSILResultType(
          IGM.getMaximalTypeExpansionContext());
      auto &resultTI = IGM.getTypeInfo(resultType);
      auto &native = resultTI.nativeReturnValueSchema(IGM);
      auto errorType = getSILFuncConventions().getSILErrorType(
          IGM.getMaximalTypeExpansionContext());
      auto &errorTI = IGM.getTypeInfo(errorType);
      auto &nativeError = errorTI.nativeReturnValueSchema(IGM);

      if (getSILFuncConventions().hasIndirectSILResults() ||
          getSILFuncConventions().hasIndirectSILErrorResults() ||
          native.requiresIndirect() ||
          nativeError.shouldReturnTypedErrorIndirectly()) {
        ParamIRTypes.push_back(IGM.getStorageType(errorType)->getPointerTo());
      }
    }
  }

  // Witness methods have some extra parameter types.
  if (FnType->getRepresentation() ==
        SILFunctionTypeRepresentation::WitnessMethod) {
    expandTrailingWitnessSignature(IGM, FnType, ParamIRTypes);
  }
}

/// Expand the result and parameter types of a SIL function into the
/// physical parameter types of an LLVM function and return the result
/// type.
void SignatureExpansion::expandFunctionType(
    SignatureExpansionABIDetails *recordedABIDetails) {
  switch (FnType->getLanguage()) {
  case SILFunctionLanguage::Swift: {
    if (FnType->isAsync()) {
      expandAsyncEntryType();
      return;
    }
    expandResult(recordedABIDetails);

    switch (FnType->getRepresentation()) {
    case SILFunctionTypeRepresentation::KeyPathAccessorGetter:
    case SILFunctionTypeRepresentation::KeyPathAccessorSetter:
    case SILFunctionTypeRepresentation::KeyPathAccessorEquals:
    case SILFunctionTypeRepresentation::KeyPathAccessorHash:
      expandKeyPathAccessorParameters();
      break;
    default:
      expandParameters(recordedABIDetails);
      break;
    }
    return;
  }
  case SILFunctionLanguage::C:
    expandExternalSignatureTypes();
    return;
  }
  llvm_unreachable("bad abstract calling convention");
}

void SignatureExpansion::expandCoroutineContinuationType() {
  expandCoroutineResult(/*for continuation*/ true);
  expandCoroutineContinuationParameters();
}

llvm::Type *SignatureExpansion::getErrorRegisterType() {
  if (getSILFuncConventions().isTypedError())
    return IGM.Int8PtrTy;

  return IGM.getStorageType(getSILFuncConventions().getSILType(
            FnType->getErrorResult(), IGM.getMaximalTypeExpansionContext()));
}

void SignatureExpansion::expandAsyncReturnType() {
  // Build up the signature of the return continuation function.
  // void (AsyncTask *, SerialExecutorRef, AsyncContext *, DirectResult0, ...,
  //                                                 DirectResultN, Error*);
  ResultIRType = IGM.VoidTy;
  addAsyncParameters();
  SmallVector<llvm::Type *, 8> components;

  auto addErrorResult = [&]() {
    // Add the error pointer at the end.
    if (FnType->hasErrorResult()) {
      llvm::Type *errorType = getErrorRegisterType();
      claimSelf();
      auto selfIdx = ParamIRTypes.size();
      IGM.addSwiftSelfAttributes(Attrs, selfIdx);
      AsyncResumeFunctionSwiftSelfIdx = selfIdx;
      ParamIRTypes.push_back(errorType);
    }
  };

  auto fnConv = getSILFuncConventions();

  auto resultType =
      fnConv.getSILResultType(IGM.getMaximalTypeExpansionContext());
  auto &ti = IGM.getTypeInfo(resultType);
  auto &native = ti.nativeReturnValueSchema(IGM);

  if (!fnConv.hasIndirectSILResults() && !fnConv.hasIndirectSILErrorResults() &&
      !native.requiresIndirect() && fnConv.funcTy->hasErrorResult() &&
      fnConv.isTypedError()) {
    auto errorType = getSILFuncConventions().getSILErrorType(
        IGM.getMaximalTypeExpansionContext());
    auto &errorTi = IGM.getTypeInfo(errorType);
    auto &nativeError = errorTi.nativeReturnValueSchema(IGM);
    if (!nativeError.shouldReturnTypedErrorIndirectly()) {
      auto combined = combineResultAndTypedErrorType(IGM, native, nativeError);

      if (combined.combinedTy->isVoidTy()) {
        addErrorResult();
        return;
      }

      if (auto *structTy = dyn_cast<llvm::StructType>(combined.combinedTy)) {
        for (auto *elem : structTy->elements()) {
          ParamIRTypes.push_back(elem);
        }
      } else {
        ParamIRTypes.push_back(combined.combinedTy);
      }
    }
    addErrorResult();
    return;
  }

  if (native.requiresIndirect() || native.empty()) {
    addErrorResult();
    return;
  }

  // Add the result type components as trailing parameters.
  native.enumerateComponents(
      [&](clang::CharUnits offset, clang::CharUnits end, llvm::Type *type) {
        ParamIRTypes.push_back(type);
      });

  addErrorResult();
}

void SignatureExpansion::addIndirectThrowingResult() {
  if (getSILFuncConventions().funcTy->hasErrorResult() &&
      getSILFuncConventions().isTypedError()) {
    auto resultType = getSILFuncConventions().getSILResultType(
        IGM.getMaximalTypeExpansionContext());
    auto &ti = IGM.getTypeInfo(resultType);
    auto &native = ti.nativeReturnValueSchema(IGM);

    auto errorType = getSILFuncConventions().getSILErrorType(
        IGM.getMaximalTypeExpansionContext());
    const TypeInfo &errorTI = IGM.getTypeInfo(errorType);
    auto &nativeError = errorTI.nativeReturnValueSchema(IGM);

    if (getSILFuncConventions().hasIndirectSILResults() ||
        getSILFuncConventions().hasIndirectSILErrorResults() ||
        native.requiresIndirect() ||
        nativeError.shouldReturnTypedErrorIndirectly()) {
      auto errorStorageTy = errorTI.getStorageType();
      ParamIRTypes.push_back(errorStorageTy->getPointerTo());
    }
  }

}
void SignatureExpansion::expandAsyncEntryType() {
  ResultIRType = IGM.VoidTy;

  // FIXME: Claim the SRet for now. The way we have set up the function type to
  // start with the three async specific arguments does not allow for use of
  // sret.
  CanUseSRet = false;

  // Add the indirect 'direct' result type.
  auto resultType = getSILFuncConventions().getSILResultType(
      IGM.getMaximalTypeExpansionContext());
  auto &ti = IGM.getTypeInfo(resultType);
  auto &native = ti.nativeReturnValueSchema(IGM);
  if (native.requiresIndirect())
    addIndirectResult(resultType);

  // Add the indirect result types.
  expandIndirectResults();

  // Add the async context parameter.
  addAsyncParameters();

  // Add the parameters.
  auto params = FnType->getParameters();
  auto hasSelfContext = false;
  if (hasSelfContextParameter(FnType)) {
    hasSelfContext = true;
    params = params.drop_back();
  }

  for (auto param : params) {
    expand(param);
  }

  // Next, the generic signature.
  if (hasPolymorphicParameters(FnType) &&
      !FnKind.shouldSuppressPolymorphicArguments())
    expandPolymorphicSignature(IGM, FnType, ParamIRTypes);
  if (FnKind.shouldPassContinuationDirectly()) {
    // Async waiting functions add the resume function pointer.
    // (But skip passing the metadata.)
    ParamIRTypes.push_back(IGM.Int8PtrTy);
    ParamIRTypes.push_back(IGM.SwiftContextPtrTy);
  }

  // Context is next.
  if (hasSelfContext) {
    auto curLength = ParamIRTypes.size();
    (void)curLength;
    expand(FnType->getSelfParameter());
    assert(ParamIRTypes.size() == curLength + 1 &&
           "adding 'self' added unexpected number of parameters");
    if (claimSelf())
      IGM.addSwiftSelfAttributes(Attrs, curLength);
  } else {
    auto needsContext = [=]() -> bool {
      switch (FnType->getRepresentation()) {
      case SILFunctionType::Representation::Block:
        llvm_unreachable("adding block parameter in Swift CC expansion?");

      // Always leave space for a context argument if we have an error result.
      case SILFunctionType::Representation::CFunctionPointer:
      case SILFunctionType::Representation::Method:
      case SILFunctionType::Representation::WitnessMethod:
      case SILFunctionType::Representation::ObjCMethod:
      case SILFunctionType::Representation::Thin:
      case SILFunctionType::Representation::Closure:
      case SILFunctionType::Representation::CXXMethod:
      case SILFunctionType::Representation::KeyPathAccessorGetter:
      case SILFunctionType::Representation::KeyPathAccessorSetter:
      case SILFunctionType::Representation::KeyPathAccessorEquals:
      case SILFunctionType::Representation::KeyPathAccessorHash:
        return false;

      case SILFunctionType::Representation::Thick:
        return true;
      }
      llvm_unreachable("bad representation kind");
    };
    if (needsContext()) {
      if (claimSelf())
        IGM.addSwiftSelfAttributes(Attrs, ParamIRTypes.size());
      ParamIRTypes.push_back(IGM.RefCountedPtrTy);
    }
  }

  addIndirectThrowingResult();

  // For now we continue to store the error result in the context to be able to
  // reuse non throwing functions.

  // Witness methods have some extra parameter types.
  if (FnType->getRepresentation() ==
      SILFunctionTypeRepresentation::WitnessMethod) {
    expandTrailingWitnessSignature(IGM, FnType, ParamIRTypes);
  }
}

void SignatureExpansion::expandAsyncAwaitType() {
  expandAsyncEntryType();

  SmallVector<llvm::Type *, 8> components;
  // Async context.
  AsyncContextIdx = 0;
  components.push_back(IGM.Int8PtrTy);

  auto addErrorResult = [&]() {
    if (FnType->hasErrorResult()) {
      llvm::Type *errorType = getErrorRegisterType();
          IGM.getStorageType(getSILFuncConventions().getSILType(
              FnType->getErrorResult(), IGM.getMaximalTypeExpansionContext()));
      auto selfIdx = components.size();
      AsyncResumeFunctionSwiftSelfIdx = selfIdx;
      components.push_back(errorType);
    }
  };

  // Direct result type as arguments.
  auto resultType = getSILFuncConventions().getSILResultType(
      IGM.getMaximalTypeExpansionContext());
  auto &ti = IGM.getTypeInfo(resultType);
  auto &native = ti.nativeReturnValueSchema(IGM);

  if (!getSILFuncConventions().hasIndirectSILResults() &&
      !getSILFuncConventions().hasIndirectSILErrorResults() &&
      getSILFuncConventions().funcTy->hasErrorResult() &&
      !native.requiresIndirect() && getSILFuncConventions().isTypedError()) {
    auto errorType = getSILFuncConventions().getSILErrorType(
        IGM.getMaximalTypeExpansionContext());
    auto &errorTi = IGM.getTypeInfo(errorType);
    auto &nativeError = errorTi.nativeReturnValueSchema(IGM);
    if (!nativeError.shouldReturnTypedErrorIndirectly()) {
      auto combined = combineResultAndTypedErrorType(IGM, native, nativeError);

      if (!combined.combinedTy->isVoidTy()) {
        if (auto *structTy = dyn_cast<llvm::StructType>(combined.combinedTy)) {
          for (auto *elem : structTy->elements()) {
            components.push_back(elem);
          }
        } else {
          components.push_back(combined.combinedTy);
        }
      }
      addErrorResult();
      ResultIRType = llvm::StructType::get(IGM.getLLVMContext(), components);
      return;
    }
  }

  if (native.requiresIndirect() || native.empty()) {
    addErrorResult();
    ResultIRType = llvm::StructType::get(IGM.getLLVMContext(), components);
    return;
  }

  // Add the result type components as trailing parameters.
  native.enumerateComponents(
      [&](clang::CharUnits offset, clang::CharUnits end, llvm::Type *type) {
        components.push_back(type);
      });

  addErrorResult();
  ResultIRType = llvm::StructType::get(IGM.getLLVMContext(), components);
}

Signature SignatureExpansion::getSignature() {
  // Create the appropriate LLVM type.
  llvm::FunctionType *llvmType =
    llvm::FunctionType::get(ResultIRType, ParamIRTypes, /*variadic*/ false);

  assert((ForeignInfo.ClangInfo != nullptr) ==
           (FnType->getLanguage() == SILFunctionLanguage::C) &&
         "C function type without C function info");

  auto callingConv =
      expandCallingConv(IGM, FnType->getRepresentation(), FnType->isAsync());

  Signature result;
  result.Type = llvmType;
  result.CallingConv = callingConv;
  result.Attributes = Attrs;
  using ExtraData = Signature::ExtraData;
  if (FnType->getLanguage() == SILFunctionLanguage::C) {
    // This is a potentially throwing function. The use of 'noexcept' /
    // 'nothrow' is applied at the call site in the \c FunctionPointer class.
    ForeignInfo.canThrow = IGM.isForeignExceptionHandlingEnabled();
    result.ExtraDataKind = ExtraData::kindForMember<ForeignFunctionInfo>();
    result.ExtraDataStorage.emplace<ForeignFunctionInfo>(result.ExtraDataKind,
                                                         ForeignInfo);
  } else if (FnType->isCoroutine()) {
    result.ExtraDataKind = ExtraData::kindForMember<CoroutineInfo>();
    result.ExtraDataStorage.emplace<CoroutineInfo>(result.ExtraDataKind,
                                                   CoroInfo);
  } else if (FnType->isAsync()) {
    result.ExtraDataKind = ExtraData::kindForMember<AsyncInfo>();
    AsyncInfo info;
    info.AsyncContextIdx = AsyncContextIdx;
    info.AsyncResumeFunctionSwiftSelfIdx = AsyncResumeFunctionSwiftSelfIdx;
    result.ExtraDataStorage.emplace<AsyncInfo>(result.ExtraDataKind, info);
  } else {
    result.ExtraDataKind = ExtraData::kindForMember<void>();
  }
  return result;
}

Signature Signature::getUncached(IRGenModule &IGM,
                                 CanSILFunctionType formalType,
                                 FunctionPointerKind fpKind, bool forStaticCall,
                                 const clang::CXXConstructorDecl *cxxCtorDecl) {
  GenericContextScope scope(IGM, formalType->getInvocationGenericSignature());
  SignatureExpansion expansion(IGM, formalType, fpKind, forStaticCall,
                               cxxCtorDecl);
  expansion.expandFunctionType();
  return expansion.getSignature();
}

SignatureExpansionABIDetails Signature::getUncachedABIDetails(
    IRGenModule &IGM, CanSILFunctionType formalType, FunctionPointerKind kind) {
  GenericContextScope scope(IGM, formalType->getInvocationGenericSignature());
  SignatureExpansion expansion(IGM, formalType, kind);
  SignatureExpansionABIDetails result;
  expansion.expandFunctionType(&result);
  result.numParamIRTypesInSignature = expansion.ParamIRTypes.size();
  return result;
}

Signature Signature::forCoroutineContinuation(IRGenModule &IGM,
                                              CanSILFunctionType fnType) {
  assert(fnType->isCoroutine());
  SignatureExpansion expansion(IGM, fnType, FunctionPointerKind(fnType));
  expansion.expandCoroutineContinuationType();
  return expansion.getSignature();
}

Signature Signature::forAsyncReturn(IRGenModule &IGM,
                                    CanSILFunctionType fnType) {
  assert(fnType->isAsync());
  GenericContextScope scope(IGM, fnType->getInvocationGenericSignature());
  SignatureExpansion expansion(IGM, fnType, FunctionPointerKind(fnType));
  expansion.expandAsyncReturnType();
  return expansion.getSignature();
}

Signature Signature::forAsyncAwait(IRGenModule &IGM, CanSILFunctionType fnType,
                                   FunctionPointerKind fnKind) {
  assert(fnType->isAsync());
  GenericContextScope scope(IGM, fnType->getInvocationGenericSignature());
  SignatureExpansion expansion(IGM, fnType, fnKind);
  expansion.expandAsyncAwaitType();
  return expansion.getSignature();
}

Signature Signature::forAsyncEntry(IRGenModule &IGM, CanSILFunctionType fnType,
                                   FunctionPointerKind fnKind) {
  assert(fnType->isAsync());
  GenericContextScope scope(IGM, fnType->getInvocationGenericSignature());
  SignatureExpansion expansion(IGM, fnType, fnKind);
  expansion.expandAsyncEntryType();
  return expansion.getSignature();
}

void irgen::extractScalarResults(IRGenFunction &IGF, llvm::Type *bodyType,
                                 llvm::Value *call, Explosion &out) {
  assert(!bodyType->isVoidTy() && "Unexpected void result type!");

  auto *returned = call;
  auto *callType = call->getType();

  // If the type of the result of the call differs from the type used
  // elsewhere in the caller due to ABI type coercion, we need to
  // coerce the result back from the ABI type before extracting the
  // elements.
  if (bodyType != callType)
    returned = IGF.coerceValue(returned, bodyType, IGF.IGM.DataLayout);

  if (auto *structType = dyn_cast<llvm::StructType>(bodyType))
    IGF.emitAllExtractValues(returned, structType, out);
  else
    out.add(returned);
}

void IRGenFunction::emitAllExtractValues(llvm::Value *value,
                                         llvm::StructType *structType,
                                         Explosion &out) {
  assert(value->getType() == structType);
  for (unsigned i = 0, e = structType->getNumElements(); i != e; ++i)
    out.add(Builder.CreateExtractValue(value, i));
}

namespace {
// TODO(compnerd) analyze if this should be out-lined via a runtime call rather
// than be open-coded.  This needs to account for the fact that we are able to
// statically optimize this often times due to CVP changing the select to a
// `select i1 true, ...`.
llvm::Value *emitIndirectAsyncFunctionPointer(IRGenFunction &IGF,
                                              llvm::Value *pointer) {
  llvm::IntegerType *IntPtrTy = IGF.IGM.IntPtrTy;
  llvm::Type *AsyncFunctionPointerPtrTy = IGF.IGM.AsyncFunctionPointerPtrTy;
  llvm::Constant *Zero =
      llvm::Constant::getIntegerValue(IntPtrTy, APInt(IntPtrTy->getBitWidth(),
                                                      0));
  llvm::Constant *One =
      llvm::Constant::getIntegerValue(IntPtrTy, APInt(IntPtrTy->getBitWidth(),
                                                      1));
  llvm::Constant *NegativeOne =
      llvm::Constant::getIntegerValue(IntPtrTy, APInt(IntPtrTy->getBitWidth(),
                                                      -2));
  swift::irgen::Alignment PointerAlignment = IGF.IGM.getPointerAlignment();

  llvm::Value *PtrToInt = IGF.Builder.CreatePtrToInt(pointer, IntPtrTy);
  llvm::Value *And = IGF.Builder.CreateAnd(PtrToInt, One);
  llvm::Value *ICmp = IGF.Builder.CreateICmpEQ(And, Zero);

  llvm::Value *BitCast =
      IGF.Builder.CreateBitCast(pointer, AsyncFunctionPointerPtrTy);

  llvm::Value *UntaggedPointer = IGF.Builder.CreateAnd(PtrToInt, NegativeOne);
  llvm::Value *IntToPtr =
      IGF.Builder.CreateIntToPtr(UntaggedPointer,
                                 AsyncFunctionPointerPtrTy->getPointerTo());
  llvm::Value *Load = IGF.Builder.CreateLoad(
      IntToPtr, AsyncFunctionPointerPtrTy, PointerAlignment);

  // (select (icmp eq, (and (ptrtoint %AsyncFunctionPointer), 1), 0),
  //         (%AsyncFunctionPointer),
  //         (inttoptr (and (ptrtoint %AsyncFunctionPointer), -2)))
  return IGF.Builder.CreateSelect(ICmp, BitCast, Load);
}
}

std::pair<llvm::Value *, llvm::Value *> irgen::getAsyncFunctionAndSize(
    IRGenFunction &IGF, SILFunctionTypeRepresentation representation,
    FunctionPointer functionPointer, llvm::Value *thickContext,
    std::pair<bool, bool> values) {
  assert(values.first || values.second);
  assert(functionPointer.getKind() != FunctionPointer::Kind::Function);

  bool emitFunction = values.first;
  bool emitSize = values.second;
  assert(emitFunction || emitSize);

  // Ensure that the AsyncFunctionPointer is not auth'd if it is not used and
  // that it is not auth'd more than once if it is needed.
  //
  // The AsyncFunctionPointer is not needed in the case where only the function
  // is being loaded and the FunctionPointer was created from a function_ref
  // instruction.
  std::optional<llvm::Value *> afpPtrValue = std::nullopt;
  auto getAFPPtr = [&]() {
    if (!afpPtrValue) {
      auto *ptr = functionPointer.getRawPointer();
      if (auto authInfo = functionPointer.getAuthInfo()) {
        ptr = emitPointerAuthAuth(IGF, ptr, authInfo);
      }
      afpPtrValue =
          (IGF.IGM.getOptions().IndirectAsyncFunctionPointer)
              ? emitIndirectAsyncFunctionPointer(IGF, ptr)
              : IGF.Builder.CreateBitCast(ptr,
                                          IGF.IGM.AsyncFunctionPointerPtrTy);
    }
    return *afpPtrValue;
  };

  llvm::Value *fn = nullptr;
  if (emitFunction) {
    // If the FP is not an async FP, then we just have the direct
    // address of the async function.  This only happens for special
    // async functions right now.
    if (!functionPointer.getKind().isAsyncFunctionPointer()) {
      assert(functionPointer.getStaticAsyncContextSize(IGF.IGM));
      fn = functionPointer.getRawPointer();

    // If we've opportunistically also emitted the direct address of the
    // function, always prefer that.
    } else if (auto *function = functionPointer.getRawAsyncFunction()) {
      fn = function;

    // Otherwise, extract the function pointer from the async FP structure.
    } else {
      llvm::Value *addrPtr = IGF.Builder.CreateStructGEP(
          IGF.IGM.AsyncFunctionPointerTy, getAFPPtr(), 0);
      fn = IGF.emitLoadOfCompactFunctionPointer(
          Address(addrPtr, IGF.IGM.RelativeAddressTy,
                  IGF.IGM.getPointerAlignment()),
          /*isFar*/ false,
          /*expectedType*/ functionPointer.getFunctionType());
    }

    if (auto authInfo =
            functionPointer.getAuthInfo().getCorrespondingCodeAuthInfo()) {
      fn = emitPointerAuthSign(IGF, fn, authInfo);
    }
  }

  llvm::Value *size = nullptr;
  if (emitSize) {
    if (auto staticSize = functionPointer.getStaticAsyncContextSize(IGF.IGM)) {
      size = llvm::ConstantInt::get(IGF.IGM.Int32Ty, staticSize->getValue());
    } else {
      auto *sizePtr = IGF.Builder.CreateStructGEP(
          IGF.IGM.AsyncFunctionPointerTy, getAFPPtr(), 1);
      size = IGF.Builder.CreateLoad(sizePtr, IGF.IGM.Int32Ty,
                                    IGF.IGM.getPointerAlignment());
    }
  }
  return {fn, size};
}

namespace {

class SyncCallEmission final : public CallEmission {
  using super = CallEmission;

public:
  SyncCallEmission(IRGenFunction &IGF, llvm::Value *selfValue, Callee &&callee)
      : CallEmission(IGF, selfValue, std::move(callee)) {
    setFromCallee();
  }

  FunctionPointer getCalleeFunctionPointer() override {
    return getCallee().getFunctionPointer().getAsFunction(IGF);
  }
  SILType getParameterType(unsigned index) override {
    SILFunctionConventions origConv(getCallee().getOrigFunctionType(),
                                    IGF.getSILModule());
    return origConv.getSILArgumentType(
        index, IGF.IGM.getMaximalTypeExpansionContext());
  }

  llvm::CallBase *createCall(const FunctionPointer &fn,
                             ArrayRef<llvm::Value *> args) override {
    return IGF.Builder.CreateCallOrInvoke(fn, Args, invokeNormalDest,
                                          invokeUnwindDest);
  }

  void begin() override { super::begin(); }
  void end() override { super::end(); }
  void setFromCallee() override {
    super::setFromCallee();

    auto fnType = CurCallee.getOrigFunctionType();

    if (fnType->getRepresentation() ==
        SILFunctionTypeRepresentation::WitnessMethod) {
      unsigned n = getTrailingWitnessSignatureLength(IGF.IGM, fnType);
      while (n--) {
        Args[--LastArgWritten] = nullptr;
      }
    }

    llvm::Value *contextPtr = CurCallee.getSwiftContext();

    // Add the error result if we have one.
    if (fnType->hasErrorResult()) {
      // The invariant is that this is always zero-initialized, so we
      // don't need to do anything extra here.
      auto substFnType = CurCallee.getSubstFunctionType();
      SILFunctionConventions fnConv(substFnType, IGF.getSILModule());
      Address errorResultSlot = IGF.getCalleeErrorResultSlot(
          fnConv.getSILErrorType(IGF.IGM.getMaximalTypeExpansionContext()),
          fnConv.isTypedError());

      assert(LastArgWritten > 0);
      if (fnConv.isTypedError()) {
        if (fnConv.hasIndirectSILErrorResults()) {
          // We will set the value later when lowering the arguments.
          setIndirectTypedErrorResultSlotArgsIndex(--LastArgWritten);
          Args[LastArgWritten] = nullptr;
        } else {
          auto silResultTy =
              fnConv.getSILResultType(IGF.IGM.getMaximalTypeExpansionContext());
          auto silErrorTy =
              fnConv.getSILErrorType(IGF.IGM.getMaximalTypeExpansionContext());

          auto &nativeSchema =
              IGF.IGM.getTypeInfo(silResultTy).nativeReturnValueSchema(IGF.IGM);
          auto &errorSchema =
              IGF.IGM.getTypeInfo(silErrorTy).nativeReturnValueSchema(IGF.IGM);

          if (fnConv.hasIndirectSILResults() ||
              nativeSchema.requiresIndirect() ||
              errorSchema.shouldReturnTypedErrorIndirectly()) {
            // Return the error indirectly.
            auto buf = IGF.getCalleeTypedErrorResultSlot(silErrorTy);
            Args[--LastArgWritten] = buf.getAddress();
          }
        }
      }
      Args[--LastArgWritten] = errorResultSlot.getAddress();
      addParamAttribute(LastArgWritten, llvm::Attribute::NoCapture);
      IGF.IGM.addSwiftErrorAttributes(CurCallee.getMutableAttributes(),
                                      LastArgWritten);

      // Fill in the context pointer if necessary.
      if (!contextPtr) {
        assert(!CurCallee.getOrigFunctionType()->getExtInfo().hasContext() &&
               "Missing context?");
        contextPtr = llvm::UndefValue::get(IGF.IGM.RefCountedPtrTy);
      }
    }

    // Add the data pointer if we have one.
    // (Note that we're emitting backwards, so this correctly goes
    // *before* the error pointer.)
    if (contextPtr) {
      assert(LastArgWritten > 0);
      Args[--LastArgWritten] = contextPtr;
      IGF.IGM.addSwiftSelfAttributes(CurCallee.getMutableAttributes(),
                                     LastArgWritten);
    }
  }
  void setArgs(Explosion &original, bool isOutlined,
               WitnessMetadata *witnessMetadata) override {
    // Convert arguments to a representation appropriate to the calling
    // convention.
    Explosion adjusted;

    auto origCalleeType = CurCallee.getOrigFunctionType();
    SILFunctionConventions fnConv(origCalleeType, IGF.getSILModule());

    // Pass along the indirect result pointers.
    auto passIndirectResults = [&]() {
        original.transferInto(adjusted, fnConv.getNumIndirectSILResults());
    };
    // Indirect results for C++ methods can come
    // after `this`.
    if (getCallee().getRepresentation() !=
        SILFunctionTypeRepresentation::CXXMethod)
      passIndirectResults();

    // Pass along the coroutine buffer.
    switch (origCalleeType->getCoroutineKind()) {
    case SILCoroutineKind::YieldOnce2:
      if (IGF.IGM.IRGen.Opts.EmitYieldOnce2AsYieldOnce) {
        LLVM_FALLTHROUGH;
      } else {
        llvm::report_fatal_error("unimplemented");
        break;
      }
    case SILCoroutineKind::YieldOnce:
    case SILCoroutineKind::YieldMany:
      original.transferInto(adjusted, 1);
      break;

    case SILCoroutineKind::None:
      break;
    }

    // Translate the formal arguments and handle any special arguments.
    switch (getCallee().getRepresentation()) {
    case SILFunctionTypeRepresentation::ObjCMethod:
      adjusted.add(getCallee().getObjCMethodReceiver());
      if (!getCallee().isDirectObjCMethod())
        adjusted.add(getCallee().getObjCMethodSelector());
      externalizeArguments(IGF, getCallee(), original, adjusted, Temporaries,
                           isOutlined);
      break;

    case SILFunctionTypeRepresentation::Block:
    case SILFunctionTypeRepresentation::CXXMethod:
      if (getCallee().getRepresentation() == SILFunctionTypeRepresentation::Block) {
        adjusted.add(getCallee().getBlockObject());
      } else {
        auto selfParam = origCalleeType->getSelfParameter();
        auto *arg = getCallee().getCXXMethodSelf();
        // We might need to fix the level of indirection for foreign reference types.
        if (selfParam.getInterfaceType().isForeignReferenceType() &&
            isIndirectFormalParameter(selfParam.getConvention())) {
          auto paramTy = fnConv.getSILType(
              selfParam, IGF.IGM.getMaximalTypeExpansionContext());
          auto &paramTI = cast<FixedTypeInfo>(IGF.IGM.getTypeInfo(paramTy));
          arg = IGF.Builder.CreateLoad(arg, paramTI.getStorageType(),
                                       IGF.IGM.getPointerAlignment());
        }

        // Windows ABI places `this` before the
        // returned indirect values.
        auto &returnInfo =
            getCallee().getForeignInfo().ClangInfo->getReturnInfo();
        if (returnInfo.isIndirect() && !returnInfo.isSRetAfterThis())
          passIndirectResults();
        adjusted.add(arg);
        if (returnInfo.isIndirect() && returnInfo.isSRetAfterThis())
          passIndirectResults();
      }

      LLVM_FALLTHROUGH;

    case SILFunctionTypeRepresentation::CFunctionPointer:
      externalizeArguments(IGF, getCallee(), original, adjusted, Temporaries,
                           isOutlined);
      break;

    case SILFunctionTypeRepresentation::KeyPathAccessorGetter:
    case SILFunctionTypeRepresentation::KeyPathAccessorSetter:
    case SILFunctionTypeRepresentation::KeyPathAccessorEquals:
    case SILFunctionTypeRepresentation::KeyPathAccessorHash:
      setKeyPathAccessorArguments(original, isOutlined, adjusted);
      break;
    case SILFunctionTypeRepresentation::WitnessMethod:
      assert(witnessMetadata);
      assert(witnessMetadata->SelfMetadata->getType() ==
             IGF.IGM.TypeMetadataPtrTy);
      assert(witnessMetadata->SelfWitnessTable->getType() ==
             IGF.IGM.WitnessTablePtrTy);
      Args.rbegin()[1] = witnessMetadata->SelfMetadata;
      Args.rbegin()[0] = witnessMetadata->SelfWitnessTable;
      LLVM_FALLTHROUGH;

    case SILFunctionTypeRepresentation::Closure:
    case SILFunctionTypeRepresentation::Method:
    case SILFunctionTypeRepresentation::Thin:
    case SILFunctionTypeRepresentation::Thick: {
      // Check for value arguments that need to be passed indirectly.
      // But don't expect to see 'self' if it's been moved to the context
      // position.
      auto params = origCalleeType->getParameters();
      if (hasSelfContextParameter(origCalleeType)) {
        params = params.drop_back();
      }
      for (auto param : params) {
        addNativeArgument(IGF, original, origCalleeType, param, adjusted,
                          isOutlined);
      }

      // Anything else, just pass along.  This will include things like
      // generic arguments.
      adjusted.add(original.claimAll());

      break;
    }
    }
    super::setArgs(adjusted, isOutlined, witnessMetadata);
  }
  void emitCallToUnmappedExplosion(llvm::CallBase *call,
                                   Explosion &out) override {
    SILFunctionConventions fnConv(getCallee().getOrigFunctionType(),
                                  IGF.getSILModule());
    bool mayReturnErrorDirectly = mayReturnTypedErrorDirectly();

    // Bail out immediately on a void result.
    llvm::Value *result = call;
    if (result->getType()->isVoidTy() && !mayReturnErrorDirectly)
      return;

    // If the result was returned autoreleased, implicitly insert the reclaim.
    // This is only allowed on a single direct result.
    if (fnConv.getNumDirectSILResults() == 1
        && (fnConv.getDirectSILResults().begin()->getConvention()
            == ResultConvention::Autoreleased)) {
      if (IGF.IGM.Context.LangOpts.EnableObjCInterop)
        result = emitObjCRetainAutoreleasedReturnValue(IGF, result);
      else
        IGF.emitNativeStrongRetain(result, IGF.getDefaultAtomicity());
    }

    auto origFnType = getCallee().getOrigFunctionType();

    // Specially handle noreturn c function which would return a 'Never' SIL result
    // type.
    if (origFnType->getLanguage() == SILFunctionLanguage::C &&
        origFnType->isNoReturnFunction(
            IGF.getSILModule(), IGF.IGM.getMaximalTypeExpansionContext())) {
      auto clangResultTy = result->getType();
      extractScalarResults(IGF, clangResultTy, result, out);
      return;
    }

    SILType resultType;
    if (convertDirectToIndirectReturn) {
      resultType = SILType::getPrimitiveObjectType(
              origFnType->getSingleResult().getReturnValueType(
              IGF.IGM.getSILModule(), origFnType, TypeExpansionContext::minimal()));
    } else {
      // Get the natural IR type in the body of the function that makes
      // the call. This may be different than the IR type returned by the
      // call itself due to ABI type coercion.
      resultType =
          fnConv.getSILResultType(IGF.IGM.getMaximalTypeExpansionContext());
    }

    auto &nativeSchema = IGF.IGM.getTypeInfo(resultType).nativeReturnValueSchema(IGF.IGM);

    // Handle direct return of typed errors
    if (mayReturnErrorDirectly && !nativeSchema.requiresIndirect()) {
      return emitToUnmappedExplosionWithDirectTypedError(resultType, result,
                                                         out);
    }

    if (result->getType()->isVoidTy())
      return;

    // For ABI reasons the result type of the call might not actually match the
    // expected result type.
    //
    // This can happen when calling C functions, or class method dispatch thunks
    // for methods that have covariant ABI-compatible overrides.
    auto expectedNativeResultType = nativeSchema.getExpandedType(IGF.IGM);
    // If the expected result type is void, bail.
    if (expectedNativeResultType->isVoidTy())
      return;
    if (result->getType() != expectedNativeResultType) {
      result =
          IGF.coerceValue(result, expectedNativeResultType, IGF.IGM.DataLayout);
    }
    if (convertDirectToIndirectReturn) {
      IGF.Builder.CreateStore(result, indirectReturnAddress);
      return;
    }

    // Gather the values.
    Explosion nativeExplosion;
    extractScalarResults(IGF, result->getType(), result, nativeExplosion);

    out = nativeSchema.mapFromNative(IGF.IGM, IGF, nativeExplosion, resultType);
  }
  Address getCalleeErrorSlot(SILType errorType, bool isCalleeAsync) override {
    SILFunctionConventions fnConv(getCallee().getOrigFunctionType(),
                                  IGF.getSILModule());

    return IGF.getCalleeErrorResultSlot(errorType, fnConv.isTypedError());
  };

  llvm::Value *getResumeFunctionPointer() override {
    llvm_unreachable("Should not call getResumeFunctionPointer on a sync call");
  }

  llvm::Value *getAsyncContext() override {
    llvm_unreachable("Should not call getAsyncContext on a sync call");
  }
};

class AsyncCallEmission final : public CallEmission {
  using super = CallEmission;

  Address contextBuffer;
  Address context;
  llvm::Value *calleeFunction = nullptr;
  llvm::Value *currentResumeFn = nullptr;
  llvm::Value *thickContext = nullptr;
  Size staticContextSize = Size(0);
  std::optional<AsyncContextLayout> asyncContextLayout;

  AsyncContextLayout getAsyncContextLayout() {
    if (!asyncContextLayout) {
      asyncContextLayout.emplace(::getAsyncContextLayout(
          IGF.IGM, getCallee().getOrigFunctionType(),
          getCallee().getSubstFunctionType(), getCallee().getSubstitutions()));
    }
    return *asyncContextLayout;
  }

  void saveValue(ElementLayout layout, llvm::Value *value, bool isOutlined) {
    Address addr = layout.project(IGF, context, /*offsets*/ std::nullopt);
    auto &ti = cast<LoadableTypeInfo>(layout.getType());
    Explosion explosion;
    explosion.add(value);
    ti.initialize(IGF, explosion, addr, isOutlined);
  }
  void loadValue(ElementLayout layout, Explosion &explosion) {
    Address addr = layout.project(IGF, context, /*offsets*/ std::nullopt);
    auto &ti = cast<LoadableTypeInfo>(layout.getType());
    ti.loadAsTake(IGF, addr, explosion);
  }

public:
  AsyncCallEmission(IRGenFunction &IGF, llvm::Value *selfValue, Callee &&callee)
      : CallEmission(IGF, selfValue, std::move(callee)) {
    setFromCallee();
  }

  void begin() override {
    super::begin();
    assert(!contextBuffer.isValid());
    assert(!context.isValid());
    auto layout = getAsyncContextLayout();
    // Allocate space for the async context.

    llvm::Value *dynamicContextSize32;
    std::tie(calleeFunction, dynamicContextSize32) = getAsyncFunctionAndSize(
        IGF, CurCallee.getOrigFunctionType()->getRepresentation(),
        CurCallee.getFunctionPointer(), thickContext);
    auto *dynamicContextSize =
        IGF.Builder.CreateZExt(dynamicContextSize32, IGF.IGM.SizeTy);
    if (auto staticSize = dyn_cast<llvm::ConstantInt>(dynamicContextSize)) {
      staticContextSize = Size(staticSize->getZExtValue());
      assert(!staticContextSize.isZero());
      contextBuffer = emitStaticAllocAsyncContext(IGF, staticContextSize);
    } else {
      contextBuffer = emitAllocAsyncContext(IGF, dynamicContextSize);
    }
    context = layout.emitCastTo(IGF, contextBuffer.getAddress());
  }
  void end() override {
    assert(contextBuffer.isValid());
    assert(context.isValid());
    if (getCallee().getStaticAsyncContextSize(IGF.IGM)) {
      assert(!staticContextSize.isZero());
      emitStaticDeallocAsyncContext(IGF, contextBuffer, staticContextSize);
    } else {
      emitDeallocAsyncContext(IGF, contextBuffer);
    }
    super::end();
  }
  void setFromCallee() override {
    thickContext = nullptr; // TODO: this should go

    super::setFromCallee();

    auto fnType = CurCallee.getOrigFunctionType();

    if (fnType->getRepresentation() ==
        SILFunctionTypeRepresentation::WitnessMethod) {
      unsigned n = getTrailingWitnessSignatureLength(IGF.IGM, fnType);
      while (n--) {
        Args[--LastArgWritten] = nullptr;
      }
    }

    // Add the indirect typed error result if we have one.
    SILFunctionConventions fnConv(fnType, IGF.getSILModule());
    if (fnType->hasErrorResult() && fnConv.isTypedError()) {
      // The invariant is that this is always zero-initialized, so we
      // don't need to do anything extra here.
      assert(LastArgWritten > 0);
      // Return the error indirectly.
      if (fnConv.hasIndirectSILErrorResults()) {
          // We will set the value later when lowering the arguments.
          setIndirectTypedErrorResultSlotArgsIndex(--LastArgWritten);
          Args[LastArgWritten] = nullptr;
      } else {
        auto silResultTy =
            fnConv.getSILResultType(IGF.IGM.getMaximalTypeExpansionContext());
        auto silErrorTy =
            fnConv.getSILErrorType(IGF.IGM.getMaximalTypeExpansionContext());

        auto &nativeSchema =
            IGF.IGM.getTypeInfo(silResultTy).nativeReturnValueSchema(IGF.IGM);
        auto &errorSchema =
            IGF.IGM.getTypeInfo(silErrorTy).nativeReturnValueSchema(IGF.IGM);

        if (nativeSchema.requiresIndirect() ||
            errorSchema.shouldReturnTypedErrorIndirectly() ||
            (errorSchema.empty() &&
             fnConv.hasIndirectSILResults())) { // direct empty typed errors are
                                                // passed
          // indirectly for compatibility with generic
          // functions.
          // Return the error indirectly.
          auto buf = IGF.getCalleeTypedErrorResultSlot(silErrorTy);
          Args[--LastArgWritten] = buf.getAddress();
        }
      }
    }

    llvm::Value *contextPtr = CurCallee.getSwiftContext();
    // Add the data pointer if we have one.
    if (contextPtr) {
      assert(LastArgWritten > 0);
      Args[--LastArgWritten] = contextPtr;
      IGF.IGM.addSwiftSelfAttributes(CurCallee.getMutableAttributes(),
                                     LastArgWritten);
    }
  }

  FunctionPointer getCalleeFunctionPointer() override {
    PointerAuthInfo codeAuthInfo = CurCallee.getFunctionPointer()
                                       .getAuthInfo()
                                       .getCorrespondingCodeAuthInfo();
    auto awaitSig =
        Signature::forAsyncAwait(IGF.IGM, getCallee().getOrigFunctionType(),
                                 getCallee().getFunctionPointer().getKind());
    auto awaitEntrySig =
        Signature::forAsyncEntry(IGF.IGM, getCallee().getOrigFunctionType(),
                                 getCallee().getFunctionPointer().getKind());

    return FunctionPointer::createForAsyncCall(
        IGF.Builder.CreateBitCast(calleeFunction,
                                  awaitEntrySig.getType()->getPointerTo()),
        codeAuthInfo, awaitSig, awaitEntrySig.getType());
  }

  SILType getParameterType(unsigned index) override {
    SILFunctionConventions origConv(getCallee().getOrigFunctionType(),
                                    IGF.getSILModule());
    return origConv.getSILArgumentType(
        index, IGF.IGM.getMaximalTypeExpansionContext());
  }
  void setArgs(Explosion &original, bool isOutlined,
               WitnessMetadata *witnessMetadata) override {
    Explosion asyncExplosion;
    // Convert arguments to a representation appropriate to the calling
    // convention.

    auto origCalleeType = CurCallee.getOrigFunctionType();
    SILFunctionConventions fnConv(origCalleeType, IGF.getSILModule());

    // Pass along the indirect result pointers.
    original.transferInto(asyncExplosion, fnConv.getNumIndirectSILResults());

    // Pass the async context.  For special direct-continuation functions,
    // we pass our own async context; otherwise we pass the context
    // we created.
    if (getCallee().shouldPassContinuationDirectly()) {
      asyncExplosion.add(IGF.getAsyncContext());
    } else
      asyncExplosion.add(contextBuffer.getAddress());

    // Pass along the coroutine buffer.
    switch (origCalleeType->getCoroutineKind()) {
    case SILCoroutineKind::YieldMany:
    case SILCoroutineKind::YieldOnce:
    case SILCoroutineKind::YieldOnce2:
      assert(false && "Should not reach this");
      break;

    case SILCoroutineKind::None:
      break;
    }

    // Translate the formal arguments and handle any special arguments.
    switch (getCallee().getRepresentation()) {
    case SILFunctionTypeRepresentation::ObjCMethod:
    case SILFunctionTypeRepresentation::Block:
    case SILFunctionTypeRepresentation::CFunctionPointer:
    case SILFunctionTypeRepresentation::CXXMethod:
    case SILFunctionTypeRepresentation::KeyPathAccessorGetter:
    case SILFunctionTypeRepresentation::KeyPathAccessorSetter:
    case SILFunctionTypeRepresentation::KeyPathAccessorEquals:
    case SILFunctionTypeRepresentation::KeyPathAccessorHash:
      assert(false && "Should not reach this");
      break;

    case SILFunctionTypeRepresentation::WitnessMethod:
      assert(witnessMetadata);
      assert(witnessMetadata->SelfMetadata->getType() ==
             IGF.IGM.TypeMetadataPtrTy);
      assert(witnessMetadata->SelfWitnessTable->getType() ==
             IGF.IGM.WitnessTablePtrTy);
      Args.rbegin()[1] = witnessMetadata->SelfMetadata;
      Args.rbegin()[0] = witnessMetadata->SelfWitnessTable;
      LLVM_FALLTHROUGH;

    case SILFunctionTypeRepresentation::Closure:
    case SILFunctionTypeRepresentation::Method:
    case SILFunctionTypeRepresentation::Thin:
    case SILFunctionTypeRepresentation::Thick: {
      // Check for value arguments that need to be passed indirectly.
      // But don't expect to see 'self' if it's been moved to the context
      // position.
      auto params = origCalleeType->getParameters();
      if (hasSelfContextParameter(origCalleeType)) {
        params = params.drop_back();
      }
      for (auto param : params) {
        addNativeArgument(IGF, original, origCalleeType, param, asyncExplosion,
                          isOutlined);
      }

      // Anything else, just pass along.  This will include things like
      // generic arguments.
      asyncExplosion.add(original.claimAll());

      break;
    }
    }
    super::setArgs(asyncExplosion, false, witnessMetadata);

    auto layout = getAsyncContextLayout();

    // Initialize the async context for returning if we're not using
    // the special convention which suppresses that.
    if (!getCallee().shouldPassContinuationDirectly()) {
      // Set the caller context to the current context.
      Explosion explosion;
      auto parentContextField = layout.getParentLayout();
      auto *context = IGF.getAsyncContext();
      if (auto schema = IGF.IGM.getOptions().PointerAuth.AsyncContextParent) {
        Address fieldAddr =
            parentContextField.project(IGF, this->context,
                                       /*offsets*/ std::nullopt);
        auto authInfo = PointerAuthInfo::emit(
            IGF, schema, fieldAddr.getAddress(), PointerAuthEntity());
        context = emitPointerAuthSign(IGF, context, authInfo);
      }
      saveValue(parentContextField, context, isOutlined);

      // Set the caller resumption function to the resumption function
      // for this suspension.
      assert(currentResumeFn == nullptr);
      auto resumeParentField = layout.getResumeParentLayout();
      currentResumeFn = IGF.Builder.CreateIntrinsicCall(
          llvm::Intrinsic::coro_async_resume, {});
      auto fnVal = currentResumeFn;
      // Sign the pointer.
      if (auto schema = IGF.IGM.getOptions().PointerAuth.AsyncContextResume) {
        Address fieldAddr = resumeParentField.project(IGF, this->context,
                                                      /*offsets*/ std::nullopt);
        auto authInfo = PointerAuthInfo::emit(
            IGF, schema, fieldAddr.getAddress(), PointerAuthEntity());
        fnVal = emitPointerAuthSign(IGF, fnVal, authInfo);
      }
      fnVal = IGF.Builder.CreateBitCast(fnVal,
                                        IGF.IGM.TaskContinuationFunctionPtrTy);
      saveValue(resumeParentField, fnVal, isOutlined);
    }
  }
  void emitCallToUnmappedExplosion(llvm::CallBase *call,
                                   Explosion &out) override {
    // Bail out on a void result type.
    auto &IGM = IGF.IGM;
    llvm::Value *result = call;
    auto *suspendResultTy = cast<llvm::StructType>(result->getType());
    auto numAsyncContextParams =
        Signature::forAsyncReturn(IGM, getCallee().getSubstFunctionType())
            .getAsyncContextIndex() +
        1;
    if (suspendResultTy->getNumElements() == numAsyncContextParams)
      return;

    auto &Builder = IGF.Builder;

    auto resultTys =
        llvm::ArrayRef(suspendResultTy->element_begin() + numAsyncContextParams,
                       suspendResultTy->element_end());

    auto substCalleeType = getCallee().getSubstFunctionType();
    SILFunctionConventions substConv(substCalleeType, IGF.getSILModule());
    auto hasError = substCalleeType->hasErrorResult();
    SILType errorType;
    if (hasError)
      errorType =
          substConv.getSILErrorType(IGM.getMaximalTypeExpansionContext());

    SILFunctionConventions fnConv(getCallee().getOrigFunctionType(),
                                  IGF.getSILModule());

    // Get the natural IR type in the body of the function that makes
    // the call. This may be different than the IR type returned by the
    // call itself due to ABI type coercion.
    auto resultType =
        fnConv.getSILResultType(IGF.IGM.getMaximalTypeExpansionContext());
    auto &nativeSchema =
        IGF.IGM.getTypeInfo(resultType).nativeReturnValueSchema(IGF.IGM);

    bool mayReturnErrorDirectly = mayReturnTypedErrorDirectly();
    if (mayReturnErrorDirectly && !nativeSchema.requiresIndirect()) {
      llvm::Value *resultAgg;

      auto directResultTys = resultTys.drop_back();
      if (directResultTys.size() == 1) {
        resultAgg = Builder.CreateExtractValue(result, numAsyncContextParams);
      } else {
        auto resultTy =
            llvm::StructType::get(IGM.getLLVMContext(), directResultTys);
        resultAgg = llvm::UndefValue::get(resultTy);
        for (unsigned i = 0, e = directResultTys.size(); i != e; ++i) {
          llvm::Value *elt =
              Builder.CreateExtractValue(result, numAsyncContextParams + i);
          resultAgg = Builder.CreateInsertValue(resultAgg, elt, i);
        }
      }
      emitToUnmappedExplosionWithDirectTypedError(resultType, resultAgg, out);
      auto errorResult = Builder.CreateExtractValue(
          result, numAsyncContextParams + directResultTys.size());
      Address errorAddr =
          IGF.getCalleeErrorResultSlot(errorType, substConv.isTypedError());
      Builder.CreateStore(errorResult, errorAddr);
      return;
    } else if (resultTys.size() == 1) {
      result = Builder.CreateExtractValue(result, numAsyncContextParams);
      if (hasError) {
        Address errorAddr = IGF.getCalleeErrorResultSlot(errorType,
																												 substConv.isTypedError());
        Builder.CreateStore(result, errorAddr);
        return;
      }
    } else if (resultTys.size() == 2 && hasError) {
      auto tmp = result;
      result = Builder.CreateExtractValue(result, numAsyncContextParams);
      auto errorResult =  Builder.CreateExtractValue(tmp, numAsyncContextParams + 1);
			Address errorAddr = IGF.getCalleeErrorResultSlot(errorType, substConv.isTypedError());
      Builder.CreateStore(errorResult, errorAddr);
    } else {
      auto directResultTys = hasError ? resultTys.drop_back() : resultTys;
      auto resultTy = llvm::StructType::get(IGM.getLLVMContext(), directResultTys);
      llvm::Value *resultAgg = llvm::UndefValue::get(resultTy);
      for (unsigned i = 0, e = directResultTys.size(); i != e; ++i) {
        llvm::Value *elt =
            Builder.CreateExtractValue(result, numAsyncContextParams + i);
        resultAgg = Builder.CreateInsertValue(resultAgg, elt, i);
      }
      if (hasError) {
        auto errorResult = Builder.CreateExtractValue(
            result, numAsyncContextParams + directResultTys.size());
        Address errorAddr = IGF.getCalleeErrorResultSlot(errorType, substConv.isTypedError());
        Builder.CreateStore(errorResult, errorAddr);
      }
      result = resultAgg;
    }

    // For ABI reasons the result type of the call might not actually match the
    // expected result type.
    //
    // This can happen when calling C functions, or class method dispatch thunks
    // for methods that have covariant ABI-compatible overrides.
    auto expectedNativeResultType = nativeSchema.getExpandedType(IGF.IGM);
    // If the expected result type is void, bail.
    if (expectedNativeResultType->isVoidTy())
      return;
    if (result->getType() != expectedNativeResultType) {
      result =
          IGF.coerceValue(result, expectedNativeResultType, IGF.IGM.DataLayout);
    }

    // Gather the values.
    Explosion nativeExplosion;
    extractScalarResults(IGF, result->getType(), result, nativeExplosion);

    out = nativeSchema.mapFromNative(IGF.IGM, IGF, nativeExplosion, resultType);
  }
  Address getCalleeErrorSlot(SILType errorType, bool isCalleeAsync) override {
		SILFunctionConventions fnConv(getCallee().getOrigFunctionType(),
                                  IGF.getSILModule());
    return IGF.getCalleeErrorResultSlot(errorType, fnConv.isTypedError());
  }

  llvm::CallBase *createCall(const FunctionPointer &fn,
                             ArrayRef<llvm::Value *> args) override {
    auto &IGM = IGF.IGM;
    auto &Builder = IGF.Builder;
    // Setup the suspend point.
    SmallVector<llvm::Value *, 8> arguments;
    auto signature = fn.getSignature();
    auto asyncContextIndex =
        signature.getAsyncContextIndex();
    auto paramAttributeFlags =
        asyncContextIndex |
        (signature.getAsyncResumeFunctionSwiftSelfIndex() << 8);
    // Index of swiftasync context | ((index of swiftself) << 8).
    arguments.push_back(
        IGM.getInt32(paramAttributeFlags));
    arguments.push_back(currentResumeFn);
    // The special direct-continuation convention will pass our context
    // when it resumes.  The standard convention passes the callee's
    // context, so we'll need to pop that off to get ours.
    auto resumeProjFn = getCallee().shouldPassContinuationDirectly()
                            ? IGF.getOrCreateResumeFromSuspensionFn()
                            : IGF.getOrCreateResumePrjFn();
    arguments.push_back(
        Builder.CreateBitOrPointerCast(resumeProjFn, IGM.Int8PtrTy));
    auto dispatchFn = IGF.createAsyncDispatchFn(
        getFunctionPointerForDispatchCall(IGM, fn), args);
    arguments.push_back(
        Builder.CreateBitOrPointerCast(dispatchFn, IGM.Int8PtrTy));
    arguments.push_back(
        Builder.CreateBitOrPointerCast(fn.getRawPointer(), IGM.Int8PtrTy));
    if (auto authInfo = fn.getAuthInfo()) {
      arguments.push_back(fn.getAuthInfo().getDiscriminator());
    }
    for (auto arg: args)
      arguments.push_back(arg);
    auto resultTy =
        cast<llvm::StructType>(signature.getType()->getReturnType());
    return IGF.emitSuspendAsyncCall(asyncContextIndex, resultTy, arguments);
  }
  llvm::Value *getResumeFunctionPointer() override {
    assert(getCallee().shouldPassContinuationDirectly());
    assert(currentResumeFn == nullptr);
    currentResumeFn =
        IGF.Builder.CreateIntrinsicCall(llvm::Intrinsic::coro_async_resume, {});
    auto signedResumeFn = currentResumeFn;
    // Sign the task resume function with the C function pointer schema.
    if (auto schema = IGF.IGM.getOptions().PointerAuth.FunctionPointers) {
      // Use the Clang type for TaskContinuationFunction*
      // to make this work with type diversity.
      if (schema.hasOtherDiscrimination())
        schema =
            IGF.IGM.getOptions().PointerAuth.ClangTypeTaskContinuationFunction;
      auto authInfo =
          PointerAuthInfo::emit(IGF, schema, nullptr, PointerAuthEntity());
      signedResumeFn = emitPointerAuthSign(IGF, signedResumeFn, authInfo);
    }
    return signedResumeFn;
  }


  llvm::Value *getAsyncContext() override {
    return contextBuffer.getAddress();
  }
};

} // end anonymous namespace

std::unique_ptr<CallEmission> irgen::getCallEmission(IRGenFunction &IGF,
                                                     llvm::Value *selfValue,
                                                     Callee &&callee) {
  if (callee.getOrigFunctionType()->isAsync()) {
    return std::make_unique<AsyncCallEmission>(IGF, selfValue,
                                               std::move(callee));
  } else {
    return std::make_unique<SyncCallEmission>(IGF, selfValue,
                                              std::move(callee));
  }
}

/// Emit the unsubstituted result of this call into the given explosion.
/// The unsubstituted result must be naturally returned directly.
void CallEmission::emitToUnmappedExplosion(Explosion &out) {
  assert(state == State::Emitting);
  assert(LastArgWritten == 0 && "emitting unnaturally to explosion");

  auto call = emitCallSite();

  emitCallToUnmappedExplosion(call, out);
}

/// Emit the unsubstituted result of this call to the given address.
/// The unsubstituted result must be naturally returned indirectly.
void CallEmission::emitToUnmappedMemory(Address result) {
  assert(state == State::Emitting);
  assert(LastArgWritten == 1 && "emitting unnaturally to indirect result");

  Args[0] = result.getAddress();

  auto *FI = getCallee().getForeignInfo().ClangInfo;
  if (FI && FI->getReturnInfo().isIndirect() &&
      FI->getReturnInfo().isSRetAfterThis() &&
      Args[1] == getCallee().getCXXMethodSelf()) {
    // C++ methods in MSVC ABI pass `this` before the
    // indirectly returned value.
    std::swap(Args[0], Args[1]);
    assert(!isa<llvm::UndefValue>(Args[1]));
  }
  SILFunctionConventions FnConv(CurCallee.getSubstFunctionType(),
                                IGF.getSILModule());

#ifndef NDEBUG
  LastArgWritten = 0; // appease an assert
#endif

  auto call = emitCallSite();

  // Async calls need to store the error result that is passed as a parameter.
  if (CurCallee.getSubstFunctionType()->isAsync()) {
    auto &IGM = IGF.IGM;
    auto &Builder = IGF.Builder;
    auto numAsyncContextParams =
        Signature::forAsyncReturn(IGM, CurCallee.getSubstFunctionType())
            .getAsyncContextIndex() +
        1;

    auto substCalleeType = CurCallee.getSubstFunctionType();
    SILFunctionConventions substConv(substCalleeType, IGF.getSILModule());
    auto hasError = substCalleeType->hasErrorResult();
    SILType errorType;
    if (hasError) {
      errorType =
          substConv.getSILErrorType(IGM.getMaximalTypeExpansionContext());
      auto result = Builder.CreateExtractValue(call, numAsyncContextParams);
      Address errorAddr = IGF.getCalleeErrorResultSlot(errorType,
																											 substConv.isTypedError());
      Builder.CreateStore(result, errorAddr);
    }
  }
}
static FunctionPointer getProfilingFuncFor(IRGenFunction &IGF,
                                           FunctionPointer fnToCall,
                                           Callee &callee) {
    auto genericFn = cast<llvm::Function>(fnToCall.getRawPointer());
    auto replacementTypes = callee.getSubstitutions().getReplacementTypes();
    llvm::SmallString<64> name;
    {
      llvm::raw_svector_ostream os(name);
      os << "__swift_prof_thunk__generic_func__";
      os << replacementTypes.size();
      os << "__";
      for (auto replTy : replacementTypes) {
        IRGenMangler mangler(IGF.IGM.Context);
        os << mangler.mangleTypeMetadataFull(replTy->getCanonicalType());
        os << "___";
      }
      os << "fun__";
    }
    auto *thunk = IGF.IGM.getOrCreateProfilingThunk(genericFn, name);
    return fnToCall.withProfilingThunk(thunk);
}

/// The private routine to ultimately emit a call or invoke instruction.
llvm::CallBase *CallEmission::emitCallSite() {
  assert(state == State::Emitting);
  assert(LastArgWritten == 0);
  assert(!EmittedCall);
  EmittedCall = true;

  // Make the call and clear the arguments array.
  FunctionPointer fn = getCalleeFunctionPointer();
  assert(fn.getKind().getBasicKind() == FunctionPointer::Kind::Function);
  auto fnTy = fn.getFunctionType();

  // Coerce argument types for those cases where the IR type required
  // by the ABI differs from the type used within the function body.
  assert(fnTy->getNumParams() == Args.size());
  for (int i = 0, e = fnTy->getNumParams(); i != e; ++i) {
    auto *paramTy = fnTy->getParamType(i);
    auto *argTy = Args[i]->getType();
    if (paramTy != argTy)
      Args[i] = IGF.coerceValue(Args[i], paramTy, IGF.IGM.DataLayout);
  }

  if (fn.canThrowForeignException()) {
    if (!fn.doesForeignCallCatchExceptionInThunk()) {
      invokeNormalDest = IGF.createBasicBlock("invoke.cont");
      invokeUnwindDest = IGF.createExceptionUnwindBlock();
    } else
      IGF.setCallsThunksWithForeignExceptionTraps();
  }

  auto fnToCall = fn;
  if (UseProfilingThunk) {
    assert(fnToCall.isConstant() && "Non constant function in profiling thunk");
    fnToCall = getProfilingFuncFor(IGF, fnToCall, CurCallee);
  }

  auto call = createCall(fnToCall, Args);
  if (invokeNormalDest)
    IGF.Builder.emitBlock(invokeNormalDest);

  // Make coroutines calls opaque to LLVM analysis.
  if (IsCoroutine) {
    // Go back and insert some instructions right before the call.
    // It's easier to do this than to mess around with copying and
    // modifying the FunctionPointer above.
    IGF.Builder.SetInsertPoint(call);

    // Insert a call to @llvm.coro.prepare.retcon, then bitcast to the right
    // function type.
    auto origCallee = call->getCalledOperand();
    llvm::Value *opaqueCallee = origCallee;
    opaqueCallee =
      IGF.Builder.CreateBitCast(opaqueCallee, IGF.IGM.Int8PtrTy);
    opaqueCallee = IGF.Builder.CreateIntrinsicCall(
        llvm::Intrinsic::coro_prepare_retcon, {opaqueCallee});
    opaqueCallee =
      IGF.Builder.CreateBitCast(opaqueCallee, origCallee->getType());
    call->setCalledFunction(fn.getFunctionType(), opaqueCallee);

    // Reset the insert point to after the call.
    IGF.Builder.SetInsertPoint(call->getParent());
  }

  Args.clear();

  // Destroy any temporaries we needed.
  // We don't do this for coroutines because we need to wait until the
  // coroutine is complete.
  if (!IsCoroutine) {
    Temporaries.destroyAll(IGF);

    for (auto &stackAddr : RawTempraries) {
      IGF.emitDeallocateDynamicAlloca(stackAddr);
    }

    // Clear the temporary set so that we can assert that there are no
    // temporaries later.
    Temporaries.clear();
    RawTempraries.clear();
  }

  // Return.
  return call;
}

llvm::CallBase *IRBuilder::CreateCallOrInvoke(
    const FunctionPointer &fn, ArrayRef<llvm::Value *> args,
    llvm::BasicBlock *invokeNormalDest, llvm::BasicBlock *invokeUnwindDest) {
  assert(fn.getKind().getBasicKind() == FunctionPointer::Kind::Function);
  SmallVector<llvm::OperandBundleDef, 1> bundles;

  // Add a pointer-auth bundle if necessary.
  if (const auto &authInfo = fn.getAuthInfo()) {
    auto key = getInt32(authInfo.getKey());
    auto discriminator = authInfo.getDiscriminator();
    llvm::Value *bundleArgs[] = { key, discriminator };
    bundles.emplace_back("ptrauth", bundleArgs);
  }

  assert(!isTrapIntrinsic(fn.getRawPointer()) && "Use CreateNonMergeableTrap");
  auto fnTy = cast<llvm::FunctionType>(fn.getFunctionType());
  llvm::CallBase *call;
  if (!fn.shouldUseInvoke())
    call = IRBuilderBase::CreateCall(fnTy, fn.getRawPointer(), args, bundles);
  else
    call =
        IRBuilderBase::CreateInvoke(fnTy, fn.getRawPointer(), invokeNormalDest,
                                    invokeUnwindDest, args, bundles);

  llvm::AttributeList attrs = fn.getAttributes();
  // If a parameter of a function is SRet, the corresponding argument should be
  // wrapped in SRet(...).
  if (auto func = dyn_cast<llvm::Function>(fn.getRawPointer())) {
    for (unsigned argIndex = 0; argIndex < func->arg_size(); ++argIndex) {
      if (func->hasParamAttribute(argIndex, llvm::Attribute::StructRet)) {
        llvm::AttrBuilder builder(func->getContext());
        // See if there is a sret parameter in the signature. There are cases
        // where the called function has a sret parameter, but the signature
        // doesn't (e.g., noreturn functions).
        llvm::Type *ty = attrs.getParamStructRetType(argIndex);
        if (!ty)
          ty = func->getParamStructRetType(argIndex);
        builder.addStructRetAttr(ty);
        attrs = attrs.addParamAttributes(func->getContext(), argIndex, builder);
      }
      if (func->hasParamAttribute(argIndex, llvm::Attribute::ByVal)) {
        llvm::AttrBuilder builder(func->getContext());
        builder.addByValAttr(func->getParamByValType(argIndex));
        attrs = attrs.addParamAttributes(func->getContext(), argIndex, builder);
      }
    }
  }
  call->setAttributes(attrs);
  call->setCallingConv(fn.getCallingConv());
  return call;
}

llvm::CallInst *IRBuilder::CreateCall(const FunctionPointer &fn,
                                      ArrayRef<llvm::Value *> args) {
  assert(!fn.shouldUseInvoke());
  return cast<llvm::CallInst>(CreateCallOrInvoke(
      fn, args, /*invokeNormalDest=*/nullptr, /*invokeUnwindDest=*/nullptr));
}

/// Emit the result of this call to memory.
void CallEmission::emitToMemory(Address addr,
                                const LoadableTypeInfo &indirectedResultTI,
                                bool isOutlined) {
  assert(state == State::Emitting);
  assert(LastArgWritten <= 1);

  // If the call is naturally to an explosion, emit it that way and
  // then initialize the temporary.
  if (LastArgWritten == 0) {
    Explosion result;
    emitToExplosion(result, isOutlined);
    indirectedResultTI.initialize(IGF, result, addr, isOutlined);
    return;
  }

  // Okay, we're naturally emitting to memory.
  Address origAddr = addr;

  auto origFnType = CurCallee.getOrigFunctionType();
  auto substFnType = CurCallee.getSubstFunctionType();

  // We're never being asked to do anything with *formal*
  // indirect results here, just the possibility of a direct-in-SIL
  // result that's actually being passed indirectly.
  //
  // TODO: SIL address lowering should be able to handle such cases earlier.
  auto origResultType =
      origFnType
          ->getDirectFormalResultsType(IGF.IGM.getSILModule(),
                                       IGF.IGM.getMaximalTypeExpansionContext())
          .getASTType();
  auto substResultType =
      substFnType
          ->getDirectFormalResultsType(IGF.IGM.getSILModule(),
                                       IGF.IGM.getMaximalTypeExpansionContext())
          .getASTType();

  if (origResultType->hasTypeParameter())
    origResultType = IGF.IGM.getGenericEnvironment()
      ->mapTypeIntoContext(origResultType)
      ->getCanonicalType();

  if (origResultType != substResultType) {
    auto origTy = IGF.IGM.getStorageTypeForLowered(origResultType);
    origAddr = IGF.Builder.CreateElementBitCast(origAddr, origTy);
  }

  emitToUnmappedMemory(origAddr);
}

static void emitCastToSubstSchema(IRGenFunction &IGF, Explosion &in,
                                  const ExplosionSchema &schema,
                                  Explosion &out) {
  assert(in.size() == schema.size());
  for (unsigned i = 0, e = schema.size(); i != e; ++i) {
    llvm::Type *expectedType = schema.begin()[i].getScalarType();
    llvm::Value *value = in.claimNext();
    if (value->getType() != expectedType)
      value = IGF.Builder.CreateBitCast(value, expectedType,
                                        value->getName() + ".asSubstituted");
    out.add(value);
  }
}

void CallEmission::emitYieldsToExplosion(Explosion &out) {
  assert(state == State::Emitting);
  // Emit the call site.
  auto call = emitCallSite();

  // Pull the raw return values out.
  Explosion rawReturnValues;
  extractScalarResults(IGF, call->getType(), call, rawReturnValues);

  auto coroInfo = getCallee().getSignature().getCoroutineInfo();

  // Go ahead and forward the continuation pointer as an opaque pointer.
  auto continuation = rawReturnValues.claimNext();
  out.add(continuation);

  // Collect the raw value components.
  Explosion rawYieldComponents;

  // Add all the direct yield components.
  rawYieldComponents.add(
    rawReturnValues.claim(coroInfo.NumDirectYieldComponents));

  // Add all the indirect yield components.
  assert(rawReturnValues.size() <= 1);
  if (!rawReturnValues.empty()) {
    // Extract the indirect yield buffer.
    auto indirectPointer = rawReturnValues.claimNext();
    auto indirectStructTy =
        cast<llvm::StructType>(coroInfo.indirectResultsType);
    auto layout = IGF.IGM.DataLayout.getStructLayout(indirectStructTy);
    Address indirectBuffer(indirectPointer, indirectStructTy,
                           Alignment(layout->getAlignment().value()));

    for (auto i : indices(indirectStructTy->elements())) {
      // Skip padding.
      if (indirectStructTy->getElementType(i)->isArrayTy())
        continue;

      auto eltAddr = IGF.Builder.CreateStructGEP(indirectBuffer, i, layout);
      rawYieldComponents.add(IGF.Builder.CreateLoad(eltAddr));
    }
  }

  auto substCoroType = getCallee().getSubstFunctionType();
  SILFunctionConventions fnConv(substCoroType, IGF.getSILModule());
  for (auto yield : fnConv.getYields()) {
    YieldSchema schema(IGF.IGM, fnConv, yield);

    // If the schema says it's indirect, then we expect a pointer.
    if (schema.isIndirect()) {
      auto pointer = IGF.Builder.CreateBitCast(rawYieldComponents.claimNext(),
                                               schema.getIndirectPointerType());

      // If it's formally indirect, then we should just add that pointer
      // to the output.
      if (schema.isFormalIndirect()) {
        out.add(pointer);
        continue;
      }

      // Otherwise, we need to load.
      auto &yieldTI = cast<LoadableTypeInfo>(schema.getTypeInfo());
      yieldTI.loadAsTake(IGF, yieldTI.getAddressForPointer(pointer), out);
      continue;
    }

    // Otherwise, it's direct.  Remap.
    const auto &directSchema = schema.getDirectSchema();
    Explosion eltValues;
    rawYieldComponents.transferInto(eltValues, directSchema.size());
    auto temp = directSchema.mapFromNative(IGF.IGM, IGF, eltValues,
                                           schema.getSILType());

    auto &yieldTI = cast<LoadableTypeInfo>(schema.getTypeInfo());
    emitCastToSubstSchema(IGF, temp, yieldTI.getSchema(), out);
  }
}

/// Emit the result of this call to an explosion.
void CallEmission::emitToExplosion(Explosion &out, bool isOutlined) {
  assert(state == State::Emitting);
  assert(LastArgWritten <= 1);

  // For coroutine calls, we need to collect the yields, not the results;
  // this looks very different.
  if (IsCoroutine) {
    assert(LastArgWritten == 0 && "coroutine with indirect result?");
    emitYieldsToExplosion(out);
    return;
  }

  SILFunctionConventions fnConv(getCallee().getSubstFunctionType(),
                                IGF.getSILModule());
  SILType substResultType =
      fnConv.getSILResultType(IGF.IGM.getMaximalTypeExpansionContext());

  auto &substResultTI =
    cast<LoadableTypeInfo>(IGF.getTypeInfo(substResultType));

  auto origFnType = getCallee().getOrigFunctionType();
  auto isNoReturnCFunction =
      origFnType->getLanguage() == SILFunctionLanguage::C &&
      origFnType->isNoReturnFunction(IGF.getSILModule(),
                                     IGF.IGM.getMaximalTypeExpansionContext());

  // If the call is naturally to memory, emit it that way and then
  // explode that temporary.
  if (LastArgWritten == 1) {
    if (isNoReturnCFunction) {
      auto fnType = getCallee().getFunctionPointer().getFunctionType();
      assert(fnType->getNumParams() > 0);
      // The size of the return buffer should not matter since the callee is not
      // returning but lets try our best to use the right size.
      llvm::Type *resultTy = IGF.IGM.Int8Ty;
      auto func = dyn_cast<llvm::Function>(
          getCallee().getFunctionPointer().getRawPointer());
      if (func && func->hasParamAttribute(0, llvm::Attribute::StructRet)) {
        resultTy = func->getParamStructRetType(0);
      }
      auto temp = IGF.createAlloca(resultTy, Alignment(), "indirect.result");
      emitToMemory(temp, substResultTI, isOutlined);
      return;
    }

    auto *FI = getCallee().getForeignInfo().ClangInfo;
    if (FI && FI->getReturnInfo().isIndirect() &&
        FI->getReturnInfo().isSRetAfterThis() && substResultType.isVoid()) {
      // Some C++ methods return a value but are imported as
      // returning `Void` (e.g. `operator +=`). In this case
      // we should allocate the correct temp indirect return
      // value for it.
      // FIXME: MSVC ABI hits this as it makes some SIL direct
      // returns as indirect at IR layer, so fix this for MSVC
      // first to get this into Swfit 5.9. However, then investigate
      // if this could also apply to Itanium ABI too.
      auto fnType = getCallee().getFunctionPointer().getFunctionType();
      assert(fnType->getNumParams() > 1);
      auto func = dyn_cast<llvm::Function>(
          getCallee().getFunctionPointer().getRawPointer());
      if (func) {
        // `this` comes before the returned value under the MSVC ABI
        // so return value is parameter #1.
        assert(func->hasParamAttribute(1, llvm::Attribute::StructRet));
        auto resultTy = func->getParamStructRetType(1);
        auto temp = IGF.createAlloca(resultTy, Alignment(/*safe alignment*/ 16),
                                     "indirect.result");
        emitToMemory(temp, substResultTI, isOutlined);
        return;
      }
    }

    StackAddress ctemp = substResultTI.allocateStack(IGF, substResultType,
                                                     "call.aggresult");
    Address temp = ctemp.getAddress();
    emitToMemory(temp, substResultTI, isOutlined);

    // We can use a take.
    substResultTI.loadAsTake(IGF, temp, out);

    substResultTI.deallocateStack(IGF, ctemp, substResultType);
    return;
  }

  // Okay, we're naturally emitting to an explosion.
  Explosion temp;
  emitToUnmappedExplosion(temp);

  // Specially handle noreturn c function which would return a 'Never' SIL result
  // type: there is no need to cast the result.
  if (isNoReturnCFunction) {
    temp.transferInto(out, temp.size());
    return;
  }

  // We might need to bitcast the results.
  emitCastToSubstSchema(IGF, temp, substResultTI.getSchema(), out);
}

CallEmission::CallEmission(CallEmission &&other)
  : IGF(other.IGF),
    Args(std::move(other.Args)),
    CurCallee(std::move(other.CurCallee)),
    LastArgWritten(other.LastArgWritten),
    EmittedCall(other.EmittedCall) {
  // Prevent other's destructor from asserting.
  LastArgWritten = 0;
  EmittedCall = true;
  state = State::Finished;
}

CallEmission::~CallEmission() {
  assert(LastArgWritten == 0);
  assert(EmittedCall);
  assert(Temporaries.hasBeenCleared());
  assert(RawTempraries.empty());
  assert(state == State::Finished);
}

void CallEmission::begin() {}

void CallEmission::end() {
  assert(state == State::Emitting);
  state = State::Finished;
}

Callee::Callee(CalleeInfo &&info, const FunctionPointer &fn,
               llvm::Value *firstData, llvm::Value *secondData)
    : Info(std::move(info)), Fn(fn),
      FirstData(firstData), SecondData(secondData) {

#ifndef NDEBUG
  // We should have foreign info if it's a foreign call.
  assert((Fn.getForeignInfo().ClangInfo != nullptr) ==
         (Info.OrigFnType->getLanguage() == SILFunctionLanguage::C));

  // We should have the right data values for the representation.
  switch (Info.OrigFnType->getRepresentation()) {
  case SILFunctionTypeRepresentation::ObjCMethod:
    assert(FirstData);
    break;
  case SILFunctionTypeRepresentation::Method:
  case SILFunctionTypeRepresentation::WitnessMethod:
    assert((FirstData != nullptr) ==
           hasSelfContextParameter(Info.OrigFnType));
    assert(!SecondData);
    break;
  case SILFunctionTypeRepresentation::Thick:
  case SILFunctionTypeRepresentation::Block:
    assert(FirstData && !SecondData);
    break;
  case SILFunctionTypeRepresentation::Thin:
  case SILFunctionTypeRepresentation::Closure:
  case SILFunctionTypeRepresentation::CFunctionPointer:
  case SILFunctionTypeRepresentation::KeyPathAccessorGetter:
  case SILFunctionTypeRepresentation::KeyPathAccessorSetter:
  case SILFunctionTypeRepresentation::KeyPathAccessorEquals:
  case SILFunctionTypeRepresentation::KeyPathAccessorHash:
    assert(!FirstData && !SecondData);
    break;
  case SILFunctionTypeRepresentation::CXXMethod:
    assert(FirstData && !SecondData);
    break;
  }
#endif

}

llvm::Value *Callee::getSwiftContext() const {
  switch (Info.OrigFnType->getRepresentation()) {
  case SILFunctionTypeRepresentation::Block:
  case SILFunctionTypeRepresentation::ObjCMethod:
  case SILFunctionTypeRepresentation::CFunctionPointer:
  case SILFunctionTypeRepresentation::Thin:
  case SILFunctionTypeRepresentation::Closure:
  case SILFunctionTypeRepresentation::CXXMethod:
  case SILFunctionTypeRepresentation::KeyPathAccessorGetter:
  case SILFunctionTypeRepresentation::KeyPathAccessorSetter:
  case SILFunctionTypeRepresentation::KeyPathAccessorEquals:
  case SILFunctionTypeRepresentation::KeyPathAccessorHash:
    return nullptr;

  case SILFunctionTypeRepresentation::WitnessMethod:
  case SILFunctionTypeRepresentation::Method:
    // This may or may not be null.
    return FirstData;

  case SILFunctionTypeRepresentation::Thick:
    assert(FirstData && "no context value set on callee");
    return FirstData;
  }
  llvm_unreachable("bad representation");
}

llvm::Value *Callee::getBlockObject() const {
  assert(Info.OrigFnType->getRepresentation() ==
           SILFunctionTypeRepresentation::Block &&
         "not a block");
  assert(FirstData && "no block object set on callee");
  return FirstData;
}

llvm::Value *Callee::getCXXMethodSelf() const {
  assert(Info.OrigFnType->getRepresentation() ==
             SILFunctionTypeRepresentation::CXXMethod &&
         "not a C++ method");
  assert(FirstData && "no self object set on callee");
  return FirstData;
}

llvm::Value *Callee::getObjCMethodReceiver() const {
  assert(Info.OrigFnType->getRepresentation() ==
           SILFunctionTypeRepresentation::ObjCMethod &&
         "not a method");
  assert(FirstData && "no receiver set on callee");
  return FirstData;
}

llvm::Value *Callee::getObjCMethodSelector() const {
  assert(Info.OrigFnType->getRepresentation() ==
           SILFunctionTypeRepresentation::ObjCMethod &&
         "not a method");
  assert(SecondData && "no selector set on callee");
  return SecondData;
}

bool Callee::isDirectObjCMethod() const {
  return Info.OrigFnType->getRepresentation() ==
           SILFunctionTypeRepresentation::ObjCMethod && SecondData == nullptr;
}

/// Set up this emitter afresh from the current callee specs.
void CallEmission::setFromCallee() {
  assert(state == State::Emitting);
  IsCoroutine = CurCallee.getSubstFunctionType()->isCoroutine();
  EmittedCall = false;

  unsigned numArgs = CurCallee.getLLVMFunctionType()->getNumParams();

  // Set up the args array.
  assert(Args.empty());
  Args.resize_for_overwrite(numArgs);
  LastArgWritten = numArgs;
}

bool irgen::canCoerceToSchema(IRGenModule &IGM,
                              ArrayRef<llvm::Type*> expandedTys,
                              const ExplosionSchema &schema) {
  // If the schemas don't even match in number, we have to go
  // through memory.
  if (expandedTys.size() != schema.size())
    return false;

  // If there's just one element, we can always coerce as a scalar.
  if (expandedTys.size() == 1) return true;

  // If there are multiple elements, the pairs of types need to
  // match in size for the coercion to work.
  for (size_t i = 0, e = expandedTys.size(); i != e; ++i) {
    llvm::Type *inputTy = schema[i].getScalarType();
    llvm::Type *outputTy = expandedTys[i];
    if (inputTy != outputTy &&
        IGM.DataLayout.getTypeSizeInBits(inputTy) !=
        IGM.DataLayout.getTypeSizeInBits(outputTy))
      return false;
  }

  // Okay, everything is fine.
  return true;
}

static llvm::Type *getOutputType(TranslationDirection direction, unsigned index,
                                 const ExplosionSchema &nativeSchema,
                                 ArrayRef<llvm::Type*> expandedForeignTys) {
  assert(nativeSchema.size() == expandedForeignTys.size());
  return (direction == TranslationDirection::ToForeign
            ? expandedForeignTys[index]
            : nativeSchema[index].getScalarType());
}

static void emitCoerceAndExpand(IRGenFunction &IGF, Explosion &in,
                                Explosion &out, SILType paramTy,
                                const LoadableTypeInfo &paramTI,
                                llvm::StructType *coercionTy,
                                ArrayRef<llvm::Type *> expandedTys,
                                TranslationDirection direction,
                                bool isOutlined) {
  // If we can directly coerce the scalar values, avoid going through memory.
  auto schema = paramTI.getSchema();
  if (canCoerceToSchema(IGF.IGM, expandedTys, schema)) {
    for (auto index : indices(expandedTys)) {
      llvm::Value *arg = in.claimNext();
      assert(arg->getType() ==
               getOutputType(reverse(direction), index, schema, expandedTys));
      auto outputTy = getOutputType(direction, index, schema, expandedTys);

      if (arg->getType() != outputTy)
        arg = IGF.coerceValue(arg, outputTy, IGF.IGM.DataLayout);
      out.add(arg);
    }
    return;
  }

  // Otherwise, materialize to a temporary.
  auto temporaryAlloc =
    paramTI.allocateStack(IGF, paramTy, "coerce-and-expand.temp");
  Address temporary = temporaryAlloc.getAddress();

  auto coercionTyLayout = IGF.IGM.DataLayout.getStructLayout(coercionTy);

  // Make the alloca at least as aligned as the coercion struct, just
  // so that the element accesses we make don't end up under-aligned.
  Alignment coercionTyAlignment =
      Alignment(coercionTyLayout->getAlignment().value());
  auto alloca = cast<llvm::AllocaInst>(temporary.getAddress());
  if (alloca->getAlign() < coercionTyAlignment.getValue()) {
    alloca->setAlignment(
        llvm::MaybeAlign(coercionTyAlignment.getValue()).valueOrOne());
    temporary = Address(temporary.getAddress(), temporary.getElementType(),
                        coercionTyAlignment);
  }

  // If we're translating *to* the foreign expansion, do an ordinary
  // initialization from the input explosion.
  if (direction == TranslationDirection::ToForeign) {
    paramTI.initialize(IGF, in, temporary, isOutlined);
  }

  Address coercedTemporary =
    IGF.Builder.CreateElementBitCast(temporary, coercionTy);

#ifndef NDEBUG
  size_t expandedTyIndex = 0;
#endif

  for (auto eltIndex : indices(coercionTy->elements())) {
    auto eltTy = coercionTy->getElementType(eltIndex);

    // Skip padding fields.
    if (eltTy->isArrayTy()) continue;
    assert(expandedTys[expandedTyIndex++] == eltTy);

    // Project down to the field.
    Address eltAddr =
      IGF.Builder.CreateStructGEP(coercedTemporary, eltIndex, coercionTyLayout);

    // If we're translating *to* the foreign expansion, pull the value out
    // of the field and add it to the output.
    if (direction == TranslationDirection::ToForeign) {
      llvm::Value *value = IGF.Builder.CreateLoad(eltAddr);
      out.add(value);

    // Otherwise, claim the next value from the input and store that
    // in the field.
    } else {
      llvm::Value *value = in.claimNext();
      IGF.Builder.CreateStore(value, eltAddr);
    }
  }

  assert(expandedTyIndex == expandedTys.size());

  // If we're translating *from* the foreign expansion, do an ordinary
  // load into the output explosion.
  if (direction == TranslationDirection::ToNative) {
    paramTI.loadAsTake(IGF, temporary, out);
  }

  paramTI.deallocateStack(IGF, temporaryAlloc, paramTy);
}

static void emitDirectExternalArgument(IRGenFunction &IGF, SILType argType,
                                       const clang::CodeGen::ABIArgInfo &AI,
                                       Explosion &in, Explosion &out,
                                       bool isOutlined) {
  bool IsDirectFlattened = AI.isDirect() && AI.getCanBeFlattened();
  bool IsIndirect = !AI.isDirect();

  // If we're supposed to pass directly as a struct type, that
  // really means expanding out as multiple arguments.
  llvm::Type *coercedTy = AI.getCoerceToType();
  ArrayRef<llvm::Type *> expandedTys =
      expandScalarOrStructTypeToArray(coercedTy);

  auto &argTI = cast<LoadableTypeInfo>(IGF.getTypeInfo(argType));
  auto inputSchema = argTI.getSchema();

  // Check to see if we can pairwise coerce Swift's exploded scalars
  // to Clang's expanded elements.
  if ((IsDirectFlattened || IsIndirect) &&
      canCoerceToSchema(IGF.IGM, expandedTys, inputSchema)) {
    for (auto outputTy : expandedTys) {
      llvm::Value *arg = in.claimNext();
      if (arg->getType() != outputTy)
        arg = IGF.coerceValue(arg, outputTy, IGF.IGM.DataLayout);
      out.add(arg);
    }
    return;
  }

  // Otherwise, we need to coerce through memory.
  Address temporary;
  Size tempSize;
  std::tie(temporary, tempSize) =
      allocateForCoercion(IGF, argTI.getStorageType(), coercedTy, "coerced-arg");
  IGF.Builder.CreateLifetimeStart(temporary, tempSize);

  // Store to a temporary.
  Address tempOfArgTy =
      IGF.Builder.CreateElementBitCast(temporary, argTI.getStorageType());
  argTI.initializeFromParams(IGF, in, tempOfArgTy, argType, isOutlined);

  // Bitcast the temporary to the expected type.
  Address coercedAddr = IGF.Builder.CreateElementBitCast(temporary, coercedTy);

  if (IsDirectFlattened && isa<llvm::StructType>(coercedTy)) {
    // Project out individual elements if necessary.
    auto *ST = cast<llvm::StructType>(coercedTy);
    const auto *layout = IGF.IGM.DataLayout.getStructLayout(ST);
    for (unsigned EI : range(ST->getNumElements())) {
      auto offset = Size(layout->getElementOffset(EI));
      auto address = IGF.Builder.CreateStructGEP(coercedAddr, EI, offset);
      out.add(IGF.Builder.CreateLoad(address));
    }
  } else {
    // Otherwise, collect the single scalar.
    out.add(IGF.Builder.CreateLoad(coercedAddr));
  }

  IGF.Builder.CreateLifetimeEnd(temporary, tempSize);
}

namespace {
  /// Load a clang argument expansion from a buffer.
  struct ClangExpandLoadEmitter :
    ClangExpandProjection<ClangExpandLoadEmitter> {

    Explosion &Out;
    ClangExpandLoadEmitter(IRGenFunction &IGF, Explosion &out)
      : ClangExpandProjection(IGF), Out(out) {}

    void visitScalar(llvm::Type *scalarTy, Address addr) {
      addr = IGF.Builder.CreateElementBitCast(addr, scalarTy);
      auto value = IGF.Builder.CreateLoad(addr);
      Out.add(value);
    }
  };

  /// Store a clang argument expansion into a buffer.
  struct ClangExpandStoreEmitter :
    ClangExpandProjection<ClangExpandStoreEmitter> {

    Explosion &In;
    ClangExpandStoreEmitter(IRGenFunction &IGF, Explosion &in)
      : ClangExpandProjection(IGF), In(in) {}

    void visitScalar(llvm::Type *scalarTy, Address addr) {
      auto value = In.claimNext();

      addr = IGF.Builder.CreateElementBitCast(addr, scalarTy);
      IGF.Builder.CreateStore(value, addr);
    }
  };
} // end anonymous namespace

/// Given a Swift value explosion in 'in', produce a Clang expansion
/// (according to ABIArgInfo::Expand) in 'out'.
static void
emitClangExpandedArgument(IRGenFunction &IGF, Explosion &in, Explosion &out,
                          clang::CanQualType clangType, SILType swiftType,
                          const LoadableTypeInfo &swiftTI, bool isOutlined) {
  // If Clang's expansion schema matches Swift's, great.
  auto swiftSchema = swiftTI.getSchema();
  if (doesClangExpansionMatchSchema(IGF.IGM, clangType, swiftSchema)) {
    return in.transferInto(out, swiftSchema.size());
  }

  // Otherwise, materialize to a temporary.
  auto ctemp = swiftTI.allocateStack(IGF, swiftType, "clang-expand-arg.temp");
  Address temp = ctemp.getAddress();
  swiftTI.initialize(IGF, in, temp, isOutlined);

  Address castTemp = IGF.Builder.CreateElementBitCast(temp, IGF.IGM.Int8Ty);
  ClangExpandLoadEmitter(IGF, out).visit(clangType, castTemp);

  swiftTI.deallocateStack(IGF, ctemp, swiftType);
}

/// Given a Clang-expanded (according to ABIArgInfo::Expand) parameter
/// in 'in', produce a Swift value explosion in 'out'.
void irgen::emitClangExpandedParameter(IRGenFunction &IGF,
                                       Explosion &in, Explosion &out,
                                       clang::CanQualType clangType,
                                       SILType swiftType,
                                       const LoadableTypeInfo &swiftTI) {
  // If Clang's expansion schema matches Swift's, great.
  auto swiftSchema = swiftTI.getSchema();
  if (doesClangExpansionMatchSchema(IGF.IGM, clangType, swiftSchema)) {
    return in.transferInto(out, swiftSchema.size());
  }

  // Otherwise, materialize to a temporary.
  auto tempAlloc = swiftTI.allocateStack(IGF, swiftType,
                                         "clang-expand-param.temp");
  Address temp = tempAlloc.getAddress();
  Address castTemp = IGF.Builder.CreateElementBitCast(temp, IGF.IGM.Int8Ty);
  ClangExpandStoreEmitter(IGF, in).visit(clangType, castTemp);

  // Then load out.
  swiftTI.loadAsTake(IGF, temp, out);

  swiftTI.deallocateStack(IGF, tempAlloc, swiftType);
}

Address getForwardableAlloca(const TypeInfo &TI, bool isForwardableArgument,
                             Explosion &in) {
  if (!isForwardableArgument)
    return Address();

  auto *load = dyn_cast<llvm::LoadInst>(*in.begin());
  if (!load)
    return Address();

  auto *gep = dyn_cast<llvm::GetElementPtrInst>(load->getPointerOperand());
  if (!gep)
    return Address();

  auto *alloca = dyn_cast<llvm::AllocaInst>(getUnderlyingObject(gep));
  if (!alloca)
    return Address();

  return TI.getAddressForPointer(alloca);
}

void CallEmission::externalizeArguments(IRGenFunction &IGF, const Callee &callee,
                                 Explosion &in, Explosion &out,
                                 TemporarySet &temporaries,
                                 bool isOutlined) {
  auto fnType = callee.getOrigFunctionType();
  auto silConv = SILFunctionConventions(fnType, IGF.IGM.silConv);
  auto params = fnType->getParameters();

  assert(callee.getForeignInfo().ClangInfo);
  auto &FI = *callee.getForeignInfo().ClangInfo;

  // The index of the first "physical" parameter from paramTys/FI that
  // corresponds to a logical parameter from params.
  unsigned firstParam = 0;
  unsigned paramEnd = FI.arg_size();

  // Handle the ObjC prefix.
  if (callee.getRepresentation() == SILFunctionTypeRepresentation::ObjCMethod) {
    // Ignore both the logical and the physical parameters associated
    // with self and (if not objc_direct) _cmd.
    firstParam += callee.isDirectObjCMethod() ? 1 :  2;
    params = params.drop_back();

  // Or the block prefix.
  } else if (fnType->getRepresentation()
                == SILFunctionTypeRepresentation::Block) {
    // Ignore the physical block-object parameter.
    firstParam += 1;
  } else if (callee.getRepresentation() ==
             SILFunctionTypeRepresentation::CXXMethod) {
    // Skip the "self" param.
    firstParam += 1;
    params = params.drop_back();
  }

  bool formalIndirectResult = fnType->getNumResults() > 0 &&
                              fnType->getSingleResult().isFormalIndirect();
  if (!FI.getReturnInfo().isIndirect() && formalIndirectResult) {
    // clang returns directly and swift returns indirectly

    SILType returnTy = SILType::getPrimitiveObjectType(
        fnType->getSingleResult().getReturnValueType(
            IGF.IGM.getSILModule(), fnType, TypeExpansionContext::minimal()));

    if (returnTy.isSensitive()) {
      // Sensitive return types are represented as indirect return value in SIL,
      // but are returned as values (if small) in LLVM IR.
      assert(out.size() == 1 && "expect a single address for the return value");
      llvm::Value *returnAddr = out.claimNext();
      out.reset();
      assert(returnAddr == indirectReturnAddress.getAddress());
      convertDirectToIndirectReturn = true;
    } else {
      // This must be a constructor call. In that case, skip the "self" param.
      firstParam += 1;
    }
  }

  for (unsigned i = firstParam; i != paramEnd; ++i) {
    auto clangParamTy = FI.arg_begin()[i].type;
    auto &AI = FI.arg_begin()[i].info;

    // We don't need to do anything to handle the Swift parameter-ABI
    // attributes here because we shouldn't be trying to round-trip
    // swiftcall function pointers through SIL as C functions anyway.
    assert(FI.getExtParameterInfo(i).getABI() == clang::ParameterABI::Ordinary);

    // Add a padding argument if required.
    if (auto *padType = AI.getPaddingType())
      out.add(llvm::UndefValue::get(padType));

    const SILParameterInfo &paramInfo = params[i - firstParam];
    SILType paramType = silConv.getSILType(
        paramInfo, IGF.IGM.getMaximalTypeExpansionContext());

    bool isForwardableArgument = IGF.isForwardableArgument(i - firstParam);

    // In Swift, values that are foreign references types will always be
    // pointers. Additionally, we only import functions which use foreign
    // reference types indirectly (as pointers), so we know in every case, if
    // the argument type is a foreign reference type, the types will match up
    // and we can simply use the input directly.
    if (paramType.isForeignReferenceType()) {
      auto *arg = in.claimNext();
      if (isIndirectFormalParameter(params[i - firstParam].getConvention())) {
        auto storageTy = IGF.IGM.getTypeInfo(paramType).getStorageType();
        arg = IGF.Builder.CreateLoad(arg, storageTy,
                                     IGF.IGM.getPointerAlignment());
      }
      out.add(arg);
      continue;
    }

    bool passIndirectToDirect = paramInfo.isIndirectInGuaranteed() && paramType.isSensitive();
    if (passIndirectToDirect) {
      llvm::Value *ptr = in.claimNext();

      if (AI.getKind() == clang::CodeGen::ABIArgInfo::Indirect) {
        // It's a large struct which is also passed indirectl in LLVM IR.
        // The C function (= the callee) is allowed to modify the memory used
        // for passing arguments, therefore we need to copy the argument value
        // to a temporary.
        // TODO: avoid the temporary if the SIL parameter value in memory is
        //       not used anymore after the call.
        auto &ti = cast<LoadableTypeInfo>(IGF.getTypeInfo(paramType));
        auto temp = ti.allocateStack(IGF, paramType, "indirect-temporary");
        Address tempAddr = temp.getAddress();
        temporaries.add({temp, paramType});
        Address paramAddr = ti.getAddressForPointer(ptr);
        ti.initializeWithCopy(IGF, tempAddr, paramAddr, paramType, isOutlined);

        out.add(tempAddr.getAddress());
        continue;
      }

      auto &ti = cast<LoadableTypeInfo>(IGF.getTypeInfo(paramType));
      Explosion loadedValue;
      ti.loadAsCopy(IGF, ti.getAddressForPointer(ptr), loadedValue);
      in.transferInto(loadedValue, in.size());
      in = std::move(loadedValue);
    }

    switch (AI.getKind()) {
    case clang::CodeGen::ABIArgInfo::Extend: {
      bool signExt = clangParamTy->hasSignedIntegerRepresentation();
      assert((signExt || clangParamTy->hasUnsignedIntegerRepresentation()) &&
             "Invalid attempt to add extension attribute to argument!");
      (void) signExt;
      LLVM_FALLTHROUGH;
    }
    case clang::CodeGen::ABIArgInfo::Direct: {
      auto toTy = AI.getCoerceToType();

      // Indirect parameters are bridged as Clang pointer types.
      if (silConv.isSILIndirect(params[i - firstParam]) && !passIndirectToDirect) {
        assert(paramType.isAddress() && "SIL type is not an address?");

        auto addr = in.claimNext();
        if (addr->getType() != toTy)
          addr = IGF.coerceValue(addr, toTy, IGF.IGM.DataLayout);
        out.add(addr);
        break;
      }

      emitDirectExternalArgument(IGF, paramType, AI, in, out, isOutlined);
      break;
    }
    case clang::CodeGen::ABIArgInfo::IndirectAliased:
      llvm_unreachable("not implemented");
    case clang::CodeGen::ABIArgInfo::Indirect: {
      auto &ti = cast<LoadableTypeInfo>(IGF.getTypeInfo(paramType));

      auto temp = ti.allocateStack(IGF, paramType, "indirect-temporary");
      temporaries.add({temp, paramType});

      Address addr = temp.getAddress();
      // Set at least the alignment the ABI expects.
      if (AI.getIndirectByVal()) {
        auto ABIAlign = AI.getIndirectAlign();
        if (ABIAlign > addr.getAlignment()) {
          auto *AS = cast<llvm::AllocaInst>(addr.getAddress());
          AS->setAlignment(
              llvm::MaybeAlign(ABIAlign.getQuantity()).valueOrOne());
          addr = Address(addr.getAddress(), addr.getElementType(),
                         Alignment(ABIAlign.getQuantity()));
        }
      }
      Address forwardFromAddr = getForwardableAlloca(ti, isForwardableArgument,
                                                     in);
      // Try to forward the address from a `load` instruction "immediately"
      // preceeding the apply.
      if (isForwardableArgument && forwardFromAddr.isValid()) {
        ti.initializeWithTake(IGF, addr, forwardFromAddr,
                              paramType.getAddressType(), isOutlined,
                              /*zeroizeIfSensitive=*/ true);
        (void)in.claim(ti.getSchema().size());
      } else {
        ti.initialize(IGF, in, addr, isOutlined);
      }

      out.add(addr.getAddress());
      break;
    }
    case clang::CodeGen::ABIArgInfo::CoerceAndExpand: {
      auto &paramTI = cast<LoadableTypeInfo>(IGF.getTypeInfo(paramType));
      emitCoerceAndExpand(IGF, in, out, paramType, paramTI,
                          AI.getCoerceAndExpandType(),
                          AI.getCoerceAndExpandTypeSequence(),
                          TranslationDirection::ToForeign, isOutlined);
      break;
    }
    case clang::CodeGen::ABIArgInfo::Expand:
      emitClangExpandedArgument(
          IGF, in, out, clangParamTy, paramType,
          cast<LoadableTypeInfo>(IGF.getTypeInfo(paramType)), isOutlined);
      break;
    case clang::CodeGen::ABIArgInfo::Ignore:
      break;
    case clang::CodeGen::ABIArgInfo::InAlloca:
      llvm_unreachable("Need to handle InAlloca when externalizing arguments");
      break;
    }
  }
}

bool CallEmission::mayReturnTypedErrorDirectly() const {
  SILFunctionConventions fnConv(getCallee().getOrigFunctionType(),
                                IGF.getSILModule());
  bool mayReturnErrorDirectly = false;
  if (!convertDirectToIndirectReturn && !fnConv.hasIndirectSILResults() &&
      !fnConv.hasIndirectSILErrorResults() && fnConv.funcTy->hasErrorResult() &&
      fnConv.isTypedError()) {
    auto errorType =
        fnConv.getSILErrorType(IGF.IGM.getMaximalTypeExpansionContext());
    auto &errorSchema =
        IGF.IGM.getTypeInfo(errorType).nativeReturnValueSchema(IGF.IGM);

    mayReturnErrorDirectly = !errorSchema.shouldReturnTypedErrorIndirectly();
  }

  return mayReturnErrorDirectly;
}

void CallEmission::emitToUnmappedExplosionWithDirectTypedError(
    SILType resultType, llvm::Value *result, Explosion &out) {
  SILFunctionConventions fnConv(getCallee().getOrigFunctionType(),
                                IGF.getSILModule());
  auto &nativeSchema =
      IGF.IGM.getTypeInfo(resultType).nativeReturnValueSchema(IGF.IGM);
  auto errorType =
      fnConv.getSILErrorType(IGF.IGM.getMaximalTypeExpansionContext());
  auto &errorSchema =
      IGF.IGM.getTypeInfo(errorType).nativeReturnValueSchema(IGF.IGM);

  auto combined =
      combineResultAndTypedErrorType(IGF.IGM, nativeSchema, errorSchema);

  if (combined.combinedTy->isVoidTy()) {
    typedErrorExplosion = Explosion();
    return;
  }

  Explosion nativeExplosion;
  extractScalarResults(IGF, result->getType(), result, nativeExplosion);
  auto values = nativeExplosion.claimAll();

  Explosion errorExplosion;
  if (!errorSchema.empty()) {
    if (auto *structTy =
            dyn_cast<llvm::StructType>(errorSchema.getExpandedType(IGF.IGM))) {
      for (unsigned i = 0, e = structTy->getNumElements(); i < e; ++i) {
        llvm::Value *elt = values[combined.errorValueMapping[i]];
        auto *nativeTy = structTy->getElementType(i);
        elt = convertForDirectError(IGF, elt, nativeTy, /*forExtraction*/ true);
        errorExplosion.add(elt);
      }
    } else {
      auto *converted =
          convertForDirectError(IGF, values[combined.errorValueMapping[0]],
                                combined.combinedTy, /*forExtraction*/ true);
      errorExplosion.add(converted);
    }

    typedErrorExplosion =
        errorSchema.mapFromNative(IGF.IGM, IGF, errorExplosion, errorType);
  } else {
    typedErrorExplosion = std::move(errorExplosion);
  }

  // If the regular result type is void, there is nothing to explode
  if (!nativeSchema.empty()) {
    Explosion resultExplosion;
    if (auto *structTy =
            dyn_cast<llvm::StructType>(nativeSchema.getExpandedType(IGF.IGM))) {
      for (unsigned i = 0, e = structTy->getNumElements(); i < e; ++i) {
        auto *nativeTy = structTy->getElementType(i);
        auto *converted = convertForDirectError(IGF, values[i], nativeTy,
                                                /*forExtraction*/ true);
        resultExplosion.add(converted);
      }
    } else {
      auto *converted = convertForDirectError(
          IGF, values[0], combined.combinedTy, /*forExtraction*/ true);
      resultExplosion.add(converted);
    }
    out = nativeSchema.mapFromNative(IGF.IGM, IGF, resultExplosion, resultType);
  }
}

void CallEmission::setKeyPathAccessorArguments(Explosion &in, bool isOutlined,
                                               Explosion &out) {
  auto origCalleeType = CurCallee.getOrigFunctionType();
  auto params = origCalleeType->getParameters();

  switch (getCallee().getRepresentation()) {
  case SILFunctionTypeRepresentation::KeyPathAccessorGetter: {
    // add base value
    addNativeArgument(IGF, in, origCalleeType, params[0], out, isOutlined);
    params = params.drop_back();
    break;
  }
  case SILFunctionTypeRepresentation::KeyPathAccessorSetter: {
    // add base value
    addNativeArgument(IGF, in, origCalleeType, params[0], out, isOutlined);
    // add new value
    addNativeArgument(IGF, in, origCalleeType, params[1], out, isOutlined);
    params = params.drop_back(2);
    break;
  }
  default:
    llvm_unreachable("unexpected representation");
  }

  std::optional<StackAddress> dynamicArgsBuf;
  SmallVector<SILType, 4> indiceTypes;
  for (auto i : indices(params)) {
    auto ty = getParameterType(i);
    indiceTypes.push_back(ty);
  }
  auto sig = origCalleeType->getInvocationGenericSignature();
  auto args = emitKeyPathArgument(IGF, getCallee().getSubstitutions(), sig,
                                  indiceTypes, in, dynamicArgsBuf);
  if (dynamicArgsBuf) {
    RawTempraries.push_back(*dynamicArgsBuf);
  }

  // add arg buffer
  out.add(args.first);
  // add arg buffer size
  out.add(args.second);
}

/// Returns whether allocas are needed.
bool irgen::addNativeArgument(IRGenFunction &IGF,
                              Explosion &in,
                              CanSILFunctionType fnTy,
                              SILParameterInfo origParamInfo, Explosion &out,
                              bool isOutlined) {
  // Addresses consist of a single pointer argument.
  if (IGF.IGM.silConv.isSILIndirect(origParamInfo)) {
    out.add(in.claimNext());
    return false;
  }
  auto paramType = IGF.IGM.silConv.getSILType(
      origParamInfo, fnTy, IGF.IGM.getMaximalTypeExpansionContext());
  auto &ti = cast<LoadableTypeInfo>(IGF.getTypeInfo(paramType));
  auto schema = ti.getSchema();
  auto &nativeSchema = ti.nativeParameterValueSchema(IGF.IGM);
  if (nativeSchema.requiresIndirect()) {
    // Pass the argument indirectly.
    auto buf = IGF.createAlloca(ti.getStorageType(),
                                ti.getFixedAlignment(), "");
    ti.initialize(IGF, in, buf, isOutlined);
    out.add(buf.getAddress());
    return true;
  } else {
    if (schema.empty()) {
      assert(nativeSchema.empty());
      return false;
    }
    assert(!nativeSchema.empty());

    // Pass the argument explosion directly, mapping into the native swift
    // calling convention.
    Explosion nonNativeParam;
    ti.reexplode(in, nonNativeParam);
    Explosion nativeParam = nativeSchema.mapIntoNative(
        IGF.IGM, IGF, nonNativeParam, paramType, isOutlined);
    nativeParam.transferInto(out, nativeParam.size());
    return false;
  }
}

/// Emit a direct parameter that was passed under a C-based CC.
static void emitDirectForeignParameter(IRGenFunction &IGF, Explosion &in,
                                       const clang::CodeGen::ABIArgInfo &AI,
                                       Explosion &out, SILType paramType,
                                       const LoadableTypeInfo &paramTI) {
  // The ABI IR types for the entrypoint might differ from the
  // Swift IR types for the body of the function.

  bool IsDirectFlattened = AI.isDirect() && AI.getCanBeFlattened();

  llvm::Type *coercionTy = AI.getCoerceToType();

  ArrayRef<llvm::Type*> expandedTys;
  if (IsDirectFlattened && isa<llvm::StructType>(coercionTy)) {
    const auto *ST = cast<llvm::StructType>(coercionTy);
    expandedTys = llvm::ArrayRef(ST->element_begin(), ST->getNumElements());
  } else if (coercionTy == paramTI.getStorageType()) {
    // Fast-path a really common case.  This check assumes that either
    // the storage type of a type is an llvm::StructType or it has a
    // single-element explosion.
    out.add(in.claimNext());
    return;
  } else {
    expandedTys = coercionTy;
  }

  auto outputSchema = paramTI.getSchema();

  // Check to see if we can pairwise-coerce Swift's exploded scalars
  // to Clang's expanded elements.
  if (canCoerceToSchema(IGF.IGM, expandedTys, outputSchema)) {
    for (auto &outputElt : outputSchema) {
      llvm::Value *param = in.claimNext();
      llvm::Type *outputTy = outputElt.getScalarType();
      if (param->getType() != outputTy)
        param = IGF.coerceValue(param, outputTy, IGF.IGM.DataLayout);
      out.add(param);
    }
    return;
  }

  // Otherwise, we need to traffic through memory.
  // Create a temporary.
  Address temporary; Size tempSize;
  std::tie(temporary, tempSize) = allocateForCoercion(IGF,
                                          coercionTy,
                                          paramTI.getStorageType(),
                                          "");
  IGF.Builder.CreateLifetimeStart(temporary, tempSize);

  // Write the input parameters into the temporary:
  Address coercedAddr = IGF.Builder.CreateElementBitCast(temporary, coercionTy);

  // Break down a struct expansion if necessary.
  if (IsDirectFlattened && isa<llvm::StructType>(coercionTy)) {
    auto expansionTy = cast<llvm::StructType>(coercionTy);
    auto layout = IGF.IGM.DataLayout.getStructLayout(expansionTy);
    for (unsigned i = 0, e = expansionTy->getNumElements(); i != e; ++i) {
      auto fieldOffset = Size(layout->getElementOffset(i));
      auto fieldAddr = IGF.Builder.CreateStructGEP(coercedAddr, i, fieldOffset);
      IGF.Builder.CreateStore(in.claimNext(), fieldAddr);
    }

  // Otherwise, store the single scalar.
  } else {
    IGF.Builder.CreateStore(in.claimNext(), coercedAddr);
  }

  // Pull out the elements.
  temporary =
      IGF.Builder.CreateElementBitCast(temporary, paramTI.getStorageType());
  paramTI.loadAsTake(IGF, temporary, out);

  // Deallocate the temporary.
  // `deallocateStack` emits the lifetime.end marker for us.
  paramTI.deallocateStack(IGF, StackAddress(temporary), paramType);
}

void irgen::emitForeignParameter(IRGenFunction &IGF, Explosion &params,
                                 ForeignFunctionInfo foreignInfo,
                                 unsigned foreignParamIndex, SILType paramTy,
                                 const LoadableTypeInfo &paramTI,
                                 Explosion &paramExplosion, bool isOutlined) {
  assert(foreignInfo.ClangInfo);
  auto &FI = *foreignInfo.ClangInfo;

  auto clangArgTy = FI.arg_begin()[foreignParamIndex].type;
  auto AI = FI.arg_begin()[foreignParamIndex].info;

  // We don't need to do anything to handle the Swift parameter-ABI
  // attributes here because we shouldn't be trying to round-trip
  // swiftcall function pointers through SIL as C functions anyway.
  assert(FI.getExtParameterInfo(foreignParamIndex).getABI()
           == clang::ParameterABI::Ordinary);

  // Drop padding arguments.
  if (AI.getPaddingType())
    params.claimNext();

  switch (AI.getKind()) {
  case clang::CodeGen::ABIArgInfo::Extend:
  case clang::CodeGen::ABIArgInfo::Direct:
    emitDirectForeignParameter(IGF, params, AI, paramExplosion, paramTy,
                               paramTI);
    return;
  case clang::CodeGen::ABIArgInfo::IndirectAliased:
      llvm_unreachable("not implemented");
  case clang::CodeGen::ABIArgInfo::Indirect: {
    Address address = paramTI.getAddressForPointer(params.claimNext());
    paramTI.loadAsTake(IGF, address, paramExplosion);
    return;
  }
  case clang::CodeGen::ABIArgInfo::Expand: {
    emitClangExpandedParameter(IGF, params, paramExplosion, clangArgTy,
                               paramTy, paramTI);
    return;
  }
  case clang::CodeGen::ABIArgInfo::CoerceAndExpand: {
    auto &paramTI = cast<LoadableTypeInfo>(IGF.getTypeInfo(paramTy));
    emitCoerceAndExpand(IGF, params, paramExplosion, paramTy, paramTI,
                        AI.getCoerceAndExpandType(),
                        AI.getCoerceAndExpandTypeSequence(),
                        TranslationDirection::ToNative, isOutlined);
    break;
  }

  case clang::CodeGen::ABIArgInfo::Ignore:
    return;

  case clang::CodeGen::ABIArgInfo::InAlloca:
    llvm_unreachable("Need to handle InAlloca during signature expansion");
  }
}

std::pair<PointerAuthSchema, PointerAuthEntity>
irgen::getCoroutineResumeFunctionPointerAuth(IRGenModule &IGM,
                                             CanSILFunctionType fnType) {
  switch (fnType->getCoroutineKind()) {
  case SILCoroutineKind::None:
    llvm_unreachable("not a coroutine");
  case SILCoroutineKind::YieldMany:
    return { IGM.getOptions().PointerAuth.YieldManyResumeFunctions,
             PointerAuthEntity::forYieldTypes(fnType) };
  case SILCoroutineKind::YieldOnce2:
    if (IGM.IRGen.Opts.EmitYieldOnce2AsYieldOnce) {
      LLVM_FALLTHROUGH;
    } else {
      return {IGM.getOptions().PointerAuth.YieldOnce2ResumeFunctions,
              PointerAuthEntity::forYieldTypes(fnType)};
    }
  case SILCoroutineKind::YieldOnce:
    return { IGM.getOptions().PointerAuth.YieldOnceResumeFunctions,
             PointerAuthEntity::forYieldTypes(fnType) };
  }
  llvm_unreachable("bad coroutine kind");
}

static void
emitRetconCoroutineEntry(IRGenFunction &IGF, CanSILFunctionType fnType,
                         NativeCCEntryPointArgumentEmission &emission,
                         llvm::Intrinsic::ID idIntrinsic, Size bufferSize,
                         Alignment bufferAlignment) {
  auto prototype =
    IGF.IGM.getOpaquePtr(IGF.IGM.getAddrOfContinuationPrototype(fnType));

  // Use malloc and free as our allocator.
  auto allocFn = IGF.IGM.getOpaquePtr(IGF.IGM.getMallocFn());
  auto deallocFn = IGF.IGM.getOpaquePtr(IGF.IGM.getFreeFn());

  // Call the right 'llvm.coro.id.retcon' variant.
  llvm::Value *buffer = emission.getCoroutineBuffer();
  llvm::Value *id = IGF.Builder.CreateIntrinsicCall(idIntrinsic, {
    llvm::ConstantInt::get(IGF.IGM.Int32Ty, bufferSize.getValue()),
    llvm::ConstantInt::get(IGF.IGM.Int32Ty, bufferAlignment.getValue()),
    buffer,
    prototype,
    allocFn,
    deallocFn
  });

  // Call 'llvm.coro.begin', just for consistency with the normal pattern.
  // This serves as a handle that we can pass around to other intrinsics.
  auto hdl = IGF.Builder.CreateIntrinsicCall(
      llvm::Intrinsic::coro_begin,
      {id, llvm::ConstantPointerNull::get(IGF.IGM.Int8PtrTy)});

  // Set the coroutine handle; this also flags that is a coroutine so that
  // e.g. dynamic allocas use the right code generation.
  IGF.setCoroutineHandle(hdl);

  auto *pt = IGF.Builder.IRBuilderBase::CreateAlloca(IGF.IGM.Int1Ty,
                                                     /*array size*/ nullptr,
                                                     "earliest insert point");
  IGF.setEarliestInsertionPoint(pt);
}

void IRGenModule::addAsyncCoroIDMapping(llvm::GlobalVariable *asyncFunctionPointer,
                                        llvm::CallInst *coro_id_builtin) {
  AsyncCoroIDsForPadding[asyncFunctionPointer] = coro_id_builtin;
}

llvm::CallInst *
IRGenModule::getAsyncCoroIDMapping(llvm::GlobalVariable *asyncFunctionPointer) {
  auto found = AsyncCoroIDsForPadding.find(asyncFunctionPointer);
  if (found == AsyncCoroIDsForPadding.end())
    return nullptr;
  return found->second;
}

void IRGenModule::markAsyncFunctionPointerForPadding(
                                   llvm::GlobalVariable *asyncFunctionPointer) {
  AsyncCoroIDsForPadding[asyncFunctionPointer] = nullptr;
}

bool IRGenModule::isAsyncFunctionPointerMarkedForPadding(
                                   llvm::GlobalVariable *asyncFunctionPointer) {
  auto found = AsyncCoroIDsForPadding.find(asyncFunctionPointer);
  if (found == AsyncCoroIDsForPadding.end())
    return false;
  return found->second == nullptr;
}

void irgen::emitAsyncFunctionEntry(IRGenFunction &IGF,
                                   const AsyncContextLayout &layout,
                                   LinkEntity asyncFunction,
                                   unsigned asyncContextIndex) {
  auto &IGM = IGF.IGM;
  auto size = layout.getSize();
  auto asyncFuncPointerVar = cast<llvm::GlobalVariable>(IGM.getAddrOfAsyncFunctionPointer(asyncFunction));
  bool isPadded = IGM
    .isAsyncFunctionPointerMarkedForPadding(asyncFuncPointerVar);
  auto asyncFuncPointer = IGF.Builder.CreateBitOrPointerCast(
                                           asyncFuncPointerVar, IGM.Int8PtrTy);
  
  if (isPadded) {
    size = std::max(layout.getSize(),
                    NumWords_AsyncLet * IGM.getPointerSize());
  }
  
  auto *id = IGF.Builder.CreateIntrinsicCall(
      llvm::Intrinsic::coro_id_async,
      {llvm::ConstantInt::get(IGM.Int32Ty, size.getValue()),
       llvm::ConstantInt::get(IGM.Int32Ty, 16),
       llvm::ConstantInt::get(IGM.Int32Ty, asyncContextIndex),
       asyncFuncPointer});
  
  IGM.addAsyncCoroIDMapping(asyncFuncPointerVar, id);

  // Call 'llvm.coro.begin', just for consistency with the normal pattern.
  // This serves as a handle that we can pass around to other intrinsics.
  auto hdl = IGF.Builder.CreateIntrinsicCall(
      llvm::Intrinsic::coro_begin,
      {id, llvm::ConstantPointerNull::get(IGM.Int8PtrTy)});

  // Set the coroutine handle; this also flags that is a coroutine so that
  // e.g. dynamic allocas use the right code generation.
  IGF.setCoroutineHandle(hdl);
  auto *pt = IGF.Builder.IRBuilderBase::CreateAlloca(IGF.IGM.Int1Ty,
                                                     /*array size*/ nullptr,
                                                     "earliest insert point");
  IGF.setEarliestInsertionPoint(pt);
  IGF.setupAsync(asyncContextIndex);
}

void irgen::emitYieldOnceCoroutineEntry(
    IRGenFunction &IGF, CanSILFunctionType fnType,
    NativeCCEntryPointArgumentEmission &emission) {
  emitRetconCoroutineEntry(IGF, fnType, emission,
                           llvm::Intrinsic::coro_id_retcon_once,
                           getYieldOnceCoroutineBufferSize(IGF.IGM),
                           getYieldOnceCoroutineBufferAlignment(IGF.IGM));
}

void irgen::emitYieldManyCoroutineEntry(
    IRGenFunction &IGF, CanSILFunctionType fnType,
    NativeCCEntryPointArgumentEmission &emission) {
  emitRetconCoroutineEntry(IGF, fnType, emission,
                           llvm::Intrinsic::coro_id_retcon,
                           getYieldManyCoroutineBufferSize(IGF.IGM),
                           getYieldManyCoroutineBufferAlignment(IGF.IGM));
}

void irgen::emitYieldOnce2CoroutineEntry(
    IRGenFunction &IGF, CanSILFunctionType fnType,
    NativeCCEntryPointArgumentEmission &emission) {
  llvm::report_fatal_error("unimplemented");
}

static Address createOpaqueBufferAlloca(IRGenFunction &IGF,
                                        Size size, Alignment align) {
  auto ty = llvm::ArrayType::get(IGF.IGM.Int8Ty, size.getValue());
  auto addr = IGF.createAlloca(ty, align);
  addr = IGF.Builder.CreateStructGEP(addr, 0, Size(0));
  IGF.Builder.CreateLifetimeStart(addr, size);
  return addr;
}

Address irgen::emitAllocYieldOnceCoroutineBuffer(IRGenFunction &IGF) {
  return createOpaqueBufferAlloca(IGF, getYieldOnceCoroutineBufferSize(IGF.IGM),
                                 getYieldOnceCoroutineBufferAlignment(IGF.IGM));
}

Address irgen::emitAllocYieldManyCoroutineBuffer(IRGenFunction &IGF) {
  return createOpaqueBufferAlloca(IGF, getYieldManyCoroutineBufferSize(IGF.IGM),
                                 getYieldManyCoroutineBufferAlignment(IGF.IGM));
}

void irgen::emitDeallocYieldOnceCoroutineBuffer(IRGenFunction &IGF,
                                                Address buffer) {
  auto bufferSize = getYieldOnceCoroutineBufferSize(IGF.IGM);
  IGF.Builder.CreateLifetimeEnd(buffer, bufferSize);
}

void irgen::emitDeallocYieldManyCoroutineBuffer(IRGenFunction &IGF,
                                                Address buffer) {
  auto bufferSize = getYieldManyCoroutineBufferSize(IGF.IGM);
  IGF.Builder.CreateLifetimeEnd(buffer, bufferSize);
}

void irgen::emitDeallocYieldOnce2CoroutineFrame(IRGenFunction &IGF,
                                                llvm::Value *allocation) {
  if (IGF.IGM.IRGen.Opts.EmitYieldOnce2AsYieldOnce) {
    assert(!allocation);
    return;
  }
  assert(allocation);
  llvm::report_fatal_error("unimplemented");
}

Address irgen::emitAllocAsyncContext(IRGenFunction &IGF,
                                     llvm::Value *sizeValue) {
  auto alignment = IGF.IGM.getAsyncContextAlignment();
  auto address = IGF.emitTaskAlloc(sizeValue, alignment);
  IGF.Builder.CreateLifetimeStart(address, Size(-1) /*dynamic size*/);
  return address;
}

void irgen::emitDeallocAsyncContext(IRGenFunction &IGF, Address context) {
  IGF.emitTaskDealloc(context);
  IGF.Builder.CreateLifetimeEnd(context, Size(-1) /*dynamic size*/);
}

Address irgen::emitStaticAllocAsyncContext(IRGenFunction &IGF,
                                           Size size) {
  auto alignment = IGF.IGM.getAsyncContextAlignment();
  auto &IGM = IGF.IGM;
  auto address = IGF.createAlloca(IGM.Int8Ty, IGM.getSize(size), alignment);
  IGF.Builder.CreateLifetimeStart(address, size);
  return address;
}

void irgen::emitStaticDeallocAsyncContext(IRGenFunction &IGF, Address context,
                                          Size size) {
  IGF.Builder.CreateLifetimeEnd(context, size);
}

llvm::Value *irgen::emitYield(IRGenFunction &IGF,
                              CanSILFunctionType coroutineType,
                              Explosion &substValues) {
  // TODO: Handle async!
  auto coroSignature = IGF.IGM.getSignature(coroutineType);
  auto coroInfo = coroSignature.getCoroutineInfo();

  // Translate the arguments to an unsubstituted form.
  Explosion allComponents;
  for (auto yield : coroutineType->getYields())
    addNativeArgument(IGF, substValues, coroutineType,
                      yield, allComponents, false);

  // Figure out which arguments need to be yielded directly.
  SmallVector<llvm::Value*, 8> yieldArgs;

  // Add the direct yield components.
  auto directComponents =
    allComponents.claim(coroInfo.NumDirectYieldComponents);
  yieldArgs.append(directComponents.begin(), directComponents.end());

  // The rest need to go into an indirect buffer.
  auto indirectComponents = allComponents.claimAll();

  auto resultStructTy =
    dyn_cast<llvm::StructType>(coroSignature.getType()->getReturnType());
  assert((!resultStructTy
             && directComponents.empty()
             && indirectComponents.empty())
         || (resultStructTy
             && resultStructTy->getNumElements() ==
                  (1 + directComponents.size()
                     + unsigned(!indirectComponents.empty()))));

  // Fill in the indirect buffer if necessary.
  std::optional<Address> indirectBuffer;
  Size indirectBufferSize;
  if (!indirectComponents.empty()) {
    auto bufferStructTy = coroInfo.indirectResultsType;
    auto layout = IGF.IGM.DataLayout.getStructLayout(bufferStructTy);
    indirectBuffer = IGF.createAlloca(
        bufferStructTy, Alignment(layout->getAlignment().value()));
    indirectBufferSize = Size(layout->getSizeInBytes());
    IGF.Builder.CreateLifetimeStart(*indirectBuffer, indirectBufferSize);

    for (size_t i : indices(bufferStructTy->elements())) {
      // Skip padding elements.
      if (bufferStructTy->getElementType(i)->isArrayTy())
        continue;

      assert(!indirectComponents.empty() &&
             "insufficient number of indirect yield components");

      auto addr = IGF.Builder.CreateStructGEP(*indirectBuffer, i, layout);
      IGF.Builder.CreateStore(indirectComponents.front(), addr);
      indirectComponents = indirectComponents.drop_front();
    }

    assert(indirectComponents.empty() && "too many indirect yield components");

    // Remember to yield the indirect buffer.
    yieldArgs.push_back(indirectBuffer->getAddress());
  }

  // Perform the yield.
  auto isUnwind = IGF.Builder.CreateIntrinsicCall(
      llvm::Intrinsic::coro_suspend_retcon, {IGF.IGM.Int1Ty}, yieldArgs);

  // We're done with the indirect buffer.
  if (indirectBuffer) {
    IGF.Builder.CreateLifetimeEnd(*indirectBuffer, indirectBufferSize);
  }

  return isUnwind;
}

/// Add a new set of arguments to the function.
void CallEmission::setArgs(Explosion &adjusted, bool isOutlined,
                           WitnessMetadata *witnessMetadata) {
  assert(state == State::Emitting);
  // Add the given number of arguments.
  assert(LastArgWritten >= adjusted.size());

  size_t targetIndex = LastArgWritten - adjusted.size();
  assert(targetIndex <= 1);
  LastArgWritten = targetIndex;

  auto argIterator = Args.begin() + targetIndex;
  for (auto value : adjusted.claimAll()) {
    *argIterator++ = value;
  }
}

void CallEmission::addFnAttribute(llvm::Attribute::AttrKind attr) {
  assert(state == State::Emitting);
  auto &attrs = CurCallee.getMutableAttributes();
  attrs = attrs.addFnAttribute(IGF.IGM.getLLVMContext(), attr);
}

void CallEmission::addParamAttribute(unsigned paramIndex,
                                     llvm::Attribute::AttrKind attr) {
  assert(state == State::Emitting);
  auto &attrs = CurCallee.getMutableAttributes();
  attrs = attrs.addParamAttribute(IGF.IGM.getLLVMContext(), paramIndex, attr);
}

/// Initialize an Explosion with the parameters of the current
/// function.  All of the objects will be added unmanaged.  This is
/// really only useful when writing prologue code.
Explosion IRGenFunction::collectParameters() {
  Explosion params;
  for (auto i = CurFn->arg_begin(), e = CurFn->arg_end(); i != e; ++i)
    params.add(&*i);
  return params;
}
Address IRGenFunction::createErrorResultSlot(SILType errorType, bool isAsync,
                                             bool setSwiftErrorFlag,
                                             bool isTypedError) {

  IRBuilder builder(IGM.getLLVMContext(), IGM.DebugInfo != nullptr);
  builder.SetInsertPoint(AllocaIP->getParent(), AllocaIP->getIterator());

  auto errorStorageType = isTypedError ? IGM.Int8PtrTy :
    cast<FixedTypeInfo>(getTypeInfo(errorType)).getStorageType();
  auto errorAlignment  = isTypedError ? IGM.getPointerAlignment() :
    cast<FixedTypeInfo>(getTypeInfo(errorType)).getFixedAlignment();

  // Pass an address for zero sized types.
  if (!isTypedError && !setSwiftErrorFlag &&
      cast<FixedTypeInfo>(getTypeInfo(errorType)).getFixedSize() == Size(0)) {
    errorStorageType = IGM.Int8PtrTy;
    errorAlignment = IGM.getPointerAlignment();
  }

  // Create the alloca.  We don't use allocateStack because we're
  // not allocating this in stack order.
  auto addr = createAlloca(errorStorageType,
                           errorAlignment, "swifterror");

  if (!isAsync) {
    builder.SetInsertPoint(getEarliestInsertionPoint()->getParent(),
                           getEarliestInsertionPoint()->getIterator());
  }

  // Only add the swifterror attribute on ABIs that pass it in a register.
  // We create a shadow stack location of the swifterror parameter for the
  // debugger on platforms that pass swifterror by reference and so we can't
  // mark the parameter with a swifterror attribute for these.
  // The slot for async callees cannot be annotated swifterror because those
  // errors are never passed in registers but rather are always passed
  // indirectly in the async context.
  if (IGM.ShouldUseSwiftError && !isAsync && setSwiftErrorFlag)
    cast<llvm::AllocaInst>(addr.getAddress())->setSwiftError(true);

  // Initialize at the alloca point.
  if (setSwiftErrorFlag) {
    auto nullError = llvm::ConstantPointerNull::get(
        cast<llvm::PointerType>(errorStorageType));
    builder.CreateStore(nullError, addr);
  }

  return addr;
}

/// Fetch the error result slot.
Address IRGenFunction::getCalleeErrorResultSlot(SILType errorType,
                                                bool isTypedError) {
  if (!CalleeErrorResultSlot.isValid()) {
    CalleeErrorResultSlot = createErrorResultSlot(errorType, /*isAsync=*/false,
                                                  /*setSwiftError*/true,
                                                  isTypedError);
  }
  return CalleeErrorResultSlot;
}

Address IRGenFunction::getCalleeTypedErrorResultSlot(SILType errorType) {

  auto &errorTI = cast<FixedTypeInfo>(getTypeInfo(errorType));
  if (!CalleeTypedErrorResultSlot.isValid() ||
      CalleeTypedErrorResultSlot.getElementType() != errorTI.getStorageType()) {
    CalleeTypedErrorResultSlot =
      createErrorResultSlot(errorType, /*isAsync=*/false,
                            /*setSwiftErrorFlag*/false);
  }
  return CalleeTypedErrorResultSlot;
}

void IRGenFunction::setCalleeTypedErrorResultSlot(Address addr) {
  CalleeTypedErrorResultSlot = addr;
}


/// Fetch the error result slot received from the caller.
Address IRGenFunction::getCallerErrorResultSlot() {
  assert(CallerErrorResultSlot.isValid() && "no error result slot!");
  assert(isa<llvm::Argument>(CallerErrorResultSlot.getAddress()) &&
             !isAsync() ||
         isa<llvm::LoadInst>(CallerErrorResultSlot.getAddress()) && isAsync() &&
             "error result slot is local!");
  return CallerErrorResultSlot;
}

// Set the error result slot.  This should only be done in the prologue.
void IRGenFunction::setCallerErrorResultSlot(Address address) {
  assert(!CallerErrorResultSlot.isValid() &&
         "already have a caller error result slot!");
  assert(isa<llvm::PointerType>(address.getAddress()->getType()));
  CallerErrorResultSlot = address;
  if (!isAsync()) {
    CalleeErrorResultSlot = address;
  }
}

// Set the error result slot for a typed throw for the current function.
// This should only be done in the prologue.
void IRGenFunction::setCallerTypedErrorResultSlot(Address address) {
  assert(!CallerTypedErrorResultSlot.isValid() &&
         "already have a caller error result slot!");
  assert(isa<llvm::PointerType>(address.getAddress()->getType()));
  CallerTypedErrorResultSlot = address;
}

Address IRGenFunction::getCallerTypedErrorResultSlot() {
  assert(CallerTypedErrorResultSlot.isValid() && "no error result slot!");
  assert(isa<llvm::Argument>(CallerTypedErrorResultSlot.getAddress()));
  return CallerTypedErrorResultSlot;
}
/// Emit the basic block that 'return' should branch to and insert it into
/// the current function. This creates a second
/// insertion point that most blocks should be inserted before.
void IRGenFunction::emitBBForReturn() {
  ReturnBB = createBasicBlock("return");
  CurFn->insert(CurFn->end(), ReturnBB);
}

llvm::BasicBlock *IRGenFunction::createExceptionUnwindBlock() {
  auto *result = createBasicBlock("exception.unwind");
  IRBuilder::SavedInsertionPointRAII insertRAII(Builder, result);

  // Create a catch-all landing pad.
  // FIXME: Refactor Clang/lib/CodeGen to call into Clang here.
  // FIXME: MSVC support.
  llvm::LandingPadInst *lpad = Builder.CreateLandingPad(
      llvm::StructType::get(IGM.Int8PtrTy, IGM.Int32Ty), 0);
  lpad->addClause(llvm::ConstantPointerNull::get(IGM.Int8PtrTy));

  // The trap with a message informs the user that the exception hasn't
  // been caught. The trap creates a new debug inline frame for the message,
  // so ensure that the current debug location is preserved.
  auto oldDebugLoc = Builder.getCurrentDebugLocation();
  emitTrap(IGM.Context.LangOpts.EnableObjCInterop
               ? "unhandled C++ / Objective-C exception"
               : "unhandled C++ exception",
           /*Unreachable=*/true);
  Builder.SetCurrentDebugLocation(oldDebugLoc);

  ExceptionUnwindBlocks.push_back(result);
  return result;
}

void IRGenFunction::createExceptionTrapScope(
    llvm::function_ref<void(llvm::BasicBlock *, llvm::BasicBlock *)>
        invokeEmitter) {
  auto *invokeNormalDest = createBasicBlock("invoke.cont");
  auto *invokeUnwindDest = createExceptionUnwindBlock();
  invokeEmitter(invokeNormalDest, invokeUnwindDest);
  Builder.emitBlock(invokeNormalDest);
}

/// Emit the prologue for the function.
void IRGenFunction::emitPrologue() {
  // Set up the IRBuilder.
  llvm::BasicBlock *EntryBB = createBasicBlock("entry");
  assert(CurFn->empty() && "prologue already emitted?");
  CurFn->insert(CurFn->end(), EntryBB);
  Builder.SetInsertPoint(EntryBB);

  // Set up the alloca insertion point.
  AllocaIP = Builder.IRBuilderBase::CreateAlloca(IGM.Int1Ty,
                                                 /*array size*/ nullptr,
                                                 "alloca point");
  EarliestIP = AllocaIP;
}

/// Emit a branch to the return block and set the insert point there.
/// Returns true if the return block is reachable, false otherwise.
bool IRGenFunction::emitBranchToReturnBB() {
  // If there are no edges to the return block, we never want to emit it.
  if (ReturnBB->use_empty()) {
    ReturnBB->eraseFromParent();
    
    // Normally this means that we'll just insert the epilogue in the
    // current block, but if the current IP is unreachable then so is
    // the entire epilogue.
    if (!Builder.hasValidIP())
      return false;
    
    // Otherwise, branch to it if the current IP is reachable.
  } else if (Builder.hasValidIP()) {
    Builder.CreateBr(ReturnBB);
    Builder.SetInsertPoint(ReturnBB);
    
    // Otherwise, if there is exactly one use of the return block, merge
    // it into its predecessor.
  } else if (ReturnBB->hasOneUse()) {
    // return statements are never emitted as conditional branches.
    llvm::BranchInst *Br = cast<llvm::BranchInst>(*ReturnBB->use_begin());
    assert(Br->isUnconditional());
    Builder.SetInsertPoint(Br->getParent());
    Br->eraseFromParent();
    ReturnBB->eraseFromParent();
    
    // Otherwise, just move the IP to the return block.
  } else {
    Builder.SetInsertPoint(ReturnBB);
  }
  return true;
}

llvm::Function *IRGenModule::getForeignExceptionHandlingPersonalityFunc() {
  if (foreignExceptionHandlingPersonalityFunc)
    return foreignExceptionHandlingPersonalityFunc;
  foreignExceptionHandlingPersonalityFunc = llvm::Function::Create(
      llvm::FunctionType::get(Int32Ty, true), llvm::Function::ExternalLinkage,
      "__gxx_personality_v0", getModule());
  return foreignExceptionHandlingPersonalityFunc;
}

bool IRGenModule::isForeignExceptionHandlingEnabled() const {
  // FIXME: Support exceptions on windows MSVC.
  if (Triple.isWindowsMSVCEnvironment())
    return false;
  const auto &clangLangOpts =
      Context.getClangModuleLoader()->getClangASTContext().getLangOpts();
  return Context.LangOpts.EnableCXXInterop && clangLangOpts.Exceptions &&
         !clangLangOpts.IgnoreExceptions;
}

bool IRGenModule::isCxxNoThrow(clang::FunctionDecl *fd, bool defaultNoThrow) {
  auto *fpt = fd->getType()->getAs<clang::FunctionProtoType>();
  if (!fpt)
    return defaultNoThrow;
  if (fpt->getExceptionSpecType() ==
      clang::ExceptionSpecificationType::EST_Unevaluated) {
    // Clang might not have evaluated the exception spec for
    // a constructor, so force the evaluation of it.
    auto &clangSema = Context.getClangModuleLoader()->getClangSema();
    clangSema.EvaluateImplicitExceptionSpec(fd->getLocation(), fd);
    fpt = fd->getType()->getAs<clang::FunctionProtoType>();
    if (!fpt)
      return defaultNoThrow;
  }
  return fpt->isNothrow();
}

/// Emit the epilogue for the function.
void IRGenFunction::emitEpilogue() {
  if (EarliestIP != AllocaIP)
    EarliestIP->eraseFromParent();
  // Destroy the alloca insertion point.
  AllocaIP->eraseFromParent();
  // Add exception unwind blocks and additional exception handling info
  // if needed.
  if (!ExceptionUnwindBlocks.empty() ||
      callsAnyAlwaysInlineThunksWithForeignExceptionTraps) {
    // The function should have an unwind table when catching exceptions.
    CurFn->addFnAttr(llvm::Attribute::getWithUWTableKind(
        *IGM.LLVMContext, llvm::UWTableKind::Default));

    llvm::Constant *personality;

    if (IGM.isSwiftExceptionPersonalityFeatureAvailable()) {
      // The function should use our personality routine
      auto swiftPersonality = IGM.getExceptionPersonalityFunctionPointer();
      personality = swiftPersonality.getDirectPointer();
    } else {
      personality = IGM.getForeignExceptionHandlingPersonalityFunc();
    }

    CurFn->setPersonalityFn(personality);
  }
  for (auto *bb : ExceptionUnwindBlocks)
    CurFn->insert(CurFn->end(), bb);
}

std::pair<Address, Size>
irgen::allocateForCoercion(IRGenFunction &IGF,
                           llvm::Type *fromTy,
                           llvm::Type *toTy,
                           const llvm::Twine &basename) {
  auto &DL = IGF.IGM.DataLayout;
  
  auto fromSize = DL.getTypeSizeInBits(fromTy);
  auto toSize = DL.getTypeSizeInBits(toTy);
  auto bufferTy = fromSize >= toSize
    ? fromTy
    : toTy;

  llvm::Align alignment =
      std::max(DL.getABITypeAlign(fromTy), DL.getABITypeAlign(toTy));

  auto buffer = IGF.createAlloca(bufferTy, Alignment(alignment.value()),
                                 basename + ".coerced");
  
  Size size(std::max(fromSize, toSize));
  return {buffer, size};
}

llvm::Value* IRGenFunction::coerceValue(llvm::Value *value, llvm::Type *toTy,
                                        const llvm::DataLayout &DL)
{
  llvm::Type *fromTy = value->getType();
  assert(fromTy != toTy && "Unexpected same types in type coercion!");
  assert(!fromTy->isVoidTy()
         && "Unexpected void source type in type coercion!");
  assert(!toTy->isVoidTy()
         && "Unexpected void destination type in type coercion!");

  // Use the pointer/pointer and pointer/int casts if we can.
  if (toTy->isPointerTy()) {
    if (fromTy->isPointerTy())
      return Builder.CreateBitCast(value, toTy);
    if (fromTy == IGM.IntPtrTy)
      return Builder.CreateIntToPtr(value, toTy);
  } else if (fromTy->isPointerTy()) {
    if (toTy == IGM.IntPtrTy) {
      return Builder.CreatePtrToInt(value, toTy);
    }
  }

  // Otherwise we need to store, bitcast, and load.
  Address address; Size size;
  std::tie(address, size) = allocateForCoercion(*this, fromTy, toTy,
                                                value->getName() + ".coercion");
  Builder.CreateLifetimeStart(address, size);
  auto orig = Builder.CreateElementBitCast(address, fromTy);
  Builder.CreateStore(value, orig);
  auto coerced = Builder.CreateElementBitCast(address, toTy);
  auto loaded = Builder.CreateLoad(coerced);
  Builder.CreateLifetimeEnd(address, size);
  return loaded;
}

llvm::Value *irgen::convertForDirectError(IRGenFunction &IGF,
                                          llvm::Value *value, llvm::Type *toTy,
                                          bool forExtraction) {
  auto &Builder = IGF.Builder;
  auto *fromTy = value->getType();
  if (toTy->isIntOrPtrTy() && fromTy->isIntOrPtrTy() && toTy != fromTy) {

    if (toTy->isPointerTy()) {
      if (fromTy->isPointerTy())
        return Builder.CreateBitCast(value, toTy);
      return Builder.CreateIntToPtr(value, toTy);
    } else if (fromTy->isPointerTy()) {
      return Builder.CreatePtrToInt(value, toTy);
    }

    if (forExtraction) {
      return Builder.CreateTruncOrBitCast(value, toTy);
    } else {
      return Builder.CreateZExtOrBitCast(value, toTy);
    }
  }
  return value;
}

void IRGenFunction::emitScalarReturn(llvm::Type *resultType,
                                     Explosion &result) {
  if (result.empty()) {
    Builder.CreateRetVoid();
    return;
  }

  auto *ABIType = CurFn->getReturnType();

  if (result.size() == 1) {
    auto *returned = result.claimNext();
    if (ABIType != returned->getType())
      returned = coerceValue(returned, ABIType, IGM.DataLayout);

    Builder.CreateRet(returned);
    return;
  }

  // Multiple return values are returned as a struct.
  assert(cast<llvm::StructType>(resultType)->getNumElements() == result.size());
  llvm::Value *resultAgg = llvm::UndefValue::get(resultType);
  for (unsigned i = 0, e = result.size(); i != e; ++i) {
    llvm::Value *elt = result.claimNext();
    resultAgg = Builder.CreateInsertValue(resultAgg, elt, i);
  }

  if (ABIType != resultType)
    resultAgg = coerceValue(resultAgg, ABIType, IGM.DataLayout);

  Builder.CreateRet(resultAgg);
}

/// Adjust the alignment of the alloca pointed to by \p allocaAddr to the
/// required alignment of the struct \p type.
static void adjustAllocaAlignment(const llvm::DataLayout &DL,
                                  Address allocaAddr, llvm::StructType *type) {
  auto layout = DL.getStructLayout(type);
  Alignment layoutAlignment = Alignment(layout->getAlignment().value());
  auto alloca = cast<llvm::AllocaInst>(allocaAddr.getAddress());
  if (alloca->getAlign() < layoutAlignment.getValue()) {
    alloca->setAlignment(
        llvm::MaybeAlign(layoutAlignment.getValue()).valueOrOne());
    allocaAddr = Address(allocaAddr.getAddress(), allocaAddr.getElementType(),
                         layoutAlignment);
  }
}

unsigned NativeConventionSchema::size() const {
  if (empty())
    return 0;
  unsigned size = 0;
  enumerateComponents([&](clang::CharUnits offset, clang::CharUnits end,
                          llvm::Type *type) { ++size; });
  return size;
}

static bool canMatchByTruncation(IRGenModule &IGM,
                                 ArrayRef<llvm::Type*> expandedTys,
                                 const ExplosionSchema &schema) {
  // If the schemas don't even match in number, we have to go
  // through memory.
  if (expandedTys.size() != schema.size() || expandedTys.empty())
    return false;

  if (expandedTys.size() == 1) return false;

  // If there are multiple elements, the pairs of types need to
  // match in size upto the penultimate for the truncation to work.
  size_t e = expandedTys.size();
  for (size_t i = 0; i != e - 1; ++i) {
    // Check that we can truncate the last element.
    llvm::Type *outputTy = schema[i].getScalarType();
    llvm::Type *inputTy = expandedTys[i];
    if (inputTy != outputTy &&
        IGM.DataLayout.getTypeSizeInBits(inputTy) !=
        IGM.DataLayout.getTypeSizeInBits(outputTy))
      return false;
  }
  llvm::Type *outputTy = schema[e-1].getScalarType();
  llvm::Type *inputTy = expandedTys[e-1];
  return inputTy == outputTy || (IGM.DataLayout.getTypeSizeInBits(inputTy) ==
                                 IGM.DataLayout.getTypeSizeInBits(outputTy)) ||
         (IGM.DataLayout.getTypeSizeInBits(inputTy) >
              IGM.DataLayout.getTypeSizeInBits(outputTy) &&
          isa<llvm::IntegerType>(inputTy) && isa<llvm::IntegerType>(outputTy));
}

Explosion NativeConventionSchema::mapFromNative(IRGenModule &IGM,
                                                IRGenFunction &IGF,
                                                Explosion &native,
                                                SILType type) const {
  if (native.empty()) {
    assert(empty() && "Empty explosion must match the native convention");
    return Explosion();
  }

  assert(!empty());

  auto *nativeTy = getExpandedType(IGM);
  auto expandedTys = expandScalarOrStructTypeToArray(nativeTy);
  auto &TI = IGM.getTypeInfo(type);
  auto schema = TI.getSchema();
  // The expected explosion type.
  auto *explosionTy = schema.getScalarResultType(IGM);

  // Check whether we can coerce the explosion to the expected type convention.
  auto &DataLayout = IGM.DataLayout;
  Explosion nonNativeExplosion;
  if (canCoerceToSchema(IGM, expandedTys, schema)) {
    if (native.size() == 1) {
      auto *elt = native.claimNext();
      if (explosionTy != elt->getType()) {
        if (isa<llvm::IntegerType>(explosionTy) &&
            isa<llvm::IntegerType>(elt->getType())) {
          // [HACK: Atomic-Bool-IRGen] In the case of _Atomic(_Bool), Clang
          // treats it as i8 whereas Swift works with i1, so we need to zext
          // in that case.
          elt = IGF.Builder.CreateZExtOrTrunc(elt, explosionTy);
        } else {
          elt = IGF.coerceValue(elt, explosionTy, DataLayout);
        }
      }
      nonNativeExplosion.add(elt);
      return nonNativeExplosion;
    } else if (nativeTy == explosionTy) {
      native.transferInto(nonNativeExplosion, native.size());
      return nonNativeExplosion;
    }
    // Otherwise, we have to go through memory if we can match by truncation.
  } else if (canMatchByTruncation(IGM, expandedTys, schema)) {
    assert(expandedTys.size() == schema.size());
    for (size_t i = 0, e = expandedTys.size(); i != e; ++i) {
      auto *elt = native.claimNext();
      auto *schemaTy = schema[i].getScalarType();
      auto *nativeTy = elt->getType();
      assert(nativeTy == expandedTys[i]);
      if (schemaTy == nativeTy) {
        // elt = elt
      } else if (DataLayout.getTypeSizeInBits(schemaTy) ==
                 DataLayout.getTypeSizeInBits(nativeTy))
        elt = IGF.coerceValue(elt, schemaTy, DataLayout);
      else {
        assert(DataLayout.getTypeSizeInBits(schemaTy) <
               DataLayout.getTypeSizeInBits(nativeTy));
        elt = IGF.Builder.CreateTrunc(elt, schemaTy);
      }
      nonNativeExplosion.add(elt);
    }
    return nonNativeExplosion;
  }

  // If not, go through memory.
  auto &loadableTI = cast<LoadableTypeInfo>(TI);

  // We can get two layouts if there are overlapping ranges in the legal type
  // sequence.
  llvm::StructType *coercionTy, *overlappedCoercionTy;
  SmallVector<unsigned, 8> expandedTyIndicesMap;
  std::tie(coercionTy, overlappedCoercionTy) =
      getCoercionTypes(IGM, expandedTyIndicesMap);

  // Get the larger layout out of those two.
  auto coercionSize = DataLayout.getTypeSizeInBits(coercionTy);
  auto overlappedCoercionSize =
      DataLayout.getTypeSizeInBits(overlappedCoercionTy);
  llvm::StructType *largerCoercion = coercionSize >= overlappedCoercionSize
                                         ? coercionTy
                                         : overlappedCoercionTy;

  // Allocate a temporary for the coercion.
  Address temporary;
  Size tempSize;
  std::tie(temporary, tempSize) = allocateForCoercion(
      IGF, largerCoercion, loadableTI.getStorageType(), "temp-coercion");

  // Make sure we have sufficiently large alignment.
  adjustAllocaAlignment(DataLayout, temporary, coercionTy);
  adjustAllocaAlignment(DataLayout, temporary, overlappedCoercionTy);

  auto &Builder = IGF.Builder;
  Builder.CreateLifetimeStart(temporary, tempSize);

  // Store the expanded type elements.
  auto coercionAddr = Builder.CreateElementBitCast(temporary, coercionTy);
  unsigned expandedMapIdx = 0;

  auto eltsArray = native.claimAll();
  SmallVector<llvm::Value *, 8> nativeElts(eltsArray.begin(), eltsArray.end());
  auto storeToFn = [&](llvm::StructType *ty, Address structAddr) {
    for (auto eltIndex : indices(ty->elements())) {
      auto layout = DataLayout.getStructLayout(ty);
      auto eltTy = ty->getElementType(eltIndex);
      // Skip padding fields.
      if (eltTy->isArrayTy())
        continue;
      Address eltAddr = Builder.CreateStructGEP(structAddr, eltIndex, layout);
      auto index = expandedTyIndicesMap[expandedMapIdx];
      assert(index < nativeElts.size() && nativeElts[index] != nullptr);
      auto nativeElt = nativeElts[index];
      Builder.CreateStore(nativeElt, eltAddr);
      nativeElts[index] = nullptr;
      ++expandedMapIdx;
    }
  };

  storeToFn(coercionTy, coercionAddr);
  if (!overlappedCoercionTy->isEmptyTy()) {
    auto overlappedCoercionAddr =
        Builder.CreateElementBitCast(temporary, overlappedCoercionTy);
    storeToFn(overlappedCoercionTy, overlappedCoercionAddr);
  }

  // Reload according to the types schema.
  Address storageAddr =
      Builder.CreateElementBitCast(temporary, loadableTI.getStorageType());
  loadableTI.loadAsTake(IGF, storageAddr, nonNativeExplosion);

  Builder.CreateLifetimeEnd(temporary, tempSize);

  return nonNativeExplosion;
}

Explosion NativeConventionSchema::mapIntoNative(IRGenModule &IGM,
                                                IRGenFunction &IGF,
                                                Explosion &fromNonNative,
                                                SILType type,
                                                bool isOutlined,
                                                bool mayPeepholeLoad) const {
  if (fromNonNative.empty()) {
    assert(empty() && "Empty explosion must match the native convention");
    return Explosion();
  }

  assert(!requiresIndirect() && "Expected direct convention");
  assert(!empty());

  auto *nativeTy = getExpandedType(IGM);
  auto expandedTys = expandScalarOrStructTypeToArray(nativeTy);
  auto &TI = IGM.getTypeInfo(type);
  auto schema = TI.getSchema();
  auto *explosionTy = schema.getScalarResultType(IGM);

  // Check whether we can coerce the explosion to the expected type convention.
  auto &DataLayout = IGM.DataLayout;
  Explosion nativeExplosion;
  if (canCoerceToSchema(IGM, expandedTys, schema)) {
    if (fromNonNative.size() == 1) {
      auto *elt = fromNonNative.claimNext();
      if (nativeTy != elt->getType()) {
        if (isa<llvm::IntegerType>(nativeTy) &&
            isa<llvm::IntegerType>(elt->getType())) {
          // [HACK: Atomic-Bool-IRGen] In the case of _Atomic(_Bool), Clang
          // treats it as i8 whereas Swift works with i1, so we need to trunc
          // in that case.
          elt = IGF.Builder.CreateZExtOrTrunc(elt, nativeTy);
        } else {
          elt = IGF.coerceValue(elt, nativeTy, DataLayout);
        }
      }
      nativeExplosion.add(elt);
      return nativeExplosion;
    } else if (nativeTy == explosionTy) {
      fromNonNative.transferInto(nativeExplosion, fromNonNative.size());
      return nativeExplosion;
    }
    // Otherwise, we have to go through memory if we can't match by truncation.
  } else if (canMatchByTruncation(IGM, expandedTys, schema)) {
    assert(expandedTys.size() == schema.size());
    for (size_t i = 0, e = expandedTys.size(); i != e; ++i) {
      auto *elt = fromNonNative.claimNext();
      auto *schemaTy = elt->getType();
      auto *nativeTy = expandedTys[i];
      assert(schema[i].getScalarType() == schemaTy);
      if (schemaTy == nativeTy) {
        // elt = elt
      } else if (DataLayout.getTypeSizeInBits(schemaTy) ==
                 DataLayout.getTypeSizeInBits(nativeTy))
        elt = IGF.coerceValue(elt, nativeTy, DataLayout);
      else {
        assert(DataLayout.getTypeSizeInBits(schemaTy) <
               DataLayout.getTypeSizeInBits(nativeTy));
        elt = IGF.Builder.CreateZExt(elt, nativeTy);
      }
      nativeExplosion.add(elt);
    }
    return nativeExplosion;
  }

  // If not, go through memory.
  auto &loadableTI = cast<LoadableTypeInfo>(TI);

  // We can get two layouts if there are overlapping ranges in the legal type
  // sequence.
  llvm::StructType *coercionTy, *overlappedCoercionTy;
  SmallVector<unsigned, 8> expandedTyIndicesMap;
  std::tie(coercionTy, overlappedCoercionTy) =
      getCoercionTypes(IGM, expandedTyIndicesMap);

  // Get the larger layout out of those two.
  auto coercionSize = DataLayout.getTypeSizeInBits(coercionTy);
  auto overlappedCoercionSize =
      DataLayout.getTypeSizeInBits(overlappedCoercionTy);
  llvm::StructType *largerCoercion = coercionSize >= overlappedCoercionSize
                                         ? coercionTy
                                         : overlappedCoercionTy;

  if (mayPeepholeLoad) {
    auto succeeded = [&]() -> bool {
      if (!overlappedCoercionTy->isEmptyTy())
        return false;
      auto load = dyn_cast<llvm::LoadInst>(*fromNonNative.begin());
      if (!load)
        return false;
      auto *gep = dyn_cast<llvm::GetElementPtrInst>(load->getPointerOperand());
      if (!gep)
        return false;
      auto *alloca = dyn_cast<llvm::AllocaInst>(getUnderlyingObject(gep));
      if (!alloca)
        return false;
      auto numExplosions = fromNonNative.size();
      if (numExplosions < 2)
        return false;
      for (unsigned i = 0, e = numExplosions; i < e; ++i) {
        auto *otherLoad = dyn_cast<llvm::LoadInst>(*(fromNonNative.begin() + i));
        if (!otherLoad)
          return false;
        auto otherAlloca = dyn_cast<llvm::AllocaInst>(
          getUnderlyingObject(otherLoad->getPointerOperand()));
        if (!otherAlloca || otherAlloca != alloca)
          return false;
        load = otherLoad;
      }
      auto allocaSize =
        DataLayout.getTypeSizeInBits(alloca->getAllocatedType());

      Address origAlloca(alloca, alloca->getAllocatedType(),
                         Alignment(alloca->getAlign().value()));

      IRBuilder Builder(*IGM.LLVMContext, false);
      Builder.SetInsertPoint(load);

      if (allocaSize < coercionSize) {
        auto coerced = IGF.createAlloca(coercionTy, Alignment(alloca->getAlign().value()) , "tmp.coerce");
        // Copy the defined bytes.
        Builder.CreateMemCpy(coerced, origAlloca, Size(allocaSize/8));
        origAlloca = coerced;
      }

      adjustAllocaAlignment(DataLayout, origAlloca, coercionTy);


      unsigned expandedMapIdx = 0;
      SmallVector<llvm::Value *, 8> expandedElts(expandedTys.size(), nullptr);
      auto structAddr = Builder.CreateElementBitCast(origAlloca, coercionTy);
      for (auto eltIndex : indices(coercionTy->elements())) {
        auto layout = DataLayout.getStructLayout(coercionTy);
        auto eltTy = coercionTy->getElementType(eltIndex);
        // Skip padding fields.
        if (eltTy->isArrayTy())
          continue;
        Address eltAddr = Builder.CreateStructGEP(structAddr, eltIndex, layout);
        llvm::Value *elt = Builder.CreateLoad(eltAddr);
        auto index = expandedTyIndicesMap[expandedMapIdx];
        assert(expandedElts[index] == nullptr);
        expandedElts[index] = elt;
        ++expandedMapIdx;
      }

      // Add the values to the explosion.
      for (auto *val : expandedElts)
        nativeExplosion.add(val);
      assert(expandedTys.size() == nativeExplosion.size());

      return true;
    }();

    if (succeeded) {
      (void)fromNonNative.claimAll();
      return nativeExplosion;
    }
  }

  // Allocate a temporary for the coercion.
  Address temporary;
  Size tempSize;
  std::tie(temporary, tempSize) = allocateForCoercion(
      IGF, largerCoercion, loadableTI.getStorageType(), "temp-coercion");

  // Make sure we have sufficiently large alignment.
  adjustAllocaAlignment(DataLayout, temporary, coercionTy);
  adjustAllocaAlignment(DataLayout, temporary, overlappedCoercionTy);

  auto &Builder = IGF.Builder;
  Builder.CreateLifetimeStart(temporary, tempSize);

  // Initialize the memory of the temporary.
  Address storageAddr =
      Builder.CreateElementBitCast(temporary, loadableTI.getStorageType());
  loadableTI.initialize(IGF, fromNonNative, storageAddr, isOutlined);

  // Load the expanded type elements from memory.
  auto coercionAddr = Builder.CreateElementBitCast(temporary, coercionTy);

  unsigned expandedMapIdx = 0;
  SmallVector<llvm::Value *, 8> expandedElts(expandedTys.size(), nullptr);

  auto loadFromFn = [&](llvm::StructType *ty, Address structAddr) {
    for (auto eltIndex : indices(ty->elements())) {
      auto layout = DataLayout.getStructLayout(ty);
      auto eltTy = ty->getElementType(eltIndex);
      // Skip padding fields.
      if (eltTy->isArrayTy())
        continue;
      Address eltAddr = Builder.CreateStructGEP(structAddr, eltIndex, layout);
      llvm::Value *elt = Builder.CreateLoad(eltAddr);
      auto index = expandedTyIndicesMap[expandedMapIdx];
      assert(expandedElts[index] == nullptr);
      expandedElts[index] = elt;
      ++expandedMapIdx;
    }
  };

  loadFromFn(coercionTy, coercionAddr);
  if (!overlappedCoercionTy->isEmptyTy()) {
    auto overlappedCoercionAddr =
        Builder.CreateElementBitCast(temporary, overlappedCoercionTy);
    loadFromFn(overlappedCoercionTy, overlappedCoercionAddr);
  }

  Builder.CreateLifetimeEnd(temporary, tempSize);

  // Add the values to the explosion.
  for (auto *val : expandedElts)
    nativeExplosion.add(val);

  assert(expandedTys.size() == nativeExplosion.size());
  return nativeExplosion;
}

Explosion IRGenFunction::coerceValueTo(SILType fromTy, Explosion &from,
                                       SILType toTy) {
  if (fromTy == toTy)
    return std::move(from);

  auto &fromTI = cast<LoadableTypeInfo>(IGM.getTypeInfo(fromTy));
  auto &toTI = cast<LoadableTypeInfo>(IGM.getTypeInfo(toTy));

  Explosion result;
  if (fromTI.getStorageType()->isPointerTy() &&
      toTI.getStorageType()->isPointerTy()) {
    auto ptr = from.claimNext();
    ptr = Builder.CreateBitCast(ptr, toTI.getStorageType());
    result.add(ptr);
    return result;
  }

  auto temporary = toTI.allocateStack(*this, toTy, "coerce.temp");

  auto addr =
      Address(Builder.CreateBitCast(temporary.getAddressPointer(),
                                    fromTI.getStorageType()->getPointerTo()),
              fromTI.getStorageType(), temporary.getAlignment());
  fromTI.initialize(*this, from, addr, false);

  toTI.loadAsTake(*this, temporary.getAddress(), result);
  toTI.deallocateStack(*this, temporary, toTy);
  return result;
}

void IRGenFunction::emitScalarReturn(SILType returnResultType,
                                     SILType funcResultType, Explosion &result,
                                     bool isSwiftCCReturn, bool isOutlined,
                                     bool mayPeepholeLoad, SILType errorType) {
  bool mayReturnErrorDirectly = false;
  if (errorType) {
    auto &errorTI = IGM.getTypeInfo(errorType);
    auto &nativeError = errorTI.nativeReturnValueSchema(IGM);
    mayReturnErrorDirectly = !nativeError.shouldReturnTypedErrorIndirectly();
  }

  if (result.empty() && !mayReturnErrorDirectly) {
    assert(IGM.getTypeInfo(returnResultType)
               .nativeReturnValueSchema(IGM)
               .empty() &&
           "Empty explosion must match the native calling convention");

    Builder.CreateRetVoid();
    return;
  }

  // In the native case no coercion is needed.
  if (isSwiftCCReturn) {
    auto &resultTI = IGM.getTypeInfo(funcResultType);
    auto &nativeSchema = resultTI.nativeReturnValueSchema(IGM);
    assert(!nativeSchema.requiresIndirect());
    result = coerceValueTo(returnResultType, result, funcResultType);

    Explosion native = nativeSchema.mapIntoNative(IGM, *this, result,
                                                  funcResultType, isOutlined,
                                                  mayPeepholeLoad);
    llvm::Value *nativeAgg = nullptr;

    if (mayReturnErrorDirectly) {
      auto &errorTI = IGM.getTypeInfo(errorType);
      auto &nativeError = errorTI.nativeReturnValueSchema(IGM);
      auto *combinedTy =
          combineResultAndTypedErrorType(IGM, nativeSchema, nativeError)
              .combinedTy;

      if (combinedTy->isVoidTy()) {
        Builder.CreateRetVoid();
        return;
      }

      if (native.empty()) {
        Builder.CreateRet(llvm::UndefValue::get(combinedTy));
        return;
      }

      if (auto *structTy = dyn_cast<llvm::StructType>(combinedTy)) {
        nativeAgg = llvm::UndefValue::get(combinedTy);
        for (unsigned i = 0, e = native.size(); i != e; ++i) {
          llvm::Value *elt = native.claimNext();
          auto *nativeTy = structTy->getElementType(i);
          elt = convertForDirectError(*this, elt, nativeTy,
                                      /*forExtraction*/ false);
          nativeAgg = Builder.CreateInsertValue(nativeAgg, elt, i);
        }
      } else {
        nativeAgg = convertForDirectError(*this, native.claimNext(), combinedTy,
                                          /*forExtraction*/ false);
      }
    }

    if (!nativeAgg) {
      if (native.size() == 1) {
        Builder.CreateRet(native.claimNext());
        return;
      }

      nativeAgg = llvm::UndefValue::get(nativeSchema.getExpandedType(IGM));

      for (unsigned i = 0, e = native.size(); i != e; ++i) {
        llvm::Value *elt = native.claimNext();
        nativeAgg = Builder.CreateInsertValue(nativeAgg, elt, i);
      }
    }

    Builder.CreateRet(nativeAgg);

    return;
  }

  // Otherwise we potentially need to coerce the type. We don't need to go
  // through the mapping to the native calling convention.
  auto *ABIType = CurFn->getReturnType();
  if (result.size() == 1) {
    auto *returned = result.claimNext();
    if (ABIType != returned->getType())
      returned = coerceValue(returned, ABIType, IGM.DataLayout);

    Builder.CreateRet(returned);
    return;
  }

  auto &resultTI = IGM.getTypeInfo(returnResultType);
  auto schema = resultTI.getSchema();
  auto *bodyType = schema.getScalarResultType(IGM);

  // Multiple return values are returned as a struct.
  assert(cast<llvm::StructType>(bodyType)->getNumElements() == result.size());
  llvm::Value *resultAgg = llvm::UndefValue::get(bodyType);
  for (unsigned i = 0, e = result.size(); i != e; ++i) {
    llvm::Value *elt = result.claimNext();
    resultAgg = Builder.CreateInsertValue(resultAgg, elt, i);
  }

  if (ABIType != bodyType)
    resultAgg = coerceValue(resultAgg, ABIType, IGM.DataLayout);

  Builder.CreateRet(resultAgg);
}

/// Modify the given variable to hold a pointer whose type is the
/// LLVM lowering of the given function type, and return the signature
/// for the type.
Signature irgen::emitCastOfFunctionPointer(IRGenFunction &IGF,
                                           llvm::Value *&fnPtr,
                                           CanSILFunctionType fnType,
                                           bool forAsyncReturn) {
  // Figure out the function type.
  // FIXME: Cache async signature.
  auto sig = forAsyncReturn ? Signature::forAsyncReturn(IGF.IGM, fnType)
                            : IGF.IGM.getSignature(fnType);

  // Emit the cast.
  fnPtr = IGF.Builder.CreateBitCast(fnPtr, sig.getType()->getPointerTo());

  // Return the information.
  return sig;
}

Callee irgen::getBlockPointerCallee(IRGenFunction &IGF,
                                    llvm::Value *blockPtr,
                                    CalleeInfo &&info) {
  // Grab the block pointer and make it the first physical argument.
  llvm::PointerType *blockPtrTy = IGF.IGM.ObjCBlockPtrTy;
  auto castBlockPtr = IGF.Builder.CreateBitCast(blockPtr, blockPtrTy);

  // Extract the invocation pointer for blocks.
  llvm::Value *invokeFnPtrPtr =
      IGF.Builder.CreateStructGEP(IGF.IGM.ObjCBlockStructTy, castBlockPtr, 3);
  Address invokeFnPtrAddr(invokeFnPtrPtr, IGF.IGM.FunctionPtrTy,
                          IGF.IGM.getPointerAlignment());
  llvm::Value *invokeFnPtr = IGF.Builder.CreateLoad(invokeFnPtrAddr);

  auto sig = emitCastOfFunctionPointer(IGF, invokeFnPtr, info.OrigFnType);

  auto &schema = IGF.getOptions().PointerAuth.BlockInvocationFunctionPointers;
  auto authInfo = PointerAuthInfo::emit(IGF, schema,
                                        invokeFnPtrAddr.getAddress(),
                                        info.OrigFnType);

  auto fn = FunctionPointer::createSigned(FunctionPointer::Kind::Function,
                                          invokeFnPtr, authInfo, sig);

  return Callee(std::move(info), fn, blockPtr);
}

Callee irgen::getSwiftFunctionPointerCallee(
    IRGenFunction &IGF, llvm::Value *fnPtr, llvm::Value *dataPtr,
    CalleeInfo &&calleeInfo, bool castOpaqueToRefcountedContext, bool isClosure) {
  auto sig = emitCastOfFunctionPointer(IGF, fnPtr, calleeInfo.OrigFnType);
  auto authInfo =
    PointerAuthInfo::forFunctionPointer(IGF.IGM, calleeInfo.OrigFnType);

  auto fn = isClosure ? FunctionPointer::createSignedClosure(calleeInfo.OrigFnType, fnPtr, authInfo, sig) :
    FunctionPointer::createSigned(calleeInfo.OrigFnType, fnPtr, authInfo, sig,
                                  true);
  if (castOpaqueToRefcountedContext) {
    assert(dataPtr && dataPtr->getType() == IGF.IGM.OpaquePtrTy &&
           "Expecting trivial closure context");
    dataPtr = IGF.Builder.CreateBitCast(dataPtr, IGF.IGM.RefCountedPtrTy);
  }
  return Callee(std::move(calleeInfo), fn, dataPtr);
}

Callee irgen::getCFunctionPointerCallee(IRGenFunction &IGF,
                                        llvm::Value *fnPtr,
                                        CalleeInfo &&calleeInfo) {
  auto sig = emitCastOfFunctionPointer(IGF, fnPtr, calleeInfo.OrigFnType);
  auto authInfo =
      PointerAuthInfo::forFunctionPointer(IGF.IGM, calleeInfo.OrigFnType);

  auto fn = FunctionPointer::createSigned(FunctionPointer::Kind::Function,
                                          fnPtr, authInfo, sig);

  return Callee(std::move(calleeInfo), fn);
}

FunctionPointer FunctionPointer::forDirect(IRGenModule &IGM,
                                           llvm::Constant *fnPtr,
                                           llvm::Constant *secondaryValue,
                                           CanSILFunctionType fnType) {
  return forDirect(fnType, fnPtr, secondaryValue, IGM.getSignature(fnType));
}

StringRef FunctionPointer::getName(IRGenModule &IGM) const {
  assert(isConstant());
  switch (getBasicKind()) {
  case BasicKind::Function:
    return getRawPointer()->getName();
  case BasicKind::AsyncFunctionPointer: {
    auto *asyncFnPtr = getDirectPointer();
    // Handle windows style async function pointers.
    if (auto *ce = dyn_cast<llvm::ConstantExpr>(asyncFnPtr)) {
      if (ce->getOpcode() == llvm::Instruction::IntToPtr) {
        asyncFnPtr = cast<llvm::Constant>(asyncFnPtr->getOperand(0));
      }
    }
    asyncFnPtr = cast<llvm::Constant>(asyncFnPtr->stripPointerCasts());
    return IGM
        .getSILFunctionForAsyncFunctionPointer(asyncFnPtr)->getName();
  }
  }
  llvm_unreachable("unhandled case");
}

llvm::Value *FunctionPointer::getPointer(IRGenFunction &IGF) const {
  switch (getBasicKind()) {
  case BasicKind::Function:
    return Value;
  case BasicKind::AsyncFunctionPointer: {
    if (auto *rawFunction = getRawAsyncFunction()) {
      // If the pointer to the underlying function is available, it means that
      // this FunctionPointer instance was created via 
      // FunctionPointer::forDirect and as such has no AuthInfo.
      assert(!AuthInfo && "have PointerAuthInfo for an async FunctionPointer "
                          "for which the raw function is known");
      return rawFunction;
    }
    auto *fnPtr = Value;
    if (auto authInfo = AuthInfo) {
      fnPtr = emitPointerAuthAuth(IGF, fnPtr, authInfo);
      if (IGF.IGM.getOptions().IndirectAsyncFunctionPointer)
        fnPtr = emitIndirectAsyncFunctionPointer(IGF, fnPtr);
    }
    auto *descriptorPtr =
        IGF.Builder.CreateBitCast(fnPtr, IGF.IGM.AsyncFunctionPointerPtrTy);
    auto *addrPtr = IGF.Builder.CreateStructGEP(IGF.IGM.AsyncFunctionPointerTy,
                                                descriptorPtr, 0);
    auto *result = IGF.emitLoadOfCompactFunctionPointer(
        Address(addrPtr, IGF.IGM.RelativeAddressTy,
                IGF.IGM.getPointerAlignment()),
        /*isFar*/ false,
        /*expectedType*/ getFunctionType());
    if (auto codeAuthInfo = AuthInfo.getCorrespondingCodeAuthInfo()) {
      result = emitPointerAuthSign(IGF, result, codeAuthInfo);
    }
    return result;
  }
  }
  llvm_unreachable("unhandled case");
}

FunctionPointer FunctionPointer::forExplosionValue(IRGenFunction &IGF,
                                                   llvm::Value *fnPtr,
                                                   CanSILFunctionType fnType) {
  // Bitcast out of an opaque pointer type.
  assert(fnPtr->getType() == IGF.IGM.Int8PtrTy);
  auto sig = emitCastOfFunctionPointer(IGF, fnPtr, fnType);
  auto authInfo = PointerAuthInfo::forFunctionPointer(IGF.IGM, fnType);

  return FunctionPointer(fnType, fnPtr, authInfo, sig);
}

llvm::Value *
FunctionPointer::getExplosionValue(IRGenFunction &IGF,
                                   CanSILFunctionType fnType) const {
  llvm::Value *fnPtr = getRawPointer();

  // Re-sign to the appropriate schema for this function pointer type.
  auto resultAuthInfo = PointerAuthInfo::forFunctionPointer(IGF.IGM, fnType);
  if (getAuthInfo() != resultAuthInfo) {
    fnPtr = emitPointerAuthResign(IGF, fnPtr, getAuthInfo(), resultAuthInfo);
  }

  // Bitcast to an opaque pointer type.
  fnPtr = IGF.Builder.CreateBitCast(fnPtr, IGF.IGM.Int8PtrTy);

  return fnPtr;
}

FunctionPointer FunctionPointer::getAsFunction(IRGenFunction &IGF) const {
  switch (getBasicKind()) {
  case FunctionPointer::BasicKind::Function:
    return *this;
  case FunctionPointer::BasicKind::AsyncFunctionPointer: {
    auto authInfo = AuthInfo.getCorrespondingCodeAuthInfo();
    return FunctionPointer(Kind::Function, getPointer(IGF), authInfo, Sig);
  }
  }
  llvm_unreachable("unhandled case");
}

void irgen::emitAsyncReturn(
    IRGenFunction &IGF, AsyncContextLayout &asyncLayout,
    CanSILFunctionType fnType,
    std::optional<ArrayRef<llvm::Value *>> nativeResultArgs) {
  auto contextAddr = asyncLayout.emitCastTo(IGF, IGF.getAsyncContext());
  auto returnToCallerLayout = asyncLayout.getResumeParentLayout();
  auto returnToCallerAddr =
      returnToCallerLayout.project(IGF, contextAddr, std::nullopt);
  Explosion fn;
  cast<LoadableTypeInfo>(returnToCallerLayout.getType())
      .loadAsCopy(IGF, returnToCallerAddr, fn);
  llvm::Value *fnVal = fn.claimNext();

  if (auto schema = IGF.IGM.getOptions().PointerAuth.AsyncContextResume) {
    Address fieldAddr = returnToCallerLayout.project(IGF, contextAddr,
                                                     /*offsets*/ std::nullopt);
    auto authInfo = PointerAuthInfo::emit(IGF, schema, fieldAddr.getAddress(),
                                          PointerAuthEntity());
    fnVal = emitPointerAuthAuth(IGF, fnVal, authInfo);
  }

  auto sig = emitCastOfFunctionPointer(IGF, fnVal, fnType, true);
  auto fnPtr = FunctionPointer::createUnsigned(FunctionPointer::Kind::Function,
                                               fnVal, sig);

  SmallVector<llvm::Value*, 4> Args;
  // Get the current async context.
  Args.push_back(IGF.getAsyncContext());
  if (nativeResultArgs) {
    for (auto nativeResultArg : *nativeResultArgs)
      Args.push_back(nativeResultArg);
  }

  // Setup the coro.end.async intrinsic call.
  auto &Builder = IGF.Builder;
  auto mustTailCallFn = IGF.createAsyncDispatchFn(fnPtr,Args);

  auto handle = IGF.getCoroutineHandle();
  auto rawFnPtr =
      Builder.CreateBitOrPointerCast(fnPtr.getRawPointer(), IGF.IGM.Int8PtrTy);

  SmallVector<llvm::Value*, 8> arguments;
  arguments.push_back(handle);
  arguments.push_back(/*is unwind*/Builder.getFalse());
  arguments.push_back(mustTailCallFn);
  arguments.push_back(rawFnPtr);
  for (auto *arg: Args)
    arguments.push_back(arg);

  Builder.CreateIntrinsicCall(llvm::Intrinsic::coro_end_async, arguments);

  if (IGF.IGM.AsyncTailCallKind == llvm::CallInst::TCK_MustTail) {
    Builder.CreateUnreachable();
  } else {
    // If target doesn't support musttail (e.g. WebAssembly), the function
    // passed to coro.end.async can return control back to the caller.
    // So use ret void instead of unreachable to allow it.
    Builder.CreateRetVoid();
  }
}

void irgen::emitAsyncReturn(IRGenFunction &IGF, AsyncContextLayout &asyncLayout,
                            SILType funcResultTypeInContext,
                            CanSILFunctionType fnType, Explosion &result,
                            Explosion &error) {
  assert((fnType->hasErrorResult() && !error.empty()) ||
         (!fnType->hasErrorResult() && error.empty()));

  auto &IGM = IGF.IGM;

  // Map the explosion to the native result type.
  std::optional<ArrayRef<llvm::Value *>> nativeResults = std::nullopt;
  SmallVector<llvm::Value *, 16> nativeResultsStorage;
  SILFunctionConventions conv(fnType, IGF.getSILModule());
  auto &nativeSchema =
      IGM.getTypeInfo(funcResultTypeInContext).nativeReturnValueSchema(IGM);

  if (fnType->hasErrorResult() && !conv.hasIndirectSILResults() &&
      !conv.hasIndirectSILErrorResults() && !nativeSchema.requiresIndirect() &&
      conv.isTypedError()) {
    auto errorType = conv.getSILErrorType(IGM.getMaximalTypeExpansionContext());
    auto &errorTI = IGM.getTypeInfo(errorType);
    auto &nativeError = errorTI.nativeReturnValueSchema(IGM);
    if (!nativeError.shouldReturnTypedErrorIndirectly()) {
      assert(!error.empty() && "Direct error return must have error value");
      auto *combinedTy =
          combineResultAndTypedErrorType(IGM, nativeSchema, nativeError)
              .combinedTy;

      if (combinedTy->isVoidTy()) {
        assert(result.empty() && "Unexpected result values");
      } else {
        if (auto *structTy = dyn_cast<llvm::StructType>(combinedTy)) {
          llvm::Value *nativeAgg = llvm::UndefValue::get(structTy);
          for (unsigned i = 0, e = result.size(); i < e; ++i) {
            llvm::Value *elt = result.claimNext();
            auto *nativeTy = structTy->getElementType(i);
            elt = convertForDirectError(IGF, elt, nativeTy,
                                        /*forExtraction*/ false);
            nativeAgg = IGF.Builder.CreateInsertValue(nativeAgg, elt, i);
          }
          Explosion out;
          IGF.emitAllExtractValues(nativeAgg, structTy, out);
          while (!out.empty()) {
            nativeResultsStorage.push_back(out.claimNext());
          }
        } else if (!result.empty()) {
          auto *converted = convertForDirectError(
              IGF, result.claimNext(), combinedTy, /*forExtraction*/ false);
          nativeResultsStorage.push_back(converted);
        } else {
          nativeResultsStorage.push_back(llvm::UndefValue::get(combinedTy));
        }
      }

      nativeResultsStorage.push_back(error.claimNext());
      nativeResults = nativeResultsStorage;

      emitAsyncReturn(IGF, asyncLayout, fnType, nativeResults);
      return;
    }
  }

  if (result.empty() && !nativeSchema.empty()) {
    if (!nativeSchema.requiresIndirect())
      // When we throw, we set the return values to undef.
      nativeSchema.enumerateComponents([&](clang::CharUnits begin,
                                           clang::CharUnits end,
                                           llvm::Type *componentTy) {
        nativeResultsStorage.push_back(llvm::UndefValue::get(componentTy));
      });
    if (!error.empty())
      nativeResultsStorage.push_back(error.claimNext());
    nativeResults = nativeResultsStorage;
  } else if (!result.empty()) {
    assert(!nativeSchema.empty());
    assert(!nativeSchema.requiresIndirect());
    Explosion native = nativeSchema.mapIntoNative(
        IGM, IGF, result, funcResultTypeInContext, false /*isOutlined*/);
    while (!native.empty()) {
      nativeResultsStorage.push_back(native.claimNext());
    }
    if (!error.empty())
      nativeResultsStorage.push_back(error.claimNext());
    nativeResults = nativeResultsStorage;
  } else if (!error.empty()) {
    nativeResultsStorage.push_back(error.claimNext());
    nativeResults = nativeResultsStorage;
  }
  emitAsyncReturn(IGF, asyncLayout, fnType, nativeResults);
}

void irgen::emitYieldOnceCoroutineResult(IRGenFunction &IGF, Explosion &result,
                                         SILType funcResultType, SILType returnResultType) {
  auto &Builder = IGF.Builder;
  auto &IGM = IGF.IGM;

  // Prepare coroutine result values
  auto &coroResults = IGF.coroutineResults;
  assert(coroResults.empty() && "must only be single return");
  if (result.empty()) {
    assert(IGM.getTypeInfo(returnResultType)
           .nativeReturnValueSchema(IGM)
           .empty() &&
           "Empty explosion must match the native calling convention");
  } else {
    result = IGF.coerceValueTo(returnResultType, result, funcResultType);
    auto &nativeSchema =
      IGM.getTypeInfo(funcResultType).nativeReturnValueSchema(IGM);
    assert(!nativeSchema.requiresIndirect());

    Explosion native = nativeSchema.mapIntoNative(IGM, IGF, result,
                                                  funcResultType,
                                                  false /* isOutlined */);
    for (unsigned i = 0, e = native.size(); i != e; ++i)
      coroResults.push_back(native.claimNext());
  }

  auto coroEndBB = IGF.getCoroutineExitBlock();
  auto handle = IGF.getCoroutineHandle();
  bool newEndBlock = false;
  if (!coroEndBB) {
    coroEndBB = IGF.createBasicBlock("coro.end");
    IGF.setCoroutineExitBlock(coroEndBB);
    newEndBlock = true;
  }

  // If there are coroutine results, then we need to capture them via
  // @llvm.coro_end_results intrinsics. However, since unwind blocks would
  // jump to the same block, we wrap values into phi nodes.
  Builder.CreateBr(coroEndBB);

  // Emit the end block.
  llvm::BasicBlock *returnBB = Builder.GetInsertBlock();

  if (newEndBlock) {
    Builder.emitBlock(coroEndBB);

    llvm::Value *resultToken = nullptr;
    if (coroResults.empty()) {
      // No results: just use none token
      resultToken = llvm::ConstantTokenNone::get(Builder.getContext());
    } else {
      // Otherwise, wrap result values into singleton phi nodes
      for (auto &val : coroResults) {
        auto *phi = Builder.CreatePHI(val->getType(), 0);
        phi->addIncoming(val, returnBB);
        val = phi;
      }

      // Capture results via result token
      resultToken =
        Builder.CreateIntrinsicCall(llvm::Intrinsic::coro_end_results, coroResults);
    }
    Builder.CreateIntrinsicCall(llvm::Intrinsic::coro_end,
                                {handle,
                                 /*is unwind*/ Builder.getFalse(),
                                 resultToken});
    Builder.CreateUnreachable();
  } else {
    if (coroResults.empty()) {
      // No results, we do not need to change anything around existing coro.end
      return;
    }

    // Otherwise, we'd need to insert new coro.end.results intrinsics capturing
    // result values.  However, we'd need to wrap results into phi nodes adding
    // undef for all values coming from incoming unwind blocks.

    // Find coro.end intrinsic
    llvm::CallInst *coroEndCall = nullptr;
    for (llvm::Instruction &inst : coroEndBB->instructionsWithoutDebug()) {
      if (auto *CI = dyn_cast<llvm::CallInst>(&inst)) {
        if (CI->getIntrinsicID() == llvm::Intrinsic::coro_end) {
          coroEndCall = CI;
          break;
        }
      }
    }

    assert(coroEndCall && isa<llvm::ConstantTokenNone>(coroEndCall->getArgOperand(2)) &&
           "invalid unwind coro.end call");

    Builder.SetInsertPoint(&*coroEndBB->getFirstInsertionPt());

    for (auto &val : coroResults) {
      auto *phi = Builder.CreatePHI(val->getType(), llvm::pred_size(coroEndBB));
      for (auto *predBB : llvm::predecessors(coroEndBB))
        phi->addIncoming(predBB == returnBB ? val : llvm::UndefValue::get(val->getType()),
                         predBB);

      val = phi;
    }

    // Capture results via result token and replace coro.end token operand
    auto *resultToken =
      Builder.CreateIntrinsicCall(llvm::Intrinsic::coro_end_results, coroResults);
    coroEndCall->setArgOperand(2, resultToken);
    Builder.SetInsertPoint(returnBB);
  }
}

FunctionPointer
IRGenFunction::getFunctionPointerForResumeIntrinsic(llvm::Value *resume) {
  auto *fnTy = llvm::FunctionType::get(
      IGM.VoidTy, {IGM.Int8PtrTy},
      false /*vaargs*/);
  auto attrs = IGM.constructInitialAttributes();
  attrs = attrs.addParamAttribute(IGM.getLLVMContext(), 0,
                                  llvm::Attribute::SwiftAsync);
  auto signature =
      Signature(fnTy, attrs, IGM.SwiftAsyncCC);
  auto fnPtr = FunctionPointer::createUnsigned(
      FunctionPointer::Kind::Function,
      Builder.CreateBitOrPointerCast(resume, fnTy->getPointerTo()), signature);
  return fnPtr;
}

Address irgen::emitAutoDiffCreateLinearMapContextWithType(
    IRGenFunction &IGF, llvm::Value *topLevelSubcontextMetatype) {
  topLevelSubcontextMetatype = IGF.Builder.CreateBitCast(
      topLevelSubcontextMetatype, IGF.IGM.TypeMetadataPtrTy);
  auto *call = IGF.Builder.CreateCall(
      IGF.IGM.getAutoDiffCreateLinearMapContextWithTypeFunctionPointer(),
      {topLevelSubcontextMetatype});
  call->setDoesNotThrow();
  call->setCallingConv(IGF.IGM.SwiftCC);
  return Address(call, IGF.IGM.RefCountedStructTy,
                 IGF.IGM.getPointerAlignment());
}

Address irgen::emitAutoDiffProjectTopLevelSubcontext(
    IRGenFunction &IGF, Address context) {
  auto *call = IGF.Builder.CreateCall(
      IGF.IGM.getAutoDiffProjectTopLevelSubcontextFunctionPointer(),
      {context.getAddress()});
  call->setDoesNotThrow();
  call->setCallingConv(IGF.IGM.SwiftCC);
  return Address(call, IGF.IGM.Int8Ty, IGF.IGM.getPointerAlignment());
}

Address irgen::emitAutoDiffAllocateSubcontextWithType(
    IRGenFunction &IGF, Address context, llvm::Value *subcontextMetatype) {
  subcontextMetatype =
      IGF.Builder.CreateBitCast(subcontextMetatype, IGF.IGM.TypeMetadataPtrTy);
  auto *call = IGF.Builder.CreateCall(
      IGF.IGM.getAutoDiffAllocateSubcontextWithTypeFunctionPointer(),
      {context.getAddress(), subcontextMetatype});
  call->setDoesNotThrow();
  call->setCallingConv(IGF.IGM.SwiftCC);
  return Address(call, IGF.IGM.Int8Ty, IGF.IGM.getPointerAlignment());
}

FunctionPointer
irgen::getFunctionPointerForDispatchCall(IRGenModule &IGM,
                                         const FunctionPointer &fn) {
  // Strip off the return type. The original function pointer signature
  // captured both the entry point type and the resume function type.
  auto *fnTy = llvm::FunctionType::get(
      IGM.VoidTy, fn.getSignature().getType()->params(), false /*vaargs*/);
  auto signature =
      Signature(fnTy, fn.getSignature().getAttributes(), IGM.SwiftAsyncCC);
  auto fnPtr = FunctionPointer::createSigned(FunctionPointer::Kind::Function,
                                             fn.getRawPointer(),
                                             fn.getAuthInfo(), signature);
  return fnPtr;
}

void irgen::forwardAsyncCallResult(IRGenFunction &IGF,
                                   CanSILFunctionType fnType,
                                   AsyncContextLayout &layout,
                                   llvm::CallInst *call) {
  auto &IGM = IGF.IGM;
  auto numAsyncContextParams =
      Signature::forAsyncReturn(IGM, fnType).getAsyncContextIndex() + 1;
  llvm::Value *result = call;
  auto *suspendResultTy = cast<llvm::StructType>(result->getType());
  Explosion resultExplosion;
  Explosion errorExplosion;
  auto hasError = fnType->hasErrorResult();
  std::optional<ArrayRef<llvm::Value *>> nativeResults = std::nullopt;
  SmallVector<llvm::Value *, 16> nativeResultsStorage;

  if (suspendResultTy->getNumElements() == numAsyncContextParams) {
    // no result to forward.
    assert(!hasError);
  } else {
    auto &Builder = IGF.Builder;
    auto resultTys =
        llvm::ArrayRef(suspendResultTy->element_begin() + numAsyncContextParams,
                       suspendResultTy->element_end());

    for (unsigned i = 0, e = resultTys.size(); i != e; ++i) {
      llvm::Value *elt =
          Builder.CreateExtractValue(result, numAsyncContextParams + i);
      nativeResultsStorage.push_back(elt);
    }
    nativeResults = nativeResultsStorage;
  }
  emitAsyncReturn(IGF, layout, fnType, nativeResults);
}

llvm::FunctionType *FunctionPointer::getFunctionType() const {
  // Static async function pointers can read the type off the secondary value
  // (the function definition.
  if (SecondaryValue) {
    assert(kind == FunctionPointer::Kind::AsyncFunctionPointer);
    return cast<llvm::Function>(SecondaryValue)->getFunctionType();
  }

  if (awaitSignature) {
    return cast<llvm::FunctionType>(awaitSignature);
  }

  // Read the function type off the global or else from the Signature.
  if (auto *constant = dyn_cast<llvm::Constant>(Value)) {
    auto *gv = dyn_cast<llvm::GlobalValue>(Value);
    if (!gv) {
      return Sig.getType();
    }

    if (useSignature) { // Because of various casting (e.g thin_to_thick) the
                      // signature of the function Value might mismatch
                      // (e.g no context argument).
      return Sig.getType();
    }

    return cast<llvm::FunctionType>(gv->getValueType());
  }

  return Sig.getType();
}

void irgen::buildDirectError(IRGenFunction &IGF,
                             const CombinedResultAndErrorType &combined,
                             const NativeConventionSchema &errorSchema,
                             SILType silErrorTy, Explosion &errorResult,
                             bool forAsync, Explosion &out) {
  if (combined.combinedTy->isVoidTy()) {
    return;
  }

  llvm::Value *expandedResult = llvm::UndefValue::get(combined.combinedTy);
  auto *structTy = dyn_cast<llvm::StructType>(combined.combinedTy);

  if (!errorSchema.getExpandedType(IGF.IGM)->isVoidTy()) {
    auto nativeError =
        errorSchema.mapIntoNative(IGF.IGM, IGF, errorResult, silErrorTy, false);

    if (structTy) {
      for (unsigned i : combined.errorValueMapping) {
        llvm::Value *elt = nativeError.claimNext();
        auto *nativeTy = structTy->getElementType(i);
        elt = convertForDirectError(IGF, elt, nativeTy,
                                    /*forExtraction*/ false);
        expandedResult = IGF.Builder.CreateInsertValue(expandedResult, elt, i);
      }
      if (forAsync) {
        IGF.emitAllExtractValues(expandedResult, structTy, out);
      } else {
        out = expandedResult;
      }
    } else if (!errorSchema.getExpandedType(IGF.IGM)->isVoidTy()) {
      out = convertForDirectError(IGF, nativeError.claimNext(),
                                  combined.combinedTy,
                                  /*forExtraction*/ false);
    }
  } else {
    if (forAsync && structTy) {
      IGF.emitAllExtractValues(expandedResult, structTy, out);
    } else {
      out = expandedResult;
    }
  }
}
