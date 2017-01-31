//===--- ARCEntryPointBuilder.h ---------------------------------*- C++ -*-===//
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

#ifndef SWIFT_LLVMPASSES_ARCENTRYPOINTBUILDER_H
#define SWIFT_LLVMPASSES_ARCENTRYPOINTBUILDER_H

#include "swift/Basic/NullablePtr.h"
#include "swift/Runtime/Config.h"
#include "swift/Runtime/RuntimeFnWrappersGen.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/Triple.h"

namespace swift {

namespace RuntimeConstants {
  const auto ReadNone = llvm::Attribute::ReadNone;
  const auto ReadOnly = llvm::Attribute::ReadOnly;
  const auto NoReturn = llvm::Attribute::NoReturn;
  const auto NoUnwind = llvm::Attribute::NoUnwind;
  const auto ZExt = llvm::Attribute::ZExt;
}

using namespace RuntimeConstants;

/// A class for building ARC entry points. It is a composition wrapper around an
/// IRBuilder and a constant Cache. It cannot be moved or copied. It is meant
/// to be created once and passed around by reference.
class ARCEntryPointBuilder {
  using IRBuilder = llvm::IRBuilder<>;
  using Constant = llvm::Constant;
  using Type = llvm::Type;
  using Function = llvm::Function;
  using Instruction = llvm::Instruction;
  using CallInst = llvm::CallInst;
  using Value = llvm::Value;
  using Module = llvm::Module;
  using AttributeSet = llvm::AttributeSet;
  using Attribute = llvm::Attribute;
  using APInt = llvm::APInt;
  
  // The builder which we are wrapping.
  IRBuilder B;

  // The constant cache.
  NullablePtr<Constant> Retain;
  NullablePtr<Constant> Release;
  NullablePtr<Constant> CheckUnowned;
  NullablePtr<Constant> RetainN;
  NullablePtr<Constant> ReleaseN;
  NullablePtr<Constant> UnknownRetainN;
  NullablePtr<Constant> UnknownReleaseN;
  NullablePtr<Constant> BridgeRetainN;
  NullablePtr<Constant> BridgeReleaseN;

  // The type cache.
  NullablePtr<Type> ObjectPtrTy;
  NullablePtr<Type> BridgeObjectPtrTy;

  llvm::CallingConv::ID DefaultCC;
  llvm::CallingConv::ID RegisterPreservingCC;

  llvm::CallInst *CreateCall(Constant *Fn, Value *V) {
    CallInst *CI = B.CreateCall(Fn, V);
    if (auto Fun = llvm::dyn_cast<llvm::Function>(Fn))
      CI->setCallingConv(Fun->getCallingConv());
    return CI;
  }

  llvm::CallInst *CreateCall(Constant *Fn, llvm::ArrayRef<Value *> Args) {
    CallInst *CI = B.CreateCall(Fn, Args);
    if (auto Fun = llvm::dyn_cast<llvm::Function>(Fn))
      CI->setCallingConv(Fun->getCallingConv());
    return CI;
  }

public:
  ARCEntryPointBuilder(Function &F)
      : B(&*F.begin()), Retain(), ObjectPtrTy(),
        DefaultCC(SWIFT_LLVM_CC(DefaultCC)) {
    //If the target does not support the new calling convention,
    //set RegisterPreservingCC to use a default calling convention.
    RegisterPreservingCC = DefaultCC;

    // Check if the register preserving calling convention
    // is supported by the backend and should be used
    // for the deployment target provided for this compilation.
    if (SWIFT_RT_USE_RegisterPreservingCC) {
      bool ShouldUseRegisterPreservingCC = false;
      auto &TargetTriple = F.getParent()->getTargetTriple();
      llvm::Triple Triple(TargetTriple);
      auto Arch = Triple.getArch();
      if (Arch == llvm::Triple::ArchType::aarch64) {
        ShouldUseRegisterPreservingCC = true;
      }

      if (ShouldUseRegisterPreservingCC)
        RegisterPreservingCC = SWIFT_LLVM_CC(RegisterPreservingCC);
    }
  }

  ~ARCEntryPointBuilder() = default;
  ARCEntryPointBuilder(ARCEntryPointBuilder &&) = delete;
  ARCEntryPointBuilder(const ARCEntryPointBuilder &) = delete;

  ARCEntryPointBuilder &operator=(const ARCEntryPointBuilder &) = delete;
  void operator=(ARCEntryPointBuilder &&C) = delete;

  void setInsertPoint(Instruction *I) {
    B.SetInsertPoint(I);
  }

  Value *createInsertValue(Value *V1, Value *V2, unsigned Idx) {
    return B.CreateInsertValue(V1, V2, Idx);
  }

  Value *createExtractValue(Value *V, unsigned Idx) {
    return B.CreateExtractValue(V, Idx);
  }

  Value *createIntToPtr(Value *V, Type *Ty) {
    return B.CreateIntToPtr(V, Ty);
  }

  CallInst *createRetain(Value *V, CallInst *OrigI) {
    // Cast just to make sure that we have the right type.
    V = B.CreatePointerCast(V, getObjectPtrTy());

    // Create the call.
    CallInst *CI = CreateCall(getRetain(OrigI), V);
    CI->setTailCall(true);
    return CI;
  }

  CallInst *createRelease(Value *V, CallInst *OrigI) {
    // Cast just to make sure that we have the right type.
    V = B.CreatePointerCast(V, getObjectPtrTy());

    // Create the call.
    CallInst *CI = CreateCall(getRelease(OrigI), V);
    CI->setTailCall(true);
    return CI;
  }

  
  CallInst *createCheckUnowned(Value *V, CallInst *OrigI) {
    // Cast just to make sure that we have the right type.
    V = B.CreatePointerCast(V, getObjectPtrTy());
    
    CallInst *CI = CreateCall(getCheckUnowned(OrigI), V);
    CI->setTailCall(true);
    return CI;
  }

  CallInst *createRetainN(Value *V, uint32_t n, CallInst *OrigI) {
    // Cast just to make sure that we have the right object type.
    V = B.CreatePointerCast(V, getObjectPtrTy());
    CallInst *CI = CreateCall(getRetainN(OrigI), {V, getIntConstant(n)});
    CI->setTailCall(true);
    return CI;
  }

  CallInst *createReleaseN(Value *V, uint32_t n, CallInst *OrigI) {
    // Cast just to make sure we have the right object type.
    V = B.CreatePointerCast(V, getObjectPtrTy());
    CallInst *CI = CreateCall(getReleaseN(OrigI), {V, getIntConstant(n)});
    CI->setTailCall(true);
    return CI;
  }

  CallInst *createUnknownRetainN(Value *V, uint32_t n, CallInst *OrigI) {
    // Cast just to make sure that we have the right object type.
    V = B.CreatePointerCast(V, getObjectPtrTy());
    CallInst *CI = CreateCall(getUnknownRetainN(OrigI), {V, getIntConstant(n)});
    CI->setTailCall(true);
    return CI;
  }

  CallInst *createUnknownReleaseN(Value *V, uint32_t n, CallInst *OrigI) {
    // Cast just to make sure we have the right object type.
    V = B.CreatePointerCast(V, getObjectPtrTy());
    CallInst *CI = CreateCall(getUnknownReleaseN(OrigI),
                              {V, getIntConstant(n)});
    CI->setTailCall(true);
    return CI;
  }

  CallInst *createBridgeRetainN(Value *V, uint32_t n, CallInst *OrigI) {
    // Cast just to make sure we have the right object type.
    V = B.CreatePointerCast(V, getBridgeObjectPtrTy());
    CallInst *CI = CreateCall(getBridgeRetainN(OrigI), {V, getIntConstant(n)});
    CI->setTailCall(true);
    return CI;
  }

  CallInst *createBridgeReleaseN(Value *V, uint32_t n, CallInst *OrigI) {
    // Cast just to make sure we have the right object type.
    V = B.CreatePointerCast(V, getBridgeObjectPtrTy());
    CallInst *CI = CreateCall(getBridgeReleaseN(OrigI), {V, getIntConstant(n)});
    CI->setTailCall(true);
    return CI;
  }

  bool isNonAtomic(CallInst *I) {
    return (I->getCalledFunction()->getName().find("nonatomic") !=
            llvm::StringRef::npos);
  }

  bool isAtomic(CallInst *I) {
    return !isNonAtomic(I);
  }
private:
  Module &getModule() {
    return *B.GetInsertBlock()->getModule();
  }

  /// getRetain - Return a callable function for swift_retain.
  Constant *getRetain(CallInst *OrigI) {
    if (Retain)
      return Retain.get();
    auto *ObjectPtrTy = getObjectPtrTy();
    auto *VoidTy = Type::getVoidTy(getModule().getContext());

    llvm::Constant *cache = nullptr;
    Retain = getWrapperFn(
        getModule(), cache,
        isNonAtomic(OrigI) ? "swift_nonatomic_retain" : "swift_retain",
        isNonAtomic(OrigI) ? SWIFT_RT_ENTRY_REF_AS_STR(swift_nonatomic_retain)
                           : SWIFT_RT_ENTRY_REF_AS_STR(swift_retain),
        RegisterPreservingCC, {VoidTy}, {ObjectPtrTy}, {NoUnwind});

    return Retain.get();
  }

  /// getRelease - Return a callable function for swift_release.
  Constant *getRelease(CallInst *OrigI) {
    if (Release)
      return Release.get();
    auto *ObjectPtrTy = getObjectPtrTy();
    auto *VoidTy = Type::getVoidTy(getModule().getContext());

    llvm::Constant *cache = nullptr;
    Release = getWrapperFn(
        getModule(), cache,
        isNonAtomic(OrigI) ? "swift_nonatomic_release" : "swift_release",
        isNonAtomic(OrigI) ? SWIFT_RT_ENTRY_REF_AS_STR(swift_nonatomic_release)
                           : SWIFT_RT_ENTRY_REF_AS_STR(swift_release),
        RegisterPreservingCC, {VoidTy}, {ObjectPtrTy}, {NoUnwind});

    return Release.get();
  }

  Constant *getCheckUnowned(CallInst *OrigI) {
    if (CheckUnowned)
      return CheckUnowned.get();
    
    auto *ObjectPtrTy = getObjectPtrTy();
    auto &M = getModule();
    auto AttrList = AttributeSet::get(M.getContext(), 1, Attribute::NoCapture);
    AttrList = AttrList.addAttribute(
        M.getContext(), AttributeSet::FunctionIndex, Attribute::NoUnwind);
    CheckUnowned = M.getOrInsertFunction("swift_checkUnowned", AttrList,
                                         Type::getVoidTy(M.getContext()),
                                         ObjectPtrTy, nullptr);
    if (llvm::Triple(M.getTargetTriple()).isOSBinFormatCOFF() &&
        !llvm::Triple(M.getTargetTriple()).isOSCygMing())
      if (auto *F = llvm::dyn_cast<llvm::Function>(CheckUnowned.get()))
        F->setDLLStorageClass(llvm::GlobalValue::DLLImportStorageClass);
    return CheckUnowned.get();
  }

  /// getRetainN - Return a callable function for swift_retain_n.
  Constant *getRetainN(CallInst *OrigI) {
    if (RetainN)
      return RetainN.get();
    auto *ObjectPtrTy = getObjectPtrTy();
    auto *Int32Ty = Type::getInt32Ty(getModule().getContext());
    auto *VoidTy = Type::getVoidTy(getModule().getContext());

    llvm::Constant *cache = nullptr;
    RetainN = getWrapperFn(
        getModule(), cache,
        isNonAtomic(OrigI) ? "swift_nonatomic_retain_n" : "swift_retain_n",
        isNonAtomic(OrigI) ? SWIFT_RT_ENTRY_REF_AS_STR(swift_nonatomic_retain_n)
                           : SWIFT_RT_ENTRY_REF_AS_STR(swift_retain_n),
        RegisterPreservingCC, {VoidTy}, {ObjectPtrTy, Int32Ty}, {NoUnwind});

    return RetainN.get();
  }

  /// Return a callable function for swift_release_n.
  Constant *getReleaseN(CallInst *OrigI) {
    if (ReleaseN)
      return ReleaseN.get();
    auto *ObjectPtrTy = getObjectPtrTy();
    auto *Int32Ty = Type::getInt32Ty(getModule().getContext());
    auto *VoidTy = Type::getVoidTy(getModule().getContext());

    llvm::Constant *cache = nullptr;
    ReleaseN = getWrapperFn(
        getModule(), cache,
        isNonAtomic(OrigI) ? "swift_nonatomic_release_n" : "swift_release_n",
        isNonAtomic(OrigI)
            ? SWIFT_RT_ENTRY_REF_AS_STR(swift_nonatomic_release_n)
            : SWIFT_RT_ENTRY_REF_AS_STR(swift_release_n),
        RegisterPreservingCC, {VoidTy}, {ObjectPtrTy, Int32Ty}, {NoUnwind});

    return ReleaseN.get();
  }

  /// getUnknownRetainN - Return a callable function for swift_unknownRetain_n.
  Constant *getUnknownRetainN(CallInst *OrigI) {
    if (UnknownRetainN)
      return UnknownRetainN.get();
    auto *ObjectPtrTy = getObjectPtrTy();
    auto *Int32Ty = Type::getInt32Ty(getModule().getContext());
    auto *VoidTy = Type::getVoidTy(getModule().getContext());

    llvm::Constant *cache = nullptr;
    UnknownRetainN =
        getRuntimeFn(getModule(), cache,
                     isNonAtomic(OrigI) ? "swift_nonatomic_unknownRetain_n"
                                        : "swift_unknownRetain_n",
                     DefaultCC, {VoidTy}, {ObjectPtrTy, Int32Ty}, {NoUnwind});

    return UnknownRetainN.get();
  }

  /// Return a callable function for swift_unknownRelease_n.
  Constant *getUnknownReleaseN(CallInst *OrigI) {
    if (UnknownReleaseN)
      return UnknownReleaseN.get();
    auto *ObjectPtrTy = getObjectPtrTy();
    auto *Int32Ty = Type::getInt32Ty(getModule().getContext());
    auto *VoidTy = Type::getVoidTy(getModule().getContext());

    llvm::Constant *cache = nullptr;
    UnknownReleaseN =
        getRuntimeFn(getModule(), cache,
                     isNonAtomic(OrigI) ? "swift_nonatomic_unknownRelease_n"
                                        : "swift_unknownRelease_n",
                     DefaultCC, {VoidTy}, {ObjectPtrTy, Int32Ty}, {NoUnwind});

    return UnknownReleaseN.get();
  }

  /// Return a callable function for swift_bridgeRetain_n.
  Constant *getBridgeRetainN(CallInst *OrigI) {
    if (BridgeRetainN)
      return BridgeRetainN.get();
    auto *BridgeObjectPtrTy = getBridgeObjectPtrTy();
    auto *Int32Ty = Type::getInt32Ty(getModule().getContext());

    llvm::Constant *cache = nullptr;
    BridgeRetainN =
        getRuntimeFn(getModule(), cache,
                     isNonAtomic(OrigI) ? "swift_nonatomic_bridgeObjectRetain_n"
                                        : "swift_bridgeObjectRetain_n",
                     DefaultCC, {BridgeObjectPtrTy},
                     {BridgeObjectPtrTy, Int32Ty}, {NoUnwind});
    return BridgeRetainN.get();
  }

  /// Return a callable function for swift_bridgeRelease_n.
  Constant *getBridgeReleaseN(CallInst *OrigI) {
    if (BridgeReleaseN)
      return BridgeReleaseN.get();

    auto *BridgeObjectPtrTy = getBridgeObjectPtrTy();
    auto *Int32Ty = Type::getInt32Ty(getModule().getContext());
    auto *VoidTy = Type::getVoidTy(getModule().getContext());

    llvm::Constant *cache = nullptr;
    BridgeReleaseN = getRuntimeFn(
        getModule(), cache,
        isNonAtomic(OrigI) ? "swift_nonatomic_bridgeObjectRelease_n"
                           : "swift_bridgeObjectRelease_n",
        DefaultCC, {VoidTy}, {BridgeObjectPtrTy, Int32Ty}, {NoUnwind});
    return BridgeReleaseN.get();
  }

  Type *getObjectPtrTy() {
    if (ObjectPtrTy)
      return ObjectPtrTy.get();
    auto &M = getModule();
    ObjectPtrTy = M.getTypeByName("swift.refcounted")->getPointerTo();
    assert(ObjectPtrTy && "Could not find the swift heap object type by name");
    return ObjectPtrTy.get();
  }

  Type *getBridgeObjectPtrTy() {
    if (BridgeObjectPtrTy)
      return BridgeObjectPtrTy.get();
    auto &M = getModule();
    BridgeObjectPtrTy = M.getTypeByName("swift.bridge")->getPointerTo();
    assert(BridgeObjectPtrTy &&
           "Could not find the swift bridge object type by name");
    return BridgeObjectPtrTy.get();
  }

  Constant *getIntConstant(uint32_t constant) {
    auto &M = getModule();
    auto *Int32Ty = Type::getInt32Ty(M.getContext());
    return Constant::getIntegerValue(Int32Ty, APInt(32, constant));
  }
};

} // end swift namespace

#endif

