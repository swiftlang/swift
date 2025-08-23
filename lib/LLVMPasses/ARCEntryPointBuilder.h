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

#include "swift/Basic/LLVM.h"
#include "swift/Basic/NullablePtr.h"
#include "swift/Runtime/Config.h"
#include "swift/Runtime/RuntimeFnWrappersGen.h"
#include "llvm/ADT/APInt.h"
#include "llvm/TargetParser/Triple.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"

namespace swift {

namespace RuntimeConstants {
  const auto ReadNone = llvm::Attribute::ReadNone;
  const auto ReadOnly = llvm::Attribute::ReadOnly;
  const auto NoReturn = llvm::Attribute::NoReturn;
  const auto NoUnwind = llvm::Attribute::NoUnwind;
  const auto ZExt = llvm::Attribute::ZExt;
  const auto FirstParamReturned = llvm::Attribute::Returned;
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
  using AttributeList = llvm::AttributeList;
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
  NullablePtr<Constant> UnknownObjectRetainN;
  NullablePtr<Constant> UnknownObjectReleaseN;
  NullablePtr<Constant> BridgeRetainN;
  NullablePtr<Constant> BridgeReleaseN;

  // The type cache.
  llvm::PointerType *PtrTy;

  llvm::CallingConv::ID DefaultCC;

  llvm::CallInst *CreateCall(Constant *Fn, Value *V) {
    CallInst *CI = B.CreateCall(
        cast<llvm::FunctionType>(cast<llvm::Function>(Fn)->getValueType()), Fn,
        V);
    if (auto Fun = llvm::dyn_cast<llvm::Function>(Fn))
      CI->setCallingConv(Fun->getCallingConv());
    return CI;
  }

  llvm::CallInst *CreateCall(Constant *Fn, llvm::ArrayRef<Value *> Args) {
    CallInst *CI = B.CreateCall(
        cast<llvm::FunctionType>(cast<llvm::Function>(Fn)->getValueType()), Fn,
        Args);
    if (auto Fun = llvm::dyn_cast<llvm::Function>(Fn))
      CI->setCallingConv(Fun->getCallingConv());
    return CI;
  }

public:
  ARCEntryPointBuilder(Function &F)
      : B(&*F.begin()), Retain(),
        PtrTy(llvm::PointerType::getUnqual(B.getContext())),
        DefaultCC(SWIFT_DEFAULT_LLVM_CC) {}

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
    V = B.CreatePointerCast(V, PtrTy);

    // Create the call.
    CallInst *CI = CreateCall(getRetain(OrigI), V);
    return CI;
  }

  CallInst *createRelease(Value *V, CallInst *OrigI) {
    // Cast just to make sure that we have the right type.
    V = B.CreatePointerCast(V, PtrTy);

    // Create the call.
    CallInst *CI = CreateCall(getRelease(OrigI), V);
    return CI;
  }

  
  CallInst *createCheckUnowned(Value *V, CallInst *OrigI) {
    // Cast just to make sure that we have the right type.
    V = B.CreatePointerCast(V, PtrTy);

    CallInst *CI = CreateCall(getCheckUnowned(OrigI), V);
    return CI;
  }

  CallInst *createRetainN(Value *V, uint32_t n, CallInst *OrigI) {
    // Cast just to make sure that we have the right object type.
    V = B.CreatePointerCast(V, PtrTy);
    CallInst *CI = CreateCall(getRetainN(OrigI), {V, getIntConstant(n)});
    return CI;
  }

  CallInst *createReleaseN(Value *V, uint32_t n, CallInst *OrigI) {
    // Cast just to make sure we have the right object type.
    V = B.CreatePointerCast(V, PtrTy);
    CallInst *CI = CreateCall(getReleaseN(OrigI), {V, getIntConstant(n)});
    return CI;
  }

  CallInst *createUnknownObjectRetainN(Value *V, uint32_t n, CallInst *OrigI) {
    // Cast just to make sure that we have the right object type.
    V = B.CreatePointerCast(V, PtrTy);
    CallInst *CI =
        CreateCall(getUnknownObjectRetainN(OrigI), {V, getIntConstant(n)});
    return CI;
  }

  CallInst *createUnknownObjectReleaseN(Value *V, uint32_t n, CallInst *OrigI) {
    // Cast just to make sure we have the right object type.
    V = B.CreatePointerCast(V, PtrTy);
    CallInst *CI =
        CreateCall(getUnknownObjectReleaseN(OrigI), {V, getIntConstant(n)});
    return CI;
  }

  CallInst *createBridgeRetainN(Value *V, uint32_t n, CallInst *OrigI) {
    // Cast just to make sure we have the right object type.
    V = B.CreatePointerCast(V, PtrTy);
    CallInst *CI = CreateCall(getBridgeRetainN(OrigI), {V, getIntConstant(n)});
    return CI;
  }

  CallInst *createBridgeReleaseN(Value *V, uint32_t n, CallInst *OrigI) {
    // Cast just to make sure we have the right object type.
    V = B.CreatePointerCast(V, PtrTy);
    CallInst *CI = CreateCall(getBridgeReleaseN(OrigI), {V, getIntConstant(n)});
    return CI;
  }

  bool isNonAtomic(CallInst *I) {
    // If we have an intrinsic, we know it must be an objc intrinsic. All objc
    // intrinsics are atomic today.
    if (I->getIntrinsicID() != llvm::Intrinsic::not_intrinsic)
      return false;
    return (I->getCalledFunction()->getName().contains("nonatomic"));
  }

  bool isAtomic(CallInst *I) {
    return !isNonAtomic(I);
  }

  /// Perform a pointer cast of pointer value \p V to \p Ty if \p V has a
  /// different type than \p Ty. If \p V equals \p Ty, just return V.
  llvm::Value *maybeCast(llvm::Value *V, llvm::Type *Ty) {
    if (V->getType() == Ty)
      return V;
    return B.CreatePointerCast(V, Ty);
  }

private:
  Module &getModule() {
    return *B.GetInsertBlock()->getModule();
  }

  /// getRetain - Return a callable function for swift_retain.
  Constant *getRetain(CallInst *OrigI) {
    if (Retain)
      return Retain.get();

    llvm::Constant *cache = nullptr;
    Retain = getRuntimeFn(getModule(), cache, "Swift",
                          isNonAtomic(OrigI) ? "swift_nonatomic_retain"
                                             : "swift_retain",
                          DefaultCC, RuntimeAvailability::AlwaysAvailable,
                          {PtrTy}, {PtrTy}, {NoUnwind, FirstParamReturned}, {});

    return Retain.get();
  }

  /// getRelease - Return a callable function for swift_release.
  Constant *getRelease(CallInst *OrigI) {
    if (Release)
      return Release.get();

    auto *VoidTy = Type::getVoidTy(getModule().getContext());

    llvm::Constant *cache = nullptr;
    Release = getRuntimeFn(getModule(), cache, "Swift",
                           isNonAtomic(OrigI) ? "swift_nonatomic_release"
                                              : "swift_release",
                           DefaultCC, RuntimeAvailability::AlwaysAvailable,
                           {VoidTy}, {PtrTy}, {NoUnwind}, {});

    return Release.get();
  }

  Constant *getCheckUnowned(CallInst *OrigI) {
    if (CheckUnowned)
      return CheckUnowned.get();

    auto *ObjectPtrTy = PtrTy;
    auto &M = getModule();
    auto AttrList = AttributeList::get(M.getContext(), 1, Attribute::NoCapture);
    AttrList = AttrList.addFnAttribute(M.getContext(), Attribute::NoUnwind);
    CheckUnowned = cast<llvm::Function>(
        M.getOrInsertFunction("swift_checkUnowned", AttrList,
                              Type::getVoidTy(M.getContext()), ObjectPtrTy)
            .getCallee());
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

    auto *Int32Ty = Type::getInt32Ty(getModule().getContext());

    llvm::Constant *cache = nullptr;
    RetainN = getRuntimeFn(
        getModule(), cache, "Swift",
        isNonAtomic(OrigI) ? "swift_nonatomic_retain_n" : "swift_retain_n",
        DefaultCC, RuntimeAvailability::AlwaysAvailable, {PtrTy},
        {PtrTy, Int32Ty}, {NoUnwind, FirstParamReturned}, {});

    return RetainN.get();
  }

  /// Return a callable function for swift_release_n.
  Constant *getReleaseN(CallInst *OrigI) {
    if (ReleaseN)
      return ReleaseN.get();

    auto *Int32Ty = Type::getInt32Ty(getModule().getContext());
    auto *VoidTy = Type::getVoidTy(getModule().getContext());

    llvm::Constant *cache = nullptr;
    ReleaseN = getRuntimeFn(getModule(), cache, "Swift",
                            isNonAtomic(OrigI) ? "swift_nonatomic_release_n"
                                               : "swift_release_n",
                            DefaultCC, RuntimeAvailability::AlwaysAvailable,
                            {VoidTy}, {PtrTy, Int32Ty}, {NoUnwind}, {});

    return ReleaseN.get();
  }

  /// getUnknownObjectRetainN - Return a callable function for
  /// swift_unknownObjectRetain_n.
  Constant *getUnknownObjectRetainN(CallInst *OrigI) {
    if (UnknownObjectRetainN)
      return UnknownObjectRetainN.get();

    auto *Int32Ty = Type::getInt32Ty(getModule().getContext());

    llvm::Constant *cache = nullptr;
    UnknownObjectRetainN = getRuntimeFn(
        getModule(), cache, "Swift",
        isNonAtomic(OrigI) ? "swift_nonatomic_unknownObjectRetain_n"
                           : "swift_unknownObjectRetain_n",
        DefaultCC, RuntimeAvailability::AlwaysAvailable, {PtrTy},
        {PtrTy, Int32Ty}, {NoUnwind, FirstParamReturned}, {});

    return UnknownObjectRetainN.get();
  }

  /// Return a callable function for swift_unknownObjectRelease_n.
  Constant *getUnknownObjectReleaseN(CallInst *OrigI) {
    if (UnknownObjectReleaseN)
      return UnknownObjectReleaseN.get();

    auto *Int32Ty = Type::getInt32Ty(getModule().getContext());
    auto *VoidTy = Type::getVoidTy(getModule().getContext());

    llvm::Constant *cache = nullptr;
    UnknownObjectReleaseN = getRuntimeFn(
        getModule(), cache, "Swift",
        isNonAtomic(OrigI) ? "swift_nonatomic_unknownObjectRelease_n"
                           : "swift_unknownObjectRelease_n",
        DefaultCC, RuntimeAvailability::AlwaysAvailable, {VoidTy},
        {PtrTy, Int32Ty}, {NoUnwind}, {});

    return UnknownObjectReleaseN.get();
  }

  /// Return a callable function for swift_bridgeRetain_n.
  Constant *getBridgeRetainN(CallInst *OrigI) {
    if (BridgeRetainN)
      return BridgeRetainN.get();

    auto *Int32Ty = Type::getInt32Ty(getModule().getContext());

    llvm::Constant *cache = nullptr;
    BridgeRetainN =
        getRuntimeFn(getModule(), cache, "Swift",
                     isNonAtomic(OrigI) ? "swift_nonatomic_bridgeObjectRetain_n"
                                        : "swift_bridgeObjectRetain_n",
                     DefaultCC, RuntimeAvailability::AlwaysAvailable, {PtrTy},
                     {PtrTy, Int32Ty}, {NoUnwind}, {});
    return BridgeRetainN.get();
  }

  /// Return a callable function for swift_bridgeRelease_n.
  Constant *getBridgeReleaseN(CallInst *OrigI) {
    if (BridgeReleaseN)
      return BridgeReleaseN.get();

    auto *Int32Ty = Type::getInt32Ty(getModule().getContext());
    auto *VoidTy = Type::getVoidTy(getModule().getContext());

    llvm::Constant *cache = nullptr;
    BridgeReleaseN = getRuntimeFn(
        getModule(), cache, "Swift",
        isNonAtomic(OrigI) ? "swift_nonatomic_bridgeObjectRelease_n"
                           : "swift_bridgeObjectRelease_n",
        DefaultCC, RuntimeAvailability::AlwaysAvailable, {VoidTy},
        {PtrTy, Int32Ty}, {NoUnwind}, {});
    return BridgeReleaseN.get();
  }

  Constant *getIntConstant(uint32_t constant) {
    auto &M = getModule();
    auto *Int32Ty = Type::getInt32Ty(M.getContext());
    return Constant::getIntegerValue(Int32Ty, APInt(32, constant));
  }
};

} // end swift namespace

#endif

