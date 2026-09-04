//===--- GenDirectRuntime.cpp - Direct runtime IR emission ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements emission of direct runtime retain/release functions
//  as LLVM IR. The functions use weak_odr linkage so the linker coalesces
//  duplicates across translation units. Callframe helpers use preserve_most
//  CC so LLVM handles register save/restore and pointer authentication
//  automatically.
//
//===----------------------------------------------------------------------===//

#include "IRGenModule.h"
#include "swift/ABI/System.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/TargetParser/Triple.h"

using namespace swift;
using namespace irgen;

/// Create an IR function that saves preserve_most registers, masks the object
/// pointer, calls the given target function, and returns. This allows LLVM to
/// handle pointer authentication and frame layout automatically, rather than
/// hand-coding PAC instructions in inline assembly.
static llvm::Function *createCallFrameHelper(
    IRGenModule &IGM, StringRef name, StringRef targetName,
    bool returnObject, uint64_t pointerMask) {
  auto &Module = IGM.Module;
  auto &Ctx = Module.getContext();
  auto *ptrTy = llvm::PointerType::getUnqual(Ctx);
  auto *voidTy = llvm::Type::getVoidTy(Ctx);
  auto *i64Ty = llvm::Type::getInt64Ty(Ctx);

  // Helper function signature: ptr(ptr) for retain, void(ptr) for release.
  auto *retTy = returnObject ? static_cast<llvm::Type *>(ptrTy) : voidTy;
  auto *fnTy = llvm::FunctionType::get(retTy, {ptrTy}, false);

  auto *fn = llvm::Function::Create(
      fnTy, llvm::GlobalValue::WeakODRLinkage, name, &Module);
  fn->setVisibility(llvm::GlobalValue::HiddenVisibility);
  fn->setCallingConv(llvm::CallingConv::PreserveMost);
  fn->setAttributes(IGM.constructInitialAttributes());
  fn->addFnAttr(llvm::Attribute::NoInline);

  auto *entry = llvm::BasicBlock::Create(Ctx, "", fn);
  llvm::IRBuilder<> builder(entry);

  auto *obj = fn->getArg(0);

  // Mask the object pointer to clear non-pointer bits (e.g., ObjC/tagged bits
  // from bridge objects).
  auto *objInt = builder.CreatePtrToInt(obj, i64Ty);
  auto *maskedInt = builder.CreateAnd(
      objInt, llvm::ConstantInt::get(i64Ty, pointerMask));
  auto *cleanPtr = builder.CreateIntToPtr(maskedInt, ptrTy);

  // Declare and call the target function (C calling convention).
  auto *targetRetTy =
      returnObject ? static_cast<llvm::Type *>(ptrTy) : voidTy;
  auto *targetTy = llvm::FunctionType::get(targetRetTy, {ptrTy}, false);
  auto targetCallee = Module.getOrInsertFunction(targetName, targetTy);
  builder.CreateCall(targetCallee, {cleanPtr});

  // For retain: return the original (unmasked) pointer.
  // For release: return void.
  if (returnObject)
    builder.CreateRet(obj);
  else
    builder.CreateRetVoid();

  return fn;
}

/// Get or create a function with the given properties. If the function already
/// exists as a declaration, update its linkage and attributes. Otherwise create
/// a new function.
static llvm::Function *getOrCreateFunction(
    llvm::Module &Module, StringRef name, llvm::FunctionType *fnTy,
    llvm::GlobalValue::LinkageTypes linkage,
    llvm::GlobalValue::VisibilityTypes visibility,
    llvm::CallingConv::ID cc) {
  llvm::Function *fn = Module.getFunction(name);
  if (fn) {
    assert(fn->empty() && "Function already has a body");
    fn->setLinkage(linkage);
  } else {
    fn = llvm::Function::Create(fnTy, linkage, name, &Module);
  }
  fn->setVisibility(visibility);
  fn->setCallingConv(cc);
  fn->addFnAttr(llvm::Attribute::NoUnwind);
  fn->addFnAttr(llvm::Attribute::NoInline);
  return fn;
}

/// Create a musttail call followed by the appropriate return instruction.
static void createTailCallAndRet(
    llvm::IRBuilder<> &B, llvm::FunctionType *fnTy, llvm::Value *callee,
    llvm::Value *arg, llvm::CallingConv::ID cc, bool hasReturnValue) {
  auto *call = B.CreateCall(fnTy, callee, {arg});
  call->setCallingConv(cc);
  call->setTailCallKind(llvm::CallInst::TCK_Tail);
  if (hasReturnValue)
    B.CreateRet(call);
  else
    B.CreateRetVoid();
}

/// Create a separate noinline function for the retain/release slow path.
/// Moving the slow path into its own function allows the main retain/release
/// function to remain frameless: it tail-calls this function on the slow path,
/// and this function handles calling the runtime.
static llvm::Function *createSlowpathFunction(
    IRGenModule &IGM,
    StringRef name,
    StringRef preservemostName,
    StringRef callframeName,
    llvm::FunctionType *fnTy,
    bool targetHasPreservemost,
    bool hasReturnValue) {
  auto &Module = IGM.Module;
  auto &Ctx = Module.getContext();
  auto *ptrTy = llvm::PointerType::getUnqual(Ctx);
  auto cc = llvm::CallingConv::PreserveMost;

  auto *fn = llvm::Function::Create(
      fnTy, llvm::GlobalValue::PrivateLinkage, name, &Module);
  fn->setCallingConv(cc);
  fn->addFnAttr(llvm::Attribute::NoUnwind);
  fn->addFnAttr(llvm::Attribute::NoInline);

  auto *obj = fn->getArg(0);
  llvm::IRBuilder<> B(Ctx);

  if (targetHasPreservemost) {
    // The deployment target guarantees the preservemost symbol exists.
    // Declare it as strong external so LLVM can tail-call it directly.
    auto *entry = llvm::BasicBlock::Create(Ctx, "entry", fn);
    B.SetInsertPoint(entry);
    auto callee = Module.getOrInsertFunction(preservemostName, fnTy);
    if (auto *pmDecl = llvm::dyn_cast<llvm::Function>(callee.getCallee()))
      pmDecl->setCallingConv(cc);
    createTailCallAndRet(B, fnTy, callee.getCallee(), obj, cc,
                         hasReturnValue);
  } else {
    // Declare the weak preservemost symbol if not already declared.
    // LLVM won't tail-call an extern_weak on AArch64 (ELF/MachO), so the
    // preservemost path uses a regular call while the callframe path
    // uses a tail call. LLVM's shrink-wrapping pushes the frame setup
    // into only the preservemost path.
    auto *pmFn = Module.getFunction(preservemostName);
    if (!pmFn) {
      pmFn = llvm::Function::Create(fnTy,
          llvm::GlobalValue::ExternalWeakLinkage,
          preservemostName, &Module);
      pmFn->setCallingConv(cc);
    }

    auto *entry = llvm::BasicBlock::Create(Ctx, "entry", fn);
    B.SetInsertPoint(entry);

    // Check if the weak symbol is null at runtime.
    auto *isNull = B.CreateICmpEQ(pmFn,
        llvm::ConstantPointerNull::get(ptrTy));

    auto *callPM = llvm::BasicBlock::Create(Ctx, "call_preservemost", fn);
    auto *callCF = llvm::BasicBlock::Create(Ctx, "call_callframe", fn);
    B.CreateCondBr(isNull, callCF, callPM);

    // Call the preservemost entrypoint (not a tail call due to extern_weak).
    B.SetInsertPoint(callPM);
    createTailCallAndRet(B, fnTy, pmFn, obj, cc, hasReturnValue);

    // Fall back to the callframe helper (tail call).
    B.SetInsertPoint(callCF);
    auto *cfFn = Module.getFunction(callframeName);
    createTailCallAndRet(B, fnTy, cfFn, obj, cc, hasReturnValue);
  }

  return fn;
}

/// Emit swift_releaseDirect as LLVM IR.
///
/// Fast path: atomically decrement the strong refcount via cmpxchg.
/// Slow path: tail-call into the runtime (preservemost or callframe helper).
static void emitSwiftReleaseDirect(
    IRGenModule &IGM,
    llvm::GlobalVariable *slowpathMask,
    uint64_t strongRCOne,
    bool targetHasPreservemost) {
  auto &Module = IGM.Module;
  auto &Ctx = Module.getContext();
  auto *ptrTy = llvm::PointerType::getUnqual(Ctx);
  auto *i64Ty = llvm::Type::getInt64Ty(Ctx);
  auto *voidTy = llvm::Type::getVoidTy(Ctx);
  auto *i8Ty = llvm::Type::getInt8Ty(Ctx);

  auto *fnTy = llvm::FunctionType::get(voidTy, {ptrTy}, false);
  auto *fn = getOrCreateFunction(Module, "swift_releaseDirect", fnTy,
      llvm::GlobalValue::WeakODRLinkage,
      llvm::GlobalValue::HiddenVisibility,
      llvm::CallingConv::PreserveMost);

  auto *obj = fn->getArg(0);
  llvm::IRBuilder<> B(Ctx);

  auto *entry = llvm::BasicBlock::Create(Ctx, "entry", fn);
  auto *work = llvm::BasicBlock::Create(Ctx, "work", fn);
  auto *done = llvm::BasicBlock::Create(Ctx, "done", fn);
  auto *loop = llvm::BasicBlock::Create(Ctx, "loop", fn);
  auto *fastpath = llvm::BasicBlock::Create(Ctx, "fastpath", fn);
  auto *slowpath = llvm::BasicBlock::Create(Ctx, "slowpath", fn);

  // entry: null/negative check. Retain/release of NULL or values with the
  // high bit set is a no-op.
  B.SetInsertPoint(entry);
  auto *objInt = B.CreatePtrToInt(obj, i64Ty);
  auto *isNonPositive = B.CreateICmpSLE(objInt,
      llvm::ConstantInt::get(i64Ty, 0));
  B.CreateCondBr(isNonPositive, done, work);

  // work: compute refcount field pointer (object + 8), do initial load.
  B.SetInsertPoint(work);
  auto *refcountPtr = B.CreateGEP(i8Ty, obj,
      llvm::ConstantInt::get(i64Ty, 8));
  auto *initialLoad = B.CreateLoad(i64Ty, refcountPtr);
  B.CreateBr(loop);

  // loop: check whether the slow path is needed. The slow path is taken if
  // any bits in the slowpath mask are set in the refcount, or if the strong
  // refcount is zero (which triggers deallocation).
  B.SetInsertPoint(loop);
  auto *old = B.CreatePHI(i64Ty, 2, "old");
  auto *mask = B.CreateLoad(i64Ty, slowpathMask, "mask");
  auto *slowbits = B.CreateAnd(old, mask);
  auto *hasSlowbits = B.CreateICmpNE(slowbits,
      llvm::ConstantInt::get(i64Ty, 0));
  auto *rcOne = llvm::ConstantInt::get(i64Ty, strongRCOne);
  auto *tooSmall = B.CreateICmpSLT(old, rcOne);
  auto *needSlow = B.CreateOr(hasSlowbits, tooSmall);
  B.CreateCondBr(needSlow, slowpath, fastpath);

  // fastpath: decrement the refcount via atomic compare-and-swap with release
  // ordering, so that dealloc on another thread sees all prior stores.
  B.SetInsertPoint(fastpath);
  auto *newVal = B.CreateSub(old, rcOne);
  auto *result = B.CreateAtomicCmpXchg(
      refcountPtr, old, newVal,
      llvm::Align(8),
      llvm::AtomicOrdering::Release,
      llvm::AtomicOrdering::Monotonic);
  auto *loaded = B.CreateExtractValue(result, 0, "loaded");
  auto *success = B.CreateExtractValue(result, 1, "success");
  B.CreateCondBr(success, done, loop);

  old->addIncoming(initialLoad, work);
  old->addIncoming(loaded, fastpath);

  // done: return.
  B.SetInsertPoint(done);
  B.CreateRetVoid();

  // slowpath: tail-call into a separate slowpath function. Using a separate
  // function keeps the main function frameless (no register saves).
  B.SetInsertPoint(slowpath);
  auto *slowFn = createSlowpathFunction(IGM,
      "swift_releaseDirect.slowpath",
      "swift_release_preservemost", "swift_release_callframe",
      fnTy, targetHasPreservemost, /*hasReturnValue=*/false);
  createTailCallAndRet(B, fnTy, slowFn, obj,
      llvm::CallingConv::PreserveMost, /*hasReturnValue=*/false);
}

/// Emit swift_bridgeObjectReleaseDirect as LLVM IR.
///
/// Checks for tagged pointers (bit 63) and ObjC objects (bit 62), then masks
/// the pointer and tail-calls swift_releaseDirect.
static void emitSwiftBridgeObjectReleaseDirect(
    IRGenModule &IGM, bool objcInterop,
    uint64_t bridgeObjectPointerBits) {
  auto &Module = IGM.Module;
  auto &Ctx = Module.getContext();
  auto *ptrTy = llvm::PointerType::getUnqual(Ctx);
  auto *i64Ty = llvm::Type::getInt64Ty(Ctx);
  auto *voidTy = llvm::Type::getVoidTy(Ctx);

  auto *fnTy = llvm::FunctionType::get(voidTy, {ptrTy}, false);
  auto *fn = getOrCreateFunction(Module, "swift_bridgeObjectReleaseDirect",
      fnTy, llvm::GlobalValue::WeakODRLinkage,
      llvm::GlobalValue::HiddenVisibility,
      llvm::CallingConv::PreserveMost);

  auto *obj = fn->getArg(0);
  llvm::IRBuilder<> B(Ctx);

  auto *entry = llvm::BasicBlock::Create(Ctx, "entry", fn);
  B.SetInsertPoint(entry);
  auto *objInt = B.CreatePtrToInt(obj, i64Ty);

  if (objcInterop) {
    auto *notTagged = llvm::BasicBlock::Create(Ctx, "not_tagged", fn);
    auto *taggedRet = llvm::BasicBlock::Create(Ctx, "tagged_ret", fn);
    auto *callRelease = llvm::BasicBlock::Create(Ctx, "call_release", fn);
    auto *objcRelease = llvm::BasicBlock::Create(Ctx, "objc_release", fn);

    // Check bit 63: tagged pointer means no-op.
    auto *bit63 = B.CreateAnd(objInt,
        llvm::ConstantInt::get(i64Ty, 1ULL << 63));
    auto *isTagged = B.CreateICmpNE(bit63,
        llvm::ConstantInt::get(i64Ty, 0));
    B.CreateCondBr(isTagged, taggedRet, notTagged);

    B.SetInsertPoint(taggedRet);
    B.CreateRetVoid();

    // Check bit 62: ObjC object goes to objc_release callframe.
    B.SetInsertPoint(notTagged);
    auto *bit62 = B.CreateAnd(objInt,
        llvm::ConstantInt::get(i64Ty, 1ULL << 62));
    auto *isObjC = B.CreateICmpNE(bit62,
        llvm::ConstantInt::get(i64Ty, 0));
    B.CreateCondBr(isObjC, objcRelease, callRelease);

    B.SetInsertPoint(objcRelease);
    auto *cfFn = Module.getFunction("swift_objc_release_callframe");
    createTailCallAndRet(B, fnTy, cfFn, obj,
        llvm::CallingConv::PreserveMost, /*hasReturnValue=*/false);

    // Mask pointer bits and tail-call swift_releaseDirect.
    B.SetInsertPoint(callRelease);
    auto *maskedInt = B.CreateAnd(objInt,
        llvm::ConstantInt::get(i64Ty, bridgeObjectPointerBits));
    auto *maskedPtr = B.CreateIntToPtr(maskedInt, ptrTy);
    auto *releaseFn = Module.getFunction("swift_releaseDirect");
    createTailCallAndRet(B, fnTy, releaseFn, maskedPtr,
        llvm::CallingConv::PreserveMost, /*hasReturnValue=*/false);
  } else {
    // Without ObjC interop, just mask and release. No tagged pointer check
    // needed; swift_releaseDirect's null/negative check handles high-bit values.
    auto *maskedInt = B.CreateAnd(objInt,
        llvm::ConstantInt::get(i64Ty, bridgeObjectPointerBits));
    auto *maskedPtr = B.CreateIntToPtr(maskedInt, ptrTy);
    auto *releaseFn = Module.getFunction("swift_releaseDirect");
    createTailCallAndRet(B, fnTy, releaseFn, maskedPtr,
        llvm::CallingConv::PreserveMost, /*hasReturnValue=*/false);
  }
}

/// Emit swift_retainDirect as LLVM IR.
///
/// Fast path: atomically increment the strong refcount via cmpxchg.
/// Slow path: tail-call into the runtime.
/// Returns the original (potentially unmasked) object pointer.
static void emitSwiftRetainDirect(
    IRGenModule &IGM,
    llvm::GlobalVariable *slowpathMask,
    uint64_t strongRCOne,
    uint64_t bridgeObjectPointerBits,
    bool targetHasPreservemost) {
  auto &Module = IGM.Module;
  auto &Ctx = Module.getContext();
  auto *ptrTy = llvm::PointerType::getUnqual(Ctx);
  auto *i64Ty = llvm::Type::getInt64Ty(Ctx);
  auto *i8Ty = llvm::Type::getInt8Ty(Ctx);

  auto *fnTy = llvm::FunctionType::get(ptrTy, {ptrTy}, false);
  auto *fn = getOrCreateFunction(Module, "swift_retainDirect", fnTy,
      llvm::GlobalValue::WeakODRLinkage,
      llvm::GlobalValue::HiddenVisibility,
      llvm::CallingConv::PreserveMost);

  auto *obj = fn->getArg(0);
  llvm::IRBuilder<> B(Ctx);

  auto *entry = llvm::BasicBlock::Create(Ctx, "entry", fn);
  auto *work = llvm::BasicBlock::Create(Ctx, "work", fn);
  auto *done = llvm::BasicBlock::Create(Ctx, "done", fn);
  auto *loop = llvm::BasicBlock::Create(Ctx, "loop", fn);
  auto *fastpath = llvm::BasicBlock::Create(Ctx, "fastpath", fn);
  auto *slowpath = llvm::BasicBlock::Create(Ctx, "slowpath", fn);

  // entry: null/negative check.
  B.SetInsertPoint(entry);
  auto *objInt = B.CreatePtrToInt(obj, i64Ty);
  auto *isNonPositive = B.CreateICmpSLE(objInt,
      llvm::ConstantInt::get(i64Ty, 0));
  B.CreateCondBr(isNonPositive, done, work);

  // work: mask pointer to get clean HeapObject*, compute refcount field
  // pointer (object + 8), do initial load. The mask handles spare bits from
  // bridge objects; for a plain HeapObject* the mask is a no-op.
  B.SetInsertPoint(work);
  auto *maskedInt = B.CreateAnd(objInt,
      llvm::ConstantInt::get(i64Ty, bridgeObjectPointerBits));
  auto *cleanPtr = B.CreateIntToPtr(maskedInt, ptrTy);
  auto *refcountPtr = B.CreateGEP(i8Ty, cleanPtr,
      llvm::ConstantInt::get(i64Ty, 8));
  auto *initialLoad = B.CreateLoad(i64Ty, refcountPtr);
  B.CreateBr(loop);

  // loop: check whether the slow path is needed. Test the INCREMENTED refcount
  // against the slowpath mask, which catches both the side-table case and
  // overflow (overflow sets the high bit).
  B.SetInsertPoint(loop);
  auto *old = B.CreatePHI(i64Ty, 2, "old");
  auto *rcOne = llvm::ConstantInt::get(i64Ty, strongRCOne);
  auto *newVal = B.CreateAdd(old, rcOne, "new");
  auto *mask = B.CreateLoad(i64Ty, slowpathMask, "mask");
  auto *slowbits = B.CreateAnd(newVal, mask);
  auto *needSlow = B.CreateICmpNE(slowbits,
      llvm::ConstantInt::get(i64Ty, 0));
  B.CreateCondBr(needSlow, slowpath, fastpath);

  // fastpath: atomic compare-and-swap with relaxed ordering (retain doesn't
  // need release semantics).
  B.SetInsertPoint(fastpath);
  auto *result = B.CreateAtomicCmpXchg(
      refcountPtr, old, newVal,
      llvm::Align(8),
      llvm::AtomicOrdering::Monotonic,
      llvm::AtomicOrdering::Monotonic);
  auto *loaded = B.CreateExtractValue(result, 0, "loaded");
  auto *success = B.CreateExtractValue(result, 1, "success");
  B.CreateCondBr(success, done, loop);

  old->addIncoming(initialLoad, work);
  old->addIncoming(loaded, fastpath);

  // done: return the original (potentially unmasked) object pointer.
  B.SetInsertPoint(done);
  B.CreateRet(obj);

  // slowpath: tail-call into a separate slowpath function.
  B.SetInsertPoint(slowpath);
  auto *slowFn = createSlowpathFunction(IGM,
      "swift_retainDirect.slowpath",
      "swift_retain_preservemost", "swift_retain_callframe",
      fnTy, targetHasPreservemost, /*hasReturnValue=*/true);
  createTailCallAndRet(B, fnTy, slowFn, obj,
      llvm::CallingConv::PreserveMost, /*hasReturnValue=*/true);
}

/// Emit swift_bridgeObjectRetainDirect as LLVM IR.
///
/// Checks for tagged pointers (bit 63) and ObjC objects (bit 62), then
/// tail-calls swift_retainDirect (which handles pointer masking internally).
static void emitSwiftBridgeObjectRetainDirect(
    IRGenModule &IGM, bool objcInterop) {
  auto &Module = IGM.Module;
  auto &Ctx = Module.getContext();
  auto *ptrTy = llvm::PointerType::getUnqual(Ctx);
  auto *i64Ty = llvm::Type::getInt64Ty(Ctx);

  auto *fnTy = llvm::FunctionType::get(ptrTy, {ptrTy}, false);
  auto *fn = getOrCreateFunction(Module, "swift_bridgeObjectRetainDirect",
      fnTy, llvm::GlobalValue::WeakODRLinkage,
      llvm::GlobalValue::HiddenVisibility,
      llvm::CallingConv::PreserveMost);

  auto *obj = fn->getArg(0);
  llvm::IRBuilder<> B(Ctx);

  auto *entry = llvm::BasicBlock::Create(Ctx, "entry", fn);
  B.SetInsertPoint(entry);

  if (objcInterop) {
    auto *notTagged = llvm::BasicBlock::Create(Ctx, "not_tagged", fn);
    auto *taggedRet = llvm::BasicBlock::Create(Ctx, "tagged_ret", fn);
    auto *callRetain = llvm::BasicBlock::Create(Ctx, "call_retain", fn);
    auto *objcRetain = llvm::BasicBlock::Create(Ctx, "objc_retain", fn);

    auto *objInt = B.CreatePtrToInt(obj, i64Ty);

    // Check bit 63: tagged pointer means return as-is.
    auto *bit63 = B.CreateAnd(objInt,
        llvm::ConstantInt::get(i64Ty, 1ULL << 63));
    auto *isTagged = B.CreateICmpNE(bit63,
        llvm::ConstantInt::get(i64Ty, 0));
    B.CreateCondBr(isTagged, taggedRet, notTagged);

    B.SetInsertPoint(taggedRet);
    B.CreateRet(obj);

    // Check bit 62: ObjC object goes to objc_retain callframe.
    B.SetInsertPoint(notTagged);
    auto *bit62 = B.CreateAnd(objInt,
        llvm::ConstantInt::get(i64Ty, 1ULL << 62));
    auto *isObjC = B.CreateICmpNE(bit62,
        llvm::ConstantInt::get(i64Ty, 0));
    B.CreateCondBr(isObjC, objcRetain, callRetain);

    B.SetInsertPoint(objcRetain);
    auto *cfFn = Module.getFunction("swift_objc_retain_callframe");
    createTailCallAndRet(B, fnTy, cfFn, obj,
        llvm::CallingConv::PreserveMost, /*hasReturnValue=*/true);

    // Tail-call swift_retainDirect with the original bridgeObject value.
    // retainDirect handles pointer masking internally.
    B.SetInsertPoint(callRetain);
    auto *retainFn = Module.getFunction("swift_retainDirect");
    createTailCallAndRet(B, fnTy, retainFn, obj,
        llvm::CallingConv::PreserveMost, /*hasReturnValue=*/true);
  } else {
    // Without ObjC interop, swift_retainDirect handles everything: the
    // null/negative check catches tagged pointers (high bit set), and the
    // pointer mask clears spare bits.
    auto *retainFn = Module.getFunction("swift_retainDirect");
    createTailCallAndRet(B, fnTy, retainFn, obj,
        llvm::CallingConv::PreserveMost, /*hasReturnValue=*/true);
  }
}

/// Emit direct retain/release functions as LLVM IR for ARM64.
static void emitDirectRetainReleaseARM64(IRGenModule &IGM) {
  auto &Module = IGM.Module;
  auto &Ctx = Module.getContext();

  // Check whether any direct retain/release functions are referenced.
  bool needRetain =
      Module.getNamedValue("swift_retainDirect") != nullptr ||
      Module.getNamedValue("swift_bridgeObjectRetainDirect") != nullptr;
  bool needRelease =
      Module.getNamedValue("swift_releaseDirect") != nullptr ||
      Module.getNamedValue("swift_bridgeObjectReleaseDirect") != nullptr;

  if (!needRetain && !needRelease)
    return;

  // Determine target configuration.
  bool objcInterop = IGM.ObjCInterop;

  // If the deployment target guarantees the preservemost entrypoints exist,
  // skip the fallback. Exclude macOS from this in order to support tools that
  // build with a new deployment target but are run on older OS versions.
  bool targetHasPreservemostRetainRelease =
      !IGM.Triple.isMacOSX() &&
      IGM.getAvailabilityRange().isContainedIn(
          IGM.Context.getPreservemostRetainReleaseAvailability());

  // ABI constants.
  // The strong refcount field is at bit offset 33 within the refcount word:
  // PureSwiftDealloc (1) + UnownedRefCount (31) + IsDeiniting (1) = 33.
  const uint64_t strongRCOne = 1ULL << 33;
  const uint64_t bridgeObjectPointerBits =
      ~(uint64_t)SWIFT_ABI_ARM64_SWIFT_SPARE_BITS_MASK;

  // Set up __retainRelease_slowpath_mask. This mask indicates when we must call
  // into the runtime slowpath. If the object's refcount field has any bits set
  // that are in the mask, then we must take the slow path. The variable is
  // placed in a special section so the runtime can locate and override it. It
  // is derived from the address of _swift_retainRelease_slowpath_mask_v1. The
  // addend is set such that it has the correct value for older runtimes that
  // don't have that symbol.
  auto *slowpathMaskExtern = Module.getOrInsertGlobal(
      "_swift_retainRelease_slowpath_mask_v1",
      llvm::Type::getInt64Ty(Ctx));
  if (auto *GV = llvm::dyn_cast<llvm::GlobalVariable>(slowpathMaskExtern)) {
    GV->setLinkage(llvm::GlobalValue::ExternalWeakLinkage);
  }

  const char *maskName = "__retainRelease_slowpath_mask";
  auto *maskTy = llvm::Type::getInt64Ty(Ctx);
  auto *maskGV = new llvm::GlobalVariable(
      Module, maskTy, /*isConstant=*/false,
      llvm::GlobalValue::WeakODRLinkage,
      /*Initializer=*/llvm::ConstantExpr::getAdd(
          llvm::ConstantExpr::getPtrToInt(slowpathMaskExtern, maskTy),
          llvm::ConstantInt::get(maskTy, 0x8000000000000000ULL)),
      maskName);
  maskGV->setAlignment(llvm::Align(8));
  maskGV->setSection("__DATA,__swift5_rr_mask");
  maskGV->setVisibility(llvm::GlobalValue::HiddenVisibility);
  IGM.addUsedGlobal(maskGV);

  // Create IR helper functions for the slowpath call frames. These are
  // LLVM IR functions with preserve_most CC and the standard function
  // attributes, so LLVM handles pointer authentication and register
  // save/restore automatically.
  if (!targetHasPreservemostRetainRelease) {
    if (needRetain)
      createCallFrameHelper(IGM, "swift_retain_callframe", "swift_retain",
                            /*returnObject=*/true, bridgeObjectPointerBits);
    if (needRelease)
      createCallFrameHelper(IGM, "swift_release_callframe", "swift_release",
                            /*returnObject=*/false, bridgeObjectPointerBits);
  }
  if (objcInterop) {
    if (needRetain)
      createCallFrameHelper(IGM, "swift_objc_retain_callframe", "objc_retain",
                            /*returnObject=*/true, bridgeObjectPointerBits);
    if (needRelease)
      createCallFrameHelper(IGM, "swift_objc_release_callframe",
                            "objc_release",
                            /*returnObject=*/false, bridgeObjectPointerBits);
  }

  // Emit the direct retain/release functions as LLVM IR.
  if (needRelease) {
    emitSwiftReleaseDirect(IGM, maskGV, strongRCOne,
                           targetHasPreservemostRetainRelease);
    emitSwiftBridgeObjectReleaseDirect(IGM, objcInterop,
                                       bridgeObjectPointerBits);
  }
  if (needRetain) {
    emitSwiftRetainDirect(IGM, maskGV, strongRCOne, bridgeObjectPointerBits,
                          targetHasPreservemostRetainRelease);
    emitSwiftBridgeObjectRetainDirect(IGM, objcInterop);
  }
}

void IRGenModule::emitDirectRuntimeAsm() {
  if (!TargetInfo.HasSwiftSwiftDirectRuntimeLibrary ||
      !getOptions().EnableSwiftDirectRetainRelease)
    return;

  if (Triple.getArch() == llvm::Triple::aarch64 && Triple.isOSDarwin())
    emitDirectRetainReleaseARM64(*this);
}
