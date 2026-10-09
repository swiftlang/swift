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
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/Module.h"
#include "llvm/TargetParser/Triple.h"

using namespace swift;
using namespace irgen;

/// The registers for which `_xN` variants of the direct retain/release
/// entrypoints are emitted. When the object pointer already lives in one of
/// these registers, the call site can branch to the matching variant instead of
/// first moving the value into x0.
///
/// x0 is the base entrypoint, so it is not in this list. x16 and x17 are
/// clobbered by call stubs, x18 is reserved by the platform, and x29 and x30 are
/// the frame pointer and link register.
static const char *const DirectRRVariantRegisters[] = {
    "x1",  "x2",  "x3",  "x4",  "x5",  "x6",  "x7",  "x8",  "x9",
    "x10", "x11", "x12", "x13", "x14", "x15", "x19", "x20", "x21",
    "x22", "x23", "x24", "x25", "x26", "x27", "x28",
};

/// Build the symbol name for a direct retain/release entrypoint. The base
/// entrypoint (object in x0) uses the bare name; a variant appends the register
/// its object arrives in, e.g. "swift_retainDirect_x21".
static std::string directRRName(StringRef base, StringRef objectRegister) {
  if (objectRegister.empty())
    return base.str();
  return (base + "_" + objectRegister).str();
}

/// The signature of a direct retain/release entrypoint. The base entrypoint
/// takes the object as an ordinary argument. A variant takes no IR argument at
/// all: its object arrives in a fixed physical register, which is a private
/// contract between the call site and the body that the IR type system does not
/// express.
static llvm::FunctionType *directRRFunctionType(llvm::LLVMContext &Ctx,
                                                bool hasReturnValue,
                                                StringRef objectRegister) {
  auto *ptrTy = llvm::PointerType::getUnqual(Ctx);
  auto *retTy = hasReturnValue ? static_cast<llvm::Type *>(ptrTy)
                               : llvm::Type::getVoidTy(Ctx);
  if (objectRegister.empty())
    return llvm::FunctionType::get(retTy, {ptrTy}, false);
  return llvm::FunctionType::get(retTy, {}, false);
}

/// Produce the incoming object pointer. For the base entrypoint that is just
/// the IR argument. For an `_xN` variant the object arrives in a fixed physical
/// register, which is read with an inline-asm `mov` out of that register.
///
/// The read is spelled as literal asm text with a virtual `=r` output rather
/// than as an empty asm with an `={xN}` output constraint bound to the physical
/// register. An `={xN}` output makes LLVM treat xN as defined by the asm, and
/// since preserve_most makes x9-x15 and x19-x28 callee-saved, LLVM then
/// save/restores xN around the body -- a stack frame in a function whose whole
/// purpose is to stay frameless. Naming the register only inside the asm text
/// leaves xN untouched as far as LLVM is concerned, so no register is preserved
/// and no frame is built.
///
/// The call is marked as having side effects so it cannot be sunk or deleted,
/// and it is emitted first in the entry block so nothing can allocate xN as a
/// temporary before it is read. Prologue register *saves* would preserve xN, so
/// a frame would not itself break the read; the requirement is only that the
/// read precede any use of xN as an allocatable temporary.
static llvm::Value *emitDirectRRObject(llvm::IRBuilder<> &B,
                                       llvm::Function *fn,
                                       StringRef objectRegister) {
  if (objectRegister.empty())
    return fn->getArg(0);

  auto &Ctx = fn->getContext();
  auto *i64Ty = llvm::Type::getInt64Ty(Ctx);
  auto *ptrTy = llvm::PointerType::getUnqual(Ctx);

  auto *asmTy = llvm::FunctionType::get(i64Ty, {}, false);
  auto *readReg = llvm::InlineAsm::get(asmTy,
      /*asmString=*/("mov $0, " + objectRegister).str(),
      /*constraints=*/"=r",
      /*hasSideEffects=*/true);
  auto *objInt = B.CreateCall(readReg);
  return B.CreateIntToPtr(objInt, ptrTy);
}

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

/// Create a tail call followed by the appropriate return instruction.
///
/// By default the call is emitted as a `musttail` call, which guarantees a
/// frameless tail call in the backend. The forwarding calls here all satisfy
/// musttail's requirements (matching prototype and calling convention, with the
/// call in tail position), so this leaves no stack frame in the caller. The
/// exception is a tail call to an extern_weak symbol, which LLVM cannot lower as
/// a tail call on AArch64; such call sites must pass `mustTail=false` and are
/// emitted as an ordinary (frame-establishing) call instead.
static void createTailCallAndRet(
    llvm::IRBuilder<> &B, llvm::FunctionType *fnTy, llvm::Value *callee,
    llvm::Value *arg, llvm::CallingConv::ID cc, bool hasReturnValue,
    bool mustTail = true) {
  auto *call = B.CreateCall(fnTy, callee, {arg});
  call->setCallingConv(cc);
  call->setTailCallKind(mustTail ? llvm::CallInst::TCK_MustTail
                                 : llvm::CallInst::TCK_Tail);
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
    createTailCallAndRet(B, fnTy, pmFn, obj, cc, hasReturnValue,
                         /*mustTail=*/false);

    // Fall back to the callframe helper (tail call).
    B.SetInsertPoint(callCF);
    auto *cfFn = Module.getFunction(callframeName);
    createTailCallAndRet(B, fnTy, cfFn, obj, cc, hasReturnValue);
  }

  return fn;
}

/// Emit swift_releaseDirect, or one of its `_xN` register variants, as LLVM IR.
///
/// Fast path: atomically decrement the strong refcount via cmpxchg.
/// Slow path: tail-call the shared slowpath function.
///
/// \param objectRegister empty for the base entrypoint, which takes the object
/// as an IR argument; otherwise the register the object arrives in.
static void emitSwiftReleaseDirect(
    IRGenModule &IGM,
    llvm::GlobalVariable *slowpathMask,
    uint64_t strongRCOne,
    llvm::Function *slowFn,
    StringRef objectRegister) {
  auto &Module = IGM.Module;
  auto &Ctx = Module.getContext();
  auto *i64Ty = llvm::Type::getInt64Ty(Ctx);
  auto *i8Ty = llvm::Type::getInt8Ty(Ctx);

  auto *fnTy = directRRFunctionType(Ctx, /*hasReturnValue=*/false,
                                    objectRegister);
  auto *fn = getOrCreateFunction(Module,
      directRRName("swift_releaseDirect", objectRegister), fnTy,
      llvm::GlobalValue::WeakODRLinkage,
      llvm::GlobalValue::HiddenVisibility,
      llvm::CallingConv::PreserveMost);

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
  auto *obj = emitDirectRRObject(B, fn, objectRegister);
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

  // slowpath: tail-call the shared slowpath function. Using a separate
  // function keeps the main function frameless (no register saves).
  //
  // The slowpath always takes the object as an ordinary argument, so for an
  // `_xN` variant the callee's prototype differs from this function's. musttail
  // requires matching prototypes, so variants use an ordinary tail call, which
  // the backend still lowers to a frameless branch.
  B.SetInsertPoint(slowpath);
  auto *slowFnTy = directRRFunctionType(Ctx, /*hasReturnValue=*/false,
                                        /*objectRegister=*/"");
  createTailCallAndRet(B, slowFnTy, slowFn, obj,
      llvm::CallingConv::PreserveMost, /*hasReturnValue=*/false,
      /*mustTail=*/objectRegister.empty());
}

/// Emit swift_bridgeObjectReleaseDirect, or one of its `_xN` register variants,
/// as LLVM IR.
///
/// Checks for tagged pointers (bit 63) and ObjC objects (bit 62), then masks
/// the pointer and tail-calls swift_releaseDirect.
///
/// A variant forwards to the *base* swift_releaseDirect, passing the object as
/// an ordinary x0 argument. Release also has to mask the pointer here (unlike
/// retain, which masks internally), and the masked value could not be left in xN
/// anyway.
///
/// \param objectRegister empty for the base entrypoint, which takes the object
/// as an IR argument; otherwise the register the object arrives in.
static void emitSwiftBridgeObjectReleaseDirect(
    IRGenModule &IGM, bool objcInterop,
    uint64_t bridgeObjectPointerBits,
    StringRef objectRegister) {
  auto &Module = IGM.Module;
  auto &Ctx = Module.getContext();
  auto *ptrTy = llvm::PointerType::getUnqual(Ctx);
  auto *i64Ty = llvm::Type::getInt64Ty(Ctx);

  auto *fnTy = directRRFunctionType(Ctx, /*hasReturnValue=*/false,
                                    objectRegister);
  auto *fn = getOrCreateFunction(Module,
      directRRName("swift_bridgeObjectReleaseDirect", objectRegister),
      fnTy, llvm::GlobalValue::WeakODRLinkage,
      llvm::GlobalValue::HiddenVisibility,
      llvm::CallingConv::PreserveMost);

  // The callee always takes the object as an ordinary argument.
  auto *targetTy = directRRFunctionType(Ctx, /*hasReturnValue=*/false,
                                        /*objectRegister=*/"");

  llvm::IRBuilder<> B(Ctx);

  auto *entry = llvm::BasicBlock::Create(Ctx, "entry", fn);
  B.SetInsertPoint(entry);
  auto *obj = emitDirectRRObject(B, fn, objectRegister);
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
    createTailCallAndRet(B, targetTy, cfFn, obj,
        llvm::CallingConv::PreserveMost, /*hasReturnValue=*/false,
        /*mustTail=*/objectRegister.empty());

    // Mask pointer bits and tail-call swift_releaseDirect.
    B.SetInsertPoint(callRelease);
    auto *maskedInt = B.CreateAnd(objInt,
        llvm::ConstantInt::get(i64Ty, bridgeObjectPointerBits));
    auto *maskedPtr = B.CreateIntToPtr(maskedInt, ptrTy);
    auto *releaseFn = Module.getFunction("swift_releaseDirect");
    createTailCallAndRet(B, targetTy, releaseFn, maskedPtr,
        llvm::CallingConv::PreserveMost, /*hasReturnValue=*/false,
        /*mustTail=*/objectRegister.empty());
  } else {
    // Without ObjC interop, just mask and release. No tagged pointer check
    // needed; swift_releaseDirect's null/negative check handles high-bit values.
    auto *maskedInt = B.CreateAnd(objInt,
        llvm::ConstantInt::get(i64Ty, bridgeObjectPointerBits));
    auto *maskedPtr = B.CreateIntToPtr(maskedInt, ptrTy);
    auto *releaseFn = Module.getFunction("swift_releaseDirect");
    createTailCallAndRet(B, targetTy, releaseFn, maskedPtr,
        llvm::CallingConv::PreserveMost, /*hasReturnValue=*/false,
        /*mustTail=*/objectRegister.empty());
  }
}

/// Emit swift_retainDirect, or one of its `_xN` register variants, as LLVM IR.
///
/// Fast path: atomically increment the strong refcount via cmpxchg.
/// Slow path: tail-call the shared slowpath function.
/// Returns the original (potentially unmasked) object pointer in x0.
///
/// \param objectRegister empty for the base entrypoint, which takes the object
/// as an IR argument; otherwise the register the object arrives in.
static void emitSwiftRetainDirect(
    IRGenModule &IGM,
    llvm::GlobalVariable *slowpathMask,
    uint64_t strongRCOne,
    uint64_t bridgeObjectPointerBits,
    llvm::Function *slowFn,
    StringRef objectRegister) {
  auto &Module = IGM.Module;
  auto &Ctx = Module.getContext();
  auto *ptrTy = llvm::PointerType::getUnqual(Ctx);
  auto *i64Ty = llvm::Type::getInt64Ty(Ctx);
  auto *i8Ty = llvm::Type::getInt8Ty(Ctx);

  auto *fnTy = directRRFunctionType(Ctx, /*hasReturnValue=*/true,
                                    objectRegister);
  auto *fn = getOrCreateFunction(Module,
      directRRName("swift_retainDirect", objectRegister), fnTy,
      llvm::GlobalValue::WeakODRLinkage,
      llvm::GlobalValue::HiddenVisibility,
      llvm::CallingConv::PreserveMost);

  llvm::IRBuilder<> B(Ctx);

  auto *entry = llvm::BasicBlock::Create(Ctx, "entry", fn);
  auto *work = llvm::BasicBlock::Create(Ctx, "work", fn);
  auto *done = llvm::BasicBlock::Create(Ctx, "done", fn);
  auto *loop = llvm::BasicBlock::Create(Ctx, "loop", fn);
  auto *fastpath = llvm::BasicBlock::Create(Ctx, "fastpath", fn);
  auto *slowpath = llvm::BasicBlock::Create(Ctx, "slowpath", fn);

  // entry: null/negative check.
  B.SetInsertPoint(entry);
  auto *obj = emitDirectRRObject(B, fn, objectRegister);
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

  // slowpath: tail-call the shared slowpath function. See the corresponding
  // comment in emitSwiftReleaseDirect for why variants cannot use musttail.
  B.SetInsertPoint(slowpath);
  auto *slowFnTy = directRRFunctionType(Ctx, /*hasReturnValue=*/true,
                                        /*objectRegister=*/"");
  createTailCallAndRet(B, slowFnTy, slowFn, obj,
      llvm::CallingConv::PreserveMost, /*hasReturnValue=*/true,
      /*mustTail=*/objectRegister.empty());
}

/// Emit swift_bridgeObjectRetainDirect, or one of its `_xN` register variants,
/// as LLVM IR.
///
/// Checks for tagged pointers (bit 63) and ObjC objects (bit 62), then
/// tail-calls swift_retainDirect (which handles pointer masking internally).
///
/// A variant forwards to the *base* swift_retainDirect, passing the object as an
/// ordinary x0 argument rather than to the matching `_xN` variant. Once the
/// object has been read out of xN into a value, nothing in the IR keeps xN live
/// up to the tail call, so the register-passing contract could not be honored;
/// forwarding in x0 costs one mov and is correct.
///
/// \param objectRegister empty for the base entrypoint, which takes the object
/// as an IR argument; otherwise the register the object arrives in.
static void emitSwiftBridgeObjectRetainDirect(
    IRGenModule &IGM, bool objcInterop, StringRef objectRegister) {
  auto &Module = IGM.Module;
  auto &Ctx = Module.getContext();
  auto *i64Ty = llvm::Type::getInt64Ty(Ctx);

  auto *fnTy = directRRFunctionType(Ctx, /*hasReturnValue=*/true,
                                    objectRegister);
  auto *fn = getOrCreateFunction(Module,
      directRRName("swift_bridgeObjectRetainDirect", objectRegister),
      fnTy, llvm::GlobalValue::WeakODRLinkage,
      llvm::GlobalValue::HiddenVisibility,
      llvm::CallingConv::PreserveMost);

  // The callees always take the object as an ordinary argument.
  auto *targetTy = directRRFunctionType(Ctx, /*hasReturnValue=*/true,
                                        /*objectRegister=*/"");
  auto *retainFn = Module.getFunction("swift_retainDirect");
  bool mustTail = objectRegister.empty();

  llvm::IRBuilder<> B(Ctx);

  auto *entry = llvm::BasicBlock::Create(Ctx, "entry", fn);
  B.SetInsertPoint(entry);
  auto *obj = emitDirectRRObject(B, fn, objectRegister);

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
    createTailCallAndRet(B, targetTy, cfFn, obj,
        llvm::CallingConv::PreserveMost, /*hasReturnValue=*/true, mustTail);

    // Tail-call swift_retainDirect with the original bridgeObject value.
    // retainDirect handles pointer masking internally.
    B.SetInsertPoint(callRetain);
    createTailCallAndRet(B, targetTy, retainFn, obj,
        llvm::CallingConv::PreserveMost, /*hasReturnValue=*/true, mustTail);
  } else {
    // Without ObjC interop, swift_retainDirect handles everything: the
    // null/negative check catches tagged pointers (high bit set), and the
    // pointer mask clears spare bits.
    createTailCallAndRet(B, targetTy, retainFn, obj,
        llvm::CallingConv::PreserveMost, /*hasReturnValue=*/true, mustTail);
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

  // Emit the direct retain/release functions as LLVM IR. Each is emitted as a
  // base entrypoint taking the object in x0, plus one `_xN` variant per
  // register in DirectRRVariantRegisters that reads the object out of xN. The
  // slowpath functions are shared by all variants of a given operation.
  //
  // The core variant is emitted before the matching bridge variant, which
  // forwards to it.
  if (needRelease) {
    auto *releaseSlowFn = createSlowpathFunction(IGM,
        "swift_releaseDirect.slowpath",
        "swift_release_preservemost", "swift_release_callframe",
        directRRFunctionType(Ctx, /*hasReturnValue=*/false,
                             /*objectRegister=*/""),
        targetHasPreservemostRetainRelease, /*hasReturnValue=*/false);

    emitSwiftReleaseDirect(IGM, maskGV, strongRCOne, releaseSlowFn,
                           /*objectRegister=*/"");
    emitSwiftBridgeObjectReleaseDirect(IGM, objcInterop,
                                       bridgeObjectPointerBits,
                                       /*objectRegister=*/"");
    for (StringRef reg : DirectRRVariantRegisters) {
      emitSwiftReleaseDirect(IGM, maskGV, strongRCOne, releaseSlowFn, reg);
      emitSwiftBridgeObjectReleaseDirect(IGM, objcInterop,
                                         bridgeObjectPointerBits, reg);
    }
  }
  if (needRetain) {
    auto *retainSlowFn = createSlowpathFunction(IGM,
        "swift_retainDirect.slowpath",
        "swift_retain_preservemost", "swift_retain_callframe",
        directRRFunctionType(Ctx, /*hasReturnValue=*/true,
                             /*objectRegister=*/""),
        targetHasPreservemostRetainRelease, /*hasReturnValue=*/true);

    emitSwiftRetainDirect(IGM, maskGV, strongRCOne, bridgeObjectPointerBits,
                          retainSlowFn, /*objectRegister=*/"");
    emitSwiftBridgeObjectRetainDirect(IGM, objcInterop,
                                      /*objectRegister=*/"");
    for (StringRef reg : DirectRRVariantRegisters) {
      emitSwiftRetainDirect(IGM, maskGV, strongRCOne, bridgeObjectPointerBits,
                            retainSlowFn, reg);
      emitSwiftBridgeObjectRetainDirect(IGM, objcInterop, reg);
    }
  }
}

void IRGenModule::emitDirectRuntimeAsm() {
  if (!TargetInfo.HasSwiftSwiftDirectRuntimeLibrary ||
      !getOptions().EnableSwiftDirectRetainRelease)
    return;

  if (Triple.getArch() == llvm::Triple::aarch64 && Triple.isOSDarwin())
    emitDirectRetainReleaseARM64(*this);
}
