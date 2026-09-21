//===--- GenDirectRuntime.cpp - Direct runtime inline asm emission --------===//
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
//  This file implements emission of direct runtime functions as module-level
//  inline assembly. The assembly is read from an installed .s template file and
//  parameterized via .set directives. The functions use .weak_definition so the
//  linker coalesces duplicates. This allows non-Swift projects linking Swift
//  static libraries to work without needing Swift's library search paths.
//
//===----------------------------------------------------------------------===//

#include "IRGenModule.h"
#include "swift/ABI/System.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
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

/// Emit direct retain/release functions as ARM64 inline assembly.
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
  auto *STI = IGM.TargetMachine->getMCSubtargetInfo();
  bool useCAS = STI->checkFeatures("+lse");
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

  // --- Read the assembly template ---
  auto &resourcePath = IGM.Context.SearchPathOpts.RuntimeResourcePath;
  llvm::SmallString<128> asmPath(resourcePath);
  llvm::sys::path::append(asmPath, "DirectRuntimeRetainRelease-arm64.s");

  auto fs =
      IGM.getSwiftModule()->getASTContext().SourceMgr.getFileSystem();
  auto fileOrErr = fs->getBufferForFile(asmPath);
  if (!fileOrErr) {
    IGM.error(SourceLoc(),
              "cannot read direct retain/release assembly template at '" +
                  asmPath + "'");
    return;
  }

  // --- Build the assembly preamble ---
  std::string asmStr;
  llvm::raw_string_ostream OS(asmStr);

  // Emit a .set directive to define an assembler constant.
  auto setConst = [&](const char *name, uint64_t value) {
    OS << ".set " << name << ", " << value << "\n";
  };

  // Set parameters that the assembly template reads via .if conditionals.
  setConst("USE_CAS", useCAS);
  setConst("OBJC_INTEROP", objcInterop);
  setConst("TARGET_HAS_PRESERVEMOST", targetHasPreservemostRetainRelease);
  setConst("NEED_RETAIN", needRetain);
  setConst("NEED_RELEASE", needRelease);
  setConst("STRONG_RC_ONE", strongRCOne);
  setConst("BRIDGEOBJECT_POINTER_BITS", bridgeObjectPointerBits);

  // Append the assembly template.
  OS << fileOrErr.get()->getBuffer();

  OS.flush();
  Module.appendModuleInlineAsm(asmStr);
}

void IRGenModule::emitDirectRuntimeAsm() {
  if (!TargetInfo.HasSwiftSwiftDirectRuntimeLibrary ||
      !getOptions().EnableSwiftDirectRetainRelease)
    return;

  if (Triple.getArch() == llvm::Triple::aarch64 && Triple.isOSDarwin())
    emitDirectRetainReleaseARM64(*this);
}
