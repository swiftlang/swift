//===--- Passes.cpp - LLVM Reference Counting Optimizations ---------------===//
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
// This file implements optimizations for reference counting, object allocation,
// and other runtime entrypoints. Most of this code will be removed once the SIL
// level ARC optimizer causes it to no longer be needed.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "swift-arc-opts"
#include "swift/LLVMPasses/Passes.h"
#include "LLVMARCOpts.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/InstructionSimplify.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/Transforms/Utils/SSAUpdater.h"
#include "llvm/Transforms/Utils/Local.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/ADT/Triple.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/CommandLine.h"

using namespace llvm;
using namespace swift;
using swift::SwiftARCOpt;
using swift::SwiftARCExpandPass;

STATISTIC(NumNoopDeleted,
          "Number of no-op swift calls eliminated");
STATISTIC(NumRetainReleasePairs,
          "Number of swift retain/release pairs eliminated");
STATISTIC(NumObjCRetainReleasePairs,
          "Number of objc retain/release pairs eliminated");
STATISTIC(NumAllocateReleasePairs,
          "Number of swift allocate/release pairs eliminated");
STATISTIC(NumStoreOnlyObjectsEliminated,
          "Number of swift stored-only objects eliminated");
STATISTIC(NumReturnThreeTailCallsFormed,
          "Number of swift_retainAndReturnThree tail calls formed");

llvm::cl::opt<bool>
DisableARCOpts("disable-llvm-arc-opts", llvm::cl::init(false));

//===----------------------------------------------------------------------===//
//                            Utility Functions
//===----------------------------------------------------------------------===//

/// getRetain - Return a callable function for swift_retain.  F is the function
/// being operated on, ObjectPtrTy is an instance of the object pointer type to
/// use, and Cache is a null-initialized place to make subsequent requests
/// faster.
static Constant *getRetain(Function &F, Type *ObjectPtrTy, Constant *&Cache) {
  if (Cache) return Cache;

  auto AttrList = AttributeSet::get(F.getContext(),
                                    AttributeSet::FunctionIndex,
                                    Attribute::NoUnwind);

  Module *M = F.getParent();
  return Cache = M->getOrInsertFunction("swift_retain", AttrList,
                                        ObjectPtrTy, ObjectPtrTy, NULL);
}

/// getRetainNoResult - Return a callable function for swift_retain_noresult.
/// F is the function being operated on, ObjectPtrTy is an instance of the
/// object pointer type to use, and Cache is a null-initialized place to make
/// subsequent requests faster.
static Constant *getRetainNoResult(Function &F, Type *ObjectPtrTy,
                                   Constant *&Cache) {
  if (Cache) return Cache;

  auto AttrList = AttributeSet::get(F.getContext(), 1, Attribute::NoCapture);
  AttrList = AttrList.addAttribute(F.getContext(),
                                   AttributeSet::FunctionIndex,
                                   Attribute::NoUnwind);
  Module *M = F.getParent();
  return Cache = M->getOrInsertFunction("swift_retain_noresult", AttrList,
                                        Type::getVoidTy(F.getContext()),
                                        ObjectPtrTy, NULL);
}

/// getRetainAndReturnThree - Return a callable function for
/// swift_retainAndReturnThree.  F is the function being operated on,
/// ObjectPtrTy is an instance of the object pointer type to use, and Cache is a
/// null-initialized place to make subsequent requests faster.
static Constant *getRetainAndReturnThree(Function &F, Type *ObjectPtrTy,
                                         Constant *&Cache) {
  if (Cache) return Cache;

  auto AttrList = AttributeSet::get(F.getContext(),
                                    AttributeSet::FunctionIndex,
                                    Attribute::NoUnwind);
  Module *M = F.getParent();

  Type *Int64Ty = Type::getInt64Ty(F.getContext());
  Type *RetTy = StructType::get(Int64Ty, Int64Ty, Int64Ty, NULL);

  return Cache = M->getOrInsertFunction("swift_retainAndReturnThree", AttrList,
                                        RetTy, ObjectPtrTy,
                                        Int64Ty, Int64Ty, Int64Ty, NULL);
}

//===----------------------------------------------------------------------===//
//                          Input Function Canonicalizer
//===----------------------------------------------------------------------===//

/// updateCallValueUses - We have something like this:
///   %z = ptrtoint %swift.refcounted* %2 to i64
///   %3 = call { i64, i64, i64 }
///           @swift_retainAndReturnThree(..., i64 %x, i64 %1, i64 %z)
///   %a = extractvalue { i64, i64, i64 } %3, 0
///   %b = extractvalue { i64, i64, i64 } %3, 1
///   %c = extractvalue { i64, i64, i64 } %3, 2
///   %a1 = inttoptr i64 %a to i8*
///   %c1 = inttoptr i64 %c to %swift.refcounted*
///
/// This function is invoked three times (once each for the three arg/retvalues
/// that need to be replaced) and tries a best effort to patch up things to
/// avoid all the casts.  "Inst" coming into here is the call to
/// swift_retainAndReturnThree or to an extract that returns all three words.
///
static void updateCallValueUses(CallInst &CI, unsigned EltNo) {
  Value *Op = CI.getArgOperand(1+EltNo);
  for (auto UI = CI.user_begin(), E = CI.user_end(); UI != E; ++UI) {
    ExtractValueInst *Extract = dyn_cast<ExtractValueInst>(*UI);

    // Make sure this extract is relevant to EltNo.
    if (Extract == 0 || Extract->getNumIndices() != 1 ||
        Extract->getIndices()[0] != EltNo)
      continue;

    // Both the input and result should be i64's.
    assert(Extract->getType() == Op->getType() && "Should have i64's here");

    for (auto UI2 = Extract->user_begin(), E = Extract->user_end(); UI2 != E; ){
      IntToPtrInst *ExtractUser = dyn_cast<IntToPtrInst>(*UI2++);
      PtrToIntInst *OpCast = dyn_cast<PtrToIntInst>(Op);
      if (ExtractUser && OpCast &&
          OpCast->getOperand(0)->getType() == ExtractUser->getType()) {
        ExtractUser->replaceAllUsesWith(OpCast->getOperand(0));
        ExtractUser->eraseFromParent();
      }
    }

    // Stitch up anything other than the ptrtoint -> inttoptr.
    Extract->replaceAllUsesWith(Op);

    // Zap the dead ExtractValue's.
    RecursivelyDeleteTriviallyDeadInstructions(Extract);
    return;
  }
}

/// canonicalizeInputFunction - Functions like swift_retain return an
/// argument as a low-level performance optimization.  This makes it difficult
/// to reason about pointer equality though, so undo it as an initial
/// canonicalization step.  After this step, all swift_retain's have been
/// replaced with swift_retain_noresult.
///
/// This also does some trivial peep-hole optimizations as we go.
static bool canonicalizeInputFunction(Function &F) {
  Constant *RetainNoResultCache = 0;

  bool Changed = false;
  for (auto &BB : F)
  for (auto I = BB.begin(); I != BB.end(); ) {
    Instruction &Inst = *I++;

    switch (classifyInstruction(Inst)) {
    case RT_Unknown:
    case RT_BridgeRelease:
    case RT_AllocObject:
    case RT_FixLifetime:
    case RT_NoMemoryAccessed:
      break;
    case RT_RetainNoResult: {
      CallInst &CI = cast<CallInst>(Inst);
      Value *ArgVal = CI.getArgOperand(0);
      // retain_noresult(null) is a no-op.
      if (isa<ConstantPointerNull>(ArgVal)) {
        CI.eraseFromParent();
        Changed = true;
        ++NumNoopDeleted;
        continue;
      }
      break;
    }
    case RT_Retain: {
      // If any x = swift_retain(y)'s got here, canonicalize them into:
      //   x = y; swift_retain_noresult(y).
      // This is important even though the front-end doesn't generate them,
      // because inlined functions can be ARC optimized, and thus may contain
      // swift_retain calls.
      CallInst &CI = cast<CallInst>(Inst);
      Value *ArgVal = CI.getArgOperand(0);

      // Rewrite uses of the result to use the argument.
      if (!CI.use_empty())
        Inst.replaceAllUsesWith(ArgVal);

      // Insert a call to swift_retain_noresult to replace this and reset the
      // iterator so that we visit it next.
      I = CallInst::Create(getRetainNoResult(F, ArgVal->getType(),
                                             RetainNoResultCache),
                           ArgVal, "", &CI);
      I->setDebugLoc(CI.getDebugLoc());
      CI.eraseFromParent();
      Changed = true;
      break;
    }
    case RT_Release:
    case RT_UnknownRelease: {
      CallInst &CI = cast<CallInst>(Inst);
      // swift_release(null) is a noop, zap it.
      Value *ArgVal = CI.getArgOperand(0);
      if (isa<ConstantPointerNull>(ArgVal)) {
        CI.eraseFromParent();
        Changed = true;
        ++NumNoopDeleted;
        continue;
      }
      break;
    }
    case RT_RetainAndReturnThree: {
      // (a,b,c) = swift_retainAndReturnThree(obj, d,e,f)
      // -> swift_retain_noresult(obj)
      // -> (a,b,c) = (d,e,f)
      //
      // The important case of doing this is when this function has been inlined
      // into another function.  In this case, there is no return anymore.
      CallInst &CI = cast<CallInst>(Inst);

      IRBuilder<> B(&CI);
      B.SetCurrentDebugLocation(CI.getDebugLoc());
      Type *HeapObjectTy = CI.getArgOperand(0)->getType();

      // Reprocess starting at the new swift_retain_noresult.
      I = B.CreateCall(getRetainNoResult(F, HeapObjectTy, RetainNoResultCache),
                       CI.getArgOperand(0));

      // See if we can eliminate all of the extractvalue's that are hanging off
      // the swift_retainAndReturnThree.  This is important to eliminate casts
      // that will block optimizations and generally results in better IR.  Note
      // that this is just a best-effort attempt though.
      updateCallValueUses(CI, 0);
      updateCallValueUses(CI, 1);
      updateCallValueUses(CI, 2);

      // If our best-effort wasn't good enough, fall back to generating terrible
      // but correct code.
      if (!CI.use_empty()) {
        Value *V = UndefValue::get(CI.getType());
        V = B.CreateInsertValue(V, CI.getArgOperand(1), 0U);
        V = B.CreateInsertValue(V, CI.getArgOperand(2), 1U);
        V = B.CreateInsertValue(V, CI.getArgOperand(3), 2U);
        CI.replaceAllUsesWith(V);
      }

      CI.eraseFromParent();
      Changed = true;
      break;
    }

    case RT_ObjCRelease: {
      CallInst &CI = cast<CallInst>(Inst);
      Value *ArgVal = CI.getArgOperand(0);
      // objc_release(null) is a noop, zap it.
      if (isa<ConstantPointerNull>(ArgVal)) {
        CI.eraseFromParent();
        Changed = true;
        ++NumNoopDeleted;
        continue;
      }
      break;
    }

    // These retain instructions return their argument so must be processed
    // specially.
    case RT_UnknownRetain:
    case RT_BridgeRetain:
    case RT_ObjCRetain: {
      // Canonicalize the retain so that nothing uses its result.
      CallInst &CI = cast<CallInst>(Inst);
      Value *ArgVal = CI.getArgOperand(0);
      if (!CI.use_empty()) {
        CI.replaceAllUsesWith(ArgVal);
        Changed = true;
      }

      // {objc_retain,swift_unknownRetain}(null) is a noop, delete it.
      if (isa<ConstantPointerNull>(ArgVal)) {
        CI.eraseFromParent();
        Changed = true;
        ++NumNoopDeleted;
        continue;
      }

      break;
    }
    }
  }
  return Changed;
}

//===----------------------------------------------------------------------===//
//                         Release() Motion
//===----------------------------------------------------------------------===//

/// performLocalReleaseMotion - Scan backwards from the specified release,
/// moving it earlier in the function if possible, over instructions that do not
/// access the released object.  If we get to a retain or allocation of the
/// object, zap both.
static bool performLocalReleaseMotion(CallInst &Release, BasicBlock &BB) {
  // FIXME: Call classifier should identify the object for us.  Too bad C++
  // doesn't have nice Swift-style enums.
  Value *ReleasedObject = Release.getArgOperand(0);

  BasicBlock::iterator BBI = &Release;

  // Scan until we get to the top of the block.
  while (BBI != BB.begin()) {
    --BBI;

    // Don't analyze PHI nodes.  We can't move retains before them and they
    // aren't "interesting".
    if (isa<PHINode>(BBI) ||
        // If we found the instruction that defines the value we're releasing,
        // don't push the release past it.
        &*BBI == ReleasedObject) {
      ++BBI;
      goto OutOfLoop;
    }

    switch (classifyInstruction(*BBI)) {
    case RT_Retain: // Canonicalized away, shouldn't exist.
    case RT_RetainAndReturnThree:
      llvm_unreachable("these entrypoints should be canonicalized away");
    case RT_NoMemoryAccessed:
      // Skip over random instructions that don't touch memory.  They don't need
      // protection by retain/release.
      continue;

    case RT_UnknownRelease:
    case RT_BridgeRelease:
    case RT_ObjCRelease:
    case RT_Release: {
      // If we get to a release, we can generally ignore it and scan past it.
      // However, if we get to a release of obviously the same object, we stop
      // scanning here because it should have already be moved as early as
      // possible, so there is no reason to move its friend to the same place.
      //
      // NOTE: If this occurs frequently, maybe we can have a release(Obj, N)
      // API to drop multiple retain counts at once.
      CallInst &ThisRelease = cast<CallInst>(*BBI);
      Value *ThisReleasedObject = ThisRelease.getArgOperand(0);
      if (ThisReleasedObject == ReleasedObject) {
        //Release.dump(); ThisRelease.dump(); BB.getParent()->dump();
        ++BBI;
        goto OutOfLoop;
      }
      continue;
    }

    case RT_UnknownRetain:
    case RT_BridgeRetain:
    case RT_ObjCRetain:
    case RT_RetainNoResult: {  // swift_retain_noresult(obj)
      CallInst &Retain = cast<CallInst>(*BBI);
      Value *RetainedObject = Retain.getArgOperand(0);

      // Since we canonicalized earlier, we know that if our retain has any
      // uses, they were replaced already. This assertion documents this
      // assumption.
      assert(Retain.use_empty() && "Retain should have been canonicalized to "
             "have no uses.");

      // If the retain and release are to obviously pointer-equal objects, then
      // we can delete both of them.  We have proven that they do not protect
      // anything of value.
      if (RetainedObject == ReleasedObject) {
        Retain.eraseFromParent();
        Release.eraseFromParent();
        ++NumRetainReleasePairs;
        return true;
      }

      // Otherwise, this is a retain of an object that is not statically known
      // to be the same object.  It may still be dynamically the same object
      // though.  In this case, we can't move the release past it.
      // TODO: Strengthen analysis.
      //Release.dump(); ThisRelease.dump(); BB.getParent()->dump();
     ++BBI;
      goto OutOfLoop;
    }

    case RT_AllocObject: {   // %obj = swift_alloc(...)
      CallInst &Allocation = cast<CallInst>(*BBI);

      // If this is an allocation of an unrelated object, just ignore it.
      // TODO: This is not safe without proving the object being released is not
      // related to the allocated object.  Consider something silly like this:
      //   A = allocate()
      //   B = bitcast A to object
      //   release(B)
      if (ReleasedObject != &Allocation) {
        // Release.dump(); BB.getParent()->dump();
        ++BBI;
        goto OutOfLoop;
      }

      // If this is a release right after an allocation of the object, then we
      // can zap both.
      Allocation.replaceAllUsesWith(UndefValue::get(Allocation.getType()));
      Allocation.eraseFromParent();
      Release.eraseFromParent();
      ++NumAllocateReleasePairs;
      return true;
    }

    case RT_FixLifetime:
    case RT_Unknown:
      // Otherwise, we have reached something that we do not understand. Do not
      // attempt to shorten the lifetime of this object beyond this point so we
      // are conservative.
      ++BBI;
      goto OutOfLoop;
    }
  }
OutOfLoop:


  // If we got to the top of the block, (and if the instruction didn't start
  // there) move the release to the top of the block.
  // TODO: This is where we'd plug in some global algorithms someday.
  if (&*BBI != &Release) {
    Release.moveBefore(BBI);
    return true;
  }

  return false;
}


//===----------------------------------------------------------------------===//
//                         Retain() Motion
//===----------------------------------------------------------------------===//

/// performLocalRetainMotion - Scan forward from the specified retain, moving it
/// later in the function if possible, over instructions that provably can't
/// release the object.  If we get to a release of the object, zap both.
///
/// NOTE: this handles both objc_retain and swift_retain_noresult.
///
static bool performLocalRetainMotion(CallInst &Retain, BasicBlock &BB) {
  // FIXME: Call classifier should identify the object for us.  Too bad C++
  // doesn't have nice Swift-style enums.
  Value *RetainedObject = Retain.getArgOperand(0);

  BasicBlock::iterator BBI = &Retain, BBE = BB.getTerminator();

  bool isObjCRetain = Retain.getCalledFunction()->getName() == "objc_retain";

  bool MadeProgress = false;

  // Scan until we get to the end of the block.
  for (++BBI; BBI != BBE; ++BBI) {
    Instruction &CurInst = *BBI;

    // Classify the instruction. This switch does a "break" when the instruction
    // can be skipped and is interesting, and a "continue" when it is a retain
    // of the same pointer.
    switch (classifyInstruction(CurInst)) {
    case RT_Retain: // Canonicalized away, shouldn't exist.
    case RT_RetainAndReturnThree:
      llvm_unreachable("these entrypoints should be canonicalized away");
    case RT_NoMemoryAccessed:
    case RT_AllocObject:
      // Skip over random instructions that don't touch memory.  They don't need
      // protection by retain/release.
      break;

    case RT_FixLifetime: // This only stops release motion. Retains can move over it.
      break;

    case RT_RetainNoResult:
    case RT_UnknownRetain:
    case RT_BridgeRetain:
    case RT_ObjCRetain: {  // swift_retain_noresult(obj)
      //CallInst &ThisRetain = cast<CallInst>(CurInst);
      //Value *ThisRetainedObject = ThisRetain.getArgOperand(0);

      // If we see a retain of the same object, we can skip over it, but we
      // can't count it as progress.  Just pushing a retain(x) past a retain(y)
      // doesn't change the program.
      continue;
    }

    case RT_UnknownRelease:
    case RT_BridgeRelease:
    case RT_ObjCRelease:
    case RT_Release: {
      // If we get to a release that is provably to this object, then we can zap
      // it and the retain.
      CallInst &ThisRelease = cast<CallInst>(CurInst);
      Value *ThisReleasedObject = ThisRelease.getArgOperand(0);
      if (ThisReleasedObject == RetainedObject) {
        Retain.eraseFromParent();
        ThisRelease.eraseFromParent();
        if (isObjCRetain) {
          ++NumObjCRetainReleasePairs;
        } else {
          ++NumRetainReleasePairs;
        }
        return true;
      }

      // Otherwise, if this is some other pointer, we can only ignore it if we
      // can prove that the two objects don't alias.
      // Retain.dump(); ThisRelease.dump(); BB.getParent()->dump();
      goto OutOfLoop;
    }

    case RT_Unknown:
      // Loads cannot affect the retain.
      if (isa<LoadInst>(CurInst))
        continue;

      // Load, store, memcpy etc can't do a release.
      if (isa<LoadInst>(CurInst) || isa<StoreInst>(CurInst) ||
          isa<MemIntrinsic>(CurInst))
        break;

      // CurInst->dump(); BBI->dump();
      // Otherwise, we get to something unknown/unhandled.  Bail out for now.
      goto OutOfLoop;
    }

    // If the switch did a break, we made some progress moving this retain.
    MadeProgress = true;
  }
OutOfLoop:

  // If we were able to move the retain down, move it now.
  // TODO: This is where we'd plug in some global algorithms someday.
  if (MadeProgress) {
    Retain.moveBefore(BBI);
    return true;
  }

  return false;
}


//===----------------------------------------------------------------------===//
//                       Store-Only Object Elimination
//===----------------------------------------------------------------------===//

/// DT_Kind - Classification for destructor semantics.
enum class DtorKind {
  /// NoSideEffects - The destructor does nothing, or just touches the local
  /// object in a non-observable way after it is destroyed.
  NoSideEffects,

  /// NoEscape - The destructor potentially has some side effects, but the
  /// address of the destroyed object never escapes (in the LLVM IR sense).
  NoEscape,

  /// Unknown - Something potentially crazy is going on here.
  Unknown
};

/// analyzeDestructor - Given the heap.metadata argument to swift_allocObject,
/// take a look a the destructor and try to decide if it has side effects or any
/// other bad effects that can prevent it from being optimized.
static DtorKind analyzeDestructor(Value *P) {
  // If we have a null pointer for the metadata info, the dtor has no side
  // effects.  Actually, the final release would crash.  This is really only
  // useful for writing testcases.
  if (isa<ConstantPointerNull>(P->stripPointerCasts()))
    return DtorKind::NoSideEffects;

  // We have to have a known heap metadata value, reject dynamically computed
  // ones, or places
  GlobalVariable *GV = dyn_cast<GlobalVariable>(P->stripPointerCasts());
  if (GV == 0 || GV->mayBeOverridden()) return DtorKind::Unknown;

  ConstantStruct *CS = dyn_cast_or_null<ConstantStruct>(GV->getInitializer());
  if (CS == 0 || CS->getNumOperands() == 0) return DtorKind::Unknown;

  // FIXME: Would like to abstract the dtor slot (#0) out from this to somewhere
  // unified.
  enum { DTorSlotOfHeapMeatadata = 0 };
  Function *DtorFn =dyn_cast<Function>(CS->getOperand(DTorSlotOfHeapMeatadata));
  if (DtorFn == 0 || DtorFn->mayBeOverridden() || DtorFn->hasExternalLinkage())
    return DtorKind::Unknown;

  // Okay, we have a body, and we can trust it.  If the function is marked
  // readonly, then we know it can't have any interesting side effects, so we
  // don't need to analyze it at all.
  if (DtorFn->onlyReadsMemory())
    return DtorKind::NoSideEffects;

  // The first argument is the object being destroyed.
  assert(DtorFn->arg_size() == 1 && !DtorFn->isVarArg() &&
         "expected a single object argument to destructors");
  Value *ThisObject = DtorFn->arg_begin();

  // Scan the body of the function, looking for anything scary.
  for (BasicBlock &BB : *DtorFn) {
    for (Instruction &I : BB) {
      // Note that the destructor may not be in any particular canonical form.
      switch (classifyInstruction(I)) {
      case RT_NoMemoryAccessed:
      case RT_AllocObject:
      case RT_FixLifetime:
        // Skip over random instructions that don't touch memory in the caller.
        continue;

      case RT_Retain:                // x = swift_retain(y)
      case RT_BridgeRetain:          // x = swift_bridgeRetain(y)
      case RT_RetainAndReturnThree:  // swift_retainAndReturnThree(obj,a,b,c)
      case RT_RetainNoResult: {      // swift_retain_noresult(obj)

        // Ignore retains of the "self" object, no ressurection is possible.
        Value *ThisRetainedObject = cast<CallInst>(I).getArgOperand(0);
        if (ThisRetainedObject->stripPointerCasts() ==
            ThisObject->stripPointerCasts())
          continue;
        // Otherwise, we may be retaining something scary.
        break;
      }

      case RT_Release: {
        // If we get to a release that is provably to this object, then we can
        // ignore it.
        Value *ThisReleasedObject = cast<CallInst>(I).getArgOperand(0);

        if (ThisReleasedObject->stripPointerCasts() ==
            ThisObject->stripPointerCasts())
          continue;
        // Otherwise, we may be retaining something scary.
        break;
      }

      case RT_ObjCRelease:
      case RT_ObjCRetain:
      case RT_UnknownRetain:
      case RT_UnknownRelease:
      case RT_BridgeRelease:
        // Objective-C retain and release can have arbitrary side effects.
        break;

      case RT_Unknown:
        // Ignore all instructions with no side effects.
        if (!I.mayHaveSideEffects()) continue;

        // store, memcpy, memmove *to* the object can be dropped.
        if (StoreInst *SI = dyn_cast<StoreInst>(&I)) {
          if (SI->getPointerOperand()->stripInBoundsOffsets() == ThisObject)
            continue;
        }

        if (MemIntrinsic *MI = dyn_cast<MemIntrinsic>(&I)) {
          if (MI->getDest()->stripInBoundsOffsets() == ThisObject)
            continue;
        }

        // Otherwise, we can't remove the deallocation completely.
        break;
      }

      // Okay, the function has some side effects, if it doesn't capture the
      // object argument, at least that is something.
      return DtorFn->doesNotCapture(0) ? DtorKind::NoEscape : DtorKind::Unknown;
    }
  }

  // If we didn't find any side effects, we win.
  return DtorKind::NoSideEffects;
}


/// performStoreOnlyObjectElimination - Scan the graph of uses of the specified
/// object allocation.  If the object does not escape and is only stored to
/// (this happens because GVN and other optimizations hoists forward substitutes
/// all stores to the object to eliminate all loads from it), then zap the
/// object and all accesses related to it.
static bool performStoreOnlyObjectElimination(CallInst &Allocation,
                                              BasicBlock::iterator &BBI) {
  DtorKind DtorInfo = analyzeDestructor(Allocation.getArgOperand(0));

  // We can't delete the object if its destructor has side effects.
  if (DtorInfo != DtorKind::NoSideEffects)
    return false;

  // Do a depth first search exploring all of the uses of the object pointer,
  // following through casts, pointer adjustments etc.  If we find any loads or
  // any escape sites of the object, we give up.  If we succeed in walking the
  // entire graph of uses, we can remove the resultant set.
  SmallSetVector<Instruction*, 16> InvolvedInstructions;
  SmallVector<Instruction*, 16> Worklist;
  Worklist.push_back(&Allocation);

  // Stores - Keep track of all of the store instructions we see.
  SmallVector<StoreInst*, 16> Stores;

  while (!Worklist.empty()) {
    Instruction *I = Worklist.pop_back_val();

    // Insert the instruction into our InvolvedInstructions set.  If we have
    // already seen it, then don't reprocess all of the uses.
    if (!InvolvedInstructions.insert(I)) continue;

    // Okay, this is the first time we've seen this instruction, proceed.
    switch (classifyInstruction(*I)) {
    case RT_Retain:
    case RT_RetainAndReturnThree:
      llvm_unreachable("These should be canonicalized away");

    case RT_AllocObject:
      // If this is a different swift_allocObject than we started with, then
      // there is some computation feeding into a size or alignment computation
      // that we have to keep... unless we can delete *that* entire object as
      // well.
      break;

    case RT_NoMemoryAccessed:
      // If no memory is accessed, then something is being done with the
      // pointer: maybe it is bitcast or GEP'd. Since there are no side effects,
      // it is perfectly fine to delete this instruction if all uses of the
      // instruction are also eliminable.

      if (I->mayHaveSideEffects() || isa<TerminatorInst>(I))
        return false;
      break;

    case RT_Release:
    case RT_RetainNoResult:
    case RT_FixLifetime:
      // It is perfectly fine to eliminate various retains and releases of this
      // object: we are zapping all accesses or none.
      break;

    // If this is an unknown instruction, we have more interesting things to
    // consider.
    case RT_Unknown:
    case RT_ObjCRelease:
    case RT_ObjCRetain:
    case RT_UnknownRetain:
    case RT_UnknownRelease:
    case RT_BridgeRetain:
    case RT_BridgeRelease:

      // Otherwise, this really is some unhandled instruction.  Bail out.
      return false;
    }

    // Okay, if we got here, the instruction can be eaten so-long as all of its
    // uses can be.  Scan through the uses and add them to the worklist for
    // recursive processing.
    for (auto UI = I->user_begin(), E = I->user_end(); UI != E; ++UI) {
      Instruction *User = cast<Instruction>(*UI);

      // Handle stores as a special case here: we want to make sure that the
      // object is being stored *to*, not itself being stored (which would be an
      // escape point).  Since stores themselves don't have any uses, we can
      // short-cut the classification scheme above.
      if (StoreInst *SI = dyn_cast<StoreInst>(User)) {
        // If this is a store *to* the object, we can zap it.
        if (UI.getUse().getOperandNo() == StoreInst::getPointerOperandIndex()) {
          InvolvedInstructions.insert(SI);
          continue;
        }
        // Otherwise, using the object as a source (or size) is an escape.
        return false;
      }
      if (MemIntrinsic *MI = dyn_cast<MemIntrinsic>(User)) {
        // If this is a memset/memcpy/memmove *to* the object, we can zap it.
        if (UI.getUse().getOperandNo() == 0) {
          InvolvedInstructions.insert(MI);
          continue;
        }
        // Otherwise, using the object as a source (or size) is an escape.
        return false;
      }

      // Otherwise, normal instructions just go on the worklist for processing.
      Worklist.push_back(User);
    }
  }

  // Ok, we succeeded!  This means we can zap all of the instructions that use
  // the object.  One thing we have to be careful of is to make sure that we
  // don't invalidate "BBI" (the iterator the outer walk of the optimization
  // pass is using, and indicates the next instruction to process).  This would
  // happen if we delete the instruction it is pointing to.  Advance the
  // iterator if that would happen.
  while (InvolvedInstructions.count(BBI))
    ++BBI;

  // Zap all of the instructions.
  for (auto I : InvolvedInstructions) {
    if (!I->use_empty())
      I->replaceAllUsesWith(UndefValue::get(I->getType()));
    I->eraseFromParent();
  }

  ++NumStoreOnlyObjectsEliminated;
  return true;
}

/// performGeneralOptimizations - This does a forward scan over basic blocks,
/// looking for interesting local optimizations that can be done.
static bool performGeneralOptimizations(Function &F) {
  bool Changed = false;

  // TODO: This is a really trivial local algorithm.  It could be much better.
  for (BasicBlock &BB : F) {
    for (BasicBlock::iterator BBI = BB.begin(), E = BB.end(); BBI != E; ) {
      // Preincrement the iterator to avoid invalidation and out trouble.
      Instruction &I = *BBI++;

      // Do various optimizations based on the instruction we find.
      switch (classifyInstruction(I)) {
      default: break;
      case RT_AllocObject:
        Changed |= performStoreOnlyObjectElimination(cast<CallInst>(I), BBI);
        break;
      case RT_BridgeRelease:
      case RT_ObjCRelease:
      case RT_UnknownRelease:
      case RT_Release:
        Changed |= performLocalReleaseMotion(cast<CallInst>(I), BB);
        break;
      case RT_BridgeRetain:
      case RT_RetainNoResult:
      case RT_UnknownRetain:
      case RT_ObjCRetain: {
        // Retain motion is a forward pass over the block.  Make sure we don't
        // invalidate our iterators by parking it on the instruction before I.
        BasicBlock::iterator Safe = &I;
        Safe = Safe != BB.begin() ? std::prev(Safe) : BB.end();
        if (performLocalRetainMotion(cast<CallInst>(I), BB)) {
          // If we zapped or moved the retain, reset the iterator on the
          // instruction *newly* after the prev instruction.
          BBI = Safe != BB.end() ? std::next(Safe) : BB.begin();
          Changed = true;
        }
        break;
      }
      }
    }
  }
  return Changed;
}


//===----------------------------------------------------------------------===//
//                            SwiftARCOpt Pass
//===----------------------------------------------------------------------===//

namespace llvm {
  void initializeSwiftARCOptPass(PassRegistry&);
}

char SwiftARCOpt::ID = 0;

INITIALIZE_PASS_BEGIN(SwiftARCOpt,
                      "swift-arc-optimize", "Swift ARC optimization",
                      false, false)
INITIALIZE_PASS_DEPENDENCY(SwiftAliasAnalysis)
INITIALIZE_PASS_END(SwiftARCOpt,
                    "swift-arc-optimize", "Swift ARC optimization",
                    false, false)

// Optimization passes.
llvm::FunctionPass *swift::createSwiftARCOptPass() {
  initializeSwiftARCOptPass(*llvm::PassRegistry::getPassRegistry());
  return new SwiftARCOpt();
}

SwiftARCOpt::SwiftARCOpt() : FunctionPass(ID) {
}


void SwiftARCOpt::getAnalysisUsage(llvm::AnalysisUsage &AU) const {
  AU.addRequired<SwiftAliasAnalysis>();
  AU.setPreservesCFG();
}

bool SwiftARCOpt::runOnFunction(Function &F) {
  if (DisableARCOpts)
    return false;

  bool Changed = false;

  // First thing: canonicalize swift_retain and similar calls so that nothing
  // uses their result.  This exposes the copy that the function does to the
  // optimizer.
  Changed |= canonicalizeInputFunction(F);

  // Next, do a pass with a couple of optimizations:
  // 1) release() motion, eliminating retain/release pairs when it turns out
  //    that a pair is not protecting anything that accesses the guarded heap
  //    object.
  // 2) deletion of stored-only objects - objects that are allocated and
  //    potentially retained and released, but are only stored to and don't
  //    escape.
  Changed |= performGeneralOptimizations(F);

  return Changed;
}



//===----------------------------------------------------------------------===//
//                      Return Argument Optimizer
//===----------------------------------------------------------------------===//

/// optimizeReturn3 - Look to see if we can optimize "ret (a,b,c)" - where one
/// of the three values was retained right before the return, into a
/// swift_retainAndReturnThree call.  This is particularly common when returning
/// a string or array slice.
static bool optimizeReturn3(ReturnInst *TheReturn) {
  // Ignore ret void.
  if (TheReturn->getNumOperands() == 0) return false;

  // See if this is a return of three things.
  Value *RetVal = TheReturn->getOperand(0);
  StructType *RetSTy = dyn_cast<StructType>(RetVal->getType());
  if (RetSTy == 0 || RetSTy->getNumElements() != 3) return false;

  // See if we can find scalars that feed into the return instruction.  If not,
  // bail out.
  Value *RetVals[3];
  for (unsigned i = 0; i != 3; ++i) {
    RetVals[i] = FindInsertedValue(RetVal, i);
    if (RetVals[i] == 0) return false;

    // If the scalar isn't int64 or pointer type, we can't transform it.
    if (!isa<PointerType>(RetVals[i]->getType()) &&
        !RetVals[i]->getType()->isIntegerTy(64))
      return false;
  }

  // The ARC optimizer will push the retains to be immediately before the
  // return, past any insertvalues.  We tolerate other non-memory instructions
  // though, in case other optimizations have moved them around.  Collect all
  // the retain candidates.
  SmallDenseMap<Value*, CallInst*, 8> RetainedPointers;

  for (BasicBlock::iterator BBI = TheReturn,E = TheReturn->getParent()->begin();
       BBI != E; ) {
    Instruction &I = *--BBI;

    switch (classifyInstruction(I)) {
    case RT_Retain: {
      // Collect retained pointers.  If a pointer is multiply retained, it
      // doesn't matter which one we aquire.
      CallInst &TheRetain = cast<CallInst>(I);
      RetainedPointers[TheRetain.getArgOperand(0)] = &TheRetain;
      break;
    }
    case RT_NoMemoryAccessed:
      // If the instruction doesn't access memory, ignore it.
      break;
    default:
      // Otherwise, break out of the for loop.
      BBI = E;
      break;
    }
  }

  // If there are no retain candidates, we can't form a return3.
  if (RetainedPointers.empty())
    return false;

  // Check to see if any of the values returned is retained.  If so, we can form
  // a return3, which makes the retain a tail call.
  CallInst *TheRetain = 0;
  for (unsigned i = 0; i != 3; ++i) {
    // If the return value is also retained, we found our retain.
    TheRetain = RetainedPointers[RetVals[i]];
    if (TheRetain) break;

    // If we're returning the result of a known retain, then we can also handle
    // it.
    if (CallInst *CI = dyn_cast<CallInst>(RetVals[i]))
      if (classifyInstruction(*CI) == RT_Retain) {
        TheRetain = RetainedPointers[CI->getArgOperand(0)];
        if (TheRetain) break;
      }
  }

  // If none of the three values was retained, we can't form a return3.
  if (TheRetain == 0)
    return false;

  // Okay, there is, which means we can perform the transformation.  Get the
  // argument to swift_retain (the result will be zapped when we zap the call)
  // as the object to retain (of %swift.refcounted* type).
  Value *RetainedObject = TheRetain->getArgOperand(0);

  // Insert any new instructions before the return.
  IRBuilder<> B(TheReturn);
  B.SetCurrentDebugLocation(TheReturn->getDebugLoc());
  Type *Int64Ty = B.getInt64Ty();

  // The swift_retainAndReturnThree function takes the three arguments as i64.
  // Cast the arguments to i64 if needed.

  // Update the element with a cast to i64 if needed.
  for (Value *&Elt : RetVals) {
    if (isa<PointerType>(Elt->getType()))
      Elt = B.CreatePtrToInt(Elt, Int64Ty);
  }

  // Call swift_retainAndReturnThree with our pointer to retain and the three
  // i64's.
  Function &F = *TheReturn->getParent()->getParent();
  Constant *Cache = 0;  // Not utilized.
  Value *LibCall = getRetainAndReturnThree(F,RetainedObject->getType(),Cache);
  CallInst *NR = B.CreateCall(
      LibCall, {RetainedObject, RetVals[0],RetVals[1], RetVals[2]});
  NR->setTailCall(true);

  // The return type of the libcall is (i64,i64,i64).  Since at least one of
  // the pointers is a pointer (we retained it afterall!) we have to unpack
  // the elements, bitcast at least that one, and then repack to the proper
  // type expected by the ret instruction.
  for (unsigned i = 0; i != 3; ++i) {
    RetVals[i] = B.CreateExtractValue(NR, i);
    if (RetVals[i]->getType() != RetSTy->getElementType(i))
      RetVals[i] = B.CreateIntToPtr(RetVals[i], RetSTy->getElementType(i));
  }

  // Repack into an aggregate that can be returned.
  Value *RV = UndefValue::get(RetVal->getType());
  for (unsigned i = 0; i != 3; ++i)
    RV = B.CreateInsertValue(RV, RetVals[i], i);

  // Return the right thing and zap any instruction tree of inserts that
  // existed just to feed the old return.
  TheReturn->setOperand(0, RV);
  RecursivelyDeleteTriviallyDeadInstructions(RetVal);

  // Zap the retain that we're subsuming and we're done!
  if (!TheRetain->use_empty())
    TheRetain->replaceAllUsesWith(RetainedObject);
  TheRetain->eraseFromParent();
  ++NumReturnThreeTailCallsFormed;
  return true;
}

//===----------------------------------------------------------------------===//
//                        SwiftARCExpandPass Pass
//===----------------------------------------------------------------------===//

/// performARCExpansion - This implements the very late (just before code
/// generation) lowering processes that we do to expose low level performance
/// optimizations and take advantage of special features of the ABI.  These
/// expansion steps can foil the general mid-level optimizer, so they are done
/// very, very, late.
///
/// Expansions include:
///   - Lowering retain calls to swift_retain (which return the retained
///     argument) to lower register pressure.
///   - Forming calls to swift_retainAndReturnThree when the last thing in a
///     function is to retain one of its result values, and when it returns
///     exactly three values.
///
/// Coming into this function, we assume that the code is in canonical form:
/// none of these calls have any uses of their return values.
bool SwiftARCExpandPass::runOnFunction(Function &F) {
  Constant *RetainCache = nullptr;
  bool Changed = false;

  SmallVector<ReturnInst*, 8> Returns;

  // Since all of the calls are canonicalized, we know that we can just walk
  // through the function and collect the interesting heap object definitions by
  // getting the argument to these functions.
  DenseMap<Value*, TinyPtrVector<Instruction*>> DefsOfValue;

  // Keep track of which order we see values in since iteration over a densemap
  // isn't in a deterministic order, and isn't efficient anyway.
  SmallVector<Value*, 16> DefOrder;

  // Do a first pass over the function, collecting all interesting definitions.
  // In this pass, we rewrite any intra-block uses that we can, since the
  // SSAUpdater doesn't handle them.
  DenseMap<Value*, Value*> LocalUpdates;
  for (BasicBlock &BB : F) {
    for (auto II = BB.begin(), E = BB.end(); II != E; ) {
      // Preincrement iterator to avoid iteration issues in the loop.
      Instruction &Inst = *II++;

      switch (classifyInstruction(Inst)) {
      // Delete all fix lifetime instructions. After llvm-ir they have no use
      // and show up as calls in the final binary.
      case RT_FixLifetime:
        Inst.eraseFromParent();
        ++NumNoopDeleted;
        continue;
      case RT_Retain:
        llvm_unreachable("This should be canonicalized away!");
      case RT_RetainAndReturnThree:
      case RT_RetainNoResult: {
        Value *ArgVal = cast<CallInst>(Inst).getArgOperand(0);

        // First step: rewrite swift_retain_noresult to swift_retain, exposing
        // the result value.
        CallInst &CI =
           *CallInst::Create(getRetain(F, ArgVal->getType(), RetainCache),
                             ArgVal, "", &Inst);
        CI.setDebugLoc(Inst.getDebugLoc());
        CI.setTailCall(true);
        Inst.eraseFromParent();

        if (!isa<Instruction>(ArgVal))
          continue;

        TinyPtrVector<Instruction*> &GlobalEntry = DefsOfValue[ArgVal];

        // If this is the first definition of a value for the argument that
        // we've seen, keep track of it in DefOrder.
        if (GlobalEntry.empty())
          DefOrder.push_back(ArgVal);

        // Check to see if there is already an entry for this basic block.  If
        // there is another local entry, switch to using the local value and
        // remove the previous value from the GlobalEntry.
        Value *&LocalEntry = LocalUpdates[ArgVal];
        if (LocalEntry) {
          Changed = true;
          CI.setArgOperand(0, LocalEntry);
          assert(GlobalEntry.back() == LocalEntry && "Local/Global mismatch?");
          GlobalEntry.pop_back();
        }

        LocalEntry = &CI;
        GlobalEntry.push_back(&CI);
        continue;
      }
      case RT_Unknown:
      case RT_Release:
      case RT_AllocObject:
      case RT_NoMemoryAccessed:
      case RT_UnknownRelease:
      case RT_UnknownRetain:
      case RT_BridgeRelease:
      case RT_BridgeRetain:
      case RT_ObjCRelease:
      case RT_ObjCRetain:  // TODO: Could chain together objc_retains.
        // Remember returns in the first pass.
        if (ReturnInst *RI = dyn_cast<ReturnInst>(&Inst))
          Returns.push_back(RI);

        // Just remap any uses in the value.
        break;
      }

      // Check to see if there are any uses of a value in the LocalUpdates
      // map.  If so, remap it now to the locally defined version.
      for (unsigned i = 0, e = Inst.getNumOperands(); i != e; ++i)
        if (Value *V = LocalUpdates.lookup(Inst.getOperand(i))) {
          Changed = true;
          Inst.setOperand(i, V);
        }
    }
    LocalUpdates.clear();
  }

  // Now that we've collected all of the interesting heap object values that are
  // passed into argument-returning functions, rewrite uses of these pointers
  // with optimized lifetime-shorted versions of it.
  for (Value *Ptr : DefOrder) {
    // If Ptr is an instruction, remember its block.  If not, use the entry
    // block as its block (it must be an argument, constant, etc).
    BasicBlock *PtrBlock;
    if (Instruction *PI = dyn_cast<Instruction>(Ptr))
      PtrBlock = PI->getParent();
    else
      PtrBlock = &F.getEntryBlock();

    TinyPtrVector<Instruction*> &Defs = DefsOfValue[Ptr];
    // This is the same problem as SSA construction, so we just use LLVM's
    // SSAUpdater, with each retain as a definition of the virtual value.
    SSAUpdater Updater;
    Updater.Initialize(Ptr->getType(), Ptr->getName());

    // Set the return value of each of these calls as a definition of the
    // virtual value.
    for (auto D : Defs)
      Updater.AddAvailableValue(D->getParent(), D);

    // If we didn't add a definition for Ptr's block, then Ptr itself is
    // available in its block.
    if (!Updater.HasValueForBlock(PtrBlock))
      Updater.AddAvailableValue(PtrBlock, Ptr);


    // Rewrite uses of Ptr to their optimized forms.
    for (auto UI = Ptr->user_begin(), E = Ptr->user_end(); UI != E; ) {
      // Make sure to increment the use iterator before potentially rewriting
      // it.
      Use &U = UI.getUse();
      ++UI;

      // If the use is in the same block that defines it and the User is not a
      // PHI node, then this is a local use that shouldn't be rewritten.
      Instruction *User = cast<Instruction>(U.getUser());
      if (User->getParent() == PtrBlock && !isa<PHINode>(User))
        continue;

      // Otherwise, change it if profitable!
      Updater.RewriteUse(U);

      if (U.get() != Ptr)
        Changed = true;
    }
  }

  // Scan through all the returns to see if there are any that can be optimized.
  // FIXME: swift_retainAndReturnThree runtime call
  // is currently implemented only on x86_64.
  // FIXME: optimizeReturn3() implementation assumes 64-bit
  if (llvm::Triple(F.getParent()->getTargetTriple()).getArchName() == "x86_64")
    for (ReturnInst *RI : Returns)
      Changed |= optimizeReturn3(RI);

  return Changed;
}


namespace llvm {
  void initializeSwiftARCExpandPassPass(PassRegistry&);
}

char SwiftARCExpandPass::ID = 0;
INITIALIZE_PASS(SwiftARCExpandPass,
                "swift-arc-expand", "Swift ARC expansion", false, false)

llvm::FunctionPass *swift::createSwiftARCExpandPass() {
  initializeSwiftARCExpandPassPass(*llvm::PassRegistry::getPassRegistry());
  return new SwiftARCExpandPass();
}
