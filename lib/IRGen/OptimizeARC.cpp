//===--- OptimizeARC.cpp - Reference Counting Optimizations ---------------===//
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
// This file implements optimizations for reference counting, object
// allocation, and other runtime entrypoints.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "swift-optimize"
#include "IRGen.h"
#include "llvm/Instructions.h"
#include "llvm/Module.h"
#include "llvm/Pass.h"
#include "llvm/Transforms/Utils/SSAUpdater.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Support/InstIterator.h"
using namespace llvm;

STATISTIC(NumNoopDeleted,
          "Number of no-op swift calls eliminated");
STATISTIC(NumRetainReleasePairs,
          "Number of swift retain/release pairs eliminated");
STATISTIC(NumAllocateReleasePairs,
          "Number of swift allocate/release pairs eliminated");

//===----------------------------------------------------------------------===//
//                            Utility Functions
//===----------------------------------------------------------------------===//

enum RT_Kind {
  /// An instruction with this classification is known to not access (read or
  /// write) memory.
  RT_NoMemoryAccessed,
  
  /// SwiftHeapObject *swift_retain(SwiftHeapObject *object)
  RT_Retain,
  
  // void swift_retain_noresult(SwiftHeapObject *object)
  RT_RetainNoResult,
  
  /// void swift_release(SwiftHeapObject *object)
  RT_Release,
  
  /// SwiftHeapObject *swift_allocObject(SwiftHeapMetadata *metadata,
  ///                                    size_t size, size_t alignment)
  RT_AllocObject,
  
  /// This is not a runtime function that we support.  Maybe it is not a call,
  /// or is a call to something we don't care about.
  RT_Unknown,
};

/// classifyInstruction - Take a look at the specified instruction and classify
/// it into what kind of runtime entrypoint it is, if any.
static RT_Kind classifyInstruction(const Instruction &I) {
  if (!I.mayReadOrWriteMemory())
    return RT_NoMemoryAccessed;
  
  // Non-calls or calls to indirect functions are unknown.
  const CallInst *CI = dyn_cast<CallInst>(&I);
  if (CI == 0) return RT_Unknown;
  Function *F = CI->getCalledFunction();
  if (F == 0) return RT_Unknown;
  
  return StringSwitch<RT_Kind>(F->getName())
    .Case("swift_retain", RT_Retain)
    .Case("swift_retain_noresult", RT_RetainNoResult)
    .Case("swift_release", RT_Release)
    .Case("swift_allocObject", RT_AllocObject)
    .Default(RT_Unknown);
}

/// getRetain - Return a callable function for swift_retain.  F is the function
/// being operated on, ObjectPtrTy is an instance of the object pointer type to
/// use, and Cache is a null-initialized place to make subsequent requests
/// faster.
static Constant *getRetain(Function &F, Type *ObjectPtrTy, Constant *&Cache) {
  if (Cache) return Cache;
  Module *M = F.getParent();
  return Cache = M->getOrInsertFunction("swift_retain",
                                        ObjectPtrTy, ObjectPtrTy, NULL);
}

/// getRetainNoResult - Return a callable function for swift_retain_noresult.
/// F is the function being operated on, ObjectPtrTy is an instance of the
/// object pointer type to use, and Cache is a null-initialized place to make
/// subsequent requests faster.
static Constant *getRetainNoResult(Function &F, Type *ObjectPtrTy,
                                   Constant *&Cache) {
  if (Cache) return Cache;
 
  AttributeWithIndex NoCapture = { Attribute::NoCapture, 1 };
  auto AttrList = AttrListPtr::get(NoCapture);
  Module *M = F.getParent();
  return Cache = M->getOrInsertFunction("swift_retain_noresult",
                                        AttrList,
                                        Type::getVoidTy(F.getContext()),
                                        ObjectPtrTy, NULL);
}


//===----------------------------------------------------------------------===//
//                      Return Argument Canonicalizer
//===----------------------------------------------------------------------===//

/// canonicalizeArgumentReturnFunctions - Functions like swift_retain return an
/// argument as a low-level performance optimization.  This makes it difficult
/// to reason about pointer equality though, so undo it as an initial
/// canonicalization step.  After this step, all swift_retain's have been
/// replaced with swift_retain_noresult.
///
/// This also does some trivial peep-hole optimizations as we go.
static bool canonicalizeArgumentReturnFunctions(Function &F) {
  Constant *RetainNoResultCache = 0;
  
  bool Changed = false;
  for (auto &BB : F)
  for (auto I = BB.begin(); I != BB.end(); ) {
    Instruction &Inst = *I++;
    
    switch (classifyInstruction(Inst)) {
    case RT_Unknown:
    case RT_AllocObject:
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
      // x = y; swift_retain_noresult(y).
      CallInst &CI = cast<CallInst>(Inst);
      Value *ArgVal = CI.getArgOperand(0);

      // Rewrite uses of the result to use the argument.
      if (!CI.use_empty()) {
        Inst.replaceAllUsesWith(ArgVal);
        Changed = true;
      }
      
      // Insert a call to swift_retain_noresult to replace this and reset the
      // iterator so that we visit it next.
      I = CallInst::Create(getRetainNoResult(F, ArgVal->getType(),
                                             RetainNoResultCache),
                           ArgVal, "", &CI);
      CI.eraseFromParent();
      Changed = true;
      break;
    }
    case RT_Release: {
      CallInst &CI = cast<CallInst>(Inst);
      // swift_release(null) is a noop, zap it. 
      Value *ArgVal = CI.getArgOperand(0);
      if (isa<ConstantPointerNull>(ArgVal)) {
        CI.eraseFromParent();
        Changed = true;
        ++NumNoopDeleted;
        continue;
      }
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
  // doesn't have convenient oneof's.
  Value *ReleasedObject = Release.getArgOperand(0);
  
  BasicBlock::iterator BBI = &Release;
  
  // Scan until we get to the top of the block.
  while (BBI != BB.begin()) {
    --BBI;
  
    // Don't analyze PHI nodes.  We can't move retains before them and they 
    // aren't "interesting".
    if (isa<PHINode>(BBI)) {
      ++BBI;
      goto OutOfLoop;
    }
    
    switch (classifyInstruction(*BBI)) {
    case RT_Retain: // Canonicalized away, shouldn't exist.
      assert(0 && "swift_retain should be canonicalized away");
    case RT_NoMemoryAccessed:
      // Skip over random instructions that don't touch memory.  They don't need
      // protection by retain/release.
      continue;
        
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

    case RT_RetainNoResult: {  // swift_retain_noresult(obj)
      CallInst &Retain = cast<CallInst>(*BBI);
      Value *RetainedObject = Retain.getArgOperand(0);
      
      // If the retain and release are to obviously pointer-equal objects, then
      // we can delete both of them.  We have proven that they do not protect
      // anything of value.
      if (RetainedObject == ReleasedObject) {
        // Note: this assumes the retain was properly canonicalized!
        Retain.eraseFromParent();
        Release.eraseFromParent();
        ++NumRetainReleasePairs;
        return true;
      }
      
      // Otherwise, this is a retain of an object that is not statically known
      // to be the same object.  It may still be dynamically the same object
      // though.  In this case, we can't move the release past it.
      // TODO: Strengthen analysis.
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

    case RT_Unknown:
      // BBI->dump();
      // Otherwise, we get to something unknown/unhandled.  Bail out for now.
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

/// performReleaseMotion - this moves releaes functions earlier, past
/// instructions that are known to not access an object.  If they are moved to
/// touch a retain of the same object, destructive annihilation occurs!
static bool performReleaseMotion(Function &F) {
  bool Changed = false;
  
  // TODO: This is a really trivial local algorithm.  It could be much better.
  for (BasicBlock &BB : F) {
    for (auto BBI = BB.begin(), E = BB.end(); BBI != E; ) {
      // Preincrement the iterator to avoid invalidation and out trouble.
      Instruction &I = *BBI++;
      
      // Ignore instructions that are not releases.  Try to optimize ones that
      // are.
      if (classifyInstruction(I) == RT_Release)
        Changed |= performLocalReleaseMotion(cast<CallInst>(I), BB);
    }
  }
  return Changed;
}


//===----------------------------------------------------------------------===//
//                      Return Argument Optimizer
//===----------------------------------------------------------------------===//

/// optimizeArgumentReturnFunctions - Functions like swift_retain return an
/// argument as a low-level performance optimization.  Manually make use of this
/// to reduce register pressure.
///
/// Coming into this function, we assume that the code is in canonical form:
/// none of these calls have any uses of their return values.
static bool optimizeArgumentReturnFunctions(Function &F) {
  Constant *RetainCache = nullptr;
  bool Changed = false;
  
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
      case RT_Retain: assert(0 && "This should be canonicalized away!");
      case RT_RetainNoResult: {
        Value *ArgVal = cast<CallInst>(Inst).getArgOperand(0);

        // First step: rewrite swift_retain_noresult to swift_retain, exposing
        // the result value.
        CallInst &CI =
           *CallInst::Create(getRetain(F, ArgVal->getType(), RetainCache),
                             ArgVal, "", &Inst);
        Inst.eraseFromParent();

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
        break;
      }
      case RT_Unknown:
      case RT_Release:
      case RT_AllocObject:
      case RT_NoMemoryAccessed:
        // Check to see if there are any uses of a value in the LocalUpdates
        // map.  If so, remap it now to the locally defined version.
        for (unsigned i = 0, e = Inst.getNumOperands(); i != e; ++i)
          if (Value *V = LocalUpdates.lookup(Inst.getOperand(i))) {
            Changed = true;
            Inst.setOperand(i, V);
          }
        break;
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
    for (auto UI = Ptr->use_begin(), E = Ptr->use_end(); UI != E; ) {
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
  
  return Changed;
}

//===----------------------------------------------------------------------===//
//                            SwiftARCOpt Pass
//===----------------------------------------------------------------------===//

namespace {
  class SwiftARCOpt : public FunctionPass {
    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesCFG();
    }
    virtual bool runOnFunction(Function &F);
    
  public:
    static char ID;
    SwiftARCOpt() : FunctionPass(ID) {
      initializeObjCARCExpandPass(*PassRegistry::getPassRegistry());
    }
  };
}
namespace llvm {
  void initializeSwiftARCOptPass(PassRegistry&);
}

char SwiftARCOpt::ID = 0;
INITIALIZE_PASS(SwiftARCOpt,
                "swift-arc-optimize", "Swift ARC optimization", false, false)

// Optimization passes.
llvm::FunctionPass *swift::irgen::createSwiftARCOptPass() {
  return new SwiftARCOpt();
}


bool SwiftARCOpt::runOnFunction(Function &F) {
  bool Changed = false;
  // First thing: canonicalize swift_retain and similar calls so that nothing
  // uses their result.  This exposes the copy that the function does to the
  // optimizer.
  Changed |= canonicalizeArgumentReturnFunctions(F);
  
  // Next, perform release() motion, eliminating retain/release pairs when it
  // turns out that a pair is not protecting anything that accesses the guarded
  // heap object.
  Changed |= performReleaseMotion(F);
  
  // Finally, rewrite remaining heap object uses to make use of the implicit
  // copy that swift_retain and similar functions perform.
  Changed |= optimizeArgumentReturnFunctions(F);
  return Changed;
}
