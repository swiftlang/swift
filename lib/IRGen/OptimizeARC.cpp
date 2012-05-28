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

#include "IRGen.h"
#include "llvm/Instructions.h"
#include "llvm/Pass.h"
#include "llvm/Transforms/Utils/SSAUpdater.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Support/InstIterator.h"
using namespace llvm;

//===----------------------------------------------------------------------===//
//                            Utility Functions
//===----------------------------------------------------------------------===//

enum RT_Kind {
  /// This is not a runtime function that we support.  Maybe it is not a call,
  /// or is a call to something we don't care about.
  RT_Unknown,
  
  /// SwiftHeapObject *swift_retain(SwiftHeapObject *object)
  RT_Retain,
  
  /// void swift_release(SwiftHeapObject *object)
  RT_Release,
  
  /// SwiftHeapObject *swift_allocObject(SwiftHeapMetadata *metadata,
  ///                                    size_t size, size_t alignment)
  RT_AllocObject,
};

/// classifyInstruction - Take a look at the specified instruction and classify
/// it into what kind of runtime entrypoint it is, if any.
static RT_Kind classifyInstruction(Instruction *I) {
  // Non-calls or calls to indirect functions are unknown.
  CallInst *CI = dyn_cast<CallInst>(I);
  if (CI == 0) return RT_Unknown;
  Function *F = CI->getCalledFunction();
  if (F == 0) return RT_Unknown;
  
  return StringSwitch<RT_Kind>(F->getName())
    .Case("swift_retain", RT_Retain)
    .Case("swift_release", RT_Release)
    .Case("swift_allocObject", RT_AllocObject)
    .Default(RT_Unknown);
}

//===----------------------------------------------------------------------===//
//                      Return Argument Canonicalizer
//===----------------------------------------------------------------------===//

/// canonicalizeArgumentReturnFunctions - Functions like swift_retain return an
/// argument as a low-level performance optimization.  This makes it difficult
/// to reason about pointer equality though, so undo it as an initial
/// canonicalization step.
///
/// This also does some trivial peep-hole optimizations as we go.
static bool canonicalizeArgumentReturnFunctions(Function &F) {
  bool Changed = false;
  for (inst_iterator I = inst_begin(&F), E = inst_end(&F); I != E; ) {
    Instruction *Inst = &*I++;
    
    switch (classifyInstruction(Inst)) {
    case RT_Unknown:
    case RT_AllocObject:
      break;
    case RT_Retain: {
      CallInst &CI = cast<CallInst>(*Inst);
      Value *ArgVal = CI.getArgOperand(0);
        
      // Ignore functions with no uses.
      if (!Inst->use_empty()) {
        // swift_retain returns its first argument.
        Inst->replaceAllUsesWith(ArgVal);
        Changed = true;
      }
      
      // retain of null is a no-op.
      if (isa<ConstantPointerNull>(ArgVal)) {
        CI.eraseFromParent();
        Changed = true;
        continue;
      }
      break;
    }
    case RT_Release: {
      CallInst &CI = cast<CallInst>(*Inst);
      // swift_release(null) is a noop, zap it. 
      Value *ArgVal = CI.getArgOperand(0);
      if (isa<ConstantPointerNull>(ArgVal)) {
        CI.eraseFromParent();
        Changed = true;
        continue;
      }
    }
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
    for (Instruction &Inst : BB) {
      switch (classifyInstruction(&Inst)) {
      case RT_Retain: {  // swift_retain returns its first argument.
        CallInst &CI = cast<CallInst>(Inst);
        Value *ArgVal = CI.getArgOperand(0);
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
        
        LocalEntry = &Inst;
        GlobalEntry.push_back(&Inst);
        break;
      }
      case RT_Unknown:
      case RT_Release:
      case RT_AllocObject:
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
  
  // Finally, rewrite remaining heap object uses to make use of the implicit
  // copy that swift_retain and similar functions perform.
  Changed |= optimizeArgumentReturnFunctions(F);
  return Changed;
}
