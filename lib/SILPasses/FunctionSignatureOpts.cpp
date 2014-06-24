//===-- FunctionSignatureOpts.cpp - Optimizes function signatures ---------===//
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

#define DEBUG_TYPE "sil-function-signature-opts"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/Optional.h"
#include "swift/Basic/NullablePtr.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILDebugScope.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include <type_traits>

using namespace swift;

STATISTIC(NumFunctionSignaturesOptimized, "Total func sig optimized");
STATISTIC(NumDeadArgsEliminated, "Total dead args eliminated");
STATISTIC(NumCallSitesOptimized, "Total call sites optimized");

//===----------------------------------------------------------------------===//
//                             Argument Analysis
//===----------------------------------------------------------------------===//

namespace {

struct ArgumentDescriptor {
  SILArgument *Arg;
  bool IsDead;

  ArgumentDescriptor() = default;
  ArgumentDescriptor(SILArgument *A) : Arg(A), IsDead(A->use_empty()) {}
};
static_assert(std::is_pod<ArgumentDescriptor>::value,
              "Argument descriptor should be a POD");
} // end anonymous namespace

/// This function goes through the arguments of F and sees if we have anything
/// to optimize in which case it returns true. If we have nothing to optimize,
/// it returns false.
static bool
analyzeArguments(SILFunction *F,
                 llvm::SmallVectorImpl<ArgumentDescriptor> &ArgDescList) {
  // For now ignore functions with indirect results.
  if (F->getLoweredFunctionType()->hasIndirectResult())
    return false;

  bool ShouldOptimize = false;

  for (SILArgument *Arg : F->begin()->getBBArgs()) {
    ArgumentDescriptor A{Arg};

    if (A.IsDead) {
      ShouldOptimize = true;
      ++NumDeadArgsEliminated;
    }

    ArgDescList.push_back(A);
  }

  return ShouldOptimize;
}

//===----------------------------------------------------------------------===//
//                                  Mangling
//===----------------------------------------------------------------------===//

static void createNewName(SILFunction &OldF, ArrayRef<ArgumentDescriptor> Args,
                          llvm::SmallString<64> &Name) {
  llvm::raw_svector_ostream buffer(Name);

  // OS for optimized signature.
  buffer << "_TTOS_";

  // For every argument, put in what we are going to do to that arg in the
  // signature. The key is:
  //
  // 'n'   => We did nothing to the argument.
  // 'd'   => The argument was dead and will be removed.
  // 'a2v' => Was a loadable address and we promoted it to a value.
  // 'o2u' => Was an @owned argument, but we changed it to be an unowned
  //          parameter.
  // 'o2g' => Was an @owned argument, but we changed it to be a gauranteed
  //          parameter.
  // 's'   => Was a loadable value that we exploded into multiple arguments.
  //
  // Currently we only use 'n' and 'd' since we do not perform the other
  // optimizations.
  //
  // *NOTE* The gauranteed optimization requires knowledge to be taught to the
  // ARC optimizer among other passes in order to gaurantee safety. That or you
  // need to insert a fix_lifetime (swift_keepAlive) call to make sure we do not
  // eliminate the retain, release surrounding the call site in the caller.
  //
  // Additionally we use a packed signature since at this point we don't need
  // any '_'. The fact that we can run this optimization multiple times makes me
  // worried about long symbol names so I am trying to keep the symbol names as
  // short as possible especially in light of this being applied to specialized
  // functions.
  for (const ArgumentDescriptor &Arg : Args) {
    // If this arg is dead, add 'd' to the packed signature and continue.
    if (Arg.IsDead) {
      buffer << 'd';
      continue;
    }

    // Otherwise we are doing nothing so add 'n' to the packed signature.
    buffer << 'n';
  }

  buffer << '_' << OldF.getName();
}

//===----------------------------------------------------------------------===//
//                        Optimized Function Creation
//===----------------------------------------------------------------------===//

namespace {
class FunctionSignatureOptCloner
    : public SILClonerWithScopes<FunctionSignatureOptCloner> {
  using SuperTy = SILClonerWithScopes<FunctionSignatureOptCloner>;
  friend class SILVisitor<FunctionSignatureOptCloner>;
  friend class SILCloner<FunctionSignatureOptCloner>;

  SILFunction &Original;
  ArrayRef<ArgumentDescriptor> ArgDescriptors;

  FunctionSignatureOptCloner(SILFunction &Original,
                             ArrayRef<ArgumentDescriptor> ArgDescriptors,
                             StringRef NewName)
      : SuperTy(*initCloned(Original, ArgDescriptors, NewName)),
        Original(Original), ArgDescriptors(ArgDescriptors) {}

public:
  static SILFunction *cloneFunction(SILFunction *F,
                                    ArrayRef<ArgumentDescriptor> Args,
                                    StringRef NewName) {
    FunctionSignatureOptCloner C(*F, Args, NewName);
    C.populateCloned();
    ++NumFunctionSignaturesOptimized;
    return C.getCloned();
  };

protected:
  // Remap the value, handling SILArguments specially given our argument
  // descriptors.
  SILValue remapValue(SILValue Value) {
    // If we don't have a SILArgument, just call to our parent class.
    SILArgument *A = dyn_cast<SILArgument>(Value);
    if (!A)
      return SuperTy::remapValue(Value);

    // Grab our argument descriptor.
    Optional<ArgumentDescriptor> D = argDescriptorForArgument(A);

    // If this is not an argument we are tracking (i.e. from the first BB),
    // just remap the value.
    if (!D)
      return SuperTy::remapValue(Value);

    // Ok, we have a first BB value (i.e. a function arg). First make sure that
    // we are not attempting to remap a dead argument. A dead argument should
    // never have the opporunity to be remapped.
    assert(!D->IsDead && "We should never attempt to remap a dead argument");

    // Otherwise, we did not perform any optimizations to this argument. Just
    // remap it.
    return SuperTy::remapValue(Value);
  }

private:
  // Do a quick search for the Argument \p A. If A is not in the first BB, we
  // return None. If we are in the first BB, we return an optional containing
  // the argument descriptor.
  Optional<ArgumentDescriptor> argDescriptorForArgument(SILArgument *A) {
    for (auto &Arg : ArgDescriptors)
      if (Arg.Arg == A)
        return Arg;
    return Nothing_t::Nothing;
  }

  static SILFunction *initCloned(SILFunction &Orig,
                                 ArrayRef<ArgumentDescriptor> Args,
                                 StringRef NewName);

  /// Clone the body of the function into the empty function that was created
  /// by initCloned.
  void populateCloned();
  SILFunction *getCloned() { return &getBuilder().getFunction(); }
};

} // end anonymous namespace

static SILLinkage getOptimizedLinkage(SILLinkage L) {
  switch (L) {
  case SILLinkage::Public:
  case SILLinkage::PublicExternal:
  case SILLinkage::Shared:
  case SILLinkage::Hidden:
  case SILLinkage::HiddenExternal:
    // Specializations of public or hidden symbols can be shared by all TUs
    // that specialize the definition.
    return SILLinkage::Shared;

  case SILLinkage::Private:
    // Specializations of private symbols should remain so.
    return SILLinkage::Private;
  }
}

SILFunction *FunctionSignatureOptCloner::initCloned(
    SILFunction &Orig, ArrayRef<ArgumentDescriptor> Args, StringRef NewName) {
  SILModule &M = Orig.getModule();

  // TODO: Change this to always be shared perhaps.
  SILLinkage OptimizedLinkage = getOptimizedLinkage(Orig.getLinkage());

  // Create the new optimized function type.
  CanSILFunctionType OldFTy = Orig.getLoweredFunctionType();
  const ASTContext &Ctx = M.getASTContext();

  SmallVector<SILParameterInfo, 4> InterfaceParams;
  ArrayRef<SILParameterInfo> ParameterInfo = OldFTy->getInterfaceParameters();
  for (unsigned i = 0, e = ParameterInfo.size(); i != e; ++i) {
    if (Args[i].IsDead)
      continue;
    InterfaceParams.push_back(ParameterInfo[i]);
  }
  SILResultInfo InterfaceResult = OldFTy->getInterfaceResult();
  CanSILFunctionType NewFTy = SILFunctionType::get(
      OldFTy->getGenericSignature(), OldFTy->getExtInfo(),
      OldFTy->getCalleeConvention(), InterfaceParams, InterfaceResult, Ctx);

  // Create the new function.
  SILFunction *NewF = SILFunction::create(
      M, OptimizedLinkage, NewName, NewFTy, nullptr, Orig.getLocation(),
      Orig.isBare(), Orig.isTransparent(), Orig.isNoinline(),
      0, Orig.getDebugScope(), Orig.getDeclContext());

  // Return our newly created F for cloning.
  return NewF;
}

void FunctionSignatureOptCloner::populateCloned() {
  SILFunction *Cloned = getCloned();
  SILModule &M = Cloned->getModule();

  // Create arguments for the entry block.
  SILBasicBlock *OrigEntryBB = Original.begin();
  SILBasicBlock *ClonedEntryBB = new (M) SILBasicBlock(Cloned);

  // Create the entry basic block with the function arguments.
  for (size_t i = 0, e = OrigEntryBB->bbarg_size(); i != e; ++i) {
    // If we have a dead argument, don't create an argument for it.
    if (ArgDescriptors[i].IsDead)
      continue;

    // We could grab this from ArgDescriptors[i], but it makes more sense to
    // keep this independent of the argument descriptors.
    SILArgument *Arg = OrigEntryBB->getBBArg(i);

    // Otherwise create our mapped value.
    SILValue MappedValue = new (M)
        SILArgument(remapType(Arg->getType()), ClonedEntryBB, Arg->getDecl());
    ValueMap.insert(std::make_pair(Arg, MappedValue));
  }

  getBuilder().setInsertionPoint(ClonedEntryBB);
  BBMap.insert(std::make_pair(OrigEntryBB, ClonedEntryBB));
  // Recursively visit original BBs in depth-first preorder, starting with the
  // entry block, cloning all instructions other than terminators.
  visitSILBasicBlock(OrigEntryBB);

  // Now iterate over the BBs and fix up the terminators.
  for (auto BI = BBMap.begin(), BE = BBMap.end(); BI != BE; ++BI) {
    getBuilder().setInsertionPoint(BI->second);
    visit(BI->first->getTerminator());
  }
}

//===----------------------------------------------------------------------===//
//                                Main Routine
//===----------------------------------------------------------------------===//

/// This function takes in an old function OldF and a new function NewF that
/// contains the body of OldF that we moved previously into NewF. Then we create
/// a new body for OldF that just marshalls data from its arguments to the args
/// of NewF and calls NewF.
static void convertOldFunctionToThunk(SILFunction *OldF, SILFunction *NewF,
                                      ArrayRef<ArgumentDescriptor> ArgDescs) {
  // Then drop all references for the instructions in the first basic block to
  // handle any forward references. Currently the only way this can happen is
  // via the terminator, but there is no reason not to be careful here.
  SILBasicBlock &FirstBB = OldF->front();
  for (auto II = std::prev(FirstBB.end()), IE = FirstBB.begin(); II != IE;) {
    SILInstruction &I = *II--;
    I.eraseFromParent();
  }
  FirstBB.begin()->eraseFromParent();

  // Visit each basic block BB and drop all references BB or any instruction
  // that it contains may have to any other basic block. This enables us to just
  // perform eraseFromParent on basic blocks without needing to disentangle
  // use-def lists. We do not need to worry about any references to the first
  // basic block since by definition the first BB must dominate all BB implying
  // that such BB have no way to reference the first BB.
  for (auto BI = std::prev(OldF->end()), BE = OldF->begin(); BI != BE;) {
    SILBasicBlock *BB = BI;
    --BI;
    BB->dropAllReferences();
  }

  // Eliminate all basic blocks except for the first basic block. We only need
  // one basic block for our thunk.
  for (auto BI = std::prev(OldF->end()), BE = OldF->begin(); BI != BE;) {
    SILBasicBlock *BB = BI;
    --BI;
    BB->eraseFromParent();
  }

  // Create the new thunk basic block.
  SILLocation Loc = OldF->getLocation(); // TODO: What is the proper location
                                         // to use here?
  SILBuilder Builder(&FirstBB);
  FunctionRefInst *FRI = Builder.createFunctionRef(Loc, NewF);

  // Create the args for the thunk's apply, ignoring any dead arguments.
  llvm::SmallVector<SILValue, 8> ThunkArgs;
  for (unsigned i = 0, e = ArgDescs.size(); i != e; ++i) {
    if (ArgDescs[i].IsDead)
      continue;
    ThunkArgs.push_back(FirstBB.getBBArg(i));
  }

  // We are ignoring generic functions and functions with out parameters for
  // now.
  SILType LoweredType = NewF->getLoweredType();
  SILType ResultType = LoweredType.getFunctionInterfaceResultType();
  SILValue ReturnValue = Builder.createApply(Loc, FRI, LoweredType, ResultType,
                                             ArrayRef<Substitution>(),
                                             ThunkArgs, NewF->isTransparent());
  Builder.createReturn(Loc, ReturnValue);
}

/// This function takes in OldF and all callsites of OldF and rewrites the
/// callsites to call the new function.
static void
rewriteApplyInstToCallNewFunction(SILFunction *OldF, SILFunction *NewF,
                                  ArrayRef<ArgumentDescriptor> ArgDescs,
                                  SmallPtrSet<ApplyInst *, 4> &CallSites) {
  for (auto *AI : CallSites) {
    SILBuilder Builder(AI);

    FunctionRefInst *FRI = Builder.createFunctionRef(AI->getLoc(), NewF);

    // Create the args for the new apply, ignoring any dead arguments.
    llvm::SmallVector<SILValue, 8> NewArgs;
    for (unsigned i = 0, e = ArgDescs.size(); i != e; ++i) {
      if (ArgDescs[i].IsDead)
        continue;
      NewArgs.push_back(AI->getOperand(i));
    }

    // We are ignoring generic functions and functions with out parameters for
    // now.
    SILType LoweredType = NewF->getLoweredType();
    SILType ResultType = LoweredType.getFunctionInterfaceResultType();

    // Create the new apply.
    ApplyInst *NewAI = Builder.createApply(AI->getLoc(), FRI, LoweredType,
                                           ResultType, ArrayRef<Substitution>(),
                                           NewArgs, NewF->isTransparent());

    // Replace all uses of the old apply with the new apply.
    AI->replaceAllUsesWith(NewAI);

    // Erase the old apply.
    AI->eraseFromParent();

    ++NumCallSitesOptimized;
  }
}

/// This function takes in a SILFunction F and its callsites in the current
/// module and produces a new SILFunction that has the body of F but with
/// optimized function arguments. F is changed to be a thunk that calls NewF to
/// reduce code duplication in cases where we missed a callsite to F. The
/// function returns true if we were successful in creating the new function and
/// returns false otherwise.
static bool
optimizeFunctionSignature(SILFunction *F,
                          llvm::SmallPtrSet<ApplyInst *, 4> &CallSites) {
  DEBUG(llvm::dbgs() << "Optimizing Function Signature of " << F->getName()
                     << "\n");

  // If F has no body, bail...
  if (F->empty()) {
    DEBUG(llvm::dbgs() << "    Has no body... Bailing!\n");
    return false;
  }

  // For now ignore generic functions to keep things simple...
  if (F->getLoweredFunctionType()->isPolymorphic()) {
    DEBUG(llvm::dbgs() << "    Polymorphic function... Bailing!\n");
    return false;
  }

  // An array containing our ArgumentDescriptor objects that contain information
  // from our analysis.
  llvm::SmallVector<ArgumentDescriptor, 8> Arguments;

  // Analyze function arguments. If there is no work to be done, exit early.
  if (!analyzeArguments(F, Arguments)) {
    DEBUG(llvm::dbgs() << "    Has no optimizable arguments... "
                          "bailing...\n");
    return false;
  }

  DEBUG(llvm::dbgs() << "    Has optimizable arguments... Performing "
                        "optimizations...\n");

  llvm::SmallString<64> NewFName;
  createNewName(*F, Arguments, NewFName);

  SILFunction *NewF;
  // Ok, we have optimizations that we can perform here. First attempt to look
  // up the optimized function from the module if we already created it in a
  // previous pass manager iteration.
  if (!(NewF = F->getModule().lookUpFunction(NewFName))) {
    // Otherwise, create the new function and transfer the current function over
    // to that function.
    NewF = FunctionSignatureOptCloner::cloneFunction(F, Arguments, NewFName);
  }

  // Change the old function into a thunk.
  convertOldFunctionToThunk(F, NewF, Arguments);

  // Rewrite all apply inst to be to the new function.
  rewriteApplyInstToCallNewFunction(F, NewF, Arguments, CallSites);

  return true;
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

namespace {

class FunctionSignatureOpts : public SILModuleTransform {
public:
  FunctionSignatureOpts() {}

  void run() {
    SILModule *M = getModule();

    DEBUG(llvm::dbgs() << "**** Optimizing Function Signatures ****\n\n");

    // Construct a map from Callee -> Call Site Set.
    llvm::DenseMap<SILFunction *, llvm::SmallPtrSet<ApplyInst *, 4>>
    CalleeToCallSiteSetMap;
    std::vector<SILFunction *> FunctionsToVisit;
    for (auto &Caller : *M) {
      FunctionsToVisit.push_back(&Caller);
      for (auto &BB : Caller) {
        for (auto &I : BB) {
          auto *AI = dyn_cast<ApplyInst>(&I);
          if (!AI)
            continue;

          auto *FRI = dyn_cast<FunctionRefInst>(AI->getCallee());
          if (!FRI)
            continue;

          SILFunction *Callee = FRI->getReferencedFunction();
          CalleeToCallSiteSetMap[Callee].insert(AI);
        }
      }
    }

    // Process each function in the callgraph that we are able to optimize.
    //
    // TODO: Determine if it is profitable to always perform this optimization
    // even if a function is not called locally. As far as we can tell. Down the
    // line more calls may be exposed and the inliner might be able to handle
    // those calls.
    bool Changed = false;
    for (auto P : CalleeToCallSiteSetMap) {
      SILFunction *F = P.first;
      Changed |= optimizeFunctionSignature(F, CalleeToCallSiteSetMap[F]);
    }

    // If we changed anything, invalidate the call graph.
    if (Changed)
      invalidateAnalysis(SILAnalysis::InvalidationKind::CallGraph);
  }

  StringRef getName() override { return "Function Signature Optimization"; }
};
} // end anonymous namespace

SILTransform *swift::createFunctionSignatureOpts() {
  return new FunctionSignatureOpts();
}
