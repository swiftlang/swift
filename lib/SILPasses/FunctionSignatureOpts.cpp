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
#include "swift/SILAnalysis/CallGraphAnalysis.h"
#include "swift/SILAnalysis/RCIdentityAnalysis.h"
#include "swift/SILAnalysis/ARCAnalysis.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/Range.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILDebugScope.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include <type_traits>

using namespace swift;

STATISTIC(NumFunctionSignaturesOptimized, "Total func sig optimized");
STATISTIC(NumDeadArgsEliminated, "Total dead args eliminated");
STATISTIC(NumOwnedConvertedToGuaranteed, "Total owned args -> guaranteed args");
STATISTIC(NumCallSitesOptimized, "Total call sites optimized");

//===----------------------------------------------------------------------===//
//                             Argument Analysis
//===----------------------------------------------------------------------===//

namespace {

/// A structure that maintains all of the information about a specific
/// SILArgument that we are tracking.
///
/// TODO: Remove IsDead and CalleeRelease and make them properties of
/// ArgumentDescriptor based off of Arg.
struct ArgumentDescriptor {
  SILArgument *Arg;
  SILParameterInfo ParameterInfo;
  bool IsDead;
  SILInstruction *CalleeRelease;

  ArgumentDescriptor() = default;
  ArgumentDescriptor(SILArgument *A)
    : Arg(A), ParameterInfo(A->getParameterInfo()), IsDead(A->use_empty()),
      CalleeRelease() {}

  bool hasConvention(ParameterConvention P) const {
    return Arg->hasConvention(P);
  }
};

template <typename T1, typename T2>
inline T1 getFirstPairElt(const std::pair<T1, T2> &P) { return P.first; }

/// A class that contains all analysis information we gather about our
/// function. Also provides utility methods for creating the new empty function.
class FunctionAnalyzer {
  /// The function that we are analyzing.
  SILFunction *F;

  /// The rc identity analysis we are using to find rc identity roots of arc
  /// values.
  RCIdentityAnalysis *RCIA;

  /// Did we ascertain that we can optimize this function?
  bool ShouldOptimize;

  /// A list of structures which present a "view" of precompiled information on
  /// an argument that we will use during our optimization.
  llvm::SmallVector<ArgumentDescriptor, 8> ArgDescList;

  /// A list containing all dead argument indices. This is useful precompiled
  /// information that we learn early on but need later deep in the analysis
  llvm::SmallVector<unsigned, 8> DeadArgIndices;

  using ArgIndexInstMapTy =
    llvm::MapVector<unsigned, SILInstruction *,
                    llvm::SmallDenseMap<unsigned, unsigned, 8>,
                    llvm::SmallVector<std::pair<unsigned,
                                                SILInstruction *>, 8>>;
  /// A map from consumed SILArguments to the release associated with the
  /// argument.
  ///
  /// We purposefully use a map vector here since we want to ensure that we can
  /// reoutput the instructions in the same order in the callers which we see
  /// them in the callee. This is the key property preserved by the map vector.
  ArgIndexInstMapTy ConsumedArgumentIndexToReleaseMap;

public:
  FunctionAnalyzer() = delete;
  FunctionAnalyzer(const FunctionAnalyzer &) = delete;
  FunctionAnalyzer(FunctionAnalyzer &&) = delete;

  FunctionAnalyzer(SILFunction *F, RCIdentityAnalysis *RCIA)
    : F(F), RCIA(RCIA), ShouldOptimize(false) {}

  /// Analyze the given function.
  bool analyze();

  /// Returns the managled name of the function that should be generated from
  /// this function analyzer.
  llvm::SmallString<64> getOptimizedName();

  /// Create a new empty function with the optimized signature found by this
  /// analysis.
  ///
  /// *NOTE* This occurs in the same module as F.
  SILFunction *createEmptyFunctionWithOptimizedSig(llvm::SmallString<64> &Name);

  ArrayRef<ArgumentDescriptor> getArgDescList() const { return ArgDescList; }
  ArrayRef<unsigned> getDeadArgIndices() const { return DeadArgIndices; }

  Range<ArgIndexInstMapTy::reverse_iterator>
  getConsumedArgumentIndexReleasePairs() {
    return reversed(ConsumedArgumentIndexToReleaseMap);
  }

private:
  /// Compute the interface params of the optimized function.
  void
  computeOptimizedInterfaceParams(SmallVectorImpl<SILParameterInfo> &OutArray);

  /// Compute the CanSILFunctionType for the optimized function.
  CanSILFunctionType createOptimizedSILFunctionType();

  /// Populate ConsumedArgumentReleases with all releases associated with a
  /// consumed function argument.
  void findConsumedArgumentReleases();
};

} // end anonymous namespace

void
FunctionAnalyzer::findConsumedArgumentReleases() {
  // Find the return BB of F. If we fail, then bail.
  auto ReturnBB = F->findReturnBB();
  if (ReturnBB == F->end())
    return;

  for (auto II = std::next(ReturnBB->rbegin()), IE = ReturnBB->rend();
       II != IE; ++II) {
    // If we do not have a release_value or strong_release...
    if (!isa<ReleaseValueInst>(*II) && !isa<StrongReleaseInst>(*II)) {
      // And the object can not use values in a manner that will keep the object
      // alive, continue. We may be able to find additional releases.
      if (arc::canNeverUseValues(&*II))
        continue;

      // Otherwise, we need to stop computing since we do not want to reduce the
      // lifetime of objects.
      return;
    }

    // Ok, we have a release_value or strong_release. Grab Target and find the
    // RC identity root of its operand.
    SILInstruction *Target = &*II;
    SILValue Op = RCIA->getRCIdentityRoot(Target->getOperand(0));

    // If Op is not a consumed argument, we must break since this is not an Op
    // that is a part of a return sequence. We are being conservative here since
    // we could make this more general by allowing for intervening non-arg
    // releases in the sense that we do not allow for race conditions in between
    // destructors.
    auto *Arg = dyn_cast<SILArgument>(Op);
    if (!Arg || !Arg->isFunctionArg() ||
        !Arg->hasConvention(ParameterConvention::Direct_Owned))
      return;

    // Ok, we have a release on a SILArgument that is direct owned. Attempt to
    // put it into our arc opts map. If we already have it, we have exited the
    // return value sequence so break. Otherwise, continue looking for more arc
    // operations.
    if (!ConsumedArgumentIndexToReleaseMap.insert({Arg->getIndex(),
                                                   Target}).second)
      return;
  }
}

/// This function goes through the arguments of F and sees if we have anything
/// to optimize in which case it returns true. If we have nothing to optimize,
/// it returns false.
bool
FunctionAnalyzer::analyze() {
  // For now ignore functions with indirect results.
  if (F->getLoweredFunctionType()->hasIndirectResult())
    return false;

  // Search for all ARC sequence ops. This initializes the small map vector we
  // are using to store instructions. We are using this data structure on
  // purpose since it will allow us to easily recreate our arc opts in the same
  // order in caller functions.
  findConsumedArgumentReleases();

  ArrayRef<SILArgument *> Args = F->begin()->getBBArgs();
  for (unsigned i = 0, e = Args.size(); i != e; ++i) {
    ArgumentDescriptor A{Args[i]};

    if (A.IsDead) {
      ShouldOptimize = true;
      DeadArgIndices.push_back(i);
      ++NumDeadArgsEliminated;
    }

    // See if we can find a ref count equivalent strong_release or release_value
    // at the end of this function if our argument is an @owned parameter.
    if (A.hasConvention(ParameterConvention::Direct_Owned)) {
      auto P = ConsumedArgumentIndexToReleaseMap.find(i);
      if (P != ConsumedArgumentIndexToReleaseMap.end()) {
        assert(A.Arg->getIndex() == i &&
               "Make sure our argument index matches the current arg index.");
        A.CalleeRelease = P->second;
        ShouldOptimize = true;
        ++NumOwnedConvertedToGuaranteed;
      }
    }

    ArgDescList.push_back(A);
  }

  return ShouldOptimize;
}

//===----------------------------------------------------------------------===//
//                         Creating the New Function
//===----------------------------------------------------------------------===//

void
FunctionAnalyzer::
computeOptimizedInterfaceParams(SmallVectorImpl<SILParameterInfo> &OutArray) {
  CanSILFunctionType OldFTy = F->getLoweredFunctionType();
  ArrayRef<SILParameterInfo> ParameterInfo = OldFTy->getParameters();
  for (unsigned i = 0, e = ParameterInfo.size(); i != e; ++i) {
    // If we have a dead argument, skip it.
    if (ArgDescList[i].IsDead) {
      continue;
    }

    // If we found a release in the callee in the last BB on an @owned
    // parameter, change the parameter to @guaranteed and continue...
    if (ArgDescList[i].CalleeRelease) {
      const SILParameterInfo &OldInfo = ParameterInfo[i];
      assert(OldInfo.getConvention() == ParameterConvention::Direct_Owned &&
             "Can only transform @owned => @guaranteed in this code path");
      SILParameterInfo NewInfo(OldInfo.getType(),
                               ParameterConvention::Direct_Guaranteed);
      OutArray.push_back(NewInfo);
      continue;
    }

    // Otherwise just propagate through the parameter info.
    OutArray.push_back(ParameterInfo[i]);
  }
}

CanSILFunctionType
FunctionAnalyzer::createOptimizedSILFunctionType() {
  const ASTContext &Ctx = F->getModule().getASTContext();
  CanSILFunctionType FTy = F->getLoweredFunctionType();

  // The only way that we modify the arity of function parameters is here for
  // dead arguments. Doing anything else is unsafe since by definition non-dead
  // arguments will have SSA uses in the function. We would need to be smarter
  // in our moving to handle such cases.
  llvm::SmallVector<SILParameterInfo, 8> InterfaceParams;
  computeOptimizedInterfaceParams(InterfaceParams);

  SILResultInfo InterfaceResult = FTy->getResult();

  return SILFunctionType::get(FTy->getGenericSignature(),
                              FTy->getExtInfo(),
                              FTy->getCalleeConvention(),
                              InterfaceParams, InterfaceResult, Ctx);
}

SILFunction *
FunctionAnalyzer::
createEmptyFunctionWithOptimizedSig(llvm::SmallString<64> &NewFName) {
  SILModule &M = F->getModule();

  // Create the new optimized function type.
  CanSILFunctionType NewFTy = createOptimizedSILFunctionType();

  // Create the new function.
  auto *NewDebugScope = new (M) SILDebugScope(*F->getDebugScope());
  SILFunction *NewF = SILFunction::create(
      M, F->getLinkage(), NewFName, NewFTy, nullptr, F->getLocation(),
      F->isBare(), F->isTransparent(), F->isFragile(),
      F->getInlineStrategy(), F->getEffectsInfo(), 0,
      NewDebugScope, F->getDeclContext());
  NewF->setSemanticsAttr(F->getSemanticsAttr());
  NewDebugScope->SILFn = NewF;

  return NewF;
}

//===----------------------------------------------------------------------===//
//                                  Mangling
//===----------------------------------------------------------------------===//

static bool isSpecializedFunction(SILFunction &F) {
  return F.getName().startswith("_TTOS");
}

llvm::SmallString<64> FunctionAnalyzer::getOptimizedName() {
  llvm::SmallString<64> Name;

  {
    llvm::raw_svector_ostream buffer(Name);
    // OS for optimized signature.
    buffer << "_TTOS_";

    // For every argument, put in what we are going to do to that arg in the
    // signature. The key is:
    //
    // 'n'   => We did nothing to the argument.
    // 'd'   => The argument was dead and will be removed.
    // 'a2v' => Was a loadable address and we promoted it to a value.
    // 'o2g' => Was an @owned argument, but we changed it to be a guaranteed
    //          parameter.
    // 's'   => Was a loadable value that we exploded into multiple arguments.
    //
    // Currently we only use 'n' and 'd' since we do not perform the other
    // optimizations.
    //
    // *NOTE* The guaranteed optimization requires knowledge to be taught to the
    // ARC optimizer among other passes in order to guarantee safety. That or
    // you need to insert a fix_lifetime call to make sure we do not eliminate
    // the retain, release surrounding the call site in the caller.
    //
    // Additionally we use a packed signature since at this point we don't need
    // any '_'. The fact that we can run this optimization multiple times makes
    // me worried about long symbol names so I am trying to keep the symbol
    // names as short as possible especially in light of this being applied to
    // specialized functions.

    for (const ArgumentDescriptor &Arg : ArgDescList) {
      // If this arg is dead, add 'd' to the packed signature and continue.
      if (Arg.IsDead) {
        buffer << 'd';
        continue;
      }

      // If we have an @owned argument and found a callee release for it,
      // convert the argument to guaranteed.
      if (Arg.CalleeRelease) {
        buffer << "o2g";
        continue;
      }

      // Otherwise we are doing nothing so add 'n' to the packed signature.
      buffer << 'n';
    }

    buffer << '_' << F->getName();
  }

  return Name;
}

//===----------------------------------------------------------------------===//
//                                Main Routine
//===----------------------------------------------------------------------===//

/// This function takes in OldF and all callsites of OldF and rewrites the
/// callsites to call the new function.
static void
rewriteApplyInstToCallNewFunction(FunctionAnalyzer &Analyzer, SILFunction *NewF,
                                  CallGraphNode::CallerCallSiteList CallSites) {
  for (ApplyInst *AI : CallSites) {
    SILBuilder Builder(AI);

    FunctionRefInst *FRI = Builder.createFunctionRef(AI->getLoc(), NewF);

    // Create the args for the new apply, ignoring any dead arguments.
    llvm::SmallVector<SILValue, 8> NewArgs;
    ArrayRef<ArgumentDescriptor> ArgDescList = Analyzer.getArgDescList();
    for (unsigned i = 0, e = ArgDescList.size(); i != e; ++i) {
      if (ArgDescList[i].IsDead)
        continue;
      NewArgs.push_back(AI->getArgument(i));
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

    // If we have any arguments that were consumed but are now guaranteed,
    // insert a fix lifetime instruction and a release_value.
    //
    // We iterate over the consumed argument index to release map backwards
    // since we inserted the instructions in reverse order.
    for (auto P : Analyzer.getConsumedArgumentIndexReleasePairs()) {
      Builder.createFixLifetime(AI->getLoc(), AI->getArgument(P.first));
      Builder.createReleaseValue(AI->getLoc(), AI->getArgument(P.first));
    }

    // Erase the old apply.
    AI->eraseFromParent();

    ++NumCallSitesOptimized;
  }
}

static void createThunkBody(SILBasicBlock *BB, SILFunction *NewF,
                            FunctionAnalyzer &Analyzer) {
  // TODO: What is the proper location to use here?
  SILLocation Loc = BB->getParent()->getLocation();
  SILBuilder Builder(BB);

  FunctionRefInst *FRI = Builder.createFunctionRef(Loc, NewF);

  // Create the args for the thunk's apply, ignoring any dead arguments.
  llvm::SmallVector<SILValue, 8> ThunkArgs;

  ArrayRef<unsigned> DeadArgs = Analyzer.getDeadArgIndices();
  unsigned deadarg_i = 0, deadarg_e = DeadArgs.size();
  for (unsigned i = 0, e = BB->getNumBBArg(); i < e; ++i) {
    if (deadarg_i < deadarg_e && DeadArgs[deadarg_i] == i) {
      deadarg_i++;
      continue;
    }

    ThunkArgs.push_back(BB->getBBArg(i));
  }

  // We are ignoring generic functions and functions with out parameters for
  // now.
  SILType LoweredType = NewF->getLoweredType();
  SILType ResultType = LoweredType.getFunctionInterfaceResultType();
  SILValue ReturnValue = Builder.createApply(Loc, FRI, LoweredType, ResultType,
                                             ArrayRef<Substitution>(),
                                             ThunkArgs, NewF->isTransparent());

  // If we have any arguments that were consumed but are now guaranteed,
  // insert a fix lifetime instruction and a release_value.
  for (auto P : Analyzer.getConsumedArgumentIndexReleasePairs()) {
    SILValue Arg = BB->getBBArg(P.first);
    Builder.createFixLifetime(Loc, Arg);
    Builder.createReleaseValue(Loc, Arg);
  }

  Builder.createReturn(Loc, ReturnValue);
}

static SILFunction *
moveFunctionBodyToNewFunctionWithName(SILFunction *F,
                                      llvm::SmallString<64> &NewFName,
                                      FunctionAnalyzer &Analyzer) {
  // First we create an empty function (i.e. no BB) whose function signature has
  // had its arity modified.
  //
  // We only do this to remove dead arguments. All other function signature
  // optimization is done later by modifying the function signature elements
  // themselves.
  SILFunction *NewF = Analyzer.createEmptyFunctionWithOptimizedSig(NewFName);

  // Then we transfer the body of F to NewF. At this point, the arguments of the
  // first BB will not match.
  NewF->spliceBody(F);

  // Create a new BB called ThunkBB to use to create F's new body and use NewFs
  // first BB's arguments as a template for the BB's args.
  SILBasicBlock *ThunkBody = F->createBasicBlock();
  SILBasicBlock *NewFEntryBB = &*NewF->begin();
  for (auto *A : NewFEntryBB->getBBArgs()) {
    ThunkBody->createArgument(A->getType(), A->getDecl());
  }

  // Then erase the dead arguments from NewF using DeadArgs. We go backwards so
  // we remove arg elements by decreasing index so we don't invalidate our
  // indices.
  for (unsigned i : reversed(Analyzer.getDeadArgIndices())) {
    NewFEntryBB->eraseArgument(i);
  }

  // Intrusively optimize the function signature of NewF.

  // Then we create a new body for ThunkBody that calls NewF.
  createThunkBody(ThunkBody, NewF, Analyzer);

  return NewF;
}

/// This function takes in a SILFunction F and its callsites in the current
/// module and produces a new SILFunction that has the body of F but with
/// optimized function arguments. F is changed to be a thunk that calls NewF to
/// reduce code duplication in cases where we missed a callsite to F. The
/// function returns true if we were successful in creating the new function and
/// returns false otherwise.
static bool
optimizeFunctionSignature(SILFunction *F,
                          CallGraphNode::CallerCallSiteList CallSites,
                          RCIdentityAnalysis *RCIA) {
  DEBUG(llvm::dbgs() << "Optimizing Function Signature of " << F->getName()
                     << "\n");

  // An array containing our ArgumentDescriptor objects that contain information
  // from our analysis.
  llvm::SmallVector<ArgumentDescriptor, 8> Arguments;

  // Analyze function arguments. If there is no work to be done, exit early.
  FunctionAnalyzer Analyzer(F, RCIA);
  if (!Analyzer.analyze()) {
    DEBUG(llvm::dbgs() << "    Has no optimizable arguments... "
                          "bailing...\n");
    return false;
  }

  DEBUG(llvm::dbgs() << "    Has optimizable arguments... Performing "
                        "optimizations...\n");

  ++NumFunctionSignaturesOptimized;

  DEBUG(for (auto *AI : CallSites) {
    llvm::dbgs()  << "        CALLSITE: " << *AI;
  });

  llvm::SmallString<64> NewFName = Analyzer.getOptimizedName();

  // If we already have a specialized version of this function, do not
  // respecialize. For now just bail.
  //
  // TODO: Improve this. I do not expect this to occur often so I am fine for
  // now avoiding this issue. The main things I am worried about are assumptions
  // that we make about the callee and caller being violated. That said, this is
  // just a fear.
  if (F->getModule().lookUpFunction(NewFName))
    return false;

  // Otherwise, move F over to NewF.
  SILFunction *NewF =
    moveFunctionBodyToNewFunctionWithName(F, NewFName, Analyzer);

  // And remove all Callee releases that we found and made redundent via owned
  // to guaranteed conversion.
  //
  // TODO: If more stuff needs to be placed here, refactor into its own method.
  for (auto &A : Analyzer.getArgDescList()) {
    if (A.CalleeRelease) {
      A.CalleeRelease->eraseFromParent();
    }
  }

  // Rewrite all apply insts calling F to call NewF. Update each call site as
  // appropriate given the form of function signature optimization performed.
  rewriteApplyInstToCallNewFunction(Analyzer, NewF, CallSites);

  return true;
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

static bool isSpecializableCC(AbstractCC CC) {
  switch (CC) {
  case AbstractCC::Method:
  case AbstractCC::Freestanding:
  case AbstractCC::C:
    return true;
  case AbstractCC::WitnessMethod:
  case AbstractCC::ObjCMethod:
    return false;
  }
}

/// Returns true if F is a function which the pass know show to specialize
/// function signatures for.
static bool canSpecializeFunction(SILFunction &F) {
  // Do not specialize the signature of SILFunctions that are external
  // declarations since there is no body to optimize.
  if (F.isExternalDeclaration())
    return false;

  // Do not specialize functions that are available externally. If an external
  // function was able to be specialized, it would have been specialized in its
  // own module. We will inline the original function as a thunk. The thunk will
  // call the specialized function.
  if (F.isAvailableExternally())
    return false;

  // Do not specialize functions that we already specialized.
  if (isSpecializedFunction(F))
    return false;

  // Do not specialize the signature of transparent functions or always inline
  // functions, we will just inline them and specialize each one of the
  // individual functions that these sorts of functions are inlined into.
  if (F.isTransparent() || F.getInlineStrategy() == Inline_t::AlwaysInline)
    return false;

  // For now ignore generic functions to keep things simple...
  if (F.getLoweredFunctionType()->isPolymorphic())
    return false;

  // Make sure F has a linkage that we can optimize.
  if (!isSpecializableCC(F.getAbstractCC()))
    return false;

  return true;
}

namespace {

class FunctionSignatureOpts : public SILModuleTransform {
public:
  FunctionSignatureOpts() {}

  void run() {
    SILModule *M = getModule();
    auto *CGA = getAnalysis<CallGraphAnalysis>();
    auto *RCIA = getAnalysis<RCIdentityAnalysis>();

    DEBUG(llvm::dbgs() << "**** Optimizing Function Signatures ****\n\n");

    CallGraph &CG = CGA->getCallGraph();

    // Construct a map from Callee -> Call Site Set.

    // Process each function in the callgraph that we are able to optimize.
    //
    // TODO: Determine if it is profitable to always perform this optimization
    // even if a function is not called locally. As far as we can tell. Down the
    // line more calls may be exposed and the inliner might be able to handle
    // those calls.
    bool Changed = false;

    for (auto &F : *M) {
      // Check the signature of F to make sure that it is a function that we can
      // specialize. These are conditions independent of the call graph.
      if (!canSpecializeFunction(F))
        continue;

      // Then try and grab F's call graph node.
      CallGraphNode *FNode = CG.getCallGraphNode(&F);

      // If we don't have any call graph information for F, skip F.
      if (!FNode)
        continue;

      // Now that we have our call graph, grab the CallSites of F.
      auto CallSites = FNode->getKnownCallerCallSites();

      // If this function is not called anywhere, for now don't do anything.
      //
      // TODO: If it is public, it may still make sense to specialize since if
      // we link in the public function in another module, we may be able to
      // inline it and access the specialized version.
      if (CallSites.empty())
        continue;

      // Otherwise, try to optimize the function signature of F.
      Changed |= optimizeFunctionSignature(&F, CallSites, RCIA);
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
