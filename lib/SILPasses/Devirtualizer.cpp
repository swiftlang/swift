//===-- Devirtualizer.cpp ------ Devirtualize virtual calls ---------------===//
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
// Devirtualizes virtual function calls into direct function calls.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "devirtualization"
#include "swift/Basic/Demangle.h"
#include "swift/SIL/CallGraph.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILPasses/Devirtualizer.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILPasses/PassManager.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/SILInliner.h"
#include "swift/AST/ASTContext.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"
using namespace swift;

static const unsigned RecursionMaxDepth = 8;

STATISTIC(NumDevirtualized, "Number of calls devirtualzied");
STATISTIC(NumDynApply, "Number of dynamic apply devirtualzied");
STATISTIC(NumAMI, "Number of archetype_method devirtualzied");
STATISTIC(NumArgSpecialized, "# functions specialized on polymorphic args");

/// Given two SILResolvedArgIdxLists from the same function, return true if
/// RASuper is a superset of RASub.
static bool includesArgs(SILResolvedArgList &RASuper,
                         SILResolvedArgList &RASub) {
  for (auto SuperI = RASuper.begin(), SuperE = RASuper.end(),
         SubI = RASub.begin(), SubE = RASub.end(); SuperI != SuperE; ++SuperI) {
    if (*SuperI == *SubI) {
      ++SubI;
      if (SubI == SubE)
        return true;
    }
  }
  return false;
}

bool swift::isCoveringSpecialization(SILResolvedArgList &RASuper,
                                     SILResolvedArgList &RASub) {
  assert(!RASuper.empty() && !RASub.empty() && "Empty resolved argument list");
  ArrayRef<SILArgument*> SuperFuncArgs =
    RASuper[0].Arg->getFunction()->begin()->getBBArgs();
  ArrayRef<SILArgument*> SubFuncArgs =
    RASub[0].Arg->getFunction()->begin()->getBBArgs();
  assert(SuperFuncArgs.size() == SubFuncArgs.size() &&
         "Specialization signature mismatch.");

  unsigned ArgIdx = 0;
  for (auto SuperI = RASuper.begin(), SuperE = RASuper.end(),
         SubI = RASub.begin(), SubE = RASub.end();
       SuperI != SuperE; ++SuperI, ++ArgIdx) {
    while (SuperFuncArgs[ArgIdx] != SuperI->Arg)
      ++ArgIdx;
    if (SubFuncArgs[ArgIdx] == SubI->Arg && SuperI->Ty == SubI->Ty) {
      ++SubI;
      if (SubI == SubE)
        return true;
    }
  }
  return false;
}

SILArgTypeSpecialization *SILSpecializedArgsAnalysis::
getMinSpecialization(SILFunction *OrigF, SILResolvedArgList &ResArgs) {
  assert(!ResArgs.empty() && "Empty resolved argument list");

  auto FoundI = SpecializedFuncMap.find(OrigF);
  if (FoundI == SpecializedFuncMap.end())
    return nullptr;

  // Visit each of the function's specializations.
  // The first match from the right is minimal.
  for (unsigned ATSIdx : reversed(FoundI->second)) {
    if (isCoveringSpecialization(Specializations[ATSIdx].ResArgs, ResArgs))
      return &Specializations[ATSIdx];
  }
  return nullptr;
}

SILArgTypeSpecialization *SILSpecializedArgsAnalysis::
getMaxSpecialization(SILFunction *OrigF, SILResolvedArgList &ResArgs) {
  assert(!ResArgs.empty() && "Empty resolved argument list");

  auto FoundI = SpecializedFuncMap.find(OrigF);
  if (FoundI == SpecializedFuncMap.end())
    return nullptr;

  // Visit each of the function's specializations.
  // The first match is minimal.
  for (unsigned ATSIdx : FoundI->second) {
    if (isCoveringSpecialization(Specializations[ATSIdx].ResArgs, ResArgs))
      return &Specializations[ATSIdx];
  }
  return nullptr;
}

bool SILSpecializedArgsAnalysis::
addSpecialization(SILFunction *OrigF, SILFunction *NewF,
                  SILResolvedArgList &ResArgs) {
  // Visit each of the function's specializations.
  auto I = SpecializedFuncMap[OrigF].begin();
  for (auto E = SpecializedFuncMap[OrigF].end(); I != E; ++I) {
    SILArgTypeSpecialization &ATS = Specializations[*I];
    if (ResArgs.size() > ATS.ResArgs.size())
      break;
    assert(ATS.ResArgs != ResArgs && "Redundant specialization.");
  }
  SpecializedFuncMap[OrigF].insert(I, Specializations.size());
  assert(!SpecializedArgsMap.count(NewF) && "Already mapped.");
  SpecializedArgsMap[NewF] = Specializations.size();
  Specializations.emplace_back(SILArgTypeSpecialization(NewF, ResArgs));
  return true;
}

CanType SILSpecializedArgsAnalysis::lookupSpecializedArg(SILArgument *Arg) {
  auto FoundArgIdx = SpecializedArgsMap.find(Arg->getFunction());
  if (FoundArgIdx == SpecializedArgsMap.end())
    return CanType();
  for (auto &RA : Specializations[FoundArgIdx->second].ResArgs) {
    if (RA.Arg == Arg)
      return RA.Ty;
  }
  return CanType();
}

SILResolvedArgList *SILSpecializedArgsAnalysis::
getResolvedArgs(SILFunction *SpecialF) {
  auto FoundResArgs = SpecializedArgsMap.find(SpecialF);
  if (FoundResArgs == SpecializedArgsMap.end())
    return nullptr;

  return &Specializations[FoundResArgs->second].ResArgs;
}

namespace {
struct SILDevirtualizer {
  /// The SIL Module.
  SILModule *M = 0;
  SILSpecializedArgsAnalysis *SpecializedArgsMap = 0;
  bool Changed = false;
  unsigned DevirtThreshold = 0;

  /// A list of declared function names.
  llvm::StringSet<> FunctionNameCache;

  /// Map caller functions to their list of polymorphic arguments.  Each
  /// function is mapped to an ordered list of Arguments. This is the order that
  /// the arguments were analyzed, not the order in the declaration.  Each
  /// argument may either be direct polymorphic, meaning it is used for dynamic
  /// dispatch, or indirect polymorphic, meaning it is passed as a call site
  /// argument which reaches a dynamic dispatch deeper in the call chain.
  class PolymorphicArg {
    llvm::PointerIntPair<SILArgument*, 1, bool> Arg;
  public:
    PolymorphicArg() = default;
    PolymorphicArg(SILArgument *Arg, bool isDirect): Arg(Arg, isDirect) {}

    SILArgument *getArg() const { return Arg.getPointer(); }

    bool isDirect() const { return Arg.getInt(); }
    void setDirect() { Arg.setInt(true); }
  };
  // TODO: Convert this to a map of SILArgument -> SpecializeCost
  // to directly map a polymorphic argument to a cost/benefit metric.
  // We then need a bottom up pass to propagate this info.
  typedef SmallVector<PolymorphicArg, 4> PolyArgList;
  llvm::DenseMap<SILFunction*, PolyArgList > PolyArgMap;

  /// Map callee functions to resolved argument list IDs in descreasing number
  /// of resolved args.
  typedef SmallVector<unsigned, 4> ResolvedArgIdxList;
  llvm::DenseMap<SILFunction*, ResolvedArgIdxList> ResolvedArgMap;

  /// A list of calls to the same callee whose arguments can be resolved to the
  /// same types.
  typedef SmallVector<ApplyInst*, 4> ResolvedApplyList;

  /// Associate a list of resolved arguments with a set of calls.
  struct ResolvedArgInfo {
    SILResolvedArgList ResArgs;
    ResolvedApplyList Applies;

    ResolvedArgInfo(SILResolvedArgList ResArgs): ResArgs(std::move(ResArgs)) {}
    ResolvedArgInfo(ResolvedArgInfo &&RAI): ResArgs(std::move(RAI.ResArgs)),
                                            Applies(std::move(RAI.Applies)) {}
  };
  /// Table of resolved argument lists indexed on the list ID.
  /// These are the resolved argument types for a set of apply instructions.
  std::vector<ResolvedArgInfo> ResolvedArgTable;

  /// A specialization candidate within a chain of calls.
  struct SpecializeRequest {
    ApplyInst *AI;          // Call within the original function to callee.
                            // AI=null for the deepest specialization.
                            // Updated to the cloned call site.
    unsigned RAIdx = 0;     // Provides a list of callers to original func.
    SILResolvedArgList ResArgs; // Polymorphic args in original func.

    SpecializeRequest(ApplyInst *AI, unsigned Idx, SILResolvedArgList RA)
      : AI(AI), RAIdx(Idx), ResArgs(std::move(RA)) {}
    SpecializeRequest(SpecializeRequest &&R)
      : AI(R.AI), RAIdx(R.RAIdx), ResArgs(std::move(R.ResArgs)) {}
  };
  /// A chain of specialization candidates.
  typedef SmallVector<SpecializeRequest, 4> SpecializeChain;

  /// A call graph of applies with potentially polymorphic arguments.
  CallGraphSorter<SILFunction *> CallGraphOrder;

  /// A set of specialization candidates.
  llvm::SetVector<SILFunction*> SpecializeCands;

  /// Keep track of the specialized callee for each call site.
  llvm::MapVector<ApplyInst*,SILFunction*> SpecializedCalls;

  /// A worklist of functions to specialize.
  std::vector<SILFunction*> Worklist;

  /// True if another round of specialization may help.
  bool AttemptToSpecialize = false;

  SILDevirtualizer(SILModule *M, SILSpecializedArgsAnalysis *SAA,
                   unsigned Threshold)
    : M(M), SpecializedArgsMap(SAA), Changed(false),
      DevirtThreshold(Threshold) {
    // Save the list of function names at the beginning of the specialization.
    for (SILFunction &F : *M)
      FunctionNameCache.insert(F.getName());
  }

  void collectPolyArgs(ApplyInst *AI);

  CanType resolveObjectType(SILValue Obj);

  bool recordResolvedArgs(ApplyInst *AI, SILFunction *Callee,
                          const SILResolvedArgList &ResArgs);

  bool resolveArgs(ApplyInst *AI);

  SILFunction *specializeFuncForCall(SpecializeRequest &Request);
  SILFunction *computeSpecialization(SpecializeChain &Chain, unsigned Cost);
  void specializeForArgTypes();
  void replaceCallsToSpecializedFunctions();

  void optimizeClassMethodInst(ClassMethodInst *CMI);
  void optimizeApplyInst(ApplyInst *Inst);
  void optimizeFuncBody(SILFunction *F);
  bool run();

#ifndef NDEBUG
  void dumpPolyArgs();
#endif
};

} // anonymous namespace.

/// \brief Returns the index of the argument that the function returns or -1
/// if the return value is not always an argument.
static int functionReturnsArgument(SILFunction *F) {
  if (F->getBlocks().size() != 1)
    return -1;

  // Check if there is a single terminator which is a ReturnInst.
  ReturnInst *RI = dyn_cast<ReturnInst>(F->begin()->getTerminator());
  if (!RI)
    return -1;

  // Check that the single return instruction that we found returns the
  // correct argument. Scan all of the argument and check if the return inst
  // returns them.
  ValueBase *ReturnedVal = RI->getOperand().getDef();
  for (int i = 0, e = F->begin()->getNumBBArg(); i != e; ++i)
    if (F->begin()->getBBArg(i) == ReturnedVal)
      return i;

  // The function does not return an argument.
  return -1;
}

/// \brief Returns the single return value if there is one.
static SILValue functionSingleReturn(SILFunction *F) {
  if (F->getBlocks().size() != 1)
    return SILValue();

  // Check if there is a single terminator which is a ReturnInst.
  ReturnInst *RI = dyn_cast<ReturnInst>(F->begin()->getTerminator());
  if (!RI)
    return SILValue();
  return RI->getOperand();
}

static SILValue findOrigin(SILValue S) {
  SILValue Origin = S;
  unsigned Depth = 0;
  for (; Depth < RecursionMaxDepth; ++Depth) {
    switch (Origin->getKind()) {
      default:
        break;
      case ValueKind::UpcastInst:
      case ValueKind::UnconditionalCheckedCastInst:
        Origin = cast<SILInstruction>(Origin)->getOperand(0);
        continue;
      case ValueKind::ApplyInst: {
        ApplyInst *AI = cast<ApplyInst>(Origin);
        FunctionRefInst *FR =
          dyn_cast<FunctionRefInst>(AI->getCallee().getDef());
        if (!FR)
          break;

        SILFunction *F = FR->getReferencedFunction();
        if (!F->size())
          break;

        // Does this function return one of its arguments ?
        int RetArg = functionReturnsArgument(F);
        if (RetArg != -1) {
          Origin = AI->getOperand(1 /* 1st operand is Callee */ + RetArg);
          continue;
        }
        SILValue RetV = functionSingleReturn(F);
        if (RetV.isValid()) {
          Origin = RetV;
          continue;
        }
        break;
      }
    }
    // No cast or pass-thru args found.
    break;
  }
  DEBUG(if (Depth == RecursionMaxDepth)
          llvm::dbgs() << "findMetaType: Max recursion depth.\n");

  return Origin;
}

// Find this arg in the list. Mark it direct if it's polymorphic at the current
// use. Note that it may be direct at a previously visited use.
// @return true if a new potentially polymorphic argument was discovered.
static bool
addPolyArg(SILArgument *Arg, SILDevirtualizer::PolyArgList &PolyArgs,
           bool isDirect) {
  auto Found = std::find_if(PolyArgs.begin(), PolyArgs.end(),
    [&](SILDevirtualizer::PolymorphicArg a) { return a.getArg() == Arg; });
  if (Found == PolyArgs.end()) {
    PolyArgs.push_back(SILDevirtualizer::PolymorphicArg(Arg, isDirect));
    return true;
  }
  else if (isDirect)
    Found->setDirect();

  return false;
}

/// Add indirect polymorphic arguments to the caller if any of its arguments are
/// passed directly to this call site.
void SILDevirtualizer::collectPolyArgs(ApplyInst *AI) {
  SILValue CalleeVal = AI->getCallee();
  FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(CalleeVal.getDef());
  if (!FRI)
    return;

  SILFunction *Callee = FRI->getReferencedFunction();
  if (Callee->isExternalDeclaration())
    return;

  // Find any arguments that are directly passed from caller to callee.
  // They are potentially polymorphic arguments mapped to caller.
  bool MayResolveArg = false;
  for (auto &ArgOper : AI->getArgumentOperands()) {
    SILValue ArgDef = findOrigin(ArgOper.get());
    if (SILArgument *CallerArg = dyn_cast<SILArgument>(ArgDef)) {
      MayResolveArg = true;
      if (!SpecializedArgsMap->lookupSpecializedArg(CallerArg))
        addPolyArg(CallerArg, PolyArgMap[CallerArg->getFunction()], false);
    }
    else if (!resolveObjectType(ArgDef).isNull())
      MayResolveArg = true;
  }
  // Add edges needed for a top-down call graph. This ensures that forward
  // propagation starts with the top-level caller, even in the presence of call
  // graph cycles.
  if (MayResolveArg)
    CallGraphOrder.addEdge(AI->getFunction(), Callee);
}

/// Record the argument types given in ResArgs as a specialization
/// candidate at the given call site.
bool SILDevirtualizer::recordResolvedArgs(ApplyInst *AI,
                                          SILFunction *Callee,
                                          const SILResolvedArgList &ResArgs) {
  assert(!ResArgs.empty() && "No resolved args at this call site.");

  ResolvedArgIdxList &RAIdxList = ResolvedArgMap[Callee];
  auto I = RAIdxList.begin();
  for (auto E = RAIdxList.end(); I != E; ++I) {
    ResolvedArgInfo &RAI = ResolvedArgTable[*I];
    if (ResArgs.size() > RAI.ResArgs.size())
      break;
    if (ResArgs == RAI.ResArgs) {
      if (std::find(RAI.Applies.begin(), RAI.Applies.end(), AI)
          == RAI.Applies.end()) {
        RAI.Applies.push_back(AI);
      }
      return false;
    }
  }
  DEBUG(llvm::dbgs() << "  Resolved args ";
        for (auto &RA : ResArgs) {
          if (RA.Arg) llvm::dbgs() << *RA.Arg;
          if (!RA.Ty.isNull()) llvm::dbgs() << RA.Ty->getString();
        }
        llvm::dbgs() << "\n");

  RAIdxList.insert(I, ResolvedArgTable.size());
  ResolvedArgTable.emplace_back(ResolvedArgInfo(ResArgs));
  ResolvedArgTable.back().Applies.push_back(AI);
  return true;
}

namespace {
/// An apply operand may be directly resolved to a type or resolved to a
/// function argument.
struct ResOperType {
  llvm::PointerIntPair<SILArgument*, 1, bool> Arg;
  CanType Ty;

  ResOperType() = default;
  ResOperType(SILArgument *Arg): Arg(Arg, 1) {}
  ResOperType(CanType Ty): Ty(Ty) {}

  SILArgument *getArg() const { return Arg.getPointer(); }

  bool isArg() const { return Arg.getInt(); }

  bool isNull() const { return !isArg() && !Ty.isNull(); }
};
} // anonymous namespace

/// Resolve polymorphic arguments of the callee.
///
/// TODO: Interleave this with specialization so we don't need to record all
/// combiniations of resolved arguments at all call sites unless we've already
/// found them to be profitable.
bool SILDevirtualizer::resolveArgs(ApplyInst *AI) {
  SILValue CalleeVal = AI->getCallee();
  FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(CalleeVal.getDef());
  if (!FRI)
    return false;

  SILFunction *Callee = FRI->getReferencedFunction();
  if (Callee->isExternalDeclaration())
    return false;

  auto FoundPolyArgList = PolyArgMap.find(Callee);
  if (FoundPolyArgList == PolyArgMap.end())
    return false;

  DEBUG(llvm::dbgs() << "Resolving arguments for call "
        << Demangle::demangleSymbolAsString(AI->getFunction()->getName())
        << " => " << Demangle::demangleSymbolAsString(Callee->getName())
        << "\n");

  PolyArgList &PolyArgs = FoundPolyArgList->second;

  SILBasicBlock *CalleeEntryBB = Callee->begin();
  MutableArrayRef<Operand> Opers = AI->getArgumentOperands();
  unsigned NOpers = Opers.size();
  assert(CalleeEntryBB->bbarg_size() == NOpers &&
         "ApplyInst arg count does not match callee arg count.");

  // 1. Locally resolve each operand to a type or caller argument.
  // Store the result in ResOperTypes indexed by the apply operand indices.
  SmallVector<ResOperType, 4> ResOperTypes;
  ResOperTypes.resize(NOpers);
  // Keep track of direcly polymorphic apply operands to record specialization
  // candidates. For convenience this is also indexed on the apply operand.
  SmallVector<unsigned, 4> OperCandFlags;
  OperCandFlags.resize(NOpers);
  bool HasLocalTy = false;
  SILFunction *CallerForArg = nullptr;
  // Check each apply operand for resolved types.
  for (unsigned OperIdx = 0; OperIdx < NOpers; ++OperIdx) {
    // Check whether this apply argument is potentially polymorphic in the
    // callee. If not, ignore it.
    SILArgument *CalleeArg = CalleeEntryBB->getBBArg(OperIdx);
    auto Found = std::find_if(PolyArgs.begin(), PolyArgs.end(),
                              [&](PolymorphicArg a) {
                                return a.getArg() == CalleeArg; });
    if (Found == PolyArgs.end())
      continue;

    OperCandFlags[OperIdx] = Found->isDirect();
    SILValue ArgDef = findOrigin(Opers[OperIdx].get());
    if (SILArgument *CallerArg = dyn_cast<SILArgument>(ArgDef)) {
      CanType Ty = SpecializedArgsMap->lookupSpecializedArg(CallerArg);
      if (!Ty.isNull()) {
        HasLocalTy = true;
        ResOperTypes[OperIdx] = Ty;
      }
      else {
        CallerForArg = CallerArg->getFunction();
        ResOperTypes[OperIdx] = CallerArg;
      }
      continue;
    }
    // If we handle more cases of concrete types, update the CallGraph filter in
    // collectPolyArgs.
    CanType ConcreteType = resolveObjectType(ArgDef);
    if (!ConcreteType.isNull()) {
      HasLocalTy = true;
      ResOperTypes[OperIdx] = ConcreteType;
    }
    // Unknown operand resolution.
  }
  bool NewSig = false;

  // 2. Push a specialization signature for the local resolutions only.
  if (HasLocalTy) {
    SILResolvedArgList ResArgs;
    for (unsigned Idx = 0; Idx < NOpers; ++Idx) {
      if (!ResOperTypes[Idx].isArg()) {
        SILResolvedArg RA(CalleeEntryBB->getBBArg(Idx), ResOperTypes[Idx].Ty);
        ResArgs.push_back(RA);
        if (OperCandFlags[Idx])
          SpecializeCands.insert(Callee);
      }
    }
    NewSig |= recordResolvedArgs(AI, Callee, ResArgs);
  }

  // 3. For each of the caller's specialization signatures, add a new callee
  // specialization.
  if (!CallerForArg)
    return NewSig;

  auto FoundCaller = ResolvedArgMap.find(CallerForArg);
  if (FoundCaller == ResolvedArgMap.end())
    return NewSig;

  for (unsigned ResIdx : FoundCaller->second) {
    SILResolvedArgList ResArgs;
    // Process each apply operand in order. If it is locally resolved
    // record the type. If the operand originates from a caller argument,
    // substitute the caller's specialization.
    for (unsigned Idx = 0; Idx < NOpers; ++Idx) {
      ResOperType &ResOperTy = ResOperTypes[Idx];
      if (ResOperTy.isNull())
        continue;

      SILArgument *CalleeArg = CalleeEntryBB->getBBArg(Idx);
      if (!ResOperTy.isArg()) {
        SILResolvedArg RA(CalleeArg, ResOperTy.Ty);
        ResArgs.push_back(RA);
        if (OperCandFlags[Idx])
          SpecializeCands.insert(Callee);
      }
      else {
        SILArgument *CallerArg = ResOperTy.getArg();
        SILResolvedArgList &CallerResArgs = ResolvedArgTable[ResIdx].ResArgs;
        auto CallerResArgI =
          std::find_if(CallerResArgs.begin(), CallerResArgs.end(),
                       [&](SILResolvedArg a) { return a.Arg == CallerArg; });
        if (CallerResArgI != CallerResArgs.end()) {
          // Substitute the caller's argument type at the callee argument.
          SILResolvedArg RA(CalleeArg, CallerResArgI->Ty);
          ResArgs.push_back(RA);
          if (OperCandFlags[Idx])
            SpecializeCands.insert(Callee);
        }
      }
    }
    if (!ResArgs.empty())
      NewSig |= recordResolvedArgs(AI, Callee, ResArgs);
  }
  return NewSig;
}

namespace {
/// Clone a function as-is giving it a new name.
/// If an apply instructution within this function is provided, return the
/// cloned copy of the apply.
class RawFunctionCloner : public SILCloner<RawFunctionCloner> {
  friend class SILVisitor<RawFunctionCloner>;
  friend class SILCloner<RawFunctionCloner>;

public:
  // In nonnull, AI will be overwritten with the new AI.
  static SILFunction *cloneForCall(SILFunction *OrigF, StringRef NewName,
                                   ApplyInst *&AI) {
    RawFunctionCloner Cloner(initCloned(OrigF, NewName));
    Cloner.populateClone(OrigF);
    if (AI)
      AI = Cloner.getClonedApply(AI);
    return Cloner.getClonedFunc();
  }
protected:
  RawFunctionCloner(SILFunction *F) : SILCloner(*F) {}

  SILFunction *getClonedFunc() { return &getBuilder().getFunction(); }

  ApplyInst *getClonedApply(ApplyInst *AI) {
    return cast<ApplyInst>(InstructionMap[AI]);
  }

  /// Create a new empty function with a unique name.
  static SILFunction *initCloned(SILFunction *OrigF, StringRef NewName) {
    SILModule &M = OrigF->getModule();

    // Create a new empty function.
    // TODO: Use getSpecializedLinkage() once we mangle properly.
    SILFunction *NewF =
      SILFunction::create(M, SILLinkage::Private,
                          NewName,
                          OrigF->getLoweredFunctionType(),
                          nullptr,
                          OrigF->getLocation(),
                          OrigF->isBare(),
                          OrigF->isTransparent(),
                          nullptr,
                          OrigF->getDebugScope(),
                          OrigF->getDeclContext());
    ++NumArgSpecialized;
    return NewF;
  }

  void populateClone(SILFunction *OrigF);
};
}

// TODO: This is similar to the specializer's cloner. We could factor.
void RawFunctionCloner::populateClone(SILFunction *OrigF) {
  SILFunction *Cloned = getClonedFunc();
  SILModule &M = Cloned->getModule();

  // Create arguments for the entry block.
  SILBasicBlock *OrigEntryBB = OrigF->begin();
  SILBasicBlock *ClonedEntryBB = new (M) SILBasicBlock(Cloned);

  // Create the entry basic block with the function arguments.
  auto I = OrigEntryBB->bbarg_begin(), E = OrigEntryBB->bbarg_end();
  while (I != E) {
    SILValue MappedValue =
      new (M) SILArgument((*I)->getType(), ClonedEntryBB, (*I)->getDecl());
    ValueMap.insert(std::make_pair(*I, MappedValue));
    ++I;
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

SILFunction *SILDevirtualizer::
specializeFuncForCall(SpecializeRequest &Request) {
  SILFunction *OrigF = Request.ResArgs[0].Arg->getFunction();

  DEBUG(llvm::dbgs() << " *** Specializing "
        << Demangle::demangleSymbolAsString(OrigF->getName())
        << "\n";
        for (auto &RA : Request.ResArgs)
          llvm::dbgs() << *RA.Arg << "    : " << RA.Ty->getString() << "\n");

#ifndef NDEBUG
  SILResolvedArgList *PreResArgs = SpecializedArgsMap->getResolvedArgs(OrigF);
  assert((!PreResArgs || !includesArgs(*PreResArgs, Request.ResArgs)) &&
         "Redundant Specialization.");
#endif
  // TODO: Mangle the subst types into the new function name and
  // use shared linkage. For now, we use a running counter. rdar://15658321
  unsigned Counter = 0;
  std::string ClonedName;
  do {
    ClonedName.clear();
    llvm::raw_string_ostream buffer(ClonedName);
    buffer << OrigF->getName() << "_argspec" << Counter++;
  } while (FunctionNameCache.count(ClonedName));

  // Add the new name to the list of module function names.
  FunctionNameCache.insert(ClonedName);

  // Create a new function.
  // Request.AI will be overwritten with the cloned call site, or left null.
  SILFunction *NewF =
    RawFunctionCloner::cloneForCall(OrigF, ClonedName, Request.AI);

  ArrayRef<SILArgument*> OrigArgs = OrigF->begin()->getBBArgs();
  ArrayRef<SILArgument*> NewArgs = NewF->begin()->getBBArgs();
  assert(OrigArgs.size() == NewArgs.size() && "Mismatched signature.");

  SILResolvedArgList NewResArgs;
  unsigned ArgIdx = 0;
  for (auto &RA : Request.ResArgs) {
    while (OrigArgs[ArgIdx] != RA.Arg)
      ++ArgIdx;
    NewResArgs.push_back(SILResolvedArg(NewArgs[ArgIdx], RA.Ty));
    ++ArgIdx;
  }
  // Record the specialization, mapping it to a list of resolved arguments in
  // the new function.
  SpecializedArgsMap->addSpecialization(OrigF, NewF, NewResArgs);

  Changed = true;
  Worklist.push_back(NewF);
  return NewF;
}

/// Recursively compute a chain of function to specialize. The function at the
/// bottom does dynamic dispath on polymorphic arguments. The function at the
/// top resolves call arguments to known types. It is actually possible for some
/// arguments to be resolved in intermediate functions and for some dynamic
/// dispatch to be removed in intermediate functions.
///
/// TODO: We currently don't specialize unless all resolved arguments are
/// satisfied. If we hit the cost threshold, we could miss an opportunity to
/// resolve some of the operands.
SILFunction *SILDevirtualizer::
computeSpecialization(SpecializeChain &Chain, unsigned Cost) {
  // computeSpecialization recursively pushes onto Chain, so pointers to Chain
  // entries will be invalidated.
  assert(!Chain.back().ResArgs.empty() &&
         "Specialization requires resolved args.");

  SILFunction *OrigF = Chain.back().ResArgs[0].Arg->getFunction();
  assert((!Chain.back().AI || Chain.back().AI->getFunction() == OrigF) &&
         "Specialization request call/function mismatch.");

  // Find an existing specialization of Chain.back().OrigF such that
  // ResArgs(Specialization) > Chain.back().ResArgs
  // i.e. this specialization covers all polymorphic arguments below us.
  //
  // Any call sites with sufficient resolved args should already have been
  // redirected to this specialization.
  //
  // We would like to return the apply within this specialized function so that
  // it can be immediately redirected to the specialized callee. However, we
  // don't know where it is. Instead we effectively bail out on this chain. In
  // the next round of specialization, the resolved args call graph should
  // include the call site within this specialized function.
  if (SpecializedArgsMap->getMinSpecialization(OrigF, Chain.back().ResArgs))
    return nullptr;

  // We need to specialize the requested function for this call site.
  Cost += getFunctionCost(OrigF, /*Caller=*/nullptr, DevirtThreshold);
  if (Cost > DevirtThreshold) {
    DEBUG(llvm::dbgs() << "  Cannot specialize: "
          << Demangle::demangleSymbolAsString(OrigF->getName()) << "\n"
          << "  Cost: " << Cost << "\n");
    return nullptr;
  }

  // Visit each caller that is not already redirected to a specialization. If
  // this function is already specialized, then its callers are recorded in
  // SpecializedCalls. We may still find other callers in our apply list, which
  // may result in a narrower specialization.
  ResolvedApplyList NewApplies;
  SILResolvedArgList &ResOpers = Chain.back().ResArgs;
  for (auto AI : ResolvedArgTable[Chain.back().RAIdx].Applies) {
    if (SpecializedCalls.count(AI)) {
      // The call was already redirected to a specialized function. If we want
      // to further specialize this function, or redirect calls from within the
      // specialized function, that can happen in the next round.
      AttemptToSpecialize = true;
      continue;
    }
    // Get the type of each polymorphic argument in the caller.
    llvm::DenseMap<SILArgument*,CanType> ArgTypes;
    MutableArrayRef<Operand> Opers = AI->getArgumentOperands();
    SILBasicBlock *CalleeEntryBB = OrigF->begin();
    auto ResOperI = ResOpers.begin(), ResOperE = ResOpers.end();
    for (unsigned OperIdx = 0, NOpers = Opers.size();
         OperIdx < NOpers; ++OperIdx) {
      if (CalleeEntryBB->getBBArg(OperIdx) != ResOperI->Arg)
        continue;

      SILValue ArgDef = findOrigin(Opers[OperIdx].get());
      if (SILArgument *CallerArg = dyn_cast<SILArgument>(ArgDef))
        ArgTypes[CallerArg] = ResOperI->Ty;
      else
        assert(!resolveObjectType(ArgDef).isNull() && "Arg must be resolved");
      if (++ResOperI == ResOperE)
        break;
    }
    // Order the caller's resolved args by its parameter list, and pickup any
    // prespecialized argument types.
    //
    // Note: we may not need to retain all prespecialized arguments when we
    // (re)specialize , but we can't tell if any assumptions have already been
    // made based on the type.
    SILFunction *CallerF = AI->getFunction();
    SILBasicBlock *CallerEntryBB = CallerF->begin();
    SILResolvedArgList ResArgs;
    bool CallerNeedsSpecialization = false;
    for (auto ArgI = CallerEntryBB->bbarg_begin(),
           ArgE = CallerEntryBB->bbarg_end(); ArgI != ArgE; ++ArgI) {
      CanType ArgTy = SpecializedArgsMap->lookupSpecializedArg(*ArgI);
      if (ArgTypes.count(*ArgI)) {
        // The current specialization request needs this ArgTy.
        if (!ArgTy) {
          CallerNeedsSpecialization = true;
          ArgTy = ArgTypes[*ArgI];
        }
        else
          assert(ArgTy == ArgTypes[*ArgI]
                 && "Specialized arg type does not match polymorphic arg.");
      }
      // If the argument was already specialized, we must keep its type.
      if (ArgTy)
        ResArgs.push_back(SILResolvedArg(*ArgI, ArgTy));
    }
    if (!CallerNeedsSpecialization) {
      // No further specialization. Rewrite this apply in place.
      Cost = 0;
      NewApplies.push_back(AI);
      continue;
    }
    // Find all resolved argument lists for this function that cover ResArgs.
    for (unsigned ResIdx : ResolvedArgMap[CallerF]) {
      if (includesArgs(ResolvedArgTable[ResIdx].ResArgs, ResArgs)) {
        Chain.push_back(SpecializeRequest(AI, ResIdx, ResArgs));
        // Chain.back().AI updated here.
        if (computeSpecialization(Chain, Cost)) {
          assert(Chain.back().AI && "Missing cloned call site.");
          NewApplies.push_back(Chain.back().AI);
          Cost = 0;
        }
        Chain.pop_back();
      }
    }
  }
  // Cost was reset to zero as soon as we determined the call chain would be
  // specialized.
  if (!Cost) {
    // Chain.back().AI will be updated to the new call site, or left null.
    SILFunction *NewF = specializeFuncForCall(Chain.back());
    for (auto AI : NewApplies)
      SpecializedCalls[AI] = NewF;
    return NewF;
  }
  return nullptr;
}

void SILDevirtualizer::replaceCallsToSpecializedFunctions() {
  // Clear analysis of applies before we rewrite any of them.
  ResolvedArgMap.clear();
  ResolvedArgTable.clear();
  for (auto CallI = SpecializedCalls.begin(), CallE = SpecializedCalls.end();
       CallI != CallE; ++CallI) {
    replaceWithSpecializedFunction(CallI->first, CallI->second);
    Changed = true;
  }
  SpecializedCalls.clear();
}

/// Specialize chains of functions to resolve polymorphic arguments.
void SILDevirtualizer::specializeForArgTypes() {
  // Process each specialization candidate which directly allows removal of
  // dynamic dispatch.
  for (SILFunction *OrigF : SpecializeCands) {
    assert(ResolvedArgMap.count(OrigF) && "candidate has no resolved args");

    // Construct an initial list of arguments that must be resolved to optimize
    // dynamic dispatch.
    PolyArgList &PolyArgs = PolyArgMap[OrigF];
    // The specialization signatures are sorted by the decreasing number of
    // resolved arguments.
    for (unsigned ResIdx : ResolvedArgMap[OrigF]) {
      SILResolvedArgList ResArgs;
      for (auto RA : ResolvedArgTable[ResIdx].ResArgs)
        for (auto PA : PolyArgs)
          if (PA.getArg() == RA.Arg && PA.isDirect())
            ResArgs.push_back(RA);
      if (ResArgs.empty())
        continue;
      SpecializeChain Chain;
      Chain.push_back(SpecializeRequest(nullptr, ResIdx, ResArgs));
      computeSpecialization(Chain, /*Cost=*/0);
    }
  }
  replaceCallsToSpecializedFunctions();
  SpecializeCands.clear();
}

// Strip the InOut qualifier.
CanType stripInOutQualifier(SILType Ty) {
  CanType ConcreteTy = Ty.getSwiftType();
  if (InOutType *IOT = dyn_cast<InOutType>(ConcreteTy))
    ConcreteTy = IOT->getObjectType()->getCanonicalType();
  return ConcreteTy;
}

/// \brief Scan the use-def chain and skip cast instructions that don't change
/// the value of the class. Stop on classes that define a class type.
static SILInstruction *findMetaType(SILValue S, unsigned Depth = 0) {
  SILInstruction *Inst = dyn_cast<SILInstruction>(findOrigin(S));
  if (!Inst)
    return nullptr;

  switch (Inst->getKind()) {
  case ValueKind::AllocRefInst:
  case ValueKind::MetatypeInst:
    return Inst;
  default:
    return nullptr;
  }
}

/// \brief Recursively searches the ClassDecl for the type of \p S, or null.
static ClassDecl *findClassTypeForOperand(SILValue S) {
  // Look for an instruction that defines a class type.
  SILInstruction *Meta = findMetaType(S);
  if (!Meta)
    return nullptr;

  // Look for a a static ClassTypes in AllocRefInst or MetatypeInst.
  if (AllocRefInst *ARI = dyn_cast<AllocRefInst>(Meta)) {
    return ARI->getType().getClassOrBoundGenericClass();
  } else if (MetatypeInst *MTI = dyn_cast<MetatypeInst>(Meta)) {
    CanType MetaTy = MTI->getType().getSwiftRValueType();
    TypeBase *T = cast<MetatypeType>(MetaTy)->getInstanceType().getPointer();
    return T->getClassOrBoundGenericClass();
  } else {
    return nullptr;
  }
}

void SILDevirtualizer::optimizeClassMethodInst(ClassMethodInst *CMI) {
  DEBUG(llvm::dbgs() << " *** Trying to optimize : " << *CMI);
  // Optimize a class_method and alloc_ref pair into a direct function
  // reference:
  //
  // %XX = alloc_ref $Foo
  // %YY = class_method %XX : $Foo, #Foo.get!1 : $@cc(method) @thin ...
  //
  //  or
  //
  //  %XX = metatype $...
  //  %YY = class_method %XX : ...
  //
  //  into
  //
  //  %YY = function_ref @...
  ClassDecl *Class = nullptr;
  SILValue OperDef = findOrigin(CMI->getOperand());
  if (SILArgument *Arg = dyn_cast<SILArgument>(OperDef)) {
    CanType Ty = SpecializedArgsMap->lookupSpecializedArg(Arg);
    if (Ty.isNull())
      addPolyArg(Arg, PolyArgMap[Arg->getFunction()], true);
    else
      Class = Ty->getClassOrBoundGenericClass();
  }
  else
    Class = findClassTypeForOperand(OperDef);
  if (!Class)
    return;

  // Walk up the class hierarchy and scan all members.
  // TODO: There has to be a faster way of doing this scan.
  SILDeclRef Member = CMI->getMember();
  while (Class) {
    // Search all of the vtables in the module.
    for (auto &Vtbl : CMI->getModule().getVTableList()) {
      if (Vtbl.getClass() != Class)
        continue;

      // If found the requested method.
      if (SILFunction *F = Vtbl.getImplementation(CMI->getModule(), Member)) {
        // Create a direct reference to the method.
        SILInstruction *FRI =
        new (CMI->getModule()) FunctionRefInst(CMI->getLoc(), F);
        DEBUG(llvm::dbgs() << " *** Devirtualized : " << *CMI);
        CMI->getParent()->getInstList().insert(CMI, FRI);
        CMI->replaceAllUsesWith(FRI);
        CMI->eraseFromParent();
        for (auto UI = FRI->use_begin(), UE = FRI->use_end(); UI != UE; ++UI)
          if (ApplyInst *AI = dyn_cast<ApplyInst>(UI->getUser()))
            collectPolyArgs(AI);
        NumDevirtualized++;
        Changed = true;
        return;
      }
    }

    // We could not find the member in our class. Moving to our superclass.
    if (Type T = Class->getSuperclass())
      Class = T->getClassOrBoundGenericClass();
    else
      break;
  }

  return;
}

/// \brief Scan the uses of the protocol object and return the initialization
/// instruction, which can be copy_addr or init_existential.
/// There needs to be only one initialization instruction and the
/// object must not be captured by any instruction that may re-initialize it.
static SILInstruction *
findSingleInitNoCaptureProtocol(SILValue ProtocolObject) {
  SILInstruction *Init = 0;
  for (auto UI = ProtocolObject->use_begin(), E = ProtocolObject->use_end();
       UI != E; UI++) {
    switch (UI.getUser()->getKind()) {
    case ValueKind::CopyAddrInst: {
      // If we are reading the content of the protocol (to initialize
      // something else) then its okay.
      if (cast<CopyAddrInst>(UI.getUser())->getSrc() == ProtocolObject)
        continue;

      // fallthrough: ...
    }
    case ValueKind::InitExistentialInst: {
      // Make sure there is a single initialization:
      if (Init) {
        DEBUG(llvm::dbgs() << " *** Multiple Protocol initializers: "
                           << *UI.getUser() << " and " << *Init);
        return nullptr;
      }
      // This is the first initialization.
      Init = UI.getUser();
      continue;
    }
    case ValueKind::ProjectExistentialInst:
    case ValueKind::ProtocolMethodInst:
    case ValueKind::DeallocBoxInst:
    case ValueKind::DeallocRefInst:
    case ValueKind::DeallocStackInst:
    case ValueKind::StrongReleaseInst:
    case ValueKind::DestroyAddrInst:
    case ValueKind::DestroyValueInst:
      continue;

    default: {
      DEBUG(llvm::dbgs() << " *** Protocol captured by: " << *UI.getUser());
      return nullptr;
    }
    }
  }
  return Init;
}

/// TODO: This implementation handles the obvious patterns. Revisit the logic to
/// make sure it is robust.
///
/// TODO: Factor this with optimizeApplyInst once it is pushed to trunk. The two
/// need to be consistent for specialization to be productive.
///
/// TODO: Currently we assume any argument could be an initialized
/// protocol. This shouldn't break, but it would be more sane to check that the
/// callee's argument type is a protocol before looking for protocol
/// initialization.
CanType SILDevirtualizer::resolveObjectType(SILValue Obj) {
  if (AllocRefInst *ARI = dyn_cast<AllocRefInst>(Obj)) {
    // Can we have something that's not a class or bounded generic?
    if (!ARI->getType().getClassOrBoundGenericClass())
      return CanType();
    return ARI->getType().getSwiftRValueType();
  }
  SILInstruction *InitInst = findSingleInitNoCaptureProtocol(Obj);
  if (CopyAddrInst *CAI = dyn_cast_or_null<CopyAddrInst>(InitInst)) {
    if (!CAI->isInitializationOfDest() || !CAI->isTakeOfSrc())
      return CanType();

    InitInst = findSingleInitNoCaptureProtocol(CAI->getSrc());
  }
  InitExistentialInst *Init = dyn_cast_or_null<InitExistentialInst>(InitInst);
  if (!Init)
    return CanType();

  // Strip the InOut qualifier.
  return stripInOutQualifier(Init->getConcreteType());
}

/// \brief Replaces a virtual ApplyInst instruction with a new ApplyInst
/// instruction that does not use a project_existencial \p PEI and calls \p F
/// directly. See visitApplyInst.
static ApplyInst *replaceDynApplyWithStaticApply(ApplyInst *AI, SILFunction *F,
                                                 InitExistentialInst *In,
                                                 ProjectExistentialInst *PEI) {
  // Creates a new FunctionRef Inst and inserts it to the basic block.
  FunctionRefInst *FRI = new (AI->getModule()) FunctionRefInst(AI->getLoc(), F);
  AI->getParent()->getInstList().insert(AI, FRI);
  SmallVector<SILValue, 4> Args;

  // Push all of the args and replace uses of PEI with the InitExistentional.
  MutableArrayRef<Operand> OrigArgs = AI->getArgumentOperands();
  for (unsigned i = 0; i < OrigArgs.size(); i++) {
    SILValue A = OrigArgs[i].get();
    Args.push_back(A.getDef() == PEI ? In : A);
  }

  // Create a new non-virtual ApplyInst.
  SILType FnTy = FRI->getType();
  ApplyInst *SAI = ApplyInst::create(
      AI->getLoc(), FRI, FnTy,
      FnTy.castTo<SILFunctionType>()->getInterfaceResult().getSILType(),
      ArrayRef<Substitution>(), Args, false, *F);
  AI->getParent()->getInstList().insert(AI, SAI);
  AI->replaceAllUsesWith(SAI);
  AI->eraseFromParent();
  return SAI;
}

/// \brief Given a protocol \p Proto, a member method \p Member and a concrete
/// class type \p ConcreteTy, search the witness tables and return the static
/// function that matches the member. Notice that we do not scan the class
/// hierarchy, just the concrete class type.
SILFunction *
findFuncInWitnessTable(SILDeclRef Member, CanType ConcreteTy,
                       ProtocolDecl *Proto, SILModule &Mod) {
  // Scan all of the witness tables in search of a matching protocol and class.
  for (SILWitnessTable &Witness : Mod.getWitnessTableList()) {
    ProtocolDecl *WitnessProtocol = Witness.getConformance()->getProtocol();

    // Is this the correct protocol?
    if (WitnessProtocol != Proto ||
        !ConcreteTy.getPointer()->isEqual(Witness.getConformance()->getType()))
      continue;

    // Okay, we found the correct witness table. Now look for the method.
    for (auto &Entry : Witness.getEntries()) {
      // Look at method entries only.
      if (Entry.getKind() != SILWitnessTable::WitnessKind::Method)
        continue;

      SILWitnessTable::MethodWitness MethodEntry = Entry.getMethodWitness();
      // Check if this is the member we were looking for.
      if (MethodEntry.Requirement != Member)
        continue;

      return MethodEntry.Witness;
    }
  }
  return nullptr;
}

void SILDevirtualizer::optimizeApplyInst(ApplyInst *AI) {
  DEBUG(llvm::dbgs() << " *** Trying to optimize : " << *AI);

  // Find call sites that may participate in deep devirtualization.
  collectPolyArgs(AI);

  // Devirtualize apply instructions that call archetype_method instructions:
  //
  //   %8 = archetype_method $Optional<UInt16>, #LogicValue.getLogicValue!1
  //   %9 = apply %8<Self = CodeUnit?>(%6#1) : ...
  //
  ArchetypeMethodInst *AMI = dyn_cast<ArchetypeMethodInst>(AI->getCallee());
  if (AMI && AMI->getConformance() ) {
    // Lookup the function reference in the witness tables.
    std::pair<SILWitnessTable *, ArrayRef<Substitution>> Ret =
      AI->getModule().lookUpWitnessTable(AMI->getConformance());

    if (!Ret.first)
      return;

    for (auto &Entry : Ret.first->getEntries()) {
      // Look at method entries only.
      if (Entry.getKind() != SILWitnessTable::WitnessKind::Method)
        continue;

      SILWitnessTable::MethodWitness MethodEntry = Entry.getMethodWitness();
      // Check if this is the member we were looking for.
      if (MethodEntry.Requirement != AMI->getMember())
        continue;

      SILBuilder Builder(AI);
      SILLocation Loc = AI->getLoc();
      SILFunction *F = MethodEntry.Witness;
      FunctionRefInst *FRI = Builder.createFunctionRef(Loc, F);

      // Collect the function arguments.
      SmallVector<SILValue, 4> Args;
      for (auto &A : AI->getArgumentOperands())
        Args.push_back(A.get());

      SmallVector<Substitution, 16> NewSubstList(Ret.second.begin(),
                                                 Ret.second.end());

      // Add the non-self-derived substitutions from the original application.
      assert(AI->getSubstitutions().size() && "Subst list must not be empty");
      assert(AI->getSubstitutions()[0].Archetype->getSelfProtocol() &&
             "The first substitution needs to be a 'self' substitution.");
      for (auto &origSub : AI->getSubstitutions().slice(1)) {
        if (!origSub.Archetype->isSelfDerived())
          NewSubstList.push_back(origSub);
      }

      ApplyInst *SAI = Builder.createApply(Loc, FRI,
                                           AI->getSubstCalleeSILType(),
                                           AI->getType(), NewSubstList, Args);
      AI->replaceAllUsesWith(SAI);
      AI->eraseFromParent();
      NumAMI++;
      Changed = true;
    }

    DEBUG(llvm::dbgs() << " *** Could not find a witness table.\n");
    return;
  }

  // Devirtualize protocol_method + project_existential + init_existential
  // instructions.  For example:
  //
  // %0 = alloc_box $Pingable
  // %1 = init_existential %0#1 : $*Pingable, $*Foo  <-- Foo is the static type!
  // %4 = project_existential %0#1 : $*Pingable to $*@sil_self Pingable
  // %5 = protocol_method %0#1 : $*Pingable, #Pingable.ping!1 :
  // %8 = apply %5(ARGUMENTS ... , %4) :

  // Find the protocol_method instruction.
  ProtocolMethodInst *PMI = dyn_cast<ProtocolMethodInst>(AI->getCallee());
  if (!PMI)
    return;

  // Find the last argument, which is the Self argument, which may be a
  // project_existential instruction.
  MutableArrayRef<Operand> Args = AI->getArgumentOperands();
  if (Args.size() < 1)
    return;

  SILValue LastArg = Args[Args.size() - 1].get();
  ProjectExistentialInst *PEI = dyn_cast<ProjectExistentialInst>(LastArg);
  if (!PEI)
    return;

  // Make sure that the project_existential and protocol_method instructions
  // use the same protocol.
  SILValue ProtocolObject = PMI->getOperand();
  if (PEI->getOperand().getDef() != ProtocolObject.getDef())
    return;

  DEBUG(llvm::dbgs() << " *** Protocol to devirtualize : "
                     << *ProtocolObject.getDef());

  // Find a single initialization point, and make sure the protocol is not
  // captured. We also handle the case where the initializer is the copy_addr
  // instruction by looking at the source object.
  SILInstruction *InitInst = findSingleInitNoCaptureProtocol(ProtocolObject);
  if (CopyAddrInst *CAI = dyn_cast_or_null<CopyAddrInst>(InitInst)) {
    if (!CAI->isInitializationOfDest() || !CAI->isTakeOfSrc())
      return;

    InitInst = findSingleInitNoCaptureProtocol(CAI->getSrc());
  }

  InitExistentialInst *Init = dyn_cast_or_null<InitExistentialInst>(InitInst);
  if (!Init)
    return;

  // Strip the InOut qualifier.
  CanType ConcreteTy = stripInOutQualifier(Init->getConcreteType());

  // For each protocol that our type conforms to:
  for (auto &Conf : Init->getConformances()) {
    SILFunction *StaticRef = findFuncInWitnessTable(PMI->getMember(),
                                                    ConcreteTy,
                                                    Conf->getProtocol(),
                                                    Init->getModule());
    if (!StaticRef)
      continue;

    DEBUG(llvm::dbgs() << " *** Devirtualized : " << *AI);
    ApplyInst *NewApply =
      replaceDynApplyWithStaticApply(AI, StaticRef, Init, PEI);
    collectPolyArgs(NewApply);
    NumDynApply++;
    Changed = true;
    return;
  }

  DEBUG(llvm::dbgs() << " *** Could not find a witness table for: " << *PMI);
}

void SILDevirtualizer::optimizeFuncBody(SILFunction *F) {
  DEBUG(llvm::dbgs() << "Devirtualizing: "
        << Demangle::demangleSymbolAsString(F->getName()) << "\n");
  for (auto &BB : *F) {
    auto I = BB.begin(), E = BB.end();
    while (I != E) {
      SILInstruction *Inst = I++; // Inst may be erased.
      if (ClassMethodInst *CMI = dyn_cast<ClassMethodInst>(Inst))
        optimizeClassMethodInst(CMI);
      else if (ApplyInst *AI = dyn_cast<ApplyInst>(Inst))
        optimizeApplyInst(AI);
    }
  }
}

bool SILDevirtualizer::run() {
  // Perform devirtualization locally and compute potential polymorphic
  // arguments for all existing functions.
  for (auto &F : *M)
    optimizeFuncBody(&F);

  AttemptToSpecialize = DevirtThreshold && !PolyArgMap.empty();
  while (AttemptToSpecialize) {
    AttemptToSpecialize = false;

    // Resolve argument types top-down over the call graph.
    // CallGraph::sort provides a post-ordering of the calls.
    // We reverse-visit this bottom-up ordering.
    std::vector<SILFunction *> BottomUpCalls;
    CallGraphOrder.sort(BottomUpCalls);
    for (auto &F : reversed(BottomUpCalls))
      for (auto &BB : *F)
        for (auto &I : BB)
          if (ApplyInst *AI = dyn_cast<ApplyInst>(&I))
            resolveArgs(AI);

    DEBUG(dumpPolyArgs());

    // Specialize functions based on known argument types.
    specializeForArgTypes();

    // Devirtualize newly specialized functions locally and compute potential
    // polymorphic arguments for them.
    while (!Worklist.empty()) {
      AttemptToSpecialize = true;
      SILFunction *F = Worklist.back();
      Worklist.pop_back();
      optimizeFuncBody(F);
    }
  }
  return Changed;
}

// TODO: This is a placeholder. It is very important not to invalidate
// the specializations until we have can rediscover them via demangling.
// DeadFunctionElimination needs a way to communicate that dead functions are
// invalid without killing all other analyses. We currently rely on the fact
// that we never query a dead function and can't reuse it's memory.
void SILSpecializedArgsAnalysis::invalidate(SILAnalysis::InvalidationKind K) {}

class SILDevirtualizationPass : public SILModuleTransform {
public:
  virtual ~SILDevirtualizationPass() {}

  /// The entry point to the transformation.
  virtual void run() {
    SILDevirtualizer DevirtImpl(getModule(),
                                getAnalysis<SILSpecializedArgsAnalysis>(),
                                getOptions().DevirtThreshold);
    bool Changed = DevirtImpl.run();
    if (Changed) {
      PM->scheduleAnotherIteration();
      invalidateAnalysis(SILAnalysis::InvalidationKind::CallGraph);
    }
  }

  StringRef getName() override { return "Devirtualization"; }
};

SILTransform *swift::createDevirtualization() {
  return new SILDevirtualizationPass();
}

SILAnalysis *swift::createSpecializedArgsAnalysis(SILModule *M) {
  return new SILSpecializedArgsAnalysis();
}

#ifndef NDEBUG
void SILDevirtualizer::dumpPolyArgs() {
  using llvm::dbgs;

  dbgs() << "--- Devirtualization Analysis ---\n";
  for (auto &F : *M) {
    auto PIter = PolyArgMap.find(&F);
    if (PIter != PolyArgMap.end()) {
      dbgs() << "Polymorphic Args for "
             << Demangle::demangleSymbolAsString(F.getName()) << ":\n";
      for (auto &arg : PIter->second) {
        dbgs() << "  "
               << (arg.isDirect() ? "Polymorphic " : "PassThruArg ")
               << *arg.getArg();
      }
    }
    auto RIter = ResolvedArgMap.find(&F);
    if (RIter != ResolvedArgMap.end()) {
      dbgs() << "Resolved Args for "
             << Demangle::demangleSymbolAsString(F.getName()) << ":\n";
      for (unsigned RIdx : RIter->second) {
        dbgs() << "  Specialization #" << RIdx << "\n";
        for (auto &ResArg : ResolvedArgTable[RIdx].ResArgs) {
          if (ResArg.Arg)
            dbgs() << *ResArg.Arg;
          else
            dbgs() << "NoArg\n";
          dbgs() << "    : ";
          if (ResArg.Ty.isNull())
            dbgs() << ResArg.Ty->getString();
          else
            dbgs() << "NoClass";
          dbgs() << "\n";
        }
        dbgs() << "  Called by:\n";
        for (auto *AI : ResolvedArgTable[RIdx].Applies) {
          dbgs() << "  " << Demangle::demangleSymbolAsString(
            AI->getFunction()->getName()) << "\n";
          dbgs() << "  " << *AI << "\n";
        }
      }
    }
  }
  if (!SpecializeCands.empty()) {
    dbgs() << "Specialization Candidates:\n";
    for (SILFunction *F : SpecializeCands)
      dbgs() << "  " << Demangle::demangleSymbolAsString(F->getName())
             << "\n";
  }
  dbgs() << "--- End Devirtualization Analysis ---\n";
}
#endif
