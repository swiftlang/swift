//===--- ExistentialSpecializer.cpp - Specialization of functions -----===//
//===---                 with existential arguments               -----===//
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
//
// Specialize functions with existential parameters to generic ones.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-existential-specializer"
#include "ExistentialSpecializer.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/GenericSignatureBuilder.h"
#include "swift/SIL/OptimizationRemark.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CFG.h"
#include "swift/SILOptimizer/Utils/Existential.h"
#include "swift/SILOptimizer/Utils/Generics.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SILOptimizer/Utils/SpecializationMangler.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"

using namespace swift;

STATISTIC(NumFunctionsWithExistentialArgsSpecialized,
          "Number of functions with existential args specialized");
using ArgIndexList = llvm::SmallVector<unsigned, 8>;

/// Find the set of return instructions in a function.
static void findReturnInsts(SILFunction *F,
                            llvm::SmallPtrSet<ReturnInst *, 4> &ReturnInsts) {
  for (auto &BB : *F) {
    TermInst *TI = BB.getTerminator();
    if (auto *RI = dyn_cast<ReturnInst>(TI)) {
      ReturnInsts.insert(RI);
    }
  }
}

/// Create a new function name for the newly generated protocol constrained
/// generic function.
std::string
ExistentialSpecializerTransform::createExistentialSpecializedFunctionName() {
  for (auto const &IdxIt : ExistentialArgDescriptor) {
    int Idx = IdxIt.first;
    Mangler.setArgumentExistentialToGeneric(Idx);
  }

  SILModule &M = F->getModule();
  int UniqueID = 0;
  std::string MangledName;
  do {
    MangledName = Mangler.mangle(UniqueID);
    ++UniqueID;
  } while (M.hasFunction(MangledName));
  return MangledName;
}

/// Create the signature for the newly generated protocol constrained generic
/// function.
CanSILFunctionType
ExistentialSpecializerTransform::createExistentialSpecializedFunctionType() {

  CanSILFunctionType FTy = F->getLoweredFunctionType();
  auto ExpectedFTy = F->getLoweredType().castTo<SILFunctionType>();
  SILModule &M = F->getModule();
  auto *Mod = F->getModule().getSwiftModule();
  auto &Ctx = M.getASTContext();
  GenericSignature *NewGenericSig;
  GenericEnvironment *NewGenericEnv;

  // Form a new generic signature based on the old one.
  GenericSignatureBuilder Builder(Ctx);

  /// If the original function is generic, then maintain the same.
  auto OrigGenericSig = FTy->getGenericSignature();
  /// First, add the old generic signature.
  Builder.addGenericSignature(OrigGenericSig);

  /// Original list of parameters
  SmallVector<SILParameterInfo, 4> params;
  params.append(ExpectedFTy->getParameters().begin(),
                ExpectedFTy->getParameters().end());

  int GPIdx = 0;
  /// Convert the protocol arguments of F to generic ones.
  for (auto const &IdxIt : ExistentialArgDescriptor) {
    int Idx = IdxIt.first;
    auto &param = params[Idx];
    auto PType = param.getType();

    assert(PType->is<ProtocolType>() || PType->is<ProtocolCompositionType>());
    /// Generate new generic parameter.
    auto *NewGenericParam = GenericTypeParamType::get(0, GPIdx++, Ctx);
    Builder.addGenericParameter(NewGenericParam);
    Requirement NewRequirement(RequirementKind::Conformance, NewGenericParam,
                               PType);
    auto Source =
        GenericSignatureBuilder::FloatingRequirementSource::forAbstract();
    Builder.addRequirement(NewRequirement, Source, Mod);
    Arg2GenericTypeMap[Idx] = NewGenericParam;
  }

  /// Compute the generic signature.
  NewGenericSig = std::move(Builder).computeGenericSignature(SourceLoc(), true);
  NewGenericEnv = NewGenericSig->createGenericEnvironment();

  /// Create a lambda for GenericParams.
  auto getCanonicalType = [&](Type t) -> CanType {
    return t->getCanonicalType(NewGenericSig);
  };

  /// Create the complete list of parameters.
  int Idx = 0;
  llvm::SmallVector<SILParameterInfo, 8> InterfaceParams;
  InterfaceParams.reserve(params.size());
  for (auto &param : params) {
    if (Arg2GenericTypeMap[Idx]) {
      auto GenericParam = Arg2GenericTypeMap[Idx];
      InterfaceParams.push_back(SILParameterInfo(getCanonicalType(GenericParam),
                                                 param.getConvention()));
    } else {
      auto paramIfaceTy = param.getType()->mapTypeOutOfContext();
      InterfaceParams.push_back(SILParameterInfo(getCanonicalType(paramIfaceTy),
                                                 param.getConvention()));
    }
    Idx++;
  }

  /// Now add the result type.
  llvm::SmallVector<SILResultInfo, 8> InterfaceResults;
  for (auto &result : ExpectedFTy->getResults()) {
    auto resultIfaceTy = result.getType()->mapTypeOutOfContext();
    auto InterfaceResult = result.getWithType(getCanonicalType(resultIfaceTy));
    InterfaceResults.push_back(InterfaceResult);
  }

  /// Now add the errors.
  Optional<SILResultInfo> InterfaceErrorResult;
  if (ExpectedFTy->hasErrorResult()) {
    auto errorResult = ExpectedFTy->getErrorResult();
    auto errorIfaceTy = errorResult.getType()->mapTypeOutOfContext();
    InterfaceErrorResult =
        SILResultInfo(getCanonicalType(errorIfaceTy),
                      ExpectedFTy->getErrorResult().getConvention());
  }

  /// Now add the yields.
  llvm::SmallVector<SILYieldInfo, 8> InterfaceYields;
  for (SILYieldInfo InterfaceYield : FTy->getYields()) {
    InterfaceYields.push_back(InterfaceYield);
  }

  /// Finally the ExtInfo.
  auto ExtInfo = FTy->getExtInfo();
  ExtInfo = ExtInfo.withRepresentation(SILFunctionTypeRepresentation::Thin);
  auto witnessMethodConformance = FTy->getWitnessMethodConformanceOrNone();

  /// Return the new signature.
  return SILFunctionType::get(
      NewGenericSig, ExtInfo, FTy->getCoroutineKind(),
      FTy->getCalleeConvention(), InterfaceParams, InterfaceYields,
      InterfaceResults, InterfaceErrorResult, Ctx, witnessMethodConformance);
}

/// Determine the arguments for the new function.
/// Copy the old body from F, but add a new init_existential.
/// to make the code compatible. Also rewrite the body.
void ExistentialSpecializerTransform::populateSpecializedGenericFunction() {

  SILModule &M = F->getModule();
  auto &Ctx = M.getASTContext();

  /// The arguments to the branch instruction.
  SmallVector<SILValue, 8> BranchArgs;

  /// Create a new debug scope.
  ScopeCloner SC(*NewF);
  auto DebugScope = SC.getOrCreateClonedScope(F->getDebugScope());

  /// Set Unqualified ownership, if any.
  if (!F->hasQualifiedOwnership()) {
    NewF->setUnqualifiedOwnership();
  }

  /// Create the entry basic block.
  SILBasicBlock *NewFBody = NewF->createBasicBlock();

  /// Builder to hold new instructions. It must have a ScopeClone with a
  /// debugscope that is inherited from the F.
  SILBuilder NewFBuilder(NewFBody);
  NewFBuilder.setCurrentDebugScope(DebugScope);
  SILOpenedArchetypesTracker OpenedArchetypesTrackerNewF(NewF);
  NewFBuilder.setOpenedArchetypesTracker(&OpenedArchetypesTrackerNewF);
  /// Determine the location for the new init_existential_ref.
  auto InsertLoc = F->begin()->begin()->getLoc();
  llvm::SmallDenseMap<int, SILInstruction *> Arg2AllocStackMap;
  bool MissingDestroyUse = false;
  for (auto &ArgDesc : ArgumentDescList) {
    /// For all Generic Arguments.
    if (Arg2GenericTypeMap[ArgDesc.Index]) {
      auto GenericParam = Arg2GenericTypeMap[ArgDesc.Index];
      SILType GenericSILType =
          M.Types.getLoweredType(NewF->mapTypeIntoContext(GenericParam));
      auto *NewArg = NewFBody->createFunctionArgument(GenericSILType);
      NewArg->setOwnershipKind(ValueOwnershipKind(
          M, GenericSILType, ArgDesc.Arg->getArgumentConvention()));
      /// Determine the Conformances.
      SmallVector<ProtocolConformanceRef, 1> NewConformances;
      auto ContextTy =
          NewF->mapTypeIntoContext(GenericParam->getCanonicalType());
      auto OpenedArchetype = ContextTy->getAs<ArchetypeType>();
      for (auto proto : OpenedArchetype->getConformsTo()) {
        NewConformances.push_back(ProtocolConformanceRef(proto));
      }
      ArrayRef<ProtocolConformanceRef> Conformances =
          Ctx.AllocateCopy(NewConformances);
      auto ExistentialRepr =
          ArgDesc.Arg->getType().getPreferredExistentialRepresentation(M);
      switch (ExistentialRepr) {
      case ExistentialRepresentation::Opaque: {
        /// Create this sequence for init_existential_addr.:
        /// bb0(%0 : $*T):
        /// %3 = alloc_stack $P
        /// %4 = init_existential_addr %3 : $*P, $T
        /// copy_addr %0 to [initialization] %4 : $*T
        /// destroy_addr %0 : $*T
        /// %7 = open_existential_addr immutable_access %3 : $*P to
        /// $*@opened("105977B0-D8EB-11E4-AC00-3C0754644993") P
        auto *ASI =
            NewFBuilder.createAllocStack(InsertLoc, ArgDesc.Arg->getType());
        Arg2AllocStackMap[ArgDesc.Index] = ASI;

        auto *EAI = NewFBuilder.createInitExistentialAddr(
            InsertLoc, ASI, NewArg->getType().getASTType()->getCanonicalType(),
            NewArg->getType(), Conformances);
        /// If DestroyAddr is already there, then do not use [take].
        NewFBuilder.createCopyAddr(
            InsertLoc, NewArg, EAI,
            ExistentialArgDescriptor[ArgDesc.Index].DestroyAddrUse
                ? IsTake_t::IsNotTake
                : IsTake_t::IsTake,
            IsInitialization_t::IsInitialization);
        if (ExistentialArgDescriptor[ArgDesc.Index].DestroyAddrUse) {
          NewFBuilder.createDestroyAddr(InsertLoc, NewArg);
        } else {
          MissingDestroyUse = true;
        }
        BranchArgs.push_back(ASI);
        break;
      }
      case ExistentialRepresentation::Class: {
        ///  Create an init_existential.
        /// %5 = init_existential_ref %0 : $T : $T, $P
        auto *InitRef = NewFBuilder.createInitExistentialRef(
            InsertLoc, ArgDesc.Arg->getType(),
            NewArg->getType().getASTType()->getCanonicalType(), NewArg,
            Conformances);
        BranchArgs.push_back(InitRef);
        break;
      }
      default: {
        assert(true);
        break;
      }
      };
    } else {
      auto NewArg = NewFBody->createFunctionArgument(ArgDesc.Arg->getType(),
                                                     ArgDesc.Decl);
      NewArg->setOwnershipKind(ValueOwnershipKind(
          M, ArgDesc.Arg->getType(), ArgDesc.Arg->getArgumentConvention()));
      BranchArgs.push_back(NewArg);
    }
  }

  /// Then we splice the body of F to NewF after the first basic block.
  auto It = NewF->begin();
  It++;
  NewF->getBlocks().splice(It, F->getBlocks());

  /// Determine the old entry.
  auto NewIt = NewF->begin();
  NewIt++;
  SILBasicBlock *OldEntryBB = &(*NewIt);

  /// Create a fake branch instruction, which will be eliminated by the merge
  /// function call below. Clean this up in the next version.
  NewFBuilder.createBranch(InsertLoc, OldEntryBB, BranchArgs);
  ///  Merge the first and second basic blocks since it is just a direct branch.
  ///  This is ugly though, with desired effect.
  mergeBasicBlockWithSuccessor(&(*(NewF->begin())), nullptr, nullptr);

  /// If there is an argument with no DestroyUse, insert DeallocStack
  /// before return Instruction.
  llvm::SmallPtrSet<ReturnInst *, 4> ReturnInsts;
  if (MissingDestroyUse)
    findReturnInsts(NewF, ReturnInsts);

  /// Walk over destroy_addr instructions and perform fixups.
  for (auto &ArgDesc : reversed(ArgumentDescList)) {
    int ArgIndex = ArgDesc.Index;
    if (Arg2AllocStackMap.count(ArgIndex) > 0) {
      auto *ASI = dyn_cast<AllocStackInst>(Arg2AllocStackMap[ArgIndex]);
      if (ExistentialArgDescriptor[ArgIndex].DestroyAddrUse) {
        for (Operand *ASIUse : ASI->getUses()) {
          auto *ASIUser = ASIUse->getUser();
          if (auto *DAI = dyn_cast<DestroyAddrInst>(ASIUser)) {
            SILBuilder Builder(ASIUser);
            Builder.setInsertionPoint(&*std::next(ASIUser->getIterator()));
            Builder.createDeallocStack(DAI->getLoc(), ASI);
          }
        }
      } else { // Need to insert DeallocStack before return.
        for (auto *I : ReturnInsts) {
          SILBuilder Builder(I->getParent());
          Builder.setInsertionPoint(I);
          Builder.createDeallocStack(ASI->getLoc(), ASI);
        }
      }
    }
  }
}

/// Create the Thunk Body with always_inline attribute.
void ExistentialSpecializerTransform::populateThunkBody() {

  SILModule &M = F->getModule();

  F->setThunk(IsThunk);
  F->setInlineStrategy(AlwaysInline);

  /// Create a basic block and the function arguments.
  SILBasicBlock *ThunkBody = F->createBasicBlock();
  for (auto &ArgDesc : ArgumentDescList) {
    ThunkBody->createFunctionArgument(ArgDesc.Arg->getType(), ArgDesc.Decl);
  }

  /// Builder to add new instructions in the Thunk.
  SILBuilder Builder(ThunkBody);
  SILOpenedArchetypesTracker OpenedArchetypesTracker(F);
  Builder.setOpenedArchetypesTracker(&OpenedArchetypesTracker);
  Builder.setCurrentDebugScope(ThunkBody->getParent()->getDebugScope());

  /// Location to insert new instructions.
  SILLocation Loc = ThunkBody->getParent()->getLocation();

  /// Create the function_ref instruction to the NewF.
  FunctionRefInst *FRI = Builder.createFunctionRef(Loc, NewF);

  auto GenCalleeType = NewF->getLoweredFunctionType();
  auto CalleeGenericSig = GenCalleeType->getGenericSignature();

  /// Determine arguments to Apply.
  /// Generate opened existentials for generics.
  llvm::SmallVector<SILValue, 8> ApplyArgs;

  /// Ideally this should be a SubstitutionMap. TODO.
  llvm::DenseMap<SubstitutableType *, Type> Generic2OpenedTypeMap;
  for (auto &ArgDesc : ArgumentDescList) {
    if (Arg2GenericTypeMap[ArgDesc.Index]) {
      ArchetypeType *Opened;
      SILValue OrigOperand = ThunkBody->getArgument(ArgDesc.Index);
      auto SwiftType = ArgDesc.Arg->getType().getASTType();
      auto OpenedType =
          SwiftType->openAnyExistentialType(Opened)->getCanonicalType();
      auto OpenedSILType = NewF->getModule().Types.getLoweredType(OpenedType);
      SILValue archetypeValue;
      auto ExistentialRepr =
          ArgDesc.Arg->getType().getPreferredExistentialRepresentation(M);
      switch (ExistentialRepr) {
      case ExistentialRepresentation::Opaque: {
        archetypeValue = Builder.createOpenExistentialAddr(
            Loc, OrigOperand, OpenedSILType, OpenedExistentialAccess::Mutable);
        ApplyArgs.push_back(archetypeValue);
        break;
      }
      case ExistentialRepresentation::Class: {
        archetypeValue =
            Builder.createOpenExistentialRef(Loc, OrigOperand, OpenedSILType);
        ApplyArgs.push_back(archetypeValue);
        break;
      }
      default: {
        assert(true);
        break;
      }
      };
      auto GenericParam = Arg2GenericTypeMap[ArgDesc.Index];
      Generic2OpenedTypeMap[GenericParam] = OpenedType;
    } else {
      auto CallerArg = ThunkBody->getArgument(ArgDesc.Index);
      auto SwiftType = CallerArg->getType().getASTType();
      if (auto *GP = SwiftType->getAs<GenericTypeParamType>()) {
        ArchetypeType *Opened;
        auto OpenedType =
            SwiftType->openAnyExistentialType(Opened)->getCanonicalType();
        Generic2OpenedTypeMap[GP] = OpenedType;
      }
      ApplyArgs.push_back(ThunkBody->getArgument(ArgDesc.Index));
    }
  }

  /// Create substitutions for Apply instructions.
  auto SubMap =
      SubstitutionMap::get(CalleeGenericSig,
                           [&](SubstitutableType *type) -> Type {
                             return Generic2OpenedTypeMap[type];
                           },
                           LookUpConformanceInSignature(*CalleeGenericSig));

  /// Perform the substitutions.
  auto SubstCalleeType = GenCalleeType->substGenericArgs(M, SubMap);
  SILType::getPrimitiveObjectType(SubstCalleeType);

  /// Obtain the Result Type.
  SILValue ReturnValue;
  SILType LoweredType = NewF->getLoweredType();
  SILFunctionConventions Conv(SubstCalleeType, M);
  SILType ResultType = Conv.getSILResultType();
  auto FunctionTy = LoweredType.castTo<SILFunctionType>();

  /// If the original function has error results,  we need to generate a
  /// try_apply to call a function with an error result.
  if (FunctionTy->hasErrorResult()) {
    SILFunction *Thunk = ThunkBody->getParent();
    SILBasicBlock *NormalBlock = Thunk->createBasicBlock();
    ReturnValue =
        NormalBlock->createPHIArgument(ResultType, ValueOwnershipKind::Owned);
    SILBasicBlock *ErrorBlock = Thunk->createBasicBlock();
    SILType Error =
        SILType::getPrimitiveObjectType(FunctionTy->getErrorResult().getType());
    auto *ErrorArg =
        ErrorBlock->createPHIArgument(Error, ValueOwnershipKind::Owned);
    Builder.createTryApply(Loc, FRI, SubMap, ApplyArgs, NormalBlock,
                           ErrorBlock);

    Builder.setInsertionPoint(ErrorBlock);
    Builder.createThrow(Loc, ErrorArg);
    Builder.setInsertionPoint(NormalBlock);
  } else {
    /// Create the Apply with substitutions
    ReturnValue = Builder.createApply(Loc, FRI, SubMap, ApplyArgs, false);
  }

  /// Set up the return results.
  if (NewF->isNoReturnFunction()) {
    Builder.createUnreachable(Loc);
  } else {
    Builder.createReturn(Loc, ReturnValue);
  }
}

/// Strategy to specialize existentail arguments:
/// (1) Create a protocol constrained generic function from the old function;
/// (2) Create a thunk for the original function that invokes (1) including
/// setting
///     its inline strategy as always inline.
void ExistentialSpecializerTransform::createExistentialSpecializedFunction() {
  SILModule &M = F->getModule();
  std::string Name = createExistentialSpecializedFunctionName();
  SILLinkage linkage = getSpecializedLinkage(F, F->getLinkage());

  /// Create devirtualized function type.
  auto NewFTy = createExistentialSpecializedFunctionType();

  auto NewFGenericSig = NewFTy->getGenericSignature();
  auto NewFGenericEnv = NewFGenericSig->createGenericEnvironment();

  /// Step 1: Create the new protocol constrained generic function.
  NewF = M.createFunction(linkage, Name, NewFTy, NewFGenericEnv,
                          F->getLocation(), F->isBare(), F->isTransparent(),
                          F->isSerialized(), F->getEntryCount(), F->isThunk(),
                          F->getClassSubclassScope(), F->getInlineStrategy(),
                          F->getEffectsKind(), nullptr, F->getDebugScope());

  /// Populate the body of NewF.
  populateSpecializedGenericFunction();

  /// Step 2: Create the thunk with always_inline and populate its body.
  populateThunkBody();

  assert(F->getDebugScope()->Parent != NewF->getDebugScope()->Parent);

  DEBUG(llvm::dbgs() << "After ExistentialSpecializer Pass\n"; F->dump();
        NewF->dump(););
}

namespace {

/// ExistentialSpecializer class.
class ExistentialSpecializer : public SILFunctionTransform {

  /// Determine if the current function is a target for existential
  /// specialization of args.
  bool canSpecializeExistentialArgsInFunction(
      ApplySite &Apply,
      llvm::SmallDenseMap<int, ExistentialTransformArgumentDescriptor>
          &ExistentialArgDescriptor);

  /// Can Callee be specialized?
  bool canSpecializeCalleeFunction(ApplySite &Apply);

  /// Specialize existential args in function F.
  void specializeExistentialArgsInAppliesWithinFunction(SILFunction &F);

  /// CallerAnalysis information.
  CallerAnalysis *CA;

public:
  void run() override {

    auto *F = getFunction();

    /// Don't optimize functions that should not be optimized.
    if (!F->shouldOptimize()) {
      return;
    }

    /// Get CallerAnalysis information handy.
    CA = PM->getAnalysis<CallerAnalysis>();

    /// Perform specialization.
    specializeExistentialArgsInAppliesWithinFunction(*F);
  }
};
} // namespace

/// Find concrete type from init_existential refs/addrs.
static bool findConcreteTypeFromIE(SILValue Arg, CanType &ConcreteType) {
  if (auto *IER = dyn_cast<InitExistentialRefInst>(Arg)) {
    ConcreteType = IER->getFormalConcreteType();
    return true;
  } else if (auto *IE = dyn_cast<InitExistentialAddrInst>(Arg)) {
    ConcreteType = IE->getFormalConcreteType();
    return true;
  }
  return false;
}

/// Find the concrete type of the existential argument. Wrapper
/// for findInitExistential in Local.h/cpp. In future, this code
/// can move to Local.h/cpp.
static bool findConcreteType(ApplySite AI, SILValue Arg, int ArgIdx,
                             CanType &ConcreteType) {
  bool isCopied = false;

  /// Ignore any unconditional cast instructions. Is it Safe? Do we need to
  /// also add UnconditionalCheckedCastAddrInst? TODO.
  if (auto *Instance = dyn_cast<UnconditionalCheckedCastInst>(Arg)) {
    Arg = Instance->getOperand();
  }

  /// Return init_existential if the Arg is global_addr.
  if (auto *GAI = dyn_cast<GlobalAddrInst>(Arg)) {
    SILValue InitExistential =
        findInitExistentialFromGlobalAddrAndApply(GAI, AI, ArgIdx);
    /// If the Arg is already init_existential, return the concrete type.
    if (findConcreteTypeFromIE(InitExistential, ConcreteType)) {
      return true;
    }
  }

  SILValue OrigArg = Arg;
  /// Handle AllocStack instruction separately.
  if (auto *Instance = dyn_cast<AllocStackInst>(Arg)) {
    if (SILValue Src =
            getAddressOfStackInit(Instance, AI.getInstruction(), isCopied)) {
      Arg = Src;
    }
  }

  /// If the Arg is already init_existential after getAddressofStackInit
  /// call, return the concrete type.
  if (findConcreteTypeFromIE(Arg, ConcreteType)) {
    return true;
  }

  /// Call findInitExistential and determine the init_existential.
  ArchetypeType *OpenedArchetype = nullptr;
  SILValue OpenedArchetypeDef;
  auto FAS = FullApplySite::isa(AI.getInstruction());
  if (!FAS)
    return false;
  SILInstruction *InitExistential = findInitExistential(
      FAS, OrigArg, OpenedArchetype, OpenedArchetypeDef, isCopied);
  if (!InitExistential) {
    DEBUG(llvm::dbgs() << "ExistentialSpecializer Pass: Bail! Due to "
                          "findInitExistential\n";);
    return false;
  }

  /// Return the concrete type from init_existential returned from
  /// findInitExistential.
  if (auto *IER = dyn_cast<InitExistentialRefInst>(InitExistential)) {
    ConcreteType = IER->getFormalConcreteType();
    return true;
  } else if (auto *IE = dyn_cast<InitExistentialAddrInst>(InitExistential)) {
    ConcreteType = IE->getFormalConcreteType();
    return true;
  }

  return false;
}

/// Check if the callee uses the arg in a destroy_use instruction.
static void
findIfCalleeUsesArgInDestroyUse(SILValue Arg,
                                ExistentialTransformArgumentDescriptor &ETAD) {
  for (Operand *ArgUse : Arg->getUses()) {
    auto *ArgUser = ArgUse->getUser();
    if (isa<DestroyAddrInst>(ArgUser)) {
      ETAD.DestroyAddrUse = true;
      break;
    }
  }
}

/// Check if any argument to a function meet the criteria for specialization.
bool ExistentialSpecializer::canSpecializeExistentialArgsInFunction(
    ApplySite &Apply,
    llvm::SmallDenseMap<int, ExistentialTransformArgumentDescriptor>
        &ExistentialArgDescriptor) {
  auto *F = Apply.getReferencedFunction();
  auto Args = F->begin()->getFunctionArguments();
  bool returnFlag = false;
  const CallerAnalysis::FunctionInfo &FuncInfo = CA->getCallerInfo(F);
  auto *PAI = dyn_cast<PartialApplyInst>(Apply.getInstruction());
  int minPartialAppliedArgs = FuncInfo.getMinPartialAppliedArgs();

  /// Analyze the argument for protocol conformance.
  for (unsigned Idx = 0, Num = Args.size(); Idx < Num; ++Idx) {
    if (PAI && (Idx < Num - minPartialAppliedArgs))
      continue;
    auto Arg = Args[Idx];
    auto ArgType = Arg->getType();
    auto SwiftArgType = ArgType.getASTType();
    if (!ArgType || !SwiftArgType || !(ArgType.isExistentialType()) ||
        ArgType.isAnyObject() || SwiftArgType->isAny())
      continue;
    auto ExistentialRepr =
        Arg->getType().getPreferredExistentialRepresentation(F->getModule());
    if (!(ExistentialRepr == ExistentialRepresentation::Opaque ||
          ExistentialRepr == ExistentialRepresentation::Class))
      continue;

    /// Find concrete type.
    CanType ConcreteType;
    int ApplyArgIdx = PAI ? Idx - Num + minPartialAppliedArgs : Idx;
    auto ApplyArg = Apply.getArgument(ApplyArgIdx);
    if (!findConcreteType(Apply, ApplyArg, ApplyArgIdx, ConcreteType)) {
      DEBUG(llvm::dbgs()
                << "ExistentialSpecializer Pass: Bail! Due to findConcreteType "
                   "for callee:"
                << F->getName() << " in caller:"
                << Apply.getInstruction()->getParent()->getParent()->getName()
                << "\n";);
      continue;
    }

    /// Find attributes of the argument: destroy_use, immutable_access.
    ExistentialTransformArgumentDescriptor ETAD;
    ETAD.AccessType = OpenedExistentialAccess::Immutable;
    ETAD.DestroyAddrUse = false;
    if ((Args[Idx]->getType().getPreferredExistentialRepresentation(
            F->getModule())) != ExistentialRepresentation::Class)
      findIfCalleeUsesArgInDestroyUse(Arg, ETAD);

    /// Save the protocol declaration.
    ExistentialArgDescriptor[Idx] = ETAD;
    DEBUG(llvm::dbgs() << "ExistentialSpecializer Pass:Function: "
                       << F->getName() << " Arg:" << Idx
                       << "has a concrete type.\n");
    returnFlag |= true;
  }
  return returnFlag;
}

/// Can we specialize this function?
bool ExistentialSpecializer::canSpecializeCalleeFunction(ApplySite &Apply) {

  /// Disallow generic callees.
  if (Apply.hasSubstitutions()) {
    return false;
  }

  /// Determine the caller of the apply.
  auto *Callee = Apply.getReferencedFunction();
  if (!Callee)
    return false;

  /// Callee should be optimizable.
  if (!Callee->shouldOptimize())
    return false;

  /// External function definitions.
  if ((!Callee->isDefinition()) || Callee->isExternalDeclaration())
    return false;

  /// For now ignore functions with indirect results.
  if (Callee->getConventions().hasIndirectSILResults())
    return false;

  /// Do not optimize always_inlinable functions.
  if (Callee->getInlineStrategy() == Inline_t::AlwaysInline)
    return false;

  /// Only choose a select few function representations for specilization.
  if (Callee->getRepresentation() ==
          SILFunctionTypeRepresentation::ObjCMethod ||
      Callee->getRepresentation() == SILFunctionTypeRepresentation::Block) {
    return false;
  }
  return true;
}

/// Specialize existential args passed to a function.
void ExistentialSpecializer::specializeExistentialArgsInAppliesWithinFunction(
    SILFunction &F) {
  bool Changed = false;
  for (auto &BB : F) {
    for (auto It = BB.begin(), End = BB.end(); It != End; ++It) {
      auto *I = &*It;

      /// Is it an apply site?
      ApplySite Apply = ApplySite::isa(I);
      if (!Apply)
        continue;

      /// Can the callee be specialized?
      if (!canSpecializeCalleeFunction(Apply)) {
        DEBUG(llvm::dbgs() << "ExistentialSpecializer Pass: Bail! Due to "
                              "canSpecializeCalleeFunction.\n";
              I->dump(););
        continue;
      }

      auto *Callee = Apply.getReferencedFunction();

      /// Determine the arguments that can be specialized.
      llvm::SmallDenseMap<int, ExistentialTransformArgumentDescriptor>
          ExistentialArgDescriptor;
      if (!canSpecializeExistentialArgsInFunction(Apply,
                                                  ExistentialArgDescriptor)) {
        DEBUG(llvm::dbgs()
              << "ExistentialSpecializer Pass: Bail! Due to "
                 "canSpecializeExistentialArgsInFunction in function: "
              << Callee->getName() << " -> abort\n");
        continue;
      }

      DEBUG(llvm::dbgs() << "ExistentialSpecializer Pass: Function::"
                         << Callee->getName()
                         << " has an existential argument and can be optimized "
                            "via ExistentialSpecializer\n");

      /// Name Mangler for naming the protocol constrained generic method.
      auto P = Demangle::SpecializationPass::FunctionSignatureOpts;
      Mangle::FunctionSignatureSpecializationMangler Mangler(
          P, Callee->isSerialized(), Callee);

      /// Save the arguments in a descriptor.
      llvm::SmallVector<ArgumentDescriptor, 4> ArgumentDescList;
      auto Args = Callee->begin()->getFunctionArguments();
      for (unsigned i = 0, e = Args.size(); i != e; ++i) {
        ArgumentDescList.emplace_back(Args[i]);
      }

      /// This is the function to optimize for existential specilizer.
      DEBUG(llvm::dbgs()
            << "*** Running ExistentialSpecializer Pass on function: "
            << Callee->getName() << " ***\n");

      /// Instantiate the ExistentialSpecializerTransform pass.
      ExistentialSpecializerTransform EST(Callee, Mangler, ArgumentDescList,
                                          ExistentialArgDescriptor);

      /// Run the protocol devirtualizer.
      Changed = EST.run();

      if (Changed) {
        /// Update statistics on the number of functions devirtualized.
        ++NumFunctionsWithExistentialArgsSpecialized;

        /// Make sure the PM knows about the new specialized inner function.
        notifyAddFunction(EST.getExistentialSpecializedFunction(), Callee);

        /// Invalidate analysis results of Callee.
        PM->invalidateAnalysis(Callee,
                               SILAnalysis::InvalidationKind::Everything);
      }
    }
  }
  return;
}

SILTransform *swift::createExistentialSpecializer() {
  return new ExistentialSpecializer();
}
