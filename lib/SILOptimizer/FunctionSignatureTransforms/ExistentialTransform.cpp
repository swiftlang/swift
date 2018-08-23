//===------- ExistentialTransform.cpp - Transform Existential Args -------===//
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
// Transform existential parameters to generic ones.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-existential-transform"
#include "ExistentialTransform.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/GenericSignatureBuilder.h"
#include "swift/SIL/OptimizationRemark.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/TypeSubstCloner.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CFG.h"
#include "swift/SILOptimizer/Utils/Existential.h"
#include "swift/SILOptimizer/Utils/Generics.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"
#include "swift/SILOptimizer/Utils/SpecializationMangler.h"
#include "llvm/ADT/SmallVector.h"

using namespace swift;

/// Create a SILCloner for Existential Specilizer.
namespace {
class ExistentialSpecializerCloner
    : public TypeSubstCloner<ExistentialSpecializerCloner,
                             SILOptFunctionBuilder> {
  using SuperTy =
      TypeSubstCloner<ExistentialSpecializerCloner, SILOptFunctionBuilder>;
  friend class SILInstructionVisitor<ExistentialSpecializerCloner>;
  friend class SILCloner<ExistentialSpecializerCloner>;

  SILFunction *OrigF;
  llvm::SmallVector<ArgumentDescriptor, 4> &ArgumentDescList;
  llvm::SmallDenseMap<int, GenericTypeParamType *> &ArgToGenericTypeMap;
  llvm::SmallDenseMap<int, ExistentialTransformArgumentDescriptor>
      &ExistentialArgDescriptor;

protected:
  void postProcess(SILInstruction *Orig, SILInstruction *Cloned) {
    SILClonerWithScopes<ExistentialSpecializerCloner>::postProcess(Orig,
                                                                   Cloned);
  }

public:
  ExistentialSpecializerCloner(
      SILFunction *OrigF, SILFunction *NewF, SubstitutionMap Subs,
      llvm::SmallVector<ArgumentDescriptor, 4> &ArgumentDescList,
      llvm::SmallDenseMap<int, GenericTypeParamType *> &ArgToGenericTypeMap,
      llvm::SmallDenseMap<int, ExistentialTransformArgumentDescriptor>
          &ExistentialArgDescriptor)
      : SuperTy(*NewF, *OrigF, Subs), OrigF(OrigF),
        ArgumentDescList(ArgumentDescList),
        ArgToGenericTypeMap(ArgToGenericTypeMap),
        ExistentialArgDescriptor(ExistentialArgDescriptor) {}
  void cloneAndPopulateFunction();
};
} // end anonymous namespace

/// This function will create the generic version.
void ExistentialSpecializerCloner::cloneAndPopulateFunction() {
  // Get the Builder and the NewF
  SILBuilder &NewFBuilder = getBuilder();
  SILFunction &NewF = NewFBuilder.getFunction();

  auto NewFTy = NewF.getLoweredFunctionType();
  SmallVector<SILParameterInfo, 4> params;
  params.append(NewFTy->getParameters().begin(), NewFTy->getParameters().end());

  /// Builder will have a ScopeClone with a debugscope that is inherited from
  /// the F.
  ScopeCloner SC(NewF);
  auto DebugScope = SC.getOrCreateClonedScope(OrigF->getDebugScope());
  NewFBuilder.setCurrentDebugScope(DebugScope);
  SILOpenedArchetypesTracker OpenedArchetypesTrackerNewF(&NewF);
  NewFBuilder.setOpenedArchetypesTracker(&OpenedArchetypesTrackerNewF);

  // Create the entry basic block with the function arguments.
  SILBasicBlock *OrigEntryBB = &*OrigF->begin();
  SILBasicBlock *ClonedEntryBB = NewF.createBasicBlock();
  SILModule &M = OrigF->getModule();
  auto &Ctx = M.getASTContext();
  llvm::SmallDenseMap<int, AllocStackInst *> ArgToAllocStackMap;
  bool MissingDestroyUse = false;

  NewFBuilder.setInsertionPoint(ClonedEntryBB);

  llvm::SmallVector<SILValue, 4> entryArgs;
  entryArgs.reserve(OrigEntryBB->getArguments().size());

  /// Determine the location for the new init_existential_ref.
  auto InsertLoc = OrigF->begin()->begin()->getLoc();
  for (auto &ArgDesc : ArgumentDescList) {
    auto iter = ArgToGenericTypeMap.find(ArgDesc.Index);
    SILArgument *NewArg = nullptr;
    /// For all Generic Arguments.
    if (iter != ArgToGenericTypeMap.end()) {
      auto GenericParam = iter->second;
      SILType GenericSILType =
          M.Types.getLoweredType(NewF.mapTypeIntoContext(GenericParam));
      NewArg = ClonedEntryBB->createFunctionArgument(GenericSILType);
      NewArg->setOwnershipKind(ValueOwnershipKind(
          M, GenericSILType, ArgDesc.Arg->getArgumentConvention()));
      /// Determine the Conformances.
      SmallVector<ProtocolConformanceRef, 1> NewConformances;
      auto ContextTy = NewF.mapTypeIntoContext(GenericParam);
      auto OpenedArchetype = ContextTy->castTo<ArchetypeType>();
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
        /// $*@opened P
        auto *ASI =
            NewFBuilder.createAllocStack(InsertLoc, ArgDesc.Arg->getType());
        ArgToAllocStackMap.insert(
            std::pair<int, AllocStackInst *>(ArgDesc.Index, ASI));

        auto *EAI = NewFBuilder.createInitExistentialAddr(
            InsertLoc, ASI, NewArg->getType().getASTType(), NewArg->getType(),
            Conformances);
        /// If DestroyAddr is already there, then do not use [take].
        NewFBuilder.createCopyAddr(
            InsertLoc, NewArg, EAI,
            ExistentialArgDescriptor[ArgDesc.Index].AccessType ==
                    OpenedExistentialAccess::Immutable
                ? IsTake_t::IsNotTake
                : IsTake_t::IsTake,
            IsInitialization_t::IsInitialization);
        if (ExistentialArgDescriptor[ArgDesc.Index].DestroyAddrUse) {
          NewFBuilder.createDestroyAddr(InsertLoc, NewArg);
        } else {
          MissingDestroyUse = true;
        }
        entryArgs.push_back(ASI);
        break;
      }
      case ExistentialRepresentation::Class: {
        ///  Simple case: Create an init_existential.
        /// %5 = init_existential_ref %0 : $T : $T, $P
        auto *InitRef = NewFBuilder.createInitExistentialRef(
            InsertLoc, ArgDesc.Arg->getType(), NewArg->getType().getASTType(),
            NewArg, Conformances);
        entryArgs.push_back(InitRef);
        break;
      }
      default: {
        llvm_unreachable("Unhandled existential type in ExistentialTransform!");
        break;
      }
      };
    } else {
      /// Arguments that are not rewritten.
      auto Ty = params[ArgDesc.Index].getType();
      auto LoweredTy = M.Types.getLoweredType(NewF.mapTypeIntoContext(Ty));
      auto MappedTy = LoweredTy.getCategoryType(ArgDesc.Arg->getType().getCategory());
      NewArg = ClonedEntryBB->createFunctionArgument(MappedTy, ArgDesc.Decl);
      NewArg->setOwnershipKind(ValueOwnershipKind(
          M, MappedTy, ArgDesc.Arg->getArgumentConvention()));
      entryArgs.push_back(NewArg);
    }
  }

  // Visit original BBs in depth-first preorder, starting with the
  // entry block, cloning all instructions and terminators.
  cloneFunctionBody(&Original, ClonedEntryBB, entryArgs);

  /// If there is an argument with no DestroyUse, insert DeallocStack
  /// before return Instruction.
  llvm::SmallPtrSet<ReturnInst *, 4> ReturnInsts;
  if (MissingDestroyUse) {
    /// Find the set of return instructions in a function.
    for (auto &BB : NewF) {
      TermInst *TI = BB.getTerminator();
      if (auto *RI = dyn_cast<ReturnInst>(TI)) {
        ReturnInsts.insert(RI);
      }
    }
  }

  /// Walk over destroy_addr instructions and perform fixups.
  for (auto &ArgDesc : reversed(ArgumentDescList)) {
    int ArgIndex = ArgDesc.Index;
    auto iter = ArgToAllocStackMap.find(ArgIndex);
    if (iter != ArgToAllocStackMap.end()) {
      auto it = ExistentialArgDescriptor.find(ArgIndex);
      if (it != ExistentialArgDescriptor.end() && it->second.DestroyAddrUse) {
        for (Operand *ASIUse : iter->second->getUses()) {
          auto *ASIUser = ASIUse->getUser();
          if (auto *DAI = dyn_cast<DestroyAddrInst>(ASIUser)) {
            SILBuilder Builder(ASIUser);
            Builder.setInsertionPoint(&*std::next(ASIUser->getIterator()));
            Builder.createDeallocStack(DAI->getLoc(), iter->second);
          }
        }
      } else { // Need to insert DeallocStack before return.
        for (auto *I : ReturnInsts) {
          SILBuilder Builder(I->getParent());
          Builder.setInsertionPoint(I);
          Builder.createDeallocStack(iter->second->getLoc(), iter->second);
        }
      }
    }
  }
}

/// Create a new function name for the newly generated protocol constrained
/// generic function.
std::string ExistentialTransform::createExistentialSpecializedFunctionName() {
  for (auto const &IdxIt : ExistentialArgDescriptor) {
    int Idx = IdxIt.first;
    Mangler.setArgumentExistentialToGeneric(Idx);
  }
  auto MangledName = Mangler.mangle();
  assert(!F->getModule().hasFunction(MangledName));
  return MangledName;
}

/// Convert all existential argument types to generic argument type.
void ExistentialTransform::convertExistentialArgTypesToGenericArgTypes(
    GenericSignatureBuilder &Builder) {

  SILModule &M = F->getModule();
  auto *Mod = M.getSwiftModule();
  auto &Ctx = M.getASTContext();
  auto FTy = F->getLoweredFunctionType();

  /// If the original function is generic, then maintain the same.
  auto OrigGenericSig = FTy->getGenericSignature();

  /// Original list of parameters
  SmallVector<SILParameterInfo, 4> params;
  params.append(FTy->getParameters().begin(), FTy->getParameters().end());

  /// Determine the existing generic parameter depth.
  int Depth = 0;
  if (OrigGenericSig != nullptr) {
    Depth = OrigGenericSig->getGenericParams().back()->getDepth() + 1;
  }

  /// Index of the Generic Parameter.
  int GPIdx = 0;

  /// Convert the protocol arguments of F to generic ones.
  for (auto const &IdxIt : ExistentialArgDescriptor) {
    int Idx = IdxIt.first;
    auto &param = params[Idx];
    auto PType = param.getType();
    assert(PType.isExistentialType());
    /// Generate new generic parameter.
    auto *NewGenericParam = GenericTypeParamType::get(Depth, GPIdx++, Ctx);
    Builder.addGenericParameter(NewGenericParam);
    Requirement NewRequirement(RequirementKind::Conformance, NewGenericParam,
                               PType);
    auto Source =
        GenericSignatureBuilder::FloatingRequirementSource::forAbstract();
    Builder.addRequirement(NewRequirement, Source, Mod);
    ArgToGenericTypeMap.insert(
        std::pair<int, GenericTypeParamType *>(Idx, NewGenericParam));
    assert(ArgToGenericTypeMap.find(Idx) != ArgToGenericTypeMap.end());
  }
}

/// Create the signature for the newly generated protocol constrained generic
/// function.
CanSILFunctionType
ExistentialTransform::createExistentialSpecializedFunctionType() {
  auto FTy = F->getLoweredFunctionType();
  SILModule &M = F->getModule();
  auto &Ctx = M.getASTContext();
  GenericSignature *NewGenericSig;
  GenericEnvironment *NewGenericEnv;

  // Form a new generic signature based on the old one.
  GenericSignatureBuilder Builder(Ctx);

  /// If the original function is generic, then maintain the same.
  auto OrigGenericSig = FTy->getGenericSignature();
  /// First, add the old generic signature.
  Builder.addGenericSignature(OrigGenericSig);

  /// Convert existential argument types to generic argument types.
  convertExistentialArgTypesToGenericArgTypes(Builder);

  /// Compute the updated generic signature.
  NewGenericSig = std::move(Builder).computeGenericSignature(
      SourceLoc(), /*allowConcreteGenericParams=*/true);
  NewGenericEnv = NewGenericSig->createGenericEnvironment();

  /// Create a lambda for GenericParams.
  auto getCanonicalType = [&](Type t) -> CanType {
    return t->getCanonicalType(NewGenericSig);
  };

  /// Original list of parameters
  SmallVector<SILParameterInfo, 4> params;
  params.append(FTy->getParameters().begin(), FTy->getParameters().end());

  /// Create the complete list of parameters.
  int Idx = 0;
  llvm::SmallVector<SILParameterInfo, 8> InterfaceParams;
  InterfaceParams.reserve(params.size());
  for (auto &param : params) {
    auto iter = ArgToGenericTypeMap.find(Idx);
    if (iter != ArgToGenericTypeMap.end()) {
      auto GenericParam = iter->second;
      InterfaceParams.push_back(SILParameterInfo(getCanonicalType(GenericParam),
                                                 param.getConvention()));
    } else {
      InterfaceParams.push_back(param);
    }
    Idx++;
  }

  // Add error results.
  Optional<SILResultInfo> InterfaceErrorResult;
  if (FTy->hasErrorResult()) {
    InterfaceErrorResult = FTy->getErrorResult();
  }

  /// Finally the ExtInfo.
  auto ExtInfo = FTy->getExtInfo();
  ExtInfo = ExtInfo.withRepresentation(SILFunctionTypeRepresentation::Thin);
  auto witnessMethodConformance = FTy->getWitnessMethodConformanceOrNone();

  /// Return the new signature.
  return SILFunctionType::get(
      NewGenericSig, ExtInfo, FTy->getCoroutineKind(),
      FTy->getCalleeConvention(), InterfaceParams, FTy->getYields(),
      FTy->getResults(), InterfaceErrorResult, Ctx, witnessMethodConformance);
}

/// Create the Thunk Body with always_inline attribute.
void ExistentialTransform::populateThunkBody() {

  SILModule &M = F->getModule();

  F->setThunk(IsSignatureOptimizedThunk);
  F->setInlineStrategy(AlwaysInline);

  /// Remove original body of F.
  for (auto It = F->begin(), End = F->end(); It != End;) {
    auto *BB = &*It++;
    removeDeadBlock(BB);
  }

  /// Create a basic block and the function arguments.
  auto *ThunkBody = F->createBasicBlock();
  for (auto &ArgDesc : ArgumentDescList) {
    ThunkBody->createFunctionArgument(ArgDesc.Arg->getType(), ArgDesc.Decl);
  }

  /// Builder to add new instructions in the Thunk.
  SILBuilder Builder(ThunkBody);
  SILOpenedArchetypesTracker OpenedArchetypesTracker(F);
  Builder.setOpenedArchetypesTracker(&OpenedArchetypesTracker);
  Builder.setCurrentDebugScope(ThunkBody->getParent()->getDebugScope());

  /// Location to insert new instructions.
  auto Loc = ThunkBody->getParent()->getLocation();

  /// Create the function_ref instruction to the NewF.
  auto *FRI = Builder.createFunctionRef(Loc, NewF);

  auto GenCalleeType = NewF->getLoweredFunctionType();
  auto CalleeGenericSig = GenCalleeType->getGenericSignature();
  auto OrigGenCalleeType = F->getLoweredFunctionType();
  auto OrigCalleeGenericSig = OrigGenCalleeType->getGenericSignature();

  /// Determine arguments to Apply.
  /// Generate opened existentials for generics.
  llvm::SmallVector<SILValue, 8> ApplyArgs;
  llvm::SmallDenseMap<GenericTypeParamType *, Type> GenericToOpenedTypeMap;
  for (auto &ArgDesc : ArgumentDescList) {
    auto iter = ArgToGenericTypeMap.find(ArgDesc.Index);
    auto it = ExistentialArgDescriptor.find(ArgDesc.Index);
    if (iter != ArgToGenericTypeMap.end() &&
        it != ExistentialArgDescriptor.end()) {
      ArchetypeType *Opened;
      auto OrigOperand = ThunkBody->getArgument(ArgDesc.Index);
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
            Loc, OrigOperand, OpenedSILType, it->second.AccessType);
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
        llvm_unreachable("Unhandled existential type in ExistentialTransform!");
        break;
      }
      };
      GenericToOpenedTypeMap.insert(
          std::pair<GenericTypeParamType *, Type>(iter->second, OpenedType));
      assert(GenericToOpenedTypeMap.find(iter->second) !=
             GenericToOpenedTypeMap.end());
    } else {
      ApplyArgs.push_back(ThunkBody->getArgument(ArgDesc.Index));
    }
  }

  unsigned int OrigDepth = 0;
  if (F->getLoweredFunctionType()->isPolymorphic()) {
    OrigDepth = OrigCalleeGenericSig->getGenericParams().back()->getDepth() + 1;
  }
  SubstitutionMap OrigSubMap = F->getForwardingSubstitutionMap();

  /// Create substitutions for Apply instructions.
  auto SubMap = SubstitutionMap::get(
      CalleeGenericSig,
      [&](SubstitutableType *type) -> Type {
        if (auto *GP = dyn_cast<GenericTypeParamType>(type)) {
          if (GP->getDepth() < OrigDepth) {
            return Type(GP).subst(OrigSubMap);
          } else {
            auto iter = GenericToOpenedTypeMap.find(GP);
            assert(iter != GenericToOpenedTypeMap.end());
            return iter->second;
          }
        } else {
          return type;
        }
      },
      MakeAbstractConformanceForGenericType());

  /// Perform the substitutions.
  auto SubstCalleeType = GenCalleeType->substGenericArgs(M, SubMap);

  /// Obtain the Result Type.
  SILValue ReturnValue;
  auto FunctionTy = NewF->getLoweredFunctionType();
  SILFunctionConventions Conv(SubstCalleeType, M);
  SILType ResultType = Conv.getSILResultType();

  /// If the original function has error results,  we need to generate a
  /// try_apply to call a function with an error result.
  if (FunctionTy->hasErrorResult()) {
    SILFunction *Thunk = ThunkBody->getParent();
    SILBasicBlock *NormalBlock = Thunk->createBasicBlock();
    ReturnValue =
        NormalBlock->createPhiArgument(ResultType, ValueOwnershipKind::Owned);
    SILBasicBlock *ErrorBlock = Thunk->createBasicBlock();

    SILType Error = Conv.getSILType(FunctionTy->getErrorResult());
    auto *ErrorArg =
        ErrorBlock->createPhiArgument(Error, ValueOwnershipKind::Owned);
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

/// Strategy to specialize existential arguments:
/// (1) Create a protocol constrained generic function from the old function;
/// (2) Create a thunk for the original function that invokes (1) including
/// setting
///     its inline strategy as always inline.
void ExistentialTransform::createExistentialSpecializedFunction() {
  std::string Name = createExistentialSpecializedFunctionName();
  SILLinkage linkage = getSpecializedLinkage(F, F->getLinkage());

  /// Create devirtualized function type.
  auto NewFTy = createExistentialSpecializedFunctionType();

  auto NewFGenericSig = NewFTy->getGenericSignature();
  auto NewFGenericEnv = NewFGenericSig->createGenericEnvironment();

  /// Step 1: Create the new protocol constrained generic function.
  NewF = FunctionBuilder.createFunction(
      linkage, Name, NewFTy, NewFGenericEnv, F->getLocation(), F->isBare(),
      F->isTransparent(), F->isSerialized(), IsNotDynamic, F->getEntryCount(),
      F->isThunk(), F->getClassSubclassScope(), F->getInlineStrategy(),
      F->getEffectsKind(), nullptr, F->getDebugScope());
  /// Set the semantics attributes for the new function.
  for (auto &Attr : F->getSemanticsAttrs())
    NewF->addSemanticsAttr(Attr);

  /// Set Unqualified ownership, if any.
  if (!F->hasQualifiedOwnership()) {
    NewF->setUnqualifiedOwnership();
  }

  /// Step 1a: Populate the body of NewF.
  SubstitutionMap Subs = SubstitutionMap::get(
      NewFGenericSig,
      [&](SubstitutableType *type) -> Type {
        return NewFGenericEnv->mapTypeIntoContext(type);
      },
      LookUpConformanceInModule(F->getModule().getSwiftModule()));
  ExistentialSpecializerCloner cloner(F, NewF, Subs, ArgumentDescList,
                                      ArgToGenericTypeMap,
                                      ExistentialArgDescriptor);
  cloner.cloneAndPopulateFunction();

  /// Step 2: Create the thunk with always_inline and populate its body.
  populateThunkBody();

  assert(F->getDebugScope()->Parent != NewF->getDebugScope()->Parent);

  LLVM_DEBUG(llvm::dbgs() << "After ExistentialSpecializer Pass\n"; F->dump();
             NewF->dump(););
}
