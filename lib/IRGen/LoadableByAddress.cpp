//===--- LoadableByAddress.cpp - Lower SIL address-only types. ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
// This pass lowers loadable SILTypes. On completion, the SILType of every
// function argument is an address instead of the type itself.
// This reduces the code size.
// Consequently, this pass is required for IRGen.
// It is a mandatory IRGen preparation pass (not a diagnostic pass).
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "loadable-address"
#include "FixedTypeInfo.h"
#include "IRGenMangler.h"
#include "IRGenModule.h"
#include "NativeConventionSchema.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/IRGen/IRGenSILPasses.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"

using namespace swift;
using namespace swift::irgen;

/// Utility to derive SILLocation.
///
/// TODO: This should be a common utility.
static SILLocation getLocForValue(SILValue value) {
  if (auto *instr = dyn_cast<SILInstruction>(value)) {
    return instr->getLoc();
  }
  if (auto *arg = dyn_cast<SILArgument>(value)) {
    if (arg->getDecl())
      return RegularLocation(const_cast<ValueDecl *>(arg->getDecl()));
  }
  // TODO: bbargs should probably use one of their operand locations.
  return value->getFunction()->getLocation();
}

static GenericEnvironment *getGenericEnvironment(SILModule &Mod,
                                                 CanSILFunctionType loweredTy) {
  return loweredTy->getGenericSignature()->createGenericEnvironment(
                                                         *Mod.getSwiftModule());
}

/// Utility to determine if this is a large loadable type
static bool isLargeLoadableType(GenericEnvironment *GenericEnv, SILType t,
                                irgen::IRGenModule &Mod) {
  if (t.isAddress() || t.isClassOrClassMetatype()) {
    return false;
  }

  CanType canType = t.getSwiftRValueType();
  if (canType->hasTypeParameter()) {
    assert(GenericEnv && "Expected a GenericEnv");
    canType = GenericEnv->mapTypeIntoContext(canType)->getCanonicalType();
  }

  if (canType.getAnyGeneric()) {
    assert(t.isObject() && "Expected only two categories: address and object");
    assert(!canType->hasTypeParameter());
    const TypeInfo &TI = Mod.getTypeInfoForLowered(canType);
    auto &nativeSchemaOrigParam = TI.nativeParameterValueSchema(Mod);
    return nativeSchemaOrigParam.requiresIndirect();
  }
  return false;
}

static bool modifiableFunction(CanSILFunctionType funcType) {
  if (funcType->getLanguage() == SILFunctionLanguage::C) {
    // C functions should use the old ABI
    return false;
  }
  return true;
}

static bool modResultType(GenericEnvironment *genEnv,
                          CanSILFunctionType loweredTy,
                          irgen::IRGenModule &Mod);

static bool containsLargeLoadable(GenericEnvironment *GenericEnv,
                                  ArrayRef<SILParameterInfo> params,
                                  irgen::IRGenModule &Mod) {
  for (SILParameterInfo param : params) {
    SILType storageType = param.getSILStorageType();
    CanType currCanType = storageType.getSwiftRValueType();
    if (SILFunctionType *currSILFunctionType =
            dyn_cast<SILFunctionType>(currCanType.getPointer())) {
      if (containsLargeLoadable(GenericEnv,
                                currSILFunctionType->getParameters(), Mod) ||
          modResultType(GenericEnv, CanSILFunctionType(currSILFunctionType),
                        Mod)) {
        return true;
      }
    } else if (isa<SILBlockStorageType>(currCanType.getPointer())) {
      continue;
    } else {
      switch (param.getConvention()) {
      case ParameterConvention::Indirect_In_Guaranteed:
      case ParameterConvention::Indirect_Inout:
      case ParameterConvention::Indirect_InoutAliasable:
      case ParameterConvention::Indirect_In: {
        continue;
      }
      default:
        break;
      }
      if (isLargeLoadableType(GenericEnv, storageType, Mod)) {
        return true;
      }
    }
  }
  return false;
}

// Forward declarations - functions depend on each other
static SmallVector<SILParameterInfo, 4>
getNewArgTys(GenericEnvironment *GenericEnv,
             SILFunctionType *currSILFunctionType, irgen::IRGenModule &Mod);
static SILType getNewSILType(GenericEnvironment *GenericEnv,
                             SILType storageType, irgen::IRGenModule &Mod);

static bool newResultsDiffer(GenericEnvironment *GenericEnv,
                             ArrayRef<SILResultInfo> origResults,
                             irgen::IRGenModule &Mod) {
  SmallVector<SILResultInfo, 2> newResults;
  for (auto result : origResults) {
    SILType currResultTy = result.getSILStorageType();
    SILType newSILType = getNewSILType(GenericEnv, currResultTy, Mod);
    // We (currently) only care about function signatures
    if (!isLargeLoadableType(GenericEnv, currResultTy, Mod) &&
        (newSILType != currResultTy)) {
      return true;
    }
  }
  return false;
}

static SmallVector<SILResultInfo, 2>
getNewResults(GenericEnvironment *GenericEnv,
              SILFunctionType *currSILFunctionType, irgen::IRGenModule &Mod) {
  // Get new SIL Function results - same as old results UNLESS:
  // 1) Function type results might have a different signature
  // 2) Large loadables are replaced by @out version
  auto origResults = currSILFunctionType->getResults();
  SmallVector<SILResultInfo, 2> newResults;
  for (auto result : origResults) {
    SILType currResultTy = result.getSILStorageType();
    SILType newSILType = getNewSILType(GenericEnv, currResultTy, Mod);
    // We (currently) only care about function signatures
    if (!isLargeLoadableType(GenericEnv, currResultTy, Mod) &&
        (newSILType != currResultTy)) {
      // Case (1) Above
      SILResultInfo newResult(newSILType.getSwiftRValueType(),
                              result.getConvention());
      newResults.push_back(newResult);
    } else if ((newSILType != currResultTy) &&
               modResultType(GenericEnv,
                             CanSILFunctionType(currSILFunctionType), Mod)) {
      // Case (2) Above
      SILResultInfo newSILResultInfo(newSILType.getSwiftRValueType(),
                                     ResultConvention::Indirect);
      newResults.push_back(newSILResultInfo);
    } else {
      newResults.push_back(result);
    }
  }
  return newResults;
}

static SILFunctionType *
getNewSILFunctionTypePtr(GenericEnvironment *GenericEnv,
                         SILFunctionType *currSILFunctionType,
                         irgen::IRGenModule &Mod) {
  if (!modifiableFunction(CanSILFunctionType(currSILFunctionType))) {
    return currSILFunctionType;
  }
  SmallVector<SILParameterInfo, 4> newArgTys =
      getNewArgTys(GenericEnv, currSILFunctionType, Mod);
  SILFunctionType *newSILFunctionType = SILFunctionType::get(
      currSILFunctionType->getGenericSignature(),
      currSILFunctionType->getExtInfo(),
      currSILFunctionType->getCalleeConvention(), newArgTys,
      getNewResults(GenericEnv, currSILFunctionType, Mod),
      currSILFunctionType->getOptionalErrorResult(),
      currSILFunctionType->getASTContext());
  return newSILFunctionType;
}

static SILType getNewSILFunctionType(GenericEnvironment *GenericEnv,
                                     SILFunctionType *currSILFunctionType,
                                     irgen::IRGenModule &Mod) {
  if (!modifiableFunction(CanSILFunctionType(currSILFunctionType))) {
    SILType newSILType = SILType::getPrimitiveObjectType(
        currSILFunctionType->getCanonicalType());
    return newSILType;
  }
  SILFunctionType *newSILFunctionType =
      getNewSILFunctionTypePtr(GenericEnv, currSILFunctionType, Mod);
  SILType newSILType =
      SILType::getPrimitiveObjectType(newSILFunctionType->getCanonicalType());
  return newSILType;
}

// Get the function type or the optional function type
static SILFunctionType *getInnerFunctionType(SILType storageType) {
  CanType currCanType = storageType.getSwiftRValueType();
  if (SILFunctionType *currSILFunctionType =
          dyn_cast<SILFunctionType>(currCanType.getPointer())) {
    return currSILFunctionType;
  }
  OptionalTypeKind optKind;
  if (auto optionalType = currCanType.getAnyOptionalObjectType(optKind)) {
    assert(optKind != OptionalTypeKind::OTK_None &&
           "Expected Real Optional Type");
    if (auto *currSILFunctionType =
            dyn_cast<SILFunctionType>(optionalType.getPointer())) {
      return currSILFunctionType;
    }
  }
  return nullptr;
}

static SILType getNewOptionalFunctionType(GenericEnvironment *GenericEnv,
                                          SILType storageType,
                                          irgen::IRGenModule &Mod) {
  SILType newSILType = storageType;
  CanType currCanType = storageType.getSwiftRValueType();
  OptionalTypeKind optKind;
  if (auto optionalType = currCanType.getAnyOptionalObjectType(optKind)) {
    assert(optKind != OptionalTypeKind::OTK_None &&
           "Expected Real Optional Type");
    if (auto *currSILFunctionType =
            dyn_cast<SILFunctionType>(optionalType.getPointer())) {
      if (containsLargeLoadable(GenericEnv,
                                currSILFunctionType->getParameters(), Mod) ||
          modResultType(GenericEnv, CanSILFunctionType(currSILFunctionType),
                        Mod)) {
        newSILType =
            getNewSILFunctionType(GenericEnv, currSILFunctionType, Mod);
        currCanType = newSILType.getSwiftRValueType();
        auto newType = OptionalType::get(optKind, currCanType);
        CanType newCanType = newType->getCanonicalType();
        newSILType = SILType::getPrimitiveObjectType(newCanType);
        if (storageType.isAddress()) {
          newSILType = newSILType.getAddressType();
        }
      }
    }
  }
  return newSILType;
}

static bool modResultType(GenericEnvironment *genEnv,
                          CanSILFunctionType loweredTy,
                          irgen::IRGenModule &Mod) {
  if (!modifiableFunction(loweredTy)) {
    return false;
  }
  if (loweredTy->getNumResults() != 1) {
    return false;
  }
  auto singleResult = loweredTy->getSingleResult();
  auto resultStorageType = singleResult.getSILStorageType();
  if (isLargeLoadableType(genEnv, resultStorageType, Mod)) {
    return true;
  }
  return false;
}

static bool modResultType(SILFunction *F, irgen::IRGenModule &Mod) {
  GenericEnvironment *genEnv = F->getGenericEnvironment();
  auto loweredTy = F->getLoweredFunctionType();
  if (!genEnv && loweredTy->isPolymorphic()) {
    genEnv = getGenericEnvironment(F->getModule(), loweredTy);
  }

  return modResultType(genEnv, loweredTy, Mod);
}

static SmallVector<SILParameterInfo, 4>
getNewArgTys(GenericEnvironment *GenericEnv,
             SILFunctionType *currSILFunctionType, irgen::IRGenModule &Mod) {
  ArrayRef<SILParameterInfo> params = currSILFunctionType->getParameters();
  SmallVector<SILParameterInfo, 4> newArgTys;
  for (SILParameterInfo param : params) {
    SILType storageType = param.getSILStorageType();
    SILType newOptFuncType =
        getNewOptionalFunctionType(GenericEnv, storageType, Mod);
    if (newOptFuncType != storageType) {
      auto newParam = SILParameterInfo(newOptFuncType.getSwiftRValueType(),
                                       param.getConvention());
      newArgTys.push_back(newParam);
      continue;
    }
    CanType currCanType = storageType.getSwiftRValueType();
    if (SILFunctionType *currSILFunctionType =
            dyn_cast<SILFunctionType>(currCanType.getPointer())) {
      if (containsLargeLoadable(GenericEnv,
                                currSILFunctionType->getParameters(), Mod) ||
          modResultType(GenericEnv, CanSILFunctionType(currSILFunctionType),
                        Mod)) {
        SILType newSILType =
            getNewSILFunctionType(GenericEnv, currSILFunctionType, Mod);
        if (storageType.isAddress()) {
          newSILType = newSILType.getAddressType();
        }
        auto newParam = SILParameterInfo(newSILType.getSwiftRValueType(),
                                         param.getConvention());
        newArgTys.push_back(newParam);
      } else {
        newArgTys.push_back(param);
      }
    } else if (isLargeLoadableType(GenericEnv, storageType, Mod)) {
      SILType addrType = storageType.getAddressType();
      auto newParam =
          SILParameterInfo(addrType.getSwiftRValueType(),
                           ParameterConvention::Indirect_In_Constant);
      newArgTys.push_back(newParam);
    } else {
      newArgTys.push_back(param);
    }
  }
  return newArgTys;
}

static SILType getNewSILType(GenericEnvironment *GenericEnv,
                             SILType storageType, irgen::IRGenModule &Mod) {
  SILType newSILType = getNewOptionalFunctionType(GenericEnv, storageType, Mod);
  if (newSILType != storageType) {
    return newSILType;
  }
  CanType currCanType = storageType.getSwiftRValueType();
  if (isa<SILBlockStorageType>(currCanType.getPointer())) {
    return storageType;
  }
  if (SILFunctionType *currSILFunctionType =
          dyn_cast<SILFunctionType>(currCanType.getPointer())) {
    if (containsLargeLoadable(GenericEnv, currSILFunctionType->getParameters(),
                              Mod) ||
        modResultType(GenericEnv, CanSILFunctionType(currSILFunctionType),
                      Mod)) {
      newSILType = getNewSILFunctionType(GenericEnv, currSILFunctionType, Mod);
      if (storageType.isAddress()) {
        newSILType = newSILType.getAddressType();
      }
    }
  } else if (isLargeLoadableType(GenericEnv, storageType, Mod)) {
    newSILType = storageType.getAddressType();
  }
  return newSILType;
}

//===----------------------------------------------------------------------===//
// StructLoweringState: shared state for the pass's analysis and transforms.
//===----------------------------------------------------------------------===//

namespace {
struct StructLoweringState {
  SILFunction *F;
  irgen::IRGenModule &Mod;

  // All large loadable function arguments that we modified
  SmallVector<SILValue, 16> largeLoadableArgs;
  // All modified function signature function arguments
  SmallVector<SILValue, 16> funcSigArgs;
  // All args for which we did a load
  llvm::DenseMap<SILValue, SILValue> argsToLoadedValueMap;
  // All applies for which we did an alloc
  llvm::DenseMap<SILValue, SILValue> applyRetToAllocMap;
  // recerse map of the one above
  llvm::DenseMap<SILInstruction *, SILValue> allocToApplyRetMap;
  // All call sites with SILArgument that needs to be re-written
  // Calls are removed from the set when rewritten.
  SmallVector<SILInstruction *, 16> applies;
  // All MethodInst that use the large struct
  SmallVector<MethodInst *, 16> methodInstsToMod;
  // Large loadable store instrs should call the outlined copy
  SmallVector<StoreInst *, 16> storeInstsToMod;
  // All switch_enum instrs that should be converted to switch_enum_addr
  SmallVector<SwitchEnumInst *, 16> switchEnumInstsToMod;
  // All struct_extract instrs that should be converted to struct_element_addr
  SmallVector<StructExtractInst *, 16> structExtractInstsToMod;
  // All tuple instructions for which the return type is a function type
  SmallVector<SILInstruction *, 8> tupleInstsToMod;
  // All allock stack instructions to modify
  SmallVector<AllocStackInst *, 8> allocStackInstsToMod;
  // All pointer to address instructions to modify
  SmallVector<PointerToAddressInst *, 8> pointerToAddrkInstsToMod;
  // All Retain and release instrs should be replaced with _addr version
  SmallVector<RetainValueInst *, 16> retainInstsToMod;
  SmallVector<ReleaseValueInst *, 16> releaseInstsToMod;
  // All result types instrs for which we need to convert the ResultTy
  llvm::SetVector<SILInstruction *> resultTyInstsToMod;
  // All instructions that use the large struct that are not covered above
  SmallVector<SILInstruction *, 16> instsToMod;
  // All function-exiting terminators (return or throw instructions).
  SmallVector<TermInst *, 8> returnInsts;
  // All return instructions that are modified
  SmallVector<ReturnInst *, 8> modReturnInsts;
  // All debug instructions.
  // to be modified *only if* the operands are used in "real" instructions
  SmallVector<SILInstruction *, 16> debugInstsToMod;

  StructLoweringState(SILFunction *F, irgen::IRGenModule &Mod)
      : F(F), Mod(Mod) {}
};
} // end anonymous namespace

//===----------------------------------------------------------------------===//
// LargeValueVisitor: Map large loadable values to ValueStorage.
//===----------------------------------------------------------------------===//

namespace {
class LargeValueVisitor {
  StructLoweringState &pass;
  PostOrderFunctionInfo postorderInfo;

public:
  explicit LargeValueVisitor(StructLoweringState &pass)
      : pass(pass), postorderInfo(pass.F) {}

  void mapReturnInstrs();
  void mapValueStorage();

protected:
  void visitApply(ApplySite applySite);
  void visitMethodInst(MethodInst *instr);
  void visitStoreInst(StoreInst *instr);
  void visitSwitchEnumInst(SwitchEnumInst *instr);
  void visitStructExtractInst(StructExtractInst *instr);
  void visitRetainInst(RetainValueInst *instr);
  void visitReleaseInst(ReleaseValueInst *instr);
  void visitResultTyInst(SILInstruction *instr);
  void visitDebugValueInst(DebugValueInst *instr);
  void visitTupleInst(SILInstruction *instr);
  void visitAllocStackInst(AllocStackInst *instr);
  void visitPointerToAddressInst(PointerToAddressInst *instr);
  void visitReturnInst(ReturnInst *instr);
  void visitDeallocInst(DeallocStackInst *instr);
  void visitInstr(SILInstruction *instr);
};
} // end anonymous namespace

void LargeValueVisitor::mapReturnInstrs() {
  for (auto *BB : postorderInfo.getReversePostOrder()) {
    if (BB->getTerminator()->isFunctionExiting())
      pass.returnInsts.push_back(BB->getTerminator());
  }
}

void LargeValueVisitor::mapValueStorage() {
  for (auto *BB : postorderInfo.getReversePostOrder()) {
    for (auto &II : *BB) {
      SILInstruction *currIns = &II;
      switch (currIns->getKind()) {
      case ValueKind::ApplyInst:
      case ValueKind::TryApplyInst:
      case ValueKind::PartialApplyInst: {
        visitApply(ApplySite(currIns));
        break;
      }
      case ValueKind::ClassMethodInst:
      case ValueKind::SuperMethodInst:
      case ValueKind::DynamicMethodInst:
      case ValueKind::WitnessMethodInst: {
        // TODO Any more instructions to add here?
        auto *MI = dyn_cast<MethodInst>(currIns);
        visitMethodInst(MI);
        break;
      }
      case ValueKind::StructExtractInst:
      case ValueKind::StructElementAddrInst:
      case ValueKind::RefTailAddrInst:
      case ValueKind::RefElementAddrInst:
      case ValueKind::BeginAccessInst:
      case ValueKind::EnumInst: {
        // TODO Any more instructions to add here?
        visitResultTyInst(currIns);
        break;
      }
      case ValueKind::StoreInst: {
        auto *SI = dyn_cast<StoreInst>(currIns);
        visitStoreInst(SI);
        break;
      }
      case ValueKind::RetainValueInst: {
        auto *RETI = dyn_cast<RetainValueInst>(currIns);
        visitRetainInst(RETI);
        break;
      }
      case ValueKind::ReleaseValueInst: {
        auto *RELI = dyn_cast<ReleaseValueInst>(currIns);
        visitReleaseInst(RELI);
        break;
      }
      case ValueKind::DebugValueInst: {
        auto *DI = dyn_cast<DebugValueInst>(currIns);
        visitDebugValueInst(DI);
        break;
      }
      case ValueKind::SwitchEnumInst: {
        auto *SEI = dyn_cast<SwitchEnumInst>(currIns);
        visitSwitchEnumInst(SEI);
        break;
      }
      case ValueKind::TupleElementAddrInst:
      case ValueKind::TupleExtractInst: {
        visitTupleInst(currIns);
        break;
      }
      case ValueKind::AllocStackInst: {
        auto *ASI = dyn_cast<AllocStackInst>(currIns);
        visitAllocStackInst(ASI);
        break;
      }
      case ValueKind::PointerToAddressInst: {
        auto *PTA = dyn_cast<PointerToAddressInst>(currIns);
        visitPointerToAddressInst(PTA);
        break;
      }
      case ValueKind::ReturnInst: {
        auto *RI = dyn_cast<ReturnInst>(currIns);
        visitReturnInst(RI);
        break;
      }
      case ValueKind::DeallocStackInst: {
        auto *DI = dyn_cast<DeallocStackInst>(currIns);
        visitDeallocInst(DI);
        break;
      }
      default: {
        assert(!ApplySite::isa(currIns) && "Did not expect an ApplySite");
        assert(!dyn_cast<MethodInst>(currIns) && "Unhandled Method Inst");
        visitInstr(currIns);
        break;
      }
      }
    }
  }
}

static bool modifiableApply(ApplySite applySite, irgen::IRGenModule &Mod) {
  // If the callee is a method then use the old ABI
  if (applySite.getSubstCalleeType()->getLanguage() == SILFunctionLanguage::C) {
    return false;
  }
  auto callee = applySite.getCallee();
  if (isa<ProjectBlockStorageInst>(callee)) {
    return false;
  } else if (auto *instr = dyn_cast<LoadInst>(callee)) {
    auto loadedSrcValue = instr->getOperand();
    if (isa<ProjectBlockStorageInst>(loadedSrcValue)) {
      return false;
    }
  }
  return true;
}

void LargeValueVisitor::visitApply(ApplySite applySite) {
  if (!modifiableApply(applySite, pass.Mod)) {
    return visitInstr(applySite.getInstruction());
  }
  GenericEnvironment *genEnv = pass.F->getGenericEnvironment();
  auto loweredTy = pass.F->getLoweredFunctionType();
  if (!genEnv && loweredTy->isPolymorphic()) {
    genEnv = getGenericEnvironment(pass.F->getModule(), loweredTy);
  }
  for (Operand &operand : applySite.getArgumentOperands()) {
    SILValue currOperand = operand.get();
    SILType silType = currOperand->getType();
    SILType newSilType = getNewSILType(genEnv, silType, pass.Mod);
    if (silType != newSilType ||
        std::find(pass.largeLoadableArgs.begin(), pass.largeLoadableArgs.end(),
                  currOperand) != pass.largeLoadableArgs.end() ||
        std::find(pass.funcSigArgs.begin(), pass.funcSigArgs.end(),
                  currOperand) != pass.funcSigArgs.end()) {
      pass.applies.push_back(applySite.getInstruction());
      return;
    }
  }
  SILType currType = applySite.getType();
  SILType newType = getNewSILType(genEnv, currType, pass.Mod);
  // We only care about function type results
  if (!isLargeLoadableType(genEnv, currType, pass.Mod) &&
      (currType != newType)) {
    pass.applies.push_back(applySite.getInstruction());
    return;
  }
  // Check callee - need new generic env:
  SILFunctionType *origSILFunctionType = applySite.getSubstCalleeType();
  GenericEnvironment *genEnvCallee = nullptr;
  if (origSILFunctionType->isPolymorphic()) {
    genEnvCallee = getGenericEnvironment(
        applySite.getModule(), CanSILFunctionType(origSILFunctionType));
  }
  SILFunctionType *newSILFunctionType =
      getNewSILFunctionTypePtr(genEnvCallee, origSILFunctionType, pass.Mod);
  if (origSILFunctionType != newSILFunctionType) {
    pass.applies.push_back(applySite.getInstruction());
  }
}

static bool isMethodInstUnmodifiable(MethodInst *instr) {
  for (auto *user : instr->getUses()) {
    if (ApplySite::isa(user->getUser())) {
      ApplySite applySite = ApplySite(user->getUser());
      if (applySite.getSubstCalleeType()->getLanguage() ==
          SILFunctionLanguage::C) {
        return true;
      }
    }
  }
  return false;
}

void LargeValueVisitor::visitMethodInst(MethodInst *instr) {
  if (isMethodInstUnmodifiable(instr)) {
    // Do not change the method!
    visitInstr(instr);
    return;
  }
  SILType currSILType = instr->getType();
  CanType currCanType = currSILType.getSwiftRValueType();
  SILFunctionType *currSILFunctionType =
      dyn_cast<SILFunctionType>(currCanType.getPointer());
  if (!currSILFunctionType) {
    llvm_unreachable("unsupported type");
  }
  GenericEnvironment *genEnv = nullptr;
  if (currSILFunctionType->isPolymorphic()) {
    genEnv = getGenericEnvironment(instr->getModule(),
                                   CanSILFunctionType(currSILFunctionType));
  }
  Lowering::GenericContextScope GenericScope(
      instr->getModule().Types, currSILFunctionType->getGenericSignature());
  if (containsLargeLoadable(genEnv, currSILFunctionType->getParameters(),
                            pass.Mod) ||
      modResultType(genEnv, CanSILFunctionType(currSILFunctionType),
                    pass.Mod)) {
    pass.methodInstsToMod.push_back(instr);
    return;
  }
  if (newResultsDiffer(genEnv, currSILFunctionType->getResults(), pass.Mod)) {
    pass.methodInstsToMod.push_back(instr);
  }
}

void LargeValueVisitor::visitStoreInst(StoreInst *instr) {
  SILValue src = instr->getSrc();
  if (std::find(pass.largeLoadableArgs.begin(), pass.largeLoadableArgs.end(),
                src) != pass.largeLoadableArgs.end()) {
    pass.storeInstsToMod.push_back(instr);
  }
}

static bool shouldConvertBBArg(SILArgument *arg, irgen::IRGenModule &Mod) {
  auto *F = arg->getFunction();
  SILType storageType = arg->getType();
  GenericEnvironment *genEnv = F->getGenericEnvironment();
  auto loweredTy = F->getLoweredFunctionType();
  if (!genEnv && loweredTy->isPolymorphic()) {
    genEnv = getGenericEnvironment(F->getModule(), loweredTy);
  }
  SILType newSILType = getNewSILType(genEnv, storageType, Mod);
  // We (currently) only care about function signatures
  if (!isLargeLoadableType(genEnv, storageType, Mod) &&
      (newSILType != storageType)) {
    return true;
  }
  return false;
}

void LargeValueVisitor::visitSwitchEnumInst(SwitchEnumInst *instr) {
  SILValue operand = instr->getOperand();
  if (std::find(pass.largeLoadableArgs.begin(), pass.largeLoadableArgs.end(),
                operand) != pass.largeLoadableArgs.end()) {
    pass.switchEnumInstsToMod.push_back(instr);
    return;
  }
  // In case we converted the target BB type of this enum - need to modify!
  unsigned numOfCases = instr->getNumCases();
  SmallVector<std::pair<EnumElementDecl *, SILBasicBlock *>, 16> caseBBs;
  for (unsigned i = 0; i < numOfCases; ++i) {
    auto currCase = instr->getCase(i);
    auto *currBB = currCase.second;
    for (SILArgument *arg : currBB->getArguments()) {
      if (shouldConvertBBArg(arg, pass.Mod)) {
        pass.switchEnumInstsToMod.push_back(instr);
        return;
      }
    }
  }
}

void LargeValueVisitor::visitStructExtractInst(StructExtractInst *instr) {
  SILValue operand = instr->getOperand();
  if (std::find(pass.largeLoadableArgs.begin(), pass.largeLoadableArgs.end(),
                operand) != pass.largeLoadableArgs.end()) {
    pass.structExtractInstsToMod.push_back(instr);
  }
}

void LargeValueVisitor::visitRetainInst(RetainValueInst *instr) {
  for (Operand &operand : instr->getAllOperands()) {
    if (std::find(pass.largeLoadableArgs.begin(), pass.largeLoadableArgs.end(),
                  operand.get()) != pass.largeLoadableArgs.end()) {
      pass.retainInstsToMod.push_back(instr);
      return;
    }
  }
}

void LargeValueVisitor::visitReleaseInst(ReleaseValueInst *instr) {
  for (Operand &operand : instr->getAllOperands()) {
    if (std::find(pass.largeLoadableArgs.begin(), pass.largeLoadableArgs.end(),
                  operand.get()) != pass.largeLoadableArgs.end()) {
      pass.releaseInstsToMod.push_back(instr);
      return;
    }
  }
}

void LargeValueVisitor::visitDebugValueInst(DebugValueInst *instr) {
  for (Operand &operand : instr->getAllOperands()) {
    if (std::find(pass.largeLoadableArgs.begin(), pass.largeLoadableArgs.end(),
                  operand.get()) != pass.largeLoadableArgs.end()) {
      pass.debugInstsToMod.push_back(instr);
    }
  }
}

void LargeValueVisitor::visitResultTyInst(SILInstruction *instr) {
  GenericEnvironment *genEnv = instr->getFunction()->getGenericEnvironment();
  auto loweredTy = instr->getFunction()->getLoweredFunctionType();
  if (!genEnv && loweredTy->isPolymorphic()) {
    genEnv = getGenericEnvironment(instr->getModule(), loweredTy);
  }
  SILType currSILType = instr->getType().getObjectType();
  SILType newSILType = getNewSILType(genEnv, currSILType, pass.Mod);
  if (currSILType != newSILType) {
    pass.resultTyInstsToMod.insert(instr);
  }
  auto *SEI = dyn_cast<StructExtractInst>(instr);
  if (SEI) {
    visitStructExtractInst(SEI);
  } else {
    visitInstr(instr);
  }
}

void LargeValueVisitor::visitTupleInst(SILInstruction *instr) {
  SILType currSILType = instr->getType().getObjectType();
  CanType currCanType = currSILType.getSwiftRValueType();
  if (auto funcType = dyn_cast<SILFunctionType>(currCanType)) {
    CanSILFunctionType canFuncType = CanSILFunctionType(funcType);
    GenericEnvironment *genEnv = instr->getFunction()->getGenericEnvironment();
    if (!genEnv && canFuncType->isPolymorphic()) {
      genEnv = getGenericEnvironment(instr->getModule(), canFuncType);
    }
    SILFunctionType *newSILFunctionType =
        getNewSILFunctionTypePtr(genEnv, funcType, pass.Mod);
    if (funcType != newSILFunctionType) {
      pass.tupleInstsToMod.push_back(instr);
    }
  }
  visitInstr(instr);
}

void LargeValueVisitor::visitAllocStackInst(AllocStackInst *instr) {
  SILType currSILType = instr->getType().getObjectType();
  if (getInnerFunctionType(currSILType)) {
    pass.allocStackInstsToMod.push_back(instr);
  }
}

void LargeValueVisitor::visitPointerToAddressInst(PointerToAddressInst *instr) {
  SILType currSILType = instr->getType().getObjectType();
  if (getInnerFunctionType(currSILType)) {
    pass.pointerToAddrkInstsToMod.push_back(instr);
  }
}

void LargeValueVisitor::visitReturnInst(ReturnInst *instr) {
  if (!modResultType(pass.F, pass.Mod)) {
    visitInstr(instr);
  } else {
    pass.modReturnInsts.push_back(instr);
  }
}

void LargeValueVisitor::visitDeallocInst(DeallocStackInst *instr) {
  auto opInstr = instr->getOperand();
  if (std::find(pass.largeLoadableArgs.begin(), pass.largeLoadableArgs.end(),
                opInstr) != pass.largeLoadableArgs.end()) {
    auto *opAsInstr = dyn_cast<AllocStackInst>(opInstr);
    assert(opAsInstr && "Expected an alloc stack instruction");
    assert(pass.allocToApplyRetMap.find(opAsInstr) !=
               pass.allocToApplyRetMap.end() &&
           "Unexpected dealloc instr!");
  }
}

void LargeValueVisitor::visitInstr(SILInstruction *instr) {
  for (Operand &operand : instr->getAllOperands()) {
    if (std::find(pass.largeLoadableArgs.begin(), pass.largeLoadableArgs.end(),
                  operand.get()) != pass.largeLoadableArgs.end()) {
      pass.instsToMod.push_back(instr);
      // will be replaced later by the load / alloc_stack:
      pass.argsToLoadedValueMap[operand.get()] = operand.get();
    }
  }
}

//===----------------------------------------------------------------------===//
// LoadableStorageAllocation: Generate alloc_stack and address projections
// for all loadable types we pass around.
//===----------------------------------------------------------------------===//

namespace {
class LoadableStorageAllocation {
  StructLoweringState &pass;

public:
  explicit LoadableStorageAllocation(StructLoweringState &pass) : pass(pass) {}

  void allocateLoadableStorage();
  void replaceLoadWithCopyAddr(LoadInst *optimizableLoad);
  void replaceLoadWithCopyAddrForModifiable(LoadInst *unoptimizableLoad);

protected:
  void convertIndirectFunctionArgs();
  void insertIndirectReturnArgs();
  void convertIndirectFunctionPointerArgsForUnmodifiable();
  void convertIndirectBasicBlockArgs();
  void convertApplyResults();
  void allocateForArg(SILValue value);
  AllocStackInst *allocateForApply(SILValue value, SILType type);
  SILArgument *replaceArgType(SILBuilder &argBuilder, SILArgument *arg,
                              SILType newSILType);
};
} // end anonymous namespace

static SILInstruction *createOutlinedCopyCall(SILBuilder &copyBuilder,
                                              SILValue src, SILValue tgt,
                                              StructLoweringState &pass,
                                              SILLocation *loc = nullptr) {
  SILLocation locToUse = loc ? *loc : copyBuilder.getInsertionPoint()->getLoc();
  auto *copy =
      copyBuilder.createCopyAddr(locToUse, src, tgt, IsTake, IsInitialization);
  return copy;
}

void LoadableStorageAllocation::replaceLoadWithCopyAddr(
    LoadInst *optimizableLoad) {
  SILValue value = optimizableLoad->getOperand();

  SILBuilder allocBuilder(pass.F->begin()->begin());
  AllocStackInst *allocInstr =
      allocBuilder.createAllocStack(getLocForValue(value), value->getType());

  SILBuilder outlinedBuilder(optimizableLoad);
  createOutlinedCopyCall(outlinedBuilder, value, allocInstr, pass);

  // Insert stack deallocations.
  for (TermInst *termInst : pass.returnInsts) {
    SILBuilder deallocBuilder(termInst);
    deallocBuilder.createDeallocStack(allocInstr->getLoc(), allocInstr);
  }

  for (auto *user : optimizableLoad->getUses()) {
    SILInstruction *userIns = user->getUser();
    switch (userIns->getKind()) {
    case ValueKind::CopyAddrInst:
    case ValueKind::DeallocStackInst:
      break;
    case ValueKind::ApplyInst:
    case ValueKind::TryApplyInst:
    case ValueKind::PartialApplyInst: {
      if (std::find(pass.applies.begin(), pass.applies.end(), userIns) ==
          pass.applies.end()) {
        pass.applies.push_back(userIns);
      }
      break;
    }
    case ValueKind::RetainValueInst: {
      auto *insToInsert = dyn_cast<RetainValueInst>(userIns);
      assert(insToInsert && "Unexpected cast failure");
      pass.retainInstsToMod.push_back(insToInsert);
      break;
    }
    case ValueKind::ReleaseValueInst: {
      auto *insToInsert = dyn_cast<ReleaseValueInst>(userIns);
      assert(insToInsert && "Unexpected cast failure");
      pass.releaseInstsToMod.push_back(insToInsert);
      break;
    }
    case ValueKind::StoreInst: {
      auto *insToInsert = dyn_cast<StoreInst>(userIns);
      assert(insToInsert && "Unexpected cast failure");
      pass.storeInstsToMod.push_back(insToInsert);
      break;
    }
    case ValueKind::DebugValueInst: {
      auto *insToInsert = dyn_cast<DebugValueInst>(userIns);
      assert(insToInsert && "Unexpected cast failure");
      pass.debugInstsToMod.push_back(insToInsert);
      break;
    }
    case ValueKind::StructExtractInst: {
      auto *instToInsert = dyn_cast<StructExtractInst>(userIns);
      if (std::find(pass.structExtractInstsToMod.begin(),
                    pass.structExtractInstsToMod.end(),
                    instToInsert) == pass.structExtractInstsToMod.end()) {
        pass.structExtractInstsToMod.push_back(instToInsert);
      }
      break;
    }
    case ValueKind::SwitchEnumInst: {
      auto *instToInsert = dyn_cast<SwitchEnumInst>(userIns);
      if (std::find(pass.switchEnumInstsToMod.begin(),
                    pass.switchEnumInstsToMod.end(),
                    instToInsert) == pass.switchEnumInstsToMod.end()) {
        pass.switchEnumInstsToMod.push_back(instToInsert);
      }
      break;
    }
    default:
      llvm_unreachable("Unexpected instruction");
    }
  }

  optimizableLoad->replaceAllUsesWith(allocInstr);
  optimizableLoad->getParent()->erase(optimizableLoad);
}

static bool usesContainApplies(LoadInst *unoptimizableLoad,
                               irgen::IRGenModule &Mod) {
  for (auto *user : unoptimizableLoad->getUses()) {
    SILInstruction *userIns = user->getUser();
    switch (userIns->getKind()) {
    case ValueKind::ApplyInst:
    case ValueKind::TryApplyInst:
    case ValueKind::PartialApplyInst: {
      ApplySite site(userIns);
      SILValue callee = site.getCallee();
      if (callee == unoptimizableLoad) {
        break;
      }
      SILType currType = unoptimizableLoad->getType().getObjectType();
      GenericEnvironment *genEnv =
          unoptimizableLoad->getFunction()->getGenericEnvironment();
      auto loweredTy =
          unoptimizableLoad->getFunction()->getLoweredFunctionType();
      if (!genEnv && loweredTy->isPolymorphic()) {
        genEnv =
            getGenericEnvironment(unoptimizableLoad->getModule(), loweredTy);
      }
      SILType newSILType = getNewSILType(genEnv, currType, Mod);
      if (currType == newSILType) {
        break;
      }
      return true;
    }
    default:
      break;
    }
  }
  return false;
}

void LoadableStorageAllocation::replaceLoadWithCopyAddrForModifiable(
    LoadInst *unoptimizableLoad) {
  if (!usesContainApplies(unoptimizableLoad, pass.Mod)) {
    return;
  }
  SILValue value = unoptimizableLoad->getOperand();

  SILBuilder allocBuilder(pass.F->begin()->begin());
  AllocStackInst *allocInstr =
      allocBuilder.createAllocStack(getLocForValue(value), value->getType());

  SILBuilder outlinedBuilder(unoptimizableLoad);
  createOutlinedCopyCall(outlinedBuilder, value, allocInstr, pass);

  // Insert stack deallocations.
  for (TermInst *termInst : pass.returnInsts) {
    SILBuilder deallocBuilder(termInst);
    deallocBuilder.createDeallocStack(allocInstr->getLoc(), allocInstr);
  }

  SmallVector<Operand *, 8> usersToMod;
  for (auto *user : unoptimizableLoad->getUses()) {
    SILInstruction *userIns = user->getUser();
    switch (userIns->getKind()) {
    case ValueKind::CopyAddrInst:
    case ValueKind::DeallocStackInst:
      break;
    case ValueKind::ApplyInst:
    case ValueKind::TryApplyInst:
    case ValueKind::PartialApplyInst: {
      ApplySite site(userIns);
      if (!modifiableApply(site, pass.Mod)) {
        break;
      }
      SILValue callee = site.getCallee();
      if (callee == unoptimizableLoad) {
        break;
      }
      SILType currType = unoptimizableLoad->getType().getObjectType();
      GenericEnvironment *genEnv =
          userIns->getFunction()->getGenericEnvironment();
      auto loweredTy = userIns->getFunction()->getLoweredFunctionType();
      if (!genEnv && loweredTy->isPolymorphic()) {
        genEnv = getGenericEnvironment(userIns->getModule(), loweredTy);
      }
      SILType newSILType = getNewSILType(genEnv, currType, pass.Mod);
      if (currType == newSILType) {
        break;
      }
      if (std::find(pass.applies.begin(), pass.applies.end(), userIns) ==
          pass.applies.end()) {
        pass.applies.push_back(userIns);
      }
      usersToMod.push_back(user);
      break;
    }
    case ValueKind::RetainValueInst: {
      auto *insToInsert = dyn_cast<RetainValueInst>(userIns);
      assert(insToInsert && "Unexpected cast failure");
      pass.retainInstsToMod.push_back(insToInsert);
      usersToMod.push_back(user);
      break;
    }
    case ValueKind::ReleaseValueInst: {
      auto *insToInsert = dyn_cast<ReleaseValueInst>(userIns);
      assert(insToInsert && "Unexpected cast failure");
      pass.releaseInstsToMod.push_back(insToInsert);
      usersToMod.push_back(user);
      break;
    }
    case ValueKind::StoreInst: {
      auto *insToInsert = dyn_cast<StoreInst>(userIns);
      assert(insToInsert && "Unexpected cast failure");
      pass.storeInstsToMod.push_back(insToInsert);
      usersToMod.push_back(user);
      break;
    }
    case ValueKind::DebugValueInst: {
      auto *insToInsert = dyn_cast<DebugValueInst>(userIns);
      assert(insToInsert && "Unexpected cast failure");
      pass.debugInstsToMod.push_back(insToInsert);
      usersToMod.push_back(user);
      break;
    }
    case ValueKind::StructExtractInst: {
      auto *instToInsert = dyn_cast<StructExtractInst>(userIns);
      pass.structExtractInstsToMod.push_back(instToInsert);
      usersToMod.push_back(user);
      break;
    }
    case ValueKind::SwitchEnumInst: {
      auto *instToInsert = dyn_cast<SwitchEnumInst>(userIns);
      pass.switchEnumInstsToMod.push_back(instToInsert);
      usersToMod.push_back(user);
      break;
    }
    default:
      break;
    }
  }
  while (!usersToMod.empty()) {
    auto *currUser = usersToMod.pop_back_val();
    currUser->set(allocInstr);
  }
}

void LoadableStorageAllocation::allocateLoadableStorage() {
  // We need to map all functions exists
  // required for Apply result's allocations
  // Else we might get the following error:
  // "stack dealloc does not match most recent stack alloc"
  // When we dealloc later
  LargeValueVisitor(pass).mapReturnInstrs();
  if (modifiableFunction(pass.F->getLoweredFunctionType())) {
    // Turn by-value function args to by-address ones
    convertIndirectFunctionArgs();
  } else {
    convertIndirectFunctionPointerArgsForUnmodifiable();
  }
  convertApplyResults();

  // Populate the pass' data structs
  LargeValueVisitor(pass).mapValueStorage();

  // Turn by-value BB args to by-address ones
  convertIndirectBasicBlockArgs();

  // Create an AllocStack for every used large loadable type in the function.
  for (auto &argToAlloc : pass.argsToLoadedValueMap) {
    assert(argToAlloc.first == argToAlloc.second);
    allocateForArg(argToAlloc.first);
  }
}

SILArgument *LoadableStorageAllocation::replaceArgType(SILBuilder &argBuilder,
                                                       SILArgument *arg,
                                                       SILType newSILType) {
  CopyValueInst *copyArg = argBuilder.createCopyValue(
      RegularLocation(const_cast<ValueDecl *>(arg->getDecl())),
      SILUndef::get(newSILType, pass.F->getModule()));

  arg->replaceAllUsesWith(copyArg);
  assert(std::find(pass.largeLoadableArgs.begin(), pass.largeLoadableArgs.end(),
                   arg) == pass.largeLoadableArgs.end());

  arg = arg->getParent()->replaceFunctionArgument(
      arg->getIndex(), newSILType, ValueOwnershipKind::Trivial, arg->getDecl());

  copyArg->replaceAllUsesWith(arg);
  copyArg->eraseFromParent();

  return arg;
}

void LoadableStorageAllocation::insertIndirectReturnArgs() {
  GenericEnvironment *genEnv = pass.F->getGenericEnvironment();
  auto loweredTy = pass.F->getLoweredFunctionType();
  if (!genEnv && loweredTy->isPolymorphic()) {
    genEnv = getGenericEnvironment(pass.F->getModule(), loweredTy);
  }
  auto singleResult = loweredTy->getSingleResult();
  SILType resultStorageType = singleResult.getSILStorageType();
  auto canType = resultStorageType.getSwiftRValueType();
  if (canType->hasTypeParameter()) {
    assert(genEnv && "Expected a GenericEnv");
    canType = genEnv->mapTypeIntoContext(canType)->getCanonicalType();
  }
  resultStorageType = SILType::getPrimitiveObjectType(canType);

  auto &ctx = pass.F->getModule().getASTContext();
  auto var = new (ctx) ParamDecl(
      VarDecl::Specifier::InOut, SourceLoc(), SourceLoc(),
      ctx.getIdentifier("$return_value"), SourceLoc(),
      ctx.getIdentifier("$return_value"),
      resultStorageType.getSwiftRValueType(), pass.F->getDeclContext());
  pass.F->begin()->insertFunctionArgument(0, resultStorageType.getAddressType(),
                                          ValueOwnershipKind::Trivial, var);
}

void LoadableStorageAllocation::convertIndirectFunctionArgs() {
  SILBasicBlock *entry = pass.F->getEntryBlock();
  SILBuilder argBuilder(entry->begin());

  GenericEnvironment *genEnv = pass.F->getGenericEnvironment();
  auto loweredTy = pass.F->getLoweredFunctionType();
  if (!genEnv && loweredTy->isPolymorphic()) {
    genEnv = getGenericEnvironment(pass.F->getModule(), loweredTy);
  }

  for (SILArgument *arg : entry->getArguments()) {
    SILType storageType = arg->getType();
    SILType newSILType = getNewSILType(genEnv, storageType, pass.Mod);
    if (newSILType != storageType) {
      ValueOwnershipKind ownership = arg->getOwnershipKind();
      arg = replaceArgType(argBuilder, arg, newSILType);
      if (isLargeLoadableType(genEnv, storageType, pass.Mod)) {
        // Add to largeLoadableArgs if and only if it wasn't a modified function
        // signature arg
        pass.largeLoadableArgs.push_back(arg);
      } else {
        arg->setOwnershipKind(ownership);
        pass.funcSigArgs.push_back(arg);
      }
    }
  }

  // Convert the result type to indirect if necessary:
  if (modResultType(pass.F, pass.Mod)) {
    insertIndirectReturnArgs();
  }
}

static void convertBBArgType(SILBuilder &argBuilder, SILType newSILType,
                             SILArgument *arg) {
  CopyValueInst *copyArg = argBuilder.createCopyValue(
      RegularLocation(const_cast<ValueDecl *>(arg->getDecl())),
      SILUndef::get(newSILType, arg->getFunction()->getModule()));

  arg->replaceAllUsesWith(copyArg);
  arg = arg->getParent()->replacePHIArgument(arg->getIndex(), newSILType,
                                             arg->getOwnershipKind());

  copyArg->replaceAllUsesWith(arg);
  copyArg->eraseFromParent();
}

void LoadableStorageAllocation::convertApplyResults() {
  auto &silModue = pass.F->getModule();
  for (auto &BB : *pass.F) {
    for (auto &II : BB) {
      auto *currIns = &II;
      if (!ApplySite::isa(currIns)) {
        continue;
      }
      if (dyn_cast<PartialApplyInst>(currIns)) {
        continue;
      }
      auto applySite = ApplySite(currIns);
      if (!modifiableApply(applySite, pass.Mod)) {
        continue;
      }
      SILFunctionType *origSILFunctionType = applySite.getSubstCalleeType();
      auto canSILFunctionType = CanSILFunctionType(origSILFunctionType);
      Lowering::GenericContextScope GenericScope(
          silModue.Types, canSILFunctionType->getGenericSignature());
      GenericEnvironment *genEnv = nullptr;
      if (origSILFunctionType->isPolymorphic()) {
        genEnv = getGenericEnvironment(silModue, canSILFunctionType);
      }
      if (!modResultType(genEnv, canSILFunctionType, pass.Mod)) {
        continue;
      }
      auto singleResult = canSILFunctionType->getSingleResult();
      auto resultStorageType = singleResult.getSILStorageType();
      if (!isLargeLoadableType(genEnv, resultStorageType, pass.Mod)) {
        // Make sure it is a function type
        auto canType = resultStorageType.getSwiftRValueType();
        assert(dyn_cast<SILFunctionType>(canType.getPointer()) &&
               "Expected SILFunctionType for the result type");
        continue;
      }
      auto newSILType = getNewSILType(genEnv, resultStorageType, pass.Mod);
      auto *newVal = allocateForApply(currIns, newSILType.getObjectType());
      currIns->replaceAllUsesWith(newVal);
      if (auto tryApplyIns = dyn_cast<TryApplyInst>(currIns)) {
        auto *normalBB = tryApplyIns->getNormalBB();
        SILBuilder argBuilder(normalBB->begin());
        assert(normalBB->getNumArguments() == 1 &&
               "Expected only one arg for try_apply normal BB");
        auto arg = normalBB->getArgument(0);
        arg->replaceAllUsesWith(newVal);
        auto emptyTy = SILType::getPrimitiveObjectType(
            TupleType::getEmpty(argBuilder.getModule().getASTContext()));
        convertBBArgType(argBuilder, emptyTy, arg);
      }
    }
  }
}

void LoadableStorageAllocation::
    convertIndirectFunctionPointerArgsForUnmodifiable() {
  SILBasicBlock *entry = pass.F->getEntryBlock();
  SILBuilder argBuilder(entry->begin());

  for (SILArgument *arg : entry->getArguments()) {
    SILType storageType = arg->getType();
    GenericEnvironment *genEnv = pass.F->getGenericEnvironment();
    auto loweredTy = pass.F->getLoweredFunctionType();
    if (!genEnv && loweredTy->isPolymorphic()) {
      genEnv = getGenericEnvironment(pass.F->getModule(), loweredTy);
    }
    SILType newSILType = getNewSILType(genEnv, storageType, pass.Mod);
    // We only care about function signatures that are not block storage
    if (!isLargeLoadableType(genEnv, storageType, pass.Mod) &&
        !dyn_cast<SILBlockStorageType>(
            storageType.getSwiftRValueType().getPointer()) &&
        (newSILType != storageType)) {
      auto *castInstr = argBuilder.createUncheckedBitCast(
          RegularLocation(const_cast<ValueDecl *>(arg->getDecl())), arg,
          newSILType);
      arg->replaceAllUsesWith(castInstr);
      castInstr->setOperand(0, arg);
    }
  }
}

void LoadableStorageAllocation::convertIndirectBasicBlockArgs() {
  SILBasicBlock *entry = pass.F->getEntryBlock();
  GenericEnvironment *genEnv = pass.F->getGenericEnvironment();
  auto loweredTy = pass.F->getLoweredFunctionType();
  if (!genEnv && loweredTy->isPolymorphic()) {
    genEnv = getGenericEnvironment(pass.F->getModule(), loweredTy);
  }
  for (SILBasicBlock &BB : *pass.F) {
    if (&BB == entry) {
      // Already took care of function args
      continue;
    }
    SILBuilder argBuilder(BB.begin());
    for (SILArgument *arg : BB.getArguments()) {
      if (!shouldConvertBBArg(arg, pass.Mod)) {
        continue;
      }
      SILType storageType = arg->getType();
      SILType newSILType = getNewSILType(genEnv, storageType, pass.Mod);
      convertBBArgType(argBuilder, newSILType, arg);
    }
  }
}

void LoadableStorageAllocation::allocateForArg(SILValue value) {
  if (auto *allocInstr = dyn_cast<AllocStackInst>(value)) {
    // Special case: the value was already an Alloc
    // This happens in case of values from apply results (for example)
    // we *should* add a load for the current uses.
    // Said load should happen before the first use
    // As such add it right after the apply()
    LoadInst *load = nullptr;
    assert(pass.allocToApplyRetMap.find(allocInstr) !=
               pass.allocToApplyRetMap.end() &&
           "Alloc is not for apply results");
    auto *applyInst =
        dyn_cast<SILInstruction>(pass.allocToApplyRetMap[allocInstr]);
    assert(applyInst && "Value is not an apply");
    auto II = applyInst->getIterator();
    ++II;
    SILBuilder loadBuilder(II);
    if (pass.F->hasUnqualifiedOwnership()) {
      load = loadBuilder.createLoad(applyInst->getLoc(), value,
                                    LoadOwnershipQualifier::Unqualified);
    } else {
      load = loadBuilder.createLoad(applyInst->getLoc(), value,
                                    LoadOwnershipQualifier::Take);
    }
    pass.argsToLoadedValueMap[value] = load;
    return;
  }

  assert(!ApplySite::isa(value) && "Unexpected instruction");

  SILBuilder allocBuilder(pass.F->begin()->begin());
  AllocStackInst *allocInstr =
      allocBuilder.createAllocStack(getLocForValue(value), value->getType());

  LoadInst *loadCopy = nullptr;
  auto *applyOutlinedCopy =
      createOutlinedCopyCall(allocBuilder, value, allocInstr, pass);

  if (pass.F->hasUnqualifiedOwnership()) {
    loadCopy = allocBuilder.createLoad(applyOutlinedCopy->getLoc(), allocInstr,
                                       LoadOwnershipQualifier::Unqualified);
  } else {
    loadCopy = allocBuilder.createLoad(applyOutlinedCopy->getLoc(), allocInstr,
                                       LoadOwnershipQualifier::Take);
  }
  pass.argsToLoadedValueMap[value] = loadCopy;

  // Insert stack deallocations.
  for (TermInst *termInst : pass.returnInsts) {
    SILBuilder deallocBuilder(termInst);
    deallocBuilder.createDeallocStack(allocInstr->getLoc(), allocInstr);
  }
}

AllocStackInst *LoadableStorageAllocation::allocateForApply(SILValue value,
                                                            SILType type) {
  SILBuilder allocBuilder(pass.F->begin()->begin());
  auto *allocInstr = allocBuilder.createAllocStack(getLocForValue(value), type);

  pass.largeLoadableArgs.push_back(allocInstr);
  pass.allocToApplyRetMap[allocInstr] = value;
  pass.applyRetToAllocMap[value] = allocInstr;

  for (TermInst *termInst : pass.returnInsts) {
    SILBuilder deallocBuilder(termInst);
    deallocBuilder.createDeallocStack(allocInstr->getLoc(), allocInstr);
  }

  return allocInstr;
}

//===----------------------------------------------------------------------===//
// LoadableByAddress: Top-Level Function Transform.
//===----------------------------------------------------------------------===//

namespace {
class LoadableByAddress : public SILModuleTransform {
  /// The entry point to this function transformation.
  void run() override;

  void runOnFunction(SILFunction *F);

private:
  void updateLoweredTypes(SILFunction *F);
  void recreateApplies();
  void recreateSingleApply(SILInstruction *applyInst);
  void recreateConvInstrs();
  void recreateBuiltinInstrs();
  void recreateLoadInstrs();
  void recreateUncheckedEnumDataInstrs();
  void recreateUncheckedTakeEnumDataAddrInst();
  void fixStoreToBlockStorageInstrs();

private:
  llvm::SetVector<SILFunction *> modFuncs;
  llvm::SetVector<SILInstruction *> conversionInstrs;
  llvm::SetVector<BuiltinInst *> builtinInstrs;
  llvm::SetVector<LoadInst *> loadInstrsOfFunc;
  llvm::SetVector<UncheckedEnumDataInst *> uncheckedEnumDataOfFunc;
  llvm::SetVector<UncheckedTakeEnumDataAddrInst *>
      uncheckedTakeEnumDataAddrOfFunc;
  llvm::SetVector<StoreInst *> storeToBlockStorageInstrs;
  llvm::DenseSet<SILInstruction *> modApplies;
  llvm::DenseMap<SILValue, SILValue> allApplyRetToAllocMap;
};
} // end anonymous namespace

static void setInstrUsers(StructLoweringState &pass, AllocStackInst *allocInstr,
                          SILValue instrOperand, StoreInst *store) {
  SmallVector<Operand *, 8> uses(instrOperand->getUses());
  for (Operand *userOp : uses) {
    SILInstruction *user = userOp->getUser();
    if (user == store) {
      continue;
    }
    if (ApplySite::isa(user)) {
      ApplySite site(user);
      if (modifiableApply(site, pass.Mod)) {
        userOp->set(allocInstr);
      }
    } else if (auto *storeUser = dyn_cast<StoreInst>(user)) {
      // Optimization: replace with copy_addr to reduce code size
      assert(std::find(pass.storeInstsToMod.begin(), pass.storeInstsToMod.end(),
                       storeUser) == pass.storeInstsToMod.end() &&
             "Did not expect this instr in storeInstsToMod");
      SILBuilder copyBuilder(storeUser);
      SILValue tgt = storeUser->getDest();
      createOutlinedCopyCall(copyBuilder, allocInstr, tgt, pass);
      storeUser->replaceAllUsesWith(tgt);
      storeUser->getParent()->erase(storeUser);
    }
  }
}

static void allocateAndSetForInstrOperand(StructLoweringState &pass,
                                          SILInstruction *instrOperand) {
  assert(instrOperand->getType().isObject());
  SILBuilder allocBuilder(pass.F->begin()->begin());
  AllocStackInst *allocInstr = allocBuilder.createAllocStack(
      instrOperand->getLoc(), instrOperand->getType());

  auto II = instrOperand->getIterator();
  ++II;
  SILBuilder storeBuilder(II);
  StoreInst *store = nullptr;
  if (pass.F->hasQualifiedOwnership()) {
    store = storeBuilder.createStore(instrOperand->getLoc(), instrOperand,
                                     allocInstr, StoreOwnershipQualifier::Init);
  } else {
    store = storeBuilder.createStore(instrOperand->getLoc(), instrOperand,
                                     allocInstr,
                                     StoreOwnershipQualifier::Unqualified);
  }

  // Insert stack deallocations.
  for (TermInst *termInst : pass.returnInsts) {
    SILBuilder deallocBuilder(termInst);
    deallocBuilder.createDeallocStack(allocInstr->getLoc(), allocInstr);
  }

  // Traverse all the uses of instrOperand - see if we can replace
  setInstrUsers(pass, allocInstr, instrOperand, store);
}

static void allocateAndSetForArgumentOperand(StructLoweringState &pass,
                                             SILValue value,
                                             SILInstruction *applyInst) {
  assert(value->getType().isObject());
  auto *arg = dyn_cast<SILArgument>(value);
  assert(arg && "non-instr operand must be an argument");

  SILBuilder allocBuilder(pass.F->begin()->begin());
  AllocStackInst *allocInstr =
      allocBuilder.createAllocStack(applyInst->getLoc(), value->getType());

  auto storeIt = arg->getParent()->begin();
  if (storeIt == pass.F->begin()->begin()) {
    // Store should happen *after* allocInstr
    ++storeIt;
  }
  SILBuilder storeBuilder(storeIt);

  StoreInst *store = nullptr;
  if (pass.F->hasQualifiedOwnership()) {
    store = storeBuilder.createStore(applyInst->getLoc(), value, allocInstr,
                                     StoreOwnershipQualifier::Init);
  } else {
    store = storeBuilder.createStore(applyInst->getLoc(), value, allocInstr,
                                     StoreOwnershipQualifier::Unqualified);
  }

  // Insert stack deallocations.
  for (TermInst *termInst : pass.returnInsts) {
    SILBuilder deallocBuilder(termInst);
    deallocBuilder.createDeallocStack(allocInstr->getLoc(), allocInstr);
  }

  // Traverse all the uses of instrOperand - see if we can replace
  setInstrUsers(pass, allocInstr, value, store);
}

static bool allUsesAreReplaceable(SILInstruction *instr,
                                  irgen::IRGenModule &Mod) {
  bool allUsesAreReplaceable = true;
  for (auto *user : instr->getUses()) {
    SILInstruction *userIns = user->getUser();
    switch (userIns->getKind()) {
    case ValueKind::RetainValueInst:
    case ValueKind::ReleaseValueInst:
    case ValueKind::StoreInst:
    case ValueKind::DebugValueInst:
      break;
    case ValueKind::ApplyInst:
    case ValueKind::TryApplyInst:
    case ValueKind::PartialApplyInst: {
      // Replaceable only if it is not the function pointer
      ApplySite site(userIns);
      if (!modifiableApply(site, Mod)) {
        allUsesAreReplaceable = false;
        break;
      }
      SILValue callee = site.getCallee();
      if (callee == instr) {
        allUsesAreReplaceable = false;
      }
      SILType currType = instr->getType().getObjectType();
      GenericEnvironment *genEnv =
          instr->getFunction()->getGenericEnvironment();
      auto loweredTy = instr->getFunction()->getLoweredFunctionType();
      if (!genEnv && loweredTy->isPolymorphic()) {
        genEnv = getGenericEnvironment(instr->getModule(), loweredTy);
      }
      SILType newSILType = getNewSILType(genEnv, currType, Mod);
      if (currType == newSILType) {
        allUsesAreReplaceable = false;
      }
      break;
    }
    case ValueKind::StructExtractInst:
    case ValueKind::SwitchEnumInst: {
      break;
    }
    default:
      allUsesAreReplaceable = false;
    }
  }
  return allUsesAreReplaceable;
}

static void castTupleInstr(SILInstruction *instr, IRGenModule &Mod) {
  SILType currSILType = instr->getType().getObjectType();
  CanType currCanType = currSILType.getSwiftRValueType();
  SILFunctionType *funcType = dyn_cast<SILFunctionType>(currCanType);
  assert(funcType && "Expected SILFunctionType as tuple's return");
  CanSILFunctionType canFuncType = CanSILFunctionType(funcType);
  GenericEnvironment *genEnv = instr->getFunction()->getGenericEnvironment();
  if (!genEnv && canFuncType->isPolymorphic()) {
    genEnv = getGenericEnvironment(instr->getModule(), canFuncType);
  }
  SILType newSILType = getNewSILFunctionType(genEnv, funcType, Mod);
  if (currSILType.isAddress()) {
    newSILType = newSILType.getAddressType();
  }
  auto II = instr->getIterator();
  ++II;
  SILBuilder castBuilder(II);
  SILInstruction *castInstr = nullptr;
  switch (instr->getKind()) {
  // Add cast to the new sil function type:
  case ValueKind::TupleExtractInst: {
    castInstr = castBuilder.createUncheckedBitCast(instr->getLoc(), instr,
                                                   newSILType.getObjectType());
    break;
  }
  case ValueKind::TupleElementAddrInst: {
    castInstr = castBuilder.createUncheckedAddrCast(
        instr->getLoc(), instr, newSILType.getAddressType());
    break;
  }
  default:
    llvm_unreachable("Unexpected instruction inside tupleInstsToMod");
  }
  instr->replaceAllUsesWith(castInstr);
  castInstr->setOperand(0, instr);
}

static SILValue createCopyOfEnum(StructLoweringState &pass,
                                 SwitchEnumInst *orig) {
  auto value = orig->getOperand();
  auto type = value->getType();
  if (type.isObject()) {
    SILBuilder allocBuilder(pass.F->begin()->begin());
    // support for non-address operands / enums
    auto *allocInstr = allocBuilder.createAllocStack(orig->getLoc(), type);
    SILBuilder storeBuilder(orig);
    StoreInst *store = nullptr;
    if (pass.F->hasQualifiedOwnership()) {
      store = storeBuilder.createStore(orig->getLoc(), value, allocInstr,
                                       StoreOwnershipQualifier::Init);
    } else {
      store = storeBuilder.createStore(orig->getLoc(), value, allocInstr,
                                       StoreOwnershipQualifier::Unqualified);
    }
    // Insert stack deallocations.
    for (TermInst *termInst : pass.returnInsts) {
      SILBuilder deallocBuilder(termInst);
      deallocBuilder.createDeallocStack(allocInstr->getLoc(), allocInstr);
    }
    value = allocInstr;
  }
  SILBuilder allocBuilder(pass.F->begin()->begin());
  auto *allocInstr = allocBuilder.createAllocStack(getLocForValue(value), type);

  SILBuilder copyBuilder(orig);
  createOutlinedCopyCall(copyBuilder, value, allocInstr, pass);

  for (TermInst *termInst : pass.returnInsts) {
    SILBuilder deallocBuilder(termInst);
    deallocBuilder.createDeallocStack(allocInstr->getLoc(), allocInstr);
  }

  return allocInstr;
}

static void rewriteFunction(StructLoweringState &pass,
                            LoadableStorageAllocation &allocator) {

  GenericEnvironment *genEnv = pass.F->getGenericEnvironment();
  auto loweredTy = pass.F->getLoweredFunctionType();
  if (!genEnv && loweredTy->isPolymorphic()) {
    genEnv = getGenericEnvironment(pass.F->getModule(), loweredTy);
  }
  bool repeat = false;
  llvm::DenseSet<SILInstruction *> currentModApplies;
  do {
    while (!pass.switchEnumInstsToMod.empty()) {
      auto *instr = pass.switchEnumInstsToMod.pop_back_val();
      /* unchecked_take_enum_data_addr can be destructive.
       * work on a copy instead of the original enum */
      auto copiedValue = createCopyOfEnum(pass, instr);
      SILBuilder enumBuilder(instr);
      unsigned numOfCases = instr->getNumCases();
      SmallVector<std::pair<EnumElementDecl *, SILBasicBlock *>, 16> caseBBs;
      for (unsigned i = 0; i < numOfCases; ++i) {
        auto currCase = instr->getCase(i);
        auto *currBB = currCase.second;
        SILBuilder argBuilder(currBB->begin());
        assert(currBB->getNumArguments() <= 1 && "Unhandled BB Type");
        EnumElementDecl *decl = currCase.first;
        for (SILArgument *arg : currBB->getArguments()) {
          SILType storageType = arg->getType();
          SILType newSILType = getNewSILType(genEnv, storageType, pass.Mod);
          if (storageType == newSILType) {
            newSILType = newSILType.getAddressType();
          }

          auto *newArg = argBuilder.createUncheckedTakeEnumDataAddr(
              instr->getLoc(), copiedValue, decl, newSILType.getAddressType());
          arg->replaceAllUsesWith(newArg);
          currBB->eraseArgument(0);

          // Load the enum addr then see if we can get rid of the load:
          LoadInst *loadArg = nullptr;
          if (pass.F->hasUnqualifiedOwnership()) {
            loadArg = argBuilder.createLoad(
                newArg->getLoc(), newArg, LoadOwnershipQualifier::Unqualified);
          } else {
            loadArg = argBuilder.createLoad(newArg->getLoc(), newArg,
                                            LoadOwnershipQualifier::Take);
          }
          newArg->replaceAllUsesWith(loadArg);
          loadArg->setOperand(newArg);

          // If the load is of a function type - do not replace it.
          auto loadedType = loadArg->getType();
          auto canLoadedType = loadedType.getSwiftRValueType();
          if (dyn_cast<SILFunctionType>(canLoadedType.getPointer())) {
            continue;
          }

          if (allUsesAreReplaceable(loadArg, pass.Mod)) {
            allocator.replaceLoadWithCopyAddr(loadArg);
          } else {
            allocator.replaceLoadWithCopyAddrForModifiable(loadArg);
          }
        }
        caseBBs.push_back(std::make_pair(decl, currBB));
      }
      SILBasicBlock *defaultBB =
          instr->hasDefault() ? instr->getDefaultBB() : nullptr;
      auto *newInstr = enumBuilder.createSwitchEnumAddr(
          instr->getLoc(), copiedValue, defaultBB, caseBBs);
      instr->replaceAllUsesWith(newInstr);
      instr->getParent()->erase(instr);
    }

    while (!pass.structExtractInstsToMod.empty()) {
      auto *instr = pass.structExtractInstsToMod.pop_back_val();
      bool updateResultTy = pass.resultTyInstsToMod.count(instr) != 0;
      if (updateResultTy) {
        pass.resultTyInstsToMod.remove(instr);
      }
      SILBuilder structBuilder(instr);
      auto *newInstr = structBuilder.createStructElementAddr(
          instr->getLoc(), instr->getOperand(), instr->getField(),
          instr->getType().getAddressType());
      // Load the struct element then see if we can get rid of the load:
      LoadInst *loadArg = nullptr;
      if (pass.F->hasUnqualifiedOwnership()) {
        loadArg = structBuilder.createLoad(newInstr->getLoc(), newInstr,
                                           LoadOwnershipQualifier::Unqualified);
      } else {
        loadArg = structBuilder.createLoad(newInstr->getLoc(), newInstr,
                                           LoadOwnershipQualifier::Take);
      }
      instr->replaceAllUsesWith(loadArg);
      instr->getParent()->erase(instr);

      // If the load is of a function type - do not replace it.
      auto loadedType = loadArg->getType();
      auto canLoadedType = loadedType.getSwiftRValueType();
      if (dyn_cast<SILFunctionType>(canLoadedType.getPointer())) {
        continue;
      }

      if (allUsesAreReplaceable(loadArg, pass.Mod)) {
        allocator.replaceLoadWithCopyAddr(loadArg);
      } else {
        allocator.replaceLoadWithCopyAddrForModifiable(loadArg);
      }
      if (updateResultTy) {
        pass.resultTyInstsToMod.insert(newInstr);
      }
    }

    while (!pass.applies.empty()) {
      auto *applyInst = pass.applies.pop_back_val();
      if (currentModApplies.count(applyInst) == 0) {
        currentModApplies.insert(applyInst);
      }
      ApplySite applySite = ApplySite(applyInst);
      for (Operand &operand : applySite.getArgumentOperands()) {
        SILValue currOperand = operand.get();
        SILType silType = currOperand->getType();
        if (isLargeLoadableType(genEnv, silType, pass.Mod)) {
          SILInstruction *currOperandInstr =
              dyn_cast<SILInstruction>(currOperand);
          // Get its storage location as a new operand
          if (!currOperandInstr) {
            allocateAndSetForArgumentOperand(pass, currOperand, applyInst);
          } else if (auto *load = dyn_cast<LoadInst>(currOperandInstr)) {
            // If the load is of a function type - do not replace it.
            auto loadedType = load->getType();
            auto canLoadedType = loadedType.getSwiftRValueType();
            if (dyn_cast<SILFunctionType>(canLoadedType.getPointer())) {
              continue;
            }

            if (allUsesAreReplaceable(load, pass.Mod)) {
              allocator.replaceLoadWithCopyAddr(load);
            } else {
              allocator.replaceLoadWithCopyAddrForModifiable(load);
            }
          } else {
            // TODO: peephole: special handling of known cases:
            // ApplyInst, TupleExtractInst
            allocateAndSetForInstrOperand(pass, currOperandInstr);
          }
        }
      }
    }
    repeat = !pass.switchEnumInstsToMod.empty() ||
             !pass.structExtractInstsToMod.empty();
    assert(pass.applies.empty());
    pass.applies.append(currentModApplies.begin(), currentModApplies.end());
  } while (repeat);

  for (SILInstruction *instr : pass.instsToMod) {
    for (Operand &operand : instr->getAllOperands()) {
      auto currOperand = operand.get();
      if (std::find(pass.largeLoadableArgs.begin(),
                    pass.largeLoadableArgs.end(),
                    currOperand) != pass.largeLoadableArgs.end()) {
        SILValue newOperand = pass.argsToLoadedValueMap[currOperand];
        assert(newOperand != currOperand &&
               "Did not allocate storage and convert operand");
        operand.set(newOperand);
      }
    }
  }

  for (SILInstruction *instr : pass.tupleInstsToMod) {
    castTupleInstr(instr, pass.Mod);
  }

  while (!pass.allocStackInstsToMod.empty()) {
    auto *instr = pass.allocStackInstsToMod.pop_back_val();
    SILBuilder allocBuilder(instr);
    SILType currSILType = instr->getType();
    SILType newSILType = getNewSILType(genEnv, currSILType, pass.Mod);
    auto *newInstr = allocBuilder.createAllocStack(instr->getLoc(), newSILType);
    instr->replaceAllUsesWith(newInstr);
    instr->getParent()->erase(instr);
  }

  while (!pass.pointerToAddrkInstsToMod.empty()) {
    auto *instr = pass.pointerToAddrkInstsToMod.pop_back_val();
    SILBuilder pointerBuilder(instr);
    SILType currSILType = instr->getType();
    SILType newSILType = getNewSILType(genEnv, currSILType, pass.Mod);
    auto *newInstr = pointerBuilder.createPointerToAddress(
        instr->getLoc(), instr->getOperand(), newSILType.getAddressType(),
        instr->isStrict());
    instr->replaceAllUsesWith(newInstr);
    instr->getParent()->erase(instr);
  }

  for (SILInstruction *instr : pass.debugInstsToMod) {
    assert(instr->getAllOperands().size() == 1 &&
           "Debug instructions have one operand");
    for (Operand &operand : instr->getAllOperands()) {
      auto currOperand = operand.get();
      if (pass.argsToLoadedValueMap.find(currOperand) !=
          pass.argsToLoadedValueMap.end()) {
        SILValue newOperand = pass.argsToLoadedValueMap[currOperand];
        assert(newOperand != currOperand &&
               "Did not allocate storage and convert operand");
        operand.set(newOperand);
      } else {
        assert(currOperand->getType().isAddress() &&
               "Expected an address type");
        SILBuilder debugBuilder(instr);
        debugBuilder.createDebugValueAddr(instr->getLoc(), currOperand);
        instr->getParent()->erase(instr);
      }
    }
  }

  for (StoreInst *instr : pass.storeInstsToMod) {
    SILValue src = instr->getSrc();
    SILValue tgt = instr->getDest();
    SILType srcType = src->getType();
    SILType tgtType = tgt->getType();
    assert(srcType && "Expected an address-type source");
    assert(tgtType.isAddress() && "Expected an address-type target");
    assert(srcType == tgtType && "Source and target type do not match");

    SILBuilder copyBuilder(instr);
    createOutlinedCopyCall(copyBuilder, src, tgt, pass);
    instr->replaceAllUsesWith(tgt);
    instr->getParent()->erase(instr);
  }

  for (RetainValueInst *instr : pass.retainInstsToMod) {
    SILBuilder retainBuilder(instr);
    auto *newInstr = retainBuilder.createRetainValueAddr(
        instr->getLoc(), instr->getOperand(), instr->getAtomicity());
    instr->replaceAllUsesWith(newInstr);
    instr->getParent()->erase(instr);
  }

  for (ReleaseValueInst *instr : pass.releaseInstsToMod) {
    SILBuilder releaseBuilder(instr);
    auto *newInstr = releaseBuilder.createReleaseValueAddr(
        instr->getLoc(), instr->getOperand(), instr->getAtomicity());
    instr->replaceAllUsesWith(newInstr);
    instr->getParent()->erase(instr);
  }

  for (SILInstruction *instr : pass.resultTyInstsToMod) {
    // Update the return type of these instrs
    // Note: The operand was already updated!
    SILType currSILType = instr->getType().getObjectType();
    SILType newSILType = getNewSILType(genEnv, currSILType, pass.Mod);
    SILBuilder resultTyBuilder(instr);
    SILLocation Loc = instr->getLoc();
    SILInstruction *newInstr = nullptr;
    switch (instr->getKind()) {
    case ValueKind::StructExtractInst: {
      auto *convInstr = dyn_cast<StructExtractInst>(instr);
      newInstr = resultTyBuilder.createStructExtract(
          Loc, convInstr->getOperand(), convInstr->getField(),
          newSILType.getObjectType());
      break;
    }
    case ValueKind::StructElementAddrInst: {
      auto *convInstr = dyn_cast<StructElementAddrInst>(instr);
      newInstr = resultTyBuilder.createStructElementAddr(
          Loc, convInstr->getOperand(), convInstr->getField(),
          newSILType.getAddressType());
      break;
    }
    case ValueKind::RefTailAddrInst: {
      auto *convInstr = dyn_cast<RefTailAddrInst>(instr);
      newInstr = resultTyBuilder.createRefTailAddr(Loc, convInstr->getOperand(),
                                                   newSILType.getAddressType());
      break;
    }
    case ValueKind::RefElementAddrInst: {
      auto *convInstr = dyn_cast<RefElementAddrInst>(instr);
      newInstr = resultTyBuilder.createRefElementAddr(
          Loc, convInstr->getOperand(), convInstr->getField(),
          newSILType.getAddressType());
      break;
    }
    case ValueKind::BeginAccessInst: {
      auto *convInstr = dyn_cast<BeginAccessInst>(instr);
      newInstr = resultTyBuilder.createBeginAccess(Loc, convInstr->getOperand(),
                                                   convInstr->getAccessKind(),
                                                   convInstr->getEnforcement());
      break;
    }
    case ValueKind::EnumInst: {
      auto *convInstr = dyn_cast<EnumInst>(instr);
      SILValue operand =
          convInstr->hasOperand() ? convInstr->getOperand() : SILValue();
      newInstr = resultTyBuilder.createEnum(
          Loc, operand, convInstr->getElement(), newSILType.getObjectType());
      break;
    }
    default:
      llvm_unreachable("Unhandled aggrTy instr");
    }
    instr->replaceAllUsesWith(newInstr);
    instr->getParent()->erase(instr);
  }

  for (MethodInst *instr : pass.methodInstsToMod) {
    SILType currSILType = instr->getType();
    CanType currCanType = currSILType.getSwiftRValueType();
    SILFunctionType *currSILFunctionType =
        dyn_cast<SILFunctionType>(currCanType.getPointer());
    GenericEnvironment *genEnvForMethod = nullptr;
    if (currSILFunctionType->isPolymorphic()) {
      genEnvForMethod = getGenericEnvironment(
          instr->getModule(), CanSILFunctionType(currSILFunctionType));
    }
    SILType newSILType =
        getNewSILFunctionType(genEnvForMethod, currSILFunctionType, pass.Mod);
    auto member = instr->getMember();
    auto loc = instr->getLoc();
    bool isVolatile = instr->isVolatile();
    SILBuilder methodBuilder(instr);
    MethodInst *newInstr = nullptr;

    switch (instr->getKind()) {
    case ValueKind::ClassMethodInst: {
      SILValue selfValue = instr->getOperand(0);
      newInstr = methodBuilder.createClassMethod(loc, selfValue, member,
                                                 newSILType, isVolatile);
      break;
    }
    case ValueKind::SuperMethodInst: {
      SILValue selfValue = instr->getOperand(0);
      newInstr = methodBuilder.createSuperMethod(loc, selfValue, member,
                                                 newSILType, isVolatile);
      break;
    }
    case ValueKind::DynamicMethodInst: {
      auto *DMI = dyn_cast<DynamicMethodInst>(instr);
      assert(DMI && "ValueKind is Witness Method but dyn_cast failed");
      SILValue selfValue = instr->getOperand(0);
      newInstr = methodBuilder.createDynamicMethod(loc, selfValue, member,
                                                   newSILType, isVolatile);
      break;
    }
    case ValueKind::WitnessMethodInst: {
      auto *WMI = dyn_cast<WitnessMethodInst>(instr);
      assert(WMI && "ValueKind is Witness Method but dyn_cast failed");
      newInstr = methodBuilder.createWitnessMethod(
          loc, WMI->getLookupType(), WMI->getConformance(), member, newSILType,
          isVolatile);
      break;
    }
    default:
      llvm_unreachable("Expected known MethodInst ValueKind");
    }

    instr->replaceAllUsesWith(newInstr);
    instr->getParent()->erase(instr);
  }

  while (!pass.modReturnInsts.empty()) {
    auto *instr = pass.modReturnInsts.pop_back_val();
    auto loc = instr->getLoc(); // SILLocation::RegularKind
    auto regLoc = RegularLocation(loc.getSourceLoc());
    SILBuilder retBuilder(instr);
    // Before we return an empty tuple, init return arg:
    auto *entry = pass.F->getEntryBlock();
    auto *retArg = entry->getArgument(0);
    auto retOp = instr->getOperand();
    auto storageType = retOp->getType();
    if (storageType.isAddress()) {
      createOutlinedCopyCall(retBuilder, retOp, retArg, pass, &regLoc);
    } else {
      if (pass.F->hasQualifiedOwnership()) {
        retBuilder.createStore(regLoc, retOp, retArg,
                               StoreOwnershipQualifier::Init);
      } else {
        retBuilder.createStore(regLoc, retOp, retArg,
                               StoreOwnershipQualifier::Unqualified);
      }
    }
    auto emptyTy = retBuilder.getModule().Types.getLoweredType(
        TupleType::getEmpty(retBuilder.getModule().getASTContext()));
    auto newRetTuple = retBuilder.createTuple(regLoc, emptyTy, {});
    auto newRet = retBuilder.createReturn(newRetTuple->getLoc(), newRetTuple);
    instr->replaceAllUsesWith(newRet);
    instr->getParent()->erase(instr);
  }
}

// Rewrite function return argument if it is a "function pointer"
// If it is a large type also return true - will be re-written later
// Returns true if the return argument needed re-writing
static bool rewriteFunctionReturn(StructLoweringState &pass) {
  GenericEnvironment *genEnv = pass.F->getGenericEnvironment();
  auto loweredTy = pass.F->getLoweredFunctionType();
  if (!genEnv && loweredTy->isPolymorphic()) {
    genEnv = getGenericEnvironment(pass.F->getModule(), loweredTy);
  }
  SILFunction *F = pass.F;
  SILType resultTy = loweredTy->getAllResultsType();
  SILType newSILType = getNewSILType(genEnv, resultTy, pass.Mod);
  // We (currently) only care about function signatures
  if (!isLargeLoadableType(genEnv, resultTy, pass.Mod) &&
      (newSILType != resultTy)) {
    assert(loweredTy->getNumResults() == 1 && "Expected a single result");
    SILResultInfo origResultInfo = loweredTy->getSingleResult();
    SILResultInfo newSILResultInfo(newSILType.getSwiftRValueType(),
                                   origResultInfo.getConvention());
    auto NewTy = SILFunctionType::get(
        loweredTy->getGenericSignature(), loweredTy->getExtInfo(),
        loweredTy->getCalleeConvention(), loweredTy->getParameters(),
        newSILResultInfo, loweredTy->getOptionalErrorResult(),
        F->getModule().getASTContext());
    F->rewriteLoweredTypeUnsafe(NewTy);
    return true;
  } else if (isLargeLoadableType(genEnv, resultTy, pass.Mod)) {
    return true;
  }
  return false;
}

void LoadableByAddress::runOnFunction(SILFunction *F) {
  CanSILFunctionType funcType = F->getLoweredFunctionType();
  IRGenModule *currIRMod = getIRGenModule()->IRGen.getGenModule(F);
  Lowering::GenericContextScope GenericScope(getModule()->Types,
                                             funcType->getGenericSignature());
  if (F->isExternalDeclaration()) {
    if (!modifiableFunction(funcType)) {
      return;
    }
    // External function - re-write external declaration - this is ABI!
    GenericEnvironment *genEnv = F->getGenericEnvironment();
    auto loweredTy = F->getLoweredFunctionType();
    if (!genEnv && loweredTy->isPolymorphic()) {
      genEnv = getGenericEnvironment(F->getModule(), loweredTy);
    }
    if (containsLargeLoadable(
            genEnv, F->getLoweredFunctionType()->getParameters(), *currIRMod) ||
        modResultType(genEnv, CanSILFunctionType(F->getLoweredFunctionType()),
                      *currIRMod)) {
      modFuncs.insert(F);
    }
    return;
  }

  StructLoweringState pass(F, *currIRMod);

  // Rewrite function args and insert allocs.
  LoadableStorageAllocation allocator(pass);
  allocator.allocateLoadableStorage();

  bool rewrittenReturn = false;
  if (modifiableFunction(funcType)) {
    rewrittenReturn = rewriteFunctionReturn(pass);
  }

  DEBUG(llvm::dbgs() << "\nREWRITING: " << F->getName(); F->dump());

  // Rewrite instructions relating to the loadable struct.
  rewriteFunction(pass, allocator);

  invalidateAnalysis(F, SILAnalysis::InvalidationKind::Instructions);

  // If we modified the function arguments - add to list of functions to clone
  if (modifiableFunction(funcType) &&
      (rewrittenReturn || !pass.largeLoadableArgs.empty() ||
       !pass.funcSigArgs.empty())) {
    modFuncs.insert(F);
  }
  // If we modified any applies - add them to the global list for recreation
  if (!pass.applies.empty()) {
    modApplies.insert(pass.applies.begin(), pass.applies.end());
  }
  if (!pass.applyRetToAllocMap.empty()) {
    allApplyRetToAllocMap.insert(pass.applyRetToAllocMap.begin(),
                                 pass.applyRetToAllocMap.end());
  }
}

static SmallVector<Substitution, 4>
getNewSubs(SubstitutionList origSubs, swift::irgen::IRGenModule *currIRMod,
           swift::GenericEnvironment *genEnv) {
  SmallVector<Substitution, 4> newSubs;
  for (auto sub : origSubs) {
    Type origType = sub.getReplacement();
    CanType origCanType = origType->getCanonicalType();
    if (!origCanType->isLegalSILType()) {
      newSubs.push_back(sub);
      continue;
    }
    SILType origSILType = SILType::getPrimitiveObjectType(origCanType);
    SILType newSILType = getNewSILType(genEnv, origSILType, *currIRMod);
    Type newType = newSILType.getSwiftRValueType()->getRValueType();
    newSubs.push_back(Substitution(newType, sub.getConformances()));
  }
  return newSubs;
}

void LoadableByAddress::recreateSingleApply(SILInstruction *applyInst) {
  auto *F = applyInst->getFunction();
  IRGenModule *currIRMod = getIRGenModule()->IRGen.getGenModule(F);
  // Collect common info
  ApplySite applySite = ApplySite(applyInst);
  SILValue callee = applySite.getCallee();
  if (ApplySite::isa(callee)) {
    // We need to re-create the callee's apply before recreating this one
    // else verification will fail with wrong SubstCalleeType
    auto calleInstr = dyn_cast<SILInstruction>(callee);
    assert(calleInstr && "Expected ApplySite Instr");
    if (modApplies.find(calleInstr) != modApplies.end()) {
      recreateSingleApply(calleInstr);
      modApplies.erase(calleInstr);
      callee = applySite.getCallee();
    }
  }
  SILFunctionType *origSILFunctionType = applySite.getSubstCalleeType();
  auto origCanType = CanSILFunctionType(origSILFunctionType);
  Lowering::GenericContextScope GenericScope(
      getModule()->Types, origCanType->getGenericSignature());
  GenericEnvironment *genEnv = nullptr;
  if (origSILFunctionType->isPolymorphic()) {
    genEnv = getGenericEnvironment(applyInst->getModule(),
                                   CanSILFunctionType(origSILFunctionType));
  }
  SILFunctionType *newSILFunctionType =
      getNewSILFunctionTypePtr(genEnv, origSILFunctionType, *currIRMod);
  CanSILFunctionType newCanSILFuncType(newSILFunctionType);
  SILFunctionConventions newSILFunctionConventions(newCanSILFuncType,
                                                   *getModule());
  SmallVector<SILValue, 8> callArgs;
  SILBuilder applyBuilder(applyInst);
  // If we turned a direct result into an indirect parameter
  // Find the new alloc we created earlier.
  // and pass it as first parameter:
  if (applyInst->getKind() != ValueKind::PartialApplyInst &&
      modResultType(genEnv, origCanType, *currIRMod) &&
      modifiableApply(applySite, *getIRGenModule())) {
    assert(allApplyRetToAllocMap.find(applyInst) !=
           allApplyRetToAllocMap.end());
    auto newAlloc = allApplyRetToAllocMap[applyInst];
    callArgs.push_back(newAlloc);
  }
  SmallVector<Substitution, 4> newSubs =
      getNewSubs(applySite.getSubstitutions(), currIRMod, genEnv);
  // Collect arg operands
  for (Operand &operand : applySite.getArgumentOperands()) {
    SILValue currOperand = operand.get();
    callArgs.push_back(currOperand);
  }
  // Recreate apply with new operands due to substitution-type cache
  SILInstruction *newApply = nullptr;
  switch (applyInst->getKind()) {
  case ValueKind::ApplyInst: {
    auto *castedApply = dyn_cast<ApplyInst>(applyInst);
    assert(castedApply && "ValueKind is ApplyInst but cast to it failed");
    newApply = applyBuilder.createApply(castedApply->getLoc(), callee, newSubs,
                                        callArgs, castedApply->isNonThrowing());
    applyInst->replaceAllUsesWith(newApply);
    break;
  }
  case ValueKind::TryApplyInst: {
    auto *castedApply = dyn_cast<TryApplyInst>(applyInst);
    assert(castedApply && "ValueKind is TryApplyInst but cast to it failed");
    newApply = applyBuilder.createTryApply(
        castedApply->getLoc(), callee, newSubs, callArgs,
        castedApply->getNormalBB(), castedApply->getErrorBB());
    applyInst->replaceAllUsesWith(newApply);
    break;
  }
  case ValueKind::PartialApplyInst: {
    auto *castedApply = dyn_cast<PartialApplyInst>(applyInst);
    assert(castedApply &&
           "ValueKind is PartialApplyInst but cast to it failed");
    // Change the type of the Closure
    auto partialApplyConvention = castedApply->getType()
                                      .getSwiftRValueType()
                                      ->getAs<SILFunctionType>()
                                      ->getCalleeConvention();

    newApply = applyBuilder.createPartialApply(castedApply->getLoc(), callee,
                                               newSubs, callArgs,
                                               partialApplyConvention);
    applyInst->replaceAllUsesWith(newApply);
    break;
  }
  default:
    llvm_unreachable("Unexpected instr: unknown apply type");
  }
  applyInst->getParent()->erase(applyInst);
}

void LoadableByAddress::recreateApplies() {
  while (!modApplies.empty()) {
    auto *applyInst = *modApplies.begin();
    recreateSingleApply(applyInst);
    modApplies.erase(applyInst);
  }
}

void LoadableByAddress::recreateLoadInstrs() {
  for (auto *loadInstr : loadInstrsOfFunc) {
    SILBuilder loadBuilder(loadInstr);
    // If this is a load of a function for which we changed the return type:
    // add UncheckedBitCast before the load
    auto loadOp = loadInstr->getOperand();
    SILType currSILType = loadOp->getType();
    CanType currCanType = currSILType.getSwiftRValueType();
    SILFunctionType *funcType = dyn_cast<SILFunctionType>(currCanType);
    if (funcType) {
      CanSILFunctionType canFuncType = CanSILFunctionType(funcType);
      GenericEnvironment *genEnv =
          loadInstr->getFunction()->getGenericEnvironment();
      if (!genEnv && canFuncType->isPolymorphic()) {
        genEnv = getGenericEnvironment(loadInstr->getModule(), canFuncType);
      }
      SILType newSILType =
          getNewSILFunctionType(genEnv, funcType, *getIRGenModule());
      newSILType = newSILType.getAddressType(); // we need address for loads
      if (newSILType != currSILType) {
        auto castInstr = loadBuilder.createUncheckedAddrCast(
            loadInstr->getLoc(), loadOp, newSILType);
        loadOp = castInstr;
      }
    }
    auto *newInstr = loadBuilder.createLoad(loadInstr->getLoc(), loadOp,
                                            loadInstr->getOwnershipQualifier());
    loadInstr->replaceAllUsesWith(newInstr);
    loadInstr->getParent()->erase(loadInstr);
  }
}

void LoadableByAddress::recreateUncheckedEnumDataInstrs() {
  for (auto *enumInstr : uncheckedEnumDataOfFunc) {
    SILBuilder enumBuilder(enumInstr);
    SILFunction *F = enumInstr->getFunction();
    CanSILFunctionType funcType = F->getLoweredFunctionType();
    IRGenModule *currIRMod = getIRGenModule()->IRGen.getGenModule(F);
    Lowering::GenericContextScope GenericScope(getModule()->Types,
                                               funcType->getGenericSignature());
    SILType origType = enumInstr->getType();
    GenericEnvironment *genEnv = F->getGenericEnvironment();
    auto loweredTy = F->getLoweredFunctionType();
    if (!genEnv && loweredTy->isPolymorphic()) {
      genEnv = getGenericEnvironment(F->getModule(), loweredTy);
    }
    SILType newType = getNewSILType(genEnv, origType, *currIRMod);
    auto caseTy = enumInstr->getOperand()->getType().getEnumElementType(
        enumInstr->getElement(), F->getModule());
    SILInstruction *newInstr = nullptr;
    if (caseTy != newType) {
      auto *takeEnum = enumBuilder.createUncheckedEnumData(
          enumInstr->getLoc(), enumInstr->getOperand(), enumInstr->getElement(),
          caseTy);
      newInstr = enumBuilder.createUncheckedBitCast(enumInstr->getLoc(),
                                                    takeEnum, newType);
    } else {
      newInstr = enumBuilder.createUncheckedEnumData(
          enumInstr->getLoc(), enumInstr->getOperand(), enumInstr->getElement(),
          newType);
    }
    enumInstr->replaceAllUsesWith(newInstr);
    enumInstr->getParent()->erase(enumInstr);
  }
}

void LoadableByAddress::recreateUncheckedTakeEnumDataAddrInst() {
  for (auto *enumInstr : uncheckedTakeEnumDataAddrOfFunc) {
    SILBuilder enumBuilder(enumInstr);
    SILFunction *F = enumInstr->getFunction();
    CanSILFunctionType funcType = F->getLoweredFunctionType();
    IRGenModule *currIRMod = getIRGenModule()->IRGen.getGenModule(F);
    Lowering::GenericContextScope GenericScope(getModule()->Types,
                                               funcType->getGenericSignature());
    SILType origType = enumInstr->getType();
    GenericEnvironment *genEnv = F->getGenericEnvironment();
    auto loweredTy = F->getLoweredFunctionType();
    if (!genEnv && loweredTy->isPolymorphic()) {
      genEnv = getGenericEnvironment(F->getModule(), loweredTy);
    }
    SILType newType = getNewSILType(genEnv, origType, *currIRMod);
    auto caseTy = enumInstr->getOperand()->getType().getEnumElementType(
        enumInstr->getElement(), F->getModule());
    SILInstruction *newInstr = nullptr;
    if (caseTy != origType.getObjectType()) {
      auto *takeEnum = enumBuilder.createUncheckedTakeEnumDataAddr(
          enumInstr->getLoc(), enumInstr->getOperand(), enumInstr->getElement(),
          caseTy.getAddressType());
      newInstr = enumBuilder.createUncheckedAddrCast(
          enumInstr->getLoc(), takeEnum, newType.getAddressType());
    } else {
      newInstr = enumBuilder.createUncheckedTakeEnumDataAddr(
          enumInstr->getLoc(), enumInstr->getOperand(), enumInstr->getElement(),
          newType.getAddressType());
    }
    enumInstr->replaceAllUsesWith(newInstr);
    enumInstr->getParent()->erase(enumInstr);
  }
}

void LoadableByAddress::fixStoreToBlockStorageInstrs() {
  for (auto *instr : storeToBlockStorageInstrs) {
    auto dest = instr->getDest();
    ProjectBlockStorageInst *destBlock =
        dyn_cast<ProjectBlockStorageInst>(dest);
    assert(destBlock && "Expected Block Storage dest");
    SILType destType = destBlock->getType();
    auto src = instr->getSrc();
    SILType srcType = src->getType();
    if (destType.getObjectType() != srcType) {
      // Add cast to destType
      SILBuilder castBuilder(instr);
      auto *castInstr = castBuilder.createUncheckedBitCast(
          instr->getLoc(), src, destType.getObjectType());
      instr->setOperand(StoreInst::Src, castInstr);
    }
  }
}

void LoadableByAddress::recreateConvInstrs() {
  for (auto *convInstr : conversionInstrs) {
    IRGenModule *currIRMod =
        getIRGenModule()->IRGen.getGenModule(convInstr->getFunction());
    SILType currSILType = convInstr->getType();
    CanType currCanType = currSILType.getSwiftRValueType();
    SILFunctionType *currSILFunctionType =
        dyn_cast<SILFunctionType>(currCanType.getPointer());
    Lowering::GenericContextScope GenericScope(
        getModule()->Types,
        CanSILFunctionType(currSILFunctionType)->getGenericSignature());
    if (!currSILFunctionType) {
      llvm_unreachable("unsupported type");
    }
    GenericEnvironment *genEnv =
        convInstr->getFunction()->getGenericEnvironment();
    auto loweredTy = convInstr->getFunction()->getLoweredFunctionType();
    if (!genEnv && loweredTy->isPolymorphic()) {
      genEnv = getGenericEnvironment(convInstr->getModule(), loweredTy);
    }
    SILType newType =
        getNewSILFunctionType(genEnv, currSILFunctionType, *currIRMod);
    SILBuilder convBuilder(convInstr);
    SILInstruction *newInstr = nullptr;
    switch (convInstr->getKind()) {
    case ValueKind::ThinToThickFunctionInst: {
      ThinToThickFunctionInst *instr =
          dyn_cast<ThinToThickFunctionInst>(convInstr);
      assert(instr && "Unexpected conversion instruction");
      newInstr = convBuilder.createThinToThickFunction(
          instr->getLoc(), instr->getOperand(), newType);
      break;
    }
    case ValueKind::ConvertFunctionInst: {
      auto *instr = dyn_cast<ConvertFunctionInst>(convInstr);
      assert(instr && "Unexpected conversion instruction");
      newInstr = convBuilder.createConvertFunction(
          instr->getLoc(), instr->getOperand(), newType);
      break;
    }
    default:
      llvm_unreachable("Unexpected conversion instruction");
    }
    convInstr->replaceAllUsesWith(newInstr);
    convInstr->getParent()->erase(convInstr);
  }
}

void LoadableByAddress::recreateBuiltinInstrs() {
  for (auto *builtinInstr : builtinInstrs) {
    auto *currIRMod =
        getIRGenModule()->IRGen.getGenModule(builtinInstr->getFunction());
    auto *F = builtinInstr->getFunction();
    GenericEnvironment *genEnv = F->getGenericEnvironment();
    auto loweredTy = F->getLoweredFunctionType();
    if (!genEnv && loweredTy->isPolymorphic()) {
      genEnv = getGenericEnvironment(F->getModule(), loweredTy);
    }
    auto resultTy = builtinInstr->getType();
    auto newResultTy = getNewSILType(genEnv, resultTy, *currIRMod);
    auto newSubs =
        getNewSubs(builtinInstr->getSubstitutions(), currIRMod, genEnv);

    llvm::SmallVector<SILValue, 5> newArgs;
    for (auto oldArg : builtinInstr->getArguments()) {
      newArgs.push_back(oldArg);
    }

    SILBuilder builtinBuilder(builtinInstr);
    auto *newInstr = builtinBuilder.createBuiltin(
        builtinInstr->getLoc(), builtinInstr->getName(), newResultTy, newSubs,
        newArgs);
    builtinInstr->replaceAllUsesWith(newInstr);
    builtinInstr->getParent()->erase(builtinInstr);
  }
}

void LoadableByAddress::updateLoweredTypes(SILFunction *F) {
  IRGenModule *currIRMod = getIRGenModule()->IRGen.getGenModule(F);
  CanSILFunctionType funcType = F->getLoweredFunctionType();
  auto *currSILFunctionType = dyn_cast<SILFunctionType>(funcType.getPointer());
  Lowering::GenericContextScope GenericScope(getModule()->Types,
                                             funcType->getGenericSignature());
  GenericEnvironment *genEnv = F->getGenericEnvironment();
  auto loweredTy = F->getLoweredFunctionType();
  if (!genEnv && loweredTy->isPolymorphic()) {
    genEnv = getGenericEnvironment(F->getModule(), loweredTy);
  }
  auto *newFuncTy =
      getNewSILFunctionTypePtr(genEnv, currSILFunctionType, *currIRMod);

  F->rewriteLoweredTypeUnsafe(CanSILFunctionType(newFuncTy));
}

/// The entry point to this function transformation.
void LoadableByAddress::run() {
  for (auto &F : *getModule())
    runOnFunction(&F);

  if (modFuncs.empty()) {
    getModule()->setStage(SILStage::Lowered);
    return;
  }

  // Scan the module for all references of the modified functions:
  llvm::SetVector<FunctionRefInst *> funcRefs;
  for (SILFunction &CurrF : *getModule()) {
    for (SILBasicBlock &BB : CurrF) {
      for (SILInstruction &I : BB) {
        if (auto *FRI = dyn_cast<FunctionRefInst>(&I)) {
          SILFunction *RefF = FRI->getReferencedFunction();
          if (modFuncs.count(RefF) != 0) {
            // Go over the uses and add them to lists to modify
            for (auto *user : FRI->getUses()) {
              SILInstruction *currInstr = user->getUser();
              switch (currInstr->getKind()) {
              case ValueKind::ApplyInst:
              case ValueKind::TryApplyInst:
              case ValueKind::PartialApplyInst: {
                if (modApplies.count(currInstr) == 0) {
                  modApplies.insert(currInstr);
                }
                break;
              }
              case ValueKind::ThinToThickFunctionInst: {
                conversionInstrs.insert(currInstr);
                break;
              }
              case ValueKind::BuiltinInst: {
                auto *instr = dyn_cast<BuiltinInst>(currInstr);
                builtinInstrs.insert(instr);
                break;
              }
              case ValueKind::DebugValueAddrInst:
              case ValueKind::DebugValueInst: {
                break;
              }
              default:
                llvm_unreachable("Unhandled use of FunctionRefInst");
              }
            }
            funcRefs.insert(FRI);
          }
        } else if (auto *CFI = dyn_cast<ConvertFunctionInst>(&I)) {
          SILValue val = CFI->getConverted();
          SILType currType = val->getType();
          CanType currCanType = currType.getSwiftRValueType();
          auto *fType = dyn_cast<SILFunctionType>(currCanType.getPointer());
          assert(fType && "Expected SILFunctionType");
          if (modifiableFunction(CanSILFunctionType(fType))) {
            conversionInstrs.insert(CFI);
          }
        } else if (auto *LI = dyn_cast<LoadInst>(&I)) {
          SILType currType = LI->getType();
          if (auto *fType = getInnerFunctionType(currType)) {
            if (modifiableFunction(CanSILFunctionType(fType))) {
              // need to re-create these loads: re-write type cache
              loadInstrsOfFunc.insert(LI);
            }
          }
        } else if (auto *UED = dyn_cast<UncheckedEnumDataInst>(&I)) {
          SILType currType = UED->getType();
          if (auto *fType = getInnerFunctionType(currType)) {
            if (modifiableFunction(CanSILFunctionType(fType))) {
              // need to re-create these loads: re-write type cache
              uncheckedEnumDataOfFunc.insert(UED);
            }
          }
        } else if (auto *UED = dyn_cast<UncheckedTakeEnumDataAddrInst>(&I)) {
          SILType currType = UED->getType();
          if (auto *fType = getInnerFunctionType(currType)) {
            if (modifiableFunction(CanSILFunctionType(fType))) {
              // need to re-create these loads: re-write type cache
              uncheckedTakeEnumDataAddrOfFunc.insert(UED);
            }
          }
        } else if (auto *SI = dyn_cast<StoreInst>(&I)) {
          auto dest = SI->getDest();
          if (isa<ProjectBlockStorageInst>(dest)) {
            storeToBlockStorageInstrs.insert(SI);
          }
        }
      }
    }
  }

  for (auto *F : modFuncs) {
    // Update the lowered type of the Function
    updateLoweredTypes(F);
  }

  // Update all references:
  // Note: We don't need to update the witness tables and vtables
  // They just contain a pointer to the function
  // The pointer does not change
  for (FunctionRefInst *instr : funcRefs) {
    SILFunction *F = instr->getReferencedFunction();
    SILBuilder refBuilder(instr);
    FunctionRefInst *newInstr =
        refBuilder.createFunctionRef(instr->getLoc(), F);
    instr->replaceAllUsesWith(newInstr);
    instr->getParent()->erase(instr);
  }

  // Re-create all conversions for which we modified the FunctionRef
  recreateConvInstrs();

  // Re-create all builtins for which we modified the FunctionRef
  recreateBuiltinInstrs();

  // Re-create all unchecked enum data instrs of function pointers
  recreateUncheckedEnumDataInstrs();

  // Same for data addr
  recreateUncheckedTakeEnumDataAddrInst();

  // Re-create all load instrs of function pointers
  recreateLoadInstrs();

  // Re-create all applies that we modified in the module
  recreateApplies();

  // Fix all instructions that rely on block storage type
  fixStoreToBlockStorageInstrs();

  // Set the SIL state before the PassManager has a chance to run
  // verification.
  getModule()->setStage(SILStage::Lowered);

  // Clean up the data structs:
  modFuncs.clear();
  conversionInstrs.clear();
  loadInstrsOfFunc.clear();
  uncheckedEnumDataOfFunc.clear();
  modApplies.clear();
  storeToBlockStorageInstrs.clear();
}

SILTransform *irgen::createLoadableByAddress() {
  return new LoadableByAddress();
}
