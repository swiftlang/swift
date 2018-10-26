//===--- CastOptimizer.cpp ------------------------------------------------===//
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
///
/// \file
///
/// This file contains local cast optimizations and simplifications.
///
//===----------------------------------------------------------------------===//

#include "swift/SILOptimizer/Utils/CastOptimizer.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/Module.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/DynamicCasts.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SIL/TypeLowering.h"
#include "swift/SILOptimizer/Analysis/ARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/Utils/CFG.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Compiler.h"
#include <deque>

using namespace swift;

/// Check if is a bridging cast, i.e. one of the sides is
/// a bridged type.
static bool isBridgingCast(CanType SourceType, CanType TargetType) {
  // Bridging casts cannot be further simplified.
  auto TargetIsBridgeable = TargetType->isBridgeableObjectType();
  auto SourceIsBridgeable = SourceType->isBridgeableObjectType();

  if (TargetIsBridgeable != SourceIsBridgeable)
    return true;

  return false;
}

/// If target is a Swift type bridging to an ObjC type,
/// return the ObjC type it bridges to.
/// If target is an ObjC type, return this type.
static Type getCastFromObjC(SILModule &M, CanType source, CanType target) {
  return M.getASTContext().getBridgedToObjC(M.getSwiftModule(), target);
}

/// Create a call of _forceBridgeFromObjectiveC_bridgeable or
/// _conditionallyBridgeFromObjectiveC_bridgeable which converts an ObjC
/// instance into a corresponding Swift type, conforming to
/// _ObjectiveCBridgeable.
SILInstruction *CastOptimizer::optimizeBridgedObjCToSwiftCast(
    SILInstruction *Inst, bool isConditional, SILValue Src, SILValue Dest,
    CanType Source, CanType Target, Type BridgedSourceTy, Type BridgedTargetTy,
    SILBasicBlock *SuccessBB, SILBasicBlock *FailureBB) {
  auto &M = Inst->getModule();
  auto Loc = Inst->getLoc();

  // The conformance to _BridgedToObjectiveC is statically known.
  // Retrieve the bridging operation to be used if a static conformance
  // to _BridgedToObjectiveC can be proven.
  FuncDecl *BridgeFuncDecl =
      isConditional
          ? M.getASTContext().getConditionallyBridgeFromObjectiveCBridgeable(
                nullptr)
          : M.getASTContext().getForceBridgeFromObjectiveCBridgeable(nullptr);

  assert(BridgeFuncDecl && "_forceBridgeFromObjectiveC should exist");

  SILDeclRef FuncDeclRef(BridgeFuncDecl, SILDeclRef::Kind::Func);

  // Lookup a function from the stdlib.
  SILFunction *BridgedFunc = FunctionBuilder.getOrCreateFunction(
      Loc, FuncDeclRef, ForDefinition_t::NotForDefinition);

  if (!BridgedFunc)
    return nullptr;

  CanType CanBridgedTy = BridgedTargetTy->getCanonicalType();
  SILType SILBridgedTy = SILType::getPrimitiveObjectType(CanBridgedTy);

  SILBuilderWithScope Builder(Inst);
  SILValue SrcOp;
  SILInstruction *NewI = nullptr;

  assert(Src->getType().isAddress() && "Source should have an address type");
  assert(Dest->getType().isAddress() && "Source should have an address type");

  // AnyHashable is a special case - it does not conform to NSObject -
  // If AnyHashable - Bail out of the optimization
  if (auto DT = Target.getNominalOrBoundGenericNominal()) {
    if (DT == M.getASTContext().getAnyHashableDecl()) {
      return nullptr;
    }
  }

  // If this is a conditional cast:
  // We need a new fail BB in order to add a dealloc_stack to it
  SILBasicBlock *ConvFailBB = nullptr;
  if (isConditional) {
    auto CurrInsPoint = Builder.getInsertionPoint();
    ConvFailBB = splitBasicBlockAndBranch(Builder, &(*FailureBB->begin()),
                                          nullptr, nullptr);
    Builder.setInsertionPoint(CurrInsPoint);
  }

  if (SILBridgedTy != Src->getType()) {
    // Check if we can simplify a cast into:
    // - ObjCTy to _ObjectiveCBridgeable._ObjectiveCType.
    // - then convert _ObjectiveCBridgeable._ObjectiveCType to
    // a Swift type using _forceBridgeFromObjectiveC.

    if (!Src->getType().isLoadable(M)) {
      // This code path is never reached in current test cases
      // If reached, we'd have to convert from an ObjC Any* to a loadable type
      // Should use check_addr / make a source we can actually load
      return nullptr;
    }

    // Generate a load for the source argument.
    auto *Load =
        Builder.createLoad(Loc, Src, LoadOwnershipQualifier::Unqualified);
    // Try to convert the source into the expected ObjC type first.

    if (Load->getType() == SILBridgedTy) {
      // If type of the source and the expected ObjC type are
      // equal, there is no need to generate the conversion
      // from ObjCTy to _ObjectiveCBridgeable._ObjectiveCType.
      if (isConditional) {
        SILBasicBlock *CastSuccessBB = Inst->getFunction()->createBasicBlock();
        CastSuccessBB->createPhiArgument(SILBridgedTy,
                                         ValueOwnershipKind::Owned);
        Builder.createBranch(Loc, CastSuccessBB, SILValue(Load));
        Builder.setInsertionPoint(CastSuccessBB);
        SrcOp = CastSuccessBB->getArgument(0);
      } else {
        SrcOp = Load;
      }
    } else if (isConditional) {
      SILBasicBlock *CastSuccessBB = Inst->getFunction()->createBasicBlock();
      CastSuccessBB->createPhiArgument(SILBridgedTy, ValueOwnershipKind::Owned);
      auto *CCBI = Builder.createCheckedCastBranch(Loc, false, Load,
                                      SILBridgedTy, CastSuccessBB, ConvFailBB);
      NewI = CCBI;
      splitEdge(CCBI, /* EdgeIdx to ConvFailBB */ 1);
      Builder.setInsertionPoint(CastSuccessBB);
      SrcOp = CastSuccessBB->getArgument(0);
    } else {
      auto cast =
          Builder.createUnconditionalCheckedCast(Loc, Load, SILBridgedTy);
      NewI = cast;
      SrcOp = cast;
    }
  } else {
    SrcOp = Src;
  }

  // Now emit the a cast from the casted ObjC object into a target type.
  // This is done by means of calling _forceBridgeFromObjectiveC or
  // _conditionallyBridgeFromObjectiveC_bridgeable from the Target type.
  // Lookup the required function in the Target type.

  // Lookup the _ObjectiveCBridgeable protocol.
  auto BridgedProto =
      M.getASTContext().getProtocol(KnownProtocolKind::ObjectiveCBridgeable);
  auto Conf = *M.getSwiftModule()->lookupConformance(Target, BridgedProto);

  auto ParamTypes = BridgedFunc->getLoweredFunctionType()->getParameters();

  auto *FuncRef = Builder.createFunctionRef(Loc, BridgedFunc);

  auto MetaTy = MetatypeType::get(Target, MetatypeRepresentation::Thick);
  auto SILMetaTy = M.Types.getTypeLowering(MetaTy).getLoweredType();
  auto *MetaTyVal = Builder.createMetatype(Loc, SILMetaTy);
  SmallVector<SILValue, 1> Args;

  // Add substitutions
  auto SubMap = SubstitutionMap::getProtocolSubstitutions(Conf.getRequirement(),
                                                          Target, Conf);

  auto SILFnTy = FuncRef->getType();
  SILType SubstFnTy = SILFnTy.substGenericArgs(M, SubMap);
  SILFunctionConventions substConv(SubstFnTy.castTo<SILFunctionType>(), M);

  // Temporary to hold the intermediate result.
  AllocStackInst *Tmp = nullptr;
  CanType OptionalTy;
  SILValue InOutOptionalParam;
  if (isConditional) {
    // Create a temporary
    OptionalTy = OptionalType::get(Dest->getType().getASTType())
                     ->getImplementationType()
                     ->getCanonicalType();
    Tmp = Builder.createAllocStack(Loc,
                                   SILType::getPrimitiveObjectType(OptionalTy));
    InOutOptionalParam = Tmp;
  } else {
    InOutOptionalParam = Dest;
  }

  (void)ParamTypes;
  assert(ParamTypes[0].getConvention() == ParameterConvention::Direct_Guaranteed &&
	 "Parameter should be @guaranteed");

  // Emit a retain.
  Builder.createRetainValue(Loc, SrcOp, Builder.getDefaultAtomicity());

  Args.push_back(InOutOptionalParam);
  Args.push_back(SrcOp);
  Args.push_back(MetaTyVal);

  auto *AI = Builder.createApply(Loc, FuncRef, SubMap, Args, false);

  // If we have guaranteed normal arguments, insert the destroy.
  //
  // TODO: Is it safe to just eliminate the initial retain?
  Builder.createReleaseValue(Loc, SrcOp, Builder.getDefaultAtomicity());

  // If the source of a cast should be destroyed, emit a release.
  if (isa<UnconditionalCheckedCastAddrInst>(Inst)) {
    Builder.createReleaseValue(Loc, SrcOp, Builder.getDefaultAtomicity());
  }

  if (auto *CCABI = dyn_cast<CheckedCastAddrBranchInst>(Inst)) {
    switch (CCABI->getConsumptionKind()) {
    case CastConsumptionKind::TakeAlways:
      Builder.createReleaseValue(Loc, SrcOp, Builder.getDefaultAtomicity());
      break;
    case CastConsumptionKind::TakeOnSuccess:
      // Insert a release in the success BB.
      Builder.setInsertionPoint(SuccessBB->begin());
      Builder.createReleaseValue(Loc, SrcOp, Builder.getDefaultAtomicity());
      break;
    case CastConsumptionKind::BorrowAlways:
      llvm_unreachable("checked_cast_addr_br never has BorrowAlways");
    case CastConsumptionKind::CopyOnSuccess:
      break;
    }
  }

  // Results should be checked in case we process a conditional
  // case. E.g. casts from NSArray into [SwiftType] may fail, i.e. return .None.
  if (isConditional) {
    // Copy the temporary into Dest.
    // Load from the optional.
    auto *SomeDecl = Builder.getASTContext().getOptionalSomeDecl();

    SILBasicBlock *ConvSuccessBB = Inst->getFunction()->createBasicBlock();
    SmallVector<std::pair<EnumElementDecl *, SILBasicBlock *>, 1> CaseBBs;
    CaseBBs.push_back(
        std::make_pair(M.getASTContext().getOptionalNoneDecl(), FailureBB));
    Builder.createSwitchEnumAddr(Loc, InOutOptionalParam, ConvSuccessBB,
                                 CaseBBs);

    Builder.setInsertionPoint(FailureBB->begin());
    Builder.createDeallocStack(Loc, Tmp);

    Builder.setInsertionPoint(ConvSuccessBB);
    auto Addr = Builder.createUncheckedTakeEnumDataAddr(Loc, InOutOptionalParam,
                                                        SomeDecl);

    Builder.createCopyAddr(Loc, Addr, Dest, IsTake, IsInitialization);

    Builder.createDeallocStack(Loc, Tmp);
    SmallVector<SILValue, 1> SuccessBBArgs;
    Builder.createBranch(Loc, SuccessBB, SuccessBBArgs);
  }

  EraseInstAction(Inst);
  return (NewI) ? NewI : AI;
}

static bool canOptimizeCast(const swift::Type &BridgedTargetTy,
                            swift::SILModule &M,
                            swift::SILFunctionConventions &substConv) {
  // DestTy is the type which we want to convert to
  SILType DestTy =
      SILType::getPrimitiveObjectType(BridgedTargetTy->getCanonicalType());
  // ConvTy  is the return type of the _bridgeToObjectiveCImpl()
  auto ConvTy = substConv.getSILResultType().getObjectType();
  if (ConvTy == DestTy) {
    // Destination is the same type
    return true;
  }
  // Check if a superclass/subclass of the source operand
  if (DestTy.isExactSuperclassOf(ConvTy)) {
    return true;
  }
  if (ConvTy.isExactSuperclassOf(DestTy)) {
    return true;
  }
  // check if it is a bridgeable CF type
  if (ConvTy.getASTType() ==
      getNSBridgedClassOfCFClass(M.getSwiftModule(),
                                 DestTy.getASTType())) {
    return true;
  }
  if (DestTy.getASTType() ==
      getNSBridgedClassOfCFClass(M.getSwiftModule(),
                                 ConvTy.getASTType())) {
    return true;
  }
  // All else failed - can't optimize this case
  return false;
}

/// Create a call of _bridgeToObjectiveC which converts an _ObjectiveCBridgeable
/// instance into a bridged ObjC type.
SILInstruction *CastOptimizer::optimizeBridgedSwiftToObjCCast(
    SILInstruction *Inst, CastConsumptionKind ConsumptionKind,
    bool isConditional, SILValue Src, SILValue Dest, CanType Source,
    CanType Target, Type BridgedSourceTy, Type BridgedTargetTy,
    SILBasicBlock *SuccessBB, SILBasicBlock *FailureBB) {

  auto &M = Inst->getModule();
  auto Loc = Inst->getLoc();

  bool AddressOnlyType = false;
  if (!Src->getType().isLoadable(M) || !Dest->getType().isLoadable(M)) {
    AddressOnlyType = true;
  }

  // Find the _BridgedToObjectiveC protocol.
  auto BridgedProto =
      M.getASTContext().getProtocol(KnownProtocolKind::ObjectiveCBridgeable);

  auto Conf = M.getSwiftModule()->lookupConformance(Source, BridgedProto);

  assert(Conf && "_ObjectiveCBridgeable conformance should exist");
  (void)Conf;

  // Generate code to invoke _bridgeToObjectiveC
  SILBuilderWithScope Builder(Inst);

  auto *NTD = Source.getNominalOrBoundGenericNominal();
  assert(NTD);
  SmallVector<ValueDecl *, 4> FoundMembers;
  ArrayRef<ValueDecl *> Members;
  Members = NTD->lookupDirect(M.getASTContext().Id_bridgeToObjectiveC);
  if (Members.empty()) {
    if (NTD->getDeclContext()->lookupQualified(
            NTD, M.getASTContext().Id_bridgeToObjectiveC,
            NLOptions::NL_ProtocolMembers, FoundMembers)) {
      Members = FoundMembers;
      // Returned members are starting with the most specialized ones.
      // Thus, the first element is what we are looking for.
      Members = Members.take_front(1);
    }
  }

  // There should be exactly one implementation of _bridgeToObjectiveC.
  if (Members.size() != 1)
    return nullptr;

  auto BridgeFuncDecl = Members.front();
  auto BridgeFuncDeclRef = SILDeclRef(BridgeFuncDecl);
  ModuleDecl *Mod =
      M.getASTContext().getLoadedModule(M.getASTContext().Id_Foundation);
  if (!Mod)
    return nullptr;
  SmallVector<ValueDecl *, 2> Results;
  Mod->lookupMember(Results, Source.getNominalOrBoundGenericNominal(),
                    M.getASTContext().Id_bridgeToObjectiveC, Identifier());
  ArrayRef<ValueDecl *> ResultsRef(Results);
  if (ResultsRef.empty()) {
    M.getSwiftModule()->lookupMember(
        Results, Source.getNominalOrBoundGenericNominal(),
        M.getASTContext().Id_bridgeToObjectiveC, Identifier());
    ResultsRef = Results;
  }
  if (ResultsRef.size() != 1)
    return nullptr;

  auto *resultDecl = Results.front();
  auto MemberDeclRef = SILDeclRef(resultDecl);
  auto *BridgedFunc = FunctionBuilder.getOrCreateFunction(
      Loc, MemberDeclRef, ForDefinition_t::NotForDefinition);

  // Implementation of _bridgeToObjectiveC could not be found.
  if (!BridgedFunc)
    return nullptr;

  if (Inst->getFunction()->isSerialized() &&
      !BridgedFunc->hasValidLinkageForFragileRef())
    return nullptr;

  auto ParamTypes = BridgedFunc->getLoweredFunctionType()->getParameters();

  auto SILFnTy = SILType::getPrimitiveObjectType(
      M.Types.getConstantFunctionType(BridgeFuncDeclRef));

  // TODO: Handle return from witness function.
  if (BridgedFunc->getLoweredFunctionType()
          ->getSingleResult()
          .isFormalIndirect())
    return nullptr;

  // Get substitutions, if source is a bound generic type.
  auto SubMap = Source->getContextSubstitutionMap(
      M.getSwiftModule(), BridgeFuncDecl->getDeclContext());

  SILType SubstFnTy = SILFnTy.substGenericArgs(M, SubMap);
  SILFunctionConventions substConv(SubstFnTy.castTo<SILFunctionType>(), M);

  // check that we can go through with the optimization
  if (!canOptimizeCast(BridgedTargetTy, M, substConv)) {
    return nullptr;
  }

  auto FnRef = Builder.createFunctionRef(Loc, BridgedFunc);
  if (Src->getType().isAddress() && !substConv.isSILIndirect(ParamTypes[0])) {
    // Create load
    Src = Builder.createLoad(Loc, Src, LoadOwnershipQualifier::Unqualified);
  }

  // Compensate different owning conventions of the replaced cast instruction
  // and the inserted conversion function.
  bool needRetainBeforeCall = false;
  bool needReleaseAfterCall = false;
  bool needReleaseInSuccess = false;
  switch (ParamTypes[0].getConvention()) {
  case ParameterConvention::Direct_Guaranteed:
  case ParameterConvention::Indirect_In_Guaranteed:
    switch (ConsumptionKind) {
    case CastConsumptionKind::TakeAlways:
      needReleaseAfterCall = true;
      break;
    case CastConsumptionKind::TakeOnSuccess:
      needReleaseInSuccess = true;
      break;
    case CastConsumptionKind::BorrowAlways:
    case CastConsumptionKind::CopyOnSuccess:
      // Conservatively insert a retain/release pair around the conversion
      // function because the conversion function could decrement the
      // (global) reference count of the source object.
      //
      // %src = load %global_var
      // apply %conversion_func(@guaranteed %src)
      //
      // sil conversion_func {
      //    %old_value = load %global_var
      //    store %something_else, %global_var
      //    strong_release %old_value
      // }
      needRetainBeforeCall = true;
      needReleaseAfterCall = true;
      break;
    }
    break;
  case ParameterConvention::Direct_Owned:
  case ParameterConvention::Indirect_In:
  case ParameterConvention::Indirect_In_Constant:
    // Currently this
    // cannot appear, because the _bridgeToObjectiveC protocol witness method
    // always receives the this pointer (= the source) as guaranteed.
    // If it became possible (perhaps with the advent of ownership and
    // explicit +1 annotations), the implementation should look something
    // like this:
    /*
    switch (ConsumptionKind) {
      case CastConsumptionKind::TakeAlways:
        break;
      case CastConsumptionKind::TakeOnSuccess:
        needRetainBeforeCall = true;
        needReleaseInSuccess = true;
        break;
      case CastConsumptionKind::CopyOnSuccess:
        needRetainBeforeCall = true;
        break;
    }
    break;
     */
    llvm_unreachable("this should never happen so is currently untestable");
  case ParameterConvention::Direct_Unowned:
    assert(!AddressOnlyType &&
           "AddressOnlyType with Direct_Unowned is not supported");
    break;
  case ParameterConvention::Indirect_Inout:
  case ParameterConvention::Indirect_InoutAliasable:
    // TODO handle remaining indirect argument types
    return nullptr;
  }

  bool needStackAllocatedTemporary = false;
  if (needRetainBeforeCall) {
    if (AddressOnlyType) {
      needStackAllocatedTemporary = true;
      auto NewSrc = Builder.createAllocStack(Loc, Src->getType());
      Builder.createCopyAddr(Loc, Src, NewSrc, IsNotTake, IsInitialization);
      Src = NewSrc;
    } else {
      Builder.createRetainValue(Loc, Src, Builder.getDefaultAtomicity());
    }
  }

  // Generate a code to invoke the bridging function.
  auto *NewAI = Builder.createApply(Loc, FnRef, SubMap, Src, false);

  auto releaseSrc = [&](SILBuilder &Builder) {
    if (AddressOnlyType) {
      Builder.createDestroyAddr(Loc, Src);
    } else {
      Builder.createReleaseValue(Loc, Src, Builder.getDefaultAtomicity());
    }
  };

  Optional<SILBuilder> SuccBuilder;
  if (needReleaseInSuccess || needStackAllocatedTemporary)
    SuccBuilder.emplace(SuccessBB->begin());

  if (needReleaseAfterCall) {
    releaseSrc(Builder);
  } else if (needReleaseInSuccess) {
    if (SuccessBB) {
      releaseSrc(*SuccBuilder);
    } else {
      // For an unconditional cast, success is the only defined path
      releaseSrc(Builder);
    }
  }

  // Pop the temporary stack slot for a copied temporary.
  if (needStackAllocatedTemporary) {
    assert((bool)SuccessBB == (bool)FailureBB);
    if (SuccessBB) {
      SuccBuilder->createDeallocStack(Loc, Src);
      SILBuilder FailBuilder(FailureBB->begin());
      FailBuilder.createDeallocStack(Loc, Src);
    } else {
      Builder.createDeallocStack(Loc, Src);
    }
  }

  SILInstruction *NewI = NewAI;

  if (Dest) {
    // If it is addr cast then store the result.
    auto ConvTy = NewAI->getType();
    auto DestTy = Dest->getType().getObjectType();
    assert(DestTy == SILType::getPrimitiveObjectType(
                         BridgedTargetTy->getCanonicalType()) &&
           "Expected Dest Type to be the same as BridgedTargetTy");
    SILValue CastedValue;
    if (ConvTy == DestTy) {
      CastedValue = NewAI;
    } else if (DestTy.isExactSuperclassOf(ConvTy)) {
      CastedValue = Builder.createUpcast(Loc, NewAI, DestTy);
    } else if (ConvTy.isExactSuperclassOf(DestTy)) {
      // The downcast from a base class to derived class may fail.
      if (isConditional) {
        // In case of a conditional cast, we should handle it gracefully.
        auto CondBrSuccessBB =
            NewAI->getFunction()->createBasicBlockAfter(NewAI->getParent());
        CondBrSuccessBB->createPhiArgument(DestTy, ValueOwnershipKind::Owned,
                                           nullptr);
        Builder.createCheckedCastBranch(Loc, /* isExact*/ false, NewAI, DestTy,
                                        CondBrSuccessBB, FailureBB);
        Builder.setInsertionPoint(CondBrSuccessBB, CondBrSuccessBB->begin());
        CastedValue = CondBrSuccessBB->getArgument(0);
      } else {
        CastedValue = SILValue(
            Builder.createUnconditionalCheckedCast(Loc, NewAI, DestTy));
      }
    } else if (ConvTy.getASTType() ==
                   getNSBridgedClassOfCFClass(M.getSwiftModule(),
                                              DestTy.getASTType()) ||
               DestTy.getASTType() ==
                   getNSBridgedClassOfCFClass(M.getSwiftModule(),
                                              ConvTy.getASTType())) {
      // Handle NS <-> CF toll-free bridging here.
      CastedValue =
          SILValue(Builder.createUncheckedRefCast(Loc, NewAI, DestTy));
    } else {
      llvm_unreachable("optimizeBridgedSwiftToObjCCast: should never reach "
                       "this condition: if the Destination does not have the "
                       "same type, is not a bridgeable CF type and isn't a "
                       "superclass/subclass of the source operand we should "
                       "have bailed earlier");
    }
    NewI = Builder.createStore(Loc, CastedValue, Dest,
                               StoreOwnershipQualifier::Unqualified);
    if (isConditional && NewI->getParent() != NewAI->getParent()) {
      Builder.createBranch(Loc, SuccessBB);
    }
  }

  if (Dest) {
    EraseInstAction(Inst);
  }

  return NewI;
}

/// Make use of the fact that some of these casts cannot fail.
/// For example, if the ObjC type is exactly the expected
/// _ObjectiveCType type, then it would always succeed for
/// NSString, NSNumber, etc.
/// Casts from NSArray, NSDictionary and NSSet may fail.
///
/// If ObjC class is not exactly _ObjectiveCType, then
/// its conversion to a required _ObjectiveCType may fail.
SILInstruction *CastOptimizer::optimizeBridgedCasts(
    SILInstruction *Inst, CastConsumptionKind ConsumptionKind,
    bool isConditional, SILValue Src, SILValue Dest, CanType source,
    CanType target, SILBasicBlock *SuccessBB, SILBasicBlock *FailureBB) {

  auto &M = Inst->getModule();

  // To apply the bridged optimizations, we should ensure that types are not
  // existential (and keep in mind that generic parameters can be existentials),
  // and that one of the types is a class and another one is a struct.
  if (source.isAnyExistentialType() || target.isAnyExistentialType() ||
      source->is<ArchetypeType>() || target->is<ArchetypeType>() ||
      (source.getClassOrBoundGenericClass() &&
       !target.getStructOrBoundGenericStruct()) ||
      (target.getClassOrBoundGenericClass() &&
       !source.getStructOrBoundGenericStruct()))
    return nullptr;

  // Casts involving non-bound generic types cannot be optimized.
  if (source->hasArchetype() || target->hasArchetype())
    return nullptr;

  auto BridgedTargetTy = getCastFromObjC(M, source, target);
  if (!BridgedTargetTy)
    return nullptr;

  auto BridgedSourceTy = getCastFromObjC(M, target, source);
  if (!BridgedSourceTy)
    return nullptr;

  CanType CanBridgedTargetTy = BridgedTargetTy->getCanonicalType();
  CanType CanBridgedSourceTy = BridgedSourceTy->getCanonicalType();

  if (CanBridgedSourceTy == source && CanBridgedTargetTy == target) {
    // Both source and target type are ObjC types.
    return nullptr;
  }

  if (CanBridgedSourceTy != source && CanBridgedTargetTy != target) {
    // Both source and target type are Swift types.
    return nullptr;
  }

  if ((CanBridgedSourceTy && CanBridgedSourceTy->getAnyNominal() ==
                                 M.getASTContext().getNSErrorDecl()) ||
      (CanBridgedTargetTy && CanBridgedSourceTy->getAnyNominal() ==
                                 M.getASTContext().getNSErrorDecl())) {
    // FIXME: Can't optimize bridging with NSError.
    return nullptr;
  }

  if (CanBridgedSourceTy || CanBridgedTargetTy) {
    // Check what kind of conversion it is? ObjC->Swift or Swift-ObjC?
    if (CanBridgedTargetTy != target) {
      // This is an ObjC to Swift cast.
      return optimizeBridgedObjCToSwiftCast(
          Inst, isConditional, Src, Dest, source, target, BridgedSourceTy,
          BridgedTargetTy, SuccessBB, FailureBB);
    } else {
      // This is a Swift to ObjC cast
      return optimizeBridgedSwiftToObjCCast(
          Inst, ConsumptionKind, isConditional, Src, Dest, source, target,
          BridgedSourceTy, BridgedTargetTy, SuccessBB, FailureBB);
    }
  }

  llvm_unreachable("Unknown kind of bridging");
}

SILInstruction *CastOptimizer::simplifyCheckedCastAddrBranchInst(
    CheckedCastAddrBranchInst *Inst) {
  if (auto *I = optimizeCheckedCastAddrBranchInst(Inst))
    Inst = dyn_cast<CheckedCastAddrBranchInst>(I);

  if (!Inst)
    return nullptr;

  auto Loc = Inst->getLoc();
  auto Src = Inst->getSrc();
  auto Dest = Inst->getDest();
  auto SourceType = Inst->getSourceType();
  auto TargetType = Inst->getTargetType();
  auto *SuccessBB = Inst->getSuccessBB();
  auto *FailureBB = Inst->getFailureBB();
  auto &Mod = Inst->getModule();

  SILBuilderWithScope Builder(Inst);

  // Try to determine the outcome of the cast from a known type
  // to a protocol type at compile-time.
  bool isSourceTypeExact = isa<MetatypeInst>(Inst->getSrc());

  // Check if we can statically predict the outcome of the cast.
  auto Feasibility =
      classifyDynamicCast(Mod.getSwiftModule(), SourceType, TargetType,
                          isSourceTypeExact, Mod.isWholeModule());

  if (Feasibility == DynamicCastFeasibility::WillFail) {
    if (shouldDestroyOnFailure(Inst->getConsumptionKind())) {
      auto &srcTL = Builder.getModule().getTypeLowering(Src->getType());
      srcTL.emitDestroyAddress(Builder, Loc, Src);
    }
    auto NewI = Builder.createBranch(Loc, FailureBB);
    EraseInstAction(Inst);
    WillFailAction();
    return NewI;
  }

  bool ResultNotUsed = isa<AllocStackInst>(Dest);
  if (ResultNotUsed) {
    for (auto Use : Dest->getUses()) {
      auto *User = Use->getUser();
      if (isa<DeallocStackInst>(User) || User == Inst)
        continue;
      ResultNotUsed = false;
      break;
    }
  }

  auto *BB = Inst->getParent();

  SILInstruction *BridgedI = nullptr;

  // To apply the bridged optimizations, we should
  // ensure that types are not existential,
  // and that not both types are classes.
  BridgedI = optimizeBridgedCasts(
      Inst, Inst->getConsumptionKind(),
      /* isConditional */ Feasibility == DynamicCastFeasibility::MaySucceed,
      Src, Dest, SourceType, TargetType, SuccessBB, FailureBB);

  if (!BridgedI) {
    // If the cast may succeed or fail, and it can't be optimized into a
    // bridging operation, then let it be.
    if (Feasibility == DynamicCastFeasibility::MaySucceed) {
      return nullptr;
    }

    assert(Feasibility == DynamicCastFeasibility::WillSucceed);

    // Replace by unconditional_addr_cast, followed by a branch.
    // The unconditional_addr_cast can be skipped, if the result of a cast
    // is not used afterwards.
    if (ResultNotUsed) {
      if (shouldTakeOnSuccess(Inst->getConsumptionKind())) {
        auto &srcTL = Builder.getModule().getTypeLowering(Src->getType());
        srcTL.emitDestroyAddress(Builder, Loc, Src);
      }
      EraseInstAction(Inst);
      Builder.setInsertionPoint(BB);
      auto *NewI = Builder.createBranch(Loc, SuccessBB);
      WillSucceedAction();
      return NewI;
    }

    // Since it is an addr cast, only address types are handled here.
    if (!Src->getType().isAddress() || !Dest->getType().isAddress()) {
      return nullptr;
    }

    // For CopyOnSuccess casts, we could insert an explicit copy here, but this
    // case does not happen in practice.
    //
    // Both TakeOnSuccess and TakeAlways can be reduced to an
    // UnconditionalCheckedCast, since the failure path is irrelevant.
    switch (Inst->getConsumptionKind()) {
    case CastConsumptionKind::BorrowAlways:
      llvm_unreachable("checked_cast_addr_br never has BorrowAlways");
    case CastConsumptionKind::CopyOnSuccess:
      return nullptr;
    case CastConsumptionKind::TakeAlways:
    case CastConsumptionKind::TakeOnSuccess:
      break;
    }

    if (!emitSuccessfulIndirectUnconditionalCast(Builder, Mod.getSwiftModule(),
                                                 Loc, Src, SourceType, Dest,
                                                 TargetType, Inst)) {
      // No optimization was possible.
      return nullptr;
    }
    EraseInstAction(Inst);
  }
  SILInstruction *NewI = &BB->back();
  if (!isa<TermInst>(NewI)) {
    Builder.setInsertionPoint(BB);
    NewI = Builder.createBranch(Loc, SuccessBB);
  }
  WillSucceedAction();
  return NewI;
}

SILInstruction *
CastOptimizer::simplifyCheckedCastBranchInst(CheckedCastBranchInst *Inst) {
  if (Inst->isExact()) {
    auto *ARI = dyn_cast<AllocRefInst>(stripUpCasts(Inst->getOperand()));
    if (!ARI)
      return nullptr;

    // We know the dynamic type of the operand.
    SILBuilderWithScope Builder(Inst);
    auto Loc = Inst->getLoc();
    auto *SuccessBB = Inst->getSuccessBB();
    auto *FailureBB = Inst->getFailureBB();

    if (ARI->getType() == Inst->getCastType()) {
      // This exact cast will succeed.
      SmallVector<SILValue, 1> Args;
      Args.push_back(ARI);
      auto *NewI = Builder.createBranch(Loc, SuccessBB, Args);
      EraseInstAction(Inst);
      WillSucceedAction();
      return NewI;
    }

    // This exact cast will fail.
    auto *NewI = Builder.createBranch(Loc, FailureBB);
    EraseInstAction(Inst);
    WillFailAction();
    return NewI;
  }

  if (auto *I = optimizeCheckedCastBranchInst(Inst))
    Inst = dyn_cast<CheckedCastBranchInst>(I);

  if (!Inst)
    return nullptr;

  auto LoweredTargetType = Inst->getCastType();
  auto SourceType = Inst->getSourceType();
  auto TargetType = Inst->getTargetType();
  auto Loc = Inst->getLoc();
  auto *SuccessBB = Inst->getSuccessBB();
  auto *FailureBB = Inst->getFailureBB();
  auto Op = Inst->getOperand();
  auto &Mod = Inst->getModule();
  bool isSourceTypeExact = isa<MetatypeInst>(Op);

  // Check if we can statically predict the outcome of the cast.
  auto Feasibility = classifyDynamicCast(Mod.getSwiftModule(), SourceType,
                                         TargetType, isSourceTypeExact);

  SILBuilderWithScope Builder(Inst);

  if (Feasibility == DynamicCastFeasibility::WillFail) {
    auto *NewI = Builder.createBranch(Loc, FailureBB);
    EraseInstAction(Inst);
    WillFailAction();
    return NewI;
  }

  bool ResultNotUsed = SuccessBB->getArgument(0)->use_empty();
  SILValue CastedValue;
  if (Op->getType() != LoweredTargetType) {
    auto Src = Inst->getOperand();
    auto Dest = SILValue();
    // Apply the bridged cast optimizations.
    // TODO: Bridged casts cannot be expressed by checked_cast_br yet.
    // Should we ever support it, please review this code.
    auto BridgedI = optimizeBridgedCasts(
        Inst, CastConsumptionKind::CopyOnSuccess,
        /* isConditional */ Feasibility == DynamicCastFeasibility::MaySucceed,
        Src, Dest, SourceType, TargetType, nullptr, nullptr);

    if (BridgedI) {
      llvm_unreachable(
          "Bridged casts cannot be expressed by checked_cast_br yet");
    } else {
      // If the cast may succeed or fail and can't be turned into a bridging
      // call, then let it be.
      if (Feasibility == DynamicCastFeasibility::MaySucceed) {
        return nullptr;
      }

      assert(Feasibility == DynamicCastFeasibility::WillSucceed);

      // Replace by unconditional_cast, followed by a branch.
      // The unconditional_cast can be skipped, if the result of a cast
      // is not used afterwards.
      if (!ResultNotUsed) {
        if (!canUseScalarCheckedCastInstructions(Mod, SourceType, TargetType))
          return nullptr;

        CastedValue = emitSuccessfulScalarUnconditionalCast(
            Builder, Mod.getSwiftModule(), Loc, Op, LoweredTargetType,
            SourceType, TargetType, Inst);
      } else {
        CastedValue = SILUndef::get(LoweredTargetType, Mod);
      }
      if (!CastedValue)
        CastedValue =
            Builder.createUnconditionalCheckedCast(Loc, Op, LoweredTargetType);
    }

  } else {
    // No need to cast.
    CastedValue = Op;
  }

  auto *NewI = Builder.createBranch(Loc, SuccessBB, CastedValue);
  EraseInstAction(Inst);
  WillSucceedAction();
  return NewI;
}

SILInstruction *CastOptimizer::simplifyCheckedCastValueBranchInst(
    CheckedCastValueBranchInst *Inst) {
  if (auto *I = optimizeCheckedCastValueBranchInst(Inst))
    Inst = dyn_cast<CheckedCastValueBranchInst>(I);

  if (!Inst)
    return nullptr;

  auto LoweredTargetType = Inst->getCastType();
  auto SourceType = Inst->getSourceType();
  auto TargetType = Inst->getTargetType();
  auto Loc = Inst->getLoc();
  auto *SuccessBB = Inst->getSuccessBB();
  auto *FailureBB = Inst->getFailureBB();
  auto Op = Inst->getOperand();
  auto &Mod = Inst->getModule();
  bool isSourceTypeExact = isa<MetatypeInst>(Op);

  // Check if we can statically predict the outcome of the cast.
  auto Feasibility = classifyDynamicCast(Mod.getSwiftModule(), SourceType,
                                         TargetType, isSourceTypeExact);

  SILBuilderWithScope Builder(Inst);

  if (Feasibility == DynamicCastFeasibility::WillFail) {
    auto *NewI = Builder.createBranch(Loc, FailureBB);
    EraseInstAction(Inst);
    WillFailAction();
    return NewI;
  }

  // Casting will succeed.

  bool ResultNotUsed = SuccessBB->getArgument(0)->use_empty();
  SILValue CastedValue;
  if (Op->getType() != LoweredTargetType) {
    auto Src = Inst->getOperand();
    auto Dest = SILValue();
    // Apply the bridged cast optimizations.
    // TODO: Bridged casts cannot be expressed by checked_cast_value_br yet.
    // Once the support for opaque values has landed, please review this
    // code.
    auto BridgedI = optimizeBridgedCasts(
        Inst, CastConsumptionKind::CopyOnSuccess,
        /* isConditional */ Feasibility == DynamicCastFeasibility::MaySucceed,
        Src, Dest, SourceType, TargetType, nullptr, nullptr);

    if (BridgedI) {
      llvm_unreachable(
          "Bridged casts cannot be expressed by checked_cast_value_br yet");
    } else {
      // If the cast may succeed or fail and can't be turned into a bridging
      // call, then let it be.
      if (Feasibility == DynamicCastFeasibility::MaySucceed) {
        return nullptr;
      }

      assert(Feasibility == DynamicCastFeasibility::WillSucceed);

      // Replace by unconditional_cast, followed by a branch.
      // The unconditional_cast can be skipped, if the result of a cast
      // is not used afterwards.

      if (!canUseScalarCheckedCastInstructions(Mod, SourceType, TargetType))
        return nullptr;

      if (!ResultNotUsed) {
        CastedValue = emitSuccessfulScalarUnconditionalCast(
            Builder, Mod.getSwiftModule(), Loc, Op, LoweredTargetType,
            SourceType, TargetType, Inst);
      } else {
        CastedValue = SILUndef::get(LoweredTargetType, Mod);
      }
    }
    if (!CastedValue)
      CastedValue = Builder.createUnconditionalCheckedCastValue(
          Loc, Op, LoweredTargetType);
  } else {
    // No need to cast.
    CastedValue = Op;
  }

  auto *NewI = Builder.createBranch(Loc, SuccessBB, CastedValue);
  EraseInstAction(Inst);
  WillSucceedAction();
  return NewI;
}

SILInstruction *CastOptimizer::optimizeCheckedCastAddrBranchInst(
    CheckedCastAddrBranchInst *Inst) {
  auto Loc = Inst->getLoc();
  auto Src = Inst->getSrc();
  auto Dest = Inst->getDest();
  auto *SuccessBB = Inst->getSuccessBB();
  auto *FailureBB = Inst->getFailureBB();

  // If there is an unbound generic type involved in the cast, bail.
  if (Src->getType().hasArchetype() || Dest->getType().hasArchetype())
    return nullptr;

  // %1 = metatype $A.Type
  // [%2 = init_existential_metatype %1 ...]
  // %3 = alloc_stack
  // store %1 to %3 or store %2 to %3
  // checked_cast_addr_br %3 to ...
  // ->
  // %1 = metatype $A.Type
  // %c = checked_cast_br %1 to ...
  // store %c to %3 (if successful)
  if (auto *ASI = dyn_cast<AllocStackInst>(Src)) {
    // Check if the value of this alloc_stack is set only once by a store
    // instruction, used only by CCABI and then deallocated.
    bool isLegal = true;
    StoreInst *Store = nullptr;
    for (auto Use : ASI->getUses()) {
      auto *User = Use->getUser();
      if (isa<DeallocStackInst>(User) || User == Inst)
        continue;
      if (auto *SI = dyn_cast<StoreInst>(User)) {
        if (!Store) {
          Store = SI;
          continue;
        }
      }
      isLegal = false;
      break;
    }

    if (isLegal && Store) {
      // Check what was the value stored in the allocated stack slot.
      auto Src = Store->getSrc();
      MetatypeInst *MI = nullptr;
      if (auto *IEMI = dyn_cast<InitExistentialMetatypeInst>(Src)) {
        MI = dyn_cast<MetatypeInst>(IEMI->getOperand());
      }

      if (!MI)
        MI = dyn_cast<MetatypeInst>(Src);

      if (MI) {
        if (SuccessBB->getSinglePredecessorBlock() &&
            canUseScalarCheckedCastInstructions(
                Inst->getModule(), MI->getType().getASTType(),
                Inst->getTargetType())) {
          SILBuilderWithScope B(Inst);
          auto NewI = B.createCheckedCastBranch(
              Loc, false /*isExact*/, MI, Dest->getType().getObjectType(),
              SuccessBB, FailureBB, Inst->getTrueBBCount(),
              Inst->getFalseBBCount());
          SuccessBB->createPhiArgument(Dest->getType().getObjectType(),
                                       ValueOwnershipKind::Owned);
          B.setInsertionPoint(SuccessBB->begin());
          // Store the result
          B.createStore(Loc, SuccessBB->getArgument(0), Dest,
                        StoreOwnershipQualifier::Unqualified);
          EraseInstAction(Inst);
          return NewI;
        }
      }
    }
  }
  return nullptr;
}

SILInstruction *CastOptimizer::optimizeCheckedCastValueBranchInst(
    CheckedCastValueBranchInst *Inst) {
  // TODO
  return nullptr;
}

SILInstruction *
CastOptimizer::optimizeCheckedCastBranchInst(CheckedCastBranchInst *Inst) {
  if (Inst->isExact())
    return nullptr;

  auto LoweredTargetType = Inst->getCastType();
  auto Loc = Inst->getLoc();
  auto *SuccessBB = Inst->getSuccessBB();
  auto *FailureBB = Inst->getFailureBB();
  auto Op = Inst->getOperand();

  // Try to simplify checked_cond_br instructions using existential
  // metatypes by propagating a concrete type whenever it can be
  // determined statically.

  // %0 = metatype $A.Type
  // %1 = init_existential_metatype ..., %0: $A
  // checked_cast_br %1, ....
  // ->
  // %0 = metatype $A.Type
  // checked_cast_br %0 to ...
  if (auto *IEMI = dyn_cast<InitExistentialMetatypeInst>(Op)) {
    if (auto *MI = dyn_cast<MetatypeInst>(IEMI->getOperand())) {
      SILBuilderWithScope B(Inst);
      auto *NewI = B.createCheckedCastBranch(
          Loc, /* isExact */ false, MI, LoweredTargetType, SuccessBB, FailureBB,
          Inst->getTrueBBCount(), Inst->getFalseBBCount());
      EraseInstAction(Inst);
      return NewI;
    }
  }

  if (auto *EMI = dyn_cast<ExistentialMetatypeInst>(Op)) {
    // Operand of the existential_metatype instruction.
    auto Op = EMI->getOperand();
    auto EmiTy = EMI->getType();

    // %0 = alloc_stack $T
    // %1 = init_existential_addr %0: $*T, $A
    // %2 = existential_metatype $T.Type, %0: $*T
    // checked_cast_br %2 to ...
    // ->
    // %1 = metatype $A.Type
    // checked_cast_br %1 to ...

    if (auto *ASI = dyn_cast<AllocStackInst>(Op)) {
      // Should be in the same BB.
      if (ASI->getParent() != EMI->getParent())
        return nullptr;
      // Check if this alloc_stack is only initialized once by means of
      // single init_existential_addr.
      bool isLegal = true;
      // init_existential instruction used to initialize this alloc_stack.
      InitExistentialAddrInst *FoundIEI = nullptr;
      for (auto Use : getNonDebugUses(ASI)) {
        auto *User = Use->getUser();
        if (isa<ExistentialMetatypeInst>(User) || isa<DestroyAddrInst>(User) ||
            isa<DeallocStackInst>(User))
          continue;
        if (auto *IEI = dyn_cast<InitExistentialAddrInst>(User)) {
          if (!FoundIEI) {
            FoundIEI = IEI;
            continue;
          }
        }
        isLegal = false;
        break;
      }

      if (isLegal && FoundIEI) {
        // Should be in the same BB.
        if (FoundIEI->getParent() != EMI->getParent())
          return nullptr;
        // Get the type used to initialize the existential.
        auto LoweredConcreteTy = FoundIEI->getLoweredConcreteType();
        // We don't know enough at compile time about existential
        // and generic type parameters.
        if (LoweredConcreteTy.isAnyExistentialType() ||
            LoweredConcreteTy.is<ArchetypeType>())
          return nullptr;
        // Get the metatype of this type.
        auto EMT = EmiTy.castTo<AnyMetatypeType>();
        auto *MetaTy = MetatypeType::get(LoweredConcreteTy.getASTType(),
                                         EMT->getRepresentation());
        auto CanMetaTy = CanTypeWrapper<MetatypeType>(MetaTy);
        auto SILMetaTy = SILType::getPrimitiveObjectType(CanMetaTy);
        SILBuilderWithScope B(Inst);
        B.getOpenedArchetypes().addOpenedArchetypeOperands(
            FoundIEI->getTypeDependentOperands());
        auto *MI = B.createMetatype(FoundIEI->getLoc(), SILMetaTy);

        auto *NewI = B.createCheckedCastBranch(
            Loc, /* isExact */ false, MI, LoweredTargetType, SuccessBB,
            FailureBB, Inst->getTrueBBCount(), Inst->getFalseBBCount());
        EraseInstAction(Inst);
        return NewI;
      }
    }

    // %0 = alloc_ref $A
    // %1 = init_existential_ref %0: $A, $...
    // %2 = existential_metatype ..., %1 :  ...
    // checked_cast_br %2, ....
    // ->
    // %1 = metatype $A.Type
    // checked_cast_br %1, ....
    if (auto *FoundIERI = dyn_cast<InitExistentialRefInst>(Op)) {
      auto *ASRI = dyn_cast<AllocRefInst>(FoundIERI->getOperand());
      if (!ASRI)
        return nullptr;
      // Should be in the same BB.
      if (ASRI->getParent() != EMI->getParent())
        return nullptr;
      // Check if this alloc_stack is only initialized once by means of
      // a single init_existential_ref.
      bool isLegal = true;
      for (auto Use : getNonDebugUses(ASRI)) {
        auto *User = Use->getUser();
        if (isa<ExistentialMetatypeInst>(User) || isa<StrongReleaseInst>(User))
          continue;
        if (auto *IERI = dyn_cast<InitExistentialRefInst>(User)) {
          if (IERI == FoundIERI) {
            continue;
          }
        }
        isLegal = false;
        break;
      }

      if (isLegal && FoundIERI) {
        // Should be in the same BB.
        if (FoundIERI->getParent() != EMI->getParent())
          return nullptr;
        // Get the type used to initialize the existential.
        auto ConcreteTy = FoundIERI->getFormalConcreteType();
        // We don't know enough at compile time about existential
        // and generic type parameters.
        if (ConcreteTy.isAnyExistentialType() ||
            ConcreteTy->is<ArchetypeType>())
          return nullptr;
        // Get the SIL metatype of this type.
        auto EMT = EMI->getType().castTo<AnyMetatypeType>();
        auto *MetaTy = MetatypeType::get(ConcreteTy, EMT->getRepresentation());
        auto CanMetaTy = CanTypeWrapper<MetatypeType>(MetaTy);
        auto SILMetaTy = SILType::getPrimitiveObjectType(CanMetaTy);
        SILBuilderWithScope B(Inst);
        B.getOpenedArchetypes().addOpenedArchetypeOperands(
            FoundIERI->getTypeDependentOperands());
        auto *MI = B.createMetatype(FoundIERI->getLoc(), SILMetaTy);

        auto *NewI = B.createCheckedCastBranch(
            Loc, /* isExact */ false, MI, LoweredTargetType, SuccessBB,
            FailureBB, Inst->getTrueBBCount(), Inst->getFalseBBCount());
        EraseInstAction(Inst);
        return NewI;
      }
    }
  }

  return nullptr;
}

ValueBase *CastOptimizer::optimizeUnconditionalCheckedCastInst(
    UnconditionalCheckedCastInst *Inst) {
  auto LoweredSourceType = Inst->getOperand()->getType();
  auto LoweredTargetType = Inst->getType();
  auto Loc = Inst->getLoc();
  auto Op = Inst->getOperand();
  auto &Mod = Inst->getModule();

  bool isSourceTypeExact = isa<MetatypeInst>(Op);

  // Check if we can statically predict the outcome of the cast.
  auto Feasibility =
      classifyDynamicCast(Mod.getSwiftModule(), Inst->getSourceType(),
                          Inst->getTargetType(), isSourceTypeExact);

  if (Feasibility == DynamicCastFeasibility::WillFail) {
    // Remove the cast and insert a trap, followed by an
    // unreachable instruction.
    SILBuilderWithScope Builder(Inst);
    auto *Trap = Builder.createBuiltinTrap(Loc);
    Inst->replaceAllUsesWithUndef();
    EraseInstAction(Inst);
    Builder.setInsertionPoint(std::next(SILBasicBlock::iterator(Trap)));
    auto *UnreachableInst =
        Builder.createUnreachable(ArtificialUnreachableLocation());

    // Delete everything after the unreachable except for dealloc_stack which we
    // move before the trap.
    deleteInstructionsAfterUnreachable(UnreachableInst, Trap);

    WillFailAction();
    return Trap;
  }

  if (Feasibility == DynamicCastFeasibility::WillSucceed) {

    if (Inst->use_empty()) {
      EraseInstAction(Inst);
      WillSucceedAction();
      return nullptr;
    }
  }

  SILBuilderWithScope Builder(Inst);

  // Try to apply the bridged casts optimizations
  auto SourceType = LoweredSourceType.getASTType();
  auto TargetType = LoweredTargetType.getASTType();
  auto Src = Inst->getOperand();
  auto NewI = optimizeBridgedCasts(Inst, CastConsumptionKind::CopyOnSuccess,
                                   false, Src, SILValue(), SourceType,
                                   TargetType, nullptr, nullptr);
  if (NewI) {
    // FIXME: I'm not sure why this is true!
    auto newValue = cast<SingleValueInstruction>(NewI);
    ReplaceInstUsesAction(Inst, newValue);
    EraseInstAction(Inst);
    WillSucceedAction();
    return newValue;
  }

  // If the cast may succeed or fail and can't be optimized into a bridging
  // call, let it be.
  if (Feasibility == DynamicCastFeasibility::MaySucceed) {
    return nullptr;
  }

  assert(Feasibility == DynamicCastFeasibility::WillSucceed);

  if (isBridgingCast(SourceType, TargetType))
    return nullptr;

  auto Result = emitSuccessfulScalarUnconditionalCast(
      Builder, Mod.getSwiftModule(), Loc, Op, LoweredTargetType,
      LoweredSourceType.getASTType(),
      LoweredTargetType.getASTType(), Inst);

  if (!Result) {
    // No optimization was possible.
    return nullptr;
  }

  ReplaceInstUsesAction(Inst, Result);
  EraseInstAction(Inst);
  WillSucceedAction();
  return Result;
}

/// Deletes all instructions after \p UnreachableInst except dealloc_stack
/// instructions are moved before \p TrapInst.
void CastOptimizer::deleteInstructionsAfterUnreachable(
    SILInstruction *UnreachableInst, SILInstruction *TrapInst) {
  auto UnreachableInstIt = std::next(SILBasicBlock::iterator(UnreachableInst));
  auto *Block = TrapInst->getParent();
  while (UnreachableInstIt != Block->end()) {
    SILInstruction *CurInst = &*UnreachableInstIt;
    ++UnreachableInstIt;
    if (auto *DeallocStack = dyn_cast<DeallocStackInst>(CurInst))
      if (!isa<SILUndef>(DeallocStack->getOperand())) {
        DeallocStack->moveBefore(TrapInst);
        continue;
      }
    CurInst->replaceAllUsesOfAllResultsWithUndef();
    EraseInstAction(CurInst);
  }
}

/// TODO: Move to emitSuccessfulIndirectUnconditionalCast?
///
/// Peephole to avoid runtime calls:
/// unconditional_checked_cast_addr T in %0 : $*T to P in %1 : $*P
/// ->
/// %addr = init_existential_addr %1 : $*P, T
/// copy_addr %0 to %addr
///
/// where T is a type statically known to conform to P.
///
/// In caase P is a class existential type, it generates:
/// %val = load %0 : $*T
/// %existential = init_existential_ref %val : $T, $T, P
/// store %existential to %1 : $*P
///
/// Returns true if the optimization was possible and false otherwise.
static bool optimizeStaticallyKnownProtocolConformance(
    UnconditionalCheckedCastAddrInst *Inst) {
  auto Loc = Inst->getLoc();
  auto Src = Inst->getSrc();
  auto Dest = Inst->getDest();
  auto SourceType = Inst->getSourceType();
  auto TargetType = Inst->getTargetType();
  auto &Mod = Inst->getModule();

  if (TargetType->isAnyExistentialType() &&
      !SourceType->isAnyExistentialType()) {
    auto &Ctx = Mod.getASTContext();
    auto *SM = Mod.getSwiftModule();

    auto Proto = dyn_cast<ProtocolDecl>(TargetType->getAnyNominal());
    if (Proto) {
      auto Conformance = SM->lookupConformance(SourceType, Proto);
      if (Conformance.hasValue() &&
          Conformance->getConditionalRequirements().empty()) {
        // SourceType is a non-existential type with a non-conditional
        // conformance to a protocol represented by the TargetType.
        //
        // Conditional conformances are complicated: they may depend on
        // information not known until runtime. For instance, if `X: P` where `T
        // == Int` in `func foo<T>(_: T) { ... X<T>() as? P ... }`, the cast
        // will succeed for `foo(0)` but not for `foo("string")`. There are many
        // cases where everything is completely static (`X<Int>() as? P`), but
        // we don't try to handle that at the moment.
        SILBuilder B(Inst);
        SmallVector<ProtocolConformanceRef, 1> NewConformances;
        NewConformances.push_back(Conformance.getValue());
        ArrayRef<ProtocolConformanceRef> Conformances =
            Ctx.AllocateCopy(NewConformances);

        auto ExistentialRepr =
            Dest->getType().getPreferredExistentialRepresentation(Mod,
                                                                  SourceType);

        switch (ExistentialRepr) {
        default:
          return false;
        case ExistentialRepresentation::Opaque: {
          auto ExistentialAddr = B.createInitExistentialAddr(
              Loc, Dest, SourceType, Src->getType().getObjectType(),
              Conformances);
          B.createCopyAddr(Loc, Src, ExistentialAddr, IsTake_t::IsTake,
                           IsInitialization_t::IsInitialization);
          break;
        }
        case ExistentialRepresentation::Class: {
          auto Value = B.createLoad(Loc, Src,
                                    swift::LoadOwnershipQualifier::Unqualified);
          auto Existential =
              B.createInitExistentialRef(Loc, Dest->getType().getObjectType(),
                                         SourceType, Value, Conformances);
          B.createStore(Loc, Existential, Dest,
                        swift::StoreOwnershipQualifier::Unqualified);
          break;
        }
        case ExistentialRepresentation::Boxed: {
          auto AllocBox = B.createAllocExistentialBox(Loc, Dest->getType(),
                                                      SourceType, Conformances);
          auto Projection =
              B.createProjectExistentialBox(Loc, Src->getType(), AllocBox);
          // This needs to be a copy_addr (for now) because we must handle
          // address-only types.
          B.createCopyAddr(Loc, Src, Projection, IsTake, IsInitialization);
          B.createStore(Loc, AllocBox, Dest,
                        swift::StoreOwnershipQualifier::Unqualified);
          break;
        }
        };

        return true;
      }
    }
  }
  return false;
}

SILInstruction *CastOptimizer::optimizeUnconditionalCheckedCastAddrInst(
    UnconditionalCheckedCastAddrInst *Inst) {
  auto Loc = Inst->getLoc();
  auto Src = Inst->getSrc();
  auto Dest = Inst->getDest();
  auto SourceType = Inst->getSourceType();
  auto TargetType = Inst->getTargetType();
  auto &Mod = Inst->getModule();

  bool isSourceTypeExact = isa<MetatypeInst>(Src);

  // Check if we can statically predict the outcome of the cast.
  auto Feasibility = classifyDynamicCast(Mod.getSwiftModule(), SourceType,
                                         TargetType, isSourceTypeExact);

  if (Feasibility == DynamicCastFeasibility::MaySucceed) {
    // Forced bridged casts can be still simplified here.
    // If they fail, they fail inside the conversion function.
    if (!isBridgingCast(SourceType, TargetType))
      return nullptr;
  }

  if (Feasibility == DynamicCastFeasibility::WillFail) {
    // Remove the cast and insert a trap, followed by an
    // unreachable instruction.
    SILBuilderWithScope Builder(Inst);
    // mem2reg's invariants get unhappy if we don't try to
    // initialize a loadable result.
    auto DestType = Dest->getType();
    auto &resultTL = Mod.Types.getTypeLowering(DestType);
    if (!resultTL.isAddressOnly()) {
      auto undef = SILValue(
          SILUndef::get(DestType.getObjectType(), Builder.getModule()));
      Builder.createStore(Loc, undef, Dest,
                          StoreOwnershipQualifier::Unqualified);
    }
    auto *TrapI = Builder.createBuiltinTrap(Loc);
    EraseInstAction(Inst);
    Builder.setInsertionPoint(std::next(SILBasicBlock::iterator(TrapI)));
    auto *UnreachableInst =
        Builder.createUnreachable(ArtificialUnreachableLocation());

    // Delete everything after the unreachable except for dealloc_stack which we
    // move before the trap.
    deleteInstructionsAfterUnreachable(UnreachableInst, TrapI);

    WillFailAction();
  }

  if (Feasibility == DynamicCastFeasibility::WillSucceed ||
      Feasibility == DynamicCastFeasibility::MaySucceed) {

    // Check if a result of a cast is unused. If this is the case, the cast can
    // be removed even if the cast may fail at runtime.
    // Swift optimizer does not claim to be crash-preserving.
    bool ResultNotUsed = isa<AllocStackInst>(Dest);
    DestroyAddrInst *DestroyDestInst = nullptr;
    if (ResultNotUsed) {
      for (auto Use : Dest->getUses()) {
        auto *User = Use->getUser();
        if (isa<DeallocStackInst>(User) || User == Inst)
          continue;
        if (isa<DestroyAddrInst>(User) && !DestroyDestInst) {
          DestroyDestInst = cast<DestroyAddrInst>(User);
          continue;
        }
        ResultNotUsed = false;
        DestroyDestInst = nullptr;
        break;
      }
    }

    if (ResultNotUsed) {
      SILBuilderWithScope B(Inst);
      B.createDestroyAddr(Inst->getLoc(), Inst->getSrc());
      if (DestroyDestInst)
        EraseInstAction(DestroyDestInst);
      EraseInstAction(Inst);
      WillSucceedAction();
      return nullptr;
    }

    // Try to apply the bridged casts optimizations.
    auto NewI =
        optimizeBridgedCasts(Inst, CastConsumptionKind::TakeAlways, false, Src,
                             Dest, SourceType, TargetType, nullptr, nullptr);
    if (NewI) {
      WillSucceedAction();
      return nullptr;
    }

    if (Feasibility == DynamicCastFeasibility::MaySucceed)
      return nullptr;

    assert(Feasibility == DynamicCastFeasibility::WillSucceed);

    if (optimizeStaticallyKnownProtocolConformance(Inst)) {
      EraseInstAction(Inst);
      WillSucceedAction();
      return nullptr;
    }

    if (isBridgingCast(SourceType, TargetType))
      return nullptr;

    SILBuilderWithScope Builder(Inst);
    if (!emitSuccessfulIndirectUnconditionalCast(Builder, Mod.getSwiftModule(),
                                                 Loc, Src, SourceType, Dest,
                                                 TargetType, Inst)) {
      // No optimization was possible.
      return nullptr;
    }

    EraseInstAction(Inst);
    WillSucceedAction();
  }

  return nullptr;
}
