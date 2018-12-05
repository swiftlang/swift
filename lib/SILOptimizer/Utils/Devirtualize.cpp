//===--- Devirtualize.cpp - Helper for devirtualizing apply ---------------===//
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

#define DEBUG_TYPE "sil-devirtualize-utility"
#include "swift/SILOptimizer/Analysis/ClassHierarchyAnalysis.h"
#include "swift/SILOptimizer/Utils/Devirtualize.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/Types.h"
#include "swift/SIL/OptimizationRemark.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Casting.h"
using namespace swift;

STATISTIC(NumClassDevirt, "Number of class_method applies devirtualized");
STATISTIC(NumWitnessDevirt, "Number of witness_method applies devirtualized");

//===----------------------------------------------------------------------===//
//                         Class Method Optimization
//===----------------------------------------------------------------------===//

void swift::getAllSubclasses(ClassHierarchyAnalysis *CHA,
                             ClassDecl *CD,
                             SILType ClassType,
                             SILModule &M,
                             ClassHierarchyAnalysis::ClassList &Subs) {
  // Collect the direct and indirect subclasses for the class.
  // Sort these subclasses in the order they should be tested by the
  // speculative devirtualization. Different strategies could be used,
  // E.g. breadth-first, depth-first, etc.
  // Currently, let's use the breadth-first strategy.
  // The exact static type of the instance should be tested first.
  auto &DirectSubs = CHA->getDirectSubClasses(CD);
  auto &IndirectSubs = CHA->getIndirectSubClasses(CD);

  Subs.append(DirectSubs.begin(), DirectSubs.end());
  Subs.append(IndirectSubs.begin(), IndirectSubs.end());

  if (ClassType.is<BoundGenericClassType>()) {
    // Filter out any subclasses that do not inherit from this
    // specific bound class.
    auto RemovedIt = std::remove_if(Subs.begin(), Subs.end(),
        [&ClassType](ClassDecl *Sub){
          // FIXME: Add support for generic subclasses.
          if (Sub->isGenericContext())
            return false;
          auto SubCanTy = Sub->getDeclaredInterfaceType()->getCanonicalType();
          // Handle the usual case here: the class in question
          // should be a real subclass of a bound generic class.
          return !ClassType.isBindableToSuperclassOf(
              SILType::getPrimitiveObjectType(SubCanTy));
        });
    Subs.erase(RemovedIt, Subs.end());
  }
}

/// Returns true, if a method implementation corresponding to
/// the class_method applied to an instance of the class CD is
/// effectively final, i.e. it is statically known to be not overridden
/// by any subclasses of the class CD.
///
/// \p AI  invocation instruction
/// \p ClassType type of the instance
/// \p CD  static class of the instance whose method is being invoked
/// \p CHA class hierarchy analysis
static bool isEffectivelyFinalMethod(FullApplySite AI,
                                     SILType ClassType,
                                     ClassDecl *CD,
                                     ClassHierarchyAnalysis *CHA) {
  if (CD && CD->isFinal())
    return true;

  const DeclContext *DC = AI.getModule().getAssociatedContext();

  // Without an associated context we cannot perform any
  // access-based optimizations.
  if (!DC)
    return false;

  auto *CMI = cast<MethodInst>(AI.getCallee());

  if (!calleesAreStaticallyKnowable(AI.getModule(), CMI->getMember()))
    return false;

  auto *Method = CMI->getMember().getAbstractFunctionDecl();
  assert(Method && "Expected abstract function decl!");
  assert(!Method->isFinal() && "Unexpected indirect call to final method!");

  // If this method is not overridden in the module,
  // there is no other implementation.
  if (!Method->isOverridden())
    return true;

  // Class declaration may be nullptr, e.g. for cases like:
  // func foo<C:Base>(c: C) {}, where C is a class, but
  // it does not have a class decl.
  if (!CD)
    return false;

  if (!CHA)
    return false;

  // This is a private or a module internal class.
  //
  // We can analyze the class hierarchy rooted at it and
  // eventually devirtualize a method call more efficiently.

  ClassHierarchyAnalysis::ClassList Subs;
  getAllSubclasses(CHA, CD, ClassType, AI.getModule(), Subs);

  // This is the implementation of the method to be used
  // if the exact class of the instance would be CD.
  auto *ImplMethod = CD->findImplementingMethod(Method);

  // First, analyze all direct subclasses.
  for (auto S : Subs) {
    // Check if the subclass overrides a method and provides
    // a different implementation.
    auto *ImplFD = S->findImplementingMethod(Method);
    if (ImplFD != ImplMethod)
      return false;
  }

  return true;
}

/// Check if a given class is final in terms of a current
/// compilation, i.e.:
/// - it is really final
/// - or it is private and has not sub-classes
/// - or it is an internal class without sub-classes and
///   it is a whole-module compilation.
static bool isKnownFinalClass(ClassDecl *CD, SILModule &M,
                              ClassHierarchyAnalysis *CHA) {
  const DeclContext *DC = M.getAssociatedContext();

  if (CD->isFinal())
    return true;

  // Without an associated context we cannot perform any
  // access-based optimizations.
  if (!DC)
    return false;

  // Only handle classes defined within the SILModule's associated context.
  if (!CD->isChildContextOf(DC))
    return false;

  if (!CD->hasAccess())
    return false;

  // Only consider 'private' members, unless we are in whole-module compilation.
  switch (CD->getEffectiveAccess()) {
  case AccessLevel::Open:
    return false;
  case AccessLevel::Public:
  case AccessLevel::Internal:
    if (!M.isWholeModule())
      return false;
    break;
  case AccessLevel::FilePrivate:
  case AccessLevel::Private:
    break;
  }

  // Take the ClassHierarchyAnalysis into account.
  // If a given class has no subclasses and
  // - private
  // - or internal and it is a WMO compilation
  // then this class can be considered final for the purpose
  // of devirtualization.
  if (CHA) {
    if (!CHA->hasKnownDirectSubclasses(CD)) {
      switch (CD->getEffectiveAccess()) {
      case AccessLevel::Open:
        return false;
      case AccessLevel::Public:
      case AccessLevel::Internal:
        if (!M.isWholeModule())
          return false;
        break;
      case AccessLevel::FilePrivate:
      case AccessLevel::Private:
        break;
      }

      return true;
    }
  }

  return false;
}


// Attempt to get the instance for S, whose static type is the same as
// its exact dynamic type, returning a null SILValue() if we cannot find it.
// The information that a static type is the same as the exact dynamic,
// can be derived e.g.:
// - from a constructor or
// - from a successful outcome of a checked_cast_br [exact] instruction.
SILValue swift::getInstanceWithExactDynamicType(SILValue S, SILModule &M,
                                                ClassHierarchyAnalysis *CHA) {

  while (S) {
    S = stripCasts(S);

    if (isa<AllocRefInst>(S) || isa<MetatypeInst>(S)) {
      if (S->getType().getASTType()->hasDynamicSelfType())
        return SILValue();
      return S;
    }

    auto *Arg = dyn_cast<SILArgument>(S);
    if (!Arg)
      break;

    auto *SinglePred = Arg->getParent()->getSinglePredecessorBlock();
    if (!SinglePred) {
      if (!isa<SILFunctionArgument>(Arg))
        break;
      auto *CD = Arg->getType().getClassOrBoundGenericClass();
      // Check if this class is effectively final.
      if (!CD || !isKnownFinalClass(CD, M, CHA))
        break;
      return Arg;
    }

    // Traverse the chain of predecessors.
    if (isa<BranchInst>(SinglePred->getTerminator()) ||
        isa<CondBranchInst>(SinglePred->getTerminator())) {
      S = cast<SILPhiArgument>(Arg)->getIncomingPhiValue(SinglePred);
      continue;
    }

    // If it is a BB argument received on a success branch
    // of a checked_cast_br, then we know its exact type.
    auto *CCBI = dyn_cast<CheckedCastBranchInst>(SinglePred->getTerminator());
    if (!CCBI)
      break;
    if (!CCBI->isExact() || CCBI->getSuccessBB() != Arg->getParent())
      break;
    return S;
  }

  return SILValue();
}

/// Try to determine the exact dynamic type of an object.
/// returns the exact dynamic type of the object, or an empty type if the exact
/// type could not be determined.
SILType swift::getExactDynamicType(SILValue S, SILModule &M,
                                   ClassHierarchyAnalysis *CHA,
                                   bool ForUnderlyingObject) {
  // Set of values to be checked for their exact types.
  SmallVector<SILValue, 8> WorkList;
  // The detected type of the underlying object.
  SILType ResultType;
  // Set of processed values.
  llvm::SmallSet<SILValue, 8> Processed;
  WorkList.push_back(S);

  while (!WorkList.empty()) {
    auto V = WorkList.pop_back_val();
    if (!V)
      return SILType();
    if (Processed.count(V))
      continue;
    Processed.insert(V);
    // For underlying object strip casts and projections.
    // For the object itself, simply strip casts.
    V = ForUnderlyingObject ? getUnderlyingObject(V) : stripCasts(V);

    if (isa<AllocRefInst>(V) || isa<MetatypeInst>(V)) {
      if (ResultType && ResultType != V->getType())
        return SILType();
      ResultType = V->getType();
      continue;
    }

    if (isa<LiteralInst>(V)) {
      if (ResultType && ResultType != V->getType())
        return SILType();
      ResultType = V->getType();
      continue;
    }

    if (isa<StructInst>(V) || isa<TupleInst>(V) || isa<EnumInst>(V)) {
      if (ResultType && ResultType != V->getType())
        return SILType();
      ResultType = V->getType();
      continue;
    }

    if (ForUnderlyingObject) {
      if (isa<AllocationInst>(V)) {
        if (ResultType && ResultType != V->getType())
          return SILType();
        ResultType = V->getType();
        continue;
      }
    }

    auto Arg = dyn_cast<SILArgument>(V);
    if (!Arg) {
      // We don't know what it is.
      return SILType();
    }

    if (auto *FArg = dyn_cast<SILFunctionArgument>(Arg)) {
      // Bail on metatypes for now.
      if (FArg->getType().is<AnyMetatypeType>()) {
        return SILType();
      }
      auto *CD = FArg->getType().getClassOrBoundGenericClass();
      // If it is not class and it is a trivial type, then it
      // should be the exact type.
      if (!CD && FArg->getType().isTrivial(M)) {
        if (ResultType && ResultType != FArg->getType())
          return SILType();
        ResultType = FArg->getType();
        continue;
      }

      if (!CD) {
        // It is not a class or a trivial type, so we don't know what it is.
        return SILType();
      }

      // Check if this class is effectively final.
      if (!isKnownFinalClass(CD, M, CHA)) {
        return SILType();
      }

      if (ResultType && ResultType != FArg->getType())
        return SILType();
      ResultType = FArg->getType();
      continue;
    }

    auto *SinglePred = Arg->getParent()->getSinglePredecessorBlock();
    if (SinglePred) {
      // If it is a BB argument received on a success branch
      // of a checked_cast_br, then we know its exact type.
      auto *CCBI = dyn_cast<CheckedCastBranchInst>(SinglePred->getTerminator());
      if (CCBI && CCBI->isExact() && CCBI->getSuccessBB() == Arg->getParent()) {
        if (ResultType && ResultType != Arg->getType())
          return SILType();
        ResultType = Arg->getType();
        continue;
      }
    }

    // It is a BB argument, look through incoming values. If they all have the
    // same exact type, then we consider it to be the type of the BB argument.
    SmallVector<SILValue, 4> IncomingValues;
    if (Arg->getSingleTerminatorOperands(IncomingValues)) {
      for (auto InValue : IncomingValues) {
        WorkList.push_back(InValue);
      }
      continue;
    }

    // The exact type is unknown.
    return SILType();
  }

  return ResultType;
}


/// Try to determine the exact dynamic type of the underlying object.
/// returns the exact dynamic type of a value, or an empty type if the exact
/// type could not be determined.
SILType
swift::getExactDynamicTypeOfUnderlyingObject(SILValue S, SILModule &M,
                                             ClassHierarchyAnalysis *CHA) {
  return getExactDynamicType(S, M, CHA, /* ForUnderlyingObject */ true);
}

// Start with the substitutions from the apply.
// Try to propagate them to find out the real substitutions required
// to invoke the method.
static SubstitutionMap
getSubstitutionsForCallee(SILModule &M,
                          CanSILFunctionType baseCalleeType,
                          CanType derivedSelfType,
                          FullApplySite AI) {

  // If the base method is not polymorphic, no substitutions are required,
  // even if we originally had substitutions for calling the derived method.
  if (!baseCalleeType->isPolymorphic())
    return SubstitutionMap();

  // Add any generic substitutions for the base class.
  Type baseSelfType = baseCalleeType->getSelfParameter().getType();
  if (auto metatypeType = baseSelfType->getAs<MetatypeType>())
    baseSelfType = metatypeType->getInstanceType();

  auto *baseClassDecl = baseSelfType->getClassOrBoundGenericClass();
  assert(baseClassDecl && "not a class method");

  unsigned baseDepth = 0;
  SubstitutionMap baseSubMap;
  if (auto baseClassSig = baseClassDecl->getGenericSignatureOfContext()) {
    baseDepth = baseClassSig->getGenericParams().back()->getDepth() + 1;

    // Compute the type of the base class, starting from the
    // derived class type and the type of the method's self
    // parameter.
    Type derivedClass = derivedSelfType;
    if (auto metatypeType = derivedClass->getAs<MetatypeType>())
      derivedClass = metatypeType->getInstanceType();
    baseSubMap = derivedClass->getContextSubstitutionMap(
        M.getSwiftModule(), baseClassDecl);
  }

  SubstitutionMap origSubMap = AI.getSubstitutionMap();

  Type calleeSelfType = AI.getOrigCalleeType()->getSelfParameter().getType();
  if (auto metatypeType = calleeSelfType->getAs<MetatypeType>())
    calleeSelfType = metatypeType->getInstanceType();
  auto *calleeClassDecl = calleeSelfType->getClassOrBoundGenericClass();
  assert(calleeClassDecl && "self is not a class type");

  // Add generic parameters from the method itself, ignoring any generic
  // parameters from the derived class.
  unsigned origDepth = 0;
  if (auto calleeClassSig = calleeClassDecl->getGenericSignatureOfContext())
    origDepth = calleeClassSig->getGenericParams().back()->getDepth() + 1;

  auto baseCalleeSig = baseCalleeType->getGenericSignature();

  return
    SubstitutionMap::combineSubstitutionMaps(baseSubMap,
                                             origSubMap,
                                             CombineSubstitutionMaps::AtDepth,
                                             baseDepth,
                                             origDepth,
                                             baseCalleeSig);
}

static ApplyInst *replaceApplyInst(SILBuilder &B, SILLocation Loc,
                                   ApplyInst *OldAI,
                                   SILValue NewFn,
                                   SubstitutionMap NewSubs,
                                   ArrayRef<SILValue> NewArgs) {
  auto *NewAI = B.createApply(Loc, NewFn, NewSubs, NewArgs,
                              OldAI->isNonThrowing());

  // Check if any casting is required for the return value.
  SILValue ResultValue =
    castValueToABICompatibleType(&B, Loc, NewAI, NewAI->getType(),
                                 OldAI->getType());

  OldAI->replaceAllUsesWith(ResultValue);
  return NewAI;
}

static TryApplyInst *replaceTryApplyInst(SILBuilder &B, SILLocation Loc,
                                         TryApplyInst *OldTAI,
                                         SILValue NewFn,
                                         SubstitutionMap NewSubs,
                                         ArrayRef<SILValue> NewArgs,
                                         SILFunctionConventions Conv) {
  SILBasicBlock *NormalBB = OldTAI->getNormalBB();
  SILBasicBlock *ResultBB = nullptr;

  SILType NewResultTy = Conv.getSILResultType();

  // Does the result value need to be casted?
  auto OldResultTy = NormalBB->getArgument(0)->getType();
  bool ResultCastRequired = NewResultTy != OldResultTy;

  // Create a new normal BB only if the result of the new apply differs
  // in type from the argument of the original normal BB.
  if (!ResultCastRequired) {
    ResultBB = NormalBB;
  } else {
    ResultBB = B.getFunction().createBasicBlockBefore(NormalBB);
    ResultBB->createPhiArgument(NewResultTy, ValueOwnershipKind::Owned);
  }

  // We can always just use the original error BB because we'll be
  // deleting the edge to it from the old TAI.
  SILBasicBlock *ErrorBB = OldTAI->getErrorBB();

  // Insert a try_apply here.
  // Note that this makes this block temporarily double-terminated!
  // We won't fix that until deleteDevirtualizedApply.
  auto NewTAI = B.createTryApply(Loc, NewFn, NewSubs, NewArgs,
                                 ResultBB, ErrorBB);

  if (ResultCastRequired) {
    B.setInsertionPoint(ResultBB);

    SILValue ResultValue = ResultBB->getArgument(0);
    ResultValue = castValueToABICompatibleType(&B, Loc, ResultValue,
                                               NewResultTy, OldResultTy);

    B.createBranch(Loc, NormalBB, { ResultValue });
  }

  B.setInsertionPoint(NormalBB->begin());
  return NewTAI;
}

static BeginApplyInst *replaceBeginApplyInst(SILBuilder &B, SILLocation Loc,
                                             BeginApplyInst *OldBAI,
                                             SILValue NewFn,
                                             SubstitutionMap NewSubs,
                                             ArrayRef<SILValue> NewArgs) {
  auto NewBAI = B.createBeginApply(Loc, NewFn, NewSubs, NewArgs,
                                   OldBAI->isNonThrowing());

  // Forward the token.
  OldBAI->getTokenResult()->replaceAllUsesWith(NewBAI->getTokenResult());

  auto OldYields = OldBAI->getYieldedValues();
  auto NewYields = NewBAI->getYieldedValues();
  assert(OldYields.size() == NewYields.size());

  for (auto i : indices(OldYields)) {
    auto OldYield = OldYields[i];
    auto NewYield = NewYields[i];
    NewYield = castValueToABICompatibleType(&B, Loc, NewYield,
                                            NewYield->getType(),
                                            OldYield->getType());
    OldYield->replaceAllUsesWith(NewYield);
  }

  return NewBAI;
}

static PartialApplyInst *replacePartialApplyInst(SILBuilder &B, SILLocation Loc,
                                                 PartialApplyInst *OldPAI,
                                                 SILValue NewFn,
                                                 SubstitutionMap NewSubs,
                                                 ArrayRef<SILValue> NewArgs) {
  auto Convention =
    OldPAI->getType().getAs<SILFunctionType>()->getCalleeConvention();
  auto *NewPAI = B.createPartialApply(Loc, NewFn, NewSubs, NewArgs,
                                      Convention);

  // Check if any casting is required for the partially-applied function.
  SILValue ResultValue = castValueToABICompatibleType(
      &B, Loc, NewPAI, NewPAI->getType(), OldPAI->getType());
  OldPAI->replaceAllUsesWith(ResultValue);

  return NewPAI;
}

static ApplySite replaceApplySite(SILBuilder &B, SILLocation Loc,
                                  ApplySite OldAS,
                                  SILValue NewFn,
                                  SubstitutionMap NewSubs,
                                  ArrayRef<SILValue> NewArgs,
                                  SILFunctionConventions Conv) {
  if (auto *OldAI = dyn_cast<ApplyInst>(OldAS)) {
    return replaceApplyInst(B, Loc, OldAI, NewFn, NewSubs, NewArgs);
  } else if (auto *OldTAI = dyn_cast<TryApplyInst>(OldAS)) {
    return replaceTryApplyInst(B, Loc, OldTAI, NewFn, NewSubs, NewArgs, Conv);
  } else if (auto *OldBAI = dyn_cast<BeginApplyInst>(OldAS)) {
    return replaceBeginApplyInst(B, Loc, OldBAI, NewFn, NewSubs, NewArgs);
  } else {
    auto *OldPAI = cast<PartialApplyInst>(OldAS);
    return replacePartialApplyInst(B, Loc, OldPAI, NewFn, NewSubs, NewArgs);
  }
}

/// Delete an apply site that's been successfully devirtualized.
void swift::deleteDevirtualizedApply(ApplySite Old) {
  auto *OldApply = Old.getInstruction();
  recursivelyDeleteTriviallyDeadInstructions(OldApply, true);
}

SILFunction *swift::getTargetClassMethod(SILModule &M,
                                         SILType ClassOrMetatypeType,
                                         MethodInst *MI) {
  assert((isa<ClassMethodInst>(MI) || isa<SuperMethodInst>(MI)) &&
         "Only class_method and super_method instructions are supported");

  SILDeclRef Member = MI->getMember();
  if (ClassOrMetatypeType.is<MetatypeType>())
    ClassOrMetatypeType = ClassOrMetatypeType.getMetatypeInstanceType(M);

  auto *CD = ClassOrMetatypeType.getClassOrBoundGenericClass();
  return M.lookUpFunctionInVTable(CD, Member);
}

/// Check if it is possible to devirtualize an Apply instruction
/// and a class member obtained using the class_method instruction into
/// a direct call to a specific member of a specific class.
///
/// \p AI is the apply to devirtualize.
/// \p ClassOrMetatypeType is the class type or metatype type we are
///    devirtualizing for.
/// return true if it is possible to devirtualize, false - otherwise.
bool swift::canDevirtualizeClassMethod(FullApplySite AI,
                                       SILType ClassOrMetatypeType,
                                       OptRemark::Emitter *ORE,
                                       bool isEffectivelyFinalMethod) {

  LLVM_DEBUG(llvm::dbgs() << "    Trying to devirtualize : "
                          << *AI.getInstruction());

  SILModule &Mod = AI.getModule();

  // First attempt to lookup the origin for our class method. The origin should
  // either be a metatype or an alloc_ref.
  LLVM_DEBUG(llvm::dbgs() << "        Origin Type: " << ClassOrMetatypeType);

  auto *MI = cast<MethodInst>(AI.getCallee());

  // Find the implementation of the member which should be invoked.
  auto *F = getTargetClassMethod(Mod, ClassOrMetatypeType, MI);

  // If we do not find any such function, we have no function to devirtualize
  // to... so bail.
  if (!F) {
    LLVM_DEBUG(llvm::dbgs() << "        FAIL: Could not find matching VTable "
                               "or vtable method for this class.\n");
    return false;
  }

  // We need to disable the  “effectively final” opt if a function is inlinable
  if (isEffectivelyFinalMethod && AI.getFunction()->getResilienceExpansion() ==
                                      ResilienceExpansion::Minimal) {
    LLVM_DEBUG(llvm::dbgs() << "        FAIL: Could not optimize function "
                               "because it is an effectively-final inlinable: "
                            << AI.getFunction()->getName() << "\n");
    return false;
  }

  // Mandatory inlining does class method devirtualization. I'm not sure if this
  // is really needed, but some test rely on this.
  // So even for Onone functions we have to do it if the SILStage is raw.
  if (F->getModule().getStage() != SILStage::Raw && !F->shouldOptimize()) {
    // Do not consider functions that should not be optimized.
    LLVM_DEBUG(llvm::dbgs() << "        FAIL: Could not optimize function "
                            << " because it is marked no-opt: " << F->getName()
                            << "\n");
    return false;
  }

  if (AI.getFunction()->isSerialized()) {
    // function_ref inside fragile function cannot reference a private or
    // hidden symbol.
    if (!F->hasValidLinkageForFragileRef())
      return false;
  }

  // devirtualizeClassMethod below does not support this case. It currently
  // assumes it can try_apply call the target.
  if (!F->getLoweredFunctionType()->hasErrorResult() &&
      isa<TryApplyInst>(AI.getInstruction())) {
    LLVM_DEBUG(llvm::dbgs() << "        FAIL: Trying to devirtualize a "
          "try_apply but vtable entry has no error result.\n");
    return false;
  }

  return true;
}

/// Devirtualize an apply of a class method.
///
/// \p AI is the apply to devirtualize.
/// \p ClassOrMetatype is a class value or metatype value that is the
///    self argument of the apply we will devirtualize.
/// return the result value of the new ApplyInst if created one or null.
FullApplySite swift::devirtualizeClassMethod(FullApplySite AI,
                                             SILValue ClassOrMetatype,
                                             OptRemark::Emitter *ORE) {
  LLVM_DEBUG(llvm::dbgs() << "    Trying to devirtualize : "
                          << *AI.getInstruction());

  SILModule &Mod = AI.getModule();
  auto *MI = cast<MethodInst>(AI.getCallee());
  auto ClassOrMetatypeType = ClassOrMetatype->getType();
  auto *F = getTargetClassMethod(Mod, ClassOrMetatypeType, MI);

  CanSILFunctionType GenCalleeType = F->getLoweredFunctionType();

  SubstitutionMap Subs =
    getSubstitutionsForCallee(Mod, GenCalleeType,
                              ClassOrMetatypeType.getASTType(),
                              AI);
  CanSILFunctionType SubstCalleeType = GenCalleeType;
  if (GenCalleeType->isPolymorphic())
    SubstCalleeType = GenCalleeType->substGenericArgs(Mod, Subs);
  SILFunctionConventions substConv(SubstCalleeType, Mod);

  SILBuilderWithScope B(AI.getInstruction());
  SILLocation Loc = AI.getLoc();
  auto *FRI = B.createFunctionRefFor(Loc, F);

  // Create the argument list for the new apply, casting when needed
  // in order to handle covariant indirect return types and
  // contravariant argument types.
  llvm::SmallVector<SILValue, 8> NewArgs;

  auto IndirectResultArgIter = AI.getIndirectSILResults().begin();
  for (auto ResultTy : substConv.getIndirectSILResultTypes()) {
    NewArgs.push_back(
        castValueToABICompatibleType(&B, Loc, *IndirectResultArgIter,
                                     IndirectResultArgIter->getType(), ResultTy));
    ++IndirectResultArgIter;
  }

  auto ParamArgIter = AI.getArgumentsWithoutIndirectResults().begin();
  // Skip the last parameter, which is `self`. Add it below.
  for (auto param : substConv.getParameters().drop_back()) {
    auto paramType = substConv.getSILType(param);
    NewArgs.push_back(
        castValueToABICompatibleType(&B, Loc, *ParamArgIter,
                                     ParamArgIter->getType(), paramType));
    ++ParamArgIter;
  }

  // Add the self argument, upcasting if required because we're
  // calling a base class's method.
  auto SelfParamTy = substConv.getSILType(SubstCalleeType->getSelfParameter());
  NewArgs.push_back(castValueToABICompatibleType(&B, Loc,
                                                 ClassOrMetatype,
                                                 ClassOrMetatypeType,
                                                 SelfParamTy));

  ApplySite NewAS = replaceApplySite(B, Loc, AI, FRI, Subs, NewArgs, substConv);
  FullApplySite NewAI = FullApplySite::isa(NewAS.getInstruction());
  assert(NewAI);

  LLVM_DEBUG(llvm::dbgs() << "        SUCCESS: " << F->getName() << "\n");
  if (ORE)
    ORE->emit([&]() {
        using namespace OptRemark;
        return RemarkPassed("ClassMethodDevirtualized", *AI.getInstruction())
               << "Devirtualized call to class method " << NV("Method", F);
      });
  NumClassDevirt++;

  return NewAI;
}

FullApplySite
swift::tryDevirtualizeClassMethod(FullApplySite AI, SILValue ClassInstance,
                                  OptRemark::Emitter *ORE,
                                  bool isEffectivelyFinalMethod) {
  if (!canDevirtualizeClassMethod(AI, ClassInstance->getType(), ORE,
                                  isEffectivelyFinalMethod))
    return FullApplySite();
  return devirtualizeClassMethod(AI, ClassInstance, ORE);
}


//===----------------------------------------------------------------------===//
//                        Witness Method Optimization
//===----------------------------------------------------------------------===//

/// Compute substitutions for making a direct call to a SIL function with
/// @convention(witness_method) convention.
///
/// Such functions have a substituted generic signature where the
/// abstract `Self` parameter from the original type of the protocol
/// requirement is replaced by a concrete type.
///
/// Thus, the original substitutions of the apply instruction that
/// are written in terms of the requirement's generic signature need
/// to be remapped to substitutions suitable for the witness signature.
///
/// Supported remappings are:
///
/// - (Concrete witness thunk) Original substitutions:
///   [Self := ConcreteType, R0 := X0, R1 := X1, ...]
/// - Requirement generic signature:
///   <Self : P, R0, R1, ...>
/// - Witness thunk generic signature:
///   <W0, W1, ...>
/// - Remapped substitutions:
///   [W0 := X0, W1 := X1, ...]
///
/// - (Class witness thunk) Original substitutions:
///   [Self := C<A0, A1>, T0 := X0, T1 := X1, ...]
/// - Requirement generic signature:
///   <Self : P, R0, R1, ...>
/// - Witness thunk generic signature:
///   <Self : C<B0, B1>, B0, B1, W0, W1, ...>
/// - Remapped substitutions:
///   [Self := C<B0, B1>, B0 := A0, B1 := A1, W0 := X0, W1 := X1]
///
/// - (Default witness thunk) Original substitutions:
///   [Self := ConcreteType, R0 := X0, R1 := X1, ...]
/// - Requirement generic signature:
///   <Self : P, R0, R1, ...>
/// - Witness thunk generic signature:
///   <Self : P, W0, W1, ...>
/// - Remapped substitutions:
///   [Self := ConcreteType, W0 := X0, W1 := X1, ...]
///
/// \param conformanceRef The (possibly-specialized) conformance
/// \param requirementSig The generic signature of the requirement
/// \param witnessThunkSig The generic signature of the witness method
/// \param origSubMap The substitutions from the call instruction
/// \param isDefaultWitness True if this is a default witness method
/// \param classWitness The ClassDecl if this is a class witness method
static SubstitutionMap
getWitnessMethodSubstitutions(
    ModuleDecl *mod,
    ProtocolConformanceRef conformanceRef,
    GenericSignature *requirementSig,
    GenericSignature *witnessThunkSig,
    SubstitutionMap origSubMap,
    bool isDefaultWitness,
    ClassDecl *classWitness) {

  if (witnessThunkSig == nullptr)
    return SubstitutionMap();

  if (isDefaultWitness)
    return origSubMap;

  assert(!conformanceRef.isAbstract());
  auto conformance = conformanceRef.getConcrete();

  // If `Self` maps to a bound generic type, this gives us the
  // substitutions for the concrete type's generic parameters.
  auto baseSubMap = conformance->getSubstitutions(mod);

  unsigned baseDepth = 0;
  auto *rootConformance = conformance->getRootNormalConformance();
  if (auto *witnessSig = rootConformance->getGenericSignature())
    baseDepth = witnessSig->getGenericParams().back()->getDepth() + 1;

  // If the witness has a class-constrained 'Self' generic parameter,
  // we have to build a new substitution map that shifts all generic
  // parameters down by one.
  if (classWitness != nullptr) {
    auto *proto = conformance->getProtocol();
    auto selfType = proto->getSelfInterfaceType();

    auto selfSubMap = SubstitutionMap::getProtocolSubstitutions(
        proto, selfType.subst(origSubMap), conformanceRef);
    if (baseSubMap.empty()) {
      assert(baseDepth == 0);
      baseSubMap = selfSubMap;
    } else {
      baseSubMap = SubstitutionMap::combineSubstitutionMaps(
          selfSubMap,
          baseSubMap,
          CombineSubstitutionMaps::AtDepth,
          /*firstDepth=*/1,
          /*secondDepth=*/0,
          witnessThunkSig);
    }
    baseDepth += 1;
  }

  return SubstitutionMap::combineSubstitutionMaps(
      baseSubMap,
      origSubMap,
      CombineSubstitutionMaps::AtDepth,
      /*firstDepth=*/baseDepth,
      /*secondDepth=*/1,
      witnessThunkSig);
}

static SubstitutionMap
getWitnessMethodSubstitutions(SILModule &Module, ApplySite AI, SILFunction *F,
                              ProtocolConformanceRef CRef) {
  auto witnessFnTy = F->getLoweredFunctionType();
  assert(witnessFnTy->getRepresentation() ==
         SILFunctionTypeRepresentation::WitnessMethod);

  auto requirementSig = AI.getOrigCalleeType()->getGenericSignature();
  auto witnessThunkSig = witnessFnTy->getGenericSignature();

  SubstitutionMap origSubs = AI.getSubstitutionMap();

  auto *mod = Module.getSwiftModule();
  bool isDefaultWitness =
    (witnessFnTy->getDefaultWitnessMethodProtocol()
      == CRef.getRequirement());
  auto *classWitness = witnessFnTy->getWitnessMethodClass(*mod);

  return getWitnessMethodSubstitutions(
      mod, CRef, requirementSig, witnessThunkSig,
      origSubs, isDefaultWitness, classWitness);
}

/// Generate a new apply of a function_ref to replace an apply of a
/// witness_method when we've determined the actual function we'll end
/// up calling.
static ApplySite
devirtualizeWitnessMethod(ApplySite AI, SILFunction *F,
                          ProtocolConformanceRef C, OptRemark::Emitter *ORE) {
  // We know the witness thunk and the corresponding set of substitutions
  // required to invoke the protocol method at this point.
  auto &Module = AI.getModule();

  // Collect all the required substitutions.
  //
  // The complete set of substitutions may be different, e.g. because the found
  // witness thunk F may have been created by a specialization pass and have
  // additional generic parameters.
  auto SubMap = getWitnessMethodSubstitutions(Module, AI, F, C);

  // Figure out the exact bound type of the function to be called by
  // applying all substitutions.
  auto CalleeCanType = F->getLoweredFunctionType();
  auto SubstCalleeCanType = CalleeCanType->substGenericArgs(Module, SubMap);

  // Collect arguments from the apply instruction.
  auto Arguments = SmallVector<SILValue, 4>();

  // Iterate over the non self arguments and add them to the
  // new argument list, upcasting when required.
  SILBuilderWithScope B(AI.getInstruction());
  SILFunctionConventions substConv(SubstCalleeCanType, Module);
  unsigned substArgIdx = AI.getCalleeArgIndexOfFirstAppliedArg();
  for (auto arg : AI.getArguments()) {
    auto paramType = substConv.getSILArgumentType(substArgIdx++);
    if (arg->getType() != paramType)
      arg = castValueToABICompatibleType(&B, AI.getLoc(), arg,
                                         arg->getType(), paramType);
    Arguments.push_back(arg);
  }
  assert(substArgIdx == substConv.getNumSILArguments());

  // Replace old apply instruction by a new apply instruction that invokes
  // the witness thunk.
  SILBuilderWithScope Builder(AI.getInstruction());
  SILLocation Loc = AI.getLoc();
  auto *FRI = Builder.createFunctionRefFor(Loc, F);

  ApplySite SAI = replaceApplySite(Builder, Loc, AI, FRI, SubMap, Arguments,
                                   substConv);

  if (ORE)
    ORE->emit([&]() {
        using namespace OptRemark;
        return RemarkPassed("WitnessMethodDevirtualized", *AI.getInstruction())
               << "Devirtualized call to " << NV("Method", F);
      });
  NumWitnessDevirt++;
  return SAI;
}

static bool canDevirtualizeWitnessMethod(ApplySite AI) {
  SILFunction *F;
  SILWitnessTable *WT;

  auto *WMI = cast<WitnessMethodInst>(AI.getCallee());

  std::tie(F, WT) =
    AI.getModule().lookUpFunctionInWitnessTable(WMI->getConformance(),
                                                WMI->getMember());

  if (!F)
    return false;

  if (AI.getFunction()->isSerialized()) {
    // function_ref inside fragile function cannot reference a private or
    // hidden symbol.
    if (!F->hasValidLinkageForFragileRef())
      return false;
  }

  // devirtualizeWitnessMethod below does not support this case. It currently
  // assumes it can try_apply call the target.
  if (!F->getLoweredFunctionType()->hasErrorResult() &&
      isa<TryApplyInst>(AI.getInstruction())) {
    LLVM_DEBUG(llvm::dbgs() << "        FAIL: Trying to devirtualize a "
          "try_apply but wtable entry has no error result.\n");
    return false;
  }

  return true;
}

/// In the cases where we can statically determine the function that
/// we'll call to, replace an apply of a witness_method with an apply
/// of a function_ref, returning the new apply.
ApplySite
swift::tryDevirtualizeWitnessMethod(ApplySite AI, OptRemark::Emitter *ORE) {
  if (!canDevirtualizeWitnessMethod(AI))
    return ApplySite();

  SILFunction *F;
  SILWitnessTable *WT;

  auto *WMI = cast<WitnessMethodInst>(AI.getCallee());

  std::tie(F, WT) =
    AI.getModule().lookUpFunctionInWitnessTable(WMI->getConformance(),
                                                WMI->getMember());

  return devirtualizeWitnessMethod(AI, F, WMI->getConformance(), ORE);
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

/// Attempt to devirtualize the given apply if possible, and return a
/// new instruction in that case, or nullptr otherwise.
ApplySite swift::tryDevirtualizeApply(ApplySite AI,
                                      ClassHierarchyAnalysis *CHA,
                                      OptRemark::Emitter *ORE) {
  LLVM_DEBUG(llvm::dbgs() << "    Trying to devirtualize: "
                          << *AI.getInstruction());

  // Devirtualize apply instructions that call witness_method instructions:
  //
  //   %8 = witness_method $Optional<UInt16>, #LogicValue.boolValue!getter.1
  //   %9 = apply %8<Self = CodeUnit?>(%6#1) : ...
  //
  if (isa<WitnessMethodInst>(AI.getCallee()))
    return tryDevirtualizeWitnessMethod(AI, ORE);

  // TODO: check if we can also de-virtualize partial applies of class methods.
  FullApplySite FAS = FullApplySite::isa(AI.getInstruction());
  if (!FAS)
    return ApplySite();

  /// Optimize a class_method and alloc_ref pair into a direct function
  /// reference:
  ///
  /// \code
  /// %XX = alloc_ref $Foo
  /// %YY = class_method %XX : $Foo, #Foo.get!1 : $@convention(method)...
  /// \endcode
  ///
  ///  or
  ///
  /// %XX = metatype $...
  /// %YY = class_method %XX : ...
  ///
  ///  into
  ///
  /// %YY = function_ref @...
  if (auto *CMI = dyn_cast<ClassMethodInst>(FAS.getCallee())) {
    auto &M = FAS.getModule();
    auto Instance = stripUpCasts(CMI->getOperand());
    auto ClassType = Instance->getType();
    if (ClassType.is<MetatypeType>())
      ClassType = ClassType.getMetatypeInstanceType(M);

    auto *CD = ClassType.getClassOrBoundGenericClass();

    if (isEffectivelyFinalMethod(FAS, ClassType, CD, CHA))
      return tryDevirtualizeClassMethod(FAS, Instance, ORE,
                                        true /*isEffectivelyFinalMethod*/);

    // Try to check if the exact dynamic type of the instance is statically
    // known.
    if (auto Instance = getInstanceWithExactDynamicType(CMI->getOperand(),
                                                        CMI->getModule(),
                                                        CHA))
      return tryDevirtualizeClassMethod(FAS, Instance, ORE);

    if (auto ExactTy = getExactDynamicType(CMI->getOperand(), CMI->getModule(),
                                           CHA)) {
      if (ExactTy == CMI->getOperand()->getType())
        return tryDevirtualizeClassMethod(FAS, CMI->getOperand(), ORE);
    }
  }

  if (isa<SuperMethodInst>(FAS.getCallee())) {
    if (FAS.hasSelfArgument()) {
      return tryDevirtualizeClassMethod(FAS, FAS.getSelfArgument(), ORE);
    }

    // It is an invocation of a class method.
    // Last operand is the metatype that should be used for dispatching.
    return tryDevirtualizeClassMethod(FAS, FAS.getArguments().back(), ORE);
  }

  return ApplySite();
}

bool swift::canDevirtualizeApply(FullApplySite AI, ClassHierarchyAnalysis *CHA) {
  LLVM_DEBUG(llvm::dbgs() << "    Trying to devirtualize: "
                          << *AI.getInstruction());

  // Devirtualize apply instructions that call witness_method instructions:
  //
  //   %8 = witness_method $Optional<UInt16>, #LogicValue.boolValue!getter.1
  //   %9 = apply %8<Self = CodeUnit?>(%6#1) : ...
  //
  if (isa<WitnessMethodInst>(AI.getCallee()))
    return canDevirtualizeWitnessMethod(AI);

  /// Optimize a class_method and alloc_ref pair into a direct function
  /// reference:
  ///
  /// \code
  /// %XX = alloc_ref $Foo
  /// %YY = class_method %XX : $Foo, #Foo.get!1 : $@convention(method)...
  /// \endcode
  ///
  ///  or
  ///
  /// %XX = metatype $...
  /// %YY = class_method %XX : ...
  ///
  ///  into
  ///
  /// %YY = function_ref @...
  if (auto *CMI = dyn_cast<ClassMethodInst>(AI.getCallee())) {
    auto &M = AI.getModule();
    auto Instance = stripUpCasts(CMI->getOperand());
    auto ClassType = Instance->getType();
    if (ClassType.is<MetatypeType>())
      ClassType = ClassType.getMetatypeInstanceType(M);

    auto *CD = ClassType.getClassOrBoundGenericClass();

    if (isEffectivelyFinalMethod(AI, ClassType, CD, CHA))
      return canDevirtualizeClassMethod(AI, Instance->getType(),
                                        nullptr /*ORE*/,
                                        true /*isEffectivelyFinalMethod*/);

    // Try to check if the exact dynamic type of the instance is statically
    // known.
    if (auto Instance = getInstanceWithExactDynamicType(CMI->getOperand(),
                                                        CMI->getModule(),
                                                        CHA))
      return canDevirtualizeClassMethod(AI, Instance->getType());

    if (auto ExactTy = getExactDynamicType(CMI->getOperand(), CMI->getModule(),
                                           CHA)) {
      if (ExactTy == CMI->getOperand()->getType())
        return canDevirtualizeClassMethod(AI, CMI->getOperand()->getType());
    }
  }

  if (isa<SuperMethodInst>(AI.getCallee())) {
    if (AI.hasSelfArgument()) {
      return canDevirtualizeClassMethod(AI, AI.getSelfArgument()->getType());
    }

    // It is an invocation of a class method.
    // Last operand is the metatype that should be used for dispatching.
    return canDevirtualizeClassMethod(AI, AI.getArguments().back()->getType());
  }

  return false;
}
