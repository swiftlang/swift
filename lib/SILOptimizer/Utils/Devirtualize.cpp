//===--- Devirtualize.cpp - Helper for devirtualizing apply ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-devirtualize-utility"
#include "swift/SILOptimizer/Analysis/ClassHierarchyAnalysis.h"
#include "swift/SILOptimizer/Utils/Devirtualize.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Casting.h"
using namespace swift;

STATISTIC(NumClassDevirt, "Number of class_method applies devirtualized");
STATISTIC(NumWitnessDevirt, "Number of witness_method applies devirtualized");

//===----------------------------------------------------------------------===//
//                         Class Method Optimization
//===----------------------------------------------------------------------===//

/// Compute all subclasses of a given class.
///
/// \p CHA class hierarchy analysis
/// \p CD class declaration
/// \p ClassType type of the instance
/// \p M SILModule
/// \p Subs a container to be used for storing the set of subclasses
static void getAllSubclasses(ClassHierarchyAnalysis *CHA,
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
  //SmallVector<ClassDecl *, 8> Subs(DirectSubs);
  Subs.append(IndirectSubs.begin(), IndirectSubs.end());

  if (isa<BoundGenericClassType>(ClassType.getSwiftRValueType())) {
    // Filter out any subclasses that do not inherit from this
    // specific bound class.
    auto RemovedIt = std::remove_if(Subs.begin(), Subs.end(),
        [&ClassType, &M](ClassDecl *Sub){
          auto SubCanTy = Sub->getDeclaredType()->getCanonicalType();
          // Unbound generic type can override a method from
          // a bound generic class, but this unbound generic
          // class is not considered to be a subclass of a
          // bound generic class in a general case.
          if (isa<UnboundGenericType>(SubCanTy))
            return false;
          // Handle the usual case here: the class in question
          // should be a real subclass of a bound generic class.
          return !ClassType.isBindableToSuperclassOf(
              SILType::getPrimitiveObjectType(SubCanTy));
        });
    Subs.erase(RemovedIt, Subs.end());
  }
}

/// \brief Returns true, if a method implementation corresponding to
/// the class_method applied to an instance of the class CD is
/// effectively final, i.e. it is statically known to be not overridden
/// by any subclasses of the class CD.
///
/// \p AI  invocation instruction
/// \p ClassType type of the instance
/// \p CD  static class of the instance whose method is being invoked
/// \p CHA class hierarchy analysis
bool isEffectivelyFinalMethod(FullApplySite AI,
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

  if (!CD->hasAccessibility())
    return false;

  // Only consider 'private' members, unless we are in whole-module compilation.
  switch (CD->getEffectiveAccess()) {
  case Accessibility::Open:
    return false;
  case Accessibility::Public:
  case Accessibility::Internal:
    if (!M.isWholeModule())
      return false;
    break;
  case Accessibility::FilePrivate:
  case Accessibility::Private:
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
      case Accessibility::Open:
        return false;
      case Accessibility::Public:
      case Accessibility::Internal:
        if (!M.isWholeModule())
          return false;
        break;
      case Accessibility::FilePrivate:
      case Accessibility::Private:
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
static SILValue getInstanceWithExactDynamicType(SILValue S, SILModule &M,
                                                ClassHierarchyAnalysis *CHA) {

  while (S) {
    S = stripCasts(S);
    if (isa<AllocRefInst>(S) || isa<MetatypeInst>(S))
      return S;

    auto *Arg = dyn_cast<SILArgument>(S);
    if (!Arg)
      break;

    auto *SinglePred = Arg->getParent()->getSinglePredecessor();
    if (!SinglePred) {
      if (!Arg->isFunctionArg())
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
      S = Arg->getIncomingValue(SinglePred);
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

/// Return bound generic type for the unbound type Superclass,
/// which is a superclass of a bound generic type BoundDerived
/// (Base may be also the same as BoundDerived or may be
/// non-generic at all).
static CanType bindSuperclass(CanType Superclass,
                                          SILType BoundDerived) {
  assert(BoundDerived && "Expected non-null type!");

  SILType BoundSuperclass = BoundDerived;

  do {
    // Get declaration of the superclass.
    auto *Decl = BoundSuperclass.getNominalOrBoundGenericNominal();
    // Obtain the unbound variant of the current superclass
    CanType UnboundSuperclass = Decl->getDeclaredType()->getCanonicalType();
    // Check if we found a superclass we are looking for.
    if (UnboundSuperclass == Superclass)
      return BoundSuperclass.getSwiftRValueType();

    // Get the superclass of current one
    BoundSuperclass = BoundSuperclass.getSuperclass(nullptr);
  } while (BoundSuperclass);

  llvm_unreachable("Expected to find a bound generic superclass!");
}

// Returns true if any generic types parameters of the class are
// unbound.
bool swift::isNominalTypeWithUnboundGenericParameters(SILType Ty, SILModule &M) {
  auto *ND = Ty.getNominalOrBoundGenericNominal();
  if (ND && ND->getGenericSignature()) {
    auto InstanceTypeSubsts =
        Ty.gatherAllSubstitutions(M);

    if (!InstanceTypeSubsts.empty()) {
      if (hasUnboundGenericTypes(InstanceTypeSubsts))
        return true;
    }
  }

  if (Ty.hasArchetype())
    return true;

  return false;
}

// Start with the substitutions from the apply.
// Try to propagate them to find out the real substitutions required
// to invoke the method.
static ArrayRef<Substitution>
getSubstitutionsForCallee(SILModule &M, CanSILFunctionType GenCalleeType,
                          SILType ClassInstanceType, FullApplySite AI) {
  // *NOTE*:
  // Apply instruction substitutions are for the Member from a protocol or
  // class B, where this member was first defined, before it got overridden by
  // derived classes.
  //
  // The implementation F (the implementing method) which was found may have
  // a different set of generic parameters, e.g. because it is implemented by a
  // class D1 derived from B.
  //
  // ClassInstanceType may have a type different from both the type B
  // the Member belongs to and from the ClassInstanceType, e.g. if
  // ClassInstance is of a class D2, which is derived from D1, but does not
  // override the Member.
  //
  // As a result, substitutions provided by AI are for Member, whereas
  // substitutions in ClassInstanceType are for D2. And substitutions for D1
  // are not available directly in a general case. Therefore, they have to
  // be computed.
  //
  // What we know for sure:
  //   B is a superclass of D1
  //   D1 is a superclass of D2.
  // D1 can be the same as D2. D1 can be the same as B.
  //
  // So, substitutions from AI are for class B.
  // Substitutions for class D1 by means of bindSuperclass(), which starts
  // with a bound type ClassInstanceType and checks its superclasses until it
  // finds a bound superclass matching D1 and returns its substitutions.

  // Class F belongs to.
  CanType FSelfClass = GenCalleeType->getSelfParameter().getType();

  auto *Module = M.getSwiftModule();

  ArrayRef<Substitution> ClassSubs;

  if (GenCalleeType->isPolymorphic()) {
    // Declaration of the class F belongs to.
    if (auto *FSelfTypeDecl = FSelfClass.getNominalOrBoundGenericNominal()) {
      // Get the unbound generic type F belongs to.
      CanType FSelfGenericType =
        FSelfTypeDecl->getDeclaredType()->getCanonicalType();

      assert((isa<BoundGenericType>(ClassInstanceType.getSwiftRValueType()) ||
              isa<NominalType>(ClassInstanceType.getSwiftRValueType())) &&
             "Self type should be either a bound generic type"
             "or a non-generic type");

      assert((isa<UnboundGenericType>(FSelfGenericType) ||
              isa<NominalType>(FSelfGenericType)) &&
             "Method implementation self type should be generic");

      if (isa<BoundGenericType>(ClassInstanceType.getSwiftRValueType())) {
        auto BoundBaseType = bindSuperclass(FSelfGenericType,
                                            ClassInstanceType);
        ClassSubs = BoundBaseType->gatherAllSubstitutions(Module, nullptr);
      }
    }
  } else {
    // If the callee is not polymorphic, no substitutions are required.
    return {};
  }

  if (ClassSubs.empty())
    return AI.getSubstitutions();

  auto AISubs = AI.getSubstitutions();

  CanSILFunctionType AIGenCalleeType =
      AI.getCallee()->getType().castTo<SILFunctionType>();

  unsigned NextMethodParamIdx = 0;
  unsigned NumMethodParams = 0;
  if (AIGenCalleeType->isPolymorphic()) {
    NextMethodParamIdx = AISubs.size();
    if (auto AIMethodSig = AI.getOrigCalleeType()->getGenericSignature()) {
      // Find out if the apply instruction contains any substitutions for
      // a method itself, not for the class where it is declared.
      // If there are any such parameters, remember where they start
      // in the substitutions list.
      auto InnermostGenericParams = AIMethodSig->getInnermostGenericParams();
      auto InnermostGenericParamsNum = InnermostGenericParams.size();
      if (InnermostGenericParamsNum != AIMethodSig->getGenericParams().size()) {
        auto Depth = InnermostGenericParams[0]->getDepth();
        for (NextMethodParamIdx = 0; NextMethodParamIdx < AISubs.size();
             ++NextMethodParamIdx) {
          if (auto SubstTy = dyn_cast<SubstitutedType>(
                  AISubs[NextMethodParamIdx].getReplacement().getPointer())) {
            if (auto *GenParamTy = dyn_cast<GenericTypeParamType>(
                    SubstTy->getOriginal().getPointer()))
              if (GenParamTy->getDepth() == Depth)
                break;
          }
        }
      }
    }
    NumMethodParams = AISubs.size() - NextMethodParamIdx;
  }

  unsigned NumSubs = ClassSubs.size() + NumMethodParams;

  if (ClassSubs.size() == NumSubs)
    return ClassSubs;

  // Mix class subs with method specific subs from the AI substitutions.

  // Assumptions: AI substitutions contain first the substitutions for
  // a class of the method being invoked and then the substitutions
  // for a method being invoked.
  auto Subs = M.getASTContext().Allocate<Substitution>(NumSubs);

  unsigned i = 0;
  for (auto &S : ClassSubs) {
    Subs[i++] = S;
  }

  for (; i < NumSubs; ++i, ++NextMethodParamIdx) {
    Subs[i] = AISubs[NextMethodParamIdx];
  }

  return Subs;
}

SILFunction *swift::getTargetClassMethod(SILModule &M,
                                         SILType ClassOrMetatypeType,
                                         MethodInst *MI) {
  assert((isa<ClassMethodInst>(MI) || isa<WitnessMethodInst>(MI) ||
          isa<SuperMethodInst>(MI)) &&
         "Only class_method and witness_method instructions are supported");

  SILDeclRef Member = MI->getMember();
  if (ClassOrMetatypeType.is<MetatypeType>())
    ClassOrMetatypeType = ClassOrMetatypeType.getMetatypeInstanceType(M);

  auto *CD = ClassOrMetatypeType.getClassOrBoundGenericClass();
  return M.lookUpFunctionInVTable(CD, Member);
}

/// \brief Check if it is possible to devirtualize an Apply instruction
/// and a class member obtained using the class_method instruction into
/// a direct call to a specific member of a specific class.
///
/// \p AI is the apply to devirtualize.
/// \p ClassOrMetatypeType is the class type or metatype type we are
///    devirtualizing for.
/// return true if it is possible to devirtualize, false - otherwise.
bool swift::canDevirtualizeClassMethod(FullApplySite AI,
                                       SILType ClassOrMetatypeType) {
  DEBUG(llvm::dbgs() << "    Trying to devirtualize : " << *AI.getInstruction());

  SILModule &Mod = AI.getModule();

  // First attempt to lookup the origin for our class method. The origin should
  // either be a metatype or an alloc_ref.
  DEBUG(llvm::dbgs() << "        Origin Type: " << ClassOrMetatypeType);

  auto *MI = cast<MethodInst>(AI.getCallee());

  // Find the implementation of the member which should be invoked.
  auto *F = getTargetClassMethod(Mod, ClassOrMetatypeType, MI);

  // If we do not find any such function, we have no function to devirtualize
  // to... so bail.
  if (!F) {
    DEBUG(llvm::dbgs() << "        FAIL: Could not find matching VTable or "
                          "vtable method for this class.\n");
    return false;
  }

  if (!F->shouldOptimize()) {
    // Do not consider functions that should not be optimized.
    DEBUG(llvm::dbgs() << "        FAIL: Could not optimize function "
                       << " because it is marked no-opt: " << F->getName()
                       << "\n");
    return false;
  }

  if (AI.getFunction()->isFragile()) {
    // function_ref inside fragile function cannot reference a private or
    // hidden symbol.
    if (!F->hasValidLinkageForFragileRef())
      return false;
  }

  // Type of the actual function to be called.
  CanSILFunctionType GenCalleeType = F->getLoweredFunctionType();

  // Type of the actual function to be called with substitutions applied.
  CanSILFunctionType SubstCalleeType = GenCalleeType;

  // For polymorphic functions, bail if the number of substitutions is
  // not the same as the number of expected generic parameters.
  if (GenCalleeType->isPolymorphic()) {
    // First, find proper list of substitutions for the concrete
    // method to be called.
    auto Subs = getSubstitutionsForCallee(Mod, GenCalleeType,
                                          ClassOrMetatypeType, AI);

    auto GenericSig = GenCalleeType->getGenericSignature();
    // Get the number of expected generic parameters, which
    // is a sum of the number of explicit generic parameters
    // and the number of their recursive member types exposed
    // through protocol requirements.
    auto DepTypes = GenericSig->getAllDependentTypes();
    unsigned ExpectedSubsNum = 0;

    for (auto DT: DepTypes) {
      (void)DT;
      ExpectedSubsNum++;
    }

    if (ExpectedSubsNum != Subs.size()) {
      return false;
    }

    SubstCalleeType =
        GenCalleeType->substGenericArgs(Mod, Mod.getSwiftModule(), Subs);
  }

  // Check if the optimizer knows how to cast the return type.
  SILType ReturnType = SubstCalleeType->getSILResult();

  if (!canCastValueToABICompatibleType(Mod, ReturnType, AI.getType()))
      return false;

  return true;
}

/// \brief Devirtualize an apply of a class method.
///
/// \p AI is the apply to devirtualize.
/// \p ClassOrMetatype is a class value or metatype value that is the
///    self argument of the apply we will devirtualize.
/// return the result value of the new ApplyInst if created one or null.
DevirtualizationResult swift::devirtualizeClassMethod(FullApplySite AI,
                                                     SILValue ClassOrMetatype) {
  DEBUG(llvm::dbgs() << "    Trying to devirtualize : " << *AI.getInstruction());

  SILModule &Mod = AI.getModule();
  auto *MI = cast<MethodInst>(AI.getCallee());
  auto ClassOrMetatypeType = ClassOrMetatype->getType();
  auto *F = getTargetClassMethod(Mod, ClassOrMetatypeType, MI);

  CanSILFunctionType GenCalleeType = F->getLoweredFunctionType();

  auto Subs = getSubstitutionsForCallee(Mod, GenCalleeType,
                                        ClassOrMetatypeType, AI);
  CanSILFunctionType SubstCalleeType = GenCalleeType;
  if (GenCalleeType->isPolymorphic())
    SubstCalleeType = GenCalleeType->substGenericArgs(Mod, Mod.getSwiftModule(), Subs);

  SILBuilderWithScope B(AI.getInstruction());
  FunctionRefInst *FRI = B.createFunctionRef(AI.getLoc(), F);

  // Create the argument list for the new apply, casting when needed
  // in order to handle covariant indirect return types and
  // contravariant argument types.
  llvm::SmallVector<SILValue, 8> NewArgs;

  auto IndirectResultArgs = AI.getIndirectResults();
  auto IndirectResultInfos = SubstCalleeType->getIndirectResults();
  for (unsigned i : indices(IndirectResultArgs))
    NewArgs.push_back(castValueToABICompatibleType(&B, AI.getLoc(),
                              IndirectResultArgs[i],
                              IndirectResultArgs[i]->getType(),
                              IndirectResultInfos[i].getSILType()).getValue());

  auto Args = AI.getArgumentsWithoutIndirectResults();
  auto ParamTypes = SubstCalleeType->getParameterSILTypes();
  for (unsigned i = 0, e = Args.size() - 1; i != e; ++i)
    NewArgs.push_back(castValueToABICompatibleType(&B, AI.getLoc(), Args[i],
                                                   Args[i]->getType(),
                                                   ParamTypes[i]).getValue());

  // Add the self argument, upcasting if required because we're
  // calling a base class's method.
  auto SelfParamTy = SubstCalleeType->getSelfParameter().getSILType();
  NewArgs.push_back(castValueToABICompatibleType(&B, AI.getLoc(),
                                                 ClassOrMetatype,
                                                 ClassOrMetatypeType,
                                                 SelfParamTy).getValue());

  SILType ResultTy = SubstCalleeType->getSILResult();

  SILType SubstCalleeSILType =
    SILType::getPrimitiveObjectType(SubstCalleeType);
  FullApplySite NewAI;

  SILBasicBlock *ResultBB = nullptr;
  SILBasicBlock *NormalBB = nullptr;
  SILValue ResultValue;
  bool ResultCastRequired = false;
  SmallVector<Operand *, 4> OriginalResultUses;

  if (!isa<TryApplyInst>(AI)) {
    NewAI = B.createApply(AI.getLoc(), FRI, SubstCalleeSILType, ResultTy,
                          Subs, NewArgs, cast<ApplyInst>(AI)->isNonThrowing());
    ResultValue = NewAI.getInstruction();
  } else {
    auto *TAI = cast<TryApplyInst>(AI);
    // Create new normal and error BBs only if:
    // - re-using a BB would create a critical edge
    // - or, the result of the new apply would be of different
    //   type than the argument of the original normal BB.
    if (TAI->getNormalBB()->getSinglePredecessor())
      ResultBB = TAI->getNormalBB();
    else {
      ResultBB = B.getFunction().createBasicBlock();
      ResultBB->createBBArg(ResultTy);
    }

    NormalBB = TAI->getNormalBB();

    SILBasicBlock *ErrorBB = nullptr;
    if (TAI->getErrorBB()->getSinglePredecessor())
      ErrorBB = TAI->getErrorBB();
    else {
      ErrorBB = B.getFunction().createBasicBlock();
      ErrorBB->createBBArg(TAI->getErrorBB()->getBBArg(0)->getType());
    }

    NewAI = B.createTryApply(AI.getLoc(), FRI, SubstCalleeSILType,
                             Subs, NewArgs,
                             ResultBB, ErrorBB);
    if (ErrorBB != TAI->getErrorBB()) {
      B.setInsertionPoint(ErrorBB);
      B.createBranch(TAI->getLoc(), TAI->getErrorBB(),
                     {ErrorBB->getBBArg(0)});
    }

    // Does the result value need to be casted?
    ResultCastRequired = ResultTy != NormalBB->getBBArg(0)->getType();

    if (ResultBB != NormalBB)
      B.setInsertionPoint(ResultBB);
    else if (ResultCastRequired) {
      B.setInsertionPoint(NormalBB->begin());
      // Collect all uses, before casting.
      for (auto *Use : NormalBB->getBBArg(0)->getUses()) {
        OriginalResultUses.push_back(Use);
      }
      NormalBB->getBBArg(0)->replaceAllUsesWith(SILUndef::get(AI.getType(), Mod));
      NormalBB->replaceBBArg(0, ResultTy, nullptr);
    }

    // The result value is passed as a parameter to the normal block.
    ResultValue = ResultBB->getBBArg(0);
  }

  // Check if any casting is required for the return value.
  ResultValue = castValueToABICompatibleType(&B, NewAI.getLoc(), ResultValue,
                                             ResultTy, AI.getType()).getValue();

  DEBUG(llvm::dbgs() << "        SUCCESS: " << F->getName() << "\n");
  NumClassDevirt++;

  if (NormalBB) {
    if (NormalBB != ResultBB) {
      // If artificial normal BB was introduced, branch
      // to the original normal BB.
      B.createBranch(NewAI.getLoc(), NormalBB, { ResultValue });
    } else if (ResultCastRequired) {
      // Update all original uses by the new value.
      for (auto *Use: OriginalResultUses) {
        Use->set(ResultValue);
      }
    }
    return std::make_pair(NewAI.getInstruction(), NewAI);
  }

  // We need to return a pair of values here:
  // - the first one is the actual result of the devirtualized call, possibly
  //   casted into an appropriate type. This SILValue may be a BB arg, if it
  //   was a cast between optional types.
  // - the second one is the new apply site.
  return std::make_pair(ResultValue, NewAI);
}

DevirtualizationResult swift::tryDevirtualizeClassMethod(FullApplySite AI,
                                                   SILValue ClassInstance) {
  if (!canDevirtualizeClassMethod(AI, ClassInstance->getType()))
    return std::make_pair(nullptr, FullApplySite());
  return devirtualizeClassMethod(AI, ClassInstance);
}


//===----------------------------------------------------------------------===//
//                        Witness Method Optimization
//===----------------------------------------------------------------------===//

static void getWitnessMethodSubstitutions(ApplySite AI, SILFunction *F,
                                          ArrayRef<Substitution> Subs,
                                          SmallVectorImpl<Substitution> &NewSubs) {
  auto &Module = AI.getModule();

  auto CalleeCanType = F->getLoweredFunctionType();

  ProtocolDecl *proto = nullptr;
  if (CalleeCanType->getRepresentation() ==
      SILFunctionTypeRepresentation::WitnessMethod) {
    proto = CalleeCanType->getDefaultWitnessMethodProtocol(
        *Module.getSwiftModule());
  }

  ArrayRef<Substitution> origSubs = AI.getSubstitutions();

  if (proto != nullptr) {
    // If the callee is a default witness method thunk, preserve substitutions
    // from the call site.
    NewSubs.append(origSubs.begin(), origSubs.end());
    return;
  }

  // If the callee is a concrete witness method thunk, apply substitutions
  // from the conformance, and drop any substitutions derived from the Self
  // type.
  NewSubs.append(Subs.begin(), Subs.end());

  if (auto generics = AI.getOrigCalleeType()->getGenericSignature()) {
    for (auto genericParam : generics->getAllDependentTypes()) {
      auto origSub = origSubs.front();
      origSubs = origSubs.slice(1);

      // If the callee is a concrete witness method thunk, we ignore
      // generic parameters derived from 'self', the generic parameter at
      // depth 0, index 0.
      auto type = genericParam->getCanonicalType();
      while (auto memberType = dyn_cast<DependentMemberType>(type)) {
        type = memberType.getBase();
      }
      auto paramType = cast<GenericTypeParamType>(type);
      if (paramType->getDepth() == 0) {
        // There shouldn't be any other parameters at this depth.
        assert(paramType->getIndex() == 0);
        continue;
      }

      // Okay, remember this substitution.
      NewSubs.push_back(origSub);
    }
  }

  assert(origSubs.empty() && "subs not parallel to dependent types");
}

/// Generate a new apply of a function_ref to replace an apply of a
/// witness_method when we've determined the actual function we'll end
/// up calling.
static ApplySite devirtualizeWitnessMethod(ApplySite AI, SILFunction *F,
                                           ArrayRef<Substitution> Subs) {
  // We know the witness thunk and the corresponding set of substitutions
  // required to invoke the protocol method at this point.
  auto &Module = AI.getModule();

  // Collect all the required substitutions.
  //
  // The complete set of substitutions may be different, e.g. because the found
  // witness thunk F may have been created by a specialization pass and have
  // additional generic parameters.
  SmallVector<Substitution, 4> NewSubs;

  getWitnessMethodSubstitutions(AI, F, Subs, NewSubs);

  // Figure out the exact bound type of the function to be called by
  // applying all substitutions.
  auto CalleeCanType = F->getLoweredFunctionType();
  auto SubstCalleeCanType = CalleeCanType->substGenericArgs(
    Module, Module.getSwiftModule(), NewSubs);

  // Collect arguments from the apply instruction.
  auto Arguments = SmallVector<SILValue, 4>();

  // Iterate over the non self arguments and add them to the
  // new argument list, upcasting when required.
  SILBuilderWithScope B(AI.getInstruction());
  for (unsigned ArgN = 0, ArgE = AI.getNumArguments(); ArgN != ArgE; ++ArgN) {
    SILValue A = AI.getArgument(ArgN);
    auto ParamType = SubstCalleeCanType->getSILArgumentType(
      SubstCalleeCanType->getNumSILArguments() - AI.getNumArguments() + ArgN);
    if (A->getType() != ParamType)
      A = B.createUpcast(AI.getLoc(), A, ParamType);

    Arguments.push_back(A);
  }

  // Replace old apply instruction by a new apply instruction that invokes
  // the witness thunk.
  SILBuilderWithScope Builder(AI.getInstruction());
  SILLocation Loc = AI.getLoc();
  FunctionRefInst *FRI = Builder.createFunctionRef(Loc, F);

  auto SubstCalleeSILType = SILType::getPrimitiveObjectType(SubstCalleeCanType);
  auto ResultSILType = SubstCalleeCanType->getSILResult();
  ApplySite SAI;

  if (auto *A = dyn_cast<ApplyInst>(AI))
    SAI = Builder.createApply(Loc, FRI, SubstCalleeSILType,
                              ResultSILType, NewSubs, Arguments,
                              A->isNonThrowing());
  if (auto *TAI = dyn_cast<TryApplyInst>(AI))
    SAI = Builder.createTryApply(Loc, FRI, SubstCalleeSILType,
                                 NewSubs, Arguments,
                                 TAI->getNormalBB(), TAI->getErrorBB());
  if (auto *PAI = dyn_cast<PartialApplyInst>(AI))
    SAI = Builder.createPartialApply(Loc, FRI, SubstCalleeSILType,
                                     NewSubs, Arguments, PAI->getType());

  NumWitnessDevirt++;
  return SAI;
}

/// In the cases where we can statically determine the function that
/// we'll call to, replace an apply of a witness_method with an apply
/// of a function_ref, returning the new apply.
DevirtualizationResult swift::tryDevirtualizeWitnessMethod(ApplySite AI) {
  SILFunction *F;
  ArrayRef<Substitution> Subs;
  SILWitnessTable *WT;

  auto *WMI = cast<WitnessMethodInst>(AI.getCallee());

  std::tie(F, WT, Subs) =
    AI.getModule().lookUpFunctionInWitnessTable(WMI->getConformance(),
                                                WMI->getMember());

  if (!F)
    return std::make_pair(nullptr, FullApplySite());

  if (AI.getFunction()->isFragile()) {
    // function_ref inside fragile function cannot reference a private or
    // hidden symbol.
    if (!F->hasValidLinkageForFragileRef())
      return std::make_pair(nullptr, FullApplySite());
  }

  auto Result = devirtualizeWitnessMethod(AI, F, Subs);
  return std::make_pair(Result.getInstruction(), Result);
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

/// Attempt to devirtualize the given apply if possible, and return a
/// new instruction in that case, or nullptr otherwise.
DevirtualizationResult
swift::tryDevirtualizeApply(FullApplySite AI, ClassHierarchyAnalysis *CHA) {
  DEBUG(llvm::dbgs() << "    Trying to devirtualize: " << *AI.getInstruction());

  // Devirtualize apply instructions that call witness_method instructions:
  //
  //   %8 = witness_method $Optional<UInt16>, #LogicValue.boolValue!getter.1
  //   %9 = apply %8<Self = CodeUnit?>(%6#1) : ...
  //
  if (isa<WitnessMethodInst>(AI.getCallee()))
    return tryDevirtualizeWitnessMethod(AI);

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
      return tryDevirtualizeClassMethod(AI, Instance);

    // Try to check if the exact dynamic type of the instance is statically
    // known.
    if (auto Instance = getInstanceWithExactDynamicType(CMI->getOperand(),
                                                        CMI->getModule(),
                                                        CHA))
      return tryDevirtualizeClassMethod(AI, Instance);
  }

  if (isa<SuperMethodInst>(AI.getCallee())) {
    if (AI.hasSelfArgument()) {
      return tryDevirtualizeClassMethod(AI, AI.getSelfArgument());
    }

    // It is an invocation of a class method.
    // Last operand is the metatype that should be used for dispatching.
    return tryDevirtualizeClassMethod(AI, AI.getArguments().back());
  }

  return std::make_pair(nullptr, FullApplySite());
}
