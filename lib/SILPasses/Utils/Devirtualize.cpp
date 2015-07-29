//===-- Devirtualize.cpp - Helper for devirtualizing apply ------*- C++ -*-===//
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

#define DEBUG_TYPE "sil-devirtualize-utility"
#include "swift/SILPasses/Utils/Devirtualize.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILPasses/Utils/Local.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Casting.h"
using namespace swift;

STATISTIC(NumClassDevirt, "Number of class_method applies devirtualized");
STATISTIC(NumWitnessDevirt, "Number of witness_method applies devirtualized");

//===----------------------------------------------------------------------===//
//                         Class Method Optimization
//===----------------------------------------------------------------------===//

// Attempt to get the constructor for S, returning a null SILValue()
// if we cannot find it.
static SILValue getConstructor(SILValue S) {
  S = S.stripCasts();

  if (isa<AllocRefInst>(S) || isa<MetatypeInst>(S))
    return S;

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
bool swift::isClassWithUnboundGenericParameters(SILType C, SILModule &M) {
  auto *CD = C.getClassOrBoundGenericClass();
  if (CD && CD->getGenericSignature()) {
    auto InstanceTypeSubsts =
        C.gatherAllSubstitutions(M);

    if (!InstanceTypeSubsts.empty()) {
      if (hasUnboundGenericTypes(InstanceTypeSubsts))
        return true;
    }
  }

  if (C.hasArchetype())
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

  SILType FSelfSubstType;
  Module *Module = M.getSwiftModule();

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
        if (auto BoundTy = BoundBaseType->getAs<BoundGenericType>()) {
          ClassSubs = BoundTy->getSubstitutions(Module, nullptr);
        }
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
      AI.getCallee().getType().castTo<SILFunctionType>();

  CanType AISelfClass = AIGenCalleeType->getSelfParameter().getType();

  unsigned NextMethodParamIdx = 0;
  unsigned NumMethodParams = 0;
  if (AIGenCalleeType->isPolymorphic()) {
    NextMethodParamIdx = 0;
    // Generic parameters of the method start after generic parameters
    // of the instance class.
    if (auto AISelfClassSig =
            AISelfClass.getClassBound()->getGenericSignature()) {
      NextMethodParamIdx = AISelfClassSig->getGenericParams().size();
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

static SILFunction *getTargetClassMethod(SILModule &M,
                                         SILType ClassOrMetatypeType,
                                         SILDeclRef Member) {
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

  // Bail if any generic types parameters of the class instance type are
  // unbound.
  // We cannot devirtualize unbound generic calls yet.
  if (isClassWithUnboundGenericParameters(ClassOrMetatypeType, Mod))
    return false;

  // First attempt to lookup the origin for our class method. The origin should
  // either be a metatype or an alloc_ref.
  DEBUG(llvm::dbgs() << "        Origin Type: " << ClassOrMetatypeType);

  auto *CMI = cast<ClassMethodInst>(AI.getCallee());

  // Find the implementation of the member which should be invoked.
  auto *F = getTargetClassMethod(Mod, ClassOrMetatypeType, CMI->getMember());

  // If we do not find any such function, we have no function to devirtualize
  // to... so bail.
  if (!F) {
    DEBUG(llvm::dbgs() << "        FAIL: Could not find matching VTable or "
                          "vtable method for this class.\n");
    return false;
  }

  if (AI.getFunction()->isFragile()) {
    // function_ref inside fragile function cannot reference a private or
    // hidden symbol.
    if (!(F->isFragile() || isValidLinkageForFragileRef(F->getLinkage()) ||
          F->isExternalDeclaration()))
      return false;
  }

  CanSILFunctionType GenCalleeType = F->getLoweredFunctionType();

  auto Subs = getSubstitutionsForCallee(Mod, GenCalleeType,
                                        ClassOrMetatypeType, AI);

  // For polymorphic functions, bail if the number of substitutions is
  // not the same as the number of expected generic parameters.
  if (GenCalleeType->isPolymorphic()) {
    auto GenericSig = GenCalleeType->getGenericSignature();
    // Get the number of expected generic parameters, which
    // is a sum of the number of explicit generic parameters
    // and the number of their recursive member types exposed
    // through protocol requirements.
    auto DepTypes = GenericSig->getAllDependentTypes();
    unsigned ExpectedGenParamsNum = 0;

    for (auto DT: DepTypes) {
      (void)DT;
      ExpectedGenParamsNum++;
    }

    if (ExpectedGenParamsNum != Subs.size())
      return false;
  }

  return true;
}

/// Insert a cast for an address argument if its type is not the same
/// as the expected parameter type, which can happen with covariant
/// indirect return types and contravariant argument types.
static SILValue conditionallyCastAddr(SILBuilderWithScope<16> &B,
                                      SILLocation Loc, SILValue Arg,
                                      SILType ParamTy) {
  if (Arg.getType() == ParamTy)
    return Arg;

  if (Arg.getType().isAddress())
    return B.createUncheckedAddrCast(Loc, Arg, ParamTy);

  assert(Arg.getType().isHeapObjectReferenceType() &&
         "Expected address type or heap object reference for argument!");

  return B.createUncheckedRefCast(Loc, Arg, ParamTy);
}

/// Insert instructions to cast the tuple return type into appropiate
/// tuple type expected by the original apply_inst.
static SILInstruction *castTupleReturnType(FullApplySite AI, SILValue Value,
    CanTypeWrapper<TupleType> ResultTupleTy, SILFunction *F, SILBuilder& B) {
  auto AITupleTy = cast<TupleType>(AI.getType().getSwiftRValueType());
  SmallVector<SILValue, 4> TupleElements;
  auto TupleElementTypes = ResultTupleTy.getElementTypes();
  unsigned NumElements = ResultTupleTy->getElements().size();
  for (unsigned i = 0; i < NumElements; ++i) {
    auto EltTy = TupleElementTypes[i];
    auto ExtractedElt = B.createTupleExtract(AI.getLoc(), Value, i);
    OptionalTypeKind OTK;
    auto OptionalEltTy =
        EltTy.getCanonicalTypeOrNull()->getAnyOptionalObjectType(OTK);
    if (!OptionalEltTy
        || !isa<FunctionType>(OptionalEltTy.getCanonicalTypeOrNull())) {
      // No need to convert this parameter
      TupleElements.push_back(ExtractedElt);
      continue;
    }

    // Dereference the optional value
    auto *SomeDecl = B.getASTContext().getOptionalSomeDecl(OTK);
    auto FuncPtr = B.createUncheckedEnumData(AI.getLoc(), ExtractedElt,
        SomeDecl);

    auto AIOptionalEltTy =
        AITupleTy.getElementType(i).getCanonicalTypeOrNull()->getAnyOptionalObjectType();
    auto SILAIOptionalEltTy = AI.getModule().Types.getLoweredType(
        AIOptionalEltTy);
    auto ConvertedFuncPtr = B.createConvertFunction(AI.getLoc(), FuncPtr,
        SILAIOptionalEltTy);

    TupleElements.push_back(
        B.createOptionalSome(AI.getLoc(), ConvertedFuncPtr, OTK,
            SILType::getPrimitiveObjectType(AITupleTy.getElementType(i))));
  }

  // Now create a new tuple
  DEBUG(llvm::dbgs() << "        SUCCESS: " << F->getName() << "\n");
  NumClassDevirt++;
  return B.createTuple(AI.getLoc(), AI.getType(), TupleElements);
}

/// \brief Devirtualize an apply of a class method.
///
/// \p AI is the apply to devirtualize.
/// \p ClassOrMetatype is a class value or metatype value that is the
///    self argument of the apply we will devirtualize.
/// return the result value of the new ApplyInst if created one or null.
SILInstruction *swift::devirtualizeClassMethod(FullApplySite AI,
                                               SILValue ClassOrMetatype) {
  DEBUG(llvm::dbgs() << "    Trying to devirtualize : " << *AI.getInstruction());

  SILModule &Mod = AI.getModule();
  auto *CMI = cast<ClassMethodInst>(AI.getCallee());
  auto ClassOrMetatypeType = ClassOrMetatype.getType();
  auto *F = getTargetClassMethod(Mod, ClassOrMetatypeType, CMI->getMember());

  CanSILFunctionType GenCalleeType = F->getLoweredFunctionType();

  auto Subs = getSubstitutionsForCallee(Mod, GenCalleeType,
                                        ClassOrMetatypeType, AI);
  CanSILFunctionType SubstCalleeType = GenCalleeType;
  if (GenCalleeType->isPolymorphic())
    SubstCalleeType = GenCalleeType->substGenericArgs(Mod, Mod.getSwiftModule(), Subs);

  SILBuilderWithScope<16> B(AI.getInstruction());
  FunctionRefInst *FRI = B.createFunctionRef(AI.getLoc(), F);

  // Create the argument list for the new apply, casting when needed
  // in order to handle covariant indirect return types and
  // contravariant argument types.
  llvm::SmallVector<SILValue, 8> NewArgs;
  auto Args = AI.getArguments();
  auto ParamTypes = SubstCalleeType->getParameterSILTypes();

  for (unsigned i = 0, e = Args.size() - 1; i != e; ++i)
    NewArgs.push_back(conditionallyCastAddr(B, AI.getLoc(), Args[i],
                                            ParamTypes[i]));

  // Add the self argument, upcasting if required because we're
  // calling a base class's method.
  auto SelfParamTy = SubstCalleeType->getSelfParameter().getSILType();
  if (ClassOrMetatypeType == SelfParamTy)
    NewArgs.push_back(ClassOrMetatype);
  else
    NewArgs.push_back(B.createUpcast(AI.getLoc(), ClassOrMetatype,
                                     SelfParamTy));

  // If we have a direct return type, make sure we use the subst callee return
  // type. If we have an indirect return type, AI's return type of the empty
  // tuple should be ok.
  SILType ReturnType = AI.getType();
  if (!SubstCalleeType->hasIndirectResult()) {
    ReturnType = SubstCalleeType->getSILResult();
  }

  SILType SubstCalleeSILType =
    SILType::getPrimitiveObjectType(SubstCalleeType);
  FullApplySite NewAI;

  SILBasicBlock *ResultBB = nullptr;
  SILBasicBlock *NormalBB = nullptr;
  SILValue ResultValue;

  if (!isa<TryApplyInst>(AI)) {
    NewAI = B.createApply(AI.getLoc(), FRI, SubstCalleeSILType, ReturnType,
                          Subs, NewArgs);
    ResultValue = SILValue(NewAI.getInstruction(), 0);
  } else {
    auto *TAI = cast<TryApplyInst>(AI);
    // Always create a new BB for normal and error BBs.
    // This avoids creation of critical edges.
    ResultBB = B.getFunction().createBasicBlock();
    ResultBB->createBBArg(ReturnType);
    NormalBB = TAI->getNormalBB();

    auto *ErrorBB = B.getFunction().createBasicBlock();
    ErrorBB->createBBArg(TAI->getErrorBB()->getBBArg(0)->getType());

    NewAI = B.createTryApply(AI.getLoc(), FRI, SubstCalleeSILType,
                             Subs, NewArgs,
                             ResultBB, ErrorBB);
    // The result value is passed as a parameter to the normal block.
    ResultValue = ResultBB->getBBArg(0);
    B.setInsertionPoint(ErrorBB);
    B.createBranch(TAI->getLoc(), TAI->getErrorBB(),
                   {ErrorBB->getBBArg(0)});
    B.setInsertionPoint(ResultBB);
  }

  SILInstruction *CastedReturnValue = NewAI.getInstruction();

  if (ReturnType == AI.getType()) {
    DEBUG(llvm::dbgs() << "        SUCCESS: " << F->getName() << "\n");
    NumClassDevirt++;
    if (NormalBB) {
      B.createBranch(NewAI.getLoc(), NormalBB, { ResultBB->getBBArg(0) });
    }
    return CastedReturnValue;
  }

  // If our return type differs from AI's return type, then we know that we have
  // a covariant return type. Cast it before we RAUW. This can not happen


  // Accessors could return a tuple where one of the elements is of a function
  // type, which may refer to a subclass instead of a superclass in its
  // signature.
  // These function types are ABI-compatible between the subclass and a
  // superclass. To make the SIL type system happy, insert a conversion between
  // the function types and reconstruct the tuple.
  if (auto *FD = dyn_cast<FuncDecl>(CMI->getMember().getDecl())) {
    if (FD->isAccessor()) {
      if (auto ResultTupleTy = dyn_cast<TupleType>(ReturnType.getSwiftRValueType())) {
        assert(isa<ApplyInst>(NewAI) && "should be an apply_inst");
        CastedReturnValue = castTupleReturnType(AI, ResultValue, ResultTupleTy, F, B);
        if (NormalBB) {
          B.createBranch(NewAI.getLoc(), NormalBB, { CastedReturnValue });
        }
        return CastedReturnValue;
      }
    }
  }


  // Check if the return type is an optional of the apply_inst type
  // or the other way around
  bool UnwrapOptionalResult = false;
  bool WrapOptionalResult = false;
  OptionalTypeKind OTK;
  OptionalTypeKind AI_OTK;

  auto OptionalReturnType = ReturnType.getSwiftRValueType()
    .getAnyOptionalObjectType(OTK);

  auto OptionalAIType = AI.getType().getSwiftRValueType()
    .getAnyOptionalObjectType(AI_OTK);

  // Return type if not an optional, but the expected type is an optional
  // and the first one is the subclass of the second one.
  if (OptionalAIType && !OptionalReturnType &&
      SILType::getPrimitiveObjectType(OptionalAIType)
               .isSuperclassOf(ReturnType)) {
    // The devirtualized method returns a non-optional result.
    // We need to wrap it into an optional.
    auto OptType = OptionalType::get(AI_OTK,
                                     ReturnType.getSwiftRValueType()).
                                 getCanonicalTypeOrNull();
    ResultValue = B.createOptionalSome(AI.getLoc(), ResultValue,
                                       AI_OTK,
                                       SILType::getPrimitiveObjectType(OptType));
    OptionalReturnType = ReturnType.getSwiftRValueType();

    if (OptionalAIType == OptionalReturnType) {
      DEBUG(llvm::dbgs() << "        SUCCESS: " << F->getName() << "\n");
      NumClassDevirt++;
      CastedReturnValue = dyn_cast<SILInstruction>(ResultValue.getDef());
      if (NormalBB) {
        B.createBranch(NewAI.getLoc(), NormalBB, { CastedReturnValue });
        return NewAI.getInstruction();
      }
      return CastedReturnValue;
    }
  }

  if (OptionalReturnType && OptionalAIType &&
      SILType::getPrimitiveObjectType(OptionalAIType)
          .isSuperclassOf(
              SILType::getPrimitiveObjectType(OptionalReturnType))) {
    // Both types are optional and one of them is the superclass of the other.
    DEBUG(llvm::dbgs() << "        SUCCESS: " << F->getName() << "\n");
    NumClassDevirt++;
    CastedReturnValue = B.createUpcast(AI.getLoc(), ResultValue, AI.getType());
    if (NormalBB) {
      B.createBranch(NewAI.getLoc(), NormalBB, { CastedReturnValue });
      return NewAI.getInstruction();
    }
    return CastedReturnValue;
  }

  if (OptionalReturnType == AI.getType().getSwiftRValueType()) {
    UnwrapOptionalResult = true;
  }

  if (OptionalAIType == ReturnType.getSwiftRValueType()) {
    WrapOptionalResult = true;
  }

  assert((ReturnType.isAddress() ||
          ReturnType.isHeapObjectReferenceType() ||
          UnwrapOptionalResult || WrapOptionalResult) &&
         "Only addresses and refs can have their types changed due to "
         "covariant return types or contravariant argument types.");

  if (UnwrapOptionalResult) {
    // The devirtualized method returns an optional result.
    // We need to extract the actual result from the optional.
    auto *SomeDecl = B.getASTContext().getOptionalSomeDecl(OTK);
    CastedReturnValue = B.createUncheckedEnumData(AI.getLoc(),
                                                  ResultValue, SomeDecl);
  } else if (WrapOptionalResult) {
    // The devirtualized method returns a non-optional result.
    // We need to wrap it into an optional.
    CastedReturnValue = B.createOptionalSome(AI.getLoc(), ResultValue,
                                             AI_OTK, AI.getType());
  } else if (ReturnType.isAddress()) {
    CastedReturnValue = B.createUncheckedAddrCast(AI.getLoc(),
                                                  ResultValue, AI.getType());
  } else {
    CastedReturnValue = B.createUncheckedRefCast(AI.getLoc(),
                                                 ResultValue, AI.getType());
  }

  DEBUG(llvm::dbgs() << "        SUCCESS: " << F->getName() << "\n");
  NumClassDevirt++;
  if (NormalBB) {
    B.createBranch(NewAI.getLoc(), NormalBB, { CastedReturnValue });
    return NewAI.getInstruction();
  }
  return CastedReturnValue;
}

SILInstruction *swift::tryDevirtualizeClassMethod(FullApplySite AI,
                                             SILValue ClassInstance) {
  if (!canDevirtualizeClassMethod(AI, ClassInstance.getType()))
    return nullptr;
  return devirtualizeClassMethod(AI, ClassInstance);
}


//===----------------------------------------------------------------------===//
//                        Witness Method Optimization
//===----------------------------------------------------------------------===//

/// Generate a new apply of a function_ref to replace an apply of a
/// witness_method when we've determined the actual function we'll end
/// up calling.
static FullApplySite devirtualizeWitnessMethod(FullApplySite AI, SILFunction *F,
                                            ArrayRef<Substitution> Subs) {
  // We know the witness thunk and the corresponding set of substitutions
  // required to invoke the protocol method at this point.
  auto &Module = AI.getModule();

  // Collect all the required substitutions.
  //
  // The complete set of substitutions may be different, e.g. because the found
  // witness thunk F may have been created by a  specialization pass and have
  // additional generic parameters.
  SmallVector<Substitution, 16> NewSubstList(Subs.begin(), Subs.end());

  // Add the non-self-derived substitutions from the original application.
  for (auto &origSub : AI.getSubstitutionsWithoutSelfSubstitution())
    if (!origSub.getArchetype()->isSelfDerived())
      NewSubstList.push_back(origSub);

  // Figure out the exact bound type of the function to be called by
  // applying all substitutions.
  auto CalleeCanType = F->getLoweredFunctionType();
  auto SubstCalleeCanType = CalleeCanType->substGenericArgs(
    Module, Module.getSwiftModule(), NewSubstList);

  // Collect arguments from the apply instruction.
  auto Arguments = SmallVector<SILValue, 4>();

  auto ParamTypes = SubstCalleeCanType->getParameterSILTypes();
  // Type of the current parameter being processed
  auto ParamType = ParamTypes.begin();

  // Iterate over the non self arguments and add them to the
  // new argument list, upcasting when required.
  SILBuilderWithScope<8> B(AI.getInstruction());
  for (SILValue A : AI.getArguments()) {
    if (A.getType() != *ParamType)
      A = B.createUpcast(AI.getLoc(), A, *ParamType);

    Arguments.push_back(A);
    ++ParamType;
  }

  // Replace old apply instruction by a new apply instruction that invokes
  // the witness thunk.
  SILBuilderWithScope<2> Builder(AI.getInstruction());
  SILLocation Loc = AI.getLoc();
  FunctionRefInst *FRI = Builder.createFunctionRef(Loc, F);

  auto SubstCalleeSILType = SILType::getPrimitiveObjectType(SubstCalleeCanType);
  auto ResultSILType = SubstCalleeCanType->getSILResult();
  FullApplySite SAI;

  if (isa<ApplyInst>(AI))
    SAI = Builder.createApply(Loc, FRI, SubstCalleeSILType,
                                  ResultSILType, NewSubstList, Arguments);
  if (auto *TAI = dyn_cast<TryApplyInst>(AI))
    SAI = Builder.createTryApply(Loc, FRI, SubstCalleeSILType,
                                 NewSubstList, Arguments,
                                 TAI->getNormalBB(), TAI->getErrorBB());

  NumWitnessDevirt++;
  return SAI;
}

/// In the cases where we can statically determine the function that
/// we'll call to, replace an apply of a witness_method with an apply
/// of a function_ref, returning the new apply.
static SILInstruction *tryDevirtualizeWitnessMethod(FullApplySite AI) {
  SILFunction *F;
  ArrayRef<Substitution> Subs;
  SILWitnessTable *WT;

  auto *WMI = cast<WitnessMethodInst>(AI.getCallee());

  std::tie(F, WT, Subs) =
    AI.getModule().lookUpFunctionInWitnessTable(WMI->getConformance(),
                                                 WMI->getMember());

  if (!F)
    return nullptr;

  return devirtualizeWitnessMethod(AI, F, Subs).getInstruction();
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

/// Return the final class decl based on access control information.
static bool isKnownFinal(SILModule &M, SILDeclRef Member) {
  if (Member.isForeign)
    return false;

  const DeclContext *AssocDC = M.getAssociatedContext();
  if (!AssocDC)
    return false;

  FuncDecl *FD = Member.getFuncDecl();

  // FIXME: Handle other things like init().
  if (!FD)
    return false;

  assert(!FD->isFinal() && "Unexpected indirect call to final method!");

  // Only handle members defined within the SILModule's associated context.
  if (!FD->isChildContextOf(AssocDC))
    return false;

  if (FD->isDynamic() || FD->isOverridden())
    return false;

  // Only consider 'private' members, unless we are in whole-module compilation.
  switch (FD->getEffectiveAccess()) {
  case Accessibility::Public:
    return false;
  case Accessibility::Internal:
    return M.isWholeModule();
  case Accessibility::Private:
    return true;
  }
}

/// Attempt to devirtualize the given apply if possible, and return a
/// new instruction in that case, or nullptr otherwise.
SILInstruction *swift::tryDevirtualizeApply(FullApplySite AI) {
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
    // Check if the class member is known to be final.
    if (isKnownFinal(CMI->getModule(), CMI->getMember()))
      return tryDevirtualizeClassMethod(AI, CMI->getOperand());

    // Try to search for the point of construction.
    if (auto Constructor = getConstructor(CMI->getOperand()))
      return tryDevirtualizeClassMethod(AI, Constructor);
  }

  return nullptr;
}
