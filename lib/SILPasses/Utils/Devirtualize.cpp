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

STATISTIC(NumClassDevirt, "Number of calls devirtualized");
STATISTIC(NumWitnessDevirt, "Number of witness_method devirtualized");

//===----------------------------------------------------------------------===//
//                         Class Method Optimization
//===----------------------------------------------------------------------===//

// Is the value passed in actually an instruction that allows us to infer the
// correct dynamic type for the value?
bool isConstructor(SILValue S) {
  return isa<AllocRefInst>(S) || isa<MetatypeInst>(S);
}

/// Return bound generic type for the unbound type Superclass,
/// which is a superclass of a bound generic type BoundDerived
/// (Base may be also the same as BoundDerived).
static CanBoundGenericType bindSuperclass(CanType Superclass,
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
      return cast<BoundGenericType>(BoundSuperclass.getSwiftRValueType());

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
  return false;
}

static ArrayRef<Substitution>
getSubstitutionsForSuperclass(SILModule &M, CanSILFunctionType GenCalleeType,
                              SILType ClassInstanceType, ApplyInst *AI) {
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
        return BoundBaseType->getSubstitutions(Module, nullptr);
      }
    }
  }

  return AI->getSubstitutions();
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
bool swift::canDevirtualizeClassMethod(ApplyInst *AI,
                                       SILType ClassOrMetatypeType) {
  DEBUG(llvm::dbgs() << "    Trying to devirtualize : " << *AI);

  SILModule &Mod = AI->getModule();

  // Bail if any generic types parameters of the class instance type are
  // unbound.
  // We cannot devirtualize unbound generic calls yet.
  if (isClassWithUnboundGenericParameters(ClassOrMetatypeType, Mod))
    return false;

  // First attempt to lookup the origin for our class method. The origin should
  // either be a metatype or an alloc_ref.
  DEBUG(llvm::dbgs() << "        Origin Type: " << ClassOrMetatypeType);

  auto *CMI = cast<ClassMethodInst>(AI->getCallee());

  // Find the implementation of the member which should be invoked.
  auto *F = getTargetClassMethod(Mod, ClassOrMetatypeType, CMI->getMember());

  // If we do not find any such function, we have no function to devirtualize
  // to... so bail.
  if (!F) {
    DEBUG(llvm::dbgs() << "        FAIL: Could not find matching VTable or "
                          "vtable method for this class.\n");
    return false;
  }

  CanSILFunctionType GenCalleeType = F->getLoweredFunctionType();

  auto Subs = getSubstitutionsForSuperclass(Mod, GenCalleeType,
                                            ClassOrMetatypeType, AI);

  // For polymorphic functions, bail if the number of substitutions is
  // not the same as the number of expected generic parameters.
  if (GenCalleeType->isPolymorphic()) {
    auto GenericSig = GenCalleeType->getGenericSignature();
    auto CalleeGenericParamsNum = GenericSig->getGenericParams().size();
    if (CalleeGenericParamsNum != Subs.size())
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

/// \brief Devirtualize an apply of a class method.
///
/// \p AI is the apply to devirtualize.
/// \p ClassOrMetatype is a class value or metatype value that is the
///    self argument of the apply we will devirtualize.
/// return the new ApplyInst if created one or null.
ApplyInst *swift::devirtualizeClassMethod(ApplyInst *AI,
                                          SILValue ClassOrMetatype) {
  DEBUG(llvm::dbgs() << "    Trying to devirtualize : " << *AI);

  SILModule &Mod = AI->getModule();
  auto *CMI = cast<ClassMethodInst>(AI->getCallee());
  auto ClassOrMetatypeType = ClassOrMetatype.getType();
  auto *F = getTargetClassMethod(Mod, ClassOrMetatypeType, CMI->getMember());

  CanSILFunctionType GenCalleeType = F->getLoweredFunctionType();

  auto Subs = getSubstitutionsForSuperclass(Mod, GenCalleeType,
                                            ClassOrMetatypeType, AI);
  auto SubstCalleeType =
    GenCalleeType->substGenericArgs(Mod, Mod.getSwiftModule(), Subs);

  SILBuilderWithScope<16> B(AI);
  FunctionRefInst *FRI = B.createFunctionRef(AI->getLoc(), F);

  // Create the argument list for the new apply, casting when needed
  // in order to handle covariant indirect return types and
  // contravariant argument types.
  llvm::SmallVector<SILValue, 8> NewArgs;
  auto Args = AI->getArguments();
  auto ParamTypes = SubstCalleeType->getParameterSILTypes();

  for (unsigned i = 0, e = Args.size() - 1; i != e; ++i)
    NewArgs.push_back(conditionallyCastAddr(B, AI->getLoc(), Args[i],
                                            ParamTypes[i]));

  // Add the self argument, upcasting if required because we're
  // calling a base class's method.
  auto SelfParamTy = SubstCalleeType->getSelfParameter().getSILType();
  if (ClassOrMetatypeType == SelfParamTy)
    NewArgs.push_back(ClassOrMetatype);
  else
    NewArgs.push_back(B.createUpcast(AI->getLoc(), ClassOrMetatype,
                                     SelfParamTy));

  // If we have a direct return type, make sure we use the subst callee return
  // type. If we have an indirect return type, AI's return type of the empty
  // tuple should be ok.
  SILType ReturnType = AI->getType();
  if (!SubstCalleeType->hasIndirectResult()) {
    ReturnType = SubstCalleeType->getSILResult();
  }

  SILType SubstCalleeSILType =
    SILType::getPrimitiveObjectType(SubstCalleeType);
  ApplyInst *NewAI =
    B.createApply(AI->getLoc(), FRI, SubstCalleeSILType, ReturnType,
                  Subs, NewArgs);

  if (ReturnType == AI->getType()) {
    AI->replaceAllUsesWith(NewAI);
    AI->eraseFromParent();

    DEBUG(llvm::dbgs() << "        SUCCESS: " << F->getName() << "\n");
    NumClassDevirt++;
    return NewAI;
  }

  // If our return type differs from AI's return type, then we know that we have
  // a covariant return type. Cast it before we RAUW. This can not happen

  // Check if the return type is an optional of the apply_inst type
  // or the other way around
  bool UnwrapOptionalResult = false;
  OptionalTypeKind OTK;

  auto OptionalReturnType = ReturnType.getSwiftRValueType()
    .getAnyOptionalObjectType();
  if (OptionalReturnType == AI->getType().getSwiftRValueType()) {
    ReturnType.getSwiftRValueType().getAnyOptionalObjectType(OTK);
    UnwrapOptionalResult = true;
  }

  assert((ReturnType.isAddress() ||
          ReturnType.isHeapObjectReferenceType() ||
          UnwrapOptionalResult) &&
         "Only addresses and refs can have their types changed due to "
         "covariant return types or contravariant argument types.");

  SILValue CastedAI = NewAI;
  if (UnwrapOptionalResult) {
    // The devirtualized method returns an optional result.
    // We need to extract the actual result from the optional.
    auto *SomeDecl = B.getASTContext().getOptionalSomeDecl(OTK);
    CastedAI = B.createUncheckedEnumData(AI->getLoc(), NewAI, SomeDecl);
  } else if (ReturnType.isAddress()) {
    CastedAI = B.createUncheckedAddrCast(AI->getLoc(), NewAI, AI->getType());
  } else {
    CastedAI = B.createUncheckedRefCast(AI->getLoc(), NewAI, AI->getType());
  }
  SILValue(AI).replaceAllUsesWith(CastedAI);


  AI->eraseFromParent();

  DEBUG(llvm::dbgs() << "        SUCCESS: " << F->getName() << "\n");
  NumClassDevirt++;
  return NewAI;
}

/// This is a simplified version of devirtualizeClassMethod, which can
/// be called without the previously prepared DevirtClassMethodInfo.
ApplyInst *swift::tryDevirtualizeClassMethod(ApplyInst *AI,
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
static ApplyInst *devirtualizeWitnessMethod(ApplyInst *AI, SILFunction *F,
                                            ArrayRef<Substitution> Subs) {
  // We know the witness thunk and the corresponding set of substitutions
  // required to invoke the protocol method at this point.
  auto &Module = AI->getModule();

  // Collect all the required substitutions.
  //
  // The complete set of substitutions may be different, e.g. because the found
  // witness thunk F may have been created by a  specialization pass and have
  // additional generic parameters.
  SmallVector<Substitution, 16> NewSubstList(Subs.begin(), Subs.end());

  // Add the non-self-derived substitutions from the original application.
  for (auto &origSub : AI->getSubstitutionsWithoutSelfSubstitution())
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
  SILBuilderWithScope<8> B(AI);
  for (SILValue A : AI->getArguments()) {
    if (A.getType() != *ParamType)
      A = B.createUpcast(AI->getLoc(), A, *ParamType);

    Arguments.push_back(A);
    ++ParamType;
  }

  // Replace old apply instruction by a new apply instruction that invokes
  // the witness thunk.
  SILBuilderWithScope<2> Builder(AI);
  SILLocation Loc = AI->getLoc();
  FunctionRefInst *FRI = Builder.createFunctionRef(Loc, F);

  auto SubstCalleeSILType = SILType::getPrimitiveObjectType(SubstCalleeCanType);
  auto ResultSILType = SubstCalleeCanType->getSILResult();
  auto *SAI = Builder.createApply(Loc, FRI, SubstCalleeSILType,
                                  ResultSILType, NewSubstList, Arguments);

  auto *WMI = cast<WitnessMethodInst>(AI->getCallee());

  AI->replaceAllUsesWith(SAI);
  AI->eraseFromParent();

  if (WMI->use_empty())
    WMI->eraseFromParent();

  NumWitnessDevirt++;
  return SAI;
}

/// In the cases where we can statically determine the function that
/// we'll call to, replace an apply of a witness_method with an apply
/// of a function_ref, returning the new apply.
static ApplyInst *tryDevirtualizeWitnessMethod(ApplyInst *AI) {
  SILFunction *F;
  ArrayRef<Substitution> Subs;
  SILWitnessTable *WT;

  auto *WMI = cast<WitnessMethodInst>(AI->getCallee());

  std::tie(F, WT, Subs) =
    AI->getModule().lookUpFunctionInWitnessTable(WMI->getConformance(),
                                                 WMI->getMember());

  if (!F)
    return nullptr;

  return devirtualizeWitnessMethod(AI, F, Subs);
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
/// new apply in that case, or nullptr otherwise.
ApplyInst *swift::tryDevirtualizeApply(ApplyInst *AI) {
  DEBUG(llvm::dbgs() << "    Trying to devirtualize: " << *AI);

  // Devirtualize apply instructions that call witness_method instructions:
  //
  //   %8 = witness_method $Optional<UInt16>, #LogicValue.boolValue!getter.1
  //   %9 = apply %8<Self = CodeUnit?>(%6#1) : ...
  //
  if (isa<WitnessMethodInst>(AI->getCallee()))
    return tryDevirtualizeWitnessMethod(AI);

  /// Optimize a class_method and alloc_ref pair into a direct function
  /// reference:
  ///
  /// \code
  /// %XX = alloc_ref $Foo
  /// %YY = class_method %XX : $Foo, #Foo.get!1 : $@cc(method) @thin ...
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
  if (auto *CMI = dyn_cast<ClassMethodInst>(AI->getCallee())) {
    // Check if the class member is known to be final.
    if (isKnownFinal(CMI->getModule(), CMI->getMember()))
      return tryDevirtualizeClassMethod(AI, CMI->getOperand());

    // Try to search for the point of construction.
    if (isConstructor(CMI->getOperand().stripUpCasts()))
      return tryDevirtualizeClassMethod(AI, CMI->getOperand().stripUpCasts());
  }

  return nullptr;
}
