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

/// Return the dynamic class type of the value S, or nullptr if it
/// cannot be determined whether S has a class type or what type that
/// is.
static ClassDecl *getClassFromConstructor(SILValue S) {
  // First strip off casts.
  S = S.stripCasts();

  // Look for a a static ClassTypes in AllocRefInst or MetatypeInst.
  if (AllocRefInst *ARI = dyn_cast<AllocRefInst>(S))
    return ARI->getType().getClassOrBoundGenericClass();

  auto *MTI = dyn_cast<MetatypeInst>(S);
  if (!MTI)
    return nullptr;

  CanType instTy = MTI->getType().castTo<MetatypeType>().getInstanceType();
  return instTy.getClassOrBoundGenericClass();
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

/// \brief Check if it is possible to devirtualize an Apply instruction
/// and a class member obtained using the class_method instruction into
/// a direct call to a specific member of a specific class.
///
/// \p AI is the apply to devirtualize.
/// \p Member is the class member to devirtualize.
/// \p ClassInstance is the operand for the ClassMethodInst or an alternative
///    reference (such as downcasted class reference).
/// \p CD is the class declaration of the class, where the lookup of the member
///    should be performed.
/// \p DCMI is the devirt. class_method analysis result to be used for
///    performing the transformation.
/// return true if it is possible to devirtualize, false - otherwise.
bool swift::canDevirtualizeClassMethod(ApplyInst *AI,
                                       SILType ClassInstanceType,
                                       ClassDecl *CD,
                                       DevirtClassMethodInfo& DCMI) {
  DEBUG(llvm::dbgs() << "    Trying to devirtualize : " << *AI);

  auto *CMI = cast<ClassMethodInst>(AI->getCallee());
  auto Member = CMI->getMember();

  // First attempt to lookup the origin for our class method. The origin should
  // either be a metatype or an alloc_ref.
  DEBUG(llvm::dbgs() << "        Origin Type: " << ClassInstanceType);

  assert(CD && "Invalid class type");

  // Otherwise lookup from the module the least derived implementing method from
  // the module vtables.
  SILModule &Mod = AI->getModule();
  // Find the implementation of the member which should be invoked.
  SILFunction *F = Mod.lookUpFunctionInVTable(CD, Member);

  // If we do not find any such function, we have no function to devirtualize
  // to... so bail.
  if (!F) {
    DEBUG(llvm::dbgs() << "        FAIL: Could not find matching VTable or "
                          "vtable method for this class.\n");
    return false;
  }

  // Bail if any generic types parameters of the class instance type are
  // unbound.
  // We cannot devirtualize unbound generic calls yet.
  if (isClassWithUnboundGenericParameters(ClassInstanceType, AI->getModule()))
    return false;

  CanSILFunctionType GenCalleeType = F->getLoweredFunctionType();
  SILModule &M = F->getModule();

  auto Subs = getSubstitutionsForSuperclass(M, GenCalleeType,
                                            ClassInstanceType, AI);

  // For polymorphic functions, bail if the number of substitutions is
  // not the same as the number of expected generic parameters.
  if (GenCalleeType->isPolymorphic()) {
    auto GenericSig = GenCalleeType->getGenericSignature();
    auto CalleeGenericParamsNum = GenericSig->getGenericParams().size();
    if (CalleeGenericParamsNum != Subs.size())
      return false;
  }

  Module *Module = M.getSwiftModule();

  DCMI.F = F;
  DCMI.Substitutions = Subs;

  DCMI.SubstCalleeType =
    GenCalleeType->substGenericArgs(M, Module, DCMI.Substitutions);

  // If F's this pointer has a different type from CMI's operand and the
  // "this" pointer type is a super class of the CMI's operand, insert an
  // upcast.
  DCMI.ParamTypes =
    DCMI.SubstCalleeType->getParameterSILTypesWithoutIndirectResult();

  // We should always have a this pointer. Assert on debug builds, return
  // nullptr on release builds.
  assert(!DCMI.ParamTypes.empty() &&
         "Must have a this pointer when calling a class method inst.");

  return true;
}

/// \brief Devirtualize an Apply instruction and a class member obtained
/// using the class_method instruction into a direct call to a specific
/// member of a specific class.
///
/// \p AI is the apply to devirtualize.
/// \p Member is the class member to devirtualize.
/// \p ClassInstance is the operand for the ClassMethodInst or an alternative
///    reference (such as downcasted class reference).
/// \p DCMI is the devirtualization class_method analysis result to be used for
///    performing the transformation.
/// return the new ApplyInst if created one or null.
ApplyInst *swift::devirtualizeClassMethod(ApplyInst *AI,
                                          SILValue ClassInstance,
                                          DevirtClassMethodInfo& DCMI) {
  DEBUG(llvm::dbgs() << "    Trying to devirtualize : " << *AI);

  // Grab the self type from the function ref and the self type from the class
  // method inst.
  SILType FuncSelfTy = DCMI.ParamTypes[DCMI.ParamTypes.size() - 1];
  SILType OriginTy = ClassInstance.getType();
  SILBuilderWithScope<16> B(AI);

  // Then compare the two types and if they are unequal...
  if (FuncSelfTy != OriginTy) {
    if (ClassInstance.stripUpCasts().getType().getAs<MetatypeType>()) {
      auto &Module = AI->getModule();
      (void) Module;
      assert(FuncSelfTy.getMetatypeInstanceType(Module).
             isSuperclassOf(OriginTy.getMetatypeInstanceType(Module)) &&
             "Can not call a class method"
             " on a non-subclass of the class_methods class.");
    } else {
      assert(FuncSelfTy.isSuperclassOf(OriginTy) &&
             "Can not call a class method"
             " on a non-subclass of the class_methods class.");
    }
    // Otherwise, upcast origin to the appropriate type.
    ClassInstance = B.createUpcast(AI->getLoc(), ClassInstance, FuncSelfTy);
  }

  // Success! Perform the devirtualization.
  FunctionRefInst *FRI = B.createFunctionRef(AI->getLoc(), DCMI.F);

  // Construct a new arg list. First process all non-self operands, ref, addr
  // casting them to the appropriate types for F so that we allow for covariant
  // indirect return types and contravariant arguments.
  llvm::SmallVector<SILValue, 8> NewArgs;
  auto Args = AI->getArguments();
  auto allParamTypes = DCMI.SubstCalleeType->getParameterSILTypes();

  // For each old argument Op...
  for (unsigned i = 0, e = Args.size() - 1; i != e; ++i) {
    SILValue Op = Args[i];
    SILType OpTy = Op.getType();
    SILType FOpTy = allParamTypes[i];

    // If the type matches the type for the given parameter in F, just add it to
    // our arg list and continue.
    if (OpTy == FOpTy) {
      NewArgs.push_back(Op);
      continue;
    }

    // Otherwise we have either a covariant return type or a contravariant
    // argument type. Cast it appropriately.
    assert((OpTy.isAddress() || OpTy.isHeapObjectReferenceType()) &&
           "Only addresses and refs can have their types changed due to "
           "covariant return types or contravariant argument types.");

    // If OpTy is an address, perform an unchecked_addr_cast.
    if (OpTy.isAddress()) {
      NewArgs.push_back(B.createUncheckedAddrCast(AI->getLoc(), Op, FOpTy));
    } else {
      // Otherwise perform a ref cast.
      NewArgs.push_back(B.createUncheckedRefCast(AI->getLoc(), Op, FOpTy));
    }
  }

  // Add in self to the end.
  NewArgs.push_back(ClassInstance);

  // If we have a direct return type, make sure we use the subst callee return
  // type. If we have an indirect return type, AI's return type of the empty
  // tuple should be ok.
  SILType ReturnType = AI->getType();
  if (!DCMI.SubstCalleeType->hasIndirectResult()) {
    ReturnType = DCMI.SubstCalleeType->getSILResult();
  }

  SILType SubstCalleeSILType =
    SILType::getPrimitiveObjectType(DCMI.SubstCalleeType);
  ApplyInst *NewAI =
    B.createApply(AI->getLoc(), FRI, SubstCalleeSILType, ReturnType,
                  DCMI.Substitutions, NewArgs,
                  FRI->getReferencedFunction()->isTransparent());

  // If our return type differs from AI's return type, then we know that we have
  // a covariant return type. Cast it before we RAUW. This can not happen
  if (ReturnType != AI->getType()) {

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
  } else {
    AI->replaceAllUsesWith(NewAI);
  }

  AI->eraseFromParent();

  DEBUG(llvm::dbgs() << "        SUCCESS: " << DCMI.F->getName() << "\n");
  NumClassDevirt++;
  return NewAI;
}

/// This is a simplified version of devirtualizeClassMethod, which can
/// be called without the previously prepared DevirtClassMethodInfo.
ApplyInst *swift::devirtualizeClassMethod(ApplyInst *AI,
                                          SILValue ClassInstance,
                                          ClassDecl *CD) {
  DevirtClassMethodInfo DCMI;
  if (!canDevirtualizeClassMethod(AI, ClassInstance.getType(), CD, DCMI))
    return nullptr;
  return devirtualizeClassMethod(AI, ClassInstance, DCMI);
}


//===----------------------------------------------------------------------===//
//                        Witness Method Optimization
//===----------------------------------------------------------------------===//

/// Generate a new apply of a function_ref to replace an apply of a
/// witness_method when we've determined the actual function we'll end
/// up calling.
static ApplyInst *devirtualizeWitness(ApplyInst *AI, SILFunction *F,
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
                                  ResultSILType, NewSubstList, Arguments,
                                 FRI->getReferencedFunction()->isTransparent());
  AI->replaceAllUsesWith(SAI);
  AI->eraseFromParent();
  NumWitnessDevirt++;
  return SAI;
}

/// In the cases where we can statically determine the function that
/// we'll call to, replace an apply of a witness_method with an apply
/// of a function_ref, returning the new apply.
static ApplyInst *devirtualizeWitnessMethod(ApplyInst *AI,
                                            WitnessMethodInst *WMI) {
  SILFunction *F;
  ArrayRef<Substitution> Subs;
  SILWitnessTable *WT;

  std::tie(F, WT, Subs) =
    AI->getModule().lookUpFunctionInWitnessTable(WMI->getConformance(),
                                                 WMI->getMember());

  if (!F)
    return nullptr;

  return devirtualizeWitness(AI, F, Subs);
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

/// Return the final class decl based on access control information.
static ClassDecl *getClassFromAccessControl(ClassMethodInst *CMI) {
  const DeclContext *associatedDC = CMI->getModule().getAssociatedContext();
  if (!associatedDC) {
    // Without an associated context, we can't perform any access-based
    // optimizations.
    return nullptr;
  }

  SILDeclRef Member = CMI->getMember();
  FuncDecl *FD = Member.getFuncDecl();
  SILType ClassType = CMI->getOperand().stripUpCasts().getType();
  ClassDecl *CD = ClassType.getClassOrBoundGenericClass();

  // Only handle valid non-dynamic non-overridden members.
  if (!CD || !FD || FD->isInvalid() || FD->isDynamic() || FD->isOverridden())
    return nullptr;

  // Only handle members defined within the SILModule's associated context.
  if (!FD->isChildContextOf(associatedDC))
    return nullptr;

  if (!FD->hasAccessibility())
    return nullptr;

  // Only consider 'private' members, unless we are in whole-module compilation.
  switch (FD->getAccessibility()) {
  case Accessibility::Public:
    return nullptr;
  case Accessibility::Internal:
    if (!CMI->getModule().isWholeModule())
      return nullptr;
    break;
  case Accessibility::Private:
    break;
  }

  Type selfTypeInMember = FD->getDeclContext()->getDeclaredTypeInContext();
  return selfTypeInMember->getClassOrBoundGenericClass();
}

/// Attempt to devirtualize the given apply if possible, and return a
/// new apply in that case, or nullptr otherwise.
ApplyInst *swift::devirtualizeApply(ApplyInst *AI) {
  DEBUG(llvm::dbgs() << "    Trying to devirtualize: " << *AI);

  // Devirtualize apply instructions that call witness_method instructions:
  //
  //   %8 = witness_method $Optional<UInt16>, #LogicValue.boolValue!getter.1
  //   %9 = apply %8<Self = CodeUnit?>(%6#1) : ...
  //
  if (auto *AMI = dyn_cast<WitnessMethodInst>(AI->getCallee()))
    return devirtualizeWitnessMethod(AI, AMI);

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
    if (ClassDecl *C = getClassFromAccessControl(CMI))
      return devirtualizeClassMethod(AI, CMI->getOperand(), C);

    // Try to search for the point of construction.
    if (ClassDecl *C = getClassFromConstructor(CMI->getOperand()))
      return devirtualizeClassMethod(AI, CMI->getOperand().stripUpCasts(), C);
  }

  return nullptr;
}
