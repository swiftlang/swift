//===--- TypeCheckOverload.cpp - Overload Resolution ----------------------===//
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
// This file implements overload resolution.
//
//===----------------------------------------------------------------------===//
#include "TypeChecker.h"
#include "TypeCheckerOld.h"
#include "swift/AST/Attr.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Types.h"
using namespace swift;

// In TypeCheckCoercion.cpp.
CoercionResult isCoercibleToType(TypeChecker &tc, Expr *E, Type Ty,
                                 CoercionKind Kind, CoercionContext *CC);
bool isSameType(TypeChecker &tc, Type T1, Type T2,
                CoercionContext *CC = nullptr, bool Labeled = true);

static Expr *specializeOverloadResult(TypeChecker &TC, 
                                      const OverloadCandidate &Candidate,
                                      Expr *E);

static Identifier getFirstOverloadedIdentifier(const Expr *Fn) {
  if (const DeclRefExpr *DR = dyn_cast<DeclRefExpr>(Fn))
    return DR->getDecl()->getName();
  const OverloadSetRefExpr *ODR = cast<OverloadSetRefExpr>(Fn);
  auto Decls = ODR->getDecls();
  assert(!Decls.empty());
  return (*Decls.begin())->getName();
}

static bool displayOperandType(Type T) {
  return !T->isUnresolvedType() && !T->is<ErrorType>();
}

void printOverloadSetCandidates(TypeChecker &TC, ArrayRef<ValueDecl *> Candidates) {
  for (auto TheDecl : Candidates)
    TC.diagnose(TheDecl, diag::found_candidate);
}

void diagnoseEmptyOverloadSet(TypeChecker &TC, Expr *E,
                              ArrayRef<ValueDecl *> Candidates) {
  if (const BinaryExpr *BE = dyn_cast<BinaryExpr>(E)) {
    // FIXME: this feels a bit ad hoc with how we dig through the AST, and
    // it possibly makes assumptions that aren't true or I don't understand.
    // Some of this structure would feel nice to put back into the AST
    // itself.
    const Expr *Fn = BE->getFn();
    const TupleExpr *Arg = cast<TupleExpr>(BE->getArg());
    auto Elements = Arg->getElements();
    SourceLoc L = Fn->getLoc();
    Identifier I = getFirstOverloadedIdentifier(Fn);
    // Issue an error indicating the types of the operands, but only do
    // so if they are both unresolved types and not "error types".
    Type TypeA = Elements[0]->getType();
    Type TypeB = Elements[1]->getType();
    if (displayOperandType(TypeA) && displayOperandType(TypeB)) {
      TC.diagnose(L, diag::no_candidates_binop, I, TypeA, TypeB)
        .highlight(Elements[0]->getSourceRange())
        .highlight(Elements[1]->getSourceRange());
    }
    else {
      TC.diagnose(L, diag::no_candidates_op, 0, I)
        .highlight(Elements[0]->getSourceRange())
        .highlight(Elements[1]->getSourceRange());
    }
  }
  else if (isa<PrefixUnaryExpr>(E) || isa<PostfixUnaryExpr>(E)) {
    const ApplyExpr *UE = cast<ApplyExpr>(E);
    // FIXME: this feels a bit ad hoc with how we dig through the AST, and
    // it possibly makes assumptions that aren't true or I don't understand.
    // Some of this structure would feel nice to put back into the AST
    // itself.
    const Expr *Fn = UE->getFn();
    const Expr *Arg = UE->getArg();
    Identifier I = getFirstOverloadedIdentifier(Fn);
    // Issue a note indicating the types of the operand, but only do
    // so if it is a unresolved type.  Otherwise, the diagnostic is confusing.
    Type TypeArg = Arg->getType();
    if (displayOperandType(TypeArg)) {      
      TC.diagnose(Arg->getLoc(), diag::no_candidates_unary, I, TypeArg)
        .highlight(Arg->getSourceRange());
    }
    else {
      TC.diagnose(Arg->getLoc(), diag::no_candidates_op, 1, I)
        .highlight(Arg->getSourceRange());
    }
  }
  else {
    TC.diagnose(E->getLoc(), diag::no_candidates)
      .highlight(E->getSourceRange());
  }
  printOverloadSetCandidates(TC, Candidates);
}

static void
getDeducibleTypes(TypeChecker &TC, MutableArrayRef<Type> Types,
                  ArrayRef<GenericParam> GenericParams,
                  const GenericParamList *OuterGenericParams,
                 SmallVectorImpl<DeducibleGenericParamType *> &DeducibleParams){
  TypeSubstitutionMap PolySubstitutions;
  SmallVector<DeducibleGenericParamType *, 2> DeducibleTypes;
  DeducibleParams.reserve(GenericParams.size());

  // Collect the outer parameter lists.
  SmallVector<ArrayRef<GenericParam>, 2> paramLists;
  paramLists.push_back(GenericParams);
  for (const auto *gpList = OuterGenericParams; gpList;
         gpList = gpList->getOuterParameters()) {
    paramLists.push_back(gpList->getParams());
  }

  for (auto gpList = paramLists.rbegin(), gpListEnd = paramLists.rend();
       gpList != gpListEnd; ++gpList) {
    for (auto GP : *gpList) {
      TypeAliasDecl *TypeParam = GP.getAsTypeParam();
      auto Archetype = TypeParam->getDeclaredType()->getAs<ArchetypeType>();
      auto Deducible = DeducibleGenericParamType::getNew(TC.Context, nullptr,
                                                         Archetype);
      PolySubstitutions[Archetype] = Deducible;
      DeducibleParams.push_back(Deducible);
    }
  }
  
  for (unsigned I = 0, N = Types.size(); I != N; ++I) {
    Types[I] = TC.substType(Types[I], PolySubstitutions);
  }
}

/// \brief Form a version of the type \c T with all of the given generic
/// parameters replaced by deducible parameters.
///
/// \param DeducibleParams Will be populated with the synthesized deducible
/// generic parameters.
static Type
getDeducibleType(TypeChecker &TC, Type T, ArrayRef<GenericParam> GenericParams,
                 const GenericParamList *OuterGenericParams,
                 SmallVectorImpl<DeducibleGenericParamType *> &DeducibleParams){
  getDeducibleTypes(TC, MutableArrayRef<Type>(&T, 1), GenericParams,
                    OuterGenericParams,
                    DeducibleParams);
  return T;
}

static Type openPolymorphicType(TypeChecker &TC, Type T,
                                const GenericParamList &GenericParams,
                                CoercionContext &CC,
                                bool OnlyInnermostParams) {
  SmallVector<DeducibleGenericParamType *, 2> deducibleParams;
  T = getDeducibleType(TC, T, GenericParams.getParams(),
                       OnlyInnermostParams? nullptr
                                          : GenericParams.getOuterParameters(),
                       deducibleParams);
  CC.requestSubstitutionsFor(deducibleParams);
  return T;
}

static void openPolymorphicTypes(TypeChecker &TC, MutableArrayRef<Type> Types,
                                 const GenericParamList &GenericParams,
                                 CoercionContext &CC,
                                 bool OnlyInnermostParams) {
  SmallVector<DeducibleGenericParamType *, 2> deducibleParams;
  getDeducibleTypes(TC, Types, GenericParams.getParams(),
                    OnlyInnermostParams? nullptr
                                       : GenericParams.getOuterParameters(),
                    deducibleParams);
  CC.requestSubstitutionsFor(deducibleParams);
}

/// checkPolymorphicApply - Check the application of a function of the given
/// polymorphic type to a particular argument with, optionally, a destination
/// type.
OverloadCandidate checkPolymorphicApply(TypeChecker &TC,
                                        PolymorphicFunctionType *PolyFn,
                                        CoercionKind Kind,
                                        Expr *Arg,
                                        Type DestTy) {
  // Set up a coercion context to deduce the parameters to the polymorphic
  // function.
  CoercionContext CC(TC);
  AnyFunctionType *FunctionTy
    = openPolymorphicType(TC, PolyFn, PolyFn->getGenericParams(), CC,
                          /*OnlyInnermostParams=*/true)
        ->castTo<AnyFunctionType>();

  // Check whether arguments are suitable for this function.
  if (isCoercibleToType(TC, Arg, FunctionTy->getInput(), Kind, &CC)
        == CoercionResult::Failed)
    return OverloadCandidate();

  // Check whether we can coerce the result type.
  if (DestTy) { 
    OpaqueValueExpr OVE(Arg->getLoc(), FunctionTy->getResult());
    if (isCoercibleToType(TC, &OVE, DestTy, CoercionKind::Normal, &CC)
          == CoercionResult::Failed)
      return OverloadCandidate();
  }

  if (!CC.hasCompleteSubstitutions())
    return OverloadCandidate(nullptr, PolyFn, false);

  // We have a complete set of substitutions. Apply them.
  Type SubstFunctionTy = TC.substType(FunctionTy, CC.Substitutions);
  if (!SubstFunctionTy)
    return OverloadCandidate();
  TC.validateTypeSimple(SubstFunctionTy);

  FunctionTy = SubstFunctionTy->castTo<AnyFunctionType>();

  // We stared with a polymorphic function type, and have substituted away
  // all of the generic parameters. Turn it into the corresponding
  // monomorphic function type.
  FunctionTy = FunctionType::get(FunctionTy->getInput(),
                                 FunctionTy->getResult(),
                                 TC.Context);

  // FIXME: Check coercibility again.

  OverloadCandidate::SubstitutionInfoType SubstitutionInfo
    = { std::move(CC.Substitutions), std::move(CC.Conformance) };
  return OverloadCandidate(nullptr, FunctionTy, std::move(SubstitutionInfo));
}

OverloadCandidate checkPolymorphicUse(TypeChecker &TC, 
                                      PolymorphicFunctionType *PolyFn, Type DestTy,
                                      SourceLoc Loc,
                                      CoercionContext *CC) {
  // Set up a coercion context to deduce the parameters to the polymorphic
  // function.
  CoercionContext LocalCC(TC);
  AnyFunctionType *FunctionTy
    = openPolymorphicType(TC, PolyFn, PolyFn->getGenericParams(), LocalCC,
                          /*OnlyInnermostParams=*/true)
        ->castTo<AnyFunctionType>();

  auto DestLV = DestTy->getAs<LValueType>();
  if (DestLV)
    DestTy = DestLV->getObjectType();

  // Create a non-polymorphic function type against which we can perform
  // the match.
  Type SrcTy = FunctionType::get(FunctionTy->getInput(),
                                 FunctionTy->getResult(),
                                 TC.Context);

  // Match against the polymorphic type.
  if (DestLV) {
    // Check for trivial subtyping, to deduce any generic parameters
    // that require deduction. We'll do a specific type check later,
    // after substitution.
    if (!isSameType(TC, SrcTy, DestTy, &LocalCC))
      return OverloadCandidate();
  } else {
    OpaqueValueExpr OVE(Loc, SrcTy);
    if (isCoercibleToType(TC, &OVE, DestTy, CoercionKind::Normal, &LocalCC)
          == CoercionResult::Failed) {
      return OverloadCandidate();
    }
  }

  // Substitute deduced types.
  SrcTy = TC.substType(SrcTy, LocalCC.Substitutions);
  if (!SrcTy)
    return OverloadCandidate();

  // If we don't have a complete set of substitutions, this may match, but
  // we don't know yet.
  if (!LocalCC.hasCompleteSubstitutions()) {
    return OverloadCandidate(nullptr, SrcTy, /*Complete=*/false);
  }

  // If either the source or destination type is still unresolved,
  // we may need to deduce arguments.
  CoercionContext GlobalCC(TC);
  // Establish a context in which we will compute substitutions.
  if (CC && CC->requiresSubstitution()) {
    GlobalCC.Substitutions = CC->Substitutions;
    GlobalCC.Conformance = CC->Conformance;
  }

  // FIXME: Seems like this should only kick in when either destination or
  // source is still unresolved.
  if (GlobalCC.requiresSubstitution()) {
    // Deduce arguments.
    if (DestLV) {
      if (!isSameType(TC, SrcTy, DestTy, &GlobalCC))
        return OverloadCandidate();
    } else {
      OpaqueValueExpr OVE(Loc, SrcTy);
      if (isCoercibleToType(TC, &OVE, DestTy, CoercionKind::Normal, &GlobalCC)
            == CoercionResult::Failed) {
        return OverloadCandidate();
      }
    }

    // Substitute deductions.
    SrcTy = TC.substType(SrcTy, GlobalCC.Substitutions);
    if (!SrcTy)
      return OverloadCandidate();
    DestTy = TC.substType(DestTy, GlobalCC.Substitutions);
    if (!DestTy)
      return OverloadCandidate();

    // If we don't have a complete set of substitutions, we don't know if
    // this matches.
    if (!GlobalCC.hasCompleteSubstitutions()) {
      return OverloadCandidate(nullptr, SrcTy, /*Complete=*/false);
    }
  }

  if (DestLV) {
    // Simply checking for type equality suffices.
    if (!isSameType(TC, SrcTy, DestTy, &GlobalCC))
      return OverloadCandidate();
  } else {
    // Check that the source is coercible to the destination.
    OpaqueValueExpr OVE(Loc, SrcTy);
    if (isCoercibleToType(TC, &OVE, DestTy, CoercionKind::Normal, &GlobalCC)
          == CoercionResult::Failed) {
      return OverloadCandidate();
    }
  }


  TC.validateTypeSimple(SrcTy);
  OverloadCandidate::SubstitutionInfoType SubstitutionInfo
    = { std::move(LocalCC.Substitutions), std::move(LocalCC.Conformance) };
  return OverloadCandidate(nullptr, SrcTy, std::move(SubstitutionInfo));
}

static Type substBaseForGenericTypeMember(TypeChecker &TC, ValueDecl *VD,
                                          Type BaseTy,
                                          Type T,
                                          SourceLoc Loc,
                                          CoercionContext &CC,
                                          GenericParamList **OutGenericParams = nullptr) {
  // Dig out the instance type we're dealing with for the base.
  BaseTy = BaseTy->getRValueType();
  if (auto BaseMeta = BaseTy->getAs<MetaTypeType>())
    BaseTy = BaseMeta->getInstanceType();

  // Find the innermost generic parameters that apply to this member.
  auto owner = VD->getDeclContext();
  auto genericParams = owner->getGenericParamsOfContext();
  Type ownerTy;
  if (auto nominalOwner = dyn_cast<NominalTypeDecl>(owner))
    ownerTy = nominalOwner->getDeclaredTypeInContext();
  else {
    auto extensionOwner = cast<ExtensionDecl>(owner);
    auto extendedTy = extensionOwner->getExtendedType();
    if (auto nominal = extendedTy->getAs<NominalType>())
      ownerTy = nominal->getDecl()->getDeclaredTypeInContext();
    else if (auto unbound = extendedTy->getAs<UnboundGenericType>())
      ownerTy = unbound->getDecl()->getDeclaredTypeInContext();
    else
      llvm_unreachable("unknown owner for generic type member");
  }

  // Open up the owner type so we can deduce its arguments.
  Type openedTypes[2] = { ownerTy, T };
  openPolymorphicTypes(TC, openedTypes, *genericParams, CC,
                       /*OnlyInnermostParams=*/false);

  if (OutGenericParams)
    *OutGenericParams = genericParams;

  // Deduce the arguments.
  OpaqueValueExpr base(Loc, BaseTy);
  CoercionResult cr = isCoercibleToType(TC, &base, openedTypes[0],
                                        CoercionKind::Normal, &CC);
  if (cr == CoercionResult::Failed)
    return nullptr;

  assert(CC.hasCompleteSubstitutions() && "Incomplete substitution?");

  // Substitute the deduced arguments into the type T.
  T = TC.substType(openedTypes[1], CC.Substitutions);

  // We've already substituted through the parameters of the polymorphic
  // function type, so make it a monomorphic function type.
  if (auto polyFn = T->getAs<PolymorphicFunctionType>()) {
    T = FunctionType::get(polyFn->getInput(), polyFn->getResult(), TC.Context);
  }

  TC.validateTypeSimple(T);
  return T;
}

static bool substBaseForGenericTypeMember(TypeChecker &TC, 
                                          ValueDecl *VD,
                                          Type BaseTy,
                                          MutableArrayRef<Type> Types,
                                          SourceLoc Loc,
                                          CoercionContext &CC,
                                          GenericParamList **OutGenericParams = nullptr) {
  // Dig out the instance type we're dealing with for the base.
  BaseTy = BaseTy->getRValueType();
  if (auto BaseMeta = BaseTy->getAs<MetaTypeType>())
    BaseTy = BaseMeta->getInstanceType();

  // Find the innermost generic parameters that apply to this member.
  auto owner = cast<NominalTypeDecl>(VD->getDeclContext());
  auto genericParams = owner->getGenericParamsOfContext();
  Type ownerTy = owner->getDeclaredTypeInContext();

  // Open up the owner type so we can deduce its arguments.
  SmallVector<Type, 4> openedTypes;
  openedTypes.reserve(Types.size() + 1);
  openedTypes.append(Types.begin(), Types.end());
  openedTypes.push_back(ownerTy);
  openPolymorphicTypes(TC, openedTypes, *genericParams, CC,
                       /*OnlyInnermostParams=*/false);

  if (OutGenericParams)
    *OutGenericParams = genericParams;

  // Deduce the arguments.
  OpaqueValueExpr base(Loc, BaseTy);
  CoercionResult cr = isCoercibleToType(TC, &base, openedTypes.back(),
                                        CoercionKind::Normal, &CC);
  if (cr == CoercionResult::Failed)
    return true;

  assert(CC.hasCompleteSubstitutions() && "Incomplete substitution?");

  for (unsigned i = 0, n = Types.size(); i != n; ++i) {
    Type T = TC.substType(openedTypes[i], CC.Substitutions);

    // We've already substituted through the parameters of the polymorphic
    // function type, so make it a monomorphic function type.
    if (auto polyFn = T->getAs<PolymorphicFunctionType>()) {
      T = FunctionType::get(polyFn->getInput(), polyFn->getResult(), TC.Context);
    }

    Types[i] = T;
  }

  return false;
}

static bool lookPastThisArgument(Type BaseTy, ValueDecl *VD) {
  if (!BaseTy)
    return false;

  if (FuncDecl *FD = dyn_cast<FuncDecl>(VD)) {
    if (!BaseTy->is<MetaTypeType>())
      return true;
    if (FD->isStatic())
      return true;
  } else if (isa<OneOfElementDecl>(VD)) {
    return true;
  } else if (isa<ConstructorDecl>(VD)) {
    return true;
  }

  return false;
}

OverloadCandidate filterOverloadSet(TypeChecker &TC, ArrayRef<ValueDecl *> Candidates,
                                    bool OperatorSyntax,
                                    Type BaseTy,
                                    Expr *Arg,
                                    Type DestTy,
                                    SmallVectorImpl<ValueDecl *> &Viable) {
  SmallVector<OverloadCandidate, 4> ViableCandidates;
  for (ValueDecl *VD : Candidates) {
    Type VDType = VD->getTypeOfReference()->getRValueType();

    // Must have function type to be called.
    AnyFunctionType *FunctionTy = VDType->getAs<AnyFunctionType>();
    if (!FunctionTy)
      continue;

    // If we're referring to an instance method, constructor, or oneof element
    // within a generic type, we deduce the generic type's generic arguments
    // from the base type, then substitute those results through the rest of
    // the type.
    bool substitutedWithBase = false;
    if (auto nominalOwner = dyn_cast<NominalTypeDecl>(VD->getDeclContext())) {
      if (BaseTy && nominalOwner->getGenericParamsOfContext()) {
        if (isa<ConstructorDecl>(VD) || isa<OneOfElementDecl>(VD) ||
            isa<FuncDecl>(VD)) {
          CoercionContext InitialCC(TC);
          FunctionTy = substBaseForGenericTypeMember(TC, VD, BaseTy, FunctionTy,
                                                     Arg->getLoc(), InitialCC)
                           ->castTo<AnyFunctionType>();
          substitutedWithBase = true;
        }
      }
    }

    // If we have a 'this' argument and the declaration is a method,
    // the method's 'this' parameter has already been bound. Look instead at the
    // actual argument types.
    if (lookPastThisArgument(BaseTy, VD)) {
      // FIXME: Derived-to-base conversions will eventually be needed.
      if (auto resultFnTy = FunctionTy->getResult()->getAs<AnyFunctionType>())
        FunctionTy = resultFnTy;
      else
        continue;
    }

    if (!substitutedWithBase) {
      // Substitute into the type of this member, if indeed it is a member.
      Type SubstFunctionTy = TC.substMemberTypeWithBase(FunctionTy, VD, BaseTy);
      if (!SubstFunctionTy)
        continue;
      FunctionTy = SubstFunctionTy->castTo<AnyFunctionType>();
    }
    
    // Handle polymorphic functions.
    if (auto polyFn = FunctionTy->getAs<PolymorphicFunctionType>()) {
      CoercionKind Kind = CoercionKind::Normal;
      if (OperatorSyntax && VD->getAttrs().isAssignment())
        Kind = CoercionKind::Assignment;
      OverloadCandidate Candidate
        = checkPolymorphicApply(TC, polyFn, Kind, Arg, DestTy);
      if (!Candidate.getType())
        continue;

      Candidate.setDecl(VD);
      ViableCandidates.push_back(std::move(Candidate));
      continue;
    }

    
    // Establish the coercion context, which is required when this coercion
    // involves generic functions.
    CoercionContext CC(TC);
    // As a temporary hack, manually introduce the substitutions for operators
    // which are members of protocols.  (This will go away once we start using
    // PolymorphicFunctionTypes for protocol members.)
    // FIXME: Eliminate this hack.
    // FIXME: Extend this hack's replacement to operators in generic classes
    // and structs.
    SmallVector<DeducibleGenericParamType *, 1> operatorDeducibleParams;
    if (VD->getName().isOperator()) {
      if (Type Extension = cast<FuncDecl>(VD)->getExtensionType()) {
        FunctionTy = FunctionTy->getResult()->castTo<AnyFunctionType>();
        if (ProtocolType *P = Extension->getAs<ProtocolType>()) {
          GenericParam Param = P->getDecl()->getThis();
          FunctionTy = getDeducibleType(TC, FunctionTy, { &Param, 1 },
                                        nullptr, operatorDeducibleParams)
                         ->castTo<AnyFunctionType>();
          CC.requestSubstitutionsFor(operatorDeducibleParams);
        }
      }
    }

    // Check whether arguments are suitable for this function.
    CoercionKind Kind = CoercionKind::Normal;
    if (OperatorSyntax && VD->getAttrs().isAssignment())
      Kind = CoercionKind::Assignment;
    if (isCoercibleToType(TC, Arg, FunctionTy->getInput(), Kind, &CC)
          == CoercionResult::Failed)
      continue;

    // Check whether we can coerce the result type.
    if (DestTy) {
      OpaqueValueExpr OVE(Arg->getLoc(), FunctionTy->getResult());
      if (isCoercibleToType(TC, &OVE, DestTy, CoercionKind::Normal, &CC)
            == CoercionResult::Failed)
        continue;
    }

    // Add without substitutions.
    if (!CC.requiresSubstitution() || !CC.hasCompleteSubstitutions()) {
      ViableCandidates.push_back(
        OverloadCandidate(VD, FunctionTy, CC.hasCompleteSubstitutions()));
      continue;
    }

    // We have a complete set of substitutions. Apply them.
    Type SubstFunctionTy = TC.substType(FunctionTy, CC.Substitutions);
    if (!SubstFunctionTy)
      continue;

    FunctionTy = SubstFunctionTy->castTo<AnyFunctionType>();

    // We stared with a polymorphic function type, and have substituted away
    // all of the generic parameters. Turn it into the corresponding
    // monomorphic function type.
    FunctionTy = FunctionType::get(FunctionTy->getInput(),
                                   FunctionTy->getResult(),
                                   TC.Context);
    TC.validateTypeSimple(FunctionTy);

    if (!operatorDeducibleParams.empty()) {
      // For an operator found in a protocol, use the type inferred for 'This'
      // as the inferred base type.
      Type inferredBaseType = CC.Substitutions[operatorDeducibleParams.front()];

      // Only support this operation on archetypes.
      // FIXME: We may, at some point, allow this for 'extensions' protocols,
      // or when there are default function implementations in protocols.
      if (!inferredBaseType->is<ArchetypeType>())
        continue;

      ViableCandidates.push_back(OverloadCandidate(VD, FunctionTy,
                                                   inferredBaseType));
      continue;
    }

    // Add with substitutions.
    OverloadCandidate::SubstitutionInfoType SubstitutionInfo
      = { std::move(CC.Substitutions), std::move(CC.Conformance) };
    ViableCandidates.push_back(OverloadCandidate(VD, FunctionTy,
                                                 std::move(SubstitutionInfo)));
  }

  // If we found a fully-resolved a viable candidate, we're done.
  if (ViableCandidates.size() == 1 && ViableCandidates[0].isComplete())
    return std::move(ViableCandidates[0]);

  // Create the resulting viable-candidates vector.
  Viable.clear();
  for (auto const& VC : ViableCandidates)
    Viable.push_back(VC.getDecl());

  return OverloadCandidate();
}

OverloadCandidate filterOverloadSetForValue(TypeChecker &TC, 
                                            ArrayRef<ValueDecl *> Candidates,
                                            SourceLoc Loc,
                                            Type BaseTy,
                                            Type DestTy,
                                            SmallVectorImpl<ValueDecl *> &Viable,
                                            CoercionContext *CC) {
  SmallVector<OverloadCandidate, 4> ViableCandidates;

  auto DestLV = DestTy->getAs<LValueType>();
  if (DestLV)
    DestTy = DestLV->getObjectType();

  for (ValueDecl *VD : Candidates) {
    Type SrcTy = VD->getTypeOfReference();
    auto SrcLV = SrcTy->getAs<LValueType>();

    // Deal with destination lvalues.
    if (DestLV) {
      // If the source isn't an lvalue, skip it.
      // FIXME: Improve recovery.
      if (!SrcLV)
        continue;

      // If the source is more qualified than the destination, skip it.
      // FIXME: Improve recovery.
      if (SrcLV->getQualifiers() > DestLV->getQualifiers())
        continue;

      SrcTy = SrcLV->getObjectType();
    }
    
    // If we have a base type and the declaration is a constructor or
    // method, look past the 'this' argument.
    if (lookPastThisArgument(BaseTy, VD)) {
      // FIXME: Derived-to-base conversions will eventually be needed.
      SrcTy = SrcTy->castTo<AnyFunctionType>()->getResult();
    }

    // Substitute into the type of this member, if indeed it is a member.
    SrcTy = TC.substMemberTypeWithBase(SrcTy, VD, BaseTy);
    if (!SrcTy)
      continue;

    if (auto polyFn = SrcTy->getAs<PolymorphicFunctionType>()) {
      OverloadCandidate Candidate = checkPolymorphicUse(TC, polyFn, DestTy, Loc,
                                                        CC);

      if (!Candidate.getType())
        continue;

      Candidate.setDecl(VD);
      ViableCandidates.push_back(std::move(Candidate));
      continue;
    }

    // If either the source or destination type is still unresolved,
    // we may need to deduce arguments.
    CoercionContext GlobalCC(TC);
    // Establish a context in which we will compute substitutions.
    if (CC && CC->requiresSubstitution()) {
      GlobalCC.Substitutions = CC->Substitutions;
      GlobalCC.Conformance = CC->Conformance;
    }

    Type EffectiveDestTy = DestTy;
    // FIXME: Seems like this should only kick in when either destination or
    // source is still unresolved.
    if (GlobalCC.requiresSubstitution()) {
      // Deduce arguments.
      if (DestLV) {
        if (!isSameType(TC, SrcTy, DestTy, &GlobalCC))
          continue;
      } else {
        OpaqueValueExpr OVE(Loc, SrcTy);
        if (isCoercibleToType(TC, &OVE, DestTy, CoercionKind::Normal, &GlobalCC)
              == CoercionResult::Failed) {
          continue;
        }
      }

      // If we don't have a complete set of substitutions, we don't know if
      // this matches.
      if (!GlobalCC.hasCompleteSubstitutions()) {
        ViableCandidates.push_back(OverloadCandidate(VD, SrcTy, false));
        continue;
      }

      SrcTy = TC.substType(SrcTy, GlobalCC.Substitutions);
      if (!SrcTy)
        continue;
      EffectiveDestTy = TC.substType(EffectiveDestTy, GlobalCC.Substitutions);
      if (!EffectiveDestTy)
        continue;

      TC.validateTypeSimple(SrcTy);
      TC.validateTypeSimple(EffectiveDestTy);
    }

    if (DestLV) {
      // Simply checking for type equality suffices.
      if (!isSameType(TC, SrcTy, EffectiveDestTy, &GlobalCC))
        continue;
    } else {
    // Check that the source is coercible to the destination.
      OpaqueValueExpr OVE(Loc, SrcTy);
      if (isCoercibleToType(TC, &OVE, EffectiveDestTy, CoercionKind::Normal,
                            &GlobalCC)
            == CoercionResult::Failed) {
        continue;
      }
    }

    // Compute the type of the reference, which may be an lvalue.
    Type ResultTy = SrcTy;
    if (SrcLV)
      ResultTy = LValueType::get(ResultTy, SrcLV->getQualifiers(), TC.Context);
    ViableCandidates.push_back(OverloadCandidate(VD, ResultTy, true));
  }

  // If we found a fully-resolved a viable candidate, we're done.
  if (ViableCandidates.size() == 1 && ViableCandidates[0].isComplete())
    return std::move(ViableCandidates[0]);

  // Create the resulting viable-candidates vector.
  Viable.clear();
  for (auto const& VC : ViableCandidates)
    Viable.push_back(VC.getDecl());
  
  return OverloadCandidate();
}

/// \brief Replace the original semantics-providing expression (\c Orig) with
/// a new expression (\c New) within the given expression (\c E).
///
/// This also clears out the types for all of the 'sugar' expressions between
/// \c E and \c Orig, since they will need to be type-checked again.
static Expr *replaceSemanticsProvidingExpr(Expr *Orig, Expr *New, Expr *E) {
  if (Orig == E)
    return New;

  if (auto parens = dyn_cast<ParenExpr>(E)) {
    Expr *sub = replaceSemanticsProvidingExpr(Orig, New, parens->getSubExpr());
    parens->setSubExpr(sub);
    parens->setType(sub->getType());
    return E;
  }

  llvm_unreachable("Unhandled expression sugar");
}

static Expr *recheckTypes(TypeChecker &TC, Expr *E) {
  if (TC.typeCheckExpressionShallow(E))
    return nullptr;
  return E;
}

Expr *buildFilteredOverloadSet(TypeChecker &TC, OverloadedExpr Ovl,
                               ArrayRef<ValueDecl *> Remaining) {
  Expr *expr = Ovl.getExpr()->getSemanticsProvidingExpr();

  // Create an appropriate expression to refer to the remaining set of
  // declarations.
  Expr *result;
  if (auto DRE = dyn_cast<OverloadedDeclRefExpr>(expr)) {
    result = TC.buildRefExpr(Remaining, DRE->getLoc());
  } else if (auto MRE = dyn_cast<OverloadedMemberRefExpr>(expr)) {
    result = buildMemberRefExpr(TC, MRE->getBase(), MRE->getDotLoc(),
                                Remaining, MRE->getMemberLoc());
    if (!result)
      return nullptr;

    result = recheckTypes(TC, result);
    if (!result)
      return nullptr;
  } else {
    assert((isa<DeclRefExpr>(expr) || isa<MemberRefExpr>(expr) ||
            isa<ExistentialMemberRefExpr>(expr) ||
            isa<ArchetypeMemberRefExpr>(expr)) &&
           "Not a declaration reference expression");
    assert(Remaining.size() == 1);
    return Ovl.getExpr();
  }

  // Replace the semantics-providing expression with the newly-built result.
  return replaceSemanticsProvidingExpr(expr, result, Ovl.getExpr());
}

Expr * buildFilteredOverloadSet(TypeChecker &TC, OverloadedExpr Ovl,
                                const OverloadCandidate &Candidate){
  assert(Candidate.isComplete() && "Incomplete overload candidate!");

  Expr *expr = Ovl.getExpr()->getSemanticsProvidingExpr();

  Expr *result = nullptr;
  if (auto DRE = dyn_cast<OverloadedDeclRefExpr>(expr)) {
    result = new (TC.Context) DeclRefExpr(Candidate.getDecl(), DRE->getLoc(),
                                          Candidate.getDecl()->getTypeOfReference());
    result = recheckTypes(TC, result);
    if (!result)
      return nullptr;
  } else if (auto MRE = dyn_cast<OverloadedMemberRefExpr>(expr)) {
    SmallVector<ValueDecl *, 1> decls(1, Candidate.getDecl());
    result = recheckTypes(TC,
                          buildMemberRefExpr(TC, MRE->getBase(), MRE->getDotLoc(),
                                             decls, MRE->getMemberLoc()));
    if (!result)
      return nullptr;
  } else {
    assert((isa<DeclRefExpr>(expr) || isa<MemberRefExpr>(expr) ||
            isa<ExistentialMemberRefExpr>(expr) ||
            isa<ArchetypeMemberRefExpr>(expr)) &&
           "Not a declaration reference expression");
    result = expr;
  }

  result = specializeOverloadResult(TC, Candidate, result);

  // Replace the semantics-providing expression with the newly-built result.
  return replaceSemanticsProvidingExpr(expr, result, Ovl.getExpr());
}

Expr *buildCandidateRefExpr(TypeChecker &TC, const OverloadCandidate &Candidate,
                            SourceLoc NameLoc) {
  auto decl = Candidate.getDecl();
  Expr *result = new (TC.Context) DeclRefExpr(decl, NameLoc,
                                              decl->getTypeOfReference());

  return specializeOverloadResult(TC, Candidate, result);
}

static Expr *specializeOverloadResult(TypeChecker &TC, 
                                      const OverloadCandidate &Candidate,
                                      Expr *E) {
  if (Type baseTy = Candidate.getInferredBaseType()) {
    if (auto dre = dyn_cast<DeclRefExpr>(E)) {
      Expr *baseExpr
        = new (TC.Context) MetatypeExpr(nullptr, E->getLoc(),
                                        MetaTypeType::get(baseTy, TC.Context));
      ValueDecl *decl = dre->getDecl();
      E = buildMemberRefExpr(TC, baseExpr, SourceLoc(), { &decl, 1 },
                             dre->getLoc());
      if (!E)
        return nullptr;
      E = recheckTypes(TC, E);
      if (!E)
        return nullptr;
    }
  }

  if (Candidate.hasSubstitutions()) {
    E = TC.buildSpecializeExpr(E, Candidate.getType(),
                               Candidate.getSubstitutions(),
                               Candidate.getConformances(),
                               /*ArchetypesAreOpen=*/true,
                               /*OnlyInnermostParams=*/true);
  }

  return E;
}

Expr *buildMemberRefExpr(TypeChecker &TC, Expr *Base, SourceLoc DotLoc,
                         ArrayRef<ValueDecl *> Decls,
                         SourceLoc MemberLoc) {
  assert(!Decls.empty() && "Must have at least one declaration");
  
  // Figure out the actual base type, and whether we have an instance of that
  // type or its metatype.
  Type baseTy = Base->getType()->getRValueType();
  bool baseIsInstance = true;
  if (auto baseMeta = baseTy->getAs<MetaTypeType>()) {
    baseIsInstance = false;
    baseTy = baseMeta->getInstanceType();
  }

  // Check whether the first declaration is valid. If it is, they're all
  // potential candidates, because we don't allow overloading across different
  // classes of entities (e.g., variables and types cannot be overloaded).
  // If not, complain now.
  if (!baseIsInstance && isa<VarDecl>(Decls[0])) {
    TC.diagnose(MemberLoc, diag::member_ref_metatype_variable,
                Decls[0]->getName(), baseTy);
    TC.diagnose(Decls[0], diag::decl_declared_here, Decls[0]->getName());
    
    Expr *BadExpr = new (TC.Context) UnresolvedDotExpr(Base, DotLoc,
                                                       Decls[0]->getName(),
                                                       MemberLoc);
    BadExpr->setType(ErrorType::get(TC.Context));
    return BadExpr;
  }

  // If we have a single declaration, build an AST for it.
  if (Decls.size() == 1) {
    ValueDecl *Member = Decls[0];

    // Okay to refer to the member of an existential type.
    // FIXME: ExistentialMemberRefExpr needs to cope with a base of metatype
    // type.
    if (baseTy->isExistentialType()) {
      return new (TC.Context) ExistentialMemberRefExpr(Base, DotLoc, Member,
                                                       MemberLoc);
    }

    // Okay to refer to a member of an archetype.
    if (baseTy->is<ArchetypeType>()) {
      return new (TC.Context) ArchetypeMemberRefExpr(Base, DotLoc, Member,
                                                     MemberLoc);
    }

    // Reference to a member of a generic type.
    if (baseTy->isSpecialized()) {
      if (isa<FuncDecl>(Member) || isa<OneOfElementDecl>(Member) ||
          isa<ConstructorDecl>(Member)) {
        // We're binding a reference to an instance method of a generic
        // type, which we build as a reference to the underlying declaration
        // specialized based on the deducing the arguments of the generic
        // type.
        CoercionContext CC(TC);
        Type substTy = substBaseForGenericTypeMember(TC,
                                                     Member, baseTy,
                                                     Member->getTypeOfReference(),
                                                     MemberLoc, CC);
            // FIXME: Check for errors here?

        // Reference to the generic member.
        Expr *ref = new (TC.Context) DeclRefExpr(Member, MemberLoc,
                                                 Member->getTypeOfReference());

        // Specialize the member with the types deduced from the object
        // argument. This eliminates the genericity that comes from being
        // an instance method of a generic class.
        Expr *specializedRef
          = TC.buildSpecializeExpr(ref, substTy, CC.Substitutions,
                                   CC.Conformance,
                                   /*ArchetypesAreOpen=*/true,
                                   /*OnlyInnermostParams=*/false);

        Expr *apply;
        if (!baseIsInstance && Member->isInstanceMember()) {
          apply = new (TC.Context) DotSyntaxBaseIgnoredExpr(Base, DotLoc,
                                                            specializedRef);
        } else {
          assert((!baseIsInstance || Member->isInstanceMember()) &&
                 "can't call a static method on an instance");
          apply = new (TC.Context) DotSyntaxCallExpr(specializedRef, DotLoc, Base);
        }
        return recheckTypes(TC, apply);
      }

      return new (TC.Context) GenericMemberRefExpr(Base, DotLoc, Member,
                                                   MemberLoc);
    }

    // Refer to a member variable of an instance.
    if (!baseTy->is<ModuleType>()) {
      if (auto Var = dyn_cast<VarDecl>(Member)) {
        assert(baseIsInstance && "Referencing variable of metatype!");
        return new (TC.Context) MemberRefExpr(Base, DotLoc, Var, MemberLoc);
      }
    }

    Expr *Ref = new (TC.Context) DeclRefExpr(Member, MemberLoc,
                                             Member->getTypeOfReference());

    // Refer to a member function that binds 'this':
    if ((isa<FuncDecl>(Member) && Member->getDeclContext()->isTypeContext()) ||
        isa<OneOfElementDecl>(Member) || isa<ConstructorDecl>(Member)) {
      if (baseIsInstance == Member->isInstanceMember())
        return new (TC.Context) DotSyntaxCallExpr(Ref, DotLoc, Base);

      assert((!baseIsInstance || Member->isInstanceMember()) &&
             "can't call a static method on an instance");
    }

    return new (TC.Context) DotSyntaxBaseIgnoredExpr(Base, DotLoc, Ref);
  }

  // We have multiple declarations. Build an overloaded member reference.
  Decls = TC.Context.AllocateCopy(Decls);
  return new (TC.Context) OverloadedMemberRefExpr(Base, DotLoc, Decls, MemberLoc,
                         UnstructuredUnresolvedType::get(TC.Context));
}

Expr *buildMemberRefExpr(TypeChecker &TC, Expr *Base, SourceLoc DotLoc,
                         MemberLookup &Results,
                         SourceLoc MemberLoc) {
  assert(Results.isSuccess() && "Cannot build non-successful member reference");

  // If we have an ambiguous result, build an overload set.
  SmallVector<ValueDecl*, 8> ResultSet;
  for (MemberLookupResult X : Results.Results)
    ResultSet.push_back(X.D);

  return buildMemberRefExpr(TC, Base, DotLoc, ResultSet, MemberLoc);
}

