//===--- CSApply.cpp - Constraint Application -----------------------------===//
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
// This file implements application of a solution to a constraint
// system to a particular expression, resulting in a
// fully-type-checked expression.
//
//===----------------------------------------------------------------------===//

#include "ConstraintSystem.h"
#include "swift/AST/ArchetypeBuilder.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Attr.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/SaveAndRestore.h"

using namespace swift;
using namespace constraints;

/// \brief Retrieve the fixed type for the given type variable.
Type Solution::getFixedType(TypeVariableType *typeVar) const {
  auto knownBinding = typeBindings.find(typeVar);
  assert(knownBinding != typeBindings.end());
  return knownBinding->second;
}

Type Solution::computeSubstitutions(Type origType, DeclContext *dc,
                                    Type openedType,
                 SmallVectorImpl<Substitution> &substitutions) const {
  auto &tc = getConstraintSystem().getTypeChecker();
  auto &ctx = tc.Context;

  // Gather the substitutions from archetypes to concrete types, found
  // by identifying all of the type variables in the original type
  // FIXME: It's unfortunate that we're using archetypes here, but we don't
  // have another way to map from type variables back to dependent types (yet);
  TypeSubstitutionMap typeSubstitutions;
  auto type = openedType.transform([&](Type type) -> Type {
                if (auto tv = dyn_cast<TypeVariableType>(type.getPointer())) {
                  auto archetype = tv->getImpl().getArchetype();
                  auto simplified = getFixedType(tv);
                  typeSubstitutions[archetype] = simplified;
                  return SubstitutedType::get(archetype, simplified,
                                              tc.Context);
                }

                return type;
              });

  auto currentModule = getConstraintSystem().DC->getParentModule();
  SmallVector<ProtocolConformance *, 4> currentConformances;

  ArrayRef<Requirement> requirements;
  if (auto genericFn = origType->getAs<GenericFunctionType>()) {
    requirements = genericFn->getRequirements();
  } else {
    requirements = dc->getDeclaredTypeOfContext()->getAnyNominal()
                     ->getGenericRequirements();
  }

  for (const auto &req : requirements) {
    // Drop requirements for parameters that have been constrained away to
    // concrete types.
    auto firstArchetype
      = ArchetypeBuilder::mapTypeIntoContext(dc, req.getFirstType())
        ->getAs<ArchetypeType>();
    if (!firstArchetype)
      continue;
    
    switch (req.getKind()) {
    case RequirementKind::Conformance:
      // If this is a protocol conformance requirement, get the conformance
      // and record it.
      if (auto protoType = req.getSecondType()->getAs<ProtocolType>()) {
        assert(firstArchetype
               == substitutions.back().Archetype && "Archetype out-of-sync");
        ProtocolConformance *conformance = nullptr;
        Type replacement = substitutions.back().Replacement;
        bool conforms = tc.conformsToProtocol(replacement,
                                              protoType->getDecl(),
                                              getConstraintSystem().DC,
                                              &conformance);
        assert((conforms ||
                replacement->isExistentialType() ||
                replacement->is<GenericTypeParamType>()) &&
               "Constraint system missed a conformance?");
        (void)conforms;

        assert(conformance ||
               replacement->isExistentialType() ||
               replacement->is<ArchetypeType>() ||
               replacement->is<GenericTypeParamType>());
        currentConformances.push_back(conformance);
        break;
      }
      break;

    case RequirementKind::SameType:
      // Same-type requirements aren't recorded in substitutions.
      break;

    case RequirementKind::WitnessMarker:
      // Flush the current conformances.
      if (!substitutions.empty()) {
        substitutions.back().Conformance
          = ctx.AllocateCopy(currentConformances);
        currentConformances.clear();
      }

      // Each value witness marker starts a new substitution.
      substitutions.push_back(Substitution());
      substitutions.back().Archetype = firstArchetype;
      substitutions.back().Replacement =
      tc.substType(currentModule, substitutions.back().Archetype,
                   typeSubstitutions);
      break;
    }
  }
  
  // Flush the final conformances.
  if (!substitutions.empty()) {
    substitutions.back().Conformance = ctx.AllocateCopy(currentConformances);
    currentConformances.clear();
  }

  return type;
}

/// \brief Find a particular named function witness for a type that conforms to
/// the given protocol.
///
/// \param tc The type check we're using.
///
/// \param dc The context in which we need a witness.
///
/// \param type The type whose witness to find.
///
/// \param proto The protocol to which the type conforms.
///
/// \param name The name of the requirement.
///
/// \param diag The diagnostic to emit if the protocol definition doesn't
/// have a requirement with the given name.
///
/// \returns The named witness.
static FuncDecl *findNamedWitness(TypeChecker &tc, DeclContext *dc,
                                  Type type, ProtocolDecl *proto,
                                  Identifier name,
                                  Diag<> diag) {
  // Find the named requirement.
  FuncDecl *requirement = nullptr;
  for (auto member : proto->getMembers()) {
    auto fd = dyn_cast<FuncDecl>(member);
    if (!fd || !fd->hasName())
      continue;

    if (fd->getName() == name) {
      requirement = fd;
      break;
    }
  }
  
  if (!requirement || requirement->isInvalid()) {
    tc.diagnose(proto->getLoc(), diag);
    return nullptr;
  }

  // Find the member used to satisfy the named requirement.
  ProtocolConformance *conformance = 0;
  bool conforms = tc.conformsToProtocol(type, proto, dc, &conformance);
  if (!conforms)
    return nullptr;

  // For an archetype, just return the requirement from the protocol. There
  // are no protocol conformance tables.
  if (type->is<ArchetypeType>()) {
    return requirement;
  }

  assert(conformance && "Missing conformance information");
  // FIXME: Dropping substitutions here.
  return cast<FuncDecl>(conformance->getWitness(requirement, &tc).getDecl());
}

/// Adjust the given type to become the self type when referring to
/// the given member.
static Type adjustSelfTypeForMember(Type baseTy, ValueDecl *member,
                                    bool IsDirectPropertyAccess,
                                    DeclContext *UseDC) {
  auto baseObjectTy = baseTy->getLValueOrInOutObjectType();
  if (auto func = dyn_cast<AbstractFunctionDecl>(member)) {
    // If 'self' is an inout type, turn the base type into an lvalue
    // type with the same qualifiers.
    auto selfTy = func->getType()->getAs<AnyFunctionType>()->getInput();
    if (selfTy->is<InOutType>()) {
      // Unless we're looking at a non-@mutating existential member.  In which
      // case, the member will be modeled as an inout but ExistentialMemberRef
      // and ArchetypeMemberRef want to take the base as an rvalue.
      if (auto *fd = dyn_cast<FuncDecl>(func))
        if (!fd->isMutating() &&
            (baseObjectTy->isExistentialType() ||
             baseObjectTy->is<ArchetypeType>()))
            return baseObjectTy;
      
      return InOutType::get(baseObjectTy);
    }

    // Otherwise, return the rvalue type.
    return baseObjectTy;
  }

  // If the base of the access is mutable, then we may be invoking a getter or
  // setter and the base needs to be mutable.
  if (auto *VD = dyn_cast<VarDecl>(member)) {
    if (VD->hasAccessorFunctions() && baseTy->is<InOutType>() &&
        !IsDirectPropertyAccess)
      return InOutType::get(baseObjectTy);
   
    // If the member is immutable in this context, the base is always an
    // unqualified baseObjectTy.
    if (!VD->isSettable(UseDC))
      return baseObjectTy;
  }
  
  // If the base of the subscript is mutable, then we may be invoking a mutable
  // getter or setter.
  if (isa<SubscriptDecl>(member) && !baseTy->hasReferenceSemantics() &&
      baseTy->is<InOutType>())
    return InOutType::get(baseObjectTy);
  
  // Accesses to non-function members in value types are done through an @lvalue
  // type.
  if (baseTy->is<InOutType>())
    return LValueType::get(baseObjectTy);
  
  // Accesses to members in values of reference type (classes, metatypes) are
  // always done through a the reference to self.  Accesses to value types with
  // a non-mutable self are also done through the base type.
  return baseTy;
}

/// Return true if a MemberReferenceExpr with the specified base and member in
/// the specified DeclContext should be implicitly marked as
/// "isDirectPropertyAccess".
static bool isImplicitDirectMemberReference(Expr *base, VarDecl *member,
                                            DeclContext *DC) {
  // Properties that have storage and accessors are frequently accessed through
  // accessors.  However, in the init and destructor methods for the type
  // immediately containing the property, accesses are done direct.
  if (auto *AFD_DC = dyn_cast<AbstractFunctionDecl>(DC))
    if (member->hasStorage() &&
        // In a ctor or dtor.
        (isa<ConstructorDecl>(AFD_DC) || isa<DestructorDecl>(AFD_DC)) &&

        // Ctor or dtor are for immediate class, not a derived class.
        AFD_DC->getParent()->getDeclaredTypeOfContext()->getCanonicalType() ==
          member->getDeclContext()->getDeclaredTypeOfContext()->getCanonicalType() &&

        // Is a "self.property" reference.
        isa<DeclRefExpr>(base) &&
        AFD_DC->getImplicitSelfDecl() == cast<DeclRefExpr>(base)->getDecl()) {
      // Access this directly instead of going through (e.g.) observing or
      // trivial accessors.
      return true;
    }

  // If the value is always directly accessed from this context, do it.
  return member->isUseFromContextDirect(DC);
}

namespace {
  /// \brief Rewrites an expression by applying the solution of a constraint
  /// system to that expression.
  class ExprRewriter : public ExprVisitor<ExprRewriter, Expr *> {
  public:
    ConstraintSystem &cs;
    DeclContext *dc;
    const Solution &solution;
    
  private:
    /// \brief Coerce the given tuple to another tuple type.
    ///
    /// \param expr The expression we're converting.
    ///
    /// \param fromTuple The tuple type we're converting from, which is the same
    /// as \c expr->getType().
    ///
    /// \param toTuple The tuple type we're converting to.
    ///
    /// \param locator Locator describing where this tuple conversion occurs.
    ///
    /// \param sources The sources of each of the elements to be used in the
    /// resulting tuple, as provided by \c computeTupleShuffle.
    ///
    /// \param variadicArgs The source indices that are mapped to the variadic
    /// parameter of the resulting tuple, as provided by \c computeTupleShuffle.
    Expr *coerceTupleToTuple(Expr *expr, TupleType *fromTuple,
                             TupleType *toTuple,
                             ConstraintLocatorBuilder locator,
                             SmallVectorImpl<int> &sources,
                             SmallVectorImpl<unsigned> &variadicArgs);

    /// \brief Coerce the given scalar value to the given tuple type.
    ///
    /// \param expr The expression to be coerced.
    /// \param toTuple The tuple type to which the expression will be coerced.
    /// \param toScalarIdx The index of the scalar field within the tuple type
    /// \c toType.
    /// \param locator Locator describing where this conversion occurs.
    ///
    /// \returns The coerced expression, whose type will be equivalent to
    /// \c toTuple.
    Expr *coerceScalarToTuple(Expr *expr, TupleType *toTuple,
                              int toScalarIdx,
                              ConstraintLocatorBuilder locator);

    /// \brief Coerce the given value to existential type.
    ///
    /// \param expr The expression to be coerced.
    /// \param toType The tupe to which the expression will be coerced.
    /// \param locator Locator describing where this conversion occurs.
    ///
    /// \return The coerced expression, whose type will be equivalent to
    /// \c toType.
    Expr *coerceExistential(Expr *expr, Type toType,
                            ConstraintLocatorBuilder locator);

    /// \brief Coerce the expression to another type via a user-defined
    /// conversion.
    ///
    /// \param expr The expression to be coerced.
    /// \param toType The tupe to which the expression will be coerced.
    /// \param locator Locator describing where this conversion occurs.
    ///
    /// \return The coerced expression, whose type will be equivalent to
    /// \c toType.
    Expr *coerceViaUserConversion(Expr *expr, Type toType,
                                  ConstraintLocatorBuilder locator);

    /// \brief Coerce an expression of (possibly unchecked) optional
    /// type to have a different (possibly unchecked) optional type.
    Expr *coerceOptionalToOptional(Expr *expr, Type toType,
                                   ConstraintLocatorBuilder locator);

    /// \brief Coerce an expression of unchecked optional type to its
    /// underlying value type, in the correct way for an implicit
    /// look-through.
    Expr *coerceUncheckedOptionalToValue(Expr *expr, Type objTy,
                                         ConstraintLocatorBuilder locator);

  public:
    /// \brief Build a reference to the given declaration.
    Expr *buildDeclRef(ValueDecl *decl, SourceLoc loc, Type openedType,
                       ConstraintLocatorBuilder locator,
                       bool specialized, bool implicit,
                       bool isDirectPropertyAccess) {
      // Determine the declaration selected for this overloaded reference.
      auto &ctx = cs.getASTContext();

      // If this is a member of a nominal type, build a reference to the
      // member with an implied base type.
      if (decl->getDeclContext()->isTypeContext() && isa<FuncDecl>(decl)) {
        assert(isa<FuncDecl>(decl) && "Can only refer to functions here");
        assert(cast<FuncDecl>(decl)->isOperator() && "Must be an operator");
        auto openedFnType = openedType->castTo<FunctionType>();
        auto baseTy = simplifyType(openedFnType->getInput())
                        ->getRValueInstanceType();

        Expr * base = new (ctx) MetatypeExpr(nullptr, loc,
                                             MetatypeType::get(baseTy));

        return buildMemberRef(base, openedType, SourceLoc(), decl,
                              loc, openedFnType->getResult(),
                              locator, implicit, isDirectPropertyAccess);
      }

      // If this is a declaration with generic function type, build a
      // specialized reference to it.
      if (auto genericFn
            = decl->getInterfaceType()->getAs<GenericFunctionType>()) {
        auto dc = decl->getPotentialGenericDeclContext();

        SmallVector<Substitution, 4> substitutions;
        auto type = solution.computeSubstitutions(genericFn, dc, openedType,
                                                  substitutions);
        return new (ctx) DeclRefExpr(ConcreteDeclRef(ctx, decl, substitutions),
                                     loc, implicit, isDirectPropertyAccess,
                                     type);
      }

      auto type = simplifyType(openedType);
      return new (ctx) DeclRefExpr(decl, loc, implicit, isDirectPropertyAccess,
                                   type);
    }

    /// Describes an opened existential that has not yet been closed.
    struct OpenedExistential {
      /// The existential value being opened.
      Expr *ExistentialValue;

      /// The opaque value (of archetype type) stored within the
      /// existential.
      OpaqueValueExpr *OpaqueValue;
    };

    /// A mapping from archetype types that resulted from opening an
    /// existential to the opened existential. This mapping captures
    /// only those existentials that have been opened, but for which
    /// we have not yet created an \c OpenExistentialExpr.
    llvm::SmallDenseMap<ArchetypeType *, OpenedExistential> OpenedExistentials;

    /// Open an existential value into a new, opaque value of
    /// archetype type.
    ///
    /// \param base An expression of existential type whose value will
    /// be opened.
    ///
    /// \returns A pair (expr, type) that provides a reference to the value
    /// stored within the expression or its metatype (if the base was a
    /// metatype) and the new archetype that describes the dynamic type stored
    /// within the existential.
    std::tuple<Expr *, ArchetypeType *>
    openExistentialReference(Expr *base) {
      auto &tc = cs.getTypeChecker();
      base = tc.coerceToRValue(base);

      auto baseTy = base->getType()->getRValueType();
      bool isMetatype = false;
      if (auto metaTy = baseTy->getAs<AnyMetatypeType>()) {
        isMetatype = true;
        baseTy = metaTy->getInstanceType();
      }
      assert(baseTy->isAnyExistentialType() && "Type must be existential");

      // Create the archetype.
      SmallVector<ProtocolDecl *, 4> protocols;
      auto &ctx = tc.Context;
      baseTy->getAnyExistentialTypeProtocols(protocols);
      auto archetype = ArchetypeType::getOpened(baseTy);

      // Create the opaque opened value. If we started with a
      // metatype, it's a metatype.
      Type opaqueType = archetype;
      if (isMetatype)
        opaqueType = MetatypeType::get(archetype);
      auto archetypeVal = new (ctx) OpaqueValueExpr(base->getLoc(), opaqueType);
      archetypeVal->setUniquelyReferenced(true);

      // Record this opened existential.
      OpenedExistentials[archetype] = { base, archetypeVal };

      return std::make_tuple(archetypeVal, archetype);
    }

    /// Is the given function a constructor of a class or protocol?
    /// Such functions are subject to DynamicSelf manipulations.
    ///
    /// We want to avoid taking the DynamicSelf paths for other
    /// constructors for two reasons:
    ///   - it's an unnecessary cost
    ///   - optionality preservation has a problem with constructors on
    ///     optional types
    static bool isPolymorphicConstructor(AbstractFunctionDecl *fn) {
      if (!isa<ConstructorDecl>(fn))
        return false;
      DeclContext *parent = fn->getParent();
      if (auto extension = dyn_cast<ExtensionDecl>(parent))
        parent = extension->getExtendedType()->getAnyNominal();
      return (isa<ClassDecl>(parent) || isa<ProtocolDecl>(parent));
    }

    /// \brief Build a new member reference with the given base and member.
    Expr *buildMemberRef(Expr *base, Type openedFullType, SourceLoc dotLoc,
                         ValueDecl *member, SourceLoc memberLoc,
                         Type openedType, ConstraintLocatorBuilder locator,
                         bool Implicit, bool IsDirectPropertyAccess) {
      auto &tc = cs.getTypeChecker();
      auto &context = tc.Context;

      bool isSuper = base->isSuperExpr();

      Type baseTy = base->getType()->getRValueType();

      // Explicit member accesses are permitted to implicitly look
      // through UncheckedOptional<T>.
      if (!Implicit) {
        if (auto objTy = cs.lookThroughUncheckedOptionalType(baseTy)) {
          base = coerceUncheckedOptionalToValue(base, objTy, locator);
          if (!base) return nullptr;
          baseTy = objTy;
        }
      }

      // Figure out the actual base type, and whether we have an instance of
      // that type or its metatype.
      bool baseIsInstance = true;
      if (auto baseMeta = baseTy->getAs<AnyMetatypeType>()) {
        baseIsInstance = false;
        baseTy = baseMeta->getInstanceType();
      }

      // Produce a reference to the member, the type of the container it
      // resides in, and the type produced by the reference itself.
      Type containerTy;
      ConcreteDeclRef memberRef;
      Type refTy;
      Type dynamicSelfFnType;
      if (openedFullType->hasTypeVariable()) {
        // We require substitutions. Figure out what they are.

        // Figure out the declaration context where we'll get the generic
        // parameters.
        auto dc = member->getPotentialGenericDeclContext();

        // Build a reference to the generic member.
        SmallVector<Substitution, 4> substitutions;
        refTy = solution.computeSubstitutions(member->getInterfaceType(),
                                              dc,
                                              openedFullType,
                                              substitutions);

        memberRef = ConcreteDeclRef(context, member, substitutions);

        if (auto openedFullFnType = openedFullType->getAs<FunctionType>()) {
          auto openedBaseType = openedFullFnType->getInput()
                                  ->getRValueInstanceType();
          containerTy = solution.simplifyType(tc, openedBaseType);
        }
      } else {
        // No substitutions required; the declaration reference is simple.
        containerTy = member->getDeclContext()->getDeclaredTypeOfContext();
        memberRef = member;
        refTy = tc.getUnopenedTypeOfReference(member, Type(), dc,
                                              /*wantInterfaceType=*/true);
      }

      // If this is a method whose result type is dynamic Self, or a
      // construction, replace the result type with the actual object type.
      if (auto func = dyn_cast<AbstractFunctionDecl>(member)) {
        if ((isa<FuncDecl>(func) && cast<FuncDecl>(func)->hasDynamicSelf()) ||
            isPolymorphicConstructor(func)) {
          // For a DynamicSelf method on an existential, open up the
          // existential.
          if (func->getExtensionType()->is<ProtocolType>() &&
              baseTy->isAnyExistentialType()) {
            std::tie(base, baseTy) = openExistentialReference(base);
            containerTy = baseTy;
            openedType = openedType->replaceCovariantResultType(
                           baseTy,
                           func->getNumParamPatterns()-1);

            // The member reference is a specialized declaration
            // reference that replaces the Self of the protocol with
            // the existential type; change it to refer to the opened
            // archetype type.
            // FIXME: We should do this before we create the
            // specialized declaration reference, but that requires
            // redundant hasDynamicSelf checking.
            auto oldSubstitutions = memberRef.getSubstitutions();
            SmallVector<Substitution, 4> newSubstitutions(
                                           oldSubstitutions.begin(),
                                           oldSubstitutions.end());
            auto &selfSubst = newSubstitutions.front();
            assert(selfSubst.Archetype->getSelfProtocol() &&
                   "Not the Self archetype for a protocol?");
            selfSubst.Replacement = baseTy;
            unsigned numConformances = selfSubst.Conformance.size();
            auto newConformances
              = context.Allocate<ProtocolConformance *>(numConformances);
            std::fill(newConformances.begin(), newConformances.end(), nullptr);
            selfSubst.Conformance = newConformances;
            memberRef = ConcreteDeclRef(context, memberRef.getDecl(),
                                        newSubstitutions);
          }

          refTy = refTy->replaceCovariantResultType(containerTy,
                                                  func->getNumParamPatterns());
          dynamicSelfFnType = refTy->replaceCovariantResultType(
                                baseTy,
                                func->getNumParamPatterns());

          // If the type after replacing DynamicSelf with the provided base
          // type is no different, we don't need to perform a conversion here.
          if (refTy->isEqual(dynamicSelfFnType))
            dynamicSelfFnType = nullptr;
        }
      }

      // If we're referring to the member of a module, it's just a simple
      // reference.
      if (baseTy->is<ModuleType>()) {
        assert(!IsDirectPropertyAccess &&
               "Direct property access doesn't make sense for this");
        assert(!dynamicSelfFnType && "No reference type to convert to");
        Expr *ref = new (context) DeclRefExpr(memberRef, memberLoc, Implicit);
        ref->setType(refTy);
        return new (context) DotSyntaxBaseIgnoredExpr(base, dotLoc, ref);
      }

      // Otherwise, we're referring to a member of a type.

      // Is it an archetype or existential member?
      bool isArchetypeOrExistentialRef
            = isa<ProtocolDecl>(member->getDeclContext()) &&
              (baseTy->is<ArchetypeType>() || baseTy->isAnyExistentialType());

      // If we are referring to an optional member of a protocol.
      if (isArchetypeOrExistentialRef && member->getAttrs().isOptional()) {
        auto proto =tc.getProtocol(memberLoc, KnownProtocolKind::AnyObject);
        if (!proto)
          return nullptr;

        baseTy = proto->getDeclaredType();
      }

      // References to properties with accessors and storage usually go
      // through the accessors, but sometimes are direct.
      if (auto *VD = dyn_cast<VarDecl>(member))
        IsDirectPropertyAccess |= isImplicitDirectMemberReference(base, VD, dc);

      if (baseIsInstance) {
        // Convert the base to the appropriate container type, turning it
        // into an lvalue if required.
        Type selfTy;
        if (isArchetypeOrExistentialRef)
          selfTy = baseTy;
        else
          selfTy = containerTy;
        
        // If the base is already an lvalue with the right base type, we can
        // pass it as an inout qualified type.
        if (selfTy->isEqual(baseTy) && !selfTy->hasReferenceSemantics())
          if (base->getType()->is<LValueType>())
            selfTy = InOutType::get(selfTy);
        base = coerceObjectArgumentToType(
                 base,  selfTy, member, IsDirectPropertyAccess,
                 locator.withPathElement(ConstraintLocator::MemberRefBase));
      } else {
        // Convert the base to an rvalue of the appropriate metatype.
        base = coerceToType(base,
                            MetatypeType::get(isArchetypeOrExistentialRef
                                                ? baseTy
                                                : containerTy),
                            locator.withPathElement(
                              ConstraintLocator::MemberRefBase));
        if (!base)
          return nullptr;

        base = tc.coerceToRValue(base);
      }
      assert(base && "Unable to convert base?");

      // Handle archetype and existential references.
      if (isArchetypeOrExistentialRef) {
        assert(!IsDirectPropertyAccess &&
               "Direct property access doesn't make sense for this");
        assert(!dynamicSelfFnType && 
               "Archetype/existential DynamicSelf with extra conversion");

        Expr *ref;

        if (member->getAttrs().isOptional()) {
          base = tc.coerceToRValue(base);
          if (!base) return nullptr;
          ref = new (context) DynamicMemberRefExpr(base, dotLoc, memberRef,
                                                   memberLoc);
        } else {
          assert(!dynamicSelfFnType && "Converted type doesn't make sense here");
          ref = new (context) MemberRefExpr(base, dotLoc, memberRef,
                                            memberLoc, Implicit,
                                            IsDirectPropertyAccess);
          cast<MemberRefExpr>(ref)->setIsSuper(isSuper);
        }
        
        ref->setImplicit(Implicit);
        ref->setType(simplifyType(openedType));
        
        return ref;
      }

      // For types and properties, build member references.
      if (isa<TypeDecl>(member) || isa<VarDecl>(member)) {
        assert(!dynamicSelfFnType && "Converted type doesn't make sense here");
        auto result
          = new (context) MemberRefExpr(base, dotLoc, memberRef,
                                        memberLoc, Implicit,
                                        IsDirectPropertyAccess);
        result->setIsSuper(isSuper);

        // Skip the synthesized 'self' input type of the opened type.
        result->setType(simplifyType(openedType));
        return result;
      }
      
      assert(!IsDirectPropertyAccess &&
             "Direct property access doesn't make sense for this");
      
      // Handle all other references.
      Expr *ref = new (context) DeclRefExpr(memberRef, memberLoc, Implicit);
      ref->setType(refTy);

      // If the reference needs to be converted, do so now.
      if (dynamicSelfFnType) {
        ref = new (context) CovariantFunctionConversionExpr(ref,
                                                            dynamicSelfFnType);
      }

      ApplyExpr *apply;
      if (isa<ConstructorDecl>(member)) {
        // FIXME: Provide type annotation.
        apply = new (context) ConstructorRefCallExpr(ref, base);
      } else if (!baseIsInstance && member->isInstanceMember()) {
        // Reference to an unbound instance method.
        return new (context) DotSyntaxBaseIgnoredExpr(base, dotLoc, ref);
      } else {
        assert((!baseIsInstance || member->isInstanceMember()) &&
               "can't call a static method on an instance");
        apply = new (context) DotSyntaxCallExpr(ref, dotLoc, base);
      }
      return finishApply(apply, openedType, nullptr);
    }
    
    /// \brief Build a new dynamic member reference with the given base and
    /// member.
    Expr *buildDynamicMemberRef(Expr *base, SourceLoc dotLoc, ValueDecl *member,
                                SourceLoc memberLoc, Type openedType,
                                ConstraintLocatorBuilder locator) {
      auto &context = cs.getASTContext();

      // If we're specializing a polymorphic function, compute the set of
      // substitutions and form the member reference.
      Optional<ConcreteDeclRef> memberRef(member);
      if (auto func = dyn_cast<FuncDecl>(member)) {
        auto resultTy = func->getType()->castTo<AnyFunctionType>()->getResult();
        (void)resultTy;
        assert(!resultTy->is<PolymorphicFunctionType>() &&
               "Polymorphic function type slipped through");
      }

      // The base must always be an rvalue.
      base = cs.getTypeChecker().coerceToRValue(base);
      if (!base) return nullptr;
      if (auto objTy = cs.lookThroughUncheckedOptionalType(base->getType())) {
        base = coerceUncheckedOptionalToValue(base, objTy, locator);
        if (!base) return nullptr;
      }

      auto result = new (context) DynamicMemberRefExpr(base, dotLoc, *memberRef,
                                                       memberLoc);
      result->setType(simplifyType(openedType));
      return result;
    }

    /// \brief Describes either a type or the name of a type to be resolved.
    typedef llvm::PointerUnion<Identifier, Type> TypeOrName;

    /// \brief Convert the given literal expression via a protocol pair.
    ///
    /// This routine handles the two-step literal conversion process used
    /// by integer, float, character, and string literals. The first step
    /// uses \c protocol while the second step uses \c builtinProtocol.
    ///
    /// \param literal The literal expression.
    ///
    /// \param type The literal type. This type conforms to \c protocol,
    /// and may also conform to \c builtinProtocol.
    ///
    /// \param openedType The literal type as it was opened in the type system.
    ///
    /// \param protocol The protocol that describes the literal requirement.
    ///
    /// \param literalType Either the name of the associated type in
    /// \c protocol that describes the argument type of the conversion function
    /// (\c literalFuncName) or the argument type itself.
    ///
    /// \param literalFuncName The name of the conversion function requirement
    /// in \c protocol.
    ///
    /// \param builtinProtocol The "builtin" form of the protocol, which
    /// always takes builtin types and can only be properly implemented
    /// by standard library types. If \c type does not conform to this
    /// protocol, it's literal type will.
    ///
    /// \param builtinLiteralType Either the name of the associated type in
    /// \c builtinProtocol that describes the argument type of the builtin
    /// conversion function (\c builtinLiteralFuncName) or the argument type
    /// itself.
    ///
    /// \param builtinLiteralFuncName The name of the conversion function
    /// requirement in \c builtinProtocol.
    ///
    /// \param isBuiltinArgType Function that determines whether the given
    /// type is acceptable as the argument type for the builtin conversion.
    ///
    /// \param brokenProtocolDiag The diagnostic to emit if the protocol
    /// is broken.
    ///
    /// \param brokenBuiltinProtocolDiag The diagnostic to emit if the builtin
    /// protocol is broken.
    ///
    /// \returns the converted literal expression.
    Expr *convertLiteral(Expr *literal,
                         Type type,
                         Type openedType,
                         ProtocolDecl *protocol,
                         TypeOrName literalType,
                         Identifier literalFuncName,
                         ProtocolDecl *builtinProtocol,
                         TypeOrName builtinLiteralType,
                         Identifier builtinLiteralFuncName,
                         bool (*isBuiltinArgType)(Type),
                         Diag<> brokenProtocolDiag,
                         Diag<> brokenBuiltinProtocolDiag);

    /// \brief Finish a function application by performing the appropriate
    /// conversions on the function and argument expressions and setting
    /// the resulting type.
    ///
    /// \param apply The function application to finish type-checking, which
    /// may be a newly-built expression.
    ///
    /// \param openedType The "opened" type this expression had during
    /// type checking, which will be used to specialize the resulting,
    /// type-checked expression appropriately.
    ///
    /// \param locator The locator for the original expression.
    Expr *finishApply(ApplyExpr *apply, Type openedType,
                      ConstraintLocatorBuilder locator);

  private:
    /// \brief Retrieve the overload choice associated with the given
    /// locator.
    SelectedOverload getOverloadChoice(ConstraintLocator *locator) {
      return *getOverloadChoiceIfAvailable(locator);
    }

    /// \brief Retrieve the overload choice associated with the given
    /// locator.
    Optional<SelectedOverload>
    getOverloadChoiceIfAvailable(ConstraintLocator *locator) {
      auto known = solution.overloadChoices.find(locator);
      if (known != solution.overloadChoices.end())
        return known->second;

      return Nothing;
    }

    /// \brief Simplify the given type by substituting all occurrences of
    /// type variables for their fixed types.
    Type simplifyType(Type type) {
      return solution.simplifyType(cs.getTypeChecker(), type);
    }

  public:
    /// \brief Coerce the given expression to the given type.
    ///
    /// This operation cannot fail.
    ///
    /// \param expr The expression to coerce.
    /// \param toType The type to coerce the expression to.
    /// \param locator Locator used to describe where in this expression we are.
    ///
    /// \returns the coerced expression, which will have type \c ToType.
    Expr *coerceToType(Expr *expr, Type toType,
                       ConstraintLocatorBuilder locator);

    /// \brief Coerce the given object argument (e.g., for the base of a
    /// member expression) to the given type.
    ///
    /// \param expr The expression to coerce.
    ///
    /// \param baseTy The base type
    ///
    /// \param member The member being accessed.
    ///
    /// \param IsDirectPropertyAccess True if this is a direct access to
    ///        computed properties that have storage.
    ///
    /// \param locator Locator used to describe where in this expression we are.
    Expr *coerceObjectArgumentToType(Expr *expr,
                                     Type baseTy, ValueDecl *member,
                                     bool IsDirectPropertyAccess,
                                     ConstraintLocatorBuilder locator);

  private:
    /// \brief Build a new subscript.
    ///
    /// \param base The base of the subscript.
    /// \param index The index of the subscript.
    /// \param locator The locator used to refer to the subscript.
    Expr *buildSubscript(Expr *base, Expr *index,
                         ConstraintLocatorBuilder locator) {
      // Determine the declaration selected for this subscript operation.
      auto selected = getOverloadChoice(
                        cs.getConstraintLocator(
                          locator.withPathElement(
                            ConstraintLocator::SubscriptMember)));
      auto choice = selected.choice;
      auto subscript = cast<SubscriptDecl>(choice.getDecl());

      auto &tc = cs.getTypeChecker();
      auto baseTy = base->getType()->getRValueType();

      // Check whether the base is 'super'.
      bool isSuper = base->isSuperExpr();

      // Handle accesses that implicitly look through UncheckedOptional<T>.
      if (auto objTy = cs.lookThroughUncheckedOptionalType(baseTy)) {
        base = coerceUncheckedOptionalToValue(base, objTy, locator);
        if (!base) return nullptr;
      }

      // Figure out the index and result types.
      auto containerTy
        = subscript->getDeclContext()->getDeclaredTypeOfContext();
      auto subscriptTy = simplifyType(selected.openedType);
      auto indexTy = subscriptTy->castTo<AnyFunctionType>()->getInput();
      auto resultTy = subscriptTy->castTo<AnyFunctionType>()->getResult();

      // Coerce the index argument.
      index = coerceToType(index, indexTy,
                           locator.withPathElement(
                             ConstraintLocator::SubscriptIndex));
      if (!index)
        return nullptr;

      // Form the subscript expression.

      // Handle dynamic lookup.
      if (selected.choice.getKind() == OverloadChoiceKind::DeclViaDynamic ||
          subscript->getAttrs().isOptional()) {
        // If we've found an optional method in a protocol, the base type is
        // AnyObject.
        if (selected.choice.getKind() != OverloadChoiceKind::DeclViaDynamic) {
          auto proto = tc.getProtocol(index->getStartLoc(),
                                      KnownProtocolKind::AnyObject);
          if (!proto)
            return nullptr;

          baseTy = proto->getDeclaredType();
        }

        base = coerceObjectArgumentToType(base, baseTy, subscript, false,
                                          locator);
        if (!base)
          return nullptr;

        auto subscriptExpr = new (tc.Context) DynamicSubscriptExpr(base,
                                                                   index,
                                                                   subscript);
        subscriptExpr->setType(resultTy);
        return subscriptExpr;
      }

      // Handle subscripting of generics.
      if (subscript->getDeclContext()->isGenericContext()) {
        auto dc = subscript->getDeclContext();

        // Compute the substitutions used to reference the subscript.
        SmallVector<Substitution, 4> substitutions;
        solution.computeSubstitutions(subscript->getInterfaceType(),
                                      dc,
                                      selected.openedFullType,
                                      substitutions);

        // Convert the base.
        auto openedFullFnType = selected.openedFullType->castTo<FunctionType>();
        auto openedBaseType = openedFullFnType->getInput();
        containerTy = solution.simplifyType(tc, openedBaseType);
        base = coerceObjectArgumentToType(base, containerTy, subscript, false,
                                          locator);
                 locator.withPathElement(ConstraintLocator::MemberRefBase);
        if (!base)
          return nullptr;

        // Form the generic subscript expression.
        auto subscriptExpr
          = new (tc.Context) SubscriptExpr(base, index,
                                           ConcreteDeclRef(tc.Context,
                                                           subscript,
                                                           substitutions));
        subscriptExpr->setType(resultTy);
        subscriptExpr->setIsSuper(isSuper);
        return subscriptExpr;
      }

      Type selfTy = containerTy;
      if (selfTy->isEqual(baseTy) && !selfTy->hasReferenceSemantics())
        if (base->getType()->is<LValueType>())
          selfTy = InOutType::get(selfTy);

      // Coerce the base to the container type.
      base = coerceObjectArgumentToType(base, selfTy, subscript, false,
                                        locator);
      if (!base)
        return nullptr;

      // Form a normal subscript.
      auto *subscriptExpr
        = new (tc.Context) SubscriptExpr(base, index, subscript);
      subscriptExpr->setType(resultTy);
      subscriptExpr->setIsSuper(isSuper);
      return subscriptExpr;
    }

    /// \brief Build a new reference to another constructor.
    Expr *buildOtherConstructorRef(Type openedFullType,
                                   ConstructorDecl *ctor, SourceLoc loc,
                                   bool implicit) {
      auto &tc = cs.getTypeChecker();
      auto &ctx = tc.Context;

      // Compute the concrete reference.
      ConcreteDeclRef ref;
      Type resultTy;
      if (ctor->getInterfaceType()->is<GenericFunctionType>()) {
        // Compute the reference to the generic constructor.
        SmallVector<Substitution, 4> substitutions;
        resultTy = solution.computeSubstitutions(
                     ctor->getInterfaceType(),
                     ctor,
                     openedFullType,
                     substitutions);

        ref = ConcreteDeclRef(ctx, ctor, substitutions);

        // The constructor was opened with the allocating type, not the
        // initializer type. Map the former into the latter.
        auto resultFnTy = resultTy->castTo<FunctionType>();
        auto selfTy = resultFnTy->getInput()->getRValueInstanceType();
        if (!selfTy->hasReferenceSemantics())
          selfTy = InOutType::get(selfTy);

        resultTy = FunctionType::get(selfTy, resultFnTy->getResult(),
                                     resultFnTy->getExtInfo());
      } else {
        ref = ConcreteDeclRef(ctor);
        resultTy = ctor->getInitializerType();
      }

      // Build the constructor reference.
      Expr *refExpr = new (ctx) OtherConstructorDeclRefExpr(ref, loc, implicit,
                                                            resultTy);
      return refExpr;
    }
    
    TypeAliasDecl *MaxIntegerTypeDecl = nullptr;
    TypeAliasDecl *MaxFloatTypeDecl = nullptr;
    
  public:
    ExprRewriter(ConstraintSystem &cs, const Solution &solution)
      : cs(cs), dc(cs.DC), solution(solution) { }

    ConstraintSystem &getConstraintSystem() const { return cs; }

    /// \brief Simplify the expression type and return the expression.
    ///
    /// This routine is used for 'simple' expressions that only need their
    /// types simplified, with no further computation.
    Expr *simplifyExprType(Expr *expr) {
      auto toType = simplifyType(expr->getType());
      expr->setType(toType);
      return expr;
    }

    Expr *visitErrorExpr(ErrorExpr *expr) {
      // Do nothing with error expressions.
      return expr;
    }

    Expr *handleIntegerLiteralExpr(LiteralExpr *expr) {
      auto &tc = cs.getTypeChecker();
      ProtocolDecl *protocol
        = tc.getProtocol(expr->getLoc(),
                         KnownProtocolKind::IntegerLiteralConvertible);
      ProtocolDecl *builtinProtocol
        = tc.getProtocol(expr->getLoc(),
                         KnownProtocolKind::BuiltinIntegerLiteralConvertible);

      // For type-sugar reasons, prefer the spelling of the default literal
      // type.
      auto type = simplifyType(expr->getType());
      if (auto defaultType = tc.getDefaultType(protocol, dc)) {
        if (defaultType->isEqual(type))
          type = defaultType;
      }
      if (auto floatProtocol
            = tc.getProtocol(expr->getLoc(),
                             KnownProtocolKind::FloatLiteralConvertible)) {
        if (auto defaultFloatType = tc.getDefaultType(floatProtocol, dc)) {
          if (defaultFloatType->isEqual(type))
            type = defaultFloatType;
        }
      }

      // Find the maximum-sized builtin integer type.
      
      if(!MaxIntegerTypeDecl) {
        UnqualifiedLookup lookup(tc.Context.Id_MaxBuiltinIntegerType,
                                 tc.getStdlibModule(dc),
                                 &tc);
        MaxIntegerTypeDecl =
            dyn_cast_or_null<TypeAliasDecl>(lookup.getSingleTypeResult());
      }
      if (!MaxIntegerTypeDecl ||
          !MaxIntegerTypeDecl->getUnderlyingType()->is<BuiltinIntegerType>()) {
        tc.diagnose(expr->getLoc(), diag::no_MaxBuiltinIntegerType_found);
        return nullptr;
      }
      auto maxType = MaxIntegerTypeDecl->getUnderlyingType();

      return convertLiteral(
               expr,
               type,
               expr->getType(),
               protocol,
               tc.Context.Id_IntegerLiteralType,
               tc.Context.Id_ConvertFromIntegerLiteral,
               builtinProtocol,
               maxType,
               tc.Context.Id_ConvertFromBuiltinIntegerLiteral,
               nullptr,
               diag::integer_literal_broken_proto,
               diag::builtin_integer_literal_broken_proto);
    }
    
    Expr *visitIntegerLiteralExpr(IntegerLiteralExpr *expr) {
      return handleIntegerLiteralExpr(expr);
    }

    Expr *visitFloatLiteralExpr(FloatLiteralExpr *expr) {
      auto &tc = cs.getTypeChecker();
      ProtocolDecl *protocol
        = tc.getProtocol(expr->getLoc(),
                         KnownProtocolKind::FloatLiteralConvertible);
      ProtocolDecl *builtinProtocol
        = tc.getProtocol(expr->getLoc(),
                         KnownProtocolKind::BuiltinFloatLiteralConvertible);

      // For type-sugar reasons, prefer the spelling of the default literal
      // type.
      auto type = simplifyType(expr->getType());
      if (auto defaultType = tc.getDefaultType(protocol, dc)) {
        if (defaultType->isEqual(type))
          type = defaultType;
      }

      // Find the maximum-sized builtin float type.
      // FIXME: Cache name lookup.
      if (!MaxFloatTypeDecl) {
        UnqualifiedLookup lookup(tc.Context.Id_MaxBuiltinFloatType,
                                 tc.getStdlibModule(dc),
                                 &tc);
        MaxFloatTypeDecl =
          dyn_cast_or_null<TypeAliasDecl>(lookup.getSingleTypeResult());
      }
      if (!MaxFloatTypeDecl ||
          !MaxFloatTypeDecl->getUnderlyingType()->is<BuiltinFloatType>()) {
        tc.diagnose(expr->getLoc(), diag::no_MaxBuiltinFloatType_found);
        return nullptr;
      }
      auto maxType = MaxFloatTypeDecl->getUnderlyingType();

      return convertLiteral(
               expr,
               type,
               expr->getType(),
               protocol,
               tc.Context.Id_FloatLiteralType,
               tc.Context.Id_ConvertFromFloatLiteral,
               builtinProtocol,
               maxType,
               tc.Context.Id_ConvertFromBuiltinFloatLiteral,
               nullptr,
               diag::float_literal_broken_proto,
               diag::builtin_float_literal_broken_proto);
    }

    Expr *visitCharacterLiteralExpr(CharacterLiteralExpr *expr) {
      auto &tc = cs.getTypeChecker();
      ProtocolDecl *protocol
        = tc.getProtocol(expr->getLoc(),
                         KnownProtocolKind::CharacterLiteralConvertible);
      ProtocolDecl *builtinProtocol
        = tc.getProtocol(expr->getLoc(),
                         KnownProtocolKind::BuiltinCharacterLiteralConvertible);

      // For type-sugar reasons, prefer the spelling of the default literal
      // type.
      auto type = simplifyType(expr->getType());
      if (auto defaultType = tc.getDefaultType(protocol, dc)) {
        if (defaultType->isEqual(type))
          type = defaultType;
      }

      return convertLiteral(
               expr,
               type,
               expr->getType(),
               protocol,
               tc.Context.Id_CharacterLiteralType,
               tc.Context.Id_ConvertFromCharacterLiteral,
               builtinProtocol,
               Type(BuiltinIntegerType::get(32, tc.Context)),
               tc.Context.Id_ConvertFromBuiltinCharacterLiteral,
               [] (Type type) -> bool {
                 if (auto builtinInt = type->getAs<BuiltinIntegerType>()) {
                   return builtinInt->isFixedWidth(32);
                 }
                 return false;
               },
               diag::character_literal_broken_proto,
               diag::builtin_character_literal_broken_proto);
    }

    Expr *handleStringLiteralExpr(LiteralExpr *expr) {
      auto stringLiteral = dyn_cast<StringLiteralExpr>(expr);
      auto magicLiteral = dyn_cast<MagicIdentifierLiteralExpr>(expr);
      assert(bool(stringLiteral) != bool(magicLiteral) &&
             "literal must be either a string literal or a magic literal");

      auto &tc = cs.getTypeChecker();
      ProtocolDecl *protocol
        = tc.getProtocol(expr->getLoc(),
                         KnownProtocolKind::StringLiteralConvertible);

      // For type-sugar reasons, prefer the spelling of the default literal
      // type.
      auto type = simplifyType(expr->getType());
      if (auto defaultType = tc.getDefaultType(protocol, dc)) {
        if (defaultType->isEqual(type))
          type = defaultType;
      }
      
      TupleTypeElt elementsArray[3] = {
        TupleTypeElt(tc.Context.TheRawPointerType),
        TupleTypeElt(BuiltinIntegerType::getWordType(tc.Context)),
        TupleTypeElt(BuiltinIntegerType::get(1, tc.Context))
      };

      Identifier CFSLID = tc.Context.Id_ConvertFromStringLiteral;


      // If the type can handle UTF-16 string literals, prefer them.
      Identifier CFBSLID;
      ProtocolDecl *builtinProtocol
        = tc.getProtocol(
            expr->getLoc(),
            KnownProtocolKind::BuiltinUTF16StringLiteralConvertible);
      ArrayRef<TupleTypeElt> elements;
      if (tc.conformsToProtocol(type, builtinProtocol, cs.DC)) {
        CFBSLID = tc.Context.Id_ConvertFromBuiltinUTF16StringLiteral;
        elements = llvm::makeArrayRef(elementsArray).slice(0, 2);
        if (stringLiteral)
          stringLiteral->setEncoding(StringLiteralExpr::UTF16);
        else
          magicLiteral->setStringEncoding(StringLiteralExpr::UTF16);
      } else {
        // Otherwise, fall back to UTF-8.
        builtinProtocol
          = tc.getProtocol(expr->getLoc(),
                           KnownProtocolKind::BuiltinStringLiteralConvertible);
        CFBSLID = tc.Context.Id_ConvertFromBuiltinStringLiteral;
        elements = elementsArray;
        if (stringLiteral)
          stringLiteral->setEncoding(StringLiteralExpr::UTF8);
        else
          magicLiteral->setStringEncoding(StringLiteralExpr::UTF8);
      }

      return convertLiteral(expr,
                            type,
                            expr->getType(),
                            protocol,
                            tc.Context.Id_StringLiteralType,
                            CFSLID,
                            builtinProtocol,
                            TupleType::get(elements, tc.Context),
                            CFBSLID,
                            nullptr,
                            diag::string_literal_broken_proto,
                            diag::builtin_string_literal_broken_proto);
    }
    
    Expr *visitStringLiteralExpr(StringLiteralExpr *expr) {
      return handleStringLiteralExpr(expr);
    }

    Expr *
    visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *expr) {
      // Figure out the string type we're converting to.
      auto openedType = expr->getType();
      auto type = simplifyType(openedType);
      expr->setType(type);

      // Find the string interpolation protocol we need.
      auto &tc = cs.getTypeChecker();
      auto interpolationProto
        = tc.getProtocol(expr->getLoc(),
                         KnownProtocolKind::StringInterpolationConvertible);
      assert(interpolationProto && "Missing string interpolation protocol?");

      // FIXME: Cache name,
      auto name = tc.Context.Id_ConvertFromStringInterpolation;
      auto member = findNamedWitness(tc, dc, type, interpolationProto, name,
                                     diag::interpolation_broken_proto);
      if (!member)
        return nullptr;

      // Build a reference to the convertFromStringInterpolation member.
      auto typeRef = new (tc.Context) MetatypeExpr(
                                        nullptr, expr->getStartLoc(),
                                        MetatypeType::get(type));
      Expr *memberRef = new (tc.Context) MemberRefExpr(typeRef,
                                                       expr->getStartLoc(),
                                                       member,
                                                       expr->getStartLoc(),
                                                       /*Implicit=*/true);
      bool failed = tc.typeCheckExpressionShallow(memberRef, cs.DC);
      assert(!failed && "Could not reference string interpolation witness");
      (void)failed;

      // Create a tuple containing all of the coerced segments.
      SmallVector<Expr *, 4> segments;
      unsigned index = 0;
      ConstraintLocatorBuilder locatorBuilder(cs.getConstraintLocator(expr));
      for (auto segment : expr->getSegments()) {
        segment = coerceToType(segment, type,
                               locatorBuilder.withPathElement(
                                 LocatorPathElt::getInterpolationArgument(
                                   index++)));
        if (!segment)
          return nullptr;

        segments.push_back(segment);
      }

      Expr *argument = nullptr;
      if (segments.size() == 1)
        argument = segments.front();
      else {
        SmallVector<TupleTypeElt, 4> tupleElements(segments.size(),
                                                   TupleTypeElt(type));
        argument = new (tc.Context) TupleExpr(expr->getStartLoc(),
                                              tc.Context.AllocateCopy(segments),
                                              nullptr,
                                              expr->getStartLoc(),
                                              /*hasTrailingClosure=*/false,
                                              /*Implicit=*/true,
                                              TupleType::get(tupleElements,
                                                             tc.Context));
      }

      // Call the convertFromStringInterpolation member with the arguments.
      ApplyExpr *apply = new (tc.Context) CallExpr(memberRef, argument,
                                                   /*Implicit=*/true);
      expr->setSemanticExpr(finishApply(apply, openedType, locatorBuilder));
      return expr;
    }
    
    Expr *visitMagicIdentifierLiteralExpr(MagicIdentifierLiteralExpr *expr) {
      switch (expr->getKind()) {
      case MagicIdentifierLiteralExpr::File:
      case MagicIdentifierLiteralExpr::Function:
        return handleStringLiteralExpr(expr);

      case MagicIdentifierLiteralExpr::Line:
      case MagicIdentifierLiteralExpr::Column:
        return handleIntegerLiteralExpr(expr);
      }
    }

    /// \brief Retrieve the type of a reference to the given declaration.
    Type getTypeOfDeclReference(ValueDecl *decl, bool isSpecialized) {
      if (auto typeDecl = dyn_cast<TypeDecl>(decl)) {
        // Resolve the reference to this type declaration in our
        // current context.
        auto type = cs.getTypeChecker().resolveTypeInContext(typeDecl, dc,
                                                             isSpecialized);
        if (!type)
          return nullptr;

        // Refer to the metatype of this type.
        return MetatypeType::get(type);
      }

      return cs.TC.getUnopenedTypeOfReference(decl, Type(), dc,
                                              /*wantInterfaceType=*/true);
    }

    Expr *visitDeclRefExpr(DeclRefExpr *expr) {
      auto locator = cs.getConstraintLocator(expr);

      // Find the overload choice used for this declaration reference.
      auto selected = getOverloadChoice(locator);
      auto choice = selected.choice;
      auto decl = choice.getDecl();

      // FIXME: Cannibalize the existing DeclRefExpr rather than allocating a
      // new one?
      return buildDeclRef(decl, expr->getLoc(), selected.openedFullType,
                          locator, expr->isSpecialized(), expr->isImplicit(),
                          expr->isDirectPropertyAccess());
    }

    Expr *visitSuperRefExpr(SuperRefExpr *expr) {
      simplifyExprType(expr);
      return expr;
    }

    Expr *visitOtherConstructorDeclRefExpr(OtherConstructorDeclRefExpr *expr) {
      expr->setType(expr->getDecl()->getInitializerType());
      return expr;
    }

    Expr *visitUnresolvedConstructorExpr(UnresolvedConstructorExpr *expr) {
      // Resolve the callee to the constructor declaration selected.
      auto selected = getOverloadChoice(
                        cs.getConstraintLocator(
                          expr,
                          ConstraintLocator::ConstructorMember));
      auto choice = selected.choice;
      auto *ctor = cast<ConstructorDecl>(choice.getDecl());

      auto arg = expr->getSubExpr()->getSemanticsProvidingExpr();
      auto &tc = cs.getTypeChecker();

      // If the subexpression is a metatype, build a direct reference to the
      // constructor.
      if (arg->getType()->is<AnyMetatypeType>()) {
        return buildMemberRef(expr->getSubExpr(),
                              selected.openedFullType,
                              expr->getDotLoc(),
                              ctor,
                              expr->getConstructorLoc(),
                              expr->getType(),
                              ConstraintLocatorBuilder(
                                cs.getConstraintLocator(expr)),
                              expr->isImplicit(),
                              /*IsDirectPropertyAccess=*/false);
      }

      // The subexpression must be either 'self' or 'super'.
      if (!arg->isSuperExpr()) {
        // 'super' references have already been fully checked; handle the
        // 'self' case below.
        bool diagnoseBadInitRef = true;
        if (auto dre = dyn_cast<DeclRefExpr>(arg)) {
          if (dre->getDecl()->getName() == cs.getASTContext().Id_self) {
            // We have a reference to 'self'.
            diagnoseBadInitRef = false;

            // Make sure the reference to 'self' occurs within an initializer.
            if (!dyn_cast_or_null<ConstructorDecl>(
                   cs.DC->getInnermostMethodContext())) {
              tc.diagnose(expr->getDotLoc(),
                          diag::init_delegation_outside_initializer);
            }
          }
        }

        // If we need to diagnose this as a bad reference to an initializer,
        // do so now.
        if (diagnoseBadInitRef) {
          // Determine whether 'super' would have made sense as a base.
          bool hasSuper = false;
          if (auto func = cs.DC->getInnermostMethodContext()) {
            if (auto nominalType
                       = func->getDeclContext()->getDeclaredTypeOfContext()) {
              if (auto classDecl = nominalType->getClassOrBoundGenericClass()) {
                hasSuper = classDecl->hasSuperclass();
              }
            }
          }

          tc.diagnose(expr->getDotLoc(), diag::bad_init_ref_base, hasSuper);
        }
      }

      // Build a partial application of the initializer.
      Expr *ctorRef = buildOtherConstructorRef(selected.openedFullType,
                                               ctor, expr->getConstructorLoc(),
                                               expr->isImplicit());
      auto *call
        = new (cs.getASTContext()) DotSyntaxCallExpr(ctorRef,
                                                     expr->getDotLoc(),
                                                     expr->getSubExpr());
      return finishApply(call, expr->getType(),
                         ConstraintLocatorBuilder(
                           cs.getConstraintLocator(expr)));
    }

    Expr *visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *expr) {
      return simplifyExprType(expr);
    }

    Expr *visitOverloadedDeclRefExpr(OverloadedDeclRefExpr *expr) {
      // Determine the declaration selected for this overloaded reference.
      auto locator = cs.getConstraintLocator(expr);
      auto selected = getOverloadChoice(locator);
      auto choice = selected.choice;
      auto decl = choice.getDecl();

      return buildDeclRef(decl, expr->getLoc(), selected.openedFullType,
                          locator, expr->isSpecialized(), expr->isImplicit(),
                          /*isDirectPropertyAccess*/false);
    }

    Expr *visitOverloadedMemberRefExpr(OverloadedMemberRefExpr *expr) {
      auto selected = getOverloadChoice(
                        cs.getConstraintLocator(expr,
                                                ConstraintLocator::Member));
      return buildMemberRef(expr->getBase(),
                            selected.openedFullType,
                            expr->getDotLoc(),
                            selected.choice.getDecl(), expr->getMemberLoc(),
                            selected.openedType,
                            cs.getConstraintLocator(expr),
                            expr->isImplicit(), /*direct ivar*/false);
    }

    Expr *visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *expr) {
      // FIXME: We should have generated an overload set from this, in which
      // case we can emit a typo-correction error here but recover well.
      return nullptr;
    }

    Expr *visitUnresolvedSpecializeExpr(UnresolvedSpecializeExpr *expr) {
      // Our specializations should have resolved the subexpr to the right type.
      if (auto DRE = dyn_cast<DeclRefExpr>(expr->getSubExpr())) {
        assert(DRE->getGenericArgs().empty() ||
            DRE->getGenericArgs().size() == expr->getUnresolvedParams().size());
        if (DRE->getGenericArgs().empty()) {
          SmallVector<TypeRepr *, 8> GenArgs;
          for (auto TL : expr->getUnresolvedParams())
            GenArgs.push_back(TL.getTypeRepr());
          DRE->setGenericArgs(GenArgs);
        }
      }
      return expr->getSubExpr();
    }

    Expr *visitMemberRefExpr(MemberRefExpr *expr) {
      auto selected = getOverloadChoice(
                        cs.getConstraintLocator(expr,
                                                ConstraintLocator::Member));
      return buildMemberRef(expr->getBase(),
                            selected.openedFullType,
                            expr->getDotLoc(),
                            selected.choice.getDecl(), expr->getNameLoc(),
                            selected.openedType,
                            cs.getConstraintLocator(expr),
                            expr->isImplicit(),
                            expr->isDirectPropertyAccess());
    }

    Expr *visitDynamicMemberRefExpr(DynamicMemberRefExpr *expr) {
      auto selected = getOverloadChoice(
                        cs.getConstraintLocator(expr,
                                                ConstraintLocator::Member));

      return buildDynamicMemberRef(expr->getBase(), expr->getDotLoc(),
                                   selected.choice.getDecl(),
                                   expr->getNameLoc(),
                                   selected.openedType,
                                   cs.getConstraintLocator(expr));
    }

    Expr *visitUnresolvedMemberExpr(UnresolvedMemberExpr *expr) {
      // Dig out the type of the base, which will be the result
      // type of this expression.
      Type baseTy = simplifyType(expr->getType())->getRValueType();
      auto &tc = cs.getTypeChecker();
      auto baseMetaTy = MetatypeType::get(baseTy);

      // Find the selected member.
      auto selected = getOverloadChoice(
                        cs.getConstraintLocator(
                          expr, ConstraintLocator::UnresolvedMember));
      auto member = selected.choice.getDecl();

      // The base expression is simply the metatype of the base type.
      auto base = new (tc.Context) MetatypeExpr(nullptr,
                                                expr->getDotLoc(),
                                                baseMetaTy);

      // Build the member reference.
      auto result = buildMemberRef(base,
                                   selected.openedFullType,
                                   expr->getDotLoc(), member, 
                                   expr->getNameLoc(),
                                   selected.openedType,
                                   cs.getConstraintLocator(expr),
                                   expr->isImplicit(), /*direct ivar*/false);
      if (!result)
        return nullptr;

      // If there was an argument, apply it.
      if (auto arg = expr->getArgument()) {
        ApplyExpr *apply = new (tc.Context) CallExpr(result, arg, 
                                                     /*Implicit=*/false);
        result = finishApply(apply, Type(), cs.getConstraintLocator(expr));
      }

      return result;
    }
    
  private:
    struct MemberPartialApplication {
      unsigned level : 31;
      // Selector for the partial_application_of_method_invalid diagnostic
      // message.
      enum : unsigned {
        Struct,
        Enum,
        Archetype,
        Protocol,
        ObjC
      };
      unsigned kind : 3;
    };
    
    // A map used to track partial applications of methods to require that they
    // be fully applied. Partial applications of value types would capture
    // 'self' as an inout and hide any mutation of 'self', which is surprising.
    llvm::SmallDenseMap<Expr*, MemberPartialApplication, 2>
      InvalidPartialApplications;
    
    Expr *applyMemberRefExpr(Expr *expr,
                             Expr *base,
                             SourceLoc dotLoc,
                             SourceLoc nameLoc,
                             bool implicit) {
      // Determine the declaration selected for this overloaded reference.
      auto selected = getOverloadChoice(
                        cs.getConstraintLocator(
                          expr,
                          ConstraintLocator::MemberRefBase));

      switch (selected.choice.getKind()) {
      case OverloadChoiceKind::Decl: {
        auto member = buildMemberRef(base,
                                     selected.openedFullType,
                                     dotLoc,
                                     selected.choice.getDecl(),
                                     nameLoc,
                                     selected.openedType,
                                     cs.getConstraintLocator(expr),
                                     implicit, /*direct ivar*/false);
        // If this is an application of a value type method, arrange for us to
        // check that it gets fully applied.
        FuncDecl *fn = nullptr;
        unsigned kind;
        if (auto apply = dyn_cast<ApplyExpr>(member)) {
          auto selfTy = apply->getArg()->getType()->getRValueType();
          auto fnDeclRef = dyn_cast<DeclRefExpr>(apply->getFn());
          if (!fnDeclRef)
            goto not_value_type_member;
          fn = dyn_cast<FuncDecl>(fnDeclRef->getDecl());
          if (selfTy->getStructOrBoundGenericStruct())
            kind = MemberPartialApplication::Struct;
          else if (selfTy->getEnumOrBoundGenericEnum())
            kind = MemberPartialApplication::Enum;
          else if (fnDeclRef->getDecl()->isObjC())
            kind = MemberPartialApplication::ObjC;
          else
            goto not_value_type_member;
        } else if (auto pmRef = dyn_cast<MemberRefExpr>(member)) {
          auto baseTy = pmRef->getBase()->getType();
          if (baseTy->hasReferenceSemantics())
            goto not_value_type_member;
          if (baseTy->isAnyExistentialType()) {
            kind = MemberPartialApplication::Protocol;
          } else if (isa<FuncDecl>(pmRef->getMember().getDecl()))
            kind = MemberPartialApplication::Archetype;
          else
            goto not_value_type_member;
          fn = dyn_cast<FuncDecl>(pmRef->getMember().getDecl());
        }
        if (!fn)
          goto not_value_type_member;
        if (fn->isInstanceMember())
          InvalidPartialApplications.insert({
            member,
            // We need to apply all of the non-self argument clauses.
            {fn->getNaturalArgumentCount() - 1, kind},
          });

      not_value_type_member:
        return member;
      }

      case OverloadChoiceKind::DeclViaDynamic:
        return buildDynamicMemberRef(base, dotLoc,
                                     selected.choice.getDecl(),
                                     nameLoc,
                                     selected.openedType,
                                     cs.getConstraintLocator(expr));

      case OverloadChoiceKind::TupleIndex: {
        auto baseTy = base->getType()->getRValueType();
        if (auto objTy = cs.lookThroughUncheckedOptionalType(baseTy)) {
          base = coerceUncheckedOptionalToValue(base, objTy,
                                         cs.getConstraintLocator(base));
          if (!base) return nullptr;
        }

        return new (cs.getASTContext()) TupleElementExpr(
                                          base,
                                          dotLoc,
                                          selected.choice.getTupleIndex(),
                                          nameLoc,
                                          simplifyType(expr->getType()));
      }

      case OverloadChoiceKind::BaseType: {
        // FIXME: Losing ".0" sugar here.
        return base;
      }

      case OverloadChoiceKind::TypeDecl:
        llvm_unreachable("Nonsensical overload choice");
      }
    }
    
  public:
    Expr *visitUnresolvedSelectorExpr(UnresolvedSelectorExpr *expr) {
      return applyMemberRefExpr(expr, expr->getBase(), expr->getDotLoc(),
                                expr->getNameRange().Start,
                                expr->isImplicit());
      llvm_unreachable("not implemented");
    }
    
    
    Expr *visitUnresolvedDotExpr(UnresolvedDotExpr *expr) {
      return applyMemberRefExpr(expr, expr->getBase(), expr->getDotLoc(),
                                expr->getNameLoc(), expr->isImplicit());
    }

    Expr *visitSequenceExpr(SequenceExpr *expr) {
      llvm_unreachable("Expression wasn't parsed?");
    }

    Expr *visitIdentityExpr(IdentityExpr *expr) {
      expr->setType(expr->getSubExpr()->getType());
      return expr;
    }

    Expr *visitTupleExpr(TupleExpr *expr) {
      return simplifyExprType(expr);
    }

    Expr *visitSubscriptExpr(SubscriptExpr *expr) {
      return buildSubscript(expr->getBase(), expr->getIndex(),
                            cs.getConstraintLocator(expr));
    }

    Expr *visitArrayExpr(ArrayExpr *expr) {
      Type openedType = expr->getType();
      Type arrayTy = simplifyType(openedType);
      auto &tc = cs.getTypeChecker();

      ProtocolDecl *arrayProto
        = tc.getProtocol(expr->getLoc(),
                         KnownProtocolKind::ArrayLiteralConvertible);
      assert(arrayProto && "type-checked array literal w/o protocol?!");

      ProtocolConformance *conformance = nullptr;
      bool conforms = tc.conformsToProtocol(arrayTy, arrayProto,
                                            cs.DC, &conformance);
      (void)conforms;
      assert(conforms && "Type does not conform to protocol?");

      // Call the witness that builds the array literal.
      // FIXME: callWitness() may end up re-doing some work we already did
      // to convert the array literal elements to the element type. It would
      // be nicer to re-use them.
      // FIXME: Cache the name.
      Expr *typeRef = new (tc.Context) MetatypeExpr(nullptr,
                                         expr->getLoc(),
                                         MetatypeType::get(arrayTy));
      auto name = tc.Context.Id_ConvertFromArrayLiteral;
      auto arg = expr->getSubExpr();
      Expr *result = tc.callWitness(typeRef, dc, arrayProto, conformance,
                                    name, arg, diag::array_protocol_broken);
      if (!result)
        return nullptr;

      expr->setSemanticExpr(result);
      expr->setType(arrayTy);
      return expr;
    }

    Expr *visitDictionaryExpr(DictionaryExpr *expr) {
      Type openedType = expr->getType();
      Type dictionaryTy = simplifyType(openedType);
      auto &tc = cs.getTypeChecker();

      ProtocolDecl *dictionaryProto
        = tc.getProtocol(expr->getLoc(),
                         KnownProtocolKind::DictionaryLiteralConvertible);

      ProtocolConformance *conformance = nullptr;
      bool conforms = tc.conformsToProtocol(dictionaryTy, dictionaryProto,
                                            cs.DC, &conformance);
      if (!conforms)
        return nullptr;

      // Call the witness that builds the dictionary literal.
      // FIXME: callWitness() may end up re-doing some work we already did
      // to convert the dictionary literal elements to the (key, value) tuple.
      // It would be nicer to re-use them.
      // FIXME: Cache the name.
      Expr *typeRef = new (tc.Context) MetatypeExpr(
                                         nullptr,
                                         expr->getLoc(),
                                         MetatypeType::get(dictionaryTy));
      auto name = tc.Context.Id_ConvertFromDictionaryLiteral;
      auto arg = expr->getSubExpr();
      Expr *result = tc.callWitness(typeRef, dc, dictionaryProto,
                                    conformance, name, arg,
                                    diag::dictionary_protocol_broken);
      if (!result)
        return nullptr;

      expr->setSemanticExpr(result);
      expr->setType(dictionaryTy);
      return expr;
    }

    Expr *visitDynamicSubscriptExpr(DynamicSubscriptExpr *expr) {
      return buildSubscript(expr->getBase(), expr->getIndex(),
                            cs.getConstraintLocator(expr));
    }

    Expr *visitTupleElementExpr(TupleElementExpr *expr) {
      // Handle accesses that implicitly look through UncheckedOptional<T>.
      auto base = expr->getBase();
      auto baseTy = base->getType()->getRValueType();
      if (auto objTy = cs.lookThroughUncheckedOptionalType(baseTy)) {
        base = coerceUncheckedOptionalToValue(base, objTy,
                                              cs.getConstraintLocator(base));
        if (!base) return nullptr;
        expr->setBase(base);
      }

      simplifyExprType(expr);
      return expr;
    }

    Expr *visitClosureExpr(ClosureExpr *expr) {
      llvm_unreachable("Handled by the walker directly");
    }

    Expr *visitAutoClosureExpr(AutoClosureExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Expr *visitModuleExpr(ModuleExpr *expr) { return expr; }

    Expr *visitInOutExpr(InOutExpr *expr) {
      auto &C = cs.getASTContext();
      
      // Determine the disjunction choice.
      auto locator = cs.getConstraintLocator(expr,
                                           ConstraintLocator::InOutConversion);
      // The order of the disjunction choices.
      enum InOutConversion : unsigned {
        InOut,
        AddressConversion,
        WritebackConversion,
        Last_Conversion = WritebackConversion,
      };
      unsigned choice = solution.getDisjunctionChoice(locator);
      assert(choice <= InOutConversion::Last_Conversion
             && "inout conversion kinds are not synced with disjunction "
                "constraint system");
      
      auto buildInOutConversionExpr = [&](const SelectedOverload &choice,
                                          Type resultTy,
                                          Expr *lvExpr) -> Expr * {
        auto inout = new (C) InOutExpr(lvExpr->getLoc(), lvExpr,
               InOutType::get(lvExpr->getType()->getLValueOrInOutObjectType()),
               /*implicit*/ true);
        
        // Build up a call to the method.
        auto &C = cs.getASTContext();
        auto resultMeta = new (C) MetatypeExpr(nullptr,
                                             expr->getSubExpr()->getStartLoc(),
                                             MetatypeType::get(resultTy));
        auto memberRef = buildMemberRef(resultMeta, choice.openedFullType,
                                    expr->getSubExpr()->getStartLoc(),
                                    choice.choice.getDecl(),
                                    expr->getSubExpr()->getStartLoc(),
                                    choice.openedType,
                                    ConstraintLocatorBuilder(locator),
                                    /*implicit*/ true,
                                    /*directPropertyAccess*/ false);
        auto methodTy = memberRef->getType()->castTo<AnyFunctionType>();
        
        ApplyExpr *call
          = new (C) CallExpr(memberRef, inout, /*implicit*/ true);
        call->setType(methodTy->getResult());
        Expr *conversion = finishApply(call, choice.openedType,
                                       ConstraintLocatorBuilder(locator));
        conversion = coerceToType(conversion, resultTy,
                                  ConstraintLocatorBuilder(locator));
        
        // Wrap the call in an InOutConversion node to mark its special
        // writeback semantics.
        return new (C) InOutConversionExpr(expr->getLoc(), conversion);
      };
      
      auto lvTy = expr->getSubExpr()->getType()->castTo<LValueType>();
      switch (InOutConversion(choice)) {
      case InOut: {
        // The type is simply inout.
        // Compute the type of the inout expression.
        expr->setType(InOutType::get(lvTy->getObjectType()));
        return expr;
      }
          
      case AddressConversion: {
        auto resultTy = simplifyType(expr->getType());
        
        // Find the conversion method we chose.
        auto choice = getOverloadChoice(locator);
        
        // Use it to convert the lvalue.
        return buildInOutConversionExpr(choice, resultTy,
                                        expr->getSubExpr());
      }
          
      case WritebackConversion: {
        auto resultTy = simplifyType(expr->getType());
        
        // Find the conversion methods we chose.
        auto conversionChoice = getOverloadChoice(cs.getConstraintLocator(expr,
                                   ConstraintLocator::WritebackConversion));
        auto getChoice = getOverloadChoice(cs.getConstraintLocator(expr,
                                   ConstraintLocator::WritebackConversionGet));
        auto setChoice = getOverloadChoice(cs.getConstraintLocator(expr,
                                   ConstraintLocator::WritebackConversionSet));
        
        // Build the LValueConversion through the get/set pair.
        auto &C = cs.getASTContext();
        auto resultMeta = new (C) MetatypeExpr(nullptr,
                                             expr->getSubExpr()->getStartLoc(),
                                             MetatypeType::get(resultTy));
        Expr *getMemberRef = buildMemberRef(resultMeta, getChoice.openedFullType,
                                           expr->getSubExpr()->getStartLoc(),
                                           getChoice.choice.getDecl(),
                                           expr->getSubExpr()->getStartLoc(),
                                           getChoice.openedType,
                                           ConstraintLocatorBuilder(locator),
                                           /*implicit*/ true,
                                           /*directPropertyAccess*/ false);
        auto writebackTy = getMemberRef->getType()
          ->castTo<AnyFunctionType>()
          ->getResult();
        
        Expr *setMemberRef = buildMemberRef(resultMeta, setChoice.openedFullType,
                                           expr->getSubExpr()->getStartLoc(),
                                           setChoice.choice.getDecl(),
                                           expr->getSubExpr()->getStartLoc(),
                                           setChoice.openedType,
                                           ConstraintLocatorBuilder(locator),
                                           /*implicit*/ true,
                                           /*directPropertyAccess*/ false);
        
        auto lvConversion = new (C) LValueConversionExpr(expr->getSubExpr(),
                                                   LValueType::get(writebackTy),
                                                   getMemberRef,
                                                   setMemberRef);
        
        // Convert the converted lvalue.
        return buildInOutConversionExpr(conversionChoice, resultTy,
                                        lvConversion);
      }
      }
    }

    Expr *visitNewArrayExpr(NewArrayExpr *expr) {
      auto &tc = cs.getTypeChecker();

      // Convert the subexpression to an array bound.
      auto outerBoundLocator
        = cs.getConstraintLocator(expr->getBounds()[0].Value);
      auto outerBound = solution.convertToArrayBound(expr->getBounds()[0].Value,
                                                     outerBoundLocator);
      if (!outerBound)
        return nullptr;
      expr->getBounds()[0].Value = outerBound;


      // Dig out the element type of the new array expression.
      auto resultType = simplifyType(expr->getType());
      auto elementType = resultType->castTo<BoundGenericType>()
                           ->getGenericArgs()[0];
      expr->setElementType(elementType);

      // Make sure that the result type is a slice type, even if
      // canonicalization mapped it down to Array<T>.
      auto sliceType = dyn_cast<ArraySliceType>(resultType.getPointer());
      if (!sliceType) {
        resultType = tc.getArraySliceType(expr->getLoc(), elementType);
        if (!resultType)
          return nullptr;

        sliceType = cast<ArraySliceType>(resultType.getPointer());
      }
      expr->setType(resultType);

      // Find the appropriate injection function.
      Expr* injectionFn = tc.buildArrayInjectionFnRef(dc, sliceType,
                            expr->getBounds()[0].Value->getType(),
                            expr->getNewLoc());
      if (!injectionFn)
        return nullptr;
      expr->setInjectionFunction(injectionFn);

      // If we gave an explicit construction closure, it should have
      // IndexType -> ElementType type.
      if (expr->hasConstructionFunction()) {
        // FIXME: Assume the index type is DefaultIntegerLiteralType for now.
        auto intProto =
          tc.getProtocol(expr->getConstructionFunction()->getLoc(),
                         KnownProtocolKind::IntegerLiteralConvertible);
        Type intTy = tc.getDefaultType(intProto, dc);
        Type constructionTy = FunctionType::get(intTy, elementType);

        auto constructionFn = expr->getConstructionFunction();
        auto locator = cs.getConstraintLocator(
                         expr,
                         ConstraintLocator::NewArrayConstructor);
        constructionFn = solution.coerceToType(constructionFn, constructionTy,
                                               locator);
        if (!constructionFn)
          return nullptr;

        expr->setConstructionFunction(constructionFn);
      } else {
        // If the element type is default constructible, form a partial
        // application of it.
        auto selected = getOverloadChoice(cs.getConstraintLocator(expr,
                                          ConstraintLocator::NewArrayElement));
        
        auto baseElementType = elementType;
        while (true) {
          if (auto arrayTy = baseElementType->getAs<ArrayType>())
            baseElementType = arrayTy->getBaseType();
          else if (auto sliceTy =
                     dyn_cast<ArraySliceType>(baseElementType.getPointer()))
            baseElementType = sliceTy->getBaseType();
          else
            break;
        }
        
        Expr *ctor = tc.buildRefExpr(selected.choice.getDecl(), dc,
                                     SourceLoc(), /*implicit*/ true);
        Expr *metaty = new (tc.Context) MetatypeExpr(nullptr, SourceLoc(),
                               MetatypeType::get(baseElementType));
        Expr *applyExpr = new(tc.Context) ConstructorRefCallExpr(ctor, metaty);
        if (tc.typeCheckExpression(applyExpr, dc, Type(), /*discarded*/ false))
          llvm_unreachable("should not fail");
      
        expr->setConstructionFunction(applyExpr);
      }
      
      return expr;
    }

    Expr *visitMetatypeExpr(MetatypeExpr *expr) {
      auto &tc = cs.getTypeChecker();

      if (Expr *base = expr->getBase()) {
        base = tc.coerceToRValue(base);
        if (!base) return nullptr;
        expr->setBase(base);
      }

      return simplifyExprType(expr);
    }

    Expr *visitOpaqueValueExpr(OpaqueValueExpr *expr) {
      return expr;
    }

    Expr *visitDefaultValueExpr(DefaultValueExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Expr *visitApplyExpr(ApplyExpr *expr) {
      
      auto result = finishApply(expr, expr->getType(),
                         ConstraintLocatorBuilder(
                           cs.getConstraintLocator(expr)));

      // See if this application advanced a partial value type application.
      auto foundApplication = InvalidPartialApplications.find(
                                   expr->getFn()->getSemanticsProvidingExpr());
      if (foundApplication != InvalidPartialApplications.end()) {
        unsigned level = foundApplication->second.level;
        assert(level > 0);
        InvalidPartialApplications.erase(foundApplication);
        if (level > 1)
          InvalidPartialApplications.insert({
            result, {
              level - 1,
              foundApplication->second.kind
            }
          });
      }
      
      return result;
    }

    Expr *visitRebindSelfInConstructorExpr(RebindSelfInConstructorExpr *expr) {
      return expr;
    }

    Expr *visitIfExpr(IfExpr *expr) {
      auto resultTy = simplifyType(expr->getType());
      expr->setType(resultTy);

      // Convert the condition to a logic value.
      auto cond
        = solution.convertToLogicValue(expr->getCondExpr(),
                                       cs.getConstraintLocator(expr));
      if (!cond) {
        cond->setType(ErrorType::get(cs.getASTContext()));
      } else {
        expr->setCondExpr(cond);
      }

      // Coerce the then/else branches to the common type.
      expr->setThenExpr(coerceToType(expr->getThenExpr(), resultTy,
                                     ConstraintLocatorBuilder(
                                       cs.getConstraintLocator(expr))
                                     .withPathElement(
                                       ConstraintLocator::IfThen)));
      expr->setElseExpr(coerceToType(expr->getElseExpr(), resultTy,
                                     ConstraintLocatorBuilder(
                                       cs.getConstraintLocator(expr))
                                     .withPathElement(
                                       ConstraintLocator::IfElse)));

      return expr;
    }
    
    Expr *visitImplicitConversionExpr(ImplicitConversionExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Expr *visitIsaExpr(IsaExpr *expr) {
      // Turn the subexpression into an rvalue.
      auto &tc = cs.getTypeChecker();
      auto sub = tc.coerceToRValue(expr->getSubExpr());
      if (!sub)
        return nullptr;
      expr->setSubExpr(sub);

      // Set the type we checked against.
      auto toType = simplifyType(expr->getCastTypeLoc().getType());
      expr->getCastTypeLoc().setType(toType, /*validated=*/true);
      auto fromType = sub->getType();
      auto castKind = tc.typeCheckCheckedCast(
                        fromType, toType, cs.DC,
                        expr->getLoc(),
                        sub->getSourceRange(),
                        expr->getCastTypeLoc().getSourceRange(),
                        [&](Type commonTy) -> bool {
                          return tc.convertToType(sub, commonTy, cs.DC);
                        });
      
      switch (castKind) {
      case CheckedCastKind::Unresolved:
        // Invalid type check.
        return nullptr;
      case CheckedCastKind::Coercion:
        // Check is trivially true.
        tc.diagnose(expr->getLoc(), diag::isa_is_always_true,
                    expr->getSubExpr()->getType(),
                    expr->getCastTypeLoc().getType());
        expr->setCastKind(castKind);
        break;
      
      case CheckedCastKind::Downcast:
      case CheckedCastKind::SuperToArchetype:
      case CheckedCastKind::ArchetypeToArchetype:
      case CheckedCastKind::ArchetypeToConcrete:
      case CheckedCastKind::ExistentialToArchetype:
      case CheckedCastKind::ExistentialToConcrete:
      case CheckedCastKind::ConcreteToArchetype:
      case CheckedCastKind::ConcreteToUnrelatedExistential:
        // Valid checks.
        expr->setCastKind(castKind);
        break;
      }

      // SIL-generation magically turns this into a Bool; make sure it can.
      if (!cs.getASTContext().getGetBoolDecl(&cs.getTypeChecker())) {
        tc.diagnose(expr->getLoc(), diag::bool_intrinsics_not_found);
        // Continue anyway.
      }

      return expr;
    }

    Expr *visitConditionalCheckedCastExpr(ConditionalCheckedCastExpr *expr) {
      // Simplify the type we're casting to.
      auto toType = simplifyType(expr->getCastTypeLoc().getType());
      expr->getCastTypeLoc().setType(toType, /*validated=*/true);

      // The subexpression is always an rvalue.
      auto &tc = cs.getTypeChecker();
      auto sub = tc.coerceToRValue(expr->getSubExpr());
      if (!sub)
        return nullptr;
      expr->setSubExpr(sub);

      auto fromType = sub->getType();
      auto castKind = tc.typeCheckCheckedCast(
                        fromType, toType, cs.DC,
                        expr->getLoc(),
                        sub->getSourceRange(),
                        expr->getCastTypeLoc().getSourceRange(),
                        [&](Type commonTy) -> bool {
                          return tc.convertToType(sub, commonTy,
                                                 cs.DC);
                        });
      switch (castKind) {
        /// Invalid cast.
      case CheckedCastKind::Unresolved:
        return nullptr;
      case CheckedCastKind::Coercion: {
        // This is a coercion. Convert the subexpression.
        bool failed = tc.convertToType(sub, toType, cs.DC);
        (void)failed;
        assert(!failed && "Not convertible?");

        // Transmute the checked cast into a coercion expression.
        Expr *result = new (tc.Context) CoerceExpr(sub, expr->getLoc(),
                                                   expr->getCastTypeLoc());

        // The result type is the type we're converting to.
        result->setType(toType);
        return result;
      }

      // Valid casts.
      case CheckedCastKind::Downcast:
      case CheckedCastKind::SuperToArchetype:
      case CheckedCastKind::ArchetypeToArchetype:
      case CheckedCastKind::ArchetypeToConcrete:
      case CheckedCastKind::ExistentialToArchetype:
      case CheckedCastKind::ExistentialToConcrete:
      case CheckedCastKind::ConcreteToArchetype:
      case CheckedCastKind::ConcreteToUnrelatedExistential:
        expr->setCastKind(castKind);
        break;
      }

      Type finalResultType = simplifyType(expr->getType());

      // Handle optional operands or optional results.

      // FIXME: some of this work needs to be delayed until runtime to
      // properly account for archetypes dynamically being optional
      // types.  For example, if we're casting T to NSView?, that
      // should succeed if T=NSObject? and its value is actually nil.

      /// A helper function to plumb a stack of optional types.
      auto plumbOptionals = [](Type type, SmallVectorImpl<Type> &optionals) {
        while (auto valueType = type->getAnyOptionalObjectType()) {
          optionals.push_back(type);
          type = valueType;
        }
        return type;
      };

      Expr *subExpr = expr->getSubExpr();
      Type srcType = subExpr->getType();

      // There's nothing special to do if the operand isn't optional.
      SmallVector<Type, 4> srcOptionals;
      srcType = plumbOptionals(srcType, srcOptionals);
      if (srcOptionals.empty()) {
        expr->setType(finalResultType);
        return expr;
      }

      SmallVector<Type, 4> destOptionals;
      auto destValueType = plumbOptionals(finalResultType, destOptionals);

      // This is a checked cast, so the result type will always have
      // at least one level of optional, which should become the type
      // of the checked-cast expression.
      assert(!destOptionals.empty() &&
             "result of checked cast is not an optional type");
      expr->setType(destOptionals.back());

      // The result type (without the final optional) is a subtype of
      // the operand type, so it will never have a higher depth.
      assert(destOptionals.size() - 1 <= srcOptionals.size());

      // The outermost N levels of optionals on the operand must all
      // be present or the cast fails.  The innermost M levels of
      // optionals on the operand are reflected in the requested
      // destination type, so we should map these nils into the result.
      unsigned numRequiredOptionals =
        srcOptionals.size() - (destOptionals.size() - 1);

      // The number of OptionalEvaluationExprs between the point of the
      // inner cast and the enclosing OptionalEvaluationExpr (exclusive)
      // which represents failure for the entire operation.
      unsigned failureDepth = destOptionals.size() - 1;

      // Drill down on the operand until it's non-optional.
      SourceLoc fakeQuestionLoc = subExpr->getEndLoc();
      for (unsigned i : indices(srcOptionals)) {
        // As we move into the range of mapped optionals, start
        // lowering the depth.
        unsigned depth = failureDepth;
        if (i >= numRequiredOptionals) {
          depth -= (i - numRequiredOptionals) + 1;
        }

        Type valueType =
          (i + 1 == srcOptionals.size() ? srcType : srcOptionals[i+1]);
        subExpr = new (tc.Context) BindOptionalExpr(subExpr, fakeQuestionLoc,
                                                    depth, valueType);
        subExpr->setImplicit(true);
      }
      expr->setSubExpr(subExpr);

      // If we're casting to an optional type, we need to capture the
      // final M bindings.
      Expr *result = expr;
      if (destOptionals.size() > 1) {
        // If the innermost cast fails, the entire expression fails.  To
        // get this behavior, we have to bind and then re-inject the result.
        // (SILGen should know how to peephole this.)
        result = new (tc.Context) BindOptionalExpr(result, expr->getEndLoc(),
                                                   failureDepth, destValueType);
        result->setImplicit(true);

        for (unsigned i = destOptionals.size(); i != 0; --i) {
          Type destType = destOptionals[i-1];
          result = new (tc.Context) InjectIntoOptionalExpr(result, destType);
          result = new (tc.Context) OptionalEvaluationExpr(result, destType);
        }

      // Otherwise, we just need to capture the failure-depth binding.
      } else {
        result = new (tc.Context) OptionalEvaluationExpr(result,
                                                         finalResultType);
      }

      return result;
    }

    Expr *visitCoerceExpr(CoerceExpr *expr) {
      return expr;
    }

    Expr *visitAssignExpr(AssignExpr *expr) {
      llvm_unreachable("Handled by ExprWalker");
    }

    Expr *visitAssignExpr(AssignExpr *expr, ConstraintLocator *srcLocator) {
      // Compute the type to which the source must be converted to allow
      // assignment to the destination.
      //
      // FIXME: This is also computed when the constraint system is set up.
      auto destTy = cs.computeAssignDestType(expr->getDest(), expr->getLoc());
      if (!destTy)
        return nullptr;

      // Convert the source to the simplified destination type.
      Expr *src = solution.coerceToType(expr->getSrc(),
                                        destTy,
                                        srcLocator);
      if (!src)
        return nullptr;
      
      expr->setSrc(src);
      
      return expr;
    }
    
    Expr *visitDiscardAssignmentExpr(DiscardAssignmentExpr *expr) {
      return simplifyExprType(expr);
    }
    
    Expr *visitUnresolvedPatternExpr(UnresolvedPatternExpr *expr) {
      llvm_unreachable("should have been eliminated during name binding");
    }
    
    Expr *visitBindOptionalExpr(BindOptionalExpr *expr) {
      Type valueType = simplifyType(expr->getType());
      Type optType =
        cs.getTypeChecker().getOptionalType(expr->getQuestionLoc(), valueType);
      if (!optType) return nullptr;

      Expr *subExpr = coerceToType(expr->getSubExpr(), optType,
                                   cs.getConstraintLocator(expr));
      if (!subExpr) return nullptr;

      // Complain if the sub-expression was converted to T? via the
      // inject-into-optional implicit conversion.
      //
      // It should be the case that that's always the last conversion applied.
      if (isa<InjectIntoOptionalExpr>(subExpr)) {
        cs.getTypeChecker().diagnose(subExpr->getLoc(),
                                     diag::binding_injected_optional,
                               expr->getSubExpr()->getType()->getRValueType())
          .highlight(subExpr->getSourceRange())
          .fixItRemove(expr->getQuestionLoc());
      }

      expr->setSubExpr(subExpr);
      expr->setType(valueType);
      return expr;
    }

    Expr *visitOptionalEvaluationExpr(OptionalEvaluationExpr *expr) {
      Type optType = simplifyType(expr->getType());
      Expr *subExpr = coerceToType(expr->getSubExpr(), optType,
                                   cs.getConstraintLocator(expr));
      if (!subExpr) return nullptr;

      expr->setSubExpr(subExpr);
      expr->setType(optType);
      return expr;
    }

    /// Whether this type is AnyObject or an implicit lvalue thereof.
    bool isDynamicLookupType(Type type) {
      // Check whether we have a protocol type.
      auto protoTy = type->getAs<ProtocolType>();
      if (!protoTy)
        return false;

      // Check whether this is AnyObject.
      return protoTy->getDecl()->isSpecificProtocol(
                                   KnownProtocolKind::AnyObject);
    }

    Expr *visitForceValueExpr(ForceValueExpr *expr) {
      Type valueType = simplifyType(expr->getType());
      auto &tc = cs.getTypeChecker();

      // Choice #0 is forcing a T? to T.
      // Choice #1 is forcing an AnyObject or AnyObject? to a class type.
      unsigned disjChoice =
        solution.getDisjunctionChoice(cs.getConstraintLocator(expr));
      bool isAnyObjectDowncast = (disjChoice != 0);

      // If the subexpression is of AnyObject type, introduce a conditional
      // cast to the value type. This cast produces a value of optional type.
      Expr *subExpr = expr->getSubExpr();
      if (isAnyObjectDowncast) {
        // Coerce the subexpression to an rvalue.
        subExpr = tc.coerceToRValue(subExpr);
        if (!subExpr) return nullptr;

        // If the operand is AnyObject?, force it.
        if (auto operandValueType =
              subExpr->getType()->getAnyOptionalObjectType()) {
          subExpr = new (tc.Context) ForceValueExpr(subExpr,
                                                    expr->getExclaimLoc());
          subExpr->setType(operandValueType);
          subExpr->setImplicit(true);
        }

        // At this point, we should have an AnyObject.
        assert(isDynamicLookupType(subExpr->getType()));

        // Create a conditional checked cast to the value type, e.g., x as T.
        bool isArchetype = valueType->is<ArchetypeType>();
        auto cast = new (tc.Context) ConditionalCheckedCastExpr(
                                       subExpr,
                                       SourceLoc(),
                                       TypeLoc::withoutLoc(valueType));
        cast->setImplicit(true);
        cast->setType(OptionalType::get(valueType));
        cast->setCastKind(isArchetype? CheckedCastKind::ExistentialToArchetype
                                     : CheckedCastKind::ExistentialToConcrete);
        subExpr = cast;
      } else {
        Type optType = OptionalType::get(valueType);
        
        // Coerce the subexpression to the appropriate optional type.
        subExpr = coerceToType(subExpr, optType,
                               cs.getConstraintLocator(expr));
        if (!subExpr) return nullptr;

        // Complain if the sub-expression was converted to T? via the
        // inject-into-optional implicit conversion.
        //
        // It should be the case that that's always the last conversion applied.
        if (isa<InjectIntoOptionalExpr>(subExpr)) {
          tc.diagnose(subExpr->getLoc(), diag::forcing_injected_optional,
                      expr->getSubExpr()->getType()->getRValueType())
            .highlight(subExpr->getSourceRange())
            .fixItRemove(expr->getExclaimLoc());
        }
      }

      expr->setSubExpr(subExpr);
      expr->setType(valueType);
      return expr;
    }

    Expr *visitOpenExistentialExpr(OpenExistentialExpr *expr) {
      llvm_unreachable("Already type-checked");
    }
    Expr *visitInOutConversionExpr(InOutConversionExpr *expr) {
      llvm_unreachable("Already type-checked");
    }
    
    void finalize() {
      // Check that all value type methods were fully applied.
      auto &tc = cs.getTypeChecker();
      for (auto &unapplied : InvalidPartialApplications) {
        tc.diagnose(unapplied.first->getLoc(),
                    diag::partial_application_of_method_invalid,
                    unapplied.second.kind);
      }

      // We should have complained above if there were any
      // existentials that haven't been closed yet.
      assert((OpenedExistentials.empty() || 
              !InvalidPartialApplications.empty()) &&
             "Opened existentials have not been closed");
    }
  };
}

/// \brief Given a constraint locator, find the owner of default arguments for
/// that tuple, i.e., a FuncDecl.
static AbstractFunctionDecl *
findDefaultArgsOwner(ConstraintSystem &cs, const Solution &solution,
                     ConstraintLocator *locator) {
  if (locator->getPath().empty() || !locator->getAnchor())
    return nullptr;

  // If the locator points to a function application, find the function itself.
  if (locator->getPath().back().getKind() == ConstraintLocator::ApplyArgument) {
    assert(locator->getPath().back().getNewSummaryFlags() == 0 &&
           "ApplyArgument adds no flags");
    SmallVector<LocatorPathElt, 4> newPath;
    newPath.append(locator->getPath().begin(), locator->getPath().end()-1);
    unsigned newFlags = locator->getSummaryFlags();

    // If we have an interpolation argument, dig out the constructor if we
    // can.
    // FIXME: This representation is actually quite awful
    if (newPath.size() == 1 &&
        newPath[0].getKind() == ConstraintLocator::InterpolationArgument) {
      newPath.push_back(ConstraintLocator::ConstructorMember);

      locator = cs.getConstraintLocator(locator->getAnchor(), newPath, newFlags);
      auto known = solution.overloadChoices.find(locator);
      if (known != solution.overloadChoices.end()) {
        auto &choice = known->second.choice;
        if (choice.getKind() == OverloadChoiceKind::Decl)
          return cast<AbstractFunctionDecl>(choice.getDecl());
      }
      return nullptr;
    } else {
      newPath.push_back(ConstraintLocator::ApplyFunction);
    }
    assert(newPath.back().getNewSummaryFlags() == 0 &&
           "added element that changes the flags?");
    locator = cs.getConstraintLocator(locator->getAnchor(), newPath, newFlags);
  }

  // Simplify the locator.
  SourceRange range1, range2;
  locator = simplifyLocator(cs, locator, range1, range2);

  // If we didn't map down to a specific expression, we can't handle a default
  // argument.
  if (!locator->getAnchor() || !locator->getPath().empty())
    return nullptr;

  if (auto resolved
        = resolveLocatorToDecl(cs, locator,
            [&](ConstraintLocator *locator) -> Optional<OverloadChoice> {
              auto known = solution.overloadChoices.find(locator);
              if (known == solution.overloadChoices.end()) {
                return Nothing;
              }

              return known->second.choice;
            })) {
    return cast<AbstractFunctionDecl>(resolved.getDecl());
  }

  return nullptr;
}

/// Produce the caller-side default argument for this default argument, or
/// null if the default argument will be provided by the callee.
static Expr *getCallerDefaultArg(TypeChecker &tc, DeclContext *dc,
                                 SourceLoc loc, AbstractFunctionDecl *&owner,
                                 unsigned index) {
  auto defArg = owner->getDefaultArg(index);
  MagicIdentifierLiteralExpr::Kind magicKind;
  switch (defArg.first) {
  case DefaultArgumentKind::None:
    llvm_unreachable("No default argument here?");

  case DefaultArgumentKind::Normal:
    return nullptr;

  case DefaultArgumentKind::Inherited:
    // Update the owner to reflect inheritance here.
    owner = owner->getOverriddenDecl();
    return getCallerDefaultArg(tc, dc, loc, owner, index);

  case DefaultArgumentKind::Column:
    magicKind = MagicIdentifierLiteralExpr::Column;
    break;

  case DefaultArgumentKind::File:
    magicKind = MagicIdentifierLiteralExpr::File;
    break;
    
  case DefaultArgumentKind::Line:
    magicKind = MagicIdentifierLiteralExpr::Line;
    break;
      
  case DefaultArgumentKind::Function:
    magicKind = MagicIdentifierLiteralExpr::Function;
    break;
  }

  // Create the default argument, which is a converted magic identifier
  // literal expression.
  Expr *init = new (tc.Context) MagicIdentifierLiteralExpr(magicKind, loc,
                                                           /*Implicit=*/true);
  bool invalid = tc.typeCheckExpression(init, dc, defArg.second,
                                        /*discardedExpr=*/false);
  assert(!invalid && "conversion cannot fail");
  (void)invalid;
  return init;
}

Expr *ExprRewriter::coerceTupleToTuple(Expr *expr, TupleType *fromTuple,
                                       TupleType *toTuple,
                                       ConstraintLocatorBuilder locator,
                                       SmallVectorImpl<int> &sources,
                                       SmallVectorImpl<unsigned> &variadicArgs){
  auto &tc = cs.getTypeChecker();

  // Capture the tuple expression, if there is one.
  Expr *innerExpr = expr;
  while (auto paren = dyn_cast<IdentityExpr>(innerExpr))
    innerExpr = paren->getSubExpr();
  TupleExpr *fromTupleExpr = dyn_cast<TupleExpr>(innerExpr);

  /// Check each of the tuple elements in the destination.
  bool hasVarArg = false;
  bool anythingShuffled = false;
  bool hasInits = false;
  SmallVector<TupleTypeElt, 4> toSugarFields;
  SmallVector<TupleTypeElt, 4> fromTupleExprFields(
                                 fromTuple->getFields().size());
  SmallVector<Expr *, 2> callerDefaultArgs;
  AbstractFunctionDecl *defaultArgsOwner = nullptr;
  for (unsigned i = 0, n = toTuple->getFields().size(); i != n; ++i) {
    const auto &toElt = toTuple->getFields()[i];
    auto toEltType = toElt.getType();

    // If we're default-initializing this member, there's nothing to do.
    if (sources[i] == TupleShuffleExpr::DefaultInitialize) {
      // Dig out the owner of the default arguments.
      if (!defaultArgsOwner) {
        defaultArgsOwner 
          = findDefaultArgsOwner(cs, solution,
                                 cs.getConstraintLocator(locator));
        assert(defaultArgsOwner && "Missing default arguments owner?");
      } else {
        assert(findDefaultArgsOwner(cs, solution, 
                                    cs.getConstraintLocator(locator))
                 == defaultArgsOwner);
      }

      anythingShuffled = true;
      hasInits = true;
      toSugarFields.push_back(toElt);

      // Create a caller-side default argument, if we need one.
      if (auto defArg = getCallerDefaultArg(tc, dc, expr->getLoc(),
                                            defaultArgsOwner, i)) {
        callerDefaultArgs.push_back(defArg);
        sources[i] = TupleShuffleExpr::CallerDefaultInitialize;
      }
      continue;
    }

    // If this is the variadic argument, note it.
    if (sources[i] == TupleShuffleExpr::FirstVariadic) {
      assert(i == n-1 && "Vararg not at the end?");
      toSugarFields.push_back(toElt);
      hasVarArg = true;
      anythingShuffled = true;
      continue;
    }

    // If the source and destination index are different, we'll be shuffling.
    if ((unsigned)sources[i] != i) {
      anythingShuffled = true;
    }

    // We're matching one element to another. If the types already
    // match, there's nothing to do.
    const auto &fromElt = fromTuple->getFields()[sources[i]];
    auto fromEltType = fromElt.getType();
    if (fromEltType->isEqual(toEltType)) {
      // Get the sugared type directly from the tuple expression, if there
      // is one.
      if (fromTupleExpr)
        fromEltType = fromTupleExpr->getElement(sources[i])->getType();

      toSugarFields.push_back(TupleTypeElt(fromEltType,
                                           toElt.getName(),
                                           toElt.getDefaultArgKind(),
                                           toElt.isVararg()));
      fromTupleExprFields[sources[i]] = fromElt;
      hasInits |= toElt.hasInit();
      continue;
    }

    // We need to convert the source element to the destination type.
    if (!fromTupleExpr) {
      // FIXME: Lame! We can't express this in the AST.
      tc.diagnose(expr->getLoc(),
                  diag::tuple_conversion_not_expressible,
                  fromTuple, toTuple);
      return nullptr;
    }

    // Actually convert the source element.
    auto convertedElt
      = coerceToType(fromTupleExpr->getElement(sources[i]), toEltType,
                     locator.withPathElement(
                       LocatorPathElt::getTupleElement(sources[i])));
    if (!convertedElt)
      return nullptr;

    fromTupleExpr->setElement(sources[i], convertedElt);

    // Record the sugared field name.
    toSugarFields.push_back(TupleTypeElt(convertedElt->getType(),
                                         toElt.getName(),
                                         toElt.getDefaultArgKind(),
                                         toElt.isVararg()));
    fromTupleExprFields[sources[i]] = TupleTypeElt(convertedElt->getType(),
                                                   fromElt.getName(),
                                                   fromElt.getDefaultArgKind(),
                                                   fromElt.isVararg());
    hasInits |= toElt.hasInit();
  }

  // Convert all of the variadic arguments to the destination type.
  Expr *injectionFn = nullptr;
  if (hasVarArg) {
    Type toEltType = toTuple->getFields().back().getVarargBaseTy();
    for (int fromFieldIdx : variadicArgs) {
      const auto &fromElt = fromTuple->getFields()[fromFieldIdx];
      Type fromEltType = fromElt.getType();

      // If the source and destination types match, there's nothing to do.
      if (toEltType->isEqual(fromEltType)) {
        sources.push_back(fromFieldIdx);
        fromTupleExprFields[fromFieldIdx] = fromElt;
        continue;
      }

      // We need to convert the source element to the destination type.
      if (!fromTupleExpr) {
        // FIXME: Lame! We can't express this in the AST.
        tc.diagnose(expr->getLoc(),
                    diag::tuple_conversion_not_expressible,
                    fromTuple, toTuple);
        return nullptr;
      }

      // Actually convert the source element.
      auto convertedElt = coerceToType(
                            fromTupleExpr->getElement(fromFieldIdx),
                            toEltType,
                            locator.withPathElement(
                              LocatorPathElt::getTupleElement(fromFieldIdx)));
      if (!convertedElt)
        return nullptr;

      fromTupleExpr->setElement(fromFieldIdx, convertedElt);
      sources.push_back(fromFieldIdx);

      fromTupleExprFields[fromFieldIdx] = TupleTypeElt(
                                            convertedElt->getType(),
                                            fromElt.getName(),
                                            fromElt.getDefaultArgKind(),
                                            fromElt.isVararg());
    }

    // Find the appropriate injection function.
    ArraySliceType *sliceType
      = cast<ArraySliceType>(
          toTuple->getFields().back().getType().getPointer());
    Type boundType = BuiltinIntegerType::getWordType(tc.Context);
    injectionFn = tc.buildArrayInjectionFnRef(dc,
                                              sliceType, boundType,
                                              expr->getStartLoc());
    if (!injectionFn)
      return nullptr;
  }

  // Compute the updated 'from' tuple type, since we may have
  // performed some conversions in place.
  Type fromTupleType = TupleType::get(fromTupleExprFields, tc.Context);
  if (fromTupleExpr) {
    fromTupleExpr->setType(fromTupleType);

    // Update the types of parentheses around the tuple expression.
    for (auto paren = dyn_cast<IdentityExpr>(expr); paren;
         paren = dyn_cast<IdentityExpr>(paren->getSubExpr()))
      paren->setType(fromTupleType);
  }

  // Compute the re-sugared tuple type.
  Type toSugarType = hasInits? toTuple
                             : TupleType::get(toSugarFields, tc.Context);

  // If we don't have to shuffle anything, we're done.
  if (!anythingShuffled && fromTupleExpr) {
    fromTupleExpr->setType(toSugarType);

    // Update the types of parentheses around the tuple expression.
    for (auto paren = dyn_cast<IdentityExpr>(expr); paren;
         paren = dyn_cast<IdentityExpr>(paren->getSubExpr()))
      paren->setType(toSugarType);

    return expr;
  }
  
  // Create the tuple shuffle.
  ArrayRef<int> mapping = tc.Context.AllocateCopy(sources);
  auto callerDefaultArgsCopy = tc.Context.AllocateCopy(callerDefaultArgs);
  auto shuffle = new (tc.Context) TupleShuffleExpr(expr, mapping, 
                                                   defaultArgsOwner,
                                                   callerDefaultArgsCopy,
                                                   toSugarType);
  shuffle->setVarargsInjectionFunction(injectionFn);
  return shuffle;
}



Expr *ExprRewriter::coerceScalarToTuple(Expr *expr, TupleType *toTuple,
                                        int toScalarIdx,
                                        ConstraintLocatorBuilder locator) {
  auto &tc = solution.getConstraintSystem().getTypeChecker();

  // If the destination type is variadic, compute the injection function to use.
  Expr *injectionFn = nullptr;
  const auto &lastField = toTuple->getFields().back();

  if (lastField.isVararg()) {
    // Find the appropriate injection function.
    ArraySliceType *sliceType
      = cast<ArraySliceType>(lastField.getType().getPointer());
    Type boundType = BuiltinIntegerType::getWordType(tc.Context);
    injectionFn = tc.buildArrayInjectionFnRef(dc,
                                              sliceType, boundType,
                                              expr->getStartLoc());
    if (!injectionFn)
      return nullptr;
  }

  // If we're initializing the varargs list, use its base type.
  const auto &field = toTuple->getFields()[toScalarIdx];
  Type toScalarType;
  if (field.isVararg())
    toScalarType = field.getVarargBaseTy();
  else
    toScalarType = field.getType();

  // Coerce the expression to the type to the scalar type.
  expr = coerceToType(expr, toScalarType,
                      locator.withPathElement(
                        ConstraintLocator::ScalarToTuple));
  if (!expr)
    return nullptr;

  // Preserve the sugar of the scalar field.
  // FIXME: This doesn't work if the type has default values because they fail
  // to canonicalize.
  SmallVector<TupleTypeElt, 4> sugarFields;
  bool hasInit = false;
  int i = 0;
  for (auto &field : toTuple->getFields()) {
    if (field.hasInit()) {
      hasInit = true;
      break;
    }

    if (i == toScalarIdx) {
      if (field.isVararg()) {
        assert(expr->getType()->isEqual(field.getVarargBaseTy()) &&
               "scalar field is not equivalent to dest vararg field?!");

        sugarFields.push_back(TupleTypeElt(field.getType(),
                                           field.getName(),
                                           field.getDefaultArgKind(),
                                           true));
      }
      else {
        assert(expr->getType()->isEqual(field.getType()) &&
               "scalar field is not equivalent to dest tuple field?!");
        sugarFields.push_back(TupleTypeElt(expr->getType(),
                                           field.getName()));
      }

      // Record the
    } else {
      sugarFields.push_back(field);
    }
    ++i;
  }

  // Compute the elements of the resulting tuple.
  SmallVector<ScalarToTupleExpr::Element, 4> elements;
  AbstractFunctionDecl *defaultArgsOwner = nullptr;
  i = 0;
  for (auto &field : toTuple->getFields()) {
    // Use a null entry to indicate that this is the scalar field.
    if (i == toScalarIdx) {
      elements.push_back(ScalarToTupleExpr::Element());
      ++i;
      continue;
    }

    if (field.isVararg()) {
      ++i;
      continue;
    }

    assert(field.hasInit() && "Expected a default argument");

    // Dig out the owner of the default arguments.
    if (!defaultArgsOwner) {
      defaultArgsOwner
      = findDefaultArgsOwner(cs, solution,
                             cs.getConstraintLocator(locator));
      assert(defaultArgsOwner && "Missing default arguments owner?");
    } else {
      assert(findDefaultArgsOwner(cs, solution,
                                  cs.getConstraintLocator(locator))
             == defaultArgsOwner);
    }

    // Create a caller-side default argument, if we need one.
    if (auto defArg = getCallerDefaultArg(tc, dc, expr->getLoc(),
                                          defaultArgsOwner, i)) {
      // Record the caller-side default argument expression.
      // FIXME: Do we need to record what this was synthesized from?
      elements.push_back(defArg);
    } else {
      // Record the owner of the default argument.
      elements.push_back(defaultArgsOwner);
    }

    ++i;
  }

  Type destSugarTy = hasInit? toTuple
                            : TupleType::get(sugarFields, tc.Context);

  return new (tc.Context) ScalarToTupleExpr(expr, destSugarTy,
                                            tc.Context.AllocateCopy(elements),
                                            injectionFn);
}

Expr *ExprRewriter::coerceExistential(Expr *expr, Type toType,
                                      ConstraintLocatorBuilder locator) {
  auto &tc = solution.getConstraintSystem().getTypeChecker();
  Type fromType = expr->getType();

  // Compute the conformances for each of the protocols in the existential
  // type.
  SmallVector<ProtocolDecl *, 4> protocols;
  toType->getAnyExistentialTypeProtocols(protocols);
  SmallVector<ProtocolConformance *, 4> conformances;
  for (auto proto : protocols) {
    ProtocolConformance *conformance = nullptr;
    bool conforms = tc.conformsToProtocol(fromType, proto, cs.DC, &conformance);
    assert(conforms && "Type does not conform to protocol?");
    (void)conforms;
    conformances.push_back(conformance);
  }

  // If we have all of the conformances we need, create an erasure expression.
  return new (tc.Context) ErasureExpr(expr, toType,
                                      tc.Context.AllocateCopy(conformances));
}

Expr *ExprRewriter::coerceViaUserConversion(Expr *expr, Type toType,
                                            ConstraintLocatorBuilder locator) {
  auto &tc = solution.getConstraintSystem().getTypeChecker();

  // Determine the locator that corresponds to the conversion member.
  auto storedLocator
    = cs.getConstraintLocator(
        locator.withPathElement(ConstraintLocator::ConversionMember));
  auto knownOverload = solution.overloadChoices.find(storedLocator);
  if (knownOverload != solution.overloadChoices.end()) {
    auto selected = knownOverload->second;

    // FIXME: Location information is suspect throughout.
    // Form a reference to the conversion member.
    auto memberRef = buildMemberRef(expr,
                                    selected.openedFullType,
                                    expr->getStartLoc(),
                                    selected.choice.getDecl(),
                                    expr->getEndLoc(),
                                    selected.openedType,
                                    locator,
                                    /*Implicit=*/true, /*direct ivar*/false);

    // Form an empty tuple.
    Expr *args = new (tc.Context) TupleExpr(expr->getStartLoc(),
                                            expr->getEndLoc(),
                                            /*Implicit=*/true,
                                            TupleType::getEmpty(tc.Context));

    // Call the conversion function with an empty tuple.
    ApplyExpr *apply = new (tc.Context) CallExpr(memberRef, args,
                                                 /*Implicit=*/true);
    auto openedType = selected.openedType->castTo<FunctionType>()->getResult();
    expr = finishApply(apply, openedType,
                       ConstraintLocatorBuilder(
                         cs.getConstraintLocator(apply)));

    if (!expr)
      return nullptr;

    return coerceToType(expr, toType, locator);
  }

  // If there was no conversion member, look for a constructor member.
  // This is only used for handling interpolated string literals, where
  // we allow construction or conversion.
  storedLocator
    = cs.getConstraintLocator(
        locator.withPathElement(ConstraintLocator::ConstructorMember));
  knownOverload = solution.overloadChoices.find(storedLocator);
  assert(knownOverload != solution.overloadChoices.end());

  auto selected = knownOverload->second;

  // FIXME: Location information is suspect throughout.
  // Form a reference to the constructor.

  // Form a reference to the constructor or enum declaration.
  Expr *typeBase = new (tc.Context) MetatypeExpr(
                                      nullptr,
                                      expr->getStartLoc(),
                                      MetatypeType::get(toType));
  Expr *declRef = buildMemberRef(typeBase,
                                 selected.openedFullType,
                                 expr->getStartLoc(),
                                 selected.choice.getDecl(),
                                 expr->getStartLoc(),
                                 selected.openedType,
                                 storedLocator,
                                 /*Implicit=*/true, /*direct ivar*/false);

  // FIXME: Lack of openedType here is an issue.
  ApplyExpr *apply = new (tc.Context) CallExpr(declRef, expr,
                                               /*Implicit=*/true);
  expr = finishApply(apply, toType, locator);
  if (!expr)
    return nullptr;

  return coerceToType(expr, toType, locator);
}

Expr *ExprRewriter::coerceOptionalToOptional(Expr *expr, Type toType,
                                             ConstraintLocatorBuilder locator) {
  auto &tc = cs.getTypeChecker();
  Type fromType = expr->getType();
  
  auto fromGenericType = fromType->castTo<BoundGenericType>();
  auto toGenericType = toType->castTo<BoundGenericType>();
  assert(fromGenericType->getDecl()->classifyAsOptionalType());
  assert(toGenericType->getDecl()->classifyAsOptionalType());
  tc.requireOptionalIntrinsics(expr->getLoc());

  Type fromValueType = fromGenericType->getGenericArgs()[0];
  Type toValueType = toGenericType->getGenericArgs()[0];

  expr = new (tc.Context) BindOptionalExpr(expr, expr->getSourceRange().End,
                                           /*depth*/ 0, fromValueType);
  expr->setImplicit(true);
  expr = coerceToType(expr, toValueType, locator);
  if (!expr) return nullptr;
      
  expr = new (tc.Context) InjectIntoOptionalExpr(expr, toType);
      
  expr = new (tc.Context) OptionalEvaluationExpr(expr, toType);
  expr->setImplicit(true);
  return expr;
}

Expr *ExprRewriter::coerceUncheckedOptionalToValue(Expr *expr, Type objTy,
                                            ConstraintLocatorBuilder locator) {
  // Coerce to an r-value.
  auto rvalueTy = expr->getType()->getRValueType();
  assert(rvalueTy->getUncheckedOptionalObjectType()->isEqual(objTy));

  if (cs.getTypeChecker().requireOptionalIntrinsics(expr->getLoc()))
    return nullptr;

  expr = coerceToType(expr, rvalueTy, /*bogus?*/ locator);
  if (!expr) return nullptr;

  expr = new (cs.getTypeChecker().Context) ForceValueExpr(expr, expr->getEndLoc());
  expr->setType(objTy);
  expr->setImplicit();
  return expr;
}

Expr *ExprRewriter::coerceToType(Expr *expr, Type toType,
                                 ConstraintLocatorBuilder locator) {
  auto &tc = cs.getTypeChecker();

  // The type we're converting from.
  Type fromType = expr->getType();

  // If the types are already equivalent, we don't have to do anything.
  if (fromType->isEqual(toType))
    return expr;

  // If the solver recorded what we should do here, just do it immediately.
  auto knownRestriction = solution.ConstraintRestrictions.find(
                            { fromType->getCanonicalType(),
                              toType->getCanonicalType() });
  if (knownRestriction != solution.ConstraintRestrictions.end()) {
    switch (knownRestriction->second) {
    case ConversionRestrictionKind::TupleToTuple: {
      auto fromTuple = expr->getType()->castTo<TupleType>();
      auto toTuple = toType->castTo<TupleType>();
      SmallVector<int, 4> sources;
      SmallVector<unsigned, 4> variadicArgs;
      bool failed = computeTupleShuffle(fromTuple, toTuple, sources,
                                        variadicArgs,
                                        hasMandatoryTupleLabels(expr));
      assert(!failed && "Couldn't convert tuple to tuple?");
      (void)failed;
      return coerceTupleToTuple(expr, fromTuple, toTuple, locator, sources,
                                variadicArgs);
    }

    case ConversionRestrictionKind::ScalarToTuple: {
      auto toTuple = toType->castTo<TupleType>();
      return coerceScalarToTuple(expr, toTuple,
                                 toTuple->getFieldForScalarInit(), locator);
    }

    case ConversionRestrictionKind::TupleToScalar: {
      // If this was a single-element tuple expression, reach into that
      // subexpression.
      // FIXME: This is a hack to deal with @lvalue-ness issues. It loses
      // source information.
      if (auto fromTupleExpr = dyn_cast<TupleExpr>(expr)) {
        if (fromTupleExpr->getNumElements() == 1) {
          return coerceToType(fromTupleExpr->getElement(0), toType,
                              locator.withPathElement(
                                LocatorPathElt::getTupleElement(0)));
        }
      }

      // Extract the element.
      auto fromTuple = fromType->castTo<TupleType>();
      expr = new (cs.getASTContext()) TupleElementExpr(
                                        expr,
                                        expr->getLoc(),
                                        0,
                                        expr->getLoc(),
                                        fromTuple->getElementType(0));
      expr->setImplicit(true);

      // Coerce the element to the expected type.
      return coerceToType(expr, toType,
                          locator.withPathElement(
                            LocatorPathElt::getTupleElement(0)));
    }

    case ConversionRestrictionKind::DeepEquality:
      llvm_unreachable("Equality handled above");

    case ConversionRestrictionKind::Superclass: {
      // Coercion from archetype to its (concrete) superclass.
      if (auto fromArchetype = fromType->getAs<ArchetypeType>()) {
        expr = new (tc.Context) ArchetypeToSuperExpr(
                                  expr,
                                  fromArchetype->getSuperclass());

        // If we are done succeeded, use the coerced result.
        if (expr->getType()->isEqual(toType)) {
          return expr;
        }

        fromType = expr->getType();
      }
      
      // Coercion from subclass to superclass.
      return new (tc.Context) DerivedToBaseExpr(expr, toType);
    }

    case ConversionRestrictionKind::LValueToRValue: {
      // Load from the lvalue.
      expr = new (tc.Context) LoadExpr(expr, fromType->getRValueType());

      // Coerce the result.
      return coerceToType(expr, toType, locator);
    }

    case ConversionRestrictionKind::Existential:
      return coerceExistential(expr, toType, locator);

    case ConversionRestrictionKind::ValueToOptional: {
      auto toGenericType = toType->castTo<BoundGenericType>();
      assert(toGenericType->getDecl()->classifyAsOptionalType());
      tc.requireOptionalIntrinsics(expr->getLoc());

      Type valueType = toGenericType->getGenericArgs()[0];
      expr = coerceToType(expr, valueType, locator);
      if (!expr) return nullptr;

      return new (tc.Context) InjectIntoOptionalExpr(expr, toType);
    }

    case ConversionRestrictionKind::OptionalToUncheckedOptional:
    case ConversionRestrictionKind::UncheckedOptionalToOptional:
    case ConversionRestrictionKind::OptionalToOptional:
      return coerceOptionalToOptional(expr, toType, locator);

    case ConversionRestrictionKind::ForceUnchecked: {
      auto valueTy = fromType->getUncheckedOptionalObjectType();
      assert(valueTy);
      expr = coerceUncheckedOptionalToValue(expr, valueTy, locator);
      if (!expr) return nullptr;
      return coerceToType(expr, toType, locator);
    }

    case ConversionRestrictionKind::User:
      return coerceViaUserConversion(expr, toType, locator);
    }
  }

  // Tuple-to-scalar conversion.
  // FIXME: Will go away when tuple labels go away.
  if (auto fromTuple = fromType->getAs<TupleType>()) {
    if (fromTuple->getNumElements() == 1 &&
        !fromTuple->getFields()[0].isVararg() &&
        !toType->is<TupleType>()) {
      expr = new (cs.getASTContext()) TupleElementExpr(
                                        expr,
                                        expr->getLoc(),
                                        0,
                                        expr->getLoc(),
                                        fromTuple->getElementType(0));
      expr->setImplicit(true);
    }
  }

  // Coercions from an lvalue: load or perform implicit address-of. We perform
  // these coercions first because they are often the first step in a multi-step
  // coercion.
  if (auto fromLValue = fromType->getAs<LValueType>()) {
    if (auto *toIO = toType->getAs<InOutType>()) {
      (void)toIO;
      // In an @assignment operator like "++i", the operand is converted from
      // an implicit lvalue to an inout argument.
      assert(toIO->getObjectType()->isEqual(fromLValue->getObjectType()));
      return new (tc.Context) InOutExpr(expr->getStartLoc(), expr,
                                            toType, /*isImplicit*/true);
    }

    // If we're actually turning this into an lvalue tuple element, don't
    // load.
    bool performLoad = true;
    if (auto toTuple = toType->getAs<TupleType>()) {
      int scalarIdx = toTuple->getFieldForScalarInit();
      if (scalarIdx >= 0 &&
          toTuple->getElementType(scalarIdx)->is<InOutType>())
        performLoad = false;
    }

    if (performLoad) {
      // Load from the lvalue.
      expr = new (tc.Context) LoadExpr(expr, fromLValue->getObjectType());

      // Coerce the result.
      return coerceToType(expr, toType, locator);
    }
  }

  // Coercions to tuple type.
  if (auto toTuple = toType->getAs<TupleType>()) {
    // Coerce from a tuple to a tuple.
    if (auto fromTuple = fromType->getAs<TupleType>()) {
      SmallVector<int, 4> sources;
      SmallVector<unsigned, 4> variadicArgs;
      if (!computeTupleShuffle(fromTuple, toTuple, sources, variadicArgs,
                               hasMandatoryTupleLabels(expr))) {
        return coerceTupleToTuple(expr, fromTuple, toTuple,
                                  locator, sources, variadicArgs);
      }
    }

    // Coerce scalar to tuple.
    int toScalarIdx = toTuple->getFieldForScalarInit();
    if (toScalarIdx != -1) {
      return coerceScalarToTuple(expr, toTuple, toScalarIdx, locator);
    }
  }

  // Coercion from a subclass to a superclass.
  if (fromType->mayHaveSuperclass() &&
      toType->getClassOrBoundGenericClass()) {
    for (auto fromSuperClass = tc.getSuperClassOf(fromType);
         fromSuperClass;
         fromSuperClass = tc.getSuperClassOf(fromSuperClass)) {
      if (fromSuperClass->isEqual(toType)) {
        
        // Coercion from archetype to its (concrete) superclass.
        if (auto fromArchetype = fromType->getAs<ArchetypeType>()) {
          expr = new (tc.Context) ArchetypeToSuperExpr(
                                    expr,
                                    fromArchetype->getSuperclass());

          // If we succeeded, use the coerced result.
          if (expr->getType()->isEqual(toType))
            return expr;
        }

        // Coercion from subclass to superclass.
        expr = new (tc.Context) DerivedToBaseExpr(expr, toType);
        return expr;
      }
    }
  }

  // Coercions to function type.
  if (auto toFunc = toType->getAs<FunctionType>()) {
    // Coercion to an autoclosure type produces an implicit closure.
    // FIXME: The type checker is more lenient, and allows @auto_closures to
    // be subtypes of non-@auto_closures, which is bogus.
    if (toFunc->isAutoClosure()) {
      // Convert the value to the expected result type of the function.
      expr = coerceToType(expr, toFunc->getResult(),
                          locator.withPathElement(ConstraintLocator::Load));

      // We'll set discriminator values on all the autoclosures in a
      // later pass.
      auto discriminator = AutoClosureExpr::InvalidDiscriminator;
      auto Closure = new (tc.Context) AutoClosureExpr(expr, toType,
                                                      discriminator, dc);
      Pattern *pattern = TuplePattern::create(tc.Context, expr->getLoc(),
                                              ArrayRef<TuplePatternElt>(),
                                              expr->getLoc());
      pattern->setType(TupleType::getEmpty(tc.Context));
      Closure->setParams(pattern);

      // Compute the capture list, now that we have analyzed the expression.
      tc.computeCaptures(Closure);

      return Closure;
    }

    // Coercion to a block function type from non-block function type.
    auto fromFunc = fromType->getAs<FunctionType>();
    if (toFunc->getRepresentation() == FunctionType::Representation::Block
        && (!fromFunc || fromFunc->getRepresentation() != FunctionType::Representation::Block)) {
      // Coerce the expression to the non-block form of the function type.
      auto toNonBlockTy = FunctionType::get(toFunc->getInput(),
                                            toFunc->getResult());
      expr = coerceToType(expr, toNonBlockTy, locator);

      // Bridge to the block form of this function type.
      return new (tc.Context) BridgeToBlockExpr(expr, toType);
    }

    // Coercion from one function type to another.
    if (fromFunc) {
      return new (tc.Context) FunctionConversionExpr(expr, toType);
    }
  }

  // Coercions from a type to an existential type.
  if (toType->isExistentialType()) {
    return coerceExistential(expr, toType, locator);
  }

  // Coercion to Optional<T>.
  if (auto toGenericType = toType->getAs<BoundGenericType>()) {
    if (toGenericType->getDecl()->classifyAsOptionalType()) {
      tc.requireOptionalIntrinsics(expr->getLoc());

      Type valueType = toGenericType->getGenericArgs()[0];
      expr = coerceToType(expr, valueType, locator);
      if (!expr) return nullptr;

      return new (tc.Context) InjectIntoOptionalExpr(expr, toType);
    }
  }

  // Coerce via conversion function or constructor.
  if (fromType->getNominalOrBoundGenericNominal()||
      fromType->is<ArchetypeType>() ||
      toType->getNominalOrBoundGenericNominal() ||
      toType->is<ArchetypeType>()) {
    return coerceViaUserConversion(expr, toType, locator);
  }

  // Coercion from one metatype to another.
  if (fromType->is<AnyMetatypeType>()) {
    if (auto toMeta = toType->getAs<AnyMetatypeType>()) {
      return new (tc.Context) MetatypeConversionExpr(expr, toMeta);
    }
  }

  llvm_unreachable("Unhandled coercion");
}

Expr *
ExprRewriter::coerceObjectArgumentToType(Expr *expr,
                                         Type baseTy, ValueDecl *member,
                                         bool IsDirectPropertyAccess,
                                         ConstraintLocatorBuilder locator) {
  Type toType = adjustSelfTypeForMember(baseTy, member, IsDirectPropertyAccess,
                                        dc);

  // If our expression already has the right type, we're done.
  Type fromType = expr->getType();
  if (fromType->isEqual(toType))
    return expr;

  // If we're coercing to an rvalue type, just do it.
  if (!toType->is<InOutType>())
    return coerceToType(expr, toType, locator);

  assert(fromType->is<LValueType>() && "Can only convert lvalues to inout");

  auto &ctx = cs.getTypeChecker().Context;

  // Use InOutExpr to convert it to an explicit inout argument for the
  // receiver.
  return new (ctx) InOutExpr(expr->getStartLoc(), expr,
                                 toType, /*isImplicit*/true);
}

Expr *ExprRewriter::convertLiteral(Expr *literal,
                                   Type type,
                                   Type openedType,
                                   ProtocolDecl *protocol,
                                   TypeOrName literalType,
                                   Identifier literalFuncName,
                                   ProtocolDecl *builtinProtocol,
                                   TypeOrName builtinLiteralType,
                                   Identifier builtinLiteralFuncName,
                                   bool (*isBuiltinArgType)(Type),
                                   Diag<> brokenProtocolDiag,
                                   Diag<> brokenBuiltinProtocolDiag) {
  auto &tc = cs.getTypeChecker();

  // Check whether this literal type conforms to the builtin protocol.
  ProtocolConformance *builtinConformance = nullptr;
  if (tc.conformsToProtocol(type, builtinProtocol, cs.DC, &builtinConformance)){
    // Find the builtin argument type we'll use.
    Type argType;
    if (builtinLiteralType.is<Type>())
      argType = builtinLiteralType.get<Type>();
    else
      argType = tc.getWitnessType(type, builtinProtocol,
                                  builtinConformance,
                                  builtinLiteralType.get<Identifier>(),
                                  brokenBuiltinProtocolDiag);
    if (!argType)
      return nullptr;

    // Make sure it's of an appropriate builtin type.
    if (isBuiltinArgType && !isBuiltinArgType(argType)) {
      tc.diagnose(builtinProtocol->getLoc(), brokenBuiltinProtocolDiag);
      return nullptr;
    }

    // The literal expression has this type.
    literal->setType(argType);

    // Call the builtin conversion operation.
    Expr *base = new (tc.Context) MetatypeExpr(nullptr, literal->getLoc(),
                                               MetatypeType::get(type));
    Expr *result = tc.callWitness(base, dc,
                                  builtinProtocol, builtinConformance,
                                  builtinLiteralFuncName,
                                  literal,
                                  brokenBuiltinProtocolDiag);
    if (result)
      result->setType(type);
    return result;
  }

  // This literal type must conform to the (non-builtin) protocol.
  assert(protocol && "requirements should have stopped recursion");
  ProtocolConformance *conformance = nullptr;
  bool conforms = tc.conformsToProtocol(type, protocol, cs.DC, &conformance);
  assert(conforms && "must conform to literal protocol");
  (void)conforms;

  // Figure out the (non-builtin) argument type.
  Type argType;
  if (literalType.is<Type>())
    argType = literalType.get<Type>();
  else
    argType = tc.getWitnessType(type, protocol, conformance,
                                literalType.get<Identifier>(),
                                brokenProtocolDiag);
  if (!argType)
    return nullptr;

  // Convert the literal to the non-builtin argument type via the
  // builtin protocol, first.
  // FIXME: Do we need an opened type here?
  literal = convertLiteral(literal, argType, argType, nullptr, Identifier(),
                           Identifier(), builtinProtocol,
                           builtinLiteralType, builtinLiteralFuncName,
                           isBuiltinArgType, brokenProtocolDiag,
                           brokenBuiltinProtocolDiag);
  if (!literal)
    return nullptr;

  // Convert the resulting expression to the final literal type.
  Expr *base = new (tc.Context) MetatypeExpr(nullptr, literal->getLoc(),
                                             MetatypeType::get(type));
  literal = tc.callWitness(base, dc,
                           protocol, conformance, literalFuncName,
                           literal, brokenProtocolDiag);
  if (literal)
    literal->setType(type);
  return literal;
}

Expr *ExprRewriter::finishApply(ApplyExpr *apply, Type openedType,
                                ConstraintLocatorBuilder locator) {
  TypeChecker &tc = cs.getTypeChecker();

  // Handle applications that implicitly look through UncheckedOptional<T>.
  auto fn = apply->getFn();
  if (auto fnTy = cs.lookThroughUncheckedOptionalType(fn->getType())) {
    fn = coerceUncheckedOptionalToValue(fn, fnTy, locator);
    if (!fn) return nullptr;
  }

  // The function is always an rvalue.
  fn = tc.coerceToRValue(fn);
  assert(fn && "Rvalue conversion failed?");
  if (!fn)
    return nullptr;

  // If we're applying a function that resulted from a covariant
  // function conversion, strip off that conversion.
  // FIXME: It would be nicer if we could build the ASTs properly in the
  // first shot.
  Type covariantResultType;
  if (auto covariant = dyn_cast<CovariantFunctionConversionExpr>(fn)) {
    // Strip off one layer of application from the covariant result.
    covariantResultType
      = covariant->getType()->castTo<AnyFunctionType>()->getResult();
   
    // Use the subexpression as the function.
    fn = covariant->getSubExpr();
  }

  apply->setFn(fn);

  // Check whether the argument is 'super'.
  bool isSuper = apply->getArg()->isSuperExpr();

  // For function application, convert the argument to the input type of
  // the function.
  if (auto fnType = fn->getType()->getAs<FunctionType>()) {
    auto origArg = apply->getArg();
    Expr *arg = coerceToType(origArg, fnType->getInput(),
                             locator.withPathElement(
                           ConstraintLocator::ApplyArgument));

    if (!arg) {
      return nullptr;
    }

    apply->setArg(arg);
    apply->setType(fnType->getResult());
    apply->setIsSuper(isSuper);

    assert(!apply->getType()->is<PolymorphicFunctionType>() &&
           "Polymorphic function type slipped through");
    Expr *result = tc.substituteInputSugarTypeForResult(apply);

    // If the result is an archetype from an opened existential, erase
    // the existential and create the OpenExistentialExpr.
    // FIXME: This is a localized form of a much more general rule for
    // placement of open existential expressions. It only works for 
    // DynamicSelf.
    OptionalTypeKind optKind;
    auto resultTy = result->getType();
    if (auto optValueTy = resultTy->getAnyOptionalObjectType(optKind)) {
      resultTy = optValueTy;
    }
    if (auto archetypeTy = resultTy->getAs<ArchetypeType>()) {
      auto opened = OpenedExistentials.find(archetypeTy);
      if (opened != OpenedExistentials.end()) {
        // Erase the archetype to its corresponding existential:
        auto openedTy = archetypeTy->getOpenedExistentialType();

        //   - Drill down to the optional value (if necessary).
        if (optKind) {
          result = new (tc.Context) BindOptionalExpr(result,
                                                     result->getEndLoc(),
                                                     0, archetypeTy);
          result->setImplicit(true);
        }

        //   - Coerce to an existential value.
        result = coerceToType(result, openedTy, locator);
        if (!result)
          return result;

        //   - Bind up the result back up as an optional (if necessary).
        if (optKind) {
          Type optOpenedTy = OptionalType::get(optKind, openedTy);
          result = new (tc.Context) InjectIntoOptionalExpr(result, optOpenedTy);
          result = new (tc.Context) OptionalEvaluationExpr(result, optOpenedTy);
        }

        // Create the expression that opens the existential.
        result = new (tc.Context) OpenExistentialExpr(
                                    opened->second.ExistentialValue,
                                    opened->second.OpaqueValue,
                                    result);        

        // Remove this from the set of opened existentials.
        OpenedExistentials.erase(opened);
      }
    }

    // If we have a covariant result type, perform the conversion now.
    if (covariantResultType) {
      if (covariantResultType->is<FunctionType>())
        result = new (tc.Context) CovariantFunctionConversionExpr(
                                    result,
                                    covariantResultType);
      else
        result = new (tc.Context) CovariantReturnConversionExpr(
                                    result, 
                                    covariantResultType);
    }

    return result;
  }

  // We have a type constructor.
  auto metaTy = fn->getType()->castTo<AnyMetatypeType>();
  auto ty = metaTy->getInstanceType();

  // If we're "constructing" a tuple type, it's simply a conversion.
  if (auto tupleTy = ty->getAs<TupleType>()) {
    // FIXME: Need an AST to represent this properly.
    return coerceToType(apply->getArg(), tupleTy, locator);
  }

  // We're constructing a value of nominal type. Look for the constructor or
  // enum element to use.
  assert(ty->getNominalOrBoundGenericNominal() || ty->is<DynamicSelfType>() ||
         ty->is<ArchetypeType>() || ty->isExistentialType());
  auto selected = getOverloadChoiceIfAvailable(
                    cs.getConstraintLocator(
                      locator.withPathElement(
                        ConstraintLocator::ConstructorMember)));

  // We have the constructor.
  auto choice = selected->choice;
  auto decl = choice.getDecl();

  // Consider the constructor decl reference expr 'implicit', but the
  // constructor call expr itself has the apply's 'implicitness'.
  Expr *declRef = buildMemberRef(fn,
                                 selected->openedFullType,
                                 /*DotLoc=*/SourceLoc(),
                                 decl, fn->getEndLoc(),
                                 selected->openedType, locator,
                                 /*Implicit=*/true, /*direct ivar*/false);
  declRef->setImplicit(apply->isImplicit());
  apply->setFn(declRef);

  // If we're constructing a class object, either the metatype must be
  // statically derived (rather than an arbitrary value of metatype type) or
  // the referenced constructor must be abstract.
  if ((ty->getClassOrBoundGenericClass() || ty->is<DynamicSelfType>()) &&
      !fn->isStaticallyDerivedMetatype() &&
      !cast<ConstructorDecl>(decl)->isRequired()) {
    tc.diagnose(apply->getLoc(), diag::dynamic_construct_class, ty)
      .highlight(fn->getSourceRange());
    auto ctor = cast<ConstructorDecl>(decl);
    // FIXME: Better description of the initializer than just it's type.
    if (ctor->isImplicit())
      tc.diagnose(decl, diag::note_nonrequired_implicit_initializer,
                  ctor->getArgumentType());
    else
      tc.diagnose(decl, diag::note_nonrequired_initializer);
  } else if (isa<ConstructorDecl>(decl) && ty->isExistentialType() &&
             fn->isStaticallyDerivedMetatype()) {
    tc.diagnose(apply->getLoc(), diag::static_construct_existential, ty)
      .highlight(fn->getSourceRange());
  }

  // Tail-recur to actually call the constructor.
  return finishApply(apply, openedType, locator);
}

/// \brief Apply a given solution to the expression, producing a fully
/// type-checked expression.
Expr *ConstraintSystem::applySolution(const Solution &solution,
                                      Expr *expr) {

  class ExprWalker : public ASTWalker {
    ExprRewriter &Rewriter;
    unsigned LeftSideOfAssignment = 0;

  public:
    ExprWalker(ExprRewriter &Rewriter) : Rewriter(Rewriter) { }

    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      // For a default-value expression, do nothing.
      if (isa<DefaultValueExpr>(expr)) {
        return { false, expr };
      }

      // For closures, update the parameter types and check the body.
      if (auto closure = dyn_cast<ClosureExpr>(expr)) {
        Rewriter.simplifyExprType(expr);
        auto &cs = Rewriter.getConstraintSystem();
        auto &tc = cs.getTypeChecker();

        // Coerce the pattern, in case we resolved something.
        auto fnType = closure->getType()->castTo<FunctionType>();
        Pattern *params = closure->getParams();
        TypeResolutionOptions TROptions;
        TROptions |= TR_OverrideType;
        TROptions |= TR_FromNonInferredPattern;
        if (tc.coercePatternToType(params, closure, fnType->getInput(),
                                   TROptions))
          return { false, nullptr };
        closure->setParams(params);

        // If this is a single-expression closure, convert the expression
        // in the body to the result type of the closure.
        if (closure->hasSingleExpressionBody()) {
          // Enter the context of the closure when type-checking the body.
          llvm::SaveAndRestore<DeclContext *> savedDC(Rewriter.dc, closure);
          Expr *body = closure->getSingleExpressionBody()->walk(*this);
          if (body)
            body = Rewriter.coerceToType(body,
                                         fnType->getResult(),
                                         cs.getConstraintLocator(
                                           closure,
                                           ConstraintLocator::ClosureResult));
          if (!body)
            return { false, nullptr } ;

          closure->setSingleExpressionBody(body);
        } else {
          // For other closures, type-check the body.
          tc.typeCheckClosureBody(closure);
        }

        // Compute the capture list, now that we have type-checked the body.
        tc.computeCaptures(closure);
        return { false, closure };
      }

      // Don't recurse into metatype expressions that have a specified type.
      if (auto metatypeExpr = dyn_cast<MetatypeExpr>(expr)) {
        if (metatypeExpr->getBaseTypeRepr())
          return { false, expr };
      }

      // Track whether we're in the left-hand side of an assignment...
      if (auto assign = dyn_cast<AssignExpr>(expr)) {
        ++LeftSideOfAssignment;
        
        if (auto dest = assign->getDest()->walk(*this))
          assign->setDest(dest);
        else
          return { false, nullptr };
        
        --LeftSideOfAssignment;

        auto &cs = Rewriter.getConstraintSystem();
        auto srcLocator = cs.getConstraintLocator(
                            assign->getSrc(),
                            ConstraintLocator::AssignSource);

        if (auto src = assign->getSrc()->walk(*this))
          assign->setSrc(src);
        else
          return { false, nullptr };
        
        expr = Rewriter.visitAssignExpr(assign, srcLocator);
        return { false, expr };
      }
      
      // ...so we can verify that '_' only appears there.
      if (isa<DiscardAssignmentExpr>(expr) && LeftSideOfAssignment == 0)
        Rewriter.getConstraintSystem().getTypeChecker()
          .diagnose(expr->getLoc(), diag::discard_expr_outside_of_assignment);
      
      return { true, expr };
    }

    Expr *walkToExprPost(Expr *expr) override {
      return Rewriter.visit(expr);
    }

    /// \brief Ignore statements.
    std::pair<bool, Stmt *> walkToStmtPre(Stmt *stmt) override {
      return { false, stmt };
    }

    /// \brief Ignore declarations.
    bool walkToDeclPre(Decl *decl) override { return false; }
  };

  ExprRewriter rewriter(*this, solution);
  ExprWalker walker(rewriter);
  auto result = expr->walk(walker);
  rewriter.finalize();
  return result;
}

Expr *ConstraintSystem::applySolutionShallow(const Solution &solution,
                                             Expr *expr) {
  ExprRewriter rewriter(*this, solution);
  return rewriter.visit(expr);
}

Expr *Solution::coerceToType(Expr *expr, Type toType,
                             ConstraintLocator *locator) const {
  auto &cs = getConstraintSystem();
  ExprRewriter rewriter(cs, *this);
  return rewriter.coerceToType(expr, toType, locator);
}

Expr *TypeChecker::callWitness(Expr *base, DeclContext *dc,
                               ProtocolDecl *protocol,
                               ProtocolConformance *conformance,
                               Identifier name,
                               MutableArrayRef<Expr *> arguments,
                               Diag<> brokenProtocolDiag) {
  // Construct an empty constraint system and solution.
  ConstraintSystem cs(*this, dc);

  // Find the witness we need to use.
  auto type = base->getType();
  if (auto metaType = type->getAs<AnyMetatypeType>())
    type = metaType->getInstanceType();
  
  auto witness = findNamedWitness(*this, dc, type->getRValueType(), protocol,
                                  name, brokenProtocolDiag);
  if (!witness)
    return nullptr;

  // Form a reference to the witness itself.
  Type openedFullType, openedType;
  std::tie(openedFullType, openedType)
    = cs.getTypeOfMemberReference(base->getType(), witness,
                                  /*isTypeReference=*/false,
                                  /*isDynamicResult=*/false);
  auto locator = cs.getConstraintLocator(base);

  // Form the call argument.
  Expr *arg;
  if (arguments.size() == 1)
    arg = arguments[0];
  else {
    SmallVector<TupleTypeElt, 4> elementTypes;
    for (auto elt : arguments)
      elementTypes.push_back(TupleTypeElt(elt->getType()));

    arg = new (Context) TupleExpr(base->getStartLoc(),
                                  Context.AllocateCopy(arguments),
                                  nullptr,
                                  base->getEndLoc(),
                                  /*hasTrailingClosure=*/false,
                                  /*Implicit=*/true,
                                  TupleType::get(elementTypes, Context));
  }

  // Add the conversion from the argument to the function parameter type.
  cs.addConstraint(ConstraintKind::Conversion, arg->getType(),
                   openedType->castTo<FunctionType>()->getInput(),
                   cs.getConstraintLocator(arg,
                                           ConstraintLocator::ApplyArgument));

  // Solve the system.
  SmallVector<Solution, 1> solutions;
  bool failed = cs.solve(solutions);
  (void)failed;
  assert(!failed && "Unable to solve for call to witness?");

  Solution &solution = solutions.front();
  ExprRewriter rewriter(cs, solution);

  auto memberRef = rewriter.buildMemberRef(base, openedFullType,
                                           base->getStartLoc(),
                                           witness, base->getEndLoc(),
                                           openedType, locator,
                                           /*Implicit=*/true,
                                           /*direct ivar*/false);

  // Call the witness.
  ApplyExpr *apply = new (Context) CallExpr(memberRef, arg, /*Implicit=*/true);
  return rewriter.finishApply(apply, openedType,
                              cs.getConstraintLocator(arg));
}

/// \brief Convert an expression via a builtin protocol.
///
/// \param solution The solution to the expression's constraint system,
/// which must have included a constraint that the expression's type
/// conforms to the give \c protocol.
/// \param expr The expression to convert.
/// \param locator The locator describing where the conversion occurs.
/// \param protocol The protocol to use for conversion.
/// \param generalName The name of the protocol method to use for the
/// conversion.
/// \param builtinName The name of the builtin method to use for the
/// last step of the conversion.
/// \param brokenProtocolDiag Diagnostic to emit if the protocol
/// definition is missing.
/// \param brokenBuiltinDiag Diagnostic to emit if the builtin definition
/// is broken.
///
/// \returns the converted expression.
static Expr *convertViaBuiltinProtocol(const Solution &solution,
                                       Expr *expr,
                                       ConstraintLocator *locator,
                                       ProtocolDecl *protocol,
                                       Identifier generalName,
                                       Identifier builtinName,
                                       Diag<> brokenProtocolDiag,
                                       Diag<> brokenBuiltinDiag) {
  auto &cs = solution.getConstraintSystem();
  ExprRewriter rewriter(cs, solution);

  // FIXME: Cache name.
  auto &tc = cs.getTypeChecker();
  auto &ctx = tc.Context;
  auto type = expr->getType();

  // Look for the builtin name. If we don't have it, we need to call the
  // general name via the witness table.
  auto witnesses = tc.lookupMember(type->getRValueType(), builtinName, cs.DC);
  if (!witnesses) {
    // Find the witness we need to use.
    auto witness = findNamedWitness(tc, cs.DC, type->getRValueType(), protocol,
                                    generalName, brokenProtocolDiag);

    // Form a reference to this member.
    Expr *memberRef = new (ctx) MemberRefExpr(expr, expr->getStartLoc(),
                                              witness, expr->getEndLoc(),
                                              /*Implicit=*/true);
    bool failed = tc.typeCheckExpressionShallow(memberRef, cs.DC);
    if (failed) {
      // If the member reference expression failed to type check, the Expr's
      // type does not conform to the given protocol.
      tc.diagnose(expr->getLoc(),
                  diag::type_does_not_conform,
                  type,
                  protocol->getType());
      return nullptr;
    }

    // Call the witness.
    Expr *arg = new (ctx) TupleExpr(expr->getStartLoc(), expr->getEndLoc(),
                                    /*Implicit=*/true,
                                    TupleType::getEmpty(ctx));
    expr = new (ctx) CallExpr(memberRef, arg, /*Implicit=*/true);
    failed = tc.typeCheckExpressionShallow(expr, cs.DC);
    assert(!failed && "Could not call witness?");
    (void)failed;

    // At this point, we must have a type with the builtin member.
    type = expr->getType();
    witnesses = tc.lookupMember(type->getRValueType(), builtinName, cs.DC);
    if (!witnesses) {
      tc.diagnose(protocol->getLoc(), brokenProtocolDiag);
      return nullptr;
    }
  }

  // Find the builtin method.
  if (witnesses.size() != 1) {
    tc.diagnose(protocol->getLoc(), brokenBuiltinDiag);
    return nullptr;
  }
  FuncDecl *builtinMethod = dyn_cast<FuncDecl>(witnesses[0]);
  if (!builtinMethod) {
    tc.diagnose(protocol->getLoc(), brokenBuiltinDiag);
    return nullptr;

  }

  // Form a reference to the builtin method.
  Expr *memberRef = new (ctx) MemberRefExpr(expr, SourceLoc(),
                                            builtinMethod, expr->getLoc(),
                                            /*Implicit=*/true);
  bool failed = tc.typeCheckExpressionShallow(memberRef, cs.DC);
  assert(!failed && "Could not reference witness?");
  (void)failed;

  // Call the builtin method.
  Expr *arg = new (ctx) TupleExpr(expr->getStartLoc(), expr->getEndLoc(),
                                  /*Implicit=*/true, TupleType::getEmpty(ctx));
  expr = new (ctx) CallExpr(memberRef, arg, /*Implicit=*/true);
  failed = tc.typeCheckExpressionShallow(expr, cs.DC);
  assert(!failed && "Could not call witness?");
  (void)failed;
  return expr;
}

Expr *
Solution::convertToLogicValue(Expr *expr, ConstraintLocator *locator) const {
  auto &tc = getConstraintSystem().getTypeChecker();

  // Special case: already a builtin logic value.
  if (expr->getType()->getRValueType()->isBuiltinIntegerType(1)) {
    return tc.coerceToRValue(expr);
  }

  // FIXME: Cache names.
  auto result = convertViaBuiltinProtocol(
                  *this, expr, locator,
                  tc.getProtocol(expr->getLoc(),
                                 KnownProtocolKind::LogicValue),
                  tc.Context.Id_GetLogicValue,
                  tc.Context.Id_GetBuiltinLogicValue,
                  diag::condition_broken_proto,
                  diag::broken_bool);
  if (result && !result->getType()->isBuiltinIntegerType(1)) {
    tc.diagnose(expr->getLoc(), diag::broken_bool);
    return nullptr;
  }

  return result;
}

Expr *
Solution::convertToArrayBound(Expr *expr, ConstraintLocator *locator) const {
  // FIXME: Cache names.
  auto &tc = getConstraintSystem().getTypeChecker();
  auto result = convertViaBuiltinProtocol(
                  *this, expr, locator,
                  tc.getProtocol(expr->getLoc(),
                                 KnownProtocolKind::ArrayBound),
                  tc.Context.Id_GetArrayBoundValue,
                  tc.Context.Id_GetBuiltinArrayBoundValue,
                  diag::broken_array_bound_proto,
                  diag::broken_builtin_array_bound);
  if (result && !result->getType()->is<BuiltinIntegerType>()) {
    tc.diagnose(expr->getLoc(), diag::broken_builtin_array_bound);
    return nullptr;
  }
  
  return result;
}
