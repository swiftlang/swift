//===--- TypeCheckConstraintsApply.cpp - Constraint Application -----------===//
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

Type Solution::computeSubstitutions(
                 Type origType,
                 DeclContext *dc,
                 Type openedType,
                 SmallVectorImpl<Substitution> &substitutions) const {
  auto &tc = getConstraintSystem().getTypeChecker();
  auto &ctx = tc.Context;

  // Gather the substitutions from archetypes to concrete types, found
  // by identifying all of the type variables in the original type
  // FIXME: It's unfortunate that we're using archetypes here, but we don't
  // have another way to map from type variables back to dependent types (yet);
  TypeSubstitutionMap typeSubstitutions;
  auto type = tc.transformType(openedType,
                               [&](Type type) -> Type {
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
    switch (req.getKind()) {
    case RequirementKind::Conformance:
      // If this is a protocol conformance requirement, get the conformance
      // and record it.
      if (auto protoType = req.getSecondType()->getAs<ProtocolType>()) {
        assert(ArchetypeBuilder::mapTypeIntoContext(dc, req.getFirstType())
                 ->castTo<ArchetypeType>()
               == substitutions.back().Archetype && "Archetype out-of-sync");
        ProtocolConformance *conformance = nullptr;
        Type replacement = substitutions.back().Replacement;
        bool conforms = tc.conformsToProtocol(replacement,
                                              protoType->getDecl(),
                                              getConstraintSystem().DC,
                                              &conformance);
        assert((conforms ||
                replacement->isExistentialType()) &&
               "Constraint system missed a conformance?");
        (void)conforms;

        assert(conformance ||
               replacement->isExistentialType() ||
               replacement->is<ArchetypeType>());
        currentConformances.push_back(conformance);
        break;
      }
      break;

    case RequirementKind::SameType:
      // Same-type requirements aren't recorded in substitutions.
      break;

    case RequirementKind::ValueWitnessMarker:
      // Flush the current conformances.
      if (!substitutions.empty()) {
        substitutions.back().Conformance
          = ctx.AllocateCopy(currentConformances);
        currentConformances.clear();
      }

      // Each value witness marker starts a new substitution.
      substitutions.push_back(Substitution());
      substitutions.back().Archetype
        = ArchetypeBuilder::mapTypeIntoContext(dc, req.getFirstType())
            ->castTo<ArchetypeType>();
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
    if (!fd || fd->getName().empty())
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
  (void)conforms;
  assert(conforms && "Protocol conformance broken?");

  // For an archetype, just return the requirement from the protocol. There
  // are no protocol conformance tables.
  if (type->is<ArchetypeType>()) {
    return requirement;
  }

  assert(conformance && "Missing conformance information");
  // FIXME: Dropping substitutions here.
  return cast<FuncDecl>(conformance->getWitness(requirement).getDecl());
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

  public:
    /// \brief Build a reference to the given declaration.
    Expr *buildDeclRef(ValueDecl *decl, SourceLoc loc, Type openedType,
                       ConstraintLocatorBuilder locator,
                       bool specialized, bool implicit) {
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
                                             MetaTypeType::get(baseTy, ctx));

        return buildMemberRef(base, openedType, SourceLoc(), decl,
                              loc, openedFnType->getResult(),
                              locator, implicit);
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
                                     loc, implicit, type);
      }

      auto type = simplifyType(openedType);
      return new (ctx) DeclRefExpr(decl, loc, implicit, type);
    }

    /// \brief Build a new member reference with the given base and member.
    Expr *buildMemberRef(Expr *base, Type openedFullType, SourceLoc dotLoc,
                         ValueDecl *member, SourceLoc memberLoc,
                         Type openedType, ConstraintLocatorBuilder locator,
                         bool Implicit) {
      auto &tc = cs.getTypeChecker();
      auto &context = tc.Context;

      // Figure out the actual base type, and whether we have an instance of
      // that type or its metatype.
      Type baseTy = base->getType()->getRValueType();
      bool baseIsInstance = true;
      if (auto baseMeta = baseTy->getAs<MetaTypeType>()) {
        baseIsInstance = false;
        baseTy = baseMeta->getInstanceType();
      }

      // Produce a reference to the member, the type of the container it
      // resides in, and the type produced by the reference itself.
      Type containerTy;
      ConcreteDeclRef memberRef;
      Type refTy;
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

        auto openedFullFnType = openedFullType->castTo<FunctionType>();
        auto openedBaseType = openedFullFnType->getInput()
                                ->getRValueInstanceType();
        containerTy = solution.simplifyType(tc, openedBaseType);
      } else {
        // No substitutions required; the declaration reference is simple.
        containerTy = member->getDeclContext()->getDeclaredTypeOfContext();
        memberRef = member;
        refTy = tc.getUnopenedTypeOfReference(member);
      }

      // If we're referring to the member of a module, it's just a simple
      // reference.
      if (baseTy->is<ModuleType>()) {
        Expr *ref = new (context) DeclRefExpr(memberRef, memberLoc, Implicit);
        ref->setType(refTy);
        return new (context) DotSyntaxBaseIgnoredExpr(base, dotLoc, ref);
      }

      // Otherwise, we're referring to a member of a type.

      // Is it an archetype or existential member?
      bool isArchetypeOrExistentialRef
            = isa<ProtocolDecl>(member->getDeclContext()) &&
              (baseTy->is<ArchetypeType>() || baseTy->isExistentialType());

      // If we are referring to an optional member of a protocol.
      if (isArchetypeOrExistentialRef && member->getAttrs().isOptional()) {
        auto proto =tc.getProtocol(memberLoc, KnownProtocolKind::DynamicLookup);
        if (!proto)
          return nullptr;

        baseTy = proto->getDeclaredType();
      }

      if (baseIsInstance) {
        // Convert the base to the appropriate container type, turning it
        // into an lvalue if required.
        base = coerceObjectArgumentToType(
                 base, isArchetypeOrExistentialRef ? baseTy : containerTy,
                 locator.withPathElement(ConstraintLocator::MemberRefBase));
      } else {
        // Convert the base to an rvalue of the appropriate metatype.
        base = coerceToType(base,
                            MetaTypeType::get(isArchetypeOrExistentialRef
                                                ? baseTy
                                                : containerTy,
                                              context),
                            locator.withPathElement(
                              ConstraintLocator::MemberRefBase));
        base = tc.coerceToRValue(base);
      }
      assert(base && "Unable to convert base?");

      // Handle archetype and existential references.
      if (isArchetypeOrExistentialRef) {
        Expr *ref;

        if (member->getAttrs().isOptional()) {
          base = tc.coerceToRValue(base);
          if (!base) return nullptr;
          ref = new (context) DynamicMemberRefExpr(base, dotLoc, memberRef,
                                                   memberLoc);
        } else if (baseTy->is<ArchetypeType>()) {
          ref = new (context) ArchetypeMemberRefExpr(base, dotLoc, memberRef,
                                                     memberLoc);
        } else {
          ref = new (context) ExistentialMemberRefExpr(base, dotLoc, memberRef,
                                                       memberLoc);
        }
        ref->setImplicit(Implicit);
        ref->setType(simplifyType(openedType));
        return ref;
      }

      // For types and properties, build member references.
      if (isa<TypeDecl>(member) || isa<VarDecl>(member)) {
        auto result
          = new (context) MemberRefExpr(base, dotLoc, memberRef,
                                        memberLoc, Implicit);

        // Skip the synthesized 'self' input type of the opened type.
        result->setType(simplifyType(openedType));
        return result;
      }
      
      // Handle all other references.
      Expr *ref = new (context) DeclRefExpr(memberRef, memberLoc, Implicit);
      ref->setType(refTy);

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
    /// \param toType The type to coerce to. This function ignores whether
    /// the 'to' type is an lvalue type or not, and produces an expression
    /// with the correct type for use as an lvalue.
    ///
    /// \param locator Locator used to describe where in this expression we are.
    Expr *coerceObjectArgumentToType(Expr *expr, Type toType,
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

      // Determine the result type of the subscript expression.
      resultTy = resultTy->getRValueType();

      // Form the subscript expression.

      // Handle dynamic lookup.
      if (selected.choice.getKind() == OverloadChoiceKind::DeclViaDynamic ||
          subscript->getAttrs().isOptional()) {
        // If we've found an optional method in a protocol, the base type is
        // DynamicLookup.
        if (selected.choice.getKind() != OverloadChoiceKind::DeclViaDynamic) {
          auto proto = tc.getProtocol(index->getStartLoc(),
                                      KnownProtocolKind::DynamicLookup);
          if (!proto)
            return nullptr;

          baseTy = proto->getDeclaredType();
        }

        // Materialize if we need to.
        base = coerceObjectArgumentToType(base, baseTy, locator);
        if (!base)
          return nullptr;

        auto subscriptExpr = new (tc.Context) DynamicSubscriptExpr(base,
                                                                   index,
                                                                   subscript);
        subscriptExpr->setType(resultTy);
        return subscriptExpr;
      }

      // Handle subscripting of archetypes.
      if (baseTy->is<ArchetypeType>() && containerTy->is<ProtocolType>()) {
        // Coerce as an object argument.
        base = coerceObjectArgumentToType(base, baseTy, locator);
        if (!base)
          return nullptr;

        // Create the archetype subscript operation.
        auto subscriptExpr = new (tc.Context) ArchetypeSubscriptExpr(base,
                                                                     index,
                                                                     subscript);
        subscriptExpr->setType(resultTy);
        return subscriptExpr;
      }

      // The remaining subscript kinds
      resultTy = LValueType::get(resultTy,
                                 LValueType::Qual::DefaultForMemberAccess,
                                 tc.Context);

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
        auto openedBaseType = openedFullFnType->getInput()
                                ->getRValueInstanceType();
        containerTy = solution.simplifyType(tc, openedBaseType);
        base = coerceObjectArgumentToType(base, containerTy, locator);
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
        return subscriptExpr;
      }

      // Handle subscripting of existential types.
      if (baseTy->isExistentialType()) {
        // Materialize if we need to.
        base = coerceObjectArgumentToType(base, baseTy, locator);
        if (!base)
          return nullptr;

        auto subscriptExpr
          = new (tc.Context) ExistentialSubscriptExpr(base, index, subscript);
        subscriptExpr->setType(resultTy);
        return subscriptExpr;
      }

      // Coerce the base to the container type.
      base = coerceObjectArgumentToType(base, containerTy, locator);
      if (!base)
        return nullptr;

      // Form a normal subscript.
      SubscriptExpr *subscriptExpr
        = new (tc.Context) SubscriptExpr(base, index, subscript);
      subscriptExpr->setType(resultTy);
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
          selfTy = LValueType::get(selfTy,
                                   LValueType::Qual::DefaultForMemberAccess,
                                   ctx);

        resultTy = FunctionType::get(selfTy, resultFnTy->getResult(),
                                     resultFnTy->getExtInfo(), ctx);
      } else {
        ref = ConcreteDeclRef(ctor);
        resultTy = ctor->getInitializerType();
      }

      // Build the constructor reference.
      Expr *refExpr = new (ctx) OtherConstructorDeclRefExpr(ref, loc,
                                                            resultTy);
      if (implicit)
        refExpr->setImplicit();
      return refExpr;
    }

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
      // FIXME: Cache name lookup.
      auto maxTypeName = tc.Context.getIdentifier("MaxBuiltinIntegerType");
      UnqualifiedLookup lookup(maxTypeName, tc.getStdlibModule(dc), &tc);
      auto maxTypeDecl
        = dyn_cast_or_null<TypeAliasDecl>(lookup.getSingleTypeResult());
      if (!maxTypeDecl ||
          !maxTypeDecl->getUnderlyingType()->is<BuiltinIntegerType>()) {
        tc.diagnose(expr->getLoc(), diag::no_MaxBuiltinIntegerType_found);
        return nullptr;
      }
      auto maxType = maxTypeDecl->getUnderlyingType();

      return convertLiteral(
               expr,
               type,
               expr->getType(),
               protocol,
               tc.Context.getIdentifier("IntegerLiteralType"),
               tc.Context.getIdentifier("convertFromIntegerLiteral"),
               builtinProtocol,
               maxType,
               tc.Context.getIdentifier("_convertFromBuiltinIntegerLiteral"),
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
      auto maxTypeName = tc.Context.getIdentifier("MaxBuiltinFloatType");
      UnqualifiedLookup lookup(maxTypeName, tc.getStdlibModule(dc), &tc);
      auto maxTypeDecl
      = dyn_cast_or_null<TypeAliasDecl>(lookup.getSingleTypeResult());
      if (!maxTypeDecl ||
          !maxTypeDecl->getUnderlyingType()->is<BuiltinFloatType>()) {
        tc.diagnose(expr->getLoc(), diag::no_MaxBuiltinFloatType_found);
        return nullptr;
      }
      auto maxType = maxTypeDecl->getUnderlyingType();

      return convertLiteral(
               expr,
               type,
               expr->getType(),
               protocol,
               tc.Context.getIdentifier("FloatLiteralType"),
               tc.Context.getIdentifier("convertFromFloatLiteral"),
               builtinProtocol,
               maxType,
               tc.Context.getIdentifier("_convertFromBuiltinFloatLiteral"),
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
               tc.Context.getIdentifier("CharacterLiteralType"),
               tc.Context.getIdentifier("convertFromCharacterLiteral"),
               builtinProtocol,
               Type(BuiltinIntegerType::get(21, tc.Context)),
               tc.Context.getIdentifier("_convertFromBuiltinCharacterLiteral"),
               [] (Type type) -> bool {
                 if (auto builtinInt = type->getAs<BuiltinIntegerType>()) {
                   return builtinInt->isFixedWidth(21);
                 }
                 return false;
               },
               diag::character_literal_broken_proto,
               diag::builtin_character_literal_broken_proto);
    }

    Expr *handleStringLiteralExpr(LiteralExpr *expr) {
      auto &tc = cs.getTypeChecker();
      ProtocolDecl *protocol
        = tc.getProtocol(expr->getLoc(),
                         KnownProtocolKind::StringLiteralConvertible);
      ProtocolDecl *builtinProtocol
        = tc.getProtocol(expr->getLoc(),
                         KnownProtocolKind::BuiltinStringLiteralConvertible);
      
      // For type-sugar reasons, prefer the spelling of the default literal
      // type.
      auto type = simplifyType(expr->getType());
      if (auto defaultType = tc.getDefaultType(protocol, dc)) {
        if (defaultType->isEqual(type))
          type = defaultType;
      }
      
      // Note: We always use 64-bits here, even on 32-bit platforms to keep the
      // AST consistent across different architectures.  LLVM optimizers will
      // handily slice off the top bits for 32-bit targets.
      TupleTypeElt elements[3] = {
        TupleTypeElt(tc.Context.TheRawPointerType),
        TupleTypeElt(BuiltinIntegerType::get(64, tc.Context)),
        TupleTypeElt(BuiltinIntegerType::get(1, tc.Context))
      };
      
      auto CFSLID = tc.Context.getIdentifier("convertFromStringLiteral");
      auto CFBSLID=tc.Context.getIdentifier("_convertFromBuiltinStringLiteral");
      
      return convertLiteral(expr,
                            type,
                            expr->getType(),
                            protocol,
                            tc.Context.getIdentifier("StringLiteralType"),
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
      auto name = tc.Context.getIdentifier("convertFromStringInterpolation");
      auto member = findNamedWitness(tc, dc, type, interpolationProto, name,
                                     diag::interpolation_broken_proto);
      if (!member)
        return nullptr;

      // Build a reference to the convertFromStringInterpolation member.
      auto typeRef = new (tc.Context) MetatypeExpr(
                                        nullptr, expr->getStartLoc(),
                                        MetaTypeType::get(type, tc.Context));
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
      ConstraintLocatorBuilder locatorBuilder(cs.getConstraintLocator(expr,
                                                                      { }));
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
        return handleStringLiteralExpr(expr);

      case MagicIdentifierLiteralExpr::Line:
      case MagicIdentifierLiteralExpr::Column:
        return handleIntegerLiteralExpr(expr);
      }
    }

    /// \brief Retrieve the type of a reference to the given declaration.
    Type getTypeOfDeclReference(ValueDecl *decl, bool isSpecialized) {
      if (auto typeDecl = dyn_cast<TypeDecl>(decl)) {
        // Resolve the reference to this type declaration in our current context.
        auto type = cs.getTypeChecker().resolveTypeInContext(typeDecl, dc,
                                                             isSpecialized);
        if (!type)
          return nullptr;

        // Refer to the metatype of this type.
        return MetaTypeType::get(type, cs.getASTContext());
      }

      Type type = cs.TC.getUnopenedTypeOfReference(decl, Type(),
                                                   /*wantInterfaceType=*/true);
      return adjustLValueForReference(type, decl->getAttrs().isAssignment(),
                                      cs.TC.Context);
    }

    Expr *visitDeclRefExpr(DeclRefExpr *expr) {
      auto locator = cs.getConstraintLocator(expr, { });

      // If this is an anonymous closure argument, there's no overload
      // associated with it.
      if (auto var = dyn_cast<VarDecl>(expr->getDecl())) {
        if (var->isAnonClosureParam()) {
          return buildDeclRef(expr->getDecl(), expr->getLoc(), expr->getType(),
                              locator, expr->isSpecialized(),
                              expr->isImplicit());
        }
      }

      // Find the overload choice used for this declaration reference.
      auto selected = getOverloadChoice(locator);
      auto choice = selected.choice;
      auto decl = choice.getDecl();

      // FIXME: Cannibalize the existing DeclRefExpr rather than allocating a
      // new one?
      return buildDeclRef(decl, expr->getLoc(), selected.openedFullType,
                          locator, expr->isSpecialized(), expr->isImplicit());
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

      // Build a call to the initializer for the constructor.
      Expr *ctorRef = buildOtherConstructorRef(selected.openedFullType,
                                               ctor, expr->getConstructorLoc(),
                                               expr->isImplicit());
      auto *call
        = new (cs.getASTContext()) DotSyntaxCallExpr(ctorRef,
                                                     expr->getDotLoc(),
                                                     expr->getSubExpr());
      return finishApply(call, expr->getType(),
                         ConstraintLocatorBuilder(
                           cs.getConstraintLocator(expr, { })));
    }

    Expr *visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *expr) {
      return simplifyExprType(expr);
    }

    Expr *visitOverloadedDeclRefExpr(OverloadedDeclRefExpr *expr) {
      // Determine the declaration selected for this overloaded reference.
      auto locator = cs.getConstraintLocator(expr, { });
      auto selected = getOverloadChoice(locator);
      auto choice = selected.choice;
      auto decl = choice.getDecl();

      return buildDeclRef(decl, expr->getLoc(), selected.openedFullType,
                          locator, expr->isSpecialized(), expr->isImplicit());
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
                            cs.getConstraintLocator(expr, { }),
                            expr->isImplicit());
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
                            cs.getConstraintLocator(expr, { }),
                            expr->isImplicit());
    }

    Expr *visitExistentialMemberRefExpr(ExistentialMemberRefExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Expr *visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *expr) {
      auto selected = getOverloadChoice(
                        cs.getConstraintLocator(expr,
                                                ConstraintLocator::Member));
      return buildMemberRef(expr->getBase(),
                            selected.openedFullType,
                            expr->getDotLoc(),
                            selected.choice.getDecl(), expr->getNameLoc(),
                            selected.openedType,
                            cs.getConstraintLocator(expr, { }),
                            expr->isImplicit());
    }

    Expr *visitDynamicMemberRefExpr(DynamicMemberRefExpr *expr) {
      auto selected = getOverloadChoice(
                        cs.getConstraintLocator(expr,
                                                ConstraintLocator::Member));

      return buildDynamicMemberRef(expr->getBase(), expr->getDotLoc(),
                                   selected.choice.getDecl(),
                                   expr->getNameLoc(),
                                   selected.openedType,
                                   cs.getConstraintLocator(expr, { }));
    }

    Expr *visitUnresolvedMemberExpr(UnresolvedMemberExpr *expr) {
      // Dig out the type of the base, which will be the result
      // type of this expression.
      Type baseTy = simplifyType(expr->getType());
      auto &tc = cs.getTypeChecker();
      auto baseMetaTy = MetaTypeType::get(baseTy, tc.Context);

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
                                   cs.getConstraintLocator(expr, { }),
                                   expr->isImplicit());
      if (!result)
        return nullptr;

      // If there was an argument, apply it.
      if (auto arg = expr->getArgument()) {
        ApplyExpr *apply = new (tc.Context) CallExpr(result, arg, 
                                                     /*Implicit=*/false);
        result = finishApply(apply, Type(), cs.getConstraintLocator(expr, { }));
      }

      return result;
    }
    
  private:
    struct ValueTypeMemberApplication {
      unsigned level : 30;
      // Selector for the partial_application_of_value_type_method diagnostic
      // message.
      enum : unsigned {
        Struct,
        Enum,
        Archetype,
        Protocol,
      };
      unsigned kind : 2;
    };
    
    // A map used to track partial applications of value type methods to
    // require that they be fully applied. Partial applications of value types
    // would capture 'self' as an [inout] and hide any mutation of 'self',
    // which is surprising.
    llvm::DenseMap<Expr*, ValueTypeMemberApplication>
      ValueTypeMemberApplications;
    
  public:
    Expr *visitUnresolvedDotExpr(UnresolvedDotExpr *expr) {
      // Determine the declaration selected for this overloaded reference.
      auto selected = getOverloadChoice(
                        cs.getConstraintLocator(
                          expr,
                          ConstraintLocator::MemberRefBase));

      switch (selected.choice.getKind()) {
      case OverloadChoiceKind::Decl: {
        auto member = buildMemberRef(expr->getBase(),
                                     selected.openedFullType,
                                     expr->getDotLoc(),
                                     selected.choice.getDecl(),
                                     expr->getNameLoc(),
                                     selected.openedType,
                                     cs.getConstraintLocator(expr, { }),
                                     expr->isImplicit());
        // If this is an application of a value type method, arrange for us to
        // check that it gets fully applied.
        FuncDecl *fn = nullptr;
        unsigned kind;
        if (auto apply = dyn_cast<ApplyExpr>(member)) {
          auto selfLVT = apply->getArg()->getType()->getAs<LValueType>();
          if (!selfLVT)
            goto not_value_type_member;
          auto fnDeclRef = dyn_cast<DeclRefExpr>(apply->getFn());
          if (!fnDeclRef)
            goto not_value_type_member;
          fn = dyn_cast<FuncDecl>(fnDeclRef->getDecl());
          if (selfLVT->getObjectType()->getStructOrBoundGenericStruct())
            kind = ValueTypeMemberApplication::Struct;
          else if (selfLVT->getObjectType()->getEnumOrBoundGenericEnum())
            kind = ValueTypeMemberApplication::Enum;
          else
            llvm_unreachable("unknown kind of value type?!");
        } else if (auto amRef = dyn_cast<ArchetypeMemberRefExpr>(member)) {
          auto selfLVT = amRef->getBase()->getType()->getAs<LValueType>();
          if (!selfLVT)
            goto not_value_type_member;
          fn = dyn_cast<FuncDecl>(amRef->getDecl());
          kind = ValueTypeMemberApplication::Archetype;
        } else if (auto pmRef = dyn_cast<ExistentialMemberRefExpr>(member)) {
          auto selfLVT = pmRef->getBase()->getType()->getAs<LValueType>();
          if (!selfLVT)
            goto not_value_type_member;
          fn = dyn_cast<FuncDecl>(pmRef->getDecl());
          kind = ValueTypeMemberApplication::Protocol;
        }
        if (!fn)
          goto not_value_type_member;
        if (fn->isInstanceMember())
          ValueTypeMemberApplications.insert({
            member,
            // We need to apply all of the non-self argument clauses.
            {fn->getNaturalArgumentCount() - 1, kind},
          });

      not_value_type_member:
        return member;
      }

      case OverloadChoiceKind::DeclViaDynamic:
        return buildDynamicMemberRef(expr->getBase(), expr->getDotLoc(),
                                     selected.choice.getDecl(),
                                     expr->getNameLoc(),
                                     selected.openedType,
                                     cs.getConstraintLocator(expr, { }));

      case OverloadChoiceKind::TupleIndex: {
        auto base = expr->getBase();
        // If the base expression is not an lvalue, make everything inside it
        // materializable.
        if (!base->getType()->is<LValueType>()) {
          base = cs.getTypeChecker().coerceToMaterializable(base);
          if (!base)
            return nullptr;
        }

        return new (cs.getASTContext()) TupleElementExpr(
                                          base,
                                          expr->getDotLoc(),
                                          selected.choice.getTupleIndex(),
                                          expr->getNameLoc(),
                                          simplifyType(expr->getType()));
      }

      case OverloadChoiceKind::BaseType: {
        // FIXME: Losing ".0" sugar here.
        return expr->getBase();
      }

      case OverloadChoiceKind::TypeDecl:
      case OverloadChoiceKind::IdentityFunction:
        llvm_unreachable("Nonsensical overload choice");
      }
    }

    Expr *visitSequenceExpr(SequenceExpr *expr) {
      llvm_unreachable("Expression wasn't parsed?");
    }

    Expr *visitParenExpr(ParenExpr *expr) {
      expr->setType(expr->getSubExpr()->getType());
      return expr;
    }

    Expr *visitTupleExpr(TupleExpr *expr) {
      return simplifyExprType(expr);
    }

    Expr *visitSubscriptExpr(SubscriptExpr *expr) {
      return buildSubscript(expr->getBase(), expr->getIndex(),
                            cs.getConstraintLocator(expr, { }));
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
                                         MetaTypeType::get(arrayTy,
                                                           tc.Context));
      auto name = tc.Context.getIdentifier("convertFromArrayLiteral");
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
      (void)conforms;
      assert(conforms && "Type does not conform to protocol?");

      // Call the witness that builds the dictionary literal.
      // FIXME: callWitness() may end up re-doing some work we already did
      // to convert the dictionary literal elements to the (key, value) tuple.
      // It would be nicer to re-use them.
      // FIXME: Cache the name.
      Expr *typeRef = new (tc.Context) MetatypeExpr(
                                         nullptr,
                                         expr->getLoc(),
                                         MetaTypeType::get(dictionaryTy,
                                                           tc.Context));
      auto name = tc.Context.getIdentifier("convertFromDictionaryLiteral");
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

    Expr *visitExistentialSubscriptExpr(ExistentialSubscriptExpr *expr) {
      return buildSubscript(expr->getBase(), expr->getIndex(),
                            cs.getConstraintLocator(expr, { }));
    }

    Expr *visitArchetypeSubscriptExpr(ArchetypeSubscriptExpr *expr) {
      return buildSubscript(expr->getBase(), expr->getIndex(),
                            cs.getConstraintLocator(expr, { }));
    }

    Expr *visitDynamicSubscriptExpr(DynamicSubscriptExpr *expr) {
      return buildSubscript(expr->getBase(), expr->getIndex(),
                            cs.getConstraintLocator(expr, { }));
    }

    Expr *visitTupleElementExpr(TupleElementExpr *expr) {
      simplifyExprType(expr);
      return expr;
    }

    void simplifyPatternTypes(Pattern *pattern) {
      switch (pattern->getKind()) {
      case PatternKind::Paren:
        // Parentheses don't affect the type.
        return simplifyPatternTypes(
                 cast<ParenPattern>(pattern)->getSubPattern());

      case PatternKind::Any:
      case PatternKind::Typed:
        return;

      case PatternKind::Named: {
        // Simplify the type of any variables.
        auto var = cast<NamedPattern>(pattern)->getDecl();
        var->overwriteType(simplifyType(var->getType()));
        return;
      }

      case PatternKind::Tuple: {
        auto tuplePat = cast<TuplePattern>(pattern);
        for (auto tupleElt : tuplePat->getFields()) {
          simplifyPatternTypes(tupleElt.getPattern());
        }
        return;
      }

      //TODO
#define PATTERN(Id, Parent)
#define REFUTABLE_PATTERN(Id, Parent) case PatternKind::Id:
#include "swift/AST/PatternNodes.def"
        llvm_unreachable("not implemented");
      }

      llvm_unreachable("Unhandled pattern kind");
    }

    Expr *visitClosureExpr(ClosureExpr *expr) {
      llvm_unreachable("Handled by the walker directly");
    }

    Expr *visitAutoClosureExpr(AutoClosureExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Expr *visitModuleExpr(ModuleExpr *expr) { return expr; }

    Expr *visitAddressOfExpr(AddressOfExpr *expr) {
      // Compute the type of the address-of expression.
      // FIXME: Do we really need to compute this, or is this just a hack
      // due to the presence of the 'nonheap' bit?
      auto lv = expr->getSubExpr()->getType()->getAs<LValueType>();
      assert(lv && "Subexpression is not an lvalue?");
      assert(lv->isSettable() &&
             "Solved an address-of constraint with a non-settable lvalue?!");

      auto destQuals = lv->getQualifiers() - LValueType::Qual::Implicit;
      expr->setType(LValueType::get(lv->getObjectType(), destQuals,
                                    cs.getASTContext()));
      return expr;
    }

    Expr *visitNewArrayExpr(NewArrayExpr *expr) {
      auto &tc = cs.getTypeChecker();

      // Dig out the element type of the new array expression.
      auto resultType = simplifyType(expr->getType());
      auto elementType = resultType->castTo<BoundGenericType>()
        ->getGenericArgs()[0];
      expr->setElementType(elementType);

      // Make sure that the result type is a slice type, even if
      // canonicalization mapped it down to Slice<T>.
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
        auto intProto = tc.getProtocol(expr->getConstructionFunction()->getLoc(),
                                 KnownProtocolKind::IntegerLiteralConvertible);
        Type intTy = tc.getDefaultType(intProto, dc);
        
        Expr *constructionFn = expr->getConstructionFunction();
        Type constructionTy = FunctionType::get(intTy,
                                                elementType,
                                                tc.Context);
        if (tc.typeCheckExpression(constructionFn, dc,
                                   constructionTy,
                                   /*discarded*/false))
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
        
        Expr *ctor = tc.buildRefExpr(selected.choice.getDecl(),
                               SourceLoc(),
                               /*implicit*/ true);
        Expr *metaty = new (tc.Context) MetatypeExpr(nullptr, SourceLoc(),
                               MetaTypeType::get(baseElementType, tc.Context));
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
        expr->setType(MetaTypeType::get(base->getType(), tc.Context));
      }

      return expr;
    }

    Expr *visitOpaqueValueExpr(OpaqueValueExpr *expr) {
      return expr;
    }

    Expr *visitZeroValueExpr(ZeroValueExpr *expr) {
      // Do nothing with zero-value initialization expressions.
      return simplifyExprType(expr);
    }

    Expr *visitDefaultValueExpr(DefaultValueExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Expr *visitApplyExpr(ApplyExpr *expr) {
      
      auto result = finishApply(expr, expr->getType(),
                         ConstraintLocatorBuilder(
                           cs.getConstraintLocator(expr, { })));

      // See if this application advanced a partial value type application.
      auto foundApplication = ValueTypeMemberApplications.find(
                                   expr->getFn()->getSemanticsProvidingExpr());
      if (foundApplication != ValueTypeMemberApplications.end()) {
        unsigned level = foundApplication->second.level;
        assert(level > 0);
        ValueTypeMemberApplications.erase(foundApplication);
        if (level > 1)
          ValueTypeMemberApplications.insert({
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
                                       cs.getConstraintLocator(expr, { }));
      if (!cond)
        return nullptr;
      expr->setCondExpr(cond);

      // Coerce the then/else branches to the common type.
      expr->setThenExpr(coerceToType(expr->getThenExpr(), resultTy,
                                     ConstraintLocatorBuilder(
                                       cs.getConstraintLocator(expr, { }))
                                     .withPathElement(
                                       ConstraintLocator::IfThen)));
      expr->setElseExpr(coerceToType(expr->getElseExpr(), resultTy,
                                     ConstraintLocatorBuilder(
                                       cs.getConstraintLocator(expr, { }))
                                     .withPathElement(
                                       ConstraintLocator::IfElse)));

      return expr;
    }
    
    Expr *visitImplicitConversionExpr(ImplicitConversionExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    /// Type-check a checked cast expression.
    CheckedCastKind checkCheckedCastExpr(CheckedCastExpr *expr) {
      auto &tc = cs.getTypeChecker();

      // Simplify the type we're converting to.
      Type toType = expr->getCastTypeLoc().getType();

      // Type-check the subexpression in isolation.
      Expr *sub = expr->getSubExpr();
      if (tc.typeCheckExpression(sub, dc, Type(), /*discardedExpr=*/false)) {
        return CheckedCastKind::Unresolved;
      }
      sub = tc.coerceToRValue(sub);
      if (!sub) {
        return CheckedCastKind::Unresolved;
      }
      expr->setSubExpr(sub);

      Type fromType = sub->getType();
      
      return tc.typeCheckCheckedCast(fromType, toType, dc,
                              expr->getLoc(),
                              sub->getSourceRange(),
                              expr->getCastTypeLoc().getSourceRange(),
                              [&](Type commonTy) -> bool {
                                return tc.convertToType(sub, commonTy, dc);
                              });
    }
    
    Expr *visitIsaExpr(IsaExpr *expr) {
      // SIL-generation magically turns this into a Bool; make sure it can.
      if (!cs.getASTContext().getGetBoolDecl(&cs.getTypeChecker())) {
        cs.getTypeChecker().diagnose(expr->getLoc(),
                                     diag::bool_intrinsics_not_found);
        // Continue anyway.
      }

      CheckedCastKind castKind = checkCheckedCastExpr(expr);
      switch (castKind) {
      // Invalid type check.
      case CheckedCastKind::Unresolved:
        return nullptr;
      // Check is trivially true.
      case CheckedCastKind::InvalidCoercible:
        cs.getTypeChecker().diagnose(expr->getLoc(), diag::isa_is_always_true,
                                     expr->getSubExpr()->getType(),
                                     expr->getCastTypeLoc().getType());
        break;
          
      // Valid checks.
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
      return expr;
    }
    
    Expr *checkAsCastExpr(CheckedCastExpr *expr) {
      Type toType = expr->getCastTypeLoc().getType();
      
      CheckedCastKind castKind = checkCheckedCastExpr(expr);
      switch (castKind) {
      /// Invalid cast.
      case CheckedCastKind::Unresolved:
        return nullptr;
      /// Cast trivially succeeds. Emit a fixit and reduce to a coercion.
      case CheckedCastKind::InvalidCoercible: {
        // Only complain if the cast was explicitly generated.
        // FIXME: This leniency is here for the Clang module importer,
        // which doesn't necessarily know whether it needs to force the
        // cast or not. instancetype should eliminate the need for it.
        if (!expr->isImplicit()) {
          cs.getTypeChecker().diagnose(expr->getLoc(),
                                       diag::downcast_to_supertype,
                                       expr->getSubExpr()->getType(),
                                       expr->getCastTypeLoc().getType())
            .highlight(expr->getSubExpr()->getSourceRange())
            .highlight(expr->getCastTypeLoc().getSourceRange())
            .fixItRemove(SourceRange(expr->getLoc(), expr->getEndLoc()));
        }
        
        // If the types are equivalent, we don't need the 'as' at all.
        if (expr->getType()->isEqual(expr->getSubExpr()->getType()))
          return expr->getSubExpr();
        
        // Just perform the coercion directly, wrapping in an optional to
        // preserve the expected type of 'as'.
        auto coerced = coerceToType(expr->getSubExpr(), toType,
                                    cs.getConstraintLocator(expr, { }));
        return new (cs.getASTContext())
          InjectIntoOptionalExpr(coerced,
                                 OptionalType::get(toType, cs.getASTContext()));
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
      return expr;
    }
    
    Expr *visitConditionalCheckedCastExpr(ConditionalCheckedCastExpr *expr) {
      expr->setType(cs.getTypeChecker().getOptionalType(expr->getLoc(),
                                            expr->getCastTypeLoc().getType()));
      Expr *result = checkAsCastExpr(expr);
      if (!result)
        return nullptr;
      return result;
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
                                   cs.getConstraintLocator(expr, { }));
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
                                   cs.getConstraintLocator(expr, { }));
      if (!subExpr) return nullptr;

      expr->setSubExpr(subExpr);
      expr->setType(optType);
      return expr;
    }

    /// Whether this type is DynamicLookup or an implicit lvalue thereof.
    bool isDynamicLookupType(Type type) {
      // Look through lvalues, metatypes.
      if (auto lvalue = type->getAs<LValueType>()) {
        if (!lvalue->getQualifiers().isImplicit())
          return false;

        type = lvalue->getObjectType();
      }

      // Check whether we have a protocol type.
      auto protoTy = type->getAs<ProtocolType>();
      if (!protoTy)
        return false;

      // Check whether this is DynamicLookup.
      return protoTy->getDecl()->isSpecificProtocol(
                                   KnownProtocolKind::DynamicLookup);
    }

    Expr *visitForceValueExpr(ForceValueExpr *expr) {
      Type valueType = simplifyType(expr->getType());
      auto &tc = cs.getTypeChecker();
      Type optType = OptionalType::get(valueType, cs.getASTContext());

      // If the subexpression is of DynamicLookup type, introduce a conditional
      // cast to the value type. This cast produces a value of optional type.
      Expr *subExpr = expr->getSubExpr();
      if (isDynamicLookupType(expr->getSubExpr()->getType())) {
        // Coerce the subexpression to an rvalue.
        subExpr = tc.coerceToRValue(subExpr);
        if (!subExpr) return nullptr;

        // Create a conditional checked cast to the value type, e.g., x as T.
        bool isArchetype = valueType->is<ArchetypeType>();
        auto cast = new (tc.Context) ConditionalCheckedCastExpr(
                                       subExpr,
                                       SourceLoc(),
                                       TypeLoc::withoutLoc(valueType));
        cast->setImplicit(true);
        cast->setType(optType);
        cast->setCastKind(isArchetype? CheckedCastKind::ExistentialToArchetype
                                     : CheckedCastKind::ExistentialToConcrete);
        subExpr = cast;
      } else {
        // Coerce the subexpression to the appropriate optional type.
        subExpr = coerceToType(subExpr, optType,
                               cs.getConstraintLocator(expr, { }));
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

    void finalize() {
      // Check that all value type methods were fully applied.
      for (auto &unapplied : ValueTypeMemberApplications) {
        cs.getTypeChecker().diagnose(unapplied.first->getLoc(),
                               diag::partial_application_of_value_type_method,
                               unapplied.second.kind);
      }
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
    SmallVector<LocatorPathElt, 4> newPath;
    newPath.append(locator->getPath().begin(), locator->getPath().end()-1);

    // If we have an interpolation argument, dig out the constructor if we
    // can.
    // FIXME: This representation is actually quite awful
    if (newPath.size() == 1 &&
        newPath[0].getKind() == ConstraintLocator::InterpolationArgument) {
      newPath.push_back(ConstraintLocator::ConstructorMember);

      locator = cs.getConstraintLocator(locator->getAnchor(), newPath);
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
    locator = cs.getConstraintLocator(locator->getAnchor(), newPath);
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
                                 SourceLoc loc, AbstractFunctionDecl *owner,
                                 unsigned index) {
  auto defArg = owner->getDefaultArg(index);
  MagicIdentifierLiteralExpr::KindTy magicKind;
  switch (defArg.first) {
    case DefaultArgumentKind::None:
      llvm_unreachable("No default argument here?");

    case DefaultArgumentKind::Normal:
      return nullptr;

    case DefaultArgumentKind::Column:
      magicKind = MagicIdentifierLiteralExpr::Column;
      break;

    case DefaultArgumentKind::File:
      magicKind = MagicIdentifierLiteralExpr::File;
      break;


    case DefaultArgumentKind::Line:
      magicKind = MagicIdentifierLiteralExpr::Line;
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
  while (auto paren = dyn_cast<ParenExpr>(innerExpr))
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
    Type boundType = BuiltinIntegerType::get(64, tc.Context);
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
    for (auto paren = dyn_cast<ParenExpr>(expr); paren;
         paren = dyn_cast<ParenExpr>(paren->getSubExpr()))
      paren->setType(fromTupleType);
  }

  // Compute the re-sugared tuple type.
  Type toSugarType = hasInits? toTuple
                             : TupleType::get(toSugarFields, tc.Context);

  // If we don't have to shuffle anything, we're done.
  if (!anythingShuffled && fromTupleExpr) {
    fromTupleExpr->setType(toSugarType);

    // Update the types of parentheses around the tuple expression.
    for (auto paren = dyn_cast<ParenExpr>(expr); paren;
         paren = dyn_cast<ParenExpr>(paren->getSubExpr()))
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
    Type boundType = BuiltinIntegerType::get(64, tc.Context);
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
  bool isExistential = toType->isExistentialType(protocols);
  assert(isExistential && "Not converting to existential?");
  (void)isExistential;
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
                                    /*Implicit=*/true);

    // Form an empty tuple.
    Expr *args = new (tc.Context) TupleExpr(expr->getStartLoc(), { },
                                            nullptr, expr->getEndLoc(),
                                            /*hasTrailingClosure=*/false,
                                            /*Implicit=*/true,
                                            TupleType::getEmpty(tc.Context));

    // Call the conversion function with an empty tuple.
    ApplyExpr *apply = new (tc.Context) CallExpr(memberRef, args,
                                                 /*Implicit=*/true);
    auto openedType = selected.openedType->castTo<FunctionType>()->getResult();
    expr = finishApply(apply, openedType,
                       ConstraintLocatorBuilder(
                         cs.getConstraintLocator(apply, { })));

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

  // If we chose the identity constructor, coerce to the expected type
  // based on the application argument locator.
  if (selected.choice.getKind() == OverloadChoiceKind::IdentityFunction) {
    return coerceToType(expr, toType,
                        locator.withPathElement(
                          ConstraintLocator::ApplyArgument));
  }

  // FIXME: Location information is suspect throughout.
  // Form a reference to the constructor.

  // Form a reference to the constructor or enum declaration.
  Expr *typeBase = new (tc.Context) MetatypeExpr(
                                      nullptr,
                                      expr->getStartLoc(),
                                      MetaTypeType::get(toType,tc.Context));
  Expr *declRef = buildMemberRef(typeBase,
                                 selected.openedFullType,
                                 expr->getStartLoc(),
                                 selected.choice.getDecl(),
                                 expr->getStartLoc(),
                                 selected.openedType,
                                 storedLocator,
                                 /*Implicit=*/true);

  // FIXME: Lack of openedType here is an issue.
  ApplyExpr *apply = new (tc.Context) CallExpr(declRef, expr,
                                               /*Implicit=*/true);
  expr = finishApply(apply, toType, locator);
  if (!expr)
    return nullptr;

  return coerceToType(expr, toType, locator);
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
  auto knownRestriction = solution.constraintRestrictions.find(
                            { fromType->getCanonicalType(),
                              toType->getCanonicalType() });
  if (knownRestriction != solution.constraintRestrictions.end()) {
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
      assert(toGenericType->getDecl() == tc.Context.getOptionalDecl());
      tc.requireOptionalIntrinsics(expr->getLoc());

      Type valueType = toGenericType->getGenericArgs()[0];
      expr = coerceToType(expr, valueType, locator);
      if (!expr) return nullptr;

      return new (tc.Context) InjectIntoOptionalExpr(expr, toType);
    }

    case ConversionRestrictionKind::OptionalToOptional: {
      auto fromGenericType = fromType->castTo<BoundGenericType>();
      auto toGenericType = toType->castTo<BoundGenericType>();
      assert(fromGenericType->getDecl() == tc.Context.getOptionalDecl());
      assert(toGenericType->getDecl() == tc.Context.getOptionalDecl());
      tc.requireOptionalIntrinsics(expr->getLoc());

      Type fromValueType = fromGenericType->getGenericArgs()[0];
      Type toValueType = toGenericType->getGenericArgs()[0];
      
      expr = new (tc.Context) BindOptionalExpr(expr, expr->getSourceRange().End,
                                               fromValueType);
      expr->setImplicit(true);
      expr = coerceToType(expr, toValueType, locator);
      if (!expr) return nullptr;
      
      expr = new (tc.Context) InjectIntoOptionalExpr(expr, toType);
      
      expr = new (tc.Context) OptionalEvaluationExpr(expr, toType);
      expr->setImplicit(true);
      return expr;
    }

    case ConversionRestrictionKind::User:
      return coerceViaUserConversion(expr, toType, locator);
    }
  }

  // Coercions from an lvalue: requalify and load. We perform these coercions
  // first because they are often the first step in a multi-step coercion.
  if (auto fromLValue = fromType->getAs<LValueType>()) {
    // Coercion of a SuperRefExpr. Refine the type of the 'super' reference
    // so we don't insert a DerivedToBase conversion later.
    if (auto superRef = dyn_cast<SuperRefExpr>(expr)) {
      assert(tc.isSubtypeOf(fromLValue->getObjectType(),
                            toType->getRValueType(), dc)
             && "coercing super expr to non-supertype?!");
      fromLValue = LValueType::get(toType->getRValueType(),
                                   fromLValue->getQualifiers(),
                                   tc.Context);
      superRef->setType(fromLValue);
    }

    if (auto toLValue = toType->getAs<LValueType>()) {
      // Update the qualifiers on the lvalue.
      expr = new (tc.Context) RequalifyExpr(
                                expr,
                                LValueType::get(fromLValue->getObjectType(),
                                                toLValue->getQualifiers(),
                                                tc.Context));

      // Coerce the result.
      return coerceToType(expr, toType, locator);
    }

    if (fromLValue->getQualifiers().isImplicit()) {
      // If we're actually turning this into an lvalue tuple element, don't
      // load.
      bool performLoad = true;
      if (auto toTuple = toType->getAs<TupleType>()) {
        int scalarIdx = toTuple->getFieldForScalarInit();
        if (scalarIdx >= 0 &&
            toTuple->getElementType(scalarIdx)->is<LValueType>())
          performLoad = false;
      }

      if (performLoad) {
        // Load from the lvalue.
        expr = new (tc.Context) LoadExpr(expr, fromLValue->getObjectType());

        // Coerce the result.
        return coerceToType(expr, toType, locator);
      }
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

  // Coercions to an lvalue: materialize the value.
  // FIXME: When we remember 'implicit' inout bits, sanity check that
  // toType is an implicit inout.
  if (auto toLValue = toType->getAs<LValueType>()) {
    // Convert the expression to the expected object type.
    expr = coerceToType(expr, toLValue->getObjectType(), locator);
    if (!expr)
      return nullptr;

    // Materialize.
    return new (tc.Context) MaterializeExpr(expr, toType);
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
          if (expr->getType()->isEqual(toType)) {
            return expr;
          }

          fromType = expr->getType();
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
    // FIXME: The type checker is more lenient, and allows [auto_closure]s to
    // be subtypes of non-[auto_closures], which is bogus.
    if (toFunc->isAutoClosure()) {
      // Convert the value to the expected result type of the function.
      expr = coerceToType(expr, toFunc->getResult(),
                          locator.withPathElement(ConstraintLocator::Load));

      auto Closure = new (tc.Context) AutoClosureExpr(expr, toType, dc);
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
    if (toFunc->isBlock() && (!fromFunc || !fromFunc->isBlock())) {
      // Coerce the expression to the non-block form of the function type.
      auto toNonBlockTy = FunctionType::get(toFunc->getInput(),
                                            toFunc->getResult(),
                                            tc.Context);
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
    if (toGenericType->getDecl() == tc.Context.getOptionalDecl()) {
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
  if (fromType->is<MetaTypeType>()) {
    if (auto toMeta = toType->getAs<MetaTypeType>()) {
      return new (tc.Context) MetatypeConversionExpr(expr, toMeta);
    }
  }

  llvm_unreachable("Unhandled coercion");
}

Expr *
ExprRewriter::coerceObjectArgumentToType(Expr *expr, Type toType,
                                         ConstraintLocatorBuilder locator) {
  // Map down to the underlying object type. We'll build an lvalue
  Type containerType = toType->getRValueType();

  // If the container type has reference semantics or is a metatype,
  // just perform the coercion to that type.
  if (containerType->hasReferenceSemantics() ||
      containerType->is<MetaTypeType>()) {
    return coerceToType(expr, containerType, locator);
  }

  // Types with value semantics are passed by reference.

  // Form the lvalue type we will be producing.
  auto &tc = cs.getTypeChecker();
  Type destType = LValueType::get(containerType,
                                  LValueType::Qual::DefaultForMemberAccess,
                                  tc.Context);

  // If our expression already has the right type, we're done.
  Type fromType = expr->getType();
  if (fromType->isEqual(destType))
    return expr;

  // If the source is an lvalue...
  if (auto fromLValue = fromType->getAs<LValueType>()) {
    // If the object types are the same, just requalify it.
    if (fromLValue->getObjectType()->isEqual(containerType))
      return new (tc.Context) RequalifyExpr(expr, destType,
                                            /*forObject*/ true);

    // If the object types are different, coerce to the container type.
    expr = coerceToType(expr, containerType, locator);

    // Fall through to materialize.
  }

  // If the source is not an lvalue, materialize it.
  return new (tc.Context) MaterializeExpr(expr, destType);
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
                                               MetaTypeType::get(type,
                                                                 tc.Context));
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
                                             MetaTypeType::get(type,
                                                               tc.Context));
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

  // The function is always an rvalue.
  auto fn = tc.coerceToRValue(apply->getFn());
  assert(fn && "Rvalue conversion failed?");
  if (!fn)
    return nullptr;
  apply->setFn(fn);

  // Check whether the argument is 'super'.
  bool isSuper = false;
  {
    Expr *arg = apply->getArg()->getSemanticsProvidingExpr();
    while (auto ice = dyn_cast<ImplicitConversionExpr>(arg))
      arg = ice->getSubExpr()->getSemanticsProvidingExpr();
    isSuper = isa<SuperRefExpr>(arg);
  }
  
  // For function application, convert the argument to the input type of
  // the function.
  if (auto fnType = fn->getType()->getAs<FunctionType>()) {
    auto origArg = apply->getArg();
    Expr *arg = nullptr;
    if (isa<SelfApplyExpr>(apply))
      arg = coerceObjectArgumentToType(origArg, fnType->getInput(), nullptr);
    else
      arg = coerceToType(origArg, fnType->getInput(),
                         locator.withPathElement(
                           ConstraintLocator::ApplyArgument));

    if (!arg) {
      // FIXME: Shouldn't ever happen.
      tc.diagnose(fn->getLoc(), diag::while_converting_function_argument,
                  fnType->getInput())
        .highlight(origArg->getSourceRange());

      return nullptr;
    }

    apply->setArg(arg);
    apply->setType(fnType->getResult());
    apply->setIsSuper(isSuper);

    assert(!apply->getType()->is<PolymorphicFunctionType>() &&
           "Polymorphic function type slipped through");
    return tc.substituteInputSugarTypeForResult(apply);
  }

  // We have a type constructor.
  auto metaTy = fn->getType()->castTo<MetaTypeType>();
  auto ty = metaTy->getInstanceType();

  // If we're "constructing" a tuple type, it's simply a conversion.
  if (auto tupleTy = ty->getAs<TupleType>()) {
    // FIXME: Need an AST to represent this properly.
    return coerceToType(apply->getArg(), tupleTy, locator);
  }

  // We're constructing a struct or enum. Look for the constructor or enum
  // element to use.
  // Note: we also allow class types here, for now, because T(x) is still
  // allowed to use coercion syntax.
  assert(ty->getNominalOrBoundGenericNominal());
  auto selected = getOverloadChoiceIfAvailable(
                    cs.getConstraintLocator(
                      locator.withPathElement(
                        ConstraintLocator::ConstructorMember)));

  // If there is no overload choice, or it was simply the identity function,
  // it's because this was a coercion rather than a construction. Just perform
  // the appropriate conversion.
  if (!selected ||
      selected->choice.getKind() == OverloadChoiceKind::IdentityFunction) {
    // FIXME: Need an AST to represent this properly.
    return coerceToType(apply->getArg(), ty, locator);
  }

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
                                 /*Implicit=*/true);
  declRef->setImplicit(apply->isImplicit());
  apply->setFn(declRef);

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
      // For an array, just walk the expression itself; its children have
      // already been type-checked.
      if (auto newArray = dyn_cast<NewArrayExpr>(expr)) {
        Rewriter.visitNewArrayExpr(newArray);
        return { false, expr };
      }

      // For checked cast expressions, the subexpression is checked
      // separately.
      if (auto unchecked = dyn_cast<CheckedCastExpr>(expr)) {
        return { false, Rewriter.visit(unchecked) };
      }

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
        if (tc.coerceToType(params, closure, fnType->getInput(),
                            /*allowOverride=*/true))
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
  if (auto metaType = type->getAs<MetaTypeType>())
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
  auto locator = cs.getConstraintLocator(base, { });

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
                                           /*Implicit=*/true);

  // Call the witness.
  ApplyExpr *apply = new (Context) CallExpr(memberRef, arg, /*Implicit=*/true);
  return rewriter.finishApply(apply, openedType,
                              cs.getConstraintLocator(arg, { }));
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
    assert(!failed && "Could not reference witness?");
    (void)failed;

    // Call the witness.
    Expr *arg = new (ctx) TupleExpr(expr->getStartLoc(),
                                    { }, nullptr,
                                    expr->getEndLoc(),
                                    /*hasTrailingClosure=*/false,
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
  Expr *arg = new (ctx) TupleExpr(expr->getStartLoc(),
                                  { }, nullptr,
                                  expr->getEndLoc(),
                                  /*hasTrailingClosure=*/false,
                                  /*Implicit=*/true,
                                  TupleType::getEmpty(ctx));
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
                  tc.Context.getIdentifier("getLogicValue"),
                  tc.Context.getIdentifier("_getBuiltinLogicValue"),
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
                  tc.Context.getIdentifier("getArrayBoundValue"),
                  tc.Context.getIdentifier("_getBuiltinArrayBoundValue"),
                  diag::broken_array_bound_proto,
                  diag::broken_builtin_array_bound);
  if (result && !result->getType()->is<BuiltinIntegerType>()) {
    tc.diagnose(expr->getLoc(), diag::broken_builtin_array_bound);
    return nullptr;
  }
  
  return result;
}

int Solution::getFixedScore() const {
  if (fixedScore)
    return *fixedScore;

  int score = 0;

  // Consider overload choices.
  for (auto overload : overloadChoices) {
    auto choice = overload.second.choice;
    if (choice.getKind() != OverloadChoiceKind::Decl)
      continue;

    // -3 penalty for each user-defined conversion.
    if (choice.getDecl()->getAttrs().isConversion())
      score -= 3;
  }

  // Consider type bindings.
  auto &tc = getConstraintSystem().getTypeChecker();
  for (auto binding : typeBindings) {
    // Look for type variables corresponding directly to an expression.
    auto typeVar = binding.first;
    auto locator = typeVar->getImpl().getLocator();
    if (!locator || !locator->getAnchor() || !locator->getPath().empty())
      continue;

    // Check whether there is a literal protocol corresponding to the
    // anchor expression.
    auto literalProtocol
      = tc.getLiteralProtocol(locator->getAnchor());
    if (!literalProtocol)
      continue;

    // Retrieve the default type for this literal protocol, if there is one.
    auto defaultType = tc.getDefaultType(literalProtocol,
                                         getConstraintSystem().DC);
    if (!defaultType)
      continue;

    // +1 if the bound type matches the default type for this literal protocol.
    // Literal types are always nominal, so we simply check the nominal
    // declaration. This covers e.g., Slice vs. Slice<T>.
    if (defaultType->getAnyNominal() == binding.second->getAnyNominal())
      ++score;
  }

  // Save the fixed score.
  fixedScore = score;
  return score;
}
