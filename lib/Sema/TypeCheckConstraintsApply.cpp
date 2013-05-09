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
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Attr.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/SaveAndRestore.h"

using namespace swift;
using namespace constraints;

//===--------------------------------------------------------------------===//
// Constraint application statistics
//===--------------------------------------------------------------------===//
#define DEBUG_TYPE "Constraint application"
STATISTIC(NumMissedCoercions, "# of coercions not handled directly");
STATISTIC(NumCoercions, "# of coercions");

/// \brief Convert the object argument to the given type.
static Expr *convertObjectArgumentToType(TypeChecker &tc, Expr *expr,
                                         Type toType) {
  // FIXME: Disable the constraint-based type checker here, because we are
  // falling back to the existing type checker.
  llvm::SaveAndRestore<bool> savedUseCS(tc.getLangOpts().UseConstraintSolver,
                                        false);
  return tc.coerceObjectArgument(expr, toType);
}

/// \brief Determine whether the given expression refers to an assignment
/// function.
static bool isAssignmentFn(Expr *expr) {
  expr = expr->getSemanticsProvidingExpr();
  if (auto spec = dyn_cast<SpecializeExpr>(expr))
    expr = spec->getSubExpr()->getSemanticsProvidingExpr();
  if (auto dre = dyn_cast<DeclRefExpr>(expr))
    return dre->getDecl()->getAttrs().isAssignment();
  if (auto dotCall = dyn_cast<DotSyntaxCallExpr>(expr))
    return isAssignmentFn(dotCall->getFn());
  if (auto emr = dyn_cast<ExistentialMemberRefExpr>(expr))
    return emr->getDecl()->getAttrs().isAssignment();
  if (auto amr = dyn_cast<ArchetypeMemberRefExpr>(expr))
    return amr->getDecl()->getAttrs().isAssignment();
  if (auto gmr = dyn_cast<GenericMemberRefExpr>(expr))
    return gmr->getDecl()->getAttrs().isAssignment();
  return false;
}

namespace {
  /// \brief Rewrites an expression by applying the solution of a constraint
  /// system to that expression.
  class ExprRewriter : public ExprVisitor<ExprRewriter, Expr *> {
    ConstraintSystem &cs;
    const Solution &solution;

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

    Expr *specialize(Expr *expr, PolymorphicFunctionType *polyFn,
                     Type openedType) {
      // Gather the substitutions from archetypes to concrete types, found
      // by identifying all of the type variables in the original type
      TypeSubstitutionMap substitutions;
      auto &tc = cs.getTypeChecker();
      auto type
        = tc.transformType(openedType,
            [&](Type type) -> Type {
              if (auto tv = dyn_cast<TypeVariableType>(type.getPointer())) {
                auto archetype = tv->getImpl().getArchetype();
                auto simplified = getFixedType(tv);
                substitutions[archetype] = simplified;

                return SubstitutedType::get(archetype, simplified, tc.Context);
              }

              return type;
            });

      // Validate the generated type, now that we've substituted in for
      // type variables.
      tc.validateTypeSimple(type);

      // Check that the substitutions we've produced actually work.
      // FIXME: We'd like the type checker to ensure that this always
      // succeeds.
      ConformanceMap conformances;
      if (tc.checkSubstitutions(substitutions, conformances, expr->getLoc(),
                                &substitutions))
        return nullptr;

      // Build the specialization expression.
      auto encodedSubs = tc.encodeSubstitutions(&polyFn->getGenericParams(),
                                                substitutions, conformances,
                                                /*ArchetypesAreOpen=*/false,
                                                /*OnlyInnermostParams=*/true);
      return new (tc.Context) SpecializeExpr(expr, type, encodedSubs);
    }

    /// \brief Build a new member reference with the given base and member.
    Expr *buildMemberRef(Expr *base, SourceLoc dotLoc, ValueDecl *member,
                         SourceLoc memberLoc, Type openedType,
                         ConstraintLocatorBuilder locator) {
      auto &tc = cs.getTypeChecker();
      auto &context = tc.Context;

      // Figure out the actual base type, and whether we have an instance of that
      // type or its metatype.
      Type baseTy = base->getType()->getRValueType();
      bool baseIsInstance = true;
      if (auto baseMeta = baseTy->getAs<MetaTypeType>()) {
        baseIsInstance = false;
        baseTy = baseMeta->getInstanceType();
      }

      // Member references into existential or archetype types.
      if (baseTy->isExistentialType() || baseTy->is<ArchetypeType>()) {
        // Convert the base to the type of the 'this' parameter.
        Type containerTy;
        if (baseTy->isExistentialType())
          containerTy = member->getDeclContext()->getDeclaredTypeOfContext();
        else
          containerTy = baseTy;
        
        if (baseIsInstance) {
          // Convert the base to the appropriate container type, turning it
          // into an lvalue if required.
          base = convertObjectArgumentToType(tc, base, containerTy);
        } else {
          // Convert the base to an rvalue of the appropriate metatype.
          base = coerceToType(base, MetaTypeType::get(containerTy, context),
                              /*isAssignment=*/false,
                              locator.withPathElement(
                                ConstraintLocator::MemberRefBase));
          base = tc.convertToRValue(base);
        }
        assert(base && "Unable to convert base?");

        // Build the member reference expression.
        Expr *result;
        if (baseTy->isExistentialType())
          result = new (context) ExistentialMemberRefExpr(base, dotLoc,
                                                          member, memberLoc);
        else
          result = new (context) ArchetypeMemberRefExpr(base, dotLoc,
                                                        member, memberLoc);

        // Simplify the type of this reference.
        result->setType(simplifyType(openedType));
        return result;
      }

      // Reference to a member of a generic type.
      if (baseTy->isSpecialized()) {
        // FIXME: Feels like we're re-doing a lot of work that the type
        // checker already did, given that we know the eventual type we're
        // going to produce.
        GenericParamList *genericParams = nullptr;
        CoercionContext cc(tc);
        Type substTy
          = tc.substBaseForGenericTypeMember(member, baseTy,
                                             member->getTypeOfReference(),
                                             memberLoc, cc,
                                             &genericParams);

        if (isa<FuncDecl>(member) || isa<OneOfElementDecl>(member) ||
            isa<ConstructorDecl>(member)) {
          // We're binding a reference to an instance method of a generic
          // type, which we build as a reference to the underlying declaration
          // specialized based on the deducing the arguments of the generic
          // type.

          // Reference to the generic member.
          Expr *ref = new (context) DeclRefExpr(member, memberLoc,
                                                member->getTypeOfReference());

          // Specialize the member with the types deduced from the object
          // argument. This eliminates the genericity that comes from being
          // an instance method of a generic class.
          Expr *specializedRef
            = tc.buildSpecializeExpr(ref, substTy, cc.Substitutions,
                                     cc.Conformance,
                                     /*OnlyInnermostParams=*/false);

          ApplyExpr *apply;
          if (isa<ConstructorDecl>(member)) {
            // FIXME: Provide type annotation.
            apply = new (context) ConstructorRefCallExpr(specializedRef, base);
          } else if (!baseIsInstance && member->isInstanceMember()) {
            return new (context) DotSyntaxBaseIgnoredExpr(base, dotLoc,
                                                          specializedRef);
          } else {
            assert((!baseIsInstance || member->isInstanceMember()) &&
                   "can't call a static method on an instance");
            apply = new (context) DotSyntaxCallExpr(specializedRef, dotLoc, base);
          }
          return finishApply(apply, openedType, nullptr);
        }

        // Convert the base appropriately.
        // FIXME: We could be referring to a member of a superclass, so find
        // that superclass and convert to it.
        if (baseIsInstance) {
          // Convert the base to the appropriate container type, turning it
          // into an lvalue if required.
          base = convertObjectArgumentToType(tc, base, baseTy);
        } else {
          // Convert the base to an rvalue of the appropriate metatype.
          base = coerceToType(base, MetaTypeType::get(baseTy, context),
                              /*isAssignment=*/false,
                              locator.withPathElement(
                                ConstraintLocator::MemberRefBase));
          base = tc.convertToRValue(base);
        }
        assert(base && "Unable to convert base?");

        // Build a reference to a generic member.
        auto result = new (context) GenericMemberRefExpr(base, dotLoc, member,
                                                         memberLoc);

        // Set the (substituted) type and the set of substitutions.
        // FIXME: Use simplifyType(openedType);
        result->setType(substTy);
        result->setSubstitutions(tc.encodeSubstitutions(genericParams,
                                                        cc.Substitutions,
                                                        cc.Conformance,
                                                        true, false));
        return result;
      }

      // Reference to a variable within a class.
      if (auto var = dyn_cast<VarDecl>(member)) {
        if (!baseTy->is<ModuleType>()) {
          // Convert the base to the type of the 'this' parameter.
          Type containerTy = var->getDeclContext()->getDeclaredTypeOfContext();
          assert(baseIsInstance && "Can only access variables of an instance");

          // Convert the base to the appropriate container type, turning it
          // into an lvalue if required.
          base = convertObjectArgumentToType(tc, base, containerTy);

          auto result
            = new (context) MemberRefExpr(base, dotLoc, var, memberLoc);
          result->setType(simplifyType(openedType));
          return result;
        }
      }

      // Handle references to non-variable struct/class/oneof members, as
      // well as module members.
      Expr *ref = new (context) DeclRefExpr(member, memberLoc,
                                            member->getTypeOfReference());

      // Refer to a member function that binds 'this':
      if ((isa<FuncDecl>(member) && member->getDeclContext()->isTypeContext()) ||
          isa<OneOfElementDecl>(member) || isa<ConstructorDecl>(member)) {
        // Constructor calls.
        if (isa<ConstructorDecl>(member)) {
          return finishApply(new (context) ConstructorRefCallExpr(ref, base),
                             openedType, nullptr);
        }

        // Non-static member function calls.
        if (baseIsInstance == member->isInstanceMember()) {
          return finishApply(new (context) DotSyntaxCallExpr(ref, dotLoc, base),
                             openedType, nullptr);
        }
        
        assert((!baseIsInstance || member->isInstanceMember()) &&
               "can't call a static method on an instance");
      }

      // Build a reference where the base is ignored.
      Expr *result = new (context) DotSyntaxBaseIgnoredExpr(base, dotLoc, ref);
      if (auto polyFn = result->getType()->getAs<PolymorphicFunctionType>()) {
        return specialize(result, polyFn, openedType);
      }

      return result;
    }

    /// \brief Convert the given literal expression to a specific type with
    /// a known literal kind.
    Expr *convertLiteral(Expr *literal, Type type, LiteralKind kind,
                         Type openedType);

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

    /// \brief Retrieve the fixed type for the given type variable.
    Type getFixedType(TypeVariableType *typeVar) {
      auto knownBinding = solution.typeBindings.find(typeVar);
      assert(knownBinding != solution.typeBindings.end());
      return knownBinding->second;
    }

    /// \brief Retrieve the overload choice associated with the given
    /// locator.
    std::pair<OverloadChoice, Type>
    getOverloadChoice(ConstraintLocator *locator) {
      return *getOverloadChoiceIfAvailable(locator);
    }

    /// \brief Retrieve the overload choice associated with the given
    /// locator.
    Optional<std::pair<OverloadChoice, Type>>
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
    ///
    /// \param isAssignment FIXME: Whether this is an assignment,
    /// which is only needed by the "old" type checker fallback.
    ///
    /// \param locator Locator used to describe where in this expression we are.
    ///
    /// \returns the coerced expression, which will have type \c ToType.
    Expr *coerceToType(Expr *expr, Type toType, bool isAssignment,
                       ConstraintLocatorBuilder locator);

  private:
    /// \brief Build a new subscript.
    Expr *buildSubscript(Expr *expr, Expr *base, Expr *index) {
      // Determine the declaration selected for this subscript operation.
      auto choice = getOverloadChoice(
                      cs.getConstraintLocator(
                        expr,
                        ConstraintLocator::SubscriptMember)).first;
      auto subscript = cast<SubscriptDecl>(choice.getDecl());

      // FIXME: Falls back to existing type checker to actually populate
      // these nodes.
      auto &tc = cs.getTypeChecker();
      auto baseTy = base->getType()->getRValueType();

      // Subscripting an existential type.
      if (baseTy->isExistentialType()) {
        auto result
          = new (tc.Context) ExistentialSubscriptExpr(base, index, subscript);
        return tc.semaSubscriptExpr(result);
      }

      // Subscripting an archetype.
      if (baseTy->is<ArchetypeType>()) {
        auto result
          = new (tc.Context) ArchetypeSubscriptExpr(base, index, subscript);
        return tc.semaSubscriptExpr(result);
      }

      // Subscripting a specialization of a generic type.
      if (baseTy->isSpecialized()) {
        auto result
          = new (tc.Context) GenericSubscriptExpr(base, index, subscript);
        return tc.semaSubscriptExpr(result);
      }

      // Subscripting a normal, nominal type.
      SubscriptExpr *subscriptExpr = new (tc.Context) SubscriptExpr(base,index);
      subscriptExpr->setDecl(subscript);
      return tc.semaSubscriptExpr(subscriptExpr);
    }

    /// \brief Build a reference to an operator within a protocol.
    Expr *buildProtocolOperatorRef(ProtocolDecl *proto, ValueDecl *value,
                                   SourceLoc nameLoc, Type openedType,
                                   ConstraintLocatorBuilder locator) {
      assert(isa<FuncDecl>(value) && "Only functions allowed");
      assert(cast<FuncDecl>(value)->isOperator() && "Only operators allowed");

      // Figure out the base type, which we do by finding the type variable
      // in the open type that corresponds to the 'This' archetype, which
      // we opened.
      // FIXME: This is both inefficient and suspicious. We should probably
      // find a place to cache the type variable, rather than searching for it
      // again.
      Type baseTy;
      auto thisArchetype
        = proto->getThis()->getDeclaredType()->castTo<ArchetypeType>();
      cs.getTypeChecker().transformType(openedType, [&](Type type) -> Type {
        if (auto typeVar = dyn_cast<TypeVariableType>(type.getPointer())) {
          if (typeVar->getImpl().getArchetype() == thisArchetype) {
            baseTy = getFixedType(typeVar);
            return nullptr;
          }
        }

        return type;
      });
      assert(baseTy && "Unable to find base type for protocol operator ref");
      // FIXME: Check whether baseTy is an archetype?

      auto &ctx = cs.getASTContext();
      auto base = new (ctx) MetatypeExpr(nullptr, nameLoc,
                                         MetaTypeType::get(baseTy, ctx));
      return buildMemberRef(base, SourceLoc(), value, nameLoc, openedType,
                            locator);
    }

  public:
    ExprRewriter(ConstraintSystem &cs, const Solution &solution)
      : cs(cs), solution(solution) { }

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

    Expr *visitIntegerLiteralExpr(IntegerLiteralExpr *expr) {
      return convertLiteral(expr, simplifyType(expr->getType()),
                            LiteralKind::Int, expr->getType());
    }

    Expr *visitFloatLiteralExpr(FloatLiteralExpr *expr) {
      return convertLiteral(expr, simplifyType(expr->getType()),
                            LiteralKind::Float, expr->getType());
    }

    Expr *visitCharacterLiteralExpr(CharacterLiteralExpr *expr) {
      return convertLiteral(expr, simplifyType(expr->getType()),
                            LiteralKind::Char, expr->getType());
    }

    Expr *visitStringLiteralExpr(StringLiteralExpr *expr) {
      // FIXME: Already did this when generating constraints.
      auto kind = LiteralKind::ASCIIString;
      for (unsigned char c : expr->getValue()) {
        if (c > 127) {
          kind = LiteralKind::UTFString;
          break;
        }
      }

      return convertLiteral(expr, simplifyType(expr->getType()), kind,
                            expr->getType());
    }

    Expr *
    visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *expr) {
      // Figure out the string type we're converting to.
      auto openedType = expr->getType();
      auto type = simplifyType(openedType);
      expr->setType(type);

      // Find the string interpolation protocol we need.
      auto &tc = cs.getTypeChecker();
      auto interpolationProto = tc.getStringInterpolationConvertibleProtocol();
      assert(interpolationProto && "Missing string interpolation protocol?");

      // Find the 'convertFromStringInterpolation' requirement.
      FuncDecl *requirement = nullptr;
      for (auto member : interpolationProto->getMembers()) {
        auto fd = dyn_cast<FuncDecl>(member);
        if (!fd || fd->getName().empty())
          continue;

        if (fd->getName().str() == "convertFromStringInterpolation") {
          requirement = fd;
          break;
        }
      }
      if (!requirement) {
        tc.diagnose(interpolationProto->getLoc(),
                    diag::interpolation_broken_proto);
        return nullptr;
      }

      // Find the member used to satisfy the 'convertFromStringInterpolation'
      // requirement.
      ProtocolConformance *conformance = 0;
      bool conforms = tc.conformsToProtocol(type, interpolationProto,
                                            &conformance);
      (void)conforms;
      assert(conforms && conformance && "Interpolation conformance broken?");
      assert(conformance->Mapping.count(requirement) && "Missing conformance");
      auto member = conformance->Mapping[requirement];

      // Build a reference to the convertFromStringInterpolation member.
      // FIXME: Dubious source location information.
      auto typeRef = new (tc.Context) MetatypeExpr(
                                        nullptr, expr->getStartLoc(),
                                        MetaTypeType::get(type, tc.Context));
      // FIXME: The openedType is wrong for generic string types.
      Expr *memberRef = buildMemberRef(typeRef, expr->getStartLoc(), member,
                                       expr->getStartLoc(),
                                       member->getTypeOfReference(),
                                       cs.getConstraintLocator(expr, { }));

      // Create a tuple containing all of the coerced segments.
      SmallVector<Expr *, 4> segments;
      unsigned index = 0;
      ConstraintLocatorBuilder locatorBuilder(cs.getConstraintLocator(expr,
                                                                      { }));
      for (auto segment : expr->getSegments()) {
        segment = coerceToType(segment, type, /*isAssignment=*/false,
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
                                              TupleType::get(tupleElements,
                                                             tc.Context));
      }

      // Call the convertFromStringInterpolation member with the arguments.
      ApplyExpr *apply = new (tc.Context) CallExpr(memberRef, argument);
      expr->setSemanticExpr(finishApply(apply, openedType, locatorBuilder));
      return expr;
    }

    Expr *visitDeclRefExpr(DeclRefExpr *expr) {
      auto fromType = expr->getType();

      if (auto proto
            = dyn_cast<ProtocolDecl>(expr->getDecl()->getDeclContext())) {
        // If this a member of a protocol, build an appropriate operator
        // reference.
        return buildProtocolOperatorRef(proto, expr->getDecl(), expr->getLoc(),
                                        fromType,
                                        cs.getConstraintLocator(expr, { }));
      }

      // Set the type of this expression to the actual type of the reference.
      expr->setType(expr->getDecl()->getTypeOfReference());

      // If there is no type variable in the original expression type, we're
      // done.
      if (!fromType->hasTypeVariable())
        return expr;

      // Check whether this is a polymorphic function type, which needs to
      // be specialized.
      if (auto polyFn = expr->getType()->getAs<PolymorphicFunctionType>()) {
        return specialize(expr, polyFn, fromType);
      }

      simplifyExprType(expr);

      // Check whether this is a generic type.
      if (auto meta = expr->getType()->getAs<MetaTypeType>()) {
        if (meta->getInstanceType()->is<UnboundGenericType>()) {
          // If so, type the declref as the bound generic type.
          // FIXME: Is this right?
          auto simplifiedType = simplifyType(fromType);
          expr->setType(simplifiedType);
          return expr;
        }
      }

      // No polymorphic function; this a reference to a declaration with a
      // deduced type, such as $0.
      simplifyExprType(expr);
      return expr;
    }

    Expr *visitSuperRefExpr(SuperRefExpr *expr) {
      // Recast the expr as being of the original type of 'this' so that
      // coercion inserts the necessary conversion nodes for IRGen.
      expr->setType(expr->getThis()->getTypeOfReference());
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
      auto choice = selected.first;
      auto *ctor = cast<ConstructorDecl>(choice.getDecl());

      // Build a call to the initializer for the constructor.
      Expr *ctorRef
        = new (cs.getASTContext()) OtherConstructorDeclRefExpr(ctor,
                                     expr->getConstructorLoc(),
                                     ctor->getInitializerType());
      if (auto polyFn = ctorRef->getType()->getAs<PolymorphicFunctionType>()) {
        ctorRef = specialize(ctorRef, polyFn, selected.second);
      }

      auto *call
        = new (cs.getASTContext()) DotSyntaxCallExpr(ctorRef,
                                                     expr->getDotLoc(),
                                                     expr->getSubExpr());
      return finishApply(call, expr->getType(),
                         ConstraintLocatorBuilder(
                           cs.getConstraintLocator(expr, { })));
    }

    Expr *visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Expr *visitOverloadedDeclRefExpr(OverloadedDeclRefExpr *expr) {
      // Determine the declaration selected for this overloaded reference.
      auto &context = cs.getASTContext();
      auto selected = getOverloadChoice(cs.getConstraintLocator(expr, { }));
      auto choice = selected.first;
      auto decl = choice.getDecl();

      if (auto proto = dyn_cast<ProtocolDecl>(decl->getDeclContext())) {
        // If this a member of a protocol, build an appropriate operator
        // reference.
        return buildProtocolOperatorRef(proto, decl, expr->getLoc(),
                                        selected.second,
                                        cs.getConstraintLocator(expr, { }));
      }

      // Normal path: build a declaration reference.
      auto result = new (context) DeclRefExpr(decl, expr->getLoc(),
                                              decl->getTypeOfReference());

      // For a polymorphic function type, we have to specialize our reference.
      if (auto polyFn = result->getType()->getAs<PolymorphicFunctionType>()) {
        return specialize(result, polyFn, selected.second);
      }

      return result;
    }

    Expr *visitOverloadedMemberRefExpr(OverloadedMemberRefExpr *expr) {
      auto selected = getOverloadChoice(
                        cs.getConstraintLocator(expr,
                                                ConstraintLocator::Member));
      return buildMemberRef(expr->getBase(), expr->getDotLoc(),
                            selected.first.getDecl(), expr->getMemberLoc(),
                            selected.second,
                            cs.getConstraintLocator(expr, { }));
    }

    Expr *visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *expr) {
      // FIXME: We should have generated an overload set from this, in which
      // case we can emit a typo-correction error here but recover well.
      return nullptr;
    }

    Expr *visitUnresolvedSpecializeExpr(UnresolvedSpecializeExpr *expr) {
      // Our specializations should have resolved the subexpr to the right type.
      // FIXME: Should preserve generic argument list for source fidelity
      return expr->getSubExpr();
    }

    Expr *visitMemberRefExpr(MemberRefExpr *expr) {
      return buildMemberRef(expr->getBase(), expr->getDotLoc(), expr->getDecl(),
                            expr->getNameLoc(), expr->getType(),
                            cs.getConstraintLocator(expr, { }));
    }

    Expr *visitExistentialMemberRefExpr(ExistentialMemberRefExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Expr *visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *expr) {
      auto selected = getOverloadChoice(
                        cs.getConstraintLocator(expr,
                                                ConstraintLocator::Member));
      return buildMemberRef(expr->getBase(), expr->getDotLoc(),
                            selected.first.getDecl(), expr->getNameLoc(),
                            selected.second,
                            cs.getConstraintLocator(expr, { }));
    }

    Expr *visitGenericMemberRefExpr(GenericMemberRefExpr *expr) {
      auto selected = getOverloadChoice(
                        cs.getConstraintLocator(expr,
                                                ConstraintLocator::Member));
      return buildMemberRef(expr->getBase(), expr->getDotLoc(),
                            selected.first.getDecl(), expr->getNameLoc(),
                            selected.second,
                            cs.getConstraintLocator(expr, { }));
    }

    Expr *visitUnresolvedMemberExpr(UnresolvedMemberExpr *expr) {
      // Dig out the type of the 'oneof', which will either be the result
      // type of this expression (for unit OneOfElements) or the result of
      // the function type of this expression (for non-unit OneOfElements).
      Type oneofTy = simplifyType(expr->getType());
      if (auto funcTy = oneofTy->getAs<FunctionType>())
        oneofTy = funcTy->getResult();
      auto &tc = cs.getTypeChecker();
      auto oneofMetaTy = MetaTypeType::get(oneofTy, tc.Context);

      // Find the selected member.
      auto selected = getOverloadChoice(
                        cs.getConstraintLocator(expr,
                                                ConstraintLocator::Member));
      auto member = selected.first.getDecl();

      // The base expression is simply the metatype of a oneof type.
      auto base = new (tc.Context) MetatypeExpr(nullptr,
                                                expr->getDotLoc(),
                                                oneofMetaTy);

      // Build the member reference.
      return buildMemberRef(base, expr->getDotLoc(), member, expr->getNameLoc(),
                            selected.second,
                            cs.getConstraintLocator(expr, { }));
    }

    Expr *visitUnresolvedDotExpr(UnresolvedDotExpr *expr) {
      // Determine the declaration selected for this overloaded reference.
      auto selected = getOverloadChoice(
                        cs.getConstraintLocator(
                          expr,
                          ConstraintLocator::MemberRefBase));

      switch (selected.first.getKind()) {
      case OverloadChoiceKind::Decl:
        return buildMemberRef(expr->getBase(), expr->getDotLoc(),
                              selected.first.getDecl(), expr->getNameLoc(),
                              selected.second,
                              cs.getConstraintLocator(expr, { }));

      case OverloadChoiceKind::TupleIndex: {
        auto base = expr->getBase();
        // If the base expression is not an lvalue, make everything inside it
        // materializable.
        if (!base->getType()->is<LValueType>()) {
          base = cs.getTypeChecker().convertToMaterializable(base);
          if (!base)
            return nullptr;
        }

        return new (cs.getASTContext()) TupleElementExpr(
                                          base,
                                          expr->getDotLoc(),
                                          selected.first.getTupleIndex(),
                                          expr->getNameLoc(),
                                          simplifyType(expr->getType()));
      }

      case OverloadChoiceKind::BaseType:
      case OverloadChoiceKind::FunctionReturningBaseType:
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
      return buildSubscript(expr, expr->getBase(), expr->getIndex());
    }

    Expr *visitArrayExpr(ArrayExpr *expr) {
      Type openedType = expr->getType();
      Type arrayTy = simplifyType(openedType);

      ProtocolDecl *arrayProto = cs.getTypeChecker().getArrayLiteralProtocol();
      assert(arrayProto && "type-checked array literal w/o protocol?!");

       // Use a value member constraint to find the appropriate
       // convertFromArrayLiteral call.
       // FIXME: Switch to protocol conformance.
      auto selected = getOverloadChoice(
                        cs.getConstraintLocator(
                          expr,
                          ConstraintLocator::MemberRefBase));
      auto choice = selected.first;
      auto converterDecl = cast<FuncDecl>(choice.getDecl());

      // Construct the semantic expr as a convertFromArrayLiteral application.
      ASTContext &C = cs.getASTContext();
      Expr *typeRef = new (C) MetatypeExpr(nullptr,
                                           expr->getLoc(),
                                           MetaTypeType::get(arrayTy, C));
      // FIXME: Location information is suspect.
      Expr *memberRef = buildMemberRef(typeRef, expr->getLoc(),
                                       converterDecl,
                                       expr->getLoc(),
                                       selected.second,
                                       cs.getConstraintLocator(expr, { }));

      ApplyExpr *apply = new (C) CallExpr(memberRef, expr->getSubExpr());
      expr->setSemanticExpr(finishApply(apply, openedType,
                                        ConstraintLocatorBuilder(
                                          cs.getConstraintLocator(expr, { }))));
      if (!expr->getSemanticExpr()) {
        // FIXME: Should never happen.
        cs.getTypeChecker().diagnose(arrayProto->getLoc(),
                                     diag::array_protocol_broken);
        return nullptr;
      }

      expr->setType(arrayTy);
      return expr;
    }

    Expr *visitDictionaryExpr(DictionaryExpr *expr) {
      Type openedType = expr->getType();
      Type dictionaryTy = simplifyType(openedType);

      ProtocolDecl *dictionaryProto
      = cs.getTypeChecker().getDictionaryLiteralProtocol();
      assert(dictionaryProto && "type-checked dictionary literal w/o protocol?");

      // Use a value member constraint to find the appropriate
      // convertFromDictionaryLiteral call.
      // FIXME: Switch to protocol conformance.
      auto selected = getOverloadChoice(
                        cs.getConstraintLocator(
                          expr,
                          ConstraintLocator::MemberRefBase));
      auto choice = selected.first;
      auto converterDecl = cast<FuncDecl>(choice.getDecl());

      // Construct the semantic expr as a convertFromDictionaryLiteral
      // application.
      ASTContext &C = cs.getASTContext();
      Expr *typeRef = new (C) MetatypeExpr(nullptr,
                                           expr->getLoc(),
                                           MetaTypeType::get(dictionaryTy, C));

      // FIXME: Location information is suspect.
      Expr *memberRef = buildMemberRef(typeRef, expr->getLoc(),
                                       converterDecl,
                                       expr->getLoc(),
                                       selected.second,
                                       cs.getConstraintLocator(expr, { }));

      ApplyExpr *apply = new (C) CallExpr(memberRef, expr->getSubExpr());
      expr->setSemanticExpr(finishApply(apply, openedType,
                                        ConstraintLocatorBuilder(
                                          cs.getConstraintLocator(expr, { }))));
      if (!expr->getSemanticExpr()) {
        // FIXME: Should never happen.
        cs.getTypeChecker().diagnose(dictionaryProto->getLoc(),
                                     diag::dictionary_protocol_broken);
        return nullptr;
      }

      expr->setType(dictionaryTy);
      return expr;
    }

    Expr *visitOverloadedSubscriptExpr(OverloadedSubscriptExpr *expr) {
      return buildSubscript(expr, expr->getBase(), expr->getIndex());
    }

    Expr *visitExistentialSubscriptExpr(ExistentialSubscriptExpr *expr) {
      return buildSubscript(expr, expr->getBase(), expr->getIndex());
    }

    Expr *visitArchetypeSubscriptExpr(ArchetypeSubscriptExpr *expr) {
      return buildSubscript(expr, expr->getBase(), expr->getIndex());
    }

    Expr *visitGenericSubscriptExpr(GenericSubscriptExpr *expr) {
      return buildSubscript(expr, expr->getBase(), expr->getIndex());
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
      }

      llvm_unreachable("Unhandled pattern kind");
    }

    Expr *visitFuncExpr(FuncExpr *expr) {
      // FIXME: Type-check the function now? Or queue for later?
      simplifyExprType(expr);

      // Coerce the FuncExpr's pattern, in case we resolved something.
      Type input = expr->getType()->castTo<FunctionType>()->getInput();
      auto &tc = cs.getTypeChecker();
      if (tc.coerceToType(expr->getArgParamPatterns()[0], input, false))
        return nullptr;
      if (tc.coerceToType(expr->getBodyParamPatterns()[0], input, false))
        return nullptr;

      return expr;
    }

    Expr *visitExplicitClosureExpr(ExplicitClosureExpr *expr) {
      auto type = simplifyType(expr->getType())->castTo<FunctionType>();

      // Count the number of arguments.
      unsigned numInputArgs = 1;
      TupleType *inputTT = type->getInput()->getAs<TupleType>();
      if (inputTT)
        numInputArgs = inputTT->getFields().size();

      // Build up the set of VarDecls (building more if necessary).
      std::vector<VarDecl*> argVars(expr->getParserVarDecls().begin(),
                                    expr->getParserVarDecls().end());
      Pattern *argPat;
      SourceLoc loc = expr->getLoc();
      expr->GenerateVarDecls(numInputArgs, argVars, cs.getASTContext());

      // Build the patterns and update the variable types.
      if (inputTT) {
        std::vector<TuplePatternElt> argElts;
        for (unsigned i = 0; i < numInputArgs; ++i) {
          argVars[i]->overwriteType(inputTT->getElementType(i));
          auto p = new (cs.getASTContext()) NamedPattern(argVars[i]);
          p->setType(inputTT->getElementType(i));
          argElts.emplace_back(p);
        }
        argPat = TuplePattern::create(cs.getASTContext(), loc, argElts, loc);
      } else {
        argVars[0]->overwriteType(type->getInput());
        argPat = new (cs.getASTContext()) NamedPattern(argVars[0]);
      }
      argPat->setType(type->getInput());
      expr->setPattern(argPat);

      // Convert the expression in the body to the result type of the explicit
      // closure.
      auto resultType = type->getResult();
      if (Expr *body = coerceToType(expr->getBody(), resultType,
                                    /*isAssignment=*/false,
                                    cs.getConstraintLocator(
                                      expr,
                                      ConstraintLocator::ClosureResult)))
        expr->setBody(body);
      expr->setType(type);

      // Compute the capture list, now that we have analyzed the expression.
      cs.getTypeChecker().computeCaptures(expr);

      return expr;
    }

    Expr *visitImplicitClosureExpr(ImplicitClosureExpr *expr) {
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
        sliceType = ArraySliceType::get(elementType, tc.Context);
        sliceType->setImplementationType(
          resultType->castTo<BoundGenericType>());
        resultType = sliceType;
      }
      expr->setType(resultType);

      // Find the appropriate injection function.
      Expr* injectionFn = tc.buildArrayInjectionFnRef(sliceType,
                            expr->getBounds()[0].Value->getType(),
                            expr->getNewLoc());
      if (!injectionFn)
        return nullptr;
      expr->setInjectionFunction(injectionFn);

      return expr;
    }

    Expr *visitMetatypeExpr(MetatypeExpr *expr) {
      auto &tc = cs.getTypeChecker();

      if (Expr *base = expr->getBase()) {
        base = tc.convertToRValue(base);
        if (!base) return nullptr;
        expr->setBase(base);
        expr->setType(MetaTypeType::get(base->getType(), tc.Context));
      }
      return expr;
    }

    Expr *visitOpaqueValueExpr(OpaqueValueExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Expr *visitZeroValueExpr(ZeroValueExpr *expr) {
      // Do nothing with zero-value initialization expressions.
      return simplifyExprType(expr);
    }

    Expr *visitDefaultValueExpr(DefaultValueExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Expr *visitApplyExpr(ApplyExpr *expr) {
      return finishApply(expr, expr->getType(),
                         ConstraintLocatorBuilder(
                           cs.getConstraintLocator(expr, { })));
    }

    Expr *visitRebindThisInConstructorExpr(RebindThisInConstructorExpr *expr) {
      return expr;
    }

    Expr *visitIfExpr(IfExpr *expr) {
      {
        // FIXME: Disable the constraint-based type checker here, because we are
        // falling back to the existing type checker.
        auto &tc = cs.getTypeChecker();
        llvm::SaveAndRestore<bool> savedUseCS(tc.getLangOpts()
                                                .UseConstraintSolver,
                                              false);

        Expr *condExpr = expr->getCondExpr();
        if (tc.typeCheckCondition(condExpr))
          return nullptr;
        expr->setCondExpr(condExpr);
      }

      auto resultTy = simplifyType(expr->getType());
      expr->setType(resultTy);

      expr->setThenExpr(coerceToType(expr->getThenExpr(), resultTy,
                                     /*isAssignment=*/false,
                                     ConstraintLocatorBuilder(
                                       cs.getConstraintLocator(expr, { }))
                                     .withPathElement(
                                       ConstraintLocator::IfThen)));
      expr->setElseExpr(coerceToType(expr->getElseExpr(), resultTy,
                                     /*isAssignment=*/false,
                                     ConstraintLocatorBuilder(
                                       cs.getConstraintLocator(expr, { }))
                                     .withPathElement(
                                       ConstraintLocator::IfElse)));

      return expr;
    }
    
    Expr *visitUnresolvedIfExpr(UnresolvedIfExpr *E) {
      llvm_unreachable("this node should be eliminated by name binding");
    }
    Expr *visitUnresolvedElseExpr(UnresolvedElseExpr *E) {
      llvm_unreachable("this node should be eliminated by name binding");
    }

    Expr *visitImplicitConversionExpr(ImplicitConversionExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Expr *visitCoerceExpr(CoerceExpr *expr) {
      expr->setType(simplifyType(expr->getType()));
      Expr *subExpr = coerceToType(expr->getSubExpr(), expr->getType(),
                                   /*isAssignment=*/false,
                                   cs.getConstraintLocator(expr, { }));
      expr->setSubExpr(subExpr);
      return expr;
    }

    Expr *visitUncheckedDowncastExpr(UncheckedDowncastExpr *expr) {
      auto &C = cs.getASTContext();

      expr->setType(simplifyType(expr->getType()));

      Expr *sub = cs.getTypeChecker().convertToRValue(expr->getSubExpr());
      if (!sub) return nullptr;
      expr->setSubExpr(sub);

      // If the source had archetype type, convert to its superclass first.
      // We then downcast that value.
      if (auto *srcArchetype
          = expr->getSubExpr()->getType()->getAs<ArchetypeType>()) {
        auto *subExpr = new (C) ArchetypeToSuperExpr(expr->getSubExpr(),
                                                     srcArchetype->getSuperclass());
        expr->setSubExpr(subExpr);
      }
      
      // If the destination type is an archetype, this is a super-to-archetype
      // cast.
      if (expr->getType()->is<ArchetypeType>()) {
        auto *stoa = new (C) UncheckedSuperToArchetypeExpr(expr->getSubExpr(),
                                                           expr->getLoc(),
                                                           expr->getBangLoc(),
                                                           expr->getTypeLoc());
        stoa->setType(expr->getType());
        return stoa;
      }
      
      return expr;
    }
    
    Expr *visitUncheckedSuperToArchetypeExpr(
            UncheckedSuperToArchetypeExpr *expr) {
      llvm_unreachable("Already type-checked");
    }
    
    Expr *visitIsSubtypeExpr(IsSubtypeExpr *expr) {
      expr->setType(simplifyType(expr->getType()));
      
      Expr *sub = cs.getTypeChecker().convertToRValue(expr->getSubExpr());
      if (!sub) return nullptr;
      expr->setSubExpr(sub);

      return expr;
    }
  };
}

Expr *ExprRewriter::coerceTupleToTuple(Expr *expr, TupleType *fromTuple,
                                       TupleType *toTuple,
                                       ConstraintLocatorBuilder locator,
                                       SmallVectorImpl<int> &sources,
                                       SmallVectorImpl<unsigned> &variadicArgs){
  auto &tc = cs.getTypeChecker();

  // Capture the tuple expression, if there is one.
  TupleExpr *fromTupleExpr = dyn_cast<TupleExpr>(expr);

  /// Check each of the tuple elements in the destination.
  bool hasVarArg = false;
  bool anythingShuffled = false;
  bool hasInits = false;
  SmallVector<TupleTypeElt, 4> toSugarFields;
  SmallVector<TupleTypeElt, 4> fromTupleExprFields(
                                 fromTuple->getFields().size());
  for (unsigned i = 0, n = toTuple->getFields().size(); i != n; ++i) {
    const auto &toElt = toTuple->getFields()[i];
    auto toEltType = toElt.getType();

    // If we're default-initializing this member, there's nothing to do.
    if (sources[i] == TupleShuffleExpr::DefaultInitialize) {
      anythingShuffled = true;
      hasInits = true;
      toSugarFields.push_back(toElt);
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
                                           toElt.getInit(),
                                           toElt.getVarargBaseTy()));
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
                     /*isAssignment=*/false,
                     locator.withPathElement(
                       LocatorPathElt::getTupleElement(sources[i])));
    if (!convertedElt)
      return nullptr;

    fromTupleExpr->setElement(sources[i], convertedElt);

    // Record the sugared field name.
    toSugarFields.push_back(TupleTypeElt(convertedElt->getType(),
                                         toElt.getName(),
                                         toElt.getInit(),
                                         toElt.getVarargBaseTy()));
    fromTupleExprFields[sources[i]] = TupleTypeElt(convertedElt->getType(),
                                                   fromElt.getName(),
                                                   fromElt.getInit(),
                                                   fromElt.getVarargBaseTy());
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
                            /*isAssignment=*/false,
                            locator.withPathElement(
                              LocatorPathElt::getTupleElement(fromFieldIdx)));
      if (!convertedElt)
        return nullptr;

      fromTupleExpr->setElement(fromFieldIdx, convertedElt);
      sources.push_back(fromFieldIdx);

      fromTupleExprFields[fromFieldIdx] = TupleTypeElt(
                                            convertedElt->getType(),
                                            fromElt.getName(),
                                            fromElt.getInit(),
                                            fromElt.getVarargBaseTy());
    }

    // Find the appropriate injection function.
    ArraySliceType *sliceType
      = cast<ArraySliceType>(
          toTuple->getFields().back().getType().getPointer());
    Type boundType = BuiltinIntegerType::get(64, tc.Context);
    injectionFn = tc.buildArrayInjectionFnRef(sliceType, boundType,
                                              expr->getStartLoc());
    if (!injectionFn)
      return nullptr;
  }

  // Compute the updated 'from' tuple type, since we may have
  // performed some conversions in place.
  Type fromTupleType = TupleType::get(fromTupleExprFields, tc.Context);
  if (fromTupleExpr)
    fromTupleExpr->setType(fromTupleType);

  // Compute the re-sugared tuple type.
  Type toSugarType = hasInits? toTuple
                             : TupleType::get(toSugarFields, tc.Context);

  // If we don't have to shuffle anything, we're done.
  if (!anythingShuffled && fromTupleExpr) {
    expr->setType(toSugarType);
    return expr;
  }
  
  // Create the tuple shuffle.
  ArrayRef<int> mapping = tc.Context.AllocateCopy(sources);
  auto shuffle = new (tc.Context) TupleShuffleExpr(expr, mapping, toSugarType);
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
    injectionFn = tc.buildArrayInjectionFnRef(sliceType, boundType,
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
  expr = coerceToType(expr, toScalarType, /*isAssignment=*/false,
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
        assert((field.getVarargBaseTy()->isUnresolvedType() ||
                expr->getType()->isEqual(field.getVarargBaseTy())) &&
               "scalar field is not equivalent to dest vararg field?!");

        sugarFields.push_back(TupleTypeElt(field.getType(),
                                           field.getName(),
                                           field.getInit(),
                                           expr->getType()));
      }
      else {
        assert((field.getType()->isUnresolvedType() ||
                expr->getType()->isEqual(field.getType())) &&
               "scalar field is not equivalent to dest tuple field?!");
        sugarFields.push_back(TupleTypeElt(expr->getType(),
                                           field.getName()));
      }
    } else {
      sugarFields.push_back(field);
    }
    ++i;
  }

  Type destSugarTy = hasInit? toTuple
                            : TupleType::get(sugarFields, tc.Context);

  return new (tc.Context) ScalarToTupleExpr(expr, destSugarTy, toScalarIdx,
                                            injectionFn);
}

Expr *ExprRewriter::coerceToType(Expr *expr, Type toType, bool isAssignment,
                                 ConstraintLocatorBuilder locator) {
  auto &tc = cs.getTypeChecker();

  // The type we're converting from.
  Type fromType = expr->getType();

  // If the types are already equivalent, we don't have to do anything.
  if (fromType->isEqual(toType))
    return expr;

  ++NumCoercions;

  // Save the original expression. If we fail to fully coerce the type,
  // fall back to the original.
  Expr *origExpr = expr;

  // Coercions to tuple type.
  if (auto toTuple = toType->getAs<TupleType>()) {
    // Coerce from a tuple to a tuple.
    if (auto fromTuple = fromType->getAs<TupleType>()) {
      SmallVector<int, 4> sources;
      SmallVector<unsigned, 4> variadicArgs;
      if (!computeTupleShuffle(fromTuple, toTuple, sources, variadicArgs)) {
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

  // Coercions from an lvalue: requalify and load. We perform these coercions
  // first because they are often the first step in a multi-step coercion.
  if (auto fromLValue = fromType->getAs<LValueType>()) {
    if (auto toLValue = toType->getAs<LValueType>()) {
      // Update the qualifiers on the lvalue.
      expr = new (tc.Context) RequalifyExpr(
                                expr,
                                LValueType::get(fromLValue->getObjectType(),
                                                toLValue->getQualifiers(),
                                                tc.Context));
    } else {
      // Load from the lvalue.
      expr = new (tc.Context) LoadExpr(expr, fromLValue->getObjectType());
    }

    // If we succeeded, use the coerced result.
    if (expr->getType()->isEqual(toType)) {
      return expr;
    }

    fromType = expr->getType();
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
      expr = coerceToType(expr, toFunc->getResult(), /*isAssignment=*/false,
                          locator.withPathElement(ConstraintLocator::Load));

      // FIXME: Bogus declaration context.
      auto ice = new (tc.Context) ImplicitClosureExpr(expr, &tc.TU, toType);
      Pattern *pattern = TuplePattern::create(tc.Context, expr->getLoc(),
                                              ArrayRef<TuplePatternElt>(),
                                              expr->getLoc());
      pattern->setType(TupleType::getEmpty(tc.Context));
      ice->setPattern(pattern);

      // Compute the capture list, now that we have analyzed the expression.
      tc.computeCaptures(ice);

      return ice;
    }

    // Coercion from one function type to another.
    auto fromFunc = fromType->getAs<FunctionType>();
    if (fromFunc) {
      bool trivial = false;
      bool isSubtype = tc.isSubtypeOf(fromType, toType, trivial);
      (void)isSubtype;
      assert(isSubtype && "No subtyping relationship between function types?");
      return new (tc.Context) FunctionConversionExpr(expr, toType, trivial);
    }
  }

  // Coercions from a type to an existential type.
  if (toType->isExistentialType()) {
    // Compute the conformances for each of the protocols in the existential
    // type.
    SmallVector<ProtocolDecl *, 4> protocols;
    toType->isExistentialType(protocols);
    SmallVector<ProtocolConformance *, 4> conformances;
    bool failed = false;
    for (auto proto : protocols) {
      ProtocolConformance *conformance = nullptr;
      if (!tc.conformsToProtocol(fromType, proto, &conformance)) {
        failed = true;
        break;
      }

      conformances.push_back(conformance);
    }

    // If we have all of the conformances we need, create an erasure expression.
    if (!failed) {
      expr = new (tc.Context) ErasureExpr(
                                expr, toType,
                                tc.Context.AllocateCopy(conformances));
      return expr;
    }

    // Fall through to handle user-defined conversions.
    // FIXME: Can the type checker cope with the crazy case where we can
    // call a user-defined conversion on an existential to produce an
    // existential of some related kind?
  }

  // Check for user-defined conversions.
  if (fromType->getNominalOrBoundGenericNominal() ||
      toType->getNominalOrBoundGenericNominal()) {
    // Determine the locator that corresponds to the conversion member.
    auto storedLocator
      = cs.getConstraintLocator(
          locator.withPathElement(ConstraintLocator::ConversionMember));
    auto knownOverload = solution.overloadChoices.find(storedLocator);
    if (knownOverload != solution.overloadChoices.end()) {
      auto selected = knownOverload->second;
      
      // FIXME: Location information is suspect throughout.
      // Form a reference to the conversion member.
      auto memberRef = buildMemberRef(expr, expr->getStartLoc(),
                                      selected.first.getDecl(),
                                      expr->getEndLoc(),
                                      selected.second,
                                      locator);

      // Form an empty tuple.
      Expr *args = new (tc.Context) TupleExpr(expr->getStartLoc(), { },
                                              nullptr, expr->getEndLoc(),
                                              TupleType::getEmpty(tc.Context));

      // Call the conversion function with an empty tuple.
      ApplyExpr *apply = new (tc.Context) CallExpr(memberRef, args);
      auto openedType = selected.second->castTo<FunctionType>()->getResult();
      expr = finishApply(apply, openedType,
                         ConstraintLocatorBuilder(
                           cs.getConstraintLocator(expr, { })));
    } else {
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
      
      // Form a reference to the constructor or oneof declaration.
      Expr *typeBase = new (tc.Context) MetatypeExpr(
                                          nullptr,
                                          expr->getStartLoc(),
                                          MetaTypeType::get(toType,tc.Context));
      Expr *declRef = buildMemberRef(typeBase, expr->getStartLoc(),
                                     selected.first.getDecl(),
                                     expr->getStartLoc(),
                                     selected.second,
                                     storedLocator);

      // FIXME: Lack of openedType here is an issue.
      ApplyExpr *apply = new (tc.Context) CallExpr(declRef, expr);
      expr = finishApply(apply, toType, locator);
    }
  }

  // If we succeeded, use the coerced result.
  if (expr->getType()->isEqual(toType)) {
    return expr;
  }

  // FIXME: Disable the constraint-based type checker here, because we are
  // falling back to the existing type checker.
  llvm::SaveAndRestore<bool> savedUseCS(tc.getLangOpts().UseConstraintSolver,
                                        false);
  ++NumMissedCoercions;
  return tc.coerceToType(origExpr, toType,
                         isAssignment? CoercionKind::Assignment
                                     : CoercionKind::Normal);
}

/// \brief Determine if this literal kind is a string literal.
static bool isStringLiteralKind(LiteralKind kind) {
  return kind == LiteralKind::UTFString ||
         kind == LiteralKind::ASCIIString;
}

// Check for (Builtin.RawPointer, Builtin.Int64).
static bool isRawPtrAndInt64(Type ty) {
  TupleType *tt = ty->getAs<TupleType>();
  if (!tt)
    return false;
  if (tt->getFields().size() != 2)
    return false;
  if (!tt->getElementType(0)->is<BuiltinRawPointerType>())
    return false;
  BuiltinIntegerType *intTy
    = tt->getElementType(1)->getAs<BuiltinIntegerType>();
  if (!intTy)
    return false;
  if (intTy->getBitWidth() != 64)
    return false;
  return true;
}

Expr *ExprRewriter::convertLiteral(Expr *literal, Type type, LiteralKind kind,
                                   Type openedType) {
  TypeChecker &tc = cs.getTypeChecker();
  
  // Check the destination type to see if it is compatible with literals,
  // diagnosing the failure if not.
  // FIXME: The Complain and Location arguments are pointless for us.
  FuncDecl *method;
  Type argType;
  std::tie(method, argType)
    = tc.isLiteralCompatibleType(type, SourceLoc(), kind, /*Complain=*/false);
  assert(method && "Literal type is not compatible?");

  // If the destination type is equivalent to the default literal type, use
  // the default literal type as the sugared type of the literal.
  Type defaultLiteralTy = tc.getDefaultLiteralType(kind);
  if (type->isEqual(defaultLiteralTy))
    type = defaultLiteralTy;
  // Additionally, if an integer literal gets used as the default floating-point
  // type, use the default floating-point literal type as the sugared type.
  else if (kind == LiteralKind::Int) {
    Type defaultFloatLiteralTy = tc.getDefaultLiteralType(LiteralKind::Float);
    if (type->isEqual(defaultFloatLiteralTy))
      type = defaultFloatLiteralTy;
  }
  
  // The argument type must either be a Builtin:: type (in which case
  // this is a type in the standard library) or some other type that itself has
  // a conversion function from a builtin type (in which case we have
  // "chaining", and an implicit conversion through that type).
  Expr *intermediate;
  BuiltinIntegerType *BIT;
  BuiltinFloatType *BFT;
  if (kind == LiteralKind::Int &&
      (BIT = argType->getAs<BuiltinIntegerType>())) {
    // If this is a direct use of the builtin integer type, use the integer size
    // to diagnose excess precision issues.
    llvm::APInt Value(1, 0);
    StringRef IntText = cast<IntegerLiteralExpr>(literal)->getText();
    unsigned Radix;
    if (IntText.startswith("0x")) {
      IntText = IntText.substr(2);
      Radix = 16;
    } else if (IntText.startswith("0o")) {
      IntText = IntText.substr(2);
      Radix = 8;
    } else if (IntText.startswith("0b")) {
      IntText = IntText.substr(2);
      Radix = 2;
    } else {
      Radix = 10;
    }
    bool Failure = IntText.getAsInteger(Radix, Value);
    assert(!Failure && "Lexer should have verified a reasonable type!");
    (void)Failure;

    if (Value.getActiveBits() > BIT->getBitWidth())
      tc.diagnose(literal->getLoc(), diag::int_literal_too_large,
                  Value.getBitWidth(), type);

    // Give the integer literal the builtin integer type.
    literal->setType(argType);
    intermediate = literal;
  }
  else if (kind == LiteralKind::Float &&
           (BFT = argType->getAs<BuiltinFloatType>())) {
    // If this is a direct use of a builtin floating point type, use the
    // floating point type to do the syntax verification.
    llvm::APFloat Val(BFT->getAPFloatSemantics());
    switch (Val.convertFromString(cast<FloatLiteralExpr>(literal)->getText(),
                                  llvm::APFloat::rmNearestTiesToEven)) {
      case llvm::APFloat::opOverflow: {
        llvm::SmallString<20> Buffer;
        llvm::APFloat::getLargest(Val.getSemantics()).toString(Buffer);
        tc.diagnose(literal->getLoc(), diag::float_literal_overflow, Buffer);
        break;
      }
      case llvm::APFloat::opUnderflow: {
        // Denormals are ok, but reported as underflow by APFloat.
        if (!Val.isZero()) break;
        llvm::SmallString<20> Buffer;
        llvm::APFloat::getSmallest(Val.getSemantics()).toString(Buffer);
        tc.diagnose(literal->getLoc(), diag::float_literal_underflow, Buffer);
        break;
      }
      default:
        break;
    }

    literal->setType(argType);
    intermediate = literal;
  } else if (isStringLiteralKind(kind) && argType->is<BuiltinRawPointerType>()){
    // Nothing to do.
    literal->setType(argType);
    intermediate = literal;
  } else if (isStringLiteralKind(kind) && isRawPtrAndInt64(argType)) {
    // Nothing to do.
    literal->setType(argType);
    intermediate = literal;
  } else if (kind == LiteralKind::Char &&
             argType->is<BuiltinIntegerType>() &&
             argType->getAs<BuiltinIntegerType>()->getBitWidth() == 32) {
    // Nothing to do.
    literal->setType(argType);
    intermediate = literal;
  } else {
    // Check to see if this is the chaining case, where ArgType itself has a
    // conversion from a Builtin type.
    FuncDecl *chainedMethod;
    Type chainedArgType;
    std::tie(chainedMethod, chainedArgType)
      = tc.isLiteralCompatibleType(argType, SourceLoc(), kind, false);
    assert(chainedMethod && "Literal type is not compatible?");

    if (kind == LiteralKind::Int &&
        chainedArgType->is<BuiltinIntegerType>()) {
      // ok.
    } else if (kind == LiteralKind::Float &&
               chainedArgType->is<BuiltinFloatType>()) {
      // ok.
    } else if (kind == LiteralKind::Char &&
               chainedArgType->is<BuiltinIntegerType>() &&
               chainedArgType->getAs<BuiltinIntegerType>()->getBitWidth() == 32) {
      // ok.

    } else if (isStringLiteralKind(kind) &&
               chainedArgType->is<BuiltinRawPointerType>()) {
      // ok.
    } else if (isStringLiteralKind(kind) && isRawPtrAndInt64(chainedArgType)) {
      // ok.
    } else {
      llvm_unreachable("Literal conversion defined improperly");
    }

    // If this a 'chaining' case, recursively convert the literal to the
    // intermediate type, then use our conversion function to finish the
    // translation.
    intermediate = convertLiteral(literal, argType, kind, argType);

    // Okay, now Intermediate is known to have type 'argType' so we can use a
    // call to our conversion function to finish things off.
  }

  Expr *result = new (tc.Context) MetatypeExpr(nullptr,
                                               intermediate->getStartLoc(),
                                               method->computeThisType());
  result = buildMemberRef(result, SourceLoc(), method,
                          intermediate->getStartLoc(),
                          openedType,
                          cs.getConstraintLocator(literal, { }));

  // Return a new call of the conversion function, passing in the (possibly
  // converted) argument.
  return new (tc.Context) CallExpr(result, intermediate, type);

}

Expr *ExprRewriter::finishApply(ApplyExpr *apply, Type openedType,
                                ConstraintLocatorBuilder locator) {
  TypeChecker &tc = cs.getTypeChecker();

  // The function is always an rvalue.
  // FIXME: Bring convertToRvalue into the application step.
  auto fn = tc.convertToRValue(apply->getFn());
  assert(fn && "Rvalue conversion failed?");
  if (!fn)
    return nullptr;
  apply->setFn(fn);

  // Check whether the argument is 'super'.
  bool isSuper = isa<SuperRefExpr>(apply->getArg());
  
  // For function application, convert the argument to the input type of
  // the function.
  if (auto fnType = fn->getType()->getAs<FunctionType>()) {
    auto origArg = apply->getArg();
    Expr *arg = nullptr;
    if (isa<ThisApplyExpr>(apply))
      arg = convertObjectArgumentToType(tc, origArg, fnType->getInput());
    else
      arg = coerceToType(origArg, fnType->getInput(), isAssignmentFn(fn),
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

    if (auto polyFn = apply->getType()->getAs<PolymorphicFunctionType>()) {
      return specialize(apply, polyFn, openedType);
    }

    return tc.substituteInputSugarTypeForResult(apply);
  }

  // We have a type constructor.
  auto metaTy = fn->getType()->castTo<MetaTypeType>();
  auto ty = metaTy->getInstanceType();

  // If we're "constructing" a tuple type, it's simply a conversion.
  if (auto tupleTy = ty->getAs<TupleType>()) {
    // FIXME: Need an AST to represent this properly.
    return coerceToType(apply->getArg(), tupleTy, /*isAssignment=*/false,
                        locator);
  }

  // We're constructing a struct or oneof. Look for the constructor or oneof
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
      selected->first.getKind() == OverloadChoiceKind::IdentityFunction) {
    // FIXME: Need an AST to represent this properly.
    return coerceToType(apply->getArg(), ty, /*isAssignment=*/false, locator);
  }

  // We have the constructor.
  auto choice = selected->first;
  auto decl = choice.getDecl();

  // Form a reference to the constructor or oneof declaration.
  Expr *typeBase = new (tc.Context) MetatypeExpr(nullptr, apply->getLoc(),
                                                 metaTy);
  Expr *declRef = buildMemberRef(typeBase, apply->getLoc(),
                                 decl, apply->getLoc(),
                                 selected->second, locator);
  apply->setFn(declRef);

  // Tail-recurse to actually call the constructor.
  return finishApply(apply, openedType, locator);
}

/// \brief Apply a given solution to the expression, producing a fully
/// type-checked expression.
Expr *ConstraintSystem::applySolution(const Solution &solution,
                                      Expr *expr) {

  class ExprWalker : public ASTWalker {
    ExprRewriter &Rewriter;

  public:
    ExprWalker(ExprRewriter &Rewriter) : Rewriter(Rewriter) { }

    virtual bool walkToExprPre(Expr *expr) {
      if (auto closure = dyn_cast<ExplicitClosureExpr>(expr)) {
        // Update the types of the $I variables with their simplified versions.
        // We do this before walking into the body of the closure expression,
        // so that the body will have proper types for its references to the
        // $I variables.
        auto &cs = Rewriter.getConstraintSystem();
        for (auto var : closure->getParserVarDecls())
          var->overwriteType(cs.simplifyType(var->getType()));

        return true;
      }

      // For an array, just walk the expression itself; its children have
      // already been type-checked.
      if (auto newArray = dyn_cast<NewArrayExpr>(expr)) {
        Rewriter.visitNewArrayExpr(newArray);
        return false;
      }

      // For a default-value expression, do nothing.
      if (isa<DefaultValueExpr>(expr)) {
        return false;
      }

      return true;
    }

    virtual Expr *walkToExprPost(Expr *expr) {
      return Rewriter.visit(expr);
    }

    /// \brief Ignore statements.
    virtual bool walkToStmtPre(Stmt *stmt) { return false; }

    /// \brief Ignore declarations.
    virtual bool walkToDeclPre(Decl *decl) { return false; }
  };

  ExprRewriter rewriter(*this, solution);
  ExprWalker walker(rewriter);
  return expr->walk(walker);
}

Expr *Solution::coerceToType(Expr *expr, Type toType, bool isAssignment) const {
  auto &cs = getConstraintSystem();
  ExprRewriter rewriter(cs, *this);
  return rewriter.coerceToType(expr, toType, isAssignment,
                               cs.getConstraintLocator(expr, { }));
}

Expr *Solution::coerceToType(Expr *expr, Type toType, bool isAssignment,
                             ConstraintLocator *locator) const {
  auto &cs = getConstraintSystem();
  ExprRewriter rewriter(cs, *this);
  return rewriter.coerceToType(expr, toType, isAssignment, locator);
}
