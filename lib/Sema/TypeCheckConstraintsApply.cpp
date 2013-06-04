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
#include "llvm/Support/SaveAndRestore.h"

using namespace swift;
using namespace constraints;

/// \brief Find a particular named function witness for a type that conforms to
/// the given protocol.
///
/// \param tc The type check we're using.
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
static FuncDecl *findNamedWitness(TypeChecker &tc, Type type,
                                  ProtocolDecl *proto,
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
  
  if (!requirement) {
    tc.diagnose(proto->getLoc(), diag);
    return nullptr;
  }

  // Find the member used to satisfy the named requirement.
  ProtocolConformance *conformance = 0;
  bool conforms = tc.conformsToProtocol(type, proto, &conformance);
  (void)conforms;
  assert(conforms && "Protocol conformance broken?");

  // For an archetype, just return the requirement from the protocol. There
  // are no protocol conformance tables.
  if (type->is<ArchetypeType>()) {
    return requirement;
  }

  assert(conformance && "Missing conformance information");
  assert(conformance->Mapping.count(requirement) && "Missing conformance");
  return cast<FuncDecl>(conformance->Mapping[requirement]);
}

/// \brief Perform the substitutions required to convert a given object type
/// to the object type required to access a specific member, producing the
/// archetype-to-replacement mappings and protocol conformance information
/// as a result.
///
/// \param tc The type checker we're using to perform the substitution.
/// \param member The member we will be accessing after performing the
/// conversion.
/// \param objectTy The type of object in which we want to access the member,
/// which will be a subtype of the member's context type.
/// \param otherTypes A set of types that will also be substituted, and modified
/// in place.
/// \param loc The location of this substitution.
/// \param substitutions Will be populated with the archetype-to-fixed type
/// mappings needed for the conversion.
/// \param conformances The protocol conformances required to perform the
/// conversion.
/// \param genericParams Will be set to the generic parameter list used for
/// substitution.
static void substForBaseConversion(TypeChecker &tc, ValueDecl *member,
                                   Type objectTy,
                                   MutableArrayRef<Type> otherTypes,
                                   SourceLoc loc,
                                   TypeSubstitutionMap &substitutions,
                                   ConformanceMap &conformances,
                                   GenericParamList *&genericParams);

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

  public:
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

      // Figure out the type of the container in which the member actually
      // resides.
      auto containerTy
        = member->getDeclContext()->getDeclaredTypeOfContext();

      // Member references into an archetype or existential type that resolves
      // to a protocol requirement.
      if (containerTy && containerTy->is<ProtocolType>() &&
          (baseTy->is<ArchetypeType>() || baseTy->isExistentialType())) {
        // For an archetype, just convert to the archetype itself.
        if (baseTy->is<ArchetypeType>())
          containerTy = baseTy;

        // Convert the base appropriately.
        if (baseIsInstance) {
          // Convert the base to the appropriate container type, turning it
          // into an lvalue if required.
          base = coerceObjectArgumentToType(
                   base, containerTy,
                   locator.withPathElement(ConstraintLocator::MemberRefBase));
        } else {
          // Convert the base to an rvalue of the appropriate metatype.
          base = coerceToType(base, MetaTypeType::get(containerTy, context),
                              locator.withPathElement(
                                ConstraintLocator::MemberRefBase));
          base = tc.coerceToRValue(base);
        }

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
      if (containerTy && containerTy->isUnspecializedGeneric()) {
        // Figure out the substitutions required to convert to the base.
        GenericParamList *genericParams = nullptr;
        TypeSubstitutionMap substitutions;
        ConformanceMap conformances;
        Type otherTypes[2] = {
          member->getTypeOfReference(),
          member->getDeclContext()->getDeclaredTypeInContext()
        };

        substForBaseConversion(tc, member, baseTy, otherTypes, memberLoc,
                               substitutions, conformances, genericParams);
        Type substTy = otherTypes[0];
        containerTy = otherTypes[1];

        // Convert the base appropriately.
        // FIXME: We could be referring to a member of a superclass, so find
        // that superclass and convert to it.
        if (baseIsInstance) {
          // Convert the base to the appropriate container type, turning it
          // into an lvalue if required.
          base = coerceObjectArgumentToType(
                   base, containerTy,
                   locator.withPathElement(ConstraintLocator::MemberRefBase));
        } else {
          // Convert the base to an rvalue of the appropriate metatype.
          base = coerceToType(base, MetaTypeType::get(containerTy, context),
                              locator.withPathElement(
                                ConstraintLocator::MemberRefBase));
          base = tc.coerceToRValue(base);
        }
        assert(base && "Unable to convert base?");

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
            = tc.buildSpecializeExpr(ref, substTy, substitutions,
                                     conformances,
                                     /*ArchetypesAreOpen=*/false,
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

        // Build a reference to a generic member.
        auto result = new (context) GenericMemberRefExpr(base, dotLoc, member,
                                                         memberLoc);

        // Set the (substituted) type and the set of substitutions.
        // FIXME: Use simplifyType(openedType);
        result->setType(substTy);
        result->setSubstitutions(tc.encodeSubstitutions(genericParams,
                                                        substitutions,
                                                        conformances,
                                                        false, false));
        return result;
      }

      // Reference to a variable within a class.
      if (auto var = dyn_cast<VarDecl>(member)) {
        if (!baseTy->is<ModuleType>()) {
          // Convert the base to the type of the 'this' parameter.
          assert(baseIsInstance && "Can only access variables of an instance");

          // Convert the base to the appropriate container type, turning it
          // into an lvalue if required.
          base = coerceObjectArgumentToType(base, containerTy, nullptr);

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
    /// \param type The literal type as it was opened in the type system.
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
      auto choice = selected.first;
      auto subscript = cast<SubscriptDecl>(choice.getDecl());

      auto &tc = cs.getTypeChecker();
      auto baseTy = base->getType()->getRValueType();

      // Figure out the index and result types.
      auto containerTy
        = subscript->getDeclContext()->getDeclaredTypeOfContext();
      auto subscriptTy = simplifyType(selected.second);
      auto indexTy = subscriptTy->castTo<AnyFunctionType>()->getInput();
      auto resultTy = subscriptTy->castTo<AnyFunctionType>()->getResult();

      // Coerce the index argument.
      index = coerceToType(index, indexTy,
                           locator.withPathElement(
                             ConstraintLocator::SubscriptIndex));
      if (!index)
        return nullptr;

      // Determine the result type of the subscript expression.
      resultTy = LValueType::get(resultTy->getRValueType(),
                                 LValueType::Qual::DefaultForMemberAccess,
                                 tc.Context);

      // Form the subscript expression.

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

      // Handle subscripting of generics.
      if (containerTy->isUnspecializedGeneric()) {
        // Compute the substitutions we need to apply for the generic subscript,
        // along with the base type of the subscript.
        GenericParamList *genericParams = nullptr;
        TypeSubstitutionMap substitutions;
        ConformanceMap conformances;
        containerTy = subscript->getDeclContext()->getDeclaredTypeInContext();
        substForBaseConversion(tc, subscript, baseTy, containerTy,
                               index->getStartLoc(), substitutions,
                               conformances, genericParams);

        // Coerce the base to the (substituted) container type.
        base = coerceObjectArgumentToType(base, containerTy, locator);
        if (!base)
          return nullptr;

        // Form the generic subscript expression.
        auto subscriptExpr = new (tc.Context) GenericSubscriptExpr(base, index,
                                                                   subscript);
        subscriptExpr->setType(resultTy);
        subscriptExpr->setSubstitutions(
          tc.encodeSubstitutions(genericParams, substitutions,
                                 conformances, false, false));
        return subscriptExpr;
      }

      // Coerce the base to the container type.
      base = coerceObjectArgumentToType(base, containerTy, locator);
      if (!base)
        return nullptr;

      // Handle subscripting of existential types.
      if (baseTy->isExistentialType()) {
        auto subscriptExpr
          = new (tc.Context) ExistentialSubscriptExpr(base, index, subscript);
        subscriptExpr->setType(resultTy);
        return subscriptExpr;
      }

      // Form a normal subscript.
      SubscriptExpr *subscriptExpr = new (tc.Context) SubscriptExpr(base,index);
      subscriptExpr->setType(resultTy);
      subscriptExpr->setDecl(subscript);
      return subscriptExpr;
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
      auto &tc = cs.getTypeChecker();
      ProtocolDecl *protocol
        = tc.getProtocol(KnownProtocolKind::FloatLiteralConvertible);
      ProtocolDecl *builtinProtocol
        = tc.getProtocol(KnownProtocolKind::BuiltinFloatLiteralConvertible);

      // For type-sugar reasons, prefer the spelling of the default literal
      // type.
      auto type = simplifyType(expr->getType());
      if (auto defaultType = tc.getDefaultLiteralType(LiteralKind::Float)) {
        if (defaultType->isEqual(type))
          type = defaultType;
      }

      return convertLiteral(expr,
                            type,
                            expr->getType(),
                            protocol,
                            tc.Context.getIdentifier("FloatLiteralType"),
                            tc.Context.getIdentifier("convertFromFloatLiteral"),
                            builtinProtocol,
                            tc.Context.getIdentifier("BuiltinFloatLiteralType"),
                            tc.Context.getIdentifier(
                              "_convertFromBuiltinFloatLiteral"),
                            [] (Type type) -> bool {
                              return type->is<BuiltinFloatType>();
                            },
                            diag::float_literal_broken_proto,
                            diag::builtin_float_literal_broken_proto);
    }

    Expr *visitCharacterLiteralExpr(CharacterLiteralExpr *expr) {
      auto &tc = cs.getTypeChecker();
      ProtocolDecl *protocol
        = tc.getProtocol(KnownProtocolKind::CharacterLiteralConvertible);
      ProtocolDecl *builtinProtocol
        = tc.getProtocol(KnownProtocolKind::BuiltinCharacterLiteralConvertible);

      // For type-sugar reasons, prefer the spelling of the default literal
      // type.
      auto type = simplifyType(expr->getType());
      if (auto defaultType = tc.getDefaultLiteralType(LiteralKind::Char)) {
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
               Type(BuiltinIntegerType::get(32, tc.Context)),
               tc.Context.getIdentifier("_convertFromBuiltinCharacterLiteral"),
               [] (Type type) -> bool {
                 if (auto builtinInt = type->getAs<BuiltinIntegerType>()) {
                   return builtinInt->getBitWidth() == 32;
                 }
                 return false;
               },
               diag::character_literal_broken_proto,
               diag::builtin_character_literal_broken_proto);
    }

    Expr *visitStringLiteralExpr(StringLiteralExpr *expr) {
      auto &tc = cs.getTypeChecker();
      ProtocolDecl *protocol
        = tc.getProtocol(KnownProtocolKind::StringLiteralConvertible);
      ProtocolDecl *builtinProtocol
        = tc.getProtocol(KnownProtocolKind::BuiltinStringLiteralConvertible);

      // For type-sugar reasons, prefer the spelling of the default literal
      // type.
      auto type = simplifyType(expr->getType());
      if (auto defaultType = tc.getDefaultLiteralType(LiteralKind::String)) {
        if (defaultType->isEqual(type))
          type = defaultType;
      }

      // FIXME: 32-bit platforms should use 32-bit size here?
      TupleTypeElt elements[3] = {
        TupleTypeElt(tc.Context.TheRawPointerType),
        TupleTypeElt(BuiltinIntegerType::get(64, tc.Context)),
        TupleTypeElt(BuiltinIntegerType::get(1, tc.Context))
      };
      return convertLiteral(
               expr,
               type,
               expr->getType(),
               protocol,
               tc.Context.getIdentifier("StringLiteralType"),
               tc.Context.getIdentifier("convertFromStringLiteral"),
               builtinProtocol,
               TupleType::get(elements, tc.Context),
               tc.Context.getIdentifier("_convertFromBuiltinStringLiteral"),
               nullptr,
               diag::string_literal_broken_proto,
               diag::builtin_string_literal_broken_proto);
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
        = tc.getProtocol(KnownProtocolKind::StringInterpolationConvertible);
      assert(interpolationProto && "Missing string interpolation protocol?");

      // FIXME: Cache name,
      auto name = tc.Context.getIdentifier("convertFromStringInterpolation");
      auto member = findNamedWitness(tc, type, interpolationProto, name,
                                     diag::interpolation_broken_proto);

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
      return simplifyExprType(expr);
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
          base = cs.getTypeChecker().coerceToMaterializable(base);
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
      return buildSubscript(expr->getBase(), expr->getIndex(),
                            cs.getConstraintLocator(expr, { }));
    }

    Expr *visitArrayExpr(ArrayExpr *expr) {
      Type openedType = expr->getType();
      Type arrayTy = simplifyType(openedType);
      auto &tc = cs.getTypeChecker();

      ProtocolDecl *arrayProto
        = tc.getProtocol(KnownProtocolKind::ArrayLiteralConvertible);
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
      auto &tc = cs.getTypeChecker();

      ProtocolDecl *dictionaryProto
        = tc.getProtocol(KnownProtocolKind::DictionaryLiteralConvertible);

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

    Expr *visitExistentialSubscriptExpr(ExistentialSubscriptExpr *expr) {
      return buildSubscript(expr->getBase(), expr->getIndex(),
                            cs.getConstraintLocator(expr, { }));
    }

    Expr *visitArchetypeSubscriptExpr(ArchetypeSubscriptExpr *expr) {
      return buildSubscript(expr->getBase(), expr->getIndex(),
                            cs.getConstraintLocator(expr, { }));
    }

    Expr *visitGenericSubscriptExpr(GenericSubscriptExpr *expr) {
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
      }

      llvm_unreachable("Unhandled pattern kind");
    }

    Expr *visitFuncExpr(FuncExpr *expr) {
      // FIXME: Type-check the function now? Or queue for later?
      simplifyExprType(expr);

      // Coerce the FuncExpr's pattern, in case we resolved something.
      Type input = expr->getType()->castTo<FunctionType>()->getInput();
      auto &tc = cs.getTypeChecker();
      if (tc.coerceToType(expr->getArgParamPatterns()[0], cs.DC, input))
        return nullptr;
      if (tc.coerceToType(expr->getBodyParamPatterns()[0], cs.DC, input))
        return nullptr;

      return expr;
    }

    Expr *visitPipeClosureExpr(PipeClosureExpr *expr) {
      simplifyExprType(expr);

      // Coerce the pattern, in case we resolved something.
      auto fnType = expr->getType()->castTo<FunctionType>();
      auto &tc = cs.getTypeChecker();
      if (tc.coerceToType(expr->getParams(), cs.DC, fnType->getInput()))
        return nullptr;

      // If this is a single-expression closure, convert the expression
      // in the body to the result type of the closure.
      if (expr->hasSingleExpressionBody()) {
        Expr *body = coerceToType(expr->getSingleExpressionBody(),
                                  fnType->getResult(),
                                  cs.getConstraintLocator(
                                    expr,
                                    ConstraintLocator::ClosureResult));
        if (!body)
          return nullptr;

        expr->setSingleExpressionBody(body);

        // Compute the capture list, now that we have analyzed the expression.
        tc.computeCaptures(expr);

        return expr;
      }

      // For other closures, type-check the body.
      tc.typeCheckClosureBody(expr);

      // Compute the capture list, now that we have type-checked the body.
      tc.computeCaptures(expr);
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
      Expr* injectionFn = tc.buildArrayInjectionFnRef(cs.DC, sliceType,
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
        base = tc.coerceToRValue(base);
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
      auto resultTy = simplifyType(expr->getType());
      expr->setType(resultTy);

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
                                   cs.getConstraintLocator(expr, { }));
      expr->setSubExpr(subExpr);
      return expr;
    }

    Expr *visitUncheckedDowncastExpr(UncheckedDowncastExpr *expr) {
      auto &tc = cs.getTypeChecker();
      auto &C = cs.getASTContext();

      // Simplify the type we're converting to.
      Type toType = simplifyType(expr->getType());
      expr->setType(toType);

      // Type-check the subexpression in isolation.
      Expr *sub = expr->getSubExpr();
      if (tc.typeCheckExpression(sub, cs.DC)) {
        // FIXME: Mark as error.
        return nullptr;
      }
      sub = tc.coerceToRValue(sub);
      if (!sub) {
        // FIXME: Mark as error.
        return nullptr;
      }
      expr->setSubExpr(sub);

      // A downcast is okay when:
      Type fromType = sub->getType();
      Type origFromType = fromType;
      bool toArchetype = toType->is<ArchetypeType>();
      bool fromArchetype = fromType->is<ArchetypeType>();

      // A downcast can:
      //   - convert an archetype to a (different) archetype type.
      if (fromArchetype && toArchetype) {
        // FIXME: We don't support this yet.
        tc.diagnose(expr->getLoc(), diag::downcast_archetype,
                    fromType, toType);
        return nullptr;
      }

      //   - convert an archetype to a subclass of its superclass type.
      if (fromArchetype) {
        // Introduce the archetype-to-super conversion.
        auto fromSuperType = fromType->castTo<ArchetypeType>()->getSuperclass();
        if (!fromSuperType) {
          // FIXME: We should be able to do this without the to-super
          // conversion. Specifically, we should be able to cast to any
          // concrete type that meets the requirements of the archetype.
          tc.diagnose(expr->getLoc(), diag::downcast_archetype_without_super,
                      fromType, toType);
          return nullptr;
        }

        sub = new (C) ArchetypeToSuperExpr(sub, fromSuperType);
        expr->setSubExpr(sub);
        fromArchetype = false;
        fromType = fromSuperType;
      }

      //   - convert from a superclass to an archetype.
      if (toArchetype) {
        auto toSuperType = toType->castTo<ArchetypeType>()->getSuperclass();
        if (!toSuperType) {
          // FIXME: We should be able to do this.
          tc.diagnose(expr->getLoc(), diag::downcast_archetype_without_super,
                      origFromType, toType);
          return nullptr;
        }

        // Coerce to the supertype of the archetype.
        if (tc.convertToType(sub, toSuperType, cs.DC))
          return nullptr;
        
        // The source type must be equivalent to or a supertype of the supertype
        // of the destination archetype.
        if (!tc.isSubtypeOf(toSuperType, fromType)) {
          // FIXME: Still too strong?
          tc.diagnose(expr->getLoc(), diag::downcast_not_class_cast,
                      origFromType, toType);
          return nullptr;
        }

        // Construct the supertype-to-archetype cast.
        auto *stoa = new (C) UncheckedSuperToArchetypeExpr(sub,
                                                           expr->getLoc(),
                                                           expr->getBangLoc(),
                                                           expr->getTypeLoc());
        stoa->setType(expr->getType());
        return stoa;
      }

      assert(!fromArchetype && "archetypes should have been handled above");
      assert(!toArchetype && "archetypes should have been handled above");

      // If the from/to types are equivalent, this should have been a
      // coercion expression (b as A) rather than a cast (a as! B). Complain.
      if (fromType->isEqual(toType) || tc.isSubtypeOf(fromType, toType)) {
        // Only complain if the cast itself was implicitly generated.
        // FIXME: This leniency is here for the Clang module importer,
        // which doesn't necessarily know whether it needs to force the
        // cast or not. instancetype should eliminate the need for it.
        if (!expr->isImplicit()) {
          tc.diagnose(expr->getLoc(), diag::downcast_to_supertype,
                      origFromType, toType)
            .highlight(sub->getSourceRange())
            .highlight(expr->getTypeLoc().getSourceRange())
            .fixItRemove(SourceRange(expr->getBangLoc()));
        }

        Expr *coerce = new (C) CoerceExpr(sub, expr->getLoc(),
                                          expr->getTypeLoc());
        coerce->setType(expr->getType());
        return coerce;
      }

      // The destination type must be a subtype of the source type.
      if (!tc.isSubtypeOf(toType, fromType)) {
        tc.diagnose(expr->getLoc(), diag::downcast_to_unrelated,
                    origFromType, toType)
          .highlight(sub->getSourceRange())
          .highlight(expr->getTypeLoc().getSourceRange());
        return nullptr;
      }

      // Okay, we're done.
      return expr;
    }
    
    Expr *visitUncheckedSuperToArchetypeExpr(
            UncheckedSuperToArchetypeExpr *expr) {
      llvm_unreachable("Already type-checked");
    }
    
    Expr *visitIsSubtypeExpr(IsSubtypeExpr *expr) {
      expr->setType(simplifyType(expr->getType()));
      
      auto &tc = cs.getTypeChecker();
      Expr *sub = tc.coerceToRValue(expr->getSubExpr());
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
    injectionFn = tc.buildArrayInjectionFnRef(cs.DC,
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
    injectionFn = tc.buildArrayInjectionFnRef(cs.DC,
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

Expr *ExprRewriter::coerceToType(Expr *expr, Type toType,
                                 ConstraintLocatorBuilder locator) {
  auto &tc = cs.getTypeChecker();

  // The type we're converting from.
  Type fromType = expr->getType();

  // If the types are already equivalent, we don't have to do anything.
  if (fromType->isEqual(toType))
    return expr;

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

    // Coerce the result.
    return coerceToType(expr, toType, locator);
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
      bool trivial = false;
      bool isConvertible = cs.isConvertibleTo(fromType, toType, trivial);
      (void)isConvertible;
      assert(isConvertible && "No conversion between function types?");
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

  // Coerce via conversion function or constructor.
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
                                              /*hasTrailingClosure=*/false,
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

      // If we chose the identity constructor, coerce to the expected type
      // based on the application argument locator.
      if (selected.first.getKind() == OverloadChoiceKind::IdentityFunction) {
        return coerceToType(expr, toType,
                            locator.withPathElement(
                              ConstraintLocator::ApplyArgument));
      }

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

    return coerceToType(expr, toType, locator);
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
      return new (tc.Context) RequalifyExpr(expr, destType);

    // If the object types are different, coerce to the container type.
    expr = coerceToType(expr, containerType, locator);

    // Fall through to materialize.
  }

  // If the source is not an lvalue, materialize it.
  return new (tc.Context) MaterializeExpr(expr, destType);
}

Expr *ExprRewriter::convertLiteral(Expr *literal, Type type, LiteralKind kind,
                                   Type openedType) {
  assert(kind != LiteralKind::Float);
  
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
  if (defaultLiteralTy && type->isEqual(defaultLiteralTy))
    type = defaultLiteralTy;
  // Additionally, if an integer literal gets used as the default floating-point
  // type, use the default floating-point literal type as the sugared type.
  else if (kind == LiteralKind::Int) {
    Type defaultFloatLiteralTy = tc.getDefaultLiteralType(LiteralKind::Float);
    if (defaultFloatLiteralTy && type->isEqual(defaultFloatLiteralTy))
      type = defaultFloatLiteralTy;
  }
  
  // The argument type must either be a Builtin:: type (in which case
  // this is a type in the standard library) or some other type that itself has
  // a conversion function from a builtin type (in which case we have
  // "chaining", and an implicit conversion through that type).
  Expr *intermediate;
  BuiltinIntegerType *BIT;
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
  if (tc.conformsToProtocol(type, builtinProtocol, &builtinConformance)) {
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
    Expr *result = tc.callWitness(base, cs.DC,
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
  bool conforms = tc.conformsToProtocol(type, protocol, &conformance);
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
  literal = tc.callWitness(base, cs.DC,
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
  bool isSuper = isa<SuperRefExpr>(apply->getArg());
  
  // For function application, convert the argument to the input type of
  // the function.
  if (auto fnType = fn->getType()->getAs<FunctionType>()) {
    auto origArg = apply->getArg();
    Expr *arg = nullptr;
    if (isa<ThisApplyExpr>(apply))
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
    return coerceToType(apply->getArg(), tupleTy, locator);
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
    return coerceToType(apply->getArg(), ty, locator);
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

void substForBaseConversion(TypeChecker &tc, ValueDecl *member,
                            Type objectTy,
                            MutableArrayRef<Type> otherTypes,
                            SourceLoc loc,
                            TypeSubstitutionMap &substitutions,
                            ConformanceMap &conformances,
                            GenericParamList *&genericParams) {
  ConstraintSystem cs(tc, nullptr);

  // The archetypes that have been opened up and replaced with type variables.
  llvm::DenseMap<ArchetypeType *, TypeVariableType *> replacements;

  // Open up the owning context of the member.
  Type ownerTy = cs.openTypeOfContext(member->getDeclContext(), replacements,
                                      &genericParams);

  // The base type of the member access needs to be convertible to the
  // opened type of the member's context.
  cs.addConstraint(ConstraintKind::Conversion, objectTy, ownerTy);

  // Solve the constraint system.
  llvm::SmallVector<Solution, 1> solutions;
  bool failed = cs.solve(solutions);
  (void)failed;
  assert(!failed && "Solution failed");
  assert(solutions.size() == 1 && "Multiple solutions?");

  // Fill in the set of substitutions.
  auto &solution = solutions.front();
  for (auto replacement : replacements) {
    substitutions[replacement.first]
      = solution.simplifyType(tc, replacement.second);
  }

  // Finalize the set of protocol conformances.
  failed = tc.checkSubstitutions(substitutions, conformances, loc,
                                 &substitutions);
  assert(!failed && "Substitutions cannot fail?");

  // Substitute all of the 'other' types with the substitutions we computed.
  for (auto &otherType : otherTypes) {
    // Replace the already-opened archetypes in the requested "other" type with
    // their replacements.
    otherType = tc.substType(otherType, substitutions);

    // If we have a polymorphic function type for which all of the generic
    // parameters have been replaced, make it monomorphic.
    // FIXME: Arguably, this should be part of substType
    if (auto polyFn = otherType->getAs<PolymorphicFunctionType>()) {
      bool allReplaced = true;
      for (auto gp : polyFn->getGenericParams().getParams()) {
        auto archetype
        = gp.getAsTypeParam()->getDeclaredType()->castTo<ArchetypeType>();
        if (!substitutions.count(archetype)) {
          allReplaced = false;
          break;
        }
      }

      if (allReplaced) {
        otherType = FunctionType::get(polyFn->getInput(), polyFn->getResult(),
                                      tc.Context);
      }
    }

    // Validate this type.
    tc.validateTypeSimple(otherType);
  }
}

/// \brief Apply a given solution to the expression, producing a fully
/// type-checked expression.
Expr *ConstraintSystem::applySolution(const Solution &solution,
                                      Expr *expr) {

  class ExprWalker : public ASTWalker {
    ExprRewriter &Rewriter;

  public:
    ExprWalker(ExprRewriter &Rewriter) : Rewriter(Rewriter) { }

    virtual std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      // For an array, just walk the expression itself; its children have
      // already been type-checked.
      if (auto newArray = dyn_cast<NewArrayExpr>(expr)) {
        Rewriter.visitNewArrayExpr(newArray);
        return { false, expr };
      }

      // For ternary expressions, visit the then and else branches;
      // the condition was checked separately.
      if (auto ifExpr = dyn_cast<IfExpr>(expr)) {
        // FIXME: Record failures.
        if (auto thenExpr = ifExpr->getThenExpr()->walk(*this)) {
          ifExpr->setThenExpr(thenExpr);
        }

        if (auto elseExpr = ifExpr->getElseExpr()->walk(*this)) {
          ifExpr->setElseExpr(elseExpr);
        }

        Rewriter.visitIfExpr(ifExpr);
        return { false, expr };
      }

      // For unchecked downcast expressions, the subexpression is checked
      // separately.
      if (auto unchecked = dyn_cast<UncheckedDowncastExpr>(expr)) {
        return { false, Rewriter.visitUncheckedDowncastExpr(unchecked) };
      }

      // For a default-value expression, do nothing.
      if (isa<DefaultValueExpr>(expr)) {
        return { false, expr };
      }

      return { true, expr };
    }

    virtual Expr *walkToExprPost(Expr *expr) {
      return Rewriter.visit(expr);
    }

    /// \brief Ignore statements.
    virtual std::pair<bool, Stmt *> walkToStmtPre(Stmt *stmt) {
      return { false, stmt };
    }

    /// \brief Ignore declarations.
    virtual bool walkToDeclPre(Decl *decl) { return false; }
  };

  ExprRewriter rewriter(*this, solution);
  ExprWalker walker(rewriter);
  return expr->walk(walker);
}

Expr *ConstraintSystem::applySolutionShallow(const Solution &solution,
                                             Expr *expr) {
  ExprRewriter rewriter(*this, solution);
  return rewriter.visit(expr);
}

Expr *Solution::coerceToType(Expr *expr, Type toType) const {
  auto &cs = getConstraintSystem();
  ExprRewriter rewriter(cs, *this);
  return rewriter.coerceToType(expr, toType,
                               cs.getConstraintLocator(expr, { }));
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
  Solution solution(cs);
  ExprRewriter rewriter(cs, solution);

  // Find the witness we need to use.
  auto type = base->getType();
  if (auto metaType = type->getAs<MetaTypeType>())
    type = metaType->getInstanceType();
  
  auto witness = findNamedWitness(*this, type->getRValueType(), protocol,
                                  name, brokenProtocolDiag);
  if (!witness)
    return nullptr;

  // FIXME: Could be smarter here w.r.t. generics, e.g., by opening up the
  // type appropriately and 
  auto openedType
    = witness->getType()->castTo<AnyFunctionType>()->getResult();

  auto locator = cs.getConstraintLocator(base, { });
  auto memberRef = rewriter.buildMemberRef(base, base->getStartLoc(),
                                           witness, base->getEndLoc(),
                                           openedType, locator);

  // Call the witness.
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
                                  TupleType::get(elementTypes, Context));
  }
  
  ApplyExpr *apply = new (Context) CallExpr(memberRef, arg);
  return rewriter.finishApply(apply, openedType, locator);
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
  auto type = expr->getType();

  // Look for the builtin name. If we don't have it, we need to call the
  // general name via the witness table.
  MemberLookup lookup(type->getRValueType(), builtinName, tc.TU);
  if (!lookup.isSuccess()) {
    // Find the witness we need to use.
    auto witness = findNamedWitness(tc, type->getRValueType(), protocol,
                                    generalName, brokenProtocolDiag);

    // Form a reference to the general name.
    // FIXME: openedType won't capture generics. The protocol definition
    // prevents this, but it feels hacky.
    auto openedType
      = witness->getType()->castTo<AnyFunctionType>()->getResult();
    auto memberRef = rewriter.buildMemberRef(expr, expr->getStartLoc(),
                                             witness, expr->getEndLoc(),
                                             openedType, locator);

    // Call the witness.
    Expr *arg = new (tc.Context) TupleExpr(expr->getStartLoc(),
                                           { }, nullptr,
                                           expr->getEndLoc(),
                                           /*hasTrailingClosure=*/false,
                                           TupleType::getEmpty(tc.Context));
    ApplyExpr *apply = new (tc.Context) CallExpr(memberRef, arg);
    expr = rewriter.finishApply(apply, openedType, locator);

    // At this point, we must have a type with the builtin member.
    type = expr->getType();
    lookup = MemberLookup(type->getRValueType(), builtinName, tc.TU);
    if (!lookup.isSuccess()) {
      tc.diagnose(protocol->getLoc(), brokenProtocolDiag);
      return nullptr;
    }
  }

  // Find the builtin method.
  if (lookup.Results.size() != 1) {
    tc.diagnose(protocol->getLoc(), brokenBuiltinDiag);
    return nullptr;
  }
  FuncDecl *builtinMethod = dyn_cast<FuncDecl>(lookup.Results[0].D);
  if (!builtinMethod) {
    tc.diagnose(protocol->getLoc(), brokenBuiltinDiag);
    return nullptr;

  }

  // Form a reference to the builtin method.
  auto openedType
    = builtinMethod->getType()->castTo<AnyFunctionType>()->getResult();
  auto memberRef = rewriter.buildMemberRef(expr, expr->getStartLoc(),
                                           builtinMethod, expr->getEndLoc(),
                                           openedType, locator);

  // Call the builtin method.
  Expr *arg = new (tc.Context) TupleExpr(expr->getStartLoc(),
                                         { }, nullptr,
                                         expr->getEndLoc(),
                                         /*hasTrailingClosure=*/false,
                                         TupleType::getEmpty(tc.Context));
  ApplyExpr *apply = new (tc.Context) CallExpr(memberRef, arg);
  return rewriter.finishApply(apply, openedType, locator);
}

/// \brief Determine whether the given type is a Builtin i1.
static bool isBuiltinI1(Type type) {
  if (auto builtinIntTy = type->getAs<BuiltinIntegerType>()) {
    return builtinIntTy->getBitWidth() == 1;
  }
  return false;
}

Expr *
Solution::convertToLogicValue(Expr *expr, ConstraintLocator *locator) const {
  auto &tc = getConstraintSystem().getTypeChecker();
  // FIXME: Cache names.
  auto result = convertViaBuiltinProtocol(
                  *this, expr, locator,
                  tc.getProtocol(KnownProtocolKind::LogicValue),
                  tc.Context.getIdentifier("getLogicValue"),
                  tc.Context.getIdentifier("_getBuiltinLogicValue"),
                  diag::condition_broken_proto,
                  diag::broken_bool);
  if (result && !isBuiltinI1(result->getType())) {
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
                  tc.getProtocol(KnownProtocolKind::ArrayBound),
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

