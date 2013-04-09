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

Expr *constraints::convertToType(TypeChecker &tc, Expr *expr, Type toType,
                                 bool isAssignment) {
  // FIXME: Temporary hack that uses the existing coercion logic.
  return tc.coerceToType(expr, toType,
                         isAssignment? CoercionKind::Assignment
                                     : CoercionKind::Normal);
}

/// \brief Convert the object argument to the given type.
static Expr *convertObjectArgumentToType(TypeChecker &tc, Expr *expr,
                                         Type toType) {
  // FIXME: Temporary hack that uses the existing coercion logic.
  return tc.coerceObjectArgument(expr, toType);
}

/// \brief Determine whether the given expression refers to an assignment
/// function.
static bool isAssignmentFn(Expr *expr) {
  expr = expr->getSemanticsProvidingExpr();
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

/// \brief Convert the given literal expression to the specified literal kind.
static Expr *convertLiteral(TypeChecker &tc, Expr *literal,
                            Type type, LiteralKind kind) {
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
    intermediate = convertLiteral(tc, literal, argType, kind);

    // Okay, now Intermediate is known to have type 'argType' so we can use a
    // call to our conversion function to finish things off.
  }

  Expr *result = new (tc.Context) MetatypeExpr(nullptr,
                                            intermediate->getStartLoc(),
                                            method->computeThisType());
  result = tc.recheckTypes(tc.buildMemberRefExpr(result, SourceLoc(), method,
                                                 intermediate->getStartLoc()));

  // Return a new call of the conversion function, passing in the (possible
  // converted) argument.
  return new (tc.Context) CallExpr(result, intermediate, type);

}

/// \brief Apply a given solution to the expression, producing a fully
/// type-checked expression.
Expr *ConstraintSystem::applySolution(const Solution &solution,
                                      Expr *expr) {
  class ExprRewriter : public ExprVisitor<ExprRewriter, Expr *> {
    ConstraintSystem &cs;
    const Solution &solution;

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

                return SubstitutedType::get(archetype, simplified,
                                           tc.Context);
              }

              return type;
            });

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
                         SourceLoc memberLoc, Type openedType) {
      // FIXME: Falls back to the old type checker.

      auto &tc = cs.getTypeChecker();
      auto result = tc.buildMemberRefExpr(base, dotLoc, { &member, 1 },
                                          memberLoc);
      if (!result)
        return nullptr;

      result = tc.recheckTypes(result);
      if (!result)
        return nullptr;

      if (auto polyFn = result->getType()->getAs<PolymorphicFunctionType>()) {
        return specialize(result, polyFn, openedType);
      }

      return result;
    }

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
      auto known = solution.overloadChoices.find(locator);
      assert(known != solution.overloadChoices.end());
      return known->second;
    }

    /// \brief Simplify the given type by substituting all occurrences of
    /// type variables for their fixed types.
    Type simplifyType(Type type) {
      return solution.simplifyType(cs.getTypeChecker(), type);
    }

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
                                   SourceLoc nameLoc, Type openedType) {
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
      return buildMemberRef(base, SourceLoc(), value, nameLoc, openedType);
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
      auto &tc = cs.getTypeChecker();
      return convertLiteral(tc, expr, simplifyType(expr->getType()),
                            LiteralKind::Int);
    }

    Expr *visitFloatLiteralExpr(FloatLiteralExpr *expr) {
      auto &tc = cs.getTypeChecker();
      return convertLiteral(tc, expr, simplifyType(expr->getType()),
                            LiteralKind::Float);
    }

    Expr *visitCharacterLiteralExpr(CharacterLiteralExpr *expr) {
      auto &tc = cs.getTypeChecker();
      return convertLiteral(tc, expr, simplifyType(expr->getType()),
                            LiteralKind::Char);
    }

    Expr *visitStringLiteralExpr(StringLiteralExpr *expr) {
      auto &tc = cs.getTypeChecker();

      // FIXME: Already did this when generating constraints.
      auto kind = LiteralKind::ASCIIString;
      for (unsigned char c : expr->getValue()) {
        if (c > 127) {
          kind = LiteralKind::UTFString;
          break;
        }
      }

      return convertLiteral(tc, expr, simplifyType(expr->getType()), kind);
    }

    Expr *
    visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *expr) {
      // FIXME: The existing literal coercion code should move here.
      auto type = simplifyType(expr->getType());
      expr->setType(UnstructuredUnresolvedType::get(cs.getASTContext()));
      return convertToType(cs.getTypeChecker(), expr, type);
    }

    // FIXME: Add specific entries for the various literal expressions.

    Expr *visitDeclRefExpr(DeclRefExpr *expr) {
      auto fromType = expr->getType();

      if (auto proto
            = dyn_cast<ProtocolDecl>(expr->getDecl()->getDeclContext())) {
        // If this a member of a protocol, build an appropriate operator
        // reference.
        return buildProtocolOperatorRef(proto, expr->getDecl(), expr->getLoc(),
                                        fromType);
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
                          ConstraintLocator::ConstructorMember)).first;
      auto *ctor = cast<ConstructorDecl>(selected.getDecl());
      // Build a call to the initializer for the constructor.
      auto *ctorRef
        = new (cs.getASTContext()) OtherConstructorDeclRefExpr(ctor,
                                                 expr->getConstructorLoc(),
                                                 ctor->getInitializerType());
      auto *call
        = new (cs.getASTContext()) DotSyntaxCallExpr(ctorRef,
                                                     expr->getDotLoc(),
                                                     expr->getSubExpr());
      auto &tc = cs.getTypeChecker();
      return tc.semaApplyExpr(call);
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
                                        selected.second);
      }

      // Normal path: build a declaration reference.
      auto result = new (context) DeclRefExpr(decl, expr->getLoc(),
                                              decl->getTypeOfReference());

      // For a polymorphic function type, we have to specialize our reference.
      if (auto polyFn = expr->getType()->getAs<PolymorphicFunctionType>()) {
        assert(false);
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
                            selected.second);
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
      // FIXME: Falls back to the old type checker.
      return cs.getTypeChecker().recheckTypes(expr);
    }
    
    Expr *visitExistentialMemberRefExpr(ExistentialMemberRefExpr *expr) {
      // FIXME: Falls back to the old type checker.
      
      llvm_unreachable("Already type-checked");
    }

    Expr *visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *expr) {
      auto selected = getOverloadChoice(
                        cs.getConstraintLocator(expr,
                                                ConstraintLocator::Member));
      return buildMemberRef(expr->getBase(), expr->getDotLoc(),
                            selected.first.getDecl(), expr->getNameLoc(),
                            selected.second);
    }

    Expr *visitGenericMemberRefExpr(GenericMemberRefExpr *expr) {
      auto selected = getOverloadChoice(
                        cs.getConstraintLocator(expr,
                                                ConstraintLocator::Member));
      return buildMemberRef(expr->getBase(), expr->getDotLoc(),
                            selected.first.getDecl(), expr->getNameLoc(),
                            selected.second);
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

      // The base expression is simply the metatype of a oneof type.
      auto base = new (tc.Context) MetatypeExpr(nullptr,
                                                expr->getDotLoc(),
                                                oneofMetaTy);

      // Find the member and build a reference to it.
      // FIXME: Redundant member lookup.
      MemberLookup &lookup = cs.lookupMember(oneofMetaTy, expr->getName());
      assert(lookup.isSuccess() && "Failed lookup?");
      auto member = lookup.Results[0].D;
      auto result = tc.buildMemberRefExpr(base, expr->getDotLoc(),
                                          ArrayRef<ValueDecl *>(&member, 1),
                                          expr->getNameLoc());
      return tc.recheckTypes(result);
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
                              selected.second);

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
      return simplifyExprType(expr);
    }

    Expr *visitTupleExpr(TupleExpr *expr) {
      return simplifyExprType(expr);
    }

    Expr *visitSubscriptExpr(SubscriptExpr *expr) {
      return buildSubscript(expr, expr->getBase(), expr->getIndex());
    }
    
    Expr *visitArrayExpr(ArrayExpr *expr) {
      simplifyExprType(expr);
      Type arrayTy = expr->getType();
      
      ProtocolDecl *arrayProto = cs.getTypeChecker().getArrayLiteralProtocol();
      assert(arrayProto && "type-checked array literal w/o protocol?!");
      
      /* FIXME: Ideally we'd use the protocol conformance as below, but
       * generic types (an in particular, T[]) can't be checked for conformance
       * yet <rdar://problem/13153805>
       *
       
      // Get the ArrayLiteralConvertible conformance.
      ProtocolConformance *conformance = nullptr;
      if (!CS.TC.conformsToProtocol(arrayTy, arrayProto, &conformance,
                                    expr->getLoc())) {
        // FIXME: This shouldn't happen at all when we solve the system using
        // a proper protocol constraint.
        CS.TC.diagnose(arrayProto->getLoc(), diag::array_protocol_broken);
        return nullptr;
      }
      
      // Find the convertFromArrayLiteral witness.
      FuncDecl *converterDecl;
      for (auto member : arrayProto->getMembers()) {
        auto func = dyn_cast<FuncDecl>(member);
        if (!func) continue;
        
        if (func->getName().str().equals("convertFromArrayLiteral")) {
          converterDecl = cast<FuncDecl>(conformance->Mapping[func]);
          break;
        }
      }
      
      if (!converterDecl) {
        CS.TC.diagnose(arrayProto->getLoc(), diag::array_protocol_broken);
        return nullptr;
      }
      
      if (!converterDecl->isStatic()) {
        CS.TC.diagnose(arrayProto->getLoc(), diag::array_protocol_broken);
        return nullptr;
      }
       
       *
       * For the time being, use a value member constraint to find the
       * appropriate convertFromArrayLiteral call.
       */
      auto choice = getOverloadChoice(
                      cs.getConstraintLocator(
                        expr,
                        ConstraintLocator::MemberRefBase)).first;
      auto converterDecl = cast<FuncDecl>(choice.getDecl());
      
      // Construct the semantic expr as a convertFromArrayLiteral application.
      ASTContext &C = cs.getASTContext();
      Expr *converterRef = new (C) DeclRefExpr(converterDecl, expr->getLoc(),
                                           converterDecl->getTypeOfReference());
      Expr *typeRef = new (C) MetatypeExpr(nullptr,
                                           expr->getLoc(),
                                           MetaTypeType::get(arrayTy, C));
      
      Expr *semanticExpr = new (C) DotSyntaxCallExpr(converterRef,
                                                     expr->getLoc(),
                                                     typeRef);
      semanticExpr = cs.getTypeChecker().recheckTypes(semanticExpr);
      
      semanticExpr = new (C) CallExpr(semanticExpr,
                                      expr->getSubExpr());

      semanticExpr = cs.getTypeChecker().recheckTypes(semanticExpr);
      
      if (!semanticExpr) {
        cs.getTypeChecker().diagnose(arrayProto->getLoc(),
                                     diag::array_protocol_broken);
        return nullptr;
      }
      
      expr->setSemanticExpr(semanticExpr);
      return expr;
    }

    Expr *visitDictionaryExpr(DictionaryExpr *expr) {
      simplifyExprType(expr);
      Type dictionaryTy = expr->getType();
      
      ProtocolDecl *dictionaryProto
        = cs.getTypeChecker().getDictionaryLiteralProtocol();
      assert(dictionaryProto && "type-checked dictionary literal w/o protocol?");
      
      /* FIXME: Ideally we'd use the protocol conformance as below, but
       * generic types (an in particular, T[]) can't be checked for conformance
       * yet <rdar://problem/13153805>
       *
       *
       * For the time being, use a value member constraint to find the
       * appropriate convertFromDictionaryLiteral call.
       */
      auto choice = getOverloadChoice(
                      cs.getConstraintLocator(
                        expr,
                        ConstraintLocator::MemberRefBase)).first;
      auto converterDecl = cast<FuncDecl>(choice.getDecl());
      
      // Construct the semantic expr as a convertFromDictionaryLiteral
      // application.
      ASTContext &C = cs.getASTContext();
      Expr *converterRef = new (C) DeclRefExpr(converterDecl, expr->getLoc(),
                                           converterDecl->getTypeOfReference());
      Expr *typeRef = new (C) MetatypeExpr(nullptr,
                                           expr->getLoc(),
                                           MetaTypeType::get(dictionaryTy, C));
      
      Expr *semanticExpr = new (C) DotSyntaxCallExpr(converterRef,
                                                     expr->getLoc(),
                                                     typeRef);
      semanticExpr = cs.getTypeChecker().recheckTypes(semanticExpr);
      
      semanticExpr = new (C) CallExpr(semanticExpr,
                                      expr->getSubExpr());

      semanticExpr = cs.getTypeChecker().recheckTypes(semanticExpr);
      
      if (!semanticExpr) {
        cs.getTypeChecker().diagnose(dictionaryProto->getLoc(),
                                     diag::dictionary_protocol_broken);
        return nullptr;
      }
      
      expr->setSemanticExpr(semanticExpr);
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
      if (Expr *body = convertToType(cs.getTypeChecker(), expr->getBody(),
                                     resultType))
        expr->setBody(body);
      expr->setType(type);

      // Compute the capture list, now that we have analyzed the expression.
      expr->computeCaptures(cs.getASTContext());

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
    
    Expr *visitApplyExpr(ApplyExpr *expr) {
      auto &tc = cs.getTypeChecker();

      // The function is always an rvalue.
      auto fn = tc.convertToRValue(expr->getFn());
      if (!fn)
        return nullptr;
      expr->setFn(fn);
      
      // For function application, convert the argument to the input type of
      // the function.
      if (auto fnType = fn->getType()->getAs<FunctionType>()) {
        auto origArg = expr->getArg();
        Expr *arg = nullptr;
        if (isa<DotSyntaxCallExpr>(expr))
          arg = convertObjectArgumentToType(tc, origArg, fnType->getInput());
        else
          arg = convertToType(tc, origArg, fnType->getInput(),
                              isAssignmentFn(fn));

        if (!arg) {
          tc.diagnose(fn->getLoc(), diag::while_converting_function_argument,
                      fnType->getInput())
            << origArg->getSourceRange();

          return nullptr;
        }

        auto origType = expr->getType();
        expr->setArg(arg);
        expr->setType(fnType->getResult());

        if (auto polyFn = expr->getType()->getAs<PolymorphicFunctionType>()) {
          return specialize(expr, polyFn, origType);
        }

        return tc.substituteInputSugarTypeForResult(expr);
      }

      // FIXME: Implement support for metatypes here.
      return tc.semaApplyExpr(expr);
    }
    
    Expr *visitNewReferenceExpr(NewReferenceExpr *expr) {
      auto instanceTy = simplifyType(expr->getType());
      expr->setType(instanceTy);

      // Find the constructor we selected for this expression.
      auto choice = getOverloadChoice(
                      cs.getConstraintLocator(
                        expr,
                        ConstraintLocator::ConstructorMember)).first;
      auto constructor = cast<ConstructorDecl>(choice.getDecl());

      // Form the constructor call expression.
      auto &tc = cs.getTypeChecker();
      auto classMetaTy = MetaTypeType::get(instanceTy, tc.Context);
      Expr *typeBase = new (tc.Context) MetatypeExpr(nullptr, expr->getLoc(),
                                                     classMetaTy);
      Expr *ctorRef = new (tc.Context) DeclRefExpr(constructor, expr->getLoc(),
                                         constructor->getTypeOfReference());
      ctorRef = new (tc.Context) ConstructorRefCallExpr(ctorRef, typeBase);

      // Extract (or create) the argument;
      Expr *arg = expr->getArg();
      if (!arg) {
        arg = new (tc.Context) TupleExpr(expr->getLoc(),
                                         MutableArrayRef<Expr *>(), nullptr,
                                         expr->getLoc());
        arg->setType(TupleType::getEmpty(tc.Context));
      }

      // FIXME: We need to know what happened to the constraints on the function
      // and on the argument to perform the appropriate conversions,
      // specializations, etc. For now, fall back to the existing type checker.
      ctorRef = tc.recheckTypes(ctorRef);
      expr->setFn(ctorRef);
      expr->setArg(arg);
      return tc.semaApplyExpr(expr);
    }

    // FIXME: Other subclasses of ApplyExpr?
    
    Expr *visitRebindThisInConstructorExpr(RebindThisInConstructorExpr *expr) {
      return expr;
    }
    
    Expr *visitIfExpr(IfExpr *expr) {
      Expr *condExpr = expr->getCondExpr();
      if (cs.getTypeChecker().typeCheckCondition(condExpr))
        return nullptr;
      expr->setCondExpr(condExpr);

      auto resultTy = simplifyType(expr->getType());
      expr->setType(resultTy);

      expr->setThenExpr(convertToType(cs.getTypeChecker(),
                                      expr->getThenExpr(),
                                      resultTy));
      expr->setElseExpr(convertToType(cs.getTypeChecker(),
                                      expr->getElseExpr(),
                                      resultTy));
      
      return expr;
    }

    Expr *visitImplicitConversionExpr(ImplicitConversionExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Expr *visitCoerceExpr(CoerceExpr *expr) {
      expr->setType(simplifyType(expr->getType()));
      Expr *subExpr = convertToType(cs.getTypeChecker(),
                                    expr->getSubExpr(),
                                    expr->getType());
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
      auto &C = cs.getASTContext();

      expr->setType(simplifyType(expr->getType()));
      
      Expr *sub = cs.getTypeChecker().convertToRValue(expr->getSubExpr());
      if (!sub) return nullptr;
      expr->setSubExpr(sub);
      
      // If the destination type is an archetype, this is a super-is-archetype
      // query.
      if (expr->getTypeLoc().getType()->is<ArchetypeType>()) {
        auto *sisa = new (C) SuperIsArchetypeExpr(expr->getSubExpr(),
                                                  expr->getLoc(),
                                                  expr->getTypeLoc());
        sisa->setType(expr->getType());
        return sisa;
      }
      
      return expr;
    }
    
    Expr *visitSuperIsArchetypeExpr(SuperIsArchetypeExpr *expr) {
      llvm_unreachable("Already type-checked");
    }
  };

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

  // FIXME: Disable the constraint-based type checker here, because we depend
  // heavily on the existing type checker.
  llvm::SaveAndRestore<bool> savedUseCS(getTypeChecker().getLangOpts()
                                          .UseConstraintSolver,
                                        false);
  ExprRewriter rewriter(*this, solution);
  ExprWalker walker(rewriter);
  return expr->walk(walker);
}
#include "swift/AST/ASTVisitor.h"
