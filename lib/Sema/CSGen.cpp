//===--- TypeCheckConstraintsGen.cpp - Constraint Generator ---------------===//
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
// This file implements constraint generation for the type checker.
//
//===----------------------------------------------------------------------===//
#include "ConstraintSystem.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Expr.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/APInt.h"

using namespace swift;
using namespace swift::constraints;

/// \brief Retrieve the name bound by the given (immediate) pattern.
static Identifier findPatternName(Pattern *pattern) {
  switch (pattern->getKind()) {
  case PatternKind::Paren:
  case PatternKind::Any:
  case PatternKind::Tuple:
    return Identifier();

  case PatternKind::Named:
    return cast<NamedPattern>(pattern)->getBoundName();

  case PatternKind::Typed:
    return findPatternName(cast<TypedPattern>(pattern)->getSubPattern());

  // TODO
#define PATTERN(Id, Parent)
#define REFUTABLE_PATTERN(Id, Parent) case PatternKind::Id:
#include "swift/AST/PatternNodes.def"
    llvm_unreachable("not implemented");
  }

  llvm_unreachable("Unhandled pattern kind");  
}

/// \brief Skip any implicit conversions applied to this expression.
static Expr *skipImplicitConversions(Expr *expr) {
  while (auto ice = dyn_cast<ImplicitConversionExpr>(expr))
    expr = ice->getSubExpr();
  return expr;
}

/// \brief Find the declaration directly referenced by this expression.
static ValueDecl *findReferencedDecl(Expr *expr, SourceLoc &loc) {
  do {
    expr = expr->getSemanticsProvidingExpr();

    if (auto ice = dyn_cast<ImplicitConversionExpr>(expr)) {
      expr = ice->getSubExpr();
      continue;
    }

    if (auto dre = dyn_cast<DeclRefExpr>(expr)) {
      loc = dre->getLoc();
      return dre->getDecl();
    }

    return nullptr;
  } while (true);
}

namespace {
  class ConstraintGenerator : public ExprVisitor<ConstraintGenerator, Type> {
    ConstraintSystem &CS;

    /// \brief Add constraints for a reference to a named member of the given
    /// base type, and return the type of such a reference.
    Type addMemberRefConstraints(Expr *expr, Expr *base, Identifier name) {
      // The base must have a member of the given name, such that accessing
      // that member through the base returns a value convertible to the type
      // of this expression.
      auto baseTy = base->getType();
      auto tv = CS.createTypeVariable(
                  CS.getConstraintLocator(expr, ConstraintLocator::Member),
                  TVO_CanBindToLValue);
      // FIXME: Constraint below should be a ::Member constraint?
      CS.addValueMemberConstraint(baseTy, name, tv,
        CS.getConstraintLocator(expr, ConstraintLocator::MemberRefBase));
      return tv;
    }

    /// \brief Add constraints for a reference to a specific member of the given
    /// base type, and return the type of such a reference.
    Type addMemberRefConstraints(Expr *expr, Expr *base, ValueDecl *decl) {
      // If we're referring to an invalid declaration, fail.
      CS.getTypeChecker().validateDecl(decl, true);
      if (decl->isInvalid())
        return nullptr;

      auto tv = CS.createTypeVariable(
                  CS.getConstraintLocator(expr, ConstraintLocator::Member),
                  TVO_CanBindToLValue);
      OverloadChoice choice(base->getType(), decl, /*isSpecialized=*/false);
      auto locator = CS.getConstraintLocator(expr, ConstraintLocator::Member);
      CS.addBindOverloadConstraint(tv, choice, locator);
      return tv;
    }

    /// \brief Add constraints for a subscript operation.
    Type addSubscriptConstraints(Expr *expr, Expr *base, Expr *index) {
      ASTContext &Context = CS.getASTContext();

      // Locators used in this expression.
      auto indexLocator
        = CS.getConstraintLocator(expr, ConstraintLocator::SubscriptIndex);
      auto resultLocator
        = CS.getConstraintLocator(expr, ConstraintLocator::SubscriptResult);

      // The base type must have a subscript declaration with type
      // I -> [inout] O, where I and O are fresh type variables. The index
      // expression must be convertible to I and the subscript expression
      // itself has type [inout] O, where O may or may not be settable.
      auto inputTv = CS.createTypeVariable(indexLocator, /*options=*/0);
      auto outputTv = CS.createTypeVariable(resultLocator,
                                            TVO_CanBindToLValue);

      // Add the member constraint for a subscript declaration.
      // FIXME: lame name!
      auto baseTy = base->getType();
      auto fnTy = FunctionType::get(inputTv, outputTv, Context);
      CS.addValueMemberConstraint(baseTy, Context.getIdentifier("subscript"),
                                  fnTy,
                                  CS.getConstraintLocator(expr,
                                    ConstraintLocator::SubscriptMember));

      // Add the constraint that the index expression's type be convertible
      // to the input type of the subscript operator.
      CS.addConstraint(ConstraintKind::Conversion, index->getType(), inputTv,
                       indexLocator);
      return outputTv;
    }

  public:
    ConstraintGenerator(ConstraintSystem &CS) : CS(CS) { }

    ConstraintSystem &getConstraintSystem() const { return CS; }
    
    Type visitErrorExpr(ErrorExpr *E) {
      // FIXME: Can we do anything with error expressions at this point?
      return nullptr;
    }

    Type visitLiteralExpr(LiteralExpr *expr) {
      auto protocol = CS.getTypeChecker().getLiteralProtocol(expr);
      if (!protocol) {
        return nullptr;
      }

      auto tv = CS.createTypeVariable(CS.getConstraintLocator(expr, { }),
                                      TVO_PrefersSubtypeBinding);
      CS.addConstraint(ConstraintKind::ConformsTo, tv,
                       protocol->getDeclaredType());
      return tv;
    }

    Type
    visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *expr) {
      // Dig out the StringInterpolationConvertible protocol.
      auto &tc = CS.getTypeChecker();
      auto interpolationProto
        = tc.getProtocol(expr->getLoc(),
                         KnownProtocolKind::StringInterpolationConvertible);
      if (!interpolationProto) {
        tc.diagnose(expr->getStartLoc(), diag::interpolation_missing_proto);
        return nullptr;
      }

      // The type of the expression must conform to the
      // StringInterpolationConvertible protocol.
      auto tv = CS.createTypeVariable(CS.getConstraintLocator(expr, { }),
                                      TVO_PrefersSubtypeBinding);
      CS.addConstraint(ConstraintKind::ConformsTo, tv,
                       interpolationProto->getDeclaredType());

      // Each of the segments needs to be either convertible to the underlying
      // string type or there must be a string constructor from the segment.
      unsigned index = 0;
      for (auto segment : expr->getSegments()) {
        auto locator = CS.getConstraintLocator(
                         expr,
                         LocatorPathElt::getInterpolationArgument(index++));
        Constraint *constraints[2] = {
          Constraint::create(CS, ConstraintKind::Conversion, segment->getType(),
                             tv, Identifier(), locator),
          Constraint::create(CS, ConstraintKind::Construction, 
                             segment->getType(), tv, Identifier(), locator)
        };

        CS.addConstraint(Constraint::createDisjunction(CS, constraints,
                                                       locator));
      }
      
      return tv;
    }

    Type visitDeclRefExpr(DeclRefExpr *E) {
      // If we're referring to an invalid declaration, don't type-check.
      //
      // FIXME: If the decl is in error, we get no information from this.
      // We may, alternatively, want to use a type variable in that case,
      // and possibly infer the type of the variable that way.
      CS.getTypeChecker().validateDecl(E->getDecl(), true);
      if (E->getDecl()->isInvalid())
        return nullptr;

      auto locator = CS.getConstraintLocator(E, { });

      // If this is an anonymous closure argument, take it's type and turn it
      // into an implicit lvalue type. This accounts for the fact that the
      // closure argument type itself might be inferred to an lvalue type.
      if (auto var = dyn_cast<VarDecl>(E->getDecl())) {
        if (var->isAnonClosureParam()) {
          auto tv = CS.createTypeVariable(locator, /*options=*/0);
          CS.addConstraint(ConstraintKind::Equal, tv, E->getDecl()->getType());
          return LValueType::get(tv, LValueType::Qual::DefaultForVar,
                                 CS.getASTContext());
        }
      }

      // Create an overload choice referencing this declaration and immediately
      // resolve it. This records the overload for use later.
      auto tv = CS.createTypeVariable(locator, TVO_CanBindToLValue);
      CS.resolveOverload(locator, tv,
                         OverloadChoice(Type(), E->getDecl(),
                                        E->isSpecialized()));

      return tv;
    }

    Type visitOtherConstructorDeclRefExpr(OtherConstructorDeclRefExpr *E) {
      return E->getType();
    }

    Type visitSuperRefExpr(SuperRefExpr *E) {
      if (!E->getType()) {
        // Resolve the super type of 'self'.
        Type superTy = getSuperType(E->getSelf(), E->getLoc(),
                                    diag::super_not_in_class_method,
                                    diag::super_with_no_base_class);
        if (!superTy)
          return nullptr;
        
        superTy = LValueType::get(superTy,
                                  LValueType::Qual::DefaultForVar,
                                  CS.getASTContext());
        
        return adjustLValueForReference(superTy,
                                        E->getSelf()->getAttrs().isAssignment(),
                                        CS.getASTContext());
      }
      
      return E->getType();
    }
    
    Type visitUnresolvedConstructorExpr(UnresolvedConstructorExpr *expr) {
      ASTContext &C = CS.getASTContext();
      
      // Open a member constraint for constructors on the subexpr type.
      auto baseTy = expr->getSubExpr()->getType()->getRValueType();
      auto argsTy = CS.createTypeVariable(CS.getConstraintLocator(expr, { }),
                                          TVO_CanBindToLValue|TVO_PrefersSubtypeBinding);
      auto methodTy = FunctionType::get(argsTy, baseTy, C);
      CS.addValueMemberConstraint(baseTy, C.getIdentifier("init"),
        methodTy,
        CS.getConstraintLocator(expr, ConstraintLocator::ConstructorMember));
      
      // The result of the expression is the partial application of the
      // constructor to 'self'.
      return methodTy;
    }
    
    Type visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Type visitOverloadedDeclRefExpr(OverloadedDeclRefExpr *expr) {
      // For a reference to an overloaded declaration, we create a type variable
      // that will be equal to different types depending on which overload
      // is selected.
      auto locator = CS.getConstraintLocator(expr, { });
      auto tv = CS.createTypeVariable(locator, TVO_CanBindToLValue);
      ArrayRef<ValueDecl*> decls = expr->getDecls();
      SmallVector<OverloadChoice, 4> choices;
      for (unsigned i = 0, n = decls.size(); i != n; ++i) {
        // If the result is invalid, skip it.
        // FIXME: Note this as invalid, in case we don't find a solution,
        // so we don't let errors cascade further.
        CS.getTypeChecker().validateDecl(decls[i], true);
        if (decls[i]->isInvalid())
          continue;

        choices.push_back(OverloadChoice(Type(), decls[i],
                                         expr->isSpecialized()));
      }

      // If there are no valid overloads, give up.
      if (choices.empty())
        return nullptr;

      // Record this overload set.
      CS.addOverloadSet(tv, choices, locator);
      return tv;
    }

    Type visitOverloadedMemberRefExpr(OverloadedMemberRefExpr *expr) {
      // For a reference to an overloaded declaration, we create a type variable
      // that will be bound to different types depending on which overload
      // is selected.
      auto tv = CS.createTypeVariable(CS.getConstraintLocator(expr, { }),
                                      TVO_CanBindToLValue);
      ArrayRef<ValueDecl*> decls = expr->getDecls();
      SmallVector<OverloadChoice, 4> choices;
      auto baseTy = expr->getBase()->getType();
      for (unsigned i = 0, n = decls.size(); i != n; ++i) {
        // If the result is invalid, skip it.
        // FIXME: Note this as invalid, in case we don't find a solution,
        // so we don't let errors cascade further.
        CS.getTypeChecker().validateDecl(decls[i], true);
        if (decls[i]->isInvalid())
          continue;

        choices.push_back(OverloadChoice(baseTy, decls[i],
                                         /*isSpecialized=*/false));
      }

      // If there are no valid overloads, give up.
      if (choices.empty())
        return nullptr;

      // Record this overload set.
      auto locator = CS.getConstraintLocator(expr, ConstraintLocator::Member);
      CS.addOverloadSet(tv, choices, locator);
      return tv;
    }
    
    Type visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *expr) {
      // This is an error case, where we're trying to use type inference
      // to help us determine which declaration the user meant to refer to.
      // FIXME: Do we need to note that we're doing some kind of recovery?
      return CS.createTypeVariable(CS.getConstraintLocator(expr, { }),
                                   TVO_CanBindToLValue);
    }
    
    Type visitMemberRefExpr(MemberRefExpr *expr) {
      return addMemberRefConstraints(expr, expr->getBase(),
                                     expr->getMember().getDecl());
    }
    
    Type visitExistentialMemberRefExpr(ExistentialMemberRefExpr *expr) {
      return addMemberRefConstraints(expr, expr->getBase(), expr->getDecl());
    }

    Type visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *expr) {
      return addMemberRefConstraints(expr, expr->getBase(), expr->getDecl());
    }

    Type visitDynamicMemberRefExpr(DynamicMemberRefExpr *expr) {
      return addMemberRefConstraints(expr, expr->getBase(),
                                     expr->getMember().getDecl());
    }
    
    Type visitUnresolvedMemberExpr(UnresolvedMemberExpr *expr) {
      auto baseLocator = CS.getConstraintLocator(
                            expr,
                            ConstraintLocator::MemberRefBase);
      auto memberLocator
        = CS.getConstraintLocator(expr, ConstraintLocator::UnresolvedMember);
      auto baseTy = CS.createTypeVariable(baseLocator, /*options=*/0);
      auto memberTy = CS.createTypeVariable(memberLocator, TVO_CanBindToLValue);

      // An unresolved member expression '.member' is modeled as a value member
      // constraint
      //
      //   T0.metatype[.member] == T1
      //
      // for fresh type variables T0 and T1, which pulls out a static
      // member, i.e., an enum case or a static variable.
      auto &ctx = CS.getASTContext();
      auto baseMetaTy = MetaTypeType::get(baseTy, ctx);
      CS.addValueMemberConstraint(baseMetaTy, expr->getName(), memberTy,
                                  memberLocator);

      // If there is an argument, apply it.
      if (auto arg = expr->getArgument()) {
        // The result type of the function must be the base type.
        auto outputTy
          = CS.createTypeVariable(
              CS.getConstraintLocator(expr, ConstraintLocator::FunctionResult),
              /*options=*/0);
        CS.addConstraint(ConstraintKind::Equal, outputTy, baseTy,
          CS.getConstraintLocator(expr, ConstraintLocator::RvalueAdjustment));

        // The function/enum case must be callable with the given argument.
        auto funcTy = FunctionType::get(arg->getType(), outputTy, ctx);
        CS.addConstraint(ConstraintKind::ApplicableFunction, funcTy,
          memberTy,
          CS.getConstraintLocator(expr, ConstraintLocator::ApplyFunction));
      } else {
        // Otherwise, the member needs to have the same type as the base.
        CS.addConstraint(ConstraintKind::Equal, baseTy, memberTy,
          CS.getConstraintLocator(expr, ConstraintLocator::RvalueAdjustment));
      }

      return baseTy;
    }

    Type visitUnresolvedDotExpr(UnresolvedDotExpr *expr) {
      return addMemberRefConstraints(expr, expr->getBase(), expr->getName());
    }
    
    Type visitUnresolvedSpecializeExpr(UnresolvedSpecializeExpr *expr) {
      auto baseTy = expr->getSubExpr()->getType();
      
      // We currently only support explicit specialization of generic types.
      // FIXME: We could support explicit function specialization.
      auto &tc = CS.getTypeChecker();
      if (baseTy->is<AnyFunctionType>()) {
        tc.diagnose(expr->getSubExpr()->getLoc(),
                    diag::cannot_explicitly_specialize_generic_function);
        tc.diagnose(expr->getLAngleLoc(),
                    diag::while_parsing_as_left_angle_bracket);
        return Type();
      }
      
      if (MetaTypeType *meta = baseTy->getAs<MetaTypeType>()) {
        if (BoundGenericType *bgt
              = meta->getInstanceType()->getAs<BoundGenericType>()) {
          ArrayRef<Type> typeVars = bgt->getGenericArgs();
          ArrayRef<TypeLoc> specializations = expr->getUnresolvedParams();

          // If we have too many generic arguments, complain.
          if (specializations.size() > typeVars.size()) {
            tc.diagnose(expr->getSubExpr()->getLoc(),
                        diag::type_parameter_count_mismatch,
                        bgt->getDecl()->getName(),
                        typeVars.size(), specializations.size(),
                        false)
              .highlight(SourceRange(expr->getLAngleLoc(),
                                     expr->getRAngleLoc()));
            tc.diagnose(bgt->getDecl(), diag::generic_type_declared_here,
                        bgt->getDecl()->getName());
            return Type();
          }

          // Bind the specified generic arguments to the type variables in the
          // open type.
          for (size_t i = 0, size = specializations.size(); i < size; ++i) {
            CS.addConstraint(ConstraintKind::Equal,
                             typeVars[i], specializations[i].getType());
          }
          
          return baseTy;
        } else {
          tc.diagnose(expr->getSubExpr()->getLoc(), diag::not_a_generic_type,
                      meta->getInstanceType());
          tc.diagnose(expr->getLAngleLoc(),
                      diag::while_parsing_as_left_angle_bracket);
          return Type();
        }
      }

      // FIXME: If the base type is a type variable, constrain it to a metatype
      // of a bound generic type.
      
      tc.diagnose(expr->getSubExpr()->getLoc(),
                  diag::not_a_generic_definition);
      tc.diagnose(expr->getLAngleLoc(),
                  diag::while_parsing_as_left_angle_bracket);
      return Type();
    }
    
    Type visitSequenceExpr(SequenceExpr *expr) {
      llvm_unreachable("Didn't even parse?");
    }

    Type visitParenExpr(ParenExpr *expr) {
      expr->setType(expr->getSubExpr()->getType());
      return expr->getType();
    }

    Type visitTupleExpr(TupleExpr *expr) {
      // The type of a tuple expression is simply a tuple of the types of
      // its subexpressions.
      SmallVector<TupleTypeElt, 4> elements;
      elements.reserve(expr->getNumElements());
      for (unsigned i = 0, n = expr->getNumElements(); i != n; ++i) {
        elements.push_back(TupleTypeElt(expr->getElement(i)->getType(),
                                        expr->getElementName(i)));
      }

      return TupleType::get(elements, CS.getASTContext());
    }

    Type visitSubscriptExpr(SubscriptExpr *expr) {
      return addSubscriptConstraints(expr, expr->getBase(), expr->getIndex());
    }
    
    Type visitArrayExpr(ArrayExpr *expr) {
      ASTContext &C = CS.getASTContext();
      
      // An array expression can be of a type T that conforms to the
      // ArrayLiteralConvertible protocol.
      auto &tc = CS.getTypeChecker();
      ProtocolDecl *arrayProto
        = tc.getProtocol(expr->getLoc(),
                         KnownProtocolKind::ArrayLiteralConvertible);
      if (!arrayProto) {
        return Type();
      }

      auto locator = CS.getConstraintLocator(expr, { });
      auto arrayTy = CS.createTypeVariable(locator, TVO_PrefersSubtypeBinding);

      // The array must be an array literal type.
      CS.addConstraint(ConstraintKind::ConformsTo, arrayTy,
                       arrayProto->getDeclaredType(),
                       locator);
      
      // Its subexpression should be convertible to a tuple (T.Element...).
      // FIXME: We should really go through the conformance above to extract
      // the element type, rather than just looking for the element type.
      // FIXME: Member constraint is still weird here.
      auto arrayElementTy
        = CS.createTypeVariable(
            CS.getConstraintLocator(expr, ConstraintLocator::Member),
            /*options=*/0);
      CS.addTypeMemberConstraint(arrayTy,
                                 C.getIdentifier("Element"),
                                 arrayElementTy);

      // Introduce conversions from each element to the element type of the
      // array.
      unsigned index = 0;
      for (auto element : expr->getElements()) {
        CS.addConstraint(ConstraintKind::Conversion,
                         element->getType(),
                         arrayElementTy,
                         CS.getConstraintLocator(
                           expr,
                           LocatorPathElt::getTupleElement(index++)));
      }

      return arrayTy;
    }

    Type visitDictionaryExpr(DictionaryExpr *expr) {
      ASTContext &C = CS.getASTContext();
      // A dictionary expression can be of a type T that conforms to the
      // DictionaryLiteralConvertible protocol.
      // FIXME: This isn't actually used for anything at the moment.
      auto &tc = CS.getTypeChecker();
      ProtocolDecl *dictionaryProto
        = tc.getProtocol(expr->getLoc(),
                         KnownProtocolKind::DictionaryLiteralConvertible);
      if (!dictionaryProto) {
        return Type();
      }

      auto locator = CS.getConstraintLocator(expr, { });
      auto dictionaryTy = CS.createTypeVariable(locator,
                                                TVO_PrefersSubtypeBinding);

      // The array must be a dictionary literal type.
      CS.addConstraint(ConstraintKind::ConformsTo, dictionaryTy,
                       dictionaryProto->getDeclaredType(),
                       locator);


      // Its subexpression should be convertible to a tuple
      // ((T.Key,T.Value)...).
      //
      // FIXME: We should be getting Key/Value through the witnesses, not
      // directly from the type.
      // FIXME: Member locator here is weird.
      auto memberLocator = CS.getConstraintLocator(expr,
                                                   ConstraintLocator::Member);
      auto dictionaryKeyTy = CS.createTypeVariable(memberLocator,
                                                   /*options=*/0);
      CS.addTypeMemberConstraint(dictionaryTy,
                                 C.getIdentifier("Key"),
                                 dictionaryKeyTy);

      auto dictionaryValueTy = CS.createTypeVariable(memberLocator,
                                                     /*options=*/0);
      CS.addTypeMemberConstraint(dictionaryTy,
                                 C.getIdentifier("Value"),
                                 dictionaryValueTy);
      
      TupleTypeElt tupleElts[2] = { TupleTypeElt(dictionaryKeyTy),
                                    TupleTypeElt(dictionaryValueTy) };
      Type elementTy = TupleType::get(tupleElts, C);

      // Introduce conversions from each element to the element type of the
      // dictionary.
      unsigned index = 0;
      for (auto element : expr->getElements()) {
        CS.addConstraint(ConstraintKind::Conversion,
                         element->getType(),
                         elementTy,
                         CS.getConstraintLocator(
                           expr,
                           LocatorPathElt::getTupleElement(index++)));
      }

      return dictionaryTy;
    }

    Type visitExistentialSubscriptExpr(ExistentialSubscriptExpr *expr) {
      return addSubscriptConstraints(expr, expr->getBase(), expr->getIndex());
    }

    Type visitArchetypeSubscriptExpr(ArchetypeSubscriptExpr *expr) {
      return addSubscriptConstraints(expr, expr->getBase(), expr->getIndex());
    }

    Type visitDynamicSubscriptExpr(DynamicSubscriptExpr *expr) {
      return addSubscriptConstraints(expr, expr->getBase(), expr->getIndex());
    }

    Type visitTupleElementExpr(TupleElementExpr *expr) {
      ASTContext &context = CS.getASTContext();
      Identifier name
        = context.getIdentifier(llvm::utostr(expr->getFieldNumber()));
      return addMemberRefConstraints(expr, expr->getBase(), name);
    }

    /// \brief Produces a type for the given pattern, filling in any missing
    /// type information with fresh type variables.
    ///
    /// \param pattern The pattern.
    Type getTypeForPattern(Pattern *pattern, bool forFunctionParam,
                           ConstraintLocatorBuilder locator) {
      switch (pattern->getKind()) {
      case PatternKind::Paren:
        // Parentheses don't affect the type.
        return getTypeForPattern(cast<ParenPattern>(pattern)->getSubPattern(),
                                 forFunctionParam, locator);

      case PatternKind::Any:
        // For a pattern of unknown type, create a new type variable.
        return CS.createTypeVariable(CS.getConstraintLocator(locator),
                                     forFunctionParam? TVO_CanBindToLValue : 0);

      case PatternKind::Named: {
        auto var = cast<NamedPattern>(pattern)->getDecl();

        // For a named pattern without a type, create a new type variable
        // and use it as the type of the variable.
        Type ty = CS.createTypeVariable(CS.getConstraintLocator(locator),
                                        forFunctionParam? TVO_CanBindToLValue
                                                        : 0);

        // For [weak] variables, use Optional<T>.
        if (!forFunctionParam && var->getAttrs().isWeak()) {
          ty = CS.getTypeChecker().getOptionalType(var->getLoc(), ty);
          if (!ty) return Type();
        }

        // We want to set the variable's type here when type-checking
        // a function's parameter clauses because we're going to
        // type-check the entire function body within the context of
        // the constraint system.  In contrast, when type-checking a
        // variable binding, we really don't want to set the
        // variable's type because it can easily escape the constraint
        // system and become a dangling type reference.
        if (forFunctionParam)
          var->setType(ty);
        return ty;
      }

      case PatternKind::Typed:
        // For a typed pattern, simply return the opened type of the pattern.
        // FIXME: Error recovery if the type is an error type?
        return CS.openType(cast<TypedPattern>(pattern)->getType());

      case PatternKind::Tuple: {
        auto tuplePat = cast<TuplePattern>(pattern);
        SmallVector<TupleTypeElt, 4> tupleTypeElts;
        tupleTypeElts.reserve(tuplePat->getNumFields());
        for (unsigned i = 0, e = tuplePat->getFields().size(); i != e; ++i) {
          auto tupleElt = tuplePat->getFields()[i];
          bool isVararg = tuplePat->hasVararg() && i == e-1;
          Type eltTy = getTypeForPattern(tupleElt.getPattern(), forFunctionParam,
                                         locator.withPathElement(
                                           LocatorPathElt::getTupleElement(i)));

          // Only cons up a tuple element name in a function signature.
          Identifier name;
          if (forFunctionParam) name = findPatternName(tupleElt.getPattern());

          Type varArgBaseTy;
          tupleTypeElts.push_back(TupleTypeElt(eltTy, name,
                                               tupleElt.getDefaultArgKind(),
                                               isVararg));
        }
        return TupleType::get(tupleTypeElts, CS.getASTContext());
      }
      
      // TODO
#define PATTERN(Id, Parent)
#define REFUTABLE_PATTERN(Id, Parent) case PatternKind::Id:
#include "swift/AST/PatternNodes.def"
        llvm_unreachable("not implemented");
      }

      llvm_unreachable("Unhandled pattern kind");
    }

    Type visitClosureExpr(ClosureExpr *expr) {
      // Closure expressions always have function type. In cases where a
      // parameter or return type is omitted, a fresh type variable is used to
      // stand in for that parameter or return type, allowing it to be inferred
      // from context.
      Type funcTy;
      if (expr->hasExplicitResultType()) {
        funcTy = expr->getExplicitResultTypeLoc().getType();
      } else {
        // If no return type was specified, create a fresh type
        // variable for it.
        funcTy = CS.createTypeVariable(
                   CS.getConstraintLocator(expr,
                                           ConstraintLocator::ClosureResult),
                   /*options=*/0);
      }

      // Walk through the patterns in the func expression, backwards,
      // computing the type of each pattern (which may involve fresh type
      // variables where parameter types where no provided) and building the
      // eventual function type.
      auto paramTy = getTypeForPattern(
                       expr->getParams(), /*forFunctionParam*/ true,
                       CS.getConstraintLocator(
                         expr,
                         LocatorPathElt::getTupleElement(0)));
      funcTy = FunctionType::get(paramTy, funcTy, CS.getASTContext());

      return funcTy;
    }

    Type visitAutoClosureExpr(AutoClosureExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Type visitModuleExpr(ModuleExpr *expr) {
      // Module expressions always have a fixed type.
      return expr->getType();
    }

    Type visitAddressOfExpr(AddressOfExpr *expr) {
      // The address-of operator produces an explicit lvalue
      // [inout(settable)] T from a (potentially implicit) settable lvalue S.
      // We model this with the constraint
      //
      //     S < [inout(implicit, settable)] T
      //
      // where T is a fresh type variable.
      auto tv = CS.createTypeVariable(CS.getConstraintLocator(expr, { }),
                                      /*options=*/0);
      auto bound = LValueType::get(tv,
                                   LValueType::Qual::DefaultForType|
                                   LValueType::Qual::Implicit,
                                   CS.getASTContext());
      auto result = LValueType::get(tv,
                                    LValueType::Qual::DefaultForType,
                                    CS.getASTContext());

      CS.addConstraint(ConstraintKind::Subtype,
                       expr->getSubExpr()->getType(), bound,
                       CS.getConstraintLocator(expr,
                                               ConstraintLocator::AddressOf));
      return result;
    }

    Type visitNewArrayExpr(NewArrayExpr *expr) {
      // Open up the element type.
      auto elementTy = CS.openType(expr->getElementTypeLoc().getType());
      auto resultTy = elementTy;
      auto &tc = CS.getTypeChecker();
      for (unsigned i = expr->getBounds().size(); i != 1; --i) {
        auto &bound = expr->getBounds()[i-1];
        if (!bound.Value) {
          resultTy = tc.getArraySliceType(bound.Brackets.Start, resultTy);
          continue;
        }

        // FIXME: When we get a constant expression evaluator, we'll have
        // to use it here.
        auto literal = cast<IntegerLiteralExpr>(
                         bound.Value->getSemanticsProvidingExpr());
        resultTy = ArrayType::get(resultTy, literal->getValue().getZExtValue(),
                                  tc.Context);
      }
      
      auto &outerBound = expr->getBounds()[0];

      // Either we have an explicit constructor closure or else ElementType must
      // be default constructible.
      if (!expr->hasConstructionFunction()) {
        Type defaultCtorTy = FunctionType::get(TupleType::getEmpty(tc.Context),
                                               elementTy,
                                               tc.Context);
        CS.addValueMemberConstraint(elementTy, tc.Context.getIdentifier("init"),
            defaultCtorTy,
            CS.getConstraintLocator(expr, ConstraintLocator::NewArrayElement));
      }
      
      return tc.getArraySliceType(outerBound.Brackets.Start, resultTy);
    }

    Type visitMetatypeExpr(MetatypeExpr *expr) {
      if (auto base = expr->getBase()) {
        auto tv = CS.createTypeVariable(CS.getConstraintLocator(expr, { }),
                                        /*options=*/0);
        CS.addConstraint(ConstraintKind::Equal, tv, base->getType(),
          CS.getConstraintLocator(expr, ConstraintLocator::RvalueAdjustment));

        return MetaTypeType::get(tv, CS.getASTContext());
      }

      if (auto baseTyR = expr->getBaseTypeRepr()) {
        auto type = CS.TC.resolveType(baseTyR, /*SIL*/ false, CS.DC);
        if (type)
          return MetaTypeType::get(type, CS.getASTContext());

        return Type();
      }

      // This is an artificial MetatypeExpr, so it's fully type-checked.
      return expr->getType();
    }

    Type visitOpaqueValueExpr(OpaqueValueExpr *expr) {
      return expr->getType();
    }

    Type visitZeroValueExpr(ZeroValueExpr *expr) {
      return expr->getType();
    }

    Type visitDefaultValueExpr(DefaultValueExpr *expr) {
      expr->setType(expr->getSubExpr()->getType());
      return expr->getType();
    }

    Type visitApplyExpr(ApplyExpr *expr) {
      ASTContext &Context = CS.getASTContext();

      // The function subexpression has some rvalue type T1 -> T2 for fresh
      // variables T1 and T2.
      auto outputTy
        = CS.createTypeVariable(
            CS.getConstraintLocator(expr, ConstraintLocator::FunctionResult),
            /*options=*/0);

      auto funcTy = FunctionType::get(expr->getArg()->getType(), outputTy, 
                                      Context);

      CS.addConstraint(ConstraintKind::ApplicableFunction, funcTy,
        expr->getFn()->getType(),
        CS.getConstraintLocator(expr, ConstraintLocator::ApplyFunction));

      return outputTy;
    }

    Type getSuperType(ValueDecl *selfDecl,
                      SourceLoc diagLoc,
                      Diag<> diag_not_in_class,
                      Diag<> diag_no_base_class) {
      DeclContext *typeContext = selfDecl->getDeclContext()->getParent();
      assert(typeContext && "constructor without parent context?!");
      auto &tc = CS.getTypeChecker();
      ClassDecl *classDecl = typeContext->getDeclaredTypeInContext()
                               ->getClassOrBoundGenericClass();
      if (!classDecl) {
        tc.diagnose(diagLoc, diag_not_in_class);
        return Type();
      }
      if (!classDecl->hasSuperclass()) {
        tc.diagnose(diagLoc, diag_no_base_class);
        return Type();
      }

      Type superclassTy = typeContext->getDeclaredTypeInContext()
                            ->getSuperclass(&tc);
      if (selfDecl->getType()->is<MetaTypeType>())
        superclassTy = MetaTypeType::get(superclassTy, CS.getASTContext());
      return superclassTy;
    }
    
    Type visitRebindSelfInConstructorExpr(RebindSelfInConstructorExpr *expr) {
      // The subexpression must be a supertype of 'self' type.
      CS.addConstraint(ConstraintKind::Subtype,
                       expr->getSelf()->getType(),
                       expr->getSubExpr()->getType());
      // The result is void.
      return TupleType::getEmpty(CS.getASTContext());
    }
    
    Type visitIfExpr(IfExpr *expr) {
      // The conditional expression must conform to LogicValue.
      Expr *condExpr = expr->getCondExpr();
      auto logicValue
        = CS.getTypeChecker().getProtocol(expr->getQuestionLoc(),
                                          KnownProtocolKind::LogicValue);
      if (!logicValue)
        return Type();

      CS.addConstraint(ConstraintKind::ConformsTo, condExpr->getType(),
                       logicValue->getDeclaredType(),
                       CS.getConstraintLocator(expr, { }));

      // The branches must be convertible to a common type.
      auto resultTy = CS.createTypeVariable(CS.getConstraintLocator(expr, { }),
                                            TVO_PrefersSubtypeBinding);
      CS.addConstraint(ConstraintKind::Conversion,
                       expr->getThenExpr()->getType(), resultTy,
                       CS.getConstraintLocator(expr,
                                               ConstraintLocator::IfThen));
      CS.addConstraint(ConstraintKind::Conversion,
                       expr->getElseExpr()->getType(), resultTy,
                       CS.getConstraintLocator(expr,
                                               ConstraintLocator::IfElse));
      return resultTy;
    }
    
    Type visitImplicitConversionExpr(ImplicitConversionExpr *expr) {
      llvm_unreachable("Already type-checked");
    }
    
    Type visitConditionalCheckedCastExpr(ConditionalCheckedCastExpr *expr) {
      auto &tc = CS.getTypeChecker();
      return tc.getOptionalType(expr->getLoc(),
                                expr->getCastTypeLoc().getType());
    }
    
    Type visitIsaExpr(IsaExpr *expr) {
      // The result is Bool.
      return CS.getTypeChecker().lookupBoolType(CS.DC);
    }

    Type visitCoerceExpr(CoerceExpr *expr) {
      return expr->getCastTypeLoc().getType();
    }

    Type visitDiscardAssignmentExpr(DiscardAssignmentExpr *expr) {
      // '_' is only allowed in assignments, so give it an AssignDest locator.
      return CS.createTypeVariable(
         CS.getConstraintLocator(expr,
                                 ConstraintLocator::AssignDest),
         TVO_CanBindToLValue);
    }
    
    Type visitAssignExpr(AssignExpr *expr) {
      // Compute the type to which the source must be converted to allow
      // assignment to the destination.
      auto destTy = CS.computeAssignDestType(expr->getDest(), expr->getLoc());
      if (!destTy)
        return Type();
      
      // The source must be convertible to the destination.
      auto assignLocator = CS.getConstraintLocator(expr->getSrc(),
                                               ConstraintLocator::AssignSource);
      CS.addConstraint(ConstraintKind::Conversion,
                       expr->getSrc()->getType(), destTy,
                       assignLocator);
      
      expr->setType(TupleType::getEmpty(CS.getASTContext()));
      return expr->getType();
    }
    
    Type visitUnresolvedPatternExpr(UnresolvedPatternExpr *expr) {
      // If there are UnresolvedPatterns floating around after name binding,
      // they are pattern productions in invalid positions.
      CS.TC.diagnose(expr->getLoc(), diag::pattern_in_expr,
                     expr->getSubPattern()->getKind());
      return Type();
    }

    /// Get the type T?
    ///
    ///  This is not the ideal source location, but it's only used for
    /// diagnosing ill-formed standard libraries, so it really isn't
    /// worth QoI efforts.
    Type getOptionalType(SourceLoc optLoc, Type valueTy) {
      auto optTy = CS.getTypeChecker().getOptionalType(optLoc, valueTy);
      if (!optTy || CS.getTypeChecker().requireOptionalIntrinsics(optLoc))
        return Type();

      return optTy;
    }

    Type visitBindOptionalExpr(BindOptionalExpr *expr) {
      // The operand must be coercible to T?, and we will have type T.
      auto valueTy = CS.createTypeVariable(CS.getConstraintLocator(expr, { }),
                                            /*options*/ 0);

      Type optTy = getOptionalType(expr->getQuestionLoc(), valueTy);
      if (!optTy)
        return Type();

      CS.addConstraint(ConstraintKind::Conversion,
                       expr->getSubExpr()->getType(), optTy,
                       CS.getConstraintLocator(expr, {}));
      return valueTy;
    }
    
    Type visitOptionalEvaluationExpr(OptionalEvaluationExpr *expr) {
      // The operand must be coercible to T? for some type T.  We'd
      // like this to be the smallest possible nesting level of
      // optional types, e.g. T? over T??; otherwise we don't really
      // have a preference.
      auto valueTy = CS.createTypeVariable(CS.getConstraintLocator(expr, { }),
                                           TVO_PrefersSubtypeBinding);

      Type optTy = getOptionalType(expr->getSubExpr()->getLoc(), valueTy);
      if (!optTy)
        return Type();

      CS.addConstraint(ConstraintKind::Conversion,
                       expr->getSubExpr()->getType(), optTy,
                       CS.getConstraintLocator(expr, {}));
      return optTy;
    }

    Type visitForceValueExpr(ForceValueExpr *expr) {
      // The value can be forced in two different ways:
      //   - Either the value is coercible to T? and the result is T, which
      //     retrieves the value stored in the optional
      //   - The value is of rvalue type DynamicLookup, and the result is
      //     some class type T.
      auto valueTy = CS.createTypeVariable(CS.getConstraintLocator(expr, { }),
                                           TVO_PrefersSubtypeBinding);

      Type optTy = getOptionalType(expr->getSubExpr()->getLoc(), valueTy);
      if (!optTy)
        return Type();

      auto locator = CS.getConstraintLocator(expr, {});
      Constraint *downcastConstraints[2] = {
        Constraint::create(CS, ConstraintKind::DynamicLookupValue,
                           expr->getSubExpr()->getType(),
                           Type(),
                           Identifier(),
                           locator),
        Constraint::create(CS, ConstraintKind::Class,
                           valueTy,
                           Type(),
                           Identifier(),
                           locator)
      };
      Constraint *constraints[2] = {
        // The subexpression is convertible to T?
        Constraint::create(CS, ConstraintKind::Conversion,
                           expr->getSubExpr()->getType(), optTy,
                           Identifier(),
                           locator),
        // The subexpression is a DynamicLookup value and the resulting value
        // is of class type.
        Constraint::createConjunction(CS, downcastConstraints, locator)
      };
      CS.addConstraint(Constraint::createDisjunction(CS, constraints, locator));

      // The result is of type T.
      return valueTy;
    }
  };

  /// \brief AST walker that "sanitizes" an expression for the
  /// constraint-based type checker.
  ///
  /// This is only necessary because Sema fills in too much type information
  /// before the type-checker runs, causing redundant work.
  class SanitizeExpr : public ASTWalker {
    TypeChecker &TC;
  public:
    SanitizeExpr(TypeChecker &tc) : TC(tc) { }

    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      // Don't recur into array-new, default-value expressions, or explicit
      // cast expressions.
      return {
        !isa<NewArrayExpr>(expr)
          && !isa<DefaultValueExpr>(expr)
          && !isa<ExplicitCastExpr>(expr),
        expr
      };
    }

    Expr *walkToExprPost(Expr *expr) override {
      if (auto implicit = dyn_cast<ImplicitConversionExpr>(expr)) {
        // Skip implicit conversions completely.
        return implicit->getSubExpr();
      }

      if (auto dotCall = dyn_cast<DotSyntaxCallExpr>(expr)) {
        // A DotSyntaxCallExpr is a member reference that has already been
        // type-checked down to a call; turn it back into an overloaded
        // member reference expression.
        SourceLoc memberLoc;
        if (auto member = findReferencedDecl(dotCall->getFn(), memberLoc)) {
          auto base = skipImplicitConversions(dotCall->getArg());
          auto members
            = TC.Context.AllocateCopy(ArrayRef<ValueDecl *>(&member, 1));
          return new (TC.Context) OverloadedMemberRefExpr(base,
                                   dotCall->getDotLoc(), members, memberLoc,
                                   expr->isImplicit());
        }
      }

      if (auto dotIgnored = dyn_cast<DotSyntaxBaseIgnoredExpr>(expr)) {
        // A DotSyntaxBaseIgnoredExpr is a static member reference that has
        // already been type-checked down to a call where the argument doesn't
        // actually matter; turn it back into an overloaded member reference
        // expression.
        SourceLoc memberLoc;
        if (auto member = findReferencedDecl(dotIgnored->getRHS(), memberLoc)) {
          auto base = skipImplicitConversions(dotIgnored->getLHS());
          auto members
            = TC.Context.AllocateCopy(ArrayRef<ValueDecl *>(&member, 1));
          return new (TC.Context) OverloadedMemberRefExpr(base,
                                    dotIgnored->getDotLoc(), members,
                                    memberLoc, expr->isImplicit());
        }
      }
      
      return expr;
    }
  };

  class ConstraintWalker : public ASTWalker {
    ConstraintGenerator &CG;

  public:
    ConstraintWalker(ConstraintGenerator &CG) : CG(CG) { }

    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      // For closures containing only a single expression, the body participates
      // in type checking.
      if (auto closure = dyn_cast<ClosureExpr>(expr)) {
        if (closure->hasSingleExpressionBody()) {
          // Visit the closure itself, which produces a function type.
          auto funcTy = CG.visit(expr)->castTo<FunctionType>();
          expr->setType(funcTy);
        }

        return { true, expr };
      }

      // For new array expressions, we visit the node but not any of its
      // children.
      // FIXME: If new array expressions gain an initializer, we'll need to
      // visit that first.
      if (auto newArray = dyn_cast<NewArrayExpr>(expr)) {
        auto type = CG.visitNewArrayExpr(newArray);
        expr->setType(type);
        return { false, expr };
      }

      // For explicit casts, we've already visited the subexpression.
      if (auto cast = dyn_cast<ExplicitCastExpr>(expr)) {
        auto type = CG.visit(cast);
        expr->setType(type);
        return { false, expr };
      }

      // We don't visit default value expressions; they've already been
      // type-checked.
      if (isa<DefaultValueExpr>(expr)) {
        return { false, expr };
      }
      
      return { true, expr };
    }

    /// \brief Once we've visited the children of the given expression,
    /// generate constraints from the expression.
    Expr *walkToExprPost(Expr *expr) override {
      if (auto closure = dyn_cast<ClosureExpr>(expr)) {
        if (closure->hasSingleExpressionBody()) {
          // Visit the body. It's type needs to be convertible to the function's
          // return type.
          auto resultTy = closure->getResultType();
          auto bodyTy = closure->getSingleExpressionBody()->getType();
          CG.getConstraintSystem()
            .addConstraint(ConstraintKind::Conversion, bodyTy,
                           resultTy,
                           CG.getConstraintSystem()
                             .getConstraintLocator(
                               expr,
                               ConstraintLocator::ClosureResult));
          return expr;
        }
      }

      if (auto type = CG.visit(expr)) {
        expr->setType(CG.getConstraintSystem().simplifyType(type));
        return expr;
      }

      return nullptr;
    }

    /// \brief Ignore statements.
    std::pair<bool, Stmt *> walkToStmtPre(Stmt *stmt) override {
      return { false, stmt };
    }

    /// \brief Ignore declarations.
    bool walkToDeclPre(Decl *decl) override { return false; }
  };
} // end anonymous namespace

Expr *ConstraintSystem::generateConstraints(Expr *expr) {
  // Remove implicit conversions from the expression.
  expr = expr->walk(SanitizeExpr(getTypeChecker()));

  // Walk the expression, generating constraints.
  ConstraintGenerator cg(*this);
  ConstraintWalker cw(cg);
  return expr->walk(cw);
}

Expr *ConstraintSystem::generateConstraintsShallow(Expr *expr) {
  // Sanitize the expression.
  expr = SanitizeExpr(getTypeChecker()).walkToExprPost(expr);

  // Visit the top-level expression generating constraints.
  ConstraintGenerator cg(*this);
  auto type = cg.visit(expr);
  if (!type)
    return nullptr;
  expr->setType(type);
  return expr;
}

Type ConstraintSystem::generateConstraints(Pattern *pattern,
                                           ConstraintLocatorBuilder locator) {
  ConstraintGenerator cg(*this);
  return cg.getTypeForPattern(pattern, /*forFunctionParam*/ false, locator);
}
