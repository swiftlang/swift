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
      auto tv = CS.createTypeVariable(expr);
      CS.addValueMemberConstraint(baseTy, name, tv,
        CS.getConstraintLocator(expr, ConstraintLocator::MemberRefBase));
      return tv;
    }

    /// \brief Add constraints for a reference to a specific member of the given
    /// base type, and return the type of such a reference.
    Type addMemberRefConstraints(Expr *expr, Expr *base, ValueDecl *decl) {
      auto tv = CS.createTypeVariable(expr);
      OverloadChoice choice(base->getType(), decl);
      auto locator = CS.getConstraintLocator(expr, ConstraintLocator::Member);
      CS.addOverloadSet(OverloadSet::getNew(CS, tv, locator, { &choice, 1 }));
      return tv;
    }

    /// \brief Add constraints for a subscript operation.
    Type addSubscriptConstraints(Expr *expr, Expr *base, Expr *index) {
      ASTContext &Context = CS.getASTContext();

      // The base type must have a subscript declaration with type
      // I -> [byref] O, where I and O are fresh type variables. The index
      // expression must be convertible to I and the subscript expression
      // itself has type [byref] O, where O may or may not be settable.
      auto inputTv = CS.createTypeVariable(expr);
      auto outputTv = CS.createTypeVariable(expr);

      auto outputSuperTv = CS.createTypeVariable(expr);
      auto outputSuperTy = LValueType::get(outputSuperTv,
                                           LValueType::Qual::DefaultForMemberAccess|
                                           LValueType::Qual::Implicit|
                                           LValueType::Qual::NonSettable,
                                           Context);

      // Add the member constraint for a subscript declaration.
      // FIXME: lame name!
      auto baseTy = base->getType();
      auto fnTy = FunctionType::get(inputTv, outputTv, Context);
      CS.addValueMemberConstraint(baseTy, Context.getIdentifier("__subscript"),
                                  fnTy,
                                  CS.getConstraintLocator(expr,
                                    ConstraintLocator::SubscriptMember));

      // Add the subtype constraint that the output type must be some lvalue
      // subtype.
      CS.addConstraint(ConstraintKind::Subtype, outputTv, outputSuperTy,
        CS.getConstraintLocator(expr, ConstraintLocator::SubscriptResult));

      // Add the constraint that the index expression's type be convertible
      // to the input type of the subscript operator.
      CS.addConstraint(ConstraintKind::Conversion, index->getType(), inputTv,
        CS.getConstraintLocator(expr, ConstraintLocator::SubscriptIndex));
      return outputTv;
    }

  public:
    ConstraintGenerator(ConstraintSystem &CS) : CS(CS) { }

    ConstraintSystem &getConstraintSystem() const { return CS; }
    
    Type visitErrorExpr(ErrorExpr *E) {
      // FIXME: Can we do anything with error expressions at this point?
      return nullptr;
    }

    Type visitIntegerLiteralExpr(IntegerLiteralExpr *expr) {
      auto protocol
        = CS.getTypeChecker().getProtocol(
            expr->getLoc(),
            KnownProtocolKind::IntegerLiteralConvertible);
      if (!protocol) {
        return nullptr;
      }

      auto tv = CS.createTypeVariable(expr);
      CS.addConstraint(ConstraintKind::ConformsTo, tv,
                       protocol->getDeclaredType());
      return tv;
    }

    Type visitFloatLiteralExpr(FloatLiteralExpr *expr) {
      auto protocol
        = CS.getTypeChecker().getProtocol(
            expr->getLoc(),
            KnownProtocolKind::FloatLiteralConvertible);
      if (!protocol) {
        return nullptr;
      }

      // Require conformance to the FloatLiteralConvertible protocol.
      auto tv = CS.createTypeVariable(expr);
      CS.addConstraint(ConstraintKind::ConformsTo, tv,
                       protocol->getDeclaredType());

      return tv;
    }

    Type visitCharacterLiteralExpr(CharacterLiteralExpr *expr) {
      auto protocol
        = CS.getTypeChecker().getProtocol(
            expr->getLoc(),
            KnownProtocolKind::CharacterLiteralConvertible);
      if (!protocol) {
        return nullptr;
      }
      
      auto tv = CS.createTypeVariable(expr);
      CS.addConstraint(ConstraintKind::ConformsTo, tv,
                       protocol->getDeclaredType());
      return tv;
    }

    Type visitStringLiteralExpr(StringLiteralExpr *expr) {
      auto protocol
        = CS.getTypeChecker().getProtocol(
            expr->getLoc(),
            KnownProtocolKind::StringLiteralConvertible);
      if (!protocol) {
        return nullptr;
      }

      auto tv = CS.createTypeVariable(expr);
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
      auto tv = CS.createTypeVariable(expr);
      CS.addConstraint(ConstraintKind::ConformsTo, tv,
                       interpolationProto->getDeclaredType());

      // Each of the segments needs to be convertible to a constructor argument
      // for the underlying string type.
      unsigned index = 0;
      for (auto segment : expr->getSegments()) {
        CS.addConstraint(ConstraintKind::Construction, segment->getType(), tv,
          CS.getConstraintLocator(expr,
            LocatorPathElt::getInterpolationArgument(index++)));
      }
      
      return tv;
    }

    Type visitDeclRefExpr(DeclRefExpr *E) {
      // FIXME: If the decl is in error, we get no information from this.
      // We may, alternatively, want to use a type variable in that case,
      // and possibly infer the type of the variable that way.
      return adjustLValueForReference(CS.getTypeOfReference(E->getDecl()),
                                      E->getDecl()->getAttrs().isAssignment(),
                                      CS.getASTContext());
    }

    Type visitOtherConstructorDeclRefExpr(OtherConstructorDeclRefExpr *E) {
      return E->getType();
    }

    Type visitSuperRefExpr(SuperRefExpr *E) {
      if (!E->getType()) {
        // Resolve the super type of 'this'.
        Type superTy = getSuperType(E->getThis(), E->getLoc(),
                                    diag::super_not_in_class_method,
                                    diag::super_with_no_base_class);
        if (!superTy)
          return nullptr;
        
        superTy = LValueType::get(superTy,
                                  LValueType::Qual::DefaultForVar,
                                  CS.getASTContext());
        
        return adjustLValueForReference(superTy,
                                        E->getThis()->getAttrs().isAssignment(),
                                        CS.getASTContext());
      }
      
      return E->getType();
    }
    
    Type visitUnresolvedConstructorExpr(UnresolvedConstructorExpr *E) {
      ASTContext &C = CS.getASTContext();
      
      // Open a member constraint for constructors on the subexpr type.
      auto baseTy = E->getSubExpr()->getType()->getRValueType();
      auto argsTy = CS.createTypeVariable(E);
      auto methodTy = FunctionType::get(argsTy, baseTy, C);
      CS.addValueMemberConstraint(baseTy, C.getIdentifier("constructor"),
        methodTy,
        CS.getConstraintLocator(E, ConstraintLocator::ConstructorMember));
      
      // The result of the expression is the partial application of the
      // constructor to 'this'.
      return methodTy;
    }
    
    Type visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E) {
      llvm_unreachable("Already type-checked");
    }

    Type visitOverloadedDeclRefExpr(OverloadedDeclRefExpr *expr) {
      // For a reference to an overloaded declaration, we create a type variable
      // that will be equal to different types depending on which overload
      // is selected.
      auto tv = CS.createTypeVariable(expr);
      ArrayRef<ValueDecl*> decls = expr->getDecls();
      SmallVector<OverloadChoice, 4> choices;
      for (unsigned i = 0, n = decls.size(); i != n; ++i) {
        choices.push_back(OverloadChoice(Type(), decls[i]));
      }

      // Record this overload set.
      CS.addOverloadSet(OverloadSet::getNew(CS, tv,
                                            CS.getConstraintLocator(expr, { }),
                                            choices));
      return tv;
    }

    Type visitOverloadedMemberRefExpr(OverloadedMemberRefExpr *expr) {
      // For a reference to an overloaded declaration, we create a type variable
      // that will be bound to different types depending on which overload
      // is selected.
      auto tv = CS.createTypeVariable(expr);
      ArrayRef<ValueDecl*> decls = expr->getDecls();
      SmallVector<OverloadChoice, 4> choices;
      auto baseTy = expr->getBase()->getType();
      for (unsigned i = 0, n = decls.size(); i != n; ++i) {
        choices.push_back(OverloadChoice(baseTy, decls[i]));
      }
      
      // Record this overload set.
      auto locator = CS.getConstraintLocator(expr, ConstraintLocator::Member);
      CS.addOverloadSet(OverloadSet::getNew(CS, tv, locator, choices));
      return tv;
    }
    
    Type visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *expr) {
      // This is an error case, where we're trying to use type inference
      // to help us determine which declaration the user meant to refer to.
      // FIXME: Do we need to note that we're doing some kind of recovery?
      return CS.createTypeVariable(expr);
    }
    
    Type visitMemberRefExpr(MemberRefExpr *expr) {
      return addMemberRefConstraints(expr, expr->getBase(), expr->getDecl());
    }
    
    Type visitExistentialMemberRefExpr(ExistentialMemberRefExpr *expr) {
      return addMemberRefConstraints(expr, expr->getBase(), expr->getDecl());
    }

    Type visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *expr) {
      return addMemberRefConstraints(expr, expr->getBase(), expr->getDecl());
    }

    Type visitGenericMemberRefExpr(GenericMemberRefExpr *expr) {
      return addMemberRefConstraints(expr, expr->getBase(), expr->getDecl());
    }

    Type visitUnresolvedMemberExpr(UnresolvedMemberExpr *expr) {
      auto oneofTy = CS.createTypeVariable(expr);
      auto memberTy = CS.createTypeVariable(expr);

      // An unresolved member expression '.member' is modeled as a value member
      // constraint
      //
      //   T0[.member] == T1
      //
      // for fresh type variables T0 and T1. Depending on whether the member
      // will end up having unit type () or an actual type, T1 will either be
      // T0 or will be T2 -> T0 for some fresh type variable T2. Since T0
      // cannot be determined without picking one of these options, and we
      // cannot know whether to select the value form (T0) or the function
      // form (T2 -> T0) until T0 has been deduced, we cannot model this
      // directly within the constraint system. Instead, we introduce a new
      // overload set with two entries: one for T0 and one for T2 -> T0.
      auto oneofMetaTy = MetaTypeType::get(oneofTy, CS.getASTContext());
      CS.addValueMemberConstraint(oneofMetaTy, expr->getName(), memberTy,
                                  CS.getConstraintLocator(
                                    expr,
                                    ConstraintLocator::Member));

      OverloadChoice choices[2] = {
        OverloadChoice(oneofTy, OverloadChoiceKind::BaseType),
        OverloadChoice(oneofTy, OverloadChoiceKind::FunctionReturningBaseType),
      };
      auto locator = CS.getConstraintLocator(
                         expr,
                         ConstraintLocator::MemberRefBase);
      CS.addOverloadSet(OverloadSet::getNew(CS, memberTy, locator, choices));
      return memberTy;
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
          
          // FIXME: We could potentially allow unspecified type parameters
          // to be deduced if there are fewer parameters than type variables.
          if (typeVars.size() != specializations.size()) {
            tc.diagnose(expr->getSubExpr()->getLoc(),
                        diag::type_parameter_count_mismatch,
                        typeVars.size(), specializations.size());
            tc.diagnose(expr->getLAngleLoc(),
                        diag::while_parsing_as_left_angle_bracket);
            return Type();
          }
          
          for (size_t i = 0, size = typeVars.size(); i < size; ++i) {
            CS.addConstraint(ConstraintKind::Equal,
                             typeVars[i], specializations[i].getType());
          }
          
          return baseTy;
        } else {
          tc.diagnose(expr->getSubExpr()->getLoc(), diag::not_a_generic_type);
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

      auto arrayTy = CS.createTypeVariable(expr);

      // The array must be an array literal type.
      CS.addConstraint(ConstraintKind::ConformsTo, arrayTy,
                       arrayProto->getDeclaredType(),
                       CS.getConstraintLocator(expr, { }));
      
      // Its subexpression should be convertible to a tuple (T.Element...).
      // FIXME: We should really go through the conformance above to extract
      // the element type, rather than just looking for the element type.
      auto arrayElementTy = CS.createTypeVariable(expr);
      CS.addTypeMemberConstraint(arrayTy,
                                 C.getIdentifier("Element"),
                                 arrayElementTy);
      
      Type arrayEltsTy = tc.getArraySliceType(expr->getLoc(), arrayElementTy);
      TupleTypeElt arrayEltsElt{arrayEltsTy,
                                /*name=*/ Identifier(),
                                /*init=*/ nullptr,
                                arrayElementTy};
      Type arrayEltsTupleTy = TupleType::get(arrayEltsElt, C);
      CS.addConstraint(ConstraintKind::Conversion,
                       expr->getSubExpr()->getType(),
                       arrayEltsTupleTy,
                       CS.getConstraintLocator(
                         expr,
                         ConstraintLocator::ApplyArgument));

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

      auto dictionaryTy = CS.createTypeVariable(expr);

      // The array must be a dictionary literal type.
      CS.addConstraint(ConstraintKind::ConformsTo, dictionaryTy,
                       dictionaryProto->getDeclaredType(),
                       CS.getConstraintLocator(expr, { }));


      // Its subexpression should be convertible to a tuple
      // ((T.Key,T.Value)...).
      //
      // FIXME: We should be getting Key/Value through the witnesses, not
      // directly from the type.
      auto dictionaryKeyTy = CS.createTypeVariable(expr);
      CS.addTypeMemberConstraint(dictionaryTy,
                                 C.getIdentifier("Key"),
                                 dictionaryKeyTy);

      auto dictionaryValueTy = CS.createTypeVariable(expr);
      CS.addTypeMemberConstraint(dictionaryTy,
                                 C.getIdentifier("Value"),
                                 dictionaryValueTy);
      
      TupleTypeElt tupleElts[2] = { TupleTypeElt(dictionaryKeyTy),
                                    TupleTypeElt(dictionaryValueTy) };
      Type elementTy = TupleType::get(tupleElts, C);
      Type dictionaryEltsTy = tc.getArraySliceType(expr->getLoc(), elementTy);
      TupleTypeElt dictionaryEltsElt(dictionaryEltsTy,
                                     /*name=*/ Identifier(),
                                     /*init=*/ nullptr,
                                     elementTy);
      Type dictionaryEltsTupleTy = TupleType::get(dictionaryEltsElt, C);
      CS.addConstraint(ConstraintKind::Conversion,
                       expr->getSubExpr()->getType(),
                       dictionaryEltsTupleTy,
                       CS.getConstraintLocator(
                         expr,
                         ConstraintLocator::ApplyArgument));

      return dictionaryTy;
    }

    Type visitExistentialSubscriptExpr(ExistentialSubscriptExpr *expr) {
      return addSubscriptConstraints(expr, expr->getBase(), expr->getIndex());
    }

    Type visitArchetypeSubscriptExpr(ArchetypeSubscriptExpr *expr) {
      return addSubscriptConstraints(expr, expr->getBase(), expr->getIndex());
    }

    Type visitGenericSubscriptExpr(GenericSubscriptExpr *expr) {
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
    /// \param expr The expression that 'anchors' the pattern, which is used
    /// to create fresh type variables.
    Type getTypeForPattern(Pattern *pattern, Expr *expr) {
      switch (pattern->getKind()) {
      case PatternKind::Paren:
        // Parentheses don't affect the type.
        return getTypeForPattern(cast<ParenPattern>(pattern)->getSubPattern(),
                                 expr);

      case PatternKind::Any:
        // For a pattern of unknown type, create a new type variable.
        return CS.createTypeVariable(expr);

      case PatternKind::Named: {
        // For a named pattern without a type, create a new type variable
        // and use it as the type of the variable.
        auto tv = CS.createTypeVariable(expr);
        cast<NamedPattern>(pattern)->getDecl()->setType(tv);
        return tv;
      }

      case PatternKind::Typed:
        // For a typed pattern, simply return the type of the pattern.
        // FIXME: Error recovery if the type is an error type?
        return cast<TypedPattern>(pattern)->getTypeLoc().getType();

      case PatternKind::Tuple: {
        auto tuplePat = cast<TuplePattern>(pattern);
        SmallVector<TupleTypeElt, 4> tupleTypeElts;
        tupleTypeElts.reserve(tuplePat->getNumFields());
        for (auto tupleElt : tuplePat->getFields()) {
          Type eltTy = getTypeForPattern(tupleElt.getPattern(), expr);
          auto name = findPatternName(tupleElt.getPattern());
          Type varArgBaseTy;
          if (tupleElt.isVararg()) {
            varArgBaseTy
              = cast<ArraySliceType>(eltTy.getPointer())->getBaseType();
          }
          tupleTypeElts.push_back(TupleTypeElt(eltTy, name, tupleElt.getInit(),
                                               varArgBaseTy));
        }
        return TupleType::get(tupleTypeElts, CS.getASTContext());
      }
      }

      llvm_unreachable("Unhandled pattern kind");
    }

    Type visitFuncExpr(FuncExpr *expr) {
      // Func expressions always have function type. In cases where a parameter
      // or return type is omitted, a fresh type variable is used to stand in
      // for that parameter or return type, allowing it to be inferred from
      // context.
      auto funcTy = expr->getBodyResultTypeLoc().getType();
      if (!funcTy) {
        // If no return type was specified, create a fresh type
        // variable for it.
        funcTy = CS.createTypeVariable(expr);
      }

      // Walk through the patterns in the func expression, backwards,
      // computing the type of each pattern (which may involve fresh type
      // variables where parameter types where no provided) and building the
      // eventual function type.
      auto patterns = expr->getArgParamPatterns();
      for (unsigned i = 0, e = patterns.size(); i != e; ++i) {
        Type paramTy = getTypeForPattern(patterns[e - i - 1], expr);
        funcTy = FunctionType::get(paramTy, funcTy, CS.getASTContext());
      }

      return funcTy;
    }

    Type visitPipeClosureExpr(PipeClosureExpr *expr) {
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
        funcTy = CS.createTypeVariable(expr);
      }

      // Walk through the patterns in the func expression, backwards,
      // computing the type of each pattern (which may involve fresh type
      // variables where parameter types where no provided) and building the
      // eventual function type.
      auto paramTy = getTypeForPattern(expr->getParams(), expr);
      funcTy = FunctionType::get(paramTy, funcTy, CS.getASTContext());

      return funcTy;
    }

    Type visitImplicitClosureExpr(ImplicitClosureExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Type visitModuleExpr(ModuleExpr *expr) {
      // Module expressions always have a fixed type.
      return expr->getType();
    }

    Type visitAddressOfExpr(AddressOfExpr *expr) {
      // The address-of operator produces an explicit lvalue
      // [byref(settable)] T from a (potentially implicit) settable lvalue S.
      // We model this with the constraint
      //
      //     S < [byref(implicit, settable)] T
      //
      // where T is a fresh type variable.
      auto tv = CS.createTypeVariable(expr);
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
      auto resultTy = CS.openType(expr->getElementTypeLoc().getType());
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
      return tc.getArraySliceType(outerBound.Brackets.Start, resultTy);
    }

    Type visitMetatypeExpr(MetatypeExpr *expr) {
      auto base = expr->getBase();

      // If this is an artificial MetatypeExpr, it's fully type-checked.
      if (!base) return expr->getType();

      auto tv = CS.createTypeVariable();
      CS.addConstraint(ConstraintKind::EqualRvalue, tv, base->getType(),
        CS.getConstraintLocator(expr, ConstraintLocator::RvalueAdjustment));

      return MetaTypeType::get(tv, CS.getASTContext());
    }

    Type visitOpaqueValueExpr(OpaqueValueExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Type visitZeroValueExpr(ZeroValueExpr *expr) {
      return CS.openType(expr->getType());
    }

    Type visitDefaultValueExpr(DefaultValueExpr *expr) {
      expr->setType(expr->getSubExpr()->getType());
      return expr->getType();
    }

    Type visitApplyExpr(ApplyExpr *expr) {
      ASTContext &Context = CS.getASTContext();

      // If the function subexpression has metatype type, this is a type
      // construction.
      // Note that matching the metatype type here, within constraint
      // generation, naturally restricts the use of metatypes to whatever
      // the constraint generator can eagerly evaluate.
      // FIXME: Specify this as a syntactic restriction on the form that one
      // can use for a coercion or type construction.
      if (isa<CallExpr>(expr)) {
        auto fnTy = CS.simplifyType(expr->getFn()->getType());
        if (auto metaTy = fnTy->getAs<MetaTypeType>()) {
          auto instanceTy = metaTy->getInstanceType();
          CS.addConstraint(ConstraintKind::Construction,
                           expr->getArg()->getType(), instanceTy,
                           CS.getConstraintLocator(expr, { }));
          return instanceTy;
        }
      }

      // The function subexpression has some rvalue type T1 -> T2 for fresh
      // variables T1 and T2.
      auto inputTy = CS.createTypeVariable(expr);
      auto outputTy = CS.createTypeVariable(expr);
      auto funcTy = FunctionType::get(inputTy, outputTy, Context);
      CS.addConstraint(ConstraintKind::EqualRvalue, funcTy,
        expr->getFn()->getType(),
        CS.getConstraintLocator(expr, ConstraintLocator::ApplyFunction));

      // The argument type must be convertible to the input type.
      CS.addConstraint(ConstraintKind::Conversion, expr->getArg()->getType(),
        inputTy,
        CS.getConstraintLocator(expr, ConstraintLocator::ApplyArgument));

      return outputTy;
    }

    Type getSuperType(ValueDecl *thisDecl,
                      SourceLoc diagLoc,
                      Diag<> diag_not_in_class,
                      Diag<> diag_no_base_class) {
      DeclContext *typeContext = thisDecl->getDeclContext()->getParent();
      assert(typeContext && "constructor without parent context?!");
      auto &tc = CS.getTypeChecker();
      const ClassType *classType
        = typeContext->getDeclaredTypeInContext()->getAs<ClassType>();
      if (!classType) {
        tc.diagnose(diagLoc, diag_not_in_class);
        return Type();
      }
      ClassDecl *classDecl = classType->getDecl();
      if (!classDecl->hasBaseClass()) {
        tc.diagnose(diagLoc, diag_no_base_class);
        return Type();
      }

      Type baseTy = classDecl->getBaseClass();
      if (thisDecl->getType()->is<MetaTypeType>())
        baseTy = MetaTypeType::get(baseTy, CS.getASTContext());
      return baseTy;
    }
    
    Type visitRebindThisInConstructorExpr(RebindThisInConstructorExpr *expr) {
      // The subexpression must be a supertype of 'this' type.
      CS.addConstraint(ConstraintKind::Subtype,
                       expr->getThis()->getType(),
                       expr->getSubExpr()->getType());
      // The result is void.
      return TupleType::getEmpty(CS.getASTContext());
    }
    
    Type visitIfExpr(IfExpr *expr) {
      // The condition expression must be convertible with getLogicValue.
      // We handle this type-check completely separately, because it has no
      // bearing on the results of the type-check of the expression containing
      // the ternary.
      Expr *condExpr = expr->getCondExpr();
      // FIXME: Mark expression as an error if this fails.
      CS.getTypeChecker().typeCheckCondition(condExpr, CS.DC);
      expr->setCondExpr(condExpr);

      // The branches must be convertible to a common type.
      ConstraintLocatorBuilder locatorBuilder(CS.getConstraintLocator(expr,
                                                                      { }));
      auto resultTy = CS.createTypeVariable(expr);
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
    
    Type visitUnsequencedTernaryExpr(UnsequencedTernaryExpr *E) {
      llvm_unreachable("this node should be eliminated by name binding");
    }
    Type visitUnsequencedAssignExpr(UnsequencedAssignExpr *E) {
      llvm_unreachable("this node should be eliminated by name binding");
    }
    Type visitImplicitConversionExpr(ImplicitConversionExpr *expr) {
      llvm_unreachable("Already type-checked");
    }
    
    Type visitCoerceExpr(CoerceExpr *expr) {
      // FIXME: Could split the system here.
      Type ty = expr->getTypeLoc().getType();
      CS.addConstraint(ConstraintKind::Conversion,
                       expr->getSubExpr()->getType(), ty,
                       CS.getConstraintLocator(expr, { }));
      return ty;
    }

    Type visitUncheckedDowncastExpr(UncheckedDowncastExpr *expr) {
      // FIXME: Open this type.
      return expr->getTypeLoc().getType();
    }

    Type visitUncheckedSuperToArchetypeExpr(
                                          UncheckedSuperToArchetypeExpr *expr) {
      llvm_unreachable("Already type-checked");
    }
    
    Type visitIsSubtypeExpr(IsSubtypeExpr *expr) {
      ASTContext &C = CS.getASTContext();

      // The subexpression must be castable to the destination type.
      auto tv = CS.createTypeVariable();
      CS.addConstraint(ConstraintKind::EqualRvalue, tv,
           expr->getSubExpr()->getType(),
           CS.getConstraintLocator(expr, ConstraintLocator::RvalueAdjustment));

      CS.addConstraint(ConstraintKind::Subtype,
                       expr->getTypeLoc().getType(), tv);
      
      // The result is Bool.
      auto &tc = CS.getTypeChecker();
      UnqualifiedLookup boolLookup(C.getIdentifier("Bool"),
                                   &tc.TU);
      if (!boolLookup.isSuccess()) {
        tc.diagnose(expr->getLoc(), diag::bool_type_broken);
        return nullptr;
      }
      TypeDecl *tyDecl = boolLookup.getSingleTypeResult();
      
      if (!tyDecl) {
        tc.diagnose(expr->getLoc(), diag::bool_type_broken);
        return nullptr;
      }
      
      return tyDecl->getDeclaredType();
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

    virtual std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      // Don't recur into array-new or default-value expressions.
      return {
        !isa<NewArrayExpr>(expr)
          && !isa<DefaultValueExpr>(expr),
        expr
      };
    }

    virtual Expr *walkToExprPost(Expr *expr) {
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
                                   UnstructuredUnresolvedType::get(TC.Context));
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
                                    dotIgnored->getDotLoc(), members, memberLoc,
                                    UnstructuredUnresolvedType::get(TC.Context));
        }
      }
      
      return expr;
    }
  };

  class ConstraintWalker : public ASTWalker {
    ConstraintGenerator &CG;

  public:
    ConstraintWalker(ConstraintGenerator &CG) : CG(CG) { }

    virtual std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      // For function expressions, we visit the closure first to assign a type
      // with appropriate type variables. We don't walk into the bodies.
      // Don't walk into the bodies of function expressions.
      if (auto func = dyn_cast<FuncExpr>(expr)) {
        auto type = CG.visitFuncExpr(func);
        func->setType(type);
        return { false, func };
      }

      // For closures containing only a single expression, the body participates
      // in type checking.
      if (auto closure = dyn_cast<PipeClosureExpr>(expr)) {
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

      // For unchecked downcast expressions, we visit the subexpression
      // separately.
      if (auto unchecked = dyn_cast<UncheckedDowncastExpr>(expr)) {
        auto type = CG.visitUncheckedDowncastExpr(unchecked);
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
    virtual Expr *walkToExprPost(Expr *expr) {
      if (auto closure = dyn_cast<PipeClosureExpr>(expr)) {
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
        expr->setType(type);
        return expr;
      }

      return nullptr;
    }

    /// \brief Ignore statements.
    virtual std::pair<bool, Stmt *> walkToStmtPre(Stmt *stmt) override {
      return { false, stmt };
    }

    /// \brief Ignore declarations.
    virtual bool walkToDeclPre(Decl *decl) { return false; }
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

