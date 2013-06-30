//===--- TypeCheckPattern.cpp - Type Checking for Patterns ----------------===//
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
// This file implements semantic analysis for patterns, analysing a
// pattern tree in both bottom-up and top-down ways.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/Attr.h"
#include "swift/AST/ExprHandle.h"
#include "swift/AST/ASTVisitor.h"
using namespace swift;

namespace {

class ResolvePattern : public ASTVisitor<ResolvePattern,
                                         /*ExprRetTy=*/Pattern*,
                                         /*StmtRetTy=*/void,
                                         /*DeclRetTy=*/void,
                                         /*PatternRetTy=*/Pattern*>
{
public:
  ASTContext &C;
  
  ResolvePattern(ASTContext &C) : C(C) {}
  
  // Handle productions that are always leaf patterns or are already resolved.
#define ALWAYS_RESOLVED_PATTERN(Id) \
  Pattern *visit##Id##Pattern(Id##Pattern *P) { return P; }
  ALWAYS_RESOLVED_PATTERN(Named)
  ALWAYS_RESOLVED_PATTERN(Any)
  ALWAYS_RESOLVED_PATTERN(Isa)
  ALWAYS_RESOLVED_PATTERN(Paren)
  ALWAYS_RESOLVED_PATTERN(Tuple)
  ALWAYS_RESOLVED_PATTERN(NominalType)
  ALWAYS_RESOLVED_PATTERN(Typed)
#undef ALWAYS_RESOLVED_PATTERN

  Pattern *visitVarPattern(VarPattern *P) {
    Pattern *newSub = visit(P->getSubPattern());
    P->setSubPattern(newSub);
    return P;
  }
  
  Pattern *visitExprPattern(ExprPattern *P) {
    if (P->isResolved())
      return P;
    
    // Try to convert to a pattern.
    Pattern *exprAsPattern = visit(P->getSubExpr());
    // If we failed, keep the ExprPattern as is.
    if (!exprAsPattern) {
      P->setResolved(true);
      return P;
    }
    return exprAsPattern;
  }
  
  // Most exprs remain exprs and should be wrapped in ExprPatterns.
  Pattern *visitExpr(Expr *E) {
    return nullptr;
  }
  
  // Unwrap UnresolvedPatternExprs.
  Pattern *visitUnresolvedPatternExpr(UnresolvedPatternExpr *E) {
    return visit(E->getSubPattern());
  }
  
  // Parens and tuples have to become patterns if they contain any patterns. If
  // they have only exprs as children, a tuple pattern or expr would be
  // equivalent, but it avoids some destructuring if we leave them as leaf
  // exprs.
  Pattern *visitParenExpr(ParenExpr *E) {
    if (Pattern *subPattern = visit(E->getSubExpr()))
      return new (C) ParenPattern(E->getLParenLoc(), subPattern,
                                  E->getRParenLoc());
    return nullptr;
  }
  
  Pattern *visitTupleExpr(TupleExpr *E) {
    SmallVector<Pattern*, 4> patterns;
    bool hasPattern = false;
    
    for (auto *subExpr : E->getElements()) {
      Pattern *pattern = visit(subExpr);
      hasPattern |= bool(pattern);
      patterns.push_back(pattern);
    }
    
    // If there were no subpatterns, leave the entire tuple as a leaf
    // expression.
    if (!hasPattern)
      return nullptr;
    
    // Otherwise, construct a TuplePattern.
    // FIXME: Carry over field labels.
    SmallVector<TuplePatternElt, 4> patternElts;
    
    for (unsigned i = 0, e = E->getNumElements(); i < e; ++i) {
      Pattern *pattern = patterns[i];
      if (!pattern)
        pattern = new (C) ExprPattern(E->getElement(i),
                                      /*isResolved*/ true,
                                      /*matchFn*/ false);
      patternElts.push_back(TuplePatternElt(pattern));
    }
    
    return TuplePattern::create(C, E->getLoc(),
                                patternElts, E->getRParenLoc());
  }
  
  // TODO: Call syntax 'T(x...)' forms a pattern if 'T' is a type and '(x...)' is a
  // TuplePattern with labeled fields.
  /*
  Pattern *visitCallExpr(CallExpr *E) {
    // The arguments must form a tuple with all named keys.
    auto *argTuple = dyn_cast<TupleExpr>(E->getArg());
    if (!argTuple)
      return nullptr;
    if (!argTuple->getElementNames())
      return nullptr;
    for (unsigned i = 0, e = argTuple->getNumElements(); i < e; ++i)
      if (!argTuple->getElementName(i).get())
        return nullptr;
    ...;
  }
   */
};

} // end anonymous namespace

/// Perform top-down syntactic disambiguation of a pattern. Where ambiguous
/// expr/pattern productions occur (tuples, function calls, etc.), favor the
/// pattern interpretation if it forms a valid pattern; otherwise, leave it as
/// an expression.
Pattern *TypeChecker::resolvePattern(Pattern *P) {
  return ResolvePattern(Context).visit(P);
}

/// Perform bottom-up type-checking on a pattern.  If this returns
/// false, the type of the pattern will have been set.  If allowUnknownTypes is
/// true, then this accepts "any" and "named" patterns, setting their type to
/// UnresolvedType.
bool TypeChecker::typeCheckPattern(Pattern *P, DeclContext *dc,
                                   bool isFirstPass,
                                   bool allowUnknownTypes) {
  switch (P->getKind()) {
  // Type-check paren patterns by checking the sub-pattern and
  // propagating that type out.
  case PatternKind::Paren: {
    Pattern *SP = cast<ParenPattern>(P)->getSubPattern();
    if (typeCheckPattern(SP, dc, isFirstPass, allowUnknownTypes)) {
      P->setType(ErrorType::get(Context));
      return true;
    }
    P->setType(SP->getType());
    return false;
  }

  // If we see an explicit type annotation, coerce the sub-pattern to
  // that type.
  case PatternKind::Typed: {
    bool hadError = false;
    TypedPattern *TP = cast<TypedPattern>(P);
    if (validateType(TP->getTypeLoc())) {
      TP->setType(ErrorType::get(Context));
      hadError = true;
    } else {
      TP->setType(TP->getTypeLoc().getType());
    }
    hadError |= coerceToType(TP->getSubPattern(), dc, P->getType());
    return hadError;
  }

  // A wildcard or name pattern cannot appear by itself in a context
  // which requires an explicit type.
  case PatternKind::Any:
  case PatternKind::Named:
    // If we're type checking this pattern in a context that can provide type
    // information, then the lack of type information is not an error.  Just set
    // our type to UnresolvedType.
    if (allowUnknownTypes) {
      P->setType(Context.TheUnstructuredUnresolvedType);
      return false;
    }
      
    diagnose(P->getLoc(), diag::cannot_infer_type_for_pattern);
    P->setType(ErrorType::get(Context));
    return true;

  // A tuple pattern propagates its tuple-ness out.
  case PatternKind::Tuple: {
    bool hadError = false;
    SmallVector<TupleTypeElt, 8> typeElts;

    for (TuplePatternElt &elt : cast<TuplePattern>(P)->getFields()) {
      Type type;
      ExprHandle *init = elt.getInit();
      Pattern *pattern = elt.getPattern();
      if (typeCheckPattern(pattern, dc, isFirstPass, allowUnknownTypes)) {
        hadError = true;
      } else {
        type = pattern->getType();
      }

      if (init && !isFirstPass && !init->alreadyChecked()) {
        Expr *e = init->getExpr();
        typeCheckExpression(e, dc, type);
        init->setExpr(e, true);
      }

      typeElts.push_back(TupleTypeElt(type, pattern->getBoundName(), init,
                                      elt.getVarargBaseType()));
    }

    if (hadError) {
      P->setType(ErrorType::get(Context));
      return true;
    }
    P->setType(TupleType::get(typeElts, Context));
    return false;
  }

#define PATTERN(Id, Parent)
#define REFUTABLE_PATTERN(Id, Parent) case PatternKind::Id:
#include "swift/AST/PatternNodes.def"
    llvm_unreachable("not implemented");
  }
  llvm_unreachable("bad pattern kind!");
}

/// Perform top-down type coercion on the given pattern.
bool TypeChecker::coerceToType(Pattern *P, DeclContext *dc, Type type) {
  switch (P->getKind()) {
  // For parens and vars, just set the type annotation and propagate inwards.
  case PatternKind::Paren:
    P->setType(type);
    return coerceToType(cast<ParenPattern>(P)->getSubPattern(), dc, type);
  case PatternKind::Var:
    P->setType(type);
    return coerceToType(cast<VarPattern>(P)->getSubPattern(), dc, type);

  // If we see an explicit type annotation, coerce the sub-pattern to
  // that type.
  case PatternKind::Typed: {
    TypedPattern *TP = cast<TypedPattern>(P);
    bool hadError = false;
    if (validateType(TP->getTypeLoc())) {
      TP->overwriteType(ErrorType::get(Context));
      hadError = true;
    } else {
      TP->setType(TP->getTypeLoc().getType());
      if (!type->isEqual(TP->getType()) && !type->is<ErrorType>()) {
        // Complain if the types don't match exactly.
        // TODO: allow implicit conversions?
        diagnose(P->getLoc(), diag::pattern_type_mismatch_context, type);
        hadError = true;
      }
    }

    hadError |= coerceToType(TP->getSubPattern(), dc, TP->getType());
    return hadError;
  }

  // For wildcard and name patterns, just set the type.
  case PatternKind::Named: {
    NamedPattern *NP = cast<NamedPattern>(P);
    NP->getDecl()->overwriteType(type);

    P->setType(type);
    return false;
  }
  case PatternKind::Any:
    P->setType(type);
    return false;

  // We can match a tuple pattern with a tuple type.
  // TODO: permit implicit conversions?
  case PatternKind::Tuple: {
    TuplePattern *TP = cast<TuplePattern>(P);
    bool hadError = false;

    if (type->is<ErrorType>())
      hadError = true;

    // The context type must be a tuple.
    TupleType *tupleTy = type->getAs<TupleType>();
    if (!tupleTy && !hadError) {
      diagnose(TP->getLParenLoc(), diag::tuple_pattern_in_non_tuple_context,
               type);
      hadError = true;
    }

    // The number of elements must match exactly.
    // TODO: incomplete tuple patterns, with some syntax.
    if (!hadError && tupleTy->getFields().size() != TP->getNumFields()) {
      diagnose(TP->getLParenLoc(), diag::tuple_pattern_length_mismatch, type);
      hadError = true;
    }

    // Coerce each tuple element to the respective type.
    // TODO: detect and diagnose shuffling
    // TODO: permit shuffling
    P->setType(type);

    for (unsigned i = 0, e = TP->getNumFields(); i != e; ++i) {
      TuplePatternElt &elt = TP->getFields()[i];
      Pattern *pattern = elt.getPattern();

      Type CoercionType;
      if (hadError)
        CoercionType = ErrorType::get(Context);
      else
        CoercionType = tupleTy->getFields()[i].getType();

      hadError |= coerceToType(pattern, dc, CoercionType);

      // Type-check the initialization expression.
      if (ExprHandle *initHandle = elt.getInit()) {
        Expr *init = initHandle->getExpr();
        if (initHandle->alreadyChecked()) {
          // Nothing to do
        } else if (typeCheckExpression(init, dc, CoercionType)) {
          initHandle->setExpr(nullptr, true);
        } else {
          initHandle->setExpr(init, true);
        }
      }
    }

    return hadError;
  }

  // Coerce expressions to the type.
  // TODO: Look up a match operator to resolve the type of the expression.
  case PatternKind::Expr: {
    ExprPattern *EP = dyn_cast<ExprPattern>(P);
    Expr *subExpr = EP->getSubExpr();
    if (typeCheckExpression(subExpr, dc, type))
      return true;
    EP->setSubExpr(subExpr);
    EP->setType(type);
    return false;
  }
      
  // Coerce an 'is' pattern by determining the cast kind.
  case PatternKind::Isa: {
    // TODO: Resolve the cast kind.
    P->setType(type);
    return false;
  }
      
  case PatternKind::NominalType:
    // TODO: Check the type against the coerced type, then coerce the
    // subpatterns to the field types.
    llvm_unreachable("not implemented");
  }
  llvm_unreachable("bad pattern kind!");
}
