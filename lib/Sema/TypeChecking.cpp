//===--- TypeChecking.cpp - Type Checking ---------------------------------===//
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
// This file implements semantic analysis for expressions, and other pieces
// that require final type checking.  If this passes a translation unit with no
// errors, then it is good to go.
//
//===----------------------------------------------------------------------===//

#include "swift/Subsystems.h"
#include "TypeChecking.h"
#include "swift/AST/ASTVisitor.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/SourceMgr.h"
using namespace swift;

void TypeChecker::note(SMLoc Loc, const Twine &Message) {
  Context.SourceMgr.PrintMessage(Loc, Message, "note");
}
void TypeChecker::warning(SMLoc Loc, const Twine &Message) {
  Context.SourceMgr.PrintMessage(Loc, Message, "warning");
}
void TypeChecker::error(SMLoc Loc, const Twine &Message) {
  Context.setHadError();
  Context.SourceMgr.PrintMessage(Loc, Message, "error");
}


//===----------------------------------------------------------------------===//
// BindAndValidateClosureArgs - When a closure is formed, this walks an AST to
// update AnonClosureArgExpr to be of the right type.
//===----------------------------------------------------------------------===//

namespace {
struct RewriteAnonArgExpr {
  Type FuncInputTy;
  TypeChecker &TC;
  
  RewriteAnonArgExpr(Type funcInputTy, TypeChecker &tc)
    : FuncInputTy(funcInputTy), TC(tc) {}
  
  static Expr *WalkFn(Expr *E, Expr::WalkOrder Order, void *rewriter) {
    RewriteAnonArgExpr &Rewriter = *static_cast<RewriteAnonArgExpr*>(rewriter);
    Type FuncInputTy = Rewriter.FuncInputTy;
  
    if (Order == Expr::WalkOrder::PreOrder) {
      // If this is a ClosureExpr, don't walk into it.  This would find *its*
      // anonymous closure arguments, not ours.
      if (isa<ClosureExpr>(E)) return 0; // Don't recurse into it.
      
      // Otherwise, do recurse into it.  We handle anon args in the postorder
      // visitation.
      return E;
    }
  
    // If we found a closure argument, process it.
    AnonClosureArgExpr *A = dyn_cast<AnonClosureArgExpr>(E);
    if (A == 0) return E;  
    
    // If the input to the function is a non-tuple, only $0 is valid, if it is a
    // tuple, then $0..$N are valid depending on the number of inputs to the
    // tuple.
    unsigned NumInputArgs = 1;
    if (TupleType *TT = dyn_cast<TupleType>(FuncInputTy.getPointer()))
      NumInputArgs = TT->Fields.size();
    
    assert(A->Ty->is<DependentType>() && "Anon arg already has a type?");
    
    // Verify that the argument number isn't too large, e.g. using $4 when the
    // bound function only has 2 inputs.
    if (A->ArgNo >= NumInputArgs) {
      Rewriter.TC.error(A->Loc,
               "use of invalid anonymous argument, with number higher than"
               " # arguments to bound function");
      return 0;
    }
    
    // Assign the AnonDecls their actual concrete types now that we know the
    // context they are being used in.
    if (TupleType *TT = dyn_cast<TupleType>(FuncInputTy.getPointer())) {
      A->Ty = TT->getElementType(A->ArgNo);
    } else {
      assert(NumInputArgs == 1 && "Must have unary case");
      A->Ty = FuncInputTy;
    }
    return A;
  }
};
} // end anonymous namespace

/// BindAndValidateClosureArgs - The specified list of anonymous closure
/// arguments was bound to a closure function with the specified input
/// arguments.  Validate the argument list and, if valid, allocate and return
/// a pointer to the argument to be used for the ClosureExpr.
static bool
BindAndValidateClosureArgs(Expr *Body, Type FuncInput, TypeChecker &TC) {  
  RewriteAnonArgExpr Rewriter(FuncInput, TC);
  
  // Walk the body and rewrite any anonymous arguments.  Note that this
  // isn't a particularly efficient way to handle this, because we walk subtrees
  // even if they have no anonymous arguments.
  return Body->WalkExpr(RewriteAnonArgExpr::WalkFn, 0, &Rewriter) == 0;
}


//===----------------------------------------------------------------------===//
// SemaCoerceBottomUp
//===----------------------------------------------------------------------===//

namespace {
  /// SemaCoerceBottomUp - This class implements bottom-up semantic analysis
  /// (aka "root to leaf", using the type of "+" to infer the type of "a" in
  /// "a+1") of an already-existing expression tree.  This is performed when an
  /// expression with dependent type is used in a context that forces a specific
  /// type.  
  ///
  /// Each visit method reanalyzes the node to see if the type can be propagated
  /// into it.  If not, it returns it.  If so it checks to see if the type
  /// is contradictory (in which case it returns NULL) otherwise it applies the
  /// type (possibly recursively) and returns the new/updated expression.
  class SemaCoerceBottomUp : public ExprVisitor<SemaCoerceBottomUp, Expr*> {
    friend ExprVisitor<SemaCoerceBottomUp, Expr*>;
    TypeChecker &TC;
    Type DestTy;
    
    Expr *visitIntegerLiteralExpr(IntegerLiteralExpr *E) {
      assert(0 && "Integer literals never have dependent type!");
      return 0;
    }
    Expr *visitDeclRefExpr(DeclRefExpr *E) {
      return E;
    }
    
    Expr *visitOverloadSetRefExpr(OverloadSetRefExpr *E) {
      // If any decl that is in the overload set exactly matches the expected
      // type, then select it.
      // FIXME: Conversion ranking.
      for (ValueDecl *VD : E->Decls) {
        if (VD->Ty->isEqual(DestTy, TC.Context))
          return new (TC.Context) DeclRefExpr(VD, E->Loc, VD->Ty);
      }
      return E;
    }
    
    // If this is an UnresolvedMemberExpr, then this provides the type we've
    // been looking for!
    Expr *visitUnresolvedMemberExpr(UnresolvedMemberExpr *UME) {
      // The only valid type for an UME is a OneOfType.
      OneOfType *DT = DestTy->getAs<OneOfType>();
      if (DT == 0) {
        TC.error(UME->getLocStart(),
                 "dependent reference to member '" + UME->Name.str() +
                 "' cannot convert to '" + DestTy->getString() + "'");
        return 0;
      }
      
      // The oneof type must have an element of the specified name.
      OneOfElementDecl *DED = DT->getElement(UME->Name);
      if (DED == 0) {
        TC.error(UME->getLocStart(),
                 "type '" + DestTy->getString() + "' has no member named '" +
                 UME->Name.str() + "'");
        TC.note(DT->OneOfLoc, "type declared here");
        return 0;
      }
      
      // If it does, then everything is good, resolve the reference.
      return new (TC.Context) DeclRefExpr(DED, UME->ColonLoc, DED->Ty);
    }  
    
    Expr *visitTupleExpr(TupleExpr *E);
    
    Expr *visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *E) {
      return E;
    }
    Expr *visitUnresolvedDotExpr(UnresolvedDotExpr *E) {
      return E;
    }
    
    Expr *visitUnresolvedScopedIdentifierExpr
    (UnresolvedScopedIdentifierExpr *E) {
      assert(0 && "This node should be resolved already!");
    }
    
    Expr *visitTupleElementExpr(TupleElementExpr *E) {
      // TupleElementExpr is fully resolved.
      assert(0 && "This node doesn't exist for dependent types");
      return 0;
    }
    
    Expr *visitTupleShuffleExpr(TupleShuffleExpr *E) {
      // TupleElementExpr is fully resolved.
      assert(0 && "This node doesn't exist for dependent types");
      return 0;
    }

    
    Expr *visitCallExpr(CallExpr *E) {
      // If we have ":f(x)" and the result type of the call is a OneOfType, then
      // :f must be an element constructor for the oneof value.  Note that
      // handling this syntactically causes us to reject "(:f) x" as ambiguous.
      if (UnresolvedMemberExpr *UME =
            dyn_cast<UnresolvedMemberExpr>(E->Fn)) {
        if (OneOfType *DT = DestTy->getAs<OneOfType>()) {
          // The oneof type must have an element of the specified name.
          OneOfElementDecl *DED = DT->getElement(UME->Name);
          if (DED == 0 || !DED->Ty->is<FunctionType>()) {
            TC.error(UME->getLocStart(), "invalid type '" +
                     DestTy->getString() + "' to initialize member");
            return 0;
          }

          // FIXME: Preserve source locations.
          E->Fn = new (TC.Context) DeclRefExpr(DED, UME->ColonLoc, DED->Ty);
          if (TC.semaApplyExpr(E))
            return 0;
            
          return E;
        }
      }
      
      // FIXME: Given a CallExpr a(b) where "a" is an overloaded value, we
      // may be able to prune the overload set based on the known result type.
      // Doing this may allow the ambiguity to resolve by removing candidates
      // that caused the ambiguity.  For example if we know that the destination
      // type is 'int', and we had "int -> int" and "SomeTy -> float", we can
      // prune the second one, and then recursively apply 'int' to b.
      return E;
    }
    Expr *visitSequenceExpr(SequenceExpr *E) {
      assert(0 && "SequenceExprs should all be resolved by this pass");
      return 0;
    }

    Expr *visitFuncExpr(FuncExpr *E) {
      return E;      
    }

    Expr *visitClosureExpr(ClosureExpr *E) {
      return E;      
    }
    
    Expr *visitAnonClosureArgExpr(AnonClosureArgExpr *E) {
      return E;
    }

    Expr *visitUnaryExpr(UnaryExpr *E) {
      // TODO: If the function is an overload set and the result type that we're
      // coercing onto the binop is completely incompatible with some elements
      // of the overload set, trim them out.      
      return E;
    }

    Expr *visitBinaryExpr(BinaryExpr *E) {
      // TODO: If the function is an overload set and the result type that we're
      // coercing onto the binop is completely incompatible with some elements
      // of the overload set, trim them out.      
      return E;
    }
    
    SemaCoerceBottomUp(TypeChecker &tc, Type destTy) : TC(tc), DestTy(destTy) {
      assert(!DestTy->is<DependentType>());
    }
    Expr *doIt(Expr *E) {
      return visit(E);
    }
  public:
    
    /// convertToType - This is the main entrypoint to SemaCoerceBottomUp.
    static Expr *convertToType(Expr *E, Type DestTy, bool IgnoreAnonDecls,
                               TypeChecker &TC);

  private:
    static Expr *convertScalarToTupleType(Expr *E, TupleType *DestTy,
                                          unsigned FieldNo, TypeChecker &TC);
    static Expr *
    convertTupleToTupleType(Expr *E, unsigned NumExprElements,
                            TupleType *DestTy, TypeChecker &TC);
  };
} // end anonymous namespace.


Expr *SemaCoerceBottomUp::visitTupleExpr(TupleExpr *E) {
  // If we're providing a type for a tuple expr, we have a couple of
  // different cases.  If the tuple has a single element and the destination
  // type is not a tuple type, then this just recursively forces the scalar
  // type into the single element.
  if (E->isGroupingParen()) {
    E->SubExprs[0] = convertToType(E->SubExprs[0], DestTy, true, TC);
    if (E->SubExprs[0] == 0) return 0;
    
    E->Ty = E->SubExprs[0]->Ty;
    return E;
  }
  
  return convertToType(E, DestTy, true, TC);
}

/// convertTupleToTupleType - Given an expression that has tuple type, convert
/// it to have some other tuple type.
///
/// The caller gives us a list of the expressions named arguments and a count of
/// tuple elements for E in the IdentList+NumIdents array.  DestTy specifies the
/// type to convert to, which is known to be a TupleType.
Expr *
SemaCoerceBottomUp::convertTupleToTupleType(Expr *E, unsigned NumExprElements,
                                            TupleType *DestTy, TypeChecker &TC){
  
  // If the tuple expression or destination type have named elements, we
  // have to match them up to handle the swizzle case for when:
  //   (.y = 4, .x = 3)
  // is converted to type:
  //   (.x = int, .y = int)
  SmallVector<Identifier, 8> IdentList(NumExprElements);
  
  // Check to see if this conversion is ok by looping over all the destination
  // elements and seeing if they are provided by the input.
  
  // Keep track of which input elements are used.
  // TODO: Record where the destination elements came from in the AST.
  SmallVector<bool, 16> UsedElements(NumExprElements);
  SmallVector<int, 16>  DestElementSources(DestTy->Fields.size(), -1);

  if (TupleType *ETy = E->Ty->getAs<TupleType>()) {
    assert(ETy->Fields.size() == NumExprElements && "Expr #elements mismatch!");
    for (unsigned i = 0, e = ETy->Fields.size(); i != e; ++i)
      IdentList[i] = ETy->Fields[i].Name;
    
    // First off, see if we can resolve any named values from matching named
    // inputs.
    for (unsigned i = 0, e = DestTy->Fields.size(); i != e; ++i) {
      const TupleTypeElt &DestElt = DestTy->Fields[i];
      // If this destination field is named, first check for a matching named
      // element in the input, from any position.
      if (DestElt.Name.empty()) continue;
      
      int InputElement = -1;
      for (unsigned j = 0; j != NumExprElements; ++j)
        if (IdentList[j] == DestElt.Name) {
          InputElement = j;
          break;
        }
      if (InputElement == -1) continue;
      
      DestElementSources[i] = InputElement;
      UsedElements[InputElement] = true;
    }
  }
  
  // Next step, resolve (in order) unmatched named results and unnamed results
  // to any left-over unnamed input.
  unsigned NextInputValue = 0;
  for (unsigned i = 0, e = DestTy->Fields.size(); i != e; ++i) {
    // If we already found an input to satisfy this output, we're done.
    if (DestElementSources[i] != -1) continue;
    
    // Scan for an unmatched unnamed input value.
    while (1) {
      // If we didn't find any input values, we ran out of inputs to use.
      if (NextInputValue == NumExprElements)
        break;
      
      // If this input value is unnamed and unused, use it!
      if (!UsedElements[NextInputValue] && IdentList[NextInputValue].empty())
        break;
      
      ++NextInputValue;
    }
    
    // If we ran out of input values, we either don't have enough sources to
    // fill the dest (as in when assigning (1,2) to (int,int,int), or we ran out
    // and default values should be used.
    if (NextInputValue == NumExprElements) {
      if (DestTy->Fields[i].Init != 0) {
        // If the default initializer should be used, leave the
        // DestElementSources field set to -2.
        DestElementSources[i] = -2;
        continue;
      }        
     
      // If this is a TupleExpr (common case) get a more precise location for
      // the element we care about.
      SMLoc ErrorLoc = E->getLocStart();
      if (TupleExpr *TE = dyn_cast<TupleExpr>(E))
        ErrorLoc = TE->RParenLoc;
      
      if (DestTy->Fields[i].Name.empty())
        TC.error(ErrorLoc, "no value to initialize tuple element #" +
                 Twine(i) + " in expression of type '" +
                 E->Ty->getString() + "'");
      else
        TC.error(ErrorLoc, "no value to initialize tuple element '" +
                 DestTy->Fields[i].Name.str() + "' (#" + Twine(i) +
                 ") in expression of type '" + E->Ty->getString() + "'");
      return 0;
    }
    
    // Okay, we found an input value to use.
    DestElementSources[i] = NextInputValue;
    UsedElements[NextInputValue] = true;
  }
  
  // If there were any unused input values, we fail.
  for (unsigned i = 0, e = UsedElements.size(); i != e; ++i)
    if (!UsedElements[i]) {
      // If this is a TupleExpr (common case) get a more precise location for
      // the element we care about.
      SMLoc ErrorLoc = E->getLocStart();
      if (TupleExpr *TE = dyn_cast<TupleExpr>(E))
        if (Expr *SubExp = TE->SubExprs[i])
          ErrorLoc = SubExp->getLocStart();
      
    if (IdentList[i].empty())
      TC.error(ErrorLoc, "element #" + Twine(i) +
               " of tuple value not used when converting to type '" +
               DestTy->getString() + "'");
    else
      TC.error(ErrorLoc, "tuple element '" + IdentList[i].str() +
               "' (#" + Twine(i) + ") of tuple value not used when "
               "converting to type '" + DestTy->getString() + "'");
      return 0;
    }
  
  // It looks like the elements line up, walk through them and see if the types
  // either agree or can be converted.  If the expression is a TupleExpr, we do
  // this conversion in place.
  TupleExpr *TE = dyn_cast<TupleExpr>(E);
  if (TE && TE->NumSubExprs != 1 && TE->NumSubExprs == DestTy->Fields.size()) {
    SmallVector<Expr*, 8> OrigElts(TE->SubExprs,
                                         TE->SubExprs+TE->NumSubExprs);
    
    for (unsigned i = 0, e = DestTy->Fields.size(); i != e; ++i) {
      // Extract the input element corresponding to this destination element.
      unsigned SrcField = DestElementSources[i];
      assert(SrcField != ~0U && "dest field not found?");
      
      // If SrcField is -2, then the destination element should use its default
      // value.
      if (SrcField == -2U) {
        TE->SubExprs[i] = 0;
        continue;
      }
      
      // Check to see if the src value can be converted to the destination
      // element type.
      Expr *Elt = OrigElts[SrcField];
      
      Elt = convertToType(Elt, DestTy->getElementType(i), true, TC);
      // TODO: QOI: Include a note about this failure!
      if (Elt == 0) return 0;
      TE->SubExprs[i] = Elt;
      
      if (DestTy->Fields[i].Name.get()) {
        // Allocate the array on the first element with a name.
        if (TE->SubExprNames == 0)
          TE->SubExprNames =
          TC.Context.Allocate<Identifier>(DestTy->Fields.size());
        
        TE->SubExprNames[i] = DestTy->Fields[i].Name;
      } else if (TE->SubExprNames)
        TE->SubExprNames[i] = Identifier();
    }
    
    // Okay, we updated the tuple in place.
    E->Ty = DestTy;
    return E;
  }
  
  // Otherwise, if it isn't a tuple literal, we unpack the source elementwise so
  // we can do elementwise conversions as needed, then rebuild a new TupleExpr
  // of the right destination type.
  TupleType *ETy = E->Ty->getAs<TupleType>();
  SmallVector<int, 16> NewElements(DestTy->Fields.size());
  
  for (unsigned i = 0, e = DestTy->Fields.size(); i != e; ++i) {
    // Extract the input element corresponding to this destination element.
    unsigned SrcField = DestElementSources[i];
    assert(SrcField != ~0U && "dest field not found?");
    
    if (SrcField == -2U) {
      // Use the default element for the tuple.
      NewElements[i] = -1;
      continue;
    }
    
    if (ETy->getElementType(SrcField)->getCanonicalType(TC.Context) !=
        DestTy->getElementType(i)->getCanonicalType(TC.Context)) {
      TC.error(E->getLocStart(), "element #" + Twine(i) +
               " of tuple value has type '" +
               ETy->getElementType(SrcField)->getString() +
               "', but expected type '" + 
               DestTy->getElementType(i)->getString() + "'");
      return 0;
    }
    
    NewElements[i] = SrcField;
  }
  
  // If we got here, the type conversion is successful, create a new TupleExpr.  
  ArrayRef<int> Mapping = TC.Context.AllocateCopy(NewElements);
  
  return new (TC.Context) TupleShuffleExpr(E, Mapping, DestTy);
}

/// convertScalarToTupleType - Convert the specified expression to the specified
/// tuple type, which is known to be initializable with one element.
Expr *SemaCoerceBottomUp::convertScalarToTupleType(Expr *E, TupleType *DestTy,
                                                   unsigned ScalarField,
                                                   TypeChecker &TC) {
  // If the destination is a tuple type with at most one element that has no
  // default value, see if the expression's type is convertable to the
  // element type.  This handles assigning 4 to "(a = 4, b : int)".
  Type ScalarType = DestTy->getElementType(ScalarField);
  Expr *ERes = convertToType(E, ScalarType, false, TC);
  if (ERes == 0) return 0;
  
  unsigned NumFields = DestTy->Fields.size();
  
  // Must allocate space for the AST node.
  Expr **NewSE = TC.Context.Allocate<Expr*>(NumFields);
  
  bool NeedsNames = false;
  for (unsigned i = 0, e = NumFields; i != e; ++i) {
    if (i == (unsigned)ScalarField)
      NewSE[i] = ERes;
    else
      NewSE[i] = 0;
    
    NeedsNames |= DestTy->Fields[i].Name != Identifier();
  }
  
  // Handle the name if the element is named.
  Identifier *NewName = 0;
  if (NeedsNames) {
    NewName = TC.Context.Allocate<Identifier>(NumFields);
    for (unsigned i = 0, e = NumFields; i != e; ++i)
      NewName[i] = DestTy->Fields[i].Name;
  }
  
  return new (TC.Context) TupleExpr(E->getLocStart(), NewSE, NewName,
                                    NumFields, SMLoc(), false, DestTy);
}

/// convertToType - This is the recursive implementation of
/// ConvertToType.  It does produces diagnostics and returns null on failure.
///
/// NOTE: This needs to be kept in synch with getConversionRank in Expr.cpp.
///
Expr *SemaCoerceBottomUp::convertToType(Expr *E, Type DestTy,
                                        bool IgnoreAnonDecls, TypeChecker &TC) {
  // If we have an exact match, we're done.
  if (E->Ty->getCanonicalType(TC.Context) ==
         DestTy->getCanonicalType(TC.Context))
    return E;
  
  assert(!DestTy->is<DependentType>() &&
         "Result of conversion can't be dependent");

  // If the expression is a grouping parenthesis and it has a dependent type,
  // just force the type through it, regardless of what DestTy is.
  if (TupleExpr *TE = dyn_cast<TupleExpr>(E))
    if (TE->isGroupingParen()) {
      TE->SubExprs[0] = convertToType(TE->SubExprs[0], DestTy,
                                      IgnoreAnonDecls, TC);
      if (TE->SubExprs[0] == 0) return 0;
      TE->Ty = TE->SubExprs[0]->Ty;
      return TE;
    }
  
  if (TupleType *TT = DestTy->getAs<TupleType>()) {
    // Type conversions are carefully ranked so that they "do the right thing",
    // because they can be highly ambiguous.  For example, consider something
    // like foo(4, 5) when foo is declared to take ((int,int=3), int=6).  This
    // could be parsed as either ((4,5), 6) or ((4,3),5), but the later one is
    // the "right" answer.
    
    // If the element of the tuple has dependent type and is a TupleExpr, try to
    // convert it.
    if (TupleExpr *TE = dyn_cast<TupleExpr>(E))
      return convertTupleToTupleType(TE, TE->NumSubExprs, TT,TC);

    // If the is a scalar to tuple conversion, form the tuple and return it.
    int ScalarFieldNo = TT->getFieldForScalarInit();
    if (ScalarFieldNo != -1)
      return convertScalarToTupleType(E, TT, ScalarFieldNo, TC);
    
    // If the input is a tuple and the output is a tuple, see if we can convert
    // each element.
    if (TupleType *ETy = E->Ty->getAs<TupleType>())
      return convertTupleToTupleType(E, ETy->Fields.size(), TT, TC);
  }
  
  // Otherwise, check to see if this is an auto-closure case.  This case happens
  // when we convert an expression E to a function type whose result is E's
  // type.
  if (FunctionType *FT = DestTy->getAs<FunctionType>()) {
    // If we bound any anonymous closure arguments, validate them and resolve
    // their types.
    if (!IgnoreAnonDecls && BindAndValidateClosureArgs(E, FT->Input, TC))
      return 0;

    // If there are any live anonymous closure arguments, this level will use
    // them and remove them.  When binding something like $0+$1 to
    // (int,int)->(int,int)->() the arguments bind to the first level, not the
    // inner level.  To handle this, we ignore anonymous decls in the recursive
    // case here.
    Expr *ERes = convertToType(E, FT->Result, true, TC);
    if (ERes == 0) return 0;
  
    // Now that the AnonClosureArgExpr's potentially have a type, redo semantic
    // analysis from the leaves of the expression tree up.
    ERes = TC.typeCheckExpression(ERes);
    if (ERes == 0)
      return 0;
    
    return new (TC.Context) ClosureExpr(ERes, DestTy);
  }
  
  // If the input expression has a dependent type, then there are two cases:
  // first this could be an AnonDecl whose type will be specified by a larger
  // context, second, this could be a context sensitive expression value like
  // :foo.  If this is a context sensitive expression, propagate the type down
  // into the subexpression.
  if (E->Ty->is<DependentType>())
    return SemaCoerceBottomUp(TC, DestTy).doIt(E);
  
  // Could not do the conversion.
  TC.error(E->getLocStart(), "invalid conversion from type '" +
           E->Ty->getString() + "' to '" + DestTy->getString() + "'");
  return 0;
}



Expr *TypeChecker::convertToType(Expr *E, Type DestTy) {
  return SemaCoerceBottomUp::convertToType(E, DestTy, false, *this);
}

//===----------------------------------------------------------------------===//
// Type Validation
//===----------------------------------------------------------------------===//

/// validateType - Recursively check to see if the type of a decl is valid.  If
/// not, diagnose the problem and collapse it to an ErrorType.
bool TypeChecker::validateType(ValueDecl *VD) {
  if (!validateType(VD->Ty)) return false;
  
  VD->Ty = ErrorType::get(Context);
  return true;
}

/// validateType - Types can contain expressions (in the default values for
/// tuple elements), and thus need semantic analysis to ensure that these
/// expressions are valid and that they have the appropriate conversions etc.
///
/// This returns true if the type is invalid.
bool TypeChecker::validateType(Type InTy) {
  assert(InTy && "Cannot validate null types!");

  TypeBase *T = InTy.getPointer();
  // FIXME: Verify that these aren't circular and infinite size.
  
  // If a type has a canonical type, then it is known safe.
  if (T->hasCanonicalTypeComputed()) return false;

  bool IsInvalid = false;
  
  switch (T->Kind) {
  case TypeKind::Error:
    // Error already diagnosed.
    return true;
  case TypeKind::BuiltinInt1:
  case TypeKind::BuiltinInt8:
  case TypeKind::BuiltinInt16:
  case TypeKind::BuiltinInt32:
  case TypeKind::BuiltinInt64:
  case TypeKind::Dependent:
    return false;
  case TypeKind::OneOf:
    for (OneOfElementDecl *Elt : cast<OneOfType>(T)->Elements) {
      // Ignore element decls that have no associated type.
      if (Elt->ArgumentType.isNull())
        continue;
      
      IsInvalid = validateType(Elt);
      if (IsInvalid) break;
    }
    break;
  case TypeKind::NameAlias:
    IsInvalid = validateType(cast<NameAliasType>(T)->TheDecl->UnderlyingTy);
    if (IsInvalid)
      cast<NameAliasType>(T)->TheDecl->UnderlyingTy = ErrorType::get(Context);
    break;
  case TypeKind::Tuple: {
    TupleType *TT = cast<TupleType>(T);
    
    // Okay, we found an uncanonicalized tuple type, which might have default
    // values.  If so, we'll potentially have to update it.
    for (unsigned i = 0, e = TT->Fields.size(); i != e; ++i) {
      // The element has *at least* a type or an initializer, so we start by
      // verifying each individually.
      Type EltTy = TT->Fields[i].Ty;
      if (EltTy && validateType(EltTy)) {
        IsInvalid = true;
        break;
      }

      Expr *EltInit = TT->Fields[i].Init;
      if (EltInit == 0) continue;
      
      SMLoc InitLoc = EltInit->getLocStart();
      checkBody(EltInit, EltTy);
      if (EltInit == 0) {
        note(InitLoc, "while converting default tuple value to element type");
        IsInvalid = true;
        break;
      }
        
      // If both a type and an initializer are specified, make sure the
      // initializer's type agrees with the (redundant) type.
      assert(EltTy.isNull() || EltTy->isEqual(EltInit->Ty, Context));
      EltTy = EltInit->Ty;

      TT->updateInitializedElementType(i, EltTy, EltInit);
    }
    break;
  }
      
  case TypeKind::Function: {
    FunctionType *FT = cast<FunctionType>(T);
    IsInvalid = validateType(FT->Input) || validateType(FT->Result);
    break;
  }
  case TypeKind::Array:
    ArrayType *AT = cast<ArrayType>(T);
    IsInvalid = validateType(AT->Base);
    // FIXME: We need to check AT->Size! (It also has to be convertible to int).
    break;
  }

  // If we determined that this type is invalid, erase it in the caller.
  if (IsInvalid) {
    InTy = ErrorType::get(Context);
    return true;
  }

  // Now that we decided that this type is ok, get the canonical type for it so
  // that we never reanalyze it again.
  // If it is ever a performance win to avoid computing canonical types, we can
  // just keep a SmallPtrSet of analyzed Types in TypeChecker.
  InTy->getCanonicalType(Context);
  
  // FIXME: This isn't good enough: top-level stuff can have these as well and
  // their types need to be resolved at the end of name binding.  Perhaps we
  // should require them to have explicit types even if they have values and 
  // let the value mismatch be detected at typechecking time? 
  return false;
}

//===----------------------------------------------------------------------===//
// Type Checking Entrypoint
//===----------------------------------------------------------------------===//

/// validateAttributes - Check that the func/var declaration attributes are ok.
void TypeChecker::validateAttributes(DeclAttributes &Attrs, Type Ty) {
  // If the decl is a unary operator, then it must be a function whose input is
  // a single element tuple.
  if (Attrs.isUnary) {
    bool IsError = true;
    if (FunctionType *FT = dyn_cast<FunctionType>(Ty.getPointer()))
      if (TupleType *TT = dyn_cast<TupleType>(FT->Input.getPointer()))
        IsError = TT->Fields.size() != 1;
    if (IsError) {
      error(Attrs.LSquareLoc, "function with 'unary' specified must take "
            "a single element tuple as input");
      Attrs.isUnary = false;
      // FIXME: Set the 'isError' bit on the decl.
    }
  }
  
  // If the decl has an infix precedence specified, then it must be a function
  // whose input is a two element tuple.
  if (Attrs.InfixPrecedence != -1) {
    bool IsError = true;
    if (FunctionType *FT = dyn_cast<FunctionType>(Ty.getPointer()))
      if (TupleType *TT = dyn_cast<TupleType>(FT->Input.getPointer()))
        IsError = TT->Fields.size() != 2;
    if (IsError) {
      error(Attrs.LSquareLoc, "function with 'infix' specified must take "
            "a two element tuple as input");
      Attrs.InfixPrecedence = -1;
      // FIXME: Set the 'isError' bit on the decl.
    }
  }
}

/// DiagnoseUnresolvedTypes - This function is invoked on all nodes in an
/// expression tree checking to make sure they don't contain any DependentTypes.
static Expr *DiagnoseUnresolvedTypes(Expr *E, Expr::WalkOrder Order,
                                     void *Data) {
  TypeChecker &TC = *(TypeChecker*)Data;
  
  // Ignore the preorder walk.  We'd rather diagnose use of unresolved types
  // during the postorder walk so that the inner most expressions are diagnosed
  // before the outermost ones.
  if (Order == Expr::WalkOrder::PreOrder)
    return E;
  
  assert(!isa<SequenceExpr>(E) && "Should have resolved this");
  
  // Use is to strip off sugar.
  if (!E->Ty->is<DependentType>())
    return E;
  
  TC.error(E->getLocStart(),
           "ambiguous expression was not resolved to a concrete type");
  return 0;
}


/// checkBody - Type check an expression that is used in a top-level
/// context like a var/func body, or tuple default value.  If DestTy is
/// specified, the expression is coerced to the requested type.
///
/// If the body turns out to be a sequence, this returns the single element
/// with the excess in the provided smallvector.
void TypeChecker::checkBody(Expr *&E, Type DestTy) {
  assert(E != 0 && "Can't check a null body!");
  E = typeCheckExpression(E);
  if (E == 0) return;
  
  if (DestTy)
    E = convertToType(E, DestTy);
  
  // Check the initializer/body to make sure that we succeeded in resolving
  // all of the types contained within it.  We should not have any
  // DependentType's left for subexpressions.
  if (E)
    E = E->WalkExpr(DiagnoseUnresolvedTypes, 0, this);
}

void TypeChecker::typeCheck(TypeAliasDecl *TAD) {
  validateType(TAD->getAliasType(Context));
}

void TypeChecker::typeCheckERD(ElementRefDecl *ERD) {
  // If the type is already resolved we're done.  ElementRefDecls are simple.
  if (!ERD->Ty->is<DependentType>()) return;
  
  if (Type T = ElementRefDecl::getTypeForPath(ERD->VD->Ty, ERD->AccessPath))
    ERD->Ty = T;
  else {
    error(ERD->getLocStart(), "'" + ERD->Name.str() +
          "' is an invalid index for '" + ERD->VD->Ty->getString() +
          "'");
    // FIXME: This should be "invalid"
    ERD->Ty = TupleType::getEmpty(Context);
  }
}

bool TypeChecker::validateVarName(Type Ty, DeclVarName *Name) {
  // Check for a type specifier mismatch on this level.
  assert(Ty && "This lookup should never fail");

  // If this is a simple varname, then it matches any type, and we're done.
  if (Name->isSimple())
    return false;

  // If we're peering into an unresolved type, we can't analyze it yet.
  if (Ty->is<DependentType>()) return false;

  // If we have a single-element oneof (like a struct) then we allow matching
  // the struct elements with the tuple syntax.
  if (OneOfType *OOT = Ty->getAs<OneOfType>())
    if (OOT->hasSingleElement())
      Ty = OOT->getElement(0)->ArgumentType;
  
  // If we have a complex case, Ty must be a tuple and the name specifier must
  // have the correct number of elements.
  TupleType *AccessedTuple = Ty->getAs<TupleType>();
  if (AccessedTuple == 0) {
    error(Name->LPLoc, "name specifier matches '" + Ty->getString() +
          "' which is not a tuple");
    return true;
  }

  // Verify the # elements line up.
  if (Name->Elements.size() != AccessedTuple->Fields.size()) {
    error(Name->LPLoc, "name specifier matches '" + Ty->getString() +
          "' which requires " + Twine(AccessedTuple->Fields.size()) +
          " names, but has " + Twine(Name->Elements.size()));
    return true;
  }
  
  // Okay, everything looks good at this level, recurse.
  for (unsigned i = 0, e = Name->Elements.size(); i != e; ++i) {
    if (validateVarName(AccessedTuple->Fields[i].Ty, Name->Elements[i]))
      return true;
  }

  return false;
}

void TypeChecker::typeCheckVarDecl(VarDecl *VD) {
  // Type check the ValueDecl part of a VarDecl.
  if (typeCheckValueDecl(VD))
    return;
  
  // If the VarDecl had a name specifier, verify that it lines up with the
  // actual type of the VarDecl.
  if (VD->NestedName && validateVarName(VD->Ty, VD->NestedName))
    VD->NestedName = 0;
}


bool TypeChecker::typeCheckValueDecl(ValueDecl *VD) {
  if (validateType(VD)) {
    VD->Init = 0;
    return true;
  }

  // Validate that the initializers type matches the expected type.
  if (VD->Init == 0) {
    // If we have no initializer and the type is dependent, then the initializer
    // was invalid and removed.
    if (VD->Ty->is<DependentType>()) return true;
  } else if (VD->Ty->is<DependentType>()) {
    checkBody(VD->Init, 0);
    if (VD->Init == 0)
      note(VD->getLocStart(),
           "while converting 'var' initializer to declared type");
    else
      VD->Ty = VD->Init->Ty;
  } else {
    // If both a type and an initializer are specified, make sure the
    // initializer's type agrees (or converts) to the redundant type.
    checkBody(VD->Init, VD->Ty);
    
    if (VD->Init == 0) {
      if (isa<VarDecl>(VD))
        note(VD->getLocStart(),
             "while converting 'var' initializer to declared type");
    }
  }
  
  validateAttributes(VD->Attrs, VD->Ty);
  return false;
}

/// performTypeChecking - Once parsing and namebinding are complete, these
/// walks the AST to resolve types and diagnose problems therein.
///
/// FIXME: This should be moved out to somewhere else.
void swift::performTypeChecking(TranslationUnitDecl *TUD, ASTContext &Ctx) {
  TypeChecker TC(Ctx);
  
  // Type check the top-level BraceExpr.  This sorts out any top-level
  // expressions and recursively processes the rest of the translation unit.
  TC.typeCheckTranslationUnit(TUD);
}
