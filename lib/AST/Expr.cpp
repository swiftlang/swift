//===--- Expr.cpp - Swift Language Expression ASTs ------------------------===//
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
//  This file implements the Expr class and subclasses.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Expr.h"
#include "swift/AST/AST.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/PrettyStackTrace.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/Support/raw_ostream.h"
using namespace swift;

//===----------------------------------------------------------------------===//
// Expr methods.
//===----------------------------------------------------------------------===//

// Only allow allocation of Stmts using the allocator in ASTContext.
void *Expr::operator new(size_t Bytes, ASTContext &C,
                         unsigned Alignment) throw() {
  return C.Allocate(Bytes, Alignment);
}

// Helper functions to verify statically whether the getSourceRange()
// function has been overridden.
typedef const char (&TwoChars)[2];

template<typename Class> 
inline char checkSourceRangeType(SourceRange (Class::*)() const);

inline TwoChars checkSourceRangeType(SourceRange (Expr::*)() const);

SourceRange Expr::getSourceRange() const {
  switch (Kind) {
#define EXPR(ID, PARENT) \
case ExprKind::ID: \
static_assert(sizeof(checkSourceRangeType(&ID##Expr::getSourceRange)) == 1, \
              #ID "Expr is missing getSourceRange()"); \
return cast<ID##Expr>(this)->getSourceRange();
#include "swift/AST/ExprNodes.def"
  }
  
  llvm_unreachable("expression type not handled!");
}

/// getLoc - Return the caret location of the expression.
SourceLoc Expr::getLoc() const {
  switch (Kind) {
#define EXPR(ID, PARENT) \
  case ExprKind::ID: \
    if (&Expr::getLoc != &ID##Expr::getLoc) \
      return cast<ID##Expr>(this)->getLoc(); \
    break;
#include "swift/AST/ExprNodes.def"
  }

  return getStartLoc();
}

Expr *Expr::getSemanticsProvidingExpr() {
  if (ParenExpr *parens = dyn_cast<ParenExpr>(this))
    return parens->getSubExpr()->getSemanticsProvidingExpr();
  return this;
}

Expr *Expr::getValueProvidingExpr() {
  // For now, this is totally equivalent to the above.
  // TODO:
  //   - tuple literal projection, which may become interestingly idiomatic
  return getSemanticsProvidingExpr();
}

//===----------------------------------------------------------------------===//
// Support methods for Exprs.
//===----------------------------------------------------------------------===//

/// getNumArgs - Return the number of arguments that this closure expr takes.
/// This is the length of the ArgList.
unsigned ClosureExpr::getNumArgs() const {
  Type Input = getType()->getAs<FunctionType>()->getInput();
  
  if (TupleType *TT = Input->getAs<TupleType>())
    return TT->getFields().size();
  return 1;  
}

APInt IntegerLiteralExpr::getValue() const {
  assert(!getType().isNull() && "Semantic analysis has not completed");
  unsigned BitWidth = getType()->castTo<BuiltinIntegerType>()->getBitWidth();
  
  llvm::APInt Value(BitWidth, 0);
  bool Error = getText().getAsInteger(0, Value);
  assert(!Error && "Invalid IntegerLiteral formed"); (void)Error;
  assert(Value.getActiveBits() <= BitWidth && "Value too large for size");
  if (Value.getBitWidth() != BitWidth)
    Value = Value.zextOrTrunc(BitWidth);
  return Value;
}

llvm::APFloat FloatLiteralExpr::getValue() const {
  assert(!getType().isNull() && "Semantic analysis has not completed");
  
  APFloat Val(getType()->castTo<BuiltinFloatType>()->getAPFloatSemantics());
  APFloat::opStatus Res =
    Val.convertFromString(getText(), llvm::APFloat::rmNearestTiesToEven);
  assert(Res != APFloat::opInvalidOp && "Sema didn't reject invalid number");
  (void)Res;
  return Val;
}

/// createWithCopy - Create and return a new OverloadSetRefExpr or a new
/// DeclRefExpr (if the list of decls has a single entry) from the specified
/// (non-empty) list of decls.  If we end up creating an overload set, this
/// method handles copying the list of decls into ASTContext memory.
Expr *OverloadSetRefExpr::createWithCopy(ArrayRef<ValueDecl*> Decls,
                                         SourceLoc Loc) {
  assert(!Decls.empty() &&
         "Cannot create a decl ref with an empty list of decls");
  ASTContext &C = Decls[0]->getASTContext();
  if (Decls.size() == 1)
    return new (C) DeclRefExpr(Decls[0], Loc, Decls[0]->getTypeOfReference());
  
  // Otherwise, copy the overload set into ASTContext memory and return the
  // overload set.
  return new (C) OverloadSetRefExpr(C.AllocateCopy(Decls), Loc,
                                    DependentType::get(C));
}

SequenceExpr *SequenceExpr::create(ASTContext &ctx, ArrayRef<Expr*> elements) {
  void *Buffer = ctx.Allocate(sizeof(SequenceExpr) +
                              elements.size() * sizeof(Expr*),
                              Expr::Alignment);
  return ::new(Buffer) SequenceExpr(elements);
}

SourceRange TupleExpr::getSourceRange() const {
  SourceLoc Start = LParenLoc;
  if (!Start.isValid())
    Start = getElement(0)->getStartLoc();

  SourceLoc End = RParenLoc;
  if (!End.isValid())
    End = getElement(getNumElements() - 1)->getEndLoc();
  
  return SourceRange(Start, End);
}

FuncExpr *FuncExpr::create(ASTContext &C, SourceLoc funcLoc,
                           ArrayRef<Pattern*> params, Type fnType,
                           BraceStmt *body, DeclContext *parent) {
  unsigned nParams = params.size();
  void *buf = C.Allocate(sizeof(FuncExpr) + nParams * sizeof(Pattern*),
                         Expr::Alignment);
  FuncExpr *fn = ::new(buf) FuncExpr(funcLoc, nParams, fnType, body, parent);
  for (unsigned i = 0; i != nParams; ++i)
    fn->getParamsBuffer()[i] = params[i];
  return fn;
}

SourceRange FuncExpr::getSourceRange() const {
  return SourceRange(FuncLoc, Body->getEndLoc());
}

/// Returns the result type of the function defined by the body.  For
/// an uncurried function, this is just the normal result type; for a
/// curried function, however, this is the result type of the
/// uncurried part.
///
/// Examples:
///   func(x : int) -> ((y : int) -> (int -> int))
///     The body result type is '((y : int) -> (int -> int))'.
///   func(x : int) -> (y : int) -> (int -> int)
///     The body result type is '(int -> int)'.
Type FuncExpr::getBodyResultType() const {
  unsigned n = getParamPatterns().size();
  Type ty = getType();
  do {
    ty = cast<FunctionType>(ty)->getResult();
  } while (--n);
  return ty;
}

/// getImplicitThisDecl - If this FuncExpr is a non-plus method in an
/// extension context, it will have a 'this' argument.  This method returns it
/// if present, or returns null if not.
VarDecl *FuncExpr::getImplicitThisDecl() {
  if (getParamPatterns().empty()) return 0;

  // "this" is represented as (typed_pattern (named_pattern (var_decl 'this')).
  TypedPattern *TP = dyn_cast<TypedPattern>(getParamPatterns()[0]);
  if (TP == 0) return 0;
  
  // The decl should be named 'this' and have no location information.
  NamedPattern *NP = dyn_cast<NamedPattern>(TP->getSubPattern());
  if (NP && NP->getBoundName().str() == "this" && !NP->getLoc().isValid())
    return NP->getDecl();
  return 0;
}

static ValueDecl *getCalledValue(Expr *E) {
  if (DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(E))
    return DRE->getDecl();

  Expr *E2 = E->getValueProvidingExpr();
  if (E != E2) return getCalledValue(E2);
  return nullptr;
}

ValueDecl *ApplyExpr::getCalledValue() const {
  return ::getCalledValue(Fn);
}

//===----------------------------------------------------------------------===//
//  Type Conversion Ranking
//===----------------------------------------------------------------------===//

/// convertTupleToTupleType - Given an expression that has tuple type, convert
/// it to have some other tuple type.
///
/// The caller gives us a list of the expressions named arguments and a count of
/// tuple elements for E in the IdentList+NumIdents array.  DestTy specifies the
/// type to convert to, which is known to be a TupleType.
static Expr::ConversionRank 
getTupleToTupleTypeConversionRank(const Expr *E, TupleType *ETy,
                                  TupleType *DestTy) {
  unsigned NumExprElements = ETy->getFields().size();

  // If the tuple expression or destination type have named elements, we
  // have to match them up to handle the swizzle case for when:
  //   (.y = 4, .x = 3)
  // is converted to type:
  //   (.x = int, .y = int)
  SmallVector<Identifier, 8> IdentList(NumExprElements);
  
  // Check to see if this conversion is ok by looping over all the destination
  // elements and seeing if they are provided by the input.
  
  // Keep track of which input elements are used.
  SmallVector<bool, 16> UsedElements(NumExprElements);
  SmallVector<int, 16>  DestElementSources(DestTy->getFields().size(), -1);

  assert(ETy->getFields().size() == NumExprElements && "Expr #elements mismatch!");
  {
    unsigned i = 0;
    for (const TupleTypeElt &Elt : ETy->getFields())
      IdentList[i++] = Elt.getName();
  }
  
  // First off, see if we can resolve any named values from matching named
  // inputs.
  for (unsigned i = 0, e = DestTy->getFields().size(); i != e; ++i) {
    const TupleTypeElt &DestElt = DestTy->getFields()[i];
    // If this destination field is named, first check for a matching named
    // element in the input, from any position.
    if (!DestElt.hasName()) continue;
      
    int InputElement = -1;
    for (unsigned j = 0; j != NumExprElements; ++j)
      if (IdentList[j] == DestElt.getName()) {
        InputElement = j;
        break;
      }
    if (InputElement == -1) continue;
      
    DestElementSources[i] = InputElement;
    UsedElements[InputElement] = true;
  }
  
  // Next step, resolve (in order) unmatched named results and unnamed results
  // to any left-over unnamed input.
  unsigned NextInputValue = 0;
  for (unsigned i = 0, e = DestTy->getFields().size(); i != e; ++i) {
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
      if (!DestTy->getFields()[i].hasInit())
        return Expr::CR_Invalid;
        
      // If the default initializer should be used, leave the
      // DestElementSources field set to -2.
      DestElementSources[i] = -2;
      continue;
    }
    
    // Okay, we found an input value to use.
    DestElementSources[i] = NextInputValue;
    UsedElements[NextInputValue] = true;
  }
  
  // If there were any unused input values, we fail.
  for (bool Elt : UsedElements)
    if (!Elt)
      return Expr::CR_Invalid;
  
  // It looks like the elements line up, walk through them and see if the types
  // either agree or can be converted.  If the expression is a TupleExpr, we do
  // this conversion in place.
  const TupleExpr *TE = dyn_cast<TupleExpr>(E);
  if (TE && TE->getNumElements() != 1 &&
      TE->getNumElements() == DestTy->getFields().size()) {
    Expr::ConversionRank CurRank = Expr::CR_Identity;
    
    // The conversion rank of the tuple is the worst case of the conversion rank
    // of each of its elements.
    for (unsigned i = 0, e = DestTy->getFields().size(); i != e; ++i) {
      // Extract the input element corresponding to this destination element.
      unsigned SrcField = DestElementSources[i];
      assert(SrcField != ~0U && "dest field not found?");
      
      // If SrcField is -2, then the destination element just uses its default
      // value.
      if (SrcField == -2U)
        continue;
     
      // Check to see if the src value can be converted to the destination
      // element type.
      Expr *Elt = TE->getElement(SrcField);
      CurRank = std::max(CurRank,
                         Elt->getRankOfConversionTo(DestTy->getElementType(i)));
    }
    return CurRank;
  }
  
  // A tuple-to-tuple conversion of a non-parenthesized tuple is allowed to
  // permute the elements, but cannot perform conversions of each value.
  for (unsigned i = 0, e = DestTy->getFields().size(); i != e; ++i) {
    // Extract the input element corresponding to this destination element.
    unsigned SrcField = DestElementSources[i];
    assert(SrcField != ~0U && "dest field not found?");

    // If SrcField is -2, then the destination element just uses its default
    // value.
    if (SrcField == -2U)
      continue;

    // The element types must match up exactly.
    if (ETy->getElementType(SrcField)->getCanonicalType() !=
        DestTy->getElementType(i)->getCanonicalType())
      return Expr::CR_Invalid;
  }

  return Expr::CR_Identity;
}


/// getConversionRank - Return the conversion rank for converting a value 'E' to
/// type 'ToTy'.
///
/// Note that this code needs to be kept carefully in synch with
/// SemaCoerceBottomUp::convertToType.
static Expr::ConversionRank getConversionRank(const Expr *E, Type DestTy) {
  assert(!DestTy->is<DependentType>() &&
         "Result of conversion can't be dependent");

  // If the destination is a AutoClosing FunctionType, we have special rules.
  if (FunctionType *FT = DestTy->getAs<FunctionType>())
    if (FT->isAutoClosure()) {
      // We require the expression to be an ImplicitClosureExpr that produces
      // DestTy.  If we have it, we have an identity match.
      if (E->getType()->isEqual(DestTy) && isa<ImplicitClosureExpr>(E))
        return Expr::CR_Identity;
      
      // Otherwise, the autoconversion is considered to be free.  Just see
      // whether the subexpression converts to the result type.
      return getConversionRank(E, FT->getResult());
    }

  
  // Exact matches are identity conversions.
  if (E->getType()->isEqual(DestTy))
    return Expr::CR_Identity;
  
  // Look through parentheses.
  if (const ParenExpr *PE = dyn_cast<ParenExpr>(E))
    return getConversionRank(PE->getSubExpr(), DestTy);
  
  // If we're converting to an l-value type, check for permitted
  // qualification conversions or materializations.
  if (LValueType *DestLT = DestTy->getAs<LValueType>()) {
    LValueType *SrcLT = E->getType()->getAs<LValueType>();

    // Permit l-value conversions if they respect subtyping.
    // FIXME: this should probably distiniguish overloads.
    if (SrcLT &&
        DestLT->getObjectType()->isEqual(SrcLT->getObjectType()) &&
        SrcLT->getQualifiers() <= DestLT->getQualifiers())
      return Expr::CR_Identity;

    // Permit materializations.
    if (!DestLT->isExplicit())
      return getConversionRank(E, DestLT->getObjectType());

    // Otherwise, nothing converts to an l-value.
    return Expr::CR_Invalid;
  }

  if (TupleType *TT = DestTy->getAs<TupleType>()) {
    if (isa<TupleExpr>(E))
      return getTupleToTupleTypeConversionRank(E,
                                      E->getType()->castTo<TupleType>(), TT);
    
    // If the is a scalar to tuple conversion, form the tuple and return it.
    int ScalarFieldNo = TT->getFieldForScalarInit();
    if (ScalarFieldNo != -1) {
      // If the destination is a tuple type with at most one element that has no
      // default value, see if the expression's type is convertable to the
      // element type.  This handles assigning 4 to "(a = 4, b : int)".
      return getConversionRank(E, TT->getElementType(ScalarFieldNo));
    }
    
    // If the input is a tuple and the output is a tuple, see if we can convert
    // each element.
    if (TupleType *ETy = E->getType()->getAs<TupleType>())
      return getTupleToTupleTypeConversionRank(E, ETy, TT);

    // Otherwise, fall through and see if an l2r conversion on the
    // source would help.
  }

  // If all else fails, do an lvalue-to-rvalue conversion on the source.
  if (LValueType *SrcLT = E->getType()->getAs<LValueType>()) {
    LoadExpr load(const_cast<Expr*>(E), SrcLT->getObjectType());
    return getConversionRank(&load, DestTy);
  }

  // If the expression has a dependent type or we have some other case, we fail.
  return Expr::CR_Invalid;
}

/// getRankOfConversionTo - Return the rank of a conversion from the current
/// type to the specified type.
Expr::ConversionRank Expr::getRankOfConversionTo(Type DestTy) const {
  return getConversionRank(this, DestTy);
}



//===----------------------------------------------------------------------===//
// Printing for Expr and all subclasses.
//===----------------------------------------------------------------------===//

namespace {
/// PrintExpr - Visitor implementation of Expr::print.
class PrintExpr : public ExprVisitor<PrintExpr> {
public:
  raw_ostream &OS;
  unsigned Indent;
  
  PrintExpr(raw_ostream &os, unsigned indent) : OS(os), Indent(indent) {
  }
  
  void printRec(Expr *E) {
    Indent += 2;
    if (E)
      visit(E);
    else
      OS.indent(Indent) << "(**NULL EXPRESSION**)";
    Indent -= 2;
  }
  
  /// FIXME: This should use ExprWalker to print children.
  
  void printRec(Decl *D) { D->print(OS, Indent+2); }
  void printRec(Stmt *S) { S->print(OS, Indent+2); }

  raw_ostream &printCommon(Expr *E, const char *C) {
    return OS.indent(Indent) << '(' << C << " type='" << E->getType() << '\'';
  }
  
  void visitErrorExpr(ErrorExpr *E) {
    printCommon(E, "error_expr") << ')';
  }

  void visitIntegerLiteralExpr(IntegerLiteralExpr *E) {
    printCommon(E, "integer_literal_expr") << " value=";
    if (E->getType().isNull() || E->getType()->is<DependentType>())
      OS << E->getText();
    else
      OS << E->getValue();
    OS << ')';
  }
  void visitFloatLiteralExpr(FloatLiteralExpr *E) {
    printCommon(E, "float_literal_expr") << " value=" << E->getText() << ')';
  }
  void visitDeclRefExpr(DeclRefExpr *E) {
    printCommon(E, "declref_expr")
      << " decl=" << E->getDecl()->getName() << ')';
  }
  void visitOverloadSetRefExpr(OverloadSetRefExpr *E) {
    printCommon(E, "overloadsetref_expr") << " #decls=" <<E->getDecls().size();
    for (Decl *D : E->getDecls()) {
      OS << '\n';
      printRec(D);
    }
    OS << ')';
  }
  void visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *E) {
    printCommon(E, "unresolved_decl_ref_expr")
      << " name=" << E->getName() << ')';
  }
  void visitUnresolvedMemberExpr(UnresolvedMemberExpr *E) {
    printCommon(E, "unresolved_member_expr")
      << " name='" << E->getName() << "')";
  }
  void visitParenExpr(ParenExpr *E) {
    printCommon(E, "paren_expr") << '\n';
    printRec(E->getSubExpr());
    OS << ')';
  }
  void visitTupleExpr(TupleExpr *E) {
    printCommon(E, "tuple_expr");
    for (unsigned i = 0, e = E->getNumElements(); i != e; ++i) {
      OS << '\n';
      if (E->getElement(i))
        printRec(E->getElement(i));
      else
        OS.indent(Indent+2) << "<<tuple element default value>>";
    }
    OS << ')';
  }
  void visitUnresolvedDotExpr(UnresolvedDotExpr *E) {
    printCommon(E, "unresolved_dot_expr")
      << " field '" << E->getName().str() << "'";
    if (E->getBase()) {
      OS << '\n';
      printRec(E->getBase());
    }
    OS << ')';
  }
  void visitModuleExpr(ModuleExpr *E) {
    printCommon(E, "module_expr") << '\n';
  }
  void visitSyntacticTupleElementExpr(TupleElementExpr *E) {
    printCommon(E, "syntactic_tuple_element_expr")
      << " field #" << E->getFieldNumber() << '\n';
    printRec(E->getBase());
    OS << ')';
  }
  void visitImplicitThisTupleElementExpr(TupleElementExpr *E) {
    printCommon(E, "implicit_this_tuple_element_expr")
    << " field #" << E->getFieldNumber() << '\n';
    printRec(E->getBase());
    OS << ')';
  }
  

  void visitTupleShuffleExpr(TupleShuffleExpr *E) {
    printCommon(E, "tuple_shuffle_expr") << " elements=[";
    for (unsigned i = 0, e = E->getElementMapping().size(); i != e; ++i) {
      if (i) OS << ", ";
      OS << E->getElementMapping()[i];
    }
    OS << "]\n";
    printRec(E->getSubExpr());
    OS << ')';
  }
  void visitLookThroughOneofExpr(LookThroughOneofExpr *E) {
    printCommon(E, "look_through_oneof_expr") << '\n';
    printRec(E->getSubExpr());
    OS << ')';
  }
  void visitLoadExpr(LoadExpr *E) {
    printCommon(E, "load_expr") << '\n';
    printRec(E->getSubExpr());
    OS << ')';
  }
  void visitMaterializeExpr(MaterializeExpr *E) {
    printCommon(E, "materialize_expr") << '\n';
    printRec(E->getSubExpr());
    OS << ')';
  }

  void visitAddressOfExpr(AddressOfExpr *E) {
    printCommon(E, "address_of_expr") << '\n';
    printRec(E->getSubExpr());
    OS << ')';
  }
  void visitSequenceExpr(SequenceExpr *E) {
    printCommon(E, "sequence_expr") << '\n';
    for (unsigned i = 0, e = E->getNumElements(); i != e; ++i) {
      OS << '\n';
      printRec(E->getElement(i));
    }
    OS << ')';
  }
  void visitFuncExpr(FuncExpr *E) {
    printCommon(E, "func_expr") << '\n';
    printRec(E->getBody());
    OS << ')';
  }
  void visitExplicitClosureExpr(ExplicitClosureExpr *E) {
    printCommon(E, "explicit_closure_expr") << '\n';
    printRec(E->getBody());
    OS << ')';
  }
  void visitImplicitClosureExpr(ImplicitClosureExpr *E) {
    printCommon(E, "implicit_closure_expr") << '\n';
    printRec(E->getBody());
    OS << ')';
  }
  
  void visitAnonClosureArgExpr(AnonClosureArgExpr *E) {
    printCommon(E, "anon_closure_arg_expr")
      << " ArgNo=" << E->getArgNumber() << ')';
  }
  
  void printApplyExpr(ApplyExpr *E, const char *NodeName) {
    printCommon(E, NodeName) << '\n';
    printRec(E->getFn());
    OS << '\n';
    printRec(E->getArg());
    OS << ')';
  }
  
  void visitCallExpr(CallExpr *E) {
    printApplyExpr(E, "call_expr");
  }
  void visitUnaryExpr(UnaryExpr *E) {
    printApplyExpr(E, "unary_expr");
  }
  void visitBinaryExpr(BinaryExpr *E) {
    printApplyExpr(E, "binary_expr");
  }
  void visitConstructorCallExpr(ConstructorCallExpr *E) {
    printApplyExpr(E, "constructor_call_expr");
  }
  void visitDotSyntaxCallExpr(DotSyntaxCallExpr *E) {
    printApplyExpr(E, "dot_syntax_call_expr");
  }
  void visitDotSyntaxPlusFuncUseExpr(DotSyntaxPlusFuncUseExpr *E) {
    printCommon(E, "dot_syntax_plus_func_use") << '\n';
    printRec(E->getBaseExpr());
    OS << '\n';
    printRec(E->getPlusFuncExpr());
    OS << ')';
  }    
};

} // end anonymous namespace.


void Expr::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

void Expr::print(raw_ostream &OS, unsigned Indent) const {
  PrintExpr(OS, Indent).visit(const_cast<Expr*>(this));
}
