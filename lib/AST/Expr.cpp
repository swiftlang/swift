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
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/TypeLoc.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/Twine.h"
using namespace swift;

//===----------------------------------------------------------------------===//
// Expr methods.
//===----------------------------------------------------------------------===//

// Only allow allocation of Stmts using the allocator in ASTContext.
void *Expr::operator new(size_t Bytes, ASTContext &C,
                         unsigned Alignment) {
  return C.Allocate(Bytes, Alignment);
}

StringRef Expr::getKindName(ExprKind kind) {
  switch (kind) {
#define EXPR(Id, Parent) case ExprKind::Id: return #Id;
#include "swift/AST/ExprNodes.def"
  }
}

// Helper functions to verify statically whether the getSourceRange()
// function has been overridden.
typedef const char (&TwoChars)[2];

template<typename Class> 
inline char checkSourceRangeType(SourceRange (Class::*)() const);

inline TwoChars checkSourceRangeType(SourceRange (Expr::*)() const);

SourceRange Expr::getSourceRange() const {
  switch (getKind()) {
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
  switch (getKind()) {
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
  if (ParenExpr *PE = dyn_cast<ParenExpr>(this))
    return PE->getSubExpr()->getSemanticsProvidingExpr();

  if (DefaultValueExpr *DE = dyn_cast<DefaultValueExpr>(this))
    return DE->getSubExpr()->getSemanticsProvidingExpr();
  
  return this;
}

Expr *Expr::getValueProvidingExpr() {
  // For now, this is totally equivalent to the above.
  // TODO:
  //   - tuple literal projection, which may become interestingly idiomatic
  return getSemanticsProvidingExpr();
}

bool Expr::isImplicit() const {
  if (const DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(this))
    return !DRE->getLoc().isValid();
  
  if (const ImplicitConversionExpr *ICE
        = dyn_cast<ImplicitConversionExpr>(this))
    return ICE->getSubExpr()->isImplicit();
  
  if (const MemberRefExpr *memberRef = dyn_cast<MemberRefExpr>(this))
    return memberRef->getNameLoc().isInvalid();
  if (auto memberRef = dyn_cast<GenericMemberRefExpr>(this))
    return memberRef->getNameLoc().isInvalid();
  if (auto memberRef = dyn_cast<ArchetypeMemberRefExpr>(this))
    return memberRef->getNameLoc().isInvalid();

  if (const MetatypeExpr *metatype = dyn_cast<MetatypeExpr>(this))
    return metatype->getLoc().isInvalid();

  if (const ApplyExpr *apply = dyn_cast<ApplyExpr>(this))
    return apply->getArg() && apply->getArg()->isImplicit();

  if (const TupleExpr *tuple = dyn_cast<TupleExpr>(this)) {
    if (!tuple->getSourceRange().isInvalid())
      return false;

    for (auto elt : tuple->getElements()) {
      if (!elt->isImplicit())
        return false;
    }
    return true;
  }

  if (auto downcast = dyn_cast<ExplicitCastExpr>(this)) {
    return downcast->getLoc().isInvalid() &&
           downcast->getSubExpr()->isImplicit();
  }

  if (isa<ZeroValueExpr>(this) || isa<DefaultValueExpr>(this))
    return true;
  
  if (auto assign = dyn_cast<AssignExpr>(this))
    return assign->getEqualLoc().isInvalid();

  return false;
}

//===----------------------------------------------------------------------===//
// Support methods for Exprs.
//===----------------------------------------------------------------------===//

APInt IntegerLiteralExpr::getValue(StringRef Text,
                                   unsigned BitWidth) {
  llvm::APInt Value(BitWidth, 0);
  // swift encodes octal differently than C
  bool IsCOctal = Text.size() > 1 && Text[0] == '0' && isdigit(Text[1]);
  bool Error = Text.getAsInteger(IsCOctal ? 10 : 0, Value);
  assert(!Error && "Invalid IntegerLiteral formed"); (void)Error;
  if (Value.getBitWidth() != BitWidth)
    Value = Value.zextOrTrunc(BitWidth);
  return Value;
  
}

APInt IntegerLiteralExpr::getValue() const {
  assert(!getType().isNull() && "Semantic analysis has not completed");
  return getValue(getText(),
                  getType()->castTo<BuiltinIntegerType>()->getBitWidth());
}

APFloat FloatLiteralExpr::getValue(StringRef Text,
                                   const llvm::fltSemantics &Semantics) {
  APFloat Val(Semantics);
  APFloat::opStatus Res =
    Val.convertFromString(Text, llvm::APFloat::rmNearestTiesToEven);
  assert(Res != APFloat::opInvalidOp && "Sema didn't reject invalid number");
  (void)Res;
  return Val;
}

llvm::APFloat FloatLiteralExpr::getValue() const {
  assert(!getType().isNull() && "Semantic analysis has not completed");
  
  return getValue(getText(),
                  getType()->castTo<BuiltinFloatType>()->getAPFloatSemantics());
}

MemberRefExpr::MemberRefExpr(Expr *Base, SourceLoc DotLoc, VarDecl *Value,
                             SourceLoc NameLoc)
  : Expr(ExprKind::MemberRef), Base(Base),
    Value(Value), DotLoc(DotLoc), NameLoc(NameLoc) { }

ExistentialMemberRefExpr::ExistentialMemberRefExpr(Expr *Base, SourceLoc DotLoc,
                                                   ValueDecl *Value,
                                                   SourceLoc NameLoc)
  : Expr(ExprKind::ExistentialMemberRef), Base(Base), Value(Value),
    DotLoc(DotLoc), NameLoc(NameLoc) { }

ArchetypeMemberRefExpr::ArchetypeMemberRefExpr(Expr *Base, SourceLoc DotLoc,
                                               ValueDecl *Value,
                                               SourceLoc NameLoc)
  : Expr(ExprKind::ArchetypeMemberRef), Base(Base), Value(Value),
    DotLoc(DotLoc), NameLoc(NameLoc) { }

ArchetypeType *ArchetypeMemberRefExpr::getArchetype() const {
  Type BaseTy = getBase()->getType()->getRValueType();
  if (auto Meta = BaseTy->getAs<MetaTypeType>())
    return Meta->getInstanceType()->castTo<ArchetypeType>();

  return BaseTy->castTo<ArchetypeType>();
}

bool ArchetypeMemberRefExpr::isBaseIgnored() const {
  if (isa<TypeDecl>(Value))
    return true;

  return false;
}

GenericMemberRefExpr::GenericMemberRefExpr(Expr *Base, SourceLoc DotLoc,
                                           ValueDecl *Value,
                                           SourceLoc NameLoc)
  : Expr(ExprKind::GenericMemberRef), Base(Base), Value(Value),
    DotLoc(DotLoc), NameLoc(NameLoc) { }

bool GenericMemberRefExpr::isBaseIgnored() const {
  if (getBase()->getType()->getRValueType()->is<MetaTypeType>())
    return true;

  if (isa<TypeDecl>(Value))
    return true;

  if (auto Func = dyn_cast<FuncDecl>(Value))
    return Func->isStatic();

  return false;
}

Type OverloadSetRefExpr::getBaseType() const {
  if (isa<OverloadedDeclRefExpr>(this))
    return Type();
  if (auto *DRE = dyn_cast<OverloadedMemberRefExpr>(this)) {
    return DRE->getBase()->getType()->getRValueType();
  }
  
  llvm_unreachable("Unhandled overloaded set reference expression");
}

bool OverloadSetRefExpr::hasBaseObject() const {
  if (Type BaseTy = getBaseType())
    return !BaseTy->is<MetaTypeType>();

  return false;
}

SequenceExpr *SequenceExpr::create(ASTContext &ctx, ArrayRef<Expr*> elements) {
  void *Buffer = ctx.Allocate(sizeof(SequenceExpr) +
                              elements.size() * sizeof(Expr*),
                              alignof(SequenceExpr));
  return ::new(Buffer) SequenceExpr(elements);
}

NewArrayExpr *NewArrayExpr::create(ASTContext &ctx, SourceLoc newLoc,
                                   TypeLoc elementTy, ArrayRef<Bound> bounds) {
  void *buffer = ctx.Allocate(sizeof(NewArrayExpr) +
                              bounds.size() * sizeof(Bound),
                              alignof(NewArrayExpr));
  NewArrayExpr *E =
    ::new (buffer) NewArrayExpr(newLoc, elementTy, bounds.size());
  memcpy(E->getBoundsBuffer(), bounds.data(), bounds.size() * sizeof(Bound));
  return E;
}

SourceRange TupleExpr::getSourceRange() const {
  if (LParenLoc.isValid() && !HasTrailingClosure) {
    assert(RParenLoc.isValid() && "Mismatched parens?");
    return SourceRange(LParenLoc, RParenLoc);
  }
  if (getElements().empty())
    return SourceRange();
  
  SourceLoc Start = LParenLoc.isValid()? LParenLoc
                                       : getElement(0)->getStartLoc();
  SourceLoc End = getElement(getElements().size()-1)->getEndLoc();
  return SourceRange(Start, End);
}

SubscriptExpr::SubscriptExpr(Expr *Base, Expr *Index, SubscriptDecl *D)
  : Expr(ExprKind::Subscript, D? D->getElementType() : Type()),
    D(D), Base(Base), Index(Index) {
  assert((!D ||
          !D->getDeclContext()->getDeclaredTypeOfContext()->isExistentialType())
         && "use ExistentialSubscriptExpr for existential type subscript");
}

ExistentialSubscriptExpr::
ExistentialSubscriptExpr(Expr *Base, Expr *Index, SubscriptDecl *D)
  : Expr(ExprKind::ExistentialSubscript, D? D->getElementType() : Type()),
    D(D), Base(Base), Index(Index) {
  assert(Base->getType()->getRValueType()->isExistentialType() &&
         "use SubscriptExpr for non-existential type subscript");
}

ArchetypeSubscriptExpr::
ArchetypeSubscriptExpr(Expr *Base, Expr *Index, SubscriptDecl *D)
  : Expr(ExprKind::ArchetypeSubscript, D? D->getElementType() : Type()),
    D(D), Base(Base), Index(Index) {
  assert(Base->getType()->getRValueType()->is<ArchetypeType>() &&
         "use SubscriptExpr for non-archetype type subscript");
}

GenericSubscriptExpr::
GenericSubscriptExpr(Expr *Base, Expr *Index, SubscriptDecl *D)
  : Expr(ExprKind::GenericSubscript, D? D->getElementType() : Type()),
    D(D), Base(Base), Index(Index) {
  assert(Base->getType()->getRValueType()->is<BoundGenericType>() &&
         "use SubscriptExpr for non-generic type subscript");
}

ArrayRef<Pattern *> CapturingExpr::getParamPatterns() {
  if (auto *func = dyn_cast<FuncExpr>(this))
    return func->getArgParamPatterns();
  if (auto *closure = dyn_cast<PipeClosureExpr>(this))
    return closure->getParams();
  if (auto *closure = dyn_cast<ClosureExpr>(this))
    return closure->getParamPatterns();
  llvm_unreachable("unknown capturing expr");
}

ArrayRef<const Pattern *> CapturingExpr::getParamPatterns() const {
  auto patterns = const_cast<CapturingExpr*>(this)->getParamPatterns();
  return ArrayRef<const Pattern *>(patterns.data(), patterns.size());
}


FuncExpr *FuncExpr::create(ASTContext &C, SourceLoc funcLoc,
                           ArrayRef<Pattern*> argParams,
                           ArrayRef<Pattern*> bodyParams,
                           TypeLoc fnRetType,
                           DeclContext *parent) {
  assert(argParams.size() == bodyParams.size());
  unsigned nParams = argParams.size();
  void *buf = C.Allocate(sizeof(FuncExpr) + 2 * nParams * sizeof(Pattern*),
                         alignof(FuncExpr));
  FuncExpr *fn = ::new (buf) FuncExpr(funcLoc, nParams, fnRetType, parent);
  for (unsigned i = 0; i != nParams; ++i)
    fn->getParamsBuffer()[i] = argParams[i];
  for (unsigned i = 0; i != nParams; ++i)
    fn->getParamsBuffer()[i+nParams] = bodyParams[i];
  return fn;
}

SourceRange FuncExpr::getSourceRange() const {
  if (auto *B = getBody())
    return { FuncLoc, B->getEndLoc() };
  if (FnRetType.hasLocation())
    return { FuncLoc, FnRetType.getSourceRange().End };
  const Pattern *LastPat = getArgParamPatterns().back();
  return { FuncLoc, LastPat->getEndLoc() };
}

Type FuncExpr::getResultType(ASTContext &Ctx) const {
  Type resultTy = getType();
  if (!resultTy || resultTy->is<ErrorType>())
    return resultTy;

  for (unsigned i = 0, e = getNumParamPatterns(); i != e; ++i)
    resultTy = resultTy->castTo<AnyFunctionType>()->getResult();

  if (!resultTy)
    resultTy = TupleType::getEmpty(Ctx);

  return resultTy;
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

/// getImplicitThisDecl - If this FuncExpr is a non-static method in an
/// extension context, it will have a 'this' argument.  This method returns it
/// if present, or returns null if not.
VarDecl *FuncExpr::getImplicitThisDecl() const {
  if (getNumParamPatterns() == 0) return nullptr;
  
  // "this" is represented as (typed_pattern (named_pattern (var_decl 'this')).
  auto TP = dyn_cast<TypedPattern>(getArgParamPatterns()[0]);
  if (TP == 0) return nullptr;
  
  // The decl should be named 'this' and have no location information.
  auto NP = dyn_cast<NamedPattern>(TP->getSubPattern());
  if (NP && NP->getBoundName().str() == "this" && !NP->getLoc().isValid())
    return NP->getDecl();
  return nullptr;
}

RebindThisInConstructorExpr::RebindThisInConstructorExpr(Expr *SubExpr,
                                                         ValueDecl *This)
  : Expr(ExprKind::RebindThisInConstructor,
         TupleType::getEmpty(This->getASTContext())),
    SubExpr(SubExpr), This(This)
{}


SourceRange PipeClosureExpr::getSourceRange() const {
  return body.getPointer()->getSourceRange();
}

SourceLoc PipeClosureExpr::getLoc() const {
  return body.getPointer()->getStartLoc();
}

Expr *PipeClosureExpr::getSingleExpressionBody() const {
  assert(hasSingleExpressionBody() && "Not a single-expression body");
  return cast<ReturnStmt>(body.getPointer()->getElements()[0].get<Stmt *>())
           ->getResult();
}

Type PipeClosureExpr::getResultType() const {
  if (getType()->is<ErrorType>())
    return getType();

  return getType()->castTo<AnyFunctionType>()->getResult();
}

void PipeClosureExpr::setSingleExpressionBody(Expr *newBody) {
  cast<ReturnStmt>(body.getPointer()->getElements()[0].get<Stmt *>())
    ->setResult(newBody);
}

SourceRange AssignExpr::getSourceRange() const {
  if (isFolded())
    return SourceRange(Dest->getStartLoc(), Src->getEndLoc());
  return EqualLoc;
}

SourceLoc UnresolvedPatternExpr::getLoc() const { return subPattern->getLoc(); }
SourceRange UnresolvedPatternExpr::getSourceRange() const {
  return subPattern->getSourceRange();
}

