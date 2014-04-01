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
#include "swift/AST/Decl.h" // FIXME: Bad dependency
#include "swift/AST/Stmt.h"
#include "swift/AST/AST.h"
#include "swift/AST/ASTWalker.h"
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

StringRef Expr::getKindName(ExprKind K) {
  switch (K) {
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
  if (IdentityExpr *PE = dyn_cast<IdentityExpr>(this))
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

Initializer *Expr::findExistingInitializerContext() {
  struct FindExistingInitializer : ASTWalker {
    Initializer *TheInitializer = nullptr;
    std::pair<bool,Expr*> walkToExprPre(Expr *E) override {
      assert(!TheInitializer && "continuing to walk after finding context?");
      if (auto closure = dyn_cast<AbstractClosureExpr>(E)) {
        TheInitializer = cast<Initializer>(closure->getParent());
        return { false, nullptr };
      }
      return { true, E };
    }
  } finder;
  walk(finder);
  return finder.TheInitializer;
}

bool Expr::isStaticallyDerivedMetatype() const {
  // IF the result isn't a metatype, there's nothing else to do.
  if (!getType()->is<AnyMetatypeType>())
    return false;

  const Expr *expr = this;
  do {
    // Skip syntax.
    expr = expr->getSemanticsProvidingExpr();

    // Direct reference to a type.
    if (auto declRef = dyn_cast<DeclRefExpr>(expr)) {
      return isa<TypeDecl>(declRef->getDecl());
    }

    // A "." expression that refers to a member.
    if (auto memberRef = dyn_cast<MemberRefExpr>(expr)) {
      return isa<TypeDecl>(memberRef->getMember().getDecl());
    }

    // When the base of a "." expression is ignored, look at the member.
    if (auto ignoredDot = dyn_cast<DotSyntaxBaseIgnoredExpr>(expr)) {
      expr = ignoredDot->getRHS();
      continue;
    }

    // A synthesized metatype.
    if (auto metatype = dyn_cast<MetatypeExpr>(expr)) {
      // Recurse into the base, if there is one.
      if (auto base = metatype->getBase()) {
        expr = base;
        continue;
      }

      // Either a reference to a type, or an expression that synthesizes a
      // metatype from thin air. Either way, it's statically-derived.
      return true;
    }

    // Anything else is not statically derived.
    return false;
  } while (true);
}

bool Expr::isSuperExpr() const {
  const Expr *expr = this;
  do {
    expr = expr->getSemanticsProvidingExpr();

    if (isa<SuperRefExpr>(expr))
      return true;

    if (auto derivedToBase = dyn_cast<DerivedToBaseExpr>(expr)) {
      expr = derivedToBase->getSubExpr();
      continue;
    }

    return false;
  } while (true);
}

//===----------------------------------------------------------------------===//
// Support methods for Exprs.
//===----------------------------------------------------------------------===//

static APInt getIntegerLiteralValue(bool IsNegative, StringRef Text,
                                    unsigned BitWidth) {
  llvm::APInt Value(BitWidth, 0);
  // swift encodes octal differently from C
  bool IsCOctal = Text.size() > 1 && Text[0] == '0' && isdigit(Text[1]);
  bool Error = Text.getAsInteger(IsCOctal ? 10 : 0, Value);
  assert(!Error && "Invalid IntegerLiteral formed"); (void)Error;
  if (IsNegative)
    Value = -Value;
  if (Value.getBitWidth() != BitWidth)
    Value = Value.sextOrTrunc(BitWidth);
  return Value;
}

APInt IntegerLiteralExpr::getValue(StringRef Text, unsigned BitWidth) {
  return getIntegerLiteralValue(/*IsNegative=*/false, Text, BitWidth);
}

APInt IntegerLiteralExpr::getValue() const {
  assert(!getType().isNull() && "Semantic analysis has not completed");
  assert(!getType()->is<ErrorType>() && "Should have a valid type");
  return getIntegerLiteralValue(
      isNegative(), getDigitsText(),
      getType()->castTo<BuiltinIntegerType>()->getGreatestWidth());
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

void DeclRefExpr::setDeclRef(ConcreteDeclRef ref) {
  if (auto spec = getSpecInfo())
    spec->D = ref;
  else
    DOrSpecialized = ref;
}

void DeclRefExpr::setSpecialized() {
  if (isSpecialized())
    return;

  ConcreteDeclRef ref = getDeclRef();
  void *Mem = ref.getDecl()->getASTContext().Allocate(sizeof(SpecializeInfo),
                                                      alignof(SpecializeInfo));
  auto Spec = new (Mem) SpecializeInfo;
  Spec->D = ref;
  DOrSpecialized = Spec;
}

void DeclRefExpr::setGenericArgs(ArrayRef<TypeRepr*> GenericArgs) {
  ValueDecl *D = getDecl();
  assert(D);
  setSpecialized();
  getSpecInfo()->GenericArgs = D->getASTContext().AllocateCopy(GenericArgs);
}

ConstructorDecl *OtherConstructorDeclRefExpr::getDecl() const {
  return cast_or_null<ConstructorDecl>(Ctor.getDecl());
}

MemberRefExpr::MemberRefExpr(Expr *base, SourceLoc dotLoc,
                             ConcreteDeclRef member, SourceRange nameRange,
                             bool Implicit, bool UsesDirectPropertyAccess)
  : Expr(ExprKind::MemberRef, Implicit), Base(base),
    Member(member), DotLoc(dotLoc), NameRange(nameRange) {
   
  MemberRefExprBits.IsDirectPropertyAccess = UsesDirectPropertyAccess;
  MemberRefExprBits.IsSuper = false;
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
    return !BaseTy->is<AnyMetatypeType>();

  return false;
}

SequenceExpr *SequenceExpr::create(ASTContext &ctx, ArrayRef<Expr*> elements) {
  void *Buffer = ctx.Allocate(sizeof(SequenceExpr) +
                              elements.size() * sizeof(Expr*),
                              alignof(SequenceExpr));
  return ::new(Buffer) SequenceExpr(elements);
}

NewArrayExpr *NewArrayExpr::create(ASTContext &ctx, SourceLoc newLoc,
                                   TypeLoc elementTy, ArrayRef<Bound> bounds,
                                   Expr *constructionFn) {
  void *buffer = ctx.Allocate(sizeof(NewArrayExpr) +
                              bounds.size() * sizeof(Bound),
                              alignof(NewArrayExpr));
  NewArrayExpr *E =
    ::new (buffer) NewArrayExpr(newLoc, elementTy, bounds.size(),
                                constructionFn);
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

ArrayRef<Expr *> CollectionExpr::getElements() const {
  if (auto paren = dyn_cast<ParenExpr>(SubExpr)) {
    // FIXME: Hack. When this goes away, remove IdentityExpr's friendship of
    // CollectionExpr.
    return llvm::makeArrayRef(&paren->SubExpr, 1);
  }

  return cast<TupleExpr>(SubExpr)->getElements();
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

RebindSelfInConstructorExpr::RebindSelfInConstructorExpr(Expr *SubExpr,
                                                         VarDecl *Self)
  : Expr(ExprKind::RebindSelfInConstructor, /*Implicit=*/true,
         TupleType::getEmpty(Self->getASTContext())),
    SubExpr(SubExpr), Self(Self)
{}

void AbstractClosureExpr::setParams(Pattern *P) {
  ParamPattern = P;
  // Change the DeclContext of any parameters to be this closure.
  if (P) {
    P->forEachVariable([&](VarDecl *VD) {
      VD->setDeclContext(this);
    });
  }
}


Type AbstractClosureExpr::getResultType() const {
  if (getType()->is<ErrorType>())
    return getType();

  return getType()->castTo<FunctionType>()->getResult();
}

SourceRange ClosureExpr::getSourceRange() const {
  return body.getPointer()->getSourceRange();
}

SourceLoc ClosureExpr::getLoc() const {
  return body.getPointer()->getStartLoc();
}

Expr *ClosureExpr::getSingleExpressionBody() const {
  assert(hasSingleExpressionBody() && "Not a single-expression body");
  return cast<ReturnStmt>(body.getPointer()->getElements()[0].get<Stmt *>())
           ->getResult();
}

void ClosureExpr::setSingleExpressionBody(Expr *NewBody) {
  cast<ReturnStmt>(body.getPointer()->getElements()[0].get<Stmt *>())
    ->setResult(NewBody);
}

SourceRange AutoClosureExpr::getSourceRange() const {
  return Body->getSourceRange();
}

void AutoClosureExpr::setBody(Expr *E) {
  auto &Context = getASTContext();
  auto *RS = new (Context) ReturnStmt(SourceLoc(), E);
  Body = BraceStmt::create(Context, E->getStartLoc(), { RS }, E->getEndLoc());
}

Expr *AutoClosureExpr::getSingleExpressionBody() const {
  return cast<ReturnStmt>(Body->getElements()[0].get<Stmt *>())->getResult();
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

UnresolvedSelectorExpr::UnresolvedSelectorExpr(Expr *subExpr, SourceLoc dotLoc,
                                               DeclName name,
                                               ArrayRef<ComponentLoc> components)
  : Expr(ExprKind::UnresolvedSelector, /*implicit*/ false),
    SubExpr(subExpr), DotLoc(dotLoc), Name(name)
{
  assert(name.getComponents().size() == components.size()
         && "number of component locs does not match number of name components");
  auto buf = getComponentsBuf();
  std::uninitialized_copy(components.begin(), components.end(),
                          buf.begin());
}

UnresolvedSelectorExpr *UnresolvedSelectorExpr::create(ASTContext &C,
             Expr *subExpr, SourceLoc dotLoc,
             DeclName name,
             ArrayRef<ComponentLoc> components) {
  assert(name.getComponents().size() == components.size()
         && "number of component locs does not match number of name components");
  
  void *buf = C.Allocate(sizeof(UnresolvedSelectorExpr)
                           + name.getComponents().size() * sizeof(ComponentLoc),
                         alignof(UnresolvedSelectorExpr));
  return ::new (buf) UnresolvedSelectorExpr(subExpr, dotLoc, name, components);
}

unsigned ScalarToTupleExpr::getScalarField() const {
  unsigned result = std::find(Elements.begin(), Elements.end(), Element())
                      - Elements.begin();
  assert(result != Elements.size()
         && "Tuple elements are missing the scalar 'hole'");
  return result;
}

SourceLoc MetatypeExpr::getLoc() const {
  if (auto tyR = getBaseTypeRepr())
    return tyR->getStartLoc();

  return MetatypeLoc;
}

SourceRange MetatypeExpr::getSourceRange() const {
  if (auto tyR = getBaseTypeRepr())
    return tyR->getSourceRange();

  if (auto base = getBase()) {
    if (MetatypeLoc.isValid())
      return SourceRange(base->getStartLoc(), MetatypeLoc);

    return base->getSourceRange();
  }

  return SourceRange(MetatypeLoc);
}

SourceRange UnresolvedMemberExpr::getSourceRange() const { 
  if (Argument)
    return SourceRange(DotLoc, Argument->getEndLoc());

  return SourceRange(DotLoc, NameLoc);
}

ArchetypeType *OpenExistentialExpr::getOpenedArchetype() const {
  auto type = getOpaqueValue()->getType();
  if (auto metaTy = type->getAs<MetatypeType>())
    type = metaTy->getInstanceType();
  return type->castTo<ArchetypeType>();
}

