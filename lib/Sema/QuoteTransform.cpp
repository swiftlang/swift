//===--- QuoteTransform.cpp - Quote transformation ------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements routines associated with the transformation used to
// desugar #quote(...) expressions.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/TypeVisitor.h"
#include "swift/AST/USRGeneration.h"

using namespace swift;

namespace {

class Breadcrumbs {
private:
  SmallVector<SourceRange, 16> locs;

public:
  template <typename T> void push(T quotee) {
    locs.push_back(quotee->getSourceRange());
  }

  void push(Type quotee) { locs.push_back(SourceRange()); }

  void pop() { locs.pop_back(); }

  SourceLoc getLoc() {
    for (auto loc : reversed(locs)) {
      if (loc.isValid()) {
        return loc.Start;
      }
    }
    return SourceLoc();
  }
};

class Breadcrumb {
private:
  Breadcrumbs &bcs;

public:
  template <typename T> Breadcrumb(Breadcrumbs &bcs, T quotee) : bcs(bcs) {
    bcs.push(quotee);
  }

  ~Breadcrumb() { bcs.pop(); }
};

class AbstractQuoter {
protected:
  TypeChecker &tc;
  ASTContext &ctx;
  Breadcrumbs &bcs;

  AbstractQuoter(TypeChecker &tc, Breadcrumbs &bcs)
      : tc(tc), ctx(tc.Context), bcs(bcs) {}

  Expr *quoteArray(ArrayRef<Expr *> quotedTrees) {
    auto arrayExpr =
        ArrayExpr::create(ctx, SourceLoc(), quotedTrees, {}, SourceLoc());
    arrayExpr->setImplicit();
    return arrayExpr;
  }

  Expr *quoteBool(bool val) {
    return new (ctx) BooleanLiteralExpr(val, SourceLoc(), /*Implicit=*/true);
  }

  Expr *quoteFloat(StringRef val) {
    return new (ctx) FloatLiteralExpr(val, SourceLoc(), /*Implicit=*/true);
  }

  Expr *quoteInt(StringRef val) {
    return new (ctx) IntegerLiteralExpr(val, SourceLoc(), /*Implicit=*/true);
  }

  Expr *quoteInt(unsigned val) {
    return IntegerLiteralExpr::createFromUnsigned(ctx, val);
  }

  Expr *quoteString(StringRef str) {
    return new (ctx) StringLiteralExpr(str, SourceLoc(), /*Implicit=*/true);
  }

  Expr *quoteSymbol(ValueDecl *decl) {
    if (decl) {
      auto usr = allocateString(
          [&](raw_ostream &os) { return ide::printDeclUSR(decl, os); });
      return usr.empty() ? unknownSymbol(decl) : quoteString(usr);
    } else {
      return unknownSymbol(decl);
    }
  }

  // TODO(TF-735): Implement ExpressibleByQuoteLiteral.
  Expr *makeQuote(const char *name, ArrayRef<Expr *> quotedSubnodes) {
    auto nodeInit = new (ctx) UnresolvedDeclRefExpr(
        ctx.getIdentifier(name), DeclRefKind::Ordinary, DeclNameLoc());
    return CallExpr::createImplicit(ctx, nodeInit, quotedSubnodes, {});
  }

  template <typename T> Expr *unknownName(T culprit) {
    diagnoseUnsupported(culprit);
    return makeQuote("Name", {quoteString("<?>"), quoteString(""),
                              makeQuote("UnknownTree", {})});
  }

  template <typename T> Expr *unknownTree(T culprit) {
    diagnoseUnsupported(culprit);
    return makeQuote("UnknownTree", {});
  }

  template <typename T> Expr *unknownSymbol(T culprit) {
    diagnoseUnsupported(culprit);
    return quoteString("");
  }

  template <typename T> void diagnoseUnsupported(T culprit) {
    if (culprit) {
      auto str =
          allocateString([&](raw_ostream &os) { return culprit->dump(os); });
      tc.diagnose(bcs.getLoc(), diag::quote_literal_unsupported_detailed, str);
    } else {
      tc.diagnose(bcs.getLoc(), diag::quote_literal_unsupported_brief);
    }
  }

private:
  StringRef allocateString(std::function<void(raw_ostream &)> printer) {
    llvm::SmallString<256> SS;
    llvm::raw_svector_ostream OS(SS);
    printer(OS);
    auto str = SS.str();
    if (str.empty()) {
      return str;
    } else {
      char *Mem = static_cast<char *>(ctx.Allocate(str.size(), 1));
      std::copy(str.begin(), str.end(), Mem);
      return StringRef(Mem, str.size());
    }
  }
};

class TypeQuoter : public TypeVisitor<TypeQuoter, Expr *>,
                   private AbstractQuoter {
public:
  TypeQuoter(TypeChecker &tc, Breadcrumbs &bcs) : AbstractQuoter(tc, bcs) {}

#define UNSUPPORTED_TYPE(TYPE)                                                 \
  Expr *visit##TYPE(TYPE *type) { return unknownTree(type); }

  Expr *visitAnyFunctionType(AnyFunctionType *type) {
    SmallVector<Expr *, 4> attrs;
    switch (type->getExtInfo().getDifferentiabilityKind()) {
    case DifferentiabilityKind::Nondifferentiable:
      break;
    case DifferentiabilityKind::Normal:
      attrs.push_back(makeQuote("Differentiable", {}));
      break;
    case DifferentiabilityKind::Linear:
      attrs.push_back(makeQuote("DifferentiableLinear", {}));
      break;
    }
    SmallVector<Type, 4> paramTypes;
    for (auto param : type->getParams()) {
      paramTypes.push_back(param.getOldType());
    }
    return makeQuote("FunctionType", {quoteArray(attrs), quoteTypes(paramTypes),
                                      quoteType(type->getResult())});
  }

  Expr *visitAnyMetatypeType(AnyMetatypeType *type) {
    return makeQuote("MetaType", {quoteType(type->getInstanceType())});
  }

  Expr *visitArraySliceType(ArraySliceType *type) {
    return makeQuote("ArrayType", {quoteType(type->getBaseType())});
  }

  Expr *visitBoundGenericType(BoundGenericType *type) {
    return makeQuote("SpecializedType", {quoteName(type->getDecl()),
                                         quoteTypes(type->getGenericArgs())});
  }

  // NOTE: From what I understand, these types are created later in the
  // pipeline so they cannot occur in code that is being quoted.
  UNSUPPORTED_TYPE(BuiltinType)

  Expr *visitDictionaryType(DictionaryType *type) {
    return makeQuote("DictionaryType", {quoteType(type->getKeyType()),
                                        quoteType(type->getValueType())});
  }

  // NOTE: From what I understand, these types don't occur in expressions
  // and only occur in declarations, so we aren't interested in them until
  // supporting other decls becomes a thing (#5).
  UNSUPPORTED_TYPE(DependentMemberType)
  UNSUPPORTED_TYPE(DynamicSelfType)

  // NOTE: If typechecking encountered errors, there's no point in quoting
  // anyway because none of that code will make it to runtime.
  UNSUPPORTED_TYPE(ErrorType)

  Expr *visitInOutType(InOutType *type) {
    return makeQuote("InoutType", {quoteType(type->getObjectType())});
  }

  Expr *visitLValueType(LValueType *type) {
    return makeQuote("LValueType", {quoteType(type->getObjectType())});
  }

  // NOTE: Since we ignore base in visitDotSyntaxBaseIgnoredExpr, from what
  // I understand, module types cannot reach type quoters.
  UNSUPPORTED_TYPE(ModuleType)

  Expr *visitNominalType(NominalType *type) {
    return quoteName(type->getDecl());
  }

  Expr *visitOptionalType(OptionalType *type) {
    return makeQuote("OptionalType", {quoteType(type->getBaseType())});
  }

  Expr *visitParenType(ParenType *type) {
    return quoteType(type->getUnderlyingType());
  }

  Expr *visitProtocolCompositionType(ProtocolCompositionType *type) {
    return makeQuote("AndType", {quoteTypes(type->getMembers())});
  }

  // NOTE: From what I understand, these types don't occur in expressions
  // and only occur in declarations, so we aren't interested in them until
  // supporting other decls becomes a thing (#5).
  UNSUPPORTED_TYPE(ReferenceStorageType)
  UNSUPPORTED_TYPE(SubstitutableType)

  // NOTE: From what I understand, these types are created later in the
  // pipeline so they cannot occur in code that is being quoted.
  UNSUPPORTED_TYPE(SILBlockStorageType)
  UNSUPPORTED_TYPE(SILBoxType)
  UNSUPPORTED_TYPE(SILFunctionType)
  UNSUPPORTED_TYPE(SILTokenType)

  Expr *visitTupleType(TupleType *type) {
    return makeQuote("TupleType", {quoteTypes(type->getElementTypes())});
  }

  Expr *visitTypeAliasType(TypeAliasType *type) {
    return quoteName(type->getDecl());
  }

  // NOTE: If typechecking encountered errors, there's no point in quoting
  // anyway because none of that code will make it to runtime.
  UNSUPPORTED_TYPE(TypeVariableType)
  UNSUPPORTED_TYPE(UnboundGenericType)
  UNSUPPORTED_TYPE(UnresolvedType)

#undef UNSUPPORTED_TYPE

private:
  Expr *quoteName(TypeDecl *decl) {
    return makeQuote(
        "TypeName",
        {quoteString(decl->getBaseName().userFacingName()), quoteSymbol(decl)});
  }

  Expr *quoteType(Type type) { return visit(type); }

  template <typename T> Expr *quoteTypes(T types) {
    SmallVector<Expr *, 4> quotedTypes;
    for (auto type : types) {
      quotedTypes.push_back(quoteType(type));
    }
    return quoteArray(quotedTypes);
  }
};

class ASTQuoter
    : public ASTVisitor<ASTQuoter, Expr *, Expr *, Expr *, void, void, void>,
      private AbstractQuoter {
  TypeQuoter typeQuoter;

public:
  ASTQuoter(TypeChecker &tc, Breadcrumbs &bcs)
      : AbstractQuoter(tc, bcs), typeQuoter(TypeQuoter(tc, bcs)) {}

#define UNSUPPORTED_EXPR(EXPR)                                                 \
  Expr *visit##EXPR(EXPR *expr) {                                              \
    Breadcrumb bc(bcs, expr);                                                  \
    return unknownTree(expr);                                                  \
  }

  Expr *visitArrayExpr(ArrayExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return makeQuote("ArrayLiteral", {quoteExprs(expr->getElements()),
                                      quoteType(expr->getType())});
  }

  // NOTE: If typechecking encountered errors, there's no point in quoting
  // anyway because none of that code will make it to runtime.
  UNSUPPORTED_EXPR(ArrowExpr)

  Expr *visitAssignExpr(AssignExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return makeQuote("Assign",
                     {quoteExpr(expr->getDest()), quoteExpr(expr->getSrc()),
                      quoteType(expr->getType())});
  }

  Expr *visitAutoClosureExpr(AutoClosureExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return makeQuote("Closure",
                     {quoteParams(expr->getParameters()),
                      quoteBody(expr->getBody()), quoteType(expr->getType())});
  }

  Expr *visitBinaryExpr(BinaryExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return makeQuote("Binary", {quoteExpr(expr->getArg()->getElements()[0]),
                                quoteExpr(expr->getFn()),
                                quoteExpr(expr->getArg()->getElements()[1]),
                                quoteType(expr->getType())});
  }

  // TODO(TF-723): Quote optional chaining.
  UNSUPPORTED_EXPR(BindOptionalExpr)

  Expr *visitBooleanLiteralExpr(BooleanLiteralExpr *expr) {
    Breadcrumb bc(bcs, expr);
    // TODO(TF-733): Figure out why this is not good enough.
    // auto type = expr->getType();
    auto type = ctx.getBoolDecl()->getDeclaredType();
    return makeQuote("BooleanLiteral",
                     {quoteBool(expr->getValue()), quoteType(type)});
  }

  Expr *visitCallerDefaultArgumentExpr(CallerDefaultArgumentExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return quoteExpr(expr->getSubExpr());
  }

  Expr *visitCallExpr(CallExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return makeQuote("Call",
                     {quoteExpr(expr->getFn()),
                      quoteLabels(expr->getArgumentLabels()),
                      quoteArgs(expr->getArg()), quoteType(expr->getType())});
  }

  Expr *visitCaptureListExpr(CaptureListExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return quoteExpr(expr->getClosureBody());
  }

  Expr *visitClosureExpr(ClosureExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return makeQuote("Closure",
                     {quoteParams(expr->getParameters()),
                      quoteBody(expr->getBody()), quoteType(expr->getType())});
  }

  // NOTE: There's no point in quoting IDE support artifacts.
  UNSUPPORTED_EXPR(CodeCompletionExpr)

  Expr *visitCoerceExpr(CoerceExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return makeQuote(
        "As", {quoteExpr(expr->getSubExpr()), quoteType(expr->getType())});
  }

  Expr *visitConditionalCheckedCastExpr(ConditionalCheckedCastExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return makeQuote("OptionalAs", {quoteExpr(expr->getSubExpr()),
                                    quoteType(expr->getCastTypeLoc().getType()),
                                    quoteType(expr->getType())});
  }

  Expr *visitConstructorRefCallExpr(ConstructorRefCallExpr *expr) {
    // TODO(TF-715): Allow @quoted on more decls.
    Breadcrumb bc(bcs, expr);
    if (auto ref = dyn_cast<DeclRefExpr>(expr->getFn())) {
      return makeQuote("Name", {quoteValue(ref->getDeclRef()),
                                quoteSymbol(ref->getDecl()),
                                quoteType(expr->getType())});
    } else {
      return unknownTree(expr);
    }
  }

  // NOTE: DeclQuoteExprs are only used in synthetic code generated for @quoted
  // declarations, and that cannot end up quoted.
  UNSUPPORTED_EXPR(DeclQuoteExpr)

  Expr *visitDeclRefExpr(DeclRefExpr *expr) {
    Breadcrumb bc(bcs, expr);
    auto quotedExpr = makeQuote("Name", {quoteValue(expr->getDeclRef()),
                                         quoteSymbol(expr->getDecl()),
                                         quoteType(expr->getType())});
    if (auto attr = expr->getDecl()->getAttrs().getAttribute<QuotedAttr>()) {
      auto quoteRef = new (ctx)
          DeclRefExpr(ConcreteDeclRef(attr->getQuoteDecl()), DeclNameLoc(),
                      /*Implicit=*/true);
      auto quoteCall = CallExpr::createImplicit(ctx, quoteRef, {}, {});
      auto &tc = TypeChecker::createForContext(ctx);
      auto type = tc.getTypeOfQuoteExpr(expr->getType(), expr->getLoc());
      return makeQuote("Unquote", {quotedExpr, quoteCall, quoteType(type)});
    } else {
      return quotedExpr;
    }
  }

  Expr *visitDefaultArgumentExpr(DefaultArgumentExpr *expr) {
    Breadcrumb bc(bcs, expr);
    // TODO(TF-741): Look up the corresponding decl.
    return makeQuote("Default", {quoteString(""), quoteType(expr->getType())});
  }

  Expr *visitDictionaryExpr(DictionaryExpr *expr) {
    Breadcrumb bc(bcs, expr);
    SmallVector<Expr *, 4> quotedExprs;
    for (auto element : expr->getElements()) {
      if (auto tuple = dyn_cast<TupleExpr>(element)) {
        for (auto element : tuple->getElements()) {
          quotedExprs.push_back(quoteExpr(element));
        }
      } else {
        Breadcrumb bc2(bcs, element);
        quotedExprs.push_back(unknownTree(element));
      }
    }
    return makeQuote("DictionaryLiteral",
                     {quoteArray(quotedExprs), quoteType(expr->getType())});
  }

  Expr *visitDiscardAssignmentExpr(DiscardAssignmentExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return makeQuote("Wildcard", {});
  }

  // TODO(TF-723): Quote type(of:).
  UNSUPPORTED_EXPR(DynamicTypeExpr)

  Expr *visitDotSelfExpr(DotSelfExpr *expr) {
    Breadcrumb bc(bcs, expr);
    if (auto base = dyn_cast<TypeExpr>(expr->getSubExpr())) {
      // TODO(TF-730): Figure out why we can't simply do `base->getType()`.
      if (auto metaType = expr->getType()->getAs<MetatypeType>()) {
        return makeQuote("PostfixSelf", {quoteType(metaType->getInstanceType()),
                                         quoteType(expr->getType())});
      } else {
        return unknownTree(expr);
      }
    } else {
      return makeQuote("PostfixSelf", {quoteExpr(expr->getSubExpr()),
                                       quoteType(expr->getType())});
    }
  }

  Expr *visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *expr) {
    Breadcrumb bc(bcs, expr);
    if (auto ref = dyn_cast<DeclRefExpr>(expr->getRHS())) {
      // TODO(TF-730): Figure out why this is not enough.
      auto baseType = expr->getLHS()->getType();
      auto refType = ref->getType();
      if (!baseType || !refType) {
        if (auto exprType = expr->getType()->getAs<FunctionType>()) {
          baseType = exprType->getParams()[0].getPlainType();
          refType = exprType->getResult();
        } else {
          return unknownTree(expr);
        }
      }

      auto quotedExpr =
          makeQuote("Name", {quoteValue(ref->getDeclRef()),
                             quoteSymbol(ref->getDecl()), quoteType(refType)});
      if (auto attr = ref->getDecl()->getAttrs().getAttribute<QuotedAttr>()) {
        auto quoteRef = new (ctx)
            DeclRefExpr(ConcreteDeclRef(attr->getQuoteDecl()), DeclNameLoc(),
                        /*Implicit=*/true);
        auto quoteBase = TypeExpr::createImplicit(baseType, ctx);
        auto quoteDot =
            new (ctx) DotSyntaxCallExpr(quoteRef, SourceLoc(), quoteBase);
        auto quoteCall = CallExpr::createImplicit(ctx, quoteDot, {}, {});
        auto &tc = TypeChecker::createForContext(ctx);
        auto type = tc.getTypeOfQuoteExpr(refType, ref->getLoc());
        return makeQuote("Unquote", {quotedExpr, quoteCall, quoteType(type)});
      } else {
        return quotedExpr;
      }
    } else {
      return unknownTree(expr);
    }
  }

  Expr *visitDotSyntaxCallExpr(DotSyntaxCallExpr *expr) {
    Breadcrumb bc(bcs, expr);
    if (auto ref = dyn_cast<DeclRefExpr>(expr->getFn())) {
      if (dyn_cast<TypeExpr>(expr->getBase())) {
        // TODO(TF-715): Allow @quoted on more decls.
        return makeQuote("Name", {quoteValue(ref->getDeclRef()),
                                  quoteSymbol(ref->getDecl()),
                                  quoteType(expr->getType())});
      } else {
        auto quotedExpr = makeQuote("Member", {quoteExpr(expr->getBase()),
                                               quoteValue(ref->getDeclRef()),
                                               quoteSymbol(ref->getDecl()),
                                               quoteType(expr->getType())});
        if (auto attr = ref->getDecl()->getAttrs().getAttribute<QuotedAttr>()) {
          auto quoteRef = new (ctx)
              DeclRefExpr(ConcreteDeclRef(attr->getQuoteDecl()), DeclNameLoc(),
                          /*Implicit=*/true);
          auto quoteBase =
              TypeExpr::createImplicit(expr->getBase()->getType(), ctx);
          auto quoteDot =
              new (ctx) DotSyntaxCallExpr(quoteRef, SourceLoc(), quoteBase);
          auto quoteCall = CallExpr::createImplicit(ctx, quoteDot, {}, {});
          auto &tc = TypeChecker::createForContext(ctx);
          auto type = tc.getTypeOfQuoteExpr(expr->getType(), expr->getLoc());
          return makeQuote("Unquote", {quotedExpr, quoteCall, quoteType(type)});
        } else {
          return quotedExpr;
        }
      }
    } else {
      return unknownTree(expr);
    }
  }

  // TODO(TF-723): Quote dynamic lookup exprs.
  UNSUPPORTED_EXPR(DynamicLookupExpr)

  // NOTE: There's no point in quoting IDE support artifacts.
  UNSUPPORTED_EXPR(EditorPlaceholderExpr)

  Expr *visitEnumIsCaseExpr(EnumIsCaseExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return makeQuote("Is",
                     {quoteExpr(expr->getSubExpr()),
                      quoteType(expr->getEnumElement()->getInterfaceType()),
                      quoteType(expr->getType())});
  }

  // NOTE: If typechecking encountered errors, there's no point in quoting
  // anyway because none of that code will make it to runtime.
  UNSUPPORTED_EXPR(ErrorExpr)

  Expr *visitFloatLiteralExpr(FloatLiteralExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return makeQuote("FloatLiteral", {quoteFloat(expr->getDigitsText()),
                                      quoteType(expr->getType())});
  }

  Expr *visitForcedCheckedCastExpr(ForcedCheckedCastExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return makeQuote(
        "ForceAs", {quoteExpr(expr->getSubExpr()), quoteType(expr->getType())});
  }

  Expr *visitForceTryExpr(ForceTryExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return makeQuote("ForceTry", {quoteExpr(expr->getSubExpr()),
                                  quoteType(expr->getType())});
  }

  Expr *visitForceValueExpr(ForceValueExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return makeQuote(
        "Force", {quoteExpr(expr->getSubExpr()), quoteType(expr->getType())});
  }

  Expr *visitIfExpr(IfExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return makeQuote("Ternary", {quoteExpr(expr->getCondExpr()),
                                 quoteExpr(expr->getThenExpr()),
                                 quoteExpr(expr->getElseExpr()),
                                 quoteType(expr->getType())});
  }

  Expr *visitImplicitConversionExpr(ImplicitConversionExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return makeQuote("Conversion", {quoteExpr(expr->getSubExpr()),
                                    quoteType(expr->getType())});
  }

  Expr *visitInOutExpr(InOutExpr *expr) {
    Breadcrumb bc(bcs, expr);
    // NOTE: If this expression is implicit, `expr` stands for a conversion that
    // is sometimes emitted when inout parameters are used.
    // Otherwise, `expr` stands for an in-out expression `&foo`.
    if (expr->isImplicit()) {
      return makeQuote("Conversion", {quoteExpr(expr->getSubExpr()),
                                      quoteType(expr->getType())});
    } else {
      return makeQuote(
          "Inout", {quoteExpr(expr->getSubExpr()), quoteType(expr->getType())});
    }
  }

  Expr *visitIntegerLiteralExpr(IntegerLiteralExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return makeQuote("IntegerLiteral", {quoteInt(expr->getDigitsText()),
                                        quoteType(expr->getType())});
  }

  Expr *
  visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *expr) {
    Breadcrumb bc(bcs, expr);
    /// TODO(TF-723): Finish the implementation.
    return makeQuote("StringInterpolation", {quoteType(expr->getType())});
  }

  Expr *visitIsExpr(IsExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return makeQuote("Is", {quoteExpr(expr->getSubExpr()),
                            quoteType(expr->getCastTypeLoc().getType()),
                            quoteType(expr->getType())});
  }

  // TODO(TF-723): Quote key paths.
  UNSUPPORTED_EXPR(KeyPathApplicationExpr)
  UNSUPPORTED_EXPR(KeyPathDotExpr)
  UNSUPPORTED_EXPR(KeyPathExpr)

  Expr *visitLazyInitializerExpr(LazyInitializerExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return quoteExpr(expr->getSubExpr());
  }

  Expr *visitMagicIdentifierLiteralExpr(MagicIdentifierLiteralExpr *expr) {
    Breadcrumb bc(bcs, expr);
    StringRef kind;
    switch (expr->getKind()) {
    case MagicIdentifierLiteralExpr::File:
      kind = "file";
      break;
    case MagicIdentifierLiteralExpr::Line:
      kind = "line";
      break;
    case MagicIdentifierLiteralExpr::Column:
      kind = "column";
      break;
    case MagicIdentifierLiteralExpr::Function:
      kind = "function";
      break;
    case MagicIdentifierLiteralExpr::DSOHandle:
      kind = "dsohandle";
      break;
    }
    return makeQuote("MagicLiteral",
                     {quoteString(kind), quoteType(expr->getType())});
  }

  Expr *visitMakeTemporarilyEscapableExpr(MakeTemporarilyEscapableExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return quoteExpr(expr->getSubExpr());
  }

  Expr *visitMemberRefExpr(MemberRefExpr *expr) {
    // TODO(TF-715): Allow @quoted on more decls.
    Breadcrumb bc(bcs, expr);
    if (dyn_cast<TypeExpr>(expr->getBase())) {
      return makeQuote("Name", {quoteValue(expr->getMember()),
                                quoteSymbol(expr->getMember().getDecl()),
                                quoteType(expr->getType())});
    } else {
      return makeQuote("Member", {quoteExpr(expr->getBase()),
                                  quoteValue(expr->getMember()),
                                  quoteSymbol(expr->getMember().getDecl()),
                                  quoteType(expr->getType())});
    }
  }

  Expr *visitNilLiteralExpr(NilLiteralExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return makeQuote("NilLiteral", {quoteType(expr->getType())});
  }

  // TODO(TF-723): Quote #selector.
  UNSUPPORTED_EXPR(ObjCSelectorExpr)

  // TODO(TF-723): Quote playground literals.
  UNSUPPORTED_EXPR(ObjectLiteralExpr)

  // NOTE: TapExpr nodes are handled during quoting of their parent nodes.
  // We don't have a directly equivalent node in our trees.
  UNSUPPORTED_EXPR(OpaqueValueExpr)

  Expr *visitOpenExistentialExpr(OpenExistentialExpr *expr) {
    Breadcrumb bc(bcs, expr);
    if (auto call = dyn_cast<CallExpr>(expr->getSubExpr())) {
      if (auto dot = dyn_cast<DotSyntaxCallExpr>(call->getFn())) {
        if (auto ref = dyn_cast<DeclRefExpr>(dot->getFn())) {
          auto member =
              makeQuote("Member", {quoteExpr(expr->getExistentialValue()),
                                   quoteValue(ref->getDeclRef()),
                                   quoteSymbol(ref->getDecl()),
                                   quoteType(dot->getType())});
          return makeQuote(
              "Call", {member, quoteLabels(call->getArgumentLabels()),
                       quoteArgs(call->getArg()), quoteType(call->getType())});
        }
      }
    }
    return unknownTree(expr);
  }

  // TODO(TF-723): Quote optional chaining.
  UNSUPPORTED_EXPR(OptionalEvaluationExpr)

  Expr *visitOptionalTryExpr(OptionalTryExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return makeQuote("OptionalTry", {quoteExpr(expr->getSubExpr()),
                                     quoteType(expr->getType())});
  }

  // TODO(TF-724): Quote decls.
  UNSUPPORTED_EXPR(OtherConstructorDeclRefExpr)

  // NOTE: If typechecking encountered errors, there's no point in quoting
  // anyway because none of that code will make it to runtime.
  UNSUPPORTED_EXPR(OverloadedDeclRefExpr)

  Expr *visitParenExpr(ParenExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return quoteExpr(expr->getSubExpr());
  }

  Expr *visitPrefixUnaryExpr(PrefixUnaryExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return makeQuote("Prefix",
                     {quoteExpr(expr->getFn()), quoteExpr(expr->getArg()),
                      quoteType(expr->getType())});
  }

  Expr *visitPostfixUnaryExpr(PostfixUnaryExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return makeQuote("Postfix",
                     {quoteExpr(expr->getFn()), quoteExpr(expr->getArg()),
                      quoteType(expr->getType())});
  }

  Expr *visitQuoteLiteralExpr(QuoteLiteralExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return makeQuote(
        "Meta", {quoteExpr(expr->getSubExpr()), quoteType(expr->getType())});
  }

  // TODO(TF-724): Quote decls.
  UNSUPPORTED_EXPR(RebindSelfInConstructorExpr)

  // NOTE: If typechecking encountered errors, there's no point in quoting
  // anyway because none of that code will make it to runtime.
  UNSUPPORTED_EXPR(SequenceExpr)

  Expr *visitStringLiteralExpr(StringLiteralExpr *expr) {
    Breadcrumb bc(bcs, expr);
    // TODO(TF-733): Figure out why this is not good enough.
    // auto type = expr->getType();
    auto type = ctx.getStringDecl()->getDeclaredType();
    return makeQuote("StringLiteral",
                     {quoteString(expr->getValue()), quoteType(type)});
  }

  Expr *visitSubscriptExpr(SubscriptExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return makeQuote("Subscript", {quoteExpr(expr->getBase()),
                                   quoteSymbol(expr->getMember().getDecl()),
                                   quoteLabels(expr->getArgumentLabels()),
                                   quoteExprs(expr->getIndex()),
                                   quoteType(expr->getType())});
  }

  Expr *visitSuperRefExpr(SuperRefExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return makeQuote("Super", {quoteType(expr->getType())});
  }

  // NOTE: TapExpr nodes are handled during quoting of their parent nodes.
  // We don't have a directly equivalent node in our trees.
  UNSUPPORTED_EXPR(TapExpr)

  Expr *visitTryExpr(TryExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return makeQuote(
        "Try", {quoteExpr(expr->getSubExpr()), quoteType(expr->getType())});
  }

  Expr *visitTupleElementExpr(TupleElementExpr *expr) {
    Breadcrumb bc(bcs, expr);
    return makeQuote("TupleElement", {quoteExpr(expr->getBase()),
                                      quoteInt(expr->getFieldNumber()),
                                      quoteType(expr->getType())});
  }

  Expr *visitTupleExpr(TupleExpr *expr) {
    Breadcrumb bc(bcs, expr);
    // TODO(TF-731): Figure out why expr->getType() is sometimes null.
    auto quotedType = expr->getType() ? quoteType(expr->getType())
                                      : makeQuote("UnknownTree", {});
    return makeQuote("Tuple", {quoteLabels(expr->getElementNames()),
                               quoteExprs(expr->getElements()), quotedType});
  }

  // NOTE: TypeExpr nodes are handled during quoting of their parent nodes.
  // We don't have a directly equivalent node in our trees.
  UNSUPPORTED_EXPR(TypeExpr)

  Expr *visitUnquoteExpr(UnquoteExpr *expr) {
    Breadcrumb bc(bcs, expr);
    auto value = new (ctx)
        UnresolvedDotExpr(expr->getSubExpr(), SourceLoc(),
                          ctx.getIdentifier("expression"), DeclNameLoc(),
                          /*Implicit=*/true);
    return makeQuote("Unquote", {quoteExpr(expr->getSubExpr()), value,
                                 quoteType(expr->getType())});
  }

  // NOTE: If typechecking encountered errors, there's no point in quoting
  // anyway because none of that code will make it to runtime.
  UNSUPPORTED_EXPR(UnresolvedDeclRefExpr)
  UNSUPPORTED_EXPR(UnresolvedDotExpr)
  UNSUPPORTED_EXPR(UnresolvedMemberExpr)
  UNSUPPORTED_EXPR(UnresolvedPatternExpr)
  UNSUPPORTED_EXPR(UnresolvedSpecializeExpr)

  Expr *visitVarargExpansionExpr(VarargExpansionExpr *expr) {
    Breadcrumb bc(bcs, expr);
    if (auto exprs = dyn_cast<ArrayExpr>(expr->getSubExpr())) {
      return makeQuote("Varargs", {quoteExprs(exprs->getElements()),
                                   quoteType(expr->getType())});
    } else {
      return unknownTree(expr);
    }
  }

#define UNSUPPORTED_STMT(STMT)                                                 \
  Expr *visit##STMT(STMT *stmt) {                                              \
    Breadcrumb bc(bcs, stmt);                                                  \
    return unknownTree(stmt);                                                  \
  }

  // NOTE: BraceStmts are supported within quoteBody but not in standalone form.
  UNSUPPORTED_STMT(BraceStmt)

  Expr *visitBreakStmt(BreakStmt *stmt) {
    Breadcrumb bc(bcs, stmt);
    return makeQuote("Break", {quoteLabel(stmt->getTargetName())});
  }

  // TODO(TF-714): Quote patterns and trees related to pattern matching.
  UNSUPPORTED_STMT(CaseStmt)

  // TODO(TF-714): Quote patterns and trees related to pattern matching.
  UNSUPPORTED_STMT(CatchStmt)

  Expr *visitContinueStmt(ContinueStmt *stmt) {
    Breadcrumb bc(bcs, stmt);
    return makeQuote("Continue", {quoteLabel(stmt->getTargetName())});
  }

  Expr *visitDeferStmt(DeferStmt *stmt) {
    Breadcrumb bc(bcs, stmt);
    return makeQuote("Defer", {quoteBody(stmt->getBodyAsWritten())});
  }

  // TODO(TF-714): Quote patterns and trees related to pattern matching.
  UNSUPPORTED_STMT(DoCatchStmt)

  Expr *visitDoStmt(DoStmt *stmt) {
    Breadcrumb bc(bcs, stmt);
    return makeQuote("Do", {quoteLabel(stmt->getLabelInfo().Name),
                            quoteBody(stmt->getBody())});
  }

  // TODO(TF-724): Quote decls.
  UNSUPPORTED_STMT(FailStmt)

  // TODO(TF-714): Quote patterns and trees related to pattern matching.
  UNSUPPORTED_STMT(FallthroughStmt)

  Expr *visitForEachStmt(ForEachStmt *stmt) {
    Breadcrumb bc(bcs, stmt);
    Expr *quotedName = nullptr;
    if (auto pat = dyn_cast<NamedPattern>(stmt->getPattern())) {
      Breadcrumb bc(bcs, pat);
      quotedName = quoteName(pat->getDecl());
    }
    if (!quotedName) {
      quotedName = unknownName(stmt);
    }
    return makeQuote("For", {quoteLabel(stmt->getLabelInfo().Name), quotedName,
                             quoteExpr(stmt->getSequence()),
                             quoteBody(stmt->getBody())});
  }

  Expr *visitGuardStmt(GuardStmt *stmt) {
    Breadcrumb bc(bcs, stmt);
    return makeQuote("Guard", {quoteCond(stmt), quoteBody(stmt->getBody())});
  }

  Expr *visitIfStmt(IfStmt *stmt) {
    Breadcrumb bc(bcs, stmt);
    auto quotedElse =
        stmt->getElseStmt() ? quoteBody(stmt->getElseStmt()) : quoteArray({});
    return makeQuote("If",
                     {quoteLabel(stmt->getLabelInfo().Name), quoteCond(stmt),
                      quoteBody(stmt->getThenStmt()), quotedElse});
  }

  // NOTE: PoundAssertStmt are supported within quoteBody but not in
  // standalone form.
  UNSUPPORTED_STMT(PoundAssertStmt)

  Expr *visitRepeatWhileStmt(RepeatWhileStmt *stmt) {
    Breadcrumb bc(bcs, stmt);
    return makeQuote("Repeat",
                     {quoteLabel(stmt->getLabelInfo().Name),
                      quoteBody(stmt->getBody()), quoteExpr(stmt->getCond())});
  }

  Expr *visitReturnStmt(ReturnStmt *stmt) {
    Breadcrumb bc(bcs, stmt);
    if (stmt->hasResult()) {
      return makeQuote("Return", {quoteExpr(stmt->getResult())});
    } else {
      auto emptyResult = TupleExpr::createEmpty(ctx, SourceLoc(), SourceLoc(),
                                                /*Implicit=*/true);
      return makeQuote("Return", {quoteExpr(emptyResult)});
    }
  }

  // TODO(TF-714): Quote patterns and trees related to pattern matching.
  UNSUPPORTED_STMT(SwitchStmt)

  Expr *visitThrowStmt(ThrowStmt *stmt) {
    Breadcrumb bc(bcs, stmt);
    return makeQuote("Throw", {quoteExpr(stmt->getSubExpr())});
  }

  Expr *visitWhileStmt(WhileStmt *stmt) {
    Breadcrumb bc(bcs, stmt);
    return makeQuote("While", {quoteLabel(stmt->getLabelInfo().Name),
                               quoteCond(stmt), quoteBody(stmt->getBody())});
  }

  // NOTE: Doesn't look like this is an official feature so we won't be
  // supporting it for now.
  UNSUPPORTED_STMT(YieldStmt)

#undef UNSUPPORTED_STMT

#define UNSUPPORTED_DECL(DECL)                                                 \
  Expr *visit##DECL(DECL *decl) {                                              \
    Breadcrumb bc(bcs, decl);                                                  \
    return unknownTree(decl);                                                  \
  }

  // TODO(TF-724): Quote decls.
  UNSUPPORTED_DECL(AccessorDecl)
  UNSUPPORTED_DECL(AssociatedTypeDecl)
  UNSUPPORTED_DECL(ConstructorDecl)
  UNSUPPORTED_DECL(ClassDecl)
  UNSUPPORTED_DECL(DestructorDecl)
  UNSUPPORTED_DECL(EnumCaseDecl)
  UNSUPPORTED_DECL(EnumDecl)
  UNSUPPORTED_DECL(EnumElementDecl)
  UNSUPPORTED_DECL(ExtensionDecl)

  Expr *visitFuncDecl(FuncDecl *decl) {
    return makeQuote("Function",
                     {quoteName(decl), quoteParams(decl->getParameters()),
                      quoteBody(decl->getBody())});
  }

  // TODO(TF-724): Quote decls.
  UNSUPPORTED_DECL(GenericTypeParamDecl)

  // NOTE: Doesn't have a representation in quotes.
  // We simply take active elements and use them instead of this decl.
  UNSUPPORTED_DECL(IfConfigDecl)

  // TODO(TF-724): Quote decls.
  UNSUPPORTED_DECL(ImportDecl)
  UNSUPPORTED_DECL(MissingMemberDecl)
  UNSUPPORTED_DECL(ModuleDecl)

  Expr *visitParamDecl(ParamDecl *decl) {
    Breadcrumb bc(bcs, decl);
    return makeQuote("Parameter",
                     {quoteLabel(decl->getArgumentName()), quoteName(decl)});
  }

  Expr *visitPatternBindingDecl(PatternBindingDecl *decl) {
    Breadcrumb bc(bcs, decl);
    if (auto var = decl->getSingleVar()) {
      Breadcrumb bc(bcs, var);
      if (var->isLet()) {
        return makeQuote("Let", {quoteName(var), quoteExpr(decl->getInit(0))});
      } else {
        return makeQuote("Var", {quoteName(var), quoteExpr(decl->getInit(0))});
      }
    } else {
      // TODO(TF-714): Quote patterns and trees related to pattern
      // matching.
      return unknownTree(decl);
    }
  }

  // NOTE: Doesn't have a representation in quotes since this language construct
  // belongs to the compilation stage that precedes quotes.
  UNSUPPORTED_DECL(PoundDiagnosticDecl)

  // TODO(TF-724): Quote decls.
  UNSUPPORTED_DECL(PrecedenceGroupDecl)
  UNSUPPORTED_DECL(ProtocolDecl)
  UNSUPPORTED_DECL(OpaqueTypeDecl)
  UNSUPPORTED_DECL(OperatorDecl)
  UNSUPPORTED_DECL(StructDecl)
  UNSUPPORTED_DECL(SubscriptDecl)
  UNSUPPORTED_DECL(TopLevelCodeDecl)
  UNSUPPORTED_DECL(TypeAliasDecl)
  UNSUPPORTED_DECL(VarDecl)

#undef UNSUPPORTED_DECL

private:
  Expr *quoteArgs(Expr *arg) {
    Breadcrumb bc(bcs, arg);
    if (auto args = dyn_cast<ParenExpr>(arg)) {
      return quoteExprs({args->getSubExpr()});
    } else if (auto args = dyn_cast<TupleExpr>(arg)) {
      return quoteExprs(args->getElements());
    } else {
      return quoteArray(unknownTree(arg));
    }
  }

  Expr *quoteBody(Stmt *stmt) {
    Breadcrumb bc(bcs, stmt);
    if (auto body = dyn_cast<BraceStmt>(stmt)) {
      SmallVector<Expr *, 4> quotedBody;
      std::function<void(ASTNode)> handleNode = [&](ASTNode node) {
        if (auto expr = node.dyn_cast<Expr *>()) {
          quotedBody.push_back(quoteExpr(expr));
          return;
        }
        if (auto stmt = node.dyn_cast<Stmt *>()) {
          if (isa<PoundAssertStmt>(stmt)) {
            return;
          }
          quotedBody.push_back(quoteStmt(stmt));
          return;
        }
        auto decl = node.get<Decl *>();
        if (auto poundIf = dyn_cast<IfConfigDecl>(decl)) {
          for (auto elem : poundIf->getActiveClauseElements()) {
            handleNode(elem);
          }
          return;
        }
        if (isa<PoundDiagnosticDecl>(decl)) {
          return;
        }
        if (isa<VarDecl>(decl)) {
          return;
        }
        quotedBody.push_back(quoteDecl(decl));
      };
      for (auto elem : body->getElements()) {
        handleNode(elem);
      }
      return quoteArray(quotedBody);
    } else {
      return quoteArray({quoteStmt(stmt)});
    }
  }

  template <typename T> Expr *quoteCond(T *stmt) {
    SmallVector<Expr *, 4> quotedCond;
    for (auto element : stmt->getCond()) {
      Breadcrumb bc(bcs, &element);
      Expr *quotedElement;
      switch (element.getKind()) {
      case StmtConditionElement::ConditionKind::CK_Boolean:
        quotedElement = quoteExpr(element.getBoolean());
        break;
      case StmtConditionElement::ConditionKind::CK_PatternBinding:
        // TODO(TF-714): Quote patterns and trees related to pattern
        // matching.
        quotedElement = unknownTree(stmt);
        break;
      case StmtConditionElement::ConditionKind::CK_Availability:
        // TODO(TF-723): Quote #available.
        quotedElement = unknownTree(stmt);
        break;
      }
      quotedCond.push_back(quotedElement);
    }
    return quoteArray(quotedCond);
  }

  Expr *quoteDecl(Decl *decl) { return visit(decl); }

  Expr *quoteExpr(Expr *expr) { return visit(expr); }

  Expr *quoteExprs(ArrayRef<Expr *> exprs) {
    SmallVector<Expr *, 4> quotedExprs;
    for (auto expr : exprs) {
      quotedExprs.push_back(quoteExpr(expr));
    }
    return quoteArray(quotedExprs);
  }

  Expr *quoteLabel(Identifier id) {
    if (id.empty()) {
      return quoteNil();
    } else {
      return quoteString(id.str());
    }
  }

  Expr *quoteLabels(ArrayRef<Identifier> ids) {
    SmallVector<Expr *, 4> quotedLabels;
    for (auto id : ids) {
      quotedLabels.push_back(quoteLabel(id));
    }
    return quoteArray(quotedLabels);
  }

  Expr *quoteName(ValueDecl *decl) {
    Type type;
    if (auto decl2 = dyn_cast<FuncDecl>(decl)) {
      SmallVector<AnyFunctionType::Param, 4> paramTypes;
      for (auto param : decl2->getParameters()->getArray()) {
        auto flags = ParameterTypeFlags();
        if (param->isInOut()) {
          flags = flags.withInOut(true);
        }
        auto paramType =
            AnyFunctionType::Param(param->getType(), Identifier(), flags);
        paramTypes.push_back(paramType);
      }
      auto retType = decl2->getBodyResultTypeLoc().getTypeRepr()
                         ? decl2->getBodyResultTypeLoc().getType()
                         : decl2->getResultInterfaceType();
      if (retType) {
        type = FunctionType::get(paramTypes, retType);
      }
    } else if (auto decl2 = dyn_cast<VarDecl>(decl)) {
      type = decl2->getType();
      if (auto decl2 = dyn_cast<ParamDecl>(decl)) {
        if (decl2->isInOut()) {
          type = InOutType::get(type);
        }
      }
    }
    Expr *quotedType = type ? quoteType(type) : unknownTree(decl);
    return makeQuote("Name", {quoteString(decl->getBaseName().userFacingName()),
                              quoteSymbol(decl), quotedType});
  }

  Expr *quoteNil() {
    return new (ctx) NilLiteralExpr(SourceLoc(), /*Implicit=*/true);
  }

  Expr *quoteParams(ParameterList *params) {
    SmallVector<Expr *, 4> quotedParams;
    for (auto param : params->getArray()) {
      quotedParams.push_back(quoteDecl(param));
    }
    return quoteArray(quotedParams);
  }

  Expr *quoteStmt(Stmt *stmt) { return visit(stmt); }

  Expr *quoteType(Type type) {
    return type ? typeQuoter.visit(type) : unknownTree(type);
  }

  Expr *quoteValue(ConcreteDeclRef ref) {
    // TODO(TF-740): Obtain value exactly as written by the programmer.
    // I'm currently not sure how to do that since DeclRefExpr doesn't have
    // a correct source range.
    if (auto ctor = dyn_cast<ConstructorDecl>(ref.getDecl())) {
      auto type = ctor->getResultInterfaceType();
      if (auto nomType = type->getAs<NominalType>()) {
        return quoteString(nomType->getDecl()->getBaseName().userFacingName());
      }
    }
    return quoteString(ref.getDecl()->getBaseName().userFacingName());
  }

  // TODO(TF-735): Implement ExpressibleByQuoteLiteral.
  Expr *makeQuote(const char *name, ArrayRef<Expr *> quotedSubnodes) {
    auto nodeInit = new (ctx) UnresolvedDeclRefExpr(
        ctx.getIdentifier(name), DeclRefKind::Ordinary, DeclNameLoc());
    return CallExpr::createImplicit(ctx, nodeInit, quotedSubnodes, {});
  }
};

} // end anonymous namespace

Expr *TypeChecker::quoteExpr(Expr *expr, DeclContext *dc) {
  assert(expr->getType());

  Breadcrumbs bcs;
  ASTQuoter astQuoter(*this, bcs);
  Expr *quotedExpr = astQuoter.visit(expr);
  if (!quotedExpr) {
    return nullptr;
  }

  auto quoteType = getTypeOfQuoteExpr(expr->getType(), expr->getLoc());
  if (!quoteType) {
    return nullptr;
  }

  auto quoteClassType = quoteType->getAs<BoundGenericClassType>();
  if (!quoteClassType) {
    return nullptr;
  }

  auto quoteRef =
      new (Context) UnresolvedDeclRefExpr(quoteClassType->getDecl()->getName(),
                                          DeclRefKind::Ordinary, DeclNameLoc());
  SmallVector<TypeLoc, 4> quoteTargs;
  for (auto targ : quoteClassType->getGenericArgs()) {
    auto targRepr = new (Context) FixedTypeRepr(targ, SourceLoc());
    quoteTargs.push_back(TypeLoc(targRepr));
  }
  auto quoteInit = UnresolvedSpecializeExpr::create(
      Context, quoteRef, SourceLoc(), quoteTargs, SourceLoc());
  quoteInit->setImplicit();
  Expr *quoteCall =
      CallExpr::createImplicit(Context, quoteInit, {quotedExpr}, {});

  // TODO(TF-727): Improve error reporting when quoting fails.
  if (!typeCheckExpression(quoteCall, dc)) {
    return nullptr;
  }

  // TODO(TF-734): Get to the bottom of why we need this workaround.
  if (auto call2 = dyn_cast<CallExpr>(quoteCall)) {
    if (auto init2 = dyn_cast<ConstructorRefCallExpr>(call2->getFn())) {
      init2->getBase()->setImplicit();
      return quoteCall;
    }
  }

  return nullptr;
}

Type TypeChecker::getTypeOfQuoteExpr(Type exprType, SourceLoc loc) {
  assert(exprType);
  if (!Context.getQuoteModule()) {
    diagnose(loc, diag::quote_literal_no_quote_module);
    return Type();
  }
  if (auto fnExprType = exprType->getAs<FunctionType>()) {
    auto n = fnExprType->getParams().size();
    auto quoteClass = Context.getFunctionQuoteDecl(n);
    if (!quoteClass) {
      diagnose(loc, diag::quote_literal_no_function_quote_class, n);
      return Type();
    }
    SmallVector<Type, 4> typeArgs;
    for (auto param : fnExprType->getParams()) {
      auto paramType = param.getPlainType();
      if (param.isInOut()) {
        paramType = getUnsafeMutablePointerType(SourceLoc(), paramType);
      }
      typeArgs.push_back(paramType);
    }
    typeArgs.push_back(fnExprType->getResult());
    return BoundGenericClassType::get(quoteClass, Type(), typeArgs);
  } else {
    auto quoteClass = Context.getQuoteDecl();
    if (!quoteClass) {
      diagnose(loc, diag::quote_literal_no_quote_class);
      return Type();
    }
    if (auto lvalueExprType = exprType->getAs<LValueType>()) {
      exprType = lvalueExprType->getObjectType();
    }
    return BoundGenericClassType::get(quoteClass, Type(), exprType);
  }
}

Type TypeChecker::getTypeOfUnquoteExpr(Type exprType, SourceLoc loc) {
  assert(exprType);
  if (auto genericType = exprType->getAs<BoundGenericClassType>()) {
    auto classDecl = genericType->getDecl();
    auto typeArgs = genericType->getGenericArgs();
    if (classDecl == Context.getQuoteDecl()) {
      return typeArgs[0];
    } else if (classDecl == Context.getFunctionQuoteDecl(typeArgs.size() - 1)) {
      SmallVector<AnyFunctionType::Param, 4> paramTypes;
      for (unsigned i = 0; i < typeArgs.size() - 1; ++i) {
        auto typeArg = typeArgs[i];
        auto flags = ParameterTypeFlags();
        if (auto genericTypeArg = typeArg->getAs<BoundGenericClassType>()) {
          if (genericTypeArg->getDecl() ==
              Context.getUnsafeMutablePointerDecl()) {
            flags = flags.withInOut(true);
          }
        }
        auto paramType = AnyFunctionType::Param(typeArg, Identifier(), flags);
        paramTypes.push_back(paramType);
      }
      return FunctionType::get(paramTypes, typeArgs[typeArgs.size() - 1]);
    } else {
      diagnose(loc, diag::unquote_wrong_type);
      return Type();
    }
  } else {
    diagnose(loc, diag::unquote_wrong_type);
    return Type();
  }
}

Expr *TypeChecker::quoteDecl(Decl *decl, DeclContext *dc) {
  Breadcrumbs bcs;
  ASTQuoter astQuoter(*this, bcs);
  Expr *quotedDecl = astQuoter.visit(decl);
  if (!quotedDecl) {
    return nullptr;
  }

  // TODO(TF-727): Improve error reporting when quoting fails.
  if (!typeCheckExpression(quotedDecl, dc)) {
    return nullptr;
  }

  return quotedDecl;
}

Type TypeChecker::getTypeOfQuoteDecl(SourceLoc loc) {
  if (!Context.getQuoteModule()) {
    diagnose(loc, diag::quote_literal_no_quote_module);
    return Type();
  }
  auto treeProto = Context.getTreeDecl();
  if (!treeProto) {
    diagnose(loc, diag::quote_literal_no_tree_proto);
    return Type();
  }
  return treeProto->getDeclaredType();
}
