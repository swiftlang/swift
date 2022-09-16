#include "swift/AST/CASTBridging.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTNode.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/TypeRepr.h"

using namespace swift;

template <typename T>
inline llvm::ArrayRef<T> getArrayRef(BridgedArrayRef bridged) {
  return {static_cast<const T *>(bridged.data), size_t(bridged.numElements)};
}

BridgedIdentifier
SwiftASTContext_getIdentifier(void *ctx, const char *_Nullable str, long len) {
  return static_cast<ASTContext *>(ctx)
      ->getIdentifier(StringRef{str, size_t(len)})
      .get();
}

void *SwiftImportDecl_create(void *ctx, void *dc, void *importLoc, char kind,
                             void *kindLoc, BridgedArrayRef path,
                             BridgedArrayRef pathLocs) {
  assert(path.numElements == pathLocs.numElements);
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  ImportPath::Builder importPath;
  for (auto p : llvm::zip(getArrayRef<Identifier>(path),
                          getArrayRef<SourceLoc>(pathLocs))) {
    Identifier ident;
    SourceLoc loc;
    std::tie(ident, loc) = p;
    importPath.push_back(ident, loc);
  }
  return ImportDecl::create(
      Context, static_cast<DeclContext *>(dc), *(SourceLoc *)&importLoc,
      static_cast<ImportKind>(kind), *(SourceLoc *)&kindLoc,
      std::move(importPath).get());
}

void *BridgedSourceLoc_advanced(void *loc, long len) {
  SourceLoc l = ((SourceLoc *)&loc)->getAdvancedLoc(len);
  return &l; // TODO: what?
}

void *SwiftTopLevelCodeDecl_createStmt(void *ctx, void *DC, void *startLoc,
                                       void *element, void *endLoc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  auto *S = static_cast<Stmt *>(element);
  auto Brace =
      BraceStmt::create(Context, *(SourceLoc *)&startLoc,
                        {S}, *(SourceLoc *)&endLoc,
                        /*Implicit=*/true);
  auto *TLCD =
      new (Context) TopLevelCodeDecl(static_cast<DeclContext *>(DC), Brace);
  return (Decl *)TLCD;
}

void *SwiftTopLevelCodeDecl_createExpr(void *ctx, void *DC, void *startLoc,
                                       void *element, void *endLoc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  auto *E = static_cast<Expr *>(element);
  auto Brace =
      BraceStmt::create(Context, *(SourceLoc *)&startLoc,
                        {E}, *(SourceLoc *)&endLoc,
                        /*Implicit=*/true);
  auto *TLCD =
      new (Context) TopLevelCodeDecl(static_cast<DeclContext *>(DC), Brace);
  return (Decl *)TLCD;
}

void *SwiftSequenceExpr_create(void *ctx, BridgedArrayRef exprs) {
  return SequenceExpr::create(*static_cast<ASTContext *>(ctx),
                              getArrayRef<Expr *>(exprs));
}

void *SwiftTupleExpr_create(void *ctx, void *lparen, BridgedArrayRef subs,
                            void *rparen) {
  return TupleExpr::create(
      *static_cast<ASTContext *>(ctx), *(SourceLoc *)&lparen,
      getArrayRef<Expr *>(subs), {}, {}, *(SourceLoc *)&rparen,
      /*Implicit*/ false);
}

void *SwiftFunctionCallExpr_create(void *ctx, void *fn, void *args) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  TupleExpr *TE = static_cast<TupleExpr *>(args);
  SmallVector<Argument, 8> arguments;
  for (unsigned i = 0; i < TE->getNumElements(); ++i) {
    arguments.emplace_back(TE->getElementNameLoc(i), TE->getElementName(i),
                           TE->getElement(i));
  }
  auto *argList = ArgumentList::create(Context, TE->getLParenLoc(), arguments,
                                       TE->getRParenLoc(), None,
                                       /*isImplicit*/ false);
  return CallExpr::create(Context, static_cast<Expr *>(fn), argList,
                          /*implicit*/ false);
}

void *SwiftIdentifierExpr_create(void *ctx, const char *base, void *loc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  auto name = DeclNameRef{
      swift::Identifier::getFromOpaquePointer(const_cast<char *>(base))};
  Expr *E = new (Context) UnresolvedDeclRefExpr(
      name, DeclRefKind::Ordinary, DeclNameLoc{*(SourceLoc *)&loc});
  return E;
}

void *SwiftStringLiteralExpr_create(void *ctx, const char *_Nullable string,
                                    long len, void *TokenLoc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  return new (Context) StringLiteralExpr(StringRef{string, size_t(len)},
                                         *(SourceLoc *)&TokenLoc);
}

void *SwiftIntegerLiteralExpr_create(void *ctx, const char *_Nullable string,
                                long len, void *TokenLoc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  return new (Context) IntegerLiteralExpr(StringRef{string, size_t(len)},
                                         *(SourceLoc *)&TokenLoc);
}

void *SwiftBooleanLiteralExpr_create(void *ctx, bool value, void *TokenLoc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  return new (Context) BooleanLiteralExpr(value, *(SourceLoc *)&TokenLoc);
}

void *SwiftVarDecl_create(void *ctx, const char *_Nullable nameId,
                          void *loc, bool isStatic, bool isLet, void *dc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  return new (Context) VarDecl(isStatic,
                               isLet ? VarDecl::Introducer::Let : VarDecl::Introducer::Var,
                               *(SourceLoc *)&loc, Identifier::getFromOpaquePointer((void *)nameId),
                               reinterpret_cast<DeclContext *>(dc));
}

void *IfStmt_create(void *ctx, void *ifLoc, void *cond, void *_Nullable then, void *_Nullable elseLoc,
                    void *_Nullable elseStmt) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  return new (Context) IfStmt(*(SourceLoc *)&ifLoc, (Expr *)cond, (Stmt *)then, *(SourceLoc *)&elseLoc,
                              (Stmt *)elseStmt, None, Context);
}

void *BraceStmt_create(void *ctx, void *lbloc, BridgedArrayRef elements, void *rbloc) {
  return BraceStmt::create(*static_cast<ASTContext *>(ctx), *(SourceLoc *)&lbloc,
                               getArrayRef<ASTNode>(elements),
                              *(SourceLoc *)&rbloc);
}

void *ParamDecl_create(void *ctx, void *loc,
                       void *_Nullable argLoc, void *_Nullable argName,
                       void *_Nullable paramLoc, void *_Nullable paramName,
                       void *declContext) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  return new (Context) ParamDecl(*(SourceLoc *)&loc, *(SourceLoc *)&argLoc,
                                 Identifier::getFromOpaquePointer(argName),
                                 *(SourceLoc *)&paramLoc,
                                 Identifier::getFromOpaquePointer(paramName),
                                 (DeclContext *)declContext);
}

void *FuncDecl_create(void *ctx, void *staticLoc, bool isStatic, void *funcLoc,
                      const char *name, void *nameLoc,
                      bool isAsync, void *_Nullable asyncLoc,
                      bool throws, void *_Nullable throwsLoc,
                      void *paramLLoc, BridgedArrayRef params, void *paramRLoc,
                      void *_Nullable body, void *_Nullable returnType,
                      void *declContext) {
  auto *paramList = ParameterList::create(
      *static_cast<ASTContext *>(ctx), *(SourceLoc *)&paramLLoc,
      getArrayRef<ParamDecl *>(params), *(SourceLoc *)&paramRLoc);
  auto declName =
      DeclName(*static_cast<ASTContext *>(ctx),
               Identifier::getFromOpaquePointer((void *)name), paramList);
  auto *out = FuncDecl::create(
      *static_cast<ASTContext *>(ctx), *(SourceLoc *)&staticLoc,
      isStatic ? StaticSpellingKind::KeywordStatic : StaticSpellingKind::None,
      *(SourceLoc *)&funcLoc, declName, *(SourceLoc *)&nameLoc, isAsync,
      *(SourceLoc *)&asyncLoc, throws, *(SourceLoc *)&throwsLoc, nullptr,
      paramList, (TypeRepr *)returnType, (DeclContext *)declContext);
  out->setBody((BraceStmt *)body, FuncDecl::BodyKind::Parsed);

  return static_cast<Decl *>(out);
}

void *SimpleIdentTypeRepr_create(void *ctx, void *loc, const char *id) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  return new (Context) SimpleIdentTypeRepr(DeclNameLoc(*(SourceLoc *)&loc),
                                           DeclNameRef(Identifier::getFromOpaquePointer((void *)id)));
}

void *UnresolvedDotExpr_create(void *ctx, void *base, void *dotLoc, const char *name, void *nameLoc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
      return new (Context) UnresolvedDotExpr((Expr *)base, *(SourceLoc *)&dotLoc,
                                         DeclNameRef(Identifier::getFromOpaquePointer((void *)name)),
                                         DeclNameLoc(*(SourceLoc *)&nameLoc), false);
}

void *ClosureExpr_create(void *ctx, void *body, void *dc) {
  DeclAttributes attributes;
  SourceRange bracketRange;
  SourceLoc asyncLoc;
  SourceLoc throwsLoc;
  SourceLoc arrowLoc;
  SourceLoc inLoc;

  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  auto *out = new (Context) ClosureExpr(attributes, bracketRange, nullptr,
                                        nullptr, asyncLoc, throwsLoc, arrowLoc,
                                        inLoc, nullptr, 0, (DeclContext *)dc);
  out->setBody((BraceStmt *)body, true);
  return (Expr *)out;
}

void NominalTypeDecl_setMembers(void *decl, BridgedArrayRef members) {
  auto declMembers = getArrayRef<Decl *>(members);
  for (auto m : declMembers)
    ((NominalTypeDecl *)decl)->addMember(m);
}

DeclContextAndDecl StructDecl_create(void *ctx, void *loc, const char *name, void *nameLoc,
                        void *dc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  auto *out = new (Context) StructDecl(SourceLoc(), // *(SourceLoc *)&loc,
                                       Identifier::getFromOpaquePointer((void *)name),
                                       SourceLoc(), // *(SourceLoc *)&nameLoc,
                                       {}, nullptr,
                                       (DeclContext *)dc);
  out->setImplicit(); // TODO: remove this.
  return {(DeclContext *)out, (NominalTypeDecl *)out, (Decl *)out};
}

DeclContextAndDecl ClassDecl_create(void *ctx, void *loc, const char *name, void *nameLoc,
                       void *dc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  auto *out = new (Context) ClassDecl(SourceLoc(), // *(SourceLoc *)&loc,
                                      Identifier::getFromOpaquePointer((void *)name),
                                      SourceLoc(), // *(SourceLoc *)&nameLoc,
                                      {}, nullptr,
                                      (DeclContext *)dc, false);
  out->setImplicit(); // TODO: remove this.
  return {(DeclContext *)out, (NominalTypeDecl *)out, (Decl *)out};
}

void TopLevelCodeDecl_dump(void *decl) { ((TopLevelCodeDecl *)decl)->dump(); }

void Expr_dump(void *expr) { ((Expr *)expr)->dump(); }
void Decl_dump(void *expr) { ((Decl *)expr)->dump(); }
void Stmt_dump(void *expr) { ((Stmt *)expr)->dump(); }