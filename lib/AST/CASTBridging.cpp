#include "swift/AST/CASTBridging.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTNode.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsCommon.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericParamList.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/TypeRepr.h"

using namespace swift;

template <typename T>
inline llvm::ArrayRef<T> getArrayRef(BridgedArrayRef bridged) {
  return {static_cast<const T *>(bridged.data), size_t(bridged.numElements)};
}

static SourceLoc getSourceLocFromPointer(void *loc) {
  auto smLoc = llvm::SMLoc::getFromPointer((const char *)loc);
  return SourceLoc(smLoc);
}

namespace {
  struct BridgedDiagnosticImpl {
    InFlightDiagnostic inFlight;
    std::vector<StringRef> textBlobs;

    BridgedDiagnosticImpl(const BridgedDiagnosticImpl&) = delete;
    BridgedDiagnosticImpl(BridgedDiagnosticImpl &&) = delete;
    BridgedDiagnosticImpl &operator=(const BridgedDiagnosticImpl&) = delete;
    BridgedDiagnosticImpl &operator=(BridgedDiagnosticImpl &&) = delete;

    ~BridgedDiagnosticImpl() {
      inFlight.flush();
      for (auto text: textBlobs) {
        free((void*)text.data());
      }
    }
  };
}

BridgedDiagnostic SwiftDiagnostic_create(
    void *diagnosticEngine, BridgedDiagnosticSeverity severity,
    void *sourceLocPtr,
    const uint8_t *textPtr, long textLen
) {
  StringRef origText{
    reinterpret_cast<const char *>(textPtr), size_t(textLen)};
  llvm::MallocAllocator mallocAlloc;
  StringRef text = origText.copy(mallocAlloc);

  SourceLoc loc = getSourceLocFromPointer(sourceLocPtr);

  Diag<StringRef> diagID;
  switch (severity) {
  case BridgedDiagnosticSeverity::BridgedError:
    diagID = diag::bridged_error;
    break;
  case BridgedDiagnosticSeverity::BridgedFatalError:
    diagID = diag::bridged_fatal_error;
    break;
  case BridgedDiagnosticSeverity::BridgedNote:
    diagID = diag::bridged_note;
    break;
  case BridgedDiagnosticSeverity::BridgedRemark:
    diagID = diag::bridged_remark;
    break;
  case BridgedDiagnosticSeverity::BridgedWarning:
    diagID = diag::bridged_warning;
    break;
  }

  DiagnosticEngine &diags = *static_cast<DiagnosticEngine *>(diagnosticEngine);
  return new BridgedDiagnosticImpl{diags.diagnose(loc, diagID, text), {text}};
}

/// Highlight a source range as part of the diagnostic.
void SwiftDiagnostic_highlight(
    BridgedDiagnostic diagPtr, void *startLocPtr, void *endLocPtr
) {
  SourceLoc startLoc = getSourceLocFromPointer(startLocPtr);
  SourceLoc endLoc = getSourceLocFromPointer(endLocPtr);

  BridgedDiagnosticImpl *impl = static_cast<BridgedDiagnosticImpl *>(diagPtr);
  impl->inFlight.highlightChars(startLoc, endLoc);
}

/// Add a Fix-It to replace a source range as part of the diagnostic.
void SwiftDiagnostic_fixItReplace(
    BridgedDiagnostic diagPtr, void *replaceStartLocPtr, void *replaceEndLocPtr,
    const uint8_t *newTextPtr, long newTextLen) {

  SourceLoc startLoc = getSourceLocFromPointer(replaceStartLocPtr);
  SourceLoc endLoc = getSourceLocFromPointer(replaceEndLocPtr);

  StringRef origReplaceText{
    reinterpret_cast<const char *>(newTextPtr), size_t(newTextLen)};
  llvm::MallocAllocator mallocAlloc;
  StringRef replaceText = origReplaceText.copy(mallocAlloc);

  BridgedDiagnosticImpl *impl = static_cast<BridgedDiagnosticImpl *>(diagPtr);
  impl->textBlobs.push_back(replaceText);
  impl->inFlight.fixItReplaceChars(startLoc, endLoc, replaceText);
}

/// Finish the given diagnostic and emit it.
void SwiftDiagnostic_finish(BridgedDiagnostic diagPtr) {
  BridgedDiagnosticImpl *impl = static_cast<BridgedDiagnosticImpl *>(diagPtr);
  delete impl;
}

BridgedIdentifier SwiftASTContext_getIdentifier(void *ctx,
                                                const uint8_t *_Nullable str,
                                                long len) {
  return const_cast<void *>(
      static_cast<ASTContext *>(ctx)
          ->getIdentifier(
              StringRef{reinterpret_cast<const char *>(str), size_t(len)})
          .getAsOpaquePointer());
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
      Context, static_cast<DeclContext *>(dc),
      getSourceLocFromPointer(importLoc), static_cast<ImportKind>(kind),
      getSourceLocFromPointer(kindLoc), std::move(importPath).get());
}

void *BridgedSourceLoc_advanced(void *loc, long len) {
  SourceLoc l = getSourceLocFromPointer(loc).getAdvancedLoc(len);
  return const_cast<void *>(l.getOpaquePointerValue());
}

void *SwiftTopLevelCodeDecl_createStmt(void *ctx, void *DC, void *startLoc,
                                       void *element, void *endLoc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  auto *S = static_cast<Stmt *>(element);
  auto Brace = BraceStmt::create(Context, getSourceLocFromPointer(startLoc),
                                 {S}, getSourceLocFromPointer(endLoc),
                                 /*Implicit=*/true);
  auto *TLCD =
      new (Context) TopLevelCodeDecl(static_cast<DeclContext *>(DC), Brace);
  return (Decl *)TLCD;
}

void *SwiftTopLevelCodeDecl_createExpr(void *ctx, void *DC, void *startLoc,
                                       void *element, void *endLoc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  auto *E = static_cast<Expr *>(element);
  auto Brace = BraceStmt::create(Context, getSourceLocFromPointer(startLoc),
                                 {E}, getSourceLocFromPointer(endLoc),
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
                            BridgedArrayRef names,
                            BridgedArrayRef nameLocs,
                            void *rparen) {
  auto &Context = *static_cast<ASTContext *>(ctx);
  return TupleExpr::create(
      Context, getSourceLocFromPointer(lparen),
      getArrayRef<Expr *>(subs), getArrayRef<Identifier>(names),
      getArrayRef<SourceLoc>(nameLocs), getSourceLocFromPointer(rparen),
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

void *SwiftIdentifierExpr_create(void *ctx, BridgedIdentifier base, void *loc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  auto name = DeclNameRef{swift::Identifier::getFromOpaquePointer(base)};
  Expr *E = new (Context) UnresolvedDeclRefExpr(
      name, DeclRefKind::Ordinary, DeclNameLoc{getSourceLocFromPointer(loc)});
  return E;
}

void *SwiftStringLiteralExpr_create(void *ctx, const uint8_t *_Nullable string,
                                    long len, void *TokenLoc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  auto stringRef = Context.AllocateCopy(
      StringRef{reinterpret_cast<const char *>(string), size_t(len)});
  return new (Context)
      StringLiteralExpr(stringRef, getSourceLocFromPointer(TokenLoc));
}

void *SwiftIntegerLiteralExpr_create(void *ctx, const uint8_t *_Nullable string,
                                     long len, void *TokenLoc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  auto stringRef = Context.AllocateCopy(
      StringRef{reinterpret_cast<const char *>(string), size_t(len)});
  return new (Context)
      IntegerLiteralExpr(stringRef, getSourceLocFromPointer(TokenLoc));
}

void *ArrayExpr_create(void *ctx, void *lLoc, BridgedArrayRef elements,
                       BridgedArrayRef commas, void *rLoc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  return ArrayExpr::create(
      Context, getSourceLocFromPointer(lLoc), getArrayRef<Expr *>(elements),
      getArrayRef<SourceLoc>(commas), getSourceLocFromPointer(rLoc));
}

void *SwiftBooleanLiteralExpr_create(void *ctx, bool value, void *TokenLoc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  return new (Context)
      BooleanLiteralExpr(value, getSourceLocFromPointer(TokenLoc));
}

void *SwiftVarDecl_create(void *ctx, BridgedIdentifier _Nullable nameId,
                          void *initExpr, void *loc, bool isStatic, bool isLet,
                          void *dc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  auto name = (UnresolvedDeclRefExpr *)nameId;
  auto sourceLoc = getSourceLocFromPointer(loc);
  auto varDecl = new (Context) VarDecl(
      isStatic, isLet ? VarDecl::Introducer::Let : VarDecl::Introducer::Var,
      sourceLoc, name->getName().getBaseIdentifier(),
      reinterpret_cast<DeclContext *>(dc));
  auto pattern = NamedPattern::createImplicit(Context, varDecl);
  return PatternBindingDecl::create(
      Context, sourceLoc,
      isStatic ? StaticSpellingKind::KeywordStatic : StaticSpellingKind::None,
      sourceLoc, pattern, sourceLoc, (Expr *)initExpr,
      reinterpret_cast<DeclContext *>(dc));
}

void *IfStmt_create(void *ctx, void *ifLoc, void *cond, void *_Nullable then,
                    void *_Nullable elseLoc, void *_Nullable elseStmt) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  return new (Context)
      IfStmt(getSourceLocFromPointer(ifLoc), (Expr *)cond, (Stmt *)then,
             getSourceLocFromPointer(elseLoc), (Stmt *)elseStmt, None, Context);
}

void *ReturnStmt_create(void *ctx, void *loc, void *_Nullable expr) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  return new (Context) ReturnStmt(getSourceLocFromPointer(loc), (Expr *)expr);
}

void *BraceStmt_create(void *ctx, void *lbloc, BridgedArrayRef elements,
                       void *rbloc) {
  llvm::SmallVector<ASTNode, 6> nodes;
  for (auto node : getArrayRef<ASTNodeBridged>(elements)) {
    if (node.kind == ASTNodeKindExpr) {
      auto expr = (Expr *)node.ptr;
      nodes.push_back(expr);
    } else if (node.kind == ASTNodeKindStmt) {
      auto stmt = (Stmt *)node.ptr;
      nodes.push_back(stmt);
    } else {
      assert(node.kind == ASTNodeKindDecl);
      auto decl = (Decl *)node.ptr;
      nodes.push_back(decl);
    }
  }

  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  return BraceStmt::create(Context, getSourceLocFromPointer(lbloc),
                           Context.AllocateCopy(nodes),
                           getSourceLocFromPointer(rbloc));
}

void *ParamDecl_create(void *ctx, void *loc, void *_Nullable argLoc,
                       BridgedIdentifier _Nullable argName,
                       void *_Nullable paramLoc,
                       BridgedIdentifier _Nullable paramName,
                       void *_Nullable type, void *declContext) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  if (!paramName)
    paramName = argName;
  auto paramDecl = new (Context) ParamDecl(
      getSourceLocFromPointer(loc), getSourceLocFromPointer(argLoc),
      Identifier::getFromOpaquePointer(argName),
      getSourceLocFromPointer(paramLoc),
      Identifier::getFromOpaquePointer(paramName), (DeclContext *)declContext);
  paramDecl->setTypeRepr((TypeRepr *)type);
  return paramDecl;
}

struct FuncDeclBridged
FuncDecl_create(void *ctx, void *staticLoc, bool isStatic, void *funcLoc,
                BridgedIdentifier name, void *nameLoc, bool isAsync,
                void *_Nullable asyncLoc, bool throws,
                void *_Nullable throwsLoc, void *paramLLoc,
                BridgedArrayRef params, void *paramRLoc,
                void *_Nullable returnType, void *declContext) {
  auto *paramList = ParameterList::create(
      *static_cast<ASTContext *>(ctx), getSourceLocFromPointer(paramLLoc),
      getArrayRef<ParamDecl *>(params), getSourceLocFromPointer(paramRLoc));
  auto declName = DeclName(*static_cast<ASTContext *>(ctx),
                           Identifier::getFromOpaquePointer(name), paramList);
  auto *out = FuncDecl::create(
      *static_cast<ASTContext *>(ctx), getSourceLocFromPointer(staticLoc),
      isStatic ? StaticSpellingKind::KeywordStatic : StaticSpellingKind::None,
      getSourceLocFromPointer(funcLoc), declName,
      getSourceLocFromPointer(nameLoc), isAsync,
      getSourceLocFromPointer(asyncLoc), throws,
      getSourceLocFromPointer(throwsLoc), nullptr, paramList,
      (TypeRepr *)returnType, (DeclContext *)declContext);

  return {static_cast<DeclContext *>(out), static_cast<FuncDecl *>(out),
          static_cast<Decl *>(out)};
}

void FuncDecl_setBody(void *fn, void *body) {
  ((FuncDecl *)fn)->setBody((BraceStmt *)body, FuncDecl::BodyKind::Parsed);
}

void *SimpleIdentTypeRepr_create(void *ctx, void *loc, BridgedIdentifier id) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  return new (Context)
      SimpleIdentTypeRepr(DeclNameLoc(getSourceLocFromPointer(loc)),
                          DeclNameRef(Identifier::getFromOpaquePointer(id)));
}

void *GenericIdentTypeRepr_create(void *ctx, BridgedIdentifier name,
                                  void *nameLoc, BridgedArrayRef genericArgs,
                                  void *lAngle, void *rAngle) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  auto Loc = DeclNameLoc(getSourceLocFromPointer(nameLoc));
  auto Name = DeclNameRef(Identifier::getFromOpaquePointer(name));
  SourceLoc lAngleLoc = getSourceLocFromPointer(lAngle);
  SourceLoc rAngleLoc = getSourceLocFromPointer(rAngle);
  return GenericIdentTypeRepr::create(Context, Loc, Name,
                                      getArrayRef<TypeRepr *>(genericArgs),
                                      SourceRange{lAngleLoc, rAngleLoc});
}

void *UnresolvedDotExpr_create(void *ctx, void *base, void *dotLoc,
                               BridgedIdentifier name, void *nameLoc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  return new (Context)
      UnresolvedDotExpr((Expr *)base, getSourceLocFromPointer(dotLoc),
                        DeclNameRef(Identifier::getFromOpaquePointer(name)),
                        DeclNameLoc(getSourceLocFromPointer(nameLoc)), false);
}

void *ClosureExpr_create(void *ctx, void *body, void *dc) {
  DeclAttributes attributes;
  SourceRange bracketRange;
  SourceLoc asyncLoc;
  SourceLoc throwsLoc;
  SourceLoc arrowLoc;
  SourceLoc inLoc;

  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  auto params = ParameterList::create(Context, inLoc, {}, inLoc);

  auto *out = new (Context)
      ClosureExpr(attributes, bracketRange, nullptr, nullptr, asyncLoc,
                  throwsLoc, arrowLoc, inLoc, nullptr, 0, (DeclContext *)dc);
  out->setBody((BraceStmt *)body, true);
  out->setParameterList(params);
  return (Expr *)out;
}

void NominalTypeDecl_setMembers(void *decl, BridgedArrayRef members) {
  auto declMembers = getArrayRef<Decl *>(members);
  for (auto m : declMembers)
    ((NominalTypeDecl *)decl)->addMember(m);
}

DeclContextAndDecl StructDecl_create(void *ctx, void *loc,
                                     BridgedIdentifier name, void *nameLoc,
                                     void *_Nullable genericParams, void *dc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  auto *out = new (Context) StructDecl(
      getSourceLocFromPointer(loc), Identifier::getFromOpaquePointer(name),
      getSourceLocFromPointer(nameLoc), {}, (GenericParamList *)genericParams,
      (DeclContext *)dc);
  out->setImplicit(); // TODO: remove this.
  return {(DeclContext *)out, (NominalTypeDecl *)out, (Decl *)out};
}

DeclContextAndDecl ClassDecl_create(void *ctx, void *loc,
                                    BridgedIdentifier name, void *nameLoc,
                                    void *dc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  auto *out = new (Context) ClassDecl(
      getSourceLocFromPointer(loc), Identifier::getFromOpaquePointer(name),
      getSourceLocFromPointer(nameLoc), {}, nullptr, (DeclContext *)dc, false);
  out->setImplicit(); // TODO: remove this.
  return {(DeclContext *)out, (NominalTypeDecl *)out, (Decl *)out};
}

void *OptionalTypeRepr_create(void *ctx, void *base, void *questionLoc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  return new (Context)
      OptionalTypeRepr((TypeRepr *)base, getSourceLocFromPointer(questionLoc));
}

void *ImplicitlyUnwrappedOptionalTypeRepr_create(void *ctx, void *base,
                                                 void *exclamationLoc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  return new (Context) ImplicitlyUnwrappedOptionalTypeRepr(
      (TypeRepr *)base, getSourceLocFromPointer(exclamationLoc));
}

void *ArrayTypeRepr_create(void *ctx, void *base, void *lsquareLoc,
                           void *rsquareLoc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  SourceLoc lSquareLoc = getSourceLocFromPointer(lsquareLoc);
  SourceLoc rSquareLoc = getSourceLocFromPointer(rsquareLoc);
  return new (Context)
      ArrayTypeRepr((TypeRepr *)base, SourceRange{lSquareLoc, rSquareLoc});
}

void *DictionaryTypeRepr_create(void *ctx, void *keyType, void *valueType,
                                void *lsquareLoc, void *colonloc,
                                void *rsquareLoc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  SourceLoc lSquareLoc = getSourceLocFromPointer(lsquareLoc);
  SourceLoc colonLoc = getSourceLocFromPointer(colonloc);
  SourceLoc rSquareLoc = getSourceLocFromPointer(rsquareLoc);
  return new (Context)
      DictionaryTypeRepr((TypeRepr *)keyType, (TypeRepr *)valueType, colonLoc,
                         SourceRange{lSquareLoc, rSquareLoc});
}

void *MetatypeTypeRepr_create(void *ctx, void *baseType, void *typeLoc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  SourceLoc tyLoc = getSourceLocFromPointer(typeLoc);
  return new (Context) MetatypeTypeRepr((TypeRepr *)baseType, tyLoc);
}

void *ProtocolTypeRepr_create(void *ctx, void *baseType, void *protoLoc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  SourceLoc protocolLoc = getSourceLocFromPointer(protoLoc);
  return new (Context) ProtocolTypeRepr((TypeRepr *)baseType, protocolLoc);
}

void *PackExpansionTypeRepr_create(void *ctx, void *base, void *ellipsisLoc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  return new (Context) PackExpansionTypeRepr(
      (TypeRepr *)base, getSourceLocFromPointer(ellipsisLoc));
}

void *TupleTypeRepr_create(void *ctx, BridgedArrayRef elements, void *lParenLoc,
                           void *rParenLoc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  SourceLoc lParen = getSourceLocFromPointer(lParenLoc);
  SourceLoc rParen = getSourceLocFromPointer(rParenLoc);
  SmallVector<TupleTypeReprElement, 8> tupleElements;
  for (auto element : getArrayRef<BridgedTupleTypeElement>(elements)) {
    TupleTypeReprElement elementRepr;
    elementRepr.Name = Identifier::getFromOpaquePointer(element.Name);
    elementRepr.NameLoc = getSourceLocFromPointer(element.NameLoc);
    elementRepr.SecondName =
        Identifier::getFromOpaquePointer(element.SecondName);
    elementRepr.SecondNameLoc = getSourceLocFromPointer(element.SecondNameLoc);
    elementRepr.UnderscoreLoc = getSourceLocFromPointer(element.UnderscoreLoc);
    elementRepr.ColonLoc = getSourceLocFromPointer(element.ColonLoc);
    elementRepr.Type = (TypeRepr *)element.Type;
    elementRepr.TrailingCommaLoc =
        getSourceLocFromPointer(element.TrailingCommaLoc);
    tupleElements.emplace_back(elementRepr);
  }

  return TupleTypeRepr::create(Context, tupleElements,
                               SourceRange{lParen, rParen});
}

void *IdentTypeRepr_create(void *ctx, BridgedArrayRef components) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  return IdentTypeRepr::create(
      Context, getArrayRef<ComponentIdentTypeRepr *>(components));
}

void *CompositionTypeRepr_create(void *ctx, BridgedArrayRef types,
                                 void *firstTypeLoc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  SourceLoc firstType = getSourceLocFromPointer(firstTypeLoc);
  return CompositionTypeRepr::create(Context, getArrayRef<TypeRepr *>(types),
                                     firstType, SourceRange{});
}

void *FunctionTypeRepr_create(void *ctx, void *argsTy, void *_Nullable asyncLoc,
                              void *_Nullable throwsLoc, void *arrowLoc,
                              void *returnType) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  return new (Context) FunctionTypeRepr(
      nullptr, (TupleTypeRepr *)argsTy, getSourceLocFromPointer(asyncLoc),
      getSourceLocFromPointer(throwsLoc), getSourceLocFromPointer(arrowLoc),
      (TypeRepr *)returnType);
}

void *NamedOpaqueReturnTypeRepr_create(void *ctx, void *baseTy) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  return new (Context) NamedOpaqueReturnTypeRepr((TypeRepr *)baseTy, nullptr);
}

void *OpaqueReturnTypeRepr_create(void *ctx, void *opaqueLoc, void *baseTy) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  return new (Context) OpaqueReturnTypeRepr(getSourceLocFromPointer(opaqueLoc),
                                            (TypeRepr *)baseTy);
}
void *ExistentialTypeRepr_create(void *ctx, void *anyLoc, void *baseTy) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  return new (Context)
      ExistentialTypeRepr(getSourceLocFromPointer(anyLoc), (TypeRepr *)baseTy);
}

void *GenericParamList_create(void *ctx, void *lAngleLoc,
                              BridgedArrayRef params, void *_Nullable whereLoc,
                              BridgedArrayRef reqs, void *rAngleLoc) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  SmallVector<RequirementRepr> requirements;
  for (auto req : getArrayRef<BridgedRequirementRepr>(reqs)) {
    switch (req.Kind) {
    case BridgedRequirementReprKindTypeConstraint:
      requirements.push_back(RequirementRepr::getTypeConstraint(
          (TypeRepr *)req.FirstType, getSourceLocFromPointer(req.SeparatorLoc),
          (TypeRepr *)req.SecondType));
      break;
    case BridgedRequirementReprKindSameType:
      requirements.push_back(RequirementRepr::getSameType(
          (TypeRepr *)req.FirstType, getSourceLocFromPointer(req.SeparatorLoc),
          (TypeRepr *)req.SecondType));
      break;
    case BridgedRequirementReprKindLayoutConstraint:
      llvm_unreachable("cannot handle layout constraints!");
    }
  }
  return GenericParamList::create(Context, getSourceLocFromPointer(lAngleLoc),
                                  getArrayRef<GenericTypeParamDecl *>(params),
                                  getSourceLocFromPointer(whereLoc),
                                  requirements,
                                  getSourceLocFromPointer(rAngleLoc));
}

void *GenericTypeParamDecl_create(void *ctx, void *declContext,
                                  BridgedIdentifier name, void *nameLoc,
                                  void *_Nullable ellipsisLoc, long index,
                                  bool isParameterPack) {
  return GenericTypeParamDecl::createParsed(
      static_cast<DeclContext *>(declContext),
      Identifier::getFromOpaquePointer(name), getSourceLocFromPointer(nameLoc),
      getSourceLocFromPointer(ellipsisLoc),
      /*index*/ index, isParameterPack);
}

void GenericTypeParamDecl_setInheritedType(void *ctx, void *Param, void *ty) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  auto entries = Context.AllocateCopy(
      ArrayRef<InheritedEntry>{InheritedEntry{(TypeRepr *)ty}});
  ((GenericTypeParamDecl *)Param)->setInherited(entries);
}

DeclContextAndDecl TypeAliasDecl_create(void *ctx, void *declContext,
                                        void *aliasLoc, void *equalLoc,
                                        BridgedIdentifier name, void *nameLoc,
                                        void *_Nullable genericParams) {
  ASTContext &Context = *static_cast<ASTContext *>(ctx);
  auto *out = new (Context) TypeAliasDecl(
      getSourceLocFromPointer(aliasLoc), getSourceLocFromPointer(equalLoc),
      Identifier::getFromOpaquePointer(name), getSourceLocFromPointer(nameLoc),
      (GenericParamList *)genericParams, (DeclContext *)declContext);
  return {(DeclContext *)out, (TypeAliasDecl *)out, (Decl *)out};
}

void TypeAliasDecl_setUnderlyingTypeRepr(void *decl, void *underlyingType) {
  ((TypeAliasDecl *)decl)->setUnderlyingTypeRepr((TypeRepr *)underlyingType);
}

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"

void TopLevelCodeDecl_dump(void *decl) {
  ((TopLevelCodeDecl *)decl)->dump(llvm::errs());
}

void Expr_dump(void *expr) { ((Expr *)expr)->dump(llvm::errs()); }
void Decl_dump(void *expr) { ((Decl *)expr)->dump(llvm::errs()); }
void Stmt_dump(void *expr) { ((Stmt *)expr)->dump(llvm::errs()); }
void Type_dump(void *expr) { ((TypeRepr *)expr)->dump(); }

#pragma clang diagnostic pop
