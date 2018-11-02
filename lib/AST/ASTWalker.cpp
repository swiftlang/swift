//===--- ASTWalker.cpp - AST Traversal ------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements a recursive traversal of every node in an AST.
//
//  It's important to update this traversal whenever the AST is
//  changed, whether by adding a new node class or adding a new child
//  to an existing node.  Many walker implementations rely on being
//  invoked with every node in the AST.
//
//  Please follow these general rules when implementing traversal for
//  a node:
//
//    - Every node should be walked.  If a node has both syntactic and
//      semantic components, you should make sure you visit every node
//      in both.
//
//    - Nodes should only be walked once.  So if a node has both
//      syntactic and semantic components, but the type-checker builds
//      the semantic components directly on top of the syntactic
//      components, walking the semantic components will be sufficient
//      to visit all the nodes in both.
//
//    - Explicitly-written nodes should be walked in left-to-right
//      syntactic order.  The ordering of implicit nodes isn't
//      particularly important.
//
//      Note that semantic components will generally preserve the
//      syntactic order of their children because doing something else
//      could illegally change order of evaluation.  This is why, for
//      example, shuffling a TupleExpr creates a TupleShuffleExpr
//      instead of just making a new TupleExpr with the elements in
//      different order.
//
//    - Sub-expressions and sub-statements should be replaceable.
//      It's reasonable to expect that the replacement won't be
//      completely unrelated to the original, but try to avoid making
//      assumptions about the exact representation type.  For example,
//      assuming that a child expression is literally a TupleExpr may
//      only be a reasonable assumption in an unchecked parse tree.
//
//    - Avoid relying on the AST being type-checked or even
//      well-formed during traversal.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTWalker.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PrettyStackTrace.h"
using namespace swift;

void ASTWalker::anchor() {}

namespace {

/// Traversal - This class implements a simple expression/statement
/// recursive traverser which queries a user-provided walker class
/// on every node in an AST.
class Traversal : public ASTVisitor<Traversal, Expr*, Stmt*,
                                    /*Decl*/ bool,
                                    Pattern *, /*TypeRepr*/ bool>
{
  friend class ASTVisitor<Traversal, Expr*, Stmt*, bool, Pattern*, bool>;
  typedef ASTVisitor<Traversal, Expr*, Stmt*, bool, Pattern*, bool> inherited;

  ASTWalker &Walker;
  
  /// \brief RAII object that sets the parent of the walk context 
  /// appropriately.
  class SetParentRAII {
    ASTWalker &Walker;
    decltype(ASTWalker::Parent) PriorParent;
    
  public:
    template<typename T>
    SetParentRAII(ASTWalker &walker, T *newParent)
      : Walker(walker), PriorParent(walker.Parent) {
      walker.Parent = newParent;
    }
    
    ~SetParentRAII() {
      Walker.Parent = PriorParent;
    }
  };

  Expr *visit(Expr *E) {
    SetParentRAII SetParent(Walker, E);
    return inherited::visit(E);
  }

  Stmt *visit(Stmt *S) {
    SetParentRAII SetParent(Walker, S);
    return inherited::visit(S);
  }
  
  Pattern *visit(Pattern *P) {
    SetParentRAII SetParent(Walker, P);
    return inherited::visit(P);
  }

  bool visit(Decl *D) {
    SetParentRAII SetParent(Walker, D);
    return inherited::visit(D);
  }
  
  bool visit(TypeRepr *T) {
    SetParentRAII SetParent(Walker, T);
    return inherited::visit(T);
  }
  
  bool visit(ParameterList *PL) {
    return inherited::visit(PL);
  }
  
  //===--------------------------------------------------------------------===//
  //                                 Decls
  //===--------------------------------------------------------------------===//

  bool visitImportDecl(ImportDecl *ID) {
    return false;
  }

  bool visitExtensionDecl(ExtensionDecl *ED) {
    if (doIt(ED->getExtendedTypeLoc()))
      return true;
    for (auto &Inherit : ED->getInherited()) {
      if (doIt(Inherit))
        return true;
    }
    if (auto *Where = ED->getTrailingWhereClause()) {
      for(auto &Req: Where->getRequirements()) {
        if (doIt(Req))
          return true;
      }
    }
    for (Decl *M : ED->getMembers()) {
      if (doIt(M))
        return true;
    }
    return false;
  }

  bool visitPatternBindingDecl(PatternBindingDecl *PBD) {
    unsigned idx = 0U-1;
    for (auto entry : PBD->getPatternList()) {
      ++idx;
      if (Pattern *Pat = doIt(entry.getPattern()))
        PBD->setPattern(idx, Pat, entry.getInitContext());
      else
        return true;
      if (entry.getInit() &&
          (!entry.isInitializerLazy() ||
           Walker.shouldWalkIntoLazyInitializers())) {
#ifndef NDEBUG
        PrettyStackTraceDecl debugStack("walking into initializer for", PBD);
#endif
        if (Expr *E2 = doIt(entry.getInit()))
          PBD->setInit(idx, E2);
        else
          return true;
      }
    }
    return false;
  }

  bool visitEnumCaseDecl(EnumCaseDecl *ECD) {
    // We'll visit the EnumElementDecls separately.
    return false;
  }

  bool visitTopLevelCodeDecl(TopLevelCodeDecl *TLCD) {
    if (BraceStmt *S = cast_or_null<BraceStmt>(doIt(TLCD->getBody())))
      TLCD->setBody(S);
    return false;
  }

  bool visitIfConfigDecl(IfConfigDecl *ICD) {
    // By default, just visit the elements that are actually
    // injected into the enclosing context.
    return false;
  }

  bool visitPoundDiagnosticDecl(PoundDiagnosticDecl *PDD) {
    // By default, ignore #error/#warning.
    return false;
  }

  bool visitOperatorDecl(OperatorDecl *OD) {
    return false;
  }

  bool visitPrecedenceGroupDecl(PrecedenceGroupDecl *PGD) {
    return false;
  }

  bool visitTypeAliasDecl(TypeAliasDecl *TAD) {
    if (TAD->getGenericParams() &&
        Walker.shouldWalkIntoGenericParams()) {

      if (visitGenericParamList(TAD->getGenericParams()))
        return true;
    }

    return doIt(TAD->getUnderlyingTypeLoc());
  }

  bool visitAbstractTypeParamDecl(AbstractTypeParamDecl *TPD) {
    for (auto Inherit: TPD->getInherited()) {
      if (doIt(Inherit))
        return true;
    }
    
    if (auto *ATD = dyn_cast<AssociatedTypeDecl>(TPD)) {
      if (auto *WhereClause = ATD->getTrailingWhereClause()) {
        for (auto &Req: WhereClause->getRequirements()) {
          if (doIt(Req))
            return true;
        }
      }
    }
    return false;
  }

  bool visitNominalTypeDecl(NominalTypeDecl *NTD) {
    bool WalkGenerics = NTD->getGenericParams() &&
        Walker.shouldWalkIntoGenericParams();

    if (WalkGenerics) {
      visitGenericParamList(NTD->getGenericParams());
    }

    for (auto &Inherit : NTD->getInherited()) {
      if (doIt(Inherit))
        return true;
    }

    // Visit requirements
    if (auto *Protocol = dyn_cast<ProtocolDecl>(NTD)) {
      if (auto *WhereClause = Protocol->getTrailingWhereClause()) {
        for (auto &Req: WhereClause->getRequirements()) {
          if (doIt(Req))
            return true;
        }
      }
    }
    if (WalkGenerics) {
      for (auto Req: NTD->getGenericParams()->getTrailingRequirements()) {
        if (doIt(Req))
          return true;
      }
    }
    
    for (Decl *Member : NTD->getMembers())
      if (doIt(Member))
        return true;
    return false;
  }

  bool visitModuleDecl(ModuleDecl *MD) {
    // TODO: should we recurse within the module?
    return false;
  }

  bool visitVarDecl(VarDecl *VD) {
    return false;
  }

  bool visitSubscriptDecl(SubscriptDecl *SD) {
    bool WalkGenerics = SD->getGenericParams() &&
      Walker.shouldWalkIntoGenericParams();
    if (WalkGenerics) {
      visitGenericParamList(SD->getGenericParams());
    }
    visit(SD->getIndices());
    if (doIt(SD->getElementTypeLoc()))
      return true;

    if (WalkGenerics) {
      // Visit generic requirements
      for (auto Req : SD->getGenericParams()->getTrailingRequirements()) {
        if (doIt(Req))
          return true;
      }
    }
    return false;
  }

  bool visitMissingMemberDecl(MissingMemberDecl *MMD) {
    return false;
  }

  bool visitAbstractFunctionDecl(AbstractFunctionDecl *AFD) {
#ifndef NDEBUG
    PrettyStackTraceDecl debugStack("walking into body of", AFD);
#endif

    bool WalkGenerics = AFD->getGenericParams() &&
        Walker.shouldWalkIntoGenericParams() &&
        // accessor generics are visited from the storage decl
        !isa<AccessorDecl>(AFD);

    if (WalkGenerics) {
      visitGenericParamList(AFD->getGenericParams());
    }

    if (auto *PD = AFD->getImplicitSelfDecl())
      visit(PD);
    visit(AFD->getParameters());

    if (auto *FD = dyn_cast<FuncDecl>(AFD))
      if (!isa<AccessorDecl>(FD))
        if (doIt(FD->getBodyResultTypeLoc()))
          return true;

    if (WalkGenerics) {
      // Visit trailing requirments
      for (auto Req : AFD->getGenericParams()->getTrailingRequirements()) {
        if (doIt(Req))
          return true;
      }
    }

    if (AFD->getBody(/*canSynthesize=*/false)) {
      AbstractFunctionDecl::BodyKind PreservedKind = AFD->getBodyKind();
      if (BraceStmt *S = cast_or_null<BraceStmt>(doIt(AFD->getBody())))
        AFD->setBody(S, PreservedKind);
      else
        return true;
    }

    if (auto ctor = dyn_cast<ConstructorDecl>(AFD)) {
      if (auto superInit = ctor->getSuperInitCall()) {
        if ((superInit = doIt(superInit)))
          ctor->setSuperInitCall(superInit);
        else
          return true;
      }
    }

    return false;
  }

  bool visitEnumElementDecl(EnumElementDecl *ED) {
    if (auto *PL = ED->getParameterList()) {
      visit(PL);
    }

    // The getRawValueExpr should remain the untouched original LiteralExpr for
    // serialization and validation purposes. We only traverse the type-checked
    // form, unless we haven't populated it yet.
    if (auto *rawValueExpr = ED->getTypeCheckedRawValueExpr()) {
      if (auto newRawValueExpr = doIt(rawValueExpr))
        ED->setTypeCheckedRawValueExpr(newRawValueExpr);
      else
        return true;
    } else if (auto *rawLiteralExpr = ED->getRawValueExpr()) {
      Expr *newRawExpr = doIt(rawLiteralExpr);
      if (auto newRawLiteralExpr = dyn_cast<LiteralExpr>(newRawExpr))
        ED->setRawValueExpr(newRawLiteralExpr);
      else
        return true;
    }
    return false;
  }

  bool visitGenericParamList(GenericParamList *GPL) {
    // Visit generic params
    for (auto &P : GPL->getParams()) {
      if (doIt(P))
        return true;
    }

    // Visit param conformance
    for (auto Req : GPL->getNonTrailingRequirements()) {
      if (doIt(Req))
        return true;
    }

    return false;
  }

  //===--------------------------------------------------------------------===//
  //                                  Exprs
  //===--------------------------------------------------------------------===//

  // A macro for handling the "semantic expressions" that are common
  // on sugared expression nodes like string interpolation.  The
  // semantic expression is set up by type-checking to include all the
  // other children as sub-expressions, so if it exists, we should
  // just bypass the rest of the visitation.
#define HANDLE_SEMANTIC_EXPR(NODE)                         \
  do {                                                     \
    if (Expr *_semanticExpr = NODE->getSemanticExpr()) {   \
      if ((_semanticExpr = doIt(_semanticExpr))) {         \
        NODE->setSemanticExpr(_semanticExpr);              \
      } else {                                             \
        return nullptr;                                    \
      }                                                    \
      return NODE;                                         \
    }                                                      \
  } while (false)

  Expr *visitErrorExpr(ErrorExpr *E) { return E; }
  Expr *visitCodeCompletionExpr(CodeCompletionExpr *E) { return E; }
  Expr *visitLiteralExpr(LiteralExpr *E) { return E; }
  Expr *visitDiscardAssignmentExpr(DiscardAssignmentExpr *E) { return E; }
  Expr *visitTypeExpr(TypeExpr *E) {
    if (!E->isImplicit())
      if (doIt(E->getTypeLoc()))
        return nullptr;

    return E;
  }
  Expr *visitSuperRefExpr(SuperRefExpr *E) { return E; }
  Expr *visitOtherConstructorDeclRefExpr(OtherConstructorDeclRefExpr *E) {
    return E;
  }
  
  Expr *visitOverloadedDeclRefExpr(OverloadedDeclRefExpr *E) { return E; }
  Expr *visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *E) { return E; }

  Expr *visitUnresolvedMemberExpr(UnresolvedMemberExpr *E) { 
    if (E->getArgument()) {
      if (auto arg = doIt(E->getArgument())) {
        E->setArgument(arg);
        return E;
      }

      return nullptr;
    }
    return E; 
  }

  Expr *visitOpaqueValueExpr(OpaqueValueExpr *E) { return E; }

  Expr *visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *E) {
    HANDLE_SEMANTIC_EXPR(E);

    for (auto &Segment : E->getSegments()) {
      if (Expr *Seg = doIt(Segment))
        Segment = Seg;
      else
        return nullptr;
    }
    return E;
  }

  Expr *visitObjectLiteralExpr(ObjectLiteralExpr *E) {
    HANDLE_SEMANTIC_EXPR(E);

    if (Expr *arg = E->getArg()) {
      if (Expr *arg2 = doIt(arg)) {
        E->setArg(arg2);
      } else {
        return nullptr;
      }
    }
    return E;
  }

  Expr *visitCollectionExpr(CollectionExpr *E) {
    HANDLE_SEMANTIC_EXPR(E);

    for (auto &elt : E->getElements())
      if (Expr *Sub = doIt(elt))
        elt = Sub;
      else
        return nullptr;
    return E;
  }

  Expr *visitDeclRefExpr(DeclRefExpr *E) {
    return E;
  }
  
  Expr *visitMemberRefExpr(MemberRefExpr *E) {
    if (Expr *Base = doIt(E->getBase())) {
      E->setBase(Base);
      return E;
    }
    return nullptr;
  }

  Expr *visitDynamicMemberRefExpr(DynamicMemberRefExpr *E) {
    if (Expr *Base = doIt(E->getBase())) {
      E->setBase(Base);
      return E;
    }

    return nullptr;
  }

  Expr *visitAnyTryExpr(AnyTryExpr *E) {
    if (Expr *subExpr = doIt(E->getSubExpr())) {
      E->setSubExpr(subExpr);
      return E;
    }
    return nullptr;
  }

  Expr *visitIdentityExpr(IdentityExpr *E) {
    if (Expr *subExpr = doIt(E->getSubExpr())) {
      E->setSubExpr(subExpr);
      return E;
    }
    return nullptr;
  }
  Expr *visitTupleExpr(TupleExpr *E) {
    for (unsigned i = 0, e = E->getNumElements(); i != e; ++i)
      if (E->getElement(i)) {
        if (Expr *Elt = doIt(E->getElement(i)))
          E->setElement(i, Elt);
        else
          return nullptr;
      }
    return E;
  }
  Expr *visitSubscriptExpr(SubscriptExpr *E) {
    if (Expr *Base = doIt(E->getBase()))
      E->setBase(Base);
    else
      return nullptr;
    
    if (Expr *Index = doIt(E->getIndex()))
      E->setIndex(Index);
    else
      return nullptr;
    
    return E;
  }
  Expr *visitKeyPathApplicationExpr(KeyPathApplicationExpr *E) {
    if (Expr *Base = doIt(E->getBase()))
      E->setBase(Base);
    else
      return nullptr;
    
    if (Expr *KeyPath = doIt(E->getKeyPath()))
      E->setKeyPath(KeyPath);
    else
      return nullptr;
    
    return E;
  }
  Expr *visitDynamicSubscriptExpr(DynamicSubscriptExpr *E) {
    if (Expr *Base = doIt(E->getBase()))
      E->setBase(Base);
    else
      return nullptr;
    
    if (Expr *Index = doIt(E->getIndex()))
      E->setIndex(Index);
    else
      return nullptr;
    
    return E;
  }
  Expr *visitUnresolvedDotExpr(UnresolvedDotExpr *E) {
    if (!E->getBase())
      return E;
    
    if (Expr *E2 = doIt(E->getBase())) {
      E->setBase(E2);
      return E;
    }
    return nullptr;
  }
  Expr *visitUnresolvedSpecializeExpr(UnresolvedSpecializeExpr *E) {
    if (!E->getSubExpr())
      return E;
    
    if (Expr *Sub = doIt(E->getSubExpr()))
      E->setSubExpr(Sub);
    else
      return nullptr;

    for (auto &TyLoc : E->getUnresolvedParams()) {
      if (doIt(TyLoc))
        return nullptr;
    }

    return E;
  }
  
  Expr *visitTupleElementExpr(TupleElementExpr *E) {
    if (Expr *E2 = doIt(E->getBase())) {
      E->setBase(E2);
      return E;
    }
    return nullptr;
  }

  Expr *visitImplicitConversionExpr(ImplicitConversionExpr *E) {
    if (Expr *E2 = doIt(E->getSubExpr())) {
      E->setSubExpr(E2);
      return E;
    }
    return nullptr;
  }
  
  Expr *visitCollectionUpcastConversionExpr(CollectionUpcastConversionExpr *E) {
    if (Expr *E2 = doIt(E->getSubExpr())) {
      E->setSubExpr(E2);
    } else {
      return nullptr;
    }

    if (auto &keyConv = E->getKeyConversion()) {
      auto kConv = keyConv.Conversion;
      if (!kConv) {
        return nullptr;
      } else if (Expr *E2 = doIt(kConv)) {
        E->setKeyConversion({keyConv.OrigValue, E2});
      } else {
        return nullptr;
      }
    }

    if (auto &valueConv = E->getValueConversion()) {
      auto vConv = valueConv.Conversion;
      if (!vConv) {
        return nullptr;
      } else if (Expr *E2 = doIt(vConv)) {
        E->setValueConversion({valueConv.OrigValue, E2});
      } else {
        return nullptr;
      }
    }

    return E;
  }
  
  Expr *visitTupleShuffleExpr(TupleShuffleExpr *E) {
    if (Expr *E2 = doIt(E->getSubExpr())) {
      E->setSubExpr(E2);
    } else {
      return nullptr;
    }

    for (auto &defaultArg : E->getCallerDefaultArgs()) {
      if (Expr *newDefaultArg = doIt(defaultArg))
        defaultArg = newDefaultArg;
      else
        return nullptr;
    }

    return E;
  }

  Expr *visitTryExpr(TryExpr *E) {
    if (Expr *E2 = doIt(E->getSubExpr())) {
      E->setSubExpr(E2);
      return E;
    }
    return nullptr;
  }

  Expr *visitForceTryExpr(ForceTryExpr *E) {
    if (Expr *E2 = doIt(E->getSubExpr())) {
      E->setSubExpr(E2);
      return E;
    }
    return nullptr;
  }

  Expr *visitOptionalTryExpr(OptionalTryExpr *E) {
    if (Expr *E2 = doIt(E->getSubExpr())) {
      E->setSubExpr(E2);
      return E;
    }
    return nullptr;
  }

  Expr *visitInOutExpr(InOutExpr *E) {
    if (Expr *E2 = doIt(E->getSubExpr())) {
      E->setSubExpr(E2);
      return E;
    }
    return nullptr;
  }
  
  Expr *visitSequenceExpr(SequenceExpr *E) {
    for (unsigned i = 0, e = E->getNumElements(); i != e; ++i)
      if (Expr *Elt = doIt(E->getElement(i)))
        E->setElement(i, Elt);
      else
        return nullptr;
    return E;
  }

  Expr *visitDynamicTypeExpr(DynamicTypeExpr *E) {
    Expr *base = E->getBase();
    if ((base = doIt(base)))
      E->setBase(base);
    else
      return nullptr;

    return E;
  }

  Expr *visitCaptureListExpr(CaptureListExpr *expr) {
    for (auto c : expr->getCaptureList()) {
      if (doIt(c.Var) || doIt(c.Init))
        return nullptr;
    }

    ClosureExpr *body = expr->getClosureBody();
    if ((body = cast_or_null<ClosureExpr>(doIt(body))))
      expr->setClosureBody(body);
    else
      return nullptr;
    return expr;
  }

  Expr *visitClosureExpr(ClosureExpr *expr) {
    visit(expr->getParameters());

    if (expr->hasExplicitResultType())
      if (doIt(expr->getExplicitResultTypeLoc()))
        return nullptr;

    // Handle single-expression closures.
    if (expr->hasSingleExpressionBody()) {
      if (Expr *body = doIt(expr->getSingleExpressionBody())) {
        expr->setSingleExpressionBody(body);
        return expr;
      }
      return nullptr;
    }

    // Handle other closures.
    if (BraceStmt *body = cast_or_null<BraceStmt>(doIt(expr->getBody()))) {
      expr->setBody(body, false);
      return expr;
    }
    return nullptr;
  }

  Expr *visitAutoClosureExpr(AutoClosureExpr *E) {
    if (Expr *E2 = doIt(E->getSingleExpressionBody())) {
      E->setBody(E2);
      return E;
    }
    return nullptr;
  }
  
  Expr *visitApplyExpr(ApplyExpr *E) {
    if (E->getFn()) {
      Expr *E2 = doIt(E->getFn());
      if (E2 == nullptr) return nullptr;
      E->setFn(E2);
    }

    if (E->getArg()) {
      Expr *E2 = doIt(E->getArg());
      if (E2 == nullptr) return nullptr;
      
      // Protect against setting a non-tuple argument expression for a binop,
      // which may occur as a result of error recovery.
      // E.g., "print(Array<Int)"
      if (!isa<BinaryExpr>(E) || isa<TupleExpr>(E2))
        E->setArg(E2);
    }

    return E;
  }

  Expr *visitSelfApplyExpr(SelfApplyExpr *E) {
    if (E->getBase()) {
      Expr *E2 = doIt(E->getBase());
      if (E2 == nullptr) return nullptr;
      E->setBase(E2);
    }

    if (E->getFn()) {
      Expr *E2 = doIt(E->getFn());
      if (E2 == nullptr) return nullptr;
      E->setFn(E2);
    }

    return E;
  }

  Expr *visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E) {
    Expr *E2 = doIt(E->getLHS());
    if (E2 == nullptr) return nullptr;
    E->setLHS(E2);
    
    E2 = doIt(E->getRHS());
    if (E2 == nullptr) return nullptr;
    E->setRHS(E2);
    return E;      
  }

  Expr *visitExplicitCastExpr(ExplicitCastExpr *E) {
    if (Expr *Sub = E->getSubExpr()) {
      Sub = doIt(Sub);
      if (!Sub) return nullptr;
      E->setSubExpr(Sub);
    }

    if (doIt(E->getCastTypeLoc()))
      return nullptr;

    return E;
  }

  Expr *visitArrowExpr(ArrowExpr *E) {
    if (Expr *Args = E->getArgsExpr()) {
      Args = doIt(Args);
      if (!Args) return nullptr;
      E->setArgsExpr(Args);
    }
    if (Expr *Result = E->getResultExpr()) {
      Result = doIt(Result);
      if (!Result) return nullptr;
      E->setResultExpr(Result);
    }
    return E;
  }

  Expr *visitRebindSelfInConstructorExpr(RebindSelfInConstructorExpr *E) {
    Expr *Sub = doIt(E->getSubExpr());
    if (!Sub) return nullptr;
    E->setSubExpr(Sub);
    
    return E;
  }
  
  Expr *visitAssignExpr(AssignExpr *AE) {
    if (Expr *Dest = AE->getDest()) {
      if (!(Dest = doIt(Dest)))
        return nullptr;
      AE->setDest(Dest);
    }

    if (Expr *Src = AE->getSrc()) {
      if (!(Src = doIt(AE->getSrc())))
        return nullptr;
      AE->setSrc(Src);
    }
    
    return AE;
  }
  
  Expr *visitEnumIsCaseExpr(EnumIsCaseExpr *E) {
    if (Expr *Sub = E->getSubExpr()) {
      if (!(Sub = doIt(Sub)))
        return nullptr;
      E->setSubExpr(Sub);
    }
    
    return E;
  }
  
  
  Expr *visitIfExpr(IfExpr *E) {
    if (Expr *Cond = E->getCondExpr()) {
      Cond = doIt(Cond);
      if (!Cond) return nullptr;
      E->setCondExpr(Cond);
    }
    
    Expr *Then = doIt(E->getThenExpr());
    if (!Then) return nullptr;
    E->setThenExpr(Then);
    
    if (Expr *Else = E->getElseExpr()) {
      Else = doIt(Else);
      if (!Else) return nullptr;
      E->setElseExpr(Else);
    }
    
    return E;
  }

  Expr *visitUnresolvedPatternExpr(UnresolvedPatternExpr *E) {
    Pattern *sub = doIt(E->getSubPattern());
    if (!sub) return nullptr;
    
    E->setSubPattern(sub);
    return E;
  }

  Expr *visitBindOptionalExpr(BindOptionalExpr *E) {
    Expr *sub = doIt(E->getSubExpr());
    if (!sub) return nullptr;

    E->setSubExpr(sub);
    return E;
  }

  Expr *visitOptionalEvaluationExpr(OptionalEvaluationExpr *E) {
    Expr *sub = doIt(E->getSubExpr());
    if (!sub) return nullptr;

    E->setSubExpr(sub);
    return E;
  }

  Expr *visitForceValueExpr(ForceValueExpr *E) {
    Expr *sub = doIt(E->getSubExpr());
    if (!sub) return nullptr;

    E->setSubExpr(sub);
    return E;
  }

  Expr *visitOpenExistentialExpr(OpenExistentialExpr *E) {
    Expr *existential = doIt(E->getExistentialValue());
    if (!existential) return nullptr;

    Expr *sub = doIt(E->getSubExpr());
    if (!sub) return nullptr;

    E->setExistentialValue(existential);
    E->setSubExpr(sub);
    return E;
  }

  Expr *visitMakeTemporarilyEscapableExpr(MakeTemporarilyEscapableExpr *E) {
    Expr *closure = doIt(E->getNonescapingClosureValue());
    if (!closure) return nullptr;

    Expr *sub = doIt(E->getSubExpr());
    if (!sub) return nullptr;

    E->setNonescapingClosureValue(closure);
    E->setSubExpr(sub);
    return E;
  }
  
  Expr *visitEditorPlaceholderExpr(EditorPlaceholderExpr *E) {
    HANDLE_SEMANTIC_EXPR(E);
    return E;
  }

  Expr *visitLazyInitializerExpr(LazyInitializerExpr *E) {
    // Initializer is totally opaque for most visitation purposes.
    return E;
  }

  Expr *visitObjCSelectorExpr(ObjCSelectorExpr *E) {
    Expr *sub = doIt(E->getSubExpr());
    if (!sub) return nullptr;

    E->setSubExpr(sub);
    return E;
  }

  Expr *visitKeyPathExpr(KeyPathExpr *E) {
    // For an ObjC key path, the string literal expr serves as the semantic
    // expression.
    if (auto objcStringLiteral = E->getObjCStringLiteralExpr()) {
      Expr *sub = doIt(objcStringLiteral);
      if (!sub) return nullptr;
      E->setObjCStringLiteralExpr(sub);
      return E;
    }

    auto components = E->getComponents();
    if (components.empty()) {
      // No components means a parsed-only/pre-resolution Swift key path.
      assert(!E->isObjC());
      if (auto parsedRoot = E->getParsedRoot()) {
        Expr *newRoot = doIt(parsedRoot);
        if (!newRoot)
          return nullptr;
        E->setParsedRoot(newRoot);
      }
      if (auto parsedPath = E->getParsedPath()) {
        Expr *newPath = doIt(parsedPath);
        if (!newPath)
          return nullptr;
        E->setParsedPath(newPath);
      }
      return E;
    }

    if (!E->isObjC()) {
      auto rootType = E->getRootType();
      if (rootType && doIt(rootType))
        return nullptr;
    }

    SmallVector<KeyPathExpr::Component, 4> updatedComponents;
    bool didChangeComponents = false;
    for (auto &origComponent : components) {
      auto component = origComponent;
      switch (auto kind = component.getKind()) {
      case KeyPathExpr::Component::Kind::Subscript:
      case KeyPathExpr::Component::Kind::UnresolvedSubscript: {
        Expr *origIndex = component.getIndexExpr();
        Expr *newIndex = doIt(origIndex);
        if (!newIndex) return nullptr;
        if (newIndex != origIndex) {
          didChangeComponents = true;
          component = kind == KeyPathExpr::Component::Kind::Subscript
            ? KeyPathExpr::Component::forSubscriptWithPrebuiltIndexExpr(
                component.getDeclRef(),
                newIndex,
                component.getSubscriptLabels(),
                component.getComponentType(),
                component.getLoc(),
                component.getSubscriptIndexHashableConformances())
            : KeyPathExpr::Component
                         ::forUnresolvedSubscriptWithPrebuiltIndexExpr(
                E->getType()->getASTContext(),
                newIndex, component.getSubscriptLabels(), component.getLoc());
        }
        break;
      }
        
      case KeyPathExpr::Component::Kind::OptionalChain:
      case KeyPathExpr::Component::Kind::OptionalWrap:
      case KeyPathExpr::Component::Kind::OptionalForce:
      case KeyPathExpr::Component::Kind::Property:
      case KeyPathExpr::Component::Kind::UnresolvedProperty:
      case KeyPathExpr::Component::Kind::Invalid:
        // No subexpr to visit.
        break;
      }
      updatedComponents.push_back(component);
    }
    
    if (didChangeComponents) {
      E->resolveComponents(E->getType()->getASTContext(),
                           updatedComponents);
    }

    return E;
  }

  Expr *visitKeyPathDotExpr(KeyPathDotExpr *E) { return E; }

  //===--------------------------------------------------------------------===//
  //                           Everything Else
  //===--------------------------------------------------------------------===//

#define STMT(Id, Parent) Stmt *visit##Id##Stmt(Id##Stmt *S);
#include "swift/AST/StmtNodes.def"

#define PATTERN(Id, Parent) Pattern *visit##Id##Pattern(Id##Pattern *P);
#include "swift/AST/PatternNodes.def"

#define TYPEREPR(Id, Parent) bool visit##Id##TypeRepr(Id##TypeRepr *T);
#include "swift/AST/TypeReprNodes.def"
  
  bool visitParameterList(ParameterList *PL) {
    if (!Walker.walkToParameterListPre(PL))
      return false;
    
    for (auto P : *PL) {
      // Walk each parameter's decl and typeloc and default value.
      if (doIt(P))
        return true;
      
      // Don't walk into the type if the decl is implicit, or if the type is
      // implicit.
      if (!P->isImplicit() && !P->isTypeLocImplicit() &&
          doIt(P->getTypeLoc()))
        return true;
      
      if (auto *E = P->getDefaultValue()) {
        auto res = doIt(E);
        if (!res) return true;
        P->setDefaultValue(res);
      }
    }
    
    return Walker.walkToParameterListPost(PL);
  }
  
public:
  Traversal(ASTWalker &walker) : Walker(walker) {}

  Expr *doIt(Expr *E) {
    // Do the pre-order visitation.  If it returns false, we just
    // skip entering subnodes of this tree.
    auto Pre = Walker.walkToExprPre(E);
    if (!Pre.first || !Pre.second)
      return Pre.second;

    // Otherwise, visit the children.
    E = visit(Pre.second);

    // If we didn't bail out, do post-order visitation.
    if (E) E = Walker.walkToExprPost(E);

    return E;
  }

  Stmt *doIt(Stmt *S) {
    // Do the pre-order visitation.  If it returns false, we just
    // skip entering subnodes of this tree.
    auto Pre = Walker.walkToStmtPre(S);
    if (!Pre.first || !Pre.second)
      return Pre.second;

    // Otherwise, visit the children.
    S = visit(S);

    // If we didn't bail out, do post-order visitation.
    if (S) S = Walker.walkToStmtPost(S);

    return S;
  }
  
  bool shouldSkip(Decl *D) {
    if (isa<VarDecl>(D)) {
      // VarDecls are walked via their NamedPattern, ignore them if we encounter
      // then in the few cases where they are also pushed outside as members.
      // In all those cases we can walk them via the pattern binding decl.
      // This is used for when vising VarDecls from source, when visiting a
      // module file we walk them as we encounter them.
      if (Walker.Parent.getAsModule() &&
          D->getDeclContext()->getParentSourceFile())
        return true;
      if (Decl *ParentD = Walker.Parent.getAsDecl())
        return (isa<NominalTypeDecl>(ParentD) || isa<ExtensionDecl>(ParentD));
      auto walkerParentAsStmt = Walker.Parent.getAsStmt();
      if (walkerParentAsStmt && isa<BraceStmt>(walkerParentAsStmt))
        return true;
    }
    return false;
  }

  /// Returns true on failure.
  bool doIt(Decl *D) {
    if (shouldSkip(D))
      return false;

    // Do the pre-order visitation.  If it returns false, we just
    // skip entering subnodes of this tree.
    if (!Walker.walkToDeclPre(D))
      return false;

    if (visit(D))
      return true;

    return !Walker.walkToDeclPost(D);
  }
  
  Pattern *doIt(Pattern *P) {
    // Do the pre-order visitation.  If it returns false, we just
    // skip entering subnodes of this tree.
    auto Pre = Walker.walkToPatternPre(P);
    if (!Pre.first || !Pre.second)
      return Pre.second;

    // Otherwise, visit the children.
    P = visit(P);

    // If we didn't bail out, do post-order visitation.
    if (P) P = Walker.walkToPatternPost(P);

    return P;
  }
  
  bool doIt(const StmtCondition &C) {
    for (auto &elt : C) {
      switch (elt.getKind()) {
      case StmtConditionElement::CK_Availability: break;
      case StmtConditionElement::CK_Boolean: {
        auto E = elt.getBoolean();
        // Walk an expression condition normally.
        E = doIt(E);
        if (!E)
          return true;
        elt.setBoolean(E);
        break;
      }
          
      case StmtConditionElement::CK_PatternBinding: {
        auto *P = doIt(elt.getPattern());
        if (!P) return true;
        elt.setPattern(P);

        auto *I = doIt(elt.getInitializer());
        if (!I) return true;
        elt.setInitializer(I);
      }
      }
    }
    return false;
  }

  bool doIt(TypeLoc &TL) {
    if (!Walker.walkToTypeLocPre(TL))
      return false;

    // No "visit" since TypeLocs are not a class hierarchy.  Clients can do what
    // they want in walkToTypeLocPre.

    if (auto typerepr = TL.getTypeRepr())
      if (doIt(typerepr))
        return true;

    // If we didn't bail out, do post-order visitation.
    return !Walker.walkToTypeLocPost(TL);
  }

  /// Returns true on failure.
  bool doIt(TypeRepr *T) {
    // Do the pre-order visitation.  If it returns false, we just
    // skip entering subnodes of this tree.
    if (!Walker.walkToTypeReprPre(T))
      return false;

    // Otherwise, visit the children.
    if (visit(T))
      return true;

    // If we didn't bail out, do post-order visitation.
    return !Walker.walkToTypeReprPost(T);
  }
  
  bool doIt(RequirementRepr &Req) {
    switch (Req.getKind()) {
    case RequirementReprKind::SameType:
      if (doIt(Req.getFirstTypeLoc()) || doIt(Req.getSecondTypeLoc()))
        return true;
      break;
    case RequirementReprKind::TypeConstraint:
      if (doIt(Req.getSubjectLoc()) || doIt(Req.getConstraintLoc()))
        return true;
      break;
    case RequirementReprKind::LayoutConstraint:
      if (doIt(Req.getFirstTypeLoc()))
        return true;
      break;
    }
    return false;
  }
};

} // end anonymous namespace

#pragma mark Statement traversal
Stmt *Traversal::visitBreakStmt(BreakStmt *BS) {
  return BS;
}

Stmt *Traversal::visitContinueStmt(ContinueStmt *CS) {
  return CS;
}

Stmt *Traversal::visitFallthroughStmt(FallthroughStmt *CS) {
  return CS;
}

Stmt *Traversal::visitFailStmt(FailStmt *FS) {
  return FS;
}
Stmt *Traversal::visitThrowStmt(ThrowStmt *TS) {
  if (Expr *E = doIt(TS->getSubExpr())) {
    TS->setSubExpr(E);
    return TS;
  }
  return nullptr;
}


Stmt *Traversal::visitBraceStmt(BraceStmt *BS) {
  for (auto &Elem : BS->getElements()) {
    if (auto *SubExpr = Elem.dyn_cast<Expr*>()) {
      if (Expr *E2 = doIt(SubExpr))
        Elem = E2;
      else
        return nullptr;
      continue;
    }

    if (auto *S = Elem.dyn_cast<Stmt*>()) {
      if (Stmt *S2 = doIt(S))
        Elem = S2;
      else
        return nullptr;
      continue;
    }

    if (doIt(Elem.get<Decl*>()))
      return nullptr;
  }

  return BS;
}

Stmt *Traversal::visitReturnStmt(ReturnStmt *RS) {
  if (!RS->hasResult())
    return RS;
  if (Expr *E = doIt(RS->getResult()))
    RS->setResult(E);
  else
    return nullptr;
  return RS;
}

Stmt *Traversal::visitDeferStmt(DeferStmt *DS) {
  if (doIt(DS->getTempDecl()))
    return nullptr;

  if (Expr *Call = doIt(DS->getCallExpr()))
    DS->setCallExpr(Call);
  else
    return nullptr;
  return DS;
}

Stmt *Traversal::visitIfStmt(IfStmt *IS) {
  if (doIt(IS->getCond()))
    return nullptr;

  if (Stmt *S2 = doIt(IS->getThenStmt()))
    IS->setThenStmt(S2);
  else
    return nullptr;

  if (IS->getElseStmt()) {
    if (Stmt *S2 = doIt(IS->getElseStmt()))
      IS->setElseStmt(S2);
    else
      return nullptr;
  }
  return IS;
}

Stmt *Traversal::visitGuardStmt(GuardStmt *US) {
  if (doIt(US->getCond()))
    return nullptr;
  
  if (Stmt *S2 = doIt(US->getBody()))
    US->setBody(S2);
  else
    return nullptr;
  return US;
}

Stmt *Traversal::visitDoStmt(DoStmt *DS) {
  if (Stmt *S2 = doIt(DS->getBody()))
    DS->setBody(S2);
  else
    return nullptr;

  return DS;
}

Stmt *Traversal::visitDoCatchStmt(DoCatchStmt *stmt) {
  // Transform the body of the 'do'.
  if (Stmt *newBody = doIt(stmt->getBody())) {
    stmt->setBody(newBody);
  } else {
    return nullptr;
  }

  // Transform each of the catch clauses:
  for (CatchStmt *&clause : stmt->getMutableCatches()) {
    if (auto newClause = doIt(clause)) {
      clause = cast<CatchStmt>(newClause);
    } else {
      return nullptr;
    }
  }

  return stmt;
}

Stmt *Traversal::visitCatchStmt(CatchStmt *stmt) {
  // Transform the error pattern.
  if (Pattern *newPattern = doIt(stmt->getErrorPattern())) {
    stmt->setErrorPattern(newPattern);
  } else {
    return nullptr;
  }

  // Transform the guard expression if present.
  if (Expr *oldGuardExpr = stmt->getGuardExpr()) {
    if (Expr *newGuardExpr = doIt(oldGuardExpr)) {
      stmt->setGuardExpr(newGuardExpr);
    } else {
      return nullptr;
    }
  }

  // Transform the body of the catch clause.
  if (Stmt *newCatchBody = doIt(stmt->getBody())) {
    stmt->setBody(newCatchBody);
  } else {
    return nullptr;
  }

  return stmt;
}

Stmt *Traversal::visitWhileStmt(WhileStmt *WS) {
  if (doIt(WS->getCond()))
    return nullptr;

  if (Stmt *S2 = doIt(WS->getBody()))
    WS->setBody(S2);
  else
    return nullptr;
  return WS;
}

Stmt *Traversal::visitRepeatWhileStmt(RepeatWhileStmt *RWS) {
  if (Stmt *S2 = doIt(RWS->getBody()))
    RWS->setBody(S2);
  else
    return nullptr;

  if (Expr *E2 = doIt(RWS->getCond()))
    RWS->setCond(E2);
  else
    return nullptr;

  return RWS;
}

Stmt *Traversal::visitForEachStmt(ForEachStmt *S) {
  if (Pattern *P = S->getPattern()) {
    if ((P = doIt(P)))
      assert(P == S->getPattern() && "cannot change pattern of ForEachStmt");
    else
      return nullptr;
  }

  if (Expr *Where = S->getWhere()) {
    if ((Where = doIt(Where)))
      S->setWhere(Where);
    else
      return nullptr;
  }

  // The iterator decl is built directly on top of the sequence
  // expression, so don't visit both.
  if (PatternBindingDecl *Iterator = S->getIterator()) {
    if (doIt(Iterator))
      return nullptr;

  } else if (Expr *Sequence = S->getSequence()) {
    if ((Sequence = doIt(Sequence)))
      S->setSequence(Sequence);
    else
      return nullptr;
  }

  if (auto IteratorNext = S->getIteratorNext()) {
    if ((IteratorNext = doIt(IteratorNext)))
      S->setIteratorNext(IteratorNext);
    else
      return nullptr;
  }

  if (Stmt *Body = S->getBody()) {
    if ((Body = doIt(Body)))
      S->setBody(cast<BraceStmt>(Body));
    else
      return nullptr;
  }

  return S;
}

Stmt *Traversal::visitSwitchStmt(SwitchStmt *S) {
  if (Expr *newSubject = doIt(S->getSubjectExpr()))
    S->setSubjectExpr(newSubject);
  else
    return nullptr;

  for (auto N : S->getRawCases()) {
    if (Stmt *aCase = N.dyn_cast<Stmt*>()) {
      assert(isa<CaseStmt>(aCase));
      if (Stmt *aStmt = doIt(aCase)) {
        assert(aCase == aStmt && "switch case remap not supported");
        (void)aStmt;
      } else
        return nullptr;
    } else {
      assert(isa<IfConfigDecl>(N.get<Decl*>()) || 
             isa<PoundDiagnosticDecl>(N.get<Decl*>()));
      if (doIt(N.get<Decl*>()))
        return nullptr;
    }
  }

  return S;
}

Stmt *Traversal::visitCaseStmt(CaseStmt *S) {
  for (auto &CLI : S->getMutableCaseLabelItems()) {
    if (auto *newPattern = doIt(CLI.getPattern()))
      CLI.setPattern(newPattern);
    else
      return nullptr;
    if (CLI.getGuardExpr()) {
      if (auto *newGuard = doIt(CLI.getGuardExpr()))
        CLI.setGuardExpr(newGuard);
      else
        return nullptr;
    }
  }

  if (Stmt *newBody = doIt(S->getBody()))
    S->setBody(newBody);
  else
    return nullptr;

  return S;
}

#pragma mark Pattern traversal
Pattern *Traversal::visitParenPattern(ParenPattern *P) {
  if (Pattern *newSub = doIt(P->getSubPattern()))
    P->setSubPattern(newSub);
  else
    return nullptr;
  return P;
}

Pattern *Traversal::visitTuplePattern(TuplePattern *P) {
  for (auto &element : P->getElements()) {
    if (Pattern *newField = doIt(element.getPattern()))
      element.setPattern(newField);
    else
      return nullptr;
  }
  return P;
}

Pattern *Traversal::visitNamedPattern(NamedPattern *P) {
  if (doIt(P->getDecl()))
    return nullptr;
  return P;
}

Pattern *Traversal::visitAnyPattern(AnyPattern *P) {
  return P;
}

Pattern *Traversal::visitTypedPattern(TypedPattern *P) {
  if (Pattern *newSub = doIt(P->getSubPattern()))
    P->setSubPattern(newSub);
  else
    return nullptr;
  if (!P->isImplicit())
    if (doIt(P->getTypeLoc()))
      return nullptr;
  return P;
}

Pattern *Traversal::visitIsPattern(IsPattern *P) {
  if (auto sub = P->getSubPattern()) {
    if (Pattern *newSub = doIt(sub)) {
      P->setSubPattern(newSub);
    } else {
      return nullptr;
    }
  }
  if (!P->isImplicit())
    if (doIt(P->getCastTypeLoc()))
      return nullptr;
  return P;
}

Pattern *Traversal::visitEnumElementPattern(EnumElementPattern *P) {
  if (!P->hasSubPattern())
    return P;

  if (Pattern *newSub = doIt(P->getSubPattern())) {
    P->setSubPattern(newSub);
    return P;
  }
  return nullptr;
}

Pattern *Traversal::visitExprPattern(ExprPattern *P) {
  // If the pattern has been type-checked, walk the match expression, which
  // includes the explicit subexpression.
  if (P->getMatchExpr()) {
    if (Expr *newMatch = doIt(P->getMatchExpr())) {
      P->setMatchExpr(newMatch);
      return P;
    }
    return nullptr;
  }

  if (Expr *newSub = doIt(P->getSubExpr())) {
    P->setSubExpr(newSub);
    return P;
  }
  return nullptr;
}

Pattern *Traversal::visitVarPattern(VarPattern *P) {
  if (Pattern *newSub = doIt(P->getSubPattern())) {
    P->setSubPattern(newSub);
    return P;
  }
  return nullptr;
}

Pattern *Traversal::visitOptionalSomePattern(OptionalSomePattern *P) {
  if (Pattern *newSub = doIt(P->getSubPattern())) {
    P->setSubPattern(newSub);
    return P;
  }
  return nullptr;
}

Pattern *Traversal::visitBoolPattern(BoolPattern *P) {
  return P;
}

#pragma mark Type representation traversal
bool Traversal::visitErrorTypeRepr(ErrorTypeRepr *T) {
  return false;
}

bool Traversal::visitAttributedTypeRepr(AttributedTypeRepr *T) {
  return doIt(T->getTypeRepr());
}

bool Traversal::visitSimpleIdentTypeRepr(SimpleIdentTypeRepr *T) {
  return false;
}

bool Traversal::visitGenericIdentTypeRepr(GenericIdentTypeRepr *T) {
  for (auto genArg : T->getGenericArgs()) {
    if (doIt(genArg))
      return true;
  }
  return false;
}

bool Traversal::visitCompoundIdentTypeRepr(CompoundIdentTypeRepr *T) {
  for (auto comp : T->getComponents()) {
    if (doIt(comp))
      return true;
  }
  return false;
}

bool Traversal::visitFunctionTypeRepr(FunctionTypeRepr *T) {
  return doIt(T->getArgsTypeRepr()) || doIt(T->getResultTypeRepr());
}

bool Traversal::visitArrayTypeRepr(ArrayTypeRepr *T) {
  return doIt(T->getBase());
}

bool Traversal::visitDictionaryTypeRepr(DictionaryTypeRepr *T) {
  return doIt(T->getKey()) || doIt(T->getValue());
}

bool Traversal::visitOptionalTypeRepr(OptionalTypeRepr *T) {
  return doIt(T->getBase());
}

bool Traversal::visitImplicitlyUnwrappedOptionalTypeRepr(ImplicitlyUnwrappedOptionalTypeRepr *T) {
  return doIt(T->getBase());
}

bool Traversal::visitTupleTypeRepr(TupleTypeRepr *T) {
  for (auto &elem : T->getElements()) {
    if (doIt(elem.Type))
      return true;
  }
  return false;
}

bool Traversal::visitCompositionTypeRepr(CompositionTypeRepr *T) {
  for (auto elem : T->getTypes()) {
    if (doIt(elem))
      return true;
  }
  return false;
}

bool Traversal::visitMetatypeTypeRepr(MetatypeTypeRepr *T) {
  return doIt(T->getBase());
}

bool Traversal::visitProtocolTypeRepr(ProtocolTypeRepr *T) {
  return doIt(T->getBase());
}

bool Traversal::visitInOutTypeRepr(InOutTypeRepr *T) {
  return doIt(T->getBase());
}

bool Traversal::visitSharedTypeRepr(SharedTypeRepr *T) {
  return doIt(T->getBase());
}

bool Traversal::visitOwnedTypeRepr(OwnedTypeRepr *T) {
  return doIt(T->getBase());
}

bool Traversal::visitFixedTypeRepr(FixedTypeRepr *T) {
  return false;
}

bool Traversal::visitSILBoxTypeRepr(SILBoxTypeRepr *T) {
  for (auto &field : T->getFields()) {
    if (doIt(field.getFieldType()))
      return true;
  }
  for (auto &arg : T->getGenericArguments()) {
    if (doIt(arg))
      return true;
  }
  return false;
}

Expr *Expr::walk(ASTWalker &walker) {
  return Traversal(walker).doIt(this);
}

Stmt *Stmt::walk(ASTWalker &walker) {
  return Traversal(walker).doIt(this);
}

Pattern *Pattern::walk(ASTWalker &walker) {
  return Traversal(walker).doIt(this);
}

TypeRepr *TypeRepr::walk(ASTWalker &walker) {
  Traversal(walker).doIt(this);
  return this;
}

StmtConditionElement *StmtConditionElement::walk(ASTWalker &walker) {
  Traversal(walker).doIt(*this);
  return this;
}

bool Decl::walk(ASTWalker &walker) {
  return Traversal(walker).doIt(this);
}
