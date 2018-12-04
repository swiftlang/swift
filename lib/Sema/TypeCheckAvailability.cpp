//===--- TypeCheckAvailability.cpp - Availability Diagnostics -------------===//
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
// This file implements availability diagnostics.
//
//===----------------------------------------------------------------------===//

#include "TypeCheckAvailability.h"
#include "TypeChecker.h"
#include "MiscDiagnostics.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/TypeRefinementContext.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/Parser.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/SaveAndRestore.h"
using namespace swift;

/// Returns the first availability attribute on the declaration that is active
/// on the target platform.
static const AvailableAttr *getActiveAvailableAttribute(const Decl *D,
                                                        ASTContext &AC) {
  for (auto Attr : D->getAttrs())
    if (auto AvAttr = dyn_cast<AvailableAttr>(Attr)) {
      if (!AvAttr->isInvalid() && AvAttr->isActivePlatform(AC)) {
        return AvAttr;
      }
    }
  return nullptr;
}

/// Returns true if there is any availability attribute on the declaration
/// that is active on the target platform.
static bool hasActiveAvailableAttribute(Decl *D,
                                           ASTContext &AC) {
  return getActiveAvailableAttribute(D, AC);
}

namespace {

/// A class to walk the AST to build the type refinement context hierarchy.
class TypeRefinementContextBuilder : private ASTWalker {

  struct ContextInfo {
    TypeRefinementContext *TRC;

    /// The node whose end marks the end of the refinement context.
    /// If the builder sees this node in a post-visitor, it will pop
    /// the context from the stack. This node can be null (ParentTy()),
    /// indicating that custom logic elsewhere will handle removing
    /// the context when needed.
    ParentTy ScopeNode;
  };

  std::vector<ContextInfo> ContextStack;
  TypeChecker &TC;

  /// A mapping from abstract storage declarations with accessors to
  /// to the type refinement contexts for those declarations. We refer to
  /// this map to determine the appropriate parent TRC to use when
  /// walking the accessor function.
  llvm::DenseMap<AbstractStorageDecl *, TypeRefinementContext *>
      StorageContexts;

  TypeRefinementContext *getCurrentTRC() {
    return ContextStack.back().TRC;
  }

  void pushContext(TypeRefinementContext *TRC, ParentTy PopAfterNode) {
    ContextInfo Info;
    Info.TRC = TRC;
    Info.ScopeNode = PopAfterNode;
    ContextStack.push_back(Info);
  }

public:
  TypeRefinementContextBuilder(TypeRefinementContext *TRC, TypeChecker &TC)
      : TC(TC) {
    assert(TRC);
    pushContext(TRC, ParentTy());
  }

  void build(Decl *D) {
    unsigned StackHeight = ContextStack.size();
    D->walk(*this);
    assert(ContextStack.size() == StackHeight);
    (void)StackHeight;
  }

  void build(Stmt *S) {
    unsigned StackHeight = ContextStack.size();
    S->walk(*this);
    assert(ContextStack.size() == StackHeight);
    (void)StackHeight;
  }

  void build(Expr *E) {
    unsigned StackHeight = ContextStack.size();
    E->walk(*this);
    assert(ContextStack.size() == StackHeight);
    (void)StackHeight;
  }

private:
  bool walkToDeclPre(Decl *D) override {
    TypeRefinementContext *DeclTRC = getNewContextForWalkOfDecl(D);

    if (DeclTRC) {
      pushContext(DeclTRC, D);
    }

    return true;
  }

  bool walkToDeclPost(Decl *D) override {
    if (ContextStack.back().ScopeNode.getAsDecl() == D) {
      ContextStack.pop_back();
    }
    return true;
  }

  /// Returns a new context to be introduced for the declaration, or nullptr
  /// if no new context should be introduced.
  TypeRefinementContext *getNewContextForWalkOfDecl(Decl *D) {
    if (auto accessor = dyn_cast<AccessorDecl>(D)) {
      // Use TRC of the storage rather the current TRC when walking this
      // function.
      auto it = StorageContexts.find(accessor->getStorage());
      if (it != StorageContexts.end()) {
        return it->second;
      }
    }
    
    if (declarationIntroducesNewContext(D)) {
      return buildDeclarationRefinementContext(D);
    }
    
    return nullptr;
  }

  /// Builds the type refinement hierarchy for the body of the function.
  TypeRefinementContext *buildDeclarationRefinementContext(Decl *D) {
    // We require a valid range in order to be able to query for the TRC
    // corresponding to a given SourceLoc.
    // If this assert fires, it means we have probably synthesized an implicit
    // declaration without location information. The appropriate fix is
    // probably to gin up a source range for the declaration when synthesizing
    // it.
    assert(D->getSourceRange().isValid());
    
    // The potential versions in the declaration are constrained by both
    // the declared availability of the declaration and the potential versions
    // of its lexical context.
    AvailabilityContext DeclInfo =
        swift::AvailabilityInference::availableRange(D, TC.Context);
    DeclInfo.intersectWith(getCurrentTRC()->getAvailabilityInfo());

    TypeRefinementContext *NewTRC =
        TypeRefinementContext::createForDecl(TC.Context, D, getCurrentTRC(),
                                             DeclInfo,
                                             refinementSourceRangeForDecl(D));
    
    // Record the TRC for this storage declaration so that
    // when we process the accessor, we can use this TRC as the
    // parent.
    if (auto *StorageDecl = dyn_cast<AbstractStorageDecl>(D)) {
      if (StorageDecl->hasAnyAccessors()) {
        StorageContexts[StorageDecl] = NewTRC;
      }
    }
    
    return NewTRC;
  }
  
  /// Returns true if the declaration should introduce a new refinement context.
  bool declarationIntroducesNewContext(Decl *D) {
    if (!isa<ValueDecl>(D) && !isa<ExtensionDecl>(D)) {
      return false;
    }
    
    // No need to introduce a context if the declaration does not have an
    // availability attribute.
    if (!hasActiveAvailableAttribute(D, TC.Context)) {
      return false;
    }
    
    // Only introduce for an AbstractStorageDecl if it is not local.
    // We introduce for the non-local case because these may
    // have getters and setters (and these may be synthesized, so they might
    // not even exist yet).
    if (auto *storageDecl = dyn_cast<AbstractStorageDecl>(D)) {
      if (storageDecl->getDeclContext()->isLocalContext()) {
        // No need to
        return false;
      }
    }
    
    return true;
  }

  /// Returns the source range which should be refined by declaration. This
  /// provides a convenient place to specify the refined range when it is
  /// different than the declaration's source range.
  SourceRange refinementSourceRangeForDecl(Decl *D) {
    if (auto *storageDecl = dyn_cast<AbstractStorageDecl>(D)) {
      // Use the declaration's availability for the context when checking
      // the bodies of its accessors.

      // HACK: For synthesized trivial accessors we may have not a valid
      // location for the end of the braces, so in that case we will fall back
      // to using the range for the storage declaration. The right fix here is
      // to update AbstractStorageDecl::addTrivialAccessors() to take brace
      // locations and have callers of that method provide appropriate source
      // locations.
      SourceLoc BracesEnd = storageDecl->getBracesRange().End;
      if (storageDecl->hasAnyAccessors() && BracesEnd.isValid()) {
        return SourceRange(storageDecl->getStartLoc(),
                           BracesEnd);
      }
      
      // For a variable declaration (without accessors) we use the range of the
      // containing pattern binding declaration to make sure that we include
      // any type annotation in the type refinement context range.
      if (auto varDecl = dyn_cast<VarDecl>(storageDecl)) {
        auto *PBD = varDecl->getParentPatternBinding();
        if (PBD)
          return PBD->getSourceRange();
      }
    }
    
    return D->getSourceRange();
  }

  std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
    if (auto *IS = dyn_cast<IfStmt>(S)) {
      buildIfStmtRefinementContext(IS);
      return std::make_pair(false, S);
    }

    if (auto *RS = dyn_cast<GuardStmt>(S)) {
      buildGuardStmtRefinementContext(RS);
      return std::make_pair(false, S);
    }

    if (auto *WS = dyn_cast<WhileStmt>(S)) {
      buildWhileStmtRefinementContext(WS);
      return std::make_pair(false, S);
    }

    return std::make_pair(true, S);
  }

  Stmt *walkToStmtPost(Stmt *S) override {
    // If we have multiple guard statements in the same block
    // then we may have multiple refinement contexts to pop
    // after walking that block.
    while (!ContextStack.empty() &&
           ContextStack.back().ScopeNode.getAsStmt() == S) {
      ContextStack.pop_back();
    }

    return S;
  }

  /// Builds the type refinement hierarchy for the IfStmt if the guard
  /// introduces a new refinement context for the Then branch.
  /// There is no need for the caller to explicitly traverse the children
  /// of this node.
  void buildIfStmtRefinementContext(IfStmt *IS) {
    Optional<AvailabilityContext> ThenRange;
    Optional<AvailabilityContext> ElseRange;
    std::tie(ThenRange, ElseRange) =
        buildStmtConditionRefinementContext(IS->getCond());

    if (ThenRange.hasValue()) {
      // Create a new context for the Then branch and traverse it in that new
      // context.
      auto *ThenTRC =
          TypeRefinementContext::createForIfStmtThen(TC.Context, IS,
                                                     getCurrentTRC(),
                                                     ThenRange.getValue());
      TypeRefinementContextBuilder(ThenTRC, TC).build(IS->getThenStmt());
    } else {
      build(IS->getThenStmt());
    }

    Stmt *ElseStmt = IS->getElseStmt();
    if (!ElseStmt)
      return;

    // Refine the else branch if we're given a version range for that branch.
    // For now, if present, this will only be the empty range, indicating
    // that the branch is dead. We use it to suppress potential unavailability
    // and deprecation diagnostics on code that definitely will not run with
    // the current platform and minimum deployment target.
    // If we add a more precise version range lattice (i.e., one that can
    // support "<") we should create non-empty contexts for the Else branch.
    if (ElseRange.hasValue()) {
      // Create a new context for the Then branch and traverse it in that new
      // context.
      auto *ElseTRC =
          TypeRefinementContext::createForIfStmtElse(TC.Context, IS,
                                                     getCurrentTRC(),
                                                     ElseRange.getValue());
      TypeRefinementContextBuilder(ElseTRC, TC).build(ElseStmt);
    } else {
      build(IS->getElseStmt());
    }
  }

  /// Builds the type refinement hierarchy for the WhileStmt if the guard
  /// introduces a new refinement context for the body branch.
  /// There is no need for the caller to explicitly traverse the children
  /// of this node.
  void buildWhileStmtRefinementContext(WhileStmt *WS) {
    Optional<AvailabilityContext> BodyRange =
        buildStmtConditionRefinementContext(WS->getCond()).first;

    if (BodyRange.hasValue()) {
      // Create a new context for the body and traverse it in the new
      // context.
      auto *BodyTRC = TypeRefinementContext::createForWhileStmtBody(
          TC.Context, WS, getCurrentTRC(), BodyRange.getValue());
      TypeRefinementContextBuilder(BodyTRC, TC).build(WS->getBody());
    } else {
      build(WS->getBody());
    }
  }

  /// Builds the type refinement hierarchy for the GuardStmt and pushes
  /// the fallthrough context onto the context stack so that subsequent
  /// AST elements in the same scope are analyzed in the context of the
  /// fallthrough TRC.
  void buildGuardStmtRefinementContext(GuardStmt *GS) {
    // 'guard' statements fall through if all of the
    // guard conditions are true, so we refine the range after the require
    // until the end of the enclosing block.
    // if ... {
    //   guard available(...) else { return } <-- Refined range starts here
    //   ...
    // } <-- Refined range ends here
    //
    // This is slightly tricky because, unlike our other control constructs,
    // the refined region is not lexically contained inside the construct
    // introducing the refinement context.
    Optional<AvailabilityContext> FallthroughRange;
    Optional<AvailabilityContext> ElseRange;
    std::tie(FallthroughRange, ElseRange) =
        buildStmtConditionRefinementContext(GS->getCond());

    if (Stmt *ElseBody = GS->getBody()) {
      if (ElseRange.hasValue()) {
        auto *TrueTRC = TypeRefinementContext::createForGuardStmtElse(
            TC.Context, GS, getCurrentTRC(), ElseRange.getValue());

        TypeRefinementContextBuilder(TrueTRC, TC).build(ElseBody);
      } else {
        build(ElseBody);
      }
    }

    auto *ParentBrace = dyn_cast<BraceStmt>(Parent.getAsStmt());
    assert(ParentBrace && "Expected parent of GuardStmt to be BraceStmt");
    if (!FallthroughRange.hasValue())
      return;

    // Create a new context for the fallthrough.

    auto *FallthroughTRC =
          TypeRefinementContext::createForGuardStmtFallthrough(TC.Context, GS,
              ParentBrace, getCurrentTRC(), FallthroughRange.getValue());

    pushContext(FallthroughTRC, ParentBrace);
  }

  /// Build the type refinement context for a StmtCondition and return a pair
  /// of optional version ranges, the first for the true branch and the second
  /// for the false branch. A value of None for a given branch indicates that
  /// the branch does not introduce a new refinement.
  std::pair<Optional<AvailabilityContext>, Optional<AvailabilityContext>>
  buildStmtConditionRefinementContext(StmtCondition Cond) {

    // Any refinement contexts introduced in the statement condition
    // will end at the end of the last condition element.
    StmtConditionElement LastElement = Cond.back();
    
    // Keep track of how many nested refinement contexts we have pushed on
    // the context stack so we can pop them when we're done building the
    // context for the StmtCondition.
    unsigned NestedCount = 0;

    // Tracks the potential version range when the condition is false.
    auto FalseFlow = AvailabilityContext::neverAvailable();

    TypeRefinementContext *StartingTRC = getCurrentTRC();

    for (StmtConditionElement Element : Cond) {
      TypeRefinementContext *CurrentTRC = getCurrentTRC();
      AvailabilityContext CurrentInfo = CurrentTRC->getAvailabilityInfo();

      // If the element is not a condition, walk it in the current TRC.
      if (Element.getKind() != StmtConditionElement::CK_Availability) {

        // Assume any condition element that is not a #available() can
        // potentially be false, so conservatively combine the version
        // range of the current context with the accumulated false flow
        // of all other conjuncts.
        FalseFlow.unionWith(CurrentInfo);

        Element.walk(*this);
        continue;
      }

      // #available query: introduce a new refinement context for the statement
      // condition elements following it.
      auto *Query = Element.getAvailability();

      // If this query expression has no queries, we will not introduce a new
      // refinement context. We do not diagnose here: a diagnostic will already
      // have been emitted by the parser.
      if (Query->getQueries().empty())
        continue;

      AvailabilitySpec *Spec = bestActiveSpecForQuery(Query);
      if (!Spec) {
        // We couldn't find an appropriate spec for the current platform,
        // so rather than refining, emit a diagnostic and just use the current
        // TRC.
        TC.Diags.diagnose(Query->getLoc(),
                          diag::availability_query_required_for_platform,
                          platformString(targetPlatform(TC.getLangOpts())));
        
        continue;
      }

      AvailabilityContext NewConstraint = contextForSpec(Spec);
      Query->setAvailableRange(NewConstraint.getOSVersion());

      if (Spec->getKind() == AvailabilitySpecKind::OtherPlatform) {
        // The wildcard spec '*' represents the minimum deployment target, so
        // there is no need to create a refinement context for this query.
        // Further, we won't diagnose for useless #available() conditions
        // where * matched on this platform -- presumably those conditions are
        // needed for some other platform.
        continue;
      }


      // If the version range for the current TRC is completely contained in
      // the range for the spec, then a version query can never be false, so the
      // spec is useless. If so, report this.
      if (CurrentInfo.isContainedIn(NewConstraint)) {
        DiagnosticEngine &Diags = TC.Diags;
        // Some availability checks will always pass because the minimum
        // deployment target guarantees they will never be false. We don't
        // diagnose these checks as useless because the source file may
        // be shared with other projects/targets having older deployment
        // targets. We don't currently have a mechanism for the user to
        // suppress these warnings (for example, by indicating when the
        // required compatibility version is different than the deployment
        // target).
        if (CurrentTRC->getReason() != TypeRefinementContext::Reason::Root) {
          Diags.diagnose(Query->getLoc(),
                         diag::availability_query_useless_enclosing_scope,
                         platformString(targetPlatform(TC.getLangOpts())));
          Diags.diagnose(CurrentTRC->getIntroductionLoc(),
                         diag::availability_query_useless_enclosing_scope_here);
        }

        // No need to actually create the refinement context if we know it is
        // useless.
        continue;
      }

      // If the #available() is not useless then there is potential false flow,
      // so join the false flow with the potential versions of the current
      // context.
      // We could be more precise here if we enriched the lattice to include
      // ranges of the form [x, y).
      FalseFlow.unionWith(CurrentInfo);

      auto *TRC = TypeRefinementContext::createForConditionFollowingQuery(
          TC.Context, Query, LastElement, CurrentTRC, NewConstraint);

      pushContext(TRC, ParentTy());
      NestedCount++;
    }


    Optional<AvailabilityContext> FalseRefinement = None;
    // The version range for the false branch should never have any versions
    // that weren't possible when the condition started evaluating.
    assert(FalseFlow.isContainedIn(StartingTRC->getAvailabilityInfo()));

    // If the starting version range is not completely contained in the
    // false flow version range then it must be the case that false flow range
    // is strictly smaller than the starting range (because the false flow
    // range *is* contained in the starting range), so we should introduce a
    // new refinement for the false flow.
    if (!StartingTRC->getAvailabilityInfo().isContainedIn(FalseFlow)) {
      FalseRefinement = FalseFlow;
    }

    if (NestedCount == 0)
      return std::make_pair(None, FalseRefinement);

    TypeRefinementContext *NestedTRC = getCurrentTRC();
    while (NestedCount-- > 0)
      ContextStack.pop_back();

    assert(getCurrentTRC() == StartingTRC);

    return std::make_pair(NestedTRC->getAvailabilityInfo(), FalseRefinement);
  }

  /// Return the best active spec for the target platform or nullptr if no
  /// such spec exists.
  AvailabilitySpec *bestActiveSpecForQuery(PoundAvailableInfo *available) {
    OtherPlatformAvailabilitySpec *FoundOtherSpec = nullptr;
    for (auto *Spec : available->getQueries()) {
      if (auto *OtherSpec = dyn_cast<OtherPlatformAvailabilitySpec>(Spec)) {
        FoundOtherSpec = OtherSpec;
        continue;
      }

      auto *VersionSpec = dyn_cast<PlatformVersionConstraintAvailabilitySpec>(Spec);
      if (!VersionSpec)
        continue;

      // FIXME: This is not quite right: we want to handle AppExtensions
      // properly. For example, on the OSXApplicationExtension platform
      // we want to chose the OS X spec unless there is an explicit
      // OSXApplicationExtension spec.
      if (isPlatformActive(VersionSpec->getPlatform(), TC.getLangOpts())) {
        return VersionSpec;
      }
    }

    // If we have reached this point, we found no spec for our target, so
    // we return the other spec ('*'), if we found it, or nullptr, if not.
    return FoundOtherSpec;
  }

  /// Return the availability context for the given spec.
  AvailabilityContext contextForSpec(AvailabilitySpec *Spec) {
    if (isa<OtherPlatformAvailabilitySpec>(Spec)) {
      return AvailabilityContext::alwaysAvailable();
    }

    auto *VersionSpec = cast<PlatformVersionConstraintAvailabilitySpec>(Spec);
    return AvailabilityContext(VersionRange::allGTE(VersionSpec->getVersion()));
  }

  Expr *walkToExprPost(Expr *E) override {
    if (ContextStack.back().ScopeNode.getAsExpr() == E) {
      ContextStack.pop_back();
    }

    return E;
  }
};
  
} // end anonymous namespace

void TypeChecker::buildTypeRefinementContextHierarchy(SourceFile &SF,
                                                      unsigned StartElem) {
  TypeRefinementContext *RootTRC = SF.getTypeRefinementContext();

  // If we are not starting at the beginning of the source file, we had better
  // already have a root type refinement context.
  assert(StartElem == 0 || RootTRC);

  ASTContext &AC = SF.getASTContext();

  if (!RootTRC) {
    // The root type refinement context reflects the fact that all parts of
    // the source file are guaranteed to be executing on at least the minimum
    // platform version.
    AvailabilityContext MinPlatformReq{
        VersionRange::allGTE(AC.LangOpts.getMinPlatformVersion())};
    RootTRC = TypeRefinementContext::createRoot(&SF, MinPlatformReq);
    SF.setTypeRefinementContext(RootTRC);
  }

  // Build refinement contexts, if necessary, for all declarations starting
  // with StartElem.
  TypeRefinementContextBuilder Builder(RootTRC, *this);
  for (auto D : llvm::makeArrayRef(SF.Decls).slice(StartElem)) {
    Builder.build(D);
  }
}

TypeRefinementContext *
TypeChecker::getOrBuildTypeRefinementContext(SourceFile *SF) {
  TypeRefinementContext *TRC = SF->getTypeRefinementContext();
  if (!TRC) {
    buildTypeRefinementContextHierarchy(*SF, 0);
    TRC = SF->getTypeRefinementContext();
  }

  return TRC;
}

AvailabilityContext
TypeChecker::overApproximateAvailabilityAtLocation(SourceLoc loc,
                                                   const DeclContext *DC,
                                                   const TypeRefinementContext **MostRefined) {
  SourceFile *SF = DC->getParentSourceFile();

  // If our source location is invalid (this may be synthesized code), climb
  // the decl context hierarchy until we find a location that is valid,
  // collecting availability ranges on the way up.
  // We will combine the version ranges from these annotations
  // with the TRC for the valid location to overapproximate the running
  // OS versions at the original source location.
  // Because we are climbing DeclContexts we will miss refinement contexts in
  // synthesized code that are introduced by AST elements that are themselves
  // not DeclContexts, such as  #available(..) and property declarations.
  // That is, a reference with an invalid location that is contained
  // inside a #available() and with no intermediate DeclContext will not be
  // refined. For now, this is fine -- but if we ever synthesize #available(),
  // this will be a real problem.

  // We can assume we are running on at least the minimum deployment target.
  AvailabilityContext OverApproximateContext{
    VersionRange::allGTE(getLangOpts().getMinPlatformVersion())};

  while (DC && loc.isInvalid()) {
    const Decl *D = DC->getInnermostDeclarationDeclContext();
    if (!D)
      break;

    loc = D->getLoc();

    Optional<AvailabilityContext> Info =
        AvailabilityInference::annotatedAvailableRange(D, Context);

    if (Info.hasValue()) {
      OverApproximateContext.constrainWith(Info.getValue());
    }

    DC = D->getDeclContext();
  }

  if (SF && loc.isValid()) {
    TypeRefinementContext *rootTRC = getOrBuildTypeRefinementContext(SF);
    TypeRefinementContext *TRC =
        rootTRC->findMostRefinedSubContext(loc, Context.SourceMgr);
    OverApproximateContext.constrainWith(TRC->getAvailabilityInfo());
    if (MostRefined) {
      *MostRefined = TRC;
    }
  }

  return OverApproximateContext;
}

bool TypeChecker::isDeclAvailable(const Decl *D, SourceLoc referenceLoc,
                                  const DeclContext *referenceDC,
                                  AvailabilityContext &OutAvailableInfo) {

  AvailabilityContext safeRangeUnderApprox{
      AvailabilityInference::availableRange(D, Context)};
  AvailabilityContext runningOSOverApprox =
      overApproximateAvailabilityAtLocation(referenceLoc, referenceDC);

  // The reference is safe if an over-approximation of the running OS
  // versions is fully contained within an under-approximation
  // of the versions on which the declaration is available. If this
  // containment cannot be guaranteed, we say the reference is
  // not available.
  if (!(runningOSOverApprox.isContainedIn(safeRangeUnderApprox))) {
    OutAvailableInfo = safeRangeUnderApprox;
    return false;
  }
  
  return true;
}

Optional<UnavailabilityReason>
TypeChecker::checkDeclarationAvailability(const Decl *D, SourceLoc referenceLoc,
                                          const DeclContext *referenceDC) {
  if (Context.LangOpts.DisableAvailabilityChecking) {
    return None;
  }

  if (!referenceDC->getParentSourceFile()) {
    // We only check availability if this reference is in a source file; we do
    // not check in other kinds of FileUnits.
    return None;
  }

  auto safeRangeUnderApprox = AvailabilityContext::neverAvailable();
  if (isDeclAvailable(D, referenceLoc, referenceDC, safeRangeUnderApprox)) {
    return None;
  }

  // safeRangeUnderApprox now holds the safe range.
  VersionRange version = safeRangeUnderApprox.getOSVersion();
  return UnavailabilityReason::requiresVersionRange(version);
}

void TypeChecker::diagnosePotentialUnavailability(
    const ValueDecl *D, SourceRange ReferenceRange,
    const DeclContext *ReferenceDC,
    const UnavailabilityReason &Reason) {
  diagnosePotentialUnavailability(D, D->getFullName(), ReferenceRange,
                                  ReferenceDC, Reason);
}

/// A class that walks the AST to find the innermost (i.e., deepest) node that
/// contains a target SourceRange and matches a particular criterion.
/// This class finds the innermost nodes of interest by walking
/// down the root until it has found the target range (in a Pre-visitor)
/// and then recording the innermost node on the way back up in the
/// Post-visitors. It does its best to not search unnecessary subtrees,
/// although this is complicated by the fact that not all nodes have
/// source range information.
class InnermostAncestorFinder : private ASTWalker {
public:

  /// The type of a match predicate, which takes as input a node and its
  /// parent and returns a bool indicating whether the node matches.
  using MatchPredicate = std::function<bool(ASTNode, ASTWalker::ParentTy)>;

private:
  const SourceRange TargetRange;
  const SourceManager &SM;
  const MatchPredicate Predicate;

  bool FoundTarget = false;
  Optional<ASTNode> InnermostMatchingNode;

public:
  InnermostAncestorFinder(SourceRange TargetRange, const SourceManager &SM,
                          ASTNode SearchNode, const MatchPredicate &Predicate)
      : TargetRange(TargetRange), SM(SM), Predicate(Predicate) {
    assert(TargetRange.isValid());

    SearchNode.walk(*this);
  }

  /// Returns the innermost node containing the target range that matches
  /// the predicate.
  Optional<ASTNode> getInnermostMatchingNode() { return InnermostMatchingNode; }

  std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
    return std::make_pair(walkToRangePre(E->getSourceRange()), E);
  }

  std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
    return std::make_pair(walkToRangePre(S->getSourceRange()), S);
  }

  bool walkToDeclPre(Decl *D) override {
    return walkToRangePre(D->getSourceRange());
  }

  std::pair<bool, Pattern *> walkToPatternPre(Pattern *P) override {
    return std::make_pair(walkToRangePre(P->getSourceRange()), P);
  }

  bool walkToTypeReprPre(TypeRepr *T) override {
    return walkToRangePre(T->getSourceRange());
  }

  /// Returns true if the walker should traverse an AST node with
  /// source range Range.
  bool walkToRangePre(SourceRange Range) {
    // When walking down the tree, we traverse until we have found a node
    // inside the target range. Once we have found such a node, there is no
    // need to traverse any deeper.
    if (FoundTarget)
      return false;

    // If we haven't found our target yet and the node we are pre-visiting
    // doesn't have a valid range, we still have to traverse it because its
    // subtrees may have valid ranges.
    if (Range.isInvalid())
      return true;

    // We have found our target if the range of the node we are visiting
    // is contained in the range we are looking for.
    FoundTarget = SM.rangeContains(TargetRange, Range);

    if (FoundTarget)
      return false;

    // Search the subtree if the target range is inside its range.
    return SM.rangeContains(Range, TargetRange);
  }

  Expr *walkToExprPost(Expr *E) override {
    if (walkToNodePost(E)) {
      return E;
    }

    return nullptr;
  }

  Stmt *walkToStmtPost(Stmt *S) override {
    if (walkToNodePost(S)) {
      return S;
    }

    return nullptr;
  }

  bool walkToDeclPost(Decl *D) override {
    return walkToNodePost(D);
  }

  /// Once we have found the target node, look for the innermost ancestor
  /// matching our criteria on the way back up the spine of the tree.
  bool walkToNodePost(ASTNode Node) {
    if (!InnermostMatchingNode.hasValue() && Predicate(Node, Parent)) {
      assert(Node.getSourceRange().isInvalid() ||
             SM.rangeContains(Node.getSourceRange(), TargetRange));

      InnermostMatchingNode = Node;
      return false;
    }

    return true;
  }
};

/// Starting from SearchRoot, finds the innermost node containing ChildRange
/// for which Predicate returns true. Returns None if no such root is found.
static Optional<ASTNode> findInnermostAncestor(
    SourceRange ChildRange, const SourceManager &SM, ASTNode SearchRoot,
    const InnermostAncestorFinder::MatchPredicate &Predicate) {
  InnermostAncestorFinder Finder(ChildRange, SM, SearchRoot, Predicate);
  return Finder.getInnermostMatchingNode();
}

/// Given a reference range and a declaration context containing the range,
/// attempt to find a declaration containing the reference. This may not
/// be the innermost declaration containing the range.
/// Returns null if no such declaration can be found.
static const Decl *findContainingDeclaration(SourceRange ReferenceRange,
                                             const DeclContext *ReferenceDC,
                                             const SourceManager &SM) {
  if (const Decl *D = ReferenceDC->getInnermostDeclarationDeclContext())
    return D;

  // We couldn't find a suitable node by climbing the DeclContext
  // hierarchy, so fall back to looking for a top-level declaration
  // that contains the reference range. We will hit this case for
  // top-level elements that do not themselves introduce DeclContexts,
  // such as extensions and global variables. If we don't have a reference
  // range, there is nothing we can do, so return null.
  if (ReferenceRange.isInvalid())
    return nullptr;

  SourceFile *SF = ReferenceDC->getParentSourceFile();
  if (!SF)
    return nullptr;

  for (Decl *D : SF->Decls) {
    if (SM.rangeContains(D->getSourceRange(), ReferenceRange)) {
      return D;
    }
  }

  return nullptr;
}

/// Given a declaration that allows availability attributes in the abstract
/// syntax tree, return the declaration upon which the declaration would
/// appear in concrete syntax. This function is necessary because for semantic
/// analysis, the parser attaches attributes to declarations other
/// than those on which they, concretely, appear. For these declarations (enum
/// cases and variable declarations) a Fix-It for an added availability
/// attribute should be suggested for the appropriate concrete location.
static const Decl *
concreteSyntaxDeclForAvailableAttribute(const Decl *AbstractSyntaxDecl) {
  // This function needs to be kept in sync with its counterpart,
  // abstractSyntaxDeclForAvailableAttribute().

  // The source range for VarDecls does not include 'var ' (and, in any
  // event, multiple variables can be introduced with a single 'var'),
  // so suggest adding an attribute to the PatterningBindingDecl instead.
  if (auto *VD = dyn_cast<VarDecl>(AbstractSyntaxDecl)) {
    return VD->getParentPatternBinding();
  }

  // Similarly suggest applying the Fix-It to the parent enum case rather than
  // the enum element.
  if (auto *EE = dyn_cast<EnumElementDecl>(AbstractSyntaxDecl)) {
    return EE->getParentCase();
  }

  return AbstractSyntaxDecl;
}

/// Given a declaration upon which an availability attribute would appear in
/// concrete syntax, return a declaration to which the parser
/// actually attaches the attribute in the abstract syntax tree. We use this
/// function to determine whether the concrete syntax already has an
/// availability attribute.
static const Decl *
abstractSyntaxDeclForAvailableAttribute(const Decl *ConcreteSyntaxDecl) {
  // This function needs to be kept in sync with its counterpart,
  // concreteSyntaxDeclForAvailableAttribute().

  if (auto *PBD = dyn_cast<PatternBindingDecl>(ConcreteSyntaxDecl)) {
    // Existing @available attributes in the AST are attached to VarDecls
    // rather than PatternBindingDecls, so we return the first VarDecl for
    // the pattern binding declaration.
    // This is safe, even though there may be multiple VarDecls, because
    // all parsed attribute that appear in the concrete syntax upon on the
    // PatternBindingDecl are added to all of the VarDecls for the pattern
    // binding.
    ArrayRef<PatternBindingEntry> Entries = PBD->getPatternList();
    if (!Entries.empty()) {
      VarDecl *VD = Entries.front().getPattern()->getSingleVar();
      if (VD)
        return VD;
    }
  } else if (auto *ECD = dyn_cast<EnumCaseDecl>(ConcreteSyntaxDecl)) {
    // Similar to the PatternBindingDecl case above, we return the
    // first EnumElementDecl.
    ArrayRef<EnumElementDecl *> Elems = ECD->getElements();
    if (!Elems.empty()) {
      return Elems.front();
    }
  }

  return ConcreteSyntaxDecl;
}

/// Given a declaration, return a better related declaration for which
/// to suggest an @available fixit, or the original declaration
/// if no such related declaration exists.
static const Decl *relatedDeclForAvailabilityFixit(const Decl *D) {
  if (auto *accessor = dyn_cast<AccessorDecl>(D)) {
    // Suggest @available Fix-Its on property rather than individual
    // accessors.
    D = accessor->getStorage();
  }

  return abstractSyntaxDeclForAvailableAttribute(D);
}

/// Walk the DeclContext hierarchy starting from D to find a declaration
/// at the member level (i.e., declared in a type context) on which to provide
/// an @available() Fix-It.
static const Decl *ancestorMemberLevelDeclForAvailabilityFixit(const Decl *D) {
  while (D) {
    D = relatedDeclForAvailabilityFixit(D);

    if (D->getDeclContext()->isTypeContext() &&
        DeclAttribute::canAttributeAppearOnDecl(DeclAttrKind::DAK_Available,
                                                D)) {
      break;
    }

    D = cast_or_null<AbstractFunctionDecl>(
        D->getDeclContext()->getInnermostMethodContext());
  }

  return D;
}

/// Returns true if the declaration is at the type level (either a nominal
/// type, an extension, or a global function) and can support an @available
/// attribute.
static bool isTypeLevelDeclForAvailabilityFixit(const Decl *D) {
  if (!DeclAttribute::canAttributeAppearOnDecl(DeclAttrKind::DAK_Available,
                                               D)) {
    return false;
  }

  if (isa<ExtensionDecl>(D) || isa<NominalTypeDecl>(D)) {
    return true;
  }

  bool IsModuleScopeContext = D->getDeclContext()->isModuleScopeContext();

  // We consider global functions to be "type level"
  if (isa<FuncDecl>(D)) {
    return IsModuleScopeContext;
  }

  if (auto *VD = dyn_cast<VarDecl>(D)) {
    if (!IsModuleScopeContext)
      return false;

    if (PatternBindingDecl *PBD = VD->getParentPatternBinding()) {
      return PBD->getDeclContext()->isModuleScopeContext();
    }
  }

  return false;
}

/// Walk the DeclContext hierarchy starting from D to find a declaration
/// at a member level (i.e., declared in a type context) on which to provide an
/// @available() Fix-It.
static const Decl *ancestorTypeLevelDeclForAvailabilityFixit(const Decl *D) {
  assert(D);

  D = relatedDeclForAvailabilityFixit(D);

  while (D && !isTypeLevelDeclForAvailabilityFixit(D)) {
    D = D->getDeclContext()->getInnermostDeclarationDeclContext();
  }

  return D;
}

/// Given the range of a reference to an unavailable symbol and the
/// declaration context containing the reference, make a best effort find up to
/// three locations for potential fixits.
///
/// \param FoundVersionCheckNode Returns a node that can be wrapped in a
/// if #available(...) { ... } version check to fix the unavailable reference,
/// or None if such a node cannot be found.
///
/// \param FoundMemberLevelDecl Returns member-level declaration (i.e., the
///  child of a type DeclContext) for which an @available attribute would
/// fix the unavailable reference.
///
/// \param FoundTypeLevelDecl returns a type-level declaration (a
/// a nominal type, an extension, or a global function) for which an
/// @available attribute would fix the unavailable reference.
static void findAvailabilityFixItNodes(SourceRange ReferenceRange,
                                       const DeclContext *ReferenceDC,
                                       const SourceManager &SM,
                                       Optional<ASTNode> &FoundVersionCheckNode,
                                       const Decl *&FoundMemberLevelDecl,
                                       const Decl *&FoundTypeLevelDecl) {
  FoundVersionCheckNode = None;
  FoundMemberLevelDecl = nullptr;
  FoundTypeLevelDecl = nullptr;

  // Limit tree to search based on the DeclContext of the reference.
  const Decl *DeclarationToSearch =
      findContainingDeclaration(ReferenceRange, ReferenceDC, SM);
  if (!DeclarationToSearch)
    return;

  // Const-cast to inject into ASTNode. This search will not modify
  // the declaration.
  ASTNode SearchRoot = const_cast<Decl *>(DeclarationToSearch);

  // The node to wrap in if #available(...) { ... } is the innermost node in
  // SearchRoot that (1) can be guarded with an if statement and (2)
  // contains the ReferenceRange.
  // We make no guarantee that the Fix-It, when applied, will result in
  // semantically valid code -- but, at a minimum, it should parse. So,
  // for example, we may suggest wrapping a variable declaration in a guard,
  // which would not be valid if the variable is later used. The goal
  // is discoverability of #os() (via the diagnostic and Fix-It) rather than
  // magically fixing the code in all cases.

  InnermostAncestorFinder::MatchPredicate IsGuardable =
      [](ASTNode Node, ASTWalker::ParentTy Parent) {
        if (Expr *ParentExpr = Parent.getAsExpr()) {
          auto *ParentClosure = dyn_cast<ClosureExpr>(ParentExpr);
          if (!ParentClosure || !ParentClosure->hasSingleExpressionBody()) {
            return false;
          }
        } else if (auto *ParentStmt = Parent.getAsStmt()) {
          if (!isa<BraceStmt>(ParentStmt)) {
            return false;
          }
        } else {
          return false;
        }

        return true;
      };

  FoundVersionCheckNode =
      findInnermostAncestor(ReferenceRange, SM, SearchRoot, IsGuardable);

  // Find some Decl that contains the reference range. We use this declaration
  // as a starting place to climb the DeclContext hierarchy to find
  // places to suggest adding @available() annotations.
  InnermostAncestorFinder::MatchPredicate IsDeclaration = [](
      ASTNode Node, ASTWalker::ParentTy Parent) { return Node.is<Decl *>(); };

  Optional<ASTNode> FoundDeclarationNode =
      findInnermostAncestor(ReferenceRange, SM, SearchRoot, IsDeclaration);

  const Decl *ContainingDecl = nullptr;
  if (FoundDeclarationNode.hasValue()) {
    ContainingDecl = FoundDeclarationNode.getValue().get<Decl *>();
  }

  if (!ContainingDecl) {
    ContainingDecl = ReferenceDC->getInnermostMethodContext();
  }

  // Try to find declarations on which @available attributes can be added.
  // The heuristics for finding these declarations are biased towards deeper
  // nodes in the AST to limit the scope of suggested availability regions
  // and provide a better IDE experience (it can get jumpy if Fix-It locations
  // are far away from the error needing the Fix-It).
  if (ContainingDecl) {
    FoundMemberLevelDecl =
        ancestorMemberLevelDeclForAvailabilityFixit(ContainingDecl);

    FoundTypeLevelDecl =
        ancestorTypeLevelDeclForAvailabilityFixit(ContainingDecl);
  }
}

/// Emit a diagnostic note and Fix-It to add an @available attribute
/// on the given declaration for the given version range.
static void fixAvailabilityForDecl(SourceRange ReferenceRange, const Decl *D,
                                   const VersionRange &RequiredRange,
                                   TypeChecker &TC) {
  assert(D);

  // Don't suggest adding an @available() to a declaration where we would
  // emit a diagnostic saying it is not allowed.
  if (TC.diagnosticIfDeclCannotBePotentiallyUnavailable(D).hasValue())
    return;

  if (getActiveAvailableAttribute(D, TC.Context)) {
    // For QoI, in future should emit a fixit to update the existing attribute.
    return;
  }

  // For some declarations (variables, enum elements), the location in concrete
  // syntax to suggest the Fix-It may differ from the declaration to which
  // we attach availability attributes in the abstract syntax tree during
  // parsing.
  const Decl *ConcDecl = concreteSyntaxDeclForAvailableAttribute(D);

  DescriptiveDeclKind KindForDiagnostic = ConcDecl->getDescriptiveKind();
  SourceLoc InsertLoc;

  // To avoid exposing the pattern binding declaration to the user, get the
  // descriptive kind from one of the VarDecls. We get the Fix-It location
  // from the PatternBindingDecl unless the VarDecl has attributes,
  // in which case we get the start location of the VarDecl attributes.
  DeclAttributes AttrsForLoc;
  if (KindForDiagnostic == DescriptiveDeclKind::PatternBinding) {
    KindForDiagnostic = D->getDescriptiveKind();
    AttrsForLoc = D->getAttrs();
  } else {
    InsertLoc = ConcDecl->getAttrs().getStartLoc(/*forModifiers=*/false);
  }

  InsertLoc = D->getAttrs().getStartLoc(/*forModifiers=*/false);
  if (InsertLoc.isInvalid()) {
    InsertLoc = ConcDecl->getStartLoc();
  }

  if (InsertLoc.isInvalid())
    return;

  StringRef OriginalIndent =
      Lexer::getIndentationForLine(TC.Context.SourceMgr, InsertLoc);

  std::string AttrText;
  {
    llvm::raw_string_ostream Out(AttrText);

    PlatformKind Target = targetPlatform(TC.getLangOpts());
    Out << "@available(" << platformString(Target) << " "
        << RequiredRange.getLowerEndpoint().getAsString() << ", *)\n"
        << OriginalIndent;
  }

  TC.diagnose(D, diag::availability_add_attribute,
              KindForDiagnostic)
      .fixItInsert(InsertLoc, AttrText);
}

/// In the special case of being in an existing, nontrivial type refinement
/// context that's close but not quite narrow enough to satisfy requirements
/// (i.e.  requirements are contained-in the existing TRC but off by a subminor
/// version), emit a diagnostic and fixit that narrows the existing TRC
/// condition to the required range.
static bool fixAvailabilityByNarrowingNearbyVersionCheck(
    SourceRange ReferenceRange,
    const DeclContext *ReferenceDC,
    const VersionRange &RequiredRange,
    TypeChecker &TC,
    InFlightDiagnostic &Err) {
  const TypeRefinementContext *TRC = nullptr;
  AvailabilityContext RunningOSOverApprox =
    TC.overApproximateAvailabilityAtLocation(ReferenceRange.Start,
                                             ReferenceDC, &TRC);
  VersionRange RunningRange = RunningOSOverApprox.getOSVersion();
  if (RunningRange.hasLowerEndpoint() &&
      RequiredRange.hasLowerEndpoint() &&
      AvailabilityContext(RequiredRange).isContainedIn(RunningOSOverApprox) &&
      TRC && TRC->getReason() != TypeRefinementContext::Reason::Root) {

    // Only fix situations that are "nearby" versions, meaning
    // disagreement on a minor-or-less version for non-macOS,
    // or disagreement on a subminor-or-less version for macOS.
    auto RunningVers = RunningRange.getLowerEndpoint();
    auto RequiredVers = RequiredRange.getLowerEndpoint();
    auto Platform = targetPlatform(TC.Context.LangOpts);
    if (RunningVers.getMajor() != RequiredVers.getMajor())
      return false;
    if ((Platform == PlatformKind::OSX ||
         Platform == PlatformKind::OSXApplicationExtension) &&
        !(RunningVers.getMinor().hasValue() &&
          RequiredVers.getMinor().hasValue() &&
          RunningVers.getMinor().getValue() ==
          RequiredVers.getMinor().getValue()))
      return false;

    auto FixRange = TRC->getAvailabilityConditionVersionSourceRange(
      Platform, RunningVers);
    if (!FixRange.isValid())
      return false;
    // Have found a nontrivial type refinement context-introducer to narrow.
    Err.fixItReplace(FixRange, RequiredVers.getAsString());
    return true;
  }
  return false;
}

/// Emit a diagnostic note and Fix-It to add an if #available(...) { } guard
/// that checks for the given version range around the given node.
static void fixAvailabilityByAddingVersionCheck(
    ASTNode NodeToWrap, const VersionRange &RequiredRange,
    SourceRange ReferenceRange, TypeChecker &TC) {
  SourceRange RangeToWrap = NodeToWrap.getSourceRange();
  if (RangeToWrap.isInvalid())
    return;

  SourceLoc ReplaceLocStart = RangeToWrap.Start;
  StringRef ExtraIndent;
  StringRef OriginalIndent = Lexer::getIndentationForLine(
      TC.Context.SourceMgr, ReplaceLocStart, &ExtraIndent);

  std::string IfText;
  {
    llvm::raw_string_ostream Out(IfText);

    SourceLoc ReplaceLocEnd =
        Lexer::getLocForEndOfToken(TC.Context.SourceMgr, RangeToWrap.End);

    std::string GuardedText =
        TC.Context.SourceMgr.extractText(CharSourceRange(TC.Context.SourceMgr,
                                                         ReplaceLocStart,
                                                         ReplaceLocEnd)).str();

    std::string NewLine = "\n";
    std::string NewLineReplacement = (NewLine + ExtraIndent).str();

    // Indent the body of the Fix-It if. Because the body may be a compound
    // statement, we may have to indent multiple lines.
    size_t StartAt = 0;
    while ((StartAt = GuardedText.find(NewLine, StartAt)) !=
           std::string::npos) {
      GuardedText.replace(StartAt, NewLine.length(), NewLineReplacement);
      StartAt += NewLine.length();
    }

    PlatformKind Target = targetPlatform(TC.getLangOpts());

    Out << "if #available(" << platformString(Target)
        << " " << RequiredRange.getLowerEndpoint().getAsString()
        << ", *) {\n";

    Out << OriginalIndent << ExtraIndent << GuardedText << "\n";

    // We emit an empty fallback case with a comment to encourage the developer
    // to think explicitly about whether fallback on earlier versions is needed.
    Out << OriginalIndent << "} else {\n";
    Out << OriginalIndent << ExtraIndent << "// Fallback on earlier versions\n";
    Out << OriginalIndent << "}";
  }

  TC.diagnose(ReferenceRange.Start, diag::availability_guard_with_version_check)
      .fixItReplace(RangeToWrap, IfText);
}

/// Emit suggested Fix-Its for a reference with to an unavailable symbol
/// requiting the given OS version range.
static void fixAvailability(SourceRange ReferenceRange,
                            const DeclContext *ReferenceDC,
                            const VersionRange &RequiredRange,
                            TypeChecker &TC) {
  if (ReferenceRange.isInvalid())
    return;

  Optional<ASTNode> NodeToWrapInVersionCheck;
  const Decl *FoundMemberDecl = nullptr;
  const Decl *FoundTypeLevelDecl = nullptr;

  findAvailabilityFixItNodes(ReferenceRange, ReferenceDC, TC.Context.SourceMgr,
                             NodeToWrapInVersionCheck, FoundMemberDecl,
                             FoundTypeLevelDecl);

  // Suggest wrapping in if #available(...) { ... } if possible.
  if (NodeToWrapInVersionCheck.hasValue()) {
    fixAvailabilityByAddingVersionCheck(NodeToWrapInVersionCheck.getValue(),
                                        RequiredRange, ReferenceRange, TC);
  }

  // Suggest adding availability attributes.
  if (FoundMemberDecl) {
    fixAvailabilityForDecl(ReferenceRange, FoundMemberDecl, RequiredRange, TC);
  }

  if (FoundTypeLevelDecl) {
    fixAvailabilityForDecl(ReferenceRange, FoundTypeLevelDecl, RequiredRange,
                           TC);
  }
}

void TypeChecker::diagnosePotentialUnavailability(
    const Decl *D, DeclName Name, SourceRange ReferenceRange,
    const DeclContext *ReferenceDC, const UnavailabilityReason &Reason) {

  // We only emit diagnostics for API unavailability, not for explicitly
  // weak-linked symbols.
  if (Reason.getReasonKind() !=
      UnavailabilityReason::Kind::RequiresOSVersionRange) {
    return;
  }

  auto RequiredRange = Reason.getRequiredOSVersionRange();
  {
    auto Err =
      diagnose(ReferenceRange.Start, diag::availability_decl_only_version_newer,
               Name, prettyPlatformString(targetPlatform(Context.LangOpts)),
               Reason.getRequiredOSVersionRange().getLowerEndpoint());

    // Direct a fixit to the error if an existing guard is nearly-correct
    if (fixAvailabilityByNarrowingNearbyVersionCheck(ReferenceRange,
                                                     ReferenceDC,
                                                     RequiredRange, *this, Err))
      return;
  }

  fixAvailability(ReferenceRange, ReferenceDC, RequiredRange, *this);
}

void TypeChecker::diagnosePotentialAccessorUnavailability(
    AccessorDecl *Accessor, SourceRange ReferenceRange,
    const DeclContext *ReferenceDC, const UnavailabilityReason &Reason,
    bool ForInout) {
  assert(Accessor->isGetterOrSetter());

  AbstractStorageDecl *ASD = Accessor->getStorage();
  DeclName Name = ASD->getFullName();

  auto &diag = ForInout ? diag::availability_inout_accessor_only_version_newer
                        : diag::availability_accessor_only_version_newer;

  auto RequiredRange = Reason.getRequiredOSVersionRange();
  {
    auto Err =
      diagnose(ReferenceRange.Start, diag,
               static_cast<unsigned>(Accessor->getAccessorKind()), Name,
               prettyPlatformString(targetPlatform(Context.LangOpts)),
               Reason.getRequiredOSVersionRange().getLowerEndpoint());


    // Direct a fixit to the error if an existing guard is nearly-correct
    if (fixAvailabilityByNarrowingNearbyVersionCheck(ReferenceRange,
                                                     ReferenceDC,
                                                     RequiredRange, *this, Err))
      return;
  }

  fixAvailability(ReferenceRange, ReferenceDC, RequiredRange, *this);
}

const AvailableAttr *TypeChecker::getDeprecated(const Decl *D) {
  if (auto *Attr = D->getAttrs().getDeprecated(D->getASTContext()))
    return Attr;

  // Treat extensions methods as deprecated if their extension
  // is deprecated.
  DeclContext *DC = D->getDeclContext();
  if (auto *ED = dyn_cast<ExtensionDecl>(DC)) {
    return getDeprecated(ED);
  }

  return nullptr;
}

/// Returns true if some declaration lexically enclosing the reference
/// matches the passed in predicate and false otherwise.
static bool
someEnclosingDeclMatches(SourceRange ReferenceRange,
                         const DeclContext *ReferenceDC,
                         llvm::function_ref<bool(const Decl *)> Pred) {
  // Climb the DeclContext hierarchy to see if any of the containing
  // declarations matches the predicate.
  const DeclContext *DC = ReferenceDC;
  do {
    auto *D = DC->getInnermostDeclarationDeclContext();
    if (!D)
      break;

    if (Pred(D)) {
      return true;
    }

    // If we are in an accessor, check to see if the associated
    // property matches the predicate.
    if (auto accessor = dyn_cast<AccessorDecl>(D)) {
      if (Pred(accessor->getStorage()))
        return true;
    }

    DC = DC->getParent();
  } while (DC);

  // Search the AST starting from our innermost declaration context to see if
  // if the reference is inside a property declaration but not inside an
  // accessor (this can happen for the TypeRepr for the declared type of a
  // property, for example).
  // We can't rely on the DeclContext hierarchy climb above because properties
  // do not introduce a new DeclContext. This search is potentially slow, so we
  // do it last and only if the reference declaration context is a
  // type or global context.

  if (!ReferenceDC->isTypeContext() && !ReferenceDC->isModuleScopeContext())
    return false;

  // Don't search for a containing declaration if we don't have a source range.
  if (ReferenceRange.isInvalid())
    return false;

  ASTContext &Ctx = ReferenceDC->getASTContext();
  const Decl *DeclToSearch =
      findContainingDeclaration(ReferenceRange, ReferenceDC, Ctx.SourceMgr);

  // We may not be able to find a declaration to search if the ReferenceRange
  // is invalid (i.e., we are in synthesized code).
  if (!DeclToSearch)
    return false;

  InnermostAncestorFinder::MatchPredicate IsDeclaration =
      [](ASTNode Node, ASTWalker::ParentTy Parent) {
        return Node.is<Decl *>();
  };

  Optional<ASTNode> FoundDeclarationNode =
      findInnermostAncestor(ReferenceRange, Ctx.SourceMgr,
                            const_cast<Decl *>(DeclToSearch), IsDeclaration);

  if (FoundDeclarationNode.hasValue()) {
    const Decl *D = FoundDeclarationNode.getValue().get<Decl *>();
    D = abstractSyntaxDeclForAvailableAttribute(D);
    if (Pred(D)) {
      return true;
    }
  }

  return false;
}

/// Returns true if the reference or any of its parents is an
/// implicit function.
static bool isInsideImplicitFunction(SourceRange ReferenceRange,
                                     const DeclContext *DC) {
  auto IsInsideImplicitFunc = [](const Decl *D) {
    auto *AFD = dyn_cast<AbstractFunctionDecl>(D);
    return AFD && AFD->isImplicit();
  };

  return someEnclosingDeclMatches(ReferenceRange, DC, IsInsideImplicitFunc);
}

/// Returns true if the reference or any of its parents is an
/// unavailable (or obsoleted) declaration.
static bool isInsideUnavailableDeclaration(SourceRange ReferenceRange,
                                           const DeclContext *ReferenceDC) {
  auto IsUnavailable = [](const Decl *D) {
    return D->getAttrs().getUnavailable(D->getASTContext());
  };

  return someEnclosingDeclMatches(ReferenceRange, ReferenceDC, IsUnavailable);
}

/// Returns true if the reference or any of its parents is an
/// unconditional unavailable declaration for the same platform.
static bool isInsideCompatibleUnavailableDeclaration(
    SourceRange ReferenceRange, const DeclContext *ReferenceDC,
    const AvailableAttr *attr) {
  if (!attr->isUnconditionallyUnavailable()) {
    return false;
  }
  PlatformKind platform = attr->Platform;
  if (platform == PlatformKind::none) {
    return false;
  }

  auto IsUnavailable = [platform](const Decl *D) {
    auto EnclosingUnavailable =
        D->getAttrs().getUnavailable(D->getASTContext());
    return EnclosingUnavailable && EnclosingUnavailable->Platform == platform;
  };

  return someEnclosingDeclMatches(ReferenceRange, ReferenceDC, IsUnavailable);
}

/// Returns true if the reference is lexically contained in a declaration
/// that is deprecated on all deployment targets.
static bool isInsideDeprecatedDeclaration(SourceRange ReferenceRange,
                                          const DeclContext *ReferenceDC){
  auto IsDeprecated = [](const Decl *D) {
    return D->getAttrs().getDeprecated(D->getASTContext());
  };

  return someEnclosingDeclMatches(ReferenceRange, ReferenceDC, IsDeprecated);
}

static void fixItAvailableAttrRename(InFlightDiagnostic &diag,
                                     SourceRange referenceRange,
                                     const ValueDecl *renamedDecl,
                                     const AvailableAttr *attr,
                                     const ApplyExpr *call) {
  if (isa<AccessorDecl>(renamedDecl))
    return;

  ParsedDeclName parsed = swift::parseDeclName(attr->Rename);
  if (!parsed)
    return;

  bool originallyWasKnownOperatorExpr = false;
  if (call) {
    originallyWasKnownOperatorExpr =
        isa<BinaryExpr>(call) ||
        isa<PrefixUnaryExpr>(call) ||
        isa<PostfixUnaryExpr>(call);
  }
  if (parsed.isOperator() != originallyWasKnownOperatorExpr)
    return;

  auto &ctx = renamedDecl->getASTContext();
  SourceManager &sourceMgr = ctx.SourceMgr;

  if (parsed.isInstanceMember()) {
    // Replace the base of the call with the "self argument".
    // We can only do a good job with the fix-it if we have the whole call
    // expression.
    // FIXME: Should we be validating the ContextName in some way?
    if (!call || !isa<CallExpr>(call))
      return;

    unsigned selfIndex = parsed.SelfIndex.getValue();
    const Expr *selfExpr = nullptr;
    SourceLoc removeRangeStart;
    SourceLoc removeRangeEnd;

    const Expr *argExpr = call->getArg();
    if (auto args = dyn_cast<TupleExpr>(argExpr)) {
      size_t numElementsWithinParens = args->getNumElements();
      numElementsWithinParens -= args->hasTrailingClosure();
      if (selfIndex >= numElementsWithinParens)
        return;

      if (parsed.IsGetter) {
        if (numElementsWithinParens != 1)
          return;
      } else if (parsed.IsSetter) {
        if (numElementsWithinParens != 2)
          return;
      } else {
        if (parsed.ArgumentLabels.size() != args->getNumElements() - 1)
          return;
      }

      selfExpr = args->getElement(selfIndex);

      if (selfIndex + 1 == numElementsWithinParens) {
        if (selfIndex > 0) {
          // Remove from the previous comma to the close-paren (half-open).
          removeRangeStart = args->getElement(selfIndex-1)->getEndLoc();
          removeRangeStart = Lexer::getLocForEndOfToken(sourceMgr,
                                                        removeRangeStart);
        } else {
          // Remove from after the open paren to the close paren (half-open).
          removeRangeStart = Lexer::getLocForEndOfToken(sourceMgr,
                                                        argExpr->getStartLoc());
        }

        // Prefer the r-paren location, so that we get the right behavior when
        // there's a trailing closure, but handle some implicit cases too.
        removeRangeEnd = args->getRParenLoc();
        if (removeRangeEnd.isInvalid())
          removeRangeEnd = args->getEndLoc();

      } else {
        // Remove from the label to the start of the next argument (half-open).
        SourceLoc labelLoc = args->getElementNameLoc(selfIndex);
        if (labelLoc.isValid())
          removeRangeStart = labelLoc;
        else
          removeRangeStart = selfExpr->getStartLoc();

        SourceLoc nextLabelLoc = args->getElementNameLoc(selfIndex + 1);
        if (nextLabelLoc.isValid())
          removeRangeEnd = nextLabelLoc;
        else
          removeRangeEnd = args->getElement(selfIndex + 1)->getStartLoc();
      }

      // Avoid later argument label fix-its for this argument.
      if (!parsed.isPropertyAccessor()) {
        Identifier oldLabel = args->getElementName(selfIndex);
        StringRef oldLabelStr;
        if (!oldLabel.empty())
          oldLabelStr = oldLabel.str();
        parsed.ArgumentLabels.insert(parsed.ArgumentLabels.begin() + selfIndex,
                                     oldLabelStr);
      }

    } else {
      if (selfIndex != 0 || !parsed.ArgumentLabels.empty())
        return;
      selfExpr = cast<ParenExpr>(argExpr)->getSubExpr();
      // Remove from after the open paren to the close paren (half-open).
      removeRangeStart = Lexer::getLocForEndOfToken(sourceMgr,
                                                    argExpr->getStartLoc());
      removeRangeEnd = argExpr->getEndLoc();
    }

    if (auto *inoutSelf = dyn_cast<InOutExpr>(selfExpr))
      selfExpr = inoutSelf->getSubExpr();

    CharSourceRange selfExprRange =
        Lexer::getCharSourceRangeFromSourceRange(sourceMgr,
                                                 selfExpr->getSourceRange());
    bool needsParens = !selfExpr->canAppendPostfixExpression();

    SmallString<64> selfReplace;
    if (needsParens)
      selfReplace.push_back('(');

    // If the base is contextual member lookup and we know the type,
    // let's just prepend it, otherwise we'll end up with an incorrect fix-it.
    auto base = sourceMgr.extractText(selfExprRange);
    if (!base.empty() && base.front() == '.') {
      auto newName = attr->Rename;
      // If this is not a rename, let's not
      // even try to emit a fix-it because
      // it's going to be invalid.
      if (newName.empty())
        return;

      auto parts = newName.split('.');
      auto nominalName = parts.first;
      assert(!nominalName.empty());

      selfReplace += nominalName;
    }

    selfReplace += base;
    if (needsParens)
      selfReplace.push_back(')');
    selfReplace.push_back('.');
    selfReplace += parsed.BaseName;
    diag.fixItReplace(call->getFn()->getSourceRange(), selfReplace);

    if (!parsed.isPropertyAccessor())
      diag.fixItRemoveChars(removeRangeStart, removeRangeEnd);

    // Continue on to diagnose any argument label renames.

  } else if (parsed.BaseName == "init" &&
             call && isa<CallExpr>(call)) {
    // For initializers, replace with a "call" of the context type...but only
    // if we know we're doing a call (rather than a first-class reference).
    if (parsed.isMember()) {
      diag.fixItReplace(call->getFn()->getSourceRange(), parsed.ContextName);

    } else if (auto *dotCall = dyn_cast<DotSyntaxCallExpr>(call->getFn())) {
      SourceLoc removeLoc = dotCall->getDotLoc();
      if (removeLoc.isInvalid())
        return;

      diag.fixItRemove(SourceRange(removeLoc, dotCall->getFn()->getEndLoc()));
    } else if (!isa<ConstructorRefCallExpr>(call->getFn())) {
      return;
    }

    // Continue on to diagnose any constructor argument label renames.
    
  } else {
    // Just replace the base name.
    SmallString<64> baseReplace;
    if (!parsed.ContextName.empty()) {
      baseReplace += parsed.ContextName;
      baseReplace += '.';
    }
    baseReplace += parsed.BaseName;
    if (parsed.IsFunctionName && parsed.ArgumentLabels.empty() &&
        isa<VarDecl>(renamedDecl)) {
      // If we're going from a var to a function with no arguments, emit an
      // empty parameter list.
      baseReplace += "()";
    }
    diag.fixItReplace(referenceRange, baseReplace);
  }

  if (!call || !isa<CallExpr>(call))
    return;

  const Expr *argExpr = call->getArg();
  if (parsed.IsGetter) {
    diag.fixItRemove(argExpr->getSourceRange());
    return;
  }

  if (parsed.IsSetter) {
    const Expr *newValueExpr = nullptr;

    if (auto args = dyn_cast<TupleExpr>(argExpr)) {
      size_t newValueIndex = 0;
      if (parsed.isInstanceMember()) {
        assert(parsed.SelfIndex.getValue() == 0 ||
               parsed.SelfIndex.getValue() == 1);
        newValueIndex = !parsed.SelfIndex.getValue();
      }
      newValueExpr = args->getElement(newValueIndex);
    } else {
      newValueExpr = cast<ParenExpr>(argExpr)->getSubExpr();
    }

    diag.fixItReplaceChars(argExpr->getStartLoc(), newValueExpr->getStartLoc(),
                           " = ");
    diag.fixItRemoveChars(Lexer::getLocForEndOfToken(sourceMgr,
                                                     newValueExpr->getEndLoc()),
                          Lexer::getLocForEndOfToken(sourceMgr,
                                                     argExpr->getEndLoc()));
    return;
  }

  if (!parsed.IsFunctionName)
    return;

  SmallVector<Identifier, 4> argumentLabelIDs;
  std::transform(parsed.ArgumentLabels.begin(), parsed.ArgumentLabels.end(),
                 std::back_inserter(argumentLabelIDs),
                 [&ctx](StringRef labelStr) -> Identifier {
    return labelStr.empty() ? Identifier() : ctx.getIdentifier(labelStr);
  });

  if (auto args = dyn_cast<TupleShuffleExpr>(argExpr)) {
    argExpr = args->getSubExpr();

    // Coerce the `argumentLabelIDs` to the user supplied arguments.
    // e.g:
    //   @available(.., renamed: "new(w:x:y:z:)")
    //   func old(a: Int, b: Int..., c: String="", d: Int=0){}
    //   old(a: 1, b: 2, 3, 4, d: 5)
    // coerce
    //   argumentLabelIDs = {"w", "x", "y", "z"}
    // to
    //   argumentLabelIDs = {"w", "x", "", "", "z"}
    auto elementMap = args->getElementMapping();
    if (elementMap.size() != argumentLabelIDs.size()) {
      // Mismatched lengths; give up.
      return;
    }
    auto I = argumentLabelIDs.begin();
    for (auto shuffleIdx : elementMap) {
      switch (shuffleIdx) {
      case TupleShuffleExpr::DefaultInitialize:
      case TupleShuffleExpr::CallerDefaultInitialize:
        // Defaulted: remove param label of it.
        I = argumentLabelIDs.erase(I);
        break;
      case TupleShuffleExpr::Variadic: {
        auto variadicArgsNum = args->getVariadicArgs().size();
        if (variadicArgsNum == 0) {
          // No arguments: Remove param label of it.
          I = argumentLabelIDs.erase(I);
        } else if (variadicArgsNum == 1) {
          // One argument: Just advance.
          ++I;
        } else {
          // Two or more arguments: Insert empty labels after the first one.
          I = argumentLabelIDs.insert(++I, --variadicArgsNum, Identifier());
          I += variadicArgsNum;
        }
        break;
      }
      default:
        // Normal: Just advance.
        assert(shuffleIdx == (I - argumentLabelIDs.begin()) &&
               "SE-0060 guarantee");
        ++I;
        break;
      }
    }
  }

  if (auto args = dyn_cast<TupleExpr>(argExpr)) {
    if (argumentLabelIDs.size() != args->getNumElements()) {
      // Mismatched lengths; give up.
      return;
    }

    auto argumentLabelsToCheck = llvm::makeArrayRef(argumentLabelIDs);
    // The argument label for a trailing closure is ignored.
    if (args->hasTrailingClosure())
      argumentLabelsToCheck = argumentLabelsToCheck.drop_back();

    if (args->hasElementNames()) {
      if (std::equal(argumentLabelsToCheck.begin(), argumentLabelsToCheck.end(),
                     args->getElementNames().begin())) {
        // Already matching.
        return;
      }

    } else {
      if (std::all_of(argumentLabelsToCheck.begin(),argumentLabelsToCheck.end(),
                      std::mem_fn(&Identifier::empty))) {
        // Already matching (as in, there are no labels).
        return;
      }
    }

  } else if (auto args = dyn_cast<ParenExpr>(argExpr)) {
    if (args->hasTrailingClosure()) {
      // The argument label for a trailing closure is ignored.
      return;
    }

    if (argumentLabelIDs.size() != 1) {
      // Mismatched lengths; give up.
      return;
    }

    if (argumentLabelIDs.front().empty()) {
      // Already matching (no labels).
      return;
    }
  } else {
    llvm_unreachable("Unexpected arg expression");
  }

  diagnoseArgumentLabelError(ctx, argExpr, argumentLabelIDs, false, &diag);
}

// Must be kept in sync with diag::availability_decl_unavailable_rename and
// others.
namespace {
  enum class ReplacementDeclKind : unsigned {
    None,
    InstanceMethod,
    Property,
  };
} // end anonymous namespace

static Optional<ReplacementDeclKind>
describeRename(ASTContext &ctx, const AvailableAttr *attr, const ValueDecl *D,
               SmallVectorImpl<char> &nameBuf) {
  ParsedDeclName parsed = swift::parseDeclName(attr->Rename);
  if (!parsed)
    return None;

  // Only produce special descriptions for renames to
  // - instance members
  // - properties (or global bindings)
  // - class/static methods
  // - initializers, unless the original was known to be an initializer
  // Leave non-member renames alone, as well as renames from top-level types
  // and bindings to member types and class/static properties.
  if (!(parsed.isInstanceMember() || parsed.isPropertyAccessor() ||
        (parsed.isMember() && parsed.IsFunctionName) ||
        (parsed.BaseName == "init" &&
         !dyn_cast_or_null<ConstructorDecl>(D)))) {
    return None;
  }

  llvm::raw_svector_ostream name(nameBuf);

  if (!parsed.ContextName.empty())
    name << parsed.ContextName << '.';

  if (parsed.IsFunctionName) {
    name << parsed.formDeclName(ctx);
  } else {
    name << parsed.BaseName;
  }

  if (parsed.isMember() && parsed.isPropertyAccessor())
    return ReplacementDeclKind::Property;
  if (parsed.isInstanceMember() && parsed.IsFunctionName)
    return ReplacementDeclKind::InstanceMethod;

  // We don't have enough information.
  return ReplacementDeclKind::None;
}

/// Returns a value that can be used to select between accessor kinds in
/// diagnostics.
///
/// This is correlated with diag::availability_deprecated and others.
static std::pair<unsigned, DeclName>
getAccessorKindAndNameForDiagnostics(const ValueDecl *D) {
  // This should always be one more than the last AccessorKind supported in
  // the diagnostics. If you need to change it, change the assertion below as
  // well.
  static const unsigned NOT_ACCESSOR_INDEX = 2;

  if (auto *accessor = dyn_cast<AccessorDecl>(D)) {
    DeclName Name = accessor->getStorage()->getFullName();
    assert(accessor->isGetterOrSetter());
    return {static_cast<unsigned>(accessor->getAccessorKind()), Name};
  }

  return {NOT_ACCESSOR_INDEX, D->getFullName()};
}

void TypeChecker::diagnoseIfDeprecated(SourceRange ReferenceRange,
                                       const DeclContext *ReferenceDC,
                                       const ValueDecl *DeprecatedDecl,
                                       const ApplyExpr *Call) {
  const AvailableAttr *Attr = TypeChecker::getDeprecated(DeprecatedDecl);
  if (!Attr)
    return;

  // We don't want deprecated declarations to trigger warnings
  // in synthesized code.
  if (ReferenceRange.isInvalid() &&
      isInsideImplicitFunction(ReferenceRange, ReferenceDC)) {
    return;
  }
  // We match the behavior of clang to not report deprecation warnings
  // inside declarations that are themselves deprecated on all deployment
  // targets.
  if (isInsideDeprecatedDeclaration(ReferenceRange, ReferenceDC)) {
    return;
  }

  if (!Context.LangOpts.DisableAvailabilityChecking) {
    AvailabilityContext RunningOSVersions =
        overApproximateAvailabilityAtLocation(ReferenceRange.Start,ReferenceDC);
    if (RunningOSVersions.isKnownUnreachable()) {
      // Suppress a deprecation warning if the availability checking machinery
      // thinks the reference program location will not execute on any
      // deployment target for the current platform.
      return;
    }
  }

  DeclName Name;
  unsigned RawAccessorKind;
  std::tie(RawAccessorKind, Name) =
      getAccessorKindAndNameForDiagnostics(DeprecatedDecl);

  StringRef Platform = Attr->prettyPlatformString();
  llvm::VersionTuple DeprecatedVersion;
  if (Attr->Deprecated)
    DeprecatedVersion = Attr->Deprecated.getValue();

  if (Attr->Message.empty() && Attr->Rename.empty()) {
    diagnose(ReferenceRange.Start, diag::availability_deprecated,
             RawAccessorKind, Name, Attr->hasPlatform(), Platform,
             Attr->Deprecated.hasValue(), DeprecatedVersion)
      .highlight(Attr->getRange());
    return;
  }

  SmallString<32> newNameBuf;
  Optional<ReplacementDeclKind> replacementDeclKind =
    describeRename(Context, Attr, /*decl*/nullptr, newNameBuf);
  StringRef newName = replacementDeclKind ? newNameBuf.str() : Attr->Rename;

  if (!Attr->Message.empty()) {
    EncodedDiagnosticMessage EncodedMessage(Attr->Message);
    diagnose(ReferenceRange.Start, diag::availability_deprecated_msg,
             RawAccessorKind, Name, Attr->hasPlatform(), Platform,
             Attr->Deprecated.hasValue(), DeprecatedVersion,
             EncodedMessage.Message)
      .highlight(Attr->getRange());
  } else {
    unsigned rawReplaceKind = static_cast<unsigned>(
        replacementDeclKind.getValueOr(ReplacementDeclKind::None));
    diagnose(ReferenceRange.Start, diag::availability_deprecated_rename,
             RawAccessorKind, Name, Attr->hasPlatform(), Platform,
             Attr->Deprecated.hasValue(), DeprecatedVersion,
             replacementDeclKind.hasValue(), rawReplaceKind, newName)
      .highlight(Attr->getRange());
  }

  if (!Attr->Rename.empty() && !isa<AccessorDecl>(DeprecatedDecl)) {
    auto renameDiag = diagnose(ReferenceRange.Start,
                               diag::note_deprecated_rename,
                               newName);
    fixItAvailableAttrRename(renameDiag, ReferenceRange, DeprecatedDecl,
                             Attr, Call);
  }
}


void swift::diagnoseUnavailableOverride(ValueDecl *override,
                                        const ValueDecl *base,
                                        const AvailableAttr *attr) {
  ASTContext &ctx = override->getASTContext();
  auto &diags = ctx.Diags;
  if (attr->Rename.empty()) {
    if (attr->Message.empty())
      diags.diagnose(override, diag::override_unavailable,
                     override->getBaseName());
    else
      diags.diagnose(override, diag::override_unavailable_msg,
                     override->getBaseName(), attr->Message);

    DeclName name;
    unsigned rawAccessorKind;
    std::tie(rawAccessorKind, name) =
        getAccessorKindAndNameForDiagnostics(base);
    diags.diagnose(base, diag::availability_marked_unavailable,
                   rawAccessorKind, name);
    return;
  }

  diagnoseExplicitUnavailability(base, override->getLoc(),
                                 override->getDeclContext(),
                                 [&](InFlightDiagnostic &diag) {
    ParsedDeclName parsedName = parseDeclName(attr->Rename);
    if (!parsedName || parsedName.isPropertyAccessor() ||
        parsedName.isMember() || parsedName.isOperator()) {
      return;
    }

    // Only initializers should be named 'init'.
    if (isa<ConstructorDecl>(override) ^
        (parsedName.BaseName == "init")) {
      return;
    }

    if (!parsedName.IsFunctionName) {
      diag.fixItReplace(override->getNameLoc(), parsedName.BaseName);
      return;
    }

    DeclName newName = parsedName.formDeclName(ctx);
    size_t numArgs = override->getFullName().getArgumentNames().size();
    if (!newName || newName.getArgumentNames().size() != numArgs)
      return;

    fixDeclarationName(diag, override, newName);
  });
}

/// Emit a diagnostic for references to declarations that have been
/// marked as unavailable, either through "unavailable" or "obsoleted:".
bool swift::diagnoseExplicitUnavailability(const ValueDecl *D,
                                                 SourceRange R,
                                                 const DeclContext *DC,
                                                 const ApplyExpr *call) {
  return diagnoseExplicitUnavailability(D, R, DC,
                                        [=](InFlightDiagnostic &diag) {
    fixItAvailableAttrRename(diag, R, D, AvailableAttr::isUnavailable(D),
                             call);
  });
}

/// Check if this is a subscript declaration inside String or
/// Substring that returns String, and if so return true.
bool isSubscriptReturningString(const ValueDecl *D, ASTContext &Context) {
  // Is this a subscript?
  if (!isa<SubscriptDecl>(D))
    return false;

  // Is the subscript declared in String or Substring?
  auto *declContext = D->getDeclContext();
  assert(declContext && "Expected decl context!");

  auto *stringDecl = Context.getStringDecl();
  auto *substringDecl = Context.getSubstringDecl();

  auto *typeDecl = declContext->getSelfNominalTypeDecl();
  if (!typeDecl)
    return false;

  if (typeDecl != stringDecl && typeDecl != substringDecl)
    return false;

  // Is the subscript index one we want to emit a special diagnostic
  // for, and the return type String?
  auto fnTy = D->getInterfaceType()->getAs<AnyFunctionType>();
  assert(fnTy && "Expected function type for subscript decl!");

  // We're only going to warn for BoundGenericStructType with a single
  // type argument that is not Int!
  auto params = fnTy->getParams();
  if (params.size() != 1)
    return false;

  const auto &param = params.front();
  if (param.hasLabel() || param.isVariadic())
    return false;

  auto inputTy = param.getOldType()->getAs<BoundGenericStructType>();
  if (!inputTy)
    return false;

  auto genericArgs = inputTy->getGenericArgs();
  if (genericArgs.size() != 1)
    return false;

  // The subscripts taking T<Int> do not return Substring, and our
  // special fixit does not help here.
  auto intDecl = Context.getIntDecl();
  auto nominalTypeParam = genericArgs[0]->getAs<NominalType>();
  if (!nominalTypeParam)
    return false;

  if (nominalTypeParam->getDecl() == intDecl)
    return false;

  auto resultTy = fnTy->getResult()->getAs<NominalType>();
  if (!resultTy)
    return false;

  return resultTy->getDecl() == stringDecl;
}

bool swift::diagnoseExplicitUnavailability(
    const ValueDecl *D,
    SourceRange R,
    const DeclContext *DC,
    llvm::function_ref<void(InFlightDiagnostic &)> attachRenameFixIts) {
  auto *Attr = AvailableAttr::isUnavailable(D);
  if (!Attr)
    return false;

  // Suppress the diagnostic if we are in synthesized code inside
  // a synthesized function and the reference is lexically
  // contained in a declaration that is itself marked unavailable.
  // The right thing to do here is to not synthesize that code in the
  // first place. rdar://problem/20491640
  if (R.isInvalid() && isInsideImplicitFunction(R, DC) &&
      isInsideUnavailableDeclaration(R, DC)) {
    return false;
  }

  // Calling unavailable code from within code with the same
  // unavailability is OK -- the eventual caller can't call the
  // enclosing code in the same situations it wouldn't be able to
  // call this code.
  if (isInsideCompatibleUnavailableDeclaration(R, DC, Attr)) {
    return false;
  }

  SourceLoc Loc = R.Start;
  DeclName Name;
  unsigned RawAccessorKind;
  std::tie(RawAccessorKind, Name) = getAccessorKindAndNameForDiagnostics(D);

  ASTContext &ctx = D->getASTContext();
  auto &diags = ctx.Diags;
  switch (Attr->getPlatformAgnosticAvailability()) {
  case PlatformAgnosticAvailabilityKind::Deprecated:
    break;

  case PlatformAgnosticAvailabilityKind::None:
  case PlatformAgnosticAvailabilityKind::Unavailable:
  case PlatformAgnosticAvailabilityKind::SwiftVersionSpecific:
  case PlatformAgnosticAvailabilityKind::PackageDescriptionVersionSpecific:
  case PlatformAgnosticAvailabilityKind::UnavailableInSwift: {
    bool inSwift = (Attr->getPlatformAgnosticAvailability() ==
                    PlatformAgnosticAvailabilityKind::UnavailableInSwift);

    if (!Attr->Rename.empty()) {
      SmallString<32> newNameBuf;
      Optional<ReplacementDeclKind> replaceKind =
          describeRename(ctx, Attr, D, newNameBuf);
      unsigned rawReplaceKind = static_cast<unsigned>(
          replaceKind.getValueOr(ReplacementDeclKind::None));
      StringRef newName = replaceKind ? newNameBuf.str() : Attr->Rename;

      if (Attr->Message.empty()) {
        auto diag = diags.diagnose(Loc,
                                   diag::availability_decl_unavailable_rename,
                                   RawAccessorKind, Name,
                                   replaceKind.hasValue(),
                                   rawReplaceKind, newName);
        attachRenameFixIts(diag);
      } else {
        EncodedDiagnosticMessage EncodedMessage(Attr->Message);
        auto diag =
          diags.diagnose(Loc, diag::availability_decl_unavailable_rename_msg,
                         RawAccessorKind, Name, replaceKind.hasValue(),
                         rawReplaceKind, newName, EncodedMessage.Message);
        attachRenameFixIts(diag);
      }
    } else if (isSubscriptReturningString(D, ctx)) {
      diags.diagnose(Loc, diag::availabilty_string_subscript_migration)
        .highlight(R)
        .fixItInsert(R.Start, "String(")
        .fixItInsertAfter(R.End, ")");

      // Skip the note emitted below.
      return true;
    } else if (Attr->Message.empty()) {
      diags.diagnose(Loc, inSwift ? diag::availability_decl_unavailable_in_swift
                                 : diag::availability_decl_unavailable,
                     RawAccessorKind, Name)
        .highlight(R);
    } else {
      EncodedDiagnosticMessage EncodedMessage(Attr->Message);
      diags.diagnose(Loc,
                     inSwift ? diag::availability_decl_unavailable_in_swift_msg
                             : diag::availability_decl_unavailable_msg,
                     RawAccessorKind, Name, EncodedMessage.Message)
        .highlight(R);
    }
    break;
  }
  }

  switch (Attr->getVersionAvailability(ctx)) {
  case AvailableVersionComparison::Available:
  case AvailableVersionComparison::PotentiallyUnavailable:
    llvm_unreachable("These aren't considered unavailable");

  case AvailableVersionComparison::Unavailable:
    if ((Attr->isLanguageVersionSpecific() ||
         Attr->isPackageDescriptionVersionSpecific())
        && Attr->Introduced.hasValue())
      diags.diagnose(D, diag::availability_introduced_in_version,
                     RawAccessorKind, Name,
                     (Attr->isLanguageVersionSpecific() ? 
                      "Swift" : "PackageDescription"),
                     *Attr->Introduced)
        .highlight(Attr->getRange());
    else
      diags.diagnose(D, diag::availability_marked_unavailable, RawAccessorKind,
                     Name)
        .highlight(Attr->getRange());
    break;

  case AvailableVersionComparison::Obsoleted:
    // FIXME: Use of the platformString here is non-awesome for application
    // extensions.

    StringRef platformDisplayString;
    if (Attr->isLanguageVersionSpecific()) {
      platformDisplayString = "Swift";
    } else if (Attr->isPackageDescriptionVersionSpecific()) {
      platformDisplayString = "PackageDescription";
    } else {
      platformDisplayString = Attr->prettyPlatformString();
    }

    diags.diagnose(D, diag::availability_obsoleted,
                   RawAccessorKind, Name,
                   platformDisplayString,
                   *Attr->Obsoleted)
      .highlight(Attr->getRange());
    break;
  }
  return true;
}

namespace {
class AvailabilityWalker : public ASTWalker {
  /// Describes how the next member reference will be treated as we traverse
  /// the AST.
  enum class MemberAccessContext : unsigned {
    /// The member reference is in a context where an access will call
    /// the getter.
    Getter,

    /// The member reference is in a context where an access will call
    /// the setter.
    Setter,

    /// The member reference is in a context where it will be turned into
    /// an inout argument. (Once this happens, we have to conservatively assume
    /// that both the getter and setter could be called.)
    InOut
  };

  TypeChecker &TC;
  DeclContext *DC;
  MemberAccessContext AccessContext = MemberAccessContext::Getter;
  SmallVector<const Expr *, 16> ExprStack;
  ResilienceExpansion Expansion;
  Optional<TypeChecker::FragileFunctionKind> FragileKind;
  bool TreatUsableFromInlineAsPublic = false;

public:
  AvailabilityWalker(
      TypeChecker &TC, DeclContext *DC) : TC(TC), DC(DC) {
    Expansion = DC->getResilienceExpansion();
    if (Expansion == ResilienceExpansion::Minimal)
      std::tie(FragileKind, TreatUsableFromInlineAsPublic)
        = TC.getFragileFunctionKind(DC);
  }

  std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
    ExprStack.push_back(E);

    auto visitChildren = [&]() { return std::make_pair(true, E); };
    auto skipChildren = [&]() {
      ExprStack.pop_back();
      return std::make_pair(false, E);
    };

    if (auto DR = dyn_cast<DeclRefExpr>(E)) {
      diagAvailability(DR->getDecl(), DR->getSourceRange(),
                       getEnclosingApplyExpr());
      maybeDiagStorageAccess(DR->getDecl(), DR->getSourceRange(), DC);
    }
    if (auto MR = dyn_cast<MemberRefExpr>(E)) {
      walkMemberRef(MR);
      return skipChildren();
    }
    if (auto OCDR = dyn_cast<OtherConstructorDeclRefExpr>(E))
      diagAvailability(OCDR->getDecl(),
                       OCDR->getConstructorLoc().getSourceRange(),
                       getEnclosingApplyExpr());
    if (auto DMR = dyn_cast<DynamicMemberRefExpr>(E))
      diagAvailability(DMR->getMember().getDecl(),
                       DMR->getNameLoc().getSourceRange(),
                       getEnclosingApplyExpr());
    if (auto DS = dyn_cast<DynamicSubscriptExpr>(E))
      diagAvailability(DS->getMember().getDecl(), DS->getSourceRange());
    if (auto S = dyn_cast<SubscriptExpr>(E)) {
      if (S->hasDecl()) {
        diagAvailability(S->getDecl().getDecl(), S->getSourceRange());
        maybeDiagStorageAccess(S->getDecl().getDecl(), S->getSourceRange(), DC);
      }
    }
    if (auto KP = dyn_cast<KeyPathExpr>(E)) {
      maybeDiagKeyPath(KP);
    }
    if (auto A = dyn_cast<AssignExpr>(E)) {
      walkAssignExpr(A);
      return skipChildren();
    }
    if (auto IO = dyn_cast<InOutExpr>(E)) {
      walkInOutExpr(IO);
      return skipChildren();
    }
    
    return visitChildren();
  }

  Expr *walkToExprPost(Expr *E) override {
    assert(ExprStack.back() == E);
    ExprStack.pop_back();

    return E;
  }

  bool diagAvailability(const ValueDecl *D, SourceRange R,
                        const ApplyExpr *call = nullptr,
                        bool AllowPotentiallyUnavailableProtocol = false,
                        bool SignalOnPotentialUnavailability = true);

private:
  bool diagnoseIncDecRemoval(const ValueDecl *D, SourceRange R,
                             const AvailableAttr *Attr);
  bool diagnoseMemoryLayoutMigration(const ValueDecl *D, SourceRange R,
                                     const AvailableAttr *Attr,
                                     const ApplyExpr *call);

  /// Walks up from a potential callee to the enclosing ApplyExpr.
  const ApplyExpr *getEnclosingApplyExpr() const {
    ArrayRef<const Expr *> parents = ExprStack;
    assert(!parents.empty() && "must be called while visiting an expression");
    size_t idx = parents.size() - 1;

    do {
      if (idx == 0)
        return nullptr;
      --idx;
    } while (isa<DotSyntaxBaseIgnoredExpr>(parents[idx]) || // Mod.f(a)
             isa<SelfApplyExpr>(parents[idx]) || // obj.f(a)
             isa<IdentityExpr>(parents[idx]) || // (f)(a)
             isa<ForceValueExpr>(parents[idx]) || // f!(a)
             isa<BindOptionalExpr>(parents[idx])); // f?(a)

    auto *call = dyn_cast<ApplyExpr>(parents[idx]);
    if (!call || call->getFn() != parents[idx+1])
      return nullptr;
    return call;
  }

  /// Walk an assignment expression, checking for availability.
  void walkAssignExpr(AssignExpr *E) {
    // We take over recursive walking of assignment expressions in order to
    // walk the destination and source expressions in different member
    // access contexts.
    Expr *Dest = E->getDest();
    if (!Dest) {
      return;
    }

    // Check the Dest expression in a setter context.
    // We have an implicit assumption here that the first MemberRefExpr
    // encountered walking (pre-order) is the Dest is the destination of the
    // write. For the moment this is fine -- but future syntax might violate
    // this assumption.
    walkInContext(E, Dest, MemberAccessContext::Setter);

    // Check RHS in getter context
    Expr *Source = E->getSrc();
    if (!Source) {
      return;
    }
    walkInContext(E, Source, MemberAccessContext::Getter);
  }
  
  /// Walk a member reference expression, checking for availability.
  void walkMemberRef(MemberRefExpr *E) {
    // Walk the base in a getter context.
    // FIXME: We may need to look at the setter too, if we're going to do
    // writeback. The AST should have this information.
    walkInContext(E, E->getBase(), MemberAccessContext::Getter);

    ValueDecl *D = E->getMember().getDecl();
    // Diagnose for the member declaration itself.
    if (diagAvailability(D, E->getNameLoc().getSourceRange()))
      return;

    // Diagnose for appropriate accessors, given the access context.
    maybeDiagStorageAccess(D, E->getSourceRange(), DC);
  }

  /// Walk a keypath expression, checking all of its components for
  /// availability.
  void maybeDiagKeyPath(KeyPathExpr *KP) {
    for (auto &component : KP->getComponents()) {
      switch (component.getKind()) {
      case KeyPathExpr::Component::Kind::Property:
      case KeyPathExpr::Component::Kind::Subscript: {
        auto *decl = component.getDeclRef().getDecl();
        auto loc = component.getLoc();
        SourceRange range(loc, loc);
        diagAvailability(decl, range, nullptr);
        break;
      }

      case KeyPathExpr::Component::Kind::Invalid:
      case KeyPathExpr::Component::Kind::UnresolvedProperty:
      case KeyPathExpr::Component::Kind::UnresolvedSubscript:
      case KeyPathExpr::Component::Kind::OptionalChain:
      case KeyPathExpr::Component::Kind::OptionalWrap:
      case KeyPathExpr::Component::Kind::OptionalForce:
      case KeyPathExpr::Component::Kind::Identity:
        break;
      }
    }
  }

  /// Walk an inout expression, checking for availability.
  void walkInOutExpr(InOutExpr *E) {
    walkInContext(E, E->getSubExpr(), MemberAccessContext::InOut);
  }

  /// Walk the given expression in the member access context.
  void walkInContext(Expr *baseExpr, Expr *E,
                     MemberAccessContext AccessContext) {
    llvm::SaveAndRestore<MemberAccessContext>
      C(this->AccessContext, AccessContext);
    E->walk(*this);
  }

  /// Emit diagnostics, if necessary, for accesses to storage where
  /// the accessor for the AccessContext is not available.
  void maybeDiagStorageAccess(const ValueDecl *VD,
                              SourceRange ReferenceRange,
                              const DeclContext *ReferenceDC) const {
    if (TC.getLangOpts().DisableAvailabilityChecking)
      return;

    auto *D = dyn_cast<AbstractStorageDecl>(VD);
    if (!D)
      return;

    if (!D->hasAnyAccessors()) {
      return;
    }
    
    // Check availability of accessor functions.
    // TODO: if we're talking about an inlineable storage declaration,
    // this probably needs to be refined to not assume that the accesses are
    // specifically using the getter/setter.
    switch (AccessContext) {
    case MemberAccessContext::Getter:
      diagAccessorAvailability(D->getGetter(), ReferenceRange, ReferenceDC,
                               /*ForInout=*/false);
      break;

    case MemberAccessContext::Setter:
      diagAccessorAvailability(D->getSetter(), ReferenceRange, ReferenceDC,
                               /*ForInout=*/false);
      break;

    case MemberAccessContext::InOut:
      diagAccessorAvailability(D->getGetter(), ReferenceRange, ReferenceDC,
                               /*ForInout=*/true);

      diagAccessorAvailability(D->getSetter(), ReferenceRange, ReferenceDC,
                               /*ForInout=*/true);
      break;
    }
  }

  /// Emit a diagnostic, if necessary for a potentially unavailable accessor.
  void diagAccessorAvailability(AccessorDecl *D, SourceRange ReferenceRange,
                                const DeclContext *ReferenceDC,
                                bool ForInout) const {
    if (!D) {
      return;
    }

    // If the property/subscript is unconditionally unavailable, don't bother
    // with any of the rest of this.
    if (AvailableAttr::isUnavailable(D->getStorage()))
      return;

    if (diagnoseExplicitUnavailability(D, ReferenceRange, ReferenceDC,
                                       /*call*/nullptr)) {
      return;
    }

    // Make sure not to diagnose an accessor's deprecation if we already
    // complained about the property/subscript.
    if (!TypeChecker::getDeprecated(D->getStorage()))
      TC.diagnoseIfDeprecated(ReferenceRange, ReferenceDC, D, /*call*/nullptr);

    auto MaybeUnavail = TC.checkDeclarationAvailability(D, ReferenceRange.Start,
                                                        DC);
    if (MaybeUnavail.hasValue()) {
      TC.diagnosePotentialAccessorUnavailability(D, ReferenceRange, ReferenceDC,
                                                 MaybeUnavail.getValue(),
                                                 ForInout);
    }
  }
};
} // end anonymous namespace

/// Diagnose uses of unavailable declarations. Returns true if a diagnostic
/// was emitted.
bool AvailabilityWalker::diagAvailability(const ValueDecl *D, SourceRange R,
                                          const ApplyExpr *call,
                                          bool AllowPotentiallyUnavailableProtocol,
                                          bool SignalOnPotentialUnavailability) {
  if (!D)
    return false;

  if (auto *attr = AvailableAttr::isUnavailable(D)) {
    if (diagnoseIncDecRemoval(D, R, attr))
      return true;
    if (call && diagnoseMemoryLayoutMigration(D, R, attr, call))
      return true;
  }

  if (FragileKind)
    if (R.isValid())
      if (TC.diagnoseInlinableDeclRef(R.Start, D, DC, *FragileKind,
                                      TreatUsableFromInlineAsPublic))
        return true;

  if (diagnoseExplicitUnavailability(D, R, DC, call))
    return true;

  // Diagnose for deprecation
  TC.diagnoseIfDeprecated(R, DC, D, call);

  if (AllowPotentiallyUnavailableProtocol && isa<ProtocolDecl>(D))
    return false;

  // Diagnose (and possibly signal) for potential unavailability
  auto maybeUnavail = TC.checkDeclarationAvailability(D, R.Start, DC);
  if (maybeUnavail.hasValue()) {
    TC.diagnosePotentialUnavailability(D, R, DC, maybeUnavail.getValue());
    if (SignalOnPotentialUnavailability)
      return true;
  }
  return false;
}


/// Return true if the specified type looks like an integer of floating point
/// type.
static bool isIntegerOrFloatingPointType(Type ty, DeclContext *DC,
                                         TypeChecker &TC) {
  auto integerType =
    TC.getProtocol(SourceLoc(),
                   KnownProtocolKind::ExpressibleByIntegerLiteral);
  auto floatingType =
    TC.getProtocol(SourceLoc(),
                   KnownProtocolKind::ExpressibleByFloatLiteral);
  if (!integerType || !floatingType) return false;

  return
    TC.conformsToProtocol(ty, integerType, DC,
                          ConformanceCheckFlags::InExpression) ||
    TC.conformsToProtocol(ty, floatingType, DC,
                          ConformanceCheckFlags::InExpression);
}


/// If this is a call to an unavailable ++ / -- operator, try to diagnose it
/// with a fixit hint and return true.  If not, or if we fail, return false.
bool AvailabilityWalker::diagnoseIncDecRemoval(const ValueDecl *D,
                                               SourceRange R,
                                               const AvailableAttr *Attr) {
  // We can only produce a fixit if we're talking about ++ or --.
  bool isInc = D->getBaseName() == "++";
  if (!isInc && D->getBaseName() != "--")
    return false;

  // We can only handle the simple cases of lvalue++ and ++lvalue.  This is
  // always modeled as:
  //   (postfix_unary_expr (declrefexpr ++), (inoutexpr (lvalue)))
  // if not, bail out.
  if (ExprStack.size() != 2 ||
      !isa<DeclRefExpr>(ExprStack[1]) ||
      !(isa<PostfixUnaryExpr>(ExprStack[0]) ||
        isa<PrefixUnaryExpr>(ExprStack[0])))
    return false;

  auto call = cast<ApplyExpr>(ExprStack[0]);

  // If the expression type is integer or floating point, then we can rewrite it
  // to "lvalue += 1".
  std::string replacement;
  if (isIntegerOrFloatingPointType(call->getType(), DC, TC))
    replacement = isInc ? " += 1" : " -= 1";
  else {
    // Otherwise, it must be an index type.  Rewrite to:
    // "lvalue = lvalue.successor()".
    auto &SM = TC.Context.SourceMgr;
    auto CSR = Lexer::getCharSourceRangeFromSourceRange(SM,
                                         call->getArg()->getSourceRange());
    replacement = " = " + SM.extractText(CSR).str();
    replacement += isInc ? ".successor()" : ".predecessor()";
  }
  
  if (!replacement.empty()) {
    DeclName Name;
    unsigned RawAccessorKind;
    std::tie(RawAccessorKind, Name) = getAccessorKindAndNameForDiagnostics(D);

    // If we emit a deprecation diagnostic, produce a fixit hint as well.
    auto diag = TC.diagnose(R.Start, diag::availability_decl_unavailable_msg,
                            RawAccessorKind, Name,
                            "it has been removed in Swift 3");
    if (isa<PrefixUnaryExpr>(call)) {
      // Prefix: remove the ++ or --.
      diag.fixItRemove(call->getFn()->getSourceRange());
      diag.fixItInsertAfter(call->getArg()->getEndLoc(), replacement);
    } else {
      // Postfix: replace the ++ or --.
      diag.fixItReplace(call->getFn()->getSourceRange(), replacement);
    }

    return true;
  }


  return false;
}

/// If this is a call to an unavailable sizeof family function, diagnose it
/// with a fixit hint and return true. If not, or if we fail, return false.
bool AvailabilityWalker::diagnoseMemoryLayoutMigration(const ValueDecl *D,
                                                       SourceRange R,
                                                       const AvailableAttr *Attr,
                                                       const ApplyExpr *call) {

  if (!D->getModuleContext()->isStdlibModule())
    return false;

  StringRef Property;
  if (D->getBaseName() == "sizeof") {
    Property = "size";
  } else if (D->getBaseName() == "alignof") {
    Property = "alignment";
  } else if (D->getBaseName() == "strideof") {
    Property = "stride";
  }

  if (Property.empty())
    return false;

  auto args = dyn_cast<ParenExpr>(call->getArg());
  if (!args)
    return false;

  DeclName Name;
  unsigned RawAccessorKind;
  std::tie(RawAccessorKind, Name) = getAccessorKindAndNameForDiagnostics(D);

  EncodedDiagnosticMessage EncodedMessage(Attr->Message);
  auto diag = TC.diagnose(R.Start, diag::availability_decl_unavailable_msg,
                          RawAccessorKind, Name, EncodedMessage.Message);
  diag.highlight(R);

  auto subject = args->getSubExpr();

  StringRef Prefix = "MemoryLayout<";
  StringRef Suffix = ">.";

  if (auto DTE = dyn_cast<DynamicTypeExpr>(subject)) {
    // Replace `sizeof(type(of: x))` with `MemoryLayout<X>.size`, where `X` is
    // the static type of `x`. The previous spelling misleadingly hinted that
    // `sizeof(_:)` might return the size of the *dynamic* type of `x`, when
    // it is not the case.
    auto valueType = DTE->getBase()->getType()->getRValueType();
    if (!valueType || valueType->hasError()) {
      // If we don't have a suitable argument, we can't emit a fixit.
      return true;
    }
    // Note that in rare circumstances we may be destructively replacing the
    // source text. For example, we'd replace `sizeof(type(of: doSomething()))`
    // with `MemoryLayout<T>.size`, if T is the return type of `doSomething()`.
    diag.fixItReplace(call->getSourceRange(),
                   (Prefix + valueType->getString() + Suffix + Property).str());
  } else {
    SourceRange PrefixRange(call->getStartLoc(), args->getLParenLoc());
    SourceRange SuffixRange(args->getRParenLoc());

    // We must remove `.self`.
    if (auto *DSE = dyn_cast<DotSelfExpr>(subject))
      SuffixRange.Start = DSE->getDotLoc();

    diag
      .fixItReplace(PrefixRange, Prefix)
      .fixItReplace(SuffixRange, (Suffix + Property).str());
  }

  return true;
}

/// Diagnose uses of unavailable declarations.
void swift::diagAvailability(TypeChecker &TC, const Expr *E,
                             DeclContext *DC) {
  AvailabilityWalker walker(TC, DC);
  const_cast<Expr*>(E)->walk(walker);
}

/// Run the Availability-diagnostics algorithm otherwise used in an expr
/// context, but for non-expr contexts such as TypeDecls referenced from
/// TypeReprs.
bool swift::diagnoseDeclAvailability(const ValueDecl *Decl,
                                     TypeChecker &TC,
                                     DeclContext *DC,
                                     SourceRange R,
                                     bool AllowPotentiallyUnavailableProtocol,
                                     bool SignalOnPotentialUnavailability)
{
  AvailabilityWalker AW(TC, DC);
  return AW.diagAvailability(Decl, R, nullptr,
                             AllowPotentiallyUnavailableProtocol,
                             SignalOnPotentialUnavailability);
}
