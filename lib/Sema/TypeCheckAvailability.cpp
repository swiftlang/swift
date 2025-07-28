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
#include "MiscDiagnostics.h"
#include "TypeCheckConcurrency.h"
#include "TypeCheckObjC.h"
#include "TypeCheckType.h"
#include "TypeCheckUnsafe.h"
#include "TypeChecker.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/AvailabilityConstraint.h"
#include "swift/AST/AvailabilityDomain.h"
#include "swift/AST/AvailabilityScope.h"
#include "swift/AST/AvailabilitySpec.h"
#include "swift/AST/ClangModuleLoader.h"
#include "swift/AST/DeclExportabilityVisitor.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/PackConformance.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/TypeDeclFinder.h"
#include "swift/AST/UnsafeUse.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/ParseDeclName.h"
#include "swift/Sema/IDETypeChecking.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/SaveAndRestore.h"
using namespace swift;

/// Emit a diagnostic for references to declarations that have been
/// marked as unavailable, either through "unavailable" or "obsoleted:".
static bool diagnoseExplicitUnavailability(
    SourceLoc loc, const AvailabilityConstraint &constraint,
    const RootProtocolConformance *rootConf, const ExtensionDecl *ext,
    const ExportContext &where,
    bool warnIfConformanceUnavailablePreSwift6 = false,
    bool preconcurrency = false);

/// Emit a diagnostic for references to declarations that have been
/// marked as unavailable, either through "unavailable" or "obsoleted:".
static bool diagnoseExplicitUnavailability(
    const ValueDecl *D, SourceRange R, const AvailabilityConstraint &constraint,
    const ExportContext &Where, DeclAvailabilityFlags Flags,
    llvm::function_ref<void(InFlightDiagnostic &, StringRef)>
        attachRenameFixIts);

static bool diagnoseSubstitutionMapAvailability(
    SourceLoc loc, SubstitutionMap subs, const ExportContext &where,
    Type depTy = Type(), Type replacementTy = Type(),
    bool warnIfConformanceUnavailablePreSwift6 = false,
    bool suppressParameterizationCheckForOptional = false,
    bool preconcurrency = false);

/// Diagnose uses of unavailable declarations in types.
static bool
diagnoseTypeReprAvailability(const TypeRepr *T, const ExportContext &where,
                             DeclAvailabilityFlags flags = std::nullopt);

ExportContext::ExportContext(DeclContext *DC,
                             AvailabilityContext availability,
                             FragileFunctionKind kind,
                             llvm::SmallVectorImpl<UnsafeUse> *unsafeUses,
                             bool spi, bool exported,
                             bool implicit)
    : DC(DC), Availability(availability), FragileKind(kind),
      UnsafeUses(unsafeUses) {
  SPI = spi;
  Exported = exported;
  Implicit = implicit;
  Reason = unsigned(ExportabilityReason::General);
}

template<typename Fn>
static void forEachOuterDecl(DeclContext *DC, Fn fn) {
  for (; !DC->isModuleScopeContext(); DC = DC->getParent()) {
    switch (DC->getContextKind()) {
    case DeclContextKind::AbstractClosureExpr:
    case DeclContextKind::SerializedAbstractClosure:
    case DeclContextKind::TopLevelCodeDecl:
    case DeclContextKind::SerializedTopLevelCodeDecl:
    case DeclContextKind::Package:
    case DeclContextKind::Module:
    case DeclContextKind::FileUnit:
    case DeclContextKind::MacroDecl:
      break;

    case DeclContextKind::Initializer:
      if (auto *PBI = dyn_cast<PatternBindingInitializer>(DC))
        fn(PBI->getBinding());
      else if (auto *I = dyn_cast<PropertyWrapperInitializer>(DC))
        fn(I->getWrappedVar());
      break;

    case DeclContextKind::SubscriptDecl:
      fn(cast<SubscriptDecl>(DC));
      break;

    case DeclContextKind::EnumElementDecl:
      fn(cast<EnumElementDecl>(DC));
      break;

    case DeclContextKind::AbstractFunctionDecl:
      fn(cast<AbstractFunctionDecl>(DC));

      if (auto *AD = dyn_cast<AccessorDecl>(DC))
        fn(AD->getStorage());
      break;

    case DeclContextKind::GenericTypeDecl:
      fn(cast<GenericTypeDecl>(DC));
      break;

    case DeclContextKind::ExtensionDecl:
      fn(cast<ExtensionDecl>(DC));
      break;
    }
  }
}

static void computeExportContextBits(ASTContext &Ctx, Decl *D, bool *spi,
                                     bool *implicit) {
  if (D->isSPI() ||
      D->isAvailableAsSPI())
    *spi = true;

  // Defer bodies are desugared to an implicit closure expression. We need to
  // dilute the meaning of "implicit" to make sure we're still checking
  // availability inside of defer statements.
  const auto isDeferBody = isa<FuncDecl>(D) && cast<FuncDecl>(D)->isDeferBody();
  if (D->isImplicit() && !isDeferBody)
    *implicit = true;

  if (auto *PBD = dyn_cast<PatternBindingDecl>(D)) {
    for (unsigned i = 0, e = PBD->getNumPatternEntries(); i < e; ++i) {
      if (auto *VD = PBD->getAnchoringVarDecl(i))
        computeExportContextBits(Ctx, VD, spi, implicit);
    }
  }
}

ExportContext ExportContext::forDeclSignature(Decl *D) {
  auto &Ctx = D->getASTContext();

  auto *DC = D->getInnermostDeclContext();
  auto fragileKind = DC->getFragileFunctionKind();
  auto loc = D->getLoc();
  auto availabilityContext = AvailabilityContext::forLocation(loc, DC);
  bool spi = Ctx.LangOpts.LibraryLevel == LibraryLevel::SPI;
  bool implicit = false;
  computeExportContextBits(Ctx, D, &spi, &implicit);
  forEachOuterDecl(D->getDeclContext(), [&](Decl *D) {
    computeExportContextBits(Ctx, D, &spi, &implicit);
  });

  bool exported = ::isExported(D);

  return ExportContext(DC, availabilityContext, fragileKind, nullptr,
                       spi, exported, implicit);
}

ExportContext ExportContext::forFunctionBody(DeclContext *DC, SourceLoc loc) {
  auto &Ctx = DC->getASTContext();

  auto fragileKind = DC->getFragileFunctionKind();
  auto availabilityContext = AvailabilityContext::forLocation(loc, DC);
  bool spi = Ctx.LangOpts.LibraryLevel == LibraryLevel::SPI;
  bool implicit = false;
  forEachOuterDecl(
      DC, [&](Decl *D) { computeExportContextBits(Ctx, D, &spi, &implicit); });

  bool exported = false;

  return ExportContext(DC, availabilityContext, fragileKind, nullptr,
                       spi, exported, implicit);
}

ExportContext ExportContext::forConformance(DeclContext *DC,
                                            ProtocolDecl *proto) {
  assert(isa<ExtensionDecl>(DC) || isa<NominalTypeDecl>(DC));
  auto where = forDeclSignature(DC->getInnermostDeclarationDeclContext());

  where.Exported &= proto->getFormalAccessScope(
      DC, /*usableFromInlineAsPublic*/true).isPublic();

  return where;
}

ExportContext ExportContext::withReason(ExportabilityReason reason) const {
  auto copy = *this;
  copy.Reason = unsigned(reason);
  return copy;
}

ExportContext ExportContext::withExported(bool exported) const {
  auto copy = *this;
  copy.Exported = isExported() && exported;
  return copy;
}

ExportContext ExportContext::withRefinedAvailability(
    const AvailabilityRange &availability) const {
  auto copy = *this;
  copy.Availability.constrainWithPlatformRange(availability,
                                               DC->getASTContext());
  return copy;
}

bool ExportContext::mustOnlyReferenceExportedDecls() const {
  return Exported || FragileKind.kind != FragileFunctionKind::None;
}

std::optional<ExportabilityReason>
ExportContext::getExportabilityReason() const {
  if (Exported)
    return ExportabilityReason(Reason);
  return std::nullopt;
}

static bool shouldAllowReferenceToUnavailableInSwiftDeclaration(
    const Decl *D, const ExportContext &where) {
  auto *DC = where.getDeclContext();
  auto *SF = DC->getParentSourceFile();

  // Unavailable-in-Swift declarations shouldn't be referenced directly in
  // source. However, they can be referenced in implicit declarations that are
  // printed in .swiftinterfaces.
  if (!SF || SF->Kind != SourceFileKind::Interface)
    return false;

  if (auto constructor = dyn_cast_or_null<ConstructorDecl>(DC->getAsDecl())) {
    // Designated initializers inherited from an Obj-C superclass may have
    // parameters that are unavailable-in-Swift.
    if (constructor->isObjC())
      return true;
  }

  return false;
}

/// Retrieve the innermost DeclContext that should be consulted for noasync
/// checking.
static const DeclContext *
getInnermostDeclContextForNoAsync(const DeclContext *DC) {
  if (auto *D = DC->getAsDecl()) {
    if (auto *FD = dyn_cast<FuncDecl>(D)) {
      if (FD->isDeferBody())
        // If this is a defer body, we should delegate to its parent.
        return getInnermostDeclContextForNoAsync(DC->getParent());
    }
  }
  return DC;
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
  std::optional<ASTNode> InnermostMatchingNode;

public:
  InnermostAncestorFinder(SourceRange TargetRange, const SourceManager &SM,
                          ASTNode SearchNode, const MatchPredicate &Predicate)
      : TargetRange(TargetRange), SM(SM), Predicate(Predicate) {
    assert(TargetRange.isValid());

    SearchNode.walk(*this);
  }

  /// Returns the innermost node containing the target range that matches
  /// the predicate.
  std::optional<ASTNode> getInnermostMatchingNode() {
    return InnermostMatchingNode;
  }

  MacroWalking getMacroWalkingBehavior() const override {
    // This is SourceRange based finder. 'SM.rangeContains()' fails anyway when
    // crossing source buffers.
    return MacroWalking::Arguments;
  }

  PreWalkResult<Expr *> walkToExprPre(Expr *E) override {
    return getPreWalkActionFor(E);
  }

  PreWalkResult<Stmt *> walkToStmtPre(Stmt *S) override {
    return getPreWalkActionFor(S);
  }

  PreWalkAction walkToDeclPre(Decl *D) override {
    return getPreWalkActionFor(D).Action;
  }

  PreWalkResult<Pattern *> walkToPatternPre(Pattern *P) override {
    return getPreWalkActionFor(P);
  }

  PreWalkAction walkToTypeReprPre(TypeRepr *T) override {
    return getPreWalkActionFor(T).Action;
  }

  /// Retrieve the pre-walk action for a given node, which determines whether
  /// or not it should be walked into.
  template <typename T>
  PreWalkResult<T> getPreWalkActionFor(T Node) {
    // When walking down the tree, we traverse until we have found a node
    // inside the target range. Once we have found such a node, there is no
    // need to traverse any deeper.
    if (FoundTarget)
      return Action::SkipNode(Node);

    // If we haven't found our target yet and the node we are pre-visiting
    // doesn't have a valid range, we still have to traverse it because its
    // subtrees may have valid ranges.
    auto Range = Node->getSourceRange();
    if (Range.isInvalid())
      return Action::Continue(Node);

    // We have found our target if the range of the node we are visiting
    // is contained in the range we are looking for.
    FoundTarget = SM.rangeContains(TargetRange, Range);

    if (FoundTarget) {
      walkToNodePost(Node);
      return Action::SkipNode(Node);
    }

    // Search the subtree if the target range is inside its range.
    if (!SM.rangeContains(Range, TargetRange))
      return Action::SkipNode(Node);

    return Action::Continue(Node);
  }

  PostWalkResult<Expr *> walkToExprPost(Expr *E) override {
    return walkToNodePost(E);
  }

  PostWalkResult<Stmt *> walkToStmtPost(Stmt *S) override {
    return walkToNodePost(S);
  }

  PostWalkAction walkToDeclPost(Decl *D) override {
    return walkToNodePost(D).Action;
  }

  /// Once we have found the target node, look for the innermost ancestor
  /// matching our criteria on the way back up the spine of the tree.
  template <typename T>
  PostWalkResult<T> walkToNodePost(T Node) {
    if (!InnermostMatchingNode.has_value() && Predicate(Node, Parent)) {
      assert(Node->getSourceRange().isInvalid() ||
             SM.rangeContains(Node->getSourceRange(), TargetRange));

      InnermostMatchingNode = Node;
      return Action::Stop();
    }

    return Action::Continue(Node);
  }
};

/// Starting from SearchRoot, finds the innermost node containing ChildRange
/// for which Predicate returns true. Returns None if no such root is found.
static std::optional<ASTNode> findInnermostAncestor(
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
  auto ContainsReferenceRange = [&](const Decl *D) -> bool {
    if (ReferenceRange.isInvalid())
      return false;

    return SM.rangeContains(D->getSourceRange(), ReferenceRange);
  };

  if (const Decl *D = ReferenceDC->getInnermostDeclarationDeclContext()) {
    // If we have an inner declaration context, see if we can narrow the search
    // down to one of its members. This is important for properties, which don't
    // count as DeclContexts of their own but which can still introduce
    // availability.
    if (auto *IDC = dyn_cast<IterableDeclContext>(D)) {
      auto BestMember = llvm::find_if(IDC->getMembers(),
                                      ContainsReferenceRange);
      if (BestMember != IDC->getMembers().end())
        return *BestMember;
    }
    return D;
  }

  // We couldn't find a suitable node by climbing the DeclContext hierarchy, so
  // fall back to looking for a top-level declaration that contains the
  // reference range. We will hit this case for top-level elements that do not
  // themselves introduce DeclContexts, such as global variables. If we don't
  // have a reference range, there is nothing we can do, so return null.
  if (ReferenceRange.isInvalid())
    return nullptr;

  SourceFile *SF = ReferenceDC->getParentSourceFile();
  if (!SF)
    return nullptr;

  auto BestTopLevelDecl = llvm::find_if(SF->getTopLevelDecls(),
                                        ContainsReferenceRange);
  if (BestTopLevelDecl != SF->getTopLevelDecls().end())
    return *BestTopLevelDecl;

  return nullptr;
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

  auto abiRole = ABIRoleInfo(D);
  if (!abiRole.providesAPI() && abiRole.getCounterpart()) {
    // ABI-only decls can't have @available attributes of their own.
    D = abiRole.getCounterpart();
  }

  return D->getAbstractSyntaxDeclForAttributes();
}

/// Walk the DeclContext hierarchy starting from D to find a declaration
/// at the member level (i.e., declared in a type context) on which to provide
/// an @available() Fix-It.
static const Decl *ancestorMemberLevelDeclForAvailabilityFixit(const Decl *D) {
  while (D) {
    D = relatedDeclForAvailabilityFixit(D);

    if (!D->isImplicit() && D->getDeclContext()->isTypeContext() &&
        DeclAttribute::canAttributeAppearOnDecl(DeclAttrKind::Available, D)) {
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
  if (!DeclAttribute::canAttributeAppearOnDecl(DeclAttrKind::Available, D)) {
    return false;
  }

  if (isa<ExtensionDecl>(D) || isa<NominalTypeDecl>(D)) {
    return true;
  }

  bool IsModuleScopeContext = D->getDeclContext()->isModuleScopeContext();

  // We consider global functions, type aliases, and macros to be "type level"
  if (isa<FuncDecl>(D) || isa<MacroDecl>(D) || isa<TypeAliasDecl>(D)) {
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
static void findAvailabilityFixItNodes(
    SourceRange ReferenceRange, const DeclContext *ReferenceDC,
    const SourceManager &SM, std::optional<ASTNode> &FoundVersionCheckNode,
    const Decl *&FoundMemberLevelDecl, const Decl *&FoundTypeLevelDecl) {
  FoundVersionCheckNode = std::nullopt;
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
          if (!isa<ClosureExpr>(ParentExpr))
            return false;
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

  // Try to find declarations on which @available attributes can be added.
  // The heuristics for finding these declarations are biased towards deeper
  // nodes in the AST to limit the scope of suggested availability regions
  // and provide a better IDE experience (it can get jumpy if Fix-It locations
  // are far away from the error needing the Fix-It).
  if (DeclarationToSearch) {
    FoundMemberLevelDecl =
        ancestorMemberLevelDeclForAvailabilityFixit(DeclarationToSearch);

    FoundTypeLevelDecl =
        ancestorTypeLevelDeclForAvailabilityFixit(DeclarationToSearch);
  }
}

/// Emit a diagnostic note and Fix-It to add an @available attribute
/// on the given declaration for the given version range.
static void fixAvailabilityForDecl(
    SourceRange ReferenceRange, const Decl *D, AvailabilityDomain Domain,
    const AvailabilityRange &RequiredAvailability, ASTContext &Context) {
  assert(D);

  // Don't suggest adding an @available to a declaration where we would
  // emit a diagnostic saying it is not allowed.
  if (TypeChecker::diagnosticIfDeclCannotBePotentiallyUnavailable(D).has_value())
    return;

  // Don't suggest adding an @available attribute to a declaration that already
  // has one that is active for the given domain.
  // FIXME: Emit a fix-it to adjust the existing attribute instead.
  if (D->hasAnyMatchingActiveAvailableAttr([&](SemanticAvailableAttr attr) {
        return attr.getDomain().isRelated(Domain);
      }))
    return;

  // For some declarations (variables, enum elements), the location in concrete
  // syntax to suggest the Fix-It may differ from the declaration to which
  // we attach availability attributes in the abstract syntax tree during
  // parsing.
  const Decl *ConcDecl = relatedDeclForAvailabilityFixit(D);

  // To avoid exposing the pattern binding declaration to the user, get the
  // descriptive kind from one of the VarDecls.
  const auto *DeclForDiagnostic = ConcDecl;
  if (isa<PatternBindingDecl>(DeclForDiagnostic)) {
    DeclForDiagnostic = D;
  }

  SourceLoc InsertLoc =
      ConcDecl->getAttributeInsertionLoc(/*forModifier=*/false);
  if (InsertLoc.isInvalid())
    return;

  StringRef OriginalIndent =
      Lexer::getIndentationForLine(Context.SourceMgr, InsertLoc);

  llvm::SmallString<64> FixItBuffer;
  llvm::raw_svector_ostream FixIt(FixItBuffer);

  FixIt << "@available(" << Domain.getNameForAttributePrinting();
  if (Domain.isVersioned())
    FixIt << " " <<  RequiredAvailability.getVersionString();
  if (Domain.isPlatform())
    FixIt << ", *";
  FixIt << ")\n" << OriginalIndent;

  D->diagnose(diag::availability_add_attribute, DeclForDiagnostic)
      .fixItInsert(InsertLoc, FixIt.str());
}

/// In the special case of being in an existing, nontrivial availability scope
/// that's close but not quite narrow enough to satisfy requirements
/// (i.e. requirements are contained-in the existing scope but off by a subminor
/// version), emit a diagnostic and fixit that narrows the existing scope
/// condition to the required range.
static bool fixAvailabilityByNarrowingNearbyVersionCheck(
    SourceRange ReferenceRange, const DeclContext *ReferenceDC,
    AvailabilityDomain Domain, const AvailabilityRange &RequiredAvailability,
    ASTContext &Context, InFlightDiagnostic &Err) {
  // FIXME: [availability] Support fixing availability for non-platform domains
  if (!Domain.isPlatform())
    return false;

  const AvailabilityScope *scope = nullptr;
  (void)AvailabilityContext::forLocation(ReferenceRange.Start, ReferenceDC,
                                         &scope);
  if (!scope)
    return false;

  auto ExplicitAvailability =
      scope->getExplicitAvailabilityRange(Domain, Context);
  if (ExplicitAvailability && !RequiredAvailability.isAlwaysAvailable() &&
      scope->getReason() != AvailabilityScope::Reason::Root &&
      RequiredAvailability.isContainedIn(*ExplicitAvailability)) {

    // Only fix situations that are "nearby" versions, meaning
    // disagreement on a minor-or-less version (subminor-or-less version for
    // macOS 10.x.y).
    auto RunningVers = ExplicitAvailability->getRawMinimumVersion();
    auto RequiredVers = RequiredAvailability.getRawMinimumVersion();
    auto Platform = targetPlatform(Context.LangOpts);
    if (RunningVers.getMajor() != RequiredVers.getMajor())
      return false;
    if ((Platform == PlatformKind::macOS ||
         Platform == PlatformKind::macOSApplicationExtension) &&
        RunningVers.getMajor() == 10 &&
        !(RunningVers.getMinor().has_value() &&
          RequiredVers.getMinor().has_value() &&
          RunningVers.getMinor().value() ==
          RequiredVers.getMinor().value()))
      return false;

    auto FixRange = scope->getAvailabilityConditionVersionSourceRange(
        AvailabilityDomain::forPlatform(Platform), RunningVers);
    if (!FixRange.isValid())
      return false;
    // Have found a nontrivial availability scope-introducer to narrow.
    Err.fixItReplace(FixRange, RequiredAvailability.getVersionString());
    return true;
  }
  return false;
}

/// Emit a diagnostic note and Fix-It to add an if #available(...) { } guard
/// that checks for the given version range around the given node.
static void fixAvailabilityByAddingVersionCheck(
    ASTNode NodeToWrap, AvailabilityDomain Domain,
    const AvailabilityRange &RequiredAvailability, SourceRange ReferenceRange,
    ASTContext &Context) {
  // If this is an implicit variable that wraps an expression,
  // let's point to it's initializer. For example, result builder
  // transform captures expressions into implicit variables.
  if (auto *PB =
          dyn_cast_or_null<PatternBindingDecl>(NodeToWrap.dyn_cast<Decl *>())) {
    if (PB->isImplicit() && PB->getSingleVar()) {
      if (auto *init = PB->getInit(0))
        NodeToWrap = init;
    }
  }

  SourceRange RangeToWrap = NodeToWrap.getSourceRange();
  if (RangeToWrap.isInvalid())
    return;

  SourceLoc ReplaceLocStart = RangeToWrap.Start;
  StringRef ExtraIndent;
  StringRef OriginalIndent = Lexer::getIndentationForLine(
      Context.SourceMgr, ReplaceLocStart, &ExtraIndent);

  std::string IfText;
  {
    llvm::raw_string_ostream Out(IfText);

    SourceLoc ReplaceLocEnd =
        Lexer::getLocForEndOfToken(Context.SourceMgr, RangeToWrap.End);

    std::string GuardedText =
        Context.SourceMgr.extractText(CharSourceRange(Context.SourceMgr,
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

    AvailabilityDomain QueryDomain = Domain;

    // Runtime availability checks that specify app extension platforms don't
    // work, so only suggest checks against the base platform.
    if (auto CanonicalPlatform =
            basePlatformForExtensionPlatform(QueryDomain.getPlatformKind())) {
      QueryDomain = AvailabilityDomain::forPlatform(*CanonicalPlatform);
    }

    Out << "if #available(" << QueryDomain.getNameForAttributePrinting();
    if (QueryDomain.isVersioned())
      Out << " " << RequiredAvailability.getVersionString();
    if (QueryDomain.isPlatform())
      Out << ", *";

    Out << ") {\n";

    Out << OriginalIndent << ExtraIndent << GuardedText << "\n";

    // We emit an empty fallback case with a comment to encourage the developer
    // to think explicitly about whether fallback on earlier versions is needed.
    Out << OriginalIndent << "} else {\n";
    Out << OriginalIndent << ExtraIndent << "// Fallback";
    if (QueryDomain.isVersioned())
      Out << " on earlier versions";
    Out << "\n" << OriginalIndent << "}";
  }

  Context.Diags.diagnose(
      ReferenceRange.Start, diag::availability_guard_with_version_check)
      .fixItReplace(RangeToWrap, IfText);
}

/// Emit suggested Fix-Its for a reference with to an unavailable symbol
/// requiting the given OS version range.
static void fixAvailability(SourceRange ReferenceRange,
                            const DeclContext *ReferenceDC,
                            AvailabilityDomain Domain,
                            const AvailabilityRange &RequiredAvailability,
                            ASTContext &Context) {
  if (ReferenceRange.isInvalid())
    return;

  std::optional<ASTNode> NodeToWrapInVersionCheck;
  const Decl *FoundMemberDecl = nullptr;
  const Decl *FoundTypeLevelDecl = nullptr;

  findAvailabilityFixItNodes(ReferenceRange, ReferenceDC, Context.SourceMgr,
                             NodeToWrapInVersionCheck, FoundMemberDecl,
                             FoundTypeLevelDecl);

  // Suggest wrapping in if #available(...) { ... } if possible.
  if (NodeToWrapInVersionCheck.has_value()) {
    fixAvailabilityByAddingVersionCheck(NodeToWrapInVersionCheck.value(),
                                        Domain, RequiredAvailability,
                                        ReferenceRange, Context);
  }

  // Suggest adding availability attributes.
  if (FoundMemberDecl) {
    fixAvailabilityForDecl(ReferenceRange, FoundMemberDecl, Domain,
                           RequiredAvailability, Context);
  }

  if (FoundTypeLevelDecl) {
    fixAvailabilityForDecl(ReferenceRange, FoundTypeLevelDecl, Domain,
                           RequiredAvailability, Context);
  }
}

static void diagnosePotentialUnavailability(
    SourceRange ReferenceRange,
    llvm::function_ref<InFlightDiagnostic(AvailabilityDomain,
                                          AvailabilityRange)>
        Diagnose,
    const DeclContext *ReferenceDC, AvailabilityDomain Domain,
    const AvailabilityRange &Availability) {
  ASTContext &Context = ReferenceDC->getASTContext();

  {
    auto Err = Diagnose(Domain, Availability);

    // Direct a fixit to the error if an existing guard is nearly-correct
    if (fixAvailabilityByNarrowingNearbyVersionCheck(
            ReferenceRange, ReferenceDC, Domain, Availability, Context, Err))
      return;
  }
  fixAvailability(ReferenceRange, ReferenceDC, Domain, Availability, Context);
}

// FIXME: [availability] Should this take an AvailabilityContext instead of
// AvailabilityRange?
bool TypeChecker::checkAvailability(SourceRange ReferenceRange,
                                    AvailabilityRange PlatformRange,
                                    const DeclContext *ReferenceDC,
                                    llvm::function_ref<InFlightDiagnostic(
                                        AvailabilityDomain, AvailabilityRange)>
                                        Diagnose) {
  ASTContext &ctx = ReferenceDC->getASTContext();
  if (ctx.LangOpts.DisableAvailabilityChecking)
    return false;

  auto domain = ctx.getTargetAvailabilityDomain();
  if (domain.isUniversal())
    return false;

  auto availabilityAtLocation =
      AvailabilityContext::forLocation(ReferenceRange.Start, ReferenceDC)
          .getPlatformRange();

  if (!availabilityAtLocation.isContainedIn(PlatformRange)) {
    diagnosePotentialUnavailability(ReferenceRange, Diagnose, ReferenceDC,
                                    domain, PlatformRange);
    return true;
  }

  return false;
}

bool TypeChecker::checkAvailability(
    SourceRange ReferenceRange, AvailabilityRange PlatformRange,
    Diag<AvailabilityDomain, AvailabilityRange> Diag,
    const DeclContext *ReferenceDC) {
  auto &Diags = ReferenceDC->getASTContext().Diags;
  return TypeChecker::checkAvailability(
      ReferenceRange, PlatformRange, ReferenceDC,
      [&](AvailabilityDomain domain, AvailabilityRange range) {
        return Diags.diagnose(ReferenceRange.Start, Diag, domain, range);
      });
}

void TypeChecker::checkConcurrencyAvailability(SourceRange ReferenceRange,
                                               const DeclContext *ReferenceDC) {
  checkAvailability(
      ReferenceRange,
      ReferenceDC->getASTContext().getBackDeployedConcurrencyAvailability(),
      diag::availability_concurrency_only_version_newer, ReferenceDC);
}

static bool
requiresDeploymentTargetOrEarlier(AvailabilityDomain domain,
                                  const AvailabilityRange &availability,
                                  ASTContext &ctx) {
  if (auto deploymentRange = domain.getDeploymentRange(ctx))
    return deploymentRange->isContainedIn(availability);
  return false;
}

/// Returns the diagnostic to emit for the potentially unavailable decl and sets
/// \p IsError accordingly.
static Diagnostic getPotentialUnavailabilityDiagnostic(
    const ValueDecl *D, const DeclContext *ReferenceDC,
    AvailabilityDomain Domain, const AvailabilityRange &Availability,
    bool WarnBeforeDeploymentTarget, bool &IsError) {
  ASTContext &Context = ReferenceDC->getASTContext();

  if (requiresDeploymentTargetOrEarlier(Domain, Availability, Context)) {
    // The required OS version is at or before the deployment target so this
    // diagnostic should indicate that the decl could be unavailable to clients
    // of the module containing the reference.
    IsError = !WarnBeforeDeploymentTarget;

    auto diag = Diagnostic(diag::availability_decl_only_in_for_clients, D,
                           Domain, Availability.hasMinimumVersion(),
                           Availability, ReferenceDC->getParentModule());
    if (!IsError)
      diag.setBehaviorLimit(DiagnosticBehavior::Warning);
    return diag;
  }

  IsError = true;
  return Diagnostic(diag::availability_decl_only_in, D, Domain,
                    Availability.hasMinimumVersion(), Availability);
}

// Emits a diagnostic for a reference to a declaration that is potentially
// unavailable at the given source location. Returns true if an error diagnostic
// was emitted.
static bool
diagnosePotentialUnavailability(const ValueDecl *D, SourceRange ReferenceRange,
                                const DeclContext *ReferenceDC,
                                AvailabilityDomain Domain,
                                const AvailabilityRange &Availability,
                                bool WarnBeforeDeploymentTarget = false) {
  ASTContext &Context = ReferenceDC->getASTContext();
  if (Context.LangOpts.DisableAvailabilityChecking)
    return false;

  bool IsError;
  {
    auto Diag = Context.Diags.diagnose(
        ReferenceRange.Start, getPotentialUnavailabilityDiagnostic(
                                  D, ReferenceDC, Domain, Availability,
                                  WarnBeforeDeploymentTarget, IsError));

    // Direct a fixit to the error if an existing guard is nearly-correct
    if (fixAvailabilityByNarrowingNearbyVersionCheck(
            ReferenceRange, ReferenceDC, Domain, Availability, Context, Diag))
      return IsError;
  }

  fixAvailability(ReferenceRange, ReferenceDC, Domain, Availability, Context);
  return IsError;
}

/// Emits a diagnostic for a reference to a storage accessor that is
/// potentially unavailable.
static void diagnosePotentialAccessorUnavailability(
    const AccessorDecl *Accessor, SourceRange ReferenceRange,
    const DeclContext *ReferenceDC, AvailabilityDomain Domain,
    const AvailabilityRange &Availability, bool ForInout) {
  ASTContext &Context = ReferenceDC->getASTContext();

  assert(Accessor->isGetterOrSetter());

  auto &diag = ForInout ? diag::availability_inout_accessor_only_in
                        : diag::availability_decl_only_in;

  {
    auto Err =
        Context.Diags.diagnose(ReferenceRange.Start, diag, Accessor,
                               Context.getTargetAvailabilityDomain(),
                               Availability.hasMinimumVersion(), Availability);

    // Direct a fixit to the error if an existing guard is nearly-correct
    if (fixAvailabilityByNarrowingNearbyVersionCheck(
            ReferenceRange, ReferenceDC, Domain, Availability, Context, Err))
      return;
  }

  fixAvailability(ReferenceRange, ReferenceDC, Domain, Availability, Context);
}

static DiagnosticBehavior
behaviorLimitForExplicitUnavailability(
    const RootProtocolConformance *rootConf,
    const DeclContext *fromDC) {
  auto protoDecl = rootConf->getProtocol();

  // Soften errors about unavailable `Sendable` conformances depending on the
  // concurrency checking mode.
  if (protoDecl->isSpecificProtocol(KnownProtocolKind::Sendable)) {
    SendableCheckContext checkContext(fromDC);
    if (auto nominal = rootConf->getType()->getAnyNominal())
      return checkContext.diagnosticBehavior(nominal);

    return checkContext.defaultDiagnosticBehavior();
  }

  return DiagnosticBehavior::Unspecified;
}

/// Emits a diagnostic for a protocol conformance that is potentially
/// unavailable at the given source location.
static bool diagnosePotentialUnavailability(
    const RootProtocolConformance *rootConf, const ExtensionDecl *ext,
    SourceLoc loc, const DeclContext *dc, AvailabilityDomain domain,
    const AvailabilityRange &availability) {
  ASTContext &ctx = dc->getASTContext();
  if (ctx.LangOpts.DisableAvailabilityChecking)
    return false;

  {
    auto type = rootConf->getType();
    auto proto = rootConf->getProtocol()->getDeclaredInterfaceType();
    auto err = ctx.Diags.diagnose(
        loc, diag::conformance_availability_only_version_newer, type, proto,
        domain, availability);

    auto behaviorLimit = behaviorLimitForExplicitUnavailability(rootConf, dc);
    if (behaviorLimit >= DiagnosticBehavior::Warning)
      err.limitBehavior(behaviorLimit);
    else
      err.warnUntilSwiftVersion(6);

    // Direct a fixit to the error if an existing guard is nearly-correct
    if (fixAvailabilityByNarrowingNearbyVersionCheck(loc, dc, domain,
                                                     availability, ctx, err))
      return true;
  }

  fixAvailability(loc, dc, domain, availability, ctx);
  return true;
}

/// Returns the availability attribute indicating deprecation of the
/// declaration is deprecated or null otherwise.
static std::optional<SemanticAvailableAttr> getDeprecated(const Decl *D) {
  auto &Ctx = D->getASTContext();
  if (auto Attr = D->getDeprecatedAttr())
    return Attr;

  if (Ctx.LangOpts.WarnSoftDeprecated) {
    // When -warn-soft-deprecated is specified, treat any declaration that is
    // deprecated in the future as deprecated.
    if (auto Attr = D->getSoftDeprecatedAttr())
      return Attr;
  }

  // Treat extensions methods as deprecated if their extension
  // is deprecated.
  DeclContext *DC = D->getDeclContext();
  if (auto *ED = dyn_cast<ExtensionDecl>(DC)) {
    return getDeprecated(ED);
  }

  return std::nullopt;
}

static void fixItAvailableAttrRename(InFlightDiagnostic &diag,
                                     SourceRange referenceRange,
                                     const ValueDecl *renamedDecl,
                                     StringRef newName, const Expr *call) {
  if (isa<AccessorDecl>(renamedDecl))
    return;

  ParsedDeclName parsed = swift::parseDeclName(newName);
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
    auto *CE = dyn_cast_or_null<CallExpr>(call);
    if (!CE)
      return;

    // Replace the base of the call with the "self argument".
    // We can only do a good job with the fix-it if we have the whole call
    // expression.
    // FIXME: Should we be validating the ContextName in some way?
    unsigned selfIndex = parsed.SelfIndex.value();
    const Expr *selfExpr = nullptr;
    SourceLoc removeRangeStart;
    SourceLoc removeRangeEnd;

    auto *originalArgs = CE->getArgs()->getOriginalArgs();
    size_t numElementsWithinParens = originalArgs->size();
    numElementsWithinParens -= originalArgs->getNumTrailingClosures();
    if (selfIndex >= numElementsWithinParens)
      return;

    if (parsed.IsGetter) {
      if (numElementsWithinParens != 1)
        return;
    } else if (parsed.IsSetter) {
      if (numElementsWithinParens != 2)
        return;
    } else {
      if (parsed.ArgumentLabels.size() != originalArgs->size() - 1)
        return;
    }

    selfExpr = originalArgs->getExpr(selfIndex);

    if (selfIndex + 1 == numElementsWithinParens) {
      if (selfIndex > 0) {
        // Remove from the previous comma to the close-paren (half-open).
        removeRangeStart = originalArgs->getExpr(selfIndex - 1)->getEndLoc();
        removeRangeStart = Lexer::getLocForEndOfToken(sourceMgr,
                                                      removeRangeStart);
      } else {
        // Remove from after the open paren to the close paren (half-open).
        removeRangeStart =
            Lexer::getLocForEndOfToken(sourceMgr, originalArgs->getStartLoc());
      }

      // Prefer the r-paren location, so that we get the right behavior when
      // there's a trailing closure, but handle some implicit cases too.
      removeRangeEnd = originalArgs->getRParenLoc();
      if (removeRangeEnd.isInvalid())
        removeRangeEnd = originalArgs->getEndLoc();

    } else {
      // Remove from the label to the start of the next argument (half-open).
      SourceLoc labelLoc = originalArgs->getLabelLoc(selfIndex);
      if (labelLoc.isValid())
        removeRangeStart = labelLoc;
      else
        removeRangeStart = selfExpr->getStartLoc();

      SourceLoc nextLabelLoc = originalArgs->getLabelLoc(selfIndex + 1);
      if (nextLabelLoc.isValid())
        removeRangeEnd = nextLabelLoc;
      else
        removeRangeEnd = originalArgs->getExpr(selfIndex + 1)->getStartLoc();
    }

    // Avoid later argument label fix-its for this argument.
    if (!parsed.isPropertyAccessor()) {
      Identifier oldLabel = originalArgs->getLabel(selfIndex);
      StringRef oldLabelStr;
      if (!oldLabel.empty())
        oldLabelStr = oldLabel.str();
      parsed.ArgumentLabels.insert(parsed.ArgumentLabels.begin() + selfIndex,
                                    oldLabelStr);
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

    diag.fixItReplace(CE->getFn()->getSourceRange(), selfReplace);

    if (!parsed.isPropertyAccessor())
      diag.fixItRemoveChars(removeRangeStart, removeRangeEnd);

    // Continue on to diagnose any argument label renames.

  } else if (parsed.BaseName == "init" && isa_and_nonnull<CallExpr>(call)) {
    auto *CE = cast<CallExpr>(call);

    // If it is a call to an initializer (rather than a first-class reference):

    if (parsed.isMember()) {
      // replace with a "call" to the type (instead of writing `.init`)
      diag.fixItReplace(CE->getFn()->getSourceRange(), parsed.ContextName);
    } else if (auto *dotCall = dyn_cast<DotSyntaxCallExpr>(CE->getFn())) {
      // if it's a dot call, and the left side is a type (and not `self` or 
      // `super`, for example), just remove the dot and the right side, again 
      // in order to make it a "call" to the type
      if (isa<TypeExpr>(dotCall->getBase())) {
        SourceLoc removeLoc = dotCall->getDotLoc();
        if (removeLoc.isInvalid())
          return;

        diag.fixItRemove(SourceRange(removeLoc, dotCall->getFn()->getEndLoc()));
      }
    } else if (!isa<ConstructorRefCallExpr>(CE->getFn())) {
      return;
    }

    // Continue on to diagnose any constructor argument label renames.

  } else if (parsed.IsSubscript) {
    if (auto *CE = dyn_cast_or_null<CallExpr>(call)) {
      // Renaming from CallExpr to SubscriptExpr. Remove function name and
      // replace parens with square brackets.

      if (auto *DSCE = dyn_cast<DotSyntaxCallExpr>(CE->getFn())) {
        if (DSCE->getBase()->isImplicit()) {
          // If self is implicit, self must be inserted before subscript syntax.
          diag.fixItInsert(CE->getStartLoc(), "self");
        }
      }

      diag.fixItReplace(CE->getFn()->getEndLoc(), "[");
      diag.fixItReplace(CE->getEndLoc(), "]");
    }
  } else {
    // Just replace the base name.
    SmallString<64> baseReplace;

    if (!parsed.ContextName.empty()) {
      baseReplace += parsed.ContextName;
      baseReplace += '.';
    }
    baseReplace += parsed.BaseName;

    if (parsed.IsFunctionName && isa_and_nonnull<SubscriptExpr>(call)) {
      auto *SE = cast<SubscriptExpr>(call);

      // Renaming from SubscriptExpr to CallExpr. Insert function name and
      // replace square brackets with parens.
      diag.fixItReplace(SE->getArgs()->getStartLoc(),
                        ("." + baseReplace.str() + "(").str());
      diag.fixItReplace(SE->getEndLoc(), ")");
    } else {
      bool shouldEmitRenameFixit = true;
      if (auto *CE = dyn_cast_or_null<CallExpr>(call)) {
        SmallString<64> callContextName;
        llvm::raw_svector_ostream name(callContextName);
        if (auto *DCE = dyn_cast<DotSyntaxCallExpr>(CE->getDirectCallee())) {
          if (auto *TE = dyn_cast<TypeExpr>(DCE->getBase())) {
            TE->getTypeRepr()->print(name);
            if (!parsed.ContextName.empty()) {
              // If there is a context in rename function e.g.
              // `Context.function()` and call context is a `DotSyntaxCallExpr`
              // adjust the range so it replaces the base as well.
              referenceRange =
                  SourceRange(TE->getStartLoc(), referenceRange.End);
            }
          }
        }
        // Function names are the same (including context if applicable), so
        // renaming fix-it doesn't need do be produced.
        auto calledValue = CE->getCalledValue(/*skipFunctionConversions=*/true);
        if ((parsed.ContextName.empty() ||
             parsed.ContextName == callContextName) &&
            calledValue && calledValue->getBaseName() == parsed.BaseName) {
          shouldEmitRenameFixit = false;
        }
      }
      if (shouldEmitRenameFixit) {
        if (parsed.IsFunctionName && parsed.ArgumentLabels.empty() &&
            isa<VarDecl>(renamedDecl)) {
          // If we're going from a var to a function with no arguments, emit an
          // empty parameter list.
          baseReplace += "()";
        }

        diag.fixItReplace(referenceRange, baseReplace);
      }
    }
  }

  if (!call || !call->getArgs())
    return;

  auto *originalArgs = call->getArgs()->getOriginalArgs();
  if (parsed.IsGetter) {
    diag.fixItRemove(originalArgs->getSourceRange());
    return;
  }

  if (parsed.IsSetter) {
    const Expr *newValueExpr = nullptr;

    if (originalArgs->size() >= 1) {
      size_t newValueIndex = 0;
      if (parsed.isInstanceMember()) {
        assert(parsed.SelfIndex.value() == 0 ||
               parsed.SelfIndex.value() == 1);
        newValueIndex = !parsed.SelfIndex.value();
      }
      newValueExpr = originalArgs->getExpr(newValueIndex);
    } else {
      newValueExpr = originalArgs->getExpr(0);
    }

    diag.fixItReplaceChars(originalArgs->getStartLoc(),
                           newValueExpr->getStartLoc(), " = ");
    diag.fixItRemoveChars(
        Lexer::getLocForEndOfToken(sourceMgr, newValueExpr->getEndLoc()),
        Lexer::getLocForEndOfToken(sourceMgr, originalArgs->getEndLoc()));
    return;
  }

  if (!parsed.IsFunctionName)
    return;

  SmallVector<Identifier, 4> argumentLabelIDs;
  llvm::transform(parsed.ArgumentLabels, std::back_inserter(argumentLabelIDs),
                  [&ctx](StringRef labelStr) -> Identifier {
                    return labelStr.empty() ? Identifier()
                                            : ctx.getIdentifier(labelStr);
                  });

  // Coerce the `argumentLabelIDs` to the user supplied arguments.
  // e.g:
  //   @available(.., renamed: "new(w:x:y:z:)")
  //   func old(a: Int, b: Int..., c: String="", d: Int=0){}
  //   old(a: 1, b: 2, 3, 4, d: 5)
  // coerce
  //   argumentLabelIDs = {"w", "x", "y", "z"}
  // to
  //   argumentLabelIDs = {"w", "x", "", "", "z"}
  auto I = argumentLabelIDs.begin();

  auto updateLabelsForArg = [&](Expr *expr) -> bool {
    if (I == argumentLabelIDs.end())
      return true;

    if (isa<DefaultArgumentExpr>(expr)) {
      // Defaulted: remove param label of it.
      I = argumentLabelIDs.erase(I);
      return false;
    }

    if (auto *varargExpr = dyn_cast<VarargExpansionExpr>(expr)) {
      if (auto *arrayExpr = dyn_cast<ArrayExpr>(varargExpr->getSubExpr())) {
        auto variadicArgsNum = arrayExpr->getNumElements();
        if (variadicArgsNum == 0) {
          // No arguments: Remove param label of it.
          I = argumentLabelIDs.erase(I);
        } else if (variadicArgsNum == 1) {
          // One argument: Just advance.
          ++I;
        } else {
          ++I;

          // Two or more arguments: Insert empty labels after the first one.
          --variadicArgsNum;
          I = argumentLabelIDs.insert(I, variadicArgsNum, Identifier());
          I += variadicArgsNum;
        }
        return false;
      }
    }

    // Normal: Just advance.
    ++I;
    return false;
  };

  for (auto arg : *call->getArgs()) {
    if (updateLabelsForArg(arg.getExpr()))
      return;
  }

  if (argumentLabelIDs.size() != originalArgs->size()) {
    // Mismatched lengths; give up.
    return;
  }

  // If any of the argument labels are mismatched, perform label correction.
  for (auto i : indices(*originalArgs)) {
    // The argument label of an unlabeled trailing closure is ignored.
    if (originalArgs->isUnlabeledTrailingClosureIndex(i))
      continue;
    if (argumentLabelIDs[i] != originalArgs->getLabel(i)) {
      auto paramContext = parsed.IsSubscript ? ParameterContext::Subscript
                                             : ParameterContext::Call;
      diagnoseArgumentLabelError(ctx, originalArgs, argumentLabelIDs,
                                 paramContext, &diag);
      return;
    }
  }
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

static std::optional<ReplacementDeclKind>
describeRename(ASTContext &ctx, StringRef newName, const ValueDecl *D,
               SmallVectorImpl<char> &nameBuf) {
  ParsedDeclName parsed = swift::parseDeclName(newName);
  if (!parsed)
    return std::nullopt;

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
    return std::nullopt;
  }

  llvm::raw_svector_ostream name(nameBuf);

  if (!parsed.ContextName.empty())
    name << parsed.ContextName << '.';

  if (parsed.IsFunctionName) {
    name << parsed.formDeclName(ctx, (D && isa<SubscriptDecl>(D)));
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

/// Emits a diagnostic for a reference to a declaration that is deprecated.
static void diagnoseIfDeprecated(SourceRange ReferenceRange,
                                 const ExportContext &Where,
                                 const ValueDecl *DeprecatedDecl,
                                 const Expr *Call) {
  auto Attr = getDeprecated(DeprecatedDecl);
  if (!Attr)
    return;

  auto Availability = Where.getAvailability();

  // We match the behavior of clang to not report deprecation warnings
  // inside declarations that are themselves deprecated on all deployment
  // targets.
  if (Availability.isDeprecated()) {
    return;
  }

  auto *ReferenceDC = Where.getDeclContext();
  auto &Context = ReferenceDC->getASTContext();
  if (!Context.LangOpts.DisableAvailabilityChecking) {
    if (Availability.getPlatformRange().isKnownUnreachable()) {
      // Suppress a deprecation warning if the availability checking machinery
      // thinks the reference program location will not execute on any
      // deployment target for the current platform.
      return;
    }
  }

  if (shouldIgnoreDeprecationOfConcurrencyDecl(DeprecatedDecl, ReferenceDC))
    return;

  auto Domain = Attr->getDomain();
  auto DeprecatedRange = Attr->getDeprecatedRange(Context).value();
  auto Message = Attr->getMessage();
  auto NewName = Attr->getRename();
  if (Message.empty() && NewName.empty()) {
    Context.Diags
        .diagnose(ReferenceRange.Start, diag::availability_deprecated,
                  DeprecatedDecl, Attr->isPlatformSpecific(), Domain,
                  DeprecatedRange.hasMinimumVersion(), DeprecatedRange,
                  /*message*/ StringRef())
        .highlight(Attr->getParsedAttr()->getRange());
    return;
  }

  SmallString<32> newNameBuf;
  std::optional<ReplacementDeclKind> replacementDeclKind =
      describeRename(Context, NewName, /*decl*/ nullptr, newNameBuf);
  StringRef newName = replacementDeclKind ? newNameBuf.str() : NewName;

  if (!Message.empty()) {
    EncodedDiagnosticMessage EncodedMessage(Message);
    Context.Diags
        .diagnose(ReferenceRange.Start, diag::availability_deprecated,
                  DeprecatedDecl, Attr->isPlatformSpecific(), Domain,
                  DeprecatedRange.hasMinimumVersion(), DeprecatedRange,
                  EncodedMessage.Message)
        .highlight(Attr->getParsedAttr()->getRange());
  } else {
    unsigned rawReplaceKind = static_cast<unsigned>(
        replacementDeclKind.value_or(ReplacementDeclKind::None));
    Context.Diags
        .diagnose(ReferenceRange.Start, diag::availability_deprecated_rename,
                  DeprecatedDecl, Attr->isPlatformSpecific(), Domain,
                  DeprecatedRange.hasMinimumVersion(), DeprecatedRange,
                  replacementDeclKind.has_value(), rawReplaceKind, newName)
        .highlight(Attr->getParsedAttr()->getRange());
  }

  if (!NewName.empty() && !isa<AccessorDecl>(DeprecatedDecl)) {
    auto renameDiag = Context.Diags.diagnose(
                               ReferenceRange.Start,
                               diag::note_deprecated_rename,
                               newName);
    fixItAvailableAttrRename(renameDiag, ReferenceRange, DeprecatedDecl,
                             NewName, Call);
  }
}

/// Emits a diagnostic for a reference to a conformance that is deprecated.
static bool diagnoseIfDeprecated(SourceLoc loc,
                                 const RootProtocolConformance *rootConf,
                                 const ExtensionDecl *ext,
                                 const ExportContext &where) {
  auto attr = getDeprecated(ext);
  if (!attr)
    return false;

  auto availability = where.getAvailability();

  // We match the behavior of clang to not report deprecation warnings
  // inside declarations that are themselves deprecated on all deployment
  // targets.
  if (availability.isDeprecated()) {
    return false;
  }

  auto *dc = where.getDeclContext();
  auto &ctx = dc->getASTContext();
  if (!ctx.LangOpts.DisableAvailabilityChecking) {
    if (availability.getPlatformRange().isKnownUnreachable()) {
      // Suppress a deprecation warning if the availability checking machinery
      // thinks the reference program location will not execute on any
      // deployment target for the current platform.
      return false;
    }
  }

  auto type = rootConf->getType();
  auto proto = rootConf->getProtocol()->getDeclaredInterfaceType();

  auto domain = attr->getDomain();
  auto deprecatedRange = attr->getDeprecatedRange(ctx).value();
  auto message = attr->getMessage();
  if (message.empty()) {
    ctx.Diags
        .diagnose(loc, diag::conformance_availability_deprecated, type, proto,
                  attr->isPlatformSpecific(), domain,
                  deprecatedRange.hasMinimumVersion(), deprecatedRange,
                  /*message*/ StringRef())
        .highlight(attr->getParsedAttr()->getRange());
    return true;
  }

  EncodedDiagnosticMessage encodedMessage(message);
  ctx.Diags
      .diagnose(loc, diag::conformance_availability_deprecated, type, proto,
                attr->isPlatformSpecific(), domain,
                deprecatedRange.hasMinimumVersion(), deprecatedRange,
                encodedMessage.Message)
      .highlight(attr->getParsedAttr()->getRange());
  return true;
}

void swift::diagnoseOverrideOfUnavailableDecl(ValueDecl *override,
                                              const ValueDecl *base,
                                              SemanticAvailableAttr attr) {
  ASTContext &ctx = override->getASTContext();
  auto &diags = ctx.Diags;
  if (attr.getRename().empty()) {
    EncodedDiagnosticMessage EncodedMessage(attr.getMessage());
    diags.diagnose(override, diag::override_unavailable,
                   override->getBaseName(), EncodedMessage.Message);

    diags.diagnose(base, diag::availability_marked_unavailable, base);
    return;
  }

  // FIXME: [availability] Take an unsatisfied constraint as input instead of
  // recomputing it.
  ExportContext where = ExportContext::forDeclSignature(override);
  auto constraint =
      getAvailabilityConstraintsForDecl(base, where.getAvailability())
          .getPrimaryConstraint();
  if (!constraint)
    return;

  diagnoseExplicitUnavailability(
      base, override->getLoc(), *constraint, where,
      /*Flags*/ std::nullopt,
      [&override, &ctx](InFlightDiagnostic &diag, StringRef rename) {
        ParsedDeclName parsedName = parseDeclName(rename);
        if (!parsedName || parsedName.isPropertyAccessor() ||
            parsedName.isMember() || parsedName.isOperator()) {
          return;
        }

        // Only initializers should be named 'init'.
        if (isa<ConstructorDecl>(override) ^ (parsedName.BaseName == "init")) {
          return;
        }

        if (!parsedName.IsFunctionName) {
          diag.fixItReplace(override->getNameLoc(), parsedName.BaseName);
          return;
        }

        DeclName newName = parsedName.formDeclName(ctx);
        size_t numArgs = override->getName().getArgumentNames().size();
        if (!newName || newName.getArgumentNames().size() != numArgs)
          return;

        fixDeclarationName(diag, override, newName);
      });
}

/// Emit a diagnostic for references to declarations that have been
/// marked as unavailable, either through "unavailable" or "obsoleted:".
static bool diagnoseExplicitUnavailability(
    const ValueDecl *D, SourceRange R, const AvailabilityConstraint &constraint,
    const ExportContext &Where, const Expr *call, DeclAvailabilityFlags Flags) {
  return diagnoseExplicitUnavailability(
      D, R, constraint, Where, Flags,
      [=](InFlightDiagnostic &diag, StringRef rename) {
        fixItAvailableAttrRename(diag, R, D, rename, call);
      });
}

bool shouldHideDomainNameForConstraintDiagnostic(
    const AvailabilityConstraint &constraint) {
  switch (constraint.getDomain().getKind()) {
  case AvailabilityDomain::Kind::Universal:
  case AvailabilityDomain::Kind::Embedded:
  case AvailabilityDomain::Kind::Custom:
  case AvailabilityDomain::Kind::PackageDescription:
    return true;
  case AvailabilityDomain::Kind::Platform:
    return false;
  case AvailabilityDomain::Kind::SwiftLanguage:
    switch (constraint.getReason()) {
    case AvailabilityConstraint::Reason::UnconditionallyUnavailable:
    case AvailabilityConstraint::Reason::UnavailableForDeployment:
      return false;
    case AvailabilityConstraint::Reason::PotentiallyUnavailable:
    case AvailabilityConstraint::Reason::Obsoleted:
      return true;
    }
  }
}

bool diagnoseExplicitUnavailability(SourceLoc loc,
                                    const AvailabilityConstraint &constraint,
                                    const RootProtocolConformance *rootConf,
                                    const ExtensionDecl *ext,
                                    const ExportContext &where,
                                    bool warnIfConformanceUnavailablePreSwift6,
                                    bool preconcurrency) {
  if (!constraint.isUnavailable())
    return false;

  // Invertible protocols are never unavailable.
  if (rootConf->getProtocol()->getInvertibleProtocolKind())
    return false;

  ASTContext &ctx = ext->getASTContext();
  auto &diags = ctx.Diags;

  auto type = rootConf->getType();
  auto proto = rootConf->getProtocol()->getDeclaredInterfaceType();
  auto domainAndRange = constraint.getDomainAndRange(ctx);
  auto attr = constraint.getAttr();

  // Downgrade unavailable Sendable conformance diagnostics where
  // appropriate.
  auto behavior =
      behaviorLimitForExplicitUnavailability(rootConf, where.getDeclContext());

  EncodedDiagnosticMessage EncodedMessage(attr.getMessage());
  diags
      .diagnose(loc, diag::conformance_availability_unavailable, type, proto,
                shouldHideDomainNameForConstraintDiagnostic(constraint),
                domainAndRange.getDomain(), EncodedMessage.Message)
      .limitBehaviorWithPreconcurrency(behavior, preconcurrency)
      .warnUntilSwiftVersionIf(warnIfConformanceUnavailablePreSwift6, 6);

  switch (constraint.getReason()) {
  case AvailabilityConstraint::Reason::UnconditionallyUnavailable:
    diags
        .diagnose(ext, diag::conformance_availability_marked_unavailable, type,
                  proto)
        .highlight(attr.getParsedAttr()->getRange());
    break;
  case AvailabilityConstraint::Reason::UnavailableForDeployment:
    diags.diagnose(ext, diag::conformance_availability_introduced_in_version,
                   type, proto, domainAndRange.getDomain(),
                   domainAndRange.getRange());
    break;
  case AvailabilityConstraint::Reason::Obsoleted:
    diags
        .diagnose(ext, diag::conformance_availability_obsoleted, type, proto,
                  domainAndRange.getDomain(), domainAndRange.getRange())
        .highlight(attr.getParsedAttr()->getRange());
    break;
  case AvailabilityConstraint::Reason::PotentiallyUnavailable:
    llvm_unreachable("unexpected constraint");
  }
  return true;
}

std::optional<AvailabilityConstraint>
swift::getUnsatisfiedAvailabilityConstraint(const Decl *decl,
                                            const DeclContext *referenceDC,
                                            SourceLoc referenceLoc) {
  AvailabilityConstraintFlags flags;

  // In implicit code, allow references to universally unavailable declarations
  // as long as the context is also universally unavailable.
  if (referenceLoc.isInvalid())
    flags |= AvailabilityConstraintFlag::
        AllowUniversallyUnavailableInCompatibleContexts;

  return getAvailabilityConstraintsForDecl(
             decl, AvailabilityContext::forLocation(referenceLoc, referenceDC),
             flags)
      .getPrimaryConstraint();
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
  if (param.hasLabel() || param.isVariadic() || param.isInOut())
    return false;

  auto inputTy = param.getPlainType()->getAs<BoundGenericStructType>();
  if (!inputTy)
    return false;

  auto genericArgs = inputTy->getGenericArgs();
  if (genericArgs.size() != 1)
    return false;

  // The subscripts taking T<Int> do not return Substring, and our
  // special fixit does not help here.
  auto nominalTypeParam = genericArgs[0]->getAs<NominalType>();
  if (!nominalTypeParam)
    return false;

  if (nominalTypeParam->isInt())
    return false;

  auto resultTy = fnTy->getResult()->getAs<NominalType>();
  if (!resultTy)
    return false;

  return resultTy->isString();
}

static bool diagnoseParameterizedProtocolAvailability(
    SourceRange ReferenceRange, const DeclContext *ReferenceDC) {
  return TypeChecker::checkAvailability(
      ReferenceRange,
      ReferenceDC->getASTContext().getParameterizedExistentialAvailability(),
      diag::availability_parameterized_protocol_only_version_newer,
      ReferenceDC);
}

static bool diagnoseIsolatedAnyAvailability(
    SourceRange ReferenceRange, const DeclContext *ReferenceDC) {
  return TypeChecker::checkAvailability(
      ReferenceRange,
      ReferenceDC->getASTContext().getIsolatedAnyAvailability(),
      diag::availability_isolated_any_only_version_newer,
      ReferenceDC);
}

static bool diagnoseTypedThrowsAvailability(
    SourceRange ReferenceRange, const DeclContext *ReferenceDC) {
  return TypeChecker::checkAvailability(
      ReferenceRange,
      ReferenceDC->getASTContext().getTypedThrowsAvailability(),
      diag::availability_typed_throws_only_version_newer,
      ReferenceDC);
}

/// Make sure the generic arguments conform to all known invertible protocols.
/// Runtimes prior to NoncopyableGenerics do not check if any of the
/// generic arguments conform to Copyable/Escapable during dynamic casts.
/// But a dynamic cast *needs* to check if the generic arguments conform,
/// to determine if the cast should be permitted at all. For example:
///
///    struct X<T> {}
///    extension X: P where T: Y {}
///
///     func f<Y: ~Copyable>(...) {
///       let x: X<Y> = ...
///       _ = x as? any P   // <- cast should fail
///     }
///
/// The dynamic cast here must fail because Y does not conform to Copyable,
/// thus X<Y> doesn't conform to P!
///
/// \param boundTy The generic type with its generic arguments.
/// \returns the invertible protocol for which a conformance is missing in
///          one of the generic arguments, or none if all are present for
///          every generic argument.
static std::optional<InvertibleProtocolKind> checkGenericArgsForInvertibleReqs(
    BoundGenericType *boundTy) {
  for (auto arg : boundTy->getGenericArgs()) {
    for (auto ip : InvertibleProtocolSet::allKnown()) {
      switch (ip) {
      case InvertibleProtocolKind::Copyable:
        if (arg->isNoncopyable())
          return ip;
        break;
      case InvertibleProtocolKind::Escapable:
        if (!arg->isEscapable())
          return ip;
      }
    }
  }
  return std::nullopt;
}

/// Older runtimes won't check for required invertible protocol conformances
/// at runtime during a cast.
///
/// \param srcType the source or initial type of the cast
/// \param refLoc source location of the cast
/// \param refDC decl context in which the cast occurs
/// \return true if diagnosed
static bool checkInverseGenericsCastingAvailability(Type srcType,
                                                    SourceRange refLoc,
                                                    const DeclContext *refDC) {
  if (!srcType) return false;

  auto type = srcType->getCanonicalType();

  if (auto boundTy = dyn_cast<BoundGenericType>(type)) {
    if (auto missing = checkGenericArgsForInvertibleReqs(boundTy)) {
      std::optional<Diag<AvailabilityDomain, AvailabilityRange>> diag;
      switch (*missing) {
      case InvertibleProtocolKind::Copyable:
        diag =
            diag::availability_copyable_generics_casting_only_version_newer;
        break;
      case InvertibleProtocolKind::Escapable:
        diag =
            diag::availability_escapable_generics_casting_only_version_newer;
        break;
      }

      // Enforce the availability restriction.
      return TypeChecker::checkAvailability(
          refLoc,
          refDC->getASTContext().getNoncopyableGenericsAvailability(),
          *diag,
          refDC);
    }
  }
  return false;
}

static bool checkTypeMetadataAvailabilityInternal(CanType type,
                                                  SourceRange refLoc,
                                                  const DeclContext *refDC) {
  return type.findIf([&](CanType type) {
    if (isa<ParameterizedProtocolType>(type)) {
      return diagnoseParameterizedProtocolAvailability(refLoc, refDC);
    } else if (auto fnType = dyn_cast<AnyFunctionType>(type)) {
      auto isolation = fnType->getIsolation();
      if (isolation.isErased())
        return diagnoseIsolatedAnyAvailability(refLoc, refDC);
      if (fnType.getThrownError())
        return diagnoseTypedThrowsAvailability(refLoc, refDC);
    }
    return false;
  });
}

/// Check whether type metadata is available for the given type (and its
/// component types).
bool swift::checkTypeMetadataAvailability(Type type,
                                          SourceRange refLoc,
                                          const DeclContext *refDC) {
  if (!type) return false;
  return checkTypeMetadataAvailabilityInternal(type->getCanonicalType(),
                                               refLoc, refDC);
}

/// Check whether type metadata is available for the given type, given that
/// it is the operand of a dynamic cast or existential conversion.
static bool checkTypeMetadataAvailabilityForConverted(Type refType,
                                                      SourceRange refLoc,
                                                      const DeclContext *refDC) {
  if (!refType) return false;

  auto type = refType->getCanonicalType();

  // SILGen emits these conversions by opening the outermost level of
  // existential, so we never need to emit type metadata for an
  // existential in such a position.  We necessarily have type metadata
  // for the dynamic type of the existential, so there's nothing to check
  // there.
  if (type.isAnyExistentialType()) return false;

  if (checkTypeMetadataAvailabilityInternal(type, refLoc, refDC))
    return true;

  if (checkInverseGenericsCastingAvailability(type, refLoc, refDC))
    return true;

  return false;
}

namespace {

class CheckConversionAvailability {
  SourceRange refLoc;
  const DeclContext *refDC;

public:
  CheckConversionAvailability(SourceRange refLoc, const DeclContext *refDC)
    : refLoc(refLoc), refDC(refDC) {}

  void check(CanType srcType, CanType destType);
  void checkFunction(CanAnyFunctionType srcType, CanAnyFunctionType destType);

private:
  void checkTuple(CanTupleType srcType, CanTupleType destType);
};

} // end anonymous namespace

void CheckConversionAvailability::check(CanType srcType, CanType destType) {
  if (srcType == destType)
    return;

  // We care about specific optionality structure here: converting
  // `(any P<T>)?` to `Any?` doesn't require metadata for `any P<T>`,
  // but converting `(any P<T>)?` to non-optional `Any` does.
  if (auto destObjectType = destType.getOptionalObjectType()) {
    // optional -> optional conversion
    if (auto srcObjectType = srcType.getOptionalObjectType()) {
      check(srcObjectType, destObjectType);
    // optional injection
    } else {
      check(srcType, destObjectType);
    }

  // Conversions to existential types require type metadata for the
  // source type, except that we look into existentials.
  } else if (destType.isAnyExistentialType()) {
    checkTypeMetadataAvailabilityForConverted(srcType, refLoc, refDC);

  // Conversions between function types perform a bunch of recursive
  // conversions.
  } else if (auto destFnType = dyn_cast<AnyFunctionType>(destType)) {
    if (auto srcFnType = dyn_cast<AnyFunctionType>(srcType)) {
      checkFunction(srcFnType, destFnType);
    }

  // Conversions between tuple types perform a bunch of recursive
  // conversions.
  } else if (auto destTupleType = dyn_cast<TupleType>(destType)) {
    if (auto srcTupleType = dyn_cast<TupleType>(srcType)) {
      checkTuple(srcTupleType, destTupleType);
    }

  // Conversions of things containing pack expansions convert the
  // expansion patterns.  We won't print the types we get here, so
  // we can ignore them.
  } else if (auto destExpType = dyn_cast<PackExpansionType>(destType)) {
    if (auto srcExpType = dyn_cast<PackExpansionType>(srcType)) {
      check(srcExpType.getPatternType(), destExpType.getPatternType());
    }
  }
}

void CheckConversionAvailability::checkFunction(CanAnyFunctionType srcType,
                                                CanAnyFunctionType destType) {
  // Results are covariantly converted.
  check(srcType.getResult(), destType.getResult());

  // Defensively ignored invalid conversion structure.
  if (srcType->getNumParams() != destType->getNumParams())
    return;

  // Parameters are contravariantly converted.
  for (auto i : range(srcType->getNumParams())) {
    const auto &srcParam = srcType.getParams()[i];
    const auto &destParam = destType.getParams()[i];

    // Note the reversal for contravariance.
    check(destParam.getParameterType(), srcParam.getParameterType());
  }
}

void CheckConversionAvailability::checkTuple(CanTupleType srcType,
                                             CanTupleType destType) {
  // Handle invalid structure appropriately.
  if (srcType->getNumElements() != destType->getNumElements())
    return;

  for (auto i : range(srcType->getNumElements())) {
    check(srcType.getElementType(i), destType.getElementType(i));
  }
}

static void checkFunctionConversionAvailability(Type srcType, Type destType,
                                                SourceRange refLoc,
                                                const DeclContext *refDC) {
  if (srcType && destType) {
    auto srcFnType = cast<AnyFunctionType>(srcType->getCanonicalType());
    auto destFnType = cast<AnyFunctionType>(destType->getCanonicalType());

    CheckConversionAvailability(refLoc, refDC)
      .checkFunction(srcFnType, destFnType);
  }
}

bool diagnoseExplicitUnavailability(
    const ValueDecl *D, SourceRange R, const AvailabilityConstraint &constraint,
    const ExportContext &Where, DeclAvailabilityFlags Flags,
    llvm::function_ref<void(InFlightDiagnostic &, StringRef)>
        attachRenameFixIts) {
  if (!constraint.isUnavailable())
    return false;

  auto Attr = constraint.getAttr();
  if (Attr.getDomain().isSwiftLanguage() && !Attr.isVersionSpecific()) {
    if (shouldAllowReferenceToUnavailableInSwiftDeclaration(D, Where))
      return false;
  }

  SourceLoc Loc = R.Start;
  ASTContext &ctx = D->getASTContext();
  auto &diags = ctx.Diags;
  auto domainAndRange = constraint.getDomainAndRange(ctx);

  // TODO: Consider removing this.
  // ObjC keypaths components weren't checked previously, so errors are demoted
  // to warnings to avoid source breakage. In some cases unavailable or
  // obsolete decls still map to valid ObjC runtime names, so behave correctly
  // at runtime, even though their use would produce an error outside of a
  // #keyPath expression.
  auto limit = Flags.contains(DeclAvailabilityFlag::ForObjCKeyPath)
                  ? DiagnosticBehavior::Warning
                  : DiagnosticBehavior::Unspecified;

  auto message = Attr.getMessage();
  auto rename = Attr.getRename();
  if (!rename.empty()) {
    SmallString<32> newNameBuf;
    std::optional<ReplacementDeclKind> replaceKind =
        describeRename(ctx, Attr.getRename(), D, newNameBuf);
    unsigned rawReplaceKind = static_cast<unsigned>(
        replaceKind.value_or(ReplacementDeclKind::None));
    StringRef newName = replaceKind ? newNameBuf.str() : rename;
    EncodedDiagnosticMessage EncodedMessage(message);
    auto diag = diags.diagnose(Loc, diag::availability_decl_unavailable_rename,
                               D, replaceKind.has_value(), rawReplaceKind,
                               newName, EncodedMessage.Message);
    diag.limitBehavior(limit);
    attachRenameFixIts(diag, rename);
  } else if (isSubscriptReturningString(D, ctx)) {
    diags.diagnose(Loc, diag::availability_string_subscript_migration)
      .highlight(R)
      .fixItInsert(R.Start, "String(")
      .fixItInsertAfter(R.End, ")");

    // Skip the note emitted below.
    return true;
  } else {
    EncodedDiagnosticMessage EncodedMessage(message);
    diags
        .diagnose(Loc, diag::availability_decl_unavailable, D,
                  shouldHideDomainNameForConstraintDiagnostic(constraint),
                  domainAndRange.getDomain(), EncodedMessage.Message)
        .highlight(R)
        .limitBehavior(limit);
  }

  auto sourceRange = Attr.getParsedAttr()->getRange();
  switch (constraint.getReason()) {
  case AvailabilityConstraint::Reason::UnconditionallyUnavailable:
    diags.diagnose(D, diag::availability_marked_unavailable, D)
        .highlight(sourceRange);
    break;
  case AvailabilityConstraint::Reason::UnavailableForDeployment:
    diags
        .diagnose(D, diag::availability_introduced_in_version, D,
                  domainAndRange.getDomain(), domainAndRange.getRange())
        .highlight(sourceRange);
    break;
  case AvailabilityConstraint::Reason::Obsoleted:
    diags
        .diagnose(D, diag::availability_obsoleted, D,
                  domainAndRange.getDomain(), domainAndRange.getRange())
        .highlight(sourceRange);
    break;
  case AvailabilityConstraint::Reason::PotentiallyUnavailable:
    llvm_unreachable("unexpected constraint");
    break;
  }
  return true;
}

namespace {
class ExprAvailabilityWalker : public BaseDiagnosticWalker {
  /// Models how member references will translate to accessor usage. This is
  /// used to diagnose the availability of individual accessors that may be
  /// called by the expression being checked.
  enum class MemberAccessContext : unsigned {
    /// The starting access context for the root of any expression tree. In this
    /// context, a member access will call the get accessor only.
    Default,

    /// The access context for expressions rooted in a LoadExpr. A LoadExpr
    /// coerces l-values to r-values and thus member access inside of a LoadExpr
    /// will only invoke get accessors.
    Load,

    /// The access context for the outermost member accessed in the expression
    /// tree on the left-hand side of an assignment. Only the set accessor will
    /// be invoked on this member.
    Assignment,

    /// The access context for expressions in which member is being read and
    /// then written back to. For example, a writeback will occur inside of an
    /// InOutExpr. Both the get and set accessors may be called in this context.
    Writeback
  };

  ASTContext &Context;
  MemberAccessContext AccessContext = MemberAccessContext::Default;
  SmallVector<const Expr *, 16> ExprStack;
  SmallVector<bool, 4> PreconcurrencyCalleeStack;
  const ExportContext &Where;

public:
  explicit ExprAvailabilityWalker(const ExportContext &Where)
    : Context(Where.getDeclContext()->getASTContext()), Where(Where) {}

  PreWalkAction walkToArgumentPre(const Argument &Arg) override {
    // Arguments should be walked in their own member access context which
    // starts out read-only by default.
    walkInContext(Arg.getExpr(), MemberAccessContext::Default);
    return Action::SkipChildren();
  }

  PreWalkResult<Expr *> walkToExprPre(Expr *E) override {
    auto *DC = Where.getDeclContext();

    ExprStack.push_back(E);

    if (isa<ApplyExpr>(E)) {
      PreconcurrencyCalleeStack.push_back(
          hasReferenceToPreconcurrencyDecl(E));
    }

    if (auto DR = dyn_cast<DeclRefExpr>(E)) {
      diagnoseDeclRefAvailability(DR->getDeclRef(), DR->getSourceRange(),
                                  getEnclosingApplyExpr(), std::nullopt);
      maybeDiagStorageAccess(DR->getDecl(), DR->getSourceRange(), DC);
    }
    if (auto MR = dyn_cast<MemberRefExpr>(E)) {
      walkMemberRef(MR);
      return Action::SkipChildren(E);
    }
    if (auto OCDR = dyn_cast<OtherConstructorDeclRefExpr>(E))
      diagnoseDeclRefAvailability(OCDR->getDeclRef(),
                                  OCDR->getConstructorLoc().getSourceRange(),
                                  getEnclosingApplyExpr());
    if (auto DMR = dyn_cast<DynamicMemberRefExpr>(E))
      diagnoseDeclRefAvailability(DMR->getMember(),
                                  DMR->getNameLoc().getSourceRange(),
                                  getEnclosingApplyExpr());
    if (auto DS = dyn_cast<DynamicSubscriptExpr>(E))
      diagnoseDeclRefAvailability(DS->getMember(), DS->getSourceRange());
    if (auto S = dyn_cast<SubscriptExpr>(E)) {
      if (S->hasDecl()) {
        diagnoseDeclRefAvailability(S->getDecl(), S->getSourceRange(), S);
        maybeDiagStorageAccess(S->getDecl().getDecl(), S->getSourceRange(), DC);
        PreconcurrencyCalleeStack.push_back(
            hasReferenceToPreconcurrencyDecl(S));
      }
    }

    if (auto *LE = dyn_cast<LiteralExpr>(E)) {
      if (auto literalType = LE->getType()) {
        // Check availability of the type produced by implicit literal
        // initializer.
        if (auto *nominalDecl = literalType->getAnyNominal()) {
          diagnoseDeclAvailability(nominalDecl, LE->getSourceRange(),
                                   /*call=*/nullptr, Where);
        }
      }
      diagnoseDeclRefAvailability(LE->getInitializer(), LE->getSourceRange());
    }

    // Diagnose availability for any features used in a regex literal.
    if (auto *RE = dyn_cast<RegexLiteralExpr>(E)) {
      for (auto &feature : RE->getPatternFeatures()) {
        auto featureKind = feature.getKind();
        TypeChecker::checkAvailability(
            RE->getSourceRange(), featureKind.getAvailability(Context),
            Where.getDeclContext(),
            [&](AvailabilityDomain domain, AvailabilityRange range) {
              auto sourceRange = feature.getRange();
              auto diag = Context.Diags.diagnose(
                  sourceRange.getStart(), diag::regex_feature_unavailable,
                  featureKind.getDescription(Context), domain, range);
              diag.highlightChars(sourceRange);
              return diag;
            });
      }
    }

    if (auto *CE = dyn_cast<CollectionExpr>(E)) {
      // Diagnose availability of implicit collection literal initializers.
      diagnoseDeclRefAvailability(CE->getInitializer(), CE->getSourceRange());
    }

    if (auto *FCE = dyn_cast<FunctionConversionExpr>(E)) {
      checkFunctionConversionAvailability(FCE->getSubExpr()->getType(),
                                          FCE->getType(),
                                          FCE->getLoc(),
                                          Where.getDeclContext());
    }
    if (auto KP = dyn_cast<KeyPathExpr>(E)) {
      maybeDiagKeyPath(KP);
    }
    if (auto A = dyn_cast<AssignExpr>(E)) {
      // Attempting to assign to a @preconcurrency declaration should
      // downgrade Sendable conformance mismatches to warnings.
      PreconcurrencyCalleeStack.push_back(
        hasReferenceToPreconcurrencyDecl(A->getDest()));

      walkAssignExpr(A);
      return Action::SkipChildren(E);
    }
    if (auto IO = dyn_cast<InOutExpr>(E)) {
      walkInOutExpr(IO);
      return Action::SkipChildren(E);
    }
    if (auto T = dyn_cast<TypeExpr>(E)) {
      if (!T->isImplicit()) {
        diagnoseTypeAvailability(T->getTypeRepr(), T->getType(), E->getLoc(),
                                 Where);
      }
    }
    if (auto CE = dyn_cast<ClosureExpr>(E)) {
      for (auto *param : *CE->getParameters()) {
        diagnoseTypeAvailability(param->getTypeRepr(), param->getInterfaceType(),
                                 E->getLoc(), Where);
      }
      diagnoseTypeAvailability(CE->hasExplicitResultType()
                               ? CE->getExplicitResultTypeRepr()
                               : nullptr,
                               CE->getResultType(), E->getLoc(), Where);
    }
    if (AbstractClosureExpr *closure = dyn_cast<AbstractClosureExpr>(E)) {
      walkAbstractClosure(closure);
      return Action::SkipChildren(E);
    }

    if (auto CE = dyn_cast<ExplicitCastExpr>(E)) {
      if (!isa<CoerceExpr>(CE)) {
        SourceLoc loc = CE->getCastTypeRepr() ? CE->getCastTypeRepr()->getLoc()
                                              : E->getLoc();
        checkTypeMetadataAvailability(CE->getCastType(), loc,
                                      Where.getDeclContext());
        checkTypeMetadataAvailabilityForConverted(CE->getSubExpr()->getType(),
                                                  loc, Where.getDeclContext());
      }

      diagnoseTypeAvailability(CE->getCastTypeRepr(), CE->getCastType(),
                               E->getLoc(), Where);
    }
    
    if (auto EE = dyn_cast<ErasureExpr>(E)) {
      checkTypeMetadataAvailability(EE->getSubExpr()->getType(),
                                    EE->getLoc(), Where.getDeclContext());
      checkInverseGenericsCastingAvailability(EE->getSubExpr()->getType(),
                                              EE->getLoc(),
                                              Where.getDeclContext());

      bool preconcurrency = false;
      if (!PreconcurrencyCalleeStack.empty()) {
        preconcurrency = PreconcurrencyCalleeStack.back();
      }

      for (ProtocolConformanceRef C : EE->getConformances()) {
        diagnoseConformanceAvailability(E->getLoc(), C, Where, Type(), Type(),
                                        /*useConformanceAvailabilityErrorsOpt=*/true,
                                        /*preconcurrency=*/preconcurrency);
      }
    }

    if (auto UTO = dyn_cast<UnderlyingToOpaqueExpr>(E)) {
      diagnoseSubstitutionMapAvailability(
          UTO->getLoc(), UTO->substitutions, Where);
    }

    if (auto ME = dyn_cast<MacroExpansionExpr>(E)) {
      diagnoseDeclRefAvailability(
          ME->getMacroRef(), ME->getMacroNameLoc().getSourceRange());
    }

    if (auto LE = dyn_cast<LoadExpr>(E)) {
      walkLoadExpr(LE);
      return Action::SkipChildren(E);
    }

    return Action::Continue(E);
  }

  PostWalkResult<Expr *> walkToExprPost(Expr *E) override {
    assert(ExprStack.back() == E);
    ExprStack.pop_back();

    if (isa<ApplyExpr>(E)) {
      PreconcurrencyCalleeStack.pop_back();
    }

    return Action::Continue(E);
  }

  PreWalkResult<Stmt *> walkToStmtPre(Stmt *S) override {
    // We need to recursively call diagnoseExprAvailability for any
    // sub-expressions in the statement since the availability context may
    // differ, e.g for things like `guard #available(...)`.
    class StmtRecurseWalker : public BaseDiagnosticWalker {
      DeclContext *DC;

    public:
      StmtRecurseWalker(DeclContext *DC) : DC(DC) {}

      PreWalkResult<Expr *> walkToExprPre(Expr *E) override {
        diagnoseExprAvailability(E, DC);
        return Action::SkipNode(E);
      }
    };
    StmtRecurseWalker W(Where.getDeclContext());
    S->walk(W);
    return Action::SkipNode(S);
  }

  bool
  diagnoseDeclRefAvailability(ConcreteDeclRef declRef, SourceRange R,
                              const Expr *call = nullptr,
                              DeclAvailabilityFlags flags = std::nullopt) const;

private:
  bool diagnoseIncDecRemoval(const ValueDecl *D, SourceRange R) const;
  bool diagnoseMemoryLayoutMigration(const ValueDecl *D, SourceRange R,
                                     SemanticAvailableAttr,
                                     const ApplyExpr *call) const;

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
             isa<BindOptionalExpr>(parents[idx]) || // f?(a)
             isa<FunctionConversionExpr>(parents[idx]));

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
    walkInContext(Dest, MemberAccessContext::Assignment);

    // Check RHS in getter context
    Expr *Source = E->getSrc();
    if (!Source) {
      return;
    }
    walkInContext(Source, MemberAccessContext::Default);
  }

  /// Walk a load expression, checking for availability.
  void walkLoadExpr(LoadExpr *E) {
    walkInContext(E->getSubExpr(), MemberAccessContext::Load);
  }

  /// Walk a member reference expression, checking for availability.
  void walkMemberRef(MemberRefExpr *E) {
    // Walk the base. If the access context is currently `Assignment`, then we
    // must be diagnosing the destination of an assignment. When recursing,
    // diagnose any remaining member refs in a `Writeback` context, since
    // there is a writeback occurring through them as a result of the
    // assignment.
    //
    //   someVar.x.y = 1
    //           │ ╰─ MemberAccessContext::Assignment
    //           ╰─── MemberAccessContext::Writeback
    //
    MemberAccessContext accessContext =
        (AccessContext == MemberAccessContext::Assignment)
            ? MemberAccessContext::Writeback
            : AccessContext;
    walkInContext(E->getBase(), accessContext);

    ConcreteDeclRef DR = E->getMember();
    // Diagnose for the member declaration itself.
    if (diagnoseDeclRefAvailability(DR, E->getNameLoc().getSourceRange(),
                                    getEnclosingApplyExpr(), std::nullopt))
      return;

    // Diagnose for appropriate accessors, given the access context.
    auto *DC = Where.getDeclContext();
    maybeDiagStorageAccess(DR.getDecl(), E->getSourceRange(), DC);
  }

  /// Walk a keypath expression, checking all of its components for
  /// availability.
  void maybeDiagKeyPath(KeyPathExpr *KP) {
    auto flags = DeclAvailabilityFlags();
    auto declContext = Where.getDeclContext();
    if (KP->isObjC())
      flags = DeclAvailabilityFlag::ForObjCKeyPath;

    for (auto &component : KP->getComponents()) {
      switch (component.getKind()) {
      case KeyPathExpr::Component::Kind::Member:
      case KeyPathExpr::Component::Kind::Subscript: {
        auto decl = component.getDeclRef();
        auto loc = component.getLoc();
        auto range = component.getSourceRange();
        if (diagnoseDeclRefAvailability(decl, loc, nullptr, flags))
          break;
        maybeDiagStorageAccess(decl.getDecl(), range, declContext);
        break;
      }

      case KeyPathExpr::Component::Kind::TupleElement:
        break;

      case KeyPathExpr::Component::Kind::Invalid:
      case KeyPathExpr::Component::Kind::UnresolvedMember:
      case KeyPathExpr::Component::Kind::UnresolvedSubscript:
      case KeyPathExpr::Component::Kind::UnresolvedApply:
      case KeyPathExpr::Component::Kind::Apply:
      case KeyPathExpr::Component::Kind::OptionalChain:
      case KeyPathExpr::Component::Kind::OptionalWrap:
      case KeyPathExpr::Component::Kind::OptionalForce:
      case KeyPathExpr::Component::Kind::Identity:
      case KeyPathExpr::Component::Kind::DictionaryKey:
      case KeyPathExpr::Component::Kind::CodeCompletion:
        break;
      }
    }
  }

  /// Walk an inout expression, checking for availability.
  void walkInOutExpr(InOutExpr *E) {
    // Typically an InOutExpr should begin a `Writeback` context. However,
    // inside a LoadExpr this transition is suppressed since the entire
    // expression is being coerced to an r-value.
    auto accessContext = AccessContext != MemberAccessContext::Load
                             ? MemberAccessContext::Writeback
                             : AccessContext;
    walkInContext(E->getSubExpr(), accessContext);
  }

  /// Walk an abstract closure expression, checking for availability
  void walkAbstractClosure(AbstractClosureExpr *closure) {
    // Do the walk with the closure set as the decl context of the 'where'
    auto where = ExportContext::forFunctionBody(closure, closure->getStartLoc());
    if (where.isImplicit())
      return;
    ExprAvailabilityWalker walker(where);

    // Manually dive into the body
    closure->getBody()->walk(walker);

    return;
  }

  /// Walk the given expression in the member access context.
  void walkInContext(Expr *E, MemberAccessContext AccessContext) {
    llvm::SaveAndRestore<MemberAccessContext>
      C(this->AccessContext, AccessContext);
    E->walk(*this);
  }

  /// Emit diagnostics, if necessary, for accesses to storage where
  /// the accessor for the AccessContext is not available.
  void maybeDiagStorageAccess(const ValueDecl *VD,
                              SourceRange ReferenceRange,
                              const DeclContext *ReferenceDC) const {
    if (Context.LangOpts.DisableAvailabilityChecking)
      return;

    auto *D = dyn_cast<AbstractStorageDecl>(VD);
    if (!D)
      return;

    if (!D->requiresOpaqueAccessors()) {
      return;
    }

    // Check availability of accessor functions.
    // TODO: if we're talking about an inlineable storage declaration,
    // this probably needs to be refined to not assume that the accesses are
    // specifically using the getter/setter.
    switch (AccessContext) {
    case MemberAccessContext::Default:
    case MemberAccessContext::Load:
      diagAccessorAvailability(D->getOpaqueAccessor(AccessorKind::Get),
                               ReferenceRange, ReferenceDC, std::nullopt);
      break;

    case MemberAccessContext::Assignment:
      diagAccessorAvailability(D->getOpaqueAccessor(AccessorKind::Set),
                               ReferenceRange, ReferenceDC, std::nullopt);
      break;

    case MemberAccessContext::Writeback:
      diagAccessorAvailability(D->getOpaqueAccessor(AccessorKind::Get),
                               ReferenceRange, ReferenceDC,
                               DeclAvailabilityFlag::ForInout);

      diagAccessorAvailability(D->getOpaqueAccessor(AccessorKind::Set),
                               ReferenceRange, ReferenceDC,
                               DeclAvailabilityFlag::ForInout);
      break;
    }
  }

  /// Emit a diagnostic, if necessary for a potentially unavailable accessor.
  void diagAccessorAvailability(AccessorDecl *D, SourceRange ReferenceRange,
                                const DeclContext *ReferenceDC,
                                DeclAvailabilityFlags Flags) const {
    if (!D)
      return;

    Flags &= DeclAvailabilityFlag::ForInout;
    Flags |= DeclAvailabilityFlag::ContinueOnPotentialUnavailability;
    if (diagnoseDeclAvailability(D, ReferenceRange, /*call*/ nullptr, Where,
                                 Flags))
      return;
  }

  /// Check whether the given expression references any
  /// @preconcurrency declarations.
  /// Calls, subscripts, member references can have @preconcurrency
  /// declarations at any point in their base chain.
  bool hasReferenceToPreconcurrencyDecl(Expr *expr) {
    if (auto declRef = expr->getReferencedDecl()) {
      if (declRef.getDecl()->preconcurrency())
        return true;
    }

    if (auto *selfApply = dyn_cast<SelfApplyExpr>(expr)) {
      if (hasReferenceToPreconcurrencyDecl(selfApply->getFn()))
        return true;

      // Base could be a preconcurrency declaration i.e.
      //
      // @preconcurrency var x: [any Sendable]
      // x.append(...)
      //
      // If thought `append` might not be `@preconcurrency`
      // the "base" is.
      return hasReferenceToPreconcurrencyDecl(selfApply->getBase());
    }

    if (auto *LE = dyn_cast<LookupExpr>(expr)) {
      // If subscript itself is not @preconcurrency, it's base could be.
      return hasReferenceToPreconcurrencyDecl(LE->getBase());
    }

    if (auto *apply = dyn_cast<ApplyExpr>(expr))
      return hasReferenceToPreconcurrencyDecl(apply->getFn());

    return false;
  }
};
} // end anonymous namespace

/// Diagnose uses of unsafe declarations.
static void
diagnoseDeclUnsafe(ConcreteDeclRef declRef, SourceRange R,
                   const Expr *call, const ExportContext &Where) {
  auto unsafeUses = Where.getUnsafeUses();
  if (!unsafeUses)
    return;

  SourceLoc diagLoc = call ? call->getLoc() : R.Start;
  enumerateUnsafeUses(declRef, diagLoc, call != nullptr,
                      /*skipTypeCheck=*/false,
                      [&](UnsafeUse unsafeUse) {
    unsafeUses->push_back(unsafeUse);
    return false;
  });
}

/// Diagnose uses of unavailable declarations. Returns true if a diagnostic
/// was emitted.
bool ExprAvailabilityWalker::diagnoseDeclRefAvailability(
    ConcreteDeclRef declRef, SourceRange R, const Expr *call,
    DeclAvailabilityFlags Flags) const {
  if (!declRef)
    return false;
  const ValueDecl *D = declRef.getDecl();

  // Suppress availability diagnostics for uses of builtins.  We don't
  // synthesize availability for builtin functions anyway, so this really
  // means to not check availability for the substitution maps.  This is
  // abstractly reasonable, since calls to generic builtins usually do not
  // require metadata for generic arguments the same way that calls to
  // generic functions might.  More importantly, the stdlib has to get the
  // availability right anyway, and diagnostics from builtin usage are not
  // likely to be of significant assistance in that.
  if (D->getModuleContext()->isBuiltinModule())
    return false;

  if (auto attr = D->getUnavailableAttr()) {
    if (diagnoseIncDecRemoval(D, R))
      return true;
    if (isa_and_nonnull<ApplyExpr>(call) &&
        diagnoseMemoryLayoutMigration(D, R, *attr, cast<ApplyExpr>(call)))
      return true;
  }

  if (diagnoseDeclAvailability(
          D, R, call, Where,
          Flags | DeclAvailabilityFlag::DisableUnsafeChecking))
    return true;

  diagnoseDeclUnsafe(declRef, R, call, Where);

  if (R.isValid()) {
    if (diagnoseSubstitutionMapAvailability(
            R.Start, declRef.getSubstitutions(), Where,
            Type(), Type(),
            /*warnIfConformanceUnavailablePreSwift6*/false,
            /*suppressParameterizationCheckForOptional*/false,
            /*preconcurrency*/D->preconcurrency())) {
      return true;
    }
  }

  return false;
}

/// Diagnose misuses of API in asynchronous contexts.
/// Returns true if a fatal diagnostic was emitted, false otherwise.
static bool
diagnoseDeclAsyncAvailability(const ValueDecl *D, SourceRange R,
                              const Expr *call, const ExportContext &Where) {
  // If we are not in an (effective) async context, don't check it
  auto *noAsyncDC = getInnermostDeclContextForNoAsync(Where.getDeclContext());
  if (!noAsyncDC->isAsyncContext())
    return false;

  ASTContext &ctx = Where.getDeclContext()->getASTContext();

  // Only suggest async alternatives if the DeclContext is truly async
  if (Where.getDeclContext()->isAsyncContext()) {
    if (const AbstractFunctionDecl *afd = dyn_cast<AbstractFunctionDecl>(D)) {
      if (const AbstractFunctionDecl *asyncAlt = afd->getAsyncAlternative()) {
        SourceLoc diagLoc = call ? call->getLoc() : R.Start;
        ctx.Diags.diagnose(diagLoc, diag::warn_use_async_alternative);
        asyncAlt->diagnose(diag::decl_declared_here, asyncAlt);
      }
    }
  }

  // In Swift 6 we previously didn't coerce macro arguments to parameter types,
  // so closure arguments may be treated as async in cases where they weren't in
  // Swift 6. As such we need to warn if the use is within a closure macro
  // argument until the next language mode.
  auto shouldWarnUntilFutureVersion = [&]() {
    auto *CE = dyn_cast<ClosureExpr>(noAsyncDC);
    return CE && CE->isMacroArgument();
  };

  // @available(noasync) spelling
  if (auto attr = D->getNoAsyncAttr()) {
    SourceLoc diagLoc = call ? call->getLoc() : R.Start;
    auto diag = ctx.Diags.diagnose(diagLoc, diag::async_unavailable_decl, D,
                                   attr->getMessage());
    if (D->preconcurrency()) {
      diag.limitBehavior(DiagnosticBehavior::Warning);
    } else if (shouldWarnUntilFutureVersion()) {
      diag.warnUntilFutureSwiftVersion();
    } else {
      diag.warnUntilSwiftVersion(6);
    }

    if (!attr->getRename().empty()) {
      fixItAvailableAttrRename(diag, R, D, attr->getRename(), call);
    }
    return true;
  }

  const bool hasUnavailableAttr =
      D->getAttrs().hasAttribute<UnavailableFromAsyncAttr>();

  if (!hasUnavailableAttr)
    return false;
  // @_unavailableFromAsync spelling
  const UnavailableFromAsyncAttr *attr =
      D->getAttrs().getAttribute<UnavailableFromAsyncAttr>();
  {
    SourceLoc diagLoc = call ? call->getLoc() : R.Start;
    auto diag = ctx.Diags.diagnose(diagLoc, diag::async_unavailable_decl, D,
                                   attr->Message);
    if (shouldWarnUntilFutureVersion()) {
      diag.warnUntilFutureSwiftVersion();
    } else {
      diag.warnUntilSwiftVersion(6);
    }
  }
  D->diagnose(diag::decl_declared_here, D);
  return true;
}

/// Diagnose uses of unavailable declarations. Returns true if a diagnostic
/// was emitted.
bool swift::diagnoseDeclAvailability(const ValueDecl *D, SourceRange R,
                                     const Expr *call,
                                     const ExportContext &Where,
                                     DeclAvailabilityFlags Flags) {
  // Generic parameters are always available.
  if (isa<GenericTypeParamDecl>(D))
    return false;

  if (R.isValid()) {
    if (TypeChecker::diagnoseInlinableDeclRefAccess(R.Start, D, Where))
      return true;

    if (TypeChecker::diagnoseDeclRefExportability(R.Start, D, Where))
      return true;
  }

  // Keep track if this is an accessor.
  auto accessor = dyn_cast<AccessorDecl>(D);

  if (accessor) {
    // If the property/subscript is unconditionally unavailable, don't bother
    // with any of the rest of this.
    if (accessor->getStorage()->isUnavailable())
      return false;
  }

  auto *DC = Where.getDeclContext();
  auto &ctx = DC->getASTContext();

  auto constraint =
      getAvailabilityConstraintsForDecl(D, Where.getAvailability())
          .getPrimaryConstraint();

  if (constraint) {
    if (diagnoseExplicitUnavailability(D, R, *constraint, Where, call, Flags))
      return true;
  }

  if (diagnoseDeclAsyncAvailability(D, R, call, Where))
    return true;

  if (!Flags.contains(DeclAvailabilityFlag::DisableUnsafeChecking))
    diagnoseDeclUnsafe(const_cast<ValueDecl *>(D), R, call, Where);

  // Make sure not to diagnose an accessor's deprecation if we already
  // complained about the property/subscript.
  bool isAccessorWithDeprecatedStorage =
      accessor && getDeprecated(accessor->getStorage());

  // Diagnose for deprecation
  if (!isAccessorWithDeprecatedStorage)
    diagnoseIfDeprecated(R, Where, D, call);

  if (Flags.contains(DeclAvailabilityFlag::AllowPotentiallyUnavailableProtocol)
        && isa<ProtocolDecl>(D))
    return false;

  if (!constraint)
    return false;

  // Diagnose (and possibly signal) for potential unavailability
  if (!constraint->isPotentiallyAvailable())
    return false;

  auto domainAndRange = constraint->getDomainAndRange(ctx);
  auto domain = domainAndRange.getDomain();
  auto requiredRange = domainAndRange.getRange();

  if (Flags.contains(
          DeclAvailabilityFlag::
              AllowPotentiallyUnavailableAtOrBelowDeploymentTarget) &&
      requiresDeploymentTargetOrEarlier(domain, requiredRange, ctx))
    return false;

  if (accessor) {
    bool forInout = Flags.contains(DeclAvailabilityFlag::ForInout);
    diagnosePotentialAccessorUnavailability(accessor, R, DC, domain,
                                            requiredRange, forInout);
  } else {
    if (!diagnosePotentialUnavailability(D, R, DC, domain, requiredRange))
      return false;
  }

  return !Flags.contains(
      DeclAvailabilityFlag::ContinueOnPotentialUnavailability);
}

/// Return true if the specified type looks like an integer of floating point
/// type.
static bool isIntegerOrFloatingPointType(Type ty) {
  return (TypeChecker::conformsToKnownProtocol(
            ty, KnownProtocolKind::ExpressibleByIntegerLiteral) ||
          TypeChecker::conformsToKnownProtocol(
            ty, KnownProtocolKind::ExpressibleByFloatLiteral));
}


/// If this is a call to an unavailable ++ / -- operator, try to diagnose it
/// with a fixit hint and return true.  If not, or if we fail, return false.
bool
ExprAvailabilityWalker::diagnoseIncDecRemoval(const ValueDecl *D, SourceRange R) const {
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
  if (isIntegerOrFloatingPointType(call->getType()))
    replacement = isInc ? " += 1" : " -= 1";
  else {
    // Otherwise, it must be an index type.  Rewrite to:
    // "lvalue = lvalue.successor()".
    auto &SM = Context.SourceMgr;
    auto CSR = Lexer::getCharSourceRangeFromSourceRange(
        SM, call->getArgs()->getSourceRange());
    replacement = " = " + SM.extractText(CSR).str();
    replacement += isInc ? ".successor()" : ".predecessor()";
  }
  
  if (!replacement.empty()) {
    // If we emit a deprecation diagnostic, produce a fixit hint as well.
    auto diag =
        Context.Diags.diagnose(R.Start, diag::availability_decl_unavailable, D,
                               true, AvailabilityDomain::forSwiftLanguage(),
                               "it has been removed in Swift 3");
    if (isa<PrefixUnaryExpr>(call)) {
      // Prefix: remove the ++ or --.
      diag.fixItRemove(call->getFn()->getSourceRange());
      diag.fixItInsertAfter(call->getArgs()->getEndLoc(), replacement);
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
bool
ExprAvailabilityWalker::diagnoseMemoryLayoutMigration(const ValueDecl *D,
                                                      SourceRange R,
                                                      SemanticAvailableAttr Attr,
                                                      const ApplyExpr *call) const {

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

  auto *args = call->getArgs();
  auto *subject = args->getUnlabeledUnaryExpr();
  if (!subject)
    return false;

  EncodedDiagnosticMessage EncodedMessage(Attr.getMessage());
  auto diag = Context.Diags.diagnose(
      R.Start, diag::availability_decl_unavailable, D, true,
      AvailabilityDomain::forSwiftLanguage(), EncodedMessage.Message);
  diag.highlight(R);

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
void swift::diagnoseExprAvailability(const Expr *E, DeclContext *DC) {
  auto where = ExportContext::forFunctionBody(DC, E->getStartLoc());
  if (where.isImplicit())
    return;
  ExprAvailabilityWalker walker(where);
  const_cast<Expr*>(E)->walk(walker);
}

namespace {

class StmtAvailabilityWalker : public BaseDiagnosticWalker {
  const Stmt *TopLevelStmt;
  DeclContext *DC;

public:
  explicit StmtAvailabilityWalker(const Stmt *S, DeclContext *dc)
    : TopLevelStmt(S), DC(dc) {}

  PreWalkResult<Stmt *> walkToStmtPre(Stmt *S) override {
    // `diagnoseStmtAvailability` is called for every statement, so we don't
    // want to walk into any nested statements.
    return Action::VisitNodeIf(S == TopLevelStmt, S);
  }

  PreWalkResult<Expr *> walkToExprPre(Expr *E) override {
    // Handled by ExprAvailabilityWalker.
    return Action::SkipNode(E);
  }

  PreWalkAction walkToDeclPre(Decl *D) override {
    // Handled by DeclAvailabilityChecker.
    return Action::SkipNode();
  }

  PreWalkAction walkToTypeReprPre(TypeRepr *T) override {
    auto where = ExportContext::forFunctionBody(DC, T->getStartLoc());
    diagnoseTypeReprAvailability(T, where);
    return Action::SkipNode();
  }

  PreWalkResult<Pattern *> walkToPatternPre(Pattern *P) override {
    if (auto *IP = dyn_cast<IsPattern>(P)) {
      auto where = ExportContext::forFunctionBody(DC, P->getLoc());
      diagnoseTypeAvailability(IP->getCastTypeRepr(), IP->getCastType(),
                               P->getLoc(), where, std::nullopt);
    }

    return Action::Continue(P);
  }
};
}

void swift::diagnoseStmtAvailability(const Stmt *S, DeclContext *DC) {
  StmtAvailabilityWalker walker(S, DC);
  const_cast<Stmt*>(S)->walk(walker);
}

namespace {

class TypeReprAvailabilityWalker : public ASTWalker {
  const ExportContext &where;
  DeclAvailabilityFlags flags;

  bool checkDeclRefTypeRepr(DeclRefTypeRepr *declRefTR) const {
    ArrayRef<AssociatedTypeDecl *> primaryAssociatedTypes;

    if (auto *qualIdentTR = dyn_cast<QualifiedIdentTypeRepr>(declRefTR)) {
      // If the base is unavailable, don't go on to diagnose
      // the member since that will just produce a redundant
      // diagnostic.
      if (diagnoseTypeReprAvailability(qualIdentTR->getBase(), where, flags)) {
        return true;
      }
    }

    if (auto *typeDecl = declRefTR->getBoundDecl()) {
      auto range = declRefTR->getNameLoc().getSourceRange();
      if (diagnoseDeclAvailability(typeDecl, range, nullptr, where, flags))
        return true;

      if (auto protocol = dyn_cast<ProtocolDecl>(typeDecl)) {
        primaryAssociatedTypes = protocol->getPrimaryAssociatedTypes();
      }
    }

    bool foundAnyIssues = false;

    if (declRefTR->hasGenericArgList()) {
      auto genericFlags = flags;
      genericFlags -= DeclAvailabilityFlag::AllowPotentiallyUnavailableProtocol;

      for (auto *genericArg : declRefTR->getGenericArgs()) {
        if (diagnoseTypeReprAvailability(genericArg, where, genericFlags))
          foundAnyIssues = true;

        // The associated type that is being specified must be available as
        // well.
        if (!primaryAssociatedTypes.empty()) {
          auto primaryAssociatedType = primaryAssociatedTypes.front();
          primaryAssociatedTypes = primaryAssociatedTypes.drop_front();
          if (diagnoseDeclAvailability(
                  primaryAssociatedType, genericArg->getSourceRange(),
                  nullptr, where, genericFlags))
            foundAnyIssues = true;
        }
      }
    }

    return foundAnyIssues;
  }

public:
  bool foundAnyIssues = false;

  TypeReprAvailabilityWalker(const ExportContext &where,
                             DeclAvailabilityFlags flags)
      : where(where), flags(flags) {}

  MacroWalking getMacroWalkingBehavior() const override {
    return MacroWalking::ArgumentsAndExpansion;
  }

  bool checkInlineArrayTypeRepr(InlineArrayTypeRepr *T) {
    // Has the same availability as InlineArray.
    auto &ctx = where.getDeclContext()->getASTContext();
    auto *D = ctx.getInlineArrayDecl();
    if (!D) {
      // If we have a broken stdlib we will have already diagnosed.
      return false;
    }
    return diagnoseDeclAvailability(D, T->getSourceRange(), /*call*/ nullptr,
                                    where, flags);
  }

  PreWalkAction walkToTypeReprPre(TypeRepr *T) override {
    if (auto *IAT = dyn_cast<InlineArrayTypeRepr>(T)) {
      if (checkInlineArrayTypeRepr(IAT)) {
        foundAnyIssues = true;
      }
      return Action::Continue();
    }

    auto *declRefTR = dyn_cast<DeclRefTypeRepr>(T);
    if (!declRefTR)
      return Action::Continue();

    if (checkDeclRefTypeRepr(declRefTR)) {
      foundAnyIssues = true;
    }

    // We've already visited all the children above, so we don't
    // need to recurse.
    return Action::SkipNode();
  }
};

}

/// Diagnose uses of unavailable declarations in types.
bool diagnoseTypeReprAvailability(const TypeRepr *T, const ExportContext &where,
                                  DeclAvailabilityFlags flags) {
  if (!T)
    return false;
  TypeReprAvailabilityWalker walker(where, flags);
  const_cast<TypeRepr*>(T)->walk(walker);
  return walker.foundAnyIssues;
}

namespace {

class ProblematicTypeFinder : public TypeDeclFinder {
  SourceLoc Loc;
  const ExportContext &Where;
  DeclAvailabilityFlags Flags;

public:
  ProblematicTypeFinder(SourceLoc Loc, const ExportContext &Where,
                        DeclAvailabilityFlags Flags)
      : Loc(Loc), Where(Where), Flags(Flags) {}

  void visitTypeDecl(TypeDecl *decl) {
    // We only need to diagnose exportability here. Availability was
    // already checked on the TypeRepr.
    if (Where.mustOnlyReferenceExportedDecls())
      TypeChecker::diagnoseDeclRefExportability(Loc, decl, Where);
  }

  Action visitNominalType(NominalType *ty) override {
    visitTypeDecl(ty->getDecl());

    // If some generic parameters are missing, don't check conformances.
    if (ty->hasUnboundGenericType())
      return Action::Continue;

    // When the DeclContext parameter to getContextSubstitutionMap()
    // is a protocol declaration, the receiver must be a concrete
    // type, so it doesn't make sense to perform this check on
    // protocol types.
    if (isa<ProtocolType>(ty))
      return Action::Continue;

    auto subs = ty->getContextSubstitutionMap();
    (void) diagnoseSubstitutionMapAvailability(Loc, subs, Where);
    return Action::Continue;
  }

  Action visitBoundGenericType(BoundGenericType *ty) override {
    visitTypeDecl(ty->getDecl());

    auto subs = ty->getContextSubstitutionMap();
    (void)diagnoseSubstitutionMapAvailability(
        Loc, subs, Where,
        /*depTy=*/Type(),
        /*replacementTy=*/Type(),
        /*warnIfConformanceUnavailablePreSwift6=*/false,
        /*suppressParameterizationCheckForOptional=*/ty->isOptional(),
        /*preconcurrency*/ty->getAnyNominal()->preconcurrency());
    return Action::Continue;
  }

  Action visitTypeAliasType(TypeAliasType *ty) override {
    visitTypeDecl(ty->getDecl());

    auto subs = ty->getSubstitutionMap();
    (void) diagnoseSubstitutionMapAvailability(Loc, subs, Where);
    return Action::Continue;
  }

  // We diagnose unserializable Clang function types in the
  // post-visitor so that we diagnose any unexportable component
  // types first.
  Action walkToTypePost(Type T) override {
    if (Where.mustOnlyReferenceExportedDecls()) {
      if (auto fnType = T->getAs<AnyFunctionType>()) {
        if (auto clangType = fnType->getClangTypeInfo().getType()) {
          auto *DC = Where.getDeclContext();
          auto &ctx = DC->getASTContext();
          auto loader = ctx.getClangModuleLoader();
          // Serialization will serialize the sugared type if it can,
          // but we need the canonical type to be serializable or else
          // canonicalization (e.g. in SIL) might break things.
          if (!loader->isSerializable(clangType, /*check canonical*/ true)) {
            ctx.Diags.diagnose(Loc, diag::unexportable_clang_function_type, T);
          }
        }
      }
    }

    return TypeDeclFinder::walkToTypePost(T);
  }
};

}

void swift::diagnoseTypeAvailability(const TypeRepr *TR, Type T, SourceLoc loc,
                                     const ExportContext &where,
                                     DeclAvailabilityFlags flags) {
  if (diagnoseTypeReprAvailability(TR, where, flags))
    return;

  if (!T)
    return;
  T.walk(ProblematicTypeFinder(loc, where, flags));
}

static void diagnoseMissingConformance(
    SourceLoc loc, Type type, ProtocolDecl *proto, const DeclContext *fromDC,
    bool preconcurrency) {
  assert(proto->isSpecificProtocol(KnownProtocolKind::Sendable) ||
         proto->isSpecificProtocol(KnownProtocolKind::SendableMetatype));

  if (proto->isSpecificProtocol(KnownProtocolKind::Sendable))
    diagnoseMissingSendableConformance(loc, type, fromDC, preconcurrency);

  if (proto->isSpecificProtocol(KnownProtocolKind::SendableMetatype))
    diagnoseMissingSendableMetatypeConformance(loc, type, fromDC,
                                               preconcurrency);
}

bool
swift::diagnoseConformanceAvailability(SourceLoc loc,
                                       ProtocolConformanceRef conformance,
                                       const ExportContext &where,
                                       Type depTy, Type replacementTy,
                                       bool warnIfConformanceUnavailablePreSwift6,
                                       bool preconcurrency) {
  assert(!where.isImplicit());

  if (conformance.isInvalid() || conformance.isAbstract())
    return false;

  if (conformance.isPack()) {
    bool diagnosed = false;
    auto *pack = conformance.getPack();
    for (auto patternConf : pack->getPatternConformances()) {
      diagnosed |= diagnoseConformanceAvailability(
          loc, patternConf, where, depTy, replacementTy,
          warnIfConformanceUnavailablePreSwift6,
          preconcurrency);
    }
    return diagnosed;
  }

  const ProtocolConformance *concreteConf = conformance.getConcrete();
  const RootProtocolConformance *rootConf = concreteConf->getRootConformance();

  // Conformance to Copyable and Escapable doesn't have its own availability
  // independent of the type.
  if (rootConf->getProtocol()->getInvertibleProtocolKind())
    return false;

  // Diagnose "missing" conformances where we needed a conformance but
  // didn't have one.
  auto *DC = where.getDeclContext();
  auto &ctx = DC->getASTContext();
  if (auto builtinConformance = dyn_cast<BuiltinProtocolConformance>(rootConf)){
    if (builtinConformance->isMissing()) {
      diagnoseMissingConformance(loc, builtinConformance->getType(),
                                 builtinConformance->getProtocol(), DC,
                                 preconcurrency);
    }
  }

  // Strict memory safety checking.
  if (auto unsafeUses = where.getUnsafeUses()) {
    if (auto normalConf = dyn_cast<NormalProtocolConformance>(rootConf)) {
      // @unsafe conformances are considered... unsafe.
      if (normalConf->getExplicitSafety() == ExplicitSafety::Unsafe) {
        unsafeUses->push_back(
            UnsafeUse::forConformance(
              concreteConf->getType(), conformance, loc));
      }
    }
  }

  auto maybeEmitAssociatedTypeNote = [&]() {
    if (!depTy && !replacementTy)
      return;

    Type selfTy = rootConf->getProtocol()->getSelfInterfaceType();
    if (!depTy->isEqual(selfTy)) {
      ctx.Diags.diagnose(
          loc,
          diag::assoc_conformance_from_implementation_only_module,
          depTy, replacementTy->getCanonicalType());
    }
  };

  if (auto *ext = dyn_cast<ExtensionDecl>(rootConf->getDeclContext())) {
    if (TypeChecker::diagnoseConformanceExportability(loc, rootConf, ext, where,
                                                      warnIfConformanceUnavailablePreSwift6)) {
      maybeEmitAssociatedTypeNote();
      return true;
    }

    auto constraint =
        getAvailabilityConstraintsForDecl(ext, where.getAvailability())
            .getPrimaryConstraint();
    if (constraint) {
      if (diagnoseExplicitUnavailability(loc, *constraint, rootConf, ext, where,
                                         warnIfConformanceUnavailablePreSwift6,
                                         preconcurrency)) {
        maybeEmitAssociatedTypeNote();
        return true;
      }

      // Diagnose (and possibly signal) for potential unavailability
      if (constraint->isPotentiallyAvailable()) {
        auto domainAndRange = constraint->getDomainAndRange(ctx);
        if (diagnosePotentialUnavailability(rootConf, ext, loc, DC,
                                            domainAndRange.getDomain(),
                                            domainAndRange.getRange())) {
          maybeEmitAssociatedTypeNote();
          return true;
        }
      }
    }

    // Diagnose for deprecation
    if (diagnoseIfDeprecated(loc, rootConf, ext, where)) {
      maybeEmitAssociatedTypeNote();

      // Deprecation is just a warning, so keep going with checking the
      // substitution map below.
    }
  }

  // Now, check associated conformances.
  SubstitutionMap subConformanceSubs = concreteConf->getSubstitutionMap();
  if (diagnoseSubstitutionMapAvailability(loc, subConformanceSubs, where,
                                          depTy, replacementTy,
                                          warnIfConformanceUnavailablePreSwift6,
                                          preconcurrency))
    return true;

  return false;
}

bool diagnoseSubstitutionMapAvailability(
    SourceLoc loc, SubstitutionMap subs, const ExportContext &where, Type depTy,
    Type replacementTy, bool warnIfConformanceUnavailablePreSwift6,
    bool suppressParameterizationCheckForOptional,
    bool preconcurrency) {
  bool hadAnyIssues = false;
  for (ProtocolConformanceRef conformance : subs.getConformances()) {
    if (diagnoseConformanceAvailability(loc, conformance, where,
                                        depTy, replacementTy,
                                        warnIfConformanceUnavailablePreSwift6,
                                        preconcurrency))
      hadAnyIssues = true;
  }

  // If we're looking at \c (any P)? (or any other depth of optional) then
  // there's no availability problem.
  if (suppressParameterizationCheckForOptional)
    return hadAnyIssues;

  for (auto replacement : subs.getReplacementTypes()) {
    if (checkTypeMetadataAvailability(replacement, loc, where.getDeclContext()))
      hadAnyIssues = true;
  }
  return hadAnyIssues;
}

/// Should we warn that \p decl needs an explicit availability annotation
/// in -require-explicit-availability mode?
static bool declNeedsExplicitAvailability(const Decl *decl) {
  auto &ctx = decl->getASTContext();

  // Don't require an introduced version on platforms that don't support
  // versioned availability.
  if (!ctx.supportsVersionedAvailability())
    return false;

  // Don't enforce explicit availability requirements in .swiftinterface files.
  // These diagnostics are only designed to be emitted when building from
  // source.
  if (decl->getDeclContext()->isInSwiftinterface())
    return false;

  // Skip non-public decls.
  if (auto valueDecl = dyn_cast<const ValueDecl>(decl)) {
    AccessScope scope =
      valueDecl->getFormalAccessScope(/*useDC*/nullptr,
                                      /*treatUsableFromInlineAsPublic*/true);
    if (!scope.isPublic())
      return false;
  }

  // Skip functions emitted into clients, SPI or implicit.
  if (decl->getAttrs().hasAttribute<AlwaysEmitIntoClientAttr>() ||
      decl->isSPI() ||
      decl->isImplicit())
    return false;

  // Skip unavailable decls.
  if (decl->isUnavailable())
    return false;

  // Warn on decls without a platform introduction version.
  return !decl->getAvailableAttrForPlatformIntroduction();
}

void swift::checkExplicitAvailability(Decl *decl) {
  // Skip if the command line option was not set and
  // accessors as we check the pattern binding decl instead.
  auto &ctx = decl->getASTContext();
  if (isa<AccessorDecl>(decl))
    return;

  DiagnosticBehavior DiagLevel;
  switch (ctx.LangOpts.RequireExplicitAvailabilityBehavior) {
  case LangOptions::RequireExplicitAvailabilityDiagnosticBehavior::Ignore:
    return;
  case LangOptions::RequireExplicitAvailabilityDiagnosticBehavior::Warning:
    DiagLevel = DiagnosticBehavior::Warning;
    break;
  case LangOptions::RequireExplicitAvailabilityDiagnosticBehavior::Error:
    DiagLevel = DiagnosticBehavior::Error;
    break;
  }

  // Only look at decls at module level or in extensions.
  // This could be changed to force having attributes on all decls.
  if (!decl->getDeclContext()->isModuleScopeContext() &&
      !isa<ExtensionDecl>(decl->getDeclContext())) return;

  if (auto extension = dyn_cast<ExtensionDecl>(decl)) {
    // Skip extensions that extend non-public types.
    auto extended = extension->getExtendedNominal();
    if (!extended || !extended->getFormalAccessScope().isPublic())
      return;

    // Skip extensions when none of their members need availability.
    auto members = extension->getMembers();
    auto hasMembers = std::any_of(members.begin(), members.end(),
                                  [](const Decl *D) -> bool {
      if (auto VD = dyn_cast<ValueDecl>(D))
        if (declNeedsExplicitAvailability(VD))
          return true;
      return false;
    });

    if (!hasMembers && !hasConformancesToPublicProtocols(extension))
      return;
  } else if (auto pbd = dyn_cast<PatternBindingDecl>(decl)) {
    // Check the first var instead.
    if (pbd->getNumPatternEntries() == 0)
      return;

    llvm::SmallVector<VarDecl *, 2> vars;
    pbd->getPattern(0)->collectVariables(vars);
    if (vars.empty())
      return;

    decl = vars.front();
  }

  if (declNeedsExplicitAvailability(decl)) {
    auto diag = decl->diagnose(diag::public_decl_needs_availability);
    diag.limitBehavior(DiagLevel);

    auto suggestPlatform = ctx.LangOpts.RequireExplicitAvailabilityTarget;
    if (!suggestPlatform.empty()) {
      auto InsertLoc = decl->getAttributeInsertionLoc(/*forModifiers=*/false);
      if (InsertLoc.isInvalid())
        return;

      std::string AttrText;
      {
         llvm::raw_string_ostream Out(AttrText);

         StringRef OriginalIndent = Lexer::getIndentationForLine(
           ctx.SourceMgr, InsertLoc);
         Out << "@available(" << suggestPlatform << ", *)\n"
             << OriginalIndent;
      }

      diag.fixItInsert(InsertLoc, AttrText);
    }
  }
}
