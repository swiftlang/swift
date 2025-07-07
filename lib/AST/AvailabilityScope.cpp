//===--- AvailabilityScope.cpp - Swift Availability Scopes ----------------===//
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
// This file implements the AvailabilityScope class.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/AvailabilityScope.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/AvailabilityInference.h"
#include "swift/AST/AvailabilitySpec.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/SourceManager.h"

using namespace swift;

AvailabilityScope::IntroNode::IntroNode(SourceFile *SF)
    : IntroReason(Reason::Root), DC(SF), SF(SF) {}

AvailabilityScope::IntroNode::IntroNode(Decl *D, Reason introReason)
    : IntroReason(introReason), DC(D->getDeclContext()), D(D) {
  (void)getAsDecl(); // check that assertion succeeds
}

AvailabilityScope::AvailabilityScope(ASTContext &Ctx, IntroNode Node,
                                     AvailabilityScope *Parent,
                                     SourceRange SrcRange,
                                     const AvailabilityContext Info)
    : Node(Node), SrcRange(SrcRange), AvailabilityInfo(Info) {
  if (Parent) {
    ASSERT(SrcRange.isValid());
    Parent->addChild(this, Ctx);
    DEBUG_ASSERT(Info.isContainedIn(Parent->getAvailabilityContext()));
  }
  Ctx.addDestructorCleanup(Children);
}

AvailabilityScope *
AvailabilityScope::createForSourceFile(SourceFile *SF,
                                       const AvailabilityContext Info) {
  ASSERT(SF);

  ASTContext &Ctx = SF->getASTContext();

  SourceRange range;
  AvailabilityScope *parentContext = nullptr;
  switch (SF->Kind) {
  case SourceFileKind::MacroExpansion:
  case SourceFileKind::DefaultArgument: {
    // Look up the parent context in the enclosing file that this file's
    // root context should be nested under.
    auto enclosingSF = SF->getEnclosingSourceFile();
    if (!enclosingSF)
      break;
    if (auto parentScope = enclosingSF->getAvailabilityScope()) {
      auto charRange = Ctx.SourceMgr.getRangeForBuffer(SF->getBufferID());
      range = SourceRange(charRange.getStart(), charRange.getEnd());
      auto originalNode = SF->getNodeInEnclosingSourceFile();
      parentContext = parentScope->findMostRefinedSubContext(
          originalNode.getStartLoc(), Ctx);
    }
    break;
  }
  case SourceFileKind::Library:
  case SourceFileKind::Main:
  case SourceFileKind::Interface:
    break;
  case SourceFileKind::SIL:
    llvm_unreachable("unexpected SourceFileKind");
  }

  return new (Ctx) AvailabilityScope(
      Ctx, SF, parentContext, range,
      parentContext ? parentContext->getAvailabilityContext() : Info);
}

AvailabilityScope *AvailabilityScope::createForDecl(
    ASTContext &Ctx, Decl *D, AvailabilityScope *Parent,
    const AvailabilityContext Info, SourceRange SrcRange) {
  ASSERT(D);
  ASSERT(Parent);
  return new (Ctx) AvailabilityScope(Ctx, D, Parent, SrcRange, Info);
}

AvailabilityScope *AvailabilityScope::createForDeclImplicit(
    ASTContext &Ctx, Decl *D, AvailabilityScope *Parent,
    const AvailabilityContext Info, SourceRange SrcRange) {
  ASSERT(D);
  ASSERT(Parent);
  return new (Ctx) AvailabilityScope(Ctx, IntroNode(D, Reason::DeclImplicit),
                                     Parent, SrcRange, Info);
}

AvailabilityScope *AvailabilityScope::createForIfStmtThen(
    ASTContext &Ctx, IfStmt *S, const DeclContext *DC,
    AvailabilityScope *Parent, const AvailabilityContext Info) {
  ASSERT(S);
  ASSERT(Parent);
  return new (Ctx)
      AvailabilityScope(Ctx, IntroNode(S, DC, /*IsThen=*/true), Parent,
                        S->getThenStmt()->getSourceRange(), Info);
}

AvailabilityScope *AvailabilityScope::createForIfStmtElse(
    ASTContext &Ctx, IfStmt *S, const DeclContext *DC,
    AvailabilityScope *Parent, const AvailabilityContext Info) {
  ASSERT(S);
  ASSERT(Parent);
  return new (Ctx)
      AvailabilityScope(Ctx, IntroNode(S, DC, /*IsThen=*/false), Parent,
                        S->getElseStmt()->getSourceRange(), Info);
}

AvailabilityScope *AvailabilityScope::createForConditionFollowingQuery(
    ASTContext &Ctx, PoundAvailableInfo *PAI,
    const StmtConditionElement &LastElement, const DeclContext *DC,
    AvailabilityScope *Parent, const AvailabilityContext Info) {
  ASSERT(PAI);
  ASSERT(Parent);
  SourceRange Range(PAI->getEndLoc(), LastElement.getEndLoc());
  return new (Ctx)
      AvailabilityScope(Ctx, IntroNode(PAI, DC), Parent, Range, Info);
}

AvailabilityScope *AvailabilityScope::createForGuardStmtFallthrough(
    ASTContext &Ctx, GuardStmt *RS, BraceStmt *ContainingBraceStmt,
    const DeclContext *DC, AvailabilityScope *Parent,
    const AvailabilityContext Info) {
  ASSERT(RS);
  ASSERT(ContainingBraceStmt);
  ASSERT(Parent);
  SourceRange Range(RS->getEndLoc(), ContainingBraceStmt->getEndLoc());
  return new (Ctx) AvailabilityScope(
      Ctx, IntroNode(RS, DC, /*IsFallthrough=*/true), Parent, Range, Info);
}

AvailabilityScope *AvailabilityScope::createForGuardStmtElse(
    ASTContext &Ctx, GuardStmt *RS, const DeclContext *DC,
    AvailabilityScope *Parent, const AvailabilityContext Info) {
  ASSERT(RS);
  ASSERT(Parent);
  return new (Ctx)
      AvailabilityScope(Ctx, IntroNode(RS, DC, /*IsFallthrough=*/false), Parent,
                        RS->getBody()->getSourceRange(), Info);
}

AvailabilityScope *AvailabilityScope::createForWhileStmtBody(
    ASTContext &Ctx, WhileStmt *S, const DeclContext *DC,
    AvailabilityScope *Parent, const AvailabilityContext Info) {
  ASSERT(S);
  ASSERT(Parent);
  return new (Ctx) AvailabilityScope(Ctx, IntroNode(S, DC), Parent,
                                     S->getBody()->getSourceRange(), Info);
}

void AvailabilityScope::addChild(AvailabilityScope *Child, ASTContext &Ctx) {
  bool validSourceRange = Child->getSourceRange().isValid();
  ASSERT(validSourceRange);

  // Handle the first child.
  if (Children.empty()) {
    Children.push_back(Child);
    return;
  }

  // Handle a child that is ordered after the existing children (this should be
  // the common case).
  auto &srcMgr = Ctx.SourceMgr;
  if (srcMgr.isBefore(Children.back()->getSourceRange().Start,
                      Child->getSourceRange().Start)) {
    Children.push_back(Child);
    return;
  }

  // Insert the child amongst the existing sorted children.
  auto iter = std::upper_bound(
      Children.begin(), Children.end(), Child,
      [&srcMgr](AvailabilityScope *lhs, AvailabilityScope *rhs) {
        return srcMgr.isBefore(lhs->getSourceRange().Start,
                               rhs->getSourceRange().Start);
      });

  Children.insert(iter, Child);
}

AvailabilityScope *
AvailabilityScope::findMostRefinedSubContext(SourceLoc Loc, ASTContext &Ctx) {
  DEBUG_ASSERT(Loc.isValid());

  if (SrcRange.isValid() && !Ctx.SourceMgr.containsTokenLoc(SrcRange, Loc))
    return nullptr;

  (void)evaluateOrDefault(Ctx.evaluator,
                          ExpandChildAvailabilityScopesRequest{this}, {});
  DEBUG_ASSERT(!getNeedsExpansion());

  // Do a binary search to find the first child with a source range that
  // ends after the given location.
  auto iter = std::lower_bound(Children.begin(), Children.end(), Loc,
                               [&Ctx](AvailabilityScope *scope, SourceLoc loc) {
                                 return Ctx.SourceMgr.isBefore(
                                     scope->getSourceRange().End, loc);
                               });

  // Check whether the matching child or any of its descendants contain
  // the given location.
  if (iter != Children.end()) {
    if (auto found = (*iter)->findMostRefinedSubContext(Loc, Ctx))
      return found;
  }

  // The location is in this context's range but not in any child's, so this
  // context must be the innermost context.
  return this;
}

void AvailabilityScope::dump(SourceManager &SrcMgr) const {
  dump(llvm::errs(), SrcMgr);
}

void AvailabilityScope::dump(raw_ostream &OS, SourceManager &SrcMgr) const {
  print(OS, SrcMgr, 0);
  OS << '\n';
}

SourceLoc AvailabilityScope::getIntroductionLoc() const {
  switch (getReason()) {
  case Reason::Decl:
  case Reason::DeclImplicit:
    return Node.getAsDecl()->getLoc();

  case Reason::IfStmtThenBranch:
  case Reason::IfStmtElseBranch:
    return Node.getAsIfStmt()->getIfLoc();

  case Reason::ConditionFollowingAvailabilityQuery:
    return Node.getAsPoundAvailableInfo()->getStartLoc();

  case Reason::GuardStmtFallthrough:
  case Reason::GuardStmtElseBranch:
    return Node.getAsGuardStmt()->getGuardLoc();

  case Reason::WhileStmtBody:
    return Node.getAsWhileStmt()->getStartLoc();

  case Reason::Root:
    return SourceLoc();
  }

  llvm_unreachable("Unhandled Reason in switch.");
}

static SourceRange getAvailabilityConditionVersionSourceRange(
    const PoundAvailableInfo *PAI, const DeclContext *ReferenceDC,
    AvailabilityDomain Domain, const llvm::VersionTuple &Version) {
  SourceRange Range;
  for (auto Spec : PAI->getSemanticAvailabilitySpecs(ReferenceDC)) {
    if (Spec.getDomain() == Domain && Spec.getVersion() == Version) {
      // More than one: return invalid range, no unique choice.
      if (Range.isValid())
        return SourceRange();
      else
        Range = Spec.getParsedSpec()->getVersionSrcRange();
    }
  }
  return Range;
}

static SourceRange getAvailabilityConditionVersionSourceRange(
    const MutableArrayRef<StmtConditionElement> &Conds,
    const DeclContext *ReferenceDC, AvailabilityDomain Domain,
    const llvm::VersionTuple &Version) {
  SourceRange Range;
  for (auto const &C : Conds) {
    if (C.getKind() == StmtConditionElement::CK_Availability) {
      SourceRange R = getAvailabilityConditionVersionSourceRange(
          C.getAvailability(), ReferenceDC, Domain, Version);
      // More than one: return invalid range.
      if (Range.isValid())
        return SourceRange();
      else
        Range = R;
    }
  }
  return Range;
}

static SourceRange
getAvailabilityConditionVersionSourceRange(const Decl *D,
                                           AvailabilityDomain Domain,
                                           const llvm::VersionTuple &Version) {
  SourceRange Range;
  for (auto Attr : D->getSemanticAvailableAttrs()) {
    if (Attr.getIntroduced().has_value() &&
        Attr.getIntroduced().value() == Version && Attr.getDomain() == Domain) {

      // More than one: return invalid range.
      if (Range.isValid())
        return SourceRange();
      else
        Range = Attr.getIntroducedSourceRange();
    }
  }
  return Range;
}

SourceRange AvailabilityScope::getAvailabilityConditionVersionSourceRange(
    AvailabilityDomain Domain, const llvm::VersionTuple &Version) const {
  switch (getReason()) {
  case Reason::Decl:
    return ::getAvailabilityConditionVersionSourceRange(Node.getAsDecl(),
                                                        Domain, Version);

  case Reason::IfStmtThenBranch:
  case Reason::IfStmtElseBranch:
    return ::getAvailabilityConditionVersionSourceRange(
        Node.getAsIfStmt()->getCond(), Node.getDeclContext(), Domain, Version);

  case Reason::ConditionFollowingAvailabilityQuery:
    return ::getAvailabilityConditionVersionSourceRange(
        Node.getAsPoundAvailableInfo(), Node.getDeclContext(), Domain, Version);

  case Reason::GuardStmtFallthrough:
  case Reason::GuardStmtElseBranch:
    return ::getAvailabilityConditionVersionSourceRange(
        Node.getAsGuardStmt()->getCond(), Node.getDeclContext(), Domain,
        Version);

  case Reason::WhileStmtBody:
    return ::getAvailabilityConditionVersionSourceRange(
        Node.getAsWhileStmt()->getCond(), Node.getDeclContext(), Domain,
        Version);

  case Reason::Root:
  case Reason::DeclImplicit:
    return SourceRange();
  }

  llvm_unreachable("Unhandled Reason in switch.");
}

std::optional<const AvailabilityRange>
AvailabilityScope::getExplicitAvailabilityRange() const {
  switch (getReason()) {
  case Reason::Root:
    return std::nullopt;

  case Reason::Decl: {
    auto decl = Node.getAsDecl();
    if (auto attr = decl->getAvailableAttrForPlatformIntroduction())
      return attr->getIntroducedRange(decl->getASTContext());

    return std::nullopt;
  }

  case Reason::DeclImplicit:
    return std::nullopt;

  case Reason::IfStmtThenBranch:
  case Reason::IfStmtElseBranch:
  case Reason::ConditionFollowingAvailabilityQuery:
  case Reason::GuardStmtFallthrough:
  case Reason::GuardStmtElseBranch:
  case Reason::WhileStmtBody:
    // Availability is inherently explicit for all of these nodes.
    return getPlatformAvailabilityRange();
  }

  llvm_unreachable("Unhandled Reason in switch.");
}

static std::string
stringForAvailability(const AvailabilityRange &availability) {
  if (availability.isAlwaysAvailable())
    return "all";
  if (availability.isKnownUnreachable())
    return "none";

  return availability.getVersionString();
}

void AvailabilityScope::print(raw_ostream &OS, SourceManager &SrcMgr,
                              unsigned Indent) const {
  OS.indent(Indent);
  OS << "(" << getReasonName(getReason());

  OS << " ";
  AvailabilityInfo.print(OS);

  if (getReason() == Reason::Decl || getReason() == Reason::DeclImplicit) {
    Decl *D = Node.getAsDecl();
    OS << " decl=";
    if (auto VD = dyn_cast<ValueDecl>(D)) {
      OS << VD->getName();
    } else if (auto *ED = dyn_cast<ExtensionDecl>(D)) {
      OS << "extension." << ED->getExtendedType().getString();
    } else if (isa<TopLevelCodeDecl>(D)) {
      OS << "<top-level-code>";
    } else if (auto PBD = dyn_cast<PatternBindingDecl>(D)) {
      if (auto VD = PBD->getAnchoringVarDecl(0)) {
        OS << VD->getName();
      }
    } else if (auto ECD = dyn_cast<EnumCaseDecl>(D)) {
      if (auto EED = ECD->getFirstElement()) {
        OS << EED->getName();
      }
    }
  }

  auto R = getSourceRange();
  if (R.isValid()) {
    OS << " src_range=";

    if (getReason() != Reason::Root) {
      R.print(OS, SrcMgr, /*PrintText=*/false);
    } else if (auto info = SrcMgr.getGeneratedSourceInfo(
                   Node.getAsSourceFile()->getBufferID())) {
      info->originalSourceRange.print(OS, SrcMgr, /*PrintText=*/false);
    } else {
      OS << "<unknown>";
    }
  }

  if (getReason() == Reason::Root) {
    if (auto info = SrcMgr.getGeneratedSourceInfo(
            Node.getAsSourceFile()->getBufferID())) {
      OS << " generated_kind=" << GeneratedSourceInfo::kindToString(info->kind);
    } else {
      OS << " file=" << Node.getAsSourceFile()->getFilename().str();
    }
  }

  if (auto explicitAvailability = getExplicitAvailabilityRange())
    OS << " explicit_version=" << stringForAvailability(*explicitAvailability);

  for (AvailabilityScope *Child : Children) {
    OS << '\n';
    Child->print(OS, SrcMgr, Indent + 2);
  }
  OS.indent(Indent);
  OS << ")";
}

AvailabilityScope::Reason AvailabilityScope::getReason() const {
  return Node.getReason();
}

StringRef AvailabilityScope::getReasonName(Reason R) {
  switch (R) {
  case Reason::Root:
    return "root";

  case Reason::Decl:
    return "decl";

  case Reason::DeclImplicit:
    return "decl_implicit";

  case Reason::IfStmtThenBranch:
    return "if_then";

  case Reason::IfStmtElseBranch:
    return "if_else";

  case Reason::ConditionFollowingAvailabilityQuery:
    return "condition_following_availability";

  case Reason::GuardStmtFallthrough:
    return "guard_fallthrough";

  case Reason::GuardStmtElseBranch:
    return "guard_else";

  case Reason::WhileStmtBody:
    return "while_body";
  }

  llvm_unreachable("Unhandled Reason in switch.");
}

void swift::simple_display(llvm::raw_ostream &out,
                           const AvailabilityScope *scope) {
  out << "Scope @" << scope;
}

std::optional<evaluator::SideEffect>
ExpandChildAvailabilityScopesRequest::getCachedResult() const {
  auto *scope = std::get<0>(getStorage());
  if (scope->getNeedsExpansion())
    return std::nullopt;
  return evaluator::SideEffect();
}

void ExpandChildAvailabilityScopesRequest::cacheResult(
    evaluator::SideEffect sideEffect) const {
  auto *scope = std::get<0>(getStorage());
  scope->setNeedsExpansion(false);
}

/// Emit an error message, dump each context with its corresponding label, and
/// abort.
static void verificationError(
    ASTContext &ctx, llvm::StringRef msg,
    std::initializer_list<std::pair<const char *, const AvailabilityScope *>>
        labelsAndNodes) {
  ABORT([&](auto &out) {
    out << msg << "\n";
    for (auto pair : labelsAndNodes) {
      auto label = std::get<0>(pair);
      auto scope = std::get<1>(pair);
      out << label << ":\n";
      scope->print(out, ctx.SourceMgr);
      out << "\n";
    }
  });
}

void AvailabilityScope::verify(const AvailabilityScope *parent,
                               ASTContext &ctx) const {
  // Verify the children first.
  for (auto child : Children) {
    child->verify(this, ctx);
  }

  // Verify that the children are in sorted order and that their source ranges
  // do not overlap.
  auto &srcMgr = ctx.SourceMgr;
  if (Children.size() > 1) {
    auto const *previous = Children.front();
    for (auto const *current : ArrayRef(Children).drop_front()) {
      if (!srcMgr.isAtOrBefore(previous->getSourceRange().Start,
                               current->getSourceRange().Start))
        verificationError(
            ctx, "out of order children",
            {{"child 1", previous}, {"child 2", current}, {"parent", this}});

      if (srcMgr.containsLoc(previous->getSourceRange(),
                             current->getSourceRange().Start))
        verificationError(
            ctx, "overlapping children",
            {{"child 1", previous}, {"child 2", current}, {"parent", this}});

      previous = current;
    }
  }

  // Only root nodes are allowed to have no parent.
  if (!parent) {
    if (getReason() != Reason::Root)
      verificationError(ctx, "interior node without parent", {{"node", this}});
    return;
  }

  // All nodes with a parent must have a valid source range.
  if (!SrcRange.isValid())
    verificationError(ctx, "invalid source range", {{"node", this}});

  // Child nodes must be contained by their parents in all dimensions (source
  // range, introduction version, etc).
  if (getReason() != Reason::Root) {
    auto parentRange = parent->SrcRange;
    if (parentRange.isValid() &&
        !(srcMgr.isAtOrBefore(parentRange.Start, SrcRange.Start) &&
          srcMgr.isAtOrBefore(SrcRange.End, parentRange.End)))
      verificationError(ctx, "child source range not contained",
                        {{"child", this}, {"parent", parent}});
  }

  auto context = getAvailabilityContext();
  if (!context.verify(ctx))
    verificationError(ctx, "context is invalid", {{"node", this}});

  if (!context.isContainedIn(parent->getAvailabilityContext()))
    verificationError(ctx, "child availability range not contained",
                      {{"child", this}, {"parent", parent}});
}

void AvailabilityScope::verify(ASTContext &ctx) {
  verify(nullptr, ctx);
}
