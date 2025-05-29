//===--- AvailabilityScope.h - Swift Availability Scopes ----*- C++ -----*-===//
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
// This file defines the AvailabilityScope class. An AvailabilityScope
// is the semantic construct that refines a source range with constraints
// declared using @available and if #available.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AVAILABILITYSCOPE_H
#define SWIFT_AVAILABILITYSCOPE_H

#include "swift/AST/AvailabilityContext.h"
#include "swift/AST/AvailabilityRange.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/Stmt.h" // for PoundAvailableInfo
#include "swift/Basic/Debug.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/Support/ErrorHandling.h"

namespace swift {
class BraceStmt;
class Decl;
class IfStmt;
class GuardStmt;
class SourceFile;
class Stmt;
class Expr;
class StmtConditionElement;

/// Represents a lexical context in which availability is refined. These scopes
/// form a lexical tree parallel to the AST but much more sparse: we only
/// introduce availability scopes when there is something to refine.
class AvailabilityScope : public ASTAllocated<AvailabilityScope> {

public:
  /// Describes the reason an availability scope was introduced.
  enum class Reason {
    /// The root availability scope.
    Root,

    /// The context was introduced by a declaration with an explicit
    /// availability attribute. The context contains both the signature and the
    /// body of the declaration.
    Decl,

    /// The context was introduced implicitly by a declaration. The context may
    /// cover the entire declaration or it may cover a subset of it. For
    /// example, a public, non-inlinable function declaration in an API module
    /// will have at least two associated contexts: one for the entire
    /// declaration at the declared availability of the API and a nested
    /// implicit context for the body of the function, which will always run at
    /// the deployment target of the library.
    DeclImplicit,

    /// The context was introduced for the Then branch of an IfStmt.
    IfStmtThenBranch,

    /// The context was introduced for the Else branch of an IfStmt.
    IfStmtElseBranch,

    /// The context was introduced for the remaining StmtConditionElements
    /// following an #available(...) query in a StmtCondition.
    /// For example, in the IfStmt below, the optional binding let x = expr()
    /// would be contained in this kind of context:
    ///
    /// if #available(...),
    ///    let x = expr() {
    ///  }
    ConditionFollowingAvailabilityQuery,

    /// The context was introduced for the fallthrough flow of a guard
    /// statement.
    GuardStmtFallthrough,

    /// The context was introduced for the else flow of a guard
    /// statement.
    GuardStmtElseBranch,

    // The context was introduced for the body of a while statement.
    WhileStmtBody
  };

private:
  friend class ExpandChildAvailabilityScopesRequest;

  /// Represents the AST node that introduced an availability scope.
  class IntroNode {
    Reason IntroReason;
    const DeclContext *DC;
    union {
      SourceFile *SF;
      Decl *D;
      IfStmt *IS;
      PoundAvailableInfo *PAI;
      GuardStmt *GS;
      WhileStmt *WS;
    };

  public:
    IntroNode(SourceFile *SF);
    IntroNode(Decl *D, Reason introReason = Reason::Decl);
    IntroNode(IfStmt *IS, const DeclContext *DC, bool IsThen)
        : IntroReason(IsThen ? Reason::IfStmtThenBranch
                             : Reason::IfStmtElseBranch),
          DC(DC), IS(IS) {}
    IntroNode(PoundAvailableInfo *PAI, const DeclContext *DC)
        : IntroReason(Reason::ConditionFollowingAvailabilityQuery), DC(DC),
          PAI(PAI) {}
    IntroNode(GuardStmt *GS, const DeclContext *DC, bool IsFallthrough)
        : IntroReason(IsFallthrough ? Reason::GuardStmtFallthrough
                                    : Reason::GuardStmtElseBranch),
          DC(DC), GS(GS) {}
    IntroNode(WhileStmt *WS, const DeclContext *DC)
        : IntroReason(Reason::WhileStmtBody), DC(DC), WS(WS) {}

    Reason getReason() const { return IntroReason; }

    const DeclContext *getDeclContext() const { return DC; }

    SourceFile *getAsSourceFile() const {
      assert(IntroReason == Reason::Root);
      return SF;
    }

    Decl *getAsDecl() const {
      assert(IntroReason == Reason::Decl ||
             IntroReason == Reason::DeclImplicit);
      return D;
    }

    IfStmt *getAsIfStmt() const {
      assert(IntroReason == Reason::IfStmtThenBranch ||
             IntroReason == Reason::IfStmtElseBranch);
      return IS;
    }

    PoundAvailableInfo *getAsPoundAvailableInfo() const {
      assert(IntroReason == Reason::ConditionFollowingAvailabilityQuery);
      return PAI;
    }

    GuardStmt *getAsGuardStmt() const {
      assert(IntroReason == Reason::GuardStmtFallthrough ||
             IntroReason == Reason::GuardStmtElseBranch);
      return GS;
    }

    WhileStmt *getAsWhileStmt() const {
      assert(IntroReason == Reason::WhileStmtBody);
      return WS;
    }
  };

  /// The AST node that introduced this context.
  IntroNode Node;

  SourceRange SrcRange;

  /// A canonical availability info for this context, computed top-down from the
  /// root context.
  const AvailabilityContext AvailabilityInfo;

  std::vector<AvailabilityScope *> Children;

  struct {
    /// Whether this node has child nodes that have not yet been expanded.
    unsigned needsExpansion : 1;
  } LazyInfo = {};

  void verify(const AvailabilityScope *parent, ASTContext &ctx) const;

  AvailabilityScope(ASTContext &Ctx, IntroNode Node, AvailabilityScope *Parent,
                    SourceRange SrcRange, const AvailabilityContext Info);

public:
  /// Constructs the root availability scope for the given file and builds out
  /// the scope tree for the top level contents of the file.
  static AvailabilityScope *getOrBuildForSourceFile(SourceFile &SF);

  /// Create the root availability scope for the given SourceFile.
  static AvailabilityScope *createForSourceFile(SourceFile *SF,
                                                const AvailabilityContext Info);

  /// Create an availability scope for the given declaration.
  static AvailabilityScope *createForDecl(ASTContext &Ctx, Decl *D,
                                          AvailabilityScope *Parent,
                                          const AvailabilityContext Info,
                                          SourceRange SrcRange);

  /// Create an availability scope for the given declaration.
  static AvailabilityScope *
  createForDeclImplicit(ASTContext &Ctx, Decl *D, AvailabilityScope *Parent,
                        const AvailabilityContext Info, SourceRange SrcRange);

  /// Create an availability scope for the Then branch of the given IfStmt.
  static AvailabilityScope *createForIfStmtThen(ASTContext &Ctx, IfStmt *S,
                                                const DeclContext *DC,
                                                AvailabilityScope *Parent,
                                                const AvailabilityContext Info);

  /// Create an availability scope for the Else branch of the given IfStmt.
  static AvailabilityScope *createForIfStmtElse(ASTContext &Ctx, IfStmt *S,
                                                const DeclContext *DC,
                                                AvailabilityScope *Parent,
                                                const AvailabilityContext Info);

  /// Create an availability scope for the true-branch control flow to
  /// further StmtConditionElements following a #available() query in
  /// a StmtCondition.
  static AvailabilityScope *createForConditionFollowingQuery(
      ASTContext &Ctx, PoundAvailableInfo *PAI,
      const StmtConditionElement &LastElement, const DeclContext *DC,
      AvailabilityScope *Parent, const AvailabilityContext Info);

  /// Create an availability scope for the fallthrough of a GuardStmt.
  static AvailabilityScope *createForGuardStmtFallthrough(
      ASTContext &Ctx, GuardStmt *RS, BraceStmt *ContainingBraceStmt,
      const DeclContext *DC, AvailabilityScope *Parent,
      const AvailabilityContext Info);

  /// Create an availability scope for the else branch of a GuardStmt.
  static AvailabilityScope *
  createForGuardStmtElse(ASTContext &Ctx, GuardStmt *RS, const DeclContext *DC,
                         AvailabilityScope *Parent,
                         const AvailabilityContext Info);

  /// Create an availability scope for the body of a WhileStmt.
  static AvailabilityScope *
  createForWhileStmtBody(ASTContext &Ctx, WhileStmt *WS, const DeclContext *DC,
                         AvailabilityScope *Parent,
                         const AvailabilityContext Info);

  Decl *getDeclOrNull() const {
    auto IntroReason = getReason();
    if (IntroReason == Reason::Decl || IntroReason == Reason::DeclImplicit)
      return getIntroductionNode().getAsDecl();
    return nullptr;
  }

  /// Returns the reason this scope was introduced.
  Reason getReason() const;

  /// Returns the AST node that introduced this availability scope. Note that
  /// this node may be different than the refined range. For example, an
  /// availability scope covering an IfStmt Then branch will have the
  /// IfStmt as the introduction node (and its reason as IfStmtThenBranch)
  /// but its source range will cover the Then branch.
  IntroNode getIntroductionNode() const { return Node; }

  /// Returns the location of the node that introduced this availability scope
  /// or an invalid location if the context reflects the minimum deployment
  /// target.
  SourceLoc getIntroductionLoc() const;

  /// Returns the source range covering a _single_ decl-attribute or statement
  /// condition that introduced the availability scope for a given platform
  /// version; if zero or multiple such responsible attributes or statements
  /// exist, returns an invalid SourceRange.
  SourceRange getAvailabilityConditionVersionSourceRange(
      AvailabilityDomain Domain, const llvm::VersionTuple &Version) const;

  /// Returns the availability version range that was explicitly written in
  /// source, if applicable. Otherwise, returns null.
  std::optional<const AvailabilityRange> getExplicitAvailabilityRange() const;

  /// Returns the source range this scope represents.
  SourceRange getSourceRange() const { return SrcRange; }

  /// Returns the availability context of code contained in this scope.
  const AvailabilityContext getAvailabilityContext() const {
    return AvailabilityInfo;
  }

  /// Returns the platform version range that can be assumed present at run
  /// time when running code contained in this scope.
  const AvailabilityRange getPlatformAvailabilityRange() const {
    return AvailabilityInfo.getPlatformRange();
  }

  /// Adds a child availability scope.
  void addChild(AvailabilityScope *Child, ASTContext &Ctx);

  /// Returns the innermost AvailabilityScope descendant of this scope
  /// for the given source location.
  AvailabilityScope *findMostRefinedSubContext(SourceLoc Loc, ASTContext &Ctx);

  bool getNeedsExpansion() const { return LazyInfo.needsExpansion; }

  void setNeedsExpansion(bool needsExpansion) {
    LazyInfo.needsExpansion = needsExpansion;
  }

  /// Recursively check the tree for integrity. If any errors are found, emits
  /// diagnostics to stderr and aborts.
  void verify(ASTContext &ctx);

  SWIFT_DEBUG_DUMPER(dump(SourceManager &SrcMgr));
  void dump(raw_ostream &OS, SourceManager &SrcMgr) const;
  void print(raw_ostream &OS, SourceManager &SrcMgr, unsigned Indent = 0) const;

  static StringRef getReasonName(Reason R);
};

void simple_display(llvm::raw_ostream &out, const AvailabilityScope *scope);

inline SourceLoc extractNearestSourceLoc(const AvailabilityScope *scope) {
  return scope->getIntroductionLoc();
}

} // end namespace swift

#endif
