//===--- TypeRefinementContext.h - Swift Refinement Context -----*- C++ -*-===//
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
// This file defines the TypeRefinementContext class.  A TypeRefinementContext
// is the semantic construct that refines a type within its lexical scope.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_TYPEREFINEMENTCONTEXT_H
#define SWIFT_TYPEREFINEMENTCONTEXT_H

#include "swift/AST/Identifier.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/STLExtras.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/Support/ErrorHandling.h"
#include "clang/Basic/VersionTuple.h"

namespace swift {
  class Decl;
  class IfStmt;
  class SourceFile;
  class Stmt;
  
/// A lattice of version ranges of the form [x.y.z, +Inf).
class VersionRange {
  // The lattice ordering is linear:
  // Empty <= ... <= [10.10.0,+Inf) <= ... [10.1.0,+Inf) <=  ... <= All
  // and corresponds to set inclusion.

  // The concretization of lattice elements is:
  //    Empty: empty
  //    All: all versions
  //    x.y.x: all versions greater than or equal to to x.y.z

  enum class ExtremalRange { Empty, All };
  
  // A version range is either an extremal value (Empty, All) or
  // a single version tuple value representing the lower end point x.y.z of a
  // range [x.y.z, +Inf).
  union {
    clang::VersionTuple LowerEndpoint;
    ExtremalRange ExtremalValue;
  };
  
  unsigned HasLowerEndpoint : 1;

public:
  /// Returns true if the range of versions is empty, or false otherwise.
  bool isEmpty() const {
    return !HasLowerEndpoint && ExtremalValue == ExtremalRange::Empty;
  }

  /// Returns true if the range includes all versions, or false otherwise.
  bool isAll() const {
    return !HasLowerEndpoint && ExtremalValue == ExtremalRange::All;
  }

  /// Returns true if the range has a lower end point; that is, if it is of
  /// the form [X, +Inf).
  bool hasLowerEndpoint() const { return HasLowerEndpoint; }

  /// Returns the range's lower endpoint.
  const clang::VersionTuple &getLowerEndpoint() const {
    assert(HasLowerEndpoint);
    return LowerEndpoint;
  }

  /// Returns a representation of this range as a string for debugging purposes.
  std::string getAsString() const {
    if (isEmpty()) {
      return "empty";
    } else if (isAll()) {
      return "all";
    } else {
      return "[" + getLowerEndpoint().getAsString() + ",+Inf)";
    }
  }

  /// Returns true if all versions in this range are also in the Other range.
  bool isContainedIn(const VersionRange &Other) const {
    if (isEmpty() || Other.isAll())
      return true;
    
    if (isAll() || Other.isEmpty())
      return false;

    // [v1, +Inf) is contained in [v2, +Inf) if v1 >= v2
    return getLowerEndpoint() >= Other.getLowerEndpoint();
  }

  /// Mutates this range to be the greatest lower bound of itself and Other.
  void meetWith(const VersionRange &Other) {
    if (isEmpty() || Other.isAll())
      return;

    if (isAll() || Other.isEmpty()) {
      *this = Other;
      return;
    }

    // The g.l.b of [v1, +Inf), [v2, +Inf) is [max(v1,v2), +Inf)
    const clang::VersionTuple maxVersion =
        std::max(this->getLowerEndpoint(), Other.getLowerEndpoint());

    setLowerEndpoint(maxVersion);
  }

  /// Returns a version range representing all versions.
  static VersionRange all() { return VersionRange(ExtremalRange::All); }

  /// Returns a version range representing no versions.
  static VersionRange empty() { return VersionRange(ExtremalRange::Empty); }

  /// Returns a version range representing all versions greater than or equal
  /// to the passed-in version.
  static VersionRange allGTE(const clang::VersionTuple &EndPoint) {
    return VersionRange(EndPoint);
  }

private:
  VersionRange(const clang::VersionTuple &LowerEndpoint) {
    setLowerEndpoint(LowerEndpoint);
  }

  VersionRange(ExtremalRange ExtremalValue) {
    setExtremalRange(ExtremalValue);
  }

  void setExtremalRange(ExtremalRange Version) {
    HasLowerEndpoint = 0;
    ExtremalValue = Version;
  }

  void setLowerEndpoint(const clang::VersionTuple &Version) {
    HasLowerEndpoint = 1;
    LowerEndpoint = Version;
  }
};

/// Represents a lexical context in which types are refined. For now,
/// types are refined solely for API availability checking, based on
/// the operating system versions that the refined context may execute
/// upon.
///
/// These refinement contexts form a lexical tree parallel to the AST but much
/// more sparse: we only introduce refinement contexts when there is something
/// to refine.
class TypeRefinementContext {

  /// Describes the reason a type refinement context was introduced.
  enum class Reason {
    /// The root refinement context.
    Root,

    /// The context was introduced by a declaration (e.g., the body of a
    /// function declaration or the contents of a class declaration).
    Decl,

    /// The context was introduced for the Then branch of an IfStmt.
    IfStmtThenBranch
  };

  using IntroNode = llvm::PointerUnion3<SourceFile *, Decl *, IfStmt *>;

  /// The AST node that introduced this context.
  IntroNode Node;

  SourceRange SrcRange;

  VersionRange PotentialVersions;

  std::vector<TypeRefinementContext *> Children;

  TypeRefinementContext(ASTContext &Ctx, IntroNode Node,
                        TypeRefinementContext *Parent, SourceRange SrcRange,
                        const VersionRange &Versions);

public:
  
  /// Create the root refinement context for the given SourceFile.
  static TypeRefinementContext *createRoot(ASTContext &Ctx, SourceFile *SF,
                                           const VersionRange &Versions);

  /// Create a refinement context for the given declaration.
  static TypeRefinementContext *createForDecl(ASTContext &Ctx, Decl *D,
                                              TypeRefinementContext *Parent,
                                              const VersionRange &Versions);
  
  /// Create a refinement context for the Then branch of the given IfStmt.
  static TypeRefinementContext *
  createForIfStmtThen(ASTContext &Ctx, IfStmt *S, TypeRefinementContext *Parent,
                      const VersionRange &Versions);
  
  /// Returns the reason this context was introduced.
  Reason getReason() const {
    if (Node.is<Decl *>()) {
      return Reason::Decl;
    } else if (Node.is<IfStmt *>()) {
      // We will need an additional bit to discriminate when we add
      // refinement contexts for Else branches.
      return Reason::IfStmtThenBranch;
    } else if (Node.is<SourceFile *>()) {
      return Reason::Root;
    }
    llvm_unreachable("Unhandled introduction node");
  }
  
  /// Returns the AST node that introduced this refinement context. Note that
  /// this node may be different than the refined range. For example, a
  /// refinement context covering an IfStmt Then branch will have the
  /// IfStmt as the introduction node (and its reason as IfStmtThenBranch)
  /// but its source range will cover the Then branch.
  IntroNode getIntroductionNode() { return Node; }
  
  /// Returns the source range on which this context refines types.
  SourceRange getSourceRange() const { return SrcRange; }

  /// Returns a version range representing the range of operating system
  /// versions on which the code contained in this context may run.
  const VersionRange &getPotentialVersions() const { return PotentialVersions; }

  /// Adds a child refinement context.
  void addChild(TypeRefinementContext *Child) {
    assert(Child->getSourceRange().isValid());
    Children.push_back(Child);
  }

  /// Returns the inner-most TypeRefinementContext descendent of this context
  /// for the given source location.
  TypeRefinementContext *findMostRefinedSubContext(SourceLoc Loc,
                                                   SourceManager &SM);

  // Only allow allocation of TypeRefinementContext using the allocator in
  // ASTContext.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = alignof(TypeRefinementContext));
};

} // end namespace swift

#endif
