//===--- Stmt.h - Swift Language Statement ASTs -----------------*- C++ -*-===//
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
// This file defines the Stmt class and subclasses.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_STMT_H
#define SWIFT_AST_STMT_H

#include "swift/AST/Availability.h"
#include "swift/AST/AvailabilitySpec.h"
#include "swift/AST/ASTNode.h"
#include "swift/AST/IfConfigClause.h"
#include "swift/AST/TypeAlignments.h"
#include "swift/Basic/NullablePtr.h"
#include "llvm/Support/TrailingObjects.h"

namespace swift {
  class AnyPattern;
  class ASTContext;
  class ASTWalker;
  class Decl;
  class Expr;
  class FuncDecl;
  class Pattern;
  class PatternBindingDecl;
  class VarDecl;
  
enum class StmtKind {
#define STMT(ID, PARENT) ID,
#define LAST_STMT(ID) Last_Stmt = ID,
#define STMT_RANGE(Id, FirstId, LastId) \
  First_##Id##Stmt = FirstId, Last_##Id##Stmt = LastId,
#include "swift/AST/StmtNodes.def"
};
enum : unsigned { NumStmtKindBits =
  countBitsUsed(static_cast<unsigned>(StmtKind::Last_Stmt)) };

/// Stmt - Base class for all statements in swift.
class alignas(8) Stmt {
  Stmt(const Stmt&) = delete;
  Stmt& operator=(const Stmt&) = delete;

protected:
  union { uint64_t OpaqueBits;

  SWIFT_INLINE_BITFIELD_BASE(Stmt, bitmax(NumStmtKindBits,8) + 1,
    /// Kind - The subclass of Stmt that this is.
    Kind : bitmax(NumStmtKindBits,8),

    /// Implicit - Whether this statement is implicit.
    Implicit : 1
  );

  SWIFT_INLINE_BITFIELD_FULL(BraceStmt, Stmt, 32,
    : NumPadBits,
    NumElements : 32
  );

  SWIFT_INLINE_BITFIELD_FULL(CaseStmt, Stmt, 32,
    : NumPadBits,
    NumPatterns : 32
  );

  SWIFT_INLINE_BITFIELD_EMPTY(LabeledStmt, Stmt);

  SWIFT_INLINE_BITFIELD_FULL(DoCatchStmt, LabeledStmt, 32,
    : NumPadBits,
    NumCatches : 32
  );

  SWIFT_INLINE_BITFIELD_FULL(SwitchStmt, LabeledStmt, 32,
    : NumPadBits,
    CaseCount : 32
  );

  SWIFT_INLINE_BITFIELD_FULL(YieldStmt, Stmt, 32,
    : NumPadBits,
    NumYields : 32
  );

  } Bits;

  /// Return the given value for the 'implicit' flag if present, or if None,
  /// return true if the location is invalid.
  static bool getDefaultImplicitFlag(Optional<bool> implicit, SourceLoc keyLoc){
    return implicit.hasValue() ? *implicit : keyLoc.isInvalid();
  }
  
public:
  Stmt(StmtKind kind, bool implicit) {
    Bits.OpaqueBits = 0;
    Bits.Stmt.Kind = static_cast<unsigned>(kind);
    Bits.Stmt.Implicit = implicit;
  }

  StmtKind getKind() const { return StmtKind(Bits.Stmt.Kind); }

  /// \brief Retrieve the name of the given statement kind.
  ///
  /// This name should only be used for debugging dumps and other
  /// developer aids, and should never be part of a diagnostic or exposed
  /// to the user of the compiler in any way.
  static StringRef getKindName(StmtKind kind);

  /// \brief Return the location of the start of the statement.
  SourceLoc getStartLoc() const;
  
  /// \brief Return the location of the end of the statement.
  SourceLoc getEndLoc() const;
  
  SourceRange getSourceRange() const;
  SourceLoc TrailingSemiLoc;
  
  /// isImplicit - Determines whether this statement was implicitly-generated,
  /// rather than explicitly written in the AST.
  bool isImplicit() const { return Bits.Stmt.Implicit; }

  /// walk - This recursively walks the AST rooted at this statement.
  Stmt *walk(ASTWalker &walker);
  Stmt *walk(ASTWalker &&walker) { return walk(walker); }

  LLVM_ATTRIBUTE_DEPRECATED(
      void dump() const LLVM_ATTRIBUTE_USED,
      "only for use within the debugger");
  void print(raw_ostream &OS, const ASTContext *Ctx = nullptr, unsigned Indent = 0) const;

  // Only allow allocation of Exprs using the allocator in ASTContext
  // or by doing a placement new.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = alignof(Stmt));
  
  // Make vanilla new/delete illegal for Stmts.
  void *operator new(size_t Bytes) throw() = delete;
  void operator delete(void *Data) throw() = delete;
  void *operator new(size_t Bytes, void *Mem) throw() = delete;
};

/// BraceStmt - A brace enclosed sequence of expressions, stmts, or decls, like
/// { var x = 10; print(10) }.
class BraceStmt final : public Stmt,
    private llvm::TrailingObjects<BraceStmt, ASTNode> {
  friend TrailingObjects;

  SourceLoc LBLoc;
  SourceLoc RBLoc;

  BraceStmt(SourceLoc lbloc, ArrayRef<ASTNode> elements,SourceLoc rbloc,
            Optional<bool> implicit);

public:
  static BraceStmt *create(ASTContext &ctx, SourceLoc lbloc,
                           ArrayRef<ASTNode> elements,
                           SourceLoc rbloc,
                           Optional<bool> implicit = None);

  SourceLoc getLBraceLoc() const { return LBLoc; }
  SourceLoc getRBraceLoc() const { return RBLoc; }
  
  SourceRange getSourceRange() const { return SourceRange(LBLoc, RBLoc); }

  unsigned getNumElements() const { return Bits.BraceStmt.NumElements; }

  ASTNode getElement(unsigned i) const { return getElements()[i]; }
  void setElement(unsigned i, ASTNode node) { getElements()[i] = node; }

  /// The elements contained within the BraceStmt.
  MutableArrayRef<ASTNode> getElements() {
    return {getTrailingObjects<ASTNode>(), Bits.BraceStmt.NumElements};
  }

  /// The elements contained within the BraceStmt (const version).
  ArrayRef<ASTNode> getElements() const {
    return {getTrailingObjects<ASTNode>(), Bits.BraceStmt.NumElements};
  }
  
  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::Brace; }
};

/// ReturnStmt - A return statement.  The result is optional; "return" without
/// an expression is semantically equivalent to "return ()".
///    return 42
class ReturnStmt : public Stmt {
  SourceLoc ReturnLoc;
  Expr *Result;
  
public:
  ReturnStmt(SourceLoc ReturnLoc, Expr *Result,
             Optional<bool> implicit = None)
    : Stmt(StmtKind::Return, getDefaultImplicitFlag(implicit, ReturnLoc)),
      ReturnLoc(ReturnLoc), Result(Result) {}

  SourceLoc getReturnLoc() const { return ReturnLoc; }

  SourceLoc getStartLoc() const;
  SourceLoc getEndLoc() const;

  bool hasResult() const { return Result != 0; }
  Expr *getResult() const {
    assert(Result && "ReturnStmt doesn't have a result");
    return Result;
  }
  void setResult(Expr *e) { Result = e; }
  
  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::Return;}
};

/// YieldStmt - A yield statement.  The yield-values sequence is not optional,
/// but the parentheses are.
///    yield 42
class YieldStmt final
    : public Stmt, private llvm::TrailingObjects<YieldStmt, Expr*> {
  friend TrailingObjects;

  SourceLoc YieldLoc;
  SourceLoc LPLoc;
  SourceLoc RPLoc;

  YieldStmt(SourceLoc yieldLoc, SourceLoc lpLoc, ArrayRef<Expr *> yields,
            SourceLoc rpLoc, Optional<bool> implicit = None)
    : Stmt(StmtKind::Yield, getDefaultImplicitFlag(implicit, yieldLoc)),
      YieldLoc(yieldLoc), LPLoc(lpLoc), RPLoc(rpLoc) {
    Bits.YieldStmt.NumYields = yields.size();
    memcpy(getMutableYields().data(), yields.data(),
           yields.size() * sizeof(Expr*));
  }

public:
  static YieldStmt *create(const ASTContext &ctx, SourceLoc yieldLoc,
                           SourceLoc lp, ArrayRef<Expr*> yields, SourceLoc rp,
                           Optional<bool> implicit = None);

  SourceLoc getYieldLoc() const { return YieldLoc; }
  SourceLoc getLParenLoc() const { return LPLoc; }
  SourceLoc getRParenLoc() const { return RPLoc; }

  SourceLoc getStartLoc() const { return YieldLoc; }
  SourceLoc getEndLoc() const;

  ArrayRef<Expr*> getYields() const {
    return {getTrailingObjects<Expr*>(), Bits.YieldStmt.NumYields};
  }
  MutableArrayRef<Expr*> getMutableYields() {
    return {getTrailingObjects<Expr*>(), Bits.YieldStmt.NumYields};
  }
  
  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::Yield; }
};

/// DeferStmt - A 'defer' statement.  This runs the substatement it contains
/// when the enclosing scope is exited.
///
///    defer { cleanUp() }
///
/// The AST representation for a defer statement is a bit weird.  We model this
/// as if they wrote:
///
///    func tmpClosure() { body }
///    tmpClosure()   // This is emitted on each path that needs to run this.
///
/// As such, the body of the 'defer' is actually type checked within the
/// closure's DeclContext.  We do this because of unfortunateness in SILGen,
/// some expressions (e.g. OpenExistentialExpr) cannot be multiply emitted in a
/// composable way.  When this gets fixed, patches r27767 and r27768 can be
/// reverted to go back to the simpler and more obvious representation.
///
class DeferStmt : public Stmt {
  SourceLoc DeferLoc;
  
  /// This is the bound temp function.
  FuncDecl *tempDecl;

  /// This is the invocation of the closure, which is to be emitted on any error
  /// paths.
  Expr *callExpr;
  
public:
  DeferStmt(SourceLoc DeferLoc,
            FuncDecl *tempDecl, Expr *callExpr)
    : Stmt(StmtKind::Defer, /*implicit*/false),
      DeferLoc(DeferLoc), tempDecl(tempDecl),
      callExpr(callExpr) {}
  
  SourceLoc getDeferLoc() const { return DeferLoc; }
  
  SourceLoc getStartLoc() const { return DeferLoc; }
  SourceLoc getEndLoc() const;

  FuncDecl *getTempDecl() const { return tempDecl; }
  Expr *getCallExpr() const { return callExpr; }
  void setCallExpr(Expr *E) { callExpr = E; }

  /// Dig the original user's body of the defer out for AST fidelity.
  BraceStmt *getBodyAsWritten() const;
  
  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::Defer; }
};

  
/// \brief An expression that guards execution based on whether the run-time
/// configuration supports a given API, e.g.,
/// #available(OSX >= 10.9, iOS >= 7.0).
class alignas(8) PoundAvailableInfo final :
    private llvm::TrailingObjects<PoundAvailableInfo, AvailabilitySpec *> {
  friend TrailingObjects;

  SourceLoc PoundLoc;
  SourceLoc RParenLoc;

  // The number of queries tail allocated after this object.
  unsigned NumQueries;
  
  /// The version range when this query will return true. This value is
  /// filled in by Sema.
  VersionRange AvailableRange;
  
  PoundAvailableInfo(SourceLoc PoundLoc, ArrayRef<AvailabilitySpec *> queries,
                     SourceLoc RParenLoc)
   : PoundLoc(PoundLoc), RParenLoc(RParenLoc), NumQueries(queries.size()),
     AvailableRange(VersionRange::empty()) {
    std::uninitialized_copy(queries.begin(), queries.end(),
                            getTrailingObjects<AvailabilitySpec *>());
  }
  
public:
  static PoundAvailableInfo *create(ASTContext &ctx, SourceLoc PoundLoc,
                                    ArrayRef<AvailabilitySpec *> queries,
                                    SourceLoc RParenLoc);
  
  ArrayRef<AvailabilitySpec *> getQueries() const {
    return llvm::makeArrayRef(getTrailingObjects<AvailabilitySpec *>(),
                              NumQueries);
  }
  
  SourceLoc getStartLoc() const { return PoundLoc; }
  SourceLoc getEndLoc() const;
  SourceLoc getLoc() const { return PoundLoc; }
  SourceRange getSourceRange() const { return SourceRange(getStartLoc(),
                                                          getEndLoc()); }
  
  const VersionRange &getAvailableRange() const { return AvailableRange; }
  void setAvailableRange(const VersionRange &Range) { AvailableRange = Range; }
};


/// This represents an entry in an "if" or "while" condition.  Pattern bindings
/// can bind any number of names in the pattern binding decl, and may have an
/// associated where clause.  When "if let" is involved, an arbitrary number of
/// pattern bindings and conditional expressions are permitted, e.g.:
///
///   if let x = ..., y = ... where x > y,
///      let z = ...
/// which would be represented as four StmtConditionElement entries, one for
/// the "x" binding, one for the "y" binding, one for the where clause, one for
/// "z"'s binding.  A simple "if" statement is represented as a single binding.
///
class StmtConditionElement {
  /// If this is a pattern binding, it may be the first one in a declaration, in
  /// which case this is the location of the var/let/case keyword.  If this is
  /// the second pattern (e.g. for 'y' in "var x = ..., y = ...") then this
  /// location is invalid.
  SourceLoc IntroducerLoc;

  /// In a pattern binding, this is pattern being matched.  In the case of an
  /// "implicit optional" pattern, the OptionalSome pattern is explicitly added
  /// to this as an 'implicit' pattern.
  Pattern *ThePattern = nullptr;

  /// This is either the boolean condition, the initializer for a pattern
  /// binding, or the #available information.
  llvm::PointerUnion<PoundAvailableInfo*, Expr *> CondInitOrAvailable;

public:
  StmtConditionElement() {}
  StmtConditionElement(SourceLoc IntroducerLoc, Pattern *ThePattern,
                       Expr *Init)
    : IntroducerLoc(IntroducerLoc), ThePattern(ThePattern),
      CondInitOrAvailable(Init) {}
  StmtConditionElement(Expr *cond) : CondInitOrAvailable(cond) {}

  StmtConditionElement(PoundAvailableInfo *Info) : CondInitOrAvailable(Info) {}
  
  SourceLoc getIntroducerLoc() const { return IntroducerLoc; }
  void setIntroducerLoc(SourceLoc loc) { IntroducerLoc = loc; }

  /// ConditionKind - This indicates the sort of condition this is.
  enum ConditionKind {
    CK_Boolean,
    CK_PatternBinding,
    CK_Availability
  };

  ConditionKind getKind() const {
    if (ThePattern) return CK_PatternBinding;
    return CondInitOrAvailable.is<Expr*>() ? CK_Boolean : CK_Availability;
  }

  /// Boolean Condition Accessors.
  Expr *getBooleanOrNull() const {
    return getKind() == CK_Boolean ? CondInitOrAvailable.get<Expr*>() : nullptr;
  }

  Expr *getBoolean() const {
    assert(getKind() == CK_Boolean && "Not a condition");
    return CondInitOrAvailable.get<Expr*>();
  }
  void setBoolean(Expr *E) {
    assert(getKind() == CK_Boolean && "Not a condition");
    CondInitOrAvailable = E;
  }

  /// Pattern Binding Accessors.
  Pattern *getPatternOrNull() const {
    return ThePattern;
  }

  Pattern *getPattern() const {
    assert(getKind() == CK_PatternBinding && "Not a pattern binding condition");
    return ThePattern;
  }

  void setPattern(Pattern *P) {
    assert(getKind() == CK_PatternBinding && "Not a pattern binding condition");
    ThePattern = P;
  }

  Expr *getInitializer() const {
    assert(getKind() == CK_PatternBinding && "Not a pattern binding condition");
    return CondInitOrAvailable.get<Expr*>();
  }
  void setInitializer(Expr *E) {
    assert(getKind() == CK_PatternBinding && "Not a pattern binding condition");
    CondInitOrAvailable = E;
  }
  
  // Availability Accessors
  PoundAvailableInfo *getAvailability() const {
    assert(getKind() == CK_Availability && "Not an #available condition");
    return CondInitOrAvailable.get<PoundAvailableInfo*>();
  }

  void setAvailability(PoundAvailableInfo *Info) {
    assert(getKind() == CK_Availability && "Not an #available condition");
    CondInitOrAvailable = Info;
  }

  SourceLoc getStartLoc() const;
  SourceLoc getEndLoc() const;
  SourceRange getSourceRange() const;

  /// Recursively walks the AST rooted at this statement condition element
  StmtConditionElement *walk(ASTWalker &walker);
  StmtConditionElement *walk(ASTWalker &&walker) { return walk(walker); }
};

struct LabeledStmtInfo {
  Identifier Name;
  SourceLoc Loc;
  
  // Evaluates to true if set.
  operator bool() const { return !Name.empty(); }
};
  
/// LabeledStmt - Common base class between the labeled statements (loops and
/// switch).
class LabeledStmt : public Stmt {
  LabeledStmtInfo LabelInfo;
protected:
  SourceLoc getLabelLocOrKeywordLoc(SourceLoc L) const {
    return LabelInfo ? LabelInfo.Loc : L;
  }
public:
  LabeledStmt(StmtKind Kind, bool Implicit, LabeledStmtInfo LabelInfo)
    : Stmt(Kind, Implicit), LabelInfo(LabelInfo) {}
  
  LabeledStmtInfo getLabelInfo() const { return LabelInfo; }
  void setLabelInfo(LabeledStmtInfo L) { LabelInfo = L; }

  /// Is this statement a valid target of "continue" if labeled?
  ///
  /// For the most part, non-looping constructs shouldn't be
  /// continue-able, but we threw in "do" as a sop.
  bool isPossibleContinueTarget() const;

  /// Is this statement a valid target of an unlabeled "break" or
  /// "continue"?
  ///
  /// The nice, consistent language rule is that unlabeled "break" and
  /// "continue" leave the innermost loop.  We have to include
  /// "switch" (for "break") for consistency with C: Swift doesn't
  /// require "break" to leave a switch case, but it's still way too
  /// similar to C's switch to allow different behavior for "break".
  bool requiresLabelOnJump() const;

  static bool classof(const Stmt *S) {
    return S->getKind() >= StmtKind::First_LabeledStmt &&
           S->getKind() <= StmtKind::Last_LabeledStmt;
  }
};


/// DoStmt - do statement, without any trailing clauses.
class DoStmt : public LabeledStmt {
  SourceLoc DoLoc;
  Stmt *Body;
  
public:
  DoStmt(LabeledStmtInfo labelInfo, SourceLoc doLoc,
         Stmt *body, Optional<bool> implicit = None)
    : LabeledStmt(StmtKind::Do, getDefaultImplicitFlag(implicit, doLoc),
                  labelInfo),
      DoLoc(doLoc), Body(body) {}

  SourceLoc getDoLoc() const { return DoLoc; }
  
  SourceLoc getStartLoc() const { return getLabelLocOrKeywordLoc(DoLoc); }
  SourceLoc getEndLoc() const { return Body->getEndLoc(); }
  
  Stmt *getBody() const { return Body; }
  void setBody(Stmt *s) { Body = s; }

  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::Do; }
};

/// An individual 'catch' clause.
/// 
/// This isn't really an independent statement any more than CaseStmt
/// is; it's just a structural part of a DoCatchStmt.
class CatchStmt : public Stmt {
  SourceLoc CatchLoc;
  SourceLoc WhereLoc;
  Pattern *ErrorPattern;
  Expr *GuardExpr;
  Stmt *CatchBody;

public:
  CatchStmt(SourceLoc catchLoc, Pattern *errorPattern,
            SourceLoc whereLoc, Expr *guardExpr, Stmt *body,
            Optional<bool> implicit = None)
    : Stmt(StmtKind::Catch, getDefaultImplicitFlag(implicit, catchLoc)),
      CatchLoc(catchLoc), WhereLoc(whereLoc),
      ErrorPattern(nullptr), GuardExpr(guardExpr), CatchBody(body) {
    setErrorPattern(errorPattern);
  }

  SourceLoc getCatchLoc() const { return CatchLoc; }

  /// The location of the 'where' keyword if there's a guard expression.
  SourceLoc getWhereLoc() const { return WhereLoc; }

  SourceLoc getStartLoc() const { return CatchLoc; }
  SourceLoc getEndLoc() const { return CatchBody->getEndLoc(); }

  Stmt *getBody() const { return CatchBody; }
  void setBody(Stmt *body) { CatchBody = body; }

  Pattern *getErrorPattern() { return ErrorPattern; }
  const Pattern *getErrorPattern() const { return ErrorPattern; }
  void setErrorPattern(Pattern *pattern);

  /// Is this catch clause "syntactically exhaustive"?
  bool isSyntacticallyExhaustive() const;

  /// Return the guard expression if present, or null if the catch has
  /// no guard.
  Expr *getGuardExpr() const { return GuardExpr; }
  void setGuardExpr(Expr *guard) { GuardExpr = guard; }

  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::Catch; }
};

/// DoCatchStmt - do statement with trailing 'catch' clauses.
class DoCatchStmt final : public LabeledStmt,
    private llvm::TrailingObjects<DoCatchStmt, CatchStmt *> {
  friend TrailingObjects;

  SourceLoc DoLoc;
  Stmt *Body;

  DoCatchStmt(LabeledStmtInfo labelInfo, SourceLoc doLoc,
              Stmt *body, ArrayRef<CatchStmt*> catches,
              Optional<bool> implicit)
    : LabeledStmt(StmtKind::DoCatch, getDefaultImplicitFlag(implicit, doLoc),
                  labelInfo), DoLoc(doLoc), Body(body) {
    Bits.DoCatchStmt.NumCatches = catches.size();
    std::uninitialized_copy(catches.begin(), catches.end(),
                            getTrailingObjects<CatchStmt *>());
  }

public:
  static DoCatchStmt *create(ASTContext &ctx, LabeledStmtInfo labelInfo,
                             SourceLoc doLoc, Stmt *body,
                             ArrayRef<CatchStmt*> catches,
                             Optional<bool> implicit = None);

  SourceLoc getDoLoc() const { return DoLoc; }

  SourceLoc getStartLoc() const { return getLabelLocOrKeywordLoc(DoLoc); }
  SourceLoc getEndLoc() const { return getCatches().back()->getEndLoc(); }

  Stmt *getBody() const { return Body; }
  void setBody(Stmt *s) { Body = s; }

  ArrayRef<CatchStmt*> getCatches() const {
    return {getTrailingObjects<CatchStmt*>(), Bits.DoCatchStmt.NumCatches};
  }
  MutableArrayRef<CatchStmt*> getMutableCatches() {
    return {getTrailingObjects<CatchStmt*>(), Bits.DoCatchStmt.NumCatches};
  }

  /// Does this statement contain a syntactically exhaustive catch
  /// clause?
  ///
  /// Note that an exhaustive do/catch statement can still throw
  /// errors out of its catch block(s).
  bool isSyntacticallyExhaustive() const;

  static bool classof(const Stmt *S) {
    return S->getKind() == StmtKind::DoCatch;
  }
};


/// Either an "if let" case or a simple boolean expression can appear as the
/// condition of an 'if' or 'while' statement.
using StmtCondition = MutableArrayRef<StmtConditionElement>;

/// This is the common base class between statements that can have labels, and
/// also have complex "if let" style conditions: 'if' and 'while'.
class LabeledConditionalStmt : public LabeledStmt {
  StmtCondition Cond;
public:
  LabeledConditionalStmt(StmtKind Kind, bool Implicit,
                         LabeledStmtInfo LabelInfo, StmtCondition Cond)
    : LabeledStmt(Kind, Implicit, LabelInfo) {
    setCond(Cond);
  }

  StmtCondition getCond() const { return Cond; }
  void setCond(StmtCondition e);

  static bool classof(const Stmt *S) {
    return S->getKind() >= StmtKind::First_LabeledConditionalStmt &&
           S->getKind() <= StmtKind::Last_LabeledConditionalStmt;
  }
};
  
  
/// IfStmt - if/then/else statement.  If no 'else' is specified, then the
/// ElseLoc location is not specified and the Else statement is null. After
/// type-checking, the condition is of type Builtin.Int1.
class IfStmt : public LabeledConditionalStmt {
  SourceLoc IfLoc;
  SourceLoc ElseLoc;
  Stmt *Then;
  Stmt *Else;
  
public:
  IfStmt(LabeledStmtInfo LabelInfo, SourceLoc IfLoc, StmtCondition Cond,
         Stmt *Then, SourceLoc ElseLoc, Stmt *Else,
         Optional<bool> implicit = None)
  : LabeledConditionalStmt(StmtKind::If,
                           getDefaultImplicitFlag(implicit, IfLoc),
                           LabelInfo, Cond),
    IfLoc(IfLoc), ElseLoc(ElseLoc), Then(Then), Else(Else) {}

  IfStmt(SourceLoc IfLoc, Expr *Cond, Stmt *Then, SourceLoc ElseLoc,
         Stmt *Else, Optional<bool> implicit, ASTContext &Ctx);

  SourceLoc getIfLoc() const { return IfLoc; }
  SourceLoc getElseLoc() const { return ElseLoc; }

  SourceLoc getStartLoc() const {
    return getLabelLocOrKeywordLoc(IfLoc);
  }
  SourceLoc getEndLoc() const {
    return (Else ? Else->getEndLoc() : Then->getEndLoc());
  }

  Stmt *getThenStmt() const { return Then; }
  void setThenStmt(Stmt *s) { Then = s; }

  Stmt *getElseStmt() const { return Else; }
  void setElseStmt(Stmt *s) { Else = s; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::If; }
};

/// GuardStmt - 'guard' statement.  Evaluate a condition and if it fails, run
/// its body.  The body is always guaranteed to exit the current scope (or
/// abort), it never falls through.
///
class GuardStmt : public LabeledConditionalStmt {
  SourceLoc GuardLoc;
  Stmt *Body;
  
public:
  GuardStmt(SourceLoc GuardLoc, StmtCondition Cond,
            Stmt *Body, Optional<bool> implicit = None)
  : LabeledConditionalStmt(StmtKind::Guard,
                           getDefaultImplicitFlag(implicit, GuardLoc),
                           LabeledStmtInfo(), Cond),
    GuardLoc(GuardLoc), Body(Body) {}
  
  GuardStmt(SourceLoc GuardLoc, Expr *Cond, Stmt *Body,
            Optional<bool> implicit, ASTContext &Ctx);
  
  SourceLoc getGuardLoc() const { return GuardLoc; }
  
  SourceLoc getStartLoc() const {
    return getLabelLocOrKeywordLoc(GuardLoc);
  }
  SourceLoc getEndLoc() const {
    return Body->getEndLoc();
  }
  
  Stmt *getBody() const { return Body; }
  void setBody(Stmt *s) { Body = s; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::Guard; }
};

/// WhileStmt - while statement. After type-checking, the condition is of
/// type Builtin.Int1.
class WhileStmt : public LabeledConditionalStmt {
  SourceLoc WhileLoc;
  StmtCondition Cond;
  Stmt *Body;
  
public:
  WhileStmt(LabeledStmtInfo LabelInfo, SourceLoc WhileLoc, StmtCondition Cond,
            Stmt *Body, Optional<bool> implicit = None)
  : LabeledConditionalStmt(StmtKind::While,
                           getDefaultImplicitFlag(implicit, WhileLoc),
                           LabelInfo, Cond),
    WhileLoc(WhileLoc), Body(Body) {}

  SourceLoc getStartLoc() const { return getLabelLocOrKeywordLoc(WhileLoc); }
  SourceLoc getEndLoc() const { return Body->getEndLoc(); }

  Stmt *getBody() const { return Body; }
  void setBody(Stmt *s) { Body = s; }
  
  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::While; }
};
  
/// RepeatWhileStmt - repeat/while statement. After type-checking, the
/// condition is of type Builtin.Int1.
class RepeatWhileStmt : public LabeledStmt {
  SourceLoc RepeatLoc, WhileLoc;
  Stmt *Body;
  Expr *Cond;
  
public:
  RepeatWhileStmt(LabeledStmtInfo LabelInfo, SourceLoc RepeatLoc, Expr *Cond,
              SourceLoc WhileLoc, Stmt *Body, Optional<bool> implicit = None)
    : LabeledStmt(StmtKind::RepeatWhile,
                  getDefaultImplicitFlag(implicit, RepeatLoc),
                  LabelInfo),
      RepeatLoc(RepeatLoc), WhileLoc(WhileLoc), Body(Body), Cond(Cond) {}
  
  SourceLoc getStartLoc() const { return getLabelLocOrKeywordLoc(RepeatLoc); }
  SourceLoc getEndLoc() const;
  
  Stmt *getBody() const { return Body; }
  void setBody(Stmt *s) { Body = s; }

  Expr *getCond() const { return Cond; }
  void setCond(Expr *e) { Cond = e; }
  
  static bool classof(const Stmt *S) {return S->getKind() == StmtKind::RepeatWhile;}
};

/// ForEachStmt - foreach statement that iterates over the elements in a
/// container.
///
/// Example:
/// \code
/// for i in 0...10 {
///   print(String(i))
/// }
/// \endcode
class ForEachStmt : public LabeledStmt {
  SourceLoc ForLoc;
  Pattern *Pat;
  SourceLoc InLoc;
  Expr *Sequence;
  Expr *WhereExpr = nullptr;
  BraceStmt *Body;
  
  /// The iterator variable along with its initializer.
  PatternBindingDecl *Iterator = nullptr;
  /// The expression that advances the iterator and returns an Optional with
  /// the next value or None to signal end-of-stream.
  Expr *IteratorNext = nullptr;

public:
  ForEachStmt(LabeledStmtInfo LabelInfo, SourceLoc ForLoc, Pattern *Pat,
              SourceLoc InLoc, Expr *Sequence, Expr *WhereExpr, BraceStmt *Body,
              Optional<bool> implicit = None)
    : LabeledStmt(StmtKind::ForEach, getDefaultImplicitFlag(implicit, ForLoc),
                  LabelInfo),
      ForLoc(ForLoc), Pat(nullptr), InLoc(InLoc), Sequence(Sequence),
      WhereExpr(WhereExpr), Body(Body) {
    setPattern(Pat);
  }
  
  /// getForLoc - Retrieve the location of the 'for' keyword.
  SourceLoc getForLoc() const { return ForLoc; }

  /// getInLoc - Retrieve the location of the 'in' keyword.
  SourceLoc getInLoc() const { return InLoc; }
  
  /// getPattern - Retrieve the pattern describing the iteration variables.
  /// These variables will only be visible within the body of the loop.
  Pattern *getPattern() const { return Pat; }
  void setPattern(Pattern *p);
  
  Expr *getWhere() const { return WhereExpr; }
  void setWhere(Expr *W) { WhereExpr = W; }

  /// getSequence - Retrieve the Sequence whose elements will be visited
  /// by this foreach loop, as it was written in the source code and
  /// subsequently type-checked. To determine the semantic behavior of this
  /// expression to extract a range, use \c getRangeInit().
  Expr *getSequence() const { return Sequence; }
  void setSequence(Expr *S) { Sequence = S; }
  
  /// Retrieve the pattern binding that contains the (implicit) iterator
  /// variable and its initialization from the container.
  PatternBindingDecl *getIterator() const { return Iterator; }
  void setIterator(PatternBindingDecl *It) { Iterator = It; }
  
  /// Retrieve the expression that advances the iterator.
  Expr *getIteratorNext() const { return IteratorNext; }
  void setIteratorNext(Expr *E) { IteratorNext = E; }

  /// getBody - Retrieve the body of the loop.
  BraceStmt *getBody() const { return Body; }
  void setBody(BraceStmt *B) { Body = B; }
  
  SourceLoc getStartLoc() const { return getLabelLocOrKeywordLoc(ForLoc); }
  SourceLoc getEndLoc() const { return Body->getEndLoc(); }
  
  static bool classof(const Stmt *S) {
    return S->getKind() == StmtKind::ForEach;
  }
};

/// A pattern and an optional guard expression used in a 'case' statement.
class CaseLabelItem {
  enum class Kind {
    /// A normal pattern
    Normal = 0,
    /// `default`
    Default,
  };

  Pattern *CasePattern;
  SourceLoc WhereLoc;
  llvm::PointerIntPair<Expr *, 1, Kind> GuardExprAndKind;

  CaseLabelItem(Kind kind, Pattern *casePattern, SourceLoc whereLoc,
                Expr *guardExpr)
    : CasePattern(casePattern), WhereLoc(whereLoc),
      GuardExprAndKind(guardExpr, kind) {}

public:
  CaseLabelItem(const CaseLabelItem &) = default;

  CaseLabelItem(Pattern *casePattern, SourceLoc whereLoc, Expr *guardExpr)
    : CaseLabelItem(Kind::Normal, casePattern, whereLoc, guardExpr) {}
  explicit CaseLabelItem(Pattern *casePattern)
    : CaseLabelItem(casePattern, SourceLoc(), nullptr) {}

  static CaseLabelItem getDefault(AnyPattern *pattern,
                                  SourceLoc whereLoc,
                                  Expr *guardExpr) {
    assert(pattern);
    return CaseLabelItem(Kind::Default, reinterpret_cast<Pattern *>(pattern),
                         whereLoc, guardExpr);
  }
  static CaseLabelItem getDefault(AnyPattern *pattern) {
    return getDefault(pattern, SourceLoc(), nullptr);
  }

  SourceLoc getWhereLoc() const { return WhereLoc; }

  SourceLoc getStartLoc() const;
  SourceLoc getEndLoc() const;
  SourceRange getSourceRange() const;

  Pattern *getPattern() { return CasePattern; }
  const Pattern *getPattern() const { return CasePattern; }
  void setPattern(Pattern *CasePattern) { this->CasePattern = CasePattern; }

  /// Return the guard expression if present, or null if the case label has
  /// no guard.
  Expr *getGuardExpr() { return GuardExprAndKind.getPointer(); }
  const Expr *getGuardExpr() const {
    return GuardExprAndKind.getPointer();
  }
  void setGuardExpr(Expr *e) { GuardExprAndKind.setPointer(e); }

  /// Returns true if this is syntactically a 'default' label.
  bool isDefault() const {
    return GuardExprAndKind.getInt() == Kind::Default;
  }
};

/// A 'case' or 'default' block of a switch statement.  Only valid as the
/// substatement of a SwitchStmt.  A case block begins either with one or more
/// CaseLabelItems or a single 'default' label.
///
/// Some examples:
/// \code
///   case 1:
///   case 2, 3:
///   case Foo(var x, var y) where x < y:
///   case 2 where foo(), 3 where bar():
///   default:
/// \endcode
///
class CaseStmt final : public Stmt,
    private llvm::TrailingObjects<CaseStmt, CaseLabelItem> {
  friend TrailingObjects;

  SourceLoc UnknownAttrLoc;
  SourceLoc CaseLoc;
  SourceLoc ColonLoc;

  llvm::PointerIntPair<Stmt *, 1, bool> BodyAndHasBoundDecls;

  CaseStmt(SourceLoc CaseLoc, ArrayRef<CaseLabelItem> CaseLabelItems,
           bool HasBoundDecls, SourceLoc UnknownAttrLoc, SourceLoc ColonLoc,
           Stmt *Body, Optional<bool> Implicit);

public:
  static CaseStmt *create(ASTContext &C, SourceLoc CaseLoc,
                          ArrayRef<CaseLabelItem> CaseLabelItems,
                          bool HasBoundDecls, SourceLoc UnknownAttrLoc,
                          SourceLoc ColonLoc, Stmt *Body,
                          Optional<bool> Implicit = None);

  ArrayRef<CaseLabelItem> getCaseLabelItems() const {
    return {getTrailingObjects<CaseLabelItem>(), Bits.CaseStmt.NumPatterns};
  }
  MutableArrayRef<CaseLabelItem> getMutableCaseLabelItems() {
    return {getTrailingObjects<CaseLabelItem>(), Bits.CaseStmt.NumPatterns};
  }

  Stmt *getBody() const { return BodyAndHasBoundDecls.getPointer(); }
  void setBody(Stmt *body) { BodyAndHasBoundDecls.setPointer(body); }

  /// True if the case block declares any patterns with local variable bindings.
  bool hasBoundDecls() const { return BodyAndHasBoundDecls.getInt(); }

  /// Get the source location of the 'case' or 'default' of the first label.
  SourceLoc getLoc() const { return CaseLoc; }

  SourceLoc getStartLoc() const {
    if (UnknownAttrLoc.isValid())
      return UnknownAttrLoc;
    return getLoc();
  }
  SourceLoc getEndLoc() const { return getBody()->getEndLoc(); }
  SourceRange getLabelItemsRange() const {
    return ColonLoc.isValid() ? SourceRange(getLoc(), ColonLoc) : getSourceRange();
  }

  bool isDefault() { return getCaseLabelItems()[0].isDefault(); }

  bool hasUnknownAttr() const {
    // Note: This representation doesn't allow for synthesized @unknown cases.
    // However, that's probably sensible; the purpose of @unknown is for
    // diagnosing otherwise-non-exhaustive switches, and the user can't edit
    // a synthesized case.
    return UnknownAttrLoc.isValid();
  }

  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::Case; }
};

/// Switch statement.
class SwitchStmt final : public LabeledStmt,
    private llvm::TrailingObjects<SwitchStmt, ASTNode> {
  friend TrailingObjects;

  SourceLoc SwitchLoc, LBraceLoc, RBraceLoc;
  Expr *SubjectExpr;

  SwitchStmt(LabeledStmtInfo LabelInfo, SourceLoc SwitchLoc, Expr *SubjectExpr,
             SourceLoc LBraceLoc, unsigned CaseCount, SourceLoc RBraceLoc,
             Optional<bool> implicit = None)
    : LabeledStmt(StmtKind::Switch, getDefaultImplicitFlag(implicit, SwitchLoc),
                  LabelInfo),
      SwitchLoc(SwitchLoc), LBraceLoc(LBraceLoc), RBraceLoc(RBraceLoc),
      SubjectExpr(SubjectExpr) {
    Bits.SwitchStmt.CaseCount = CaseCount;
  }

public:
  /// Allocate a new SwitchStmt in the given ASTContext.
  static SwitchStmt *create(LabeledStmtInfo LabelInfo, SourceLoc SwitchLoc,
                            Expr *SubjectExpr,
                            SourceLoc LBraceLoc,
                            ArrayRef<ASTNode> Cases,
                            SourceLoc RBraceLoc,
                            ASTContext &C);
  
  /// Get the source location of the 'switch' keyword.
  SourceLoc getSwitchLoc() const { return SwitchLoc; }
  /// Get the source location of the opening brace.
  SourceLoc getLBraceLoc() const { return LBraceLoc; }
  /// Get the source location of the closing brace.
  SourceLoc getRBraceLoc() const { return RBraceLoc; }
  
  SourceLoc getLoc() const { return SwitchLoc; }

  SourceLoc getStartLoc() const { return getLabelLocOrKeywordLoc(SwitchLoc); }
  SourceLoc getEndLoc() const { return RBraceLoc; }
  
  /// Get the subject expression of the switch.
  Expr *getSubjectExpr() const { return SubjectExpr; }
  void setSubjectExpr(Expr *e) { SubjectExpr = e; }

  ArrayRef<ASTNode> getRawCases() const {
    return {getTrailingObjects<ASTNode>(), Bits.SwitchStmt.CaseCount};
  }

private:
  struct AsCaseStmtWithSkippingNonCaseStmts {
    AsCaseStmtWithSkippingNonCaseStmts() {}
    Optional<CaseStmt*> operator()(const ASTNode &N) const {
      if (auto *CS = llvm::dyn_cast_or_null<CaseStmt>(N.dyn_cast<Stmt*>()))
        return CS;
      return None;
    }
  };

public:
  using AsCaseStmtRange = OptionalTransformRange<ArrayRef<ASTNode>,
                            AsCaseStmtWithSkippingNonCaseStmts>;
  
  /// Get the list of case clauses.
  AsCaseStmtRange getCases() const {
    return AsCaseStmtRange(getRawCases(), AsCaseStmtWithSkippingNonCaseStmts());
  }
  
  static bool classof(const Stmt *S) {
    return S->getKind() == StmtKind::Switch;
  }
};

/// BreakStmt - The "break" and "break label" statement.
class BreakStmt : public Stmt {
  SourceLoc Loc;
  Identifier TargetName; // Named target statement, if specified in the source.
  SourceLoc TargetLoc;
  LabeledStmt *Target = nullptr;  // Target stmt, wired up by Sema.
public:
  BreakStmt(SourceLoc Loc, Identifier TargetName, SourceLoc TargetLoc,
            Optional<bool> implicit = None)
    : Stmt(StmtKind::Break, getDefaultImplicitFlag(implicit, Loc)), Loc(Loc),
      TargetName(TargetName), TargetLoc(TargetLoc) {
  }

  SourceLoc getLoc() const { return Loc; }

  Identifier getTargetName() const { return TargetName; }
  void setTargetName(Identifier N) { TargetName = N; }
  SourceLoc getTargetLoc() const { return TargetLoc; }
  void setTargetLoc(SourceLoc L) { TargetLoc = L; }

  // Manipulate the target loop/switch that is bring broken out of.  This is set
  // by sema during type checking.
  void setTarget(LabeledStmt *LS) { Target = LS; }
  LabeledStmt *getTarget() const { return Target; }

  SourceLoc getStartLoc() const { return Loc; }
  SourceLoc getEndLoc() const {
    return (TargetLoc.isValid() ? TargetLoc : Loc);
  }

  static bool classof(const Stmt *S) {
    return S->getKind() == StmtKind::Break;
  }
};

/// ContinueStmt - The "continue" and "continue label" statement.
class ContinueStmt : public Stmt {
  SourceLoc Loc;
  Identifier TargetName; // Named target statement, if specified in the source.
  SourceLoc TargetLoc;
  LabeledStmt *Target = nullptr;

public:
  ContinueStmt(SourceLoc Loc, Identifier TargetName, SourceLoc TargetLoc,
               Optional<bool> implicit = None)
    : Stmt(StmtKind::Continue, getDefaultImplicitFlag(implicit, Loc)), Loc(Loc),
      TargetName(TargetName), TargetLoc(TargetLoc) {
  }

  Identifier getTargetName() const { return TargetName; }
  void setTargetName(Identifier N) { TargetName = N; }
  SourceLoc getTargetLoc() const { return TargetLoc; }
  void setTargetLoc(SourceLoc L) { TargetLoc = L; }

  // Manipulate the target loop that is bring continued.  This is set by sema
  // during type checking.
  void setTarget(LabeledStmt *LS) { Target = LS; }
  LabeledStmt *getTarget() const { return Target; }
  
  SourceLoc getLoc() const { return Loc; }
  
  SourceLoc getStartLoc() const { return Loc; }
  SourceLoc getEndLoc() const {
    return (TargetLoc.isValid() ? TargetLoc : Loc);
  }

  static bool classof(const Stmt *S) {
    return S->getKind() == StmtKind::Continue;
  }
};

/// FallthroughStmt - The keyword "fallthrough".
class FallthroughStmt : public Stmt {
  SourceLoc Loc;
  CaseStmt *FallthroughSource;
  CaseStmt *FallthroughDest;
  
public:
  FallthroughStmt(SourceLoc Loc, Optional<bool> implicit = None)
    : Stmt(StmtKind::Fallthrough, getDefaultImplicitFlag(implicit, Loc)),
      Loc(Loc), FallthroughSource(nullptr), FallthroughDest(nullptr)
  {}
  
  SourceLoc getLoc() const { return Loc; }
  
  SourceRange getSourceRange() const { return Loc; }

  /// Get the CaseStmt block from which the fallthrough transfers control.
  /// Set during Sema. (May stay null if fallthrough is invalid.)
  CaseStmt *getFallthroughSource() const {
    return FallthroughSource;
  }
  void setFallthroughSource(CaseStmt *C) {
    assert(!FallthroughSource && "fallthrough source already set?!");
    FallthroughSource = C;
  }

  /// Get the CaseStmt block to which the fallthrough transfers control.
  /// Set during Sema.
  CaseStmt *getFallthroughDest() const {
    assert(FallthroughDest && "fallthrough dest is not set until Sema");
    return FallthroughDest;
  }
  void setFallthroughDest(CaseStmt *C) {
    assert(!FallthroughDest && "fallthrough dest already set?!");
    FallthroughDest = C;
  }
  
  static bool classof(const Stmt *S) {
    return S->getKind() == StmtKind::Fallthrough;
  }
};

/// FailStmt - A statement that indicates a failable, which is currently
/// spelled as "return nil" and can only be used within failable initializers.
class FailStmt : public Stmt {
  SourceLoc ReturnLoc;
  SourceLoc NilLoc;

public:
  FailStmt(SourceLoc returnLoc, SourceLoc nilLoc,
           Optional<bool> implicit = None)
    : Stmt(StmtKind::Fail, getDefaultImplicitFlag(implicit, returnLoc)),
      ReturnLoc(returnLoc), NilLoc(nilLoc)
  {}
  
  SourceLoc getLoc() const { return ReturnLoc; }
  
  SourceRange getSourceRange() const { return SourceRange(ReturnLoc, NilLoc); }
  
  static bool classof(const Stmt *S) {
    return S->getKind() == StmtKind::Fail;
  }
};

/// ThrowStmt - Throws an error.
class ThrowStmt : public Stmt {
  Expr *SubExpr;
  SourceLoc ThrowLoc;
  
public:
  explicit ThrowStmt(SourceLoc throwLoc, Expr *subExpr)
  : Stmt(StmtKind::Throw, /*Implicit=*/false),
    SubExpr(subExpr), ThrowLoc(throwLoc) {}

  SourceLoc getThrowLoc() const { return ThrowLoc; }

  SourceLoc getStartLoc() const { return ThrowLoc; }
  SourceLoc getEndLoc() const;
  SourceRange getSourceRange() const {
    return SourceRange(ThrowLoc, getEndLoc());
  }
  
  Expr *getSubExpr() const { return SubExpr; }
  void setSubExpr(Expr *subExpr) { SubExpr = subExpr; }
  
  static bool classof(const Stmt *S) {
    return S->getKind() == StmtKind::Throw;
  }
};

} // end namespace swift

#endif // SWIFT_AST_STMT_H
