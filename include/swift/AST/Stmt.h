//===--- Stmt.h - Swift Language Statement ASTs -----------------*- C++ -*-===//
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
// This file defines the Stmt class and subclasses.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_STMT_H
#define SWIFT_AST_STMT_H

#include "swift/AST/Availability.h"
#include "swift/AST/AvailabilitySpec.h"
#include "swift/AST/ASTNode.h"
#include "swift/Basic/NullablePtr.h"

namespace swift {
  class ASTContext;
  class Decl;
  class Expr;
  class ASTWalker;
  class Pattern;
  class PatternBindingDecl;
  class VarDecl;
  
enum class StmtKind {
#define STMT(ID, PARENT) ID,
#define STMT_RANGE(Id, FirstId, LastId) \
  First_##Id##Stmt = FirstId, Last_##Id##Stmt = LastId,
#include "swift/AST/StmtNodes.def"
};

/// Stmt - Base class for all statements in swift.
class alignas(8) Stmt {
  Stmt(const Stmt&) = delete;
  void operator=(const Stmt&) = delete;

  /// Kind - The subclass of Stmt that this is.
  unsigned Kind : 31;
  /// Implicit - Whether this statement is implicit.
  unsigned Implicit : 1;

protected:
  /// Return the given value for the 'implicit' flag if present, or if None,
  /// return true if the location is invalid.
  static bool getDefaultImplicitFlag(Optional<bool> implicit, SourceLoc keyLoc){
    return implicit.hasValue() ? *implicit : keyLoc.isInvalid();
  }
  
public:
  Stmt(StmtKind kind, bool implicit)
    : Kind(unsigned(kind)), Implicit(unsigned(implicit)) {}

  StmtKind getKind() const { return StmtKind(Kind); }

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
  bool isImplicit() const { return bool(Implicit); }

  /// walk - This recursively walks the AST rooted at this statement.
  Stmt *walk(ASTWalker &walker);
  Stmt *walk(ASTWalker &&walker) { return walk(walker); }

  LLVM_ATTRIBUTE_DEPRECATED(
      void dump() const LLVM_ATTRIBUTE_USED,
      "only for use within the debugger");
  void print(raw_ostream &OS, unsigned Indent = 0) const;

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
class BraceStmt : public Stmt {
private:
  unsigned NumElements;
  
  SourceLoc LBLoc;
  SourceLoc RBLoc;

  BraceStmt(SourceLoc lbloc, ArrayRef<ASTNode> elements,SourceLoc rbloc,
            Optional<bool> implicit);
  ASTNode *getElementsStorage() {
    return reinterpret_cast<ASTNode*>(this + 1);
  }

public:
  static BraceStmt *create(ASTContext &ctx, SourceLoc lbloc,
                           ArrayRef<ASTNode> elements,
                           SourceLoc rbloc,
                           Optional<bool> implicit = None);

  SourceLoc getLBraceLoc() const { return LBLoc; }
  SourceLoc getRBraceLoc() const { return RBLoc; }
  
  SourceRange getSourceRange() const { return SourceRange(LBLoc, RBLoc); }

  unsigned getNumElements() const { return NumElements; }

  ASTNode getElement(unsigned i) const { return getElements()[i]; }

  /// The elements contained within the BraceStmt.
  MutableArrayRef<ASTNode> getElements() {
    return MutableArrayRef<ASTNode>(getElementsStorage(), NumElements);
  }

  /// The elements contained within the BraceStmt (const version).
  ArrayRef<ASTNode> getElements() const {
    return const_cast<BraceStmt*>(this)->getElements();
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
  
/// DeferStmt - A 'defer' statement.  This runs the substatement it contains
/// when the enclosing scope is exited.
///
///    defer { cleanUp() }
///
/// The AST representation for a defer statement is a bit weird.  We model this
/// as if they wrote:
///
///    @noescape let tmpClosure = { body }
///    tmpClosure()   // This is emitted on each path that needs to run this.
///
/// As such, the body of the 'defer' is actually type checked within the
/// closure's DeclContext.  We do this because of unfortunateness in SILGen,
/// not every expression (e.g. OpenExistentialExpr can be multiply emitted in a
/// composable way).  When this gets fixed, patches r27767 and r27768 can be
/// reverted to go back to the simpler and more obvious representation.
///
class DeferStmt : public Stmt {
  SourceLoc DeferLoc;
  
  /// This is the pattern binding, which includes the initializer (a closure
  /// expr).
  PatternBindingDecl *patternBinding;
  
  /// This is the bound temp variable.
  VarDecl *tempDecl;

  /// This is the invocation of the closure, which is to be emitted on any error
  /// paths.
  Expr *callExpr;
  
public:
  DeferStmt(SourceLoc DeferLoc, PatternBindingDecl *patternBinding,
            VarDecl *tempDecl, Expr *callExpr)
    : Stmt(StmtKind::Defer, /*implicit*/false),
      DeferLoc(DeferLoc), patternBinding(patternBinding), tempDecl(tempDecl),
      callExpr(callExpr) {}
  
  SourceLoc getDeferLoc() const { return DeferLoc; }
  
  SourceLoc getStartLoc() const { return DeferLoc; }
  SourceLoc getEndLoc() const;

  PatternBindingDecl *getPatternBinding() const { return patternBinding; }
  VarDecl *getTempDecl() const { return tempDecl; }
  Expr *getCallExpr() const { return callExpr; }
  void setCallExpr(Expr *E) { callExpr = E; }

  /// Dig the original users's body of the defer out for AST fidelity.
  BraceStmt *getBodyAsWritten() const;
  
  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::Defer; }
};

  
/// \brief An expression that guards execution based on whether the run-time
/// configuration supports a given API, e.g.,
/// #available(OSX >= 10.9, iOS >= 7.0).
class alignas(8) PoundAvailableInfo {
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
    memcpy((void*)getQueries().data(), queries.data(),
           queries.size() * sizeof(AvailabilitySpec *));
  }
  
public:
  static PoundAvailableInfo *create(ASTContext &ctx, SourceLoc PoundLoc,
                                    ArrayRef<AvailabilitySpec *> queries,
                                    SourceLoc RParenLoc);
  
  ArrayRef<AvailabilitySpec *> getQueries() const {
    auto buf = reinterpret_cast<AvailabilitySpec *const*>(this + 1);
    return ArrayRef<AvailabilitySpec *>(buf, NumQueries);
  }
  
  SourceLoc getStartLoc() const { return PoundLoc; }
  SourceLoc getEndLoc() const;
  SourceLoc getLoc() const { return PoundLoc; }
  SourceRange getSourceRange() const { return SourceRange(getStartLoc(),
                                                          getEndLoc()); }
  
  const VersionRange &getAvailableRange() const { return AvailableRange; }
  void setAvailableRange(const VersionRange &Range) { AvailableRange = Range; }
  
  void getPlatformKeywordRanges(SmallVectorImpl<CharSourceRange>
                                &PlatformRanges);
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
      ErrorPattern(errorPattern), GuardExpr(guardExpr), CatchBody(body) {}

  SourceLoc getCatchLoc() const { return CatchLoc; }

  /// The location of the 'where' keyword if there's a guard expression.
  SourceLoc getWhereLoc() const { return WhereLoc; }

  SourceLoc getStartLoc() const { return CatchLoc; }
  SourceLoc getEndLoc() const { return CatchBody->getEndLoc(); }

  Stmt *getBody() const { return CatchBody; }
  void setBody(Stmt *body) { CatchBody = body; }

  Pattern *getErrorPattern() { return ErrorPattern; }
  const Pattern *getErrorPattern() const { return ErrorPattern; }
  void setErrorPattern(Pattern *pattern) { ErrorPattern = pattern; }

  /// Is this catch clause "syntactically exhaustive"?
  bool isSyntacticallyExhaustive() const;

  /// Return the guard expression if present, or null if the catch has
  /// no guard.
  Expr *getGuardExpr() const { return GuardExpr; }
  void setGuardExpr(Expr *guard) { GuardExpr = guard; }

  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::Catch; }
};

/// DoCatchStmt - do statement with trailing 'catch' clauses.
class DoCatchStmt : public LabeledStmt {
  SourceLoc DoLoc;
  Stmt *Body;
  unsigned NumCatches;

  CatchStmt **getCatchesBuffer() {
    return reinterpret_cast<CatchStmt **>(this+1);
  }
  CatchStmt * const *getCatchesBuffer() const {
    return reinterpret_cast<CatchStmt * const *>(this+1);
  }
  
  DoCatchStmt(LabeledStmtInfo labelInfo, SourceLoc doLoc,
              Stmt *body, ArrayRef<CatchStmt*> catches,
              Optional<bool> implicit)
    : LabeledStmt(StmtKind::DoCatch, getDefaultImplicitFlag(implicit, doLoc),
                  labelInfo),
      DoLoc(doLoc), Body(body), NumCatches(catches.size()) {
    memcpy(getCatchesBuffer(), catches.data(),
           catches.size() * sizeof(catches[0]));
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
    return ArrayRef<CatchStmt*>(getCatchesBuffer(), NumCatches);
  }
  MutableArrayRef<CatchStmt*> getMutableCatches() {
    return MutableArrayRef<CatchStmt*>(getCatchesBuffer(), NumCatches);
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
    : LabeledStmt(Kind, Implicit, LabelInfo), Cond(Cond) {}

  StmtCondition getCond() const { return Cond; }
  void setCond(StmtCondition e) { Cond = e; }

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

  
/// This represents one part of a #if block.  If the condition field is
/// non-null, then this represents a #if or a #elseif, otherwise it represents
/// an #else block.
struct IfConfigStmtClause {
  /// The location of the #if, #elseif, or #else keyword.
  SourceLoc Loc;
  
  /// The condition guarding this #if or #elseif block.  If this is null, this
  /// is a #else clause.
  Expr *Cond;
  
  /// Elements inside the clause
  ArrayRef<ASTNode> Elements;
  
  /// True if this is the active clause of the #if block.  Since this is
  /// evaluated at parse time, this is always known.
  bool isActive;
  
  IfConfigStmtClause(SourceLoc Loc, Expr *Cond,
                     ArrayRef<ASTNode> Elements, bool isActive)
    : Loc(Loc), Cond(Cond), Elements(Elements), isActive(isActive) {
  }
};

/// IfConfigStmt - This class models the statement-side representation of
/// #if/#else/#endif blocks.
class IfConfigStmt : public Stmt {
  /// An array of clauses controlling each of the #if/#elseif/#else conditions.
  /// The array is ASTContext allocated.
  ArrayRef<IfConfigStmtClause> Clauses;
  SourceLoc EndLoc;
  bool HadMissingEnd;

public:
  IfConfigStmt(ArrayRef<IfConfigStmtClause> Clauses, SourceLoc EndLoc,
               bool HadMissingEnd)
  : Stmt(StmtKind::IfConfig, /*implicit=*/false),
    Clauses(Clauses), EndLoc(EndLoc), HadMissingEnd(HadMissingEnd) {}
  
  SourceLoc getIfLoc() const { return Clauses[0].Loc; }

  SourceLoc getStartLoc() const { return getIfLoc(); }
  SourceLoc getEndLoc() const { return EndLoc; }

  bool hadMissingEnd() const { return HadMissingEnd; }

  const ArrayRef<IfConfigStmtClause> &getClauses() const { return Clauses; }
  
  ArrayRef<ASTNode> getActiveClauseElements() const {
    for (auto &Clause : Clauses)
      if (Clause.isActive)
        return Clause.Elements;
    return ArrayRef<ASTNode>();
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Stmt *S) {
    return S->getKind() == StmtKind::IfConfig;
  }
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

/// ForStmt - for statement.  After type-checking, the condition is of
/// type Builtin.Int1.  Note that the condition is optional.  If not present,
/// it always evaluates to true.  The Initializer and Increment are also
/// optional.
class ForStmt : public LabeledStmt {
  SourceLoc ForLoc, Semi1Loc, Semi2Loc;
  NullablePtr<Expr> Initializer;
  ArrayRef<Decl*> InitializerVarDecls;
  NullablePtr<Expr> Cond;
  NullablePtr<Expr> Increment;
  Stmt *Body;
  
public:
  ForStmt(LabeledStmtInfo LabelInfo, SourceLoc ForLoc,
          NullablePtr<Expr> Initializer,
          ArrayRef<Decl*> InitializerVarDecls,
          SourceLoc Semi1Loc, NullablePtr<Expr> Cond, SourceLoc Semi2Loc,
          NullablePtr<Expr> Increment,
          Stmt *Body,
          Optional<bool> implicit = None)
  : LabeledStmt(StmtKind::For, getDefaultImplicitFlag(implicit, ForLoc),
                LabelInfo),
    ForLoc(ForLoc), Semi1Loc(Semi1Loc),
    Semi2Loc(Semi2Loc), Initializer(Initializer),
    InitializerVarDecls(InitializerVarDecls),
    Cond(Cond), Increment(Increment), Body(Body) {
  }
  
  SourceLoc getStartLoc() const { return getLabelLocOrKeywordLoc(ForLoc); }
  SourceLoc getEndLoc() const { return Body->getEndLoc(); }
  
  NullablePtr<Expr> getInitializer() const { return Initializer; }
  void setInitializer(Expr *V) { Initializer = V; }
  
  ArrayRef<Decl*> getInitializerVarDecls() const { return InitializerVarDecls; }
  void setInitializerVarDecls(ArrayRef<Decl*> D) { InitializerVarDecls = D; }

  NullablePtr<Expr> getCond() const { return Cond; }
  void setCond(NullablePtr<Expr> C) { Cond = C; }

  NullablePtr<Expr> getIncrement() const { return Increment; }
  void setIncrement(Expr *V) { Increment = V; }

  Stmt *getBody() const { return Body; }
  void setBody(Stmt *s) { Body = s; }
  
  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::For; }
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
  
  /// The generator variable along with its initializer.
  PatternBindingDecl *Generator = nullptr;
  /// The expression that advances the generator and returns an Optional with
  /// the next value or None to signal end-of-stream.
  Expr *GeneratorNext = nullptr;

public:
  ForEachStmt(LabeledStmtInfo LabelInfo, SourceLoc ForLoc, Pattern *Pat,
              SourceLoc InLoc, Expr *Sequence, Expr *WhereExpr, BraceStmt *Body,
              Optional<bool> implicit = None)
    : LabeledStmt(StmtKind::ForEach, getDefaultImplicitFlag(implicit, ForLoc),
                  LabelInfo),
      ForLoc(ForLoc), Pat(Pat), InLoc(InLoc), Sequence(Sequence),
      WhereExpr(WhereExpr), Body(Body) { }
  
  /// getForLoc - Retrieve the location of the 'for' keyword.
  SourceLoc getForLoc() const { return ForLoc; }

  /// getInLoc - Retrieve the location of the 'in' keyword.
  SourceLoc getInLoc() const { return InLoc; }
  
  /// getPattern - Retrieve the pattern describing the iteration variables.
  /// These variables will only be visible within the body of the loop.
  Pattern *getPattern() const { return Pat; }
  void setPattern(Pattern *p) { Pat = p; }

  Expr *getWhere() const { return WhereExpr; }
  void setWhere(Expr *W) { WhereExpr = W; }

  /// getSequence - Retrieve the Sequence whose elements will be visited
  /// by this foreach loop, as it was written in the source code and
  /// subsequently type-checked. To determine the semantic behavior of this
  /// expression to extract a range, use \c getRangeInit().
  Expr *getSequence() const { return Sequence; }
  void setSequence(Expr *S) { Sequence = S; }
  
  /// Retrieve the pattern binding that contains the (implicit) generator
  /// variable and its initialization from the container.
  PatternBindingDecl *getGenerator() const { return Generator; }
  void setGenerator(PatternBindingDecl *G) { Generator = G; }
  
  /// Retrieve the expression that advances the generator.
  Expr *getGeneratorNext() const { return GeneratorNext; }
  void setGeneratorNext(Expr *E) { GeneratorNext = E; }

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
  Pattern *CasePattern;
  SourceLoc WhereLoc;
  llvm::PointerIntPair<Expr *, 1, bool> GuardExprAndIsDefault;

public:
  CaseLabelItem(const CaseLabelItem &) = default;

  CaseLabelItem(bool IsDefault, Pattern *CasePattern, SourceLoc WhereLoc,
                Expr *GuardExpr)
      : CasePattern(CasePattern), WhereLoc(WhereLoc),
        GuardExprAndIsDefault(GuardExpr, IsDefault) {}

  SourceLoc getWhereLoc() const { return WhereLoc; }

  SourceLoc getStartLoc() const;
  SourceLoc getEndLoc() const;
  SourceRange getSourceRange() const;

  Pattern *getPattern() { return CasePattern; }
  const Pattern *getPattern() const { return CasePattern; }
  void setPattern(Pattern *CasePattern) { this->CasePattern = CasePattern; }

  /// Return the guard expression if present, or null if the case label has
  /// no guard.
  Expr *getGuardExpr() { return GuardExprAndIsDefault.getPointer(); }
  const Expr *getGuardExpr() const {
    return GuardExprAndIsDefault.getPointer();
  }
  void setGuardExpr(Expr *e) { GuardExprAndIsDefault.setPointer(e); }

  /// Returns true if this is syntactically a 'default' label.
  bool isDefault() const { return GuardExprAndIsDefault.getInt(); }
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

class CaseStmt : public Stmt {
  SourceLoc CaseLoc;
  SourceLoc ColonLoc;

  llvm::PointerIntPair<Stmt *, 1, bool> BodyAndHasBoundDecls;
  unsigned NumPatterns;

  const CaseLabelItem *getCaseLabelItemsBuffer() const {
    return reinterpret_cast<const CaseLabelItem *>(this + 1);
  }
  CaseLabelItem *getCaseLabelItemsBuffer() {
    return reinterpret_cast<CaseLabelItem *>(this + 1);
  }

  CaseStmt(SourceLoc CaseLoc, ArrayRef<CaseLabelItem> CaseLabelItems,
           bool HasBoundDecls, SourceLoc ColonLoc, Stmt *Body,
           Optional<bool> Implicit);

public:
  static CaseStmt *create(ASTContext &C, SourceLoc CaseLoc,
                          ArrayRef<CaseLabelItem> CaseLabelItems,
                          bool HasBoundDecls, SourceLoc ColonLoc, Stmt *Body,
                          Optional<bool> Implicit = None);

  ArrayRef<CaseLabelItem> getCaseLabelItems() const {
    return { getCaseLabelItemsBuffer(), NumPatterns };
  }
  MutableArrayRef<CaseLabelItem> getMutableCaseLabelItems() {
    return { getCaseLabelItemsBuffer(), NumPatterns };
  }

  Stmt *getBody() const { return BodyAndHasBoundDecls.getPointer(); }
  void setBody(Stmt *body) { BodyAndHasBoundDecls.setPointer(body); }

  /// True if the case block declares any patterns with local variable bindings.
  bool hasBoundDecls() const { return BodyAndHasBoundDecls.getInt(); }

  /// Get the source location of the 'case' or 'default' of the first label.
  SourceLoc getLoc() const { return CaseLoc; }

  SourceLoc getStartLoc() const { return getLoc(); }
  SourceLoc getEndLoc() const { return getBody()->getEndLoc(); }

  bool isDefault() { return getCaseLabelItems()[0].isDefault(); }

  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::Case; }
};

/// Switch statement.
class SwitchStmt : public LabeledStmt {
  SourceLoc SwitchLoc, LBraceLoc, RBraceLoc;
  Expr *SubjectExpr;
  unsigned CaseCount;
  
  CaseStmt * const *getCaseBuffer() const {
    return reinterpret_cast<CaseStmt * const *>(this + 1);
  }

  CaseStmt **getCaseBuffer() {
    return reinterpret_cast<CaseStmt **>(this + 1);
  }
  
  SwitchStmt(LabeledStmtInfo LabelInfo, SourceLoc SwitchLoc, Expr *SubjectExpr,
             SourceLoc LBraceLoc, unsigned CaseCount, SourceLoc RBraceLoc,
             Optional<bool> implicit = None)
    : LabeledStmt(StmtKind::Switch, getDefaultImplicitFlag(implicit, SwitchLoc),
                  LabelInfo),
      SwitchLoc(SwitchLoc), LBraceLoc(LBraceLoc), RBraceLoc(RBraceLoc),
      SubjectExpr(SubjectExpr), CaseCount(CaseCount)
  {}

public:
  /// Allocate a new SwitchStmt in the given ASTContext.
  static SwitchStmt *create(LabeledStmtInfo LabelInfo, SourceLoc SwitchLoc,
                            Expr *SubjectExpr,
                            SourceLoc LBraceLoc,
                            ArrayRef<CaseStmt*> Cases,
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
  
  /// Get the list of case clauses.
  ArrayRef<CaseStmt*> getCases() const {
    return {getCaseBuffer(), CaseCount};
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
  LabeledStmt *Target;  // Target stmt, wired up by Sema.
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
  LabeledStmt *Target;

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
  CaseStmt *FallthroughDest;
  
public:
  FallthroughStmt(SourceLoc Loc, Optional<bool> implicit = None)
    : Stmt(StmtKind::Fallthrough, getDefaultImplicitFlag(implicit, Loc)),
      Loc(Loc), FallthroughDest(nullptr)
  {}
  
  SourceLoc getLoc() const { return Loc; }
  
  SourceRange getSourceRange() const { return Loc; }
  
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

} // end namespace swift

#endif
