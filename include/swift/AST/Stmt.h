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

#include "swift/AST/ASTNode.h"
#include "swift/AST/Identifier.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/NullablePtr.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/PointerUnion.h"

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
  SourceLoc getStartLoc() const { return getSourceRange().Start; }
  
  /// \brief Return the location of the end of the statement.
  SourceLoc getEndLoc() const { return getSourceRange().End; }
  
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
/// { var x = 10; println(10) }.
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

  SourceRange getSourceRange() const;
  SourceLoc getReturnLoc() const { return ReturnLoc; }

  bool hasResult() const { return Result != 0; }
  Expr *getResult() const {
    assert(Result && "ReturnStmt doesn't have a result");
    return Result;
  }
  void setResult(Expr *e) { Result = e; }
  
  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::Return;}
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
  /// This is either a PBD for a variable binding or an expression for a boolean
  /// condition.
  llvm::PointerUnion<PatternBindingDecl*, Expr*> Data;
public:
  StmtConditionElement() : Data() {}
  StmtConditionElement(PatternBindingDecl *PBD) : Data(PBD) {}
  StmtConditionElement(Expr *cond) : Data(cond) {}

  bool isCondition() const {
    return Data.is<Expr*>();
  }

  bool isBinding() const {
    return Data.is<PatternBindingDecl*>();
  }

  Expr *getCondition() const {
    return Data.dyn_cast<Expr*>();
  }
  void setCondition(Expr *E) {
    Data = E;
  }
  
  PatternBindingDecl *getBinding() const {
    return Data.dyn_cast<PatternBindingDecl*>();
  }
  void setBinding(PatternBindingDecl *D) {
    Data = D;
  }
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

  // Switch and If statements are not targets of "continue".
  bool isPossibleContinueTarget() const {
    return getKind() != StmtKind::If && getKind() != StmtKind::Switch;
  }

  // If statement isn't the target of a non-labeled "break" or "continue".
  bool requiresLabelOnJump() const {
    return getKind() == StmtKind::If;
  }

  static bool classof(const Stmt *S) {
    return S->getKind() >= StmtKind::First_LabeledStmt &&
           S->getKind() <= StmtKind::Last_LabeledStmt;
  }
};


/// Either an "if let" case or a simple boolean expression can appear as the
/// condition of an 'if' or 'while' statement.
using StmtCondition = MutableArrayRef<StmtConditionElement>;

/// IfStmt - if/then/else statement.  If no 'else' is specified, then the
/// ElseLoc location is not specified and the Else statement is null. After
/// type-checking, the condition is of type Builtin.Int1.
class IfStmt : public LabeledStmt {
  SourceLoc IfLoc;
  SourceLoc ElseLoc;
  StmtCondition Cond;
  Stmt *Then;
  Stmt *Else;
  
public:
  IfStmt(LabeledStmtInfo LabelInfo, SourceLoc IfLoc, StmtCondition Cond,
         Stmt *Then, SourceLoc ElseLoc, Stmt *Else,
         Optional<bool> implicit = None)
  : LabeledStmt(StmtKind::If, getDefaultImplicitFlag(implicit, IfLoc),
                LabelInfo),
    IfLoc(IfLoc), ElseLoc(ElseLoc), Cond(Cond), Then(Then), Else(Else) {}

  IfStmt(SourceLoc IfLoc, Expr *Cond, Stmt *Then, SourceLoc ElseLoc,
         Stmt *Else, Optional<bool> implicit, ASTContext &Ctx);

  SourceLoc getIfLoc() const { return IfLoc; }
  SourceLoc getElseLoc() const { return ElseLoc; }

  SourceRange getSourceRange() const;

  StmtCondition getCond() const { return Cond; }
  void setCond(StmtCondition e) { Cond = e; }

  Stmt *getThenStmt() const { return Then; }
  void setThenStmt(Stmt *s) { Then = s; }

  Stmt *getElseStmt() const { return Else; }
  void setElseStmt(Stmt *s) { Else = s; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::If; }
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
  SourceLoc getEndLoc() const { return EndLoc; }

  bool hadMissingEnd() const { return HadMissingEnd; }

  SourceRange getSourceRange() const;
  
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
class WhileStmt : public LabeledStmt {
  SourceLoc WhileLoc;
  StmtCondition Cond;
  Stmt *Body;
  
public:
  WhileStmt(LabeledStmtInfo LabelInfo, SourceLoc WhileLoc, StmtCondition Cond,
            Stmt *Body, Optional<bool> implicit = None)
  : LabeledStmt(StmtKind::While, getDefaultImplicitFlag(implicit, WhileLoc),
                LabelInfo),
    WhileLoc(WhileLoc), Cond(Cond), Body(Body) {}

  SourceRange getSourceRange() const;

  StmtCondition getCond() const { return Cond; }
  void setCond(StmtCondition e) { Cond = e; }

  Stmt *getBody() const { return Body; }
  void setBody(Stmt *s) { Body = s; }
  
  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::While; }
};
  
/// DoWhileStmt - do/while statement. After type-checking, the condition is of
/// type Builtin.Int1.
class DoWhileStmt : public LabeledStmt {
  SourceLoc DoLoc, WhileLoc;
  Stmt *Body;
  Expr *Cond;
  
public:
  DoWhileStmt(LabeledStmtInfo LabelInfo, SourceLoc DoLoc, Expr *Cond,
              SourceLoc WhileLoc, Stmt *Body, Optional<bool> implicit = None)
    : LabeledStmt(StmtKind::DoWhile, getDefaultImplicitFlag(implicit, DoLoc),
                  LabelInfo),
      DoLoc(DoLoc), WhileLoc(WhileLoc), Body(Body), Cond(Cond) {}
  
  SourceRange getSourceRange() const;
  
  Stmt *getBody() const { return Body; }
  void setBody(Stmt *s) { Body = s; }

  Expr *getCond() const { return Cond; }
  void setCond(Expr *e) { Cond = e; }
  
  static bool classof(const Stmt *S) {return S->getKind() == StmtKind::DoWhile;}
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
  
  SourceRange getSourceRange() const;
  
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
/// for i in 0..10 {
///   println(String(i))
/// }
/// \endcode
class ForEachStmt : public LabeledStmt {
  SourceLoc ForLoc;
  SourceLoc InLoc;
  Pattern *Pat;
  Expr *Sequence;
  BraceStmt *Body;
  
  /// The generator variable along with its initializer.
  PatternBindingDecl *Generator = nullptr;
  /// The expression that advances the generator and returns an Optional with
  /// the next value or None to signal end-of-stream.
  Expr *GeneratorNext = nullptr;

public:
  ForEachStmt(LabeledStmtInfo LabelInfo, SourceLoc ForLoc, Pattern *Pat,
              SourceLoc InLoc,  Expr *Sequence, BraceStmt *Body,
              Optional<bool> implicit = None)
    : LabeledStmt(StmtKind::ForEach, getDefaultImplicitFlag(implicit, ForLoc),
                  LabelInfo),
      ForLoc(ForLoc), InLoc(InLoc), Pat(Pat),
      Sequence(Sequence), Body(Body) { }
  
  /// getForLoc - Retrieve the location of the 'for' keyword.
  SourceLoc getForLoc() const { return ForLoc; }

  /// getInLoc - Retrieve the location of the 'in' keyword.
  SourceLoc getInLoc() const { return InLoc; }
  
  /// getPattern - Retrieve the pattern describing the iteration variables.
  /// These variables will only be visible within the body of the loop.
  Pattern *getPattern() const { return Pat; }
  void setPattern(Pattern *p) { Pat = p; }
  
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
  
  SourceRange getSourceRange() const;
  
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

  SourceRange getSourceRange() const {
    return { getLoc(), getBody()->getEndLoc() };
  }

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
  SourceRange getSourceRange() const;
  
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
  
  SourceRange getSourceRange() const {
    return { Loc, TargetLoc.isValid() ? TargetLoc : Loc };
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
  
  SourceRange getSourceRange() const {
    return { Loc, TargetLoc.isValid() ? TargetLoc : Loc };
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
