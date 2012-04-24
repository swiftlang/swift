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

#include "swift/AST/LLVM.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/NullablePtr.h"
#include "llvm/ADT/PointerUnion.h"

namespace swift {
  class ASTContext;
  class Decl;
  class Expr;
  class ASTWalker;
  class Pattern;
  class PatternBindingDecl;
  
enum class StmtKind {
#define STMT(ID, PARENT) ID,
#include "swift/AST/StmtNodes.def"
};

/// Stmt - Base class for all statements in swift.
class Stmt {
  Stmt(const Stmt&) = delete;
  void operator=(const Stmt&) = delete;

  /// Kind - The subclass of Stmt that this is.
  const StmtKind Kind;
  
public:
  Stmt(StmtKind kind) : Kind(kind) {}

  StmtKind getKind() const { return Kind; }

  /// \brief Return the location of the start of the statement.
  SourceLoc getStartLoc() const { return getSourceRange().Start; }
  
  /// \brief Return the location of the end of the statement.
  SourceLoc getEndLoc() const { return getSourceRange().End; }
  
  SourceRange getSourceRange() const;
  
  /// walk - This recursively walks the AST rooted at this statement.
  Stmt *walk(ASTWalker &walker);
  Stmt *walk(ASTWalker &&walker) { return walk(walker); }
  
  
  void dump() const;
  void print(raw_ostream &OS, unsigned Indent = 0) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Stmt *) { return true; }

  enum { Alignment = 8 };

  // Only allow allocation of Exprs using the allocator in ASTContext
  // or by doing a placement new.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = Stmt::Alignment);
  
  // Make placement new and vanilla new/delete illegal for Exprs.
  void *operator new(size_t Bytes) throw() = delete;
  void operator delete(void *Data) throw() = delete;
  void *operator new(size_t Bytes, void *Mem) throw() = delete;

};
  
/// ErrorStmt - Represents a semantically erroneous statement in the AST.
class ErrorStmt : public Stmt {
  SourceRange Range;
public:
  ErrorStmt(SourceRange Range) : Stmt(StmtKind::Error), Range(Range) {}
  
  SourceRange getSourceRange() const { return Range; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const ErrorStmt *) { return true; }
  static bool classof(const Stmt *S) {
    return S->getKind() == StmtKind::Error;
  }
};


/// SemiStmt - A semicolon, the noop statement: ";"
class SemiStmt : public Stmt {
  SourceLoc Loc;
  
public:
  SemiStmt(SourceLoc Loc) : Stmt(StmtKind::Semi), Loc(Loc) {}

  SourceLoc getLoc() const { return Loc; }
  
  SourceRange getSourceRange() const { return Loc; }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const SemiStmt *) { return true; }
  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::Semi; }
};

/// AssignStmt - A value assignment, like "x = y".
class AssignStmt : public Stmt {
  Expr *Dest;
  Expr *Src;
  SourceLoc EqualLoc;

public:  
  AssignStmt(Expr *Dest, SourceLoc EqualLoc, Expr *Src)
    : Stmt(StmtKind::Assign), Dest(Dest), Src(Src), EqualLoc(EqualLoc) {}

  Expr *getDest() const { return Dest; }
  void setDest(Expr *e) { Dest = e; }
  Expr *getSrc() const { return Src; }
  void setSrc(Expr *e) { Src = e; }
  
  SourceLoc getEqualLoc() const { return EqualLoc; }
  
  SourceRange getSourceRange() const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const AssignStmt *) { return true; }
  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::Assign; }
};


/// BraceStmt - A brace enclosed sequence of expressions, stmts, or decls, like
/// { 4; 5 }.
class BraceStmt : public Stmt {
public:
  typedef llvm::PointerUnion3<Expr*, Stmt*, Decl*> ExprStmtOrDecl;

private:
  unsigned NumElements;
  
  SourceLoc LBLoc;
  SourceLoc RBLoc;

  BraceStmt(SourceLoc lbloc, ArrayRef<ExprStmtOrDecl> elements,SourceLoc rbloc);
  ExprStmtOrDecl *getElementsStorage() {
    return reinterpret_cast<ExprStmtOrDecl*>(this + 1);
  }
  const ExprStmtOrDecl *getElementsStorage() const {
    return const_cast<BraceStmt*>(this)->getElementsStorage();
  }

public:
  static BraceStmt *create(ASTContext &ctx, SourceLoc lbloc,
                           ArrayRef<ExprStmtOrDecl> elements,
                           SourceLoc rbloc);

  SourceLoc getLBraceLoc() const { return LBLoc; }
  SourceLoc getRBraceLoc() const { return RBLoc; }
  
  SourceRange getSourceRange() const { return SourceRange(LBLoc, RBLoc); }

  unsigned getNumElements() const { return NumElements; }
  ArrayRef<ExprStmtOrDecl> getElements() const {
    return ArrayRef<ExprStmtOrDecl>(getElementsStorage(), NumElements);
  }
  ExprStmtOrDecl getElement(unsigned i) const {
    assert(i < NumElements && "index out of range!");
    return getElementsStorage()[i];
  }
  void setElement(unsigned i, ExprStmtOrDecl elt) {
    assert(i < NumElements && "index out of range!");
    getElementsStorage()[i] = elt;
  }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const BraceStmt *) { return true; }
  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::Brace; }
};

/// ReturnStmt - A return statement.  Return statements with no specified
/// subexpression are expanded into a return of the empty tuple in the parser.
///    return 42
class ReturnStmt : public Stmt {
  SourceLoc ReturnLoc;
  Expr *Result;
  
public:
  ReturnStmt(SourceLoc ReturnLoc, Expr *Result)
    : Stmt(StmtKind::Return), ReturnLoc(ReturnLoc), Result(Result) {}

  SourceRange getSourceRange() const;
  SourceLoc getReturnLoc() const { return ReturnLoc; }

  Expr *getResult() const { return Result; }
  void setResult(Expr *e) { Result = e; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const ReturnStmt *) { return true; }
  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::Return; }
};

/// IfStmt - if/then/else statement.  If no 'else' is specified, then the
/// ElseLoc location is not specified and the Else statement is null.  The
/// condition of the 'if' is required to have a Builtin.Int1 type.
class IfStmt : public Stmt {
  SourceLoc IfLoc;
  SourceLoc ElseLoc;
  Expr *Cond;
  Stmt *Then;
  Stmt *Else;
  
public:
  IfStmt(SourceLoc IfLoc, Expr *Cond, Stmt *Then, SourceLoc ElseLoc,
         Stmt *Else)
  : Stmt(StmtKind::If),
    IfLoc(IfLoc), ElseLoc(ElseLoc), Cond(Cond), Then(Then), Else(Else) {}

  SourceLoc getIfLoc() const { return IfLoc; }
  SourceLoc getElseLoc() const { return ElseLoc; }

  SourceRange getSourceRange() const;

  Expr *getCond() const { return Cond; }
  void setCond(Expr *e) { Cond = e; }

  Stmt *getThenStmt() const { return Then; }
  void setThenStmt(Stmt *s) { Then = s; }

  Stmt *getElseStmt() const { return Else; }
  void setElseStmt(Stmt *s) { Else = s; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const IfStmt *) { return true; }
  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::If; }
};

/// WhileStmt - while statement.  The condition is required to have a
/// Builtin.Int1 type.
class WhileStmt : public Stmt {
  SourceLoc WhileLoc;
  Expr *Cond;
  Stmt *Body;
  
public:
  WhileStmt(SourceLoc WhileLoc, Expr *Cond, Stmt *Body)
  : Stmt(StmtKind::While),
    WhileLoc(WhileLoc), Cond(Cond), Body(Body) {}

  SourceRange getSourceRange() const;

  Expr *getCond() const { return Cond; }
  void setCond(Expr *e) { Cond = e; }

  Stmt *getBody() const { return Body; }
  void setBody(Stmt *s) { Body = s; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const WhileStmt *) { return true; }
  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::While; }
};

/// ForStmt - for statement.  The condition is required to have a
/// Builtin.Int1 type.  Note that the condition is optional.  If not present,
/// it always evaluates to one.  The Initializer and Increment are also
/// optional.
class ForStmt : public Stmt {
  SourceLoc ForLoc, LPLoc, Semi1Loc, Semi2Loc, RPLoc;
  PointerUnion<Expr*, AssignStmt*> Initializer;
  NullablePtr<Expr> Cond;
  PointerUnion<Expr*, AssignStmt*> Increment;
  Stmt *Body;
  
public:
  ForStmt(SourceLoc ForLoc, SourceLoc LPLoc, 
          PointerUnion<Expr*, AssignStmt*> Initializer,
          SourceLoc Semi1Loc, NullablePtr<Expr> Cond, SourceLoc Semi2Loc,
          PointerUnion<Expr*, AssignStmt*> Increment, SourceLoc RPLoc,
          Stmt *Body)
  : Stmt(StmtKind::For), ForLoc(ForLoc), LPLoc(LPLoc), Semi1Loc(Semi1Loc),
    Semi2Loc(Semi2Loc), RPLoc(RPLoc), Initializer(Initializer), 
    Cond(Cond), Increment(Increment), Body(Body) {
  }
  
  SourceRange getSourceRange() const {
    return SourceRange(ForLoc, Body->getEndLoc());
  }
  
  PointerUnion<Expr*, AssignStmt*> getInitializer() const { return Initializer;}
  void setInitializer(PointerUnion<Expr*, AssignStmt*> V) { Initializer = V; }
  NullablePtr<Expr> getCond() const { return Cond; }
  void setCond(NullablePtr<Expr> C) { Cond = C; }
  PointerUnion<Expr*, AssignStmt*> getIncrement() const { return Increment; }
  void setIncrement(PointerUnion<Expr*, AssignStmt*> V) { Increment = V; }
  Stmt *getBody() const { return Body; }
  void setBody(Stmt *s) { Body = s; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const ForStmt *) { return true; }
  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::For; }
};

/// ForEachStmt - foreach statement that iterates over the elements in a
/// container.
///
/// Example:
/// \code
/// foreach i in 0..10 {
///   println(String(i))
/// }
/// \endcode
class ForEachStmt : public Stmt {
  SourceLoc ForEachLoc;
  SourceLoc InLoc;
  llvm::PointerUnion<Pattern *, PatternBindingDecl *> Pat;
  Expr *Container;
  BraceStmt *Body;
  
  /// Range - The range variable along with its initializer.
  PatternBindingDecl *Range;
    
  /// RangeEmpty - The expression that determines whether the range is empty.
  Expr *RangeEmpty;
  
  /// RangeDropFirst - The expression that drops the first element in the
  /// range.
  Expr *RangeDropFirst;

public:
  ForEachStmt(SourceLoc ForEachLoc, Pattern *Pat, SourceLoc InLoc,
              Expr *Container, BraceStmt *Body)
    : Stmt(StmtKind::ForEach), ForEachLoc(ForEachLoc), InLoc(InLoc), Pat(Pat),
      Container(Container), Body(Body), Range(), RangeEmpty(),
      RangeDropFirst() { }
  
  /// getForEachLoc - Retrieve the location of the 'foreach' keyword.
  SourceLoc getForEachLoc() const { return ForEachLoc; }

  /// getInLoc - Retrieve the location of the 'in' keyword.
  SourceLoc getInLoc() const { return InLoc; }
  
  /// getPattern - Retrieve the pattern describing the iteration variables.
  /// These variables will only be visible within the body of the loop.
  Pattern *getPattern() const;
  
  /// getContainer - Retrieve the container whose elements will be visited
  /// by this foreach loop, as it was written in the source code and
  /// subsequently type-checked. To determine the semantic behavior of this
  /// expression to extract a range, use \c getRangeInit().
  Expr *getContainer() const { return Container; }
  void setContainer(Expr *C) { Container = C; }
  
  /// getRange - Retrieve the pattern binding that contains the (implicit)
  /// range variable and its initialization from the container.
  PatternBindingDecl *getRange() const { return Range; }
  void setRange(PatternBindingDecl *R) { Range = R; }
  
  /// getRangeEmpty - Retrieve the expression that determines whether the
  /// given range is empty.
  Expr *getRangeEmpty() const { return RangeEmpty; }
  void setRangeEmpty(Expr *E) { RangeEmpty = E; }
  
  /// getElementInit - Retrieve the pattern binding that binds the pattern
  /// (with the iteration variables) to the initialization of that pattern
  /// from the result of getFirst().
  PatternBindingDecl *getElementInit() const {
    return Pat.dyn_cast<PatternBindingDecl *>();
  }
  void setElementInit(PatternBindingDecl *EI) { Pat = EI; }

  /// getRangeDropFirst - Retrieve the expression that drops the first element
  /// from the range, modifying the range in-place.
  Expr *getRangeDropFirst() const { return RangeDropFirst; }
  void setRangeDropFirst(Expr *E) { RangeDropFirst = E; }

  /// getBody - Retrieve the body of the loop.
  BraceStmt *getBody() const { return Body; }
  void setBody(BraceStmt *B) { Body = B; }
  
  SourceRange getSourceRange() const {
    return SourceRange(ForEachLoc, Body->getEndLoc());
  }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const ForEachStmt *) { return true; }
  static bool classof(const Stmt *S) {
    return S->getKind() == StmtKind::ForEach;
  }
};
  
} // end namespace swift

#endif
