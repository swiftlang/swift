//===--- Expr.cpp - Swift Language Expression ASTs ------------------------===//
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
//  This file implements the Expr class and subclasses.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Expr.h"
#include "swift/AST/ExprVisitor.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Type.h"
#include "swift/AST/ASTContext.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/ErrorHandling.h"
using namespace swift;
using llvm::cast;

//===----------------------------------------------------------------------===//
// Expr methods.
//===----------------------------------------------------------------------===//

// Only allow allocation of Stmts using the allocator in ASTContext.
void *Expr::operator new(size_t Bytes, ASTContext &C,
                         unsigned Alignment) throw() {
  return C.Allocate(Bytes, Alignment);
}

/// getLocStart - Return the location of the start of the expression.
/// FIXME: Need to extend this to do full source ranges like Clang.
llvm::SMLoc Expr::getLocStart() const {
  switch (Kind) {
  case IntegerLiteralKind: return cast<IntegerLiteral>(this)->Loc;
  case DeclRefExprKind:    return cast<DeclRefExpr>(this)->Loc;
  case UnresolvedMemberExprKind:
    return cast<UnresolvedMemberExpr>(this)->ColonLoc;
  case TupleExprKind:      return cast<TupleExpr>(this)->LParenLoc;
  case UnresolvedDotExprKind:
    return cast<UnresolvedDotExpr>(this)->SubExpr->getLocStart();
  case TupleElementExprKind:
    return cast<TupleElementExpr>(this)->SubExpr->getLocStart();
  case ApplyExprKind:      return cast<ApplyExpr>(this)->Fn->getLocStart();
  case SequenceExprKind:
    return cast<SequenceExpr>(this)->Elements[0]->getLocStart();
  case BraceExprKind:      return cast<BraceExpr>(this)->LBLoc;
  case ClosureExprKind:    return cast<ClosureExpr>(this)->Input->getLocStart();
  case BinaryExprKind:     return cast<BinaryExpr>(this)->LHS->getLocStart();
  }
  
  llvm_unreachable("expression type not handled!");
}

//===----------------------------------------------------------------------===//
// Support methods for Exprs.
//===----------------------------------------------------------------------===//

/// getNumArgs - Return the number of arguments that this closure expr takes.
/// This is the length of the ArgList.
unsigned ClosureExpr::getNumArgs() const {
  // FIXME: This should desugar the type if needed!
  Type *Input = cast<FunctionType>(Ty)->Input;
  
  if (TupleType *TT = llvm::dyn_cast<TupleType>(Input))
    return TT->NumFields;
  return 1;  
}

uint64_t IntegerLiteral::getValue() const {
  unsigned long long IntVal;
  bool Error = Val.getAsInteger(0, IntVal);
  assert(!Error && "Invalid IntegerLiteral formed"); (void)Error;
  return IntVal;
}


//===----------------------------------------------------------------------===//
// Expression Walking
//===----------------------------------------------------------------------===//

namespace {
  /// ExprWalker - This class implements a simple expression walker which
  /// invokes a function pointer on every expression in an AST.  If the function
  /// pointer returns true the walk is terminated.
  class ExprWalker : public ExprVisitor<ExprWalker, Expr*> {
    friend class ExprVisitor<ExprWalker, Expr*>;
    Expr *(*Fn)(Expr *E, Expr::WalkOrder Order, void *Data);
    void *Data;
    
    
    Expr *VisitIntegerLiteral(IntegerLiteral *E) { return E; }
    Expr *VisitDeclRefExpr(DeclRefExpr *E) { return E; }
    Expr *VisitUnresolvedMemberExpr(UnresolvedMemberExpr *E) { return E; }
    
    Expr *VisitTupleExpr(TupleExpr *E) {
      for (unsigned i = 0, e = E->NumSubExprs; i != e; ++i)
        if (Expr *Elt = ProcessNode(E->SubExprs[i]))
          E->SubExprs[i] = Elt;
        else
          return 0;
      return E;
    }
    Expr *VisitUnresolvedDotExpr(UnresolvedDotExpr *E) {
      if (Expr *E2 = ProcessNode(E->SubExpr)) {
        E->SubExpr = E2;
        return E;
      }
      return 0;
    }
    Expr *VisitTupleElementExpr(TupleElementExpr *E) {
      if (Expr *E2 = ProcessNode(E->SubExpr)) {
        E->SubExpr = E2;
        return E;
      }
      return 0;
    }
    
    Expr *VisitApplyExpr(ApplyExpr *E) {
      Expr *E2 = ProcessNode(E->Fn);
      if (E2 == 0) return 0;
      E->Fn = E2;
      
      E2 = ProcessNode(E->Arg);
      if (E2 == 0) return 0;
      E->Arg = E2;
      return E;
    }
    Expr *VisitSequenceExpr(SequenceExpr *E) {
      for (unsigned i = 0, e = E->NumElements; i != e; ++i)
        if (Expr *Elt = ProcessNode(E->Elements[i]))
          E->Elements[i] = Elt;
        else
          return 0;
      return E;
    }
    Expr *VisitBraceExpr(BraceExpr *E) {
      for (unsigned i = 0, e = E->NumElements; i != e; ++i)
        if (Expr *SubExpr = E->Elements[i].dyn_cast<Expr*>()) {
          if (Expr *E2 = ProcessNode(SubExpr))
            E->Elements[i] = E2;
          else
            return 0;
        } else if (Expr *Init = E->Elements[i].get<ValueDecl*>()->Init) {
          if (Expr *E2 = ProcessNode(Init))
            E->Elements[i].get<ValueDecl*>()->Init = E2;
          else
            return 0;
        }
      
      return E;
    }
    Expr *VisitClosureExpr(ClosureExpr *E) {
      if (Expr *E2 = ProcessNode(E->Input)) {
        E->Input = E2;
        return E;
      }
      return 0;
    }
    Expr *VisitBinaryExpr(BinaryExpr *E) {
      Expr *E2 = ProcessNode(E->LHS);
      if (E2 == 0) return 0;
      E->LHS = E2;
      
      E2 = ProcessNode(E->RHS);
      if (E2 == 0) return 0;
      E->RHS = E2;
      return E;
    }
    
    Expr *ProcessNode(Expr *E) {
      E = Fn(E, Expr::Walk_PreOrder, Data);
      if (E) E = Visit(E);
      if (E) E = Fn(E, Expr::Walk_PostOrder, Data);
      return E;
    }
    
  public:
    ExprWalker(Expr *(*fn)(Expr *E, Expr::WalkOrder Order, void *Data),
               void *data) : Fn(fn), Data(data) {
    }
    Expr *doIt(Expr *E) {
      return ProcessNode(E);
    }
  };
} // end anonymous namespace.

/// WalkExpr - This function walks all the subexpressions under this
/// expression and invokes the specified function pointer on them.  The
/// function pointer is invoked both before and after the children are visted,
/// the WalkOrder specifies at each invocation which stage it is.  If the
/// function pointer returns true then the walk is terminated and WalkExpr
/// returns true.
/// 
Expr *Expr::WalkExpr(Expr *(*Fn)(Expr *E, WalkOrder Order, void *Data),
                     void *Data) {
  return ExprWalker(Fn, Data).doIt(this);  
}


//===----------------------------------------------------------------------===//
// Printing for Expr and all subclasses.
//===----------------------------------------------------------------------===//

namespace {
/// PrintExpr - Visitor implementation of Expr::print.
class PrintExpr : public ExprVisitor<PrintExpr> {
public:
  llvm::raw_ostream &OS;
  unsigned Indent;
  
  PrintExpr(llvm::raw_ostream &os, unsigned indent) : OS(os), Indent(indent) {
  }
  
  void PrintRec(Expr *E) {
    Indent += 2;
    Visit(E);
    Indent -= 2;
  }
  
  void PrintRec(Decl *D) {
    D->print(OS, Indent+2);
  }

  void VisitIntegerLiteral(IntegerLiteral *E) {
    OS.indent(Indent) << "(integer_literal type='";
    E->Ty->print(OS);
    OS << "' value=" << E->Val << ')';
  }
  void VisitDeclRefExpr(DeclRefExpr *E) {
    OS.indent(Indent) << "(declref_expr type='";
    E->Ty->print(OS);
    OS << "' decl=" << E->D->Name << ')';
  }
  void VisitUnresolvedMemberExpr(UnresolvedMemberExpr *E) {
    OS.indent(Indent) << "(unresolved_member_expr type='";
    E->Ty->print(OS);
    OS << "\' name='" << E->Name << "')";
  }
  void VisitTupleExpr(TupleExpr *E) {
    OS.indent(Indent) << "(tuple_expr type='";
    E->Ty->print(OS);
    OS << '\'';
    for (unsigned i = 0, e = E->NumSubExprs; i != e; ++i) {
      OS << '\n';
      PrintRec(E->SubExprs[i]);
    }
    OS << ')';
  }
  void VisitUnresolvedDotExpr(UnresolvedDotExpr *E) {
    OS.indent(Indent) << "(unresolved_dot_expr type='";
    E->Ty->print(OS);
    OS << "\' field '" << E->Name.get() << "'\n";
    PrintRec(E->SubExpr);
    OS << ')';
  }
  void VisitTupleElementExpr(TupleElementExpr *E) {
    OS.indent(Indent) << "(tuple_element_expr type='";
    E->Ty->print(OS);
    OS << "\' field #" << E->FieldNo << "\n";
    PrintRec(E->SubExpr);
    OS << ')';
  }
  void VisitApplyExpr(ApplyExpr *E) {
    OS.indent(Indent) << "(apply_expr type='";
    E->Ty->print(OS);
    OS << "'\n";
    PrintRec(E->Fn);
    OS << '\n';
    PrintRec(E->Arg);
    OS << ')';
  }
  void VisitSequenceExpr(SequenceExpr *E) {
    OS.indent(Indent) << "(sequence_expr type='";
    E->Ty->print(OS);
    OS << '\'';
    for (unsigned i = 0, e = E->NumElements; i != e; ++i) {
      OS << '\n';
      PrintRec(E->Elements[i]);
    }
    OS << ')';
  }
  void VisitBraceExpr(BraceExpr *E) {
    OS.indent(Indent) << "(brace_expr type='";
    E->Ty->print(OS);
    OS << '\'';
    for (unsigned i = 0, e = E->NumElements; i != e; ++i) {
      OS << '\n';
      if (Expr *SubExpr = E->Elements[i].dyn_cast<Expr*>())
        PrintRec(SubExpr);
      else
        PrintRec(E->Elements[i].get<ValueDecl*>());
    }
    OS << ')';
  }
  void VisitClosureExpr(ClosureExpr *E) {
    OS.indent(Indent) << "(closure_expr type='";
    E->Ty->print(OS);
    OS << "'\n";
    
    if (E->ArgList) {
      for (unsigned i = 0, e = E->getNumArgs(); i != e; ++i)
        if (E->ArgList[i].isNonNull()) {
          PrintRec(E->ArgList[i].get());
          OS << '\n';
        }
    }
    
    PrintRec(E->Input);
    OS << ')';
  }
  void VisitBinaryExpr(BinaryExpr *E) {
    OS.indent(Indent) << "(binary_expr '";
    if (E->Fn)
      OS << E->Fn->Name;
    else
      OS << "=";
    OS << "' type='";
    E->Ty->print(OS);
    OS << "'\n";
    PrintRec(E->LHS);
    OS << '\n';
    PrintRec(E->RHS);
    OS << ')';
  }
};

} // end anonymous namespace.


void Expr::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

void Expr::print(llvm::raw_ostream &OS, unsigned Indent) const {
  PrintExpr(OS, Indent).Visit(const_cast<Expr*>(this));
}
