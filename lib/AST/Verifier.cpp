//===--- Verifier.cpp - AST Invariant Verification ------------------------===//
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
//  This file implements a verifier of AST invariants.
//
//===----------------------------------------------------------------------===//

#include "swift/Subsystems.h"
#include "swift/AST/AST.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/Parse/Lexer.h" // bad dependency!
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
using namespace swift;

namespace {
  enum ShouldHalt { Continue, Halt };

  class Verifier : public Walker {
    TranslationUnit *TU;
    ASTContext &Ctx;
    llvm::raw_ostream &Out;

  public:
    Verifier(TranslationUnit *TU) : TU(TU), Ctx(TU->Ctx), Out(llvm::errs()) {}

    bool walkToExprPre(Expr *E) {
      switch (E->getKind()) {
#define DISPATCH(ID) return dispatchVisitPre(static_cast<ID##Expr*>(E))
#define EXPR(ID, PARENT) \
      case ExprKind::ID: \
        DISPATCH(ID);
#define UNCHECKED_EXPR(ID, PARENT) \
      case ExprKind::ID: \
        assert(TU->ASTStage < TranslationUnit::TypeChecked && \
               #ID "in wrong phase");\
        DISPATCH(ID);
#define UNBOUND_EXPR(ID, PARENT) \
      case ExprKind::ID: \
        assert(TU->ASTStage < TranslationUnit::NameBound && \
               #ID "in wrong phase"); \
        DISPATCH(ID);
#include "swift/AST/ExprNodes.def"
#undef DISPATCH
      }
      llvm_unreachable("not all cases handled!");
    }

    Expr *walkToExprPost(Expr *E) {
      switch (E->getKind()) {
#define DISPATCH(ID) return dispatchVisitPost(static_cast<ID##Expr*>(E))
#define EXPR(ID, PARENT) \
      case ExprKind::ID: \
        DISPATCH(ID);
#define UNCHECKED_EXPR(ID, PARENT) \
      case ExprKind::ID: \
        assert(TU->ASTStage < TranslationUnit::TypeChecked && \
               #ID "in wrong phase");\
        DISPATCH(ID);
#define UNBOUND_EXPR(ID, PARENT) \
      case ExprKind::ID: \
        assert(TU->ASTStage < TranslationUnit::NameBound && \
               #ID "in wrong phase"); \
        DISPATCH(ID);
#include "swift/AST/ExprNodes.def"
#undef DISPATCH
      }
      llvm_unreachable("not all cases handled!");
    }

    bool walkToStmtPre(Stmt *S) {
      switch (S->getKind()) {
#define DISPATCH(ID) return dispatchVisitPre(static_cast<ID##Stmt*>(S))
#define STMT(ID, PARENT) \
      case StmtKind::ID: \
        DISPATCH(ID);
#include "swift/AST/StmtNodes.def"
#undef DISPATCH
      }
      llvm_unreachable("not all cases handled!");
    }

    Stmt *walkToStmtPost(Stmt *S) {
      switch (S->getKind()) {
#define DISPATCH(ID) return dispatchVisitPost(static_cast<ID##Stmt*>(S))
#define STMT(ID, PARENT) \
      case StmtKind::ID: \
        DISPATCH(ID);
#include "swift/AST/StmtNodes.def"
#undef DISPATCH
      }
      llvm_unreachable("not all cases handled!");
    }

  private:
    /// Helper template for dispatching pre-visitation.
    /// If we're visiting in pre-order, don't validate the node yet;
    /// just check whether we should stop further descent.
    template <class T> bool dispatchVisitPre(T node) {
      return shouldVerify(node);
    }

    /// Helper template for dispatching post-visitation.
    template <class T> T dispatchVisitPost(T node) {
      // We always verify source ranges.
      checkSourceRanges(node);

      // Always verify the node as a parsed node.
      verifyParsed(node);

      // If we've bound names already, verify as a bound node.
      if (TU->ASTStage >= TranslationUnit::NameBound)
        verifyBound(node);

      // If we've checked types already, do some extra verification.
      if (TU->ASTStage >= TranslationUnit::TypeChecked)
        verifyChecked(node);

      // Always continue.
      return node;
    }

    // Default cases for whether we should verify within the given subtree.
    bool shouldVerify(Expr *E) { return true; }
    bool shouldVerify(Stmt *S) { return true; }

    // Base cases for the various stages of verification.
    void verifyParsed(Expr *E) {}
    void verifyParsed(Stmt *S) {}
    void verifyBound(Expr *E) {}
    void verifyBound(Stmt *S) {}
    void verifyChecked(Expr *E) {}
    void verifyChecked(Stmt *S) {}

    // Specialized verifiers.

    void verifyChecked(AssignStmt *S) {
      Type lhsTy = checkLValue(S->getDest()->getType(), "LHS of assignment");
      checkSameType(lhsTy, S->getSrc()->getType(), "assignment operands");
    }

    void verifyChecked(TupleElementExpr *E) {
      Type resultType = E->getType();
      Type baseType = E->getBase()->getType();
      checkSameLValueness(baseType, resultType, E->isLValueProjection(),
                          "base and result of TupleElementExpr");

      TupleType *tupleType = baseType->getAs<TupleType>();
      if (!tupleType) {
        Out << "base of TupleElementExpr does not have tuple type: ";
        E->getBase()->getType().print(Out);
        Out << "\n";
        abort();
      }

      if (E->getFieldNumber() >= tupleType->getFields().size()) {
        Out << "field index " << E->getFieldNumber()
            << " for TupleElementExpr is out of range [0,"
            << tupleType->getFields().size() << ")\n";
        abort();
      }

      checkSameType(resultType, tupleType->getElementType(E->getFieldNumber()),
                    "TupleElementExpr and the corresponding tuple element");
    }

    /// LookThroughOneofExpr:
    ///   lvalue-ness of operand equals lvalue-ness of result
    ///   ignoring lvalue-ness, result is transparent type of operand
    void verifyChecked(LookThroughOneofExpr *E) {
      Type operandType = E->getSubExpr()->getType();
      Type resultType = E->getType();
      checkSameLValueness(operandType, resultType, E->isLValueProjection(),
                          "operand and result of LookThroughOneofExpr");

      OneOfType *oneof = operandType->getAs<OneOfType>();
      if (!oneof) {
        Out << "operand of LookThroughOneofExpr does not have oneof type: ";
        E->getSubExpr()->getType().print(Out);
        Out << "\n";
        abort();
      }

      if (!oneof->isTransparentType()) {
        Out << "looking through a oneof with multiple elements: ";
        E->getSubExpr()->getType().print(Out);
        Out << "\n";
        abort();
      }

      checkSameType(resultType, oneof->getTransparentType(),
                    "result of LookThroughOneofExpr and single element of oneof");
    }

    /// Look through a possible l-value type, returning true if it was
    /// an l-value.
    bool lookThroughLValue(Type &type) {
      if (LValueType *lv = type->getAs<LValueType>()) {
        Type objectType = lv->getObjectType();
        if (objectType->is<LValueType>()) {
          Out << "type is an lvalue of lvalue type: ";
          type.print(Out);
          Out << "\n";
        }
        type = objectType;
        return true;
      }
      return false;
    }

    /// The two types are required to either both be l-values or
    /// both not be l-values.  They are adjusted to not be l-values.
    /// Returns true if they are both l-values.
    bool checkSameLValueness(Type &T0, Type &T1, const char *what) {
      bool isLValue0 = lookThroughLValue(T0);
      bool isLValue1 = lookThroughLValue(T1);
      
      if (isLValue0 == isLValue1)
        return isLValue0;

      Out << "lvalue-ness of " << what << " do not match: "
          << isLValue0 << ", " << isLValue1 << ")\n";
      abort();
    }

    /// The two types are required to either both be l-values or
    /// both not be l-values, and one or the other is expected.
    /// They are adjusted to not be l-values.
    void checkSameLValueness(Type &T0, Type &T1, bool expected,
                             const char *what) {
      if (checkSameLValueness(T0, T1, what) == expected)
        return;

      Out << "lvalue-ness of " << what << " does not match expectation of "
          << expected << "\n";
      abort();
    }

    Type checkLValue(Type T, const char *what) {
      LValueType *LV = T->getAs<LValueType>();
      if (LV) return LV->getObjectType();

      Out << "type is not an l-value in " << what << ": ";
      T.print(Out);
      Out << "\n";
      abort();
    }

    // Verification utilities.
    void checkSameType(Type T0, Type T1, const char *what) {
      if (T0->getCanonicalType() == T1->getCanonicalType())
        return;

      Out << "different types for " << what << ": ";
      T0.print(Out);
      Out << " vs. ";
      T1.print(Out);
      Out << "\n";
      abort();
    }
    
    void checkSourceRanges(Expr *E) {
      if (!E->getSourceRange().isValid()) {
        Out << "invalid source range for expression: ";
        E->print(Out);
        Out << "\n";
        abort();
      }
      checkSourceRanges(E->getSourceRange(), Parent,
                        ^ { E->print(Out); } );
    }
    
    void checkSourceRanges(Stmt *S) {
      if (!S->getSourceRange().isValid()) {
        Out << "invalid source range for statement: ";
        S->print(Out);
        Out << "\n";
        abort();
      }
      checkSourceRanges(S->getSourceRange(), Parent,
                        ^ { S->print(Out); });
    }
    
    /// \brief Verify that the given source ranges is contained within the
    /// parent's source range.
    void checkSourceRanges(SourceRange Current,
                           llvm::PointerUnion<Expr *, Stmt *> Parent,
                           void (^printEntity)()) {
      SourceRange Enclosing;
      if (Stmt *S = Parent.dyn_cast<Stmt *>())
        Enclosing = S->getSourceRange();
      else if (Expr *E = Parent.dyn_cast<Expr *>())
        Enclosing = E->getSourceRange();
      else // no parent
        return;
      
      // FIXME: This is a very ugly way to check inclusion.
      if (Enclosing.Start.Value.getPointer() > Current.Start.Value.getPointer()
          || Enclosing.End.Value.getPointer() < Current.End.Value.getPointer()) 
      {
        Out << "child source range not contained within its parent: ";
        printEntity();
        Out << "\n  parent range: ";
        printRange(Enclosing);
        Out << "\n  child range: ";
        printRange(Current);
        Out << "\n";
        abort();
      }
    }

    void printRange(SourceRange R) {
      SourceLoc Begin = R.Start, End = R.End;

      // If either of those locations is invalid, fall back on printing pointers.
      if (!Begin.isValid() || !End.isValid()) {
        Out << "[" << (void*) Begin.Value.getPointer()
            << "," << (void*) End.Value.getPointer()
            << "]";
        return;
      }

      // Otherwise, advance the end token.
      End = Lexer::getLocForEndOfToken(Ctx.SourceMgr, R.End);

      int BufferIndex = Ctx.SourceMgr.FindBufferContainingLoc(Begin.Value);
      if (BufferIndex != -1) {
        const llvm::MemoryBuffer *Buffer =
          Ctx.SourceMgr.getMemoryBuffer((unsigned) BufferIndex);
        const char *BufferStart = Buffer->getBufferStart();
        Out << Buffer->getBufferIdentifier() 
            << ':' << (Begin.Value.getPointer() - BufferStart)
            << '-' << (End.Value.getPointer() - BufferStart)
            << ' ';
      }

      llvm::StringRef Text(Begin.Value.getPointer(),
                           End.Value.getPointer() - Begin.Value.getPointer());
      Out << '"' << Text << '"';
    }
    
  };
}

void swift::verify(TranslationUnit *TUnit) {
  // FIXME: For now, punt if there are errors in the translation unit.
  if (TUnit->Ctx.hadError()) return;

  Verifier verifier(TUnit);
  TUnit->Body->walk(verifier);
}
