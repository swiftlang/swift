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
#include "swift/AST/ASTWalker.h"
#include "swift/Parse/Lexer.h" // bad dependency!
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
using namespace swift;

namespace {
  enum ShouldHalt { Continue, Halt };

  class Verifier : public ASTWalker {
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

    void verifyChecked(IfStmt *S) {
      checkSameType(S->getCond()->getType(), BuiltinIntegerType::get(1, Ctx),
                    "if condition type");
    }

    void verifyChecked(WhileStmt *S) {
      checkSameType(S->getCond()->getType(), BuiltinIntegerType::get(1, Ctx),
                    "while condition type");
    }

    void verifyChecked(AssignStmt *S) {
      Type lhsTy = checkLValue(S->getDest()->getType(), "LHS of assignment");
      checkSameType(lhsTy, S->getSrc()->getType(), "assignment operands");
    }

    void verifyChecked(AddressOfExpr *E) {
      LValueType::Qual resultQuals;
      Type resultObj = checkLValue(E->getType(), resultQuals,
                                   "result of AddressOfExpr");

      LValueType::Qual srcQuals;
      Type srcObj = checkLValue(E->getSubExpr()->getType(), srcQuals,
                                "source of AddressOfExpr");

      checkSameType(resultObj, srcObj, "object types for AddressOfExpr");

      if (LValueType::Qual() != srcQuals) {
        Out << "mismatched qualifiers";
        E->print(Out);
        Out << "\n";
        abort();
      }
    }

    void verifyChecked(RequalifyExpr *E) {
      LValueType::Qual dstQuals, srcQuals;
      Type dstObj = checkLValue(E->getType(), dstQuals,
                                "result of RequalifyExpr");
      Type srcObj = checkLValue(E->getSubExpr()->getType(), srcQuals,
                                "input to RequalifyExpr");
      checkSameType(dstObj, srcObj,
                    "objects of result and operand of RequalifyExpr");
      
      if (!(srcQuals < dstQuals)) {
        Out << "bad qualifier sets for RequalifyExpr";
        E->print(Out);
        Out << "\n";
        abort();
      }
    }

    void verifyChecked(MaterializeExpr *E) {
      Type obj = checkLValue(E->getType(), "result of MaterializeExpr");
      checkSameType(obj, E->getSubExpr()->getType(),
                    "result and operand of MaterializeExpr");
    }

    void verifyChecked(SuperConversionExpr *E) {
      if (E->getType()->is<LValueType>()) {
        Out << "supertype conversion cannot be an lvalue";
        E->print(Out);
        Out << "\n";
        abort();
      }

      checkSameOrSubType(E->getSubExpr()->getType(), E->getType(),
                         "supertype and subtype");
    }
    
    void verifyChecked(TupleElementExpr *E) {
      Type resultType = E->getType();
      Type baseType = E->getBase()->getType();
      checkSameLValueness(baseType, resultType,
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

    void verifyChecked(ApplyExpr *E) {
      FunctionType *FT = E->getFn()->getType()->getAs<FunctionType>();
      if (!FT) {
        Out << "callee of apply expression does not have function type:";
        E->getFn()->getType()->print(Out);
        Out << "\n";
        abort();
      }
      CanType InputExprTy = E->getArg()->getType()->getCanonicalType();
      CanType ResultExprTy = E->getType()->getCanonicalType();
      if (ResultExprTy != FT->getResult()->getCanonicalType()) {
        Out << "Type of callee does not match type of ApplyExpr:";
        E->getType()->print(Out);
        Out << " vs. ";
        FT->getResult()->print(Out);
        Out << "\n";
        abort();
      }
      if (InputExprTy != FT->getInput()->getCanonicalType()) {
        TupleType *TT = FT->getInput()->getAs<TupleType>();
        if (isa<DotSyntaxCallExpr>(E)) {
          LValueType::Qual InputExprQuals;
          Type InputExprObjectTy = checkLValue(InputExprTy, InputExprQuals,
                                               "object argument");
          LValueType::Qual FunctionInputQuals;
          Type FunctionInputObjectTy = checkLValue(FT->getInput(),
                                                   FunctionInputQuals,
                                                   "'this' parameter");
          
          checkSameOrSubType(InputExprObjectTy, FunctionInputObjectTy,
                             "object argument and 'this' parameter");
        } else if (!TT || TT->getFields().size() != 1 ||
                   TT->getFields()[0].getType()->getCanonicalType()
                     != InputExprTy) {
          Out << "Type of callee does not match type of arg for ApplyExpr:";
          E->getArg()->getType()->print(Out);
          Out << " vs. ";
          FT->getInput()->print(Out);
          Out << "\n";
          abort();
        }
      }
    }

    void verifyChecked(MemberRefExpr *E) {
      if (!E->getBase()->getType()->is<LValueType>()) {
        Out << "Member reference base type is not an lvalue:\n";
        E->dump();
        abort();
      }
      
      if (!E->getType()->is<LValueType>()) {
        Out << "Member reference type is not an lvalue\n";
        E->dump();
        abort();
      }
      
      if (!E->getDecl()) {
        Out << "Member reference is missing declaration\n";
        E->dump();
        abort();
      }
      
      LValueType *ResultLV = E->getType()->getAs<LValueType>();
      if (!ResultLV) {
        Out << "Member reference has non-lvalue type\n";
        E->dump();
        abort();
      }
      
      checkSameType(E->getDecl()->getType(), ResultLV->getObjectType(),
                    "member reference result type and referenced member");
      
      Type ContainerTy
        = E->getDecl()->getDeclContext()->getDeclaredTypeOfContext();
      if (!ContainerTy) {
        Out << "container of member reference is not a type\n";
        E->dump();
        abort();
      }
      
      Type BaseTy = E->getBase()->getType();
      if (LValueType *BaseLV = BaseTy->getAs<LValueType>())
        BaseTy = BaseLV->getObjectType();
      
      checkSameOrSubType(BaseTy, ContainerTy,
                         "member reference base and member container");
    }
    
    void verifyChecked(SubscriptExpr *E) {
      if (!E->getBase()->getType()->is<LValueType>()) {
        Out << "Subscript base type is not an lvalue";
        abort();
      }
      
      if (!E->getType()->is<LValueType>()) {
        Out << "Subscript type is not an lvalue";
        abort();
      }
      
      if (!E->getDecl()) {
        Out << "Subscript expression is missing subscript declaration";
        abort();
      }
      
      checkSameType(E->getDecl()->getIndices()->getType(),
                    E->getIndex()->getType(), "subscript indices");
      
      LValueType *ResultLV = E->getType()->getAs<LValueType>();
      if (!ResultLV) {
        Out << "Subscript expression has non-lvalue type";
        abort();
      }
      
      checkSameType(E->getDecl()->getElementType(), ResultLV->getObjectType(),
                    "subscript result type and subscript declaration");
      
      Type ContainerTy
        = E->getDecl()->getDeclContext()->getDeclaredTypeOfContext();
      if (!ContainerTy) {
        Out << "container of subscript is not a type\n";
        E->dump();
        abort();
      }
      
      Type BaseTy = E->getBase()->getType();
      if (LValueType *BaseLV = BaseTy->getAs<LValueType>())
        BaseTy = BaseLV->getObjectType();
      
      checkSameOrSubType(BaseTy, ContainerTy,
                         "subscript base and subscript container");
    }
      
    void verifyParsed(NewArrayExpr *E) {
      if (E->getBounds().empty()) {
        Out << "NewArrayExpr has an empty bounds list\n";
        abort();
      }
      if (E->getBounds()[0].Value == nullptr) {
        Out << "First bound of NewArrayExpr is missing\n";
        abort();
      }
    }

    /// Look through a possible l-value type, returning true if it was
    /// an l-value.
    bool lookThroughLValue(Type &type, LValueType::Qual &qs) {
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
    bool lookThroughLValue(Type &type) {
      LValueType::Qual qs;
      return lookThroughLValue(type, qs);
    }

    /// The two types are required to either both be l-values or
    /// both not be l-values.  They are adjusted to not be l-values.
    /// Returns true if they are both l-values.
    bool checkSameLValueness(Type &T0, Type &T1,
                             const char *what) {
      LValueType::Qual Q0, Q1;
      bool isLValue0 = lookThroughLValue(T0, Q0);
      bool isLValue1 = lookThroughLValue(T1, Q1);
      
      if (isLValue0 != isLValue1) {
        Out << "lvalue-ness of " << what << " do not match: "
            << isLValue0 << ", " << isLValue1 << "\n";
        abort();
      }

      if (isLValue0 && Q0 != Q1) {
        Out << "qualification of " << what << " do not match: ";
        printQualifiers(Q0);
        Out << ", ";
        printQualifiers(Q1);
        Out << "\n";
        abort();
      }

      return isLValue0;
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

    Type checkLValue(Type T, LValueType::Qual &Q, const char *what) {
      LValueType *LV = T->getAs<LValueType>();
      if (LV) {
        Q = LV->getQualifiers();
        return LV->getObjectType();
      }

      Out << "type is not an l-value in " << what << ": ";
      T.print(Out);
      Out << "\n";
      abort();
    }
    Type checkLValue(Type T, const char *what) {
      LValueType::Qual qs;
      return checkLValue(T, qs, what);
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

    void checkSameOrSubType(Type T0, Type T1, const char *what) {
      if (T0->getCanonicalType() == T1->getCanonicalType())
        return;

      // Protocol subtyping.
      if (auto Proto0 = T0->getAs<ProtocolType>())
        if (auto Proto1 = T1->getAs<ProtocolType>())
          if (Proto0->getDecl()->inheritsFrom(Proto1->getDecl()))
            return;
      
      Out << "incompatible types for " << what << ": ";
      T0.print(Out);
      Out << " vs. ";
      T1.print(Out);
      Out << "\n";
      abort();
    }

    void checkSourceRanges(Expr *E) {
      if (!E->getSourceRange().isValid()) {
        // We don't care about source ranges on implicitly-generated
        // expressions.
        if (E->isImplicit())
          return;
        
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
      if (Parent.isNull())
          return;
          
      if (Stmt *S = Parent.dyn_cast<Stmt*>())
        Enclosing = S->getSourceRange();
      else {
        // FIXME: This hack is required because the inclusion check below
        // doesn't compares the *start* of the ranges, not the end of the
        // ranges.  In the case of an interpolated string literal expr, the
        // subexpressions are contained within the string token.  This means
        // that comparing the start of the string token to the end of an
        // embedded expression will fail.
        if (isa<InterpolatedStringLiteralExpr>(Parent.get<Expr*>()))
          return;
        
        Enclosing = Parent.get<Expr*>()->getSourceRange();
      }
      
      // FIXME: This is a very ugly way to check inclusion.
      if (Enclosing.Start.Value.getPointer() > Current.Start.Value.getPointer()
          || Enclosing.End.Value.getPointer() < Current.End.Value.getPointer()){
        Out << "child source range not contained within its parent: ";
        printEntity();
        Out << "\n  parent range: ";
        Enclosing.print(Out, Ctx.SourceMgr);
        Out << "\n  child range: ";
        Current.print(Out, Ctx.SourceMgr);
        Out << "\n";
        abort();
      }
    }

    void printQualifiers(LValueType::Qual qs) {
      if (qs & LValueType::Qual::NonHeap) Out << "|nonheap";
    }
  };
}

void swift::verify(TranslationUnit *TUnit) {
  // FIXME: For now, punt if there are errors in the translation unit.
  if (TUnit->Ctx.hadError()) return;

  Verifier verifier(TUnit);
  for (Decl *D : TUnit->Decls)
    D->walk(verifier);
}
