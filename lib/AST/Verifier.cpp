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
#include "swift/AST/ArchetypeBuilder.h"
#include "swift/AST/AST.h"
#include "swift/AST/ASTWalker.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/Support/raw_ostream.h"
#include <functional>
using namespace swift;

namespace {
  enum ShouldHalt { Continue, Halt };

  class Verifier : public ASTWalker {
    Module *M;
    ASTContext &Ctx;
    llvm::raw_ostream &Out;
    const bool HadError;

    using FunctionLike = llvm::PointerUnion4<
        ConstructorDecl *, DestructorDecl *, FuncDecl *, ClosureExpr *>;

    /// \brief The stack of functions we're visiting.
    SmallVector<FunctionLike, 4> Functions;

    /// \brief The set of opaque value expressions active at this point.
    llvm::DenseMap<OpaqueValueExpr *, unsigned> OpaqueValues;

  public:
    Verifier(Module *M)
        : M(M), Ctx(M->Ctx), Out(llvm::errs()), HadError(M->Ctx.hadError()) {}

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      switch (E->getKind()) {
#define DISPATCH(ID) return dispatchVisitPreExpr(static_cast<ID##Expr*>(E))
#define EXPR(ID, PARENT) \
      case ExprKind::ID: \
        DISPATCH(ID);
#define UNCHECKED_EXPR(ID, PARENT) \
      case ExprKind::ID: \
        assert((HadError || !isa<TranslationUnit>(M) || \
                cast<TranslationUnit>(M)->MainSourceFile->ASTStage < \
                  SourceFile::TypeChecked) && \
               #ID "in wrong phase");\
        DISPATCH(ID);
#include "swift/AST/ExprNodes.def"
#undef DISPATCH
      }
      llvm_unreachable("not all cases handled!");
    }

    Expr *walkToExprPost(Expr *E) override {
      switch (E->getKind()) {
#define DISPATCH(ID) return dispatchVisitPost(static_cast<ID##Expr*>(E))
#define EXPR(ID, PARENT) \
      case ExprKind::ID: \
        DISPATCH(ID);
#define UNCHECKED_EXPR(ID, PARENT) \
      case ExprKind::ID: \
        assert((HadError || !isa<TranslationUnit>(M) || \
                cast<TranslationUnit>(M)->MainSourceFile->ASTStage < \
                  SourceFile::TypeChecked) && \
               #ID "in wrong phase");\
        DISPATCH(ID);
#include "swift/AST/ExprNodes.def"
#undef DISPATCH
      }
      llvm_unreachable("not all cases handled!");
    }

    std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
      switch (S->getKind()) {
#define DISPATCH(ID) return dispatchVisitPreStmt(static_cast<ID##Stmt*>(S))
#define STMT(ID, PARENT) \
      case StmtKind::ID: \
        DISPATCH(ID);
#include "swift/AST/StmtNodes.def"
#undef DISPATCH
      }
      llvm_unreachable("not all cases handled!");
    }

    Stmt *walkToStmtPost(Stmt *S) override {
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

    std::pair<bool, Pattern*> walkToPatternPre(Pattern *P) override {
      switch (P->getKind()) {
#define DISPATCH(ID) \
        return dispatchVisitPrePattern(static_cast<ID##Pattern*>(P))
#define PATTERN(ID, PARENT) \
      case PatternKind::ID: \
        DISPATCH(ID);
#include "swift/AST/PatternNodes.def"
#undef DISPATCH
      }
      llvm_unreachable("not all cases handled!");
    }

    Pattern *walkToPatternPost(Pattern *P) {
      switch (P->getKind()) {
#define DISPATCH(ID) \
        return dispatchVisitPost(static_cast<ID##Pattern*>(P))
#define PATTERN(ID, PARENT) \
      case PatternKind::ID: \
        DISPATCH(ID);
#include "swift/AST/PatternNodes.def"
#undef DISPATCH
      }
      llvm_unreachable("not all cases handled!");
    }

    bool walkToDeclPre(Decl *D) override {
      switch (D->getKind()) {
#define DISPATCH(ID) return dispatchVisitPre(static_cast<ID##Decl*>(D))
#define DECL(ID, PARENT) \
      case DeclKind::ID: \
        DISPATCH(ID);
#include "swift/AST/DeclNodes.def"
#undef DISPATCH
      }
      llvm_unreachable("not all cases handled!");
    }

    bool walkToDeclPost(Decl *D) override {
      switch (D->getKind()) {
#define DISPATCH(ID) return dispatchVisitPost(static_cast<ID##Decl*>(D))
#define DECL(ID, PARENT) \
      case DeclKind::ID: \
        DISPATCH(ID);
#include "swift/AST/DeclNodes.def"
#undef DISPATCH
      }

      llvm_unreachable("Unhandled declaratiom kind");
    }

  private:
    /// Helper template for dispatching pre-visitation.
    /// If we're visiting in pre-order, don't validate the node yet;
    /// just check whether we should stop further descent.
    template <class T> bool dispatchVisitPre(T node) {
      return shouldVerify(node);
    }

    /// Helper template for dispatching pre-visitation.
    /// If we're visiting in pre-order, don't validate the node yet;
    /// just check whether we should stop further descent.
    template <class T> std::pair<bool, Expr *> dispatchVisitPreExpr(T node) {
      return { shouldVerify(node), node };
    }

    /// Helper template for dispatching pre-visitation.
    /// If we're visiting in pre-order, don't validate the node yet;
    /// just check whether we should stop further descent.
    template <class T> std::pair<bool, Stmt *> dispatchVisitPreStmt(T node) {
      return { shouldVerify(node), node };
    }

    /// Helper template for dispatching pre-visitation.
    /// If we're visiting in pre-order, don't validate the node yet;
    /// just check whether we should stop further descent.
    template <class T>
    std::pair<bool, Pattern *> dispatchVisitPrePattern(T node) {
      return { shouldVerify(node), node };
    }

    /// Helper template for dispatching post-visitation.
    template <class T> T dispatchVisitPost(T node) {
      // Verify source ranges if the AST node was parsed from source.
      if (isa<TranslationUnit>(M))
        checkSourceRanges(node);

      // Check that nodes marked invalid have the correct type.
      checkErrors(node);

      // Always verify the node as a parsed node.
      verifyParsed(node);

      SourceFile *SF = nullptr;
      if (auto TU = dyn_cast<TranslationUnit>(M))
        SF = TU->MainSourceFile;

      // If we've bound names already, verify as a bound node.
      if (!SF || SF->ASTStage >= SourceFile::NameBound)
        verifyBound(node);

      // If we've checked types already, do some extra verification.
      if (!SF || SF->ASTStage >= SourceFile::TypeChecked) {
        verifyCheckedAlways(node);
        if (!HadError)
          verifyChecked(node);
      }

      // Clean up anything that we've placed into a stack to check.
      cleanup(node);

      // Always continue.
      return node;
    }

    // Default cases for whether we should verify within the given subtree.
    bool shouldVerify(Expr *E) { return true; }
    bool shouldVerify(Stmt *S) { return true; }
    bool shouldVerify(Pattern *S) { return true; }
    bool shouldVerify(Decl *S) { return true; }

    // Default cases for cleaning up as we exit a node.
    void cleanup(Expr *E) { }
    void cleanup(Stmt *S) { }
    void cleanup(Pattern *P) { }
    void cleanup(Decl *D) { }
    
    // Base cases for the various stages of verification.
    void verifyParsed(Expr *E) {}
    void verifyParsed(Stmt *S) {}
    void verifyParsed(Pattern *P) {}
    void verifyParsed(Decl *D) {
      if (!D->getDeclContext()) {
        Out << "every Decl should have a DeclContext";
        abort();
      }
    }
    void verifyBound(Expr *E) {}
    void verifyBound(Stmt *S) {}
    void verifyBound(Pattern *P) {}
    void verifyBound(Decl *D) {}

    /// @{
    /// These verification functions are always run on type checked ASTs
    /// (even if there were errors).
    void verifyCheckedAlways(Expr *E) {}
    void verifyCheckedAlways(Stmt *S) {}
    void verifyCheckedAlways(Pattern *P) {}
    void verifyCheckedAlways(Decl *D) {}
    /// @}

    /// @{
    /// These verification functions are run on type checked ASTs if there were
    /// no errors.
    void verifyChecked(Expr *E) {}
    void verifyChecked(Stmt *S) {}
    void verifyChecked(Pattern *P) {}
    void verifyChecked(Decl *D) {}
    /// @}

    // Specialized verifiers.

    bool shouldVerify(ClosureExpr *closure) {
      Functions.push_back(closure);
      return shouldVerify(cast<Expr>(closure));
    }

    bool shouldVerify(ConstructorDecl *CD) {
      Functions.push_back(CD);
      return shouldVerify(cast<Decl>(CD));
    }

    bool shouldVerify(DestructorDecl *DD) {
      Functions.push_back(DD);
      return shouldVerify(cast<Decl>(DD));
    }

    bool shouldVerify(FuncDecl *FD) {
      Functions.push_back(FD);
      return shouldVerify(cast<Decl>(FD));
    }

    void cleanup(ClosureExpr *closure) {
      assert(Functions.back().get<ClosureExpr*>() == closure);
      Functions.pop_back();
    }

    void cleanup(ConstructorDecl *CD) {
      assert(Functions.back().get<ConstructorDecl *>() == CD);
      Functions.pop_back();
    }

    void cleanup(DestructorDecl *DD) {
      assert(Functions.back().get<DestructorDecl *>() == DD);
      Functions.pop_back();
    }

    void cleanup(FuncDecl *FD) {
      assert(Functions.back().get<FuncDecl *>() == FD);
      Functions.pop_back();
    }

    void verifyCheckedAlways(ValueDecl *D) {
      if (D->hasType() && D->getType()->hasTypeVariable()) {
        Out << "a type variable escaped the type checker";
        D->dump(Out);
        abort();
      }
      if (auto Overridden = D->getOverriddenDecl()) {
        if (D->getDeclContext() == Overridden->getDeclContext()) {
          Out << "can not override a decl in the same DeclContext";
          D->dump(Out);
          Overridden->dump(Out);
          abort();
        }
      }
      if (D->conformsToProtocolRequirement()) {
        if (D->getConformances().empty()) {
          Out << "conforms bit set but no conformances found\n";
          D->dump(Out);
          abort();
        }
      }
    }

    void verifyChecked(ReturnStmt *S) {
      auto func = Functions.back();
      Type resultType;
      if (FuncDecl *FD = func.dyn_cast<FuncDecl *>()) {
        resultType = FD->getResultType(Ctx);
      } else if (auto closure = func.dyn_cast<ClosureExpr *>()) {
        resultType = closure->getResultType();
      } else {
        resultType = TupleType::getEmpty(Ctx);
      }
      
      if (S->hasResult()) {
        auto result = S->getResult();
        auto returnType = result->getType();
        // Make sure that the return has the same type as the function.
        checkSameType(resultType, returnType, "return type");
      } else {
        // Make sure that the function has a Void result type.
        checkSameType(resultType, TupleType::getEmpty(Ctx), "return type");
      }
    }

    void verifyChecked(IfStmt *S) {
      checkSameType(S->getCond()->getType(), BuiltinIntegerType::get(1, Ctx),
                    "if condition type");
    }

    void verifyChecked(WhileStmt *S) {
      checkSameType(S->getCond()->getType(), BuiltinIntegerType::get(1, Ctx),
                    "while condition type");
    }

    Type checkAssignDest(Expr *Dest) {
      if (TupleExpr *TE = dyn_cast<TupleExpr>(Dest)) {
        SmallVector<TupleTypeElt, 4> lhsTupleTypes;
        for (unsigned i = 0; i != TE->getNumElements(); ++i) {
          Type SubType = checkAssignDest(TE->getElement(i));
          lhsTupleTypes.push_back(TupleTypeElt(SubType, TE->getElementName(i)));
        }
        return TupleType::get(lhsTupleTypes, Ctx);
      }
      return checkLValue(Dest->getType(), "LHS of assignment");
    }

    void verifyChecked(AssignExpr *S) {
      Type lhsTy = checkAssignDest(S->getDest());
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

      if ((resultQuals | LValueType::Qual::Implicit) !=
          (srcQuals | LValueType::Qual::Implicit)) {
        Out << "mismatched qualifiers";
        E->print(Out);
        Out << "\n";
        abort();
      }
    }

    void verifyParsed(AbstractClosureExpr *E) {
      Type Ty = E->getType();
      if (!Ty)
        return;
      if (Ty->is<ErrorType>())
        return;
      if (!Ty->is<FunctionType>()) {
        Out << "a closure should have a function type";
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

      // As a hack, requalifications in the object operand are
      // permitted to remove the 'non-settable' qualifier (so that you
      // can call methods on immutable values) and 'implicit'
      // qualifier (so that you don't have to explicitly qualify take
      // the address of the object).
      if (E->isForObjectOperand()) {
        dstQuals |= LValueType::Qual::NonSettable;
        dstQuals |= LValueType::Qual::Implicit;
      }
      
      // FIXME: Should either properly check implicit here, or model the dropping
      // of 'implicit' differently.
      if (!(srcQuals < dstQuals) && !(srcQuals == dstQuals)) {
        Out << "bad qualifier sets for RequalifyExpr:\n";
        E->print(Out);
        Out << "\n";
        abort();
      }
    }

    void verifyChecked(MetatypeConversionExpr *E) {
      auto destTy = checkMetatypeType(E->getType(),
                                      "result of MetatypeConversionExpr");
      auto srcTy = checkMetatypeType(E->getSubExpr()->getType(),
                                     "source of MetatypeConversionExpr");

      if (destTy->isEqual(srcTy)) {
        Out << "trivial MetatypeConversionExpr:\n";
        E->print(Out);
        Out << "\n";
        abort();
      }

      checkTrivialSubtype(srcTy, destTy, "MetatypeConversionExpr");
    }

    void verifyChecked(DerivedToBaseExpr *E) {
      auto destTy = E->getType();
      auto srcTy = E->getSubExpr()->getType();
      if (destTy->isEqual(srcTy)) {
        Out << "trivial DerivedToBaseExpr:\n";
        E->print(Out);
        Out << "\n";
        abort();
      }

      if (!destTy->getClassOrBoundGenericClass() ||
          !srcTy->getClassOrBoundGenericClass()) {
        Out << "DerivedToBaseExpr does not involve class types:\n";
        E->print(Out);
        Out << "\n";
        abort();
      }

      checkTrivialSubtype(srcTy, destTy, "DerivedToBaseExpr");
    }

    void verifyChecked(MaterializeExpr *E) {
      Type obj = checkLValue(E->getType(), "result of MaterializeExpr");
      checkSameType(obj, E->getSubExpr()->getType(),
                    "result and operand of MaterializeExpr");
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
        E->getFn()->getType().print(Out);
        Out << "\n";
        abort();
      }
      CanType InputExprTy = E->getArg()->getType()->getCanonicalType();
      CanType ResultExprTy = E->getType()->getCanonicalType();
      if (ResultExprTy != FT->getResult()->getCanonicalType()) {
        Out << "result of ApplyExpr does not match result type of callee:";
        E->getType().print(Out);
        Out << " vs. ";
        FT->getResult()->print(Out);
        Out << "\n";
        abort();
      }
      if (InputExprTy != FT->getInput()->getCanonicalType()) {
        TupleType *TT = FT->getInput()->getAs<TupleType>();
        if (isa<SelfApplyExpr>(E)) {
          LValueType::Qual InputExprQuals;
          Type InputExprObjectTy;
          if (InputExprTy->hasReferenceSemantics() ||
              InputExprTy->is<MetaTypeType>())
            InputExprObjectTy = InputExprTy;
          else
            InputExprObjectTy = checkLValue(InputExprTy, InputExprQuals,
                                            "object argument");
          LValueType::Qual FunctionInputQuals;
          Type FunctionInputObjectTy = checkLValue(FT->getInput(),
                                                   FunctionInputQuals,
                                                   "'self' parameter");
          
          checkSameOrSubType(InputExprObjectTy, FunctionInputObjectTy,
                             "object argument and 'self' parameter");
        } else if (!TT || TT->getFields().size() != 1 ||
                   TT->getFields()[0].getType()->getCanonicalType()
                     != InputExprTy) {
          Out << "Argument type does not match parameter type in ApplyExpr:"
                 "\nArgument type: ";
          E->getArg()->getType().print(Out);
          Out << "\nParameter type: ";
          FT->getInput()->print(Out);
          Out << "\n";
          E->dump(Out);
          abort();
        }
      }
    }

    void verifyChecked(MemberRefExpr *E) {
      if (!E->getBase()->getType()->is<LValueType>() &&
          !E->getBase()->getType()->hasReferenceSemantics()) {
        Out << "Member reference base type is not an lvalue:\n";
        E->dump(Out);
        abort();
      }
      
      if (!E->getType()->is<LValueType>()) {
        Out << "Member reference type is not an lvalue\n";
        E->dump(Out);
        abort();
      }
      
      if (!E->getMember()) {
        Out << "Member reference is missing declaration\n";
        E->dump(Out);
        abort();
      }

      LValueType *ResultLV = E->getType()->getAs<LValueType>();
      if (!ResultLV) {
        Out << "Member reference has non-lvalue type\n";
        E->dump(Out);
        abort();
      }

      // FIXME: Check container/member types through substitutions.
    }

    void verifyChecked(DynamicMemberRefExpr *E) {
      // The base type must be DynamicLookup.
      auto baseTy = E->getBase()->getType();

      // The base might be a metatype of DynamicLookup.
      if (auto baseMetaTy = baseTy->getAs<MetaTypeType>()) {
        baseTy = baseMetaTy->getInstanceType();
      }

      auto baseProtoTy = baseTy->getAs<ProtocolType>();
      if (!baseProtoTy ||
          !baseProtoTy->getDecl()->isSpecificProtocol(
             KnownProtocolKind::DynamicLookup)) {
        Out << "Dynamic member reference has non-DynamicLookup base\n";
        E->dump(Out);
        abort();
      }

      // The member must be [objc].
      if (!E->getMember().getDecl()->isObjC()) {
        Out << "Dynamic member reference to non-[objc] member\n";
        E->dump(Out);
        abort();
      }
    }

    void verifyChecked(SubscriptExpr *E) {
      if (!E->getBase()->getType()->is<LValueType>() &&
          !E->getBase()->getType()->hasReferenceSemantics()) {
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

      // FIXME: Check base/member types through substitutions.
    }

    void verifyChecked(CheckedCastExpr *E) {
      if (!E->isResolved()) {
        Out << "CheckedCast kind not resolved\n";
        abort();
      }
    }

    void verifyChecked(SpecializeExpr *E) {
      if (!E->getType()->is<FunctionType>()) {
        Out << "SpecializeExpr must have FunctionType result\n";
        abort();
      }

      Type SubType = E->getSubExpr()->getType()->getRValueType();
      if (!SubType->is<PolymorphicFunctionType>()) {
        Out << "Non-polymorphic expression specialized\n";
        abort();
      }

      // Verify that the protocol conformances line up with the archetypes.
      // FIXME: It's not clear how many levels we're substituting here.
      for (auto &Subst : E->getSubstitutions()) {
        auto Archetype = Subst.Archetype;
        if (Subst.Conformance.size() != Archetype->getConformsTo().size()) {
          Out << "Wrong number of protocol conformances for archetype\n";
          abort();
        }

        for (unsigned I = 0, N = Subst.Conformance.size(); I != N; ++I) {
          const auto &Conformance = Subst.Conformance[I];
          if (!Conformance || Conformance->getWitnesses().empty())
            continue;

          if (Conformance->getWitnesses().begin()->first->getDeclContext()
                != Archetype->getConformsTo()[I]) {
            Out << "Protocol conformance doesn't match up with archetype "
                   "requirement\n";
            abort();
          }
        }
      }
    }

    void verifyChecked(TupleShuffleExpr *E) {
      TupleType *TT = E->getType()->getAs<TupleType>();
      TupleType *SubTT = E->getSubExpr()->getType()->getAs<TupleType>();
      if (!TT || !SubTT) {
        Out << "Unexpected types in TupleShuffleExpr\n";
        abort();
      }
      unsigned varargsStartIndex = 0;
      Type varargsType;
      unsigned callerDefaultArgIndex = 0;
      for (unsigned i = 0, e = E->getElementMapping().size(); i != e; ++i) {
        int subElem = E->getElementMapping()[i];
        if (subElem == TupleShuffleExpr::DefaultInitialize)
          continue;
        if (subElem == TupleShuffleExpr::FirstVariadic) {
          varargsStartIndex = i + 1;
          varargsType = TT->getFields()[i].getVarargBaseTy();
          break;
        }
        if (subElem == TupleShuffleExpr::CallerDefaultInitialize) {
          auto init = E->getCallerDefaultArgs()[callerDefaultArgIndex++];
          if (!TT->getElementType(i)->isEqual(init->getType())) {
            Out << "Type mismatch in TupleShuffleExpr\n";
            abort();
          }
          continue;
        }
        if (!TT->getElementType(i)->isEqual(SubTT->getElementType(subElem))) {
          Out << "Type mismatch in TupleShuffleExpr\n";
          abort();
        }
      }
      if (varargsStartIndex) {
        unsigned i = varargsStartIndex, e = E->getElementMapping().size();
        for (; i != e; ++i) {
          unsigned subElem = E->getElementMapping()[i];
          if (!SubTT->getElementType(subElem)->isEqual(varargsType)) {
            Out << "Vararg type mismatch in TupleShuffleExpr\n";
            abort();
          }
        }
      }
    }

    void verifyChecked(MetatypeExpr *E) {
      auto metatype = E->getType()->getAs<MetaTypeType>();
      if (!metatype) {
        Out << "MetatypeExpr must have metatype type\n";
        abort();
      }

      if (E->getBase()) {
        checkSameType(E->getBase()->getType(), metatype->getInstanceType(),
                      "base type of .metatype expression");
      }
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

    void verifyChecked(NewArrayExpr *E) {
      if (!E->hasElementType()) {
        Out << "NewArrayExpr is missing its element type";
        abort();
      }

      if (!E->hasInjectionFunction()) {
        Out << "NewArrayExpr is missing an injection function";
        abort();
      }
    }

    void verifyChecked(InjectIntoOptionalExpr *expr) {
      auto valueType = expr->getType()->getOptionalObjectType(Ctx);
      if (!valueType) {
        Out << "InjectIntoOptionalExpr is not of Optional type";
        abort();
      }

      if (!expr->getSubExpr()->getType()->isEqual(valueType)) {
        Out << "InjectIntoOptionalExpr operand is not of the value type";
        abort();
      }
    }

    void verifyChecked(IfExpr *expr) {
      auto condTy
        = expr->getCondExpr()->getType()->getAs<BuiltinIntegerType>();
      if (!condTy || condTy->getBitWidth() != 1) {
        Out << "IfExpr condition is not an i1\n";
        abort();
      }

      checkSameType(expr->getThenExpr()->getType(),
                    expr->getElseExpr()->getType(),
                    "then and else branches of an if-expr");
    }
    
    void verifyChecked(SuperRefExpr *expr) {
      if (!expr->getType()->is<LValueType>()) {
        Out << "Type of SuperRefExpr should be an LValueType";
        abort();
      }
    }

    void verifyChecked(ForceValueExpr *E) {
      auto valueTy = E->getType();
      auto optTy = OptionalType::get(valueTy, Ctx);
      checkSameType(optTy, E->getSubExpr()->getType(), "optional type");
    }

    void verifyChecked(OpaqueValueExpr *expr) {
      if (!OpaqueValues.count(expr)) {
        Out << "OpaqueValueExpr not introduced at this point in AST\n";
        abort();
      }

      ++OpaqueValues[expr];

      // Make sure "uniquely-referenced" actually is.
      if (expr->isUniquelyReferenced() && OpaqueValues[expr] > 1) {
        Out << "Multiple references to unique OpaqueValueExpr\n";
        abort();
      }
    }

    void verifyChecked(VarDecl *var) {
      // The fact that this is *directly* be a reference storage type
      // cuts the code down quite a bit in getTypeOfReference.
      if (var->getAttrs().hasOwnership() !=
          isa<ReferenceStorageType>(var->getType().getPointer())) {
        if (var->getAttrs().hasOwnership()) {
          Out << "VarDecl has an ownership attribute, but its type"
                 " is not a ReferenceStorageType: ";
        } else {
          Out << "VarDecl has no ownership attribute, but its type"
                 " is a ReferenceStorageType: ";
        }
        var->getType().print(Out);
        abort();
      }
    }

    void verifyParsed(EnumElementDecl *UED) {
      if (!isa<EnumDecl>(UED->getDeclContext())) {
        Out << "EnumElementDecl has wrong DeclContext";
        abort();
      }

      return verifyParsed(cast<ValueDecl>(UED));
    }

    void verifyParsed(AbstractFunctionDecl *AFD) {
      if (AFD->getArgParamPatterns().size() !=
          AFD->getBodyParamPatterns().size()) {
        Out << "number of arg and body parameter patterns should be equal";
        abort();
      }

      if (AFD->hasSelectorStyleSignature()) {
        unsigned NumExpectedParamPatterns = 1;
        if (AFD->getImplicitSelfDecl() && isa<FuncDecl>(AFD))
          NumExpectedParamPatterns++;
        if (AFD->getArgParamPatterns().size() != NumExpectedParamPatterns) {
          Out << "functions with selector-style signature should "
                 "not be curried";
          abort();
        }
      }

      return verifyParsed(cast<ValueDecl>(AFD));
    }

    void verifyParsed(ConstructorDecl *CD) {
      if (CD->getArgParamPatterns().size() != 1 ||
          CD->getBodyParamPatterns().size() != 1) {
        Out << "ConstructorDecl should have exactly one parameter pattern";
        abort();
      }

      auto *DC = CD->getDeclContext();
      if (!isa<NominalTypeDecl>(DC) && !isa<ExtensionDecl>(DC) &&
          !CD->isInvalid()) {
        Out << "ConstructorDecls outside nominal types and extensions "
               "should be marked invalid";
        abort();
      }

      return verifyParsed(cast<AbstractFunctionDecl>(CD));
    }

    void verifyChecked(ConstructorDecl *CD) {
      auto *ND = CD->getExtensionType()->getNominalOrBoundGenericNominal();
      if (!isa<ClassDecl>(ND) && !isa<StructDecl>(ND) && !isa<EnumDecl>(ND) &&
          !CD->isInvalid()) {
        Out << "ConstructorDecls outside structs, classes or enums"
               "should be marked invalid";
        abort();
      }
      return verifyChecked(cast<AbstractFunctionDecl>(CD));
    }

    void verifyParsed(DestructorDecl *DD) {
      if (DD->isGeneric()) {
        Out << "DestructorDecl can not be generic";
        abort();
      }
      if (!DD->getArgParamPatterns().empty() ||
          !DD->getBodyParamPatterns().empty()) {
        Out << "DestructorDecl should not have parameter patterns";
        abort();
      }

      auto *DC = DD->getDeclContext();
      if (!isa<NominalTypeDecl>(DC) && !isa<ExtensionDecl>(DC) &&
          !DD->isInvalid()) {
        Out << "DestructorDecls outside nominal types and extensions "
               "should be marked invalid";
        abort();
      }

      if (DD->hasSelectorStyleSignature()) {
        Out << "DestructorDecls can not have a selector-style signature";
        abort();
      }

      return verifyParsed(cast<AbstractFunctionDecl>(DD));
    }

    /// Check that the generic requirements line up with the archetypes.
    void checkGenericRequirements(Decl *decl,
                                  DeclContext *dc,
                                  GenericFunctionType *genericTy) {

      // We need to have generic parameters here.
      auto genericParams = dc->getGenericParamsOfContext();
      if (!genericParams) {
        Out << "Missing generic parameters\n";
        decl->dump(Out);
        abort();
      }

      // Step through the list of requirements in the generic type.
      auto requirements = genericTy->getRequirements();

      // Skip over same-type requirements.
      auto skipUnrepresentedRequirements = [&]() {
        for (; !requirements.empty(); requirements = requirements.slice(1)) {
          bool done = false;
          switch (requirements.front().getKind()) {
          case RequirementKind::Conformance:
            // If the second type is a protocol type, we're done.
            if (requirements.front().getSecondType()->is<ProtocolType>())
              done = true;

            break;

          case RequirementKind::SameType:
            // Skip the next same-type constraint.
            continue;

          case RequirementKind::ValueWitnessMarker:
            done = true;
            break;
          }

          if (done)
            break;
        }
      };
      skipUnrepresentedRequirements();

      // Collect all of the generic parameter lists.
      SmallVector<GenericParamList *, 4> allGenericParamLists;
      for (auto gpList = genericParams; gpList;
           gpList = gpList->getOuterParameters()) {
        allGenericParamLists.push_back(gpList);
      }
      std::reverse(allGenericParamLists.begin(), allGenericParamLists.end());

      // Helpers that diagnose failures when generic requirements mismatch.
      bool failed = false;
      auto noteFailure =[&]() {
        if (failed)
          return;

        Out << "Generic requirements don't match all archetypes\n";
        decl->dump(Out);

        Out << "\nGeneric type: " << genericTy->getString() << "\n";
        Out << "Expected requirements: ";
        bool first = true;
        for (auto gpList : allGenericParamLists) {
          for (auto archetype : gpList->getAllArchetypes()) {
            for (auto proto : archetype->getConformsTo()) {
              if (first)
                first = false;
              else
                Out << ", ";

              Out << archetype->getString() << " : "
                  << proto->getDeclaredType()->getString();
            }
          }
        }
        Out << "\n";

        failed = true;
      };

      // Walk through all of the archetypes in the generic parameter lists,
      // matching up their conformance requirements with those in the
      for (auto gpList : allGenericParamLists) {
        for (auto archetype : gpList->getAllArchetypes()) {
          // Make sure we have the value witness marker.
          if (requirements.empty()) {
            noteFailure();
            Out << "Ran out of requirements before we ran out of archetypes\n";
            break;
          }

          if (requirements.front().getKind()
                == RequirementKind::ValueWitnessMarker) {
            auto type = ArchetypeBuilder::mapTypeIntoContext(
                          dc,
                          requirements.front().getFirstType());
            if (type->isEqual(archetype)) {
              requirements = requirements.slice(1);
              skipUnrepresentedRequirements();
            } else {
              noteFailure();
              Out << "Value witness marker for " << type->getString()
                  << " does not match expected " << archetype->getString()
                  << "\n";
            }
          } else {
            noteFailure();
            Out << "Missing value witness marker for "
                << archetype->getString() << "\n";
          }

          for (auto proto : archetype->getConformsTo()) {
            // If there are no requirements left, we're missing requirements.
            if (requirements.empty()) {
              noteFailure();
              Out << "No requirement for " << archetype->getString()
                  << " : " << proto->getDeclaredType()->getString() << "\n";
              continue;
            }

            auto firstReqType = ArchetypeBuilder::mapTypeIntoContext(
                                  dc,
                                  requirements.front().getFirstType());
            auto secondReqType = ArchetypeBuilder::mapTypeIntoContext(
                                  dc,
                                  requirements.front().getSecondType());

            // If the requirements match up, move on to the next requirement.
            if (firstReqType->isEqual(archetype) &&
                secondReqType->isEqual(proto->getDeclaredType())) {
              requirements = requirements.slice(1);
              skipUnrepresentedRequirements();
              continue;
            }

            noteFailure();

            // If the requirements don't match up, complain.
            if (!firstReqType->isEqual(archetype)) {
              Out << "Mapped archetype " << firstReqType->getString()
                  << " does not match expected " << archetype->getString()
                  << "\n";
              continue;
            }

            Out << "Mapped conformance " << secondReqType->getString()
                << " does not match expected "
                << proto->getDeclaredType()->getString() << "\n";
          }
        }
      }

      if (!requirements.empty()) {
        noteFailure();
        Out << "Extra requirement "
            << requirements.front().getFirstType()->getString()
            << " : "
            << requirements.front().getSecondType()->getString()
            << "\n";
      }

      if (failed)
        abort();
    }

    void verifyChecked(AbstractFunctionDecl *AFD) {
      // If this function is generic or is within a generic type, it should
      // have an interface type.
      if ((AFD->getGenericParams() ||
           (AFD->getDeclContext()->isTypeContext() &&
            AFD->getDeclContext()->getGenericParamsOfContext()))
          && !AFD->getInterfaceType()) {
        Out << "Missing interface type for generic function\n";
        AFD->dump(Out);
        abort();
      }

      // If there is an interface type, it shouldn't have any unresolved
      // dependent member types.
      // FIXME: This is a general property of the type system.
      if (auto interfaceTy = AFD->getInterfaceType()) {
        Type unresolvedDependentTy;
        interfaceTy.findIf([&](Type type) -> bool {
          if (auto dependent = type->getAs<DependentMemberType>()) {
            if (dependent->getAssocType() == nullptr) {
              unresolvedDependentTy = dependent;
              return true;
            }
          }
          return false;
        });

        if (unresolvedDependentTy) {
          Out << "Unresolved dependent member type ";
          unresolvedDependentTy->print(Out);
          abort();
        }

        // If the interface type is generic, make sure its requirements
        // line up with the archetypes.
        if (auto genericTy = interfaceTy->getAs<GenericFunctionType>()) {
          checkGenericRequirements(AFD, AFD, genericTy);
        }
      }
    }

    void verifyChecked(DestructorDecl *DD) {
      auto *ND = DD->getExtensionType()->getNominalOrBoundGenericNominal();
      if (!isa<ClassDecl>(ND) && !DD->isInvalid()) {
        Out << "DestructorDecls outside classes should be marked invalid";
        abort();
      }
      return verifyChecked(cast<AbstractFunctionDecl>(DD));
    }

    void verifyChecked(FuncDecl *FD) {
      // Check that non-static member functions have "self" as the first
      // parameter.
      if (!FD->isStatic()) {
        auto NominalTy = FD->getDeclContext()->getDeclaredTypeInContext();
        if (!NominalTy) {
          // A non-member function.
          return;
        }
        auto FuncTy = FD->getInterfaceType();
        if (!FuncTy) {
          // FIXME: not all functions have an interface type.
          return;
        }
        auto InTy = FuncTy->castTo<AnyFunctionType>()->getInput();
        if (auto TupleTy = InTy->getAs<TupleType>()) {
          if (TupleTy->getNumElements() != 1) {
            Out << "Should have a single parameter 'self'";
            abort();
          }
          InTy = TupleTy->getFields()[0].getType();
        }
        InTy = InTy->getRValueType();

        Decl *ExpectedSelfDecl;
        Decl *ActualSelfDecl;
        if (auto *PD = dyn_cast<ProtocolDecl>(NominalTy->getAnyNominal())) {
          // In protocols, the first parameter refers to an associated type
          // 'Self'.
          ExpectedSelfDecl = PD->getSelf();
          ActualSelfDecl = InTy->castTo<GenericTypeParamType>()->getDecl();
        } else {
          ExpectedSelfDecl = NominalTy->getAnyNominal();
          ActualSelfDecl = InTy->getAnyNominal();
        }

        if (ActualSelfDecl != ExpectedSelfDecl) {
          Out << "Wrong type for 'self' parameter";
          abort();
        }
      }
    }

    void verifyParsed(FuncDecl *FD) {
      unsigned MinParamPatterns = FD->getImplicitSelfDecl() ? 2 : 1;
      if (FD->getArgParamPatterns().size() < MinParamPatterns ||
          FD->getBodyParamPatterns().size() < MinParamPatterns) {
        Out << "should have at least " << MinParamPatterns
            << " parameter patterns";
        abort();
      }

      if (FD->isGetterOrSetter()) {
        unsigned NumExpectedParamPatterns = 1;
        if (FD->getImplicitSelfDecl())
          NumExpectedParamPatterns++;
        if (isa<SubscriptDecl>(FD->getGetterOrSetterDecl()))
          NumExpectedParamPatterns++;
        if (FD->getArgParamPatterns().size() != NumExpectedParamPatterns) {
          Out << "getters and setters should not be curried";
          abort();
        }
      }

      return verifyParsed(cast<AbstractFunctionDecl>(FD));
    }

    void verifyChecked(ClassDecl *CD) {
      unsigned NumDestructors = 0;
      for (auto Member : CD->getMembers()) {
        if (isa<DestructorDecl>(Member)) {
          NumDestructors++;
        }
      }
      if (NumDestructors != 1) {
        Out << "every class should have exactly one destructor, "
               "explicitly provided or created by the type checker";
        abort();
      }

      return verifyChecked(cast<NominalTypeDecl>(CD));
    }

    void verifyParsed(AssociatedTypeDecl *ATD) {
      auto *DC = ATD->getDeclContext();
      if (!isa<NominalTypeDecl>(DC) ||
          !isa<ProtocolDecl>(cast<NominalTypeDecl>(DC))) {
        Out << "AssociatedTypeDecl should only occur inside a protocol";
        abort();
      }
      return verifyParsed(cast<AbstractTypeParamDecl>(ATD));
    }

    void verifyParsed(TuplePattern *TP) {
      if (TP->hasVararg()) {
        auto *LastPattern = TP->getFields().back().getPattern();
        if (!isa<TypedPattern>(LastPattern)) {
          Out << "a vararg subpattern of a TuplePattern should be"
                 "a TypedPattern";
          abort();
        }
      }
    }

    void verifyChecked(TuplePattern *TP) {
      if (TP->hasVararg()) {
        auto *LastPattern = TP->getFields().back().getPattern();
        Type T = cast<TypedPattern>(LastPattern)->getType()->getCanonicalType();
        if (auto *BGT = T->getAs<BoundGenericType>()) {
          if (BGT->getDecl() == Ctx.getSliceDecl())
            return;
        }
        Out << "a vararg subpattern of a TuplePattern has wrong type";
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
        Out << "qualification of " << what << " do not match\n";
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
        Q = LV->getQualifiers() - LValueType::Qual(LValueType::Qual::Implicit);
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
    Type checkMetatypeType(Type type, const char *what) {
      auto metatype = type->getAs<MetaTypeType>();
      if (metatype) return metatype->getInstanceType();

      Out << what << " is not a metatype: ";
      type.print(Out);
      Out << "\n";
      abort();
    }

    void checkIsTypeOfRValue(ValueDecl *D, Type rvalueType, const char *what) {
      auto declType = D->getType();
      if (auto refType = declType->getAs<ReferenceStorageType>())
        declType = refType->getReferentType();
      checkSameType(declType, rvalueType, what);
    }

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

    void checkTrivialSubtype(Type srcTy, Type destTy, const char *what) {
      if (srcTy->isEqual(destTy)) return;

      if (auto srcMetaType = srcTy->getAs<MetaTypeType>()) {
        if (auto destMetaType = destTy->getAs<MetaTypeType>()) {
          return checkTrivialSubtype(srcMetaType->getInstanceType(),
                                     destMetaType->getInstanceType(),
                                     what);
        }
        goto fail;
      }

      {
        ClassDecl *srcClass = srcTy->getClassOrBoundGenericClass();
        ClassDecl *destClass = destTy->getClassOrBoundGenericClass();

        if (!srcClass || !destClass) {
          Out << "subtype conversion in " << what
              << " doesn't involve class types: ";
          srcTy.print(Out);
          Out << " to ";
          destTy.print(Out);
          Out << "\n";
          abort();
        }

        for (Type srcSuperTy = srcTy->getSuperclass(nullptr);
             srcSuperTy;
             srcSuperTy = srcSuperTy->getSuperclass(nullptr)) {
          if (srcSuperTy->isEqual(destTy))
            return;
        }

        Out << "subtype conversion in " << what << " is not to super class: ";
        srcTy.print(Out);
        Out << " to ";
        destTy.print(Out);
        Out << "\n";
        abort();
      }

    fail:
      Out << "subtype conversion in " << what << " is invalid: ";
      srcTy.print(Out);
      Out << " to ";
      destTy.print(Out);
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
      
      // FIXME: Actually check this?
      if (T0->isExistentialType() || T1->isExistentialType())
        return;
      
      Out << "incompatible types for " << what << ": ";
      T0.print(Out);
      Out << " vs. ";
      T1.print(Out);
      Out << "\n";
      abort();
    }

    bool isGoodSourceRange(SourceRange SR) {
      if (SR.isInvalid())
        return false;
      (void) Ctx.SourceMgr.findBufferContainingLoc(SR.Start);
      (void) Ctx.SourceMgr.findBufferContainingLoc(SR.End);
      return true;
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
      if (!isGoodSourceRange(E->getSourceRange())) {
        Out << "bad source range for expression: ";
        E->print(Out);
        Out << "\n";
        abort();
      }
      // FIXME: Re-visit this to always do the check.
      if (!E->isImplicit())
        checkSourceRanges(E->getSourceRange(), Parent,
                          [&]{ E->print(Out); } );
    }

    void checkSourceRanges(Stmt *S) {
      if (!S->getSourceRange().isValid()) {
        // We don't care about source ranges on implicitly-generated
        // statements.
        if (S->isImplicit())
          return;

        Out << "invalid source range for statement: ";
        S->print(Out);
        Out << "\n";
        abort();
      }
      if (!isGoodSourceRange(S->getSourceRange())) {
        Out << "bad source range for statement: ";
        S->print(Out);
        Out << "\n";
        abort();
      }
      checkSourceRanges(S->getSourceRange(), Parent,
                        [&]{ S->print(Out); });
    }

    void checkSourceRanges(Pattern *P) {
      if (!P->getSourceRange().isValid()) {
        // We don't care about source ranges on implicitly-generated
        // patterns.
        if (P->isImplicit())
          return;

        Out << "invalid source range for pattern: ";
        P->print(Out);
        Out << "\n";
        abort();
      }
      if (!isGoodSourceRange(P->getSourceRange())) {
        Out << "bad source range for pattern: ";
        P->print(Out);
        Out << "\n";
        abort();
      }
      checkSourceRanges(P->getSourceRange(), Parent,
                        [&]{ P->print(Out); });
    }

    void checkSourceRanges(Decl *D) {
      if (!D->getSourceRange().isValid()) {
        // We don't care about source ranges on implicitly-generated
        // decls.
        if (D->isImplicit())
          return;
        
        Out << "invalid source range for decl: ";
        D->print(Out);
        Out << "\n";
        abort();
      }
      checkSourceRanges(D->getSourceRange(), Parent,
                        [&]{ D->print(Out); });
    }

    /// \brief Verify that the given source ranges is contained within the
    /// parent's source range.
    void checkSourceRanges(SourceRange Current,
                           ASTWalker::ParentTy Parent,
                           std::function<void()> printEntity) {
      SourceRange Enclosing;
      if (Parent.isNull())
          return;

      if (Parent.getAsModule()) {
        return;
      } else if (Decl *D = Parent.getAsDecl()) {
        Enclosing = D->getSourceRange();
        if (D->isImplicit())
          return;
        // FIXME: This is not working well for decl parents.
        return;
      } else if (Stmt *S = Parent.getAsStmt()) {
        Enclosing = S->getSourceRange();
        if (S->isImplicit())
          return;
      } else if (Pattern *P = Parent.getAsPattern()) {
        Enclosing = P->getSourceRange();
      } else if (Expr *E = Parent.getAsExpr()) {
        // FIXME: This hack is required because the inclusion check below
        // doesn't compares the *start* of the ranges, not the end of the
        // ranges.  In the case of an interpolated string literal expr, the
        // subexpressions are contained within the string token.  This means
        // that comparing the start of the string token to the end of an
        // embedded expression will fail.
        if (isa<InterpolatedStringLiteralExpr>(E))
          return;

        if (E->isImplicit())
          return;
        
        Enclosing = E->getSourceRange();
      } else if (TypeRepr *TyR = Parent.getAsTypeRepr()) {
        Enclosing = TyR->getSourceRange();
      } else {
        llvm_unreachable("impossible parent node");
      }
      
      if (!Ctx.SourceMgr.rangeContains(Enclosing, Current)) {
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

    void checkErrors(Expr *E) {}
    void checkErrors(Stmt *S) {}
    void checkErrors(Pattern *P) {}
    void checkErrors(Decl *D) {}
    void checkErrors(ValueDecl *D) {
      if (!D->hasType())
        return;
      if (D->isInvalid() && !D->getType()->is<ErrorType>()) {
        Out << "Invalid decl has non-error type!\n";
        D->dump(Out);
        abort();
      }
      if (D->getType()->is<ErrorType>() && !D->isInvalid()) {
        Out << "Valid decl has error type!\n";
        D->dump(Out);
        abort();
      }
    }
  };
}

void swift::verify(SourceFile &SF) {
  Verifier verifier(&SF.TU);
  SF.walk(verifier);
}

void swift::verify(Decl *D) {
  Verifier V(D->getDeclContext()->getParentModule());
  D->walk(V);
}

