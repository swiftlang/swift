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
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/AST/Mangle.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"
#include <functional>
using namespace swift;

namespace {

template<typename T>
struct ASTNodeBase {};

#define EXPR(ID, PARENT) \
    template<> \
    struct ASTNodeBase<ID ## Expr *> { \
      typedef PARENT BaseTy; \
    };
#define ABSTRACT_EXPR(ID, PARENT) EXPR(ID, PARENT)
#include "swift/AST/ExprNodes.def"

#define STMT(ID, PARENT) \
    template<> \
    struct ASTNodeBase<ID ## Stmt *> { \
      typedef PARENT BaseTy; \
    };
#include "swift/AST/StmtNodes.def"

#define DECL(ID, PARENT) \
    template<> \
    struct ASTNodeBase<ID ## Decl *> { \
      typedef PARENT BaseTy; \
    };
#define ABSTRACT_DECL(ID, PARENT) DECL(ID, PARENT)
#include "swift/AST/DeclNodes.def"

#define PATTERN(ID, PARENT) \
    template<> \
    struct ASTNodeBase<ID ## Pattern *> { \
      typedef PARENT BaseTy; \
    };
#include "swift/AST/PatternNodes.def"

  enum ShouldHalt { Continue, Halt };

  class Verifier : public ASTWalker {
    PointerUnion<Module *, SourceFile *> M;
    ASTContext &Ctx;
    llvm::raw_ostream &Out;
    const bool HadError;
    SmallVector<bool, 8> InImplicitBraceStmt;

    /// \brief The stack of functions we're visiting.
    SmallVector<DeclContext *, 4> Functions;

    /// \brief The stack of scopes we're visiting.
    using ScopeLike = llvm::PointerUnion<DeclContext *, BraceStmt *>;
    SmallVector<ScopeLike, 4> Scopes;

    /// The set of archetypes that are currently available.
    SmallPtrSet<ArchetypeType *, 4> ActiveArchetypes;

    /// \brief The stack of optional evaluations active at this point.
    SmallVector<OptionalEvaluationExpr *, 4> OptionalEvaluations;

    /// \brief The set of opaque value expressions active at this point.
    llvm::DenseMap<OpaqueValueExpr *, unsigned> OpaqueValues;

    /// The set of opened existential archetypes that are currently
    /// active.
    llvm::DenseSet<ArchetypeType *> OpenedExistentialArchetypes;

    /// A key into ClosureDiscriminators is a combination of a
    /// ("canonicalized") local DeclContext* and a flag for whether to
    /// use the explicit closure sequence (false) or the implicit
    /// closure sequence (true).
    typedef llvm::PointerIntPair<DeclContext*,1,bool> ClosureDiscriminatorKey;
    llvm::DenseMap<ClosureDiscriminatorKey,
                   llvm::SmallBitVector> ClosureDiscriminators;
    DeclContext *CanonicalTopLevelContext = nullptr;

    /// Collect all of the archetypes in this declaration context and its
    /// parents.
    void collectAllArchetypes(DeclContext *dc) {
      for (auto genericParams = dc->getGenericParamsOfContext();
           genericParams;
           genericParams = genericParams->getOuterParameters()) {
        ActiveArchetypes.insert(genericParams->getAllArchetypes().begin(),
                                genericParams->getAllArchetypes().end());
      }
    }

    Verifier(PointerUnion<Module *, SourceFile *> M, DeclContext *DC)
      : M(M),
        Ctx(M.is<Module *>() ? M.get<Module *>()->getASTContext()
                             : M.get<SourceFile *>()->getASTContext()),
        Out(llvm::errs()),
        HadError(Ctx.hadError())
    {
      Scopes.push_back(DC);
      collectAllArchetypes(DC);
    }

  public:
    Verifier(Module *M, DeclContext *DC)
      : Verifier(PointerUnion<Module *, SourceFile *>(M), DC) { }
    Verifier(SourceFile &SF, DeclContext *DC)
      : Verifier(&SF, DC) { }

    static Verifier forDecl(const Decl *D) {
      DeclContext *DC = D->getDeclContext();
      DeclContext *topDC = DC->getModuleScopeContext();
      if (auto SF = dyn_cast<SourceFile>(topDC))
        return Verifier(*SF, DC);
      return Verifier(topDC->getParentModule(), DC);
    }

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      switch (E->getKind()) {
#define DISPATCH(ID) return dispatchVisitPreExpr(static_cast<ID##Expr*>(E))
#define EXPR(ID, PARENT) \
      case ExprKind::ID: \
        DISPATCH(ID);
#define UNCHECKED_EXPR(ID, PARENT) \
      case ExprKind::ID: \
        assert((HadError || !M.is<SourceFile*>() || \
                M.get<SourceFile*>()->ASTStage < SourceFile::TypeChecked) && \
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
        assert((HadError || !M.is<SourceFile*>() || \
                M.get<SourceFile*>()->ASTStage < SourceFile::TypeChecked) && \
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

    Pattern *walkToPatternPost(Pattern *P) override {
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
      if (shouldVerify(node))
        return true;
      cleanup(node);
      return false;
    }

    /// Helper template for dispatching pre-visitation.
    /// If we're visiting in pre-order, don't validate the node yet;
    /// just check whether we should stop further descent.
    template <class T> std::pair<bool, Expr *> dispatchVisitPreExpr(T node) {
      if (shouldVerify(node))
        return { true, node };
      cleanup(node);
      return { false, node };
    }

    /// Helper template for dispatching pre-visitation.
    /// If we're visiting in pre-order, don't validate the node yet;
    /// just check whether we should stop further descent.
    template <class T> std::pair<bool, Stmt *> dispatchVisitPreStmt(T node) {
      if (shouldVerify(node))
        return { true, node };
      cleanup(node);
      return { false, node };
    }

    /// Helper template for dispatching pre-visitation.
    /// If we're visiting in pre-order, don't validate the node yet;
    /// just check whether we should stop further descent.
    template <class T>
    std::pair<bool, Pattern *> dispatchVisitPrePattern(T node) {
      if (shouldVerify(node))
        return { true, node };
      cleanup(node);
      return { false, node };
    }

    /// Helper template for dispatching post-visitation.
    template <class T> T dispatchVisitPost(T node) {
      // Verify source ranges if the AST node was parsed from source.
      SourceFile *SF = M.dyn_cast<SourceFile *>();
      if (SF) {
        // If we are inside an implicit BraceStmt, don't verify source
        // locations.  LLDB creates implicit BraceStmts which contain a mix of
        // generated/user-written code.
        if (InImplicitBraceStmt.empty() || !InImplicitBraceStmt.back())
          checkSourceRanges(node);
      }

      // Check that nodes marked invalid have the correct type.
      checkErrors(node);

      // Always verify the node as a parsed node.
      verifyParsed(node);

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
        PrettyStackTraceDecl debugStack("verifying DeclContext", D);
        abort();
      }
    }
    template<typename T>
    void verifyParsedBase(T ASTNode) {
      verifyParsed(cast<typename ASTNodeBase<T>::BaseTy>(ASTNode));
    }

    void verifyBound(Expr *E) {}
    void verifyBound(Stmt *S) {}
    void verifyBound(Pattern *P) {}
    void verifyBound(Decl *D) {}

    /// @{
    /// These verification functions are always run on type checked ASTs
    /// (even if there were errors).
    void verifyCheckedAlways(Expr *E) {
      if (E->getType())
        verifyChecked(E->getType());
    }
    void verifyCheckedAlways(Stmt *S) {}
    void verifyCheckedAlways(Pattern *P) {
      if (P->hasType())
        verifyChecked(P->getType());
    }
    void verifyCheckedAlways(Decl *D) {
      if (!D->isInvalid() &&
          D->getAttrs().hasAttribute<OverrideAttr>()) {
        if (!isa<ClassDecl>(D->getDeclContext()) &&
            !isa<ExtensionDecl>(D->getDeclContext())) {
          PrettyStackTraceDecl debugStack("verifying override", D);
          Out << "'override' attribute outside of a class\n";
          D->dump(Out);
          abort();
        }
      }
    }

    template<typename T>
    void verifyCheckedAlwaysBase(T ASTNode) {
      verifyCheckedAlways(cast<typename ASTNodeBase<T>::BaseTy>(ASTNode));
    }
    /// @}

    /// @{
    /// These verification functions are run on type checked ASTs if there were
    /// no errors.
    void verifyChecked(Expr *E) { }
    void verifyChecked(Stmt *S) {}
    void verifyChecked(Pattern *P) { }
    void verifyChecked(Decl *D) {}

    void verifyChecked(Type type) {
      if (!type)
        return;

      // Check for type variables that escaped the type checker.
      if (type->hasTypeVariable()) {
        Out << "a type variable escaped the type checker";
        abort();
      }

      bool foundError = type.findIf([&](Type type) -> bool {
        if (auto archetype = type->getAs<ArchetypeType>()) {
          // We should know about archetypes corresponding to opened
          // existerntial archetypes.
          if (archetype->getOpenedExistentialType()) {
            if (OpenedExistentialArchetypes.count(archetype) == 0) {
              Out << "Found opened existential archetype "
                  << archetype->getString()
                  << " outside enclosing OpenExistentialExpr\n";
              return true;
            }

            return false;
          }

          // Otherwise, the archetype needs to be from this scope.
          if (ActiveArchetypes.count(archetype) == 0) {
            // FIXME: Make an exception for serialized extensions, which don't
            // currently have the correct archetypes.
            if (auto activeScope = Scopes.back().dyn_cast<DeclContext *>()) {
              do {
                if (isa<ExtensionDecl>(activeScope) &&
                    isa<LoadedFile>(activeScope->getModuleScopeContext())) {
                  return false;
                }
                activeScope = activeScope->getParent();
              } while (!activeScope->isModuleScopeContext());
            }

            Out << "AST verification error: archetype " << archetype
                << " not allowed in this context\n";

            auto knownDC = Ctx.ArchetypeContexts.find(archetype);
            if (knownDC != Ctx.ArchetypeContexts.end()) {
              llvm::errs() << "archetype came from:\n";
              knownDC->second->dumpContext();
              llvm::errs() << "\n";
            }

            return true;
          }

          // Make sure that none of the nested types are dependent.
          for (const auto &nested : archetype->getNestedTypes()) {
            if (auto nestedType = nested.second.getAsConcreteType()) {
              if (nestedType->isDependentType()) {
                Out << "Nested type " << nested.first.str()
                    << " of archetype " << archetype->getString()
                    << " is dependent type " << nestedType->getString()
                    << "\n";
                return true;
              }
            }
          }
        }

        return false;
      });
      
      if (foundError)
        abort();
    }

    template<typename T>
    void verifyCheckedBase(T ASTNode) {
      verifyChecked(cast<typename ASTNodeBase<T>::BaseTy>(ASTNode));
    }
    /// @}

    // Specialized verifiers.

    /// Retrieve the generic parameters of the specified declaration context,
    /// without looking into its parent contexts.
    static GenericParamList *getImmediateGenericParams(DeclContext *dc) {
      switch (dc->getContextKind()) {
      case DeclContextKind::Module:
      case DeclContextKind::FileUnit:
      case DeclContextKind::TopLevelCodeDecl:
      case DeclContextKind::Initializer:
      case DeclContextKind::AbstractClosureExpr:
      case DeclContextKind::SerializedLocal:
        return nullptr;

      case DeclContextKind::AbstractFunctionDecl:
        return cast<AbstractFunctionDecl>(dc)->getGenericParams();

      case DeclContextKind::NominalTypeDecl:
        return cast<NominalTypeDecl>(dc)->getGenericParams();

      case DeclContextKind::ExtensionDecl:
        return cast<ExtensionDecl>(dc)->getGenericParams();
      }
      llvm_unreachable("bad DeclContextKind");
    }

    void pushScope(DeclContext *scope) {
      Scopes.push_back(scope);

      // Add any archetypes from this scope into the set of active archetypes.
      if (auto genericParams = getImmediateGenericParams(scope))
        ActiveArchetypes.insert(genericParams->getAllArchetypes().begin(),
                                genericParams->getAllArchetypes().end());
    }
    void pushScope(BraceStmt *scope) {
      Scopes.push_back(scope);
    }
    void popScope(DeclContext *scope) {
      assert(Scopes.back().get<DeclContext*>() == scope);

      // Remove archetypes from this scope from the set of active archetypes.
      if (auto genericParams
            = getImmediateGenericParams(Scopes.back().get<DeclContext*>())) {
        for (auto archetype : genericParams->getAllArchetypes()) {
          if (!ActiveArchetypes.erase(archetype)) {
            llvm::errs() << "archetype " << archetype
                         << " not introduced by scope?\n";
            abort();
          }
        }
      }

      Scopes.pop_back();
    }
    void popScope(BraceStmt *scope) {
      assert(Scopes.back().get<BraceStmt*>() == scope);
      Scopes.pop_back();
    }

    void pushFunction(DeclContext *functionScope) {
      pushScope(functionScope);
      Functions.push_back(functionScope);
    }
    void popFunction(DeclContext *functionScope) {
      assert(Functions.back() == functionScope);
      Functions.pop_back();
      popScope(functionScope);
    }

#define FUNCTION_LIKE(NODE)                                     \
    bool shouldVerify(NODE *fn) {                               \
      pushFunction(fn);                                         \
      return shouldVerify(cast<ASTNodeBase<NODE*>::BaseTy>(fn));\
    }                                                           \
    void cleanup(NODE *fn) {                                    \
      popFunction(fn);                                          \
    }
#define SCOPE_LIKE(NODE)                                        \
    bool shouldVerify(NODE *fn) {                               \
      pushScope(fn);                                            \
      if (fn->hasLazyMembers())                                 \
        return false;                                           \
      return shouldVerify(cast<ASTNodeBase<NODE*>::BaseTy>(fn));\
    }                                                           \
    void cleanup(NODE *fn) {                                    \
      popScope(fn);                                             \
    }

    FUNCTION_LIKE(AbstractClosureExpr)
    FUNCTION_LIKE(ConstructorDecl)
    FUNCTION_LIKE(DestructorDecl)
    FUNCTION_LIKE(FuncDecl)
    SCOPE_LIKE(NominalTypeDecl)
    SCOPE_LIKE(ExtensionDecl)

#undef SCOPE_LIKE
#undef FUNCTION_LIKE

    bool shouldVerify(BraceStmt *BS) {
      pushScope(BS);
      InImplicitBraceStmt.push_back(BS->isImplicit());
      return shouldVerify(cast<Stmt>(BS));
    }

    void cleanup(BraceStmt *BS) {
      InImplicitBraceStmt.pop_back();
      popScope(BS);
    }

    bool shouldVerify(OpenExistentialExpr *expr) {
      if (!shouldVerify(cast<Expr>(expr)))
        return false;

      OpaqueValues[expr->getOpaqueValue()] = 0;
      assert(OpenedExistentialArchetypes.count(expr->getOpenedArchetype())==0);
      OpenedExistentialArchetypes.insert(expr->getOpenedArchetype());
      return true;
    }

    void cleanup(OpenExistentialExpr *expr) {
      OpaqueValues.erase(expr->getOpaqueValue());
      assert(OpenedExistentialArchetypes.count(expr->getOpenedArchetype())==1);
      OpenedExistentialArchetypes.erase(expr->getOpenedArchetype());
    }

    // Keep a stack of the currently-live optional evaluations.
    bool shouldVerify(OptionalEvaluationExpr *expr) {
      if (!shouldVerify(cast<Expr>(expr)))
        return false;

      OptionalEvaluations.push_back(expr);
      return true;
    }
    void cleanup(OptionalEvaluationExpr *expr) {
      assert(OptionalEvaluations.back() == expr);
      OptionalEvaluations.pop_back();
    }

    /// Canonicalize the given DeclContext pointer, in terms of
    /// producing something that can be looked up in
    /// ClosureDiscriminators.
    DeclContext *getCanonicalDeclContext(DeclContext *DC) {
      // All we really need to do is use a single TopLevelCodeDecl.
      if (auto topLevel = dyn_cast<TopLevelCodeDecl>(DC)) {
        if (!CanonicalTopLevelContext)
          CanonicalTopLevelContext = topLevel;
        return CanonicalTopLevelContext;
      }

      // TODO: check for uniqueness of initializer contexts?

      return DC;
    }

    /// Return the appropriate discriminator set for a closure expression.
    llvm::SmallBitVector &getClosureDiscriminators(AbstractClosureExpr *closure) {
      auto dc = getCanonicalDeclContext(closure->getParent());
      bool isAutoClosure = isa<AutoClosureExpr>(closure);
      return ClosureDiscriminators[ClosureDiscriminatorKey(dc, isAutoClosure)];
    }

    void verifyCheckedAlways(ValueDecl *D) {
      if (D->hasName())
        checkMangling(D);

      if (D->hasType())
        verifyChecked(D->getType());

      if (auto Overridden = D->getOverriddenDecl()) {
        if (D->getDeclContext() == Overridden->getDeclContext()) {
          PrettyStackTraceDecl debugStack("verifying overriden", D);
          Out << "can not override a decl in the same DeclContext";
          D->dump(Out);
          Overridden->dump(Out);
          abort();
        }
      }
      verifyCheckedAlwaysBase(D);
    }

    void verifyCheckedAlways(NominalTypeDecl *D) {
      checkMangling(D);
      verifyCheckedAlwaysBase(D);
    }

    void verifyChecked(ThrowExpr *E) {
      checkSameType(E->getSubExpr()->getType(),
                    checkExceptionTypeExists("throw expression"),
                    "throw operand");
      verifyCheckedBase(E);
    }

    void verifyChecked(CatchStmt *S) {
      checkSameType(S->getErrorPattern()->getType(),
                    checkExceptionTypeExists("catch statement"),
                    "catch pattern");
      verifyCheckedBase(S);
    }

    void verifyChecked(ReturnStmt *S) {
      auto func = Functions.back();
      Type resultType;
      if (FuncDecl *FD = dyn_cast<FuncDecl>(func)) {
        resultType = FD->getResultType();
      } else if (auto closure = dyn_cast<AbstractClosureExpr>(func)) {
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

      verifyCheckedBase(S);
    }
    void verifyChecked(DeferStmt *S) {
      verifyCheckedBase(S);
    }

    void verifyChecked(FailStmt *S) {
      // Dig out the initializer we're in (if we are).
      ConstructorDecl *ctor = nullptr;
      if (!Functions.empty()) {
        ctor = dyn_cast<ConstructorDecl>(Functions.back());
      }

      // Fail statements are only permitted in initializers.
      if (!ctor) {
        Out << "'fail' statement outside of initializer\n";
        abort();
      }

      if (ctor->getFailability() == OTK_None && !ctor->isInvalid()) {
        Out << "non-failable initializer contains a 'fail' statement\n";
        ctor->dump(Out);
        abort();
      }
    }

    void checkConditionElement(const StmtConditionElement &elt) {
      if (auto E = elt.getConditionOrNull()) {
        checkSameType(E->getType(), BuiltinIntegerType::get(1, Ctx),
                      "condition type");
        return;
      }
      checkSameType(elt.getPattern()->getType(),
                    elt.getInitializer()->getType(),
                    "conditional binding type");
    }
    
    void checkCondition(StmtCondition C) {
      for (auto elt : C)
        checkConditionElement(elt);
    }
    
    void verifyChecked(IfStmt *S) {
      checkCondition(S->getCond());
      verifyCheckedBase(S);
    }

    void verifyChecked(GuardStmt *S) {
      checkCondition(S->getCond());
      verifyCheckedBase(S);
    }

    void verifyChecked(WhileStmt *S) {
      checkCondition(S->getCond());
      verifyCheckedBase(S);
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

    void verifyChecked(DeclRefExpr *E) {
      if (E->getType()->is<PolymorphicFunctionType>()) {
        PrettyStackTraceExpr debugStack(Ctx, "verifying decl reference", E);
        Out << "unspecialized reference with polymorphic type "
          << E->getType().getString() << "\n";
        E->dump(Out);
        abort();
      }
      verifyCheckedBase(E);
    }

    void verifyChecked(AssignExpr *S) {
      Type lhsTy = checkAssignDest(S->getDest());
      checkSameType(lhsTy, S->getSrc()->getType(), "assignment operands");
      verifyCheckedBase(S);
    }

    void verifyChecked(InOutExpr *E) {
      Type srcObj = checkLValue(E->getSubExpr()->getType(),
                                "result of InOutExpr");
      auto DestTy = E->getType()->castTo<InOutType>()->getObjectType();
      
      checkSameType(DestTy, srcObj, "object types for InOutExpr");
      verifyCheckedBase(E);
    }

    void verifyParsed(AbstractClosureExpr *E) {
      Type Ty = E->getType();
      if (!Ty)
        return;
      if (Ty->is<ErrorType>())
        return;
      if (!Ty->is<FunctionType>()) {
        PrettyStackTraceExpr debugStack(Ctx, "verifying closure", E);
        Out << "a closure should have a function type";
        E->print(Out);
        Out << "\n";
        abort();
      }
      verifyParsedBase(E);
    }

    void verifyChecked(AbstractClosureExpr *E) {
      PrettyStackTraceExpr debugStack(Ctx, "verifying closure", E);

      assert(Scopes.back().get<DeclContext*>() == E);
      assert(E->getParent()->isLocalContext() &&
             "closure expression was not in local context!");

      // Check that the discriminator is unique in its context.
      auto &discriminatorSet = getClosureDiscriminators(E);
      unsigned discriminator = E->getDiscriminator();
      if (discriminator >= discriminatorSet.size()) {
        discriminatorSet.resize(discriminator+1);
        discriminatorSet.set(discriminator);
      } else if (discriminatorSet.test(discriminator)) {
        Out << "a closure must have a unique discriminator in its context\n";
        E->print(Out);
        Out << "\n";
        abort();
      } else {
        discriminatorSet.set(discriminator);
      }

      // If the enclosing scope is a DC directly, rather than a local scope,
      // then the closure should be parented by an Initializer.  Otherwise,
      // it should be parented by the innermost function.
      auto enclosingScope = Scopes[Scopes.size() - 2];
      auto enclosingDC = enclosingScope.dyn_cast<DeclContext*>();
      if (enclosingDC && !isa<AbstractClosureExpr>(enclosingDC)
          && !(isa<SourceFile>(enclosingDC)
               && cast<SourceFile>(enclosingDC)->Kind == SourceFileKind::REPL)){
        auto parentDC = E->getParent();
        if (!isa<Initializer>(parentDC)) {
          Out << "a closure in non-local context should be parented "
                 "by an initializer or REPL context";
          E->print(Out);
          Out << "\n";
          abort();
        } else if (parentDC->getParent() != enclosingDC) {
          Out << "closure in non-local context not grandparented by its "
                 "enclosing function";
          E->print(Out);
          Out << "\n";
          abort();
        }
      } else if (Functions.size() >= 2 &&
                 Functions[Functions.size() - 2] != E->getParent()) {
        Out << "closure in local context not parented by its "
               "enclosing function";
        E->print(Out);
        Out << "\n";
        abort();
      }

      if (E->getDiscriminator() == AbstractClosureExpr::InvalidDiscriminator) {
        Out << "a closure expression should have a valid discriminator\n";
        E->print(Out);
        Out << "\n";
        abort();
      }
    }

    void verifyChecked(MetatypeConversionExpr *E) {
      PrettyStackTraceExpr debugStack(Ctx, "verifying MetatypeConversion", E);

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
      verifyCheckedBase(E);
    }
    
    void verifyChecked(ClassMetatypeToObjectExpr *E) {
      PrettyStackTraceExpr debugStack(Ctx, "verifying ClassMetatypeToObject", E);
      
      auto srcTy = checkMetatypeType(E->getSubExpr()->getType(),
                                     "source of ClassMetatypeToObject");
      
      if (!srcTy->mayHaveSuperclass()) {
        Out << "ClassMetatypeToObject with non-class metatype:\n";
        E->print(Out);
        Out << "\n";
        abort();
      }
      
      if (!E->getType()->isEqual(
             Ctx.getProtocol(KnownProtocolKind::AnyObject)->getDeclaredType())){
        Out << "ClassMetatypeToObject does not produce AnyObject:\n";
        E->print(Out);
        Out << "\n";
        abort();
      }
    }
    
    void verifyChecked(ExistentialMetatypeToObjectExpr *E) {
      PrettyStackTraceExpr debugStack(Ctx,
                                    "verifying ExistentialMetatypeToObject", E);
      
      auto srcTy = checkMetatypeType(E->getSubExpr()->getType(),
                                     "source of ExistentialMetatypeToObject");
      
      if (!E->getSubExpr()->getType()->is<ExistentialMetatypeType>()) {
        Out << "ExistentialMetatypeToObject with non-existential "
               "metatype:\n";
        E->print(Out);
        Out << "\n";
        abort();
      }
      if (!srcTy->isClassExistentialType()) {
        Out << "ExistentialMetatypeToObject with non-class existential "
               "metatype:\n";
        E->print(Out);
        Out << "\n";
        abort();
      }
      
      if (!E->getType()->isEqual(
             Ctx.getProtocol(KnownProtocolKind::AnyObject)->getDeclaredType())){
        Out << "ExistentialMetatypeToObject does not produce AnyObject:\n";
        E->print(Out);
        Out << "\n";
        abort();
      }
    }
    
    void verifyChecked(ProtocolMetatypeToObjectExpr *E) {
      PrettyStackTraceExpr debugStack(Ctx,
                                    "verifying ProtocolMetatypeToObject", E);
      
      auto srcTy = checkMetatypeType(E->getSubExpr()->getType(),
                                     "source of ProtocolMetatypeToObject");
      if (E->getSubExpr()->getType()->is<ExistentialMetatypeType>()) {
        Out << "ProtocolMetatypeToObject with existential "
               "metatype:\n";
        E->print(Out);
        Out << "\n";
        abort();
      }

      SmallVector<ProtocolDecl*, 2> protocols;
      if (!srcTy->isExistentialType(protocols)
          || protocols.size() != 1
          || !protocols[0]->isObjC()) {
        Out << "ProtocolMetatypeToObject with non-ObjC-protocol metatype:\n";
        E->print(Out);
        Out << "\n";
        abort();
      }

      if (!E->getType()->getClassOrBoundGenericClass()) {
        Out << "ProtocolMetatypeToObject does not produce class:\n";
        E->print(Out);
        Out << "\n";
        abort();
      }
    }
    
    void verifyChecked(PointerToPointerExpr *E) {
      PrettyStackTraceExpr debugStack(Ctx,
                                      "verifying PointerToPointer", E);

      auto fromElement = E->getSubExpr()->getType()->getAnyPointerElementType();
      auto toElement = E->getType()->getAnyPointerElementType();
      
      if (!fromElement && !toElement) {
        Out << "PointerToPointer does not convert between pointer types:\n";
        E->print(Out);
        Out << "\n";
        abort();
      }
    }
    
    void verifyChecked(InOutToPointerExpr *E) {
      PrettyStackTraceExpr debugStack(Ctx,
                                      "verifying InOutToPointer", E);
      
      auto fromElement = E->getSubExpr()->getType()->getInOutObjectType();
      auto toElement = E->getType()->getAnyPointerElementType();
      
      if (!E->getSubExpr()->getType()->is<InOutType>() && !toElement) {
        Out << "InOutToPointer does not convert from inout to pointer:\n";
        E->print(Out);
        Out << "\n";
        abort();
      }
      
      // Ensure we don't convert an array to a void pointer this way.
      
      if (fromElement->getNominalOrBoundGenericNominal() == Ctx.getArrayDecl()
          && toElement->isEqual(Ctx.TheEmptyTupleType)) {
        Out << "InOutToPointer is converting an array to a void pointer; "
               "ArrayToPointer should be used instead:\n";
        E->print(Out);
        Out << "\n";
        abort();
      }
    }
    
    void verifyChecked(ArrayToPointerExpr *E) {
      PrettyStackTraceExpr debugStack(Ctx,
                                      "verifying ArrayToPointer", E);

      // The source may be optionally inout.
      auto fromArray = E->getSubExpr()->getType()->getInOutObjectType();
      
      if (fromArray->getNominalOrBoundGenericNominal() != Ctx.getArrayDecl()) {
        Out << "ArrayToPointer does not convert from array:\n";
        E->print(Out);
        Out << "\n";
        abort();
      }
      
      auto toElement = E->getType()->getAnyPointerElementType();

      if (!toElement) {
        Out << "ArrayToPointer does not convert to pointer:\n";
        E->print(Out);
        Out << "\n";
        abort();
      }
    }
    
    void verifyChecked(StringToPointerExpr *E) {
      PrettyStackTraceExpr debugStack(Ctx,
                                      "verifying StringToPointer", E);
      
      if (E->getSubExpr()->getType()->getNominalOrBoundGenericNominal()
            != Ctx.getStringDecl()) {
        Out << "StringToPointer does not convert from string:\n";
        E->print(Out);
        Out << "\n";
        abort();
      }
      
      PointerTypeKind PTK;
      auto toElement = E->getType()->getAnyPointerElementType(PTK);
      if (!toElement) {
        Out << "StringToPointer does not convert to pointer:\n";
        E->print(Out);
        Out << "\n";
        abort();
      }
      if (PTK != PTK_UnsafePointer) {
        Out << "StringToPointer converts to non-const pointer:\n";
        E->print(Out);
        Out << "\n";
        abort();
      }
    }
    
    void verifyChecked(CollectionUpcastConversionExpr *E) {
      verifyChecked(E->getSubExpr());
      verifyCheckedBase(E);
    }
        
    void verifyChecked(DerivedToBaseExpr *E) {
      PrettyStackTraceExpr debugStack(Ctx, "verifying DerivedToBaseExpr", E);

      auto destTy = E->getType();
      auto srcTy = E->getSubExpr()->getType();
      if (destTy->isEqual(srcTy)) {
        Out << "trivial DerivedToBaseExpr:\n";
        E->print(Out);
        Out << "\n";
        abort();
      }

      if (!destTy->getClassOrBoundGenericClass() ||
          !(srcTy->getClassOrBoundGenericClass() ||
            srcTy->is<DynamicSelfType>())) {
        Out << "DerivedToBaseExpr does not involve class types:\n";
        E->print(Out);
        Out << "\n";
        abort();
      }

      checkTrivialSubtype(srcTy, destTy, "DerivedToBaseExpr");
      verifyCheckedBase(E);
    }

    void verifyChecked(TupleElementExpr *E) {
      PrettyStackTraceExpr debugStack(Ctx, "verifying TupleElementExpr", E);

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

      if (E->getFieldNumber() >= tupleType->getNumElements()) {
        Out << "field index " << E->getFieldNumber()
            << " for TupleElementExpr is out of range [0,"
            << tupleType->getNumElements() << ")\n";
        abort();
      }

      checkSameType(resultType, tupleType->getElementType(E->getFieldNumber()),
                    "TupleElementExpr and the corresponding tuple element");
      verifyCheckedBase(E);
    }

    void verifyChecked(ApplyExpr *E) {
      PrettyStackTraceExpr debugStack(Ctx, "verifying ApplyExpr", E);

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
          Type InputExprObjectTy;
          if (InputExprTy->hasReferenceSemantics() ||
              InputExprTy->is<AnyMetatypeType>())
            InputExprObjectTy = InputExprTy;
          else
            InputExprObjectTy = checkLValue(InputExprTy, "object argument");
          Type FunctionInputObjectTy = checkLValue(FT->getInput(),
                                                   "'self' parameter");
          
          checkSameOrSubType(InputExprObjectTy, FunctionInputObjectTy,
                             "object argument and 'self' parameter");
        } else if (!TT || TT->getNumElements() != 1 ||
                   TT->getElement(0).getType()->getCanonicalType()
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

      if (!E->isThrowsSet()) {
        Out << "apply expression is not marked as throwing or non-throwing\n";
        E->dump(Out);
        abort();
      } else if (E->throws() && !FT->throws()) {
        Out << "apply expression is marked as throwing, but function operand"
               "does not have a throwing function type\n";
        E->dump(Out);
        abort();
      }

      if (E->isSuper() != E->getArg()->isSuperExpr()) {
        Out << "Function application's isSuper() bit mismatch.\n";
        E->dump(Out);
        abort();
      }
      verifyCheckedBase(E);
    }

    void verifyChecked(MemberRefExpr *E) {
      PrettyStackTraceExpr debugStack(Ctx, "verifying MemberRefExpr", E);

      if (!E->getMember()) {
        Out << "Member reference is missing declaration\n";
        E->dump(Out);
        abort();
      }
      
      // The base of a member reference cannot be an existential type.
      if (E->getBase()->getType()->getLValueOrInOutObjectType()
            ->isAnyExistentialType()) {
        Out << "Member reference into an unopened existential type\n";
        E->dump(Out);
        abort();
      }

      // The only time the base is allowed to be inout is if we are accessing
      // a computed property or if the base is a protocol or existential.
      if (auto *baseIOT = E->getBase()->getType()->getAs<InOutType>()) {
        if (!baseIOT->getObjectType()->is<ArchetypeType>()) {
          VarDecl *VD = dyn_cast<VarDecl>(E->getMember().getDecl());
          if (!VD || !VD->hasAccessorFunctions()) {
            Out << "member_ref_expr on value of inout type\n";
            E->dump(Out);
            abort();
          }
        }
      }
      
      // FIXME: Check container/member types through substitutions.

      verifyCheckedBase(E);
    }

    void verifyChecked(DynamicMemberRefExpr *E) {
      PrettyStackTraceExpr debugStack(Ctx, "verifying DynamicMemberRefExpr", E);

      // The base of a dynamic member reference cannot be an
      // existential type.
      if (E->getBase()->getType()->getLValueOrInOutObjectType()
            ->isAnyExistentialType()) {
        Out << "Member reference into an unopened existential type\n";
        E->dump(Out);
        abort();
      }

      verifyCheckedBase(E);
    }

    void verifyChecked(SubscriptExpr *E) {
      PrettyStackTraceExpr debugStack(Ctx, "verifying SubscriptExpr", E);

      if (!E->getDecl()) {
        Out << "Subscript expression is missing subscript declaration";
        abort();
      }

      // The base of a subscript cannot be an existential type.
      if (E->getBase()->getType()->getLValueOrInOutObjectType()
            ->isAnyExistentialType()) {
        Out << "Member reference into an unopened existential type\n";
        E->dump(Out);
        abort();
      }

      // FIXME: Check base/member types through substitutions.

      verifyCheckedBase(E);
    }

    void verifyChecked(DynamicSubscriptExpr *E) {
      PrettyStackTraceExpr debugStack(Ctx, "verifying DynamicSubscriptExpr", E);

      // The base of a subscript cannot be an existential type.
      if (E->getBase()->getType()->getLValueOrInOutObjectType()
            ->isAnyExistentialType()) {
        Out << "Member reference into an unopened existential type\n";
        E->dump(Out);
        abort();
      }

      // FIXME: Check base/member types through substitutions.

      verifyCheckedBase(E);
    }
    
    void checkOptionalObjectType(Type optionalType,
                                 Type objectType,
                                 Expr *E) {
      auto optionalRVType = optionalType->getRValueType();
      auto objectRVType = objectType->getRValueType();
      
      checkSameType(objectRVType, optionalRVType->getAnyOptionalObjectType(),
                    "optional object type");
      
      if (objectType->is<LValueType>() != optionalType->is<LValueType>()) {
        Out << "optional operation must preserve lvalue-ness of base\n";
        E->print(Out);
        abort();
      }
    }

    void verifyChecked(OptionalEvaluationExpr *E) {
      if (E->getType()->isLValueType()) {
        Out << "Optional evaluation should not produce an lvalue";
        E->print(Out);
        abort();
      }
      checkSameType(E->getType(), E->getSubExpr()->getType(),
                    "OptionalEvaluation cannot change type");
    }
    
    void verifyChecked(BindOptionalExpr *E) {
      PrettyStackTraceExpr debugStack(Ctx, "verifying BindOptionalExpr", E);

      if (E->getDepth() >= OptionalEvaluations.size()) {
        Out << "BindOptional expression is out of its depth\n";
        E->print(Out);
        abort();
      }
      
      checkOptionalObjectType(E->getSubExpr()->getType(),
                              E->getType(), E);
      
      verifyCheckedBase(E);
    }

    void verifyChecked(CheckedCastExpr *E) {
      PrettyStackTraceExpr debugStack(Ctx, "verifying CheckCastExpr", E);

      if (!E->isResolved()) {
        Out << "CheckedCast kind not resolved\n";
        abort();
      }

      verifyCheckedBase(E);
    }

    void verifyChecked(CoerceExpr *E) {
      PrettyStackTraceExpr debugStack(Ctx, "verifying CoerceExpr", E);

      checkSameType(E->getType(), E->getSubExpr()->getType(),
                    "coercion type and subexpression type");

      verifyCheckedBase(E);
    }

    void verifyChecked(IdentityExpr *E) {
      PrettyStackTraceExpr debugStack(Ctx, "verifying IdentityExpr", E);
      if (!E->getType()->isEqual(E->getSubExpr()->getType())) {
        Out << "Unexpected types in IdentityExpr\n";
        abort();
      }

      verifyCheckedBase(E);
    }

    void verifyChecked(TupleShuffleExpr *E) {
      PrettyStackTraceExpr debugStack(Ctx, "verifying TupleShuffleExpr", E);

      TupleType *TT = E->getType()->getAs<TupleType>();
      TupleType *SubTT = E->getSubExpr()->getType()->getAs<TupleType>();
      if (!TT || (!SubTT && !E->isSourceScalar())) {
        Out << "Unexpected types in TupleShuffleExpr\n";
        abort();
      }
      auto getSubElementType = [&](unsigned i) {
        if (E->isSourceScalar()) {
          assert(i == 0);
          return E->getSubExpr()->getType();
        } else {
          return SubTT->getElementType(i);
        }
      };

      unsigned varargsStartIndex = 0;
      Type varargsType;
      unsigned callerDefaultArgIndex = 0;
      for (unsigned i = 0, e = E->getElementMapping().size(); i != e; ++i) {
        int subElem = E->getElementMapping()[i];
        if (subElem == TupleShuffleExpr::DefaultInitialize)
          continue;
        if (subElem == TupleShuffleExpr::FirstVariadic) {
          varargsStartIndex = i + 1;
          varargsType = TT->getElement(i).getVarargBaseTy();
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
        if (!TT->getElementType(i)->isEqual(getSubElementType(subElem))) {
          Out << "Type mismatch in TupleShuffleExpr\n";
          abort();
        }
      }
      if (varargsStartIndex) {
        unsigned i = varargsStartIndex, e = E->getElementMapping().size();
        for (; i != e; ++i) {
          unsigned subElem = E->getElementMapping()[i];
          if (!getSubElementType(subElem)->isEqual(varargsType)) {
            Out << "Vararg type mismatch in TupleShuffleExpr\n";
            abort();
          }
        }
      }

      verifyCheckedBase(E);
    }
    
    void verifyChecked(LValueToPointerExpr *E) {
      PrettyStackTraceExpr debugStack(Ctx, "verifying LValueToPointerExpr", E);

      if (!E->getSubExpr()->getType()->is<LValueType>()) {
        Out << "LValueToPointerExpr subexpression must be an lvalue\n";
        abort();
      }
      if (!E->getType()->isEqual(
                             E->getType()->getASTContext().TheRawPointerType)) {
        Out << "LValueToPointerExpr result type must be RawPointer\n";
        abort();
      }
      
      verifyCheckedBase(E);
    }
    
    void verifyChecked(DynamicTypeExpr *E) {
      PrettyStackTraceExpr debugStack(Ctx, "verifying DynamicTypeExpr", E);

      auto metatype = E->getType()->getAs<AnyMetatypeType>();
      if (!metatype) {
        Out << "DynamicTypeExpr must have metatype type\n";
        abort();
      }

      checkSameType(E->getBase()->getType(), metatype->getInstanceType(),
                    "base type of .Type expression");
      verifyCheckedBase(E);
    }

    void verifyChecked(InjectIntoOptionalExpr *E) {
      PrettyStackTraceExpr debugStack(Ctx, "verifying InjectIntoOptionalExpr",
                                      E);

      auto valueType = E->getType()->getAnyOptionalObjectType();
      if (!valueType) {
        Out << "InjectIntoOptionalExpr is not of Optional type";
        abort();
      }

      if (!E->getSubExpr()->getType()->isEqual(valueType)) {
        Out << "InjectIntoOptionalExpr operand is not of the value type";
        abort();
      }
      verifyCheckedBase(E);
    }

    void verifyChecked(IfExpr *E) {
      PrettyStackTraceExpr debugStack(Ctx, "verifying IfExpr", E);

      auto condTy
        = E->getCondExpr()->getType()->getAs<BuiltinIntegerType>();
      if (!condTy || !condTy->isFixedWidth() || condTy->getFixedWidth() != 1) {
        Out << "IfExpr condition is not an i1\n";
        abort();
      }

      checkSameType(E->getThenExpr()->getType(),
                    E->getElseExpr()->getType(),
                    "then and else branches of an if-expr");
      verifyCheckedBase(E);
    }
    
    void verifyChecked(SuperRefExpr *expr) {
      verifyCheckedBase(expr);
    }

    void verifyChecked(TypeExpr *expr) {
      if (!expr->getType()->getAs<AnyMetatypeType>()) {
        Out << "TypeExpr must have metatype type\n";
        abort();
      }

      verifyCheckedBase(expr);
    }

    void verifyChecked(ForceValueExpr *E) {
      checkOptionalObjectType(E->getSubExpr()->getType(),
                              E->getType(), E);
      
      verifyCheckedBase(E);
    }

    void verifyChecked(OpaqueValueExpr *E) {
      PrettyStackTraceExpr debugStack(Ctx, "verifying OpaqueValueExpr", E);

      if (!OpaqueValues.count(E)) {
        Out << "OpaqueValueExpr not introduced at this point in AST\n";
        abort();
      }

      ++OpaqueValues[E];

      // Make sure "uniquely-referenced" actually is.
      if (E->isUniquelyReferenced() && OpaqueValues[E] > 1) {
        Out << "Multiple references to unique OpaqueValueExpr\n";
        abort();
      }
      verifyCheckedBase(E);
    }

    static bool hasEnclosingFunctionContext(DeclContext *dc) {
      switch (dc->getContextKind()) {
      case DeclContextKind::AbstractClosureExpr:
      case DeclContextKind::AbstractFunctionDecl:
      case DeclContextKind::SerializedLocal:
        return true;

      case DeclContextKind::TopLevelCodeDecl:
      case DeclContextKind::Module:
      case DeclContextKind::FileUnit:
        return false;

      case DeclContextKind::Initializer:
      case DeclContextKind::NominalTypeDecl:
      case DeclContextKind::ExtensionDecl:
        return hasEnclosingFunctionContext(dc->getParent());
      }
    }

    void verifyChecked(ValueDecl *VD) {
      if (!VD->hasAccessibility() && !VD->getDeclContext()->isLocalContext() &&
          !isa<GenericTypeParamDecl>(VD) && !isa<ParamDecl>(VD)) {
        dumpRef(VD);
        Out << " does not have accessibility";
        abort();
      }

      // Make sure that there are no archetypes in the interface type.
      if (VD->getDeclContext()->isTypeContext() &&
          !hasEnclosingFunctionContext(VD->getDeclContext()) &&
          !isa<ParamDecl>(VD) && /* because of subscripts */
          VD->getInterfaceType().findIf([](Type type) {
            return type->is<ArchetypeType>();
          })) {
        Out << "Interface type contains archetypes\n";
        VD->dump(Out);
        abort();
      }

      verifyCheckedBase(VD);
    }

    void verifyChecked(PatternBindingDecl *binding) {
      // Look at all of the VarDecls being bound.
      for (auto entry : binding->getPatternList())
        if (auto *P = entry.ThePattern)
          P->forEachVariable([&](VarDecl *VD) {
            // ParamDecls never get PBD's.
            assert(!isa<ParamDecl>(VD) && "ParamDecl has a PatternBindingDecl?");
          });
    }

    void verifyChecked(AbstractStorageDecl *ASD) {
      if (ASD->hasAccessibility() && ASD->isSettable(nullptr)) {
        auto setterAccess = ASD->getSetterAccessibility();
        if (ASD->getSetter() &&
            ASD->getSetter()->getFormalAccess() != setterAccess) {
          Out << "AbstractStorageDecl's setter accessibility is out of sync"
                 " with the accessibility actually on the setter";
          abort();
        }
      }
      verifyCheckedBase(ASD);
    }

    void verifyChecked(VarDecl *var) {
      PrettyStackTraceDecl debugStack("verifying VarDecl", var);

      // The fact that this is *directly* be a reference storage type
      // cuts the code down quite a bit in getTypeOfReference.
      if (var->getAttrs().hasAttribute<OwnershipAttr>() !=
          isa<ReferenceStorageType>(var->getType().getPointer())) {
        if (var->getAttrs().hasAttribute<OwnershipAttr>()) {
          Out << "VarDecl has an ownership attribute, but its type"
                 " is not a ReferenceStorageType: ";
        } else {
          Out << "VarDecl has no ownership attribute, but its type"
                 " is a ReferenceStorageType: ";
        }
        var->getType().print(Out);
        abort();
      }
      verifyCheckedBase(var);
    }

    // Dump a reference to the given declaration.
    void dumpRef(Decl *decl) {
      if (auto value = dyn_cast<ValueDecl>(decl))
        value->dumpRef(Out);
      else if (auto ext = dyn_cast<ExtensionDecl>(decl)) {
        Out << "extension of ";
        if (ext->getExtendedType())
          ext->getExtendedType().print(Out);
      }
    }

    /// Check the given list of protocols.
    void verifyProtocolList(Decl *decl, ArrayRef<ProtocolDecl *> protocols) {
      PrettyStackTraceDecl debugStack("verifying ProtocolList", decl);

      // Make sure that the protocol list is fully expanded.
      SmallVector<ProtocolDecl *, 4> nominalProtocols(protocols.begin(),
                                                      protocols.end());
      ProtocolType::canonicalizeProtocols(nominalProtocols);

      SmallVector<Type, 4> protocolTypes;
      for (auto proto : protocols)
        protocolTypes.push_back(proto->getDeclaredType());
      SmallVector<ProtocolDecl *, 4> canonicalProtocols;
      ProtocolCompositionType::get(Ctx, protocolTypes)
        ->isExistentialType(canonicalProtocols);
      if (nominalProtocols != canonicalProtocols) {
        dumpRef(decl);
        Out << " doesn't have a complete set of protocols\n";
        abort();
      }      
    }

    /// Check the given explicit protocol conformance.
    void verifyConformance(Decl *decl, ProtocolConformance *conformance) {
      PrettyStackTraceDecl debugStack("verifying protocol conformance", decl);

      if (!conformance) {
        // FIXME: Eventually, this should itself be a verification
        // failure.
        return;
      }

      switch (conformance->getState()) {
      case ProtocolConformanceState::Complete:
      case ProtocolConformanceState::Invalid:
        // More checking below.
        break;
        
      case ProtocolConformanceState::Incomplete:
        // Ignore incomplete conformances; we didn't need them.
        return;

      case ProtocolConformanceState::Checking:
        dumpRef(decl);
        Out << " has a protocol conformance that is still being checked "
            << conformance->getProtocol()->getName().str() << "\n";
        abort();
      }

      auto normal = dyn_cast<NormalProtocolConformance>(conformance);
      if (!normal)
        return;

      // Translate the owning declaration into a DeclContext.
      NominalTypeDecl *nominal = dyn_cast<NominalTypeDecl>(decl);
      DeclContext *conformingDC;
      if (nominal) {
        conformingDC = nominal;
      } else {
        auto ext = cast<ExtensionDecl>(decl);
        conformingDC = ext;
        nominal = ext->isNominalTypeOrNominalTypeExtensionContext();
      }

      auto proto = conformance->getProtocol();
      if (normal->getDeclContext() != conformingDC) {
        Out << "AST verification error: conformance of "
            << nominal->getName().str() << " to protocol "
            << proto->getName().str() << " is in the wrong context.\n"
            << "Owning context:\n";
        conformingDC->printContext(Out);
        Out << "Conformance context:\n";
        normal->getDeclContext()->printContext(Out);
        abort();
      }

      // Check that a normal protocol conformance is complete.
      for (auto member : proto->getMembers()) {
        if (auto assocType = dyn_cast<AssociatedTypeDecl>(member)) {
          if (!normal->hasTypeWitness(assocType)) {
            dumpRef(decl);
            Out << " is missing type witness for "
                << conformance->getProtocol()->getName().str() 
                << "." << assocType->getName().str()
                << "\n";
            abort();
          }

          // Make sure that the replacement type only uses archetypes allowed
          // in the context where the normal conformance exists.
          auto replacementType
            = normal->getTypeWitness(assocType, nullptr).getReplacement();
          Verifier(M, normal->getDeclContext())
            .verifyChecked(replacementType);
          continue;
        }
        
        // If this is an accessor for something, ignore it.
        if (auto *FD = dyn_cast<FuncDecl>(member))
          if (FD->isAccessor())
            continue;
        
        if (auto req = dyn_cast<ValueDecl>(member)) {
          if (!normal->hasWitness(req)) {
            dumpRef(decl);
            Out << " is missing witness for "
                << conformance->getProtocol()->getName().str() 
                << "." << req->getName().str()
                << "\n";
            abort();
          }
          continue;
        }
      }
    }
    
    void verifyChecked(NominalTypeDecl *nominal) {
      // Make sure that the protocol list is fully expanded.
      verifyProtocolList(nominal, nominal->getLocalProtocols());

      // Make sure that the protocol conformances are complete.
      for (auto conformance : nominal->getLocalConformances()) {
        verifyConformance(nominal, conformance);
      }

      verifyCheckedBase(nominal);
    }

    void verifyChecked(ExtensionDecl *ext) {
      // Make sure that the protocol list is fully expanded.
      verifyProtocolList(ext, ext->getLocalProtocols());

      // Make sure that the protocol conformances are complete.
      for (auto conformance : ext->getLocalConformances()) {
        verifyConformance(ext, conformance);
      }

      verifyCheckedBase(ext);
    }

    void verifyParsed(EnumElementDecl *UED) {
      PrettyStackTraceDecl debugStack("verifying EnumElementDecl", UED);

      if (!isa<EnumDecl>(UED->getDeclContext())) {
        Out << "EnumElementDecl has wrong DeclContext";
        abort();
      }

      verifyParsedBase(UED);
    }

    void verifyParsed(AbstractFunctionDecl *AFD) {
      PrettyStackTraceDecl debugStack("verifying AbstractFunctionDecl", AFD);
      verifyParsedBase(AFD);
    }

    void verifyParsed(ConstructorDecl *CD) {
      PrettyStackTraceDecl debugStack("verifying ConstructorDecl", CD);

      if (CD->getBodyParamPatterns().size() != 2) {
        Out << "ConstructorDecl should have exactly two parameter patterns";
        abort();
      }

      auto *DC = CD->getDeclContext();
      if (!isa<NominalTypeDecl>(DC) && !isa<ExtensionDecl>(DC) &&
          !CD->isInvalid()) {
        Out << "ConstructorDecls outside nominal types and extensions "
               "should be marked invalid";
        abort();
      }

      verifyParsedBase(CD);
    }

    void verifyChecked(ProtocolDecl *PD) {
      PrettyStackTraceDecl debugStack("verifying ProtocolDecl", PD);

      if (PD->isObjC() && !PD->requiresClass()) {
        Out << "@objc protocols should be class protocols as well";
        abort();
      }
      verifyCheckedBase(PD);
    }

    void verifyChecked(ConstructorDecl *CD) {
      PrettyStackTraceDecl debugStack("verifying ConstructorDecl", CD);

      auto *ND = CD->getExtensionType()->getNominalOrBoundGenericNominal();
      if (!isa<ClassDecl>(ND) && !isa<StructDecl>(ND) && !isa<EnumDecl>(ND) &&
          !isa<ProtocolDecl>(ND) && !CD->isInvalid()) {
        Out << "ConstructorDecls outside structs, classes or enums "
               "should be marked invalid";
        abort();
      }

      // Verify that the optionality of the result type of the
      // initializer matches the failability of the initializer.
      if (!CD->isInvalid() && 
          CD->getDeclContext()->getDeclaredInterfaceType()->getAnyNominal() 
            != Ctx.getOptionalDecl() &&
          CD->getDeclContext()->getDeclaredInterfaceType()->getAnyNominal() 
            != Ctx.getImplicitlyUnwrappedOptionalDecl()) {
        OptionalTypeKind resultOptionality = OTK_None;
        CD->getResultType()->getAnyOptionalObjectType(resultOptionality);
        if (resultOptionality != CD->getFailability()) {
          Out << "Initializer has result optionality/failability mismatch\n";
          CD->dump(llvm::errs());
          abort();
        }

        // Also check the interface type.
        if (auto genericFn 
              = CD->getInterfaceType()->getAs<GenericFunctionType>()) {
          resultOptionality = OTK_None;
          genericFn->getResult()->castTo<AnyFunctionType>()->getResult()
            ->getAnyOptionalObjectType(resultOptionality);
          if (resultOptionality != CD->getFailability()) {
            Out << "Initializer has result optionality/failability mismatch\n";
            CD->dump(llvm::errs());
            abort();
          }
        }
      }

      verifyCheckedBase(CD);
    }

    void verifyParsed(DestructorDecl *DD) {
      PrettyStackTraceDecl debugStack("verifying DestructorDecl", DD);

      if (DD->isGeneric()) {
        Out << "DestructorDecl can not be generic";
        abort();
      }
      if (DD->getBodyParamPatterns().size() != 1) {
        Out << "DestructorDecl should have 'self' parameter pattern only";
        abort();
      }

      auto *DC = DD->getDeclContext();
      if (!isa<NominalTypeDecl>(DC) && !isa<ExtensionDecl>(DC) &&
          !DD->isInvalid()) {
        Out << "DestructorDecls outside nominal types and extensions "
               "should be marked invalid";
        abort();
      }

      verifyParsedBase(DD);
    }

    bool checkAllArchetypes(const GenericParamList *generics) {
      ArrayRef<ArchetypeType*> storedArchetypes = generics->getAllArchetypes();

      SmallVector<ArchetypeType*, 16> derivedBuffer;
      ArrayRef<ArchetypeType*> derivedArchetypes =
        GenericParamList::deriveAllArchetypes(generics->getParams(),
                                              derivedBuffer);

      return (storedArchetypes == derivedArchetypes);
    }

    /// Check that the generic requirements line up with the archetypes.
    void checkGenericRequirements(Decl *decl,
                                  DeclContext *dc,
                                  GenericFunctionType *genericTy) {

      PrettyStackTraceDecl debugStack("verifying generic requirements", decl);

      // We need to have generic parameters here.
      auto genericParams = dc->getGenericParamsOfContext();
      if (!genericParams) {
        Out << "Missing generic parameters\n";
        decl->dump(Out);
        abort();
      }

      // Verify that the list of all archetypes matches what we would
      // derive from the generic params.
      if (!checkAllArchetypes(genericParams)) {
        Out << "Archetypes list in generic parameter list doesn't "
               "match what would have been derived\n";
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

          case RequirementKind::WitnessMarker:
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
                == RequirementKind::WitnessMarker) {
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
      PrettyStackTraceDecl debugStack("verifying AbstractFunctionDecl", AFD);

      // If this function is generic or is within a generic type, it should
      // have an interface type.
      if ((AFD->getGenericParams() ||
           (AFD->getDeclContext()->isTypeContext() &&
            AFD->getDeclContext()->getGenericParamsOfContext()))
          && !AFD->getInterfaceType()->is<GenericFunctionType>()) {
        Out << "Missing interface type for generic function\n";
        AFD->dump(Out);
        abort();
      }

      // If there is an interface type, it shouldn't have any unresolved
      // dependent member types.
      // FIXME: This is a general property of the type system.
      auto interfaceTy = AFD->getInterfaceType();
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

      // Throwing @objc methods must have a foreign error convention.
      if (AFD->isObjC() &&
          static_cast<bool>(AFD->getForeignErrorConvention())
            != AFD->isBodyThrowing()) {
        if (AFD->isBodyThrowing())
          Out << "@objc method throws but does not have a foreign error "
              << "convention";
        else
          Out << "@objc method has a foreign error convention but does not "
              << "throw";
        abort();
      }

      if (AFD->getForeignErrorConvention() && !AFD->isObjC()) {
        Out << "foreign error convention on non-@objc function\n";
        AFD->dump(Out);
        abort();
      }

      verifyCheckedBase(AFD);
    }

    void verifyChecked(DestructorDecl *DD) {
      PrettyStackTraceDecl debugStack("verifying DestructorDecl", DD);

      auto *ND = DD->getExtensionType()->getNominalOrBoundGenericNominal();
      if (!isa<ClassDecl>(ND) && !DD->isInvalid()) {
        Out << "DestructorDecls outside classes should be marked invalid";
        abort();
      }
      verifyCheckedBase(DD);
    }

    void verifyChecked(FuncDecl *FD) {
      PrettyStackTraceDecl debugStack("verifying FuncDecl", FD);

      if (FD->isAccessor()) {
        auto *storageDecl = FD->getAccessorStorageDecl();
        if (!storageDecl) {
          Out << "Missing storage decl\n";
          abort();
        }

        if (FD->isGetterOrSetter()) {
          if (FD->isFinal() != storageDecl->isFinal()) {
            Out << "Property and accessor do not match for 'final'\n";
            abort();
          }
          if (FD->isDynamic() != storageDecl->isDynamic()) {
            Out << "Property and accessor do not match for 'dynamic'\n";
            abort();
          }
        }

        auto storedAccessor =
          storageDecl->getAccessorFunction(FD->getAccessorKind());
        if (storedAccessor != FD) {
          Out << "storage declaration has different accessor for this kind\n";
          abort();
        }

        switch (FD->getAccessorKind()) {
        case AccessorKind::NotAccessor: llvm_unreachable("bad kind");
        case AccessorKind::IsGetter:
        case AccessorKind::IsSetter:
        case AccessorKind::IsWillSet:
        case AccessorKind::IsDidSet:
        case AccessorKind::IsMaterializeForSet:
          if (FD->getAddressorKind() != AddressorKind::NotAddressor) {
            Out << "non-addressor accessor has an addressor kind";
            abort();
          }
          break;

        case AccessorKind::IsAddressor:
        case AccessorKind::IsMutableAddressor:
          if (FD->getAddressorKind() == AddressorKind::NotAddressor) {
            Out << "addressor does not have an addressor kind";
            abort();
          }
          break;
        }
      }

      verifyCheckedBase(FD);
    }

    void verifyParsed(FuncDecl *FD) {
      PrettyStackTraceDecl debugStack("verifying FuncDecl", FD);

      unsigned MinParamPatterns = FD->getImplicitSelfDecl() ? 2 : 1;
      if (FD->getBodyParamPatterns().size() < MinParamPatterns) {
        Out << "should have at least " << MinParamPatterns
            << " parameter patterns";
        abort();
      }

      if (FD->isAccessor()) {
        unsigned NumExpectedParamPatterns = 1;
        if (FD->getImplicitSelfDecl())
          NumExpectedParamPatterns++;
        if (FD->getBodyParamPatterns().size() != NumExpectedParamPatterns) {
          Out << "accessors should not be curried";
          abort();
        }
      }

      if (auto *VD = FD->getAccessorStorageDecl()) {
        if (isa<VarDecl>(VD)
            && cast<VarDecl>(VD)->isStatic() != FD->isStatic()) {
          Out << "getter or setter static-ness must match static-ness of var";
          abort();
        }
      }

      verifyParsedBase(FD);
    }

    void verifyChecked(ClassDecl *CD) {
      PrettyStackTraceDecl debugStack("verifying ClassDecl", CD);
      
      if (!CD->hasLazyMembers()) {
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
      }
      
      if (!CD->hasDestructor()) {
        Out << "every class's 'has destructor' bit must be set";
        abort();
      }

      verifyCheckedBase(CD);
    }

    void verifyParsed(AssociatedTypeDecl *ATD) {
      PrettyStackTraceDecl debugStack("verifying AssociatedTypeDecl", ATD);

      auto *DC = ATD->getDeclContext();
      if (!isa<NominalTypeDecl>(DC) ||
          !isa<ProtocolDecl>(cast<NominalTypeDecl>(DC))) {
        Out << "AssociatedTypeDecl should only occur inside a protocol";
        abort();
      }
      verifyParsedBase(ATD);
    }

    void verifyParsed(TuplePattern *TP) {
      PrettyStackTracePattern debugStack(Ctx, "verifying TuplePattern", TP);

      if (TP->hasVararg()) {
        auto *LastPattern = TP->getElements().back().getPattern();
        if (!isa<TypedPattern>(LastPattern)) {
          Out << "a vararg subpattern of a TuplePattern should be "
                 "a TypedPattern\n";
          abort();
        }
      }
      verifyParsedBase(TP);
    }

    void verifyChecked(TuplePattern *TP) {
      PrettyStackTracePattern debugStack(Ctx, "verifying TuplePattern", TP);

      if (TP->hasVararg()) {
        auto *LastPattern = TP->getElements().back().getPattern();
        Type T = cast<TypedPattern>(LastPattern)->getType()->getCanonicalType();
        if (auto *BGT = T->getAs<BoundGenericType>()) {
          if (BGT->getDecl() == Ctx.getArrayDecl())
            return;
        }
        Out << "a vararg subpattern of a TuplePattern has wrong type";
        abort();
      }
      verifyCheckedBase(TP);
    }

    /// Look through a possible l-value type, returning true if it was
    /// an l-value.
    bool lookThroughLValue(Type &type, bool &isInOut) {
      if (LValueType *lv = type->getAs<LValueType>()) {
        Type objectType = lv->getObjectType();
        if (objectType->is<LValueType>()) {
          Out << "type is an lvalue of lvalue type: ";
          type.print(Out);
          Out << "\n";
        }
        isInOut = false;
        type = objectType;
        return true;
      }
      if (InOutType *io = type->getAs<InOutType>()) {
        Type objectType = io->getObjectType();
        if (objectType->is<InOutType>()) {
          Out << "type is an inout of inout type: ";
          type.print(Out);
          Out << "\n";
        }
        isInOut = true;
        type = objectType;
        return true;
      }
      return false;
    }

    /// The two types are required to either both be l-values or
    /// both not be l-values.  They are adjusted to not be l-values.
    /// Returns true if they are both l-values.
    bool checkSameLValueness(Type &T0, Type &T1,
                             const char *what) {
      bool Q0, Q1;
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

    Type checkLValue(Type T, const char *what) {
      LValueType *LV = T->getAs<LValueType>();
      if (LV)
        return LV->getObjectType();

      Out << "type is not an l-value in " << what << ": ";
      T.print(Out);
      Out << "\n";
      abort();
    }

    // Verification utilities.
    Type checkMetatypeType(Type type, const char *what) {
      auto metatype = type->getAs<AnyMetatypeType>();
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

      if (auto srcMetatype = srcTy->getAs<AnyMetatypeType>()) {
        if (auto destMetatype = destTy->getAs<AnyMetatypeType>()) {
          return checkTrivialSubtype(srcMetatype->getInstanceType(),
                                     destMetatype->getInstanceType(),
                                     what);
        }
        goto fail;
      }

      // If the destination is a class, walk the supertypes of the source.
      if (destTy->getClassOrBoundGenericClass()) {
        if (!destTy->isSuperclassOf(srcTy, nullptr)) {
          srcTy.print(Out);
          Out << " is not a superclass of ";
          destTy.print(Out);
          Out << " for " << what << "\n";
          abort();
        }

        return;
      }

      // FIXME: Tighten up checking for conversions to protocol types.
      if (destTy->isExistentialType())
        return;

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

    Type checkExceptionTypeExists(const char *where) {
      auto exn = Ctx.getExceptionTypeDecl();
      if (exn) return exn->getDeclaredType();

      Out << "exception type does not exist in " << where << "\n";
      abort();
    }

    void checkMangling(ValueDecl *D) {
      llvm::SmallString<32> Buf;
      llvm::raw_svector_ostream OS(Buf);
      Mangle::Mangler Mangler(OS);
      Mangler.mangleDeclName(D);
      if (OS.str().empty()) {
        Out << "Mangler gave empty string for a ValueDecl";
        abort();
      }
    }

    bool isGoodSourceRange(SourceRange SR) {
      if (SR.isInvalid())
        return false;
      (void) Ctx.SourceMgr.findBufferContainingLoc(SR.Start);
      (void) Ctx.SourceMgr.findBufferContainingLoc(SR.End);
      return true;
    }
    
    template<typename T>
    void checkSourceRangesBase(T ASTNode) {
      checkSourceRanges(cast<typename ASTNodeBase<T>::BaseTy>(ASTNode));
    }
    
    void checkSourceRanges(Expr *E) {
      PrettyStackTraceExpr debugStack(Ctx, "verifying ranges", E);

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
      PrettyStackTraceStmt debugStack(Ctx, "verifying ranges", S);

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

    void checkSourceRanges(IfConfigStmt *S) {
      checkSourceRangesBase(S);

      SourceLoc Location = S->getStartLoc();
      for (auto &Clause : S->getClauses()) {
        // Clause start, note that the first clause start location is the
        // same as that of the whole statement
        if (Location == S->getStartLoc()) {
          if (Location != Clause.Loc) {
            Out << "bad start location of IfConfigStmt first clause\n";
            S->print(Out);
            abort();
          }
        } else {
          if (!Ctx.SourceMgr.isBeforeInBuffer(Location, Clause.Loc)) {
            Out << "bad start location of IfConfigStmt clause\n";
            S->print(Out);
            abort();
          }
        }
        Location = Clause.Loc;

        // Condition if present
        Expr *Cond = Clause.Cond;
        if (Cond) {
          if (!Ctx.SourceMgr.isBeforeInBuffer(Location, Cond->getStartLoc())) {
            Out << "invalid IfConfigStmt clause condition start location\n";
            S->print(Out);
            abort();
          }
          Location = Cond->getEndLoc();
        }
        
        // Body elements
        auto StoredLoc = Location;
        for (auto &Element : Clause.Elements) {
          auto StartLocation = Element.getStartLoc();
          if (StartLocation.isInvalid()) {
            continue;
          }
          
          if (!Ctx.SourceMgr.isBeforeInBuffer(StoredLoc, StartLocation)) {
            Out << "invalid IfConfigStmt clause element start location\n";
            S->print(Out);
            abort();
          }
          
          auto EndLocation = Element.getEndLoc();
          if (EndLocation.isValid() &&
              Ctx.SourceMgr.isBeforeInBuffer(Location, EndLocation)) {
            Location = EndLocation;
          }
        }
      }

      if (Ctx.SourceMgr.isBeforeInBuffer(S->getEndLoc(), Location)) {
        Out << "invalid IfConfigStmt end location\n";
        S->print(Out);
        abort();
      }
    }
    
    void checkSourceRanges(Pattern *P) {
      PrettyStackTracePattern debugStack(Ctx, "verifying ranges", P);

      // We don't care about source ranges on implicitly-generated
      // patterns.
      if (P->isImplicit())
        return;

      if (!P->getSourceRange().isValid()) {
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
      PrettyStackTraceDecl debugStack("verifying ranges", D);

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
      if (auto *VD = dyn_cast<VarDecl>(D)) {
        if (!VD->getTypeSourceRangeForDiagnostics().isValid()) {
          Out << "invalid type source range for variable decl: ";
          D->print(Out);
          Out << "\n";
          abort();
        }
      }
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
        if (P->isImplicit())
          return;
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
      PrettyStackTraceDecl debugStack("verifying errors", D);

      if (!D->hasType())
        return;
      if (D->getType()->is<ErrorType>() && !D->isInvalid()) {
        Out << "Valid decl has error type!\n";
        D->dump(Out);
        abort();
      }
    }
  };
}

void swift::verify(SourceFile &SF) {
#if !(defined(NDEBUG) || defined(SWIFT_DISABLE_AST_VERIFIER))
  Verifier verifier(SF, &SF);
  SF.walk(verifier);
#endif
}

bool swift::shouldVerify(const Decl *D, const ASTContext &Context) {
#if !(defined(NDEBUG) || defined(SWIFT_DISABLE_AST_VERIFIER))
  unsigned ProcessCount = Context.LangOpts.ASTVerifierProcessCount;
  unsigned ProcessId = Context.LangOpts.ASTVerifierProcessId;
  if (ProcessCount == 1) {
    // No parallelism, verify all declarations.
    return true;
  }

  if (const auto *ED = dyn_cast<ExtensionDecl>(D)) {
    return shouldVerify(ED->getExtendedType()->getAnyNominal(), Context);
  }

  const auto *VD = dyn_cast<ValueDecl>(D);
  if (!VD) {
    // Verify declarations without names everywhere.
    return true;
  }

  size_t Hash = llvm::hash_value(VD->getNameStr());
  return Hash % ProcessCount == ProcessId;
#else
  return false;
#endif
}

void swift::verify(Decl *D) {
#if !(defined(NDEBUG) || defined(SWIFT_DISABLE_AST_VERIFIER))
  Verifier V = Verifier::forDecl(D);
  D->walk(V);
#endif
}

