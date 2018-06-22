//===--- TypeCheckCompilerEvaluable.cpp - Check compiler evaluability -----===//
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
// SWIFT_ENABLE_TENSORFLOW
// Checks that function bodies follow rules for compiler evaluable functions.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"

#include "llvm/Support/Debug.h"

using namespace swift;

namespace {

/// Checks that a type is compiler representable.
/// Currently a skeleton implementation that only rejects types named Float,
/// Double and String.
/// TODO(marcrasi): Fill in a real implementation.
static bool checkCompilerRepresentable(const Type &type) {
  return type.getString() != "Double" && type.getString() != "Float" &&
         type.getString() != "String";
}

/// Checks that the body of a function is compiler evaluable.
class CheckCompilerEvaluableBody : public ASTWalker {
  TypeChecker &TC;

  // The function whose body we are checking.
  const AbstractFunctionDecl *CheckingFunc;

  // Whether the body has passed the check.
  bool CompilerEvaluable = true;

public:
  CheckCompilerEvaluableBody(TypeChecker &TC,
                             const AbstractFunctionDecl *CheckingFunc)
      : TC(TC), CheckingFunc(CheckingFunc) {}

  std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
    // If this is the ignored part of a DotSyntaxBaseIgnored, then we can accept
    // it without walking it.
    if (auto *parentDotSyntaxBaseIgnored =
            dyn_cast_or_null<DotSyntaxBaseIgnoredExpr>(Parent.getAsExpr()))
      if (parentDotSyntaxBaseIgnored->getLHS() == E)
        return {false, E};

    if (!checkCompilerRepresentable(E->getType())) {
      TC.diagnose(E->getLoc(), diag::compiler_evaluable_forbidden_type,
                  E->getType())
          .highlight(E->getSourceRange());
      CompilerEvaluable = false;
      return {false, E};
    }

    switch (E->getKind()) {
    #define ALWAYS_ALLOWED(ID)                                                 \
      case ExprKind::ID:                                                       \
        return {true, E};
    #define SOMETIMES_ALLOWED(ID)                                              \
      case ExprKind::ID:                                                       \
         return checkExpr##ID(cast<ID##Expr>(E));

    ALWAYS_ALLOWED(NilLiteral)
    ALWAYS_ALLOWED(IntegerLiteral)
    ALWAYS_ALLOWED(BooleanLiteral)
    ALWAYS_ALLOWED(MagicIdentifierLiteral)
    ALWAYS_ALLOWED(DiscardAssignment)
    SOMETIMES_ALLOWED(DeclRef)
    ALWAYS_ALLOWED(Type)
    SOMETIMES_ALLOWED(OtherConstructorDeclRef)
    ALWAYS_ALLOWED(DotSyntaxBaseIgnored)
    ALWAYS_ALLOWED(MemberRef)
    ALWAYS_ALLOWED(Paren)
    ALWAYS_ALLOWED(DotSelf)
    ALWAYS_ALLOWED(Try)
    ALWAYS_ALLOWED(ForceTry)
    ALWAYS_ALLOWED(OptionalTry)
    ALWAYS_ALLOWED(Tuple)
    ALWAYS_ALLOWED(Subscript)
    ALWAYS_ALLOWED(TupleElement)
    ALWAYS_ALLOWED(CaptureList)
    ALWAYS_ALLOWED(Closure)
    ALWAYS_ALLOWED(AutoClosure)
    ALWAYS_ALLOWED(InOut)
    ALWAYS_ALLOWED(DynamicType)
    ALWAYS_ALLOWED(RebindSelfInConstructor)
    ALWAYS_ALLOWED(BindOptional)
    ALWAYS_ALLOWED(OptionalEvaluation)
    ALWAYS_ALLOWED(ForceValue)
    SOMETIMES_ALLOWED(Call)
    ALWAYS_ALLOWED(PrefixUnary)
    ALWAYS_ALLOWED(PostfixUnary)
    ALWAYS_ALLOWED(Binary)
    ALWAYS_ALLOWED(DotSyntaxCall)
    ALWAYS_ALLOWED(ConstructorRefCall)
    ALWAYS_ALLOWED(Load)
    ALWAYS_ALLOWED(TupleShuffle)
    ALWAYS_ALLOWED(InjectIntoOptional)
    ALWAYS_ALLOWED(Coerce)
    ALWAYS_ALLOWED(If)
    ALWAYS_ALLOWED(Assign)
    ALWAYS_ALLOWED(CodeCompletion)
    ALWAYS_ALLOWED(EditorPlaceholder)

    // Allow all errors and unchecked expressions so that we don't put errors
    // on top of expressions that alrady have errors.
    ALWAYS_ALLOWED(Error)
    ALWAYS_ALLOWED(UnresolvedTypeConversion)
    #define UNCHECKED_EXPR(ID, PARENT) ALWAYS_ALLOWED(ID)
    #include "swift/AST/ExprNodes.def"

    default:
      TC.diagnose(E->getStartLoc(),
                  diag::compiler_evaluable_forbidden_expression)
          .highlight(E->getSourceRange());
      CompilerEvaluable = false;
      return {false, E};

    #undef ALWAYS_ALLOWED
    #undef SOMETIMES_ALLOWED
    }
  }

  std::pair<bool, Expr *> checkExprCall(CallExpr *call) {
    // TODO(SR-8035): Eliminate this special case.
    // Allow calls to some stdlib assertion functions without walking them
    // further, because the calls do currently-forbidden things. (They use
    // Strings and they call functions imported from C).
    if (auto *calleeRef = dyn_cast<DeclRefExpr>(call->getDirectCallee()))
      if (auto *callee = dyn_cast<AbstractFunctionDecl>(calleeRef->getDecl()))
        if (callee->isChildContextOf(TC.Context.TheStdlibModule) &&
            (callee->getNameStr() == "_precondition" ||
             callee->getNameStr() == "_preconditionFailure" ||
             callee->getNameStr() == "_sanityCheck" ||
             callee->getNameStr() == "fatalError"))
          return {false, call};

    // Otherwise, walk everything in the expression.
    return {true, call};
  }

  std::pair<bool, Expr *> checkExprDeclRef(DeclRefExpr *declRef) {
    auto *decl = declRef->getDeclRef().getDecl();
    if (auto *varDecl = dyn_cast<VarDecl>(decl)) {
      // DeclRefs to immutable variables are always allowed.
      if (varDecl->isImmutable())
        return {true, declRef};

      // DeclRefs to mutable variables are only allowed if they are declared
      // within the @compilerEvaluable function.
      if (varDecl->getDeclContext() == CheckingFunc ||
          varDecl->getDeclContext()->isChildContextOf(CheckingFunc))
        return {true, declRef};

      TC.diagnose(declRef->getLoc(),
                  diag::compiler_evaluable_non_local_mutable);
      CompilerEvaluable = false;
      return {false, declRef};
    } else if (auto *functionDecl = dyn_cast<AbstractFunctionDecl>(decl)) {
      return checkAbstractFunctionDeclRef(declRef, functionDecl);
    } else if (isa<EnumElementDecl>(decl)) {
      return {true, declRef};
    } else {
      TC.diagnose(declRef->getLoc(),
                  diag::compiler_evaluable_forbidden_expression)
          .highlight(declRef->getSourceRange());
      CompilerEvaluable = false;
      return {false, declRef};
    }
  }

  std::pair<bool, Expr *>
  checkExprOtherConstructorDeclRef(OtherConstructorDeclRefExpr *declRef) {
    return checkAbstractFunctionDeclRef(declRef, declRef->getDecl());
  }

  std::pair<bool, Expr *>
  checkAbstractFunctionDeclRef(Expr *declRef, AbstractFunctionDecl *decl) {
    // If the function is @compilerEvaluable, allow it.
    if (decl->getAttrs().hasAttribute<CompilerEvaluableAttr>(
            /*AllowInvalid=*/true))
      return {true, declRef};

    // If the function is nested within the function that we are checking, allow
    // it.
    if (decl->isChildContextOf(CheckingFunc))
      return {true, declRef};

    // For now, allow all builtins.
    // TODO: Mark which builtins are actually compiler evaluable.
    if (decl->isChildContextOf(TC.Context.TheBuiltinModule))
      return {true, declRef};

    // Allow all protocol methods. Later, the interpreter looks up the actual
    // function and emits an error when it is not @compilerEvaluable.
    if (isa<ProtocolDecl>(decl->getDeclContext()))
      return {true, declRef};

    TC.diagnose(declRef->getLoc(),
                diag::compiler_evaluable_ref_non_compiler_evaluable);
    CompilerEvaluable = false;
    return {false, declRef};
  }

  std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
    if (S->getKind() == StmtKind::While) {
      TC.diagnose(S->getStartLoc(), diag::compiler_evaluable_loop);
      CompilerEvaluable = false;
      return {false, S};
    }
    return {true, S};
  }

  bool getCompilerEvaluable() const { return CompilerEvaluable; }
};

}  // namespace

/// If the function has a valid @compilerEvaluable attribute, checks that the
/// function body follows all the rules for compiler evaluable functions.
///
/// The function body must already be type checked.
void TypeChecker::checkFunctionBodyCompilerEvaluable(AbstractFunctionDecl *D) {
  auto compilerEvaluableAttr =
      D->getAttrs().getAttribute<CompilerEvaluableAttr>();
  if (!compilerEvaluableAttr || !compilerEvaluableAttr->isValid()) return;

  assert(D->getBodyKind() == AbstractFunctionDecl::BodyKind::TypeChecked &&
         "cannot check @compilerEvaluable body that is not type checked");

  CheckCompilerEvaluableBody Checker(*this, D);
  D->getBody()->walk(Checker);
  if (!Checker.getCompilerEvaluable()) {
    compilerEvaluableAttr->setInvalid();
  }
}
