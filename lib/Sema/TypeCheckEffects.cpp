//===--- TypeCheckEffects.cpp - Type Checking for Effects Coverage --------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements semantic analysis to ensure that various effects (such
// as throwing and async) are properly handled.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "TypeCheckConcurrency.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/PrettyStackTrace.h"

using namespace swift;

namespace {

/// A function reference.
class AbstractFunction {
public:
  enum Kind : uint8_t {
    Opaque, Function, Closure, Parameter,
  };

private:
  union {
    AbstractFunctionDecl *TheFunction;
    AbstractClosureExpr *TheClosure;
    ParamDecl *TheParameter;
    Expr *TheExpr;
  };
  unsigned TheKind : 2;
  unsigned IsRethrows : 1;
  unsigned ParamCount : 2;

public:
  explicit AbstractFunction(Kind kind, Expr *fn)
    : TheKind(kind),
      IsRethrows(false),
      ParamCount(1) {
    TheExpr = fn;
  }

  explicit AbstractFunction(AbstractFunctionDecl *fn)
    : TheKind(Kind::Function),
      IsRethrows(fn->getAttrs().hasAttribute<RethrowsAttr>()),
      ParamCount(fn->getNumCurryLevels()) {
    TheFunction = fn;
  }

  explicit AbstractFunction(AbstractClosureExpr *closure)
    : TheKind(Kind::Closure),
      IsRethrows(false),
      ParamCount(1) {
    TheClosure = closure;
  }

  explicit AbstractFunction(ParamDecl *parameter)
    : TheKind(Kind::Parameter),
      IsRethrows(false),
      ParamCount(1) {
    TheParameter = parameter;
  }

  Kind getKind() const { return Kind(TheKind); }

  /// Whether the function is marked 'rethrows'.
  bool isBodyRethrows() const { return IsRethrows; }

  unsigned getNumArgumentsForFullApply() const {
    return ParamCount;
  }

  Type getType() const {
    switch (getKind()) {
    case Kind::Opaque: return getOpaqueFunction()->getType();
    case Kind::Function: return getFunction()->getInterfaceType();
    case Kind::Closure: return getClosure()->getType();
    case Kind::Parameter: return getParameter()->getType();
    }
    llvm_unreachable("bad kind");
  }

  bool isAutoClosure() const {
    if (getKind() == Kind::Closure)
      return isa<AutoClosureExpr>(getClosure());
    return false;
  }

  AbstractFunctionDecl *getFunction() const {
    assert(getKind() == Kind::Function);
    return TheFunction;
  }
  AbstractClosureExpr *getClosure() const {
    assert(getKind() == Kind::Closure);
    return TheClosure;
  }
  ParamDecl *getParameter() const {
    assert(getKind() == Kind::Parameter);
    return TheParameter;
  }
  Expr *getOpaqueFunction() const {
    assert(getKind() == Kind::Opaque);
    return TheExpr;
  }

  static AbstractFunction decomposeApply(ApplyExpr *apply,
                                         SmallVectorImpl<Expr*> &args) {
    Expr *fn;
    do {
      args.push_back(apply->getArg());
      fn = apply->getFn()->getValueProvidingExpr();
    } while ((apply = dyn_cast<ApplyExpr>(fn)));

    return decomposeFunction(fn);
  }

  static AbstractFunction decomposeFunction(Expr *fn) {
    assert(fn->getValueProvidingExpr() == fn);

    while (true) {
      // Look through Optional unwraps.
      if (auto conversion = dyn_cast<ForceValueExpr>(fn)) {
        fn = conversion->getSubExpr()->getValueProvidingExpr();
      } else if (auto conversion = dyn_cast<BindOptionalExpr>(fn)) {
        fn = conversion->getSubExpr()->getValueProvidingExpr();
      // Look through optional injections.
      } else if (auto injection = dyn_cast<InjectIntoOptionalExpr>(fn)) {
        fn = injection->getSubExpr()->getValueProvidingExpr();
      // Look through function conversions.
      } else if (auto conversion = dyn_cast<FunctionConversionExpr>(fn)) {
        fn = conversion->getSubExpr()->getValueProvidingExpr();
      // Look through base-ignored qualified references (Module.methodName).
      } else if (auto baseIgnored = dyn_cast<DotSyntaxBaseIgnoredExpr>(fn)) {
        fn = baseIgnored->getRHS();
      // Look through closure capture lists.
      } else if (auto captureList = dyn_cast<CaptureListExpr>(fn)) {
        fn = captureList->getClosureBody();
        // Look through optional evaluations.
      } else if (auto optionalEval = dyn_cast<OptionalEvaluationExpr>(fn)) {
        fn = optionalEval->getSubExpr()->getValueProvidingExpr();
      } else {
        break;
      }
    }
    
    // Constructor delegation.
    if (auto otherCtorDeclRef = dyn_cast<OtherConstructorDeclRefExpr>(fn)) {
      return AbstractFunction(otherCtorDeclRef->getDecl());
    }

    // Normal function references.
    if (auto declRef = dyn_cast<DeclRefExpr>(fn)) {
      ValueDecl *decl = declRef->getDecl();
      if (auto fn = dyn_cast<AbstractFunctionDecl>(decl)) {
        return AbstractFunction(fn);
      } else if (auto param = dyn_cast<ParamDecl>(decl)) {
        return AbstractFunction(param);
      }

    // Closures.
    } else if (auto closure = dyn_cast<AbstractClosureExpr>(fn)) {
      return AbstractFunction(closure);
    }

    // Everything else is opaque.
    return AbstractFunction(Kind::Opaque, fn);
  }
};

enum ShouldRecurse_t : bool {
  ShouldNotRecurse = false, ShouldRecurse = true
};

/// A CRTP ASTWalker implementation that looks for interesting
/// nodes for effects handling.
template <class Impl>
class EffectsHandlingWalker : public ASTWalker {
  Impl &asImpl() { return *static_cast<Impl*>(this); }
public:
  bool walkToDeclPre(Decl *D) override {
    ShouldRecurse_t recurse = ShouldRecurse;
    // Skip the implementations of all local declarations... except
    // PBD.  We should really just have a PatternBindingStmt.
    if (auto ic = dyn_cast<IfConfigDecl>(D))
      recurse = asImpl().checkIfConfig(ic);
    else if (!isa<PatternBindingDecl>(D))
      recurse = ShouldNotRecurse;
    return bool(recurse);
  }

  std::pair<bool, Expr*> walkToExprPre(Expr *E) override {
    ShouldRecurse_t recurse = ShouldRecurse;
    if (isa<ErrorExpr>(E)) {
      asImpl().flagInvalidCode();
    } else if (auto closure = dyn_cast<ClosureExpr>(E)) {
      recurse = asImpl().checkClosure(closure);
    } else if (auto autoclosure = dyn_cast<AutoClosureExpr>(E)) {
      recurse = asImpl().checkAutoClosure(autoclosure);
    } else if (auto awaitExpr = dyn_cast<AwaitExpr>(E)) {
      recurse = asImpl().checkAwait(awaitExpr);
    } else if (auto tryExpr = dyn_cast<TryExpr>(E)) {
      recurse = asImpl().checkTry(tryExpr);
    } else if (auto forceTryExpr = dyn_cast<ForceTryExpr>(E)) {
      recurse = asImpl().checkForceTry(forceTryExpr);
    } else if (auto optionalTryExpr = dyn_cast<OptionalTryExpr>(E)) {
      recurse = asImpl().checkOptionalTry(optionalTryExpr);
    } else if (auto apply = dyn_cast<ApplyExpr>(E)) {
      recurse = asImpl().checkApply(apply);
    } else if (auto interpolated = dyn_cast<InterpolatedStringLiteralExpr>(E)) {
      recurse = asImpl().checkInterpolatedStringLiteral(interpolated);
    }
    // Error handling validation (via checkTopLevelEffects) happens after
    // type checking. If an unchecked expression is still around, the code was
    // invalid.
#define UNCHECKED_EXPR(KIND, BASE) \
    else if (isa<KIND##Expr>(E)) return {false, nullptr};
#include "swift/AST/ExprNodes.def"

    return {bool(recurse), E};
  }

  std::pair<bool, Stmt*> walkToStmtPre(Stmt *S) override {
    ShouldRecurse_t recurse = ShouldRecurse;
    if (auto doCatch = dyn_cast<DoCatchStmt>(S)) {
      recurse = asImpl().checkDoCatch(doCatch);
    } else if (auto thr = dyn_cast<ThrowStmt>(S)) {
      recurse = asImpl().checkThrow(thr);
    }
    return {bool(recurse), S};
  }

  ShouldRecurse_t checkDoCatch(DoCatchStmt *S) {
    auto bodyResult = (S->isSyntacticallyExhaustive()
        ? asImpl().checkExhaustiveDoBody(S)
        : asImpl().checkNonExhaustiveDoBody(S));
    for (auto clause : S->getCatches()) {
      asImpl().checkCatch(clause, bodyResult);
    }
    return ShouldNotRecurse;
  }
};

/// A potential reason why something might throw.
class PotentialThrowReason {
public:
  enum class Kind : uint8_t {
    /// The function throws unconditionally.
    Throw,

    /// The function calls an unconditionally throwing function.
    CallThrows,

    /// The function is 'rethrows', and it was passed an explicit
    /// argument that was not rethrowing-only in this context.
    CallRethrowsWithExplicitThrowingArgument,

    /// The function is 'rethrows', and it was passed a default
    /// argument that was not rethrowing-only in this context.
    CallRethrowsWithDefaultThrowingArgument,
  };

private:
  Expr *TheExpression;
  Kind TheKind;

  explicit PotentialThrowReason(Kind kind) : TheKind(kind) {}
public:
  static PotentialThrowReason forRethrowsArgument(Expr *E) {
    PotentialThrowReason result(Kind::CallRethrowsWithExplicitThrowingArgument);
    result.TheExpression = E;
    return result;
  }
  static PotentialThrowReason forDefaultArgument() {
    return PotentialThrowReason(Kind::CallRethrowsWithDefaultThrowingArgument);
  }
  static PotentialThrowReason forThrowingApply() {
    return PotentialThrowReason(Kind::CallThrows);
  }
  static PotentialThrowReason forThrow() {
    return PotentialThrowReason(Kind::Throw);
  }

  Kind getKind() const { return TheKind; }

  /// Is this a throw expression?
  bool isThrow() const { return getKind() == Kind::Throw; }
  bool isRethrowsCall() const {
    return (getKind() == Kind::CallRethrowsWithExplicitThrowingArgument ||
            getKind() == Kind::CallRethrowsWithDefaultThrowingArgument);
  }

  /// If this was built with forRethrowsArgument, return the expression.
  Expr *getThrowingArgument() const {
    assert(getKind() == Kind::CallRethrowsWithExplicitThrowingArgument);
    return TheExpression;
  }
};

enum class ThrowingKind {
  /// The call/function can't throw.
  None,

  /// The call/function can only throw if one of the parameters in
  /// the current rethrows context can throw.
  RethrowingOnly,

  /// The call/function can throw.
  Throws,
};

/// A type expressing the result of classifying whether a call or function
/// throws or is async.
class Classification {
  bool IsInvalid = false;  // The AST is malformed.  Don't diagnose.
  bool IsAsync = false;
  ThrowingKind Result = ThrowingKind::None;
  Optional<PotentialThrowReason> Reason;
  
public:
  Classification() : Result(ThrowingKind::None) {}
  explicit Classification(ThrowingKind result, PotentialThrowReason reason,
                          bool isAsync)
      : IsAsync(isAsync), Result(result) {
    if (result == ThrowingKind::Throws ||
        result == ThrowingKind::RethrowingOnly) {
      Reason = reason;
    }
  }

  /// Return a classification saying that there's an unconditional
  /// throw site.
  static Classification forThrow(PotentialThrowReason reason, bool isAsync) {
    Classification result;
    result.Result = ThrowingKind::Throws;
    result.Reason = reason;
    result.IsAsync = isAsync;
    return result;
  }

  /// Return a classification saying that there's an unconditional
  /// throw site.
  static Classification forAsync() {
    Classification result;
    result.IsAsync = true;
    return result;
  }
  
  static Classification forInvalidCode() {
    Classification result;
    result.IsInvalid = true;
    return result;
  }

  static Classification forRethrowingOnly(PotentialThrowReason reason) {
    Classification result;
    result.Result = ThrowingKind::RethrowingOnly;
    result.Reason = reason;
    return result;
  }

  void merge(Classification other) {
    if (other.getResult() > getResult())
      *this = other;
    IsAsync |= other.IsAsync;
  }

  bool isInvalid() const { return IsInvalid; }
  ThrowingKind getResult() const { return Result; }
  PotentialThrowReason getThrowsReason() const {
    assert(getResult() == ThrowingKind::Throws ||
           getResult() == ThrowingKind::RethrowingOnly);
    return *Reason;
  }
  
  bool isAsync() const { return IsAsync; }
};


/// A class for collecting information about rethrowing functions.
class ApplyClassifier {
  /// The key to this cache is the function decl or closure being analyzed.  The
  /// value in this cache is nil when the body has an error detected in it.
  llvm::DenseMap<void*, Optional<ThrowingKind>> Cache;

public:
  DeclContext *RethrowsDC = nullptr;
  bool inRethrowsContext() const { return RethrowsDC != nullptr; }

  /// Check to see if the given function application throws.
  Classification classifyApply(ApplyExpr *E) {
    // An apply expression is a potential throw site if the function throws.
    // But if the expression didn't type-check, suppress diagnostics.
    if (!E->getType() || E->getType()->hasError())
      return Classification::forInvalidCode();

    auto type = E->getFn()->getType();
    if (!type) return Classification::forInvalidCode();
    auto fnType = type->getAs<AnyFunctionType>();
    if (!fnType) return Classification::forInvalidCode();

    bool isAsync = fnType->isAsync();
    
    // If the function doesn't throw at all, we're done here.
    if (!fnType->isThrowing())
      return isAsync ? Classification::forAsync() : Classification();

    // Decompose the application.
    SmallVector<Expr*, 4> args;
    auto fnRef = AbstractFunction::decomposeApply(E, args);

    // If any of the arguments didn't type check, fail.
    for (auto arg : args) {
      if (!arg->getType() || arg->getType()->hasError())
        return Classification::forInvalidCode();
    }

    // If we're applying more arguments than the natural argument
    // count, then this is a call to the opaque value returned from
    // the function.
    if (args.size() != fnRef.getNumArgumentsForFullApply()) {
      // Special case: a reference to an operator within a type might be
      // missing 'self'.
      // FIXME: The issue here is that this is an ill-formed expression, but
      // we don't know it from the structure of the expression.
      if (args.size() == 1 && fnRef.getKind() == AbstractFunction::Function &&
          isa<FuncDecl>(fnRef.getFunction()) &&
          cast<FuncDecl>(fnRef.getFunction())->isOperator() &&
          fnRef.getNumArgumentsForFullApply() == 2 &&
          fnRef.getFunction()->getDeclContext()->isTypeContext()) {
        // Can only happen with invalid code.
        assert(fnRef.getFunction()->getASTContext().Diags.hadAnyError());
        return Classification::forInvalidCode();
      }

      assert(args.size() > fnRef.getNumArgumentsForFullApply() &&
             "partial application was throwing?");
      return Classification::forThrow(PotentialThrowReason::forThrowingApply(),
                                      isAsync);
    }

    // If the function's body is 'rethrows' for the number of
    // arguments we gave it, apply the rethrows logic.
    if (fnRef.isBodyRethrows()) {
      // We need to walk the original parameter types in parallel
      // because it only counts for 'rethrows' purposes if it lines up
      // with a throwing function parameter in the original type.
      Type type = fnRef.getType();
      if (!type) return Classification::forInvalidCode();

      // Use the most significant result from the arguments.
      Classification result;
      for (auto arg : llvm::reverse(args)) {
        auto fnType = type->getAs<AnyFunctionType>();
        if (!fnType) return Classification::forInvalidCode();

        auto paramType = FunctionType::composeInput(fnType->getASTContext(),
                                                    fnType->getParams(), false);
        result.merge(classifyRethrowsArgument(arg, paramType));
        type = fnType->getResult();
      }
      return result;
    }

    // Try to classify the implementation of functions that we have
    // local knowledge of.
    Classification result =
      classifyThrowingFunctionBody(fnRef, PotentialThrowReason::forThrowingApply());
    assert(result.getResult() != ThrowingKind::None &&
           "body classification decided function was no-throw");
    
    if (isAsync)
      result = Classification(result.getResult(), result.getThrowsReason(),
                              isAsync);
    return result;
  }

private:
  /// Classify a throwing function according to our local knowledge of
  /// its implementation.
  ///
  /// For the most part, this only distinguishes between Throws and
  /// RethrowingOnly.  But it can return Invalid if a type-checking
  /// failure prevents it from deciding that, and it can return None
  /// if the function is an autoclosure that simply doesn't throw at all.
  Classification
  classifyThrowingFunctionBody(const AbstractFunction &fn,
                               PotentialThrowReason reason) {
    // If we're not checking a 'rethrows' context, we don't need to
    // distinguish between 'throws' and 'rethrows'.  But don't even
    // trust 'throws' for autoclosures.
    if (!inRethrowsContext() && !fn.isAutoClosure())
      return Classification::forThrow(reason, /*async*/false);

    switch (fn.getKind()) {
    case AbstractFunction::Opaque:
      return Classification::forThrow(reason, /*async*/false);
    case AbstractFunction::Parameter:
      return classifyThrowingParameterBody(fn.getParameter(), reason);
    case AbstractFunction::Function:
      return classifyThrowingFunctionBody(fn.getFunction(), reason);
    case AbstractFunction::Closure:
      return classifyThrowingFunctionBody(fn.getClosure(), reason);
    }
    llvm_unreachable("bad abstract function kind");
  }

  Classification classifyThrowingParameterBody(ParamDecl *param,
                                               PotentialThrowReason reason) {
    assert(param->getType()
               ->lookThroughAllOptionalTypes()
               ->castTo<AnyFunctionType>()
               ->isThrowing());

    // If we're currently doing rethrows-checking on the body of the
    // function which declares the parameter, it's rethrowing-only.
    if (param->getDeclContext() == RethrowsDC)
      return Classification::forRethrowingOnly(reason);

    // Otherwise, it throws unconditionally.
    return Classification::forThrow(reason, /*async*/false);
  }

  bool isLocallyDefinedInRethrowsContext(DeclContext *DC) {
    while (true) {
      assert(DC->isLocalContext());
      if (DC == RethrowsDC) return true;
      DC = DC->getParent();
      if (!DC->isLocalContext()) return false;
    }
  }

  Classification classifyThrowingFunctionBody(AbstractFunctionDecl *fn,
                                              PotentialThrowReason reason) {
    // Functions can't be rethrowing-only unless they're defined
    // within the rethrows context.
    if (!isLocallyDefinedInRethrowsContext(fn) || !fn->hasBody())
      return Classification::forThrow(reason, /*async*/false);

    auto kind = classifyThrowingFunctionBodyImpl(fn, fn->getBody(),
                                                 /*allowNone*/ false);
    if (kind.hasValue())
      return Classification(kind.getValue(), reason, /*async*/false);
    return Classification::forInvalidCode();
  }

  Classification classifyThrowingFunctionBody(AbstractClosureExpr *closure,
                                              PotentialThrowReason reason) {
    bool isAutoClosure = isa<AutoClosureExpr>(closure);

    // Closures can't be rethrowing-only unless they're defined
    // within the rethrows context.
    if (!isAutoClosure && !isLocallyDefinedInRethrowsContext(closure))
      return Classification::forThrow(reason, /*async*/false);

    BraceStmt *body;
    if (auto autoclosure = dyn_cast<AutoClosureExpr>(closure)) {
      body = autoclosure->getBody();
    } else {
      body = cast<ClosureExpr>(closure)->getBody();
    }
    if (!body) return Classification::forInvalidCode();

    auto kind = classifyThrowingFunctionBodyImpl(closure, body,
                                                 /*allowNone*/ isAutoClosure);
    if (kind.hasValue())
      return Classification(kind.getValue(), reason, /*async*/false);
    return Classification::forInvalidCode();
  }

  class FunctionBodyClassifier
      : public EffectsHandlingWalker<FunctionBodyClassifier> {
    ApplyClassifier &Self;
  public:
    bool IsInvalid = false;
    ThrowingKind Result = ThrowingKind::None;
    FunctionBodyClassifier(ApplyClassifier &self) : Self(self) {}

    void flagInvalidCode() {
      IsInvalid = true;
    }

    ShouldRecurse_t checkClosure(ClosureExpr *closure) {
      return ShouldNotRecurse;
    }
    ShouldRecurse_t checkAutoClosure(AutoClosureExpr *closure) {
      return ShouldNotRecurse;
    }
    ShouldRecurse_t checkAwait(AwaitExpr *E) {
      return ShouldRecurse;
    }
    ShouldRecurse_t checkTry(TryExpr *E) {
      return ShouldRecurse;
    }
    ShouldRecurse_t checkForceTry(ForceTryExpr *E) {
      return ShouldNotRecurse;
    }
    ShouldRecurse_t checkOptionalTry(OptionalTryExpr *E) {
      return ShouldNotRecurse;
    }
    ShouldRecurse_t checkApply(ApplyExpr *E) {
      auto classification = Self.classifyApply(E);
      IsInvalid |= classification.isInvalid();
      Result = std::max(Result, classification.getResult());
      return ShouldRecurse;
    }
    ShouldRecurse_t checkThrow(ThrowStmt *E) {
      Result = ThrowingKind::Throws;
      return ShouldRecurse;
    }
    ShouldRecurse_t checkInterpolatedStringLiteral(InterpolatedStringLiteralExpr *E) {
      return ShouldRecurse;
    }

    ShouldRecurse_t checkIfConfig(IfConfigDecl *D) {
      return ShouldRecurse;
    }

    ThrowingKind checkExhaustiveDoBody(DoCatchStmt *S) {
      // All errors thrown by the do body are caught, but any errors thrown
      // by the catch bodies are bounded by the throwing kind of the do body.
      auto savedResult = Result;
      Result = ThrowingKind::None;
      S->getBody()->walk(*this);
      auto doThrowingKind = Result;
      Result = savedResult;
      return doThrowingKind;
    }

    ThrowingKind checkNonExhaustiveDoBody(DoCatchStmt *S) {
      S->getBody()->walk(*this);
      // Because catch bodies can only be executed if the do body throws an
      // error, and because the do is non-exhaustive, we can skip checking the
      // catch bodies entirely.
      return ThrowingKind::None;
    }

    void checkCatch(CaseStmt *S, ThrowingKind doThrowingKind) {
      if (doThrowingKind != ThrowingKind::None) {
        // This was an exhaustive do body, so bound our throwing kind by its
        // throwing kind.
        auto savedResult = Result;
        Result = ThrowingKind::None;
        S->getBody()->walk(*this);
        auto boundedResult = std::min(doThrowingKind, Result);
        Result = std::max(savedResult, boundedResult);
      } else {
        // We can skip the catch body, since bounding the result by None is
        // guaranteed to give back None, which leaves our Result unchanged.
      }
    }
  };

  Optional<ThrowingKind>
  classifyThrowingFunctionBodyImpl(void *key, BraceStmt *body,
                                   bool allowNone) {
    // Look for the key in the cache.
    auto existingIter = Cache.find(key);
    if (existingIter != Cache.end())
      return existingIter->second;

    // For the purposes of finding a fixed point, consider the
    // function to be rethrowing-only within its body.  Autoclosures
    // aren't recursively referenceable, so their special treatment
    // isn't a problem for this.
    Cache.insert({key, ThrowingKind::RethrowingOnly});

    // Walk the body.
    ThrowingKind result;
    {
      FunctionBodyClassifier classifier(*this);
      body->walk(classifier);
      result = classifier.Result;
      if (classifier.IsInvalid) {
        // Represent invalid code as being null.
        Cache[key] = Optional<ThrowingKind>();
        return Optional<ThrowingKind>();
      }
    }

    // The body result cannot be 'none' unless it's an autoclosure.
    if (!allowNone) {
      result = ThrowingKind::RethrowingOnly;
    }

    // Remember the result.
    Cache[key] = result;
    return result;
  }

  /// Classify an argument being passed to a rethrows function.
  Classification classifyRethrowsArgument(Expr *arg, Type paramType) {
    arg = arg->getValueProvidingExpr();

    if (isa<DefaultArgumentExpr>(arg)) {
      return classifyArgumentByType(arg->getType(),
                                    PotentialThrowReason::forDefaultArgument());
    }

    // If this argument is `nil` literal, it doesn't cause the call to throw.
    if (isa<NilLiteralExpr>(arg)) {
      if (arg->getType()->getOptionalObjectType())
        return Classification();
    }

    // Neither does 'Optional<T>.none'.
    if (auto *DSCE = dyn_cast<DotSyntaxCallExpr>(arg)) {
      if (auto *DE = dyn_cast<DeclRefExpr>(DSCE->getFn())) {
        auto &ctx = paramType->getASTContext();
        if (DE->getDecl() == ctx.getOptionalNoneDecl())
          return Classification();
      }
    }

    // If the parameter was structurally a tuple, try to look through the
    // various tuple operations.
    if (auto paramTupleType = dyn_cast<TupleType>(paramType.getPointer())) {
      if (auto tuple = dyn_cast<TupleExpr>(arg)) {
        return classifyTupleRethrowsArgument(tuple, paramTupleType);
      }

      if (paramTupleType->getNumElements() != 1) {
        // Otherwise, we're passing an opaque tuple expression, and we
        // should treat it as contributing to 'rethrows' if the original
        // parameter type included a throwing function type.
        return classifyArgumentByType(
                                    paramType,
                                    PotentialThrowReason::forRethrowsArgument(arg));
      }

      // FIXME: There's a case where we can end up with an ApplyExpr that
      // has a single-element-tuple argument type, but the argument is just
      // a ClosureExpr and not a TupleExpr.
      paramType = paramTupleType->getElementType(0);
    }

    // Otherwise, if the original parameter type was not a throwing
    // function type, it does not contribute to 'rethrows'.
    auto paramFnType = paramType->lookThroughAllOptionalTypes()->getAs<AnyFunctionType>();
    if (!paramFnType || !paramFnType->isThrowing())
      return Classification();

    PotentialThrowReason reason = PotentialThrowReason::forRethrowsArgument(arg);

    // TODO: partial applications?

    // Decompose the function reference, then consider the type
    // of the decomposed function.
    AbstractFunction fn = AbstractFunction::decomposeFunction(arg);

    // If it doesn't have function type, we must have invalid code.
    Type argType = fn.getType();
    if (!argType) return Classification::forInvalidCode();

    auto argFnType =
        argType->lookThroughAllOptionalTypes()->getAs<AnyFunctionType>();
    if (!argFnType) return Classification::forInvalidCode();

    // If it doesn't throw, this argument does not cause the call to throw.
    if (!argFnType->isThrowing())
      return Classification();

    // Otherwise, classify the function implementation.
    return classifyThrowingFunctionBody(fn, reason);
  }

  /// Classify an argument to a 'rethrows' function that's a tuple literal.
  Classification classifyTupleRethrowsArgument(TupleExpr *tuple,
                                               TupleType *paramTupleType) {
    if (paramTupleType->getNumElements() != tuple->getNumElements())
      return Classification::forInvalidCode();

    Classification result;
    for (unsigned i : indices(tuple->getElements())) {
      result.merge(classifyRethrowsArgument(tuple->getElement(i),
                                            paramTupleType->getElementType(i)));
    }
    return result;
  }

  /// Given the type of an argument, try to determine if it contains
  /// a throwing function in a way that is permitted to cause a
  /// 'rethrows' function to throw.
  static Classification classifyArgumentByType(Type paramType,
                                               PotentialThrowReason reason) {
    if (!paramType || paramType->hasError())
      return Classification::forInvalidCode();
    if (auto fnType = paramType->getAs<AnyFunctionType>()) {
      if (fnType->isThrowing()) {
        return Classification::forThrow(reason, /*async*/false);
      } else {
        return Classification();
      }
    }
    if (auto tupleType = paramType->getAs<TupleType>()) {
      Classification result;
      for (auto eltType : tupleType->getElementTypes()) {
        result.merge(classifyArgumentByType(eltType, reason));
      }
      return result;
    }

    // No other types include throwing functions for now.
    return Classification();
  }
};

/// An context in which effects might be handled.
class Context {
public:
  enum class Kind : uint8_t {
    /// A context that potentially handles errors or async calls.
    PotentiallyHandled,

    /// A default argument expression.
    DefaultArgument,

    /// The initializer for an instance variable.
    IVarInitializer,

    /// The initializer for a global variable.
    GlobalVarInitializer,

    /// The initializer for an enum element.
    EnumElementInitializer,

    /// The pattern of a catch.
    CatchPattern,

    /// The guard expression controlling a catch.
    CatchGuard,

    /// A defer body
    DeferBody
  };

private:
  static Context getContextForPatternBinding(PatternBindingDecl *pbd) {
    if (!pbd->isStatic() && pbd->getDeclContext()->isTypeContext()) {
      return Context(Kind::IVarInitializer);
    } else {
      return Context(Kind::GlobalVarInitializer);
    }
  }

  Kind TheKind;
  Optional<AnyFunctionRef> Function;
  bool HandlesErrors = false;
  bool HandlesAsync = false;

  /// Whether error-handling queries should ignore the function context, e.g.,
  /// for autoclosure and rethrows checks.
  bool ErrorHandlingIgnoresFunction = false;
  bool IsNonExhaustiveCatch = false;
  bool DiagnoseErrorOnTry = false;
  InterpolatedStringLiteralExpr *InterpolatedString = nullptr;

  explicit Context(Kind kind)
      : TheKind(kind), Function(None), HandlesErrors(false) {
    assert(TheKind != Kind::PotentiallyHandled);
  }

  explicit Context(bool handlesErrors, bool handlesAsync,
                   Optional<AnyFunctionRef> function)
    : TheKind(Kind::PotentiallyHandled), Function(function),
      HandlesErrors(handlesErrors), HandlesAsync(handlesAsync) { }

public:
  /// Whether this is a function that rethrows.
  bool isRethrows() const {
    if (!HandlesErrors)
      return false;

    if (ErrorHandlingIgnoresFunction)
      return false;

    if (!Function)
      return false;

    auto fn = Function->getAbstractFunctionDecl();
    if (!fn)
      return false;

    return fn->getAttrs().hasAttribute<RethrowsAttr>();
  }

  /// Whether this is an autoclosure.
  bool isAutoClosure() const {
    if (!Function)
      return false;

    if (ErrorHandlingIgnoresFunction)
      return false;

    auto closure = Function->getAbstractClosureExpr();
    if (!closure)
      return false;

    return isa<AutoClosureExpr>(closure);
  }

  static Context forTopLevelCode(TopLevelCodeDecl *D) {
    // Top-level code implicitly handles errors and 'async' calls.
    return Context(/*handlesErrors=*/true, /*handlesAsync=*/true, None);
  }

  static Context forFunction(AbstractFunctionDecl *D) {
    // HACK: If the decl is the synthesized getter for a 'lazy' property, then
    // treat the context as a property initializer in order to produce a better
    // diagnostic; the only code we should be diagnosing on is within the
    // initializer expression that has been transplanted from the var's pattern
    // binding decl. We don't perform the analysis on the initializer while it's
    // still a part of that PBD, as it doesn't get a solution applied there.
    if (auto *accessor = dyn_cast<AccessorDecl>(D)) {
      if (auto *var = dyn_cast<VarDecl>(accessor->getStorage())) {
        if (accessor->isGetter() && var->getAttrs().hasAttribute<LazyAttr>()) {
          auto *pbd = var->getParentPatternBinding();
          assert(pbd && "lazy var didn't have a pattern binding decl");
          return getContextForPatternBinding(pbd);
        }
      }
    }

    return Context(D->hasThrows(), D->isAsyncContext(), AnyFunctionRef(D));
  }

  static Context forDeferBody() {
    return Context(Kind::DeferBody);
  }

  static Context forInitializer(Initializer *init) {
    if (isa<DefaultArgumentInitializer>(init)) {
      return Context(Kind::DefaultArgument);
    }

    auto *binding = cast<PatternBindingInitializer>(init)->getBinding();
    assert(!binding->getDeclContext()->isLocalContext() &&
           "setting up error context for local pattern binding?");
    return getContextForPatternBinding(binding);
  }

  static Context forEnumElementInitializer(EnumElementDecl *elt) {
    return Context(Kind::EnumElementInitializer);
  }

  static Context forClosure(AbstractClosureExpr *E) {
    // Determine whether the closure has throwing function type.
    bool closureTypeThrows = true;
    bool closureTypeIsAsync = true;
    if (auto closureType = E->getType()) {
      if (auto fnType = closureType->getAs<AnyFunctionType>()) {
        closureTypeThrows = fnType->isThrowing();
        closureTypeIsAsync = fnType->isAsync();
      }
    }

    return Context(closureTypeThrows, closureTypeIsAsync, AnyFunctionRef(E));
  }

  static Context forCatchPattern(CaseStmt *S) {
    return Context(Kind::CatchPattern);
  }

  static Context forCatchGuard(CaseStmt *S) {
    return Context(Kind::CatchGuard);
  }

  static Context forPatternBinding(PatternBindingDecl *binding) {
    return getContextForPatternBinding(binding);
  }

  Context withInterpolatedString(InterpolatedStringLiteralExpr *E) const {
    Context copy = *this;
    copy.InterpolatedString = E;
    return copy;
  }

  /// Form a subcontext that handles all errors, e.g., for the body of a
  /// do-catch with exhaustive catch clauses.
  Context withHandlesErrors() const {
    Context copy = *this;
    copy.HandlesErrors = true;
    copy.ErrorHandlingIgnoresFunction = true;
    return copy;
  }

  Kind getKind() const { return TheKind; }

  bool handlesNothing() const {
    return !HandlesErrors;
  }
  bool handles(ThrowingKind errorKind) const {
    switch (errorKind) {
    case ThrowingKind::None:
      return true;

    // A call that's rethrowing-only can be handled by 'rethrows'.
    case ThrowingKind::RethrowingOnly:
      return HandlesErrors;

    // An operation that always throws can only be handled by an
    // all-handling context.
    case ThrowingKind::Throws:
      return HandlesErrors && !isRethrows();
    }
    llvm_unreachable("bad error kind");
  }

  bool handlesAsync() const {
    return HandlesAsync;
  }

  DeclContext *getRethrowsDC() const {
    if (!isRethrows())
      return nullptr;

    return Function->getAbstractFunctionDecl();
  }

  InterpolatedStringLiteralExpr * getInterpolatedString() const {
    return InterpolatedString;
  }

  void setNonExhaustiveCatch(bool value) {
    IsNonExhaustiveCatch = value;
  }

  static void diagnoseThrowInIllegalContext(DiagnosticEngine &Diags,
                                            ASTNode node,
                                            Kind kind) {
    if (auto *e = node.dyn_cast<Expr*>()) {
      if (isa<ApplyExpr>(e)) {
        Diags.diagnose(e->getLoc(), diag::throwing_call_in_illegal_context,
                       static_cast<unsigned>(kind));
        return;
      }
    }

    Diags.diagnose(node.getStartLoc(), diag::throw_in_illegal_context,
                   static_cast<unsigned>(kind));
  }

  static void maybeAddRethrowsNote(DiagnosticEngine &Diags, SourceLoc loc,
                                   const PotentialThrowReason &reason) {
    switch (reason.getKind()) {
    case PotentialThrowReason::Kind::Throw:
      llvm_unreachable("should already have been covered");
    case PotentialThrowReason::Kind::CallThrows:
      // Already fully diagnosed.
      return;
    case PotentialThrowReason::Kind::CallRethrowsWithExplicitThrowingArgument:
      Diags.diagnose(reason.getThrowingArgument()->getLoc(),
                     diag::because_rethrows_argument_throws);
      return;
    case PotentialThrowReason::Kind::CallRethrowsWithDefaultThrowingArgument:
      Diags.diagnose(loc, diag::because_rethrows_default_argument_throws);
      return;
    }
    llvm_unreachable("bad reason kind");
  }

  void diagnoseUncoveredThrowSite(ASTContext &ctx, ASTNode E,
                                  const PotentialThrowReason &reason) {
    auto &Diags = ctx.Diags;
    auto message = diag::throwing_call_without_try;
    auto loc = E.getStartLoc();
    SourceLoc insertLoc;
    SourceRange highlight;
    
    // Generate more specific messages in some cases.
    if (auto e = dyn_cast_or_null<ApplyExpr>(E.dyn_cast<Expr*>())) {
      if (isa<PrefixUnaryExpr>(e) || isa<PostfixUnaryExpr>(e) ||
          isa<BinaryExpr>(e)) {
        loc = e->getFn()->getStartLoc();
        message = diag::throwing_operator_without_try;
      }
      insertLoc = loc;
      highlight = e->getSourceRange();
      
      if (InterpolatedString &&
          e->getCalledValue() &&
          e->getCalledValue()->getBaseName() ==
          ctx.Id_appendInterpolation) {
        message = diag::throwing_interpolation_without_try;
        insertLoc = InterpolatedString->getLoc();
      }
    }
    
    Diags.diagnose(loc, message).highlight(highlight);
    maybeAddRethrowsNote(Diags, loc, reason);

    // If this is a call without expected 'try[?|!]', like this:
    //
    // func foo() throws {}
    // [let _ = ]foo()
    //
    // Let's suggest couple of alternative fix-its
    // because complete context is unavailable.
    if (reason.getKind() != PotentialThrowReason::Kind::CallThrows)
      return;

    Diags.diagnose(loc, diag::note_forgot_try)
        .fixItInsert(insertLoc, "try ");
    Diags.diagnose(loc, diag::note_error_to_optional)
        .fixItInsert(insertLoc, "try? ");
    Diags.diagnose(loc, diag::note_disable_error_propagation)
        .fixItInsert(insertLoc, "try! ");
  }

  void diagnoseThrowInLegalContext(DiagnosticEngine &Diags, ASTNode node,
                                   bool isTryCovered,
                                   const PotentialThrowReason &reason,
                                   Diag<> diagForThrow,
                                   Diag<> diagForThrowingCall,
                                   Diag<> diagForTrylessThrowingCall) {
    auto loc = node.getStartLoc();
    if (reason.isThrow()) {
      Diags.diagnose(loc, diagForThrow);
      return;
    }

    // Allow the diagnostic to fire on the 'try' if we don't have
    // anything else to say.
    if (isTryCovered && !reason.isRethrowsCall() &&
        !isRethrows() && !isAutoClosure()) {
      DiagnoseErrorOnTry = true;
      return;
    }

    if (isTryCovered) {
      Diags.diagnose(loc, diagForThrowingCall);
    } else {
      Diags.diagnose(loc, diagForTrylessThrowingCall);
    }
    maybeAddRethrowsNote(Diags, loc, reason);
  }

  void diagnoseUnhandledThrowSite(DiagnosticEngine &Diags, ASTNode E,
                                  bool isTryCovered,
                                  const PotentialThrowReason &reason) {
    switch (getKind()) {
    case Kind::PotentiallyHandled:
      if (IsNonExhaustiveCatch) {
        diagnoseThrowInLegalContext(Diags, E, isTryCovered, reason,
                                    diag::throw_in_nonexhaustive_catch,
                                    diag::throwing_call_in_nonexhaustive_catch,
                            diag::tryless_throwing_call_in_nonexhaustive_catch);
        return;
      }

      if (isAutoClosure()) {
        diagnoseThrowInLegalContext(Diags, E, isTryCovered, reason,
                                    diag::throw_in_nonthrowing_autoclosure,
                              diag::throwing_call_in_nonthrowing_autoclosure,
                      diag::tryless_throwing_call_in_nonthrowing_autoclosure);
        return;
      }

      if (isRethrows()) {
        diagnoseThrowInLegalContext(Diags, E, isTryCovered, reason,
                                    diag::throw_in_rethrows_function,
                                    diag::throwing_call_in_rethrows_function,
                            diag::tryless_throwing_call_in_rethrows_function);
        return;
      }

      diagnoseThrowInLegalContext(Diags, E, isTryCovered, reason,
                                  diag::throw_in_nonthrowing_function,
                                  diag::throwing_call_unhandled,
                                  diag::tryless_throwing_call_unhandled);
      return;

    case Kind::EnumElementInitializer:
    case Kind::GlobalVarInitializer:
    case Kind::IVarInitializer:
    case Kind::DefaultArgument:
    case Kind::CatchPattern:
    case Kind::CatchGuard:
    case Kind::DeferBody:
      diagnoseThrowInIllegalContext(Diags, E, getKind());
      return;
    }
    llvm_unreachable("bad context kind");
  }

  void diagnoseUnhandledTry(DiagnosticEngine &Diags, TryExpr *E) {
    switch (getKind()) {
    case Kind::PotentiallyHandled:
      if (DiagnoseErrorOnTry) {
        Diags.diagnose(
            E->getTryLoc(),
            IsNonExhaustiveCatch ? diag::try_unhandled_in_nonexhaustive_catch
                                 : diag::try_unhandled);
      }
      return;

    case Kind::EnumElementInitializer:
    case Kind::GlobalVarInitializer:
    case Kind::IVarInitializer:
    case Kind::DefaultArgument:
    case Kind::CatchPattern:
    case Kind::CatchGuard:
    case Kind::DeferBody:
      assert(!DiagnoseErrorOnTry);
      // Diagnosed at the call sites.
      return;
    }
    llvm_unreachable("bad context kind");
  }

  void diagnoseUncoveredAsyncSite(ASTContext &ctx, ASTNode node) {
    SourceRange highlight;

    // Generate more specific messages in some cases.
    if (auto apply = dyn_cast_or_null<ApplyExpr>(node.dyn_cast<Expr*>()))
      highlight = apply->getSourceRange();

    auto diag = diag::async_call_without_await;
    if (isAutoClosure())
      diag = diag::async_call_without_await_in_autoclosure;
    ctx.Diags.diagnose(node.getStartLoc(), diag)
        .highlight(highlight);
  }

  void diagnoseAsyncInIllegalContext(DiagnosticEngine &Diags, ASTNode node) {
    if (auto *e = node.dyn_cast<Expr*>()) {
      if (isa<ApplyExpr>(e)) {
        Diags.diagnose(e->getLoc(), diag::async_call_in_illegal_context,
                       static_cast<unsigned>(getKind()));
        return;
      }
    }

    Diags.diagnose(node.getStartLoc(), diag::await_in_illegal_context,
                   static_cast<unsigned>(getKind()));
  }

  void maybeAddAsyncNote(DiagnosticEngine &Diags) {
    if (!Function)
      return;

    auto func = dyn_cast_or_null<FuncDecl>(Function->getAbstractFunctionDecl());
    if (!func)
      return;

    addAsyncNotes(func);
  }

  void diagnoseUnhandledAsyncSite(DiagnosticEngine &Diags, ASTNode node) {
    switch (getKind()) {
    case Kind::PotentiallyHandled:
      Diags.diagnose(node.getStartLoc(), diag::async_in_nonasync_function,
                     node.isExpr(ExprKind::Await), isAutoClosure());
      maybeAddAsyncNote(Diags);
      return;

    case Kind::EnumElementInitializer:
    case Kind::GlobalVarInitializer:
    case Kind::IVarInitializer:
    case Kind::DefaultArgument:
    case Kind::CatchPattern:
    case Kind::CatchGuard:
    case Kind::DeferBody:
      diagnoseAsyncInIllegalContext(Diags, node);
      return;
    }
  }
};

/// A class to walk over a local context and validate the correctness
/// of its error coverage.
class CheckEffectsCoverage : public EffectsHandlingWalker<CheckEffectsCoverage> {
  friend class EffectsHandlingWalker<CheckEffectsCoverage>;

  ASTContext &Ctx;

  DeclContext *RethrowsDC = nullptr;
  Context CurContext;

  class ContextFlags {
  public:
    enum ContextFlag : unsigned {
      /// Is the current context considered 'try'-covered?
      IsTryCovered = 0x1,

      /// Is the current context within a 'try' expression?
      IsInTry = 0x2,

      /// Is the current context top-level in a debugger function?  This
      /// causes 'try' suppression to apply recursively within a single
      /// level of do/catch.
      IsTopLevelDebuggerFunction = 0x4,

      /// Do we have any throw site in this context?
      HasAnyThrowSite = 0x8,

      /// Do we have a throw site using 'try' in this context?
      HasTryThrowSite = 0x10,
      
      /// Are we in the context of an 'await'?
      IsAsyncCovered = 0x20,
      
      /// Do we have any calls to 'async' functions in this context?
      HasAnyAsyncSite = 0x40,
      
      /// Do we have any 'await's in this context?
      HasAnyAwait = 0x80,
    };
  private:
    unsigned Bits;
  public:
    ContextFlags() : Bits(0) {}

    void reset() { Bits = 0; }
    bool has(ContextFlag flag) const { return Bits & flag; }
    void set(ContextFlag flag) { Bits |= flag; }
    void clear(ContextFlag flag) { Bits &= ~flag; }
    void mergeFrom(ContextFlag flag, ContextFlags other) {
      Bits |= (other.Bits & flag);
    }

    void mergeFrom(ContextFlags flags, ContextFlags other) {
      Bits |= (other.Bits & flags.Bits);
    }

    // All of the flags that can be set by throw checking.
    static ContextFlags throwFlags() {
      ContextFlags result;
      result.set(IsTryCovered);
      result.set(IsInTry);
      result.set(HasAnyThrowSite);
      result.set(HasTryThrowSite);
      return result;
    }

    // All of the flags that can be set by async/await checking.
    static ContextFlags asyncAwaitFlags() {
      ContextFlags result;
      result.set(IsAsyncCovered);
      result.set(HasAnyAsyncSite);
      result.set(HasAnyAwait);
      return result;
    }
  };

  ContextFlags Flags;

  /// The maximum combined value of all throwing expressions in the current
  /// context.
  ThrowingKind MaxThrowingKind;

  void flagInvalidCode() {
    // Suppress warnings about useless try or catch.
    Flags.set(ContextFlags::HasAnyThrowSite);
    Flags.set(ContextFlags::HasTryThrowSite);
  }

  /// An RAII object for restoring all the interesting state in an
  /// error-coverage.
  class ContextScope {
  CheckEffectsCoverage &Self;
    Context OldContext;
    DeclContext *OldRethrowsDC;
    ContextFlags OldFlags;
    ThrowingKind OldMaxThrowingKind;
  public:
    ContextScope(CheckEffectsCoverage &self, Optional<Context> newContext)
      : Self(self), OldContext(self.CurContext),
        OldRethrowsDC(self.RethrowsDC),
        OldFlags(self.Flags),
        OldMaxThrowingKind(self.MaxThrowingKind) {
      if (newContext) self.CurContext = *newContext;
    }

    ContextScope(const ContextScope &) = delete;
    ContextScope &operator=(const ContextScope &) = delete;

    void enterSubFunction() {
      Self.RethrowsDC = nullptr;
    }

    void enterTry() {
      Self.Flags.set(ContextFlags::IsInTry);
      Self.Flags.set(ContextFlags::IsTryCovered);
      Self.Flags.clear(ContextFlags::HasTryThrowSite);
    }
    
    void enterAwait() {
      Self.Flags.set(ContextFlags::IsAsyncCovered);
      Self.Flags.clear(ContextFlags::HasAnyAsyncSite);
    }

    void refineLocalContext(Context newContext) {
      Self.CurContext = newContext;
    }

    void resetCoverage() {
      Self.Flags.reset();
      Self.MaxThrowingKind = ThrowingKind::None;
    }

    void resetCoverageForAutoclosureBody() {
      Self.Flags.clear(ContextFlags::IsAsyncCovered);
      Self.Flags.clear(ContextFlags::HasAnyAsyncSite);
      Self.Flags.clear(ContextFlags::HasAnyAwait);
    }

    void resetCoverageForDoCatch() {
      Self.Flags.reset();
      Self.MaxThrowingKind = ThrowingKind::None;

      // Suppress 'try' coverage checking within a single level of
      // do/catch in debugger functions.
      if (OldFlags.has(ContextFlags::IsTopLevelDebuggerFunction))
        Self.Flags.set(ContextFlags::IsTryCovered);
    }

    void preserveCoverageFromAutoclosureBody() {
      // An autoclosure body is the part of the enclosing function
      // body for the purposes of deciding whether a try contained
      // a throwing call.
      OldFlags.mergeFrom(ContextFlags::HasTryThrowSite, Self.Flags);

      // "await" doesn't work this way; the "await" needs to be part of
      // the autoclosure expression itself, and the autoclosure must be
      // 'async'.
    }

    void preserveCoverageFromNonExhaustiveCatch() {
      OldFlags.mergeFrom(ContextFlags::HasAnyThrowSite, Self.Flags);
      OldMaxThrowingKind = std::max(OldMaxThrowingKind, Self.MaxThrowingKind);
    }

    void preserveCoverageFromAwaitOperand() {
      OldFlags.mergeFrom(ContextFlags::HasAnyAwait, Self.Flags);
      OldFlags.mergeFrom(ContextFlags::throwFlags(), Self.Flags);
      OldMaxThrowingKind = std::max(OldMaxThrowingKind, Self.MaxThrowingKind);
    }

    void preserveCoverageFromTryOperand() {
      OldFlags.mergeFrom(ContextFlags::HasAnyThrowSite, Self.Flags);
      OldFlags.mergeFrom(ContextFlags::asyncAwaitFlags(), Self.Flags);
      OldMaxThrowingKind = std::max(OldMaxThrowingKind, Self.MaxThrowingKind);
    }

    void preserveCoverageFromInterpolatedString() {
      OldFlags.mergeFrom(ContextFlags::HasAnyThrowSite, Self.Flags);
      OldFlags.mergeFrom(ContextFlags::HasTryThrowSite, Self.Flags);
      OldFlags.mergeFrom(ContextFlags::HasAnyAsyncSite, Self.Flags);
      OldFlags.mergeFrom(ContextFlags::HasAnyAwait, Self.Flags);
      OldMaxThrowingKind = std::max(OldMaxThrowingKind, Self.MaxThrowingKind);
    }
    
    bool wasTopLevelDebuggerFunction() const {
      return OldFlags.has(ContextFlags::IsTopLevelDebuggerFunction);
    }

    ~ContextScope() {
      Self.CurContext = OldContext;
      Self.RethrowsDC = OldRethrowsDC;
      Self.Flags = OldFlags;
      Self.MaxThrowingKind = OldMaxThrowingKind;
    }
  };

public:
  CheckEffectsCoverage(ASTContext &ctx, Context initialContext)
    : Ctx(ctx), CurContext(initialContext),
      MaxThrowingKind(ThrowingKind::None) {

    if (auto rethrowsDC = initialContext.getRethrowsDC()) {
      RethrowsDC = rethrowsDC;
    }
  }

  /// Mark that the current context is top-level code with
  /// throw-without-try enabled.
  void setTopLevelThrowWithoutTry() {
    Flags.set(ContextFlags::IsTryCovered);
  }

  /// Mark that the current context is covered by a 'try', as
  /// appropriate for a debugger function.
  ///
  /// Top level code in the debugger is actually implicitly wrapped in
  /// a function with a do/catch block.
  void setTopLevelDebuggerFunction() {
    Flags.set(ContextFlags::IsTryCovered);
    Flags.set(ContextFlags::IsTopLevelDebuggerFunction);
  }

private:
  ShouldRecurse_t checkClosure(ClosureExpr *E) {
    ContextScope scope(*this, Context::forClosure(E));
    scope.enterSubFunction();
    scope.resetCoverage();
    E->getBody()->walk(*this);
    return ShouldNotRecurse;
  }

  ShouldRecurse_t checkAutoClosure(AutoClosureExpr *E) {
    ContextScope scope(*this, Context::forClosure(E));
    scope.enterSubFunction();
    scope.resetCoverageForAutoclosureBody();

    // Curry thunks aren't actually a call to the asynchronous function.
    // Assume that async is covered in such contexts.
    switch (E->getThunkKind()) {
    case AutoClosureExpr::Kind::DoubleCurryThunk:
    case AutoClosureExpr::Kind::SingleCurryThunk:
      Flags.set(ContextFlags::IsAsyncCovered);
      break;

    case AutoClosureExpr::Kind::None:
      break;
    }

    E->getBody()->walk(*this);

    scope.preserveCoverageFromAutoclosureBody();
    return ShouldNotRecurse;
  }

  ThrowingKind checkExhaustiveDoBody(DoCatchStmt *S) {
    // This is a context where errors are handled.
    ContextScope scope(*this, CurContext.withHandlesErrors());
    assert(!Flags.has(ContextFlags::IsInTry) && "do/catch within try?");
    scope.resetCoverageForDoCatch();

    S->getBody()->walk(*this);

    diagnoseNoThrowInDo(S, scope);

    return MaxThrowingKind;
  }

  ThrowingKind checkNonExhaustiveDoBody(DoCatchStmt *S) {
    ContextScope scope(*this, None);
    assert(!Flags.has(ContextFlags::IsInTry) && "do/catch within try?");
    scope.resetCoverageForDoCatch();

    // If the enclosing context doesn't handle anything, use a
    // specialized diagnostic about non-exhaustive catches.
    if (CurContext.handlesNothing()) {
      CurContext.setNonExhaustiveCatch(true);
    }

    S->getBody()->walk(*this);

    diagnoseNoThrowInDo(S, scope);

    scope.preserveCoverageFromNonExhaustiveCatch();
    return MaxThrowingKind;
  }

  void diagnoseNoThrowInDo(DoCatchStmt *S, ContextScope &scope) {
    // Warn if nothing threw within the body, unless this is the
    // implicit do/catch in a debugger function.
    if (!Flags.has(ContextFlags::HasAnyThrowSite) &&
        !scope.wasTopLevelDebuggerFunction()) {
      Ctx.Diags.diagnose(S->getCatches().front()->getStartLoc(),
                         diag::no_throw_in_do_with_catch);
    }
  }

  void checkCatch(CaseStmt *S, ThrowingKind doThrowingKind) {
    for (auto &LabelItem : S->getMutableCaseLabelItems()) {
      // The pattern and guard aren't allowed to throw.
      {
        ContextScope scope(*this, Context::forCatchPattern(S));
        LabelItem.getPattern()->walk(*this);
      }
      if (auto guard = LabelItem.getGuardExpr()) {
        ContextScope scope(*this, Context::forCatchGuard(S));
        guard->walk(*this);
      }
    }

    auto savedContext = CurContext;
    if (doThrowingKind != ThrowingKind::Throws &&
        CurContext.isRethrows()) {
      // If this catch clause is reachable at all, it's because a function
      // parameter throws. So let's temporarily state that the body is allowed
      // to throw.
      CurContext = CurContext.withHandlesErrors();
    }

    // The catch body just happens in the enclosing context.
    S->getBody()->walk(*this);

    CurContext = savedContext;
  }

  ShouldRecurse_t checkApply(ApplyExpr *E) {
    // An apply expression is a potential throw site if the function throws.
    // But if the expression didn't type-check, suppress diagnostics.
    ApplyClassifier classifier;
    classifier.RethrowsDC = RethrowsDC;
    auto classification = classifier.classifyApply(E);

    checkThrowAsyncSite(E, /*requiresTry*/ true, classification);

    // HACK: functions can get queued multiple times in
    // definedFunctions, so be sure to be idempotent.
    if (!E->isThrowsSet() && !classification.isInvalid()) {
      E->setThrows(classification.getResult() == ThrowingKind::RethrowingOnly ||
                   classification.getResult() == ThrowingKind::Throws);
    }

    // If current apply expression did not type-check, don't attempt
    // walking inside of it. This accounts for the fact that we don't
    // erase types without type variables to enable better code complication,
    // so DeclRefExpr(s) or ApplyExpr with DeclRefExpr as function contained
    // inside would have their types preserved, which makes classification
    // incorrect.
    auto type = E->getType();
    return !type || type->hasError() ? ShouldNotRecurse : ShouldRecurse;
  }

  ShouldRecurse_t
  checkInterpolatedStringLiteral(InterpolatedStringLiteralExpr *E) {
    ContextScope scope(*this, CurContext.withInterpolatedString(E));
    if (E->getAppendingExpr())
      E->getAppendingExpr()->walk(*this);
    scope.preserveCoverageFromInterpolatedString();
    return ShouldNotRecurse;
  }

  ShouldRecurse_t checkIfConfig(IfConfigDecl *ICD) {
    // Check the inactive regions of a #if block to disable warnings that may
    // be due to platform specific code.
    struct ConservativeThrowChecker : public ASTWalker {
      CheckEffectsCoverage &CEC;
      ConservativeThrowChecker(CheckEffectsCoverage &CEC) : CEC(CEC) {}
      
      Expr *walkToExprPost(Expr *E) override {
        if (isa<TryExpr>(E))
          CEC.Flags.set(ContextFlags::HasAnyThrowSite);
        return E;
      }
      
      Stmt *walkToStmtPost(Stmt *S) override {
        if (isa<ThrowStmt>(S))
          CEC.Flags.set(ContextFlags::HasAnyThrowSite);

        return S;
      }
    };

    for (auto &clause : ICD->getClauses()) {
      // Active clauses are handled by the normal AST walk.
      if (clause.isActive) continue;
      
      for (auto elt : clause.Elements)
        elt.walk(ConservativeThrowChecker(*this));
    }
    return ShouldRecurse;
  }

  ShouldRecurse_t checkThrow(ThrowStmt *S) {
    checkThrowAsyncSite(S, /*requiresTry*/ false,
                        Classification::forThrow(PotentialThrowReason::forThrow(),
                                                 /*async*/false));
    return ShouldRecurse;
  }

  void checkThrowAsyncSite(ASTNode E, bool requiresTry,
                           const Classification &classification) {
    MaxThrowingKind = std::max(MaxThrowingKind, classification.getResult());

    // Suppress all diagnostics when there's an un-analyzable throw site.
    if (classification.isInvalid()) {
      Flags.set(ContextFlags::HasAnyThrowSite);
      if (requiresTry) Flags.set(ContextFlags::HasTryThrowSite);
      return;
    }

    // If the call to the function is async, handle it.
    if (classification.isAsync()) {
      // Remember that we've seen an async call.
      Flags.set(ContextFlags::HasAnyAsyncSite);

      // Diagnose async calls in a context that doesn't handle async.
      if (!CurContext.handlesAsync()) {
        CurContext.diagnoseUnhandledAsyncSite(Ctx.Diags, E);
      }
      // Diagnose async calls that are outside of an await context.
      else if (!Flags.has(ContextFlags::IsAsyncCovered)) {
        CurContext.diagnoseUncoveredAsyncSite(Ctx, E);
      }
    }
    
    switch (classification.getResult()) {
    // Completely ignores sites that don't throw.
    case ThrowingKind::None:
      return;

    // For the purposes of handling and try-coverage diagnostics,
    // being rethrowing-only still makes this a throw site.
    case ThrowingKind::RethrowingOnly:
    case ThrowingKind::Throws:
      Flags.set(ContextFlags::HasAnyThrowSite);
      if (requiresTry) Flags.set(ContextFlags::HasTryThrowSite);

      // We set the throwing bit of an apply expr after performing this
      // analysis, so ensure we don't emit duplicate diagnostics for functions
      // that have been queued multiple times.
      if (auto expr = E.dyn_cast<Expr*>())
        if (auto apply = dyn_cast<ApplyExpr>(expr))
          if (apply->isThrowsSet())
            return;

      bool isTryCovered =
        (!requiresTry || Flags.has(ContextFlags::IsTryCovered));
      if (!CurContext.handles(classification.getResult())) {
        CurContext.diagnoseUnhandledThrowSite(Ctx.Diags, E, isTryCovered,
                                              classification.getThrowsReason());
      } else if (!isTryCovered) {
        CurContext.diagnoseUncoveredThrowSite(Ctx, E,
                                              classification.getThrowsReason());
      }
      return;
    }
    llvm_unreachable("bad throwing kind");
  }
  ShouldRecurse_t checkAwait(AwaitExpr *E) {

    // Walk the operand.
    ContextScope scope(*this, None);
    scope.enterAwait();
    
    E->getSubExpr()->walk(*this);

    // Warn about 'await' expressions that weren't actually needed, unless of
    // course we're in a context that could never handle an 'async'. Then, we
    // produce an error.
    if (!Flags.has(ContextFlags::HasAnyAsyncSite)) {
      if (CurContext.handlesAsync())
        Ctx.Diags.diagnose(E->getAwaitLoc(), diag::no_async_in_await);
      else
        CurContext.diagnoseUnhandledAsyncSite(Ctx.Diags, E);
    }
    
    // Inform the parent of the walk that an 'await' exists here.
    scope.preserveCoverageFromAwaitOperand();
    return ShouldNotRecurse;
  }
  
  ShouldRecurse_t checkTry(TryExpr *E) {
    // Walk the operand.
    ContextScope scope(*this, None);
    scope.enterTry();

    E->getSubExpr()->walk(*this);

    // Warn about 'try' expressions that weren't actually needed.
    if (!Flags.has(ContextFlags::HasTryThrowSite)) {
      if (!E->isImplicit())
        Ctx.Diags.diagnose(E->getTryLoc(), diag::no_throw_in_try);

    // Diagnose all the call sites within a single unhandled 'try'
    // at the same time.
    } else if (CurContext.handlesNothing()) {
      CurContext.diagnoseUnhandledTry(Ctx.Diags, E);
    }

    scope.preserveCoverageFromTryOperand();
    return ShouldNotRecurse;
  }

  ShouldRecurse_t checkForceTry(ForceTryExpr *E) {
    // Walk the operand.  'try!' handles errors.
    ContextScope scope(*this, CurContext.withHandlesErrors());
    scope.enterTry();

    E->getSubExpr()->walk(*this);

    // Warn about 'try' expressions that weren't actually needed.
    if (!Flags.has(ContextFlags::HasTryThrowSite)) {
      Ctx.Diags.diagnose(E->getLoc(), diag::no_throw_in_try);
    }
    return ShouldNotRecurse;
  }

  ShouldRecurse_t checkOptionalTry(OptionalTryExpr *E) {
    // Walk the operand.  'try?' handles errors.
    ContextScope scope(*this, CurContext.withHandlesErrors());
    scope.enterTry();

    E->getSubExpr()->walk(*this);

    // Warn about 'try' expressions that weren't actually needed.
    if (!Flags.has(ContextFlags::HasTryThrowSite)) {
      Ctx.Diags.diagnose(E->getLoc(), diag::no_throw_in_try);
    }
    return ShouldNotRecurse;
  }
};

} // end anonymous namespace

void TypeChecker::checkTopLevelEffects(TopLevelCodeDecl *code) {
  auto &ctx = code->getDeclContext()->getASTContext();
  CheckEffectsCoverage checker(ctx, Context::forTopLevelCode(code));

  // In some language modes, we allow top-level code to omit 'try' marking.
  if (ctx.LangOpts.EnableThrowWithoutTry)
    checker.setTopLevelThrowWithoutTry();

  code->getBody()->walk(checker);
}

void TypeChecker::checkFunctionEffects(AbstractFunctionDecl *fn) {
#ifndef NDEBUG
  PrettyStackTraceDecl debugStack("checking effects handling for", fn);
#endif

  auto isDeferBody = isa<FuncDecl>(fn) && cast<FuncDecl>(fn)->isDeferBody();
  auto context =
      isDeferBody ? Context::forDeferBody() : Context::forFunction(fn);
  auto &ctx = fn->getASTContext();
  CheckEffectsCoverage checker(ctx, context);

  // If this is a debugger function, suppress 'try' marking at the top level.
  if (fn->getAttrs().hasAttribute<LLDBDebuggerFunctionAttr>())
    checker.setTopLevelDebuggerFunction();

  if (auto body = fn->getBody()) {
    body->walk(checker);
  }
  if (auto ctor = dyn_cast<ConstructorDecl>(fn))
    if (auto superInit = ctor->getSuperInitCall())
      superInit->walk(checker);
}

void TypeChecker::checkInitializerEffects(Initializer *initCtx,
                                                Expr *init) {
  auto &ctx = initCtx->getASTContext();
  CheckEffectsCoverage checker(ctx, Context::forInitializer(initCtx));
  init->walk(checker);
}

/// Check the correctness of effects within the given enum
/// element's raw value expression.
///
/// The syntactic restrictions on such expressions should make it
/// impossible for errors to ever arise, but checking them anyway (1)
/// ensures correctness if those restrictions are ever loosened,
/// perhaps accidentally, and (2) allows the verifier to assert that
/// all calls have been checked.
void TypeChecker::checkEnumElementEffects(EnumElementDecl *elt, Expr *E) {
  auto &ctx = elt->getASTContext();
  CheckEffectsCoverage checker(ctx, Context::forEnumElementInitializer(elt));
  E->walk(checker);
}

void TypeChecker::checkPropertyWrapperEffects(
    PatternBindingDecl *binding, Expr *expr) {
  auto &ctx = binding->getASTContext();
  CheckEffectsCoverage checker(ctx, Context::forPatternBinding(binding));
  expr->walk(checker);
}
