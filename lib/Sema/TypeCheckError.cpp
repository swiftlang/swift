//===--- TypeCheckError.cpp - Type Checking for Error Coverage ------------===//
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
// This file implements semantic analysis to ensure that errors are
// caught.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/DiagnosticsSema.h"

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
  unsigned IsProtocolMethod : 1;
  unsigned ParamCount : 28;

public:
  explicit AbstractFunction(Kind kind, Expr *fn)
    : TheKind(kind),
      IsRethrows(false),
      IsProtocolMethod(false),
      ParamCount(1) {
    TheExpr = fn;
  }

  explicit AbstractFunction(AbstractFunctionDecl *fn,
                            bool isProtocolMethod)
    : TheKind(Kind::Function),
      IsRethrows(fn->getAttrs().hasAttribute<RethrowsAttr>()),
      IsProtocolMethod(isProtocolMethod),
      ParamCount(fn->getNaturalArgumentCount()) {
    TheFunction = fn;
  }

  explicit AbstractFunction(AbstractClosureExpr *closure)
    : TheKind(Kind::Closure),
      IsRethrows(false),
      IsProtocolMethod(false),
      ParamCount(closure->getNaturalArgumentCount()) {
    TheClosure = closure;
  }

  explicit AbstractFunction(ParamDecl *parameter)
    : TheKind(Kind::Parameter),
      IsRethrows(false),
      IsProtocolMethod(false),
      ParamCount(1) {
    TheParameter = parameter;
  }

  Kind getKind() const { return Kind(TheKind); }

  /// Whether the function is marked 'rethrows'.
  bool isBodyRethrows() const { return IsRethrows; }

  /// The uncurry level that 'rethrows' applies to.
  unsigned getNumBodyParameters() const { return ParamCount; }

  unsigned getNumArgumentsForFullApply() const {
    return (ParamCount - unsigned(IsProtocolMethod));
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

  bool isProtocolMethod() const {
    return IsProtocolMethod;
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

    // Look through function conversions.
    while (auto conversion = dyn_cast<FunctionConversionExpr>(fn)) {
      fn = conversion->getSubExpr()->getValueProvidingExpr();
    }

    // Normal function references.
    if (auto declRef = dyn_cast<DeclRefExpr>(fn)) {
      ValueDecl *decl = declRef->getDecl();
      if (auto fn = dyn_cast<AbstractFunctionDecl>(decl)) {
        return AbstractFunction(fn, false);
      } else if (auto param = dyn_cast<ParamDecl>(decl)) {
        return AbstractFunction(param);
      }

    // Archetype function references.
    } else if (auto memberRef = dyn_cast<MemberRefExpr>(fn)) {
      if (auto fn = dyn_cast<AbstractFunctionDecl>(
                                          memberRef->getMember().getDecl())) {
        return AbstractFunction(fn, true);
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
/// nodes for error handling.
template <class Impl>
class ErrorHandlingWalker : public ASTWalker {
  Impl &asImpl() { return *static_cast<Impl*>(this); }
public:
  bool walkToDeclPre(Decl *D) override {
    // Skip the implementations of all local declarations... except
    // PBD.  We should really just have a PatternBindingStmt.
    return isa<PatternBindingDecl>(D);
  }

  std::pair<bool, Expr*> walkToExprPre(Expr *E) override {
    ShouldRecurse_t recurse = ShouldRecurse;
    if (isa<ErrorExpr>(E)) {
      asImpl().flagInvalidCode();
    } else if (auto closure = dyn_cast<ClosureExpr>(E)) {
      recurse = asImpl().checkClosure(closure);
    } else if (auto autoclosure = dyn_cast<AutoClosureExpr>(E)) {
      recurse = asImpl().checkAutoClosure(autoclosure);
    } else if (auto tryExpr = dyn_cast<TryExpr>(E)) {
      recurse = asImpl().checkTry(tryExpr);
    } else if (auto forceTryExpr = dyn_cast<ForceTryExpr>(E)) {
      recurse = asImpl().checkForceTry(forceTryExpr);
    } else if (auto apply = dyn_cast<ApplyExpr>(E)) {
      recurse = asImpl().checkApply(apply);
    }
    return {bool(recurse), E};
  }

  std::pair<bool, Stmt*> walkToStmtPre(Stmt *S) override {
    ShouldRecurse_t recurse = ShouldRecurse;
    if (auto doCatch = dyn_cast<DoCatchStmt>(S)) {
      recurse = asImpl().checkDoCatch(doCatch);
    } else if (auto thr = dyn_cast<ThrowStmt>(S)) {
      recurse = asImpl().checkThrow(thr);
    } else {
      assert(!isa<CatchStmt>(S));
    }
    return {bool(recurse), S};
  }

  ShouldRecurse_t checkDoCatch(DoCatchStmt *S) {
    if (S->isSyntacticallyExhaustive()) {
      asImpl().checkExhaustiveDoBody(S);
    } else {
      asImpl().checkNonExhaustiveDoBody(S);
    }
    for (auto clause : S->getCatches()) {
      asImpl().checkCatch(clause);
    }
    return ShouldNotRecurse;
  }
};

/// A potential reason why something might throw.
class PotentialReason {
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

  explicit PotentialReason(Kind kind) : TheKind(kind) {}
public:
  static PotentialReason forRethrowsArgument(Expr *E) {
    PotentialReason result(Kind::CallRethrowsWithExplicitThrowingArgument);
    result.TheExpression = E;
    return result;
  }
  static PotentialReason forDefaultArgument() {
    return PotentialReason(Kind::CallRethrowsWithDefaultThrowingArgument);
  }
  static PotentialReason forThrowingApply() {
    return PotentialReason(Kind::CallThrows);
  }
  static PotentialReason forThrow() {
    return PotentialReason(Kind::Throw);
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

  /// The call/function contains invalid code.
  Invalid,

  /// The call/function can only throw if one of the parameters in
  /// the current rethrows context can throw.
  RethrowingOnly,

  /// The call/function can throw.
  Throws,
};

/// A type expressing the result of classifying whether an call or function
/// throws.
class Classification {
  ThrowingKind Result;
  Optional<PotentialReason> Reason;

public:
  Classification() : Result(ThrowingKind::None) {}
  explicit Classification(ThrowingKind result, PotentialReason reason)
      : Result(result) {
    if (result == ThrowingKind::Throws ||
        result == ThrowingKind::RethrowingOnly) {
      Reason = reason;
    }
  }

  /// Return a classification saying that there's an unconditional
  /// throw site.
  static Classification forThrow(PotentialReason reason) {
    Classification result;
    result.Result = ThrowingKind::Throws;
    result.Reason = reason;
    return result;
  }

  static Classification forInvalidCode() {
    Classification result;
    result.Result = ThrowingKind::Invalid;
    return result;
  }

  static Classification forRethrowingOnly(PotentialReason reason) {
    Classification result;
    result.Result = ThrowingKind::RethrowingOnly;
    result.Reason = reason;
    return result;
  }

  void merge(Classification other) {
    if (other.getResult() > getResult()) {
      *this = other;
    }
  }

  ThrowingKind getResult() const { return Result; }
  PotentialReason getThrowsReason() const {
    assert(getResult() == ThrowingKind::Throws ||
           getResult() == ThrowingKind::RethrowingOnly);
    return *Reason;
  }
};

/// Given the type of a function, classify whether calling it with the
/// given number of arguments would throw.
static ThrowingKind
classifyFunctionByType(Type type, unsigned numArgs) {
  if (!type) return ThrowingKind::Invalid;

  assert(numArgs > 0);
  while (true) {
    auto fnType = type->getAs<AnyFunctionType>();
    if (!fnType) return ThrowingKind::Invalid;
    if (--numArgs == 0) {
      return fnType->getExtInfo().throws()
        ? ThrowingKind::Throws : ThrowingKind::None;
    }

    type = fnType->getResult();
  }
}

template <class T>
static ThrowingKind classifyFunctionBodyWithoutContext(T *fn) {
  return classifyFunctionByType(fn->getType(), fn->getNaturalArgumentCount());
}

/// A class for collecting information about rethrowing functions.
class ApplyClassifier {
  llvm::DenseMap<void*, ThrowingKind> Cache;

public:
  DeclContext *RethrowsDC = nullptr;
  bool inRethrowsContext() const { return RethrowsDC != nullptr; }

  /// Check to see if the given function application throws.
  Classification classifyApply(ApplyExpr *E) {
    // An apply expression is a potential throw site if the function throws.
    // But if the expression didn't type-check, suppress diagnostics.
    if (!E->getType() || E->getType()->is<ErrorType>())
      return Classification::forInvalidCode();
    auto type = E->getFn()->getType();
    if (!type) return Classification::forInvalidCode();
    auto fnType = type->getAs<AnyFunctionType>();
    if (!fnType) return Classification::forInvalidCode();

    // If the function doesn't throw at all, we're done here.
    if (!fnType->throws()) return Classification();

    // Decompose the application.
    SmallVector<Expr*, 4> args;
    auto fnRef = AbstractFunction::decomposeApply(E, args);

    // If we're applying more arguments than the natural argument
    // count, then this is a call to the opaque value returned from
    // the function.
    if (args.size() != fnRef.getNumArgumentsForFullApply()) {
      assert(args.size() > fnRef.getNumArgumentsForFullApply() &&
             "partial application was throwing?");
      return Classification::forThrow(PotentialReason::forThrowingApply());
    }

    // If the function's body is 'rethrows' for the number of
    // arguments we gave it, apply the rethrows logic.
    if (fnRef.isBodyRethrows()) {
      // We need to walk the original parameter types in parallel
      // because it only counts for 'rethrows' purposes if it lines up
      // with a throwing function parameter in the original type.
      Type type = fnRef.getType();
      if (!type) return Classification::forInvalidCode();

      if (fnRef.isProtocolMethod()) {
        if (auto fnType = type->getAs<AnyFunctionType>()) {
          type = fnType->getResult();
        } else {
          Classification::forInvalidCode();
        }
      }

      // Use the most significant result from the arguments.
      Classification result;
      for (auto arg : reversed(args)) {
        auto fnType = type->getAs<AnyFunctionType>();
        if (!fnType) return Classification::forInvalidCode();

        result.merge(classifyRethrowsArgument(arg, fnType->getInput()));
        type = fnType->getResult();
      }
      return result;
    }

    // Try to classify the implementation of functions that we have
    // local knowledge of.
    Classification result =
      classifyThrowingFunctionBody(fnRef, PotentialReason::forThrowingApply());
    assert(result.getResult() != ThrowingKind::None &&
           "body classification decided function was no-throw");
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
                               PotentialReason reason) {
    // If we're not checking a 'rethrows' context, we don't need to
    // distinguish between 'throws' and 'rethrows'.  But don't even
    // trust 'throws' for autoclosures.
    if (!inRethrowsContext() && !fn.isAutoClosure())
      return Classification::forThrow(reason);

    switch (fn.getKind()) {
    case AbstractFunction::Opaque:
      return Classification::forThrow(reason);
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
                                               PotentialReason reason) {
    assert(param->getType()->castTo<AnyFunctionType>()->throws());

    // If we're currently doing rethrows-checking on the body of the
    // function which declares the parameter, it's rethrowing-only.
    if (param->getDeclContext() == RethrowsDC)
      return Classification::forRethrowingOnly(reason);

    // Otherwise, it throws unconditionally.
    return Classification::forThrow(reason);
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
                                              PotentialReason reason) {
    // Functions can't be rethrowing-only unless they're defined
    // within the rethrows context.
    if (!isLocallyDefinedInRethrowsContext(fn) || !fn->hasBody())
      return Classification::forThrow(reason);

    auto kind = classifyThrowingFunctionBodyImpl(fn, fn->getBody(),
                                                 /*allowNone*/ false);
    return Classification(kind, reason);
  }

  Classification classifyThrowingFunctionBody(AbstractClosureExpr *closure,
                                              PotentialReason reason) {
    bool isAutoClosure = isa<AutoClosureExpr>(closure);

    // Closures can't be rethrowing-only unless they're defined
    // within the rethrows context.
    if (!isAutoClosure && !isLocallyDefinedInRethrowsContext(closure))
      return Classification::forThrow(reason);

    BraceStmt *body;
    if (auto autoclosure = dyn_cast<AutoClosureExpr>(closure)) {
      body = autoclosure->getBody();
    } else {
      body = cast<ClosureExpr>(closure)->getBody();
    }
    if (!body) return Classification::forInvalidCode();

    auto kind = classifyThrowingFunctionBodyImpl(closure, body,
                                                 /*allowNone*/ isAutoClosure);
    return Classification(kind, reason);
  }

  class FunctionBodyClassifier
      : public ErrorHandlingWalker<FunctionBodyClassifier> {
    ApplyClassifier &Self;
  public:
    ThrowingKind Result = ThrowingKind::None;
    FunctionBodyClassifier(ApplyClassifier &self) : Self(self) {}

    void flagInvalidCode() {
      Result = std::max(Result, ThrowingKind::Invalid);
    }

    ShouldRecurse_t checkClosure(ClosureExpr *closure) {
      return ShouldNotRecurse;
    }
    ShouldRecurse_t checkAutoClosure(AutoClosureExpr *closure) {
      return ShouldNotRecurse;
    }
    ShouldRecurse_t checkTry(TryExpr *E) {
      return ShouldRecurse;
    }
    ShouldRecurse_t checkForceTry(ForceTryExpr *E) {
      return ShouldNotRecurse;
    }
    ShouldRecurse_t checkApply(ApplyExpr *E) {
      Result = std::max(Result, Self.classifyApply(E).getResult());
      return ShouldRecurse;
    }
    ShouldRecurse_t checkThrow(ThrowStmt *E) {
      Result = ThrowingKind::Throws;
      return ShouldRecurse;
    }
    void checkExhaustiveDoBody(DoCatchStmt *S) {}
    void checkNonExhaustiveDoBody(DoCatchStmt *S) {
      S->getBody()->walk(*this);
    }
    void checkCatch(CatchStmt *S) {
      S->getBody()->walk(*this);
    }
  };

  ThrowingKind classifyThrowingFunctionBodyImpl(void *key, BraceStmt *body,
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
    ThrowingKind result; {
      FunctionBodyClassifier classifier(*this);
      body->walk(classifier);
      result = classifier.Result;
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

    // If the parameter was a tuple, try to look through the various
    // tuple operations.
    if (auto paramTupleType = paramType->getAs<TupleType>()) {
      if (auto tuple = dyn_cast<TupleExpr>(arg)) {
        return classifyTupleRethrowsArgument(tuple, paramTupleType);
      } else if (auto shuffle = dyn_cast<TupleShuffleExpr>(arg)) {
        return classifyShuffleRethrowsArgument(shuffle, paramTupleType);
      }

      // Otherwise, we're passing an opaque tuple expression, and we
      // should treat it as contributing to 'rethrows' if the original
      // parameter type included a throwing function type.
      return classifyArgumentByType(paramType,
                                    PotentialReason::forRethrowsArgument(arg));
    }

    // Otherwise, if the original parameter type was not a throwing
    // function type, it does not contribute to 'rethrows'.
    auto paramFnType = paramType->getAs<AnyFunctionType>();
    if (!paramFnType || !paramFnType->throws())
      return Classification();

    PotentialReason reason = PotentialReason::forRethrowsArgument(arg);

    // TODO: partial applications?

    // Decompose the function reference, then consider the type
    // of the decomposed function.
    AbstractFunction fn = AbstractFunction::decomposeFunction(arg);

    // If it doesn't have function type, we must have invalid code.
    Type argType = fn.getType();
    auto argFnType = (argType ? argType->getAs<AnyFunctionType>() : nullptr);
    if (!argFnType) return Classification::forInvalidCode();

    // If it doesn't throw, this argument does not cause the call to throw.
    if (!argFnType->throws()) return Classification();

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

  /// Classify an argument to a 'rethrows' function that's a tuple shuffle.
  Classification classifyShuffleRethrowsArgument(TupleShuffleExpr *shuffle,
                                                 TupleType *paramTupleType) {
    auto reversedParamType =
      reverseShuffleParamType(shuffle, paramTupleType);

    // Classify the operand.
    auto result = classifyRethrowsArgument(shuffle->getSubExpr(),
                                           reversedParamType);

    // Check for default arguments in the shuffle.
    for (auto i : indices(shuffle->getElementMapping())) {
      // If this element comes from the sub-expression, we've already
      // analyzed it.  (Variadic arguments also end up here, which is
      // correct for our purposes.)
      auto elt = shuffle->getElementMapping()[i];
      if (elt >= 0) {
        // Ignore.

      // Otherwise, it might come from a default argument.  It still
      // might contribute to 'rethrows', but treat it as an opaque source.
      } else if (elt == TupleShuffleExpr::DefaultInitialize ||
                 elt == TupleShuffleExpr::CallerDefaultInitialize) {
        result.merge(classifyArgumentByType(paramTupleType->getElementType(i),
                                       PotentialReason::forDefaultArgument()));
      }
    }

    return result;
  }

  /// Given a tuple shuffle and an original parameter type, construct
  /// the type of the source of the tuple shuffle preserving as much
  /// information as possible from the original parameter type.
  Type reverseShuffleParamType(TupleShuffleExpr *shuffle,
                               TupleType *origParamTupleType) {
    SmallVector<TupleTypeElt, 4> origSrcElts;
    if (shuffle->isSourceScalar()) {
      origSrcElts.append(1, TupleTypeElt());
    } else {
      auto srcTupleType = shuffle->getSubExpr()->getType()->castTo<TupleType>();
      origSrcElts.append(srcTupleType->getNumElements(), TupleTypeElt());
    }

    auto mapping = shuffle->getElementMapping();
    for (unsigned destIndex = 0; destIndex != mapping.size(); ++destIndex) {
      auto srcIndex = shuffle->getElementMapping()[destIndex];
      if (srcIndex >= 0) {
        origSrcElts[srcIndex] = origParamTupleType->getElement(destIndex);
      } else if (srcIndex == TupleShuffleExpr::DefaultInitialize ||
                 srcIndex == TupleShuffleExpr::CallerDefaultInitialize) {
        // Nothing interesting from the source expression.
      } else if (srcIndex == TupleShuffleExpr::FirstVariadic) {
        // Variadic arguments never contribute to 'rethrows'.
        // Assign the rest of the source elements parameter types that will
        // cause the recursive walker to ignore them.
        for (++destIndex; destIndex != mapping.size(); ++destIndex) {
          srcIndex = shuffle->getElementMapping()[destIndex];
          assert(srcIndex >= 0 && "default-initialized variadic argument?");
          origSrcElts[srcIndex] =
            origParamTupleType->getASTContext().TheRawPointerType;
        }

        // We're done iterating these elements.
        break;
      } else {
        llvm_unreachable("bad source-element mapping!");
      }
    }

    if (shuffle->isSourceScalar()) {
      return origSrcElts[0].getType();
    } else {
      return TupleType::get(origSrcElts, origParamTupleType->getASTContext());
    }
  }

  /// Given the type of an argument, try to determine if it contains
  /// a throwing function in a way that is permitted to cause a
  /// 'rethrows' function to throw.
  static Classification classifyArgumentByType(Type paramType,
                                               PotentialReason reason) {
    if (!paramType || paramType->is<ErrorType>())
      return Classification::forInvalidCode();
    if (auto fnType = paramType->getAs<AnyFunctionType>()) {
      if (fnType->throws()) {
        return Classification::forThrow(reason);
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

/// An error-handling context.
class Context {
public:
  enum class Kind : uint8_t {
    /// A context that handles errors.
    Handled,

    /// A non-throwing function.
    NonThrowingFunction,

    /// A rethrowing function.
    RethrowingFunction,

    /// A non-throwing autoclosure.
    NonThrowingAutoClosure,

    /// A non-exhaustive catch within a non-throwing function.
    NonExhaustiveCatch,

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

    /// The pattern of a catch.
    CatchGuard,
  };

private:
  template <class T>
  static Kind getKindForFunctionBody(T *fn) {
    switch (classifyFunctionBodyWithoutContext(fn)) {
    case ThrowingKind::None:
      return Kind::NonThrowingFunction;
    case ThrowingKind::Invalid:
    case ThrowingKind::RethrowingOnly:
    case ThrowingKind::Throws:
      return Kind::Handled;
    }
    llvm_unreachable("invalid classify result");
  }

  Kind TheKind;
  bool DiagnoseErrorOnTry = false;
  DeclContext *RethrowsDC = nullptr;

  explicit Context(Kind kind) : TheKind(kind) {}

public:
  static Context getHandled() {
    return Context(Kind::Handled);
  }

  static Context forTopLevelCode(TopLevelCodeDecl *D) {
    // Top-level code implicitly handles errors.
    return Context(Kind::Handled);
  }

  static Context forFunction(AbstractFunctionDecl *D) {
    if (D->getAttrs().hasAttribute<RethrowsAttr>()) {
      Context result(Kind::RethrowingFunction);
      result.RethrowsDC = D;
      return result;
    }
    return Context(getKindForFunctionBody(D));
  }

  static Context forInitializer(Initializer *init) {
    if (isa<DefaultArgumentInitializer>(init)) {
      return Context(Kind::DefaultArgument);
    }

    auto binding = cast<PatternBindingInitializer>(init)->getBinding();
    assert(!binding->getDeclContext()->isLocalContext() &&
           "setting up error context for local pattern binding?");
    if (!binding->isStatic() && binding->getDeclContext()->isTypeContext()) {
      return Context(Kind::IVarInitializer);
    } else {
      return Context(Kind::GlobalVarInitializer);
    }
  }

  static Context forEnumElementInitializer(EnumElementDecl *elt) {
    return Context(Kind::EnumElementInitializer);
  }

  static Context forClosure(AbstractClosureExpr *E) {
    auto kind = getKindForFunctionBody(E);
    if (kind != Kind::Handled && isa<AutoClosureExpr>(E))
      kind = Kind::NonThrowingAutoClosure;
    return Context(kind);
  }

  static Context forNonExhaustiveCatch(DoCatchStmt *S) {
    return Context(Kind::NonExhaustiveCatch);
  }

  static Context forCatchPattern(CatchStmt *S) {
    return Context(Kind::CatchPattern);
  }

  static Context forCatchGuard(CatchStmt *S) {
    return Context(Kind::CatchGuard);
  }

  Kind getKind() const { return TheKind; }

  bool handlesNothing() const {
    return getKind() != Kind::Handled &&
           getKind() != Kind::RethrowingFunction;
  }
  bool handles(ThrowingKind errorKind) const {
    switch (errorKind) {
    case ThrowingKind::None:
    case ThrowingKind::Invalid:
      return true;

    // A call that's rethrowing-only can be handled by 'rethrows'.
    case ThrowingKind::RethrowingOnly:
      return !handlesNothing();

    // An operation that always throws can only be handled by an
    // all-handling context.
    case ThrowingKind::Throws:
      return getKind() == Kind::Handled;
    }
    llvm_unreachable("bad error kind");
  }

  DeclContext *getRethrowsDC() const { return RethrowsDC; }

  static void diagnoseThrowInIllegalContext(TypeChecker &TC, ASTNode node,
                                            StringRef description) {
    if (auto *e = node.dyn_cast<Expr*>())
      if (isa<ApplyExpr>(e)) {
        TC.diagnose(e->getLoc(), diag::throwing_call_in_illegal_context,
                    description);
        return;
      }
    TC.diagnose(node.getStartLoc(), diag::throw_in_illegal_context,
                description);
  }

  static void maybeAddRethrowsNote(TypeChecker &TC, SourceLoc loc,
                                   const PotentialReason &reason) {
    switch (reason.getKind()) {
    case PotentialReason::Kind::Throw:
      llvm_unreachable("should already have been covered");
    case PotentialReason::Kind::CallThrows:
      // Already fully diagnosed.
      return;
    case PotentialReason::Kind::CallRethrowsWithExplicitThrowingArgument:
      TC.diagnose(reason.getThrowingArgument()->getLoc(),
                  diag::because_rethrows_argument_throws);
      return;
    case PotentialReason::Kind::CallRethrowsWithDefaultThrowingArgument:
      TC.diagnose(loc, diag::because_rethrows_default_argument_throws);
      return;
    }
    llvm_unreachable("bad reason kind");
  }

  void diagnoseUncoveredThrowSite(TypeChecker &TC, ASTNode E,
                                  const PotentialReason &reason) {
    auto message = diag::throwing_call_without_try;
    auto loc = E.getStartLoc();
    SourceRange highlight;
    
    // Generate more specific messages in some cases.
    if (auto e = dyn_cast_or_null<ApplyExpr>(E.dyn_cast<Expr*>())) {
      if (isa<PrefixUnaryExpr>(e) || isa<PostfixUnaryExpr>(e) ||
          isa<BinaryExpr>(e)) {
        loc = e->getFn()->getStartLoc();
        message = diag::throwing_operator_without_try;
      }
      highlight = e->getSourceRange();
    }
    
    TC.diagnose(loc, message).highlight(highlight);
    maybeAddRethrowsNote(TC, loc, reason);
  }

  void diagnoseThrowInLegalContext(TypeChecker &TC, ASTNode node,
                                   bool isTryCovered,
                                   const PotentialReason &reason,
                                   Diag<> diagForThrow,
                                   Diag<> diagForThrowingCall,
                                   Diag<> diagForTrylessThrowingCall) {
    auto loc = node.getStartLoc();
    if (reason.isThrow()) {
      TC.diagnose(loc, diagForThrow);
      return;
    }

    // Allow the diagnostic to fire on the 'try' if we don't have
    // anything else to say.
    if (isTryCovered && !reason.isRethrowsCall() &&
        (getKind() == Kind::NonThrowingFunction ||
         getKind() == Kind::NonExhaustiveCatch)) {
      DiagnoseErrorOnTry = true;
      return;
    }

    if (isTryCovered) {
      TC.diagnose(loc, diagForThrowingCall);
    } else {
      TC.diagnose(loc, diagForTrylessThrowingCall);
    }
    maybeAddRethrowsNote(TC, loc, reason);
  }

  void diagnoseUnhandledThrowSite(TypeChecker &TC, ASTNode E, bool isTryCovered,
                                  const PotentialReason &reason) {
    switch (getKind()) {
    case Kind::Handled:
      llvm_unreachable("throw site is handled!");

    // TODO: Doug suggested that we could generate one error per
    // non-throwing function with throw sites within it, possibly with
    // notes for the throw sites.

    case Kind::RethrowingFunction:
      diagnoseThrowInLegalContext(TC, E, isTryCovered, reason,
                                  diag::throw_in_rethrows_function,
                                  diag::throwing_call_in_rethrows_function,
                          diag::tryless_throwing_call_in_rethrows_function);
      return;

    case Kind::NonThrowingFunction:
      diagnoseThrowInLegalContext(TC, E, isTryCovered, reason,
                                  diag::throw_in_nonthrowing_function,
                                  diag::throwing_call_unhandled,
                                  diag::tryless_throwing_call_unhandled);
      return;

    case Kind::NonThrowingAutoClosure:
      diagnoseThrowInLegalContext(TC, E, isTryCovered, reason,
                                  diag::throw_in_nonthrowing_autoclosure,
                            diag::throwing_call_in_nonthrowing_autoclosure,
                    diag::tryless_throwing_call_in_nonthrowing_autoclosure);
      return;

    case Kind::NonExhaustiveCatch:
      diagnoseThrowInLegalContext(TC, E, isTryCovered, reason,
                                  diag::throw_in_nonexhaustive_catch,
                                  diag::throwing_call_in_nonexhaustive_catch,
                          diag::tryless_throwing_call_in_nonexhaustive_catch);
      return;

    case Kind::EnumElementInitializer:
      diagnoseThrowInIllegalContext(TC, E, "an enum case raw value");
      return;

    case Kind::GlobalVarInitializer:
      diagnoseThrowInIllegalContext(TC, E, "a global variable initializer");
      return;

    case Kind::IVarInitializer:
      diagnoseThrowInIllegalContext(TC, E, "a property initializer");
      return;

    case Kind::DefaultArgument:
      diagnoseThrowInIllegalContext(TC, E, "a default argument");
      return;

    case Kind::CatchPattern:
      diagnoseThrowInIllegalContext(TC, E, "a catch pattern");
      return;

    case Kind::CatchGuard:
      diagnoseThrowInIllegalContext(TC, E, "a catch guard expression");
      return;
    }
    llvm_unreachable("bad context kind");
  }

  void diagnoseUnhandledTry(TypeChecker &TC, TryExpr *E) {
    switch (getKind()) {
    case Kind::Handled:
    case Kind::RethrowingFunction:
      llvm_unreachable("try is handled!");

    case Kind::NonThrowingFunction:
      if (DiagnoseErrorOnTry)
        TC.diagnose(E->getTryLoc(), diag::try_unhandled);
      return;

    case Kind::NonExhaustiveCatch:
      if (DiagnoseErrorOnTry)
        TC.diagnose(E->getTryLoc(), diag::try_unhandled_in_nonexhaustive_catch);
      return;

    case Kind::NonThrowingAutoClosure:
    case Kind::EnumElementInitializer:
    case Kind::GlobalVarInitializer:
    case Kind::IVarInitializer:
    case Kind::DefaultArgument:
    case Kind::CatchPattern:
    case Kind::CatchGuard:
      assert(!DiagnoseErrorOnTry);
      // Diagnosed at the call sites.
      return;
    }
    llvm_unreachable("bad context kind");
  }
};

/// A class to walk over a local context and validate the correctness
/// of its error coverage.
class CheckErrorCoverage : public ErrorHandlingWalker<CheckErrorCoverage> {
  friend class ErrorHandlingWalker<CheckErrorCoverage>;

  TypeChecker &TC;

  ApplyClassifier Classifier;

  Context CurContext;

  /// Are we currently within a 'try'?
  bool IsInTry = false;

  /// Do we have any throw site in this context?
  bool HasAnyThrowSite = false;

  /// Do we have a throw site using 'try' in this context?
  bool HasTryThrowSite = false;

  void flagInvalidCode() {
    // Suppress warnings about useless try or catch.
    HasAnyThrowSite = true;
    HasTryThrowSite = true;
  }

  /// An RAII object for restoring all the interesting state in an
  /// error-coverage.
  class ContextScope {
    CheckErrorCoverage &Self;
    Context OldContext;
    DeclContext *OldRethrowsDC;
    bool OldIsInTry;
    bool OldHasAnyThrowSite;
    bool OldHasTryThrowSite;
  public:
    ContextScope(CheckErrorCoverage &self, Optional<Context> newContext)
      : Self(self), OldContext(self.CurContext),
        OldRethrowsDC(self.Classifier.RethrowsDC),
        OldIsInTry(self.IsInTry),
        OldHasAnyThrowSite(self.HasAnyThrowSite),
        OldHasTryThrowSite(self.HasTryThrowSite) {
      if (newContext) self.CurContext = *newContext;
    }

    ContextScope(const ContextScope &) = delete;
    ContextScope &operator=(const ContextScope &) = delete;

    void enterSubFunction() {
      Self.Classifier.RethrowsDC = nullptr;
    }

    void refineLocalContext(Context newContext) {
      Self.CurContext = newContext;
    }

    void resetCoverage() {
      Self.IsInTry = false;
      Self.HasAnyThrowSite = false;
      Self.HasTryThrowSite = false;
    }

    void preserveCoverageFromAutoclosureBody() {
      // An autoclosure body is the part of the enclosing function
      // body for the purposes of deciding whether a try contained
      // a throwing call.
      OldHasTryThrowSite = Self.HasTryThrowSite;
    }

    void preserveCoverageFromNonExhaustiveCatch() {
      OldHasAnyThrowSite = Self.HasAnyThrowSite;
    }

    void preserveCoverageFromTryOperand() {
      OldHasAnyThrowSite = Self.HasAnyThrowSite;
    }

    ~ContextScope() {
      Self.CurContext = OldContext;
      Self.Classifier.RethrowsDC = OldRethrowsDC;
      Self.IsInTry = OldIsInTry;
      Self.HasAnyThrowSite = OldHasAnyThrowSite;
      Self.HasTryThrowSite = OldHasTryThrowSite;
    }
  };

public:
  CheckErrorCoverage(TypeChecker &tc, Context initialContext)
    : TC(tc), CurContext(initialContext) {

    if (auto rethrowsDC = initialContext.getRethrowsDC()) {
      Classifier.RethrowsDC = rethrowsDC;
    }
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
    E->getBody()->walk(*this);
    scope.preserveCoverageFromAutoclosureBody();
    return ShouldNotRecurse;
  }

  void checkExhaustiveDoBody(DoCatchStmt *S) {
    // This is a handled context.
    ContextScope scope(*this, Context::getHandled());
    assert(!IsInTry && "do/catch within try?");
    scope.resetCoverage();

    S->getBody()->walk(*this);

    // Warn if nothing threw within the body.
    if (!HasAnyThrowSite) {
      TC.diagnose(S->getCatches().front()->getCatchLoc(),
                  diag::no_throw_in_do_with_catch);
    }
  }

  void checkNonExhaustiveDoBody(DoCatchStmt *S) {
    ContextScope scope(*this, None);
    assert(!IsInTry && "do/catch within try?");
    scope.resetCoverage();

    // If the enclosing context doesn't handle anything, use a
    // specialized diagnostic about non-exhaustive catches.
    if (CurContext.handlesNothing()) {
      scope.refineLocalContext(Context::forNonExhaustiveCatch(S));
    }

    S->getBody()->walk(*this);

    // Warn if nothing threw within the body.
    if (!HasAnyThrowSite) {
      TC.diagnose(S->getCatches().front()->getCatchLoc(),
                  diag::no_throw_in_do_with_catch);
    }

    scope.preserveCoverageFromNonExhaustiveCatch();
  }

  void checkCatch(CatchStmt *S) {
    // The pattern and guard aren't allowed to throw.
    {
      ContextScope scope(*this, Context::forCatchPattern(S));
      S->getErrorPattern()->walk(*this);
    }
    if (auto guard = S->getGuardExpr()) {
      ContextScope scope(*this, Context::forCatchGuard(S));
      guard->walk(*this);
    }

    // The catch body just happens in the enclosing context.
    S->getBody()->walk(*this);
  }

  ShouldRecurse_t checkApply(ApplyExpr *E) {
    // An apply expression is a potential throw site if the function throws.
    // But if the expression didn't type-check, suppress diagnostics.
    auto classification = Classifier.classifyApply(E);

    checkThrowSite(E, /*requiresTry*/ true, classification);

    // HACK: functions can get queued multiple times in
    // definedFunctions, so be sure to be idempotent.
    if (!E->isThrowsSet()) {
      E->setThrows(classification.getResult() == ThrowingKind::RethrowingOnly ||
                   classification.getResult() == ThrowingKind::Throws);
    }

    return ShouldRecurse;
  }

  ShouldRecurse_t checkThrow(ThrowStmt *S) {
    checkThrowSite(S, /*requiresTry*/ false,
                   Classification::forThrow(PotentialReason::forThrow()));
    return ShouldRecurse;
  }

  void checkThrowSite(ASTNode E, bool requiresTry,
                      const Classification &classification) {
    switch (classification.getResult()) {
    // Completely ignores sites that don't throw.
    case ThrowingKind::None:
      return;

    // Suppress all diagnostics when there's an un-analyzable throw site.
    case ThrowingKind::Invalid:
      HasAnyThrowSite = true;
      HasTryThrowSite |= requiresTry;
      return;

    // For the purposes of handling and try-coverage diagnostics,
    // being rethrowing-only still makes this a throw site.
    case ThrowingKind::RethrowingOnly:
    case ThrowingKind::Throws:
      HasAnyThrowSite = true;
      HasTryThrowSite |= requiresTry;

      if (auto expr = E.dyn_cast<Expr*>())
        if (auto apply = dyn_cast<ApplyExpr>(expr))
          if (apply->isThrowsSet())
            return;

      bool isTryCovered = (!requiresTry || IsInTry);
      if (!CurContext.handles(classification.getResult())) {
        CurContext.diagnoseUnhandledThrowSite(TC, E, isTryCovered,
                                              classification.getThrowsReason());
      } else if (!isTryCovered && !TC.Context.LangOpts.EnableThrowWithoutTry) {
        CurContext.diagnoseUncoveredThrowSite(TC, E,
                                              classification.getThrowsReason());
      }
      return;
    }
    llvm_unreachable("bad throwing kind");
  }

  ShouldRecurse_t checkTry(TryExpr *E) {
    // Walk the operand.
    ContextScope scope(*this, None);
    IsInTry = true;
    HasTryThrowSite = false;

    E->getSubExpr()->walk(*this);

    // Warn about 'try' expressions that weren't actually needed.
    if (!HasTryThrowSite) {
      TC.diagnose(E->getTryLoc(), diag::no_throw_in_try);

    // Diagnose all the call sites within a single unhandled 'try'
    // at the same time.
    } else if (CurContext.handlesNothing()) {
      CurContext.diagnoseUnhandledTry(TC, E);
    }

    scope.preserveCoverageFromTryOperand();
    return ShouldNotRecurse;
  }

  ShouldRecurse_t checkForceTry(ForceTryExpr *E) {
    // Walk the operand.  'try!' handles errors.
    ContextScope scope(*this, Context::getHandled());
    IsInTry = true;
    HasTryThrowSite = false;

    E->getSubExpr()->walk(*this);

    // Warn about 'try' expressions that weren't actually needed.
    if (!HasTryThrowSite) {
      TC.diagnose(E->getLoc(), diag::no_throw_in_try);
    }
    return ShouldNotRecurse;
  }
};

} // end anonymous namespace 

void TypeChecker::checkTopLevelErrorHandling(TopLevelCodeDecl *code) {
  CheckErrorCoverage checker(*this, Context::forTopLevelCode(code));
  code->getBody()->walk(checker);
}

void TypeChecker::checkFunctionErrorHandling(AbstractFunctionDecl *fn) {
  CheckErrorCoverage checker(*this, Context::forFunction(fn));
  if (auto body = fn->getBody()) {
    body->walk(checker);
  }
  if (auto ctor = dyn_cast<ConstructorDecl>(fn))
    if (auto superInit = ctor->getSuperInitCall())
      superInit->walk(checker);
}

void TypeChecker::checkInitializerErrorHandling(Initializer *initCtx,
                                                Expr *init) {
  CheckErrorCoverage checker(*this, Context::forInitializer(initCtx));
  init->walk(checker);
}

/// Check the correctness of error handling within the given enum
/// element's raw value expression.
///
/// The syntactic restrictions on such expressions should make it
/// impossible for errors to ever arise, but checking them anyway (1)
/// ensures correctness if those restrictions are ever loosened,
/// perhaps accidentally, and (2) allows the verifier to assert that
/// all calls have been checked.
void TypeChecker::checkEnumElementErrorHandling(EnumElementDecl *elt) {
  if (auto init = elt->getTypeCheckedRawValueExpr()) {
    CheckErrorCoverage checker(*this, Context::forEnumElementInitializer(elt));
    init->walk(checker);
  }
}
