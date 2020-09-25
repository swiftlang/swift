//===--- TypeCheckConcurrency.cpp - Concurrency ---------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements type checking support for Swift's concurrency model.
//
//===----------------------------------------------------------------------===//
#include "TypeCheckConcurrency.h"
#include "TypeChecker.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/TypeCheckRequests.h"

using namespace swift;

/// Check whether the @asyncHandler attribute can be applied to the given
/// function declaration.
///
/// \param diagnose Whether to emit a diagnostic when a problem is encountered.
///
/// \returns \c true if there was a problem with adding the attribute, \c false
/// otherwise.
static bool checkAsyncHandler(FuncDecl *func, bool diagnose) {
  if (!func->getResultInterfaceType()->isVoid()) {
    if (diagnose) {
      func->diagnose(diag::asynchandler_returns_value)
          .highlight(func->getResultTypeSourceRange());
    }

    return true;
  }

  if (func->hasThrows()) {
    if (diagnose) {
      func->diagnose(diag::asynchandler_throws)
          .fixItRemove(func->getThrowsLoc());
    }

    return true;
  }

  if (func->hasAsync()) {
    if (diagnose) {
      func->diagnose(diag::asynchandler_async)
          .fixItRemove(func->getAsyncLoc());
    }

    return true;
  }

  for (auto param : *func->getParameters()) {
    if (param->isInOut()) {
      if (diagnose) {
        param->diagnose(diag::asynchandler_inout_parameter)
            .fixItRemove(param->getSpecifierLoc());
      }

      return true;
    }
  }

  if (func->isMutating()) {
    if (diagnose) {
      auto diag = func->diagnose(diag::asynchandler_mutating);
      if (auto mutatingAttr = func->getAttrs().getAttribute<MutatingAttr>()) {
        diag.fixItRemove(mutatingAttr->getRange());
      }
    }

    return true;
  }

  return false;
}

void swift::addAsyncNotes(FuncDecl *func) {
  func->diagnose(diag::note_add_async_to_function, func->getName());

  if (func->canBeAsyncHandler()) {
    func->diagnose(
            diag::note_add_asynchandler_to_function, func->getName())
        .fixItInsert(func->getAttributeInsertionLoc(false), "@asyncHandler ");
  }
}

bool IsAsyncHandlerRequest::evaluate(
    Evaluator &evaluator, FuncDecl *func) const {
  // Check whether the attribute was explicitly specified.
  if (auto attr = func->getAttrs().getAttribute<AsyncHandlerAttr>()) {
    // Check for well-formedness.
    if (checkAsyncHandler(func, /*diagnose=*/true)) {
      attr->setInvalid();
      return false;
    }

    return true;
  }

  if (!func->getASTContext().LangOpts.EnableExperimentalConcurrency)
    return false;

  // Are we in a context where inference is possible?
  auto dc = func->getDeclContext();
  if (!dc->isTypeContext() || !dc->getParentSourceFile() ||
      isa<ProtocolDecl>(dc) || !func->hasBody())
    return false;

  // Is it possible to infer @asyncHandler for this function at all?
  if (!func->canBeAsyncHandler())
    return false;

  // Add an implicit @asyncHandler attribute and return true. We're done.
  auto addImplicitAsyncHandlerAttr = [&] {
    func->getAttrs().add(new (func->getASTContext()) AsyncHandlerAttr(true));
    return true;
  };

  // Check whether any of the conformances in the context of the function
  // implies @asyncHandler.
  {
    auto idc = cast<IterableDeclContext>(dc->getAsDecl());
    auto conformances = evaluateOrDefault(
        dc->getASTContext().evaluator,
        LookupAllConformancesInContextRequest{idc}, { });

    for (auto conformance : conformances) {
      auto protocol = conformance->getProtocol();
      for (auto found : protocol->lookupDirect(func->getName())) {
        if (!isa<ProtocolDecl>(found->getDeclContext()))
          continue;

        auto requirement = dyn_cast<FuncDecl>(found);
        if (!requirement)
          continue;

        if (!requirement->isAsyncHandler())
          continue;

        auto witness = conformance->getWitnessDecl(requirement);
        if (witness != func)
          continue;

        return addImplicitAsyncHandlerAttr();
      }
    }
  }

  // Look through dynamic replacements.
  if (auto replaced = func->getDynamicallyReplacedDecl()) {
    if (auto replacedFunc = dyn_cast<FuncDecl>(replaced))
      if (replacedFunc->isAsyncHandler())
        return addImplicitAsyncHandlerAttr();
  }

  return false;
}

bool CanBeAsyncHandlerRequest::evaluate(
    Evaluator &evaluator, FuncDecl *func) const {
  return !checkAsyncHandler(func, /*diagnose=*/false);
}

bool IsActorRequest::evaluate(
    Evaluator &evaluator, ClassDecl *classDecl) const {
  // If concurrency is not enabled, we don't have actors.
  auto actorAttr = classDecl->getAttrs().getAttribute<ActorAttr>();

  // If there is a superclass, we can infer actor-ness from it.
  if (auto superclassDecl = classDecl->getSuperclassDecl()) {
    // The superclass is an actor, so we are, too.
    if (superclassDecl->isActor())
      return true;

    // This class cannot be an actor; complain if the 'actor' modifier was
    // provided.
    if (actorAttr) {
      classDecl->diagnose(
          diag::actor_with_nonactor_superclass, superclassDecl->getName())
        .highlight(actorAttr->getRange());
    }

    return false;
  }

  return actorAttr != nullptr;
}

namespace {

/// The isolation restriction in effect for a given declaration that is
/// referenced from source.
class IsolationRestriction {
public:
  enum Kind {
    /// There is no restriction on references to the given declaration,
    /// e.g., because it's immutable.
    Unrestricted,

    /// Access to the declaration is unsafe in a concurrent context.
    Unsafe,

    /// The declaration is a local entity whose capture could introduce
    /// data races. The context in which the local was defined is provided.
    LocalCapture,

    /// References to this local variable that can only be made from the
    /// References to this member of an actor are only permitted on 'self'.
    ActorSelf,
  };

private:
  /// The kind of restriction
  Kind kind;

  union {
    /// The local context that an entity is tied to.
    DeclContext *localContext;

    /// The actor class that the entity is declared in.
    ClassDecl *actorClass;
  } data;

  explicit IsolationRestriction(Kind kind) : kind(kind) { }

public:
  Kind getKind() const { return kind; }

  /// Retrieve the declaration context in which a local was defined.
  DeclContext *getLocalContext() const {
    assert(kind == LocalCapture);
    return data.localContext;
  }

  /// Retrieve the actor class that the declaration is within.
  ClassDecl *getActorClass() const {
    assert(kind == ActorSelf);
    return data.actorClass;
  }

  /// There are no restrictions on the use of the entity.
  static IsolationRestriction forUnrestricted() {
    return IsolationRestriction(Unrestricted);
  }

  /// Accesses to the given declaration are unsafe.
  static IsolationRestriction forUnsafe() {
    return IsolationRestriction(Unsafe);
  }

  /// Accesses to the given declaration can only be made via the 'self' of
  /// the current actor.
  static IsolationRestriction forActorSelf(ClassDecl *actorClass) {
    IsolationRestriction result(ActorSelf);
    result.data.actorClass = actorClass;
    return result;
  }

  /// Access is restricted to code running within the given local context.
  static IsolationRestriction forLocalCapture(DeclContext *dc) {
    IsolationRestriction result(LocalCapture);
    result.data.localContext = dc;
    return result;
  }

  /// Determine the isolation rules for a given declaration.
  static IsolationRestriction forDeclaration(Decl *decl) {
    switch (decl->getKind()) {
    case DeclKind::AssociatedType:
    case DeclKind::Class:
    case DeclKind::Enum:
    case DeclKind::Extension:
    case DeclKind::GenericTypeParam:
    case DeclKind::OpaqueType:
    case DeclKind::Protocol:
    case DeclKind::Struct:
    case DeclKind::TypeAlias:
      // Types are always available.
      return forUnrestricted();

    case DeclKind::Constructor:
    case DeclKind::EnumCase:
    case DeclKind::EnumElement:
      // Type-level entities don't require isolation.
      return forUnrestricted();

    case DeclKind::IfConfig:
    case DeclKind::Import:
    case DeclKind::InfixOperator:
    case DeclKind::MissingMember:
    case DeclKind::Module:
    case DeclKind::PatternBinding:
    case DeclKind::PostfixOperator:
    case DeclKind::PoundDiagnostic:
    case DeclKind::PrecedenceGroup:
    case DeclKind::PrefixOperator:
    case DeclKind::TopLevelCode:
      // Non-value entities don't require isolation.
      return forUnrestricted();

    case DeclKind::Destructor:
      // Destructors don't require isolation.
      return forUnrestricted();

    case DeclKind::Param:
    case DeclKind::Var:
      // 'let' declarations are immutable, so there are no restrictions on
      // their access.
      if (cast<VarDecl>(decl)->isLet())
        return forUnrestricted();

      LLVM_FALLTHROUGH;

    case DeclKind::Accessor:
    case DeclKind::Func:
    case DeclKind::Subscript:
      // Local captures can only be referenced in their local context or a
      // context that is guaranteed not to run concurrently with it.
      if (cast<ValueDecl>(decl)->isLocalCapture())
        return forLocalCapture(decl->getDeclContext());

      // Determine the actor isolation of the given declaration.
      switch (auto isolation = getActorIsolation(cast<ValueDecl>(decl))) {
      case ActorIsolation::ActorInstance:
        // Protected actor instance members can only be accessed on 'self'.
        return forActorSelf(isolation.getActor());

      case ActorIsolation::Independent:
      case ActorIsolation::ActorPrivileged:
        // Actor-independent and actor-privileged declarations have no
        // restrictions on their access.
        return forUnrestricted();

      case ActorIsolation::Unspecified:
        return forUnsafe();
      }
    }
  }

  operator Kind() const { return kind; };
};

}

namespace {
  /// Describes the important parts of a partial apply thunk.
  struct PartialApplyThunkInfo {
    Expr *base;
    Expr *fn;
    bool isEscaping;
  };
}

/// Try to decompose a call that might be an invocation of a partial apply
/// thunk.
static Optional<PartialApplyThunkInfo> decomposePartialApplyThunk(
    ApplyExpr *apply, Expr *parent) {
  // Check for a call to the outer closure in the thunk.
  auto outerAutoclosure = dyn_cast<AutoClosureExpr>(apply->getFn());
  if (!outerAutoclosure ||
      outerAutoclosure->getThunkKind()
        != AutoClosureExpr::Kind::DoubleCurryThunk)
    return None;

  auto memberFn = outerAutoclosure->getUnwrappedCurryThunkExpr();
  if (!memberFn)
    return None;

  // Determine whether the partial apply thunk was immediately converted to
  // noescape.
  bool isEscaping = true;
  if (auto conversion = dyn_cast_or_null<FunctionConversionExpr>(parent)) {
    auto fnType = conversion->getType()->getAs<FunctionType>();
    isEscaping = fnType && !fnType->isNoEscape();
  }

  return PartialApplyThunkInfo{apply->getArg(), memberFn, isEscaping};
}

/// Find the immediate member reference in the given expression.
static Optional<std::pair<ValueDecl *, SourceLoc>>
findMemberReference(Expr *expr) {
  if (auto declRef = dyn_cast<DeclRefExpr>(expr))
    return std::make_pair(declRef->getDecl(), declRef->getLoc());

  if (auto otherCtor = dyn_cast<OtherConstructorDeclRefExpr>(expr)) {
    return std::make_pair(
        static_cast<ValueDecl *>(otherCtor->getDecl()), otherCtor->getLoc());
  }

  return None;
}

void swift::checkActorIsolation(const Expr *expr, const DeclContext *dc) {
  class ActorIsolationWalker : public ASTWalker {
    ASTContext &ctx;
    SmallVector<const DeclContext *, 4> contextStack;

    const DeclContext *getDeclContext() const {
      return contextStack.back();
    }

  public:
    ActorIsolationWalker(const DeclContext *dc) : ctx(dc->getASTContext()) {
      contextStack.push_back(dc);
    }

    bool shouldWalkIntoSeparatelyCheckedClosure(ClosureExpr *expr) override {
      return false;
    }

    bool shouldWalkCaptureInitializerExpressions() override { return true; }

    bool shouldWalkIntoTapExpression() override { return false; }

    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      if (auto *closure = dyn_cast<AbstractClosureExpr>(expr)) {
        contextStack.push_back(closure);
        return { true, expr };
      }

      if (auto lookup = dyn_cast<LookupExpr>(expr)) {
        checkMemberReference(lookup->getBase(), lookup->getMember().getDecl(),
                             lookup->getLoc());
        return { true, expr };
      }

      if (auto declRef = dyn_cast<DeclRefExpr>(expr)) {
        checkNonMemberReference(declRef->getDecl(), declRef->getLoc());
        return { true, expr };
      }

      if (auto apply = dyn_cast<ApplyExpr>(expr)) {
        // If this is a call to a partial apply thunk, decompose it to check it
        // like based on the original written syntax, e.g., "self.method".
        if (auto partialApply = decomposePartialApplyThunk(
                apply, Parent.getAsExpr())) {
          if (auto memberRef = findMemberReference(partialApply->fn)) {
            checkMemberReference(
                partialApply->base, memberRef->first, memberRef->second,
                partialApply->isEscaping);

            partialApply->base->walk(*this);
            return { false, expr };
          }
        }
      }

      if (auto call = dyn_cast<SelfApplyExpr>(expr)) {
        Expr *fn = call->getFn()->getValueProvidingExpr();
        if (auto memberRef = findMemberReference(fn)) {
          checkMemberReference(
              call->getArg(), memberRef->first, memberRef->second);
          call->getArg()->walk(*this);
          return { false, expr };
        }
      }

      return { true, expr };
    }

    Expr *walkToExprPost(Expr *expr) override {
      if (auto *closure = dyn_cast<AbstractClosureExpr>(expr)) {
        assert(contextStack.back() == closure);
        contextStack.pop_back();
      }

      return expr;
    }

  private:
    /// If the expression is a reference to `self`, return the 'self' parameter.
    static VarDecl *getSelfReference(Expr *expr) {
      // Look through identity expressions and implicit conversions.
      Expr *prior;
      do {
        prior = expr;

        expr = expr->getSemanticsProvidingExpr();

        if (auto conversion = dyn_cast<ImplicitConversionExpr>(expr))
          expr = conversion->getSubExpr();
      } while (prior != expr);

      // 'super' references always act on self.
      if (auto super = dyn_cast<SuperRefExpr>(expr))
        return super->getSelf();

      // Declaration references to 'self'.
      if (auto declRef = dyn_cast<DeclRefExpr>(expr)) {
        if (auto var = dyn_cast<VarDecl>(declRef->getDecl()))
          if (var->isSelfParameter())
            return var;
      }

      // Not a self reference.
      return nullptr;
    }

    /// Note that the given actor member is isolated.
    static void noteIsolatedActorMember(ValueDecl *decl) {
      if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
        // FIXME: We'd like to insert 'async' at the appropriate place, but
        // FuncDecl/AbstractFunctionDecl doesn't have the right source-location
        // information to do so.
        func->diagnose(diag::actor_isolated_method);
      } else if (isa<VarDecl>(decl)) {
        decl->diagnose(diag::actor_mutable_state);
      } else {
        decl->diagnose(diag::kind_declared_here, decl->getDescriptiveKind());
      }
    }

    /// Determine whether code in the given use context might execute
    /// concurrently with code in the definition context.
    bool mayExecuteConcurrentlyWith(
        const DeclContext *useContext, const DeclContext *defContext) {

      // Walk the context chain from the use to the definition.
      while (useContext != defContext) {
        // If we find an escaping closure, it can be run concurrently.
        if (auto closure = dyn_cast<AbstractClosureExpr>(useContext)) {
          if (auto type = closure->getType()) {
            if (auto fnType = type->getAs<AnyFunctionType>())
              if (!fnType->isNoEscape())
                return true;
          }
        }

        // If we find a local function, it can escape and be run concurrently.
        if (auto func = dyn_cast<AbstractFunctionDecl>(useContext)) {
          if (func->isLocalCapture())
            return true;
        }

        // If we hit a module-scope context, it's not concurrent.
        useContext = useContext->getParent();
        if (useContext->isModuleScopeContext())
          return false;
      }

      // We hit the same context, so it won't execute concurrently.
      return false;
    }

    // Retrieve the nearest enclosing actor context.
    static ClassDecl *getNearestEnclosingActorContext(const DeclContext *dc) {
      while (!dc->isModuleScopeContext()) {
        if (dc->isTypeContext()) {
          if (auto classDecl = dc->getSelfClassDecl()) {
            if (classDecl->isActor())
              return classDecl;
          }
        }

        dc = dc->getParent();
      }

      return nullptr;
    }

    /// Diagnose a reference to an unsafe entity.
    ///
    /// \returns true if we diagnosed the entity, \c false otherwise.
    bool diagnoseReferenceToUnsafe(ValueDecl *value, SourceLoc loc) {
      // Only diagnose unsafe concurrent accesses within the context of an
      // actor. This is globally unsafe, but locally enforceable.
      if (!getNearestEnclosingActorContext(getDeclContext()))
        return false;

      // Only diagnose direct references to mutable shared state. This is
      // globally unsafe, but reduces the noise.
      if (!isa<VarDecl>(value) || !cast<VarDecl>(value)->hasStorage())
        return false;

      ctx.Diags.diagnose(
          loc, diag::shared_mutable_state_access,
          value->getDescriptiveKind(), value->getName());
      value->diagnose(diag::kind_declared_here, value->getDescriptiveKind());
      return true;
    }

    /// Check a reference to a local or global.
    bool checkNonMemberReference(ValueDecl *value, SourceLoc loc) {
      if (!value)
        return false;

      switch (auto isolation = IsolationRestriction::forDeclaration(value)) {
      case IsolationRestriction::Unrestricted:
        return false;

      case IsolationRestriction::ActorSelf:
        llvm_unreachable("non-member reference into an actor");

      case IsolationRestriction::LocalCapture:
        // Only diagnose unsafe concurrent accesses within the context of an
        // actor. This is globally unsafe, but locally enforceable.
        if (!getNearestEnclosingActorContext(getDeclContext()))
          return false;

        // Check whether we are in a context that will not execute concurrently
        // with the context of 'self'.
        if (mayExecuteConcurrentlyWith(
                getDeclContext(), isolation.getLocalContext())) {
          ctx.Diags.diagnose(
              loc, diag::concurrent_access_local,
              value->getDescriptiveKind(), value->getName());
          value->diagnose(
              diag::kind_declared_here, value->getDescriptiveKind());
          return true;
        }

        return false;

      case IsolationRestriction::Unsafe:
        return diagnoseReferenceToUnsafe(value, loc);
      }
    }

    /// Check a reference with the given base expression to the given member.
    bool checkMemberReference(
        Expr *base, ValueDecl *member, SourceLoc memberLoc,
        bool isEscapingPartialApply = false) {
      if (!base || !member)
        return false;

      switch (auto isolation = IsolationRestriction::forDeclaration(member)) {
      case IsolationRestriction::Unrestricted:
        return false;

      case IsolationRestriction::ActorSelf: {
        // Must reference actor-isolated state on 'self'.
        auto selfVar = getSelfReference(base);
        if (!selfVar) {
          ctx.Diags.diagnose(
              memberLoc, diag::actor_isolated_non_self_reference,
              member->getDescriptiveKind(),
              member->getName(),
              isolation.getActorClass() ==
                getNearestEnclosingActorContext(getDeclContext()));
          noteIsolatedActorMember(member);
          return true;
        }

        // Check whether the context of 'self' is actor-isolated.
        switch (getActorIsolation(
                   cast<ValueDecl>(selfVar->getDeclContext()->getAsDecl()))) {
          case ActorIsolation::ActorInstance:
          case ActorIsolation::ActorPrivileged:
            // An escaping partial application of something that is part of
            // the actor's isolated state is never permitted.
            if (isEscapingPartialApply) {
              ctx.Diags.diagnose(
                  memberLoc, diag::actor_isolated_partial_apply,
                  member->getDescriptiveKind(),
                  member->getName());
              noteIsolatedActorMember(member);
              return true;
            }
            break;

          case ActorIsolation::Unspecified:
            // Okay
            break;

          case ActorIsolation::Independent:
            // The 'self' is for an actor-independent member, which means
            // we cannot refer to actor-isolated state.
            ctx.Diags.diagnose(
                memberLoc, diag::actor_isolated_self_independent_context,
                member->getDescriptiveKind(),
                member->getName());
            noteIsolatedActorMember(member);
            return true;
        }

        // Check whether we are in a context that will not execute concurrently
        // with the context of 'self'.
        if (mayExecuteConcurrentlyWith(
                getDeclContext(), selfVar->getDeclContext())) {
          ctx.Diags.diagnose(
              memberLoc, diag::actor_isolated_concurrent_access,
              member->getDescriptiveKind(), member->getName());
          noteIsolatedActorMember(member);
          return true;
        }

        // It's fine.
        return false;
      }

      case IsolationRestriction::LocalCapture:
        llvm_unreachable("Locals cannot be referenced with member syntax");

      case IsolationRestriction::Unsafe:
        return diagnoseReferenceToUnsafe(member, memberLoc);
      }
    }
  };

  ActorIsolationWalker walker(dc);
  const_cast<Expr *>(expr)->walk(walker);
}

ActorIsolation ActorIsolationRequest::evaluate(
    Evaluator &evaluator, ValueDecl *value) const {
  // If the attribute is explicitly marked @actorIndependent, report it as
  // independent.
  if (value->getAttrs().hasAttribute<ActorIndependentAttr>()) {
    return ActorIsolation::forIndependent();
  }

  // If the declaration overrides another declaration, it must have the same
  // actor isolation.
  if (auto overriddenValue = value->getOverriddenDecl()) {
    if (auto isolation = getActorIsolation(overriddenValue))
      return isolation;
  }

  // Check for instance members of actor classes, which are part of
  // actor-isolated state.
  auto classDecl = value->getDeclContext()->getSelfClassDecl();
  if (classDecl && classDecl->isActor() && value->isInstanceMember()) {
    // A function that is an asynchronous context is actor-privileged.
    if (auto func = dyn_cast<AbstractFunctionDecl>(value)) {
      if (func->isAsyncContext())
        return ActorIsolation::forActorPrivileged(classDecl);
    }

    // Everything else is part of the actor's isolated state.
    return ActorIsolation::forActorInstance(classDecl);
  }

  // Everything else is unspecified.
  return ActorIsolation::forUnspecified();
}

ActorIsolation swift::getActorIsolation(ValueDecl *value) {
  auto &ctx = value->getASTContext();
  return evaluateOrDefault(
      ctx.evaluator, ActorIsolationRequest{value},
      ActorIsolation::forUnspecified());
}
