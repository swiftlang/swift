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
#include "TypeCheckType.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/TypeCheckRequests.h"

using namespace swift;

/// Determine whether it makes sense to infer an attribute in the given
/// context.
static bool shouldInferAttributeInContext(const DeclContext *dc) {
  auto sourceFile = dc->getParentSourceFile();
  if (!sourceFile)
    return false;

  switch (sourceFile->Kind) {
  case SourceFileKind::Interface:
  case SourceFileKind::SIL:
    return false;

  case SourceFileKind::Library:
  case SourceFileKind::Main:
    return true;
  }
}

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

    if (auto fnType = param->getInterfaceType()->getAs<FunctionType>()) {
      if (fnType->isNoEscape()) {
        if (diagnose) {
          param->diagnose(diag::asynchandler_noescape_closure_parameter);
        }

        return true;
      }
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

void swift::addAsyncNotes(AbstractFunctionDecl const* func) {
  assert(func);
  if (!isa<DestructorDecl>(func))
    func->diagnose(diag::note_add_async_to_function, func->getName());
    // TODO: we need a source location for effects attributes so that we 
    // can also emit a fix-it that inserts 'async' in the right place for func.
    // It's possibly a bit tricky to get the right source location from
    // just the AbstractFunctionDecl, but it's important to circle-back
    // to this.

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

  if (!shouldInferAttributeInContext(func->getDeclContext()))
    return false;

  // Are we in a context where inference is possible?
  auto dc = func->getDeclContext();
  if (!dc->getSelfClassDecl() || !dc->getParentSourceFile() || !func->hasBody())
    return false;

  // Is it possible to infer @asyncHandler for this function at all?
  if (!func->canBeAsyncHandler())
    return false;

  if (!dc->getSelfClassDecl()->isActor())
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

    // The superclass is 'NSObject', which is known to have no state and no
    // superclass.
    if (superclassDecl->isNSObject() && actorAttr != nullptr)
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

bool IsDefaultActorRequest::evaluate(
    Evaluator &evaluator, ClassDecl *classDecl) const {
  // If the class isn't an actor class, it's not a default actor.
  if (!classDecl->isActor())
    return false;

  // If there is a superclass, and it's an actor class, we defer
  // the decision to it.
  if (auto superclassDecl = classDecl->getSuperclassDecl()) {
    // If the superclass is an actor, we inherit its default-actor-ness.
    if (superclassDecl->isActor())
      return superclassDecl->isDefaultActor();

    // If the superclass is not an actor class, it can only be
    // a default actor if it's NSObject.  (For now, other classes simply
    // can't be actors at all.)  We don't need to diagnose this; we
    // should've done that already in isActor().
    if (!superclassDecl->isNSObject())
      return false;
  }

  // If the class has explicit custom-actor methods, it's not
  // a default actor.
  if (classDecl->hasExplicitCustomActorMethods())
    return false;

  return true;
}

static bool isDeclNotAsAccessibleAsParent(ValueDecl *decl,
                                          NominalTypeDecl *parent) {
  return decl->getFormalAccess() <
         std::min(parent->getFormalAccess(), AccessLevel::Public);
}

VarDecl *GlobalActorInstanceRequest::evaluate(
    Evaluator &evaluator, NominalTypeDecl *nominal) const {
  auto globalActorAttr = nominal->getAttrs().getAttribute<GlobalActorAttr>();
  if (!globalActorAttr)
    return nullptr;

  // Ensure that the actor protocol has been loaded.
  ASTContext &ctx = nominal->getASTContext();
  auto actorProto = ctx.getProtocol(KnownProtocolKind::Actor);
  if (!actorProto) {
    nominal->diagnose(diag::concurrency_lib_missing, "Actor");
    return nullptr;
  }

  // Global actors have a static property "shared" that provides an actor
  // instance. The value must
  SmallVector<ValueDecl *, 4> decls;
  nominal->lookupQualified(
      nominal, DeclNameRef(ctx.Id_shared), NL_QualifiedDefault, decls);
  VarDecl *sharedVar = nullptr;
  llvm::TinyPtrVector<VarDecl *> candidates;
  for (auto decl : decls) {
    auto var = dyn_cast<VarDecl>(decl);
    if (!var)
      continue;

    auto varDC = var->getDeclContext();
    if (var->isStatic() &&
        !isDeclNotAsAccessibleAsParent(var, nominal) &&
        !(isa<ExtensionDecl>(varDC) &&
          cast<ExtensionDecl>(varDC)->isConstrainedExtension()) &&
        TypeChecker::conformsToProtocol(
            varDC->mapTypeIntoContext(var->getValueInterfaceType()),
            actorProto, nominal)) {
      sharedVar = var;
      break;
    }

    candidates.push_back(var);
  }

  // If we found a suitable candidate, we're done.
  if (sharedVar)
    return sharedVar;

  // Complain about the lack of a suitable 'shared' property.
  {
    auto primaryDiag = nominal->diagnose(
        diag::global_actor_missing_shared, nominal->getName());

    // If there were no candidates, provide a Fix-It with a prototype.
    if (candidates.empty() && nominal->getBraces().Start.isValid()) {
      // Figure out the indentation we need.
      SourceLoc sharedInsertionLoc = Lexer::getLocForEndOfToken(
          ctx.SourceMgr, nominal->getBraces().Start);

      StringRef extraIndent;
      StringRef currentIndent = Lexer::getIndentationForLine(
          ctx.SourceMgr, sharedInsertionLoc, &extraIndent);
      std::string stubIndent = (currentIndent + extraIndent).str();

      // From the string to add the declaration.
      std::string sharedDeclString = "\n" + stubIndent;
      if (nominal->getFormalAccess() >= AccessLevel::Public)
        sharedDeclString += "public ";

      sharedDeclString += "static let shared = <#actor instance#>";

      primaryDiag.fixItInsert(sharedInsertionLoc, sharedDeclString);
    }
  }

  // Remark about all of the candidates that failed (and why).
  for (auto candidate : candidates) {
    if (!candidate->isStatic()) {
      candidate->diagnose(diag::global_actor_shared_not_static)
        .fixItInsert(candidate->getAttributeInsertionLoc(true), "static ");
      continue;
    }

    if (isDeclNotAsAccessibleAsParent(candidate, nominal)) {
      AccessLevel needAccessLevel = std::min(
          nominal->getFormalAccess(), AccessLevel::Public);
      auto diag = candidate->diagnose(
          diag::global_actor_shared_inaccessible,
          getAccessLevelSpelling(candidate->getFormalAccess()),
          getAccessLevelSpelling(needAccessLevel));
      if (auto attr = candidate->getAttrs().getAttribute<AccessControlAttr>()) {
        if (needAccessLevel == AccessLevel::Internal) {
          diag.fixItRemove(attr->getRange());
        } else {
          diag.fixItReplace(
              attr->getRange(), getAccessLevelSpelling(needAccessLevel));
        }
      } else {
        diag.fixItInsert(
            candidate->getAttributeInsertionLoc(true),
            getAccessLevelSpelling(needAccessLevel));
      }
      continue;
    }

    if (auto ext = dyn_cast<ExtensionDecl>(candidate->getDeclContext())) {
      if (ext->isConstrainedExtension()) {
        candidate->diagnose(diag::global_actor_shared_constrained_extension);
        continue;
      }
    }

    Type varType = candidate->getDeclContext()->mapTypeIntoContext(
        candidate->getValueInterfaceType());
    candidate->diagnose(diag::global_actor_shared_non_actor_type, varType);
  }

  return nullptr;
}

Optional<std::pair<CustomAttr *, NominalTypeDecl *>>
GlobalActorAttributeRequest::evaluate(
    Evaluator &evaluator, Decl *decl) const {
  ASTContext &ctx = decl->getASTContext();
  auto dc = decl->getDeclContext();
  CustomAttr *globalActorAttr = nullptr;
  NominalTypeDecl *globalActorNominal = nullptr;

  for (auto attr : decl->getAttrs().getAttributes<CustomAttr>()) {
    auto mutableAttr = const_cast<CustomAttr *>(attr);
    // Figure out which nominal declaration this custom attribute refers to.
    auto nominal = evaluateOrDefault(ctx.evaluator,
                                     CustomAttrNominalRequest{mutableAttr, dc},
                                     nullptr);

    // Ignore unresolvable custom attributes.
    if (!nominal)
      continue;

    // We are only interested in global actor types.
    if (!nominal->isGlobalActor())
      continue;

    // Only a single global actor can be applied to a given declaration.
    if (globalActorAttr) {
      decl->diagnose(
          diag::multiple_global_actors, globalActorNominal->getName(),
          nominal->getName());
      continue;
    }

    globalActorAttr = const_cast<CustomAttr *>(attr);
    globalActorNominal = nominal;
  }

  if (!globalActorAttr)
    return None;

  // Check that a global actor attribute makes sense on this kind of
  // declaration.
  if (auto nominal = dyn_cast<NominalTypeDecl>(decl)) {
    // Nominal types are okay...
    if (auto classDecl = dyn_cast<ClassDecl>(nominal)){
      if (classDecl->isActor()) {
        // ... except for actor classes.
        nominal->diagnose(diag::global_actor_on_actor_class, nominal->getName())
            .highlight(globalActorAttr->getRangeWithAt());
        return None;
      }
    }
  } else if (auto storage = dyn_cast<AbstractStorageDecl>(decl)) {
    // Subscripts and properties are fine...
    if (auto var = dyn_cast<VarDecl>(storage)) {
      if (var->getDeclContext()->isLocalContext()) {
        var->diagnose(diag::global_actor_on_local_variable, var->getName())
            .highlight(globalActorAttr->getRangeWithAt());
        return None;
      }

      // Global actors don't make sense on a stored property of a struct.
      if (var->hasStorage() && var->getDeclContext()->getSelfStructDecl() &&
          var->isInstanceMember()) {
        var->diagnose(diag::global_actor_on_struct_property, var->getName())
          .highlight(globalActorAttr->getRangeWithAt());
        return None;
      }

    }
  } else if (isa<ExtensionDecl>(decl)) {
    // Extensions are okay.
  } else if (isa<ConstructorDecl>(decl) || isa<FuncDecl>(decl)) {
    // Functions are okay.
  } else {
    // Everything else is disallowed.
    decl->diagnose(diag::global_actor_disallowed, decl->getDescriptiveKind());
    return None;
  }

  return std::make_pair(globalActorAttr, globalActorNominal);
}

/// Determine the isolation rules for a given declaration.
ActorIsolationRestriction ActorIsolationRestriction::forDeclaration(
    ConcreteDeclRef declRef) {
  auto decl = declRef.getDecl();

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
    // A function that provides an asynchronous context has no restrictions
    // on its access.
    if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
      if (func->isAsyncContext())
        return forUnrestricted();
    }

    // Local captures can only be referenced in their local context or a
    // context that is guaranteed not to run concurrently with it.
    if (cast<ValueDecl>(decl)->isLocalCapture()) {
      // Local functions are safe to capture; their bodies are checked based on
      // where that capture is used.
      if (isa<FuncDecl>(decl))
        return forUnrestricted();

      return forLocalCapture(decl->getDeclContext());
    }

    // Determine the actor isolation of the given declaration.
    switch (auto isolation = getActorIsolation(cast<ValueDecl>(decl))) {
    case ActorIsolation::ActorInstance:
      // Protected actor instance members can only be accessed on 'self'.
      return forActorSelf(isolation.getActor());

    case ActorIsolation::GlobalActor: {
      Type actorType = isolation.getGlobalActor();
      if (auto subs = declRef.getSubstitutions())
        actorType = actorType.subst(subs);

      return forGlobalActor(actorType);
    }

    case ActorIsolation::Independent:
    case ActorIsolation::IndependentUnsafe:
      // Actor-independent have no restrictions on their access.
      return forUnrestricted();

    case ActorIsolation::Unspecified:
      return forUnsafe();
    }
  }
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
static Optional<std::pair<ConcreteDeclRef, SourceLoc>>
findMemberReference(Expr *expr) {
  if (auto declRef = dyn_cast<DeclRefExpr>(expr))
    return std::make_pair(declRef->getDeclRef(), declRef->getLoc());

  if (auto otherCtor = dyn_cast<OtherConstructorDeclRefExpr>(expr)) {
    return std::make_pair(otherCtor->getDeclRef(), otherCtor->getLoc());
  }

  return None;
}

/// Return true if the callee of an ApplyExpr is async
///
/// Note that this must be called after the implicitlyAsync flag has been set,
/// or implicitly async calls will not return the correct value.
static bool isAsyncCall(const ApplyExpr *call) {
  if (call->implicitlyAsync())
    return true;

  // Effectively the same as doing a
  // `cast_or_null<FunctionType>(call->getFn()->getType())`, check the
  // result of that and then checking `isAsync` if it's defined.
  Type funcTypeType = call->getFn()->getType();
  if (!funcTypeType)
    return false;
  AnyFunctionType *funcType = funcTypeType->getAs<AnyFunctionType>();
  if (!funcType)
    return false;
  return funcType->isAsync();
}

/// Determine whether we should diagnose data races within the current context.
///
/// By default, we do this only in code that makes use of concurrency
/// features.
static bool shouldDiagnoseExistingDataRaces(const DeclContext *dc);

/// Determine whether this closure is escaping.
static bool isEscapingClosure(const AbstractClosureExpr *closure) {
  if (auto type = closure->getType()) {
    if (auto fnType = type->getAs<AnyFunctionType>())
      return !fnType->isNoEscape();
  }

  return true;
}

namespace {
  /// Check whether a particular context may execute concurrently within
  /// another context.
  class ConcurrentExecutionChecker {
    /// Keeps track of the first location at which a given local function is
    /// referenced from a context that may execute concurrently with the
    /// context in which it was introduced.
    llvm::SmallDenseMap<const FuncDecl *, SourceLoc, 4> concurrentRefs;

  public:
    /// Determine whether (and where) a given local function is referenced
    /// from a context that may execute concurrently with the context in
    /// which it is declared.
    ///
    /// \returns the source location of the first reference to the local
    /// function that may be concurrent. If the result is an invalid
    /// source location, there are no such references.
    SourceLoc getConcurrentReferenceLoc(const FuncDecl *localFunc);

    /// Determine whether code in the given use context might execute
    /// concurrently with code in the definition context.
    bool mayExecuteConcurrentlyWith(
      const DeclContext *useContext, const DeclContext *defContext);
  };

  /// Check for adherence to the actor isolation rules, emitting errors
  /// when actor-isolated declarations are used in an unsafe manner.
  class ActorIsolationChecker : public ASTWalker {
    ASTContext &ctx;
    SmallVector<const DeclContext *, 4> contextStack;
    SmallVector<ApplyExpr*, 4> applyStack;

    ConcurrentExecutionChecker concurrentExecutionChecker;

    const DeclContext *getDeclContext() const {
      return contextStack.back();
    }

    /// Determine whether code in the given use context might execute
    /// concurrently with code in the definition context.
    bool mayExecuteConcurrentlyWith(
      const DeclContext *useContext, const DeclContext *defContext) {
      return concurrentExecutionChecker.mayExecuteConcurrentlyWith(
          useContext, defContext);
    }

  public:
    ActorIsolationChecker(const DeclContext *dc) : ctx(dc->getASTContext()) {
      contextStack.push_back(dc);
    }

    /// Searches the applyStack from back to front for the inner-most CallExpr
    /// and marks that CallExpr as implicitly async. 
    ///
    /// NOTE: Crashes if no CallExpr was found.
    /// 
    /// For example, for global actor function `curryAdd`, if we have:
    ///     ((curryAdd 1) 2)
    /// then we want to mark the inner-most CallExpr, `(curryAdd 1)`.
    ///
    /// The same goes for calls to member functions, such as calc.add(1, 2),
    /// aka ((add calc) 1 2), looks like this:
    /// 
    ///  (call_expr
    ///    (dot_syntax_call_expr
    ///      (declref_expr add)
    ///      (declref_expr calc))
    ///    (tuple_expr 
    ///      ...))
    ///
    /// and we reach up to mark the CallExpr.
    void markNearestCallAsImplicitlyAsync() {
      assert(applyStack.size() > 0 && "not contained within an Apply?");

      const auto End = applyStack.rend();
      for (auto I = applyStack.rbegin(); I != End; ++I)
        if (auto call = dyn_cast<CallExpr>(*I)) {
          call->setImplicitlyAsync(true);
          return;
        }
      llvm_unreachable("expected a CallExpr in applyStack!");
    }

    bool shouldWalkCaptureInitializerExpressions() override { return true; }

    bool shouldWalkIntoTapExpression() override { return false; }

    bool walkToDeclPre(Decl *decl) override {
      if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
        contextStack.push_back(func);
      }

      return true;
    }

    bool walkToDeclPost(Decl *decl) override {
      if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
        assert(contextStack.back() == func);
        contextStack.pop_back();
      }

      return true;
    }

    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      if (auto *closure = dyn_cast<AbstractClosureExpr>(expr)) {
        closure->setActorIsolation(determineClosureIsolation(closure));
        contextStack.push_back(closure);
        return { true, expr };
      }

      if (auto inout = dyn_cast<InOutExpr>(expr)) {
        if (!applyStack.empty())
          diagnoseInOutArg(applyStack.back(), inout, false);
      }

      if (auto lookup = dyn_cast<LookupExpr>(expr)) {
        checkMemberReference(lookup->getBase(), lookup->getMember(),
                             lookup->getLoc());
        return { true, expr };
      }

      if (auto declRef = dyn_cast<DeclRefExpr>(expr)) {
        checkNonMemberReference(declRef->getDeclRef(), declRef->getLoc());
        return { true, expr };
      }

      if (auto apply = dyn_cast<ApplyExpr>(expr)) {
        applyStack.push_back(apply);  // record this encounter

        // If this is a call to a partial apply thunk, decompose it to check it
        // like based on the original written syntax, e.g., "self.method".
        if (auto partialApply = decomposePartialApplyThunk(
                apply, Parent.getAsExpr())) {
          if (auto memberRef = findMemberReference(partialApply->fn)) {
            // NOTE: partially-applied thunks are never annotated as 
            // implicitly async, regardless of whether they are escaping.
            checkMemberReference(
                partialApply->base, memberRef->first, memberRef->second,
                partialApply->isEscaping, /*maybeImplicitAsync=*/false);

            partialApply->base->walk(*this);

            // manual clean-up since normal traversal is skipped
            assert(applyStack.back() == apply);
            applyStack.pop_back();

            return { false, expr };
          }
        }
      }

      // NOTE: SelfApplyExpr is a subtype of ApplyExpr
      if (auto call = dyn_cast<SelfApplyExpr>(expr)) {
        Expr *fn = call->getFn()->getValueProvidingExpr();
        if (auto memberRef = findMemberReference(fn)) {
          checkMemberReference(
              call->getArg(), memberRef->first, memberRef->second,
              /*isEscapingPartialApply=*/false, /*maybeImplicitAsync=*/true);

          call->getArg()->walk(*this);

          if (applyStack.size() >= 2) {
            ApplyExpr *outerCall = applyStack[applyStack.size() - 2];
            if (isAsyncCall(outerCall)) {
              // This call is a partial application within an async call.
              // If the partial application take a value inout, it is bad.
              if (InOutExpr *inoutArg = dyn_cast<InOutExpr>(
                      call->getArg()->getSemanticsProvidingExpr()))
                diagnoseInOutArg(outerCall, inoutArg, true);
            }
          }

          // manual clean-up since normal traversal is skipped
          assert(applyStack.back() == dyn_cast<ApplyExpr>(expr));
          applyStack.pop_back();

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

      if (auto *apply = dyn_cast<ApplyExpr>(expr)) {
        assert(applyStack.back() == apply);
        applyStack.pop_back();
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
      // FIXME: Make this diagnostic more sensitive to the isolation context
      // of the declaration.
      if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
        func->diagnose(diag::actor_isolated_sync_func, 
          decl->getDescriptiveKind(),
          decl->getName());
      } else if (isa<VarDecl>(decl)) {
        decl->diagnose(diag::actor_mutable_state);
      } else {
        decl->diagnose(diag::kind_declared_here, decl->getDescriptiveKind());
      }
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
    bool diagnoseReferenceToUnsafeGlobal(ValueDecl *value, SourceLoc loc) {
      if (!shouldDiagnoseExistingDataRaces(getDeclContext()))
        return false;

      // Only diagnose direct references to mutable global state.
      auto var = dyn_cast<VarDecl>(value);
      if (!var || var->isLet())
        return false;

      if (!var->getDeclContext()->isModuleScopeContext() && !var->isStatic())
        return false;

      ctx.Diags.diagnose(
          loc, diag::shared_mutable_state_access,
          value->getDescriptiveKind(), value->getName());
      value->diagnose(diag::kind_declared_here, value->getDescriptiveKind());
      return true;
    }

    /// Diagnose an inout argument passed into an async call
    ///
    /// \returns true if we diagnosed the entity, \c false otherwise.
    bool diagnoseInOutArg(const ApplyExpr *call, const InOutExpr *arg,
                          bool isPartialApply) {
      // check that the call is actually async
      if (!isAsyncCall(call))
        return false;

      Expr *subArg = arg->getSubExpr();
      ValueDecl *valueDecl = nullptr;
      if (LookupExpr *baseArg = dyn_cast<LookupExpr>(subArg)) {
        while (LookupExpr *nextLayer = dyn_cast<LookupExpr>(baseArg->getBase()))
          baseArg = nextLayer;
        // subArg: the actual property being passed inout
        // baseArg: the property in the actor who's property is being passed
        // inout

        valueDecl = baseArg->getMember().getDecl();
      } else if (DeclRefExpr *declExpr = dyn_cast<DeclRefExpr>(subArg)) {
        valueDecl = declExpr->getDecl();
      } else {
        llvm_unreachable("Inout argument is neither a lookup nor decl.");
      }
      assert(valueDecl != nullptr && "valueDecl was never set!");
      auto isolation = ActorIsolationRestriction::forDeclaration(valueDecl);
      switch (isolation) {
      case ActorIsolationRestriction::Unrestricted:
      case ActorIsolationRestriction::LocalCapture:
      case ActorIsolationRestriction::Unsafe:
        break;
      case ActorIsolationRestriction::GlobalActor: {
        ctx.Diags.diagnose(call->getLoc(),
                           diag::actor_isolated_inout_state,
                           valueDecl->getDescriptiveKind(),
                           valueDecl->getName(),
                           call->implicitlyAsync());
        valueDecl->diagnose(diag::kind_declared_here,
                            valueDecl->getDescriptiveKind());
        return true;
      }
      case ActorIsolationRestriction::ActorSelf: {
        if (isPartialApply) {
          // The partially applied InoutArg is a property of actor. This can
          // really only happen when the property is a struct with a mutating
          // async method.
          if (auto partialApply = dyn_cast<ApplyExpr>(call->getFn())) {
            ValueDecl *fnDecl =
                cast<DeclRefExpr>(partialApply->getFn())->getDecl();
            ctx.Diags.diagnose(
                call->getLoc(), diag::actor_isolated_mutating_func,
                fnDecl->getName(), valueDecl->getDescriptiveKind(),
                valueDecl->getName());
            return true;
          }
        } else {
          ctx.Diags.diagnose(
              subArg->getLoc(), diag::actor_isolated_inout_state,
              valueDecl->getDescriptiveKind(), valueDecl->getName(), call->implicitlyAsync());
          return true;
        }
      }
      }
      return false;
    }

    /// Get the actor isolation of the innermost relevant context.
    ActorIsolation getInnermostIsolatedContext(const DeclContext *constDC) {
      // Retrieve the actor isolation for a declaration somewhere in our
      // declaration context chain and map it into our own context so that
      // the types can be compared.
      auto getActorIsolation = [constDC](ValueDecl *value) {
        switch (auto isolation = swift::getActorIsolation(value)) {
        case ActorIsolation::ActorInstance:
        case ActorIsolation::Independent:
        case ActorIsolation::IndependentUnsafe:
        case ActorIsolation::Unspecified:
          return isolation;

        case ActorIsolation::GlobalActor:
          return ActorIsolation::forGlobalActor(
              constDC->mapTypeIntoContext(isolation.getGlobalActor()));
        }
      };

      auto dc = const_cast<DeclContext *>(constDC);
      while (!dc->isModuleScopeContext()) {
        // Look through non-escaping closures.
        if (auto closure = dyn_cast<AbstractClosureExpr>(dc)) {
          if (auto type = closure->getType()) {
            if (auto fnType = type->getAs<AnyFunctionType>()) {
              if (fnType->isNoEscape()) {
                dc = closure->getParent();
                continue;
              }
            }
          }
        }

        // Functions have actor isolation defined on them.
        if (auto func = dyn_cast<AbstractFunctionDecl>(dc))
          return getActorIsolation(func);

        // Subscripts have actor isolation defined on them.
        if (auto subscript = dyn_cast<SubscriptDecl>(dc))
          return getActorIsolation(subscript);

        // Pattern binding declarations have actor isolation defined on their
        // properties, if any.
        if (auto init = dyn_cast<PatternBindingInitializer>(dc)) {
          auto var = init->getBinding()->getAnchoringVarDecl(
              init->getBindingIndex());
          if (var)
            return getActorIsolation(var);

          return ActorIsolation::forUnspecified();
        }

        return ActorIsolation::forUnspecified();
      }

      // At module scope, actor independence with safety is assumed.
      return ActorIsolation::forIndependent(ActorIndependentKind::Safe);
    }

    /// Check a reference to an entity within a global actor.
    bool checkGlobalActorReference(
        ValueDecl *value, SourceLoc loc, Type globalActor) {

      /// Returns true if this global actor reference is the callee of an Apply.
      /// NOTE: This check mutates the identified ApplyExpr if it returns true!
      auto inspectForImplicitlyAsync = [&] () -> bool {

        // Is this global actor reference outside of an ApplyExpr?
        if (applyStack.size() == 0)
          return false;

        // Check our applyStack metadata from the traversal.
        // Our goal is to identify whether this global actor reference appears
        // as the called value of the enclosing ApplyExpr. We cannot simply
        // inspect Parent here because of expressions like (callee)()
        ApplyExpr *apply = applyStack.back();
        Expr *fn = apply->getFn()->getValueProvidingExpr();
        if (auto memberRef = findMemberReference(fn)) {
          auto concDecl = memberRef->first;
          if (value == concDecl.getDecl() && !apply->implicitlyAsync()) {
            // then this ValueDecl appears as the called value of the ApplyExpr.
            markNearestCallAsImplicitlyAsync();
            return true;
          }
        }

        return false;
      };

      auto declContext = getDeclContext();
      switch (auto contextIsolation =
                  getInnermostIsolatedContext(declContext)) {
      case ActorIsolation::ActorInstance:
        if (inspectForImplicitlyAsync())
          return false;

        ctx.Diags.diagnose(
            loc, diag::global_actor_from_instance_actor_context,
            value->getDescriptiveKind(), value->getName(), globalActor,
            contextIsolation.getActor()->getName());
        noteIsolatedActorMember(value);
        return true;

      case ActorIsolation::GlobalActor: {
        // If the global actor types are the same, we're done.
        if (contextIsolation.getGlobalActor()->isEqual(globalActor))
          return false;

        // Otherwise, we check if this decl reference is the callee of the
        // enclosing Apply, making it OK as an implicitly async call.
        if (inspectForImplicitlyAsync())
          return false;

        // Otherwise, this is a problematic global actor decl reference.
        ctx.Diags.diagnose(
            loc, diag::global_actor_from_other_global_actor_context,
            value->getDescriptiveKind(), value->getName(), globalActor,
            contextIsolation.getGlobalActor());
        noteIsolatedActorMember(value);
        return true;
      }

      case ActorIsolation::Independent:
      case ActorIsolation::IndependentUnsafe:
        if (inspectForImplicitlyAsync())
          return false;

        ctx.Diags.diagnose(
            loc, diag::global_actor_from_nonactor_context,
            value->getDescriptiveKind(), value->getName(), globalActor,
            /*actorIndependent=*/true);
        noteIsolatedActorMember(value);
        return true;

      case ActorIsolation::Unspecified: {
        // NOTE: we must always inspect for implicitlyAsync
        bool implicitlyAsyncCall = inspectForImplicitlyAsync();
        bool didEmitDiagnostic = false;

        auto emitError = [&](bool justNote = false) {
          didEmitDiagnostic = true;
          if (!justNote) {
            ctx.Diags.diagnose(
              loc, diag::global_actor_from_nonactor_context,
              value->getDescriptiveKind(), value->getName(), globalActor,
              /*actorIndependent=*/false);
          }
          noteIsolatedActorMember(value);
        };
        
        if (AbstractFunctionDecl const* fn = 
            dyn_cast_or_null<AbstractFunctionDecl>(declContext->getAsDecl())) {
          bool isAsyncContext = fn->isAsyncContext();

          if (implicitlyAsyncCall && isAsyncContext)
            return didEmitDiagnostic; // definitely an OK reference.

          // otherwise, there's something wrong.
          
          // if it's an implicitly-async call in a non-async context,
          // then we know later type-checking will raise an error,
          // so we just emit a note pointing out that callee of the call is
          // implicitly async.
          emitError(/*justNote=*/implicitlyAsyncCall);

          // otherwise, if it's any kind of global-actor reference within
          // this synchronous function, we'll additionally suggest becoming
          // part of the global actor associated with the reference,
          // since this function is not associated with an actor.
          if (isa<FuncDecl>(fn) && !isAsyncContext) {
            didEmitDiagnostic = true;
            fn->diagnose(diag::note_add_globalactor_to_function, 
                globalActor->getWithoutParens().getString(),
                fn->getDescriptiveKind(),
                fn->getName(),
                globalActor)
              .fixItInsert(fn->getAttributeInsertionLoc(false), 
                diag::insert_globalactor_attr, globalActor);
          }

        } else {
          // just the generic error with note.
          emitError();
        }

        return didEmitDiagnostic;
      } // end Unspecified case
      } // end switch
      llvm_unreachable("unhandled actor isolation kind!");
    }

    /// Check a reference to a local or global.
    bool checkNonMemberReference(ConcreteDeclRef valueRef, SourceLoc loc) {
      if (!valueRef)
        return false;

      auto value = valueRef.getDecl();
      switch (auto isolation =
                  ActorIsolationRestriction::forDeclaration(valueRef)) {
      case ActorIsolationRestriction::Unrestricted:
        return false;

      case ActorIsolationRestriction::ActorSelf:
        llvm_unreachable("non-member reference into an actor");

      case ActorIsolationRestriction::GlobalActor:
        return checkGlobalActorReference(
            value, loc, isolation.getGlobalActor());

      case ActorIsolationRestriction::LocalCapture:
        if (!shouldDiagnoseExistingDataRaces(getDeclContext()))
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

      case ActorIsolationRestriction::Unsafe:
        return diagnoseReferenceToUnsafeGlobal(value, loc);
      }
      llvm_unreachable("unhandled actor isolation kind!");
    }

    /// Check a reference with the given base expression to the given member.
    /// Returns true iff the member reference refers to actor-isolated state
    /// in an invalid or unsafe way such that a diagnostic was emitted.
    bool checkMemberReference(
        Expr *base, ConcreteDeclRef memberRef, SourceLoc memberLoc,
        bool isEscapingPartialApply = false, 
        bool maybeImplicitAsync = false) {
      if (!base || !memberRef)
        return false;

      auto member = memberRef.getDecl();
      switch (auto isolation =
                  ActorIsolationRestriction::forDeclaration(memberRef)) {
      case ActorIsolationRestriction::Unrestricted:
        return false;

      case ActorIsolationRestriction::ActorSelf: {
        // Must reference actor-isolated state on 'self'.
        auto selfVar = getSelfReference(base);
        if (!selfVar) {
          // actor-isolated non-self calls are implicitly async and thus OK.
          if (maybeImplicitAsync && isa<AbstractFunctionDecl>(member)) {
            markNearestCallAsImplicitlyAsync();
            return false;
          }
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
        switch (auto contextIsolation = getActorIsolation(
                   cast<ValueDecl>(selfVar->getDeclContext()->getAsDecl()))) {
          case ActorIsolation::ActorInstance:
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

          case ActorIsolation::IndependentUnsafe:
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

          case ActorIsolation::GlobalActor:
            // The 'self' is for a member that's part of a global actor, which
            // means we cannot refer to actor-isolated state.
            ctx.Diags.diagnose(
                memberLoc, diag::actor_isolated_global_actor_context,
                member->getDescriptiveKind(),
                member->getName(),
                contextIsolation.getGlobalActor());
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

      case ActorIsolationRestriction::GlobalActor:
        return checkGlobalActorReference(
            member, memberLoc, isolation.getGlobalActor());

      case ActorIsolationRestriction::LocalCapture:
        llvm_unreachable("Locals cannot be referenced with member syntax");

      case ActorIsolationRestriction::Unsafe:
        // This case is hit when passing actor state inout to functions in some
        // cases. The error is emitted by diagnoseInOutArg.
        return false;
      }
      llvm_unreachable("unhandled actor isolation kind!");
    }

    /// Determine the isolation of a particular closure.
    ///
    /// This function assumes that enclosing closures have already had their
    /// isolation checked.
    ClosureActorIsolation determineClosureIsolation(
        AbstractClosureExpr *closure) {
      // An escaping closure is always actor-independent.
      if (isEscapingClosure(closure))
        return ClosureActorIsolation::forIndependent();

      // A non-escaping closure gets its isolation from its context.
      Optional<ActorIsolation> parentIsolation;
      auto parentDC = closure->getParent();
      switch (parentDC->getContextKind()) {
      case DeclContextKind::AbstractClosureExpr: {
        auto parentClosureIsolation = cast<AbstractClosureExpr>(parentDC)
          ->getActorIsolation();
        switch (parentClosureIsolation) {
        case ClosureActorIsolation::Independent:
          parentIsolation = ActorIsolation::forIndependent(
              ActorIndependentKind::Safe);
          break;

        case ClosureActorIsolation::ActorInstance: {
          auto selfDecl = parentClosureIsolation.getActorInstance();
          auto actorClass = selfDecl->getType()->getRValueType()
              ->getClassOrBoundGenericClass();
          assert(actorClass && "Bad closure actor isolation?");
          parentIsolation = ActorIsolation::forActorInstance(actorClass);
          break;
        }

        case ClosureActorIsolation::GlobalActor:
          parentIsolation = ActorIsolation::forGlobalActor(
              parentClosureIsolation.getGlobalActor());
          break;
        }
        break;
      }

      case DeclContextKind::AbstractFunctionDecl:
      case DeclContextKind::SubscriptDecl:
        parentIsolation = getActorIsolation(
            cast<ValueDecl>(parentDC->getAsDecl()));
        break;

      case DeclContextKind::EnumElementDecl:
      case DeclContextKind::ExtensionDecl:
      case DeclContextKind::FileUnit:
      case DeclContextKind::GenericTypeDecl:
      case DeclContextKind::Initializer:
      case DeclContextKind::Module:
      case DeclContextKind::SerializedLocal:
      case DeclContextKind::TopLevelCodeDecl:
        return ClosureActorIsolation::forIndependent();
      }

      // We must have parent isolation determined to get here.
      assert(parentIsolation && "Missing parent isolation?");
      switch (*parentIsolation) {
      case ActorIsolation::Independent:
      case ActorIsolation::IndependentUnsafe:
      case ActorIsolation::Unspecified:
        return ClosureActorIsolation::forIndependent();

      case ActorIsolation::GlobalActor: {
        Type globalActorType = closure->mapTypeIntoContext(
            parentIsolation->getGlobalActor()->mapTypeOutOfContext());
        return ClosureActorIsolation::forGlobalActor(globalActorType);
      }

      case ActorIsolation::ActorInstance: {
        SmallVector<CapturedValue, 2> localCaptures;
        closure->getCaptureInfo().getLocalCaptures(localCaptures);
        for (const auto &localCapture : localCaptures) {
          if (localCapture.isDynamicSelfMetadata())
            continue;

          auto var = dyn_cast_or_null<VarDecl>(localCapture.getDecl());
          if (!var)
            continue;

          // If we have captured the 'self' parameter, the closure is isolated
          // to that actor instance.
          if (var->isSelfParameter()) {
            return ClosureActorIsolation::forActorInstance(var);
          }
        }

        // When 'self' is not captured, this closure is actor-independent.
        return ClosureActorIsolation::forIndependent();
      }
    }
    }
  };
}

SourceLoc ConcurrentExecutionChecker::getConcurrentReferenceLoc(
    const FuncDecl *localFunc) {

  // If we've already computed a result, we're done.
  auto known = concurrentRefs.find(localFunc);
  if (known != concurrentRefs.end())
    return known->second;

  // Record that there are no concurrent references to this local function. This
  // prevents infinite recursion if two local functions call each other.
  concurrentRefs[localFunc] = SourceLoc();

  class ConcurrentLocalRefWalker : public ASTWalker {
    ConcurrentExecutionChecker &checker;
    const FuncDecl *targetFunc;
    SmallVector<const DeclContext *, 4> contextStack;

    const DeclContext *getDeclContext() const {
      return contextStack.back();
    }

  public:
    ConcurrentLocalRefWalker(
      ConcurrentExecutionChecker &checker, const FuncDecl *targetFunc
    ) : checker(checker), targetFunc(targetFunc) {
      contextStack.push_back(targetFunc->getDeclContext());
    }

    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      if (auto *closure = dyn_cast<AbstractClosureExpr>(expr)) {
        contextStack.push_back(closure);
        return { true, expr };
      }

      if (auto *declRef = dyn_cast<DeclRefExpr>(expr)) {
        // If this is a reference to the target function from a context
        // that may execute concurrently with the context where the target
        // function was declared, record the location.
        if (declRef->getDecl() == targetFunc &&
            checker.mayExecuteConcurrentlyWith(
              getDeclContext(), contextStack.front())) {
          SourceLoc &loc = checker.concurrentRefs[targetFunc];
          if (loc.isInvalid())
            loc = declRef->getLoc();

          return { false, expr };
        }

        return { true, expr };
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

    bool walkToDeclPre(Decl *decl) override {
      if (isa<NominalTypeDecl>(decl) || isa<ExtensionDecl>(decl))
        return false;

      if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
        contextStack.push_back(func);
      }

      return true;
    }

    bool walkToDeclPost(Decl *decl) override {
      if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
        assert(contextStack.back() == func);
        contextStack.pop_back();
      }

      return true;
    }
  };

  // Walk the body of the enclosing function, where all references to the
  // given local function would occur.
  Stmt *enclosingBody = nullptr;
  DeclContext *enclosingDC = localFunc->getDeclContext();
  if (auto enclosingFunc = dyn_cast<AbstractFunctionDecl>(enclosingDC))
    enclosingBody = enclosingFunc->getBody();
  else if (auto enclosingClosure = dyn_cast<ClosureExpr>(enclosingDC))
    enclosingBody = enclosingClosure->getBody();

  assert(enclosingBody && "Cannot have a local function here");
  ConcurrentLocalRefWalker walker(*this, localFunc);
  enclosingBody->walk(walker);

  return concurrentRefs[localFunc];
}

bool ConcurrentExecutionChecker::mayExecuteConcurrentlyWith(
    const DeclContext *useContext, const DeclContext *defContext) {
  // Walk the context chain from the use to the definition.
  while (useContext != defContext) {
    // If we find an escaping closure, it can be run concurrently.
    if (auto closure = dyn_cast<AbstractClosureExpr>(useContext)) {
      if (isEscapingClosure(closure))
        return true;
    }

    // If we find a local function that was referenced in code that can be
    // executed concurrently with where the local function was declared, the
    // local function can be run concurrently.
    if (auto func = dyn_cast<FuncDecl>(useContext)) {
      if (func->isLocalCapture()) {
        SourceLoc concurrentLoc = getConcurrentReferenceLoc(func);
        if (concurrentLoc.isValid())
          return true;
      }
    }

    // If we hit a module-scope or type context context, it's not
    // concurrent.
    useContext = useContext->getParent();
    if (useContext->isModuleScopeContext() || useContext->isTypeContext())
      return false;
  }

  // We hit the same context, so it won't execute concurrently.
  return false;
}

void swift::checkTopLevelActorIsolation(TopLevelCodeDecl *decl) {
  ActorIsolationChecker checker(decl);
  decl->getBody()->walk(checker);
}

void swift::checkFunctionActorIsolation(AbstractFunctionDecl *decl) {
  ActorIsolationChecker checker(decl);
  if (auto body = decl->getBody()) {
    body->walk(checker);
  }
  if (auto ctor = dyn_cast<ConstructorDecl>(decl))
    if (auto superInit = ctor->getSuperInitCall())
      superInit->walk(checker);
}

void swift::checkInitializerActorIsolation(Initializer *init, Expr *expr) {
  ActorIsolationChecker checker(init);
  expr->walk(checker);
}

void swift::checkEnumElementActorIsolation(
    EnumElementDecl *element, Expr *expr) {
  ActorIsolationChecker checker(element);
  expr->walk(checker);
}

void swift::checkPropertyWrapperActorIsolation(
   PatternBindingDecl *binding, Expr *expr) {
  ActorIsolationChecker checker(binding->getDeclContext());
  expr->walk(checker);
}

/// Determine actor isolation solely from attributes.
///
/// \returns the actor isolation determined from attributes alone (with no
/// inference rules). Returns \c None if there were no attributes on this
/// declaration.
static Optional<ActorIsolation> getIsolationFromAttributes(
    const Decl *decl, bool shouldDiagnose = true) {
  // Look up attributes on the declaration that can affect its actor isolation.
  // If any of them are present, use that attribute.
  auto independentAttr = decl->getAttrs().getAttribute<ActorIndependentAttr>();
  auto globalActorAttr = decl->getGlobalActorAttr();
  unsigned numIsolationAttrs =
    (independentAttr ? 1 : 0) + (globalActorAttr ? 1 : 0);
  if (numIsolationAttrs == 0)
    return None;

  // Only one such attribute is valid.
  if (numIsolationAttrs > 1) {
    DeclName name;
    if (auto value = dyn_cast<ValueDecl>(decl)) {
      name = value->getName();
    } else if (auto ext = dyn_cast<ExtensionDecl>(decl)) {
      if (auto selfTypeDecl = ext->getSelfNominalTypeDecl())
        name = selfTypeDecl->getName();
    }

    if (shouldDiagnose) {
      decl->diagnose(
          diag::actor_isolation_multiple_attr, decl->getDescriptiveKind(),
          name, independentAttr->getAttrName(),
          globalActorAttr->second->getName().str())
        .highlight(independentAttr->getRangeWithAt())
        .highlight(globalActorAttr->first->getRangeWithAt());
    }
  }

  // If the declaration is explicitly marked @actorIndependent, report it as
  // independent.
  if (independentAttr) {
    return ActorIsolation::forIndependent(independentAttr->getKind());
  }

  // If the declaration is marked with a global actor, report it as being
  // part of that global actor.
  if (globalActorAttr) {
    ASTContext &ctx = decl->getASTContext();
    auto dc = decl->getInnermostDeclContext();
    Type globalActorType = evaluateOrDefault(
        ctx.evaluator,
        CustomAttrTypeRequest{
          globalActorAttr->first, dc, CustomAttrTypeKind::GlobalActor},
        Type());
    if (!globalActorType || globalActorType->hasError())
      return ActorIsolation::forUnspecified();

    return ActorIsolation::forGlobalActor(
        globalActorType->mapTypeOutOfContext());
  }

  llvm_unreachable("Forgot about an attribute?");
}

/// Infer isolation from witnessed protocol requirements.
static Optional<ActorIsolation> getIsolationFromWitnessedRequirements(
    ValueDecl *value) {
  auto dc = value->getDeclContext();
  auto idc = dyn_cast_or_null<IterableDeclContext>(dc->getAsDecl());
  if (!idc)
    return None;

  if (dc->getSelfProtocolDecl())
    return None;

  // Walk through each of the conformances in this context, collecting any
  // requirements that have actor isolation.
  auto conformances = evaluateOrDefault(
      dc->getASTContext().evaluator,
      LookupAllConformancesInContextRequest{idc}, { });
  using IsolatedRequirement =
      std::tuple<ProtocolConformance *, ActorIsolation, ValueDecl *>;
  SmallVector<IsolatedRequirement, 2> isolatedRequirements;
  for (auto conformance : conformances) {
    auto protocol = conformance->getProtocol();
    for (auto found : protocol->lookupDirect(value->getName())) {
      if (!isa<ProtocolDecl>(found->getDeclContext()))
        continue;

      auto requirement = dyn_cast<ValueDecl>(found);
      if (!requirement || isa<TypeDecl>(requirement))
        continue;

      auto requirementIsolation = getActorIsolation(requirement);
      if (requirementIsolation.isUnspecified())
        continue;

      auto witness = conformance->getWitnessDecl(requirement);
      if (witness != value)
        continue;

      isolatedRequirements.push_back(
          IsolatedRequirement{conformance, requirementIsolation, requirement});
    }
  }

  // Filter out duplicate actors.
  SmallPtrSet<CanType, 2> globalActorTypes;
  bool sawActorIndependent = false;
  isolatedRequirements.erase(
      std::remove_if(isolatedRequirements.begin(), isolatedRequirements.end(),
                     [&](IsolatedRequirement &isolated) {
    auto isolation = std::get<1>(isolated);
    switch (isolation) {
      case ActorIsolation::ActorInstance:
        llvm_unreachable("protocol requirements cannot be actor instances");

      case ActorIsolation::Independent:
      case ActorIsolation::IndependentUnsafe:
        // We only need one @actorIndependent.
        if (sawActorIndependent)
          return true;

        sawActorIndependent = true;
        return false;

      case ActorIsolation::Unspecified:
        return true;

      case ActorIsolation::GlobalActor: {
        // Substitute into the global actor type.
        auto conformance = std::get<0>(isolated);
        auto requirementSubs = SubstitutionMap::getProtocolSubstitutions(
            conformance->getProtocol(), dc->getSelfTypeInContext(),
            ProtocolConformanceRef(conformance));
        Type globalActor = isolation.getGlobalActor().subst(requirementSubs);
        if (!globalActorTypes.insert(globalActor->getCanonicalType()).second)
          return true;

        // Update the global actor type, now that we've done this substitution.
        std::get<1>(isolated) = ActorIsolation::forGlobalActor(globalActor);
        return false;
      }
      }
      }),
      isolatedRequirements.end());

  if (isolatedRequirements.size() != 1)
    return None;

  return std::get<1>(isolatedRequirements.front());
}

ActorIsolation ActorIsolationRequest::evaluate(
    Evaluator &evaluator, ValueDecl *value) const {
  // If this declaration has one of the actor isolation attributes, report
  // that.
  if (auto isolationFromAttr = getIsolationFromAttributes(value)) {
    return *isolationFromAttr;
  }

  // Determine the default isolation for this declaration, which may still be
  // overridden by other inference rules.
  ActorIsolation defaultIsolation = ActorIsolation::forUnspecified();

  // Check for instance members of actor classes, which are part of
  // actor-isolated state.
  auto classDecl = value->getDeclContext()->getSelfClassDecl();
  if (classDecl && classDecl->isActor() && value->isInstanceMember()) {
    defaultIsolation = ActorIsolation::forActorInstance(classDecl);
  }

  // Disable inference of actor attributes outside of normal Swift source files.
  if (!shouldInferAttributeInContext(value->getDeclContext()))
    return defaultIsolation;

  // Function used when returning an inferred isolation.
  auto inferredIsolation = [&](ActorIsolation inferred) {
    // Add an implicit attribute to capture the actor isolation that was
    // inferred, so that (e.g.) it will be printed and serialized.
    ASTContext &ctx = value->getASTContext();
    switch (inferred) {
    // FIXME: if the context is 'unsafe', is it fine to infer the 'safe' one?
    case ActorIsolation::IndependentUnsafe:
    case ActorIsolation::Independent:
      value->getAttrs().add(new (ctx) ActorIndependentAttr(
                              ActorIndependentKind::Safe, /*IsImplicit=*/true));
      break;

    case ActorIsolation::GlobalActor: {
      auto typeExpr = TypeExpr::createImplicit(inferred.getGlobalActor(), ctx);
      auto attr = CustomAttr::create(
          ctx, SourceLoc(), typeExpr, /*implicit=*/true);
      value->getAttrs().add(attr);
      break;
    }

    case ActorIsolation::ActorInstance:
    case ActorIsolation::Unspecified:
      // Nothing to do.
      break;
    }
    return inferred;
  };

  // If the declaration overrides another declaration, it must have the same
  // actor isolation.
  if (auto overriddenValue = value->getOverriddenDecl()) {
    if (auto isolation = getActorIsolation(overriddenValue)) {
      SubstitutionMap subs;
      if (auto env = value->getInnermostDeclContext()
              ->getGenericEnvironmentOfContext()) {
        subs = SubstitutionMap::getOverrideSubstitutions(
            overriddenValue, value, subs);
      }

      return inferredIsolation(isolation.subst(subs));
    }
  }

  // If this is an accessor, use the actor isolation of its storage
  // declaration.
  if (auto accessor = dyn_cast<AccessorDecl>(value)) {
    return getActorIsolation(accessor->getStorage());
  }

  // If the declaration witnesses a protocol requirement that is isolated,
  // use that.
  if (auto witnessedIsolation = getIsolationFromWitnessedRequirements(value)) {
    return inferredIsolation(*witnessedIsolation);
  }

  // If the declaration is a class with a superclass that has specified
  // isolation, use that.
  if (auto classDecl = dyn_cast<ClassDecl>(value)) {
    if (auto superclassDecl = classDecl->getSuperclassDecl()) {
      auto superclassIsolation = getActorIsolation(superclassDecl);
      if (!superclassIsolation.isUnspecified()) {
        if (superclassIsolation.requiresSubstitution()) {
          Type superclassType = classDecl->getSuperclass();
          if (!superclassType)
            return ActorIsolation::forUnspecified();

          SubstitutionMap subs = superclassType->getMemberSubstitutionMap(
              classDecl->getModuleContext(), classDecl);
          superclassIsolation = superclassIsolation.subst(subs);
        }

        return inferredIsolation(superclassIsolation);
      }
    }
  }

  // If the declaration is in an extension that has one of the isolation
  // attributes, use that.
  if (auto ext = dyn_cast<ExtensionDecl>(value->getDeclContext())) {
    if (auto isolationFromAttr = getIsolationFromAttributes(ext)) {
      return inferredIsolation(*isolationFromAttr);
    }
  }

  // If the declaration is in a nominal type (or extension thereof) that
  // has isolation, use that.
  if (auto selfTypeDecl = value->getDeclContext()->getSelfNominalTypeDecl()) {
    auto selfTypeIsolation = getActorIsolation(selfTypeDecl);
    if (!selfTypeIsolation.isUnspecified()) {
      return inferredIsolation(selfTypeIsolation);
    }
  }

  // Default isolation for this member.
  return defaultIsolation;
}

ActorIsolation swift::getActorIsolation(ValueDecl *value) {
  auto &ctx = value->getASTContext();
  return evaluateOrDefault(
      ctx.evaluator, ActorIsolationRequest{value},
      ActorIsolation::forUnspecified());
}

void swift::checkOverrideActorIsolation(ValueDecl *value) {
  if (isa<TypeDecl>(value))
    return;

  auto overridden = value->getOverriddenDecl();
  if (!overridden)
    return;

  // Determine the actor isolation of this declaration.
  auto isolation = getActorIsolation(value);

  // Determine the actor isolation of the overridden function.=
  auto overriddenIsolation = getActorIsolation(overridden);

  if (overriddenIsolation.requiresSubstitution()) {
    SubstitutionMap subs;
    if (auto env = value->getInnermostDeclContext()
            ->getGenericEnvironmentOfContext()) {
      subs = SubstitutionMap::getOverrideSubstitutions(overridden, value, subs);
      overriddenIsolation = overriddenIsolation.subst(subs);
    }
  }

  // If the isolation matches, we're done.
  if (isolation == overriddenIsolation)
    return;

  // Isolation mismatch. Diagnose it.
  value->diagnose(
      diag::actor_isolation_override_mismatch, isolation,
      value->getDescriptiveKind(), value->getName(), overriddenIsolation);
  overridden->diagnose(diag::overridden_here);
}

static bool shouldDiagnoseExistingDataRaces(const DeclContext *dc) {
  while (!dc->isModuleScopeContext()) {
    if (auto closure = dyn_cast<AbstractClosureExpr>(dc)) {
      // Async closures use concurrency features.
      if (closure->getType() && closure->isBodyAsync())
        return true;
    } else if (auto decl = dc->getAsDecl()) {
      // If any isolation attributes are present, we're using concurrency
      // features.
      if (getIsolationFromAttributes(decl, /*shouldDiagnose=*/false))
        return true;

      if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
        // Async functions use concurrency features.
        if (func->hasAsync())
          return true;

        // If there is an explicit @asyncHandler, we're using concurrency
        // features.
        if (func->getAttrs().hasAttribute<AsyncHandlerAttr>())
          return true;
      }
    }

    // If we're in an actor, we're using concurrency features.
    if (auto classDecl = dc->getSelfClassDecl()) {
      if (classDecl->isActor())
        return true;
    }

    // Keep looking.
    dc = dc->getParent();
  }

  return false;
}
