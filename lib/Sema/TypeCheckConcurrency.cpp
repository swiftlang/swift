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
#include "NonisolatedNonsendingByDefaultMigration.h"
#include "MiscDiagnostics.h"
#include "TypeCheckDistributed.h"
#include "TypeCheckInvertible.h"
#include "TypeCheckProtocol.h"
#include "TypeCheckType.h"
#include "TypeChecker.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/AvailabilityInference.h"
#include "swift/AST/Concurrency.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/DistributedDecl.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/ImportCache.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/PackConformance.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/Assertions.h"
#include "swift/Sema/IDETypeChecking.h"
#include "swift/Strings.h"

using namespace swift;

static ActorIsolation getOverriddenIsolationFor(const ValueDecl *value);

/// Determine whether it makes sense to infer an attribute in the given
/// context.
static bool shouldInferAttributeInContext(const DeclContext *dc) {
  if (auto *file = dyn_cast<FileUnit>(dc->getModuleScopeContext())) {
    switch (file->getKind()) {
    case FileUnitKind::Source:
      // Check what kind of source file we have.
      if (auto sourceFile = dc->getParentSourceFile()) {
        switch (sourceFile->Kind) {
        case SourceFileKind::Interface:
          // Interfaces have explicitly called-out Sendable conformances.
          return false;

        case SourceFileKind::DefaultArgument:
        case SourceFileKind::Library:
        case SourceFileKind::MacroExpansion:
        case SourceFileKind::Main:
        case SourceFileKind::SIL:
          return true;
        }
      }
      break;

    case FileUnitKind::Builtin:
    case FileUnitKind::SerializedAST:
    case FileUnitKind::Synthesized:
      return false;

    case FileUnitKind::ClangModule:
    case FileUnitKind::DWARFModule:
      return true;
    }

    return true;
  }

  return false;
}

void swift::addAsyncNotes(AbstractFunctionDecl const* func) {
  assert(func);
  if (!isa<DestructorDecl>(func) && !isa<AccessorDecl>(func)) {
    auto note =
        func->diagnose(diag::note_add_async_to_function, func);

    if (func->hasThrows()) {
      auto replacement = func->getAttrs().hasAttribute<RethrowsAttr>()
                        ? "async rethrows"
                        : "async throws";

      note.fixItReplace(SourceRange(func->getThrowsLoc()), replacement);
    } else if (func->getParameters()->getRParenLoc().isValid()) {
      note.fixItInsert(func->getParameters()->getRParenLoc().getAdvancedLoc(1),
                       " async");
    }
  }
}

static bool requiresFlowIsolation(ActorIsolation typeIso,
                                  ConstructorDecl const *ctor) {
  assert(ctor->isDesignatedInit());

  auto ctorIso = getActorIsolation(const_cast<ConstructorDecl *>(ctor));

  // Regardless of async-ness, a mismatch in isolation means we need to be
  // flow-sensitive.
  if (typeIso != ctorIso)
    return true;

  // Otherwise, if it's an actor instance, then it depends on async-ness.
  switch (typeIso.getKind()) {
  case ActorIsolation::GlobalActor:
  case ActorIsolation::Unspecified:
  case ActorIsolation::Nonisolated:
  case ActorIsolation::NonisolatedUnsafe:
    return false;

  // TODO: We probably want constructors to always be truly non-isolated.
  case ActorIsolation::CallerIsolationInheriting:
    llvm_unreachable("constructor cannot have nonisolated implicit actor "
                     "instance isolation");
  case ActorIsolation::Erased:
    llvm_unreachable("constructor cannot have erased isolation");

  case ActorIsolation::ActorInstance:
      return !(ctor->hasAsync()); // need flow-isolation for non-async.
  };
}

bool swift::usesFlowSensitiveIsolation(AbstractFunctionDecl const *fn) {
  if (!fn)
    return false;

  // Only designated constructors or nonisolated destructors use this kind of
  // isolation.
  if (auto const* ctor = dyn_cast<ConstructorDecl>(fn)) {
    if (!ctor->isDesignatedInit())
      return false;
  } else if (auto const *dtor = dyn_cast<DestructorDecl>(fn)) {
    if (getActorIsolation(const_cast<DestructorDecl *>(dtor))
            .isActorIsolated()) {
      return false;
    }
  } else {
    return false;
  }

  auto *dc = fn->getDeclContext();
  if (!dc)
    return false;

  // Must be part of a nominal type.
  auto *nominal = dc->getSelfNominalTypeDecl();
  if (!nominal)
    return false;

  // If it's part of an actor type, then its deinit and some of its inits use
  // flow-isolation.
  if (nominal->isAnyActor()) {
    if (isa<DestructorDecl>(fn))
      return true;

    // construct an isolation corresponding to the type.
    auto actorTypeIso = ActorIsolation::forActorInstanceSelf(
        const_cast<AbstractFunctionDecl *>(fn));

    return requiresFlowIsolation(actorTypeIso, cast<ConstructorDecl>(fn));
  }

  // Otherwise, the type must be isolated to a global actor.
  auto nominalIso = getActorIsolation(nominal);
  if (!nominalIso.isGlobalActor())
    return false;

  // if it's a deinit, then it's flow-isolated.
  if (isa<DestructorDecl>(fn))
    return true;

  return requiresFlowIsolation(nominalIso, cast<ConstructorDecl>(fn));
}

bool IsActorRequest::evaluate(
    Evaluator &evaluator, NominalTypeDecl *nominal) const {
  // Protocols are actors if they inherit from `Actor`.
  if (auto protocol = dyn_cast<ProtocolDecl>(nominal)) {
    auto &ctx = protocol->getASTContext();
    auto *actorProtocol = ctx.getProtocol(KnownProtocolKind::Actor);
    if (!actorProtocol)
      return false;

    return (protocol == actorProtocol ||
            protocol->inheritsFrom(actorProtocol));
  }

  // Class declarations are actors if they were declared with "actor".
  auto classDecl = dyn_cast<ClassDecl>(nominal);
  if (!classDecl)
    return false;

  return classDecl->isExplicitActor();
}

bool IsDefaultActorRequest::evaluate(
    Evaluator &evaluator, ClassDecl *classDecl, ModuleDecl *M,
    ResilienceExpansion expansion) const {
  // If the class isn't an actor, it's not a default actor.
  if (!classDecl->isActor())
    return false;

  // Distributed actors were not able to have custom executors until Swift 5.9,
  // so in order to avoid wrongly treating a resilient distributed actor from another
  // module as not-default we need to handle this case explicitly.
  if (classDecl->isDistributedActor()) {
    ASTContext &ctx = classDecl->getASTContext();
    auto customExecutorAvailability =
        ctx.getConcurrencyDistributedActorWithCustomExecutorAvailability();

    auto actorAvailability =
        AvailabilityContext::forDeclSignature(classDecl).getPlatformRange();

    if (!actorAvailability.isContainedIn(customExecutorAvailability)) {
      // Any 'distributed actor' declared with availability lower than the
      // introduction of custom executors for distributed actors, must be treated as default actor,
      // even if it were to declared the unowned executor property, as older compilers
      // do not have the logic to handle that case.
      return true;
    }
  }

  // If the class is resilient from the perspective of the module
  // module, it's not a default actor.
  if (classDecl->isForeign() || classDecl->isResilient(M, expansion))
    return false;

  // Check whether the class has explicit custom-actor methods.

  // If we synthesized the unownedExecutor property, we should've
  // added a semantics attribute to it (if it was actually a default
  // actor).
  bool foundExecutorPropertyImpl = false;
  bool isDefaultActor = false;
  if (auto executorProperty = classDecl->getUnownedExecutorProperty()) {
    foundExecutorPropertyImpl = true;
    isDefaultActor = isDefaultActor ||
        executorProperty->getAttrs().hasSemanticsAttr(SEMANTICS_DEFAULT_ACTOR);
  }

  // Only if we found one of the executor properties, do we return the status of default or not,
  // based on the findings of the semantics attribute of that located property.
  if (foundExecutorPropertyImpl) {
    if (!isDefaultActor &&
        classDecl->getASTContext().LangOpts.isConcurrencyModelTaskToThread() &&
        !classDecl->isUnavailable()) {
      classDecl->diagnose(
          diag::concurrency_task_to_thread_model_custom_executor,
          "task-to-thread concurrency model");
    }

    return isDefaultActor;
  }

  // Otherwise, we definitely are a default actor.
  return true;
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

  // Non-final classes cannot be global actors.
  if (auto classDecl = dyn_cast<ClassDecl>(nominal)) {
    if (!classDecl->isSemanticallyFinal()) {
      nominal->diagnose(diag::global_actor_non_final_class, nominal->getName())
        .highlight(globalActorAttr->getRangeWithAt());
    }
  }

  // Global actors have a static property "shared" that provides an actor
  // instance. The value must be of Actor type, which is validated by
  // conformance to the 'GlobalActor' protocol.
  SmallVector<ValueDecl *, 4> decls;
  nominal->lookupQualified(
      nominal, DeclNameRef(ctx.Id_shared),
      nominal->getLoc(), NL_QualifiedDefault, decls);
  for (auto decl : decls) {
    auto var = dyn_cast<VarDecl>(decl);
    if (!var)
      continue;

    if (var->getDeclContext() == nominal && var->isStatic())
      return var;
  }

  return nullptr;
}

std::optional<std::pair<CustomAttr *, NominalTypeDecl *>>
swift::checkGlobalActorAttributes(SourceLoc loc, DeclContext *dc,
                                  ArrayRef<CustomAttr *> attrs) {
  ASTContext &ctx = dc->getASTContext();

  CustomAttr *globalActorAttr = nullptr;
  NominalTypeDecl *globalActorNominal = nullptr;
  for (auto attr : attrs) {
    // Figure out which nominal declaration this custom attribute refers to.
    auto *nominal = evaluateOrDefault(ctx.evaluator,
                                      CustomAttrNominalRequest{attr, dc},
                                      nullptr);

    if (!nominal)
      continue;

    // We are only interested in global actor types.
    if (!nominal->isGlobalActor())
      continue;

    // Only a single global actor can be applied to a given entity.
    if (globalActorAttr) {
      ctx.Diags.diagnose(
          loc, diag::multiple_global_actors, globalActorNominal->getName(),
          nominal->getName());
      continue;
    }

    globalActorAttr = const_cast<CustomAttr *>(attr);
    globalActorNominal = nominal;
  }

  if (!globalActorAttr)
    return std::nullopt;

  return std::make_pair(globalActorAttr, globalActorNominal);
}

std::optional<std::pair<CustomAttr *, NominalTypeDecl *>>
GlobalActorAttributeRequest::evaluate(
    Evaluator &evaluator,
    llvm::PointerUnion<Decl *, ClosureExpr *> subject) const {
  DeclContext *dc = nullptr;
  DeclAttributes *declAttrs = nullptr;
  SourceLoc loc;
  if (auto decl = subject.dyn_cast<Decl *>()) {
    dc = decl->getDeclContext();
    declAttrs = &decl->getAttrs();
    // HACK: `getLoc`, when querying the attr from a serialized decl,
    // depending on deserialization order, may launch into arbitrary
    // type-checking when querying interface types of such decls. Which,
    // in turn, may do things like query (to print) USRs. This ends up being
    // prone to request evaluator cycles.
    //
    // Because this only applies to serialized decls, we can be confident
    // that they already went through this type-checking as primaries, so,
    // for now, to avoid cycles, we simply ignore the locs on serialized decls
    // only.
    // This is a workaround for rdar://79563942
    loc = decl->getLoc(/* SerializedOK */ false);
  } else {
    auto closure = subject.get<ClosureExpr *>();
    dc = closure;
    declAttrs = &closure->getAttrs();
    loc = closure->getLoc();
  }

  // Collect the attributes.
  SmallVector<CustomAttr *, 2> attrs;
  for (auto attr : declAttrs->getAttributes<CustomAttr>()) {
    auto mutableAttr = const_cast<CustomAttr *>(attr);
    attrs.push_back(mutableAttr);
  }

  // Look for a global actor attribute.
  auto result = checkGlobalActorAttributes(loc, dc, attrs);
  if (!result)
    return std::nullopt;

  // Closures can always have a global actor attached.
  if (subject.is<ClosureExpr *>()) {
    return result;
  }

  // Check that a global actor attribute makes sense on this kind of
  // declaration.
  auto decl = subject.get<Decl *>();

  // no further checking required if it's from a serialized module.
  if (decl->getDeclContext()->getParentSourceFile() == nullptr)
    return result;

  auto isStoredInstancePropertyOfStruct = [](VarDecl *var) {
    if (var->isStatic() || !var->isOrdinaryStoredProperty())
      return false;

    auto *nominal = var->getDeclContext()->getSelfNominalTypeDecl();
    return isa_and_nonnull<StructDecl>(nominal) &&
           !isWrappedValueOfPropWrapper(var);
  };

  auto globalActorAttr = result->first;
  if (auto nominal = dyn_cast<NominalTypeDecl>(decl)) {
    // Nominal types are okay...
    if (auto classDecl = dyn_cast<ClassDecl>(nominal)){
      if (classDecl->isActor()) {
        // ... except for actors.
        nominal->diagnose(diag::global_actor_on_actor_class, nominal->getName())
            .highlight(globalActorAttr->getRangeWithAt());
        return std::nullopt;
      }
    }
  } else if (auto storage = dyn_cast<AbstractStorageDecl>(decl)) {
    // Subscripts and properties are fine...
    if (auto var = dyn_cast<VarDecl>(storage)) {

      // ... but not if it's an async-context top-level global
      if (var->isTopLevelGlobal() &&
          (var->getDeclContext()->isAsyncContext() ||
           var->getASTContext().LangOpts.StrictConcurrencyLevel >=
             StrictConcurrency::Complete)) {
        var->diagnose(diag::global_actor_top_level_var)
            .highlight(globalActorAttr->getRangeWithAt());
        return std::nullopt;
      }

      // ... and not if it's local property
      if (var->getDeclContext()->isLocalContext()) {
        var->diagnose(diag::global_actor_on_local_variable, var->getName())
            .highlight(globalActorAttr->getRangeWithAt());
        return std::nullopt;
      }
    }
  } else if (isa<ExtensionDecl>(decl)) {
    // Extensions are okay.
  } else if (isa<ConstructorDecl>(decl) || isa<FuncDecl>(decl) ||
             isa<DestructorDecl>(decl)) {
    // None of the accessors/addressors besides a getter are allowed
    // to have a global actor attribute.
    if (auto *accessor = dyn_cast<AccessorDecl>(decl)) {
      if (!accessor->isGetter()) {
        decl->diagnose(diag::global_actor_disallowed, decl)
            .warnUntilSwiftVersion(6)
            .fixItRemove(globalActorAttr->getRangeWithAt());

        auto &ctx = decl->getASTContext();
        auto *storage = accessor->getStorage();
        // Let's suggest to move the attribute to the storage if
        // this is an accessor/addressor of a property of subscript.
        if (storage->getDeclContext()->isTypeContext()) {
          auto canMoveAttr = [&]() {
            // If enclosing declaration has a global actor,
            // skip the suggestion.
            if (storage->getGlobalActorAttr())
              return false;

            // Global actor attribute cannot be applied to
            // an instance stored property of a struct.
            if (auto *var = dyn_cast<VarDecl>(storage)) {
              return !isStoredInstancePropertyOfStruct(var);
            }

            return true;
          };

          if (canMoveAttr()) {
            decl->diagnose(diag::move_global_actor_attr_to_storage_decl,
                           storage)
                .fixItInsert(
                    storage->getAttributeInsertionLoc(/*forModifier=*/false),
                    llvm::Twine("@", result->second->getNameStr()).str());
          }
        }

        // In Swift 6, once the diag above is an error, it is disallowed.
        if (ctx.isSwiftVersionAtLeast(6))
          return std::nullopt;
      }
    }
    // Functions are okay.
  } else {
    // Everything else is disallowed.
    decl->diagnose(diag::global_actor_disallowed, decl);
    return std::nullopt;
  }

  return result;
}

Type swift::getExplicitGlobalActor(ClosureExpr *closure) {
  // Look at the explicit attribute.
  auto globalActorAttr =
      evaluateOrDefault(closure->getASTContext().evaluator,
                        GlobalActorAttributeRequest{closure}, std::nullopt);
  if (!globalActorAttr)
    return Type();

  Type globalActor = evaluateOrDefault(
      closure->getASTContext().evaluator,
      CustomAttrTypeRequest{
        globalActorAttr->first, closure, CustomAttrTypeKind::GlobalActor},
        Type());
  if (!globalActor || globalActor->hasError())
    return Type();

  return globalActor;
}

/// A 'let' declaration is safe across actors if it is either
/// nonisolated or it is accessed from within the same module.
static bool varIsSafeAcrossActors(const ModuleDecl *fromModule, VarDecl *var,
                                  const ActorIsolation &varIsolation,
                                  std::optional<ReferencedActor> actorInstance,
                                  ActorReferenceResult::Options &options) {

  bool accessWithinModule =
      (fromModule == var->getDeclContext()->getParentModule());

  if (varIsolation.getKind() == ActorIsolation::NonisolatedUnsafe)
    return true;

  if (!var->isLet()) {
    // A mutable storage of a value type accessed from within the module is
    // okay.
    if (dyn_cast_or_null<StructDecl>(var->getDeclContext()->getAsDecl()) &&
        !var->isStatic() && var->hasStorage() &&
        var->getTypeInContext()->isSendableType()) {
      if (accessWithinModule || varIsolation.isNonisolated())
        return true;
    }
    // Otherwise, must be immutable.
    return false;
  }

  switch (varIsolation) {
  case ActorIsolation::Nonisolated:
  case ActorIsolation::NonisolatedUnsafe:
  case ActorIsolation::Unspecified:
    // if nonisolated, it's OK
    return true;

  case ActorIsolation::CallerIsolationInheriting:
    return false;

  case ActorIsolation::Erased:
    llvm_unreachable("variable cannot have erased isolation");

  case ActorIsolation::ActorInstance:
  case ActorIsolation::GlobalActor:
    // If it's explicitly 'nonisolated', it's okay.
    if (var->getAttrs().hasAttribute<NonisolatedAttr>())
      return true;

    // Static 'let's are initialized upon first access, so they cannot be
    // synchronously accessed across actors.
    if (var->isGlobalStorage() && var->isLazilyInitializedGlobal()) {
      // Compiler versions <= 5.9 accepted this code, so downgrade to a
      // warning prior to Swift 6.
      options = ActorReferenceResult::Flags::Preconcurrency;
      return false;
    }

    // If it's distributed, but known to be local, it's ok
    // TODO: Check if this can be obtained from the isolation, without a need for separate argument
    if (actorInstance && actorInstance->isKnownToBeLocal()) {
      return true;
    }
    // If it's distributed, generally variable access is not okay...
    if (auto nominalParent = var->getDeclContext()->getSelfNominalTypeDecl()) {
      if (nominalParent->isDistributedActor())
        return false;
    }

    // If the type is not 'Sendable', it's unsafe
    if (!var->getTypeInContext()->isSendableType()) {
      // Compiler versions <= 5.10 treated this variable as nonisolated,
      // so downgrade async access errors in the effects checker to
      // warnings prior to Swift 6.
      if (accessWithinModule)
        options = ActorReferenceResult::Flags::Preconcurrency;

      return false;
    }

    // If it's actor-isolated but in the same module, then it's OK too.
    return accessWithinModule;
  }
}

bool swift::isLetAccessibleAnywhere(const ModuleDecl *fromModule,
                                    VarDecl *let,
                                    ActorReferenceResult::Options &options) {
  auto isolation = getActorIsolation(let);
  return varIsSafeAcrossActors(fromModule, let, isolation, std::nullopt, options);
}

bool swift::isLetAccessibleAnywhere(const ModuleDecl *fromModule,
                                    VarDecl *let) {
  ActorReferenceResult::Options options = std::nullopt;
  return isLetAccessibleAnywhere(fromModule, let, options);
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
static std::optional<PartialApplyThunkInfo>
decomposePartialApplyThunk(ApplyExpr *apply, Expr *parent) {
  // Check for a call to the outer closure in the thunk.
  auto outerAutoclosure = dyn_cast<AutoClosureExpr>(apply->getFn());
  if (!outerAutoclosure || outerAutoclosure->getThunkKind() !=
                               AutoClosureExpr::Kind::DoubleCurryThunk)
    return std::nullopt;

  auto *unarySelfArg = apply->getArgs()->getUnlabeledUnaryExpr();
  assert(unarySelfArg &&
         "Double curry should start with a unary (Self) -> ... arg");

  auto memberFn = outerAutoclosure->getUnwrappedCurryThunkExpr();
  if (!memberFn)
    return std::nullopt;

  // Determine whether the partial apply thunk was immediately converted to
  // noescape.
  bool isEscaping = true;
  if (auto conversion = dyn_cast_or_null<FunctionConversionExpr>(parent)) {
    auto fnType = conversion->getType()->getAs<FunctionType>();
    isEscaping = fnType && !fnType->isNoEscape();
  }

  return PartialApplyThunkInfo{unarySelfArg, memberFn, isEscaping};
}

/// Find the immediate member reference in the given expression.
static std::optional<std::pair<ConcreteDeclRef, SourceLoc>>
findReference(Expr *expr) {
  // Look through a function conversion.
  if (auto fnConv = dyn_cast<FunctionConversionExpr>(expr))
    expr = fnConv->getSubExpr();

  if (auto declRef = dyn_cast<DeclRefExpr>(expr))
    return std::make_pair(declRef->getDeclRef(), declRef->getLoc());

  if (auto otherCtor = dyn_cast<OtherConstructorDeclRefExpr>(expr)) {
    return std::make_pair(otherCtor->getDeclRef(), otherCtor->getLoc());
  }

  Expr *inner = expr->getValueProvidingExpr();
  if (inner != expr)
    return findReference(inner);

  return std::nullopt;
}

/// Return true if the callee of an ApplyExpr is async
///
/// Note that this must be called after the implicitlyAsync flag has been set,
/// or implicitly async calls will not return the correct value.
static bool isAsyncCall(
    llvm::PointerUnion<ApplyExpr *, LookupExpr *> call) {

  if (auto *apply = call.dyn_cast<ApplyExpr *>()) {
    if (apply->isImplicitlyAsync())
      return true;

    // Effectively the same as doing a
    // `cast_or_null<FunctionType>(call->getFn()->getType())`, check the
    // result of that and then checking `isAsync` if it's defined.
    Type funcTypeType = apply->getFn()->getType();
    if (!funcTypeType)
      return false;
    AnyFunctionType *funcType = funcTypeType->getAs<AnyFunctionType>();
    if (!funcType)
      return false;
    return funcType->isAsync();
  }

  auto *lookup = call.get<LookupExpr *>();
  if (lookup->isImplicitlyAsync())
    return true;

  return lookup->getDecl().getDecl()->isAsync();
}

/// Determine whether we should diagnose data races within the current context.
///
/// By default, we do this only in code that makes use of concurrency
/// features.
static bool shouldDiagnoseExistingDataRaces(const DeclContext *dc);

/// Returns true if this closure acts as an inference boundary in the AST. An
/// inference boundary is an expression in the AST where we newly infer
/// isolation different from our parent decl context.
///
/// Examples:
///
///   1. a @Sendable closure.
///   2. a closure literal passed to a sending parameter.
///
/// NOTE: This does not mean that it has nonisolated isolation since for
/// instance one could define an @MainActor closure in a nonisolated
/// function. That @MainActor closure would act as an Isolation Inference
/// Boundary.
///
/// \param canInheritActorContext Whether or not the closure is allowed to
/// inherit the isolation of the enclosing context. If this is \c true ,
/// the closure is not considered an isolation inference boundary if the
/// \c @_inheritActorContext attribute is applied to the closure. This
/// attribute is inferred from a parameter declaration for closure arguments,
/// and it's set on the closure in CSApply.
static bool
isIsolationInferenceBoundaryClosure(const AbstractClosureExpr *closure,
                                    bool canInheritActorContext) {
  if (auto *ce = dyn_cast<ClosureExpr>(closure)) {
    // If the closure can inherit the isolation of the enclosing context,
    // it is not an actor isolation inference boundary.
    if (canInheritActorContext && ce->inheritsActorContext())
      return false;

    if (ce->isPassedToSendingParameter())
      return true;
  }

  // An autoclosure for an async let acts as a boundary. It is non-Sendable
  // regardless of its context.
  if (auto *autoclosure = dyn_cast<AutoClosureExpr>(closure)) {
    if (autoclosure->getThunkKind() == AutoClosureExpr::Kind::AsyncLet)
      return true;
  }

  return closure->isSendable();
}

/// Add Fix-It text for the given nominal type to adopt Sendable.
static void addSendableFixIt(
    const NominalTypeDecl *nominal, InFlightDiagnostic &diag, bool unchecked) {
  if (nominal->getInherited().empty()) {
    SourceLoc fixItLoc = nominal->getBraces().Start;
    diag.fixItInsert(fixItLoc,
                     unchecked ? ": @unchecked Sendable" : ": Sendable");
  } else {
    auto fixItLoc = nominal->getInherited().getEndLoc();
    diag.fixItInsertAfter(fixItLoc,
                          unchecked ? ", @unchecked Sendable" : ", Sendable");
  }
}

static void addProtocolFixIt(const GenericTypeParamDecl *genericArgument,
                             InFlightDiagnostic &diag, StringRef protocolName) {
  if (genericArgument->getInherited().empty()) {
    auto fixItLoc = genericArgument->getLoc();
    diag.fixItInsertAfter(fixItLoc, llvm::Twine(": ", protocolName).str());
  } else {
    auto fixItLoc = genericArgument->getInherited().getEndLoc();
    diag.fixItInsertAfter(fixItLoc, llvm::Twine(" & ", protocolName).str());
  }
}
/// Add Fix-It text for the given generic param declaration type to adopt
/// Sendable.
static void addSendableFixIt(const GenericTypeParamDecl *genericArgument,
                             InFlightDiagnostic &diag) {
  addProtocolFixIt(genericArgument, diag,
                   getProtocolName(KnownProtocolKind::Sendable));
}

static void
addSendableMetatypeFixIt(const GenericTypeParamDecl *genericArgument,
                         InFlightDiagnostic &diag) {
  addProtocolFixIt(genericArgument, diag,
                   getProtocolName(KnownProtocolKind::SendableMetatype));
}

static bool shouldDiagnoseExistingDataRaces(const DeclContext *dc) {
  return contextRequiresStrictConcurrencyChecking(dc, [](const AbstractClosureExpr *) {
    return Type();
  },
  [](const ClosureExpr *closure) {
    return closure->isIsolatedByPreconcurrency();
  });
}

bool SendableCheckContext::warnInMinimalChecking() const {
  if (preconcurrencyContext)
    return false;

  if (!conformanceCheck)
    return false;

  switch (*conformanceCheck) {
  case SendableCheck::Explicit:
    return true;

  case SendableCheck::ImpliedByPreconcurrencyProtocol:
  case SendableCheck::Implicit:
  case SendableCheck::ImplicitForExternallyVisible:
    return false;
  }
}

DiagnosticBehavior SendableCheckContext::defaultDiagnosticBehavior() const {
  // If we're not supposed to diagnose existing data races from this context,
  // ignore the diagnostic entirely.
  if (!warnInMinimalChecking() &&
      !shouldDiagnoseExistingDataRaces(fromDC))
    return DiagnosticBehavior::Ignore;

  return DiagnosticBehavior::Warning;
}

DiagnosticBehavior
SendableCheckContext::implicitSendableDiagnosticBehavior() const {
  switch (fromDC->getASTContext().LangOpts.StrictConcurrencyLevel) {
  case StrictConcurrency::Targeted:
    // Limited checking only diagnoses implicit Sendable within contexts that
    // have adopted concurrency.
    if (shouldDiagnoseExistingDataRaces(fromDC))
      return DiagnosticBehavior::Warning;

    LLVM_FALLTHROUGH;

  case StrictConcurrency::Minimal:
    if (warnInMinimalChecking())
      return DiagnosticBehavior::Warning;

    return DiagnosticBehavior::Ignore;

  case StrictConcurrency::Complete:
    return defaultDiagnosticBehavior();
  }
}

/// Determine the diagnostic behavior for a Sendable reference to the given
/// nominal type.
DiagnosticBehavior SendableCheckContext::diagnosticBehavior(
    NominalTypeDecl *nominal) const {
  // If we're in a preconcurrency context, don't override the default behavior
  // based on explicit conformances. For example, a @preconcurrency @Sendable
  // closure should not warn about an explicitly unavailable Sendable
  // conformance in minimal checking.
  if (preconcurrencyContext)
    return defaultDiagnosticBehavior();

  if (hasExplicitSendableConformance(nominal))
    return DiagnosticBehavior::Warning;

  DiagnosticBehavior defaultBehavior = implicitSendableDiagnosticBehavior();

  // If we are checking an implicit Sendable conformance, don't suppress
  // diagnostics for declarations in the same module. We want them to make
  // enclosing inferred types non-Sendable.
  if (defaultBehavior == DiagnosticBehavior::Ignore &&
      nominal->getParentSourceFile() &&
      conformanceCheck && isImplicitSendableCheck(*conformanceCheck))
    return DiagnosticBehavior::Warning;

  return defaultBehavior;
}

std::optional<DiagnosticBehavior>
SendableCheckContext::preconcurrencyBehavior(
    Decl *decl,
    bool ignoreExplicitConformance) const {
  if (!decl)
    return std::nullopt;

  if (auto *nominal = dyn_cast<NominalTypeDecl>(decl)) {
    return getConcurrencyDiagnosticBehaviorLimit(nominal, fromDC,
                                                 ignoreExplicitConformance);
  }

  return std::nullopt;
}

std::optional<DiagnosticBehavior>
SendableCheckContext::preconcurrencyBehavior(Type type) const {
  return type->getConcurrencyDiagnosticBehaviorLimit(
      const_cast<DeclContext *>(fromDC));
}

static bool shouldDiagnosePreconcurrencyImports(SourceFile &sf) {
  switch (sf.Kind) {
  case SourceFileKind::Interface:
  case SourceFileKind::SIL:
      return false;

  case SourceFileKind::DefaultArgument:
  case SourceFileKind::Library:
  case SourceFileKind::Main:
  case SourceFileKind::MacroExpansion:
      return true;
  }
}

bool swift::diagnoseSendabilityErrorBasedOn(
    NominalTypeDecl *nominal, SendableCheckContext fromContext,
    llvm::function_ref<bool(DiagnosticBehavior)> diagnose) {
  auto behavior = DiagnosticBehavior::Unspecified;

  if (nominal) {
    behavior = fromContext.diagnosticBehavior(nominal);
  } else {
    behavior = fromContext.implicitSendableDiagnosticBehavior();
  }

  bool wasSuppressed = diagnose(behavior);

  SourceFile *sourceFile = fromContext.fromDC->getParentSourceFile();
  if (sourceFile && shouldDiagnosePreconcurrencyImports(*sourceFile)) {
    bool emittedDiagnostics =
        behavior != DiagnosticBehavior::Ignore && !wasSuppressed;

    // When the type is explicitly Sendable *or* explicitly non-Sendable, we
    // assume it has been audited and `@preconcurrency` is not recommended even
    // though it would actually affect the diagnostic.
    bool nominalIsImportedAndHasImplicitSendability =
        nominal &&
        nominal->getParentModule() != fromContext.fromDC->getParentModule() &&
        !hasExplicitSendableConformance(nominal);

    if (emittedDiagnostics && nominalIsImportedAndHasImplicitSendability) {
      // This type was imported from another module; try to find the
      // corresponding import.
      std::optional<AttributedImport<swift::ImportedModule>> import =
          nominal->findImport(fromContext.fromDC);

      // If we found the import that makes this nominal type visible, remark
      // that it can be @preconcurrency import.
      // Only emit this remark once per source file, because it can happen a
      // lot.
      if (import && !import->options.contains(ImportFlags::Preconcurrency) &&
          import->importLoc.isValid() && sourceFile &&
          !sourceFile->hasImportUsedPreconcurrency(*import)) {
        SourceLoc importLoc = import->importLoc;
        ASTContext &ctx = nominal->getASTContext();

        ctx.Diags
            .diagnose(importLoc, diag::add_predates_concurrency_import,
                      ctx.LangOpts.isSwiftVersionAtLeast(6),
                      nominal->getParentModule()->getName())
            .fixItInsert(importLoc, "@preconcurrency ");

        sourceFile->setImportUsedPreconcurrency(*import);
      }
    }
  }

  return behavior == DiagnosticBehavior::Unspecified && !wasSuppressed;
}

void swift::diagnoseUnnecessaryPreconcurrencyImports(SourceFile &sf) {
  if (!shouldDiagnosePreconcurrencyImports(sf))
    return;

  ASTContext &ctx = sf.getASTContext();

  if (ctx.TypeCheckerOpts.SkipFunctionBodies != FunctionBodySkipping::None)
    return;

  for (const auto &import : sf.getImports()) {
    if (import.options.contains(ImportFlags::Preconcurrency) &&
        import.importLoc.isValid() &&
        !sf.hasImportUsedPreconcurrency(import)) {
      ctx.Diags.diagnose(
          import.importLoc, diag::remove_predates_concurrency_import,
          import.module.importedModule->getName())
        .fixItRemove(import.preconcurrencyRange);
    }
  }
}

/// Produce a diagnostic for a single instance of a non-Sendable type where
/// a Sendable type is required.
static bool diagnoseSingleNonSendableType(
    Type type, SendableCheckContext fromContext,
    Type inDerivedConformance, SourceLoc loc,
    llvm::function_ref<bool(Type, DiagnosticBehavior)> diagnose) {
  if (type->hasError())
    return false;

  auto module = fromContext.fromDC->getParentModule();
  auto nominal = type->getAnyNominal();
  auto &ctx = module->getASTContext();

  return diagnoseSendabilityErrorBasedOn(nominal, fromContext,
                                         [&](DiagnosticBehavior behavior) {
    bool wasSuppressed = diagnose(type, behavior);

    // Don't emit the following notes if we didn't have any diagnostics to
    // attach them to.
    if (wasSuppressed || behavior == DiagnosticBehavior::Ignore)
      return true;

    if (inDerivedConformance) {
      ctx.Diags.diagnose(loc, diag::in_derived_conformance,
                         inDerivedConformance);
    }

    if (type->is<FunctionType>()) {
      ctx.Diags.diagnose(loc, diag::nonsendable_function_type);
    } else if (nominal && nominal->getParentModule() == module) {
      // If the nominal type is in the current module, suggest adding
      // `Sendable` if it might make sense. Otherwise, just complain.
      if (isa<StructDecl>(nominal) || isa<EnumDecl>(nominal)) {
        auto note = nominal->diagnose(
            diag::add_nominal_sendable_conformance, nominal);
        addSendableFixIt(nominal, note, /*unchecked=*/false);
      } else {
        nominal->diagnose(diag::non_sendable_nominal, nominal);
      }
    } else if (nominal) {
      // Note which nominal type does not conform to `Sendable`.
      nominal->diagnose(diag::non_sendable_nominal, nominal);
    } else if (auto genericArchetype = type->getAs<ArchetypeType>()) {
      auto interfaceType = genericArchetype->getInterfaceType();
      if (auto genericParamType =
              interfaceType->getAs<GenericTypeParamType>()) {
        auto *genericParamTypeDecl = genericParamType->getDecl();
        if (genericParamTypeDecl &&
            genericParamTypeDecl->getModuleContext() == module) {
          auto diag = genericParamTypeDecl->diagnose(
              diag::add_generic_parameter_conformance, type,
              ctx.getProtocol(KnownProtocolKind::Sendable));
          addSendableFixIt(genericParamTypeDecl, diag);
        }
      }
    }

    return false;
  });
}

bool swift::diagnoseNonSendableTypes(
    Type type, SendableCheckContext fromContext,
    Type inDerivedConformance, SourceLoc loc,
    llvm::function_ref<bool(Type, DiagnosticBehavior)> diagnose) {
  // If the Sendable protocol is missing, do nothing.
  auto proto = type->getASTContext().getProtocol(KnownProtocolKind::Sendable);
  if (!proto)
    return false;

  // FIXME: More detail for unavailable conformances.
  auto conformance = lookupConformance(type, proto, /*allowMissing=*/true);
  if (conformance.isInvalid() || conformance.hasUnavailableConformance()) {
    return diagnoseSingleNonSendableType(
        type, fromContext, inDerivedConformance, loc, diagnose);
  }

  // Walk the conformance, diagnosing any missing Sendable conformances.
  bool anyMissing = false;
  conformance.forEachMissingConformance(
      [&](BuiltinProtocolConformance *missing) {
        if (diagnoseSingleNonSendableType(
                missing->getType(), fromContext,
                inDerivedConformance, loc, diagnose)) {
          anyMissing = true;
        }

        return false;
      });

  return anyMissing;
}

static
Diag<Type, const ValueDecl *, ActorIsolation>
getSendableParamDiag(SendableCheckReason refKind) {
  switch (refKind) {
  case SendableCheckReason::CrossActor:
  case SendableCheckReason::SynchronousAsAsync:
    return diag::non_sendable_arg_into_actor;

  case SendableCheckReason::ExitingActor:
    return diag::non_sendable_arg_exits_actor;

  case SendableCheckReason::Conformance:
    return diag::non_sendable_param_in_witness;

  case SendableCheckReason::Override:
    return diag::non_sendable_param_in_override;

  case SendableCheckReason::ObjC:
    return diag::non_sendable_param_in_objc;
  }
}

static
Diag<Type, const ValueDecl *, ActorIsolation>
getSendableResultDiag(SendableCheckReason refKind) {
  switch (refKind) {
  case SendableCheckReason::CrossActor:
  case SendableCheckReason::SynchronousAsAsync:
    return diag::non_sendable_result_into_actor;

  case SendableCheckReason::ExitingActor:
    return diag::non_sendable_result_exits_actor;

  case SendableCheckReason::Conformance:
    return diag::non_sendable_result_in_witness;

  case SendableCheckReason::Override:
    return diag::non_sendable_result_in_override;

  case SendableCheckReason::ObjC:
    return diag::non_sendable_result_in_objc;
  }
}

static
Diag<Type, const ValueDecl *, ActorIsolation>
getSendablePropertyDiag(SendableCheckReason refKind) {
  switch (refKind) {
  case SendableCheckReason::CrossActor:
  case SendableCheckReason::SynchronousAsAsync:
    return diag::non_sendable_property_exits_actor;

  case SendableCheckReason::ExitingActor:
    return diag::non_sendable_property_into_actor;

  case SendableCheckReason::Conformance:
    return diag::non_sendable_property_in_witness;

  case SendableCheckReason::Override:
    return diag::non_sendable_property_in_override;

  case SendableCheckReason::ObjC:
    return diag::non_sendable_property_in_objc;
  }
}

bool swift::diagnoseNonSendableTypesInReference(
    Expr *base, ConcreteDeclRef declRef, const DeclContext *fromDC,
    SourceLoc refLoc, SendableCheckReason refKind,
    std::optional<ActorIsolation> knownIsolation,
    FunctionCheckOptions funcCheckOptions, SourceLoc diagnoseLoc) {
  // Retrieve the actor isolation to use in diagnostics.
  auto getActorIsolation = [&] {
    if (knownIsolation)
      return *knownIsolation;

    return swift::getActorIsolation(declRef.getDecl());
  };

  // If the violation is in the implementation of a derived conformance,
  // point to the location of the parent type instead.
  Type derivedConformanceType;
  if (refLoc.isInvalid()) {
    auto *decl = fromDC->getAsDecl();
    if (decl && decl->isImplicit()) {
      if (auto *implements = decl->getAttrs().getAttribute<ImplementsAttr>()) {
        auto *parentDC = decl->getDeclContext();
        refLoc = parentDC->getAsDecl()->getLoc();
        derivedConformanceType =
            implements->getProtocol(parentDC)->getDeclaredInterfaceType();
      }
    }
  }

  // Check the 'self' argument.
  if (base) {
    if (diagnoseNonSendableTypes(
            base->getType(),
            fromDC, derivedConformanceType,
            base->getStartLoc(),
            getSendableParamDiag(refKind),
            declRef.getDecl(),
            getActorIsolation()))
      return true;
  }

  // For functions or subscripts, check the parameter and result types.
  SubstitutionMap subs = declRef.getSubstitutions();
  auto decl = declRef.getDecl();
  if (isa<AbstractFunctionDecl>(decl) || isa<SubscriptDecl>(decl)) {
    if (funcCheckOptions.contains(FunctionCheckKind::Params)) {
      // only check params if funcCheckKind specifies so
      ParameterList *paramList = nullptr;
      if (auto function = dyn_cast<AbstractFunctionDecl>(decl)) {
        paramList = function->getParameters();
      } else if (auto subscript = dyn_cast<SubscriptDecl>(decl)) {
        paramList = subscript->getIndices();
      }

      // Check params of this function or subscript override for sendability
      for (auto param : *paramList) {
        Type paramType = param->getInterfaceType().subst(subs);
        if (diagnoseNonSendableTypesWithSendingCheck(
                param, paramType, fromDC, derivedConformanceType, refLoc,
                diagnoseLoc.isInvalid() ? refLoc : diagnoseLoc,
                getSendableParamDiag(refKind), decl, getActorIsolation()))
          return true;
      }
    }

    // Check the result type of a function or subscript.
    if (funcCheckOptions.contains(FunctionCheckKind::Results)) {
      Type resultType;
      if (auto func = dyn_cast<FuncDecl>(decl)) {
        resultType = func->getResultInterfaceType().subst(subs);
        decl = func;
      } else if (auto subscript = dyn_cast<SubscriptDecl>(decl)) {
        resultType = subscript->getElementInterfaceType().subst(subs);
      }
      if (!resultType) {
        return false;
      }
      if (diagnoseNonSendableTypesWithSendingCheck(
              decl, resultType, fromDC, derivedConformanceType, refLoc,
              diagnoseLoc.isInvalid() ? refLoc : diagnoseLoc,
              getSendableResultDiag(refKind), decl, getActorIsolation()))
        return true;
    }

    return false;
  }

  if (auto var = dyn_cast<VarDecl>(declRef.getDecl())) {
    Type propertyType = var->isLocalCapture()
        ? var->getTypeInContext()
        : var->getValueInterfaceType().subst(subs);
    if (diagnoseNonSendableTypes(
            propertyType, fromDC,
            derivedConformanceType, refLoc,
            getSendablePropertyDiag(refKind),
            var, getActorIsolation()))
      return true;
  }

  return false;
}

void swift::diagnoseMissingSendableConformance(
    SourceLoc loc, Type type, const DeclContext *fromDC, bool preconcurrency) {
  SendableCheckContext sendableContext(fromDC, preconcurrency);
  diagnoseNonSendableTypes(
      type, sendableContext, /*inDerivedConformance*/Type(),
      loc, diag::non_sendable_type);
}

void swift::diagnoseMissingSendableMetatypeConformance(
    SourceLoc loc, Type type, const DeclContext *fromDC, bool preconcurrency) {
  SendableCheckContext sendableContext(fromDC, preconcurrency);
  DiagnosticBehavior behavior =
      sendableContext.implicitSendableDiagnosticBehavior();

  auto &ctx = type->getASTContext();
  ctx.Diags.diagnose(loc, diag::non_sendable_metatype_type, type)
      .limitBehaviorWithPreconcurrency(behavior, preconcurrency);

  if (auto genericArchetype = type->getAs<ArchetypeType>()) {
    auto interfaceType = genericArchetype->getInterfaceType();
    if (auto genericParamType = interfaceType->getAs<GenericTypeParamType>()) {
      auto module = fromDC->getParentModule();
      auto *genericParamTypeDecl = genericParamType->getDecl();
      if (genericParamTypeDecl &&
          genericParamTypeDecl->getModuleContext() == module) {
        auto diag = genericParamTypeDecl->diagnose(
            diag::add_generic_parameter_conformance, type,
            ctx.getProtocol(KnownProtocolKind::SendableMetatype));
        addSendableMetatypeFixIt(genericParamTypeDecl, diag);
      }
    }
  }
}

namespace {
/// Infer Sendable from the instance storage of the given nominal type.
/// \returns \c std::nullopt if there is no way to make the type \c Sendable,
/// \c true if \c Sendable needs to be @unchecked, \c false if it can be
/// \c Sendable without the @unchecked.
std::optional<bool>
inferSendableFromInstanceStorage(NominalTypeDecl *nominal,
                                 SmallVectorImpl<Requirement> &requirements) {
  // Raw storage is assumed not to be sendable.
  if (auto sd = dyn_cast<StructDecl>(nominal)) {
    if (sd->getAttrs().hasAttribute<RawLayoutAttr>()) {
      return true;
    }
  }

  class Visitor : public StorageVisitor {
  public:
    NominalTypeDecl *nominal;
    SmallVectorImpl<Requirement> &requirements;
    bool isUnchecked = false;
    ProtocolDecl *sendableProto = nullptr;

    Visitor(NominalTypeDecl *nominal,
            SmallVectorImpl<Requirement> &requirements)
        : StorageVisitor(), nominal(nominal), requirements(requirements) {
      ASTContext &ctx = nominal->getASTContext();
      sendableProto = ctx.getProtocol(KnownProtocolKind::Sendable);
    }

    bool operator()(VarDecl *var, Type propertyType) override {
      // If we have a class with mutable state, only an @unchecked
      // conformance will work.
      if (isa<ClassDecl>(nominal) && var->supportsMutation())
        isUnchecked = true;

      return checkType(propertyType);
    }

    bool operator()(EnumElementDecl *element, Type elementType) override {
      return checkType(elementType);
    }

    /// Check sendability of the given type, recording any requirements.
    bool checkType(Type type) {
      if (!sendableProto)
        return true;

      auto conformance = checkConformance(type, sendableProto);
      if (conformance.isInvalid())
        return true;

      // If there is an unavailable conformance here, fail.
      if (conformance.hasUnavailableConformance())
        return true;

      // Look for missing Sendable conformances.
      return conformance.forEachMissingConformance(
          [&](BuiltinProtocolConformance *missing) {
            // For anything other than Sendable, fail.
            if (missing->getProtocol() != sendableProto)
              return true;

            // If we have an archetype, capture the requirement
            // to make this type Sendable.
            if (missing->getType()->is<ArchetypeType>()) {
              requirements.push_back(
                  Requirement(RequirementKind::Conformance,
                              missing->getType()->mapTypeOutOfContext(),
                              sendableProto->getDeclaredType()));
              return false;
            }

            return true;
          });
    }
  } visitor(nominal, requirements);

  return visitor.visit(nominal, nominal);
}
}

static bool checkSendableInstanceStorage(
    NominalTypeDecl *nominal, DeclContext *dc, SendableCheck check);

void swift::diagnoseMissingExplicitSendable(NominalTypeDecl *nominal) {
  // Only diagnose when explicitly requested.
  ASTContext &ctx = nominal->getASTContext();
  if (!ctx.LangOpts.RequireExplicitSendable)
    return;

  if (nominal->getLoc().isInvalid())
    return;

  // Protocols aren't checked.
  if (isa<ProtocolDecl>(nominal))
    return;

  // Actors are always Sendable.
  if (auto classDecl = dyn_cast<ClassDecl>(nominal))
    if (classDecl->isActor())
      return;

  // Only public/open types have this check.
  if (!nominal->getFormalAccessScope(
        /*useDC=*/nullptr,
        /*treatUsableFromInlineAsPublic=*/true).isPublic())
    return;

  // If the conformance is explicitly stated, do nothing.
  if (hasExplicitSendableConformance(nominal, /*applyModuleDefault=*/false))
    return;

  // Diagnose it.
  nominal->diagnose(diag::public_decl_needs_sendable, nominal);

  // Note to add a Sendable conformance, possibly an unchecked one.
  {
    llvm::SmallVector<Requirement, 2> requirements;
    auto canMakeSendable = inferSendableFromInstanceStorage(
        nominal, requirements);

    // Non-final classes can only have @unchecked.
    bool isUnchecked = !canMakeSendable || *canMakeSendable;
    if (auto classDecl = dyn_cast<ClassDecl>(nominal)) {
      if (!classDecl->isFinal())
        isUnchecked = true;
    }

    // If we can only make the type Sendable via @unchecked, don't provide a Fix-It.
    if (!isUnchecked) {
      auto note = nominal->diagnose(diag::add_nominal_sendable_conformance, nominal);
      if (canMakeSendable && !requirements.empty()) {
        // Produce a Fix-It containing a conditional conformance to Sendable,
        // based on the requirements harvested from instance storage.

        // Form the where clause containing all of the requirements.
        SmallString<64> whereClause;
        {
          llvm::raw_svector_ostream out(whereClause);
          llvm::interleaveComma(
              requirements, out,
              [&](const Requirement &req) {
                out << req.getFirstType().getString() << ": "
                    << req.getSecondType().getString();
              });
        }

        // Add a Fix-It containing the conditional extension text itself.
        auto insertionLoc = nominal->getBraces().End;
        note.fixItInsertAfter(
            insertionLoc,
            ("\n\nextension " + nominal->getName().str() + ": "
             + "Sendable where " + whereClause + " { }\n").str());
      } else {
        addSendableFixIt(nominal, note, isUnchecked);
      }
    }
  }

  // Note to disable the warning.
  {
    auto note = nominal->diagnose(diag::explicit_disable_sendable, nominal);
    auto insertionLoc = nominal->getBraces().End;
    note.fixItInsertAfter(
        insertionLoc,
        ("\n\n@available(*, unavailable)\nextension " + nominal->getName().str()
         + ": Sendable { }\n").str());
  }
}

void swift::tryDiagnoseExecutorConformance(ASTContext &C,
                                           const NominalTypeDecl *nominal,
                                           ProtocolDecl *proto) {
  assert(proto->isSpecificProtocol(KnownProtocolKind::Executor) ||
         proto->isSpecificProtocol(KnownProtocolKind::SerialExecutor) ||
         proto->isSpecificProtocol(KnownProtocolKind::TaskExecutor));

  auto &diags = C.Diags;
  Type nominalTy = nominal->getDeclaredInterfaceType();
  NominalTypeDecl *executorDecl = C.getExecutorDecl();

  // enqueue(_:)
  auto enqueueDeclName = DeclName(C, DeclBaseName(C.Id_enqueue), { Identifier() });

  FuncDecl *moveOnlyEnqueueRequirement = nullptr;
  FuncDecl *legacyMoveOnlyEnqueueRequirement = nullptr;
  FuncDecl *unownedEnqueueRequirement = nullptr;
  for (auto req: proto->getProtocolRequirements()) {
    auto *funcDecl = dyn_cast<FuncDecl>(req);
    if (!funcDecl)
      continue;

    if (funcDecl->getName() != enqueueDeclName)
      continue;

    // look for the first parameter being a Job or UnownedJob
    if (funcDecl->getParameters()->size() != 1)
      continue;

    if (auto param = funcDecl->getParameters()->front()) {
      StructDecl *executorJobDecl = C.getExecutorJobDecl();
      StructDecl *legacyJobDecl = C.getJobDecl();
      StructDecl *unownedJobDecl = C.getUnownedJobDecl();

      if (executorJobDecl && param->getInterfaceType()->isEqual(executorJobDecl->getDeclaredInterfaceType())) {
        assert(moveOnlyEnqueueRequirement == nullptr);
        moveOnlyEnqueueRequirement = funcDecl;
      } else if (legacyJobDecl && param->getInterfaceType()->isEqual(legacyJobDecl->getDeclaredInterfaceType())) {
        assert(legacyMoveOnlyEnqueueRequirement == nullptr);
        legacyMoveOnlyEnqueueRequirement = funcDecl;
      } else if (unownedJobDecl && param->getInterfaceType()->isEqual(unownedJobDecl->getDeclaredInterfaceType())) {
        assert(unownedEnqueueRequirement == nullptr);
        unownedEnqueueRequirement = funcDecl;
      }
    }

    // if we found all potential requirements, we're done here and break out of the loop
    if (unownedEnqueueRequirement &&
        moveOnlyEnqueueRequirement &&
        legacyMoveOnlyEnqueueRequirement)
      break; // we're done looking for the requirements
  }

  auto conformance = lookupConformance(nominalTy, proto);
  auto concreteConformance = conformance.getConcrete();
  assert(unownedEnqueueRequirement && "could not find the enqueue(UnownedJob) requirement, which should be always there");

  // try to find at least a single implementations of enqueue(_:)
  ValueDecl *unownedEnqueueWitnessDecl = concreteConformance->getWitnessDecl(unownedEnqueueRequirement);
  ValueDecl *moveOnlyEnqueueWitnessDecl = nullptr;
  ValueDecl *legacyMoveOnlyEnqueueWitnessDecl = nullptr;

  if (moveOnlyEnqueueRequirement) {
    moveOnlyEnqueueWitnessDecl = concreteConformance->getWitnessDecl(
        moveOnlyEnqueueRequirement);
  }
  if (legacyMoveOnlyEnqueueRequirement) {
    legacyMoveOnlyEnqueueWitnessDecl = concreteConformance->getWitnessDecl(
        legacyMoveOnlyEnqueueRequirement);
  }

  // --- Diagnose warnings and errors

  // true iff the nominal type's availability allows the legacy requirement
  // to be omitted in favor of moveOnlyEnqueueRequirement
  bool canRemoveOldDecls;
  if (!moveOnlyEnqueueRequirement) {
    // The move only enqueue does not exist in this lib version, we must keep relying on the UnownedJob version
    canRemoveOldDecls = false;
  } else if (C.LangOpts.DisableAvailabilityChecking) {
    // Assume we have all APIs available, and thus can use the ExecutorJob
    canRemoveOldDecls = true;
  } else {
    // Check if the availability of nominal is high enough to be using the ExecutorJob version
    AvailabilityRange requirementInfo =
        AvailabilityInference::availableRange(moveOnlyEnqueueRequirement);
    AvailabilityRange declInfo =
        AvailabilityContext::forDeclSignature(nominal).getPlatformRange();
    canRemoveOldDecls = declInfo.isContainedIn(requirementInfo);
  }

  auto concurrencyModule = C.getLoadedModule(C.Id_Concurrency);
  auto isStdlibDefaultImplDecl = [executorDecl, concurrencyModule](ValueDecl *witness) -> bool {
    if (!witness)
      return false;

    if (auto declContext = witness->getDeclContext()) {
      if (auto *extension = dyn_cast<ExtensionDecl>(declContext)) {
        auto extensionModule = extension->getParentModule();
        if (extensionModule != concurrencyModule) {
          return false;
        }

        if (auto extendedNominal = extension->getExtendedNominal()) {
          return extendedNominal->getDeclaredInterfaceType()->isEqual(
              executorDecl->getDeclaredInterfaceType());
        }
      }
    }
    return false;
  };

  // If both old and new enqueue are implemented, but the old one cannot be removed,
  // emit a warning that the new enqueue is unused.
  if (!canRemoveOldDecls && unownedEnqueueWitnessDecl && moveOnlyEnqueueWitnessDecl) {
    if (!isStdlibDefaultImplDecl(moveOnlyEnqueueWitnessDecl) &&
        !isStdlibDefaultImplDecl(unownedEnqueueWitnessDecl)) {
      diags.diagnose(moveOnlyEnqueueWitnessDecl->getLoc(),
                     diag::executor_enqueue_unused_implementation);
      if (auto decl = unownedEnqueueWitnessDecl) {
         decl->diagnose(diag::decl_declared_here, decl);
      }
    }
  }

  // We specifically do allow the old UnownedJob implementation to be present.
  // In order to ease migration and compatibility for libraries which remain compatible with old Swift versions,
  // and would be getting this warning in situations they cannot address it.

  // Old Job based impl is present, warn about it suggesting the new protocol requirement.
  if (legacyMoveOnlyEnqueueWitnessDecl) {
    if (!isStdlibDefaultImplDecl(legacyMoveOnlyEnqueueWitnessDecl)) {
      diags.diagnose(legacyMoveOnlyEnqueueWitnessDecl->getLoc(),
                     diag::executor_enqueue_deprecated_owned_job_implementation,
                     nominalTy);
    }
  }

  bool unownedEnqueueWitnessIsDefaultImpl = isStdlibDefaultImplDecl(unownedEnqueueWitnessDecl);
  bool moveOnlyEnqueueWitnessIsDefaultImpl = isStdlibDefaultImplDecl(moveOnlyEnqueueWitnessDecl);
  bool legacyMoveOnlyEnqueueWitnessDeclIsDefaultImpl = isStdlibDefaultImplDecl(legacyMoveOnlyEnqueueWitnessDecl);

  auto missingWitness = !unownedEnqueueWitnessDecl &&
                        !moveOnlyEnqueueWitnessDecl &&
                        !legacyMoveOnlyEnqueueWitnessDecl;
  auto allWitnessesAreDefaultImpls = unownedEnqueueWitnessIsDefaultImpl &&
                                     moveOnlyEnqueueWitnessIsDefaultImpl &&
                                     legacyMoveOnlyEnqueueWitnessDeclIsDefaultImpl;
  if ((missingWitness) ||
      (!missingWitness && allWitnessesAreDefaultImpls)) {
    // Neither old nor new implementation have been found, but we provide default impls for them
    // that are mutually recursive, so we must error and suggest implementing the right requirement.
    //
    // If we're running against an SDK that does not have the ExecutorJob enqueue function,
    // try to diagnose using the next-best one available.
    auto missingRequirement = C.getExecutorDecl()->getExecutorOwnedEnqueueFunction();
    if (!missingRequirement)
      missingRequirement = C.getExecutorDecl()->getExecutorLegacyOwnedEnqueueFunction();
    if (!missingRequirement)
      missingRequirement = C.getExecutorDecl()->getExecutorLegacyUnownedEnqueueFunction();

    if (missingRequirement) {
      nominal->diagnose(diag::type_does_not_conform, nominalTy, proto->getDeclaredInterfaceType());
      missingRequirement->diagnose(
          diag::no_witnesses, getProtocolRequirementKind(missingRequirement),
          missingRequirement,
          missingRequirement->getParameters()->get(0)->getInterfaceType());
      return;
    }
  }
}

bool swift::shouldIgnoreDeprecationOfConcurrencyDecl(const Decl *decl,
                                                     DeclContext *declContext) {
  auto &ctx = decl->getASTContext();
  auto concurrencyModule = ctx.getLoadedModule(ctx.Id_Concurrency);

  // Only suppress these diagnostics in the implementation of _Concurrency.
  if (declContext->getParentModule() != concurrencyModule)
    return false;

  // Only suppress deprecation diagnostics for decls defined in _Concurrency.
  if (decl->getDeclContext()->getParentModule() != concurrencyModule)
    return false;

  auto *legacyJobDecl = ctx.getJobDecl();
  auto *unownedJobDecl = ctx.getUnownedJobDecl();

  if (decl == legacyJobDecl)
    return true;

  if (auto *funcDecl = dyn_cast<FuncDecl>(decl)) {
    auto enqueueDeclName =
        DeclName(ctx, DeclBaseName(ctx.Id_enqueue), {Identifier()});

    if (funcDecl->getName() == enqueueDeclName &&
        funcDecl->getParameters()->size() == 1) {
      auto paramTy = funcDecl->getParameters()->front()->getInterfaceType();
      if (paramTy->isEqual(legacyJobDecl->getDeclaredInterfaceType()) ||
          paramTy->isEqual(unownedJobDecl->getDeclaredInterfaceType()))
        return true;
    }
  }

  return false;
}

/// Determine whether this is the main actor type.
static bool isMainActor(Type type) {
  if (auto nominal = type->getAnyNominal())
    return nominal->isMainActor();

  return false;
}

/// If this DeclContext is an actor, or an extension on an actor, return the
/// NominalTypeDecl, otherwise return null.
static NominalTypeDecl *getSelfActorDecl(const DeclContext *dc) {
  auto nominal = dc->getSelfNominalTypeDecl();
  return nominal && nominal->isActor() ? nominal : nullptr;
}

ReferencedActor ReferencedActor::forGlobalActor(VarDecl *actor,
                                                bool isPotentiallyIsolated,
                                                Type globalActor) {
  Kind kind = isMainActor(globalActor) ? MainActor : GlobalActor;
  return ReferencedActor(actor, isPotentiallyIsolated, kind, globalActor);
}

bool ReferencedActor::isKnownToBeLocal() const {
  switch (kind) {
  case GlobalActor:
  case AsyncLet:
  case MainActor:
  case NonIsolatedAutoclosure:
  case NonIsolatedContext:
  case NonIsolatedParameter:
  case SendableFunction:
  case SendableClosure:
    if (isPotentiallyIsolated)
      return true;

    return actor && actor->isKnownToBeLocal();

  case Isolated:
    return true;
  }
}

const AbstractFunctionDecl *
swift::isActorInitOrDeInitContext(const DeclContext *dc) {
  while (true) {
    // Stop looking if we hit an isolation inference boundary.
    if (auto *closure = dyn_cast<AbstractClosureExpr>(dc)) {
      if (isIsolationInferenceBoundaryClosure(closure,
                                              /*canInheritActorContext*/false))
        return nullptr;

      // Otherwise, look through our closure at the closure's parent decl
      // context.
      dc = dc->getParent();
      continue;
    }

    if (auto func = dyn_cast<AbstractFunctionDecl>(dc)) {
      // If this is an initializer or deinitializer of an actor, we're done.
      if ((isa<ConstructorDecl>(func) || isa<DestructorDecl>(func)) &&
          getSelfActorDecl(dc->getParent()))
        return func;

      // Non-Sendable local functions are considered part of the enclosing
      // context.
      if (func->getDeclContext()->isLocalContext()) {
        if (func->isSendable())
          return nullptr;

        dc = dc->getParent();
        continue;
      }
    }

    return nullptr;
  }
}

static bool isStoredProperty(ValueDecl const *member) {
  if (auto *var = dyn_cast<VarDecl>(member))
    if (var->hasStorage() && var->isInstanceMember())
      return true;
  return false;
}

static bool isNonInheritedStorage(ValueDecl const *member,
                                  DeclContext const *useDC) {
  auto *nominal = useDC->getParent()->getSelfNominalTypeDecl();
  if (!nominal)
    return false;

  return isStoredProperty(member) && member->getDeclContext() == nominal;
}

/// Based on the former escaping-use restriction, which was replaced by
/// flow-isolation. We need this to support backwards compatability in the
/// type-checker for programs prior to Swift 6.
/// \param fn either a constructor or destructor of an actor.
static bool wasLegacyEscapingUseRestriction(AbstractFunctionDecl *fn) {
  assert(fn->getDeclContext()->getSelfClassDecl()->isAnyActor());
  assert(isa<ConstructorDecl>(fn) || isa<DestructorDecl>(fn));

  auto isolationKind = getActorIsolation(fn).getKind();
  if (isa<DestructorDecl>(fn)) {
    switch (isolationKind) {
    case ActorIsolation::GlobalActor:
    case ActorIsolation::ActorInstance:
      // Isolated deinits did not exist before
      return false;
    case ActorIsolation::Nonisolated:
    case ActorIsolation::NonisolatedUnsafe:
    case ActorIsolation::Unspecified:
      assert(!fn->hasAsync());
      return true;
    case ActorIsolation::CallerIsolationInheriting:
      llvm_unreachable(
          "destructor decl cannot have non implicit actor instance isolation");
    case ActorIsolation::Erased:
      llvm_unreachable("destructor decl cannot have erased isolation");
    }
  } else if (auto *ctor = dyn_cast<ConstructorDecl>(fn)) {
    switch (isolationKind) {
    case ActorIsolation::Nonisolated:
    case ActorIsolation::NonisolatedUnsafe:
    case ActorIsolation::GlobalActor:
      // convenience inits did not have the restriction.
      if (ctor->isConvenienceInit())
        return false;
      break;
    case ActorIsolation::ActorInstance:
      // none of these had the restriction affect them.
      assert(fn->hasAsync());
      return false;
    case ActorIsolation::Erased:
      llvm_unreachable("constructor decl cannot have erased isolation");
    case ActorIsolation::CallerIsolationInheriting:
      llvm_unreachable(
          "constructor decl cannot have caller isolation inheriting isolation");

    case ActorIsolation::Unspecified:
      // this is basically just objc-marked inits.
      break;
    }
  }
  return !(fn->hasAsync()); // basic case: not async = had restriction.
}

/// Note that while a direct access to the actor-isolated property is not legal
/// you may want to consider introducing an accessing method for a mutation.
static void
maybeNoteMutatingMethodSuggestion(ASTContext &C,
                                  ValueDecl const *member,
                                  SourceLoc memberLoc,
                                  DeclContext const *refCxt,
                                  ActorIsolation isolation,
                                  std::optional<VarRefUseEnv> useKind) {
  if (!member || !isa<VarDecl>(member))
      return; // we only offer the note property mutations

  if (!(isolation.getKind() == ActorIsolation::Kind::ActorInstance ||
        isolation.getKind() == ActorIsolation::Kind::GlobalActor)) {
      return;
  }

  if (useKind != VarRefUseEnv::Mutating) {
      // This note is tailored for the 'mutating' access, i.e. when
      // attempting to mutate a property, they should instead make an actor method
      // to perform the mutation. Reading properties does not have the same restriction.
      return;
  }

  if (!refCxt->isAsyncContext()) {
      // don't suggest creating method when in sync context, as we won't be able
      // to invoke it anyway so this would not be helpful to suggest
      return;
  }

  if (auto actor = isolation.getActor()) {
      C.Diags.diagnose(
          memberLoc,
          diag::note_consider_method_for_isolated_property_mutation,
          actor);
  }
}

/// Note that the given actor member is isolated.
static void noteIsolatedActorMember(ValueDecl const *decl,
                                    std::optional<VarRefUseEnv> useKind) {
  // detect if it is a distributed actor, to provide better isolation notes

  auto nominal = decl->getDeclContext()->getSelfNominalTypeDecl();
  bool isDistributedActor = false;
  if (nominal) isDistributedActor = nominal->isDistributedActor();

  // FIXME: Make this diagnostic more sensitive to the isolation context of
  // the declaration.
  if (isDistributedActor) {
    if (auto varDecl = dyn_cast<VarDecl>(decl)) {
      if (varDecl->isDistributed()) {
        // This is an attempt to access a `distributed var` synchronously, so
        // offer a more detailed error
        decl->diagnose(diag::distributed_actor_synchronous_access_distributed_computed_property,
                       decl,
                       nominal->getName());
      } else {
        // Distributed actor properties are never accessible externally.
        decl->diagnose(diag::distributed_actor_isolated_property,
                       decl,
                       nominal->getName());
      }

    } else {
      // it's a function or subscript
      decl->diagnose(diag::note_distributed_actor_isolated_method, decl);
    }
  } else if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
    func->diagnose(diag::actor_isolated_sync_func, decl);

    // was it an attempt to mutate an actor instance's isolated state?
  } else if (useKind) {
    if (*useKind == VarRefUseEnv::Read)
      decl->diagnose(diag::kind_declared_here, decl->getDescriptiveKind());
    else
      decl->diagnose(diag::actor_mutable_state, decl);

  } else {
    decl->diagnose(diag::kind_declared_here, decl->getDescriptiveKind());
  }
}

/// An ad-hoc check specific to member isolation checking. assumed to be
/// queried when a self-member is being accessed in a context which is not
/// isolated to self. The "special permission" is part of a backwards
/// compatability with actor inits and deinits that maintains the
/// permissive nature of the escaping-use restriction, which was only
/// staged in as a warning. See implementation for more details.
///
/// \returns true if this access in the given context should be allowed
/// in Sema, with the side-effect of emitting a warning as needed.
/// If false is returned, then the "special permission" was not granted.
static bool memberAccessHasSpecialPermissionInSwift5(
    DeclContext const *refCxt, ReferencedActor &baseActor,
    ValueDecl const *member, SourceLoc memberLoc,
    std::optional<VarRefUseEnv> useKind) {
  // no need for this in Swift 6+
  if (refCxt->getASTContext().isSwiftVersionAtLeast(6))
    return false;

  // must be an access to an instance member.
  if (!member->isInstanceMember())
    return false;

  // In the history of actor initializers prior to Swift 6, self-isolated
  // members could be referenced from any init or deinit, even a synchronous
  // one, with no diagnostics at all.
  //
  // When the escaping-use restriction came into place for the release of
  // 5.5, it was implemented as a warning and only applied to initializers,
  // which stated that it would become an error in Swift 6.
  //
  // Once 5.6 was released, we also added restrictions in the deinits of
  // actors, at least for accessing members other than stored properties.
  //
  // Later on, for 5.7 we introduced flow-isolation as part of SE-327 for
  // both inits and deinits. This meant that stored property accesses now
  // are only sometimes going to be problematic. This change also brought
  // official changes in isolation for the inits and deinits to handle the
  // the non-stored-property members. Since those isolation changes are
  // currently in place, the purpose of the code below is to override the
  // isolation checking, so that the now-mismatched isolation on member
  // access is still permitted, but with a warning stating that it will
  // be rejected in Swift 6.
  //
  // In the checking below, we let stored-property accesses go ignored,
  // so that flow-isolation can warn about them only if needed. This helps
  // prevent needless warnings on property accesses that will actually be OK
  // with flow-isolation in the future.
  if (auto oldFn = isActorInitOrDeInitContext(refCxt)) {
    auto oldFnMut = const_cast<AbstractFunctionDecl*>(oldFn);

    // If function did not have the escaping-use restriction, then it gets
    // no special permissions here.
    if (!wasLegacyEscapingUseRestriction(oldFnMut))
      return false;

    // At this point, the special permission will be granted. But, we
    // need to warn now about this permission being taken away in Swift 6
    // for specific kinds of non-stored-property member accesses:

    // If the context in which we consider the access matches between the
    // old (escaping-use restriction) and new (flow-isolation) contexts,
    // and it is a stored or init accessor property, then permit it here
    // without any warning.
    // Later, flow-isolation pass will check and emit a warning if needed.
    if (refCxt == oldFn) {
      if (isStoredProperty(member))
        return true;

      if (auto *var = dyn_cast<VarDecl>(member)) {
        // Init accessor properties are permitted to access only stored
        // properties.
        if (var->hasInitAccessor())
          return true;
      }
    }

    // Otherwise, it's definitely going to be illegal, so warn and permit.
    auto &C = refCxt->getASTContext();
    auto &diags = C.Diags;
    auto useKindInt = static_cast<unsigned>(
        useKind.value_or(VarRefUseEnv::Read));

    auto isolation = getActorIsolation(const_cast<ValueDecl *>(member));
    diags.diagnose(
        memberLoc, diag::actor_isolated_non_self_reference,
        member,
        useKindInt,
        baseActor.kind + 1,
        baseActor.globalActor,
        isolation)
    .warnUntilSwiftVersion(6);

    noteIsolatedActorMember(member, useKind);
    maybeNoteMutatingMethodSuggestion(C, member, memberLoc, refCxt, isolation, useKind);
    return true;
  }

  return false;
}

/// To support flow-isolation, some member accesses in inits / deinits
/// must be permitted, despite the isolation of 'self' not being
/// correct in Sema.
///
/// \param refCxt the context in which the member reference happens.
/// \param baseActor the actor referenced in the base of the member access.
/// \param member the declaration corresponding to the accessed member.
/// \param memberLoc the source location of the reference to the member.
///
/// \returns true iff the member access is permitted in Sema because it will
/// be verified later by flow-isolation.
static bool checkedByFlowIsolation(DeclContext const *refCxt,
                                   ReferencedActor &baseActor,
                                   ValueDecl const *member, SourceLoc memberLoc,
                                   std::optional<VarRefUseEnv> useKind) {

  // base of member reference must be `self`
  if (!baseActor.isSelf())
    return false;

  // Must be directly in an init/deinit that uses flow-isolation,
  // or a defer within such a functions.
  //
  // NOTE: once flow-isolation can analyze calls to arbitrary local
  // functions, we should be using isActorInitOrDeInitContext instead
  // of this ugly loop.
  AbstractFunctionDecl const* fnDecl = nullptr;
  while (true) {
    fnDecl = dyn_cast_or_null<AbstractFunctionDecl>(refCxt->getAsDecl());
    if (!fnDecl)
      break;

    // go up one level if this context is a defer.
    if (auto *d = dyn_cast<FuncDecl>(fnDecl)) {
      if (d->isDeferBody()) {
        refCxt = refCxt->getParent();
        continue;
      }
    }
    break;
  }

  if (memberAccessHasSpecialPermissionInSwift5(refCxt, baseActor, member,
                                               memberLoc, useKind))
    return true; // then permit it now.

  if (!usesFlowSensitiveIsolation(fnDecl))
    return false;

  // Stored properties are definitely OK.
  if (isNonInheritedStorage(member, fnDecl))
    return true;

  return false;
}

/// Get the actor isolation of the innermost relevant context.
static ActorIsolation getInnermostIsolatedContext(
    const DeclContext *dc,
    llvm::function_ref<ActorIsolation(AbstractClosureExpr *)>
        getClosureActorIsolation) {
  // Retrieve the actor isolation of the context.
  auto mutableDC = const_cast<DeclContext *>(dc);
  switch (auto isolation =
              getActorIsolationOfContext(mutableDC, getClosureActorIsolation)) {
  case ActorIsolation::CallerIsolationInheriting:
  case ActorIsolation::ActorInstance:
  case ActorIsolation::Nonisolated:
  case ActorIsolation::NonisolatedUnsafe:
  case ActorIsolation::Unspecified:
    return isolation;

  case ActorIsolation::Erased:
    llvm_unreachable("closure cannot originally have dynamic isolation");

  case ActorIsolation::GlobalActor:
    return ActorIsolation::forGlobalActor(
        dc->mapTypeIntoContext(isolation.getGlobalActor()))
          .withPreconcurrency(isolation.preconcurrency());
  }
}

AbstractFunctionDecl *swift::enclosingUnsafeInheritsExecutor(
    const DeclContext *dc) {
  for (; dc; dc = dc->getParent()) {
    if (auto func = dyn_cast<AbstractFunctionDecl>(dc)) {
      if (func->getAttrs().hasAttribute<UnsafeInheritExecutorAttr>()) {
        return const_cast<AbstractFunctionDecl *>(func);
      }

      return nullptr;
    }

    if (isa<AbstractClosureExpr>(dc))
      return nullptr;

    if (dc->isTypeContext())
      return nullptr;
  }

  return nullptr;
}

/// Adjust the location used for diagnostics about #isolation to account for
/// the fact that they show up in macro expansions.
///
/// Returns a pair containing the updated location and whether it's part of
/// a default argument.
static std::pair<SourceLoc, bool> adjustPoundIsolationDiagLoc(
    CurrentContextIsolationExpr *isolationExpr,
    ModuleDecl *module
) {
  // Not part of a macro expansion.
  SourceLoc diagLoc = isolationExpr->getLoc();
  auto sourceFile = module->getSourceFileContainingLocation(diagLoc);
  if (!sourceFile)
    return { diagLoc, false };
  auto macroExpansionRange = sourceFile->getMacroInsertionRange();
  if (macroExpansionRange.Start.isInvalid())
    return { diagLoc, false };

  diagLoc = macroExpansionRange.Start;

  // If this is from a default argument, note that and go one more
  // level "out" to the place where the default argument was
  // introduced.
  auto expansionSourceFile = module->getSourceFileContainingLocation(diagLoc);
  if (!expansionSourceFile ||
      expansionSourceFile->Kind != SourceFileKind::DefaultArgument)
    return { diagLoc, false };

  return {
    expansionSourceFile->getNodeInEnclosingSourceFile().getStartLoc(),
    true
  };
}

void swift::replaceUnsafeInheritExecutorWithDefaultedIsolationParam(
    AbstractFunctionDecl *func, InFlightDiagnostic &diag) {
  auto attr = func->getAttrs().getAttribute<UnsafeInheritExecutorAttr>();
  assert(attr && "Caller didn't validate the presence of the attribute");

  // Look for the place where we should insert the new 'isolation' parameter.
  // We insert toward the back, but skip over any parameters that have function
  // type.
  unsigned insertionPos = func->getParameters()->size();
  while (insertionPos > 0) {
    Type paramType = func->getParameters()->get(insertionPos - 1)->getInterfaceType();
    if (paramType->lookThroughSingleOptionalType()->is<AnyFunctionType>()) {
      --insertionPos;
      continue;
    }

    break;
  }

  // Determine the text to insert. We put the commas before and after, then
  // slice them away depending on whether we have parameters before or after.
  StringRef newParameterText = ", isolation: isolated (any Actor)? = #isolation, ";
  if (insertionPos == 0)
    newParameterText = newParameterText.drop_front(2);
  if (insertionPos == func->getParameters()->size())
    newParameterText = newParameterText.drop_back(2);

  // Determine where to insert the new parameter.
  SourceLoc insertionLoc;
  if (insertionPos < func->getParameters()->size()) {
    insertionLoc = func->getParameters()->get(insertionPos)->getStartLoc();
  } else {
    insertionLoc = func->getParameters()->getRParenLoc();
  }

  diag.fixItRemove(attr->getRangeWithAt());
  diag.fixItInsert(insertionLoc, newParameterText);
}

/// Whether this declaration context is in the _Concurrency module.
static bool inConcurrencyModule(const DeclContext *dc) {
  return dc->getParentModule()->getName().str() == "_Concurrency";
}

void swift::introduceUnsafeInheritExecutorReplacements(
    const DeclContext *dc, SourceLoc loc, SmallVectorImpl<ValueDecl *> &decls) {
  if (decls.empty())
    return;

  auto isReplaceable = [&](ValueDecl *decl) {
    return isa<FuncDecl>(decl) && inConcurrencyModule(decl->getDeclContext()) &&
        decl->getDeclContext()->isModuleScopeContext() &&
        cast<FuncDecl>(decl)->hasAsync();
  };

  // Make sure at least some of the entries are functions in the _Concurrency
  // module.
  ModuleDecl *concurrencyModule = nullptr;
  DeclBaseName baseName;
  for (auto decl: decls) {
    if (isReplaceable(decl)) {
      concurrencyModule = decl->getDeclContext()->getParentModule();
      baseName = decl->getName().getBaseName();
      break;
    }
  }
  if (!concurrencyModule)
    return;

  // Ignore anything with a special name.
  if (baseName.isSpecial())
    return;

  // Look for entities with the _unsafeInheritExecutor_ prefix on the name.
  ASTContext &ctx = decls.front()->getASTContext();
  Identifier newIdentifier = ctx.getIdentifier(
      ("_unsafeInheritExecutor_" + baseName.getIdentifier().str()).str());

  NameLookupOptions lookupOptions = defaultUnqualifiedLookupOptions;
  LookupResult lookup = TypeChecker::lookupUnqualified(
      const_cast<DeclContext *>(dc), DeclNameRef(newIdentifier), loc,
      lookupOptions);
  if (!lookup)
    return;

  // Drop all of the _Concurrency entries in favor of the ones found by this
  // lookup.
  decls.erase(std::remove_if(decls.begin(), decls.end(), [&](ValueDecl *decl) {
                return isReplaceable(decl);
              }),
              decls.end());
  for (const auto &lookupResult: lookup) {
    if (auto decl = lookupResult.getValueDecl())
      decls.push_back(decl);
  }
}

void swift::introduceUnsafeInheritExecutorReplacements(
    const DeclContext *dc, Type base, SourceLoc loc, LookupResult &lookup) {
  if (lookup.empty())
    return;

  SmallVector<NominalTypeDecl *, 4> nominalTypes;
  namelookup::tryExtractDirectlyReferencedNominalTypes(base, nominalTypes);
  if (llvm::none_of(nominalTypes, [](NominalTypeDecl *baseNominal) {
        return inConcurrencyModule(baseNominal);
      }))
    return;

  auto isReplaceable = [&](ValueDecl *decl) {
    return isa<FuncDecl>(decl) && inConcurrencyModule(decl->getDeclContext()) &&
      cast<FuncDecl>(decl)->hasAsync();
  };

  // Make sure at least some of the entries are functions in the _Concurrency
  // module.
  ModuleDecl *concurrencyModule = nullptr;
  DeclBaseName baseName;
  for (auto &result: lookup) {
    auto decl = result.getValueDecl();
    if (isReplaceable(decl)) {
      concurrencyModule = decl->getDeclContext()->getParentModule();
      baseName = decl->getBaseName();
      break;
    }
  }
  if (!concurrencyModule)
    return;

  // Ignore anything with a special name.
  if (baseName.isSpecial())
    return;

  // Look for entities with the _unsafeInheritExecutor_ prefix on the name.
  ASTContext &ctx = base->getASTContext();
  Identifier newIdentifier = ctx.getIdentifier(
      ("_unsafeInheritExecutor_" + baseName.getIdentifier().str()).str());

  LookupResult replacementLookup = TypeChecker::lookupMember(
      const_cast<DeclContext *>(dc), base, DeclNameRef(newIdentifier), loc,
      defaultMemberLookupOptions);
  if (replacementLookup.innerResults().empty())
    return;

  // Drop all of the _Concurrency entries in favor of the ones found by this
  // lookup.
  lookup.filter([&](const LookupResultEntry &entry, bool) {
    return !isReplaceable(entry.getValueDecl());
  });

  for (const auto &entry: replacementLookup.innerResults()) {
    lookup.add(entry, /*isOuter=*/false);
  }
}

/// Check if it is safe for the \c globalActor qualifier to be removed from
/// \c ty, when the function value of that type is isolated to that actor.
///
/// In general this is safe in a narrow but common case: a global actor
/// qualifier can be dropped from a function type while in a DeclContext
/// isolated to that same actor, as long as the value is not Sendable.
///
/// \param dc the innermost context in which the cast to remove the global actor
///           is happening.
/// \param globalActor global actor that was dropped from \c ty.
/// \param ty a function type where \c globalActor was removed from it.
/// \return true if it is safe to drop the global-actor qualifier.
static bool safeToDropGlobalActor(
    DeclContext *dc, Type globalActor, Type ty, ApplyExpr *call) {
  auto funcTy = ty->getAs<AnyFunctionType>();
  if (!funcTy)
    return false;

  auto otherIsolation = funcTy->getIsolation();

  // can't add a different global actor
  if (otherIsolation.isGlobalActor()) {
    assert(otherIsolation.getGlobalActorType()->getCanonicalType()
             != globalActor->getCanonicalType()
           && "not even dropping the actor?");
    return false;
  }

  // Converting to an isolation-erased function type is fine.
  if (otherIsolation.isErased())
    return true;

  // If the argument is passed over an isolation boundary, it's not
  // safe to erase actor isolation, because the callee can call the
  // function synchronously from outside the isolation domain.
  if (call && call->getIsolationCrossing())
    return false;

  // fundamentally cannot be sendable if we want to drop isolation info
  if (funcTy->isSendable())
    return false;

  // finally, must be in a context with matching isolation.
  auto dcIsolation = getActorIsolationOfContext(dc);
  if (dcIsolation.isGlobalActor())
    if (dcIsolation.getGlobalActor()->getCanonicalType()
        == globalActor->getCanonicalType())
      return true;

  return false;
}

static FuncDecl *findAnnotatableFunction(DeclContext *dc) {
  auto fn = dyn_cast<FuncDecl>(dc);
  if (!fn) return nullptr;
  if (fn->isDeferBody())
    return findAnnotatableFunction(fn->getDeclContext());
  return fn;
}

namespace {
  /// Check for adherence to the actor isolation rules, emitting errors
  /// when actor-isolated declarations are used in an unsafe manner.
  class ActorIsolationChecker : public ASTWalker {
    ASTContext &ctx;
    SmallVector<const DeclContext *, 4> contextStack;
    SmallVector<llvm::PointerUnion<ApplyExpr *, LookupExpr *>, 4> applyStack;
    SmallVector<std::pair<OpaqueValueExpr *, Expr *>, 4> opaqueValues;
    SmallVector<const PatternBindingDecl *, 2> patternBindingStack;
    llvm::function_ref<Type(Expr *)> getType;
    llvm::function_ref<ActorIsolation(AbstractClosureExpr *)>
        getClosureActorIsolation;
    /// Whether to check if the closure captures an `isolated` parameter.
    /// This is needed as a workaround during code completion, which doesn't
    /// have types applied to the AST and thus doesn't have captures computed.
    bool checkIsolatedCapture;

    SourceLoc requiredIsolationLoc;

    /// Used under the mode to compute required actor isolation for
    /// an expression or function.
    llvm::SmallDenseMap<const DeclContext *, ActorIsolation> requiredIsolation;

    using ActorRefKindPair = std::pair<ReferencedActor::Kind, ActorIsolation>;

    using IsolationPair = std::pair<ActorIsolation, ActorIsolation>;

    using DiagnosticList = std::vector<IsolationError>;

    llvm::DenseMap<ActorRefKindPair, DiagnosticList> refErrors;

    llvm::DenseMap<IsolationPair, DiagnosticList> applyErrors;

    /// Keeps track of the capture context of variables that have been
    /// explicitly captured in closures.
    llvm::SmallDenseMap<VarDecl *, TinyPtrVector<const DeclContext *>>
      captureContexts;

    using MutableVarSource
        = llvm::PointerUnion<DeclRefExpr *, InOutExpr *, LookupExpr *>;

    using MutableVarParent
        = llvm::PointerUnion<InOutExpr *, LoadExpr *, AssignExpr *>;

    ApplyExpr *getImmediateApply() const {
      if (applyStack.empty())
        return nullptr;

      return applyStack.back().dyn_cast<ApplyExpr *>();
    }

    /// Note when the enclosing context could be put on a global actor.
    // FIXME: This should handle closures too.
    static bool missingGlobalActorOnContext(DeclContext *dc, Type globalActor,
                                            DiagnosticBehavior behavior) {
      // If we are in a synchronous function on the global actor,
      // suggest annotating with the global actor itself.
      if (auto fn = findAnnotatableFunction(dc)) {
        // Suppress this for accessors because you can't change the
        // actor isolation of an individual accessor.  Arguably we could
        // add this to the entire storage declaration, though.
        // Suppress this for async functions out of caution; but don't
        // suppress it if we looked through a defer.
        if (!isa<AccessorDecl>(fn) &&
            (!fn->isAsyncContext() || fn != dc)) {
          switch (getActorIsolation(fn)) {
          case ActorIsolation::ActorInstance:
          case ActorIsolation::CallerIsolationInheriting:
          case ActorIsolation::GlobalActor:
          case ActorIsolation::Nonisolated:
          case ActorIsolation::NonisolatedUnsafe:
              return false;

          case ActorIsolation::Erased:
            llvm_unreachable("function cannot have erased isolation");

          case ActorIsolation::Unspecified:
            if (behavior != DiagnosticBehavior::Note) {
              fn->diagnose(diag::invalid_isolated_calls_in_body,
                           globalActor->getString(), fn)
                  .limitBehaviorUntilSwiftVersion(behavior, 6);
            }

            // Overrides cannot be isolated to a global actor; the isolation
            // must match the overridden decl.
            if (fn->getOverriddenDecl())
              return false;

            fn->diagnose(diag::add_globalactor_to_decl,
                         globalActor->getString(), fn, globalActor)
                .fixItInsert(fn->getAttributeInsertionLoc(false),
                             diag::insert_globalactor_attr, globalActor);
            return true;
          }
        }
      }
      return false;
    }

  public:
    bool diagnoseIsolationErrors() {
      bool diagnosedError = false;

      for (auto list : refErrors) {
        ActorRefKindPair key = list.getFirst();
        DiagnosticList errors = list.getSecond();
        ActorIsolation isolation = key.second;

        auto behavior = DiagnosticBehavior::Warning;
        // Upgrade behavior if @preconcurrency not detected
        if (llvm::any_of(errors, [&](IsolationError error) {
          return !error.preconcurrency;
        })) {
          behavior = DiagnosticBehavior::Error;
        }

        // Add Fix-it for missing @SomeActor annotation
        if (isolation.isGlobalActor()) {
          if (missingGlobalActorOnContext(
                  const_cast<DeclContext *>(getDeclContext()),
                  isolation.getGlobalActor(), behavior) &&
              errors.size() > 1) {
            behavior = DiagnosticBehavior::Note;
          }
        }

        for (IsolationError error : errors) {
          // Diagnose actor_isolated_non_self_reference as note
          // if there are multiple of these diagnostics
          ctx.Diags.diagnose(error.loc, error.diag)
            .limitBehaviorUntilSwiftVersion(behavior, 6);
        }
      }

      for (auto list : applyErrors) {
        IsolationPair key = list.getFirst();
        DiagnosticList errors = list.getSecond();
        ActorIsolation isolation = key.first;

        auto behavior = DiagnosticBehavior::Warning;
        // Upgrade behavior if @preconcurrency not detected
        if (llvm::any_of(errors, [&](IsolationError error) {
          return !error.preconcurrency;
        })) {
          behavior = DiagnosticBehavior::Error;
        }


        // Add Fix-it for missing @SomeActor annotation
        if (isolation.isGlobalActor()) {
          if (missingGlobalActorOnContext(
                  const_cast<DeclContext *>(getDeclContext()),
                  isolation.getGlobalActor(), behavior) &&
              errors.size() > 1) {
            behavior = DiagnosticBehavior::Note;
          }
        }

        for (IsolationError error : errors) {
          // Diagnose actor_isolated_call as note if
          // if there are multiple actor-isolated function calls
          // from outside the actor
          ctx.Diags.diagnose(error.loc, error.diag)
            .limitBehaviorUntilSwiftVersion(behavior, 6);
        }
      }

      return diagnosedError;
    }

  private:
    const PatternBindingDecl *getTopPatternBindingDecl() const {
      return patternBindingStack.empty() ? nullptr : patternBindingStack.back();
    }

    /// Mapping from mutable variable reference exprs, or inout expressions,
    /// to the parent expression, when that parent is either a load or
    /// an inout expr.
    llvm::SmallDenseMap<MutableVarSource, MutableVarParent, 4>
      mutableLocalVarParent;

    static bool isPropOrSubscript(ValueDecl const* decl) {
      return isa<VarDecl>(decl) || isa<SubscriptDecl>(decl);
    }

    /// In the given expression \c use that refers to the decl, this
    /// function finds the kind of environment tracked by
    /// \c mutableLocalVarParent that corresponds to that \c use.
    ///
    /// Note that an InoutExpr is not considered a use of the decl!
    ///
    /// @returns None if the context expression is either an InOutExpr,
    ///               not tracked, or if the decl is not a property or subscript
    std::optional<VarRefUseEnv> kindOfUsage(ValueDecl const *decl,
                                            Expr *use) const {
      // we need a use for lookup.
      if (!use)
        return std::nullopt;

      // must be a property or subscript
      if (!isPropOrSubscript(decl))
        return std::nullopt;

      if (auto lookup = dyn_cast<DeclRefExpr>(use))
        return usageEnv(lookup);
      else if (auto lookup = dyn_cast<LookupExpr>(use))
        return usageEnv(lookup);

      return std::nullopt;
    }

    /// @returns the kind of environment in which this expression appears, as
    ///          tracked by \c mutableLocalVarParent
    VarRefUseEnv usageEnv(MutableVarSource src) const {
      auto result = mutableLocalVarParent.find(src);
      if (result != mutableLocalVarParent.end()) {
        MutableVarParent parent = result->second;
        assert(!parent.isNull());
        if (parent.is<LoadExpr*>())
          return VarRefUseEnv::Read;
        else if (parent.is<AssignExpr*>())
          return VarRefUseEnv::Mutating;
        else if (auto inout = parent.dyn_cast<InOutExpr*>())
          return inout->isImplicit() ? VarRefUseEnv::Mutating
                                     : VarRefUseEnv::Inout;
        else
          llvm_unreachable("non-exhaustive case match");
      }
      return VarRefUseEnv::Read; // assume if it's not tracked, it's only read.
    }

    const DeclContext *getDeclContext() const {
      return contextStack.back();
    }

    ModuleDecl *getParentModule() const {
      return getDeclContext()->getParentModule();
    }

    /// Determine whether code in the given use context might execute
    /// concurrently with code in the definition context.
    bool mayExecuteConcurrentlyWith(
        const DeclContext *useContext, const DeclContext *defContext,
        bool includeSending = false);

    /// If the subexpression is a reference to a mutable local variable from a
    /// different context, record its parent. We'll query this as part of
    /// capture semantics in concurrent functions.
    ///
    /// \returns true if we recorded anything, false otherwise.
    bool recordMutableVarParent(MutableVarParent parent, Expr *subExpr) {
      subExpr = subExpr->getValueProvidingExpr();

      if (auto declRef = dyn_cast<DeclRefExpr>(subExpr)) {
        auto var = dyn_cast_or_null<VarDecl>(declRef->getDecl());
        if (!var)
          return false;

        // Only mutable variables matter.
        if (!var->supportsMutation())
          return false;

        // Only mutable variables outside of the current context. This is an
        // optimization, because the parent map won't be queried in this case,
        // and it is the most common case for variables to be referenced in
        // their own context.
        if (var->getDeclContext() == getDeclContext())
          return false;

        assert(mutableLocalVarParent[declRef].isNull());
        mutableLocalVarParent[declRef] = parent;
        return true;
      }

      // For a member reference, try to record a parent for the base expression.
      if (auto memberRef = dyn_cast<MemberRefExpr>(subExpr)) {
        // Record the parent of this LookupExpr too.
        mutableLocalVarParent[memberRef] = parent;
        return recordMutableVarParent(parent, memberRef->getBase());
      }

      // For a subscript, try to record a parent for the base expression.
      if (auto subscript = dyn_cast<SubscriptExpr>(subExpr)) {
        // Record the parent of this LookupExpr too.
        mutableLocalVarParent[subscript] = parent;
        return recordMutableVarParent(parent, subscript->getBase());
      }

      // Look through postfix '!'.
      if (auto force = dyn_cast<ForceValueExpr>(subExpr)) {
        return recordMutableVarParent(parent, force->getSubExpr());
      }

      // Look through postfix '?'.
      if (auto bindOpt = dyn_cast<BindOptionalExpr>(subExpr)) {
        return recordMutableVarParent(parent, bindOpt->getSubExpr());
      }

      if (auto optEval = dyn_cast<OptionalEvaluationExpr>(subExpr)) {
        return recordMutableVarParent(parent, optEval->getSubExpr());
      }

      // & expressions can be embedded for references to mutable variables
      // or subscribes inside a struct/enum.
      if (auto inout = dyn_cast<InOutExpr>(subExpr)) {
        // Record the parent of the inout so we don't look at it again later.
        mutableLocalVarParent[inout] = parent;
        return recordMutableVarParent(parent, inout->getSubExpr());
      }

      // Look through an expression that opens an existential
      if (auto openExist = dyn_cast<OpenExistentialExpr>(subExpr)) {
        return recordMutableVarParent(parent, openExist->getSubExpr());
      }

      return false;
    }

    /// Some function conversions synthesized by the constraint solver may not
    /// be correct AND the solver doesn't know, so we must emit a diagnostic.
    void checkFunctionConversion(Expr *funcConv, Type fromType, Type toType) {
      auto diagnoseNonSendableParametersAndResult =
          [&](FunctionType *fnType,
              std::optional<unsigned> warnUntilSwiftMode = std::nullopt) {
            auto *dc = getDeclContext();
            llvm::SmallPtrSet<Type, 2> nonSendableTypes;

            SendableCheckContext context(dc);
            for (auto &param : fnType->getParams()) {
              diagnoseNonSendableTypes(
                  param.getPlainType(), context,
                  /*inDerivedConformance=*/Type(), funcConv->getLoc(),
                  [&](Type type, DiagnosticBehavior behavior) {
                    nonSendableTypes.insert(type);
                    return true;
                  });
            }

            diagnoseNonSendableTypes(
                fnType->getResult(), context,
                /*inDerivedConformance=*/Type(), funcConv->getLoc(),
                [&](Type type, DiagnosticBehavior behavior) {
                  nonSendableTypes.insert(type);
                  return true;
                });

            if (!nonSendableTypes.empty()) {
              {
                auto diag = ctx.Diags.diagnose(
                    funcConv->getLoc(),
                    diag::invalid_function_conversion_with_non_sendable,
                    fromType, toType);

                if (warnUntilSwiftMode)
                  diag.warnUntilSwiftVersion(*warnUntilSwiftMode);
              }

              for (auto type : nonSendableTypes) {
                ctx.Diags.diagnose(funcConv->getLoc(),
                                   diag::type_does_not_conform_to_Sendable,
                                   type);
              }
            }
          };

      if (auto fromFnType = fromType->getAs<FunctionType>()) {
        if (auto toFnType = toType->getAs<FunctionType>()) {
          auto fromIsolation = fromFnType->getIsolation();
          auto toIsolation = toFnType->getIsolation();

          if (auto fromActor = fromFnType->getGlobalActor()) {
            if (!toFnType->hasGlobalActor()) {
              auto dc = const_cast<DeclContext *>(getDeclContext());
              // If it's unsafe to drop global actor attribute:
              //  - for Sendable types we are going to perform Sendability
              //    checking of parameters/result.
              //  - for non-Sendable types we either leave it to region-based
              //    isolation to determine whether it's okay or not or
              //    diagnose if types are not-async.
              if (safeToDropGlobalActor(dc, fromActor, toType,
                                        getImmediateApply())) {
                return;
              }

              if (!toFnType->isAsync()) {
                ctx.Diags
                    .diagnose(funcConv->getLoc(),
                              diag::converting_func_loses_global_actor,
                              fromType, toType, fromActor)
                    .warnUntilSwiftVersion(6);
                return;
              }
            }
          }

          // @isolated(any) functions (async or not) cannot be converted to
          // synchronous, non-@isolated(any) functions.
          if (fromIsolation.isErased() && !toIsolation.isErased() &&
              !toFnType->isAsync()) {
            ctx.Diags
                .diagnose(funcConv->getLoc(),
                          diag::isolated_any_conversion_to_synchronous_func,
                          fromFnType, toFnType)
                .warnUntilFutureSwiftVersion();
            return;
          }

          // Conversions from non-Sendable types are handled by
          // region-based isolation.
          // Function conversions are used to inject concurrency attributes
          // into interface types until that changes we won't be able to
          // diagnose all of the cases here.
          if (!fromFnType->isSendable())
            return;

          switch (toIsolation.getKind()) {
          // Converting to `nonisolated(nonsending)` function type
          case FunctionTypeIsolation::Kind::NonIsolatedCaller: {
            switch (fromIsolation.getKind()) {
            case FunctionTypeIsolation::Kind::NonIsolated: {
              // nonisolated -> nonisolated(nonsending) doesn't cross
              // an isolation boundary.
              if (!fromFnType->isAsync())
                break;

              // @concurrent -> nonisolated(nonsending)
              // crosses an isolation boundary.
              LLVM_FALLTHROUGH;
            }
            case FunctionTypeIsolation::Kind::GlobalActor:
            case FunctionTypeIsolation::Kind::Erased:
              diagnoseNonSendableParametersAndResult(toFnType);
              break;

            case FunctionTypeIsolation::Kind::Parameter:
              llvm_unreachable("invalid conversion");

            case FunctionTypeIsolation::Kind::NonIsolatedCaller:
              // Non isolated caller is always async. This can only occur if we
              // are converting from an `@Sendable` representation to something
              // else. So we need to just check that we diagnose non sendable
              // parameters and results.
              diagnoseNonSendableParametersAndResult(toFnType);
              break;
            }
            break;
          }

          // Converting to nonisolated synchronous or @concurrent
          // asynchronous function type could require crossing an
          // isolation boundary.
          case FunctionTypeIsolation::Kind::NonIsolated: {
            switch (fromIsolation.getKind()) {
            case FunctionTypeIsolation::Kind::Parameter:
            case FunctionTypeIsolation::Kind::NonIsolatedCaller:
            case FunctionTypeIsolation::Kind::Erased:
              diagnoseNonSendableParametersAndResult(
                  toFnType, version::Version::getFutureMajorLanguageVersion());
              break;

            case FunctionTypeIsolation::Kind::GlobalActor: {
              diagnoseNonSendableParametersAndResult(toFnType,
                                                     /*warnUntilSwiftMode*/ 6);
              break;
            }

            case FunctionTypeIsolation::Kind::NonIsolated: {
              // nonisolated synchronous <-> @concurrent
              if (fromFnType->isAsync() != toFnType->isAsync()) {
                diagnoseNonSendableParametersAndResult(
                    toFnType,
                    version::Version::getFutureMajorLanguageVersion());
              }
              break;
            }
            }
            break;
          }

          // Converting to an actor-isolated function always
          // requires crossing an isolation boundary.
          case FunctionTypeIsolation::Kind::GlobalActor: {
            switch (fromIsolation.getKind()) {
            case FunctionTypeIsolation::Kind::Parameter:
            case FunctionTypeIsolation::Kind::Erased:
              diagnoseNonSendableParametersAndResult(
                  toFnType, version::Version::getFutureMajorLanguageVersion());
              break;

            case FunctionTypeIsolation::Kind::NonIsolated: {
              // Since @concurrent as an asynchronous function it
              // would mean that without Sendable check it would
              // be possible for non-Sendable state to escape from
              // actor isolation.
              if (fromFnType->isAsync()) {
                diagnoseNonSendableParametersAndResult(
                    toFnType,
                    version::Version::getFutureMajorLanguageVersion());
                break;
              }
              // Runs on the actor.
              break;
            }

            // Runs on the actor.
            case FunctionTypeIsolation::Kind::NonIsolatedCaller:
              break;

            case FunctionTypeIsolation::Kind::GlobalActor:
              // If the isolation is the same it means that conversion
              // covers loss of `@Sendable` or some other attribute and
              // we don't need Sendable checking because there is no
              // boundary crossing here.
              if (fromIsolation.getGlobalActorType()->isEqual(
                      toIsolation.getGlobalActorType()))
                break;

              diagnoseNonSendableParametersAndResult(
                  toFnType, version::Version::getFutureMajorLanguageVersion());
              break;
            }
            break;
          }

          // Converting to @isolated(any) doesn't cross an isolation
          // boundary.
          case FunctionTypeIsolation::Kind::Erased:
            break;

          // TODO: Figure out what exactly needs to happen here.
          case FunctionTypeIsolation::Kind::Parameter:
            break;
          }
        }
      }
    }

    /// Function object that refines isolation for each actor isolation it is
    /// given, returning true if all of the provided isolations have been
    /// accounted for, or false if the caller should handle them.
    class RefineConformances {
      ActorIsolationChecker &self;

    public:
      RefineConformances(ActorIsolationChecker &self) : self(self) { }

      bool operator()(ArrayRef<ActorIsolation> isolations) const {
        bool anyRefined = false;
        bool anyUnrefined = false;
        for (const auto &isolation : isolations) {
          if (self.refineRequiredIsolation(isolation))
            anyRefined = true;
          else
            anyUnrefined = true;
        }

        return anyRefined && !anyUnrefined;
      }
    };

    bool refineRequiredIsolation(ActorIsolation refinedIsolation) {
      if (requiredIsolationLoc.isInvalid())
        return false;

      auto infersIsolationFromContext =
          [](const DeclContext *dc) -> bool {
            // Isolation for declarations is based solely on explicit 
            // annotations; only infer isolation for initializer expressions
            // and closures.
            if (dc->getAsDecl())
              return false;

            if (auto *closure = dyn_cast<AbstractClosureExpr>(dc)) {
              // We cannot infer a more specific actor isolation for a Sendable
              // closure. It is an error to cast away actor isolation from a
              // function type, but this is okay for non-Sendable closures
              // because they cannot leave the isolation domain they're created
              // in anyway.
              if (isIsolationInferenceBoundaryClosure(
                      closure, /*canInheritActorContext*/false))
                return false;

              if (closure->getActorIsolation().isActorIsolated())
                return false;
            }

            return true;
          };

      // For the call to require the given actor isolation, every DeclContext
      // in the current context stack must require the same isolation. If
      // along the way to the innermost context, we find a DeclContext that
      // has a different isolation (e.g. it's a local function that does not
      // receive isolation from its decl context), then the expression cannot
      // require a different isolation.
      for (auto *dc : contextStack) {
        if (!infersIsolationFromContext(dc)) {
          requiredIsolation.clear();
          return false;
        }

        // To refine the required isolation, the existing requirement
        // must either be 'nonisolated' or exactly the same as the
        // new refinement.
        auto isolation = requiredIsolation.find(dc);
        if (isolation == requiredIsolation.end() ||
            isolation->second == ActorIsolation::Nonisolated) {
          requiredIsolation[dc] = refinedIsolation;
        } else if (isolation->second != refinedIsolation) {
          ctx.Diags.diagnose(requiredIsolationLoc,
                             diag::conflicting_default_argument_isolation,
                             isolation->second, refinedIsolation);
          requiredIsolation.clear();
          return true;
        }
      }

      return true;
    }

    void checkDefaultArgument(DefaultArgumentExpr *expr) {
      getCurrentContextIsolation(expr);

      // Check the context isolation against the required isolation for
      // evaluating the default argument synchronously. If the default
      // argument must be evaluated asynchronously, record that in the
      // expression node.
      auto requiredIsolation = expr->getRequiredIsolation();
      auto contextIsolation = getInnermostIsolatedContext(
          getDeclContext(), getClosureActorIsolation);

      if (requiredIsolation == contextIsolation)
        return;

      switch (requiredIsolation) {
      // Nonisolated is okay from any caller isolation because
      // default arguments cannot have any async calls.
      case ActorIsolation::Unspecified:
      case ActorIsolation::Nonisolated:
      case ActorIsolation::NonisolatedUnsafe:
        return;

      // Similarly to Nonisolated, caller inheriting isolation because we will
      // inherit from the context.
      case ActorIsolation::CallerIsolationInheriting:
        return;

      case ActorIsolation::Erased:
      case ActorIsolation::GlobalActor:
      case ActorIsolation::ActorInstance:
        break;
      }

      expr->setImplicitlyAsync();
    }

    /// Check closure captures for Sendable violations.
    void checkLocalCaptures(AnyFunctionRef localFunc) {
      auto *dc = getDeclContext();

      auto *closure = localFunc.getAbstractClosureExpr();
      auto *explicitClosure = dyn_cast_or_null<ClosureExpr>(closure);

      bool preconcurrency = false;
      if (closure) {
        preconcurrency =
            getActorIsolationOfContext(closure, getClosureActorIsolation)
                .preconcurrency();
      }

      for (const auto &capture : localFunc.getCaptureInfo().getCaptures()) {
        if (!capture.isLocalCapture())
          continue;
        if (capture.isDynamicSelfMetadata())
          continue;
        if (capture.isOpaqueValue())
          continue;

        // Diagnose a `self` capture inside an escaping `sending`
        // `@Sendable` closure in a deinit, which almost certainly
        // means `self` would escape deinit at runtime.
        if (explicitClosure && isa<DestructorDecl>(dc) &&
            !explicitClosure->getType()->isNoEscape() &&
            (explicitClosure->isPassedToSendingParameter() ||
             explicitClosure->isSendable())) {
          auto var = dyn_cast_or_null<VarDecl>(capture.getDecl());
          if (var && var->isSelfParameter()) {
            ctx.Diags.diagnose(explicitClosure->getLoc(),
                               diag::self_capture_deinit_task)
                .limitBehaviorWithPreconcurrency(DiagnosticBehavior::Warning,
                                                 preconcurrency);
          }
        }

        // If the closure won't execute concurrently with the context in
        // which the declaration occurred, it's okay.
        auto decl = capture.getDecl();

        // 'nonisolated' local variables are always okay to capture in
        // 'Sendable' closures because they can be accessed from anywhere.
        // Note that only 'nonisolated(unsafe)' can be applied to local
        // variables.
        if (isa<VarDecl>(decl) &&
            getActorIsolation(decl).isNonisolated())
          continue;

        auto *context = localFunc.getAsDeclContext();
        auto fnType = localFunc.getType()->getAs<AnyFunctionType>();
        if (!mayExecuteConcurrentlyWith(context, decl->getDeclContext()))
          continue;

        Type type = getDeclContext()
            ->mapTypeIntoContext(decl->getInterfaceType())
            ->getReferenceStorageReferent();

        // Pack expansions are okay to capture as long as the pattern
        // type is Sendable.
        if (auto *expansion = type->getAs<PackExpansionType>()) {
          type = expansion->getPatternType();
        }

        if (type->hasError())
          continue;

        SendableCheckContext sendableContext(getDeclContext(),
                                             preconcurrency);

        if (closure && closure->isImplicit()) {
          auto *patternBindingDecl = getTopPatternBindingDecl();
          if (patternBindingDecl && patternBindingDecl->isAsyncLet()) {
            // Defer diagnosing checking of non-Sendable types that are passed
            // into async let to SIL level region-based isolation.
            return;
          }

          // Fallback to a generic implicit capture missing sendable
          // conformance diagnostic.
          diagnoseNonSendableTypes(type, sendableContext,
                                   /*inDerivedConformance*/Type(),
                                   capture.getLoc(),
                                   diag::implicit_non_sendable_capture,
                                   decl->getName());
        } else if (fnType->isSendable()) {
          diagnoseNonSendableTypes(type, sendableContext,
                                   /*inDerivedConformance*/Type(),
                                   capture.getLoc(),
                                   diag::non_sendable_capture,
                                   decl->getName(),
                                   /*closure=*/closure != nullptr);
        } else {
          diagnoseNonSendableTypes(type, sendableContext,
                                   /*inDerivedConformance*/Type(),
                                   capture.getLoc(),
                                   diag::non_sendable_isolated_capture,
                                   decl->getName(),
                                   /*closure=*/closure != nullptr);
        }
      }

      if (ctx.LangOpts.hasFeature(Feature::RegionBasedIsolation) &&
          mayExecuteConcurrentlyWith(
              localFunc.getAsDeclContext(), getDeclContext(),
              /*includeSending*/true)) {
        auto innermostGenericDC = localFunc.getAsDeclContext();
        while (innermostGenericDC && !innermostGenericDC->isGenericContext())
          innermostGenericDC = innermostGenericDC->getParent();

        GenericSignature genericSig = innermostGenericDC
            ? innermostGenericDC->getGenericSignatureOfContext()
            : GenericSignature();

        for (const auto &capturedType :
                 localFunc.getCaptureInfo().getCapturedTypes()) {
          unsigned genericDepth;
          Type type = capturedType.getType();
          if (auto archetype = type->getAs<ArchetypeType>()) {
            genericDepth = archetype->getInterfaceType()->getRootGenericParam()
                ->getDepth();
          } else if (type->isTypeParameter()) {
            genericDepth = type->getRootGenericParam()->getDepth();
          } else {
            continue;
          }

          // If the local function is generic and this is one of its generic
          // parameters, ignore it.
          if (genericSig.getNextDepth() > 0 &&
              genericDepth >= genericSig.getNextDepth())
            continue;

          if (type->isTypeParameter() && innermostGenericDC)
            type = innermostGenericDC->mapTypeIntoContext(type);

          // Check that the metatype is sendable.
          SendableCheckContext sendableContext(getDeclContext(), preconcurrency);
          diagnoseNonSendableTypes(MetatypeType::get(type),
                                   sendableContext,
                                   /*inDerivedConformance*/Type(),
                                   capturedType.getLoc(),
                                   diag::non_sendable_metatype_capture,
                                   /*closure=*/closure != nullptr);
        }
      }
    }

  public:
    ActorIsolationChecker(
        const DeclContext *dc,
        llvm::function_ref<Type(Expr *)> getType = __Expr_getType,
        llvm::function_ref<ActorIsolation(AbstractClosureExpr *)>
            getClosureActorIsolation = __AbstractClosureExpr_getActorIsolation,
        bool checkIsolatedCapture = true)
        : ctx(dc->getASTContext()), getType(getType),
          getClosureActorIsolation(getClosureActorIsolation),
          checkIsolatedCapture(checkIsolatedCapture) {
      contextStack.push_back(dc);
    }

    ActorIsolation computeRequiredIsolation(Expr *expr) {
      if (ctx.LangOpts.hasFeature(Feature::IsolatedDefaultValues))
        requiredIsolationLoc = expr->getLoc();

      expr->walk(*this);
      requiredIsolationLoc = SourceLoc();
      return requiredIsolation[getDeclContext()];
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
    void markNearestCallAsImplicitly(std::optional<ActorIsolation> setAsync,
                                     bool setThrows = false,
                                     bool setDistributedThunk = false) {
      assert(applyStack.size() > 0 && "not contained within an Apply?");

      const auto End = applyStack.rend();
      for (auto I = applyStack.rbegin(); I != End; ++I) {
        auto *apply = I->dyn_cast<ApplyExpr *>();
        if (!apply || isa<SelfApplyExpr>(apply)) {
          continue;
        }

        if (setAsync) {
          apply->setImplicitlyAsync(*setAsync);
        }
        if (setThrows) {
          apply->setImplicitlyThrows(true);
        } else {
          apply->setImplicitlyThrows(false);
        }
        if (setDistributedThunk) {
          apply->setShouldApplyDistributedThunk(true);
        }
        return;
      }

      llvm_unreachable("expected an ApplyExpr in applyStack!");
    }

    MacroWalking getMacroWalkingBehavior() const override {
      return MacroWalking::Expansion;
    }

    LazyInitializerWalking getLazyInitializerWalkingBehavior() override {
      // We want to walk lazy initializers as part of their implicit getters
      // since we're interested in querying capture information, and captures
      // for lazy inits are computed as part of type-checking the accessor.
      return LazyInitializerWalking::InAccessor;
    }

    /// This function is a stripped down version of checkApply that only is
    /// applied to curry thunks generated by the type checker that explicitly
    /// have isolation put upon them by the typechecker to work around a bug in
    /// 6.2. We do not perform any sort of actual inference... we only use it to
    /// mark the apply as being isolation crossing if we have an autoclosure
    /// with mismatching isolation.
    ///
    /// We take advantage that we only can have two types of isolation on such
    /// an autoclosure, global actor isolation and nonisolated(nonsending).
    ///
    /// For more information, see the comment in buildSingleCurryThunk.
    void perform62AutoclosureCurryThunkChecking(ApplyExpr *apply,
                                                AutoClosureExpr *fn) {
      // The isolation of the context that we are in.
      std::optional<ActorIsolation> contextIsolation;
      auto getContextIsolation = [&]() -> ActorIsolation {
        if (contextIsolation)
          return *contextIsolation;

        auto declContext = const_cast<DeclContext *>(getDeclContext());
        contextIsolation =
            getInnermostIsolatedContext(declContext, getClosureActorIsolation);
        return *contextIsolation;
      };

      std::optional<ActorIsolation> unsatisfiedIsolation;

      // NOTE: Normally autoclosures did not have ActorIsolation set on it since
      // we do not visit the function of the partial apply due to a bug. The
      // only reason why it is set is b/c we are explicitly setting this in the
      // type checker when we generate the single and double curry thunks.
      auto fnTypeIsolation = fn->getActorIsolation();
      if (fnTypeIsolation.isGlobalActor()) {
        Type globalActor = fnTypeIsolation.getGlobalActor();
        if (!(getContextIsolation().isGlobalActor() &&
              getContextIsolation().getGlobalActor()->isEqual(globalActor)))
          unsatisfiedIsolation = ActorIsolation::forGlobalActor(globalActor);
      }

      // If there was no unsatisfied actor isolation, we're done.
      if (!unsatisfiedIsolation)
        return;

      // Record whether the callee isolation or the context isolation
      // is preconcurrency, which is used later to downgrade errors to
      // warnings in minimal checking.
      auto calleeDecl = apply->getCalledValue(/*skipFunctionConversions=*/true);
      bool preconcurrency =
          getContextIsolation().preconcurrency() ||
          (calleeDecl && getActorIsolation(calleeDecl).preconcurrency());
      unsatisfiedIsolation =
          unsatisfiedIsolation->withPreconcurrency(preconcurrency);

      // At this point, we know a jump is made to the callee that yields
      // an isolation requirement unsatisfied by the calling context, so
      // set the unsatisfiedIsolationJump fields of the ApplyExpr appropriately
      apply->setIsolationCrossing(getContextIsolation(), *unsatisfiedIsolation);
    }

    PreWalkResult<Pattern *> walkToPatternPre(Pattern *pattern) override {
      // Walking into patterns leads to nothing good because then we
      // end up visiting the AccessorDecls of a top-level
      // PatternBindingDecl twice.
      return Action::SkipNode(pattern);
    }

    PreWalkAction walkToDeclPre(Decl *decl) override {
      // Don't walk into local types because nothing in them can
      // change the outcome of our analysis, and we don't want to
      // assume things there have been type checked yet.
      if (isa<TypeDecl>(decl)) {
        return Action::SkipChildren();
      }

      if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
        if (func->getDeclContext()->isLocalContext()) {
          checkLocalCaptures(func);
        }

        contextStack.push_back(func);
      }

      if (auto *PBD = dyn_cast<PatternBindingDecl>(decl)) {
        patternBindingStack.push_back(PBD);
      }

      return Action::Continue();
    }

    PostWalkAction walkToDeclPost(Decl *decl) override {
      if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
        assert(contextStack.back() == func);
        contextStack.pop_back();
      }

      if (auto *PBD = dyn_cast<PatternBindingDecl>(decl)) {
        assert(patternBindingStack.back() == PBD);
        patternBindingStack.pop_back();
      }

      return Action::Continue();
    }

    PreWalkResult<Expr *> walkToExprPre(Expr *expr) override {
      // Skip expressions that didn't make it to solution application
      // because the constraint system diagnosed an error.
      if (!expr->getType() || expr->getType()->hasError())
        return Action::SkipNode(expr);

      if (auto *openExistential = dyn_cast<OpenExistentialExpr>(expr)) {
        opaqueValues.push_back({
            openExistential->getOpaqueValue(),
            openExistential->getExistentialValue()});
        return Action::Continue(expr);
      }

      if (auto *closure = dyn_cast<AbstractClosureExpr>(expr)) {
        closure->setActorIsolation(determineClosureIsolation(closure));
        checkLocalCaptures(closure);
        contextStack.push_back(closure);
        return Action::Continue(expr);
      }

      if (auto inout = dyn_cast<InOutExpr>(expr)) {
        if (!applyStack.empty())
          diagnoseInOutArg(applyStack.back(), inout, false);

        if (mutableLocalVarParent.count(inout) == 0)
          recordMutableVarParent(inout, inout->getSubExpr());
      }

      if (auto assign = dyn_cast<AssignExpr>(expr)) {
        // mark vars in the destination expr as being part of the Assign.
        if (auto destExpr = assign->getDest())
          recordMutableVarParent(assign, destExpr);

        return Action::Continue(expr);
      }

      if (auto load = dyn_cast<LoadExpr>(expr))
        recordMutableVarParent(load, load->getSubExpr());

      if (auto lookup = dyn_cast<LookupExpr>(expr)) {
        applyStack.push_back(lookup);
        checkReference(lookup->getBase(), lookup->getMember(), lookup->getLoc(),
                       /*partialApply*/ std::nullopt, lookup);
        return Action::Continue(expr);
      }

      if (auto declRef = dyn_cast<DeclRefExpr>(expr)) {
        auto valueRef = declRef->getDeclRef();
        auto value = valueRef.getDecl();
        auto loc = declRef->getLoc();

        // FIXME: Should this be subsumed in reference checking?
        if (value->isLocalCapture())
          checkLocalCapture(valueRef, loc, declRef);
        else
          checkReference(nullptr, valueRef, loc, std::nullopt, declRef);
        return Action::Continue(expr);
      }

      if (auto apply = dyn_cast<ApplyExpr>(expr)) {
        // If this is a call to a partial apply thunk, decompose it to check it
        // like based on the original written syntax, e.g., "self.method".
        if (auto partialApply = decomposePartialApplyThunk(
                apply, Parent.getAsExpr())) {
          if (auto memberRef = findReference(partialApply->fn)) {
            // NOTE: partially-applied thunks are never annotated as
            // implicitly async, regardless of whether they are escaping.
            checkReference(
                partialApply->base, memberRef->first, memberRef->second,
                partialApply);

            partialApply->base->walk(*this);

            // See if we have an autoclosure as our function. If so, check if we
            // have a difference in isolation. If so, make this apply an
            // isolation crossing apply.
            //
            // NOTE: This is just a work around for 6.2 to make checking of
            // double curry thunks work correctly in the face of us not
            // performing full type checking of autoclosures that are functions
            // of the apply. We are doing this to make sure that we do not
            // increase the surface area too much.
            if (auto *fn = dyn_cast<AutoClosureExpr>(apply->getFn())) {
              perform62AutoclosureCurryThunkChecking(apply, fn);
            }

            return Action::SkipNode(expr);
          }
        }

        applyStack.push_back(apply);  // record this encounter

        if (isa<SelfApplyExpr>(apply)) {
          // Self applications are checked as part of the outer call.
          // However, we look for inout issues here.
          if (applyStack.size() >= 2) {
            auto outerCall = applyStack[applyStack.size() - 2];
            if (isAsyncCall(outerCall)) {
              // This call is a partial application within an async call.
              // If the partial application take a value inout, it is bad.
              if (InOutExpr *inoutArg = dyn_cast<InOutExpr>(
                     apply->getArgs()->getExpr(0)->getSemanticsProvidingExpr()))
                diagnoseInOutArg(outerCall, inoutArg, true);
            }
          }
        } else {
          // Check the call itself.
          (void)checkApply(apply);
        }
      }

      if (auto keyPath = dyn_cast<KeyPathExpr>(expr))
        checkKeyPathExpr(keyPath);

      // The children of #selector expressions are not evaluated, so we do not
      // need to do isolation checking there. This is convenient because such
      // expressions tend to violate restrictions on the use of instance
      // methods.
      if (isa<ObjCSelectorExpr>(expr))
        return Action::SkipNode(expr);

      // Track the capture contexts for variables.
      if (auto captureList = dyn_cast<CaptureListExpr>(expr)) {
        auto *closure = captureList->getClosureBody();
        for (const auto &entry : captureList->getCaptureList()) {
          captureContexts[entry.getVar()].push_back(closure);
        }
      }

      // The constraint solver may not have chosen legal casts.
      if (auto funcConv = dyn_cast<FunctionConversionExpr>(expr)) {
        checkFunctionConversion(funcConv,
                                funcConv->getSubExpr()->getType(),
                                funcConv->getType());
      }

      if (auto *isolationErasure = dyn_cast<ActorIsolationErasureExpr>(expr)) {
        checkFunctionConversion(isolationErasure,
                                isolationErasure->getSubExpr()->getType(),
                                isolationErasure->getType());
      }

      if (auto *defaultArg = dyn_cast<DefaultArgumentExpr>(expr)) {
        checkDefaultArgument(defaultArg);
      }

      if (auto erasureExpr = dyn_cast<ErasureExpr>(expr)) {
        checkIsolatedConformancesInContext(
            erasureExpr->getConformances(), erasureExpr->getLoc(),
            getDeclContext(), RefineConformances{*this});
      }

      if (auto *underlyingToOpaque = dyn_cast<UnderlyingToOpaqueExpr>(expr)) {
        checkIsolatedConformancesInContext(
            underlyingToOpaque->substitutions, underlyingToOpaque->getLoc(),
            getDeclContext(), RefineConformances{*this});
      }

      return Action::Continue(expr);
    }

    PostWalkResult<Expr *> walkToExprPost(Expr *expr) override {
      if (auto *openExistential = dyn_cast<OpenExistentialExpr>(expr)) {
        assert(opaqueValues.back().first == openExistential->getOpaqueValue());
        opaqueValues.pop_back();
        return Action::Continue(expr);
      }

      if (auto *closure = dyn_cast<AbstractClosureExpr>(expr)) {
        assert(contextStack.back() == closure);
        contextStack.pop_back();
      }

      if (auto *apply = dyn_cast<ApplyExpr>(expr)) {
        assert(applyStack.back().get<ApplyExpr *>() == apply);
        applyStack.pop_back();
      }

      // Clear out the mutable local variable parent map on the way out.
      if (auto *declRefExpr = dyn_cast<DeclRefExpr>(expr)) {
        mutableLocalVarParent.erase(declRefExpr);
      } else if (auto *lookupExpr = dyn_cast<LookupExpr>(expr)) {
        mutableLocalVarParent.erase(lookupExpr);

        assert(applyStack.back().dyn_cast<LookupExpr *>() == lookupExpr);
        applyStack.pop_back();
      } else if (auto *inoutExpr = dyn_cast<InOutExpr>(expr)) {
        mutableLocalVarParent.erase(inoutExpr);
      }

      // Remove the tracked capture contexts.
      if (auto captureList = dyn_cast<CaptureListExpr>(expr)) {
        for (const auto &entry : captureList->getCaptureList()) {
          auto &contexts = captureContexts[entry.getVar()];
          assert(contexts.back() == captureList->getClosureBody());
          contexts.pop_back();
          if (contexts.empty())
            captureContexts.erase(entry.getVar());
        }
      }

      if (auto isolationExpr = dyn_cast<CurrentContextIsolationExpr>(expr))
        recordCurrentContextIsolation(isolationExpr);

      return Action::Continue(expr);
    }

  private:
    /// Find the directly-referenced parameter or capture of a parameter for
    /// for the given expression.
    VarDecl *getReferencedParamOrCapture(Expr *expr) {
      return ::getReferencedParamOrCapture(
          expr, [&](OpaqueValueExpr *opaqueValue) -> Expr * {
            for (const auto &known : opaqueValues) {
              if (known.first == opaqueValue) {
                return known.second;
              }
            }
            return nullptr;
          },
          [this]() -> VarDecl * {
            auto isolation = getActorIsolationOfContext(
                               const_cast<DeclContext *>(getDeclContext()),
                               getClosureActorIsolation);
            if (isolation == ActorIsolation::ActorInstance) {
              VarDecl *var = isolation.getActorInstance();
              if (!var) {
                assert(!isolation.isActorInstanceForCapture() &&
                       "capture isolation without a variable reference?");
                auto dc = const_cast<DeclContext *>(getDeclContext());
                if (isolation.isActorInstanceForSelfParameter()) {
                  var = cast<AbstractFunctionDecl>(dc)->getImplicitSelfDecl();
                } else {
                  auto paramIdx = isolation.getActorInstanceParameterIndex();
                  var = const_cast<ParamDecl *>(getParameterAt(dc, paramIdx));
                }
              }
              return var;
            }
            return nullptr;
          });
    }

    /// Find the isolated actor instance to which the given expression refers.
    ReferencedActor getIsolatedActor(Expr *expr) {
      // Check whether this expression is an isolated parameter or a reference
      // to a capture thereof.
      auto var = getReferencedParamOrCapture(expr);
      bool isPotentiallyIsolated = isPotentiallyIsolatedActor(var);

      // helps aid in giving more informative diagnostics for autoclosure args.
      auto specificNonIsoClosureKind =
        [](DeclContext const* dc) -> ReferencedActor::Kind {
          if (auto autoClos = dyn_cast<AutoClosureExpr>(dc))
            if (autoClos->getThunkKind() == AutoClosureExpr::Kind::None)
              return ReferencedActor::NonIsolatedAutoclosure;

          return ReferencedActor::NonIsolatedContext;
      };

      // Walk the scopes between the variable reference and the variable
      // declaration to determine whether it is still isolated.
      auto dc = const_cast<DeclContext *>(getDeclContext());
      for (; dc; dc = dc->getParent()) {
        // If we hit the context in which the parameter is declared, we're done.
        if (var && dc == var->getDeclContext()) {
          if (isPotentiallyIsolated) {
            return ReferencedActor(var, isPotentiallyIsolated, ReferencedActor::Isolated);
          }
        }

        // If we've hit a module or type boundary, we're done.
        if (dc->isModuleScopeContext() || dc->isTypeContext())
          break;

        if (auto closure = dyn_cast<AbstractClosureExpr>(dc)) {
          auto isolation = getClosureActorIsolation(closure);
          switch (isolation) {
          case ActorIsolation::Unspecified:
          case ActorIsolation::Nonisolated:
          case ActorIsolation::CallerIsolationInheriting:
          case ActorIsolation::NonisolatedUnsafe:
            if (closure->isSendable()) {
              return ReferencedActor(var, isPotentiallyIsolated, ReferencedActor::SendableClosure);
            }

            return ReferencedActor(var, isPotentiallyIsolated, specificNonIsoClosureKind(dc));

          case ActorIsolation::ActorInstance:
            // If the closure is isolated to the same variable, we're all set.
            if (isPotentiallyIsolated &&
                (var == isolation.getActorInstance() ||
                 (var->isSelfParamCapture() &&
                  (isolation.getActorInstance()->isSelfParameter() ||
                   isolation.getActorInstance()->isSelfParamCapture())))) {
              return ReferencedActor(var, isPotentiallyIsolated, ReferencedActor::Isolated);
            }

            return ReferencedActor(var, isPotentiallyIsolated, specificNonIsoClosureKind(dc));

          case ActorIsolation::GlobalActor:
            return ReferencedActor::forGlobalActor(
                var, isPotentiallyIsolated, isolation.getGlobalActor());

          case ActorIsolation::Erased:
            llvm_unreachable("closure cannot have erased isolation");
          }
        }

        // Check for an 'async let' autoclosure.
        if (auto autoclosure = dyn_cast<AutoClosureExpr>(dc)) {
          switch (autoclosure->getThunkKind()) {
          case AutoClosureExpr::Kind::AsyncLet:
            return ReferencedActor(var, isPotentiallyIsolated, ReferencedActor::AsyncLet);

          case AutoClosureExpr::Kind::DoubleCurryThunk:
          case AutoClosureExpr::Kind::SingleCurryThunk:
          case AutoClosureExpr::Kind::None:
            break;
          }
        }

        // Look through defers.
        // FIXME: should this be covered automatically by the logic below?
        if (auto func = dyn_cast<FuncDecl>(dc))
          if (func->isDeferBody())
            continue;

        if (auto func = dyn_cast<AbstractFunctionDecl>(dc)) {
          // @Sendable functions are nonisolated.
          if (func->isSendable())
            return ReferencedActor(var, isPotentiallyIsolated, ReferencedActor::SendableFunction);
        }

        // Check isolation of the context itself. We do this separately
        // from the closure check because closures capture specific variables
        // while general isolation is declaration-based.
        switch (auto isolation =
                    getActorIsolationOfContext(dc, getClosureActorIsolation)) {
        case ActorIsolation::CallerIsolationInheriting:
        case ActorIsolation::Nonisolated:
        case ActorIsolation::NonisolatedUnsafe:
        case ActorIsolation::Unspecified:
          // Local functions can capture an isolated parameter.
          // FIXME: This really should be modeled by getActorIsolationOfContext.
          if (isa<FuncDecl>(dc) && cast<FuncDecl>(dc)->isLocalCapture()) {
            // FIXME: Local functions could presumably capture an isolated
            // parameter that isn't 'self'.
            if (isPotentiallyIsolated &&
                (var->isSelfParameter() || var->isSelfParamCapture()))
              continue;
          }

          return ReferencedActor(var, isPotentiallyIsolated, ReferencedActor::NonIsolatedContext);

        case ActorIsolation::Erased:
          llvm_unreachable("context cannot have erased isolation");

        case ActorIsolation::GlobalActor:
          return ReferencedActor::forGlobalActor(
              var, isPotentiallyIsolated, isolation.getGlobalActor());

        case ActorIsolation::ActorInstance:
          break;
        }
      }

      if (isPotentiallyIsolated)
        return ReferencedActor(var, isPotentiallyIsolated, ReferencedActor::NonIsolatedContext);

      return ReferencedActor(var, isPotentiallyIsolated, ReferencedActor::NonIsolatedParameter);
    }

    /// Note that the given actor member is isolated.
    /// @param context is allowed to be null if no context is appropriate.
    void noteIsolatedActorMember(ValueDecl const* decl, Expr *context) {
      ::noteIsolatedActorMember(decl, kindOfUsage(decl, context));
    }

    // Retrieve the nearest enclosing actor context.
    static NominalTypeDecl *getNearestEnclosingActorContext(
        const DeclContext *dc) {
      while (!dc->isModuleScopeContext()) {
        if (dc->isTypeContext()) {
          // FIXME: Protocol extensions need specific handling here.
          if (auto nominal = dc->getSelfNominalTypeDecl()) {
            if (nominal->isActor())
              return nominal;
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
      switch (ctx.LangOpts.StrictConcurrencyLevel) {
      case StrictConcurrency::Minimal:
      case StrictConcurrency::Targeted:
        // Never diagnose.
        return false;

      case StrictConcurrency::Complete:
        break;
      }

      // Only diagnose direct references to mutable global state.
      auto var = dyn_cast<VarDecl>(value);
      if (!var || var->isLet())
        return false;

      if (!var->getDeclContext()->isModuleScopeContext() &&
          !(var->getDeclContext()->isTypeContext() && !var->isInstanceMember()))
        return false;

      if (!var->hasStorage())
        return false;

      // If it's actor-isolated, it's already been dealt with.
      const auto isolation = getActorIsolation(value);
      if (isolation.isActorIsolated())
        return false;

      if (auto attr = value->getAttrs().getAttribute<NonisolatedAttr>();
          attr && attr->isUnsafe()) {
        return false;
      }

      // If global variable checking is enabled and the global variable is
      // from the same module as the reference, we'll already have diagnosed
      // the global variable itself.
      if (ctx.LangOpts.hasFeature(Feature::GlobalConcurrency) &&
          var->getDeclContext()->getParentModule() ==
              getDeclContext()->getParentModule())
        return false;

      const auto import = var->findImport(getDeclContext());
      const bool isPreconcurrencyImport =
          import && import->options.contains(ImportFlags::Preconcurrency);
      const auto isPreconcurrencyUnspecifiedIsolation =
          isPreconcurrencyImport && isolation.isUnspecified();

      // If the global variable is preconcurrency without an explicit
      // isolation, ignore the warning. Otherwise, limit the behavior
      // to a warning until Swift 6.
      DiagnosticBehavior limit;
      if (isPreconcurrencyUnspecifiedIsolation) {
        limit = DiagnosticBehavior::Ignore;
      } else {
        limit = DiagnosticBehavior::Warning;
      }

      ctx.Diags.diagnose(loc, diag::shared_mutable_state_access, value)
          .limitBehaviorUntilSwiftVersion(limit, 6)
          // Preconcurrency global variables are warnings even in Swift 6
          .limitBehaviorIf(isPreconcurrencyImport, limit);
      value->diagnose(diag::kind_declared_here, value->getDescriptiveKind());
      if (const auto sourceFile = getDeclContext()->getParentSourceFile();
          sourceFile && isPreconcurrencyImport) {
        sourceFile->setImportUsedPreconcurrency(*import);
      }
      return true;
    }

    /// Diagnose an inout argument passed into an async call
    ///
    /// \returns true if we diagnosed the entity, \c false otherwise.
    bool diagnoseInOutArg(
        llvm::PointerUnion<ApplyExpr *, LookupExpr *> call,
        const InOutExpr *arg,
        bool isPartialApply) {
      // check that the call is actually async
      if (!isAsyncCall(call))
        return false;

      bool result = false;
      bool downgradeToWarning = false;
      auto diagnoseIsolatedInoutState = [&](
          ConcreteDeclRef declRef, SourceLoc argLoc) {
        auto decl = declRef.getDecl();
        auto isolation = getActorIsolationForReference(decl, getDeclContext());
        if (!isolation.isActorIsolated())
          return;

        if (isPartialApply) {
          auto *apply = call.get<ApplyExpr *>();
          // The partially applied InoutArg is a property of actor. This
          // can really only happen when the property is a struct with a
          // mutating async method.
          if (auto partialApply = dyn_cast<ApplyExpr>(apply->getFn())) {
            if (auto declRef = dyn_cast<DeclRefExpr>(partialApply->getFn())) {
              ValueDecl *fnDecl = declRef->getDecl();
              ctx.Diags.diagnose(apply->getLoc(),
                                 diag::actor_isolated_mutating_func,
                                 fnDecl->getName(), decl)
                  .warnUntilSwiftVersionIf(downgradeToWarning, 6);
              result = true;
              return;
            }
          }
        }

        bool isImplicitlyAsync;
        if (auto *apply = call.dyn_cast<ApplyExpr *>()) {
          isImplicitlyAsync = apply->isImplicitlyAsync().has_value();
        } else {
          auto *lookup = call.get<LookupExpr *>();
          isImplicitlyAsync = lookup->isImplicitlyAsync().has_value();
        }

        ctx.Diags.diagnose(argLoc, diag::actor_isolated_inout_state,
                           decl, isImplicitlyAsync);
        decl->diagnose(diag::kind_declared_here, decl->getDescriptiveKind());
        result = true;
        return;
      };

      auto findIsolatedState = [&](Expr *expr) -> Expr * {
        // This code used to not walk into InOutExpr, which allowed
        // some invalid code to slip by in compilers <=5.9.
        if (isa<InOutExpr>(expr))
          downgradeToWarning = true;

        if (LookupExpr *lookup = dyn_cast<LookupExpr>(expr)) {
          if (isa<DeclRefExpr>(lookup->getBase())) {
            diagnoseIsolatedInoutState(lookup->getMember().getDecl(),
                                       expr->getLoc());
            return nullptr; // Diagnosed. Don't keep walking
          }
        }
        if (DeclRefExpr *declRef = dyn_cast<DeclRefExpr>(expr)) {
          diagnoseIsolatedInoutState(declRef->getDecl(), expr->getLoc());
          return nullptr; // Diagnosed. Don't keep walking
        }
        return expr;
      };
      arg->getSubExpr()->forEachChildExpr(findIsolatedState);
      return result;
    }

    enum class AsyncMarkingResult {
      FoundAsync, // successfully marked an implicitly-async operation
      NotFound,  // fail: no valid implicitly-async operation was found
      SyncContext, // fail: a valid implicitly-async op, but in sync context
      NotDistributed, // fail: non-distributed declaration in distributed actor
    };

    /// Determine whether we can access the given declaration that is
    /// isolated to a distributed actor from a location that is potentially not
    /// local to this process.
    ///
    /// \returns the (setThrows, isDistributedThunk) bits to implicitly
    /// mark the access/call with on success, or emits an error and returns
    /// \c std::nullopt.
    std::optional<std::pair<bool, bool>>
    checkDistributedAccess(SourceLoc declLoc, ValueDecl *decl, Expr *context) {
      // If the actor itself is, we're not doing any distributed access.
      if (getIsolatedActor(context).isKnownToBeLocal()) {
        return std::make_pair(
            /*setThrows=*/false,
            /*isDistributedThunk=*/false);
      }

      // If there is no declaration, it can't possibly be distributed.
      if (!decl) {
        ctx.Diags.diagnose(declLoc, diag::distributed_actor_isolated_method);
        return std::nullopt;
      }

      // Check that we have a distributed function or computed property.
      if (auto afd = dyn_cast<AbstractFunctionDecl>(decl)) {
        if (!afd->isDistributed()) {
          ctx.Diags.diagnose(declLoc, diag::distributed_actor_isolated_method)
              .fixItInsert(decl->getAttributeInsertionLoc(true),
                           "distributed ");

          noteIsolatedActorMember(decl, context);
          return std::nullopt;
        }

        return std::make_pair(
            /*setThrows=*/true,
            /*isDistributedThunk=*/true);
      }

      if (auto *var = dyn_cast<VarDecl>(decl)) {
        if (var->isDistributed()) {
          return std::make_pair(
              /*setThrows*/ true,
              /*isDistributedThunk=*/true);
        }

        // In compiler versions <=5.10, the compiler did not diagnose cases
        // where a non-isolated distributed actor value was passed to a VarDecl
        // with a function type type that has an isolated distributed actor
        // parameter, e.g. `(isolated DA) -> Void`. Stage in the error as a
        // warning until Swift 6.
        if (var->getTypeInContext()->getAs<FunctionType>()) {
          ctx.Diags.diagnose(declLoc,
                             diag::distributed_actor_isolated_non_self_reference,
                             decl)
            .warnUntilSwiftVersion(6);
          noteIsolatedActorMember(decl, context);
          return std::nullopt;
        }
      }

      // FIXME: Subscript?

      // This is either non-distributed variable, subscript, or something else.
      ctx.Diags.diagnose(declLoc,
                         diag::distributed_actor_isolated_non_self_reference,
                         decl);
      noteIsolatedActorMember(decl, context);
      return std::nullopt;
    }

    /// Attempts to identify and mark a valid cross-actor use of a synchronous
    /// actor-isolated member (e.g., sync function application, property access)
    AsyncMarkingResult tryMarkImplicitlyAsync(SourceLoc declLoc,
                                              ConcreteDeclRef concDeclRef,
                                              Expr* context,
                                              ActorIsolation target,
                                              bool isDistributed) {
      ValueDecl *decl = concDeclRef.getDecl();
      AsyncMarkingResult result = AsyncMarkingResult::NotFound;

      // is it an access to a property?
      if (isPropOrSubscript(decl)) {
        // Cannot reference properties or subscripts of distributed actors.
        if (isDistributed) {
          bool setThrows = false;
          bool usesDistributedThunk = false;
          if (auto access = checkDistributedAccess(declLoc, decl, context)) {
            std::tie(setThrows, usesDistributedThunk) = *access;
          } else {
            return AsyncMarkingResult::NotDistributed;
          }

          // distributed computed property access, mark it throws + async
          if (auto lookupExpr = dyn_cast_or_null<LookupExpr>(context)) {
            if (auto memberRef = dyn_cast<MemberRefExpr>(lookupExpr)) {
              memberRef->setImplicitlyThrows(true);
              memberRef->setAccessViaDistributedThunk();
            } else {
              llvm_unreachable("expected distributed prop to be a MemberRef");
            }
          } else {
            llvm_unreachable("expected distributed prop to have LookupExpr");
          }
        }

        if (auto declRef = dyn_cast_or_null<DeclRefExpr>(context)) {
          if (usageEnv(declRef) == VarRefUseEnv::Read) {
            if (!getDeclContext()->isAsyncContext())
              return AsyncMarkingResult::SyncContext;

            declRef->setImplicitlyAsync(target);
            result = AsyncMarkingResult::FoundAsync;
          }
        } else if (auto lookupExpr = dyn_cast_or_null<LookupExpr>(context)) {
          if (usageEnv(lookupExpr) == VarRefUseEnv::Read) {

            if (!getDeclContext()->isAsyncContext())
              return AsyncMarkingResult::SyncContext;

            lookupExpr->setImplicitlyAsync(target);
            result = AsyncMarkingResult::FoundAsync;
          }
        }
      }

      return result;
    }

    /// Check actor isolation for a particular application.
    bool checkApply(ApplyExpr *apply) {
      auto fnExprType = getType(apply->getFn());
      if (!fnExprType)
        return false;

      auto fnType = fnExprType->getAs<FunctionType>();
      if (!fnType)
        return false;

      // The isolation of the context we're in.
      std::optional<ActorIsolation> contextIsolation;
      auto getContextIsolation = [&]() -> ActorIsolation {
        if (contextIsolation)
          return *contextIsolation;

        auto declContext = const_cast<DeclContext *>(getDeclContext());
        contextIsolation =
            getInnermostIsolatedContext(declContext, getClosureActorIsolation);
        return *contextIsolation;
      };

      // Default the call options to allow promotion to async, if it will be
      // warranted.
      ActorReferenceResult::Options callOptions;
      if (!fnType->getExtInfo().isAsync())
        callOptions |= ActorReferenceResult::Flags::AsyncPromotion;

      // Determine from the callee whether actor isolation is unsatisfied.
      std::optional<ActorIsolation> unsatisfiedIsolation;
      bool mayExitToNonisolated = true;
      Expr *argForIsolatedParam = nullptr;
      auto calleeDecl = apply->getCalledValue(/*skipFunctionConversions=*/true);

      auto fnTypeIsolation = fnType->getIsolation();
      if (fnTypeIsolation.isGlobalActor()) {
        // If the function type is global-actor-qualified, determine whether
        // we are within that global actor already.
        Type globalActor = fnTypeIsolation.getGlobalActorType();
        if (!(getContextIsolation().isGlobalActor() &&
            getContextIsolation().getGlobalActor()->isEqual(globalActor)))
          unsatisfiedIsolation = ActorIsolation::forGlobalActor(globalActor);
        mayExitToNonisolated = false;

      } else if (fnTypeIsolation.isErased()) {
        unsatisfiedIsolation = ActorIsolation::forErased();
        mayExitToNonisolated = false;

      } else if (auto *selfApplyFn = dyn_cast<SelfApplyExpr>(
                    apply->getFn()->getValueProvidingExpr())) {
        // If we're calling a member function, check whether the function
        // itself is isolated.
        auto memberFn = selfApplyFn->getFn()->getValueProvidingExpr();
        if (auto memberRef = findReference(memberFn)) {
          auto isolatedActor = getIsolatedActor(selfApplyFn->getBase());
          auto result = ActorReferenceResult::forReference(
              memberRef->first, selfApplyFn->getLoc(), getDeclContext(),
              kindOfUsage(memberRef->first.getDecl(), selfApplyFn),
              isolatedActor, std::nullopt, std::nullopt,
              getClosureActorIsolation);
          switch (result) {
          case ActorReferenceResult::SameConcurrencyDomain:
            break;

          case ActorReferenceResult::ExitsActorToNonisolated:
            unsatisfiedIsolation =
                ActorIsolation::forNonisolated(/*unsafe=*/false);
            break;

          case ActorReferenceResult::EntersActor:
            unsatisfiedIsolation = result.isolation;
            break;
          }

          callOptions = result.options;
          mayExitToNonisolated = false;
          calleeDecl = memberRef->first.getDecl();
          argForIsolatedParam = selfApplyFn->getBase();
        }
      } else if (calleeDecl &&
                 calleeDecl->getAttrs()
                     .hasAttribute<UnsafeInheritExecutorAttr>()) {
        return false;
      }

      // Check for isolated parameters.
      for (unsigned paramIdx : range(fnType->getNumParams())) {
        // We only care about isolated parameters.
        if (!fnType->getParams()[paramIdx].isIsolated())
          continue;

        auto *args = apply->getArgs();
        if (paramIdx >= args->size())
          continue;

        auto *arg = args->getExpr(paramIdx);

        // FIXME: CurrentContextIsolationExpr does not have its actor set
        // at this point.
        if (auto isolation = getCurrentContextIsolation(arg))
          arg = isolation;

        argForIsolatedParam = arg;
        unsatisfiedIsolation = std::nullopt;

        // Assume that a callee with an isolated parameter does not
        // cross an isolation boundary. We'll set this again below if
        // the given isolated argument doesn't match the isolation of the
        // caller.
        mayExitToNonisolated = false;

        // If the argument is an isolated parameter from the enclosing context,
        // or #isolation, then the call does not cross an isolation boundary.
        if (getIsolatedActor(arg) || isa<CurrentContextIsolationExpr>(arg))
          continue;

        auto calleeIsolation = ActorIsolation::forActorInstanceParameter(
            const_cast<Expr *>(arg->findOriginalValue()), paramIdx);

        if (getContextIsolation() != calleeIsolation) {
          if (calleeIsolation.isNonisolated()) {
            mayExitToNonisolated = true;
          } else {
            unsatisfiedIsolation = calleeIsolation;
          }
        }

        if (!fnType->getExtInfo().isAsync())
          callOptions |= ActorReferenceResult::Flags::AsyncPromotion;

        break;
      }

      // If we're calling an async function that's nonisolated, and we're in
      // an isolated context, then we're exiting the actor context.
      if (mayExitToNonisolated && fnType->isAsync() &&
          getContextIsolation().isActorIsolated())
        unsatisfiedIsolation = ActorIsolation::forNonisolated(/*unsafe=*/false);

      // If there was no unsatisfied actor isolation, we're done.
      if (!unsatisfiedIsolation)
        return false;

      // Record whether the callee isolation or the context isolation
      // is preconcurrency, which is used later to downgrade errors to
      // warnings in minimal checking.
      bool preconcurrency = getContextIsolation().preconcurrency() ||
          (calleeDecl && getActorIsolation(calleeDecl).preconcurrency());
      unsatisfiedIsolation =
          unsatisfiedIsolation->withPreconcurrency(preconcurrency);

      bool onlyArgsCrossIsolation = callOptions.contains(
          ActorReferenceResult::Flags::OnlyArgsCrossIsolation);
      if (!onlyArgsCrossIsolation &&
          refineRequiredIsolation(*unsatisfiedIsolation))
        return false;

      // At this point, we know a jump is made to the callee that yields
      // an isolation requirement unsatisfied by the calling context, so
      // set the unsatisfiedIsolationJump fields of the ApplyExpr appropriately
      apply->setIsolationCrossing(getContextIsolation(), *unsatisfiedIsolation);

      bool requiresAsync =
          callOptions.contains(ActorReferenceResult::Flags::AsyncPromotion);

      // If we need to mark the call as implicitly asynchronous, make sure
      // we're in an asynchronous context.
      if (requiresAsync && !getDeclContext()->isAsyncContext()) {
        auto diagnostic = calleeDecl ?
          Diagnostic(
            /*id*/diag::actor_isolated_call_decl,
            /*args*/*unsatisfiedIsolation, calleeDecl, getContextIsolation()
          ) :
          Diagnostic(
            /*id*/diag::actor_isolated_call,
            /*args*/*unsatisfiedIsolation, getContextIsolation()
          );

        if (ctx.LangOpts.hasFeature(Feature::GroupActorErrors)) {
          IsolationError mismatch(apply->getLoc(), preconcurrency, diagnostic);
          auto key = std::make_pair(
              unsatisfiedIsolation->withPreconcurrency(false),
              getContextIsolation());
          if (applyErrors.find(key) == applyErrors.end()) {
            applyErrors.insert(std::make_pair(key, DiagnosticList()));
          }

          applyErrors[key].push_back(mismatch);
        } else {
          ctx.Diags.diagnose(
            apply->getLoc(),
            diagnostic.getID(),
            diagnostic.getArgs())
              .warnUntilSwiftVersionIf(preconcurrency, 6);

          if (calleeDecl) {
            auto calleeIsolation = getInferredActorIsolation(calleeDecl);
            calleeDecl->diagnose(diag::actor_isolated_sync_func, calleeDecl);
            if (calleeIsolation.source.isInferred()) {
              calleeDecl->diagnose(diag::actor_isolation_source,
                                   calleeIsolation.isolation,
                                   calleeIsolation.source);
            }
          }

          if (unsatisfiedIsolation->isGlobalActor()) {
            missingGlobalActorOnContext(
                                        const_cast<DeclContext *>(getDeclContext()),
                                        unsatisfiedIsolation->getGlobalActor(), DiagnosticBehavior::Note);
          }
        }

        return true;
      }

      // If the actor we're hopping to is distributed, we might also need
      // to mark the call as throwing and/or using the distributed thunk.
      // FIXME: ActorReferenceResult has this information, too.
      bool setThrows = false;
      bool usesDistributedThunk = false;
      if (unsatisfiedIsolation->isDistributedActor() &&
          !(calleeDecl && isa<ConstructorDecl>(calleeDecl))) {
        auto distributedAccess = checkDistributedAccess(
            apply->getFn()->getLoc(), calleeDecl, argForIsolatedParam);
        if (!distributedAccess)
          return true;

        std::tie(setThrows, usesDistributedThunk) = *distributedAccess;
      }

      // Mark as implicitly async/throws/distributed thunk as needed.
      if (requiresAsync || setThrows || usesDistributedThunk) {
        markNearestCallAsImplicitly(
            unsatisfiedIsolation, setThrows, usesDistributedThunk);
      }

      // Sendable checking for arguments and results are deferred to region
      // isolation.

      return false;
    }

    Expr *getCurrentContextIsolation(Expr *expr) {
      // Look through caller-side default arguments for #isolation.
      auto *defaultArg = dyn_cast<DefaultArgumentExpr>(expr);
      if (defaultArg && defaultArg->isCallerSide()) {
        expr = defaultArg->getCallerSideDefaultExpr();
      }

      if (auto *macro = dyn_cast<MacroExpansionExpr>(expr)) {
        if (auto rewritten = macro->getRewritten())
          expr = rewritten;
      }

      if (auto *isolation = dyn_cast<CurrentContextIsolationExpr>(expr)) {
        recordCurrentContextIsolation(isolation);
        return isolation->getActor();
      }

      return nullptr;
    }

    /// Check whether there are _unsafeInheritExecutor_ workarounds in the
    /// given _Concurrency module.
    static bool hasUnsafeInheritExecutorWorkarounds(ASTContext &ctx,
                                                    DeclContext *dc,
                                                    SourceLoc loc) {
      Identifier name =
          ctx.getIdentifier("_unsafeInheritExecutor_withUnsafeContinuation");
      NameLookupOptions lookupOptions = defaultUnqualifiedLookupOptions;
      LookupResult lookup = TypeChecker::lookupUnqualified(
          dc, DeclNameRef(name), loc, lookupOptions);
      return !lookup.empty();
    }

    void recordCurrentContextIsolation(
        CurrentContextIsolationExpr *isolationExpr) {
      // If an actor has already been assigned, we're done.
      if (isolationExpr->getActor())
        return;

      // #isolation does not work within an `@_unsafeInheritExecutor` function.
      if (auto func = enclosingUnsafeInheritsExecutor(getDeclContext())) {
        // This expression is always written as a macro #isolation in source,
        // so find the enclosing macro expansion expression's location.
        SourceLoc diagLoc;
        bool inDefaultArgument;
        std::tie(diagLoc, inDefaultArgument) = adjustPoundIsolationDiagLoc(
            isolationExpr, getDeclContext()->getParentModule());

        bool inConcurrencyModule = ::inConcurrencyModule(getDeclContext());
        auto diag = ctx.Diags.diagnose(diagLoc,
                                       diag::isolation_in_inherits_executor,
                                       inDefaultArgument);
        diag.limitBehaviorIf(inConcurrencyModule, DiagnosticBehavior::Warning);

        if (!inConcurrencyModule &&
            !hasUnsafeInheritExecutorWorkarounds(ctx, func, func->getLoc())) {
          diag.limitBehavior(DiagnosticBehavior::Warning);
        }

        replaceUnsafeInheritExecutorWithDefaultedIsolationParam(func, diag);
      }

      auto loc = isolationExpr->getLoc();
      auto isolation = getActorIsolationOfContext(
          const_cast<DeclContext *>(getDeclContext()),
                                    getClosureActorIsolation);
      auto *dc = const_cast<DeclContext *>(getDeclContext());

      // Note that macro expansions are never implicit. They have
      // valid source locations in their macro expansion buffer, they
      // do not cause implicit 'self' capture diagnostics, etc.

      Expr *actorExpr = nullptr;
      Type isolationType = isolationExpr->getType();
      switch (isolation) {
      case ActorIsolation::ActorInstance: {
        if (auto *instance = isolation.getActorInstanceExpr()) {
          actorExpr = instance;
          break;
        }

        const VarDecl *var = isolation.getActorInstance();
        if (!var) {
          auto dc = getDeclContext();
          if (isolation.isActorInstanceForSelfParameter()) {
            var = cast<AbstractFunctionDecl>(dc)->getImplicitSelfDecl();
          } else {
            var = getParameterAt(dc, isolation.getActorInstanceParameterIndex());
          }
        }
        actorExpr = new (ctx) DeclRefExpr(
            const_cast<VarDecl *>(var), DeclNameLoc(loc),
            /*implicit=*/false);

        // For a distributed actor, we need to retrieve the local
        // actor.
        if (isolation.isDistributedActor()) {
          actorExpr = UnresolvedDotExpr::createImplicit(
              ctx, actorExpr, ctx.Id_asLocalActor);
        }
        break;
      }
      case ActorIsolation::GlobalActor: {
        // Form a <global actor type>.shared reference.
        Type globalActorType = getDeclContext()->mapTypeIntoContext(
            isolation.getGlobalActor());
        auto typeExpr = TypeExpr::createImplicit(globalActorType, ctx);
        actorExpr = new (ctx) UnresolvedDotExpr(
            typeExpr, loc, DeclNameRef(ctx.Id_shared), DeclNameLoc(loc),
            /*implicit=*/false);
        break;
      }

      case ActorIsolation::Erased:
        llvm_unreachable("context cannot have erased isolation");

      case ActorIsolation::Unspecified:
      case ActorIsolation::Nonisolated:
      case ActorIsolation::CallerIsolationInheriting:
      case ActorIsolation::NonisolatedUnsafe:
        actorExpr = new (ctx) NilLiteralExpr(loc, /*implicit=*/false);
        break;
      }


      // Convert the actor argument to the appropriate type.
      auto result = TypeChecker::typeCheckExpression(
          actorExpr, dc,
          constraints::ContextualTypeInfo(
            isolationType, CTP_CallArgument));

      // Don't set the actor if there's a type mismatch. The isolation
      // checker will treat calls using this #isolation value for an
      // isolated argument as crossing an isolation boundary.
      if (!result)
        return;

      isolationExpr->setActor(actorExpr);
    }

    /// Find the innermost context in which this declaration was explicitly
    /// captured.
    const DeclContext *findCapturedDeclContext(ValueDecl *value) {
      assert(value->isLocalCapture());
      auto var = dyn_cast<VarDecl>(value);
      if (!var)
        return value->getDeclContext();

      auto knownContexts = captureContexts.find(var);
      if (knownContexts == captureContexts.end())
        return value->getDeclContext();

      return knownContexts->second.back();
    }

    /// Check a reference to a local capture.
    bool checkLocalCapture(
        ConcreteDeclRef valueRef, SourceLoc loc, DeclRefExpr *declRefExpr) {
      auto value = valueRef.getDecl();
      auto *dc = getDeclContext();

      // Check whether we are in a context that will not execute concurrently
      // with the context of 'self'. If not, it's safe.
      if (!mayExecuteConcurrentlyWith(dc, findCapturedDeclContext(value)))
        return false;

      bool preconcurrency = false;
      if (auto *closure = dyn_cast<ClosureExpr>(dc)) {
        preconcurrency = closure->isIsolatedByPreconcurrency();
      }

      SendableCheckContext sendableBehavior(dc, preconcurrency);
      auto limit = sendableBehavior.defaultDiagnosticBehavior();

      // Check whether this is a local variable, in which case we can
      // determine whether it was safe to access concurrently.
      if (auto var = dyn_cast<VarDecl>(value)) {
        // Ignore interpolation variables.
        if (var->getBaseName() == ctx.Id_dollarInterpolation)
          return false;

        auto parent = mutableLocalVarParent[declRefExpr];

        // If the variable is immutable, it's fine so long as it involves
        // Sendable types.
        //
        // When flow-sensitive concurrent captures are enabled, we also
        // allow reads, depending on a SIL diagnostic pass to identify the
        // remaining race conditions.
        if (!var->supportsMutation() ||
            (ctx.LangOpts.hasFeature(
                 Feature::FlowSensitiveConcurrencyCaptures) &&
             parent.dyn_cast<LoadExpr *>())) {
          return false;
        }

        if (auto param = dyn_cast<ParamDecl>(value)) {
          if (param->isInOut()) {
            ctx.Diags
                .diagnose(loc, diag::concurrent_access_of_inout_param,
                          param->getName())
                .limitBehaviorWithPreconcurrency(limit, preconcurrency);
            return true;
          }
        }

        if (auto attr = var->getAttrs().getAttribute<NonisolatedAttr>();
            attr && attr->isUnsafe()) {
          return false;
        }

        // Otherwise, we have concurrent access. Complain.
        ctx.Diags.diagnose(
            loc, diag::concurrent_access_of_local_capture,
            parent.dyn_cast<LoadExpr *>(),
            var)
          .limitBehaviorWithPreconcurrency(limit, preconcurrency);
        return true;
      }

      if (auto func = dyn_cast<FuncDecl>(value)) {
        if (func->isSendable())
          return false;

        func->diagnose(diag::local_function_executed_concurrently, func)
          .fixItInsert(func->getAttributeInsertionLoc(false), "@Sendable ")
          .limitBehaviorWithPreconcurrency(limit, preconcurrency);

        // Add the @Sendable attribute implicitly, so we don't diagnose
        // again.
        const_cast<FuncDecl *>(func)->getAttrs().add(
            new (ctx) SendableAttr(true));
        return true;
      }

      // Concurrent access to some other local.
      ctx.Diags.diagnose(loc, diag::concurrent_access_local, value)
        .limitBehaviorWithPreconcurrency(limit, preconcurrency);
      value->diagnose(
          diag::kind_declared_here, value->getDescriptiveKind());
      return true;
    }

    ///
    /// \return true iff a diagnostic was emitted
    bool checkKeyPathExpr(KeyPathExpr *keyPath) {
      bool diagnosed = false;

      // check the components of the keypath.
      for (const auto &component : keyPath->getComponents()) {
        // The decl referred to by the path component cannot be within an actor.
        if (component.hasDeclRef()) {
          auto declRef = component.getDeclRef();
          auto decl = declRef.getDecl();
          auto isolation = getActorIsolationForReference(
              decl, getDeclContext());
          switch (isolation) {
          case ActorIsolation::Nonisolated:
          case ActorIsolation::CallerIsolationInheriting:
          case ActorIsolation::NonisolatedUnsafe:
          case ActorIsolation::Unspecified:
            break;

          case ActorIsolation::Erased:
            llvm_unreachable("component cannot have erased isolation");

          case ActorIsolation::GlobalActor: {
            auto result = ActorReferenceResult::forReference(
                declRef, component.getLoc(), getDeclContext(),
                kindOfUsage(decl, keyPath));

            if (result == ActorReferenceResult::SameConcurrencyDomain)
              break;

            // An isolated key-path component requires being formed in the same
            // isolation domain. Record the required isolation here if we're
            // computing the isolation of a stored property initializer.
            if (refineRequiredIsolation(isolation))
              break;

            LLVM_FALLTHROUGH;
          }

          case ActorIsolation::ActorInstance: {
            ActorReferenceResult::Options options = std::nullopt;
            if (isAccessibleAcrossActors(decl, isolation, getDeclContext(),
                                         options)) {
              break;
            }

            bool downgrade = isolation.isGlobalActor() ||
              options.contains(
                  ActorReferenceResult::Flags::Preconcurrency);

            ctx.Diags.diagnose(
                component.getLoc(), diag::actor_isolated_keypath_component,
                isolation, decl)
              .warnUntilSwiftVersionIf(downgrade, 6);

            diagnosed = !downgrade;
            break;
          }
          }
        }

        // With `InferSendableFromCaptures` feature enabled the solver is
        // responsible for inferring `& Sendable` for sendable key paths.
        if (!ctx.LangOpts.hasFeature(Feature::InferSendableFromCaptures)) {
          // Captured values in a path component must conform to Sendable.
          // These captured values appear in Subscript, such as \Type.dict[k]
          // where k is a captured dictionary key.
          if (auto *args = component.getArgs()) {
            for (auto arg : *args) {
              auto type = getType(arg.getExpr());
              if (type && shouldDiagnoseExistingDataRaces(getDeclContext()) &&
                  diagnoseNonSendableTypes(type, getDeclContext(),
                                           /*inDerivedConformance*/Type(),
                                           component.getLoc(),
                                           diag::non_sendable_keypath_capture))
                diagnosed = true;
            }
          }
        }
      }

      return diagnosed;
    }

    /// Check a reference to the given declaration.
    ///
    /// \param base For a reference to a member, the base expression. May be
    /// nullptr for non-member referenced.
    ///
    /// \returns true if the reference is invalid, in which case a diagnostic
    /// has already been emitted.
    bool checkReference(
        Expr *base, ConcreteDeclRef declRef, SourceLoc loc,
        std::optional<PartialApplyThunkInfo> partialApply = std::nullopt,
        Expr *context = nullptr) {
      if (!declRef)
        return false;

      // Make sure isolated conformances are formed in the right context.
      checkIsolatedConformancesInContext(declRef, loc, getDeclContext(),
                                         RefineConformances{*this});

      auto *const decl = declRef.getDecl();

      // If this declaration is a callee from the enclosing application,
      // it's already been checked via the call.
      if (auto *apply = getImmediateApply()) {
        auto immediateCallee =
            apply->getCalledValue(/*skipFunctionConversions=*/true);
        if (decl == immediateCallee)
          return false;
      }

      std::optional<ReferencedActor> isolatedActor;
      if (base)
        isolatedActor.emplace(getIsolatedActor(base));
      auto result = ActorReferenceResult::forReference(
          declRef, loc, getDeclContext(), kindOfUsage(decl, context),
          isolatedActor, std::nullopt, std::nullopt, getClosureActorIsolation);
      switch (result) {
      case ActorReferenceResult::SameConcurrencyDomain:
        return diagnoseReferenceToUnsafeGlobal(decl, loc);

      case ActorReferenceResult::ExitsActorToNonisolated:
        if (diagnoseReferenceToUnsafeGlobal(decl, loc))
          return true;

        return diagnoseNonSendableTypesInReference(
                   base, declRef, getDeclContext(), loc,
                   SendableCheckReason::ExitingActor,
                   result.isolation,
                   // Function reference sendability can only cross isolation
                   // boundaries when they're passed as an argument or called,
                   // and their Sendability depends only on captures; do not
                   // check the parameter or result types here.
                   FunctionCheckOptions());

      case ActorReferenceResult::EntersActor:
        // Handle all of the checking below.
        break;
      }

      // A partial application of a global-actor-isolated member is always
      // okay, because the global actor is part of the resulting function
      // type.
      if (partialApply && result.isolation.isGlobalActor())
        return false;

      // A call to a global-actor-isolated function, or a function with an
      // isolated parameter, is diagnosed elsewhere.
      if (!partialApply &&
          (result.isolation.isGlobalActor() ||
           (result.isolation == ActorIsolation::ActorInstance &&
            !result.isolation.isActorInstanceForSelfParameter())) &&
          isa<AbstractFunctionDecl>(decl))
        return false;

      // An escaping partial application of something that is part of
      // the actor's isolated state is never permitted.
      if (partialApply && partialApply->isEscaping && !decl->isAsync()) {
        ctx.Diags.diagnose(loc, diag::actor_isolated_partial_apply, decl);
        return true;
      }

      // If we do not need any async/throws/distributed checks, just perform
      // Sendable checking and we're done.
      if (!result.options) {
        return diagnoseNonSendableTypesInReference(
                   base, declRef, getDeclContext(), loc,
                   SendableCheckReason::CrossActor);
      }

      // Some combination of implicit async/throws/distributed is required.
      bool isDistributed = result.options.contains(
          ActorReferenceResult::Flags::Distributed);

      // Determine the actor hop.
      auto implicitAsyncResult = tryMarkImplicitlyAsync(
          loc, declRef, context, result.isolation, isDistributed);
      switch (implicitAsyncResult) {
      case AsyncMarkingResult::FoundAsync:
        return diagnoseNonSendableTypesInReference(
            base, declRef, getDeclContext(), loc,
            SendableCheckReason::SynchronousAsAsync);

      case AsyncMarkingResult::NotDistributed:
        // Failed, but diagnostics have already been emitted.
        return true;

      case AsyncMarkingResult::SyncContext:
      case AsyncMarkingResult::NotFound:
        // If we found an implicitly async reference in a sync
        // context and we're computing the required isolation for
        // an expression, the calling context requires the isolation
        // of the reference.
        if (refineRequiredIsolation(result.isolation)) {
          return false;
        }

        // Complain about access outside of the isolation domain.
        auto useKind = static_cast<unsigned>(
            kindOfUsage(decl, context).value_or(VarRefUseEnv::Read));

        ReferencedActor::Kind refKind;
        Type refGlobalActor;
        if (isolatedActor) {
          refKind = isolatedActor->kind;
          refGlobalActor = isolatedActor->globalActor;
        } else {
          auto contextIsolation = getInnermostIsolatedContext(
              getDeclContext(), getClosureActorIsolation);
          switch (contextIsolation) {
          case ActorIsolation::ActorInstance:
            refKind = ReferencedActor::Isolated;
            break;

          case ActorIsolation::Erased:
            llvm_unreachable("context cannot have erased isolation");

          case ActorIsolation::GlobalActor:
            refGlobalActor = contextIsolation.getGlobalActor();
            refKind = isMainActor(refGlobalActor)
                ? ReferencedActor::MainActor
                : ReferencedActor::GlobalActor;
            break;

          case ActorIsolation::Unspecified:
          case ActorIsolation::Nonisolated:
          case ActorIsolation::CallerIsolationInheriting:
          case ActorIsolation::NonisolatedUnsafe:
            refKind = ReferencedActor::NonIsolatedContext;
            break;
          }
        }

        // Does the reference originate from a @preconcurrency context?
        bool preconcurrencyContext =
          result.options.contains(ActorReferenceResult::Flags::Preconcurrency);

        Type derivedConformanceType;
        DeclName requirementName;
        if (loc.isInvalid()) {
          auto *decl = getDeclContext()->getAsDecl();
          if (decl && decl->isImplicit()) {
            auto *parentDC = decl->getDeclContext();
            loc = parentDC->getAsDecl()->getLoc();

            if (auto *implements = decl->getAttrs().getAttribute<ImplementsAttr>()) {
              derivedConformanceType =
                  implements->getProtocol(parentDC)->getDeclaredInterfaceType();
              requirementName = implements->getMemberName();
            }
          }
        }

        if (ctx.LangOpts.hasFeature(Feature::GroupActorErrors)) {
          IsolationError mismatch = IsolationError(loc,
              preconcurrencyContext,
              Diagnostic(diag::actor_isolated_non_self_reference,
                  decl, useKind, refKind + 1, refGlobalActor,
                  result.isolation));

          auto iter = refErrors.find(std::make_pair(refKind,result.isolation));
          if (iter != refErrors.end()) {
            iter->second.push_back(mismatch);
          } else {
            DiagnosticList list;
            list.push_back(mismatch);
            auto keyPair = std::make_pair(refKind,result.isolation);
            refErrors.insert(std::make_pair(keyPair, list));
          }
        } else {
          ctx.Diags.diagnose(
              loc, diag::actor_isolated_non_self_reference,
              decl, useKind,
              refKind + 1, refGlobalActor,
              result.isolation)
          .warnUntilSwiftVersionIf(preconcurrencyContext, 6);
          maybeNoteMutatingMethodSuggestion(ctx, decl, loc, getDeclContext(), result.isolation,
                                            kindOfUsage(decl, context).value_or(VarRefUseEnv::Read));

          if (derivedConformanceType) {
            auto *decl = dyn_cast<ValueDecl>(getDeclContext()->getAsDecl());
            ctx.Diags.diagnose(loc, diag::in_derived_witness, decl,
                               requirementName, derivedConformanceType);
          }

          noteIsolatedActorMember(decl, context);
          if (result.isolation.isGlobalActor()) {
            missingGlobalActorOnContext(
                const_cast<DeclContext *>(getDeclContext()),
                result.isolation.getGlobalActor(), DiagnosticBehavior::Note);
          }
        }

        return true;
      }
    }

    // Attempt to resolve the global actor type of a closure.
    Type resolveGlobalActorType(AbstractClosureExpr *ACE) const {
      // Check whether the closure's type has a global actor already.
      if (Type closureType = getType(ACE)) {
        if (auto closureFnType = closureType->getAs<FunctionType>()) {
          if (Type globalActor = closureFnType->getGlobalActor())
            return globalActor;
        }
      }

      // Look for an explicit attribute.
      if (auto *CE = dyn_cast<ClosureExpr>(ACE)) {
        if (auto globalActor = getExplicitGlobalActor(CE))
          return globalActor;
      }
      return Type();
    }

  public:
    /// Determine the isolation of a particular closure.
    ///
    /// This function assumes that enclosing closures have already had their
    /// isolation checked.
    ActorIsolation
    determineClosureIsolation(AbstractClosureExpr *closure) const;
  };
} // end anonymous namespace

/// Compute the isolation of a closure or local function from its parent
/// isolation.
///
/// Doesn't apply preconcurrency because it's generally easier for the
/// caller to do so.
static ActorIsolation
computeClosureIsolationFromParent(DeclContext *closure,
                                  ActorIsolation parentIsolation,
                                  bool checkIsolatedCapture) {
  // We must have parent isolation determined to get here.
  switch (parentIsolation) {
  case ActorIsolation::CallerIsolationInheriting:
  case ActorIsolation::Nonisolated:
  case ActorIsolation::NonisolatedUnsafe:
  case ActorIsolation::Unspecified:
    return ActorIsolation::forNonisolated(
        parentIsolation == ActorIsolation::NonisolatedUnsafe);

  case ActorIsolation::Erased:
    llvm_unreachable("context cannot have erased isolation");

  case ActorIsolation::GlobalActor:
    // This should already be an interface type, so we don't need to remap
    // it between the contexts.
    return ActorIsolation::forGlobalActor(parentIsolation.getGlobalActor());

  case ActorIsolation::ActorInstance: {
    // In non-@Sendable local functions, we always inherit the enclosing
    // isolation, forcing a capture of it if necessary.
    if (isa<FuncDecl>(closure)) {
      // We should always have a VarDecl in this case, where we got the
      // ActorIsolation from a context; the non-VarDecl cases are only used
      // locally within isolation checking.
      auto actor = parentIsolation.getActorInstance();
      assert(actor);
      return ActorIsolation::forActorInstanceCapture(actor);
    }

    if (checkIsolatedCapture) {
      auto closureAsFn = AnyFunctionRef::fromFunctionDeclContext(closure);
      if (auto param = closureAsFn.getCaptureInfo().getIsolatedParamCapture())
        return ActorIsolation::forActorInstanceCapture(param);

      auto *explicitClosure = dyn_cast<ClosureExpr>(closure);
      // @_inheritActorContext(always) forces the isolation capture.
      if (explicitClosure && explicitClosure->alwaysInheritsActorContext()) {
        if (parentIsolation.isActorInstanceIsolated()) {
          if (auto *param = parentIsolation.getActorInstance())
            return ActorIsolation::forActorInstanceCapture(param);
        }
        return parentIsolation;
      }
    } else {
      // If we don't have capture information during code completion, assume
      // that the closure captures the `isolated` parameter from the parent
      // context.
      return parentIsolation;
    }

    return ActorIsolation::forNonisolated(/*unsafe=*/false);
  }
  }
}

ActorIsolation ActorIsolationChecker::determineClosureIsolation(
    AbstractClosureExpr *closure) const {
  bool preconcurrency = false;

  ActorIsolation isolation = [&] {
    if (auto explicitClosure = dyn_cast<ClosureExpr>(closure))
      preconcurrency = explicitClosure->isIsolatedByPreconcurrency();

    // If the closure specifies a global actor, use it.
    if (Type globalActor = resolveGlobalActorType(closure)) {
      return ActorIsolation::forGlobalActor(globalActor);
    }

    if (auto *explicitClosure = dyn_cast<ClosureExpr>(closure)) {
      if (auto *attr =
              explicitClosure->getAttrs().getAttribute<NonisolatedAttr>();
          attr && ctx.LangOpts.hasFeature(Feature::ClosureIsolation)) {
        return ActorIsolation::forNonisolated(attr->isUnsafe());
      }

      if (explicitClosure->getAttrs().hasAttribute<ConcurrentAttr>()) {
        return ActorIsolation::forNonisolated(/*unsafe=*/false);
      }
    }

    // `nonisolated(nonsending)` inferred from the context makes
    // the closure caller isolated.
    if (auto closureTy = getType(closure)) {
      if (auto *closureFnTy = closureTy->getAs<FunctionType>()) {
        if (closureFnTy->getIsolation().isNonIsolatedCaller())
          return ActorIsolation::forCallerIsolationInheriting();
      }
    }

    // If a closure has an isolated parameter, it is isolated to that
    // parameter.
    for (auto param : *closure->getParameters()) {
      if (param->isIsolated())
        return ActorIsolation::forActorInstanceCapture(param);
    }

    // If we have a closure that acts as an isolation inference boundary, then
    // we return that it is non-isolated.
    //
    // NOTE: Since we already checked for global actor isolated things, we
    // know that all Sendable closures must be nonisolated. That is why it is
    // safe to rely on this path to handle Sendable closures.
    if (isIsolationInferenceBoundaryClosure(closure,
                                            /*canInheritActorContext=*/true))
      return ActorIsolation::forNonisolated(/*unsafe=*/false);

    // A non-Sendable closure gets its isolation from its context.
    auto parentIsolation = getActorIsolationOfContext(
        closure->getParent(), getClosureActorIsolation);
    preconcurrency |= parentIsolation.preconcurrency();

    return computeClosureIsolationFromParent(closure, parentIsolation,
                                             checkIsolatedCapture);
  }();

  // Apply computed preconcurrency.
  isolation = isolation.withPreconcurrency(preconcurrency);

  if (ctx.LangOpts.getFeatureState(Feature::NonisolatedNonsendingByDefault)
          .isEnabledForMigration()) {
    warnAboutNewNonisolatedAsyncExecutionBehavior(ctx, closure, isolation);
  }

  return isolation;
}

bool ActorIsolationChecker::mayExecuteConcurrentlyWith(
    const DeclContext *useContext, const DeclContext *defContext,
    bool includeSending) {
  // Fast path for when the use and definition contexts are the same.
  if (useContext == defContext)
    return false;

  auto useIsolation = getActorIsolationOfContext(
      const_cast<DeclContext *>(useContext), getClosureActorIsolation);
  if (useIsolation.isActorIsolated()) {
    auto defIsolation = getActorIsolationOfContext(
        const_cast<DeclContext *>(defContext), getClosureActorIsolation);
    // If both contexts are isolated to the same actor, then they will not
    // execute concurrently.
    if (useIsolation == defIsolation)
      return false;

    bool regionIsolationEnabled =
        ctx.LangOpts.hasFeature(Feature::RegionBasedIsolation);
    
    // Globally-isolated closures may never be executed concurrently.
    if (ctx.LangOpts.hasFeature(Feature::GlobalActorIsolatedTypesUsability) &&
        regionIsolationEnabled && useIsolation.isGlobalActor())
      return false;
  }

  // Walk the context chain from the use to the definition.
  while (useContext != defContext) {
    // If we find a concurrent closure... it can be run concurrently.
    if (auto closure = dyn_cast<AbstractClosureExpr>(useContext)) {
      if (closure->isSendable())
        return true;

      if (auto *explicitClosure = dyn_cast<ClosureExpr>(closure)) {
        if (includeSending && explicitClosure->isPassedToSendingParameter())
          return true;
      }
    }

    if (auto func = dyn_cast<FuncDecl>(useContext)) {
      if (func->isLocalCapture()) {
        // If the function is @Sendable... it can be run concurrently.
        if (func->isSendable())
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
  if (auto *body = decl->getBody())
    body->walk(checker);
}

void swift::checkFunctionActorIsolation(AbstractFunctionDecl *decl) {
  // Disable this check for @LLDBDebuggerFunction functions.
  if (decl->getAttrs().hasAttribute<LLDBDebuggerFunctionAttr>())
    return;

  auto &ctx = decl->getASTContext();
  ActorIsolationChecker checker(decl);
  if (auto body = decl->getBody()) {
    body->walk(checker);
    if (ctx.LangOpts.hasFeature(Feature::GroupActorErrors)) {
      checker.diagnoseIsolationErrors();
    }
  }
  if (auto ctor = dyn_cast<ConstructorDecl>(decl)) {
    if (auto superInit = ctor->getSuperInitCall())
      superInit->walk(checker);
  }

  if (decl->getAttrs().hasAttribute<DistributedActorAttr>()) {
    if (auto func = dyn_cast<FuncDecl>(decl)) {
      checkDistributedFunction(func);
    }
  }
}

void swift::checkEnumElementActorIsolation(
    EnumElementDecl *element, Expr *expr) {
  ActorIsolationChecker checker(element);
  expr->walk(checker);
}

void swift::checkPropertyWrapperActorIsolation(
    VarDecl *wrappedVar, Expr *expr) {
  ActorIsolationChecker checker(wrappedVar->getDeclContext());
  expr->walk(checker);
}

ActorIsolation swift::determineClosureActorIsolation(
    AbstractClosureExpr *closure, llvm::function_ref<Type(Expr *)> getType,
    llvm::function_ref<ActorIsolation(AbstractClosureExpr *)>
        getClosureActorIsolation) {
  ActorIsolationChecker checker(closure->getParent(), getType,
                                getClosureActorIsolation,
                                /*checkIsolatedCapture=*/false);
  return checker.determineClosureIsolation(closure);
}

/// Determine actor isolation solely from attributes.
///
/// \returns the actor isolation determined from attributes alone (with no
/// inference rules). Returns \c None if there were no attributes on this
/// declaration.
static std::optional<ActorIsolation>
getIsolationFromAttributes(const Decl *decl, bool shouldDiagnose = true,
                           bool onlyExplicit = false) {
  // Look up attributes on the declaration that can affect its actor isolation.
  // If any of them are present, use that attribute.

  // 'isolated` attribute in the declaration context currently applies
  // only to `deinit` declarations, invalid uses are going to
  // be diagnosed as part of attribute checking.
  auto isolatedAttr = isa<DestructorDecl>(decl)
                          ? decl->getAttrs().getAttribute<IsolatedAttr>()
                          : nullptr;
  auto nonisolatedAttr = decl->getAttrs().getAttribute<NonisolatedAttr>();
  auto globalActorAttr = decl->getGlobalActorAttr();
  auto concurrentAttr = decl->getAttrs().getAttribute<ConcurrentAttr>();

  // Remove implicit attributes if we only care about explicit ones.
  if (onlyExplicit) {
    if (nonisolatedAttr && nonisolatedAttr->isImplicit())
      nonisolatedAttr = nullptr;
    if (isolatedAttr && isolatedAttr->isImplicit())
      isolatedAttr = nullptr;
    if (globalActorAttr && globalActorAttr->first->isImplicit())
      globalActorAttr = std::nullopt;
    if (concurrentAttr && concurrentAttr->isImplicit())
      concurrentAttr = nullptr;
  }

  unsigned numIsolationAttrs =
      (isolatedAttr ? 1 : 0) + (nonisolatedAttr ? 1 : 0) +
      (globalActorAttr ? 1 : 0) + (concurrentAttr ? 1 : 0);
  if (numIsolationAttrs == 0) {
    if (isa<DestructorDecl>(decl) && !decl->isImplicit()) {
      return ActorIsolation::forNonisolated(false);
    }
    return std::nullopt;
  }

  if (concurrentAttr)
    return ActorIsolation::forNonisolated(/*is unsafe*/ false);

  // If the declaration is explicitly marked 'nonisolated', report it as
  // independent.
  if (nonisolatedAttr) {
    // 'nonisolated(nonsending)' modifier is set on the decl.
    if (nonisolatedAttr->isNonSending())
      return ActorIsolation::forCallerIsolationInheriting();

    // If the nonisolated async inherits isolation from context,
    // return caller isolation inheriting.
    if (decl->getASTContext().LangOpts.hasFeature(
            Feature::NonisolatedNonsendingByDefault)) {
      if (auto *value = dyn_cast<ValueDecl>(decl)) {
        if (value->isAsync() &&
            value->getModuleContext() == decl->getASTContext().MainModule) {
          return ActorIsolation::forCallerIsolationInheriting();
        }
      }
    }

    return ActorIsolation::forNonisolated(nonisolatedAttr->isUnsafe());
  }

  // If the declaration is explicitly marked 'isolated', infer actor isolation
  // from the context. Currently applies only to DestructorDecl.
  if (isolatedAttr) {
    auto dc = decl->getDeclContext();
    auto selfTypeDecl = dc->getSelfNominalTypeDecl();
    std::optional<ActorIsolation> result;
    if (selfTypeDecl) {
      if (selfTypeDecl->isAnyActor()) {
        result = ActorIsolation::forActorInstanceSelf(
          const_cast<AbstractFunctionDecl*>(cast<AbstractFunctionDecl>(decl)));
      } else {
        // If the declaration is in an extension that has one of the isolation
        // attributes, use that.
        if (auto ext = dyn_cast<ExtensionDecl>(dc)) {
          result = getIsolationFromAttributes(ext);
        }

        if (!result) {
          result = getActorIsolation(selfTypeDecl);
        }
      }

      if (!result || !result->isActorIsolated()) {
        if (shouldDiagnose) {
          ASTContext &ctx = decl->getASTContext();
          ctx.Diags.diagnose(isolatedAttr->getLocation(),
                             diag::isolated_deinit_no_isolation,
                             selfTypeDecl->getName());
        }
        // Try to use isolation of the overridden decl as a recovery strategy.
        // This prevents additions errors about mismatched isolation.
        if (auto value = dyn_cast<ValueDecl>(decl)) {
          ValueDecl *overriddenValue = value->getOverriddenDeclOrSuperDeinit();
          if (overriddenValue) {
            // use the overridden decl's iso as the default isolation for this
            // decl.
            auto overriddenIsolation = getOverriddenIsolationFor(value);
            if (overriddenIsolation.isActorIsolated()) {
              result = overriddenIsolation;
            }
          }
        }
      }
    }
    return result;
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

    // Handle @<global attribute type>(unsafe).
    auto *attr = globalActorAttr->first;
    bool isUnsafe = attr->isArgUnsafe();
    if (attr->hasArgs()) {
      if (isUnsafe) {
        if (!decl->getDeclContext()->isInSwiftinterface()) {
          ctx.Diags.diagnose(attr->getLocation(), diag::unsafe_global_actor)
              .fixItRemove(attr->getArgs()->getSourceRange())
              .fixItInsert(attr->getLocation(), "@preconcurrency ")
              .warnUntilSwiftVersion(6);
        }
      } else {
        ctx.Diags.diagnose(
            attr->getLocation(),
            diag::global_actor_arg, globalActorType)
          .fixItRemove(attr->getArgs()->getSourceRange());
      }
    }

    return ActorIsolation::forGlobalActor(
        globalActorType->mapTypeOutOfContext())
        .withPreconcurrency(decl->preconcurrency() || isUnsafe);
  }

  llvm_unreachable("Forgot about an attribute?");
}

/// Infer isolation from witnessed protocol requirements.
static std::optional<InferredActorIsolation>
getIsolationFromWitnessedRequirements(ValueDecl *value) {
  // Associated types cannot have isolation, so there's no such inference for
  // type witnesses.
  if (isa<TypeDecl>(value))
    return std::nullopt;

  auto dc = value->getDeclContext();
  auto idc = dyn_cast_or_null<IterableDeclContext>(dc->getAsDecl());
  if (!idc)
    return std::nullopt;

  if (dc->getSelfProtocolDecl())
    return std::nullopt;

  // Prevent isolation inference from requirements if the conforming type
  // has an explicit `nonisolated` attribute.
  if (auto *NTD = dc->getSelfNominalTypeDecl()) {
    if (NTD->getAttrs().hasAttribute<NonisolatedAttr>())
      return std::nullopt;
  }

  // Walk through each of the conformances in this context, collecting any
  // requirements that have actor isolation.
  auto conformances = idc->getLocalConformances( // note this
      ConformanceLookupKind::NonStructural);
  using IsolatedRequirement =
      std::tuple<ProtocolConformance *, ActorIsolation, ValueDecl *>;
  SmallVector<IsolatedRequirement, 2> isolatedRequirements;
  for (auto conformance : conformances) {
    auto *implied =
        conformance->getSourceKind() == ConformanceEntryKind::Implied
            ? conformance->getImplyingConformance()
            : nullptr;

    auto protocol = conformance->getProtocol();
    for (auto found : protocol->lookupDirect(value->getName())) {
      if (!isa<ProtocolDecl>(found->getDeclContext()))
        continue;

      auto requirement = dyn_cast<ValueDecl>(found);
      if (!requirement || isa<TypeDecl>(requirement))
        continue;

      // The conformance implied by an explicitly stated nonisolated protocol
      // makes all of the requirements nonisolated.
      if (implied &&
          implied->getSourceKind() == ConformanceEntryKind::Explicit) {
        auto protocol = implied->getProtocol();
        if (protocol->getAttrs().hasAttribute<NonisolatedAttr>()) {
          isolatedRequirements.push_back(IsolatedRequirement{
              implied, ActorIsolation::forNonisolated(/*unsafe=*/false),
              requirement});
          continue;
        }
      }

      auto requirementIsolation = getActorIsolation(requirement);
      switch (requirementIsolation) {
      case ActorIsolation::ActorInstance:
      case ActorIsolation::Unspecified:
        continue;

      case ActorIsolation::Erased:
        llvm_unreachable("requirement cannot have erased isolation");

      case ActorIsolation::CallerIsolationInheriting: {
        if (value->isAsync())
          break;

        // It's possible to witness requirement with an non-async
        // declaration, in such cases `nonisolated(nonsending)` does
        // not apply.
        continue;
      }

      case ActorIsolation::GlobalActor:
      case ActorIsolation::Nonisolated:
      case ActorIsolation::NonisolatedUnsafe:
        break;
      }

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

      case ActorIsolation::CallerIsolationInheriting:
      case ActorIsolation::Nonisolated:
      case ActorIsolation::NonisolatedUnsafe:
        // We only need one nonisolated.
        if (sawActorIndependent)
          return true;

        sawActorIndependent = true;
        return false;

      case ActorIsolation::Erased:
        llvm_unreachable("requirements cannot have erased isolation");

      case ActorIsolation::Unspecified:
        return true;

      case ActorIsolation::GlobalActor: {
        // Substitute into the global actor type.
        auto conformance = std::get<0>(isolated);
        auto requirementSubs = SubstitutionMap::getProtocolSubstitutions(
            ProtocolConformanceRef(conformance));
        Type globalActor = isolation.getGlobalActor().subst(requirementSubs);
        if (!globalActorTypes.insert(globalActor->getCanonicalType()).second)
          return true;

        // Update the global actor type, now that we've done this substitution.
        std::get<1>(isolated) = ActorIsolation::forGlobalActor(globalActor)
            .withPreconcurrency(isolation.preconcurrency());
        return false;
      }
      }
      }),
      isolatedRequirements.end());

  if (isolatedRequirements.size() != 1)
    return std::nullopt;

  auto requirement = isolatedRequirements.front();
  auto isolation = std::get<1>(requirement);
  auto *protocol = std::get<0>(requirement)->getProtocol();
  return InferredActorIsolation{
    isolation,
    IsolationSource(protocol, IsolationSource::Conformance)
  };
}

/// Compute the isolation of a nominal type from the conformances that
/// are directly specified on the type.
static std::optional<InferredActorIsolation>
getIsolationFromConformances(NominalTypeDecl *nominal) {
  auto &ctx = nominal->getASTContext();

  if (isa<ProtocolDecl>(nominal))
    return std::nullopt;

  std::optional<InferredActorIsolation> foundIsolation;
  for (auto conformance :
       nominal->getLocalConformances(ConformanceLookupKind::NonStructural)) {

    // Don't include inherited conformances. If a conformance is inherited
    // from a superclass, the isolation of the subclass should be inferred
    // from the superclass, which is done directly in ActorIsolationRequest.
    // If the superclass has opted out of global actor inference, such as
    // by conforming to the protocol in an extension, then the subclass should
    // not infer isolation from the protocol.
    //
    // Gate this change behind an upcoming feature flag; isolation inference
    // changes can break source in language modes < 6.
    if (conformance->getKind() == ProtocolConformanceKind::Inherited &&
        ctx.LangOpts.hasFeature(Feature::GlobalActorIsolatedTypesUsability)) {
      continue;
    }

    auto *proto = conformance->getProtocol();
    auto inferredIsolation = getInferredActorIsolation(proto);
    auto protoIsolation = inferredIsolation.isolation;
    switch (protoIsolation) {
    case ActorIsolation::ActorInstance:
    case ActorIsolation::Unspecified:
    case ActorIsolation::CallerIsolationInheriting:
    case ActorIsolation::NonisolatedUnsafe:
      break;
    case ActorIsolation::Nonisolated:
      if (inferredIsolation.source.kind == IsolationSource::Kind::Explicit) {
        if (!foundIsolation) {
          // We found an explicitly 'nonisolated' protocol.
          foundIsolation = {
              protoIsolation,
              IsolationSource(proto, IsolationSource::Conformance)};
        }
        continue;
      } else {
        break;
      }

    case ActorIsolation::Erased:
      llvm_unreachable("protocol cannot have erased isolation");

    case ActorIsolation::GlobalActor:
      // If we encountered an explicit globally isolated conformance, allow it
      // to override the _nonisolated_ isolation.
      if (!foundIsolation ||
          (foundIsolation->isolation.isNonisolated() &&
           conformance->getSourceKind() == ConformanceEntryKind::Explicit)) {
        foundIsolation = {protoIsolation,
                          IsolationSource(proto, IsolationSource::Conformance)};
        continue;
      }

      if (foundIsolation->isolation != protoIsolation)
        return std::nullopt;

      break;
    }
  }

  return foundIsolation;
}

/// Compute the isolation of a protocol
static std::optional<InferredActorIsolation>
getIsolationFromInheritedProtocols(ProtocolDecl *protocol) {
  std::optional<InferredActorIsolation> foundIsolation;
  bool conflict = false;

  auto inferIsolation = [&](ValueDecl *decl) {
    switch (auto protoIsolation = getActorIsolation(decl)) {
    case ActorIsolation::ActorInstance:
    case ActorIsolation::Unspecified:
    case ActorIsolation::Nonisolated:
    case ActorIsolation::CallerIsolationInheriting:
    case ActorIsolation::NonisolatedUnsafe:
      return;

    case ActorIsolation::Erased:
      llvm_unreachable("protocol cannot have erased isolation");

    case ActorIsolation::GlobalActor:
      if (!foundIsolation) {
        foundIsolation = {
          protoIsolation,
          IsolationSource(decl, IsolationSource::Conformance)
        };
        return;
      }

      if (foundIsolation->isolation != protoIsolation)
        conflict = true;

      return;
    }
  };

  for (auto inherited : protocol->getInheritedProtocols()) {
    inferIsolation(inherited);
  }

  if (auto *superclass = protocol->getSuperclassDecl()) {
    inferIsolation(superclass);
  }

  if (conflict)
    return std::nullopt;

  return foundIsolation;
}

/// Compute the isolation of a nominal type from the property wrappers on
/// any stored properties.
static std::optional<ActorIsolation>
getIsolationFromWrappers(NominalTypeDecl *nominal) {
  if (!isa<StructDecl>(nominal) && !isa<ClassDecl>(nominal))
    return std::nullopt;

  if (!nominal->getParentSourceFile())
    return std::nullopt;

  ASTContext &ctx = nominal->getASTContext();
  if (ctx.LangOpts.hasFeature(Feature::DisableOutwardActorInference)) {
    // In Swift 6, we no longer infer isolation of a nominal type
    // based on the property wrappers used in its stored properties
    return std::nullopt;
  }

  std::optional<ActorIsolation> foundIsolation;
  for (auto member : nominal->getMembers()) {
    auto var = dyn_cast<VarDecl>(member);
    if (!var || !var->isInstanceMember())
      continue;

    auto info = var->getAttachedPropertyWrapperTypeInfo(0);
    if (!info)
      continue;

    auto isolation = getActorIsolation(info.valueVar);

    // Inconsistent wrappedValue/projectedValue isolation disables inference.
    if (info.projectedValueVar &&
        getActorIsolation(info.projectedValueVar) != isolation)
      continue;

    switch (isolation) {
    case ActorIsolation::ActorInstance:
    case ActorIsolation::Unspecified:
    case ActorIsolation::Nonisolated:
    case ActorIsolation::CallerIsolationInheriting:
    case ActorIsolation::NonisolatedUnsafe:
      break;

    case ActorIsolation::Erased:
      llvm_unreachable("variable cannot have erased isolation");

    case ActorIsolation::GlobalActor:
      if (!foundIsolation) {
        foundIsolation = isolation;
        continue;
      }

      if (*foundIsolation != isolation)
        return std::nullopt;

      break;
    }
  }

  return foundIsolation;
}

namespace {

/// Describes how actor isolation is propagated to a member, if at all.
enum class MemberIsolationPropagation {
  GlobalActor,
  AnyIsolation
};

}
/// Determine how the given member can receive its isolation from its type
/// context.
static std::optional<MemberIsolationPropagation>
getMemberIsolationPropagation(const ValueDecl *value) {
  if (!value->getDeclContext()->isTypeContext())
    return std::nullopt;

  switch (value->getKind()) {
  case DeclKind::Import:
  case DeclKind::Extension:
  case DeclKind::TopLevelCode:
  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
  case DeclKind::PrecedenceGroup:
  case DeclKind::Missing:
  case DeclKind::MissingMember:
  case DeclKind::Class:
  case DeclKind::Enum:
  case DeclKind::Protocol:
  case DeclKind::Struct:
  case DeclKind::TypeAlias:
  case DeclKind::GenericTypeParam:
  case DeclKind::AssociatedType:
  case DeclKind::OpaqueType:
  case DeclKind::Param:
  case DeclKind::Module:
  case DeclKind::EnumCase:
  case DeclKind::EnumElement:
  case DeclKind::Macro:
  case DeclKind::MacroExpansion:
  case DeclKind::Using:
    return std::nullopt;

  case DeclKind::PatternBinding:
    return MemberIsolationPropagation::GlobalActor;

  case DeclKind::Constructor:
    return MemberIsolationPropagation::AnyIsolation;

  case DeclKind::Destructor:
    if (value->getAttrs().getAttribute<IsolatedAttr>()) {
      return MemberIsolationPropagation::AnyIsolation;
    } else {
      return std::nullopt;
    }

  case DeclKind::Func:
  case DeclKind::Accessor:
  case DeclKind::Subscript:
  case DeclKind::Var:
    return value->isInstanceMember() ? MemberIsolationPropagation::AnyIsolation
                                     : MemberIsolationPropagation::GlobalActor;

  case DeclKind::BuiltinTuple:
    llvm_unreachable("BuiltinTupleDecl should not show up here");
  }
}

/// Given a property, determine the isolation when it part of a wrapped
/// property.
static ActorIsolation getActorIsolationFromWrappedProperty(VarDecl *var) {
  // If this is a variable with a property wrapper, infer from the property
  // wrapper's wrappedValue.
  if (auto wrapperInfo = var->getAttachedPropertyWrapperTypeInfo(0)) {
    if (auto wrappedValue = wrapperInfo.valueVar) {
      if (auto isolation = getActorIsolation(wrappedValue))
        return isolation;
    }
  }

  // If this is the backing storage for a property wrapper, infer from the
  // type of the outermost property wrapper.
  if (auto originalVar = var->getOriginalWrappedProperty(
          PropertyWrapperSynthesizedPropertyKind::Backing)) {
    if (auto backingType =
            originalVar->getPropertyWrapperBackingPropertyType()) {
      if (auto backingNominal = backingType->getAnyNominal()) {
        if (!isa<ClassDecl>(backingNominal) ||
            !cast<ClassDecl>(backingNominal)->isActor()) {
          if (auto isolation = getActorIsolation(backingNominal))
            return isolation;
        }
      }
    }
  }

  // If this is the projected property for a property wrapper, infer from
  // the property wrapper's projectedValue.
  if (auto originalVar = var->getOriginalWrappedProperty(
          PropertyWrapperSynthesizedPropertyKind::Projection)) {
    if (auto wrapperInfo =
            originalVar->getAttachedPropertyWrapperTypeInfo(0)) {
      if (auto projectedValue = wrapperInfo.projectedValueVar) {
        if (auto isolation = getActorIsolation(projectedValue))
          return isolation;
      }
    }
  }

  return ActorIsolation::forUnspecified();
}

static std::optional<ActorIsolation>
getActorIsolationForMainFuncDecl(FuncDecl *fnDecl) {
  // Ensure that the base type that this function is declared in has @main
  // attribute
  NominalTypeDecl *declContext =
      dyn_cast<NominalTypeDecl>(fnDecl->getDeclContext());
  if (ExtensionDecl *exDecl =
          dyn_cast<ExtensionDecl>(fnDecl->getDeclContext())) {
    declContext = exDecl->getExtendedNominal();
  }

  // We're not even in a nominal decl type, this can't be the main function decl
  if (!declContext)
    return {};
  const bool isMainDeclContext =
      declContext->getAttrs().hasAttribute<MainTypeAttr>(
          /*allow invalid*/ true);

  ASTContext &ctx = fnDecl->getASTContext();

  const bool isMainMain = fnDecl->isMainTypeMainMethod();
  const bool isMainInternalMain =
      fnDecl->getBaseIdentifier() == ctx.getIdentifier("$main") &&
      !fnDecl->isInstanceMember() &&
      fnDecl->getResultInterfaceType()->isVoid() &&
      fnDecl->getParameters()->size() == 0;
  const bool isMainFunction =
      isMainDeclContext && (isMainMain || isMainInternalMain);
  const bool hasMainActor = !ctx.getMainActorType().isNull();

  return isMainFunction && hasMainActor
             ? ActorIsolation::forGlobalActor(
                   ctx.getMainActorType()->mapTypeOutOfContext())
             : std::optional<ActorIsolation>();
}

/// Check rules related to global actor attributes on a class declaration.
///
/// \returns true if an error occurred.
static bool checkClassGlobalActorIsolation(
    ClassDecl *classDecl, ActorIsolation isolation) {
  assert(isolation.isGlobalActor());

  // A class can only be annotated with a global actor if it has no
  // superclass, the superclass is annotated with the same global actor, or
  // the superclass is NSObject. A subclass of a global-actor-annotated class
  // must be isolated to the same global actor.
  auto superclassDecl = classDecl->getSuperclassDecl();
  if (!superclassDecl)
    return false;

  if (superclassDecl->isNSObject())
    return false;

  // Ignore actors outright. They'll be diagnosed later.
  if (classDecl->isActor() || superclassDecl->isActor())
    return false;

  // Check the superclass's isolation.
  bool downgradeToWarning = false;
  auto superIsolation = getActorIsolation(superclassDecl);
  switch (superIsolation) {
  case ActorIsolation::Unspecified:
  case ActorIsolation::Nonisolated:
  case ActorIsolation::CallerIsolationInheriting:
  case ActorIsolation::NonisolatedUnsafe: {
    return false;
  }

  case ActorIsolation::Erased:
    llvm_unreachable("class cannot have erased isolation");

  case ActorIsolation::ActorInstance:
    // This is an error that will be diagnosed later. Ignore it here.
    return false;

  case ActorIsolation::GlobalActor: {
    // If the global actors match, we're fine.
    Type superclassGlobalActor = superIsolation.getGlobalActor();
    SubstitutionMap subsMap = classDecl->getDeclaredInterfaceType()
      ->getSuperclassForDecl(superclassDecl)
      ->getContextSubstitutionMap(superclassDecl);
    Type superclassGlobalActorInSub = superclassGlobalActor.subst(subsMap);
    if (isolation.getGlobalActor()->isEqual(superclassGlobalActorInSub))
      return false;

    break;
  }
  }

  // Complain about the mismatch.
  classDecl->diagnose(diag::actor_isolation_superclass_mismatch, isolation,
                      classDecl, superIsolation, superclassDecl)
      .warnUntilSwiftVersionIf(downgradeToWarning, 6);
  return true;
}

namespace {
  /// Describes the result of checking override isolation.
  enum class OverrideIsolationResult {
    /// The override is permitted.
    Allowed,
    /// The override is permitted, but requires a Sendable check.
    Sendable,
    /// The override is not permitted.
    Disallowed,
  };
}

/// Return the isolation of the declaration overridden by this declaration,
/// in the context of the
static ActorIsolation getOverriddenIsolationFor(const ValueDecl *value) {
  auto overridden = value->getOverriddenDeclOrSuperDeinit();
  assert(overridden && "Doesn't have an overridden declaration");

  auto isolation = getActorIsolation(overridden);
  if (!isolation.requiresSubstitution())
    return isolation;

  SubstitutionMap subs;
  if (Type selfType = value->getDeclContext()->getSelfInterfaceType()) {
    subs = selfType->getMemberSubstitutionMap(overridden);
  }
  return isolation.subst(subs);
}

ConcreteDeclRef swift::getDeclRefInContext(ValueDecl *value) {
  auto declContext = value->getInnermostDeclContext();
  if (auto genericEnv = declContext->getGenericEnvironmentOfContext()) {
    return ConcreteDeclRef(
        value, genericEnv->getForwardingSubstitutionMap());
  }

  return ConcreteDeclRef(value);
}

static bool isNSObjectInit(ValueDecl *overridden) {
  auto *classDecl = dyn_cast_or_null<ClassDecl>(
      overridden->getDeclContext()->getSelfNominalTypeDecl());
  if (!classDecl || !classDecl->isNSObject()) {
    return false;
  }

  return isa<ConstructorDecl>(overridden);
}

/// Generally speaking, the isolation of the decl that overrides
/// must match the overridden decl. But there are a number of exceptions,
/// e.g., the decl that overrides can be nonisolated.
/// \param isolation the isolation of the overriding declaration.
static OverrideIsolationResult validOverrideIsolation(
    ValueDecl *value, ActorIsolation isolation,
    ValueDecl *overridden, ActorIsolation overriddenIsolation) {
  ConcreteDeclRef valueRef = getDeclRefInContext(value);
  auto declContext = value->getInnermostDeclContext();
  auto &ctx = declContext->getASTContext();

  // Normally we are checking if overriding declaration can be called by calling
  // overriden declaration. But in case of destructors, overriden declaration is
  // always callable by definition and we are checking that subclass deinit can
  // call super deinit.
  bool isDtor = isa<DestructorDecl>(value);

  auto refResult = ActorReferenceResult::forReference(
      valueRef, SourceLoc(), declContext, std::nullopt, std::nullopt,
      isDtor ? overriddenIsolation : isolation,
      isDtor ? isolation : overriddenIsolation);
  switch (refResult) {
  case ActorReferenceResult::SameConcurrencyDomain:
    return OverrideIsolationResult::Allowed;

  case ActorReferenceResult::ExitsActorToNonisolated:
    return OverrideIsolationResult::Sendable;

  case ActorReferenceResult::EntersActor:
    // It's okay to enter the actor when the overridden declaration is
    // asynchronous (because it will do the switch) or is accessible from
    // anywhere.
    if (overridden->isAsync() ||
        isAccessibleAcrossActors(overridden, refResult.isolation,
                                 declContext)) {
      return OverrideIsolationResult::Sendable;
    }

    // If the overridden declaration is from Objective-C with no actor
    // annotation, don't allow overriding isolation in complete concurrency
    // checking. Calls from outside the actor, via the nonisolated superclass
    // method that is dynamically dispatched, will crash at runtime due to
    // the dynamic isolation check in the @objc thunk.
    //
    // There's a narrow carve out for `NSObject.init()` because overriding
    // this init of `@MainActor`-isolated type is difficult-to-impossible,
    // especially if you need to call an initializer from an intermediate
    // superclass that is also `@MainActor`-isolated. This won't admit a
    // runtime data-race safety hole, because dynamic isolation checks will
    // be inserted in the @objc thunks under `DynamicActorIsolation`, and
    // direct calls will enforce `@MainActor` as usual.
    if (isNSObjectInit(overridden) ||
        (ctx.LangOpts.StrictConcurrencyLevel != StrictConcurrency::Complete &&
         overridden->hasClangNode() && !overriddenIsolation)) {
      return OverrideIsolationResult::Allowed;
    }

    return OverrideIsolationResult::Disallowed;
  }
}

/// Retrieve the index of the first isolated parameter of the given
/// declaration, if there is one.
static std::optional<unsigned> getIsolatedParamIndex(ValueDecl *value) {
  auto *params = value->getParameterList();
  if (!params)
    return std::nullopt;

  for (unsigned paramIdx : range(params->size())) {
    auto param = params->get(paramIdx);
    if (param->isIsolated())
      return paramIdx;
  }

  return std::nullopt;
}

static bool belongsToActor(ValueDecl *value) {
  if (auto nominal = value->getDeclContext()->getSelfNominalTypeDecl()) {
    return nominal->isAnyActor();
  }
  return false;
}

/// Verifies rules about `isolated` parameters for the given decl. There is more
/// checking about these in TypeChecker::checkParameterList.
///
/// This function is focused on rules that apply when it's a declaration with
/// an isolated parameter, rather than some generic parameter list in a
/// DeclContext.
///
/// This function assumes the value already contains an isolated parameter.
static void checkDeclWithIsolatedParameter(ValueDecl *value) {
  // assume there is an isolated parameter.
  assert(getIsolatedParamIndex(value));

  // Suggest removing global-actor attributes written on it, as its ignored.
  if (auto attr = value->getGlobalActorAttr()) {
    if (!attr->first->isImplicit()) {
      value
          ->diagnose(diag::isolated_parameter_combined_global_actor_attr, value)
          .fixItRemove(attr->first->getRangeWithAt())
          .warnUntilSwiftVersion(6);
    }
  }

  // Suggest removing `nonisolated` as it is also ignored
  if (auto attr = value->getAttrs().getAttribute<NonisolatedAttr>()) {
    if (!attr->isImplicit()) {
      value->diagnose(diag::isolated_parameter_combined_nonisolated, value)
          .fixItRemove(attr->getRangeWithAt())
          .warnUntilSwiftVersion(6);
    }
  }
}

static void addAttributesForActorIsolation(ValueDecl *value,
                                           ActorIsolation isolation) {
  ASTContext &ctx = value->getASTContext();
  switch (isolation) {
  case ActorIsolation::CallerIsolationInheriting:
    value->getAttrs().add(new (ctx) NonisolatedAttr(
        /*atLoc=*/{}, /*range=*/{}, NonIsolatedModifier::NonSending,
        /*implicit=*/true));
    break;
  case ActorIsolation::Nonisolated:
  case ActorIsolation::NonisolatedUnsafe: {
    value->getAttrs().add(NonisolatedAttr::createImplicit(
        ctx, isolation == ActorIsolation::NonisolatedUnsafe
                 ? NonIsolatedModifier::Unsafe
                 : NonIsolatedModifier::None));
    break;
  }
  case ActorIsolation::GlobalActor: {
    auto typeExpr = TypeExpr::createImplicit(isolation.getGlobalActor(), ctx);
    auto attr =
        CustomAttr::create(ctx, SourceLoc(), typeExpr, /*implicit=*/true);
    value->getAttrs().add(attr);

    if (isolation.preconcurrency() &&
        !value->getAttrs().hasAttribute<PreconcurrencyAttr>()) {
      auto preconcurrency = new (ctx) PreconcurrencyAttr(/*isImplicit*/ true);
      value->getAttrs().add(preconcurrency);
    }
    break;
  }
    case ActorIsolation::Erased:
      llvm_unreachable("cannot add attributes for erased isolation");
    case ActorIsolation::ActorInstance: {
      // Nothing to do. Default value for actors.
      assert(belongsToActor(value));
      break;
    }
    case ActorIsolation::Unspecified: {
      // Nothing to do. Default value for non-actors.
      assert(!belongsToActor(value));
      break;
    }
    }
}

/// Determine whether there is a SendableMetatype conformance that requires that the nominal type
/// be nonisolated (preventing @MainActor inference).
static bool sendableConformanceRequiresNonisolated(NominalTypeDecl *nominal) {
  ASTContext &ctx = nominal->getASTContext();
  if (!ctx.LangOpts.hasFeature(Feature::SendableProhibitsMainActorInference))
    return false;

  if (isa<ProtocolDecl>(nominal))
    return false;

  auto sendable = ctx.getProtocol(KnownProtocolKind::Sendable);
  auto sendableMetatype = ctx.getProtocol(KnownProtocolKind::SendableMetatype);
  if (!sendableMetatype)
    return false;

  // Check whether any of the explicit conformances is to a
  // SendableMetatype-inheriting protocol. We exclude direct conformance to
  // Sendable here, because a global-actor-isolated type is implicitly Sendable,
  // and writing Sendable explicitly
  InvertibleProtocolSet inverses;
  bool anyObject = false;
  auto inherited = getDirectlyInheritedNominalTypeDecls(
      nominal, inverses, anyObject);
  for (const auto &entry : inherited) {
    auto proto = dyn_cast<ProtocolDecl>(entry.Item);
    if (proto && proto != sendable && proto->inheritsFrom(sendableMetatype))
      return true;
  }

  // Check for member or extension macros that define conformances to
  // SendableMetatype-inheriting protocols.
  bool requiresNonisolated = false;
  auto checkMacro = [&](MacroRole role, MacroDecl *macro) {
    if (!macro || requiresNonisolated)
      return;

    SmallVector<ProtocolDecl *, 2> conformances;
    macro->getIntroducedConformances(nominal, role, conformances);
    for (auto proto : conformances) {
      if (proto == sendableMetatype || proto->inheritsFrom(sendableMetatype)) {
        requiresNonisolated = true;
        break;
      }
    }
  };

  nominal->forEachAttachedMacro(
      MacroRole::Member,
      [&](CustomAttr * attr, MacroDecl *macro) {
        checkMacro(MacroRole::Member, macro);
      });
  nominal->forEachAttachedMacro(
      MacroRole::Extension,
      [&](CustomAttr * attr, MacroDecl *macro) {
        checkMacro(MacroRole::Extension, macro);
      });
  return requiresNonisolated;
}

/// Determine the default isolation and isolation source for this declaration,
/// which may still be overridden by other inference rules.
static std::tuple<InferredActorIsolation, ValueDecl *,
                  std::optional<ActorIsolation>>
computeDefaultInferredActorIsolation(ValueDecl *value) {
  auto &ctx = value->getASTContext();

  // If we are supposed to infer main actor isolation by default for entities
  // within our module, make our default isolation main actor.
  if (value->getModuleContext() == ctx.MainModule) {
    auto globalActorHelper = [&](Type globalActor)
        -> std::optional<std::tuple<InferredActorIsolation, ValueDecl *,
                                    std::optional<ActorIsolation>>> {
      // Default global actor isolation does not apply to any declarations
      // within actors and distributed actors.
      bool inActorContext = false;
      auto *dc = value->getInnermostDeclContext();
      while (dc && !inActorContext) {
        if (auto *nominal = dc->getSelfNominalTypeDecl()) {
          if (nominal->isAnyActor())
            return {};
        }
        dc = dc->getParent();
      }

      // If this is or is a non-type member of a nominal type that conforms to a
      // SendableMetatype-inheriting protocol in its primary definition, disable
      // @MainActor inference.
      auto nominalTypeDecl = dyn_cast<NominalTypeDecl>(value);
      if (!nominalTypeDecl && !isa<TypeDecl>(value)) {
        nominalTypeDecl = value->getDeclContext()->getSelfNominalTypeDecl();
      }
      if (nominalTypeDecl &&
          sendableConformanceRequiresNonisolated(nominalTypeDecl))
        return { };

      // FIXME: deinit should be implicitly MainActor too.
      if (isa<TypeDecl>(value) || isa<ExtensionDecl>(value) ||
          isa<AbstractStorageDecl>(value) || isa<FuncDecl>(value) ||
          isa<ConstructorDecl>(value)) {
          // Preconcurrency here is used to stage the diagnostics
          // when users select `@MainActor` default isolation with
          // non-strict concurrency modes (pre Swift 6).
          auto isolation =
              ActorIsolation::forGlobalActor(globalActor)
                  .withPreconcurrency(!ctx.LangOpts.isSwiftVersionAtLeast(6));
          return {{{isolation, {}}, nullptr, {}}};
      }

      return {};
    };

    DefaultIsolation defaultIsolation = ctx.LangOpts.DefaultIsolationBehavior;
    if (auto *SF = value->getDeclContext()->getParentSourceFile()) {
      if (auto defaultIsolationInFile = SF->getDefaultIsolation())
        defaultIsolation = defaultIsolationInFile.value();
    }

    // If we are required to use main actor... just use that.
    if (defaultIsolation == DefaultIsolation::MainActor)
      if (auto result =
              globalActorHelper(ctx.getMainActorType()->mapTypeOutOfContext()))
        return *result;
  }

  // If we have an async function or storage... by default we inherit isolation.
  if (ctx.LangOpts.hasFeature(Feature::NonisolatedNonsendingByDefault)) {
    if (value->isAsync() && value->getModuleContext() == ctx.MainModule) {
      return {
          {ActorIsolation::forCallerIsolationInheriting(), {}}, nullptr, {}};
    }
  }

  if (auto func = dyn_cast<AbstractFunctionDecl>(value)) {
    // A @Sendable function is assumed to be actor-independent.
    if (func->isSendable()) {
      return {
          {ActorIsolation::forNonisolated(/*unsafe=*/false), {}}, nullptr, {}};
    }
  }

  // When no other isolation applies, an actor's non-async init is independent
  if (auto nominal = value->getDeclContext()->getSelfNominalTypeDecl())
    if (nominal->isAnyActor())
      if (auto ctor = dyn_cast<ConstructorDecl>(value))
        if (!ctor->hasAsync())
          return {{ActorIsolation::forNonisolated(/*unsafe=*/false), {}},
                  nullptr,
                  {}};

  // Look for and remember the overridden declaration's isolation.
  if (auto *overriddenValue = value->getOverriddenDeclOrSuperDeinit()) {
    // Use the overridden decl's isolation as the default isolation for this
    // decl.
    auto isolation = getOverriddenIsolationFor(value);

    // If this is an override of an async completion handler, mark
    // it `@concurrent` instead of inferring `nonisolated(nonsending)`
    // to preserve pre-SE-0461 behavior.
    if (isolation.isCallerIsolationInheriting() &&
        overriddenValue->hasClangNode()) {
      if (auto *AFD = dyn_cast<AbstractFunctionDecl>(overriddenValue)) {
        if (AFD->getForeignAsyncConvention()) {
          return {{ActorIsolation::forNonisolated(/*unsafe=*/false),
                   IsolationSource(overriddenValue, IsolationSource::Override)},
                  overriddenValue,
                  isolation};
        }
      }
    }

    return {{isolation,
             IsolationSource(overriddenValue, IsolationSource::Override)},
            overriddenValue,
            isolation}; // use the overridden decl's iso as the default
                        // isolation for this decl.
  }

  // Asynchronous variants for functions imported from ObjC are
  // `nonisolated(nonsending)` by default.
  if (value->hasClangNode()) {
    if (auto *AFD = dyn_cast<AbstractFunctionDecl>(value)) {
      if (!isa<ProtocolDecl>(AFD->getDeclContext()) &&
          AFD->getForeignAsyncConvention()) {
        return {
            {ActorIsolation::forCallerIsolationInheriting(), {}}, nullptr, {}};
      }
    }
  }

  // We did not find anything special, return unspecified.
  return {{ActorIsolation::forUnspecified(), {}}, nullptr, {}};
}

static InferredActorIsolation computeActorIsolation(Evaluator &evaluator,
                                                    ValueDecl *value) {
  // If this declaration has actor-isolated "self", it's isolated to that
  // actor.
  if (evaluateOrDefault(evaluator, HasIsolatedSelfRequest{value}, false)) {
    auto actor = value->getDeclContext()->getSelfNominalTypeDecl();
    assert(actor && "could not find the actor that 'self' is isolated to");
    return {
      ActorIsolation::forActorInstanceSelf(value),
      IsolationSource()
    };
  }

  // If this declaration has an isolated parameter, it's isolated to that
  // parameter.
  if (auto paramIdx = getIsolatedParamIndex(value)) {
    checkDeclWithIsolatedParameter(value);

    ParamDecl *param = value->getParameterList()->get(*paramIdx);
    Type paramType = param->getDeclContext()->mapTypeIntoContext(
        param->getInterfaceType());
    Type actorType;
    if (auto wrapped = paramType->getOptionalObjectType()) {
      actorType = wrapped;
    } else {
      actorType = paramType;
    }
    if (actorType->getAnyActor())
      return {
        ActorIsolation::forActorInstanceParameter(param, *paramIdx),
        IsolationSource()
      };
  }

  auto isolationFromAttr = getIsolationFromAttributes(value);
  ASTContext &ctx = value->getASTContext();
  if (isolationFromAttr && isolationFromAttr->preconcurrency() &&
      !value->getAttrs().hasAttribute<PreconcurrencyAttr>()) {
    auto preconcurrency =
        new (ctx) PreconcurrencyAttr(/*isImplicit*/true);
    value->getAttrs().add(preconcurrency);
  }

  // Check if we inferred CallerIsolationInheriting from our isolation attr, but
  // did not have an ExecutionKind::Caller attached to it.
  //
  // DISCUSSION: This occurs when we have a value decl that is explicitly marked
  // as nonisolated but since NonisolatedNonsendingByDefault is enabled, we return
  // CallerIsolationInheriting.
  if (isolationFromAttr && isolationFromAttr->getKind() ==
          ActorIsolation::CallerIsolationInheriting) {
    auto nonisolated = value->getAttrs().getAttribute<NonisolatedAttr>();
    if (!nonisolated || !nonisolated->isNonSending())
      value->getAttrs().add(new (ctx) NonisolatedAttr(
          /*atLoc*/ {}, /*range=*/{}, NonIsolatedModifier::NonSending,
          /*implicit=*/true));
  }

  if (auto *fd = dyn_cast<FuncDecl>(value)) {
    // Main.main() and Main.$main are implicitly MainActor-protected.
    // Any other isolation is an error.
    std::optional<ActorIsolation> mainIsolation =
        getActorIsolationForMainFuncDecl(fd);
    if (mainIsolation) {
      if (isolationFromAttr && isolationFromAttr->isGlobalActor()) {
        if (!areTypesEqual(isolationFromAttr->getGlobalActor(),
                           mainIsolation->getGlobalActor())) {
          fd->getASTContext().Diags.diagnose(
              fd->getLoc(), diag::main_function_must_be_mainActor);
        }
      }
      return {
        *mainIsolation,
        IsolationSource(fd, IsolationSource::MainFunction)
      };
    }
  }

  // If this declaration has one of the actor isolation attributes, report
  // that.
  if (isolationFromAttr) {
    // Classes with global actors have additional rules regarding inheritance.
    if (isolationFromAttr->isGlobalActor()) {
      if (auto classDecl = dyn_cast<ClassDecl>(value))
        checkClassGlobalActorIsolation(classDecl, *isolationFromAttr);
    }

    return {*isolationFromAttr,
            IsolationSource(/*source*/ nullptr, IsolationSource::Explicit)};
  }

  InferredActorIsolation defaultIsolation;
  ValueDecl *overriddenValue;
  std::optional<ActorIsolation> overridenIsolation;
  std::tie(defaultIsolation, overriddenValue, overridenIsolation) =
      computeDefaultInferredActorIsolation(value);

  // Function used when returning an inferred isolation.
  auto inferredIsolation = [&](ActorIsolation inferred,
                               bool onlyGlobal = false) {
    // check if the inferred isolation is valid in the context of its overridden
    // isolation.
    if (overriddenValue) {
      // if the inferred isolation is not valid, then carry-over the overridden
      // declaration's isolation as this decl's inferred isolation.
      switch (validOverrideIsolation(value, inferred, overriddenValue,
                                     *overridenIsolation)) {
      case OverrideIsolationResult::Allowed:
      case OverrideIsolationResult::Sendable:
        break;

      case OverrideIsolationResult::Disallowed:
        if (overriddenValue->hasClangNode() &&
            overridenIsolation->isUnspecified()) {
          inferred = overridenIsolation->withPreconcurrency(true);
        } else {
          inferred = *overridenIsolation;
        }
        break;
      }
    }

    // Add an implicit attribute to capture the actor isolation that was
    // inferred, so that (e.g.) it will be printed and serialized.
    switch (inferred) {
    case ActorIsolation::Nonisolated:
    case ActorIsolation::NonisolatedUnsafe:
      // Stored properties cannot be non-isolated, so don't infer it.
      if (auto var = dyn_cast<VarDecl>(value)) {
        if (!var->isStatic() && var->hasStorage())
          return ActorIsolation::forUnspecified().withPreconcurrency(
              inferred.preconcurrency());
      }

      if (onlyGlobal) {
        return ActorIsolation::forUnspecified().withPreconcurrency(
            inferred.preconcurrency());
      }

      // Add nonisolated attribute
      addAttributesForActorIsolation(value, inferred);
      break;
    case ActorIsolation::CallerIsolationInheriting:
      addAttributesForActorIsolation(value, inferred);
      break;
    case ActorIsolation::Erased:
      llvm_unreachable("cannot infer erased isolation");
    case ActorIsolation::GlobalActor: {
      // Add global actor attribute
      addAttributesForActorIsolation(value, inferred);
      break;
    }

    case ActorIsolation::ActorInstance:
    case ActorIsolation::Unspecified:
      if (onlyGlobal)
        return ActorIsolation::forUnspecified().withPreconcurrency(
            inferred.preconcurrency());

      // Nothing to do.
      break;
    }

    return inferred;
  };

  // If this is an accessor, use the actor isolation of its storage
  // declaration. All of the logic for FuncDecls below only applies to
  // non-accessor functions.
  if (auto accessor = dyn_cast<AccessorDecl>(value)) {
    return getInferredActorIsolation(accessor->getStorage());
  }

  // If this is a local function, inherit the actor isolation from its
  // context if it global or was captured.
  if (auto func = dyn_cast<FuncDecl>(value)) {
    auto *dc = func->getDeclContext();
    if (dc->isLocalContext() && !func->isSendable()) {
      llvm::PointerUnion<Decl *, AbstractClosureExpr *> inferenceSource;
      if (auto *closure = dyn_cast<AbstractClosureExpr>(dc)) {
        inferenceSource = closure;
      } else {
        inferenceSource = dc->getAsDecl();
      }

      auto enclosingIsolation = getActorIsolationOfContext(dc);
      auto isolation =
        computeClosureIsolationFromParent(func, enclosingIsolation,
                                          /*checkIsolatedCapture*/true)
          .withPreconcurrency(enclosingIsolation.preconcurrency());
      return {
        inferredIsolation(isolation),
        IsolationSource(inferenceSource, IsolationSource::LexicalContext)
      };
    }
  }

  if (auto var = dyn_cast<VarDecl>(value)) {
    auto &ctx = var->getASTContext();
    if (!ctx.LangOpts.isConcurrencyModelTaskToThread() &&
        var->isTopLevelGlobal() &&
        (ctx.LangOpts.StrictConcurrencyLevel >=
             StrictConcurrency::Complete ||
         var->getDeclContext()->isAsyncContext())) {
      if (Type mainActor = var->getASTContext().getMainActorType())
        return {
          inferredIsolation(
            ActorIsolation::forGlobalActor(mainActor))
              .withPreconcurrency(var->preconcurrency()),
          IsolationSource(/*source*/nullptr, IsolationSource::TopLevelCode)
        };
    }
    if (auto isolation = getActorIsolationFromWrappedProperty(var)) {
      return {
        inferredIsolation(isolation),
        IsolationSource(/*source*/nullptr, IsolationSource::None)
      };
    }
  }

  // If this is a dynamic replacement for another function, use the
  // actor isolation of the function it replaces.
  if (auto replacedDecl = value->getDynamicallyReplacedDecl()) {
    if (auto isolation = getActorIsolation(replacedDecl)) {
      return {
        inferredIsolation(isolation),
        IsolationSource(replacedDecl, IsolationSource::None)
      };
    }
  }

  if (shouldInferAttributeInContext(value->getDeclContext())) {
    // If the declaration witnesses a protocol requirement that is isolated,
    // use that.
    if (auto witnessedIsolation = getIsolationFromWitnessedRequirements(value)) {
      if (auto inferred = inferredIsolation(witnessedIsolation->isolation)) {
        return {
          inferred,
          witnessedIsolation->source
        };
      }
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
              return InferredActorIsolation::forUnspecified();

            SubstitutionMap subs = superclassType->getMemberSubstitutionMap(
                classDecl);
            superclassIsolation = superclassIsolation.subst(subs);
          }

          if (auto inferred = inferredIsolation(superclassIsolation)) {
            return {
              inferred,
              IsolationSource(superclassDecl, IsolationSource::Superclass)
            };
          }
        }
      }
    }

    if (auto nominal = dyn_cast<NominalTypeDecl>(value)) {
      // If the declaration is a nominal type and any of the protocols to which
      // it directly conforms is isolated to a global actor, use that.
      if (auto conformanceIsolation = getIsolationFromConformances(nominal)) {
        if (auto inferred = inferredIsolation(conformanceIsolation->isolation)) {
          return {
            inferred,
            conformanceIsolation->source
          };
        }
      }

      // For a protocol, inherit isolation from the directly-inherited
      // protocols.
      if (ctx.LangOpts.hasFeature(Feature::GlobalActorIsolatedTypesUsability)) {
        if (auto proto = dyn_cast<ProtocolDecl>(nominal)) {
          if (auto protoIsolation = getIsolationFromInheritedProtocols(proto)) {
            if (auto inferred = inferredIsolation(protoIsolation->isolation)) {
              return {
                inferred,
                protoIsolation->source
              };
            }
          }
        }
      }

      // Before Swift 6: If the declaration is a nominal type and any property
      // wrappers on its stored properties require isolation, use that.
      if (auto wrapperIsolation = getIsolationFromWrappers(nominal)) {
        if (auto inferred = inferredIsolation(*wrapperIsolation)) {
          return {
            inferred,
            IsolationSource()
          };
        }
      }
    }
  }

  // Infer isolation for a member.
  if (auto memberPropagation = getMemberIsolationPropagation(value)) {
    // If were only allowed to propagate global actors, do so.
    bool onlyGlobal =
        *memberPropagation == MemberIsolationPropagation::GlobalActor;

    // If the declaration is in an extension that has one of the isolation
    // attributes, use that.
    if (auto ext = dyn_cast<ExtensionDecl>(value->getDeclContext())) {
      if (auto isolationFromAttr = getIsolationFromAttributes(ext)) {
        return {
          inferredIsolation(*isolationFromAttr, onlyGlobal),
          IsolationSource(ext, IsolationSource::Explicit)
        };
      }
    }

    // If the declaration is in a nominal type (or extension thereof) that
    // has isolation, use that.
    if (auto selfTypeDecl = value->getDeclContext()->getSelfNominalTypeDecl()) {
      auto selfTypeIsolation = getInferredActorIsolation(selfTypeDecl);
      if (selfTypeIsolation.isolation) {
        auto isolation = selfTypeIsolation.isolation;

        if (ctx.LangOpts.hasFeature(Feature::NonisolatedNonsendingByDefault) &&
            value->isAsync() && value->getModuleContext() == ctx.MainModule &&
            isolation.isNonisolated()) {
          isolation = ActorIsolation::forCallerIsolationInheriting();
        }

        return {inferredIsolation(isolation, onlyGlobal),
                selfTypeIsolation.source};
      }
    }
  }

  // @IBAction implies @MainActor(unsafe).
  if (value->getAttrs().hasAttribute<IBActionAttr>()) {
    ASTContext &ctx = value->getASTContext();
    if (Type mainActor = ctx.getMainActorType()) {
      return {
        inferredIsolation(
          ActorIsolation::forGlobalActor(mainActor)
              .withPreconcurrency(true)),
        IsolationSource(),
      };
    }
  }

  // We did not invoke any earlier rules... so just return the default
  // isolation.
  if (defaultIsolation.isolation.getKind() ==
      ActorIsolation::CallerIsolationInheriting) {
    // If we have caller isolation inheriting, attach the attribute for it so
    // that we preserve that we chose CallerIsolationInheriting through
    // serialization. We do this since we need to support compiling where
    // nonisolated is the default and where caller isolation inheriting is the
    // default.
    addAttributesForActorIsolation(value, defaultIsolation.isolation);
  }
  return defaultIsolation;
}

InferredActorIsolation ActorIsolationRequest::evaluate(Evaluator &evaluator,
                                                       ValueDecl *value) const {
  const auto inferredIsolation = computeActorIsolation(evaluator, value);

  auto &ctx = value->getASTContext();
  if (ctx.LangOpts.getFeatureState(Feature::NonisolatedNonsendingByDefault)
          .isEnabledForMigration()) {
    warnAboutNewNonisolatedAsyncExecutionBehavior(ctx, value,
                                                  inferredIsolation.isolation);
  }

  return inferredIsolation;
}

bool HasIsolatedSelfRequest::evaluate(
    Evaluator &evaluator, ValueDecl *value) const {
  // Only ever applies to members of actors.
  auto dc = value->getDeclContext();
  auto selfTypeDecl = dc->getSelfNominalTypeDecl();
  if (!selfTypeDecl || !selfTypeDecl->isAnyActor())
    return false;

  // For accessors, consider the storage declaration.
  if (auto accessor = dyn_cast<AccessorDecl>(value)) {
    // distributed thunks are nonisolated, although the attached to storage
    // will be 'distributed var' and therefore isolated to the distributed
    // actor. Therefore, if we're a thunk, don't look at the storage for
    // deciding about isolation of this function.
    if (accessor->isDistributedThunk()) {
      return false;
    }

    value = accessor->getStorage();
  }

  // If there is an isolated parameter, then "self" is not isolated.
  if (getIsolatedParamIndex(value))
    return false;

  // Check whether this member can be isolated to an actor at all.
  auto memberIsolation = getMemberIsolationPropagation(value);
  if (!memberIsolation) {
    // Actors don't have inheritance (except inheriting from NSObject),
    // but if it were introduced, we would need to check for isolation
    // of the deinit in the super class.
    return false;
  }

  switch (*memberIsolation) {
  case MemberIsolationPropagation::GlobalActor:
    return false;

  case MemberIsolationPropagation::AnyIsolation:
    break;
  }

  // Check whether the default isolation was overridden by any attributes on
  // this declaration.
  auto attrIsolation = getIsolationFromAttributes(value);
  // ... or its extension context.
  if (!attrIsolation) {
    if (auto ext = dyn_cast<ExtensionDecl>(dc)) {
      attrIsolation = getIsolationFromAttributes(ext);
    }
  }
  if (attrIsolation) {
    return attrIsolation->getKind() == ActorIsolation::ActorInstance &&
           attrIsolation->isActorInstanceForSelfParameter();
  }

  // If this is a variable, check for a property wrapper that alters its
  // isolation.
  if (auto var = dyn_cast<VarDecl>(value)) {
    switch (auto isolation = getActorIsolationFromWrappedProperty(var)) {
    case ActorIsolation::Nonisolated:
    case ActorIsolation::CallerIsolationInheriting:
    case ActorIsolation::NonisolatedUnsafe:
    case ActorIsolation::Unspecified:
      break;

    case ActorIsolation::GlobalActor:
      return false;

    case ActorIsolation::Erased:
      llvm_unreachable("property cannot have erased isolation");

    case ActorIsolation::ActorInstance:
      if (isolation.getActor() != selfTypeDecl)
        return false;
      break;
    }
  }

  if (auto ctor = dyn_cast<ConstructorDecl>(value)) {
    // When no other isolation applies to an actor's constructor,
    // then it is isolated only if it is async.
    if (!ctor->hasAsync())
      return false;
  }

  return true;
}

ActorIsolation
DefaultInitializerIsolation::evaluate(Evaluator &evaluator,
                                      VarDecl *var) const {
  if (var->isInvalid())
    return ActorIsolation::forUnspecified();

  Initializer *dc = nullptr;
  Expr *initExpr = nullptr;
  ActorIsolation enclosingIsolation;

  if (auto *pbd = var->getParentPatternBinding()) {
    if (!var->isParentInitialized())
      return ActorIsolation::forUnspecified();

    auto i = pbd->getPatternEntryIndexForVarDecl(var);

    dc = cast<Initializer>(pbd->getInitContext(i));
    initExpr = pbd->getContextualizedInit(i);
    enclosingIsolation = getActorIsolation(var);
  } else if (auto *param = dyn_cast<ParamDecl>(var)) {
    // If this parameter corresponds to a stored property for a
    // memberwise initializer, the default argument is the default
    // initializer expression.
    if (auto *property = param->getStoredProperty()) {
      // FIXME: Force computation of property wrapper initializers.
      if (property->getOriginalWrappedProperty())
        (void)property->getPropertyWrapperInitializerInfo();

      return property->getInitializerIsolation();
    }

    if (!param->hasDefaultExpr())
      return ActorIsolation::forUnspecified();

    dc = param->getDefaultArgumentInitContext();
    initExpr = param->getTypeCheckedDefaultExpr();
    enclosingIsolation =
        getActorIsolationOfContext(param->getDeclContext());
  }

  if (!dc || !initExpr)
    return ActorIsolation::forUnspecified();

  // If the default argument has isolation, it must match the
  // isolation of the decl context.
  ActorIsolationChecker checker(dc);
  auto requiredIsolation = checker.computeRequiredIsolation(initExpr);
  if (requiredIsolation.isActorIsolated()) {
    if (enclosingIsolation != requiredIsolation) {
      bool preconcurrency =
          !isa<ParamDecl>(var) || requiredIsolation.preconcurrency();
      var->diagnose(
          diag::isolated_default_argument_context,
          requiredIsolation, enclosingIsolation)
        .warnUntilSwiftVersionIf(preconcurrency, 6);
      return ActorIsolation::forUnspecified();
    }
  }

  return requiredIsolation;
}

std::optional<DefaultIsolation>
DefaultIsolationInSourceFileRequest::evaluate(Evaluator &evaluator,
                                              const SourceFile *file) const {
  llvm::SmallVector<Decl *> usingDecls;
  llvm::copy_if(file->getTopLevelDecls(), std::back_inserter(usingDecls),
                [](Decl *D) { return isa<UsingDecl>(D); });

  if (usingDecls.empty())
    return std::nullopt;

  std::optional<std::pair<Decl *, DefaultIsolation>> isolation;

  auto setIsolation = [&isolation](Decl *D, DefaultIsolation newIsolation) {
    if (isolation) {
      D->diagnose(diag::invalid_redecl_of_file_isolation);
      isolation->first->diagnose(diag::invalid_redecl_of_file_isolation_prev);
      return;
    }

    isolation = std::make_pair(D, newIsolation);
  };

  for (auto *D : usingDecls) {
    switch (cast<UsingDecl>(D)->getSpecifier()) {
    case UsingSpecifier::MainActor:
      setIsolation(D, DefaultIsolation::MainActor);
      break;
    case UsingSpecifier::Nonisolated:
      setIsolation(D, DefaultIsolation::Nonisolated);
      break;
    }
  }

  return isolation.has_value() ? std::optional(isolation->second)
                               : std::nullopt;
}

void swift::checkOverrideActorIsolation(ValueDecl *value) {
  if (isa<TypeDecl>(value))
    return;

  auto overridden = value->getOverriddenDeclOrSuperDeinit();
  if (!overridden)
    return;

  // Determine the actor isolation of the overriding function.
  auto isolation = getActorIsolation(value);
  
  // Determine the actor isolation of the overridden function.
  auto overriddenIsolation = getOverriddenIsolationFor(value);
  switch (validOverrideIsolation(
              value, isolation, overridden, overriddenIsolation)) {
  case OverrideIsolationResult::Allowed:
    return;

  case OverrideIsolationResult::Sendable:
    // Check that the results of the overriding method are sendable
    diagnoseNonSendableTypesInReference(
        /*base=*/nullptr,
        getDeclRefInContext(value), value->getInnermostDeclContext(),
        value->getLoc(), SendableCheckReason::Override,
        getActorIsolation(value), FunctionCheckKind::Results);

    // Check that the parameters of the overridden method are sendable
    diagnoseNonSendableTypesInReference(
        /*base=*/nullptr,
        getDeclRefInContext(overridden), overridden->getInnermostDeclContext(),
        overridden->getLoc(), SendableCheckReason::Override,
        getActorIsolation(value), FunctionCheckKind::Params,
        value->getLoc());
    return;

  case OverrideIsolationResult::Disallowed:
    // Diagnose below.
    break;
  }

  // Isolation mismatch. Diagnose it.
  DiagnosticBehavior behavior = DiagnosticBehavior::Unspecified;
  if (overridden->hasClangNode() && !overriddenIsolation) {
    behavior = SendableCheckContext(value->getInnermostDeclContext())
        .defaultDiagnosticBehavior();
  }

  value->diagnose(
      diag::actor_isolation_override_mismatch, isolation,
      value, overriddenIsolation)
    .limitBehaviorUntilSwiftVersion(behavior, 6);
  overridden->diagnose(diag::overridden_here);
}

void swift::checkGlobalIsolation(VarDecl *var) {
  const auto isolation = getActorIsolation(var);

  // Skip this if the relevant features aren't supported.
  if (!var->getLoc() ||
      !var->getASTContext().LangOpts.hasFeature(Feature::GlobalConcurrency))
    return;

  // Skip if the decl is global actor-isolated or is unsafely opted-out.
  if (isolation.isGlobalActor() || isolation.isNonisolatedUnsafe())
    return;

  // We're only concerned with global storage.
  if (!var->isGlobalStorage())
    return;

  // At this point, we've found global state that may need to be diagnosed.
  auto *diagVar = var;

  // Look through property wrappers.
  if (auto *originalVar = var->getOriginalWrappedProperty())
    diagVar = originalVar;

  bool diagnosed = false;
  if (var->isLet()) {
    // `let` variables are okay if they are of Sendable type.
    auto type = var->getInterfaceType();
    diagnosed = diagnoseIfAnyNonSendableTypes(
        type, SendableCheckContext(var->getDeclContext()),
        /*inDerivedConformance=*/Type(), /*typeLoc=*/SourceLoc(),
        /*diagnoseLoc=*/var->getLoc(), diag::shared_immutable_state_decl,
        diagVar);
  } else {
    diagVar->diagnose(diag::shared_mutable_state_decl, diagVar)
        .warnUntilSwiftVersion(6);
    diagnosed = true;
  }

  // If we didn't find anything to report, we're done.
  if (!diagnosed)
    return;

  // If we diagnosed this global, tack on notes to suggest potential courses
  // of action.
  if (!var->isLet()) {
    auto diag = diagVar->diagnose(diag::shared_state_make_immutable, diagVar);
    SourceLoc fixItLoc = getFixItLocForVarToLet(diagVar);
    if (fixItLoc.isValid())
      diag.fixItReplace(fixItLoc, "let");
  }

  auto mainActor = var->getASTContext().getMainActorType();
  if (mainActor) {
    diagVar
        ->diagnose(diag::add_globalactor_to_decl, mainActor->getString(),
                   diagVar, mainActor)
        .fixItInsert(diagVar->getAttributeInsertionLoc(false),
                     diag::insert_globalactor_attr, mainActor);
  }
  diagVar->diagnose(diag::shared_state_nonisolated_unsafe, diagVar)
      .fixItInsert(diagVar->getAttributeInsertionLoc(true),
                   "nonisolated(unsafe) ");
}

bool swift::contextRequiresStrictConcurrencyChecking(
    const DeclContext *dc,
    llvm::function_ref<Type(const AbstractClosureExpr *)> getType,
    llvm::function_ref<bool(const ClosureExpr *)> isolatedByPreconcurrency) {
  auto concurrencyLevel = dc->getASTContext().LangOpts.StrictConcurrencyLevel;
  switch (concurrencyLevel) {
  case StrictConcurrency::Complete:
    return true;

  case StrictConcurrency::Targeted:
  case StrictConcurrency::Minimal:
    // Check below to see if the context has adopted concurrency features.
    break;
  }

  while (!dc->isModuleScopeContext()) {
    if (auto closure = dyn_cast<AbstractClosureExpr>(dc)) {
      // A closure with an explicit global actor, async, or Sendable
      // uses concurrency features.
      if (auto explicitClosure = dyn_cast<ClosureExpr>(closure)) {
        if (getExplicitGlobalActor(const_cast<ClosureExpr *>(explicitClosure)))
          return true;

        // Don't take any more cues if this only got its type information by
        // being provided to a `@preconcurrency` operation.
        //
        // FIXME: contextRequiresStrictConcurrencyChecking is called from
        // within the constraint system, but closures are only set to be isolated
        // by preconcurrency in solution application because it's dependent on
        // overload resolution. The constraint system either needs to check its
        // own state on the current path, or not make type inference decisions based
        // on concurrency checking level.
        if (isolatedByPreconcurrency(explicitClosure)) {
          // If we're in minimal checking, preconcurrency always suppresses
          // diagnostics. Targeted checking will still produce diagnostics if
          // the outer context has adopted explicit concurrency features.
          if (concurrencyLevel == StrictConcurrency::Minimal)
            return false;

          dc = dc->getParent();
          continue;
        }

        if (auto type = getType(closure)) {
          if (auto fnType = type->getAs<AnyFunctionType>())
            if (fnType->isAsync() || fnType->isSendable())
              return true;
        }
      }

      // Async and @Sendable closures use concurrency features.
      if (closure->isBodyAsync() || closure->isSendable())
        return true;
    } else if (auto decl = dc->getAsDecl()) {
      // If any isolation attributes are present, we're using concurrency
      // features.
      if (decl->hasExplicitIsolationAttribute())
        return true;

      // Extensions of explicitly isolated types are using concurrency
      // features.
      if (auto *extension = dyn_cast<ExtensionDecl>(decl)) {
        auto *nominal = extension->getExtendedNominal();
        if (nominal && nominal->hasExplicitIsolationAttribute() &&
            !getActorIsolation(nominal).preconcurrency())
          return true;
      }

      if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
        // Async and concurrent functions use concurrency features.
        if (func->hasAsync() || func->isSendable())
          return true;

        // If we're in an accessor declaration, also check the storage
        // declaration.
        if (auto accessor = dyn_cast<AccessorDecl>(decl)) {
          if (accessor->getStorage()->hasExplicitIsolationAttribute())
            return true;
        }
      }
    }

    // If we're in an actor, we're using concurrency features.
    if (auto nominal = dc->getSelfNominalTypeDecl()) {
      if (nominal->isActor())
        return true;
    }

    // Keep looking.
    dc = dc->getParent();
  }

  return false;
}

/// Check the instance storage of the given nominal type to verify whether
/// it is comprised only of Sendable instance storage.
static bool checkSendableInstanceStorage(
    NominalTypeDecl *nominal, DeclContext *dc, SendableCheck check) {
  // Raw storage is assumed not to be sendable.
  if (auto sd = dyn_cast<StructDecl>(nominal)) {
    if (sd->getAttrs().hasAttribute<RawLayoutAttr>()) {
      auto behavior = SendableCheckContext(
            dc, check).defaultDiagnosticBehavior();
      if (!isImplicitSendableCheck(check)
          && SendableCheckContext(dc, check)
               .defaultDiagnosticBehavior() != DiagnosticBehavior::Ignore) {
        sd->diagnose(diag::sendable_raw_storage, sd->getName())
          .limitBehaviorUntilSwiftVersion(behavior, 6);
      }
      return true;
    }
  }

  // Stored properties of structs and classes must have
  // Sendable-conforming types.
  class Visitor: public StorageVisitor {
  public:
    bool invalid = false;
    NominalTypeDecl *nominal;
    DeclContext *dc;
    SendableCheck check;
    const LangOptions &langOpts;

    Visitor(NominalTypeDecl *nominal, DeclContext *dc, SendableCheck check)
      : StorageVisitor(), nominal(nominal), dc(dc), check(check),
        langOpts(nominal->getASTContext().LangOpts) { }

    /// Handle a stored property.
    bool operator()(VarDecl *property, Type propertyType) override {
      ActorIsolation isolation = getActorIsolation(property);

      // 'nonisolated' properties are always okay in 'Sendable' types because
      // they can be accessed from anywhere. Note that 'nonisolated' without
      // '(unsafe)' can only be applied to immutable, 'Sendable' properties.
      if (isolation.isNonisolated())
        return false;

      // Classes with mutable properties are Sendable if property is
      // actor-isolated
      if (isa<ClassDecl>(nominal)) {
        if (property->supportsMutation() && isolation.isUnspecified()) {
          auto behavior =
              SendableCheckContext(dc, check).defaultDiagnosticBehavior();
          // If Sendable came from a `@preconcurrency` protocol the error
          // should be downgraded even with strict concurrency checking to
          // allow clients time to address the new requirement.
          auto preconcurrency =
              check == SendableCheck::ImpliedByPreconcurrencyProtocol;
          if (behavior != DiagnosticBehavior::Ignore) {
            property
                ->diagnose(diag::concurrent_value_class_mutable_property,
                           property->getName(), nominal)
                .limitBehaviorWithPreconcurrency(behavior, preconcurrency);
          }
          invalid = invalid || (behavior == DiagnosticBehavior::Unspecified);
          return true;
        }

        if (!(isolation.isNonisolated() || isolation.isUnspecified())) {
          return false; // skip sendable check on actor-isolated properties
        }
      }

      return checkSendabilityOfMemberType(property, propertyType);
    }

    /// Handle an enum associated value.
    bool operator()(EnumElementDecl *element, Type elementType) override {
      return checkSendabilityOfMemberType(element, elementType);
    }

  private:
    bool checkSendabilityOfMemberType(ValueDecl *member, Type memberType) {
      SendableCheckContext context(dc, check);
      diagnoseNonSendableTypes(
          memberType, context,
          /*inDerivedConformance*/ Type(), member->getLoc(),
          [&](Type type, DiagnosticBehavior behavior) {
            auto preconcurrencyBehavior = context.preconcurrencyBehavior(type);
            if (isImplicitSendableCheck(check)) {
              // If this is for an externally-visible conformance, fail.
              if (check == SendableCheck::ImplicitForExternallyVisible) {
                invalid = true;
                return true;
              }

              // If we are to ignore this diagnostic, just continue.
              if (behavior == DiagnosticBehavior::Ignore ||
                  preconcurrencyBehavior == DiagnosticBehavior::Ignore)
                return true;

              invalid = true;
              return true;
            }

            // If Sendable came from a `@preconcurrency` protocol the error
            // should be downgraded even with strict concurrency checking to
            // allow clients time to address the new requirement.
            bool fromPreconcurrencyConformance =
                check == SendableCheck::ImpliedByPreconcurrencyProtocol;

            if (preconcurrencyBehavior)
              behavior = preconcurrencyBehavior.value();

            member
                ->diagnose(diag::non_concurrent_type_member, type,
                           isa<EnumElementDecl>(member), member->getName(),
                           nominal, type->isEqual(memberType))
                .limitBehaviorWithPreconcurrency(
                    behavior, fromPreconcurrencyConformance ||
                                  preconcurrencyBehavior.has_value());
            return false;
          });

      if (invalid) {
        // For implicit checks, bail out early if anything failed.
        if (isImplicitSendableCheck(check))
          return true;
      }

      return false;
    }

  } visitor(nominal, dc, check);

  return visitor.visit(nominal, dc) || visitor.invalid;
}

bool swift::checkSendableConformance(
    ProtocolConformance *conformance, SendableCheck check) {
  ASSERT(conformance->getProtocol()->isSpecificProtocol(
           KnownProtocolKind::Sendable));

  auto conformanceDC = conformance->getDeclContext();
  auto nominal = conformance->getType()->getAnyNominal();
  if (!nominal)
    return false;

  // If this is an always-unavailable conformance, there's nothing to check.
  // We always use the root conformance for this check, because inherited
  // conformances need to walk back to the original declaration for the
  // superclass conformance to find an unavailable attribute.
  if (auto ext = dyn_cast<ExtensionDecl>(
          conformance->getRootConformance()->getDeclContext())) {
    if (ext->isUnavailable())
      return false;
  }

  bool isUnchecked = false;
  if (auto *normal = conformance->getRootNormalConformance())
    isUnchecked = normal->isUnchecked();

  if (isUnchecked) {
    // Warn if inferred or inherited '@unchecked Sendable' is not restated.
    // Beyond that, '@unchecked Sendable' requires no further checking.

    if (!isa<InheritedProtocolConformance>(conformance))
      return false;

    auto statesUnchecked =
      [](InheritedTypes inheritedTypes, DeclContext *dc) -> bool {
        for (auto i : inheritedTypes.getIndices()) {
          auto inheritedType =
              TypeResolution::forInterface(
                  dc,
                  TypeResolverContext::Inherited,
                  /*unboundTyOpener*/ nullptr,
                  /*placeholderHandler*/ nullptr,
                  /*packElementOpener*/ nullptr)
              .resolveType(inheritedTypes.getTypeRepr(i));

          if (!inheritedType || inheritedType->hasError())
            continue;

          if (inheritedType->getKnownProtocol() != KnownProtocolKind::Sendable)
            continue;

          if (inheritedTypes.getEntry(i).isUnchecked())
            return true;
        }

        return false;
      };

    if (statesUnchecked(nominal->getInherited(), nominal))
      return false;

    for (auto *extension : nominal->getExtensions()) {
      if (statesUnchecked(extension->getInherited(), extension))
        return false;
    }

    auto diag =
        nominal->diagnose(diag::restate_unchecked_sendable, nominal->getName());
    addSendableFixIt(nominal, diag, /*unchecked=*/true);
    return false;
  }

  auto classDecl = dyn_cast<ClassDecl>(nominal);
  if (classDecl) {
    // Actors implicitly conform to Sendable and protect their state.
    if (classDecl->isActor())
      return false;
  }

  // Global-actor-isolated types can be Sendable. We do not check the
  // instance data because it's all isolated to the global actor.
  switch (getActorIsolation(nominal)) {
  case ActorIsolation::Unspecified:
  case ActorIsolation::ActorInstance:
  case ActorIsolation::Nonisolated:
  case ActorIsolation::CallerIsolationInheriting:
  case ActorIsolation::NonisolatedUnsafe:
    break;

  case ActorIsolation::Erased:
    llvm_unreachable("type cannot have erased isolation");

  case ActorIsolation::GlobalActor:
    return false;
  }

  // An implied conformance is generated when you state a conformance to
  // a protocol P that inherits from Sendable.
  bool wasImplied = (conformance->getSourceKind() ==
                     ConformanceEntryKind::Implied);

  // Sendable can only be used in the same source file.
  auto conformanceDecl = conformanceDC->getAsDecl();
  SendableCheckContext checkContext(conformanceDC, check);
  DiagnosticBehavior behavior = checkContext.defaultDiagnosticBehavior();
  if (wasImplied) {
    if (auto optBehavior = checkContext.preconcurrencyBehavior(
            nominal, /*ignoreExplicitConformance=*/true))
      behavior = *optBehavior;
  }

  if (conformanceDC->getOutermostParentSourceFile() &&
      conformanceDC->getOutermostParentSourceFile() !=
      nominal->getOutermostParentSourceFile()) {
    if (!(nominal->hasClangNode() && wasImplied)) {
      conformanceDecl->diagnose(diag::concurrent_value_outside_source_file,
                                nominal)
        .limitBehaviorUntilSwiftVersion(behavior, 6);

      if (behavior == DiagnosticBehavior::Unspecified)
        return true;
    }
  }

  if (classDecl && classDecl->getParentSourceFile()) {
    bool isInherited = isa<InheritedProtocolConformance>(conformance);

    // An non-final class cannot conform to `Sendable`.
    if (!classDecl->isSemanticallyFinal()) {
      classDecl->diagnose(diag::concurrent_value_nonfinal_class,
                          classDecl->getName())
        .limitBehaviorUntilSwiftVersion(behavior, 6);

      if (behavior == DiagnosticBehavior::Unspecified)
        return true;
    }

    if (!isInherited) {
      // A 'Sendable' class cannot inherit from another class, although
      // we allow `NSObject` for Objective-C interoperability.
      if (auto superclassDecl = classDecl->getSuperclassDecl()) {
        if (!superclassDecl->isNSObject()) {
          classDecl
              ->diagnose(diag::concurrent_value_inherit,
                         nominal->getASTContext().LangOpts.EnableObjCInterop,
                         classDecl->getName())
              .limitBehaviorUntilSwiftVersion(behavior, 6);

          if (behavior == DiagnosticBehavior::Unspecified)
            return true;
        }
      }
    }
  }

  // In -swift-version 5 mode, a conditional conformance to a protocol can imply
  // a Sendable conformance. The implied conformance is unconditional, so check
  // the storage for sendability as if the conformance was declared on the nominal,
  // and not some (possibly constrained) extension.
  if (wasImplied)
    conformanceDC = nominal;
  return checkSendableInstanceStorage(nominal, conformanceDC, check);
}

/// Add "unavailable" attributes to the given extension.
static void addUnavailableAttrs(ExtensionDecl *ext, NominalTypeDecl *nominal) {
  ASTContext &ctx = nominal->getASTContext();
  llvm::VersionTuple noVersion;

  // Add platform-version-specific @available attributes. Search from nominal
  // type declaration through its enclosing declarations to find the first one
  // with platform-specific attributes.
  for (Decl *enclosing = nominal;
       enclosing;
       enclosing = enclosing->getDeclContext()
           ? enclosing->getDeclContext()->getAsDecl()
           : nullptr) {
    bool anyPlatformSpecificAttrs = false;
    for (auto available : enclosing->getSemanticAvailableAttrs()) {
      // FIXME: [availability] Generalize to AvailabilityDomain.
      if (available.getPlatform() == PlatformKind::none)
        continue;

      auto attr = new (ctx) AvailableAttr(
          SourceLoc(), SourceRange(),
          AvailabilityDomain::forPlatform(available.getPlatform()), SourceLoc(),
          AvailableAttr::Kind::Unavailable, available.getMessage(),
          /*Rename=*/"", available.getIntroduced().value_or(noVersion),
          SourceRange(), available.getDeprecated().value_or(noVersion),
          SourceRange(), available.getObsoleted().value_or(noVersion),
          SourceRange(),
          /*Implicit=*/true, available.getParsedAttr()->isSPI());
      ext->getAttrs().add(attr);
      anyPlatformSpecificAttrs = true;
    }

    // If we found any platform-specific availability attributes, we're done.
    if (anyPlatformSpecificAttrs)
      break;
  }

  // Add the blanket "unavailable".

  ext->getAttrs().add(
      AvailableAttr::createUniversallyUnavailable(ctx, /*Message=*/""));
}

ProtocolConformance *swift::deriveImplicitSendableConformance(
    Evaluator &evaluator, NominalTypeDecl *nominal) {
  // Protocols never get implicit Sendable conformances.
  if (isa<ProtocolDecl>(nominal))
    return nullptr;

  // Actor types are always Sendable; they don't get it via this path.
  auto classDecl = dyn_cast<ClassDecl>(nominal);
  if (classDecl && classDecl->isActor())
    return nullptr;

  // Check whether we can infer conformance at all.
  if (auto *file = dyn_cast<FileUnit>(nominal->getModuleScopeContext())) {
    switch (file->getKind()) {
    case FileUnitKind::Source:
      // Check what kind of source file we have.
      if (auto sourceFile = nominal->getParentSourceFile()) {
        switch (sourceFile->Kind) {
        case SourceFileKind::Interface:
          // Interfaces have explicitly called-out Sendable conformances.
          return nullptr;

        case SourceFileKind::DefaultArgument:
        case SourceFileKind::Library:
        case SourceFileKind::MacroExpansion:
        case SourceFileKind::Main:
        case SourceFileKind::SIL:
          break;
        }
      }
      break;

    case FileUnitKind::Builtin:
    case FileUnitKind::SerializedAST:
    case FileUnitKind::Synthesized:
      // Explicitly-handled modules don't infer Sendable conformances.
      return nullptr;

    case FileUnitKind::ClangModule:
    case FileUnitKind::DWARFModule:
      // Infer conformances for imported modules.
      break;
    }
  } else {
    return nullptr;
  }

  ASTContext &ctx = nominal->getASTContext();
  auto proto = ctx.getProtocol(KnownProtocolKind::Sendable);
  if (!proto)
    return nullptr;

  // Local function to form the implicit conformance.
  auto formConformance = [&](const DeclAttribute *attrMakingUnavailable)
        -> ProtocolConformance * {
    DeclContext *conformanceDC = nominal;

    // FIXME: @_nonSendable should be a builtin extension macro. This behavior
    // of explanding the unavailable conformance during implicit Sendable
    // derivation means that clients can unknowingly ignore unavailable Sendable
    // Sendable conformances from the original module added via @_nonSendable
    // because they are not expanded if an explicit conformance is found via
    // conformance lookup. So, if a retroactive, unchecked Sendable conformance
    // is written, no redundant conformance warning is emitted.
    if (attrMakingUnavailable) {
      // Conformance availability is currently tied to the declaring extension.
      // FIXME: This is a hack--we should give conformances real availability.
      auto inherits = ctx.AllocateCopy(llvm::ArrayRef(
          InheritedEntry(TypeLoc::withoutLoc(proto->getDeclaredInterfaceType()),
                         ProtocolConformanceFlags::Unchecked)));
      // If you change the use of AtLoc in the ExtensionDecl, make sure you
      // update isNonSendableExtension() in ASTPrinter.
      auto extension = ExtensionDecl::create(ctx, attrMakingUnavailable->AtLoc,
                                             nullptr, inherits,
                                             nominal->getModuleScopeContext(),
                                             nullptr);
      extension->setImplicit();
      addUnavailableAttrs(extension, nominal);

      ctx.evaluator.cacheOutput(ExtendedTypeRequest{extension},
                                nominal->getDeclaredType());
      extension->setExtendedNominal(nominal);
      nominal->addExtension(extension);

      // Make it accessible to getTopLevelDecls()
      if (auto file = dyn_cast<FileUnit>(nominal->getModuleScopeContext()))
        file->getOrCreateSynthesizedFile().addTopLevelDecl(extension);

      conformanceDC = extension;

      // Let the conformance lookup table register the conformance
      // from the extension. Otherwise, we'll end up with redundant
      // conformances between the explicit conformance from the extension
      // and the conformance synthesized below.
      SmallVector<ProtocolConformance *, 2> conformances;
      nominal->lookupConformance(proto, conformances);
      for (auto conformance : conformances) {
        if (conformance->getDeclContext() == conformanceDC) {
          return conformance;
        }
      }
    }

    ProtocolConformanceOptions options;
    if (attrMakingUnavailable != nullptr)
      options |= ProtocolConformanceFlags::Unchecked;
    auto conformance = ctx.getNormalConformance(
        nominal->getDeclaredInterfaceType(), proto, nominal->getLoc(),
        /*inheritedTypeRepr=*/nullptr, conformanceDC,
        ProtocolConformanceState::Complete, options);
    conformance->setSourceKindAndImplyingConformance(
        ConformanceEntryKind::Synthesized, nullptr);

    nominal->registerProtocolConformance(conformance, /*synthesized=*/true);
    return conformance;
  };

  // If this is a class, check the superclass. If it's already Sendable,
  // form an inherited conformance.
  if (classDecl) {
    if (Type superclass = classDecl->getSuperclass()) {
      auto inheritedConformance = checkConformance(
          classDecl->mapTypeIntoContext(superclass),
          proto, /*allowMissing=*/false);
      if (inheritedConformance) {
        inheritedConformance = inheritedConformance
            .mapConformanceOutOfContext();
        if (inheritedConformance.isConcrete()) {
          return ctx.getInheritedConformance(
              nominal->getDeclaredInterfaceType(),
              inheritedConformance.getConcrete());
        }
      }

      // Classes that add global actor isolation to non-Sendable
      // superclasses cannot be 'Sendable'.
      auto superclassDecl = classDecl->getSuperclassDecl();
      if (nominal->getGlobalActorAttr() && !superclassDecl->isNSObject()) {
        return nullptr;
      }
    }
  }

  // A non-protocol type with a global actor is implicitly Sendable.
  if (getActorIsolation(nominal).isGlobalActor()) {
    // Form the implicit conformance to Sendable.
    return formConformance(nullptr);
  }

  if (auto attr = nominal->getAttrs().getEffectiveSendableAttr()) {
    assert(!isa<SendableAttr>(attr) &&
           "Conformance should have been added by SynthesizedProtocolAttr!");
    return formConformance(cast<NonSendableAttr>(attr));
  }

  // Only structs and enums can get implicit Sendable conformances by
  // considering their instance data.
  if (!isa<StructDecl>(nominal) && !isa<EnumDecl>(nominal))
    return nullptr;

  SendableCheck check;

  // Okay to infer Sendable conformance for non-public types.
  if (!nominal->getFormalAccessScope(
          /*useDC=*/nullptr, /*treatUsableFromInlineAsPublic=*/true)
            .isPublic()) {
    check = SendableCheck::Implicit;
  } else if (nominal->hasClangNode() ||
             nominal->getAttrs().hasAttribute<FixedLayoutAttr>() ||
             nominal->getAttrs().hasAttribute<FrozenAttr>()) {
    // @_frozen public types can also infer Sendable, but be more careful here.
    check = SendableCheck::ImplicitForExternallyVisible;
  } else {
    // No inference.
    return nullptr;
  }

  // Check the instance storage for Sendable conformance.
  if (checkSendableInstanceStorage(nominal, nominal, check))
    return nullptr;

  return formConformance(nullptr);
}

/// Apply @Sendable and/or @MainActor to the given parameter type.
static Type applyUnsafeConcurrencyToParameterType(
    Type type, bool sendable, bool mainActor) {
  if (Type objectType = type->getOptionalObjectType()) {
    return OptionalType::get(
        applyUnsafeConcurrencyToParameterType(objectType, sendable, mainActor));
  }

  auto fnType = type->getAs<FunctionType>();
  if (!fnType)
    return type;

  auto isolation = fnType->getIsolation();
  if (mainActor)
    isolation = FunctionTypeIsolation::forGlobalActor(
                  type->getASTContext().getMainActorType());

  return fnType->withExtInfo(fnType->getExtInfo()
                               .withSendable(sendable)
                               .withIsolation(isolation));
}

/// Determine whether the given name is that of a DispatchQueue operation that
/// takes a closure to be executed on the queue.
std::optional<DispatchQueueOperation>
swift::isDispatchQueueOperationName(StringRef name) {
  return llvm::StringSwitch<std::optional<DispatchQueueOperation>>(name)
      .Case("sync", DispatchQueueOperation::Normal)
      .Case("async", DispatchQueueOperation::Sendable)
      .Case("asyncAndWait", DispatchQueueOperation::Normal)
      .Case("asyncUnsafe", DispatchQueueOperation::Normal)
      .Case("asyncAfter", DispatchQueueOperation::Sendable)
      .Case("concurrentPerform", DispatchQueueOperation::Sendable)
      .Default(std::nullopt);
}

/// Determine whether this function is implicitly known to have its
/// parameters of function type be @_unsafeSendable.
///
/// This hard-codes knowledge of a number of functions that will
/// eventually have @_unsafeSendable and, eventually, @Sendable,
/// on their parameters of function type.
static bool hasKnownUnsafeSendableFunctionParams(AbstractFunctionDecl *func) {
  auto nominal = func->getDeclContext()->getSelfNominalTypeDecl();
  if (!nominal)
    return false;

  // DispatchQueue operations.
  auto nominalName = nominal->getName().str();
  if (nominalName == "DispatchQueue") {
    auto name = func->getBaseName().userFacingName();
    auto operation = isDispatchQueueOperationName(name);
    if (!operation)
      return false;

    switch (*operation) {
    case DispatchQueueOperation::Normal:
      return false;

    case DispatchQueueOperation::Sendable:
      return true;
    }
  }

  return false;
}

Type swift::adjustVarTypeForConcurrency(
    Type type, VarDecl *var, DeclContext *dc,
    llvm::function_ref<Type(const AbstractClosureExpr *)> getType,
    llvm::function_ref<bool(const ClosureExpr *)> isolatedByPreconcurrency) {
  if (!var->preconcurrency())
    return type;

  if (contextRequiresStrictConcurrencyChecking(
          dc, getType, isolatedByPreconcurrency))
    return type;

  bool isLValue = false;
  if (auto *lvalueType = type->getAs<LValueType>()) {
    type = lvalueType->getObjectType();
    isLValue = true;
  }

  type = type->stripConcurrency(/*recurse=*/false, /*dropGlobalActor=*/true);

  if (isLValue)
    type = LValueType::get(type);

  return type;
}

/// Adjust a function type for @_unsafeSendable, @_unsafeMainActor, and
/// @preconcurrency.
static AnyFunctionType *applyUnsafeConcurrencyToFunctionType(
    AnyFunctionType *fnType, ValueDecl *decl,
    bool inConcurrencyContext, unsigned numApplies, bool isMainDispatchQueue) {
  // Functions/subscripts/enum elements have function types to adjust.
  auto func = dyn_cast_or_null<AbstractFunctionDecl>(decl);
  auto subscript = dyn_cast_or_null<SubscriptDecl>(decl);

  if (!func && !subscript)
    return fnType;

  AnyFunctionType *outerFnType = nullptr;
  if ((subscript && numApplies > 1) || (func && func->hasImplicitSelfDecl())) {
    outerFnType = fnType;
    fnType = outerFnType->getResult()->castTo<AnyFunctionType>();

    if (numApplies > 0)
      --numApplies;
  }

  SmallVector<AnyFunctionType::Param, 4> newTypeParams;
  auto typeParams = fnType->getParams();
  auto paramDecls = decl->getParameterList();
  assert(typeParams.size() == paramDecls->size());
  bool knownUnsafeParams = func && hasKnownUnsafeSendableFunctionParams(func);
  bool stripConcurrency =
      decl->preconcurrency() && !inConcurrencyContext;
  for (unsigned index : indices(typeParams)) {
    auto param = typeParams[index];

    // Determine whether the resulting parameter should be @Sendable or
    // @MainActor. @Sendable occurs only in concurrency contents, while
    // @MainActor occurs in concurrency contexts or those where we have an
    // application.
    bool addSendable = knownUnsafeParams && inConcurrencyContext;
    bool addMainActor = isMainDispatchQueue &&
        (inConcurrencyContext || numApplies >= 1);
    Type newParamType = param.getPlainType();
    if (addSendable || addMainActor) {
      newParamType = applyUnsafeConcurrencyToParameterType(
        param.getPlainType(), addSendable, addMainActor);
    } else if (stripConcurrency && numApplies == 0) {
      newParamType = param.getPlainType()->stripConcurrency(
          /*recurse=*/false, /*dropGlobalActor=*/numApplies == 0);
    }

    if (!newParamType || newParamType->isEqual(param.getPlainType())) {
      // If any prior parameter has changed, record this one.
      if (!newTypeParams.empty())
        newTypeParams.push_back(param);

      continue;
    }

    // If this is the first parameter to have changed, copy all of the others
    // over.
    if (newTypeParams.empty()) {
      newTypeParams.append(typeParams.begin(), typeParams.begin() + index);
    }

    // Transform the parameter type.
    newTypeParams.push_back(param.withType(newParamType));
  }

  // Compute the new result type.
  Type newResultType = fnType->getResult();
  if (stripConcurrency) {
    newResultType = newResultType->stripConcurrency(
        /*recurse=*/false, /*dropGlobalActor=*/true);

    if (!newResultType->isEqual(fnType->getResult()) && newTypeParams.empty()) {
      newTypeParams.append(typeParams.begin(), typeParams.end());
    }
  }

  // If we didn't change any parameters, we're done.
  if (newTypeParams.empty() && newResultType->isEqual(fnType->getResult())) {
    return outerFnType ? outerFnType : fnType;
  }

  // Rebuild the (inner) function type.
  fnType = FunctionType::get(
      newTypeParams, newResultType, fnType->getExtInfo());

  if (!outerFnType)
    return fnType;

  // Rebuild the outer function type.
  if (auto genericFnType = dyn_cast<GenericFunctionType>(outerFnType)) {
    return GenericFunctionType::get(
        genericFnType->getGenericSignature(), outerFnType->getParams(),
        Type(fnType), outerFnType->getExtInfo());
  }

  return FunctionType::get(
      outerFnType->getParams(), Type(fnType), outerFnType->getExtInfo());
}

AnyFunctionType *swift::adjustFunctionTypeForConcurrency(
    AnyFunctionType *fnType, ValueDecl *decl, DeclContext *dc,
    unsigned numApplies, bool isMainDispatchQueue,
    llvm::function_ref<Type(const AbstractClosureExpr *)> getType,
    llvm::function_ref<bool(const ClosureExpr *)> isolatedByPreconcurrency,
    llvm::function_ref<Type(Type)> openType) {
  // Apply unsafe concurrency features to the given function type.
  bool strictChecking = contextRequiresStrictConcurrencyChecking(
      dc, getType, isolatedByPreconcurrency);

  fnType = applyUnsafeConcurrencyToFunctionType(
      fnType, decl, strictChecking, numApplies, isMainDispatchQueue);
  std::optional<FunctionTypeIsolation> funcIsolation;
  if (decl) {
    switch (auto isolation = getActorIsolation(decl)) {
    case ActorIsolation::ActorInstance:
      // The function type may or may not have parameter isolation.
      return fnType;

    case ActorIsolation::CallerIsolationInheriting:
      assert(fnType->getIsolation().isNonIsolated());
      funcIsolation = FunctionTypeIsolation::forNonIsolatedCaller();
      break;

    case ActorIsolation::Nonisolated:
    case ActorIsolation::NonisolatedUnsafe:
    case ActorIsolation::Unspecified:
      assert(fnType->getIsolation().isNonIsolated());
      return fnType;

    case ActorIsolation::Erased:
      llvm_unreachable("declaration cannot have erased isolation");

    case ActorIsolation::GlobalActor:
      // For preconcurrency, only treat as global-actor-qualified
      // within code that has adopted Swift Concurrency features.
      if (!strictChecking && isolation.preconcurrency())
        return fnType;

      Type globalActorType = openType(isolation.getGlobalActor());
      funcIsolation = FunctionTypeIsolation::forGlobalActor(globalActorType);
      break;
    }
  }

  ASSERT(funcIsolation.has_value());

  // If there's no implicit "self" declaration, apply the isolation to
  // the outermost function type.
  bool hasImplicitSelfDecl = decl && (isa<EnumElementDecl>(decl) ||
      (isa<AbstractFunctionDecl>(decl) &&
       cast<AbstractFunctionDecl>(decl)->hasImplicitSelfDecl()));
  if (!hasImplicitSelfDecl) {
    return fnType->withExtInfo(
        fnType->getExtInfo().withIsolation(*funcIsolation));
  }

  // Dig out the inner function type.
  auto innerFnType = fnType->getResult()->getAs<AnyFunctionType>();
  if (!innerFnType)
    return fnType;

  // Update the inner function type with the isolation.
  innerFnType = innerFnType->withExtInfo(
      innerFnType->getExtInfo().withIsolation(*funcIsolation));

  // Rebuild the outer function type around it.
  if (auto genericFnType = dyn_cast<GenericFunctionType>(fnType)) {
    return GenericFunctionType::get(
        genericFnType->getGenericSignature(), fnType->getParams(),
        Type(innerFnType), fnType->getExtInfo());
  }

  return FunctionType::get(
      fnType->getParams(), Type(innerFnType), fnType->getExtInfo());
}

bool swift::completionContextUsesConcurrencyFeatures(const DeclContext *dc) {
  return contextRequiresStrictConcurrencyChecking(
      dc, [](const AbstractClosureExpr *) {
        return Type();
      },
    [](const ClosureExpr *closure) {
      return closure->isIsolatedByPreconcurrency();
    });
}

/// Find the directly-referenced parameter or capture of a parameter for
/// for the given expression.
VarDecl *swift::getReferencedParamOrCapture(
    Expr *expr,
    llvm::function_ref<Expr *(OpaqueValueExpr *)> getExistentialValue,
    llvm::function_ref<VarDecl *()> getCurrentIsolatedVar) {
  // Look through identity expressions and implicit conversions.
  Expr *prior;

  do {
    prior = expr;

    expr = expr->getSemanticsProvidingExpr();

    if (auto conversion = dyn_cast<ImplicitConversionExpr>(expr))
      expr = conversion->getSubExpr();

    // Map opaque values.
    if (auto opaqueValue = dyn_cast<OpaqueValueExpr>(expr)) {
      if (auto *value = getExistentialValue(opaqueValue))
        expr = value;
    }
  } while (prior != expr);

  // 'super' references always act on a 'self' variable.
  if (auto super = dyn_cast<SuperRefExpr>(expr))
    return super->getSelf();

  // Declaration references to a variable.
  if (auto declRef = dyn_cast<DeclRefExpr>(expr))
    return dyn_cast<VarDecl>(declRef->getDecl());

  // The current context isolation expression (#isolation) always
  // corresponds to the isolation of the given code.
  if (isa<CurrentContextIsolationExpr>(expr))
    return getCurrentIsolatedVar();

  // Distributed:
  // If we're referring to a member, it may be the special 'self.asLocalActor'
  // of an actor that is the result of #isolation of distributed actors.
  // the result of the value should be considered equal to the "self" isolation
  // as it only transforms the DistributedActor self to an "any Actor" self
  // used for isolation purposes.
  if (auto memberRef = dyn_cast<MemberRefExpr>(expr)) {
    if (auto refDecl = memberRef->getMember()) {
      if (auto decl = dyn_cast_or_null<VarDecl>(refDecl.getDecl())) {
        if (isDistributedActorAsLocalActorComputedProperty(decl)) {
          return getCurrentIsolatedVar();
        }
      }
    }
  }


  return nullptr;
}

bool swift::isPotentiallyIsolatedActor(
    VarDecl *var, llvm::function_ref<bool(ParamDecl *)> isIsolated) {
  if (!var)
    return false;

  if (var->getName().str() == "__secretlyKnownToBeLocal") {
    // FIXME(distributed): we did a dynamic check and know that this actor is
    //   local, but we can't express that to the type system; the real
    //   implementation will have to mark 'self' as "known to be local" after
    //   an is-local check.
    return true;
  }

  if (auto param = dyn_cast<ParamDecl>(var))
    return isIsolated(param);

  // If this is a captured 'self', check whether the original 'self' is
  // isolated.
  if (var->isSelfParamCapture())
    return var->isSelfParamCaptureIsolated();

  return false;
}

/// Determine the actor isolation used when we are referencing the given
/// declaration.
ActorIsolation swift::getActorIsolationForReference(ValueDecl *decl,
                                                    const DeclContext *fromDC) {
  auto declIsolation = getActorIsolation(decl);

  // If the isolation is preconcurrency global actor, adjust it based on
  // context itself. For contexts that require strict checking, treat it as
  // global actor isolation. Otherwise, treat it as unspecified isolation.
  if (declIsolation == ActorIsolation::GlobalActor &&
      declIsolation.preconcurrency()) {
    if (!contextRequiresStrictConcurrencyChecking(
            fromDC,
            [](const AbstractClosureExpr *closure) {
              return closure->getType();
            },
            [](const ClosureExpr *closure) {
              return closure->isIsolatedByPreconcurrency();
            })) {
      declIsolation = ActorIsolation::forUnspecified();
    }
  }

  // A constructor that is not explicitly 'nonisolated' is treated as
  // isolated from the perspective of the referencer.
  //
  // FIXME: The current state is that even `nonisolated` initializers are
  // externally treated as being on the actor, even though this model isn't
  // consistent. We'll fix it later.
  if (auto ctor = dyn_cast<ConstructorDecl>(decl)) {
    // If the constructor is part of an actor, references to it are treated
    // as needing to enter the actor.
    if (auto nominal = ctor->getDeclContext()->getSelfNominalTypeDecl()) {
      if (nominal->isAnyActor())
        return ActorIsolation::forActorInstanceSelf(ctor);
    }

    // Fall through to treat initializers like any other declaration.
  }

  // A 'nonisolated let' within an actor is treated as isolated if
  // the access is outside the module or if the property type is not
  // 'Sendable'.
  //
  // Note that this is only allowed for source compatibility reasons.
  // It is generally invalid to write `nonisolated` on an actor `let`
  // if the type of the property is not Sendable. However, older compilers
  // used to allow this, so it's only an error in the Swift 6 language
  // mode, and it is a warning in prior language modes.
  //
  // FIXME: getActorIsolation(decl) should treat these as isolated.
  // FIXME: Expand this out to local variables?
  if (auto var = dyn_cast<VarDecl>(decl)) {
    // 'nonisolated(unsafe)' opts out of actor isolation.
    if (declIsolation.isNonisolatedUnsafe())
      return declIsolation;

    auto *fromModule = fromDC->getParentModule();
    ActorReferenceResult::Options options = std::nullopt;
    if (varIsSafeAcrossActors(fromModule, var, declIsolation, std::nullopt, options) &&
        var->getTypeInContext()->isSendableType())
      return ActorIsolation::forNonisolated(/*unsafe*/false);

    if (var->isLet() && isStoredProperty(var) &&
        declIsolation.isNonisolated()) {
      if (auto nominal = var->getDeclContext()->getSelfNominalTypeDecl()) {
        if (nominal->isAnyActor())
          return ActorIsolation::forActorInstanceSelf(decl);

        auto nominalIsolation = getActorIsolation(nominal);
        if (nominalIsolation.isGlobalActor())
          return getActorIsolationForReference(nominal, fromDC);
      }
    }
  }

  return declIsolation;
}

/// Determine whether this declaration always throws.
bool swift::isThrowsDecl(ConcreteDeclRef declRef) {
  auto decl = declRef.getDecl();

  // An async function is asynchronously accessed.
  if (auto func = dyn_cast<AbstractFunctionDecl>(decl))
    return func->hasThrows();

  // A computed property or subscript that has an 'async' getter
  // is asynchronously accessed.
  if (auto storageDecl = dyn_cast<AbstractStorageDecl>(decl)) {
    if (auto effectfulGetter = storageDecl->getEffectfulGetAccessor())
      return effectfulGetter->hasThrows();
  }

  return false;
}

/// Determine whether a reference to this value isn't actually a value.
static bool isNonValueReference(const ValueDecl *value) {
  switch (value->getKind()) {
  case DeclKind::AssociatedType:
  case DeclKind::Class:
  case DeclKind::Enum:
  case DeclKind::Extension:
  case DeclKind::GenericTypeParam:
  case DeclKind::OpaqueType:
  case DeclKind::Protocol:
  case DeclKind::Struct:
  case DeclKind::TypeAlias:
  case DeclKind::EnumCase:
  case DeclKind::Import:
  case DeclKind::InfixOperator:
  case DeclKind::Missing:
  case DeclKind::MissingMember:
  case DeclKind::Module:
  case DeclKind::PatternBinding:
  case DeclKind::PostfixOperator:
  case DeclKind::PrecedenceGroup:
  case DeclKind::PrefixOperator:
  case DeclKind::TopLevelCode:
  case DeclKind::MacroExpansion:
  case DeclKind::Using:
    return true;

  case DeclKind::EnumElement:
  case DeclKind::Constructor:
  case DeclKind::Destructor:
  case DeclKind::Param:
  case DeclKind::Var:
  case DeclKind::Accessor:
  case DeclKind::Func:
  case DeclKind::Subscript:
  case DeclKind::Macro:
    return false;

  case DeclKind::BuiltinTuple:
    llvm_unreachable("BuiltinTupleDecl should not show up here");
  }
}

bool swift::isAccessibleAcrossActors(
    ValueDecl *value, const ActorIsolation &isolation,
    const DeclContext *fromDC, ActorReferenceResult::Options &options,
    std::optional<ReferencedActor> actorInstance) {
  // Initializers and enum elements are accessible across actors unless they
  // are global-actor qualified.
  if (isa<ConstructorDecl>(value) || isa<EnumElementDecl>(value)) {
    switch (isolation) {
    case ActorIsolation::ActorInstance:
    case ActorIsolation::Nonisolated:
    case ActorIsolation::CallerIsolationInheriting:
    case ActorIsolation::NonisolatedUnsafe:
    case ActorIsolation::Unspecified:
      return true;

    case ActorIsolation::Erased:
      llvm_unreachable("declaration cannot have erased isolation");

    case ActorIsolation::GlobalActor:
      return false;
    }
  }

  // 'let' declarations are immutable, so some of them can be accessed across
  // actors.
  if (auto var = dyn_cast<VarDecl>(value)) {
    return varIsSafeAcrossActors(fromDC->getParentModule(), var, isolation,
                                 actorInstance, options);
  }

  return false;
}

bool swift::isAccessibleAcrossActors(
    ValueDecl *value, const ActorIsolation &isolation,
    const DeclContext *fromDC, std::optional<ReferencedActor> actorInstance) {
  ActorReferenceResult::Options options = std::nullopt;
  return isAccessibleAcrossActors(
      value, isolation, fromDC, options, actorInstance);
}

ActorReferenceResult ActorReferenceResult::forSameConcurrencyDomain(
    ActorIsolation isolation, Options options) {
  return ActorReferenceResult{SameConcurrencyDomain, options, isolation};
}

ActorReferenceResult ActorReferenceResult::forEntersActor(
    ActorIsolation isolation, Options options) {
  return ActorReferenceResult{EntersActor, options, isolation};
}

ActorReferenceResult ActorReferenceResult::forExitsActorToNonisolated(
    ActorIsolation isolation, Options options) {
  return ActorReferenceResult{ExitsActorToNonisolated, options, isolation};
}

// Determine if two actor isolation contexts are considered to be equivalent.
static bool equivalentIsolationContexts(
    const ActorIsolation &lhs, const ActorIsolation &rhs) {
  if (lhs == rhs)
    return true;

  if (lhs == ActorIsolation::ActorInstance &&
      rhs == ActorIsolation::ActorInstance &&
      lhs.isDistributedActor() == rhs.isDistributedActor())
    return true;

  return false;
}

ActorReferenceResult ActorReferenceResult::forReference(
    ConcreteDeclRef declRef, SourceLoc declRefLoc, const DeclContext *fromDC,
    std::optional<VarRefUseEnv> useKind,
    std::optional<ReferencedActor> actorInstance,
    std::optional<ActorIsolation> knownDeclIsolation,
    std::optional<ActorIsolation> knownContextIsolation,
    llvm::function_ref<ActorIsolation(AbstractClosureExpr *)>
        getClosureActorIsolation) {
  auto *const decl = declRef.getDecl();

  // If not provided, compute the isolation of the declaration, adjusted
  // for references.
  ActorIsolation declIsolation = ActorIsolation::forUnspecified();
  if (knownDeclIsolation) {
    declIsolation = *knownDeclIsolation;
  } else {
    declIsolation = getActorIsolationForReference(decl, fromDC);
    if (declIsolation.requiresSubstitution())
      declIsolation = declIsolation.subst(declRef.getSubstitutions());
  }

  // Determine what adjustments we need to perform for cross-actor
  // references.
  Options options = std::nullopt;

  // FIXME: Actor constructors are modeled as isolated to the actor
  // so that Sendable checking is applied to their arguments, but the
  // call itself does not hop to another executor.
  if (auto ctor = dyn_cast<ConstructorDecl>(decl)) {
    if (auto nominal = ctor->getDeclContext()->getSelfNominalTypeDecl()) {
      if (nominal->isAnyActor())
        options |= Flags::OnlyArgsCrossIsolation;
    }
  }

  // If the entity we are referencing is not a value, we're in the same
  // concurrency domain.
  if (isNonValueReference(decl))
    return forSameConcurrencyDomain(declIsolation, options);

  // Compute the isolation of the context, if not provided.
  ActorIsolation contextIsolation = ActorIsolation::forUnspecified();
  if (knownContextIsolation) {
    contextIsolation = *knownContextIsolation;
  } else {
    contextIsolation =
        getInnermostIsolatedContext(fromDC, getClosureActorIsolation);
  }

  if (declIsolation.isCallerIsolationInheriting())
    return forSameConcurrencyDomain(declIsolation, options);

  // When the declaration is not actor-isolated, it can always be accessed
  // directly.
  if (!declIsolation.isActorIsolated()) {
    // If the declaration is asynchronous and we are in an actor-isolated
    // context (of any kind), then we exit the actor to the nonisolated context.
    if (decl->isAsync() && contextIsolation.isActorIsolated() &&
        !declRef.getDecl()
             ->getAttrs()
             .hasAttribute<UnsafeInheritExecutorAttr>())
      return forExitsActorToNonisolated(contextIsolation, options);

    // Otherwise, we stay in the same concurrency domain, whether on an actor
    // or in a task.
    return forSameConcurrencyDomain(declIsolation, options);
  }

  // The declaration we are accessing is actor-isolated. First, check whether
  // we are on the same actor already.
  if (actorInstance && declIsolation == ActorIsolation::ActorInstance &&
      declIsolation.isActorInstanceForSelfParameter()) {
    // If this instance is isolated, we're in the same concurrency domain.
    if (actorInstance->isIsolated())
      return forSameConcurrencyDomain(declIsolation, options);
  } else if (equivalentIsolationContexts(declIsolation, contextIsolation)) {
    // The context isolation matches, so we are in the same concurrency
    // domain.
    return forSameConcurrencyDomain(declIsolation, options);
  }

  // Initializing an actor-isolated stored property with a value effectively
  // passes that value from the init context into the actor-isolated context.
  // It's only okay for the value to cross isolation boundaries if the property
  // type is Sendable. Note that if the init is a nonisolated actor init,
  // Sendable checking is already performed on arguments at the call-site.
  if ((declIsolation.isActorIsolated() && contextIsolation.isGlobalActor()) ||
      declIsolation.isGlobalActor()) {
    auto *init = dyn_cast<ConstructorDecl>(fromDC);
    if (init && init->isDesignatedInit() && isStoredProperty(decl) &&
        (!actorInstance || actorInstance->isSelf())) {
      auto type = fromDC->mapTypeIntoContext(decl->getInterfaceType());
      if (!type->isSendableType()) {
        // Treat the decl isolation as 'preconcurrency' to downgrade violations
        // to warnings, because violating Sendable here is accepted by the
        // Swift 5.9 compiler.
        options |= Flags::Preconcurrency;
        return forEntersActor(declIsolation, options);
      }
    }
  }

  // If there is an instance and it is checked by flow isolation, treat it
  // as being in the same concurrency domain.
  if (actorInstance &&
      checkedByFlowIsolation(fromDC, *actorInstance, decl, declRefLoc, useKind))
    return forSameConcurrencyDomain(declIsolation, options);

  // If we are delegating to another initializer, treat them as being in the
  // same concurrency domain.
  // FIXME: This has a lot of overlap with both the stored-property checks
  // below and the flow-isolation checks above.
  if (actorInstance && actorInstance->isSelf() && isa<ConstructorDecl>(decl) &&
      isa<ConstructorDecl>(fromDC))
    return forSameConcurrencyDomain(declIsolation, options);

  // If there is an instance that corresponds to 'self',
  // we are in a constructor or destructor, and we have a stored property of
  // global-actor-qualified type, then we have problems if the stored property
  // type is non-Sendable. Note that if we get here, the type must be Sendable.
  if (actorInstance && actorInstance->isSelf() &&
      isNonInheritedStorage(decl, fromDC) && declIsolation.isGlobalActor() &&
      (isa<ConstructorDecl>(fromDC) || isa<DestructorDecl>(fromDC)))
    return forSameConcurrencyDomain(declIsolation, options);

  // At this point, we are accessing the target from outside the actor.
  // First, check whether it is something that can be accessed directly,
  // without any kind of promotion.
  if (isAccessibleAcrossActors(decl, declIsolation, fromDC, options,
                               actorInstance))
    return forEntersActor(declIsolation, options);

  // This is a cross-actor reference.

  // Note if the reference originates from a @preconcurrency-isolated context.
  if (contextIsolation.preconcurrency() || declIsolation.preconcurrency())
    options |= Flags::Preconcurrency;

  // If the declaration isn't asynchronous, promote to async.
  if (!decl->isAsync())
    options |= Flags::AsyncPromotion;

  // If the declaration is isolated to a distributed actor and we are not
  // guaranteed to be on the same node, make adjustments distributed
  // access.
  if (declIsolation.isDistributedActor()) {
    bool needsDistributed;
    if (actorInstance)
      needsDistributed = !actorInstance->isKnownToBeLocal();
    else
      needsDistributed = !contextIsolation.isDistributedActor();

    if (needsDistributed) {
      options |= Flags::Distributed;

      if (!isThrowsDecl(declRef))
        options |= Flags::ThrowsPromotion;
    }
  }

  return forEntersActor(declIsolation, options);
}

bool swift::diagnoseNonSendableFromDeinit(
    SourceLoc refLoc, VarDecl *var, DeclContext *dc) {
  return diagnoseIfAnyNonSendableTypes(
      var->getTypeInContext(), SendableCheckContext(dc), Type(), SourceLoc(),
      refLoc, diag::non_sendable_from_deinit, var);
}

std::optional<ActorIsolation> ProtocolConformance::getRawIsolation() const {
  // Only normal protocol conformances can be isolated.
  auto rootNormal =
      dyn_cast<NormalProtocolConformance>(this->getRootConformance());
  if (!rootNormal)
    return ActorIsolation::forNonisolated(false);

  if (this != rootNormal)
    return rootNormal->getRawIsolation();

  ASTContext &ctx = getDeclContext()->getASTContext();
  auto conformance = const_cast<NormalProtocolConformance *>(rootNormal);
  return evaluateOrDefault(
      ctx.evaluator, RawConformanceIsolationRequest{conformance},
      ActorIsolation());
}

ActorIsolation ProtocolConformance::getIsolation() const {
  // Only normal protocol conformances can be isolated.
  auto rootNormal =
      dyn_cast<NormalProtocolConformance>(this->getRootConformance());
  if (!rootNormal)
    return ActorIsolation::forNonisolated(false);

  if (this != rootNormal)
    return rootNormal->getIsolation();

  ASTContext &ctx = getDeclContext()->getASTContext();
  auto conformance = const_cast<NormalProtocolConformance *>(rootNormal);
  return evaluateOrDefault(
      ctx.evaluator, ConformanceIsolationRequest{conformance},
      ActorIsolation());
}

std::optional<ActorIsolation>
RawConformanceIsolationRequest::evaluate(
    Evaluator &evaluator, NormalProtocolConformance *conformance
) const {
  // If the conformance is explicitly non-isolated, report that.
  if (conformance->getOptions().contains(ProtocolConformanceFlags::Nonisolated))
    return ActorIsolation::forNonisolated(false);

  // If there is an explicitly-specified global actor on the isolation,
  // resolve it and report it.
  if (auto globalActorTypeExpr = conformance->getExplicitGlobalActorIsolation()) {
    // If we don't already have a resolved global actor type, resolve it now.
    Type globalActorType = globalActorTypeExpr->getInstanceType();
    if (!globalActorType) {
      const auto resolution = TypeResolution::forInterface(
          conformance->getDeclContext(), std::nullopt,
          /*unboundTyOpener*/ nullptr,
          /*placeholderHandler*/ nullptr,
          /*packElementOpener*/ nullptr);
      globalActorType = resolution.resolveType(globalActorTypeExpr->getTypeRepr());
      if (!globalActorType)
        return ActorIsolation::forNonisolated(false);

      // Cache the resolved type.
      globalActorTypeExpr->setType(MetatypeType::get(globalActorType));
    }

    // FIXME: Make sure the type actually is a global actor type, map it into
    // context, etc.

    return ActorIsolation::forGlobalActor(globalActorType);
  }

  auto dc = conformance->getDeclContext();
  ASTContext &ctx = dc->getASTContext();
  auto proto = conformance->getProtocol();

  // If the protocol itself is isolated, don't infer isolation for the
  // conformance.
  if (getActorIsolation(proto).isActorIsolated())
    return ActorIsolation::forNonisolated(false);

  // SendableMetatype disables isolation inference.
  auto sendableMetatypeProto =
      ctx.getProtocol(KnownProtocolKind::SendableMetatype);
  if (sendableMetatypeProto && proto->inheritsFrom(sendableMetatypeProto))
    return ActorIsolation::forNonisolated(false);

  // @preconcurrency disables isolation inference.
  if (conformance->isPreconcurrency())
    return ActorIsolation::forNonisolated(false);

  return std::nullopt;
}

ActorIsolation swift::inferConformanceIsolation(
    NormalProtocolConformance *conformance, bool hasKnownIsolatedWitness) {
  auto dc = conformance->getDeclContext();
  ASTContext &ctx = dc->getASTContext();

  // If we aren't inferring isolated conformances, we're done.
  if (!ctx.LangOpts.hasFeature(Feature::InferIsolatedConformances))
    return ActorIsolation::forNonisolated(false);

  auto nominal = dc->getSelfNominalTypeDecl();
  if (!nominal) {
    return ActorIsolation::forNonisolated(false);
  }

  // If we are inferring isolated conformances and the conforming type is
  // isolated to a global actor, we may use the conforming type's isolation.
  auto nominalIsolation = getActorIsolation(nominal);
  if (!nominalIsolation.isGlobalActor()) {
    return ActorIsolation::forNonisolated(false);
  }

  // If all of the value witnesses are nonisolated, then we should not infer
  // global actor isolation.
  if (hasKnownIsolatedWitness) {
    // The caller told us we have an isolated value witness, so infer
    // the nominal isolation.
    return nominalIsolation;
  }

  auto protocol = conformance->getProtocol();

  // Also check the value witnesses of each implied conformance to every
  // inherited protocol, recursively.
  for (auto req : protocol->getRequirementSignature().getRequirements()) {
    if (req.getKind() != RequirementKind::Conformance ||
        !req.getFirstType()->isEqual(ctx.TheSelfType))
      continue;

    auto *assocConf = conformance->getAssociatedConformance(
        req.getFirstType(), req.getProtocolDecl()).getConcrete();
    auto isolation = assocConf->getIsolation();
    if (isolation.isGlobalActor())
      return isolation;
  }

  bool anyIsolatedWitness = false;
  for (auto requirement : protocol->getProtocolRequirements()) {
    if (isa<TypeDecl>(requirement))
      continue;

    auto valueReq = cast<ValueDecl>(requirement);
    auto witness = conformance->getWitnessDecl(valueReq);
    if (!witness)
      continue;

    auto witnessIsolation = getActorIsolation(witness);
    if (witnessIsolation.isActorIsolated()) {
      anyIsolatedWitness = true;
      break;
    }
  }

  if (!anyIsolatedWitness) {
    return ActorIsolation::forNonisolated(false);
  }

  return nominalIsolation;
}

ActorIsolation
ConformanceIsolationRequest::evaluate(
    Evaluator &evaluator, NormalProtocolConformance *conformance
) const {
  // If there is raw isolation, use that.
  if (auto rawIsolation = conformance->getRawIsolation())
    return *rawIsolation;

  // Otherwise, we may infer isolation.
  return inferConformanceIsolation(
      conformance, /*hasKnownIsolatedWitness=*/false);
}

namespace {
  /// Identifies isolated conformances whose isolation differs from the
  /// context's isolation.
  class MismatchedIsolatedConformances {
    llvm::TinyPtrVector<ProtocolConformance *> badIsolatedConformances;
    DeclContext *fromDC;
    HandleConformanceIsolationFn handleBad;
    mutable std::optional<ActorIsolation> fromIsolation;

  public:
    MismatchedIsolatedConformances(const DeclContext *fromDC,
                                   HandleConformanceIsolationFn handleBad)
      : fromDC(const_cast<DeclContext *>(fromDC)),
        handleBad(handleBad) { }

    ActorIsolation getContextIsolation() const {
      if (!fromIsolation)
        fromIsolation = getActorIsolationOfContext(fromDC);

      return *fromIsolation;
    }

    ArrayRef<ProtocolConformance *> getBadIsolatedConformances() const {
      return badIsolatedConformances;
    }

    explicit operator bool() const { return !badIsolatedConformances.empty(); }

    bool operator()(ProtocolConformanceRef conformance) {
      if (conformance.isAbstract() || conformance.isPack())
        return false;

      auto concrete = conformance.getConcrete();
      auto normal = dyn_cast<NormalProtocolConformance>(
          concrete->getRootConformance());
      if (!normal)
        return false;

      auto conformanceIsolation = concrete->getIsolation();
      if (!conformanceIsolation.isGlobalActor() ||
          conformanceIsolation == getContextIsolation())
        return true;

      badIsolatedConformances.push_back(concrete);
      return false;
    }

    /// If there were any bad isolated conformances, diagnose them and return
    /// true. Otherwise, returns false.
    bool diagnose(SourceLoc loc) const {
      if (badIsolatedConformances.empty())
        return false;

      if (handleBad) {
        // Capture all of the actor isolations from the conformances.
        std::vector<ActorIsolation> badIsolations;
        for (auto conformance : badIsolatedConformances)
          badIsolations.push_back(conformance->getIsolation());

        if (handleBad(badIsolations))
          return false;
      }

      ASTContext &ctx = fromDC->getASTContext();
      auto firstConformance = badIsolatedConformances.front();
      ctx.Diags.diagnose(
          loc, diag::isolated_conformance_wrong_domain,
          firstConformance->getIsolation(),
          firstConformance->getType(),
          firstConformance->getProtocol()->getName(),
          getContextIsolation())
        .warnUntilSwiftVersion(6);
      return true;
    }
  };

}

bool swift::doNotDiagnoseConformanceIsolation(ArrayRef<ActorIsolation>) {
  return false;
}

bool swift::checkIsolatedConformancesInContext(
    ConcreteDeclRef declRef, SourceLoc loc, const DeclContext *dc,
    HandleConformanceIsolationFn handleBad) {
  MismatchedIsolatedConformances mismatched(dc, handleBad);
  forEachConformance(declRef, mismatched);
  return mismatched.diagnose(loc);
}

bool swift::checkIsolatedConformancesInContext(
    ArrayRef<ProtocolConformanceRef> conformances, SourceLoc loc,
    const DeclContext *dc,
    HandleConformanceIsolationFn handleBad) {
  MismatchedIsolatedConformances mismatched(dc, handleBad);
  for (auto conformance: conformances)
    forEachConformance(conformance, mismatched);
  return mismatched.diagnose(loc);
}

bool swift::checkIsolatedConformancesInContext(
    SubstitutionMap subs, SourceLoc loc, const DeclContext *dc,
    HandleConformanceIsolationFn handleBad) {
  MismatchedIsolatedConformances mismatched(dc, handleBad);
  forEachConformance(subs, mismatched);
  return mismatched.diagnose(loc);
}

bool swift::checkIsolatedConformancesInContext(
    Type type, SourceLoc loc, const DeclContext *dc,
    HandleConformanceIsolationFn handleBad) {
  MismatchedIsolatedConformances mismatched(dc, handleBad);
  forEachConformance(type, mismatched);
  return mismatched.diagnose(loc);
}
