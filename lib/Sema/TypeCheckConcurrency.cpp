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
#include "swift/AST/TypeVisitor.h"

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
  if (!isa<DestructorDecl>(func)) {
    auto note =
        func->diagnose(diag::note_add_async_to_function, func->getName());

    if (func->hasThrows()) {
      auto replacement = func->getAttrs().hasAttribute<RethrowsAttr>()
                        ? "async rethrows"
                        : "async throws";

      note.fixItReplace(SourceRange(func->getThrowsLoc()), replacement);

    } else {
      note.fixItInsert(func->getParameters()->getRParenLoc().getAdvancedLoc(1),
                       " async");
    }
  }

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
  bool isExplicitActor = classDecl->isExplicitActor() ||
    classDecl->getAttrs().getAttribute<ActorAttr>();

  // If there is a superclass, we can infer actor-ness from it.
  if (auto superclassDecl = classDecl->getSuperclassDecl()) {
    // The superclass is an actor, so we are, too.
    if (superclassDecl->isActor())
      return true;

    // The superclass is 'NSObject', which is known to have no state and no
    // superclass.
    if (superclassDecl->isNSObject() && isExplicitActor)
      return true;

    // This class cannot be an actor; complain if the 'actor' modifier was
    // provided.
    if (isExplicitActor) {
      classDecl->diagnose(diag::actor_with_nonactor_superclass,
                          superclassDecl->getName())
        .highlight(classDecl->getStartLoc());
    }

    return false;
  }

  return isExplicitActor;
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
  case DeclKind::Accessor:
  case DeclKind::Constructor:
  case DeclKind::Func:
  case DeclKind::Subscript: {
    // Local captures are checked separately.
    if (cast<ValueDecl>(decl)->isLocalCapture()) {
      return forLocalCapture(decl->getDeclContext());
    }

    // 'let' declarations are immutable, so they can be accessed across
    // actors.
    bool isAccessibleAcrossActors = false;
    if (auto var = dyn_cast<VarDecl>(decl)) {
      if (var->isLet())
        isAccessibleAcrossActors = true;
    }

    // A function that provides an asynchronous context has no restrictions
    // on its access.
    if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
      if (func->isAsyncContext())
        isAccessibleAcrossActors = true;
    }

    // Determine the actor isolation of the given declaration.
    switch (auto isolation = getActorIsolation(cast<ValueDecl>(decl))) {
    case ActorIsolation::ActorInstance:
      // Protected actor instance members can only be accessed on 'self'.
      return forActorSelf(isolation.getActor(),
          isAccessibleAcrossActors || isa<ConstructorDecl>(decl));

    case ActorIsolation::GlobalActor: {
      Type actorType = isolation.getGlobalActor();
      if (auto subs = declRef.getSubstitutions())
        actorType = actorType.subst(subs);

      return forGlobalActor(actorType, isAccessibleAcrossActors);
    }

    case ActorIsolation::Independent:
    case ActorIsolation::IndependentUnsafe:
      // Actor-independent have no restrictions on their access.
      return forUnrestricted();

    case ActorIsolation::Unspecified:
      return isAccessibleAcrossActors ? forUnrestricted() : forUnsafe();
    }
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

/// Determine whether this closure is escaping.
static bool isConcurrentClosure(const AbstractClosureExpr *closure) {
  if (auto type = closure->getType()) {
    if (auto fnType = type->getAs<AnyFunctionType>())
      return fnType->isConcurrent();
  }

  return false;
}

/// Determine whether the given type is suitable as a concurrent value type.
static bool isConcurrentValueType(const DeclContext *dc, Type type) {
  class IsConcurrentValue : public TypeVisitor<IsConcurrentValue, bool> {
    DeclContext *dc;
    ProtocolDecl *concurrentValueProto;

  public:
    IsConcurrentValue(const DeclContext *dc)
        : dc(const_cast<DeclContext *>(dc)) {
      concurrentValueProto = dc->getASTContext().getProtocol(
          KnownProtocolKind::ConcurrentValue);
    }

#define ALWAYS_CONCURRENT_VALUE(Id) \
    bool visit##Id##Type(Id##Type *) { return true; }

#define UNEXPECTED_TYPE(Id) ALWAYS_CONCURRENT_VALUE(Id)

    ALWAYS_CONCURRENT_VALUE(Error)
    ALWAYS_CONCURRENT_VALUE(Builtin)
    ALWAYS_CONCURRENT_VALUE(AnyMetatype)
    ALWAYS_CONCURRENT_VALUE(Module)

    UNEXPECTED_TYPE(GenericTypeParam)
    UNEXPECTED_TYPE(DependentMember)
    UNEXPECTED_TYPE(GenericFunction)

#define TYPE(Id, Parent)

 // Look through type sugar.
#define SUGARED_TYPE(Id, Parent)                    \
    bool visit##Id##Type(Id##Type *type) {          \
      return visit(type->getSinglyDesugaredType()); \
    }

// Unchecked and artificial types won't show up in well-formed code,
// but don't trip over them.
#define UNCHECKED_TYPE(Id, Parent) UNEXPECTED_TYPE(Id)
#define ARTIFICIAL_TYPE(Id, Parent) UNEXPECTED_TYPE(Id)

#include "swift/AST/TypeNodes.def"

#undef UNEXPECTED_TYPE
#undef ALWAYS_CONCURRENT_VALUE

    bool visitForConformanceCheck(TypeBase *type) {
      if (!concurrentValueProto)
        return true;

      return !TypeChecker::conformsToProtocol(
          Type(type), concurrentValueProto, dc).isInvalid();
    }

    bool visitTupleType(TupleType *type) {
      for (Type elementType : type->getElementTypes()) {
        if (!visit(elementType))
          return false;
      }

      return true;
    }

    bool visitReferenceStorageType(ReferenceStorageType *type) {
      return visit(type->getReferentType());
    }

    bool visitEnumType(EnumType *type) {
      return visitForConformanceCheck(type);
    }

    bool visitStructType(StructType *type) {
      return visitForConformanceCheck(type);
    }

    bool visitClassType(ClassType *type) {
      return visitForConformanceCheck(type);
    }

    bool visitProtocolType(ProtocolType *type) {
      if (!concurrentValueProto)
        return true;

      return !TypeChecker::containsProtocol(
        Type(type), concurrentValueProto, dc).isInvalid();
    }

    bool visitBoundGenericType(BoundGenericType *type) {
      return visitForConformanceCheck(type);
    }

    bool visitDynamicSelfType(DynamicSelfType *type) {
      return visit(type->getSelfType());
    }

    bool visitArchetypeType(ArchetypeType *type) {
      return visitForConformanceCheck(type);
    }

    bool visitFunctionType(FunctionType *type) {
      // Concurrent function types meet the requirements.
      return type->isConcurrent();
    }

    bool visitProtocolCompositionType(ProtocolCompositionType *type) {
      if (!concurrentValueProto)
        return true;

      return !TypeChecker::containsProtocol(type, concurrentValueProto, dc)
        .isInvalid();
    }

    bool visitLValueType(LValueType *type) {
      return visit(type->getObjectType());
    }

    bool visitInOutType(InOutType *type) {
      return visit(type->getObjectType());
    }
  } checker(dc);

  return checker.visit(type);
}

Optional<NonConcurrentType> NonConcurrentType::get(
    const DeclContext *dc, ConcreteDeclRef declRef) {
  // For functions, check the parameter and result types.
  SubstitutionMap subs = declRef.getSubstitutions();
  if (auto function = dyn_cast<AbstractFunctionDecl>(declRef.getDecl())) {
    for (auto param : *function->getParameters()) {
      Type paramType = param->getInterfaceType().subst(subs);
      if (!isConcurrentValueType(dc, paramType)) {
        return NonConcurrentType {
            Kind::Parameter, ConcreteDeclRef(param, subs), paramType };
      }
    }

    // Check the result type of a function.
    if (auto func = dyn_cast<FuncDecl>(function)) {
      Type resultType = func->getResultInterfaceType().subst(subs);
      if (!isConcurrentValueType(dc, resultType)) {
        return NonConcurrentType { Kind::Result, declRef, resultType };
      }
    }

    // Check the "self" type of an instance method.
    if (function->isInstanceMember()) {
      if (auto selfParam = function->getImplicitSelfDecl()) {
        Type paramType = selfParam->getInterfaceType().subst(subs);
        if (!isConcurrentValueType(dc, paramType)) {
          return NonConcurrentType {
              Kind::Parameter, ConcreteDeclRef(selfParam, subs),
              paramType };
        }
      }
    }
  } else if (auto var = dyn_cast<VarDecl>(declRef.getDecl())) {
    Type propertyType = var->getValueInterfaceType().subst(subs);
    if (!isConcurrentValueType(dc, propertyType)) {
      return NonConcurrentType {
        Kind::Property, declRef, propertyType };
    }
  }

  return None;
}

void NonConcurrentType::diagnose(SourceLoc loc) {
  ASTContext &ctx = declRef.getDecl()->getASTContext();

  switch (kind) {
  case Parameter:
    ctx.Diags.diagnose(loc, diag::non_concurrent_param_type, type);
    break;

  case Result:
    ctx.Diags.diagnose(loc, diag::non_concurrent_result_type, type);
    break;

  case Property: {
    auto var = cast<VarDecl>(declRef.getDecl());
    ctx.Diags.diagnose(loc, diag::non_concurrent_property_type,
                       var->getDescriptiveKind(), var->getName(),
                       type, var->isLocalCapture());
    break;
  }
  }
}

static bool diagnoseNonConcurrentParameter(
    SourceLoc loc, ConcurrentReferenceKind refKind, ConcreteDeclRef declRef,
    ParamDecl *param, Type paramType) {
  ASTContext &ctx = declRef.getDecl()->getASTContext();
  ctx.Diags.diagnose(loc, diag::non_concurrent_param_type, paramType);
  return false;
}

static bool diagnoseNonConcurrentResult(
    SourceLoc loc, ConcurrentReferenceKind refKind, ConcreteDeclRef declRef,
    Type resultType) {
  ASTContext &ctx = declRef.getDecl()->getASTContext();
  ctx.Diags.diagnose(loc, diag::non_concurrent_result_type, resultType);
  return false;
}

static bool diagnoseNonConcurrentProperty(
    SourceLoc loc, ConcurrentReferenceKind refKind, VarDecl *var,
    Type propertyType) {
  ASTContext &ctx = var->getASTContext();
  ctx.Diags.diagnose(loc, diag::non_concurrent_property_type,
                     var->getDescriptiveKind(), var->getName(),
                     propertyType, var->isLocalCapture());
  return false;
}

bool swift::diagnoseNonConcurrentTypesInReference(
    ConcreteDeclRef declRef, const DeclContext *dc, SourceLoc loc,
    ConcurrentReferenceKind refKind) {
  // Bail out immediately if we aren't supposed to do this checking.
  if (!dc->getASTContext().LangOpts.EnableExperimentalConcurrentValueChecking)
    return false;

  // For functions, check the parameter and result types.
  SubstitutionMap subs = declRef.getSubstitutions();
  if (auto function = dyn_cast<AbstractFunctionDecl>(declRef.getDecl())) {
    for (auto param : *function->getParameters()) {
      Type paramType = param->getInterfaceType().subst(subs);
      if (!isConcurrentValueType(dc, paramType)) {
        return diagnoseNonConcurrentParameter(
            loc, refKind, declRef, param, paramType);
      }
    }

    // Check the result type of a function.
    if (auto func = dyn_cast<FuncDecl>(function)) {
      Type resultType = func->getResultInterfaceType().subst(subs);
      if (!isConcurrentValueType(dc, resultType)) {
        return diagnoseNonConcurrentResult(loc, refKind, declRef, resultType);
      }
    }

    return false;
  }

  if (auto var = dyn_cast<VarDecl>(declRef.getDecl())) {
    Type propertyType = var->isLocalCapture()
        ? var->getType()
        : var->getValueInterfaceType().subst(subs);
    if (!isConcurrentValueType(dc, propertyType)) {
      return diagnoseNonConcurrentProperty(loc, refKind, var, propertyType);
    }
  }

  if (auto subscript = dyn_cast<SubscriptDecl>(declRef.getDecl())) {
    for (auto param : *subscript->getIndices()) {
      Type paramType = param->getInterfaceType().subst(subs);
      if (!isConcurrentValueType(dc, paramType)) {
        return diagnoseNonConcurrentParameter(
            loc, refKind, declRef, param, paramType);
      }
    }

    // Check the element type of a subscript.
    Type resultType = subscript->getElementInterfaceType().subst(subs);
    if (!isConcurrentValueType(dc, resultType)) {
      return diagnoseNonConcurrentResult(loc, refKind, declRef, resultType);
    }

    return false;
  }

  return false;
}

namespace {
  /// Check for adherence to the actor isolation rules, emitting errors
  /// when actor-isolated declarations are used in an unsafe manner.
  class ActorIsolationChecker : public ASTWalker {
    ASTContext &ctx;
    SmallVector<const DeclContext *, 4> contextStack;
    SmallVector<ApplyExpr*, 4> applyStack;

    using MutableVarSource = llvm::PointerUnion<DeclRefExpr *, InOutExpr *>;
    using MutableVarParent = llvm::PointerUnion<InOutExpr *, LoadExpr *>;

    /// Mapping from mutable local variables or inout expressions to the
    /// parent expression, when that parent is either a load or a inout expression.
    llvm::SmallDenseMap<MutableVarSource, MutableVarParent, 4>
      mutableLocalVarParent;

    const DeclContext *getDeclContext() const {
      return contextStack.back();
    }

    /// Determine whether code in the given use context might execute
    /// concurrently with code in the definition context.
    bool mayExecuteConcurrentlyWith(
        const DeclContext *useContext, const DeclContext *defContext);

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
        // optimization, because the parent map won't be queried in this case, and
        // it is the most common case for variables to be referenced in their
        // own context.
        if (var->getDeclContext() == getDeclContext())
          return false;

        assert(mutableLocalVarParent[declRef].isNull());
        mutableLocalVarParent[declRef] = parent;
        return true;
      }

      // For a member reference, try to record a parent for the base
      // expression.
      if (auto memberRef = dyn_cast<MemberRefExpr>(subExpr)) {
        return recordMutableVarParent(parent, memberRef->getBase());
      }

      // For a subscript, try to record a parent for the base expression.
      if (auto subscript = dyn_cast<SubscriptExpr>(subExpr)) {
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

      return false;
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

        if (mutableLocalVarParent.count(inout) == 0)
          recordMutableVarParent(inout, inout->getSubExpr());
      }

      if (auto load = dyn_cast<LoadExpr>(expr)) {
        recordMutableVarParent(load, load->getSubExpr());
      }

      if (auto lookup = dyn_cast<LookupExpr>(expr)) {
        checkMemberReference(lookup->getBase(), lookup->getMember(),
                             lookup->getLoc());
        return { true, expr };
      }

      if (auto declRef = dyn_cast<DeclRefExpr>(expr)) {
        checkNonMemberReference(
            declRef->getDeclRef(), declRef->getLoc(), declRef);
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

      // The children of #selector expressions are not evaluated, so we do not
      // need to do isolation checking there. This is convenient because such
      // expressions tend to violate restrictions on the use of instance
      // methods.
      if (isa<ObjCSelectorExpr>(expr))
        return { false, expr };

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

      // Clear out the mutable local variable parent map on the way out.
      if (auto *declRefExpr = dyn_cast<DeclRefExpr>(expr)) {
        mutableLocalVarParent.erase(declRefExpr);
      }
      if (auto *inoutExpr = dyn_cast<InOutExpr>(expr)) {
        mutableLocalVarParent.erase(inoutExpr);
      }

      return expr;
    }

  private:
    /// If the expression is a reference to `self`, the `self` declaration.
    static VarDecl *getReferencedSelf(Expr *expr) {
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
        if (auto var = dyn_cast<VarDecl>(declRef->getDecl())) {
          if (var->isSelfParameter() || var->isSelfParamCapture())
            return var;
        }
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

      bool result = false;
      auto checkDiagnostic = [this, call, isPartialApply,
                              &result](ValueDecl *decl, SourceLoc argLoc) {
        auto isolation = ActorIsolationRestriction::forDeclaration(decl);
        switch (isolation) {
        case ActorIsolationRestriction::Unrestricted:
        case ActorIsolationRestriction::LocalCapture:
        case ActorIsolationRestriction::Unsafe:
          break;
        case ActorIsolationRestriction::CrossGlobalActor:
        case ActorIsolationRestriction::GlobalActor: {
          ctx.Diags.diagnose(argLoc, diag::actor_isolated_inout_state,
                             decl->getDescriptiveKind(), decl->getName(),
                             call->implicitlyAsync());
          decl->diagnose(diag::kind_declared_here, decl->getDescriptiveKind());
          result = true;
          break;
        }
        case ActorIsolationRestriction::CrossActorSelf:
        case ActorIsolationRestriction::ActorSelf: {
          if (isPartialApply) {
            // The partially applied InoutArg is a property of actor. This
            // can really only happen when the property is a struct with a
            // mutating async method.
            if (auto partialApply = dyn_cast<ApplyExpr>(call->getFn())) {
              ValueDecl *fnDecl =
                  cast<DeclRefExpr>(partialApply->getFn())->getDecl();
              ctx.Diags.diagnose(call->getLoc(),
                                 diag::actor_isolated_mutating_func,
                                 fnDecl->getName(), decl->getDescriptiveKind(),
                                 decl->getName());
              result = true;
            }
          } else {
            ctx.Diags.diagnose(argLoc, diag::actor_isolated_inout_state,
                               decl->getDescriptiveKind(), decl->getName(),
                               call->implicitlyAsync());
            result = true;
          }
          break;
        }
        }
      };
      auto expressionWalker = [baseArg = arg->getSubExpr(),
                               checkDiagnostic](Expr *expr) -> Expr * {
        if (isa<InOutExpr>(expr))
          return nullptr; // AST walker will hit this again
        if (LookupExpr *lookup = dyn_cast<LookupExpr>(expr)) {
          if (isa<DeclRefExpr>(lookup->getBase())) {
            checkDiagnostic(lookup->getMember().getDecl(), baseArg->getLoc());
            return nullptr; // Diagnosed. Don't keep walking
          }
        }
        if (DeclRefExpr *declRef = dyn_cast<DeclRefExpr>(expr)) {
          checkDiagnostic(declRef->getDecl(), baseArg->getLoc());
          return nullptr; // Diagnosed. Don't keep walking
        }
        return expr;
      };
      arg->getSubExpr()->forEachChildExpr(expressionWalker);
      return result;
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
        ConcreteDeclRef valueRef, SourceLoc loc, Type globalActor,
        bool isCrossActor) {
      ValueDecl *value = valueRef.getDecl();

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

            // Check for non-concurrent types.
            (void)diagnoseNonConcurrentTypesInReference(
                valueRef, getDeclContext(), loc,
                ConcurrentReferenceKind::SynchronousAsAsyncCall);

            return true;
          }
        }

        return false;
      };

      auto declContext = getDeclContext();

      // Check whether we are within the same isolation context, in which
      // case there is nothing further to check,
      auto contextIsolation = getInnermostIsolatedContext(declContext);
      if (contextIsolation == ActorIsolation::forGlobalActor(globalActor)) {
        return false;
      }

      // A cross-actor access requires types to be concurrent-safe.
      if (isCrossActor) {
        return diagnoseNonConcurrentTypesInReference(
            valueRef, getDeclContext(), loc,
            ConcurrentReferenceKind::CrossActor);
      }

      switch (contextIsolation) {
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
        // Check if this decl reference is the callee of the enclosing Apply,
        // making it OK as an implicitly async call.
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

      case ActorIsolation::IndependentUnsafe:
        // Allow unrestricted use of something in a global actor.
        return false;

      case ActorIsolation::Independent:
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
    bool checkNonMemberReference(
        ConcreteDeclRef valueRef, SourceLoc loc, DeclRefExpr *declRefExpr) {
      if (!valueRef)
        return false;

      auto value = valueRef.getDecl();
      switch (auto isolation =
                  ActorIsolationRestriction::forDeclaration(valueRef)) {
      case ActorIsolationRestriction::Unrestricted:
        return false;

      case ActorIsolationRestriction::CrossActorSelf:
      case ActorIsolationRestriction::ActorSelf:
        llvm_unreachable("non-member reference into an actor");

      case ActorIsolationRestriction::CrossGlobalActor:
      case ActorIsolationRestriction::GlobalActor:
        return checkGlobalActorReference(
            valueRef, loc, isolation.getGlobalActor(),
            isolation == ActorIsolationRestriction::CrossGlobalActor);

      case ActorIsolationRestriction::LocalCapture:
        // Check whether we are in a context that will not execute concurrently
        // with the context of 'self'. If not, it's safe.
        if (!mayExecuteConcurrentlyWith(
                getDeclContext(), isolation.getLocalContext()))
          return false;

        // Check whether this is a local variable, in which case we can
        // determine whether it was safe to access concurrently.
        if (auto var = dyn_cast<VarDecl>(value)) {
          auto parent = mutableLocalVarParent[declRefExpr];

          // If the variable is immutable, it's fine so long as it involves
          // ConcurrentValue types.
          //
          // When flow-sensitive concurrent captures are enabled, we also
          // allow reads, depending on a SIL diagnostic pass to identify the
          // remaining race conditions.
          if (!var->supportsMutation() ||
              (ctx.LangOpts.EnableExperimentalFlowSensitiveConcurrentCaptures &&
               parent.dyn_cast<LoadExpr *>())) {
            return diagnoseNonConcurrentTypesInReference(
                valueRef, getDeclContext(), loc,
                ConcurrentReferenceKind::LocalCapture);
          }

          // Otherwise, we have concurrent access. Complain.
          ctx.Diags.diagnose(
              loc, diag::concurrent_access_of_local_capture,
              parent.dyn_cast<LoadExpr *>(),
              var->getDescriptiveKind(), var->getName());
          return true;
        }

        if (auto func = dyn_cast<FuncDecl>(value)) {
          if (func->isConcurrent())
            return false;

          func->diagnose(
              diag::local_function_executed_concurrently,
              func->getDescriptiveKind(), func->getName())
            .fixItInsert(func->getAttributeInsertionLoc(false), "@concurrent ");

          // Add the @concurrent attribute implicitly, so we don't diagnose
          // again.
          const_cast<FuncDecl *>(func)->getAttrs().add(
              new (ctx) ConcurrentAttr(true));
          return true;
        }

        // Concurrent access to some other local.
        ctx.Diags.diagnose(
            loc, diag::concurrent_access_local,
            value->getDescriptiveKind(), value->getName());
        value->diagnose(
            diag::kind_declared_here, value->getDescriptiveKind());
        return true;

      case ActorIsolationRestriction::Unsafe:
        return diagnoseReferenceToUnsafeGlobal(value, loc);
      }
      llvm_unreachable("unhandled actor isolation kind!");
    }

    /// Determine the reason for the given declaration context to be
    /// actor-independent.
    static Diag<DescriptiveDeclKind, DeclName>
    findActorIndependentReason(DeclContext *dc) {
      if (auto autoclosure = dyn_cast<AutoClosureExpr>(dc)) {
        switch (autoclosure->getThunkKind()) {
        case AutoClosureExpr::Kind::AsyncLet:
          return diag::actor_isolated_from_async_let;

        case AutoClosureExpr::Kind::DoubleCurryThunk:
        case AutoClosureExpr::Kind::SingleCurryThunk:
          return findActorIndependentReason(dc->getParent());

        case AutoClosureExpr::Kind::None:
          break;
        }
      }

      if (auto closure = dyn_cast<AbstractClosureExpr>(dc)) {
        if (isConcurrentClosure(closure)) {
          return diag::actor_isolated_from_concurrent_closure;
        }

        if (isEscapingClosure(closure)) {
          return diag::actor_isolated_from_escaping_closure;
        }

        return findActorIndependentReason(dc->getParent());
      }

      if (auto func = dyn_cast<AbstractFunctionDecl>(dc)) {
        if (func->isConcurrent())
          return diag::actor_isolated_from_concurrent_function;
      }

      return diag::actor_isolated_self_independent_context;
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

      case ActorIsolationRestriction::CrossActorSelf: {
        // If a cross-actor reference is on "self", it's not crossing actors.
        auto *selfVar = getReferencedSelf(base);
        auto curDC = const_cast<DeclContext *>(getDeclContext());
        if (selfVar &&
            getActorIsolationOfContext(curDC) ==
            ActorIsolation::forActorInstance(
                getNearestEnclosingActorContext(getDeclContext())))
          return false;

        return diagnoseNonConcurrentTypesInReference(
            memberRef, getDeclContext(), memberLoc,
            ConcurrentReferenceKind::CrossActor);
      }

      case ActorIsolationRestriction::ActorSelf: {
        // Must reference actor-isolated state on 'self'.
        auto *selfVar = getReferencedSelf(base);
        if (!selfVar) {
          // actor-isolated non-self calls are implicitly async and thus OK.
          if (maybeImplicitAsync && isa<AbstractFunctionDecl>(member)) {
            markNearestCallAsImplicitlyAsync();

            // Check for non-concurrent types.
            return diagnoseNonConcurrentTypesInReference(
                memberRef, getDeclContext(), memberLoc,
                ConcurrentReferenceKind::SynchronousAsAsyncCall);
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

        // Check whether the current context is differently-isolated.
        auto curDC = const_cast<DeclContext *>(getDeclContext());
        switch (auto contextIsolation = getActorIsolationOfContext(curDC)) {
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

            return false;

          case ActorIsolation::IndependentUnsafe:
          case ActorIsolation::Unspecified:
            return false;

          case ActorIsolation::Independent: {
            // The 'self' is for an actor-independent member, which means
            // we cannot refer to actor-isolated state.
            auto diag = findActorIndependentReason(curDC);
            ctx.Diags.diagnose(memberLoc, diag, member->getDescriptiveKind(),
                               member->getName());
            noteIsolatedActorMember(member);
            return true;
          }

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
        llvm_unreachable("Unhandled actor isolation");
      }

      case ActorIsolationRestriction::CrossGlobalActor:
      case ActorIsolationRestriction::GlobalActor:
        return checkGlobalActorReference(
            memberRef, memberLoc, isolation.getGlobalActor(),
            isolation == ActorIsolationRestriction::CrossGlobalActor);

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
      // Escaping and concurrent closures are always actor-independent.
      if (isEscapingClosure(closure) || isConcurrentClosure(closure))
        return ClosureActorIsolation::forIndependent();

      // A non-escaping closure gets its isolation from its context.
      auto parentIsolation = getActorIsolationOfContext(closure->getParent());

      // We must have parent isolation determined to get here.
      switch (parentIsolation) {
      case ActorIsolation::Independent:
      case ActorIsolation::IndependentUnsafe:
      case ActorIsolation::Unspecified:
        return ClosureActorIsolation::forIndependent();

      case ActorIsolation::GlobalActor: {
        Type globalActorType = closure->mapTypeIntoContext(
            parentIsolation.getGlobalActor()->mapTypeOutOfContext());
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

bool ActorIsolationChecker::mayExecuteConcurrentlyWith(
    const DeclContext *useContext, const DeclContext *defContext) {
  // Walk the context chain from the use to the definition.
  while (useContext != defContext) {
    // If we find a concurrent closure... it can be run concurrently.
    if (auto closure = dyn_cast<AbstractClosureExpr>(useContext)) {
      if (isConcurrentClosure(closure))
        return true;
    }

    if (auto func = dyn_cast<FuncDecl>(useContext)) {
      if (func->isLocalCapture()) {
        // If the function is @concurrent... it can be run concurrently.
        if (func->isConcurrent())
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

// Check whether a declaration is an asynchronous handler.
static bool isAsyncHandler(ValueDecl *value) {
  if (auto func = dyn_cast<AbstractFunctionDecl>(value)) {
    if (func->isAsyncHandler())
      return true;
  }

  return false;
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

  // A @concurrent function is assumed to be actor-independent.
  if (auto func = dyn_cast<AbstractFunctionDecl>(value)) {
    if (func->isConcurrent()) {
      defaultIsolation = ActorIsolation::forIndependent(
          ActorIndependentKind::Safe);
    }
  }

  // Check for instance members and initializers of actor classes,
  // which are part of actor-isolated state.
  auto classDecl = value->getDeclContext()->getSelfClassDecl();
  if (classDecl && classDecl->isActor() &&
      (value->isInstanceMember() || isa<ConstructorDecl>(value))) {
    defaultIsolation = ActorIsolation::forActorInstance(classDecl);
  }

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
    // Ignore the overridden declaration's isolation for an async handler,
    // because async handlers dispatch to wherever they need to be.
    if (!isAsyncHandler(value)) {
      auto isolation = getActorIsolation(overriddenValue);
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

  if (shouldInferAttributeInContext(value->getDeclContext())) {
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
  }

  // Instance members and initializers can infer isolation from their context.
  if (value->isInstanceMember() || isa<ConstructorDecl>(value)) {
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
  }

  // Default isolation for this member.
  return defaultIsolation;
}

void swift::checkOverrideActorIsolation(ValueDecl *value) {
  if (isa<TypeDecl>(value))
    return;

  auto overridden = value->getOverriddenDecl();
  if (!overridden)
    return;

  // Actor isolation doesn't matter for async handlers.
  if (isAsyncHandler(value))
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

  // If the overridden declaration is from Objective-C with no actor annotation,
  // and the overriding declaration has been placed in a global actor, allow it.
  if (overridden->hasClangNode() && !overriddenIsolation &&
      isolation.getKind() == ActorIsolation::GlobalActor)
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
      // Async and concurrent closures use concurrency features.
      if (auto closureType = closure->getType()) {
        if (auto fnType = closureType->getAs<AnyFunctionType>())
          if (fnType->isAsync() || fnType->isConcurrent())
            return true;
      }
    } else if (auto decl = dc->getAsDecl()) {
      // If any isolation attributes are present, we're using concurrency
      // features.
      if (getIsolationFromAttributes(decl, /*shouldDiagnose=*/false))
        return true;

      if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
        // Async and concurrent functions use concurrency features.
        if (func->hasAsync() || func->isConcurrent())
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

void swift::checkConcurrentValueConformance(ProtocolConformance *conformance) {
  auto conformanceDC = conformance->getDeclContext();
  auto nominal = conformance->getType()->getAnyNominal();
  if (!nominal)
    return;

  auto classDecl = dyn_cast<ClassDecl>(nominal);
  if (classDecl) {
    // Actors implicitly conform to ConcurrentValue and protect their state.
    if (classDecl->isActor())
      return;
  }

  // ConcurrentValue can only be used in the same source file.
  auto conformanceDecl = conformanceDC->getAsDecl();
  if (!conformanceDC->getParentSourceFile() ||
      conformanceDC->getParentSourceFile() != nominal->getParentSourceFile()) {
    conformanceDecl->diagnose(
        diag::concurrent_value_outside_source_file,
        nominal->getDescriptiveKind(), nominal->getName());
    return;
  }

  if (classDecl) {
    // An open class cannot conform to `ConcurrentValue`.
    if (classDecl->getFormalAccess() == AccessLevel::Open) {
      classDecl->diagnose(
          diag::concurrent_value_open_class, classDecl->getName());
      return;
    }

    // A 'ConcurrentValue' class cannot inherit from another class, although
    // we allow `NSObject` for Objective-C interoperability.
    if (!isa<InheritedProtocolConformance>(conformance)) {
      if (auto superclassDecl = classDecl->getSuperclassDecl()) {
        if (!superclassDecl->isNSObject()) {
          classDecl->diagnose(
              diag::concurrent_value_inherit,
              nominal->getASTContext().LangOpts.EnableObjCInterop,
              classDecl->getName());
          return;
        }
      }
    }
  }

  // Stored properties of structs and classes must have
  // ConcurrentValue-conforming types.
  if (isa<StructDecl>(nominal) || classDecl) {
    for (auto property : nominal->getStoredProperties()) {
      if (classDecl && property->supportsMutation()) {
        property->diagnose(diag::concurrent_value_class_mutable_property, property->getName(), nominal->getDescriptiveKind(),
            nominal->getName());
        continue;
      }

      auto propertyType =
          conformanceDC->mapTypeIntoContext(property->getInterfaceType());
      if (!isConcurrentValueType(conformanceDC, propertyType)) {
        property->diagnose(
            diag::non_concurrent_type_member, false, property->getName(),
            nominal->getDescriptiveKind(), nominal->getName(), propertyType);
        continue;
      }
    }

    return;
  }

  // Associated values of enum cases must have ConcurrentValue-conforming
  // types.
  if (auto enumDecl = dyn_cast<EnumDecl>(nominal)) {
    for (auto caseDecl : enumDecl->getAllCases()) {
      for (auto element : caseDecl->getElements()) {
        if (!element->hasAssociatedValues())
          continue;
        
        auto elementType = conformanceDC->mapTypeIntoContext(
            element->getArgumentInterfaceType());
        if (!isConcurrentValueType(conformanceDC, elementType)) {
          element->diagnose(
              diag::non_concurrent_type_member, true, element->getName(),
              nominal->getDescriptiveKind(), nominal->getName(), elementType);
          continue;
        }
      }
    }
  }
}
