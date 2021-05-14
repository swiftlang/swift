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
#include "swift/Strings.h"
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
  if (auto *file = dyn_cast<FileUnit>(dc->getModuleScopeContext())) {
    switch (file->getKind()) {
    case FileUnitKind::Source:
      // Check what kind of source file we have.
      if (auto sourceFile = dc->getParentSourceFile()) {
        switch (sourceFile->Kind) {
        case SourceFileKind::Interface:
          // Interfaces have explicitly called-out Sendable conformances.
          return false;

        case SourceFileKind::Library:
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
  }

  return false;
}

void swift::addAsyncNotes(AbstractFunctionDecl const* func) {
  assert(func);
  if (!isa<DestructorDecl>(func) && !isa<AccessorDecl>(func)) {
    auto note =
        func->diagnose(diag::note_add_async_to_function, func->getName());

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

bool IsActorRequest::evaluate(
    Evaluator &evaluator, NominalTypeDecl *nominal) const {
  // Protocols are actors if their `Self` type conforms to `Actor`.
  if (auto protocol = dyn_cast<ProtocolDecl>(nominal)) {
    // Simple case: we have the Actor protocol itself.
    if (protocol->isSpecificProtocol(KnownProtocolKind::Actor))
      return true;

    auto actorProto = nominal->getASTContext().getProtocol(
        KnownProtocolKind::Actor);
    if (!actorProto)
      return false;

    auto selfType = Type(protocol->getProtocolSelfType());
    auto genericSig = protocol->getGenericSignature();
    if (!genericSig)
      return false;

    return genericSig->requiresProtocol(selfType, actorProto);
  }

  // Class declarations are actors if they were declared with "actor".
  auto classDecl = dyn_cast<ClassDecl>(nominal);
  if (!classDecl)
    return false;

  return classDecl->isExplicitActor() ||
      classDecl->getAttrs().getAttribute<ActorAttr>();
}

bool IsDefaultActorRequest::evaluate(
    Evaluator &evaluator, ClassDecl *classDecl, ModuleDecl *M,
    ResilienceExpansion expansion) const {
  // If the class isn't an actor, it's not a default actor.
  if (!classDecl->isActor())
    return false;

  // If the class is resilient from the perspective of the module
  // module, it's not a default actor.
  if (classDecl->isForeign() || classDecl->isResilient(M, expansion))
    return false;

  // Check whether the class has explicit custom-actor methods.

  // If we synthesized the unownedExecutor property, we should've
  // added a semantics attribute to it (if it was actually a default
  // actor).
  if (auto executorProperty = classDecl->getUnownedExecutorProperty())
    return executorProperty->getAttrs()
             .hasSemanticsAttr(SEMANTICS_DEFAULT_ACTOR);

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
swift::checkGlobalActorAttributes(
    SourceLoc loc, DeclContext *dc, ArrayRef<CustomAttr *> attrs) {
  ASTContext &ctx = dc->getASTContext();

  CustomAttr *globalActorAttr = nullptr;
  NominalTypeDecl *globalActorNominal = nullptr;
  for (auto attr : attrs) {
    // Figure out which nominal declaration this custom attribute refers to.
    auto nominal = evaluateOrDefault(ctx.evaluator,
                                     CustomAttrNominalRequest{attr, dc},
                                     nullptr);

    // Ignore unresolvable custom attributes.
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
    return None;

  return std::make_pair(globalActorAttr, globalActorNominal);
}

Optional<std::pair<CustomAttr *, NominalTypeDecl *>>
GlobalActorAttributeRequest::evaluate(
    Evaluator &evaluator,
    llvm::PointerUnion<Decl *, ClosureExpr *> subject) const {
  DeclContext *dc;
  DeclAttributes *declAttrs;
  SourceLoc loc;
  if (auto decl = subject.dyn_cast<Decl *>()) {
    dc = decl->getDeclContext();
    declAttrs = &decl->getAttrs();
    loc = decl->getLoc();
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
    return None;

  // Closures can always have a global actor attached.
  if (auto closure = subject.dyn_cast<ClosureExpr *>()) {
    return result;
  }

  // Check that a global actor attribute makes sense on this kind of
  // declaration.
  auto decl = subject.get<Decl *>();
  auto globalActorAttr = result->first;
  if (auto nominal = dyn_cast<NominalTypeDecl>(decl)) {
    // Nominal types are okay...
    if (auto classDecl = dyn_cast<ClassDecl>(nominal)){
      if (classDecl->isActor()) {
        // ... except for actors.
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

  return result;
}

Type swift::getExplicitGlobalActor(ClosureExpr *closure) {
  // Look at the explicit attribute.
  auto globalActorAttr = evaluateOrDefault(
      closure->getASTContext().evaluator,
      GlobalActorAttributeRequest{closure}, None);
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

/// Determine the isolation rules for a given declaration.
ActorIsolationRestriction ActorIsolationRestriction::forDeclaration(
    ConcreteDeclRef declRef, bool fromExpression) {
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
    if (cast<ValueDecl>(decl)->isLocalCapture())
      return forUnrestricted();

    // 'let' declarations are immutable, so they can be accessed across
    // actors.
    bool isAccessibleAcrossActors = false;
    if (auto var = dyn_cast<VarDecl>(decl)) {
      if (var->isLet())
        isAccessibleAcrossActors = true;
    }

    // A function that provides an asynchronous context has no restrictions
    // on its access.
    //
    // FIXME: technically, synchronous functions are allowed to be cross-actor.
    // The call-sites are just conditionally async based on where they appear
    // (outside or inside the actor). This suggests that the implicitly-async
    // concept could be merged into the CrossActorSelf concept.
    if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
      if (func->isAsyncContext())
        isAccessibleAcrossActors = true;
    }

    // Similarly, a computed property or subscript that has an 'async' getter
    // provides an asynchronous context, and has no restrictions.
    if (auto storageDecl = dyn_cast<AbstractStorageDecl>(decl)) {
      if (auto effectfulGetter = storageDecl->getEffectfulGetAccessor())
        if (effectfulGetter->hasAsync())
          isAccessibleAcrossActors = true;
    }

    // Determine the actor isolation of the given declaration.
    switch (auto isolation = getActorIsolation(cast<ValueDecl>(decl))) {
    case ActorIsolation::ActorInstance:
      // Protected actor instance members can only be accessed on 'self'.
      return forActorSelf(isolation.getActor(),
          isAccessibleAcrossActors || isa<ConstructorDecl>(decl));

    case ActorIsolation::GlobalActorUnsafe:
    case ActorIsolation::GlobalActor: {
      // A global-actor-isolated function referenced within an expression
      // carries the global actor into its function type. The actual
      // reference to the function is therefore not restricted, because the
      // call to the function is.
      if (fromExpression && isa<AbstractFunctionDecl>(decl))
        return forUnrestricted();

      Type actorType = isolation.getGlobalActor();
      if (auto subs = declRef.getSubstitutions())
        actorType = actorType.subst(subs);

      return forGlobalActor(actorType, isAccessibleAcrossActors,
                            isolation == ActorIsolation::GlobalActorUnsafe);
    }

    case ActorIsolation::Independent:
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

/// Determine whether this closure should be treated as Sendable.
///
/// \param forActorIsolation Whether this check is for the purposes of
/// determining whether the closure must be non-isolated.
static bool isSendableClosure(
    const AbstractClosureExpr *closure, bool forActorIsolation) {
  if (auto explicitClosure = dyn_cast<ClosureExpr>(closure)) {
    if (forActorIsolation && explicitClosure->inheritsActorContext())
      return false;

    if (explicitClosure->isUnsafeSendable())
      return true;
  }

  if (auto type = closure->getType()) {
    if (auto fnType = type->getAs<AnyFunctionType>())
      if (fnType->isSendable())
        return true;
  }

  return false;
}

/// Determine whether the given type is suitable as a concurrent value type.
bool swift::isSendableType(const DeclContext *dc, Type type) {
  class IsSendable : public TypeVisitor<IsSendable, bool> {
    DeclContext *dc;
    ProtocolDecl *SendableProto;

  public:
    IsSendable(const DeclContext *dc)
        : dc(const_cast<DeclContext *>(dc)) {
      SendableProto = dc->getASTContext().getProtocol(
          KnownProtocolKind::Sendable);
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
      if (!SendableProto)
        return true;

      return !TypeChecker::conformsToProtocol(
          Type(type), SendableProto, dc).isInvalid();
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
      if (!SendableProto)
        return true;

      return !TypeChecker::containsProtocol(
        Type(type), SendableProto, dc).isInvalid();
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
      if (type->isSendable())
        return true;

      // C and thin function types meeting the requirements because they
      // cannot have captures.
      switch (type->getExtInfo().getRepresentation()) {
      case FunctionTypeRepresentation::Block:
      case FunctionTypeRepresentation::Swift:
        return false;

      case FunctionTypeRepresentation::CFunctionPointer:
      case FunctionTypeRepresentation::Thin:
        return true;
      }
    }

    bool visitProtocolCompositionType(ProtocolCompositionType *type) {
      if (!SendableProto)
        return true;

      return !TypeChecker::containsProtocol(type, SendableProto, dc)
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

static bool diagnoseNonConcurrentParameter(
    SourceLoc loc, ConcurrentReferenceKind refKind, ConcreteDeclRef declRef,
    ParamDecl *param, Type paramType, DiagnosticBehavior behavior) {
  ASTContext &ctx = declRef.getDecl()->getASTContext();
  ctx.Diags.diagnose(loc, diag::non_concurrent_param_type, paramType)
      .limitBehavior(behavior);
  return false;
}

static bool diagnoseNonConcurrentResult(
    SourceLoc loc, ConcurrentReferenceKind refKind, ConcreteDeclRef declRef,
    Type resultType, DiagnosticBehavior behavior) {
  ASTContext &ctx = declRef.getDecl()->getASTContext();
  ctx.Diags.diagnose(loc, diag::non_concurrent_result_type, resultType)
      .limitBehavior(behavior);
  return false;
}

static bool diagnoseNonConcurrentProperty(
    SourceLoc loc, ConcurrentReferenceKind refKind, VarDecl *var,
    Type propertyType, DiagnosticBehavior behavior) {
  ASTContext &ctx = var->getASTContext();
  ctx.Diags.diagnose(loc, diag::non_concurrent_property_type,
                     var->getDescriptiveKind(), var->getName(),
                     propertyType, var->isLocalCapture())
      .limitBehavior(behavior);
  return false;
}

/// Whether we should diagnose cases where Sendable conformances are
/// missing.
static bool shouldDiagnoseNonSendableViolations(
    const LangOptions &langOpts) {
  return langOpts.WarnConcurrency;
}

bool swift::diagnoseNonConcurrentTypesInReference(
    ConcreteDeclRef declRef, const DeclContext *dc, SourceLoc loc,
    ConcurrentReferenceKind refKind, DiagnosticBehavior behavior) {
  // Bail out immediately if we aren't supposed to do this checking.
  if (!shouldDiagnoseNonSendableViolations(dc->getASTContext().LangOpts))
    return false;

  // For functions, check the parameter and result types.
  SubstitutionMap subs = declRef.getSubstitutions();
  if (auto function = dyn_cast<AbstractFunctionDecl>(declRef.getDecl())) {
    for (auto param : *function->getParameters()) {
      Type paramType = param->getInterfaceType().subst(subs);
      if (!isSendableType(dc, paramType)) {
        return diagnoseNonConcurrentParameter(
            loc, refKind, declRef, param, paramType, behavior);
      }
    }

    // Check the result type of a function.
    if (auto func = dyn_cast<FuncDecl>(function)) {
      Type resultType = func->getResultInterfaceType().subst(subs);
      if (!isSendableType(dc, resultType)) {
        return diagnoseNonConcurrentResult(loc, refKind, declRef, resultType,
                                           behavior);
      }
    }

    return false;
  }

  if (auto var = dyn_cast<VarDecl>(declRef.getDecl())) {
    Type propertyType = var->isLocalCapture()
        ? var->getType()
        : var->getValueInterfaceType().subst(subs);
    if (!isSendableType(dc, propertyType)) {
      return diagnoseNonConcurrentProperty(loc, refKind, var, propertyType,
                                           behavior);
    }
  }

  if (auto subscript = dyn_cast<SubscriptDecl>(declRef.getDecl())) {
    for (auto param : *subscript->getIndices()) {
      Type paramType = param->getInterfaceType().subst(subs);
      if (!isSendableType(dc, paramType)) {
        return diagnoseNonConcurrentParameter(
            loc, refKind, declRef, param, paramType, behavior);
      }
    }

    // Check the element type of a subscript.
    Type resultType = subscript->getElementInterfaceType().subst(subs);
    if (!isSendableType(dc, resultType)) {
      return diagnoseNonConcurrentResult(loc, refKind, declRef, resultType,
                                         behavior);
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

    /// Keeps track of the capture context of variables that have been
    /// explicitly captured in closures.
    llvm::SmallDenseMap<VarDecl *, TinyPtrVector<const DeclContext *>>
      captureContexts;

    using MutableVarSource
        = llvm::PointerUnion<DeclRefExpr *, InOutExpr *, LookupExpr *>;

    using MutableVarParent
        = llvm::PointerUnion<InOutExpr *, LoadExpr *, AssignExpr *>;

    /// Mapping from mutable variable reference exprs, or inout expressions,
    /// to the parent expression, when that parent is either a load or
    /// an inout expr.
    llvm::SmallDenseMap<MutableVarSource, MutableVarParent, 4>
      mutableLocalVarParent;

    /// The values for each case in this enum correspond to %select numbers
    /// in a diagnostic, so be sure to update it if you add new cases.
    enum class VarRefUseEnv {
      Read = 0,
      Mutating = 1,
      Inout = 2 // means Mutating; having a separate kind helps diagnostics
    };

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
    Optional<VarRefUseEnv> kindOfUsage(ValueDecl *decl, Expr *use) const {
      // we need a use for lookup.
      if (!use)
        return None;

      // must be a property or subscript
      if (!isPropOrSubscript(decl))
        return None;

      if (auto lookup = dyn_cast<DeclRefExpr>(use))
        return usageEnv(lookup);
      else if (auto lookup = dyn_cast<LookupExpr>(use))
        return usageEnv(lookup);

      return None;
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

    bool shouldWalkIntoTapExpression() override { return true; }

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

      if (auto assign = dyn_cast<AssignExpr>(expr)) {
        // mark vars in the destination expr as being part of the Assign.
        if (auto destExpr = assign->getDest())
          recordMutableVarParent(assign, destExpr);

        return {true, expr };
      }

      if (auto load = dyn_cast<LoadExpr>(expr))
        recordMutableVarParent(load, load->getSubExpr());

      if (auto lookup = dyn_cast<LookupExpr>(expr)) {
        checkMemberReference(lookup->getBase(), lookup->getMember(),
                             lookup->getLoc(),
                             /*isEscapingPartialApply*/false,
                             lookup);
        return { true, expr };
      }

      if (auto declRef = dyn_cast<DeclRefExpr>(expr)) {
        checkNonMemberReference(
            declRef->getDeclRef(), declRef->getLoc(), declRef);
        return { true, expr };
      }

      if (auto apply = dyn_cast<ApplyExpr>(expr)) {
        applyStack.push_back(apply);  // record this encounter

        // Check the call itself.
        (void)checkApply(apply);

        // If this is a call to a partial apply thunk, decompose it to check it
        // like based on the original written syntax, e.g., "self.method".
        if (auto partialApply = decomposePartialApplyThunk(
                apply, Parent.getAsExpr())) {
          if (auto memberRef = findMemberReference(partialApply->fn)) {
            // NOTE: partially-applied thunks are never annotated as 
            // implicitly async, regardless of whether they are escaping.
            checkMemberReference(
                partialApply->base, memberRef->first, memberRef->second,
                partialApply->isEscaping);

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
              /*isEscapingPartialApply=*/false, call);

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


      if (auto keyPath = dyn_cast<KeyPathExpr>(expr))
        checkKeyPathExpr(keyPath);

      // The children of #selector expressions are not evaluated, so we do not
      // need to do isolation checking there. This is convenient because such
      // expressions tend to violate restrictions on the use of instance
      // methods.
      if (isa<ObjCSelectorExpr>(expr))
        return { false, expr };

      // Track the capture contexts for variables.
      if (auto captureList = dyn_cast<CaptureListExpr>(expr)) {
        for (const auto &entry : captureList->getCaptureList()) {
          captureContexts[entry.Var].push_back(captureList->getClosureBody());
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

      // Clear out the mutable local variable parent map on the way out.
      if (auto *declRefExpr = dyn_cast<DeclRefExpr>(expr))
        mutableLocalVarParent.erase(declRefExpr);
      else if (auto *lookupExpr = dyn_cast<LookupExpr>(expr))
        mutableLocalVarParent.erase(lookupExpr);
      else if (auto *inoutExpr = dyn_cast<InOutExpr>(expr))
        mutableLocalVarParent.erase(inoutExpr);

      // Remove the tracked capture contexts.
      if (auto captureList = dyn_cast<CaptureListExpr>(expr)) {
        for (const auto &entry : captureList->getCaptureList()) {
          auto &contexts = captureContexts[entry.Var];
          assert(contexts.back() == captureList->getClosureBody());
          contexts.pop_back();
          if (contexts.empty())
            captureContexts.erase(entry.Var);
        }
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

    /// Note when the enclosing context could be put on a global actor.
    void noteGlobalActorOnContext(DeclContext *dc, Type globalActor) {
      // If we are in a synchronous function on the global actor,
      // suggest annotating with the global actor itself.
      if (auto fn = dyn_cast<FuncDecl>(dc)) {
        if (!isa<AccessorDecl>(fn) && !fn->isAsyncContext()) {
          switch (getActorIsolation(fn)) {
          case ActorIsolation::ActorInstance:
          case ActorIsolation::GlobalActor:
          case ActorIsolation::GlobalActorUnsafe:
          case ActorIsolation::Independent:
            return;

          case ActorIsolation::Unspecified:
            fn->diagnose(diag::note_add_globalactor_to_function,
                globalActor->getWithoutParens().getString(),
                fn->getDescriptiveKind(),
                fn->getName(),
                globalActor)
              .fixItInsert(fn->getAttributeInsertionLoc(false),
                diag::insert_globalactor_attr, globalActor);
              return;
          }
        }
      }
    }

    /// Note that the given actor member is isolated.
    /// @param context is allowed to be null if no context is appropriate.
    void noteIsolatedActorMember(ValueDecl *decl, Expr *context) {
      // FIXME: Make this diagnostic more sensitive to the isolation context
      // of the declaration.
      if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
        func->diagnose(diag::actor_isolated_sync_func, 
          decl->getDescriptiveKind(),
          decl->getName());

        // was it an attempt to mutate an actor instance's isolated state?
      } else if (auto environment = kindOfUsage(decl, context)) {

        if (environment.getValue() == VarRefUseEnv::Read)
          decl->diagnose(diag::kind_declared_here, decl->getDescriptiveKind());
        else
          decl->diagnose(diag::actor_mutable_state, decl->getDescriptiveKind());

      } else {
        decl->diagnose(diag::kind_declared_here, decl->getDescriptiveKind());
      }
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
      if (!ctx.LangOpts.WarnConcurrency)
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
        case ActorIsolationRestriction::Unsafe:
          break;
        case ActorIsolationRestriction::GlobalActorUnsafe:
          // If we're not supposed to diagnose existing data races here,
          // we're done.
          if (!shouldDiagnoseExistingDataRaces(getDeclContext()))
            break;

          LLVM_FALLTHROUGH;

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
    ActorIsolation getInnermostIsolatedContext(DeclContext *dc) {
      // Retrieve the actor isolation of the context.
      switch (auto isolation = getActorIsolationOfContext(dc)) {
      case ActorIsolation::ActorInstance:
      case ActorIsolation::Independent:
      case ActorIsolation::Unspecified:
        return isolation;

      case ActorIsolation::GlobalActor:
      case ActorIsolation::GlobalActorUnsafe:
        return ActorIsolation::forGlobalActor(
            dc->mapTypeIntoContext(isolation.getGlobalActor()),
            isolation == ActorIsolation::GlobalActorUnsafe);
      }
    }

    bool isInAsynchronousContext() const {
      auto dc = getDeclContext();
      if (auto func = dyn_cast<AbstractFunctionDecl>(dc))
        return func->isAsyncContext();

      if (auto closure = dyn_cast<AbstractClosureExpr>(dc)) {
        if (auto type = closure->getType()) {
          if (auto fnType = type->getAs<AnyFunctionType>())
            return fnType->isAsync();
        }
      }

      return false;
    }

    enum class AsyncMarkingResult {
      FoundAsync, // successfully marked an implicitly-async operation
      NotFound,  // fail: no valid implicitly-async operation was found
      SyncContext, // fail: a valid implicitly-async op, but in sync context
      NotSendable  // fail: valid op and context, but not Sendable
    };

    /// Attempts to identify and mark a valid cross-actor use of a synchronous
    /// actor-isolated member (e.g., sync function application, property access)
    AsyncMarkingResult tryMarkImplicitlyAsync(SourceLoc declLoc,
                                              ConcreteDeclRef concDeclRef,
                                              Expr* context) {
      ValueDecl *decl = concDeclRef.getDecl();
      AsyncMarkingResult result = AsyncMarkingResult::NotFound;

      // is it an access to a property?
      if (isPropOrSubscript(decl)) {
        // we assume let-bound properties are taken care of elsewhere,
        // since they are never implicitly async.
        assert(!isa<VarDecl>(decl) || cast<VarDecl>(decl)->isLet() == false
               && "unexpected let-bound property; never implicitly async!");

        if (auto declRef = dyn_cast_or_null<DeclRefExpr>(context)) {
          if (usageEnv(declRef) == VarRefUseEnv::Read) {

            if (!isInAsynchronousContext())
              return AsyncMarkingResult::SyncContext;

            declRef->setImplicitlyAsync(true);
            result = AsyncMarkingResult::FoundAsync;
          }
        } else if (auto lookupExpr = dyn_cast_or_null<LookupExpr>(context)) {
          if (usageEnv(lookupExpr) == VarRefUseEnv::Read) {

            if (!isInAsynchronousContext())
              return AsyncMarkingResult::SyncContext;

            lookupExpr->setImplicitlyAsync(true);
            result = AsyncMarkingResult::FoundAsync;
          }
        }

      } else if (llvm::isa_and_nonnull<SelfApplyExpr>(context) &&
          isa<AbstractFunctionDecl>(decl)) {
        // actor-isolated non-isolated-self calls are implicitly async
        // and thus OK.

        if (!isInAsynchronousContext())
          return AsyncMarkingResult::SyncContext;

        markNearestCallAsImplicitlyAsync();
        result = AsyncMarkingResult::FoundAsync;

      } else if (!applyStack.empty()) {
        // Check our applyStack metadata from the traversal.
        // Our goal is to identify whether the actor reference appears
        // as the called value of the enclosing ApplyExpr. We cannot simply
        // inspect Parent here because of expressions like (callee)()
        // and the fact that the reference may be just an argument to an apply
        ApplyExpr *apply = applyStack.back();
        Expr *fn = apply->getFn()->getValueProvidingExpr();
        if (auto memberRef = findMemberReference(fn)) {
          auto concDecl = memberRef->first;
          if (decl == concDecl.getDecl() && !apply->implicitlyAsync()) {

            if (!isInAsynchronousContext())
              return AsyncMarkingResult::SyncContext;

            // then this ValueDecl appears as the called value of the ApplyExpr.
            markNearestCallAsImplicitlyAsync();
            result = AsyncMarkingResult::FoundAsync;
          }
        }
      }

      if (result == AsyncMarkingResult::FoundAsync) {
        // Check for non-concurrent types.
        bool problemFound =
            diagnoseNonConcurrentTypesInReference(
              concDeclRef, getDeclContext(), declLoc,
              ConcurrentReferenceKind::SynchronousAsAsyncCall);
        if (problemFound)
          result = AsyncMarkingResult::NotSendable;
      }

      return result;
    }

    /// Check actor isolation for a particular application.
    bool checkApply(ApplyExpr *apply) {
      auto fnExprType = apply->getFn()->getType();
      if (!fnExprType)
        return false;

      auto fnType = fnExprType->getAs<FunctionType>();
      if (!fnType)
        return false;

      // Handle calls to global-actor-qualified functions.
      Type globalActor = fnType->getGlobalActor();
      if (!globalActor)
        return false;

      auto declContext = const_cast<DeclContext *>(getDeclContext());

      // Check whether we are within the same isolation context, in which
      // case there is nothing further to check,
      auto contextIsolation = getInnermostIsolatedContext(declContext);
      if (contextIsolation.isGlobalActor() &&
          contextIsolation.getGlobalActor()->isEqual(globalActor)) {
        return false;
      }

      // From this point on, the only possibility is that we have an implicitly
      // aynchronous call.

      // If we are not in an asynchronous context, complain.
      if (!isInAsynchronousContext()) {
        auto isolation = ActorIsolation::forGlobalActor(
            globalActor, /*unsafe=*/false);
        if (auto calleeDecl = apply->getCalledValue()) {
          ctx.Diags.diagnose(
              apply->getLoc(), diag::actor_isolated_call_decl, isolation,
              calleeDecl->getDescriptiveKind(), calleeDecl->getName(),
              contextIsolation);
          calleeDecl->diagnose(
              diag::actor_isolated_sync_func, calleeDecl->getDescriptiveKind(),
              calleeDecl->getName());
        } else {
          ctx.Diags.diagnose(
              apply->getLoc(), diag::actor_isolated_call, isolation,
              contextIsolation);
        }
        noteGlobalActorOnContext(
            const_cast<DeclContext *>(getDeclContext()), globalActor);

        return true;
      }

      // Mark as implicitly async.
      if (!fnType->getExtInfo().isAsync())
        apply->setImplicitlyAsync(true);

      // If we don't need to check for sendability, we're done.
      if (!shouldDiagnoseNonSendableViolations(ctx.LangOpts))
        return false;

      // Check for sendability of the parameter types.
      for (const auto &param : fnType->getParams()) {
        // FIXME: Dig out the locations of the corresponding arguments.
        if (!isSendableType(getDeclContext(), param.getParameterType())) {
          ctx.Diags.diagnose(
              apply->getLoc(), diag::non_concurrent_param_type,
              param.getParameterType());
          return true;
        }
      }

      // Check for sendability of the result type.
      if (!isSendableType(getDeclContext(), fnType->getResult())) {
        ctx.Diags.diagnose(
            apply->getLoc(), diag::non_concurrent_result_type,
            fnType->getResult());
        return true;
      }

      return false;
    }

    /// Check a reference to an entity within a global actor.
    bool checkGlobalActorReference(
        ConcreteDeclRef valueRef, SourceLoc loc, Type globalActor,
        bool isCrossActor,
        Expr *context) {
      ValueDecl *value = valueRef.getDecl();
      auto declContext = const_cast<DeclContext *>(getDeclContext());

      // Check whether we are within the same isolation context, in which
      // case there is nothing further to check,
      auto contextIsolation = getInnermostIsolatedContext(declContext);
      if (contextIsolation.isGlobalActor() &&
          contextIsolation.getGlobalActor()->isEqual(globalActor)) {
        return false;
      }

      // A cross-actor access requires types to be concurrent-safe.
      if (isCrossActor) {
        return diagnoseNonConcurrentTypesInReference(
            valueRef, getDeclContext(), loc,
            ConcurrentReferenceKind::CrossActor);
      }

      switch (contextIsolation) {
      case ActorIsolation::ActorInstance: {
        auto result = tryMarkImplicitlyAsync(loc, valueRef, context);
        if (result == AsyncMarkingResult::FoundAsync)
          return false;

        auto useKind = static_cast<unsigned>(
            kindOfUsage(value, context).getValueOr(VarRefUseEnv::Read));

        ctx.Diags.diagnose(loc, diag::global_actor_from_instance_actor_context,
                           value->getDescriptiveKind(), value->getName(),
                           globalActor, contextIsolation.getActor()->getName(),
                           useKind, result == AsyncMarkingResult::SyncContext);
        noteIsolatedActorMember(value, context);
        return true;
      }

      case ActorIsolation::GlobalActor:
      case ActorIsolation::GlobalActorUnsafe: {
        // Check if this decl reference is the callee of the enclosing Apply,
        // making it OK as an implicitly async call.
        auto result = tryMarkImplicitlyAsync(loc, valueRef, context);
        if (result == AsyncMarkingResult::FoundAsync)
          return false;

        auto useKind = static_cast<unsigned>(
            kindOfUsage(value, context).getValueOr(VarRefUseEnv::Read));

        // Otherwise, this is a problematic global actor decl reference.
        ctx.Diags.diagnose(
            loc, diag::global_actor_from_other_global_actor_context,
            value->getDescriptiveKind(), value->getName(), globalActor,
            contextIsolation.getGlobalActor(), useKind,
            result == AsyncMarkingResult::SyncContext);
        noteIsolatedActorMember(value, context);
        return true;
      }

      case ActorIsolation::Independent: {
        auto result = tryMarkImplicitlyAsync(loc, valueRef, context);
        if (result == AsyncMarkingResult::FoundAsync)
          return false;

        auto useKind = static_cast<unsigned>(
            kindOfUsage(value, context).getValueOr(VarRefUseEnv::Read));

        ctx.Diags.diagnose(loc, diag::global_actor_from_nonactor_context,
                           value->getDescriptiveKind(), value->getName(),
                           globalActor,
                           /*actorIndependent=*/true, useKind,
                           result == AsyncMarkingResult::SyncContext);
        noteIsolatedActorMember(value, context);
        return true;
      }

      case ActorIsolation::Unspecified: {
        auto result = tryMarkImplicitlyAsync(loc, valueRef, context);
        if (result == AsyncMarkingResult::FoundAsync)
          return false;

        // Diagnose the reference.
        auto useKind = static_cast<unsigned>(
            kindOfUsage(value, context).getValueOr(VarRefUseEnv::Read));
        ctx.Diags.diagnose(
          loc, diag::global_actor_from_nonactor_context,
          value->getDescriptiveKind(), value->getName(), globalActor,
          /*actorIndependent=*/false, useKind,
          result == AsyncMarkingResult::SyncContext);
        noteGlobalActorOnContext(declContext, globalActor);
        noteIsolatedActorMember(value, context);

        return true;
      } // end Unspecified case
      } // end switch
      llvm_unreachable("unhandled actor isolation kind!");
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

      // Check whether we are in a context that will not execute concurrently
      // with the context of 'self'. If not, it's safe.
      if (!mayExecuteConcurrentlyWith(
              getDeclContext(), findCapturedDeclContext(value)))
        return false;

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
        if (func->isSendable())
          return false;

        func->diagnose(
            diag::local_function_executed_concurrently,
            func->getDescriptiveKind(), func->getName())
          .fixItInsert(func->getAttributeInsertionLoc(false), "@Sendable ");

        // Add the @Sendable attribute implicitly, so we don't diagnose
        // again.
        const_cast<FuncDecl *>(func)->getAttrs().add(
            new (ctx) SendableAttr(true));
        return true;
      }

      // Concurrent access to some other local.
      ctx.Diags.diagnose(
          loc, diag::concurrent_access_local,
          value->getDescriptiveKind(), value->getName());
      value->diagnose(
          diag::kind_declared_here, value->getDescriptiveKind());
      return true;
    }

    ///
    /// \return true iff a diagnostic was emitted
    bool checkKeyPathExpr(KeyPathExpr *keyPath) {
      bool diagnosed = false;

      // returns None if it is not a 'let'-bound var decl. Otherwise,
      // the bool indicates whether a diagnostic was emitted.
      auto checkLetBoundVarDecl = [&](KeyPathExpr::Component const& component)
                                                            -> Optional<bool> {
        auto decl = component.getDeclRef().getDecl();
        if (auto varDecl = dyn_cast<VarDecl>(decl)) {
          if (varDecl->isLet()) {
            auto type = component.getComponentType();
            if (shouldDiagnoseNonSendableViolations(ctx.LangOpts)
                && !isSendableType(getDeclContext(), type)) {
              ctx.Diags.diagnose(
                  component.getLoc(), diag::non_concurrent_keypath_access,
                  type);
              return true;
            }
            return false;
          }
        }
        return None;
      };

      // check the components of the keypath.
      for (const auto &component : keyPath->getComponents()) {
        // The decl referred to by the path component cannot be within an actor.
        if (component.hasDeclRef()) {
          auto concDecl = component.getDeclRef();
          auto isolation = ActorIsolationRestriction::forDeclaration(concDecl);

          switch (isolation.getKind()) {
          case ActorIsolationRestriction::Unsafe:
          case ActorIsolationRestriction::Unrestricted:
            break; // OK. Does not refer to an actor-isolated member.

          case ActorIsolationRestriction::GlobalActorUnsafe:
            // Only check if we're in code that's adopted concurrency features.
            if (!shouldDiagnoseExistingDataRaces(getDeclContext()))
              break; // do not check

            LLVM_FALLTHROUGH; // otherwise, perform checking

          case ActorIsolationRestriction::GlobalActor:
            // Disable global actor checking for now.
            if (!ctx.LangOpts.isSwiftVersionAtLeast(6))
              break;

            LLVM_FALLTHROUGH; // otherwise, it's invalid so diagnose it.

          case ActorIsolationRestriction::CrossActorSelf:
            // 'let'-bound decls with this isolation are OK, just check them.
            if (auto wasLetBound = checkLetBoundVarDecl(component)) {
              diagnosed = wasLetBound.getValue();
              break;
            }
            LLVM_FALLTHROUGH; // otherwise, it's invalid so diagnose it.

          case ActorIsolationRestriction::ActorSelf: {
            auto decl = concDecl.getDecl();
            ctx.Diags.diagnose(component.getLoc(),
                               diag::actor_isolated_keypath_component,
                               decl->getDescriptiveKind(), decl->getName());
            diagnosed = true;
            break;
          }
          }; // end switch
        }

        // Captured values in a path component must conform to Sendable.
        // These captured values appear in Subscript, aka "index" components,
        // such as \Type.dict[k] where k is a captured dictionary key.
        if (auto indexExpr = component.getIndexExpr()) {
          auto type = indexExpr->getType();
          if (type && shouldDiagnoseNonSendableViolations(ctx.LangOpts)
              && !isSendableType(getDeclContext(), type)) {
            ctx.Diags.diagnose(
                component.getLoc(), diag::non_concurrent_keypath_capture,
                indexExpr->getType());
            diagnosed = true;
          }
        }
      }

      return diagnosed;
    }

    /// Check a reference to a local or global.
    bool checkNonMemberReference(
        ConcreteDeclRef valueRef, SourceLoc loc, DeclRefExpr *declRefExpr) {
      if (!valueRef)
        return false;

      auto value = valueRef.getDecl();

      if (value->isLocalCapture())
        return checkLocalCapture(valueRef, loc, declRefExpr);

      switch (auto isolation =
                  ActorIsolationRestriction::forDeclaration(valueRef)) {
      case ActorIsolationRestriction::Unrestricted:
        return false;

      case ActorIsolationRestriction::CrossActorSelf:
      case ActorIsolationRestriction::ActorSelf:
        llvm_unreachable("non-member reference into an actor");

      case ActorIsolationRestriction::GlobalActorUnsafe:
        // Only complain if we're in code that's adopted concurrency features.
        if (!shouldDiagnoseExistingDataRaces(getDeclContext()))
          return false;

        LLVM_FALLTHROUGH;

      case ActorIsolationRestriction::GlobalActor:
        return checkGlobalActorReference(
            valueRef, loc, isolation.getGlobalActor(), isolation.isCrossActor,
            declRefExpr);

      case ActorIsolationRestriction::Unsafe:
        return diagnoseReferenceToUnsafeGlobal(value, loc);
      }
      llvm_unreachable("unhandled actor isolation kind!");
    }

    /// Determine the reason for the given declaration context to be
    /// actor-independent.
    static Diag<DescriptiveDeclKind, DeclName, unsigned>
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
        if (isSendableClosure(closure, /*forActorIsolation=*/true)) {
          return diag::actor_isolated_from_concurrent_closure;
        }

        return findActorIndependentReason(dc->getParent());
      }

      if (auto func = dyn_cast<AbstractFunctionDecl>(dc)) {
        if (func->isSendable())
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
        Expr *context = nullptr) {
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
          // Check for implicit async.
          auto result = tryMarkImplicitlyAsync(memberLoc, memberRef, context);
          if (result == AsyncMarkingResult::FoundAsync)
            return false; // no problems
          else if (result == AsyncMarkingResult::NotSendable)
            return true;

          auto useKind = static_cast<unsigned>(
              kindOfUsage(member, context).getValueOr(VarRefUseEnv::Read));

          ctx.Diags.diagnose(
              memberLoc, diag::actor_isolated_non_self_reference,
              member->getDescriptiveKind(),
              member->getName(),
              isolation.getActorType() ==
                getNearestEnclosingActorContext(getDeclContext()),
              useKind
              );
          noteIsolatedActorMember(member, context);
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
              noteIsolatedActorMember(member, context);
              return true;
            }

            return false;

          case ActorIsolation::Unspecified:
            return false;

          case ActorIsolation::Independent: {
            auto result = tryMarkImplicitlyAsync(memberLoc, memberRef, context);
            if (result == AsyncMarkingResult::FoundAsync)
              return false; // no problems
            else if (result == AsyncMarkingResult::NotSendable)
              return true;

            // The 'self' is for an actor-independent member, which means
            // we cannot refer to actor-isolated state.
            auto useKind = static_cast<unsigned>(
                kindOfUsage(member, context).getValueOr(VarRefUseEnv::Read));
            auto diag = findActorIndependentReason(curDC);
            ctx.Diags.diagnose(memberLoc, diag, member->getDescriptiveKind(),
                               member->getName(), useKind);
            noteIsolatedActorMember(member, context);
            return true;
          }

          case ActorIsolation::GlobalActor:
          case ActorIsolation::GlobalActorUnsafe: {
            auto result = tryMarkImplicitlyAsync(memberLoc, memberRef, context);
            if (result == AsyncMarkingResult::FoundAsync)
              return false; // no problems
            else if (result == AsyncMarkingResult::NotSendable)
              return true;

            // The 'self' is for a member that's part of a global actor, which
            // means we cannot refer to actor-isolated state.
            auto useKind = static_cast<unsigned>(
                kindOfUsage(member, context).getValueOr(VarRefUseEnv::Read));
            ctx.Diags.diagnose(memberLoc,
                               diag::actor_isolated_global_actor_context,
                               member->getDescriptiveKind(), member->getName(),
                               contextIsolation.getGlobalActor(), useKind,
                               result == AsyncMarkingResult::SyncContext
                               );
            noteIsolatedActorMember(member, context);
            return true;
          }
        }
        llvm_unreachable("Unhandled actor isolation");
      }

      case ActorIsolationRestriction::GlobalActorUnsafe:
        // Only complain if we're in code that's adopted concurrency features.
        if (!shouldDiagnoseExistingDataRaces(getDeclContext()))
          return false;

        LLVM_FALLTHROUGH;

      case ActorIsolationRestriction::GlobalActor: {
        const bool isInitDeInit = isa<ConstructorDecl>(getDeclContext()) ||
                                  isa<DestructorDecl>(getDeclContext());
        // If we are within an initializer or deinitilizer and are referencing a
        // stored property on "self", we are not crossing actors.
        if (isInitDeInit && isa<VarDecl>(member) &&
            cast<VarDecl>(member)->hasStorage() && getReferencedSelf(base))
          return false;
        return checkGlobalActorReference(
            memberRef, memberLoc, isolation.getGlobalActor(),
            isolation.isCrossActor, context);
      }
      case ActorIsolationRestriction::Unsafe:
        // This case is hit when passing actor state inout to functions in some
        // cases. The error is emitted by diagnoseInOutArg.
        return false;
      }
      llvm_unreachable("unhandled actor isolation kind!");
    }

    // Attempt to resolve the global actor type of a closure.
    Type resolveGlobalActorType(ClosureExpr *closure) {
      // Check whether the closure's type has a global actor already.
      if (Type closureType = closure->getType()) {
        if (auto closureFnType = closureType->getAs<FunctionType>()) {
          if (Type globalActor = closureFnType->getGlobalActor())
            return globalActor;
        }
      }

      // Look for an explicit attribute.
      return getExplicitGlobalActor(closure);
    }

    /// Determine the isolation of a particular closure.
    ///
    /// This function assumes that enclosing closures have already had their
    /// isolation checked.
    ClosureActorIsolation determineClosureIsolation(
        AbstractClosureExpr *closure) {
      // If the closure specifies a global actor, use it.
      if (auto explicitClosure = dyn_cast<ClosureExpr>(closure)) {
        if (explicitClosure->getAttrs().hasAttribute<ActorIndependentAttr>())
          return ClosureActorIsolation::forIndependent();

        if (Type globalActorType = resolveGlobalActorType(explicitClosure))
          return ClosureActorIsolation::forGlobalActor(globalActorType);

        if (explicitClosure->isUnsafeMainActor()) {
          ASTContext &ctx = closure->getASTContext();
          if (Type mainActor = ctx.getMainActorType())
            return ClosureActorIsolation::forGlobalActor(mainActor);
        }
      }

      // Sendable closures are actor-independent unless the closure has
      // specifically opted into inheriting actor isolation.
      if (isSendableClosure(closure, /*forActorIsolation=*/true))
        return ClosureActorIsolation::forIndependent();

      // A non-escaping closure gets its isolation from its context.
      auto parentIsolation = getActorIsolationOfContext(closure->getParent());

      // We must have parent isolation determined to get here.
      switch (parentIsolation) {
      case ActorIsolation::Independent:
      case ActorIsolation::Unspecified:
        return ClosureActorIsolation::forIndependent();

      case ActorIsolation::GlobalActor:
      case ActorIsolation::GlobalActorUnsafe: {
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
      if (isSendableClosure(closure, /*forActorIsolation=*/false))
        return true;
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
  decl->getBody()->walk(checker);
}

void swift::checkFunctionActorIsolation(AbstractFunctionDecl *decl) {
  // Disable this check for @LLDBDebuggerFunction functions.
  if (decl->getAttrs().hasAttribute<LLDBDebuggerFunctionAttr>())
    return;

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
    const Decl *decl, bool shouldDiagnose = true, bool onlyExplicit = false) {
  // Look up attributes on the declaration that can affect its actor isolation.
  // If any of them are present, use that attribute.
  auto independentAttr = decl->getAttrs().getAttribute<ActorIndependentAttr>();
  auto nonisolatedAttr = decl->getAttrs().getAttribute<NonisolatedAttr>();
  auto globalActorAttr = decl->getGlobalActorAttr();

  // Remove implicit attributes if we only care about explicit ones.
  if (onlyExplicit) {
    if (independentAttr && independentAttr->isImplicit())
      independentAttr = nullptr;
    if (nonisolatedAttr && nonisolatedAttr->isImplicit())
      nonisolatedAttr = nullptr;
    if (globalActorAttr && globalActorAttr->first->isImplicit())
      globalActorAttr = None;
  }

  unsigned numIsolationAttrs =
    (nonisolatedAttr ? 1 : 0) + (independentAttr ? 1 : 0) +
    (globalActorAttr ? 1 : 0);
  if (numIsolationAttrs == 0)
    return None;

  // Only one such attribute is valid, but we only actually care of one of
  // them is a global actor.
  if (numIsolationAttrs > 1) {
    DeclName name;
    if (auto value = dyn_cast<ValueDecl>(decl)) {
      name = value->getName();
    } else if (auto ext = dyn_cast<ExtensionDecl>(decl)) {
      if (auto selfTypeDecl = ext->getSelfNominalTypeDecl())
        name = selfTypeDecl->getName();
    }

    if (globalActorAttr) {
      StringRef nonisolatedAttrName;
      SourceRange nonisolatedRange;
      if (independentAttr) {
        nonisolatedAttrName = independentAttr->getAttrName();
        nonisolatedRange = independentAttr->getRangeWithAt();
      } else {
        nonisolatedAttrName = nonisolatedAttr->getAttrName();
        nonisolatedRange = nonisolatedAttr->getRangeWithAt();
      }

      if (shouldDiagnose) {
        decl->diagnose(
            diag::actor_isolation_multiple_attr, decl->getDescriptiveKind(),
            name, nonisolatedAttrName,
            globalActorAttr->second->getName().str())
          .highlight(nonisolatedRange)
          .highlight(globalActorAttr->first->getRangeWithAt());
      }
    }
  }

  // If the declaration is explicitly marked 'nonisolated', report it as
  // independent.
  if (nonisolatedAttr) {
    return ActorIsolation::forIndependent();
  }

  // If the declaration is explicitly marked @actorIndependent, report it as
  // independent.
  if (independentAttr) {
    return ActorIsolation::forIndependent();
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
    bool isUnsafe = globalActorAttr->first->isArgUnsafe();
    if (globalActorAttr->first->getArg() && !isUnsafe) {
      ctx.Diags.diagnose(
          globalActorAttr->first->getLocation(),
          diag::global_actor_non_unsafe_init, globalActorType);
    }

    return ActorIsolation::forGlobalActor(
        globalActorType->mapTypeOutOfContext(), isUnsafe);
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
  auto conformances = idc->getLocalConformances(
      ConformanceLookupKind::NonStructural);
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
      switch (requirementIsolation) {
      case ActorIsolation::ActorInstance:
      case ActorIsolation::Unspecified:
        continue;

      case ActorIsolation::GlobalActor:
      case ActorIsolation::GlobalActorUnsafe:
      case ActorIsolation::Independent:
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

      case ActorIsolation::Independent:
        // We only need one @actorIndependent.
        if (sawActorIndependent)
          return true;

        sawActorIndependent = true;
        return false;

      case ActorIsolation::Unspecified:
        return true;

      case ActorIsolation::GlobalActor:
      case ActorIsolation::GlobalActorUnsafe: {
        // Substitute into the global actor type.
        auto conformance = std::get<0>(isolated);
        auto requirementSubs = SubstitutionMap::getProtocolSubstitutions(
            conformance->getProtocol(), dc->getSelfTypeInContext(),
            ProtocolConformanceRef(conformance));
        Type globalActor = isolation.getGlobalActor().subst(requirementSubs);
        if (!globalActorTypes.insert(globalActor->getCanonicalType()).second)
          return true;

        // Update the global actor type, now that we've done this substitution.
        std::get<1>(isolated) = ActorIsolation::forGlobalActor(
            globalActor, isolation == ActorIsolation::GlobalActorUnsafe);
        return false;
      }
      }
      }),
      isolatedRequirements.end());

  if (isolatedRequirements.size() != 1)
    return None;

  return std::get<1>(isolatedRequirements.front());
}

/// Compute the isolation of a nominal type from the conformances that
/// are directly specified on the type.
static Optional<ActorIsolation> getIsolationFromConformances(
    NominalTypeDecl *nominal) {
  if (isa<ProtocolDecl>(nominal))
    return None;

  Optional<ActorIsolation> foundIsolation;
  for (auto proto :
       nominal->getLocalProtocols(ConformanceLookupKind::NonStructural)) {
    switch (auto protoIsolation = getActorIsolation(proto)) {
    case ActorIsolation::ActorInstance:
    case ActorIsolation::Unspecified:
    case ActorIsolation::Independent:
      break;

    case ActorIsolation::GlobalActor:
    case ActorIsolation::GlobalActorUnsafe:
      if (!foundIsolation) {
        foundIsolation = protoIsolation;
        continue;
      }

      if (*foundIsolation != protoIsolation)
        return None;

      break;
    }
  }

  return foundIsolation;
}

/// Compute the isolation of a nominal type from the property wrappers on
/// any stored properties.
static Optional<ActorIsolation> getIsolationFromWrappers(
    NominalTypeDecl *nominal) {
  if (!isa<StructDecl>(nominal) && !isa<ClassDecl>(nominal))
    return None;

  if (!nominal->getParentSourceFile())
    return None;
  
  Optional<ActorIsolation> foundIsolation;
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
    case ActorIsolation::Independent:
      break;

    case ActorIsolation::GlobalActor:
    case ActorIsolation::GlobalActorUnsafe:
      if (!foundIsolation) {
        foundIsolation = isolation;
        continue;
      }

      if (*foundIsolation != isolation)
        return None;

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
static Optional<MemberIsolationPropagation> getMemberIsolationPropagation(
    const ValueDecl *value) {
  if (!value->getDeclContext()->isTypeContext())
    return None;

  switch (value->getKind()) {
  case DeclKind::Import:
  case DeclKind::Extension:
  case DeclKind::TopLevelCode:
  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
  case DeclKind::IfConfig:
  case DeclKind::PoundDiagnostic:
  case DeclKind::PrecedenceGroup:
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
  case DeclKind::Destructor:
    return None;

  case DeclKind::PatternBinding:
  case DeclKind::EnumCase:
  case DeclKind::EnumElement:
  case DeclKind::Constructor:
    return MemberIsolationPropagation::GlobalActor;

  case DeclKind::Func:
  case DeclKind::Accessor:
  case DeclKind::Subscript:
  case DeclKind::Var:
    return value->isInstanceMember() ? MemberIsolationPropagation::AnyIsolation
                                     : MemberIsolationPropagation::GlobalActor;
  }
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

  // A @Sendable function is assumed to be actor-independent.
  if (auto func = dyn_cast<AbstractFunctionDecl>(value)) {
    if (func->isSendable()) {
      defaultIsolation = ActorIsolation::forIndependent();
    }
  }

  // Check for instance members and initializers of actor types,
  // which are part of actor-isolated state.
  if (auto nominal = value->getDeclContext()->getSelfNominalTypeDecl()) {
    if (nominal->isActor() &&
        (value->isInstanceMember() || isa<ConstructorDecl>(value))) {
      defaultIsolation = ActorIsolation::forActorInstance(nominal);
    }
  }

  // Function used when returning an inferred isolation.
  auto inferredIsolation = [&](
      ActorIsolation inferred, bool onlyGlobal = false) {
    // Add an implicit attribute to capture the actor isolation that was
    // inferred, so that (e.g.) it will be printed and serialized.
    ASTContext &ctx = value->getASTContext();
    switch (inferred) {
    case ActorIsolation::Independent:
      if (onlyGlobal)
        return ActorIsolation::forUnspecified();

      value->getAttrs().add(new (ctx) ActorIndependentAttr(
                              ActorIndependentKind::Safe, /*IsImplicit=*/true));
      break;

    case ActorIsolation::GlobalActorUnsafe:
    case ActorIsolation::GlobalActor: {
      auto typeExpr = TypeExpr::createImplicit(inferred.getGlobalActor(), ctx);
      auto attr = CustomAttr::create(
          ctx, SourceLoc(), typeExpr, /*implicit=*/true);
      if (inferred == ActorIsolation::GlobalActorUnsafe)
        attr->setArgIsUnsafe(true);
      value->getAttrs().add(attr);
      break;
    }

    case ActorIsolation::ActorInstance:
    case ActorIsolation::Unspecified:
      if (onlyGlobal)
        return ActorIsolation::forUnspecified();

      // Nothing to do.
      break;
    }
    return inferred;
  };

  // If the declaration overrides another declaration, it must have the same
  // actor isolation.
  if (auto overriddenValue = value->getOverriddenDecl()) {
    auto isolation = getActorIsolation(overriddenValue);
    SubstitutionMap subs;

    if (Type selfType = value->getDeclContext()->getSelfInterfaceType()) {
      subs = selfType->getMemberSubstitutionMap(
          value->getModuleContext(), overriddenValue);
    }

    return inferredIsolation(isolation.subst(subs));
  }

  // If this is an accessor, use the actor isolation of its storage
  // declaration.
  if (auto accessor = dyn_cast<AccessorDecl>(value)) {
    return getActorIsolation(accessor->getStorage());
  }

  if (auto var = dyn_cast<VarDecl>(value)) {
    // If this is a variable with a property wrapper, infer from the property
    // wrapper's wrappedValue.
    if (auto wrapperInfo = var->getAttachedPropertyWrapperTypeInfo(0)) {
      if (auto wrappedValue = wrapperInfo.valueVar) {
        if (auto isolation = getActorIsolation(wrappedValue))
          return inferredIsolation(isolation);
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
              return inferredIsolation(isolation);
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
            return inferredIsolation(isolation);
        }
      }
    }
  }

  if (shouldInferAttributeInContext(value->getDeclContext())) {
    // If the declaration witnesses a protocol requirement that is isolated,
    // use that.
    if (auto witnessedIsolation = getIsolationFromWitnessedRequirements(value)) {
      if (auto inferred = inferredIsolation(*witnessedIsolation))
        return inferred;
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

          if (auto inferred = inferredIsolation(superclassIsolation))
            return inferred;
        }
      }
    }

    if (auto nominal = dyn_cast<NominalTypeDecl>(value)) {
      // If the declaration is a nominal type and any of the protocols to which
      // it directly conforms is isolated to a global actor, use that.
      if (auto conformanceIsolation = getIsolationFromConformances(nominal))
        if (auto inferred = inferredIsolation(*conformanceIsolation))
          return inferred;

      // If the declaration is a nominal type and any property wrappers on
      // its stored properties require isolation, use that.
      if (auto wrapperIsolation = getIsolationFromWrappers(nominal)) {
        if (auto inferred = inferredIsolation(*wrapperIsolation))
          return inferred;
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
        return inferredIsolation(*isolationFromAttr, onlyGlobal);
      }
    }

    // If the declaration is in a nominal type (or extension thereof) that
    // has isolation, use that.
    if (auto selfTypeDecl = value->getDeclContext()->getSelfNominalTypeDecl()) {
      if (auto selfTypeIsolation = getActorIsolation(selfTypeDecl))
        return inferredIsolation(selfTypeIsolation, onlyGlobal);
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

  // Determine the actor isolation of this declaration.
  auto isolation = getActorIsolation(value);

  // Determine the actor isolation of the overridden function.=
  auto overriddenIsolation = getActorIsolation(overridden);

  if (overriddenIsolation.requiresSubstitution()) {
    SubstitutionMap subs;
    if (Type selfType = value->getDeclContext()->getSelfInterfaceType()) {
      subs = selfType->getMemberSubstitutionMap(
          value->getModuleContext(), overridden);
    }

    overriddenIsolation = overriddenIsolation.subst(subs);
  }

  // If the isolation matches, we're done.
  if (isolation == overriddenIsolation)
    return;

  // If the overridden declaration is from Objective-C with no actor annotation,
  // and the overriding declaration has been placed in a global actor, allow it.
  if (overridden->hasClangNode() && !overriddenIsolation &&
      isolation.isGlobalActor())
    return;

  // If the overridden declaration uses an unsafe global actor, we can do
  // anything except be actor-isolated or have a different global actor.
  if (overriddenIsolation == ActorIsolation::GlobalActorUnsafe) {
    switch (isolation) {
    case ActorIsolation::Independent:
    case ActorIsolation::Unspecified:
      return;

    case ActorIsolation::ActorInstance:
      // Diagnose below.
      break;

    case ActorIsolation::GlobalActor:
    case ActorIsolation::GlobalActorUnsafe:
      // The global actors don't match; diagnose it.
      if (overriddenIsolation.getGlobalActor()->isEqual(
              isolation.getGlobalActor()))
        return;

      // Diagnose below.
      break;
    }
  }

  // If the overriding declaration uses an unsafe global actor, we can do
  // anything that doesn't actively conflict with the overridden isolation.
  if (isolation == ActorIsolation::GlobalActorUnsafe) {
    switch (overriddenIsolation) {
    case ActorIsolation::Unspecified:
      return;

    case ActorIsolation::ActorInstance:
    case ActorIsolation::Independent:
      // Diagnose below.
      break;

    case ActorIsolation::GlobalActor:
    case ActorIsolation::GlobalActorUnsafe:
      // The global actors don't match; diagnose it.
      if (overriddenIsolation.getGlobalActor()->isEqual(
              isolation.getGlobalActor()))
        return;

      // Diagnose below.
      break;
    }
  }

  // Isolation mismatch. Diagnose it.
  value->diagnose(
      diag::actor_isolation_override_mismatch, isolation,
      value->getDescriptiveKind(), value->getName(), overriddenIsolation);
  overridden->diagnose(diag::overridden_here);
}

bool swift::contextUsesConcurrencyFeatures(const DeclContext *dc) {
  while (!dc->isModuleScopeContext()) {
    if (auto closure = dyn_cast<AbstractClosureExpr>(dc)) {
      // A closure with an explicit global actor or @actorIndependent
      // uses concurrency features.
      if (auto explicitClosure = dyn_cast<ClosureExpr>(closure)) {
        if (explicitClosure->getAttrs().hasAttribute<ActorIndependentAttr>())
          return true;

        if (getExplicitGlobalActor(const_cast<ClosureExpr *>(explicitClosure)))
          return true;
      }

      // Async and concurrent closures use concurrency features.
      if (auto closureType = closure->getType()) {
        if (auto fnType = closureType->getAs<AnyFunctionType>())
          if (fnType->isAsync() || fnType->isSendable())
            return true;
      }
    } else if (auto decl = dc->getAsDecl()) {
      // If any isolation attributes are present, we're using concurrency
      // features.
      if (getIsolationFromAttributes(
              decl, /*shouldDiagnose=*/false, /*onlyExplicit=*/true))
        return true;

      if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
        // Async and concurrent functions use concurrency features.
        if (func->hasAsync() || func->isSendable())
          return true;

        // If we're in an accessor declaration, also check the storage
        // declaration.
        if (auto accessor = dyn_cast<AccessorDecl>(decl)) {
          if (getIsolationFromAttributes(
                  accessor->getStorage(), /*shouldDiagnose=*/false,
                  /*onlyExplicit=*/true))
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

static bool shouldDiagnoseExistingDataRaces(const DeclContext *dc) {
  if (dc->getASTContext().LangOpts.WarnConcurrency)
    return true;

  return contextUsesConcurrencyFeatures(dc);
}

static DiagnosticBehavior toDiagnosticBehavior(const LangOptions &langOpts,
                                               SendableCheck check,
                                               bool diagnoseImplicit = false) {
  switch (check) {
  case SendableCheck::ImpliedByStandardProtocol:
    return shouldDiagnoseNonSendableViolations(langOpts)
        ? DiagnosticBehavior::Warning
        : DiagnosticBehavior::Ignore;
  case SendableCheck::Explicit:
    return DiagnosticBehavior::Unspecified;
  case SendableCheck::Implicit:
    return (diagnoseImplicit &&
            shouldDiagnoseNonSendableViolations(langOpts))
      ? DiagnosticBehavior::Unspecified
      : DiagnosticBehavior::Ignore;
  }
}

/// Check the instance storage of the given nominal type to verify whether
/// it is comprised only of Sendable instance storage.
static bool checkSendableInstanceStorage(
    NominalTypeDecl *nominal, DeclContext *dc, SendableCheck check) {
  // Stored properties of structs and classes must have
  // Sendable-conforming types.
  const auto &langOpts = dc->getASTContext().LangOpts;
  auto behavior = toDiagnosticBehavior(langOpts, check);
  bool invalid = false;
  if (isa<StructDecl>(nominal) || isa<ClassDecl>(nominal)) {
    auto classDecl = dyn_cast<ClassDecl>(nominal);
    for (auto property : nominal->getStoredProperties()) {
      if (classDecl && property->supportsMutation()) {
        if (behavior == DiagnosticBehavior::Ignore)
          return true;
        property->diagnose(diag::concurrent_value_class_mutable_property,
                           property->getName(), nominal->getDescriptiveKind(),
                           nominal->getName())
            .limitBehavior(behavior);
        invalid = true;
        continue;
      }

      auto propertyType = dc->mapTypeIntoContext(property->getInterfaceType());
      if (!isSendableType(dc, propertyType)) {
        if (behavior == DiagnosticBehavior::Ignore)
          return true;
        property->diagnose(diag::non_concurrent_type_member,
                           false, property->getName(),
                           nominal->getDescriptiveKind(), nominal->getName(),
                           propertyType)
            .limitBehavior(behavior);
        invalid = true;
        continue;
      }
    }

    return invalid;
  }

  // Associated values of enum cases must have Sendable-conforming
  // types.
  if (auto enumDecl = dyn_cast<EnumDecl>(nominal)) {
    for (auto caseDecl : enumDecl->getAllCases()) {
      for (auto element : caseDecl->getElements()) {
        if (!element->hasAssociatedValues())
          continue;

        auto elementType = dc->mapTypeIntoContext(
            element->getArgumentInterfaceType());
        if (!isSendableType(dc, elementType)) {
          if (behavior == DiagnosticBehavior::Ignore)
            return true;
          element->diagnose(diag::non_concurrent_type_member,
                            true, element->getName(),
                            nominal->getDescriptiveKind(), nominal->getName(),
                            elementType)
              .limitBehavior(behavior);
          invalid = true;
          continue;
        }
      }
    }
  }

  return invalid;
}

bool swift::checkSendableConformance(
    ProtocolConformance *conformance, SendableCheck check) {
  auto conformanceDC = conformance->getDeclContext();
  auto nominal = conformance->getType()->getAnyNominal();
  if (!nominal)
    return false;

  auto classDecl = dyn_cast<ClassDecl>(nominal);
  if (classDecl) {
    // Actors implicitly conform to Sendable and protect their state.
    if (classDecl->isActor())
      return false;
  }

  // Sendable can only be used in the same source file.
  auto conformanceDecl = conformanceDC->getAsDecl();
  auto behavior = toDiagnosticBehavior(
      nominal->getASTContext().LangOpts, check, /*diagnoseImplicit=*/true);
  if (!conformanceDC->getParentSourceFile() ||
      conformanceDC->getParentSourceFile() != nominal->getParentSourceFile()) {
    conformanceDecl->diagnose(diag::concurrent_value_outside_source_file,
                              nominal->getDescriptiveKind(),
                              nominal->getName())
        .limitBehavior(behavior);

    if (behavior != DiagnosticBehavior::Warning)
      return true;
  }

  if (classDecl) {
    // An non-final class cannot conform to `Sendable`.
    if (!classDecl->isFinal()) {
      classDecl->diagnose(diag::concurrent_value_nonfinal_class,
                          classDecl->getName())
          .limitBehavior(behavior);

      if (behavior != DiagnosticBehavior::Warning)
        return true;
    }

    // A 'Sendable' class cannot inherit from another class, although
    // we allow `NSObject` for Objective-C interoperability.
    if (!isa<InheritedProtocolConformance>(conformance)) {
      if (auto superclassDecl = classDecl->getSuperclassDecl()) {
        if (!superclassDecl->isNSObject()) {
          classDecl->diagnose(
              diag::concurrent_value_inherit,
              nominal->getASTContext().LangOpts.EnableObjCInterop,
              classDecl->getName())
              .limitBehavior(behavior);

          if (behavior != DiagnosticBehavior::Warning)
            return true;
        }
      }
    }
  }

  return checkSendableInstanceStorage(nominal, conformanceDC, check);
}

NormalProtocolConformance *GetImplicitSendableRequest::evaluate(
    Evaluator &evaluator, NominalTypeDecl *nominal) const {
  // Only structs and enums can get implicit Sendable conformances.
  if (!isa<StructDecl>(nominal) && !isa<EnumDecl>(nominal))
    return nullptr;

  // Public, non-frozen structs and enums defined in Swift don't get implicit
  // Sendable conformances.
  if (!nominal->getASTContext().LangOpts.EnableInferPublicSendable &&
      nominal->getFormalAccessScope(
          /*useDC=*/nullptr,
          /*treatUsableFromInlineAsPublic=*/true).isPublic() &&
      !(nominal->hasClangNode() ||
        nominal->getAttrs().hasAttribute<FixedLayoutAttr>() ||
        nominal->getAttrs().hasAttribute<FrozenAttr>())) {
    return nullptr;
  }

  // Check the context in which the conformance occurs.
  if (auto *file = dyn_cast<FileUnit>(nominal->getModuleScopeContext())) {
    switch (file->getKind()) {
    case FileUnitKind::Source:
      // Check what kind of source file we have.
      if (auto sourceFile = nominal->getParentSourceFile()) {
        switch (sourceFile->Kind) {
        case SourceFileKind::Interface:
          // Interfaces have explicitly called-out Sendable conformances.
          return nullptr;

        case SourceFileKind::Library:
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

  // Check the instance storage for Sendable conformance.
  if (checkSendableInstanceStorage(
          nominal, nominal, SendableCheck::Implicit))
    return nullptr;

  ASTContext &ctx = nominal->getASTContext();
  auto proto = ctx.getProtocol(KnownProtocolKind::Sendable);
  if (!proto)
    return nullptr;

  auto conformance = ctx.getConformance(
      nominal->getDeclaredInterfaceType(), proto, nominal->getLoc(),
      nominal, ProtocolConformanceState::Complete);
  conformance->setSourceKindAndImplyingConformance(
      ConformanceEntryKind::Synthesized, nullptr);

  return conformance;
}

AnyFunctionType *swift::applyGlobalActorType(
    AnyFunctionType *fnType, ValueDecl *funcOrEnum, DeclContext *dc) {
  Type globalActorType;
  switch (auto isolation = getActorIsolation(funcOrEnum)) {
  case ActorIsolation::ActorInstance:
  case ActorIsolation::Independent:
  case ActorIsolation::Unspecified:
    return fnType;

  case ActorIsolation::GlobalActorUnsafe:
    // Only treat as global-actor-qualified within code that has adopted
    // Swift Concurrency features.
    if (!contextUsesConcurrencyFeatures(dc))
      return fnType;

    LLVM_FALLTHROUGH;

  case ActorIsolation::GlobalActor:
    globalActorType = isolation.getGlobalActor();
    break;
  }

  // If there's no implicit "self" declaration, apply the global actor to
  // the outermost function type.
  bool hasImplicitSelfDecl = isa<EnumElementDecl>(funcOrEnum) ||
      (isa<AbstractFunctionDecl>(funcOrEnum) &&
       cast<AbstractFunctionDecl>(funcOrEnum)->hasImplicitSelfDecl());
  if (!hasImplicitSelfDecl) {
    return fnType->withExtInfo(
        fnType->getExtInfo().withGlobalActor(globalActorType));
  }

  // Dig out the inner function type.
  auto innerFnType = fnType->getResult()->getAs<AnyFunctionType>();
  if (!innerFnType)
    return fnType;

  // Update the inner function type with the global actor.
  innerFnType = innerFnType->withExtInfo(
      innerFnType->getExtInfo().withGlobalActor(globalActorType));

  // Rebuild the outer function type around it.
  if (auto genericFnType = dyn_cast<GenericFunctionType>(fnType)) {
    return GenericFunctionType::get(
        genericFnType->getGenericSignature(), fnType->getParams(),
        Type(innerFnType), fnType->getExtInfo());
  }

  return FunctionType::get(
      fnType->getParams(), Type(innerFnType), fnType->getExtInfo());
}
