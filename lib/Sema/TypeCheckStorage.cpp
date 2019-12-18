//===--- TypeCheckStorage.cpp - Checking Properties and Subscripts -------===//
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
// This file implements semantic analysis for properties, subscripts as well
// as their accessors.
//
//===----------------------------------------------------------------------===//

#include "CodeSynthesis.h"
#include "TypeChecker.h"
#include "TypeCheckAvailability.h"
#include "TypeCheckDecl.h"
#include "TypeCheckType.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/PropertyWrappers.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/Types.h"
using namespace swift;

/// Set each bound variable in the pattern to have an error type.
void swift::setBoundVarsTypeError(Pattern *pattern, ASTContext &ctx) {
  pattern->forEachVariable([&](VarDecl *var) {
    // Don't change the type of a variable that we've been able to
    // compute a type for.
    if (var->hasInterfaceType())
      return;

    var->setInvalid();
  });
}

/// Build a default initializer for the given type.
Expr *TypeChecker::buildDefaultInitializer(Type type) {
  auto &Context = type->getASTContext();
  // Default-initialize optional types and weak values to 'nil'.
  if (type->getReferenceStorageReferent()->getOptionalObjectType())
    return new (Context) NilLiteralExpr(SourceLoc(), /*Implicit=*/true);

  // Build tuple literals for tuple types.
  if (auto tupleType = type->getAs<TupleType>()) {
    SmallVector<Expr *, 2> inits;
    for (const auto &elt : tupleType->getElements()) {
      if (elt.isVararg())
        return nullptr;

      auto eltInit = TypeChecker::buildDefaultInitializer(elt.getType());
      if (!eltInit)
        return nullptr;

      inits.push_back(eltInit);
    }

    return TupleExpr::createImplicit(Context, inits, { });
  }

  // We don't default-initialize anything else.
  return nullptr;
}

/// Does the context allow pattern bindings that don't bind any variables?
static bool contextAllowsPatternBindingWithoutVariables(DeclContext *dc) {
  
  // Property decls in type context must bind variables.
  if (dc->isTypeContext())
    return false;
  
  // Global variable decls must bind variables, except in scripts.
  if (dc->isModuleScopeContext()) {
    if (dc->getParentSourceFile()
        && dc->getParentSourceFile()->isScriptMode())
      return true;
    
    return false;
  }
  
  return true;
}

static bool hasStoredProperties(NominalTypeDecl *decl) {
  return (isa<StructDecl>(decl) ||
          (isa<ClassDecl>(decl) && !decl->hasClangNode()));
}

static void computeLoweredStoredProperties(NominalTypeDecl *decl) {
  // Just walk over the members of the type, forcing backing storage
  // for lazy properties and property wrappers to be synthesized.
  for (auto *member : decl->getMembers()) {
    auto *var = dyn_cast<VarDecl>(member);
    if (!var || var->isStatic())
      continue;

    if (var->getAttrs().hasAttribute<LazyAttr>())
      (void) var->getLazyStorageProperty();

    if (var->hasAttachedPropertyWrapper())
      (void) var->getPropertyWrapperBackingProperty();
  }
}

llvm::Expected<ArrayRef<VarDecl *>>
StoredPropertiesRequest::evaluate(Evaluator &evaluator,
                                  NominalTypeDecl *decl) const {
  if (!hasStoredProperties(decl))
    return ArrayRef<VarDecl *>();

  SmallVector<VarDecl *, 4> results;

  // Unless we're in a source file we don't have to do anything
  // special to lower lazy properties and property wrappers.
  if (isa<SourceFile>(decl->getModuleScopeContext()))
    computeLoweredStoredProperties(decl);

  for (auto *member : decl->getMembers()) {
    if (auto *var = dyn_cast<VarDecl>(member))
      if (!var->isStatic() && var->hasStorage())
        results.push_back(var);
  }

  return decl->getASTContext().AllocateCopy(results);
}

llvm::Expected<ArrayRef<Decl *>>
StoredPropertiesAndMissingMembersRequest::evaluate(Evaluator &evaluator,
                                                   NominalTypeDecl *decl) const {
  if (!hasStoredProperties(decl))
    return ArrayRef<Decl *>();

  SmallVector<Decl *, 4> results;

  // Unless we're in a source file we don't have to do anything
  // special to lower lazy properties and property wrappers.
  if (isa<SourceFile>(decl->getModuleScopeContext()))
    computeLoweredStoredProperties(decl);

  for (auto *member : decl->getMembers()) {
    if (auto *var = dyn_cast<VarDecl>(member))
      if (!var->isStatic() && var->hasStorage())
        results.push_back(var);

    if (auto missing = dyn_cast<MissingMemberDecl>(member))
      if (missing->getNumberOfFieldOffsetVectorEntries() > 0)
        results.push_back(missing);
  }

  return decl->getASTContext().AllocateCopy(results);
}

/// Validate the \c entryNumber'th entry in \c binding.
llvm::Expected<const PatternBindingEntry *>
PatternBindingEntryRequest::evaluate(Evaluator &eval,
                                     PatternBindingDecl *binding,
                                     unsigned entryNumber) const {
  const auto &pbe = binding->getPatternList()[entryNumber];
  auto &Context = binding->getASTContext();

  // Resolve the pattern.
  auto *pattern = TypeChecker::resolvePattern(binding->getPattern(entryNumber),
                                              binding->getDeclContext(),
                                              /*isStmtCondition*/ true);
  if (!pattern) {
    binding->setInvalid();
    binding->getPattern(entryNumber)->setType(ErrorType::get(Context));
    return &pbe;
  }

  binding->setPattern(entryNumber, pattern,
                      binding->getInitContext(entryNumber));

  // Validate 'static'/'class' on properties in nominal type decls.
  auto StaticSpelling = binding->getStaticSpelling();
  if (StaticSpelling != StaticSpellingKind::None &&
      isa<ExtensionDecl>(binding->getDeclContext())) {
    if (auto *NTD = binding->getDeclContext()->getSelfNominalTypeDecl()) {
      if (!isa<ClassDecl>(NTD)) {
        if (StaticSpelling == StaticSpellingKind::KeywordClass) {
          binding->diagnose(diag::class_var_not_in_class, false)
              .fixItReplace(binding->getStaticLoc(), "static");
          NTD->diagnose(diag::extended_type_declared_here);
        }
      }
    }
  }

  // Check the pattern. We treat type-checking a PatternBindingDecl like
  // type-checking an expression because that's how the initial binding is
  // checked, and they have the same effect on the file's dependencies.
  //
  // In particular, it's /not/ correct to check the PBD's DeclContext because
  // top-level variables in a script file are accessible from other files,
  // even though the PBD is inside a TopLevelCodeDecl.
  TypeResolutionOptions options(TypeResolverContext::PatternBindingDecl);

  if (binding->isInitialized(entryNumber)) {
    // If we have an initializer, we can also have unknown types.
    options |= TypeResolutionFlags::AllowUnspecifiedTypes;
    options |= TypeResolutionFlags::AllowUnboundGenerics;
  }

  if (TypeChecker::typeCheckPattern(pattern, binding->getDeclContext(), options)) {
    swift::setBoundVarsTypeError(pattern, Context);
    binding->setInvalid();
    pattern->setType(ErrorType::get(Context));
    return &pbe;
  }

  // If we have a type but no initializer, check whether the type is
  // default-initializable. If so, do it.
  if (!pbe.isInitialized() &&
      binding->isDefaultInitializable(entryNumber) &&
      pattern->hasStorage() &&
      !pattern->getType()->hasError()) {
    auto type = pattern->getType();
    if (auto defaultInit = TypeChecker::buildDefaultInitializer(type)) {
      // If we got a default initializer, install it and re-type-check it
      // to make sure it is properly coerced to the pattern type.
      binding->setInit(entryNumber, defaultInit);
    }
  }

  // If the pattern didn't get a type or if it contains an unbound generic type,
  // we'll need to check the initializer.
  if (!pattern->hasType() || pattern->getType()->hasUnboundGenericType()) {
    if (TypeChecker::typeCheckPatternBinding(binding, entryNumber)) {
      binding->setInvalid();
      return &pbe;
    }

    // A pattern binding at top level is not allowed to pick up another decl's
    // opaque result type as its type by type inference.
    if (!binding->getDeclContext()->isLocalContext() &&
        binding->getInit(entryNumber)->getType()->hasOpaqueArchetype()) {
      // TODO: Check whether the type is the pattern binding's own opaque type.
      binding->diagnose(diag::inferred_opaque_type,
                        binding->getInit(entryNumber)->getType());
    }
  }

  // If the pattern binding appears in a type or library file context, then
  // it must bind at least one variable.
  if (!contextAllowsPatternBindingWithoutVariables(binding->getDeclContext())) {
    llvm::SmallVector<VarDecl *, 2> vars;
    binding->getPattern(entryNumber)->collectVariables(vars);
    if (vars.empty()) {
      // Selector for error message.
      enum : unsigned {
        Property,
        GlobalVariable,
      };
      Context.Diags.diagnose(binding->getPattern(entryNumber)->getLoc(),
                             diag::pattern_binds_no_variables,
                             binding->getDeclContext()->isTypeContext()
                                 ? Property
                                 : GlobalVariable);
    }
  }
  return &pbe;
}

llvm::Expected<bool>
IsGetterMutatingRequest::evaluate(Evaluator &evaluator,
                                  AbstractStorageDecl *storage) const {
  auto storageDC = storage->getDeclContext();
  bool result = (!storage->isStatic() && storageDC->isTypeContext() &&
                 storageDC->hasValueSemantics());

  // 'lazy' overrides the normal accessor-based rules and heavily
  // restricts what accessors can be used.  The getter is considered
  // mutating if this is instance storage on a value type.
  if (storage->getAttrs().hasAttribute<LazyAttr>()) {
    return result;
  }

  // If we have an attached property wrapper, the getter's mutating-ness
  // depends on the composition of the wrappers.
  if (auto var = dyn_cast<VarDecl>(storage)) {
    if (auto mut = var->getPropertyWrapperMutability()) {
      return mut->Getter == PropertyWrapperMutability::Mutating
        && result;
    }
  }

  auto checkMutability = [&](AccessorKind kind) -> bool {
    auto *accessor = storage->getParsedAccessor(kind);
    if (!accessor)
      return false;

    return accessor->isMutating();
  };

  // Protocol requirements are always written as '{ get }' or '{ get set }';
  // the @_borrowed attribute determines if getReadImpl() becomes Get or Read.
  if (isa<ProtocolDecl>(storageDC))
    return checkMutability(AccessorKind::Get);

  switch (storage->getReadImpl()) {
  case ReadImplKind::Stored:
  case ReadImplKind::Inherited:
    return false;

  case ReadImplKind::Get:
    return checkMutability(AccessorKind::Get);

  case ReadImplKind::Address:
    return checkMutability(AccessorKind::Address);

  case ReadImplKind::Read:
    return checkMutability(AccessorKind::Read);
  }

  llvm_unreachable("bad impl kind");
}

llvm::Expected<bool>
IsSetterMutatingRequest::evaluate(Evaluator &evaluator,
                                  AbstractStorageDecl *storage) const {
  // By default, the setter is mutating if we have an instance member of a
  // value type, but this can be overridden below.
  auto storageDC = storage->getDeclContext();
  bool result = (!storage->isStatic() && storageDC->isTypeContext() &&
                 storageDC->hasValueSemantics());

  // If we have an attached property wrapper, the setter is mutating
  // or not based on the composition of the wrappers.
  if (auto var = dyn_cast<VarDecl>(storage)) {
    if (auto mut = var->getPropertyWrapperMutability()) {
      return mut->Setter == PropertyWrapperMutability::Mutating
        && result;
    }
  }

  auto impl = storage->getImplInfo();
  switch (impl.getWriteImpl()) {
  case WriteImplKind::Immutable:
  case WriteImplKind::Stored:
    // Instance member setters are mutating; static property setters and
    // top-level setters are not.
    // It's important that we use this logic for "immutable" storage
    // in order to handle initialization of let-properties.
    return result;

  case WriteImplKind::StoredWithObservers:
  case WriteImplKind::InheritedWithObservers:
  case WriteImplKind::Set: {
    auto *setter = storage->getParsedAccessor(AccessorKind::Set);

    if (setter)
      result = setter->isMutating();


    // As a special extra check, if the user also gave us a modify
    // coroutine, check that it has the same mutatingness as the setter.
    // TODO: arguably this should require the spelling to match even when
    // it's the implied value.
    auto modifyAccessor = storage->getParsedAccessor(AccessorKind::Modify);

    if (impl.getReadWriteImpl() == ReadWriteImplKind::Modify &&
        modifyAccessor != nullptr) {
      auto modifyResult = modifyAccessor->isMutating();
      if ((result || storage->isGetterMutating()) != modifyResult) {
        modifyAccessor->diagnose(
            diag::modify_mutatingness_differs_from_setter,
            modifyResult ? SelfAccessKind::Mutating
                         : SelfAccessKind::NonMutating,
            modifyResult ? SelfAccessKind::NonMutating
                         : SelfAccessKind::Mutating);
        if (setter)
          setter->diagnose(diag::previous_accessor, "setter", 0);
        modifyAccessor->setInvalid();
      }
    }

    return result;
  }

  case WriteImplKind::MutableAddress:
    return storage->getParsedAccessor(AccessorKind::MutableAddress)
      ->isMutating();

  case WriteImplKind::Modify:
    return storage->getParsedAccessor(AccessorKind::Modify)
      ->isMutating();
  }
  llvm_unreachable("bad storage kind");
}

llvm::Expected<OpaqueReadOwnership>
OpaqueReadOwnershipRequest::evaluate(Evaluator &evaluator,
                                     AbstractStorageDecl *storage) const {
  return (storage->getAttrs().hasAttribute<BorrowedAttr>()
          ? OpaqueReadOwnership::Borrowed
          : OpaqueReadOwnership::Owned);
}

/// Insert the specified decl into the DeclContext's member list.  If the hint
/// decl is specified, the new decl is inserted next to the hint.
static void addMemberToContextIfNeeded(Decl *D, DeclContext *DC,
                                       Decl *Hint = nullptr) {
  if (auto *ntd = dyn_cast<NominalTypeDecl>(DC)) {
    ntd->addMember(D, Hint);
  } else if (auto *ed = dyn_cast<ExtensionDecl>(DC)) {
    ed->addMember(D, Hint);
  } else {
    assert((DC->isLocalContext() || isa<FileUnit>(DC)) &&
           "Unknown declcontext");
  }
}

/// Build a parameter list which can forward the formal index parameters of a
/// declaration.
///
/// \param prefix optional arguments to be prefixed onto the index
///   forwarding pattern.
static ParameterList *
buildIndexForwardingParamList(AbstractStorageDecl *storage,
                              ArrayRef<ParamDecl*> prefix,
                              ASTContext &context) {
  auto subscript = dyn_cast<SubscriptDecl>(storage);

  // Fast path: if this isn't a subscript, just use whatever we have.
  if (!subscript)
    return ParameterList::create(context, prefix);

  // Clone the parameter list over for a new decl, so we get new ParamDecls.
  auto indices = subscript->getIndices()->clone(context,
                                                ParameterList::Implicit);

  // Give all of the parameters meaningless names so that we can forward
  // them properly.  If it's declared anonymously, SILGen will think
  // it's unused.
  // TODO: use some special DeclBaseName for this?
  for (auto param : indices->getArray()) {
    if (!param->hasName())
      param->setName(context.getIdentifier("anonymous"));
    assert(param->hasName());
  }

  if (prefix.empty())
    return indices;
  
  
  // Otherwise, we need to build up a new parameter list.
  SmallVector<ParamDecl*, 4> elements;

  // Start with the fields we were given, if there are any.
  elements.append(prefix.begin(), prefix.end());
  elements.append(indices->begin(), indices->end());
  return ParameterList::create(context, elements);
}

/// Create the generic parameters needed for the given accessor, if any.
static GenericParamList *createAccessorGenericParams(
                                              AbstractStorageDecl *storage) {
  // Accessors of generic subscripts get a copy of the subscript's
  // generic parameter list, because they're not nested inside the
  // subscript.
  if (auto *subscript = dyn_cast<SubscriptDecl>(storage)) {
    if (auto genericParams = subscript->getGenericParams())
      return genericParams->clone(subscript->getDeclContext());
  }

  return nullptr;
}

static bool doesAccessorHaveBody(AccessorDecl *accessor) {
  // Protocol requirements don't have bodies.
  //
  // FIXME: Revisit this if we ever get 'real' default implementations.
  if (isa<ProtocolDecl>(accessor->getDeclContext()))
    return false;

  auto *storage = accessor->getStorage();

  // NSManaged getters and setters don't have bodies.
  if (storage->getAttrs().hasAttribute<NSManagedAttr>())
    if (accessor->isGetterOrSetter())
      return false;

  return true;
}


/// Build a reference to the subscript index variables for this subscript
/// accessor.
static Expr *buildSubscriptIndexReference(ASTContext &ctx,
                                          AccessorDecl *accessor) {
  // Pull out the body parameters, which we should have cloned
  // previously to be forwardable.  Drop the initial buffer/value
  // parameter in accessors that have one.
  auto params = accessor->getParameters()->getArray();
  auto accessorKind = accessor->getAccessorKind();

  // Ignore the value parameter of a setter.
  if (accessorKind == AccessorKind::Set) {
    params = params.slice(1);
  }

  // Okay, everything else should be forwarded, build the expression.
  return buildArgumentForwardingExpr(params, ctx);
}

namespace {
  enum class TargetImpl {
    /// We're doing an ordinary storage reference.
    Ordinary,
    /// We're referencing the physical storage created for the storage.
    Storage,
    /// We're referencing this specific implementation of the storage, not
    /// an override of it.
    Implementation,
    /// We're referencing the superclass's implementation of the storage.
    Super,
    /// We're referencing the backing property for a property with a wrapper
    /// through the 'value' property.
    Wrapper,
    /// We're referencing the backing property for a property with a wrapper
    /// through the 'projectedValue' property.
    WrapperStorage,
  };
} // end anonymous namespace

namespace  {
  /// Describes the information needed to perform property wrapper access via
  /// the enclosing self.
  struct EnclosingSelfPropertyWrapperAccess {
    /// The (genreric) subscript that will be used to perform the access.
    SubscriptDecl *subscript;

    /// The property being accessed.
    VarDecl *accessedProperty;
  };
}

/// Determine whether the given property should be accessed via the enclosing-self access pattern.
static Optional<EnclosingSelfPropertyWrapperAccess>
getEnclosingSelfPropertyWrapperAccess(VarDecl *property, bool forProjected) {
  // The enclosing-self pattern only applies to instance properties of
  // classes.
  if (!property->isInstanceMember())
    return None;
  auto classDecl = property->getDeclContext()->getSelfClassDecl();
  if (!classDecl)
    return None;

  // The pattern currently only works with the outermost property wrapper.
  Type outermostWrapperType = property->getPropertyWrapperBackingPropertyType();
  if (!outermostWrapperType)
    return None;
  NominalTypeDecl *wrapperTypeDecl = outermostWrapperType->getAnyNominal();
  if (!wrapperTypeDecl)
    return None;

  // Look for a generic subscript that fits the general form we need.
  auto wrapperInfo = wrapperTypeDecl->getPropertyWrapperTypeInfo();
  auto subscript =
      forProjected ? wrapperInfo.enclosingInstanceProjectedSubscript
                   : wrapperInfo.enclosingInstanceWrappedSubscript;
  if (!subscript)
    return None;

  EnclosingSelfPropertyWrapperAccess result;
  result.subscript = subscript;

  if (forProjected) {
    result.accessedProperty =
        property->getPropertyWrapperBackingPropertyInfo().storageWrapperVar;
  } else {
    result.accessedProperty = property;
  }
  return result;
}

/// Build an l-value for the storage of a declaration.
static Expr *buildStorageReference(AccessorDecl *accessor,
                                   AbstractStorageDecl *storage,
                                   TargetImpl target,
                                   bool isLValue,
                                   ASTContext &ctx) {
  // Local function to "finish" the expression, creating a member reference
  // to the given sequence of underlying variables.
  Optional<EnclosingSelfPropertyWrapperAccess> enclosingSelfAccess;
  llvm::TinyPtrVector<VarDecl *> underlyingVars;
  auto finish = [&](Expr *result) -> Expr * {
    for (auto underlyingVar : underlyingVars) {
      auto subs = result->getType()
          ->getWithoutSpecifierType()
          ->getContextSubstitutionMap(
            accessor->getParentModule(),
            underlyingVar->getDeclContext());

      ConcreteDeclRef memberRef(underlyingVar, subs);
      auto *memberRefExpr = new (ctx) MemberRefExpr(
          result, SourceLoc(), memberRef, DeclNameLoc(), /*Implicit=*/true);
      auto type = underlyingVar->getValueInterfaceType().subst(subs);
      if (isLValue)
        type = LValueType::get(type);
      memberRefExpr->setType(type);
      
      result = memberRefExpr;
    }
    
    return result;
  };

  VarDecl *selfDecl = accessor->getImplicitSelfDecl();

  AccessSemantics semantics;
  SelfAccessorKind selfAccessKind;
  Type selfTypeForAccess = (selfDecl ? selfDecl->getType() : Type());

  auto *genericEnv = accessor->getGenericEnvironment();
  SubstitutionMap subs;
  if (genericEnv)
    subs = genericEnv->getForwardingSubstitutionMap();

  switch (target) {
  case TargetImpl::Ordinary:
    semantics = AccessSemantics::Ordinary;
    selfAccessKind = SelfAccessorKind::Peer;
    break;

  case TargetImpl::Storage:
    semantics = AccessSemantics::DirectToStorage;
    selfAccessKind = SelfAccessorKind::Peer;
    break;

  case TargetImpl::Implementation:
    semantics = AccessSemantics::DirectToImplementation;
    selfAccessKind = SelfAccessorKind::Peer;
    break;

  case TargetImpl::Super:
    // If this really is an override, use a super-access.
    if (auto override = storage->getOverriddenDecl()) {
      semantics = AccessSemantics::Ordinary;
      selfAccessKind = SelfAccessorKind::Super;

      auto *baseClass = override->getDeclContext()->getSelfClassDecl();
      selfTypeForAccess = selfTypeForAccess->getSuperclassForDecl(baseClass);
      subs =
        selfTypeForAccess->getContextSubstitutionMap(
          accessor->getParentModule(),
          baseClass);

      storage = override;

    // Otherwise do a self-reference, which is dynamically bogus but
    // should be statically valid.  This should only happen in invalid cases.    
    } else {
      semantics = AccessSemantics::Ordinary;
      selfAccessKind = SelfAccessorKind::Peer;
    }
    break;

  case TargetImpl::Wrapper: {
    auto var = cast<VarDecl>(accessor->getStorage());
    auto *backing = var->getPropertyWrapperBackingProperty();

    // Error recovery.
    if (!backing) {
      auto type = storage->getValueInterfaceType();
      if (isLValue)
        type = LValueType::get(type);
      return new (ctx) ErrorExpr(SourceRange(), type);
    }

    storage = backing;

    // If the outermost property wrapper uses the enclosing self pattern,
    // record that.
    unsigned lastWrapperIdx = var->getAttachedPropertyWrappers().size();
    unsigned firstWrapperIdx = 0;
    enclosingSelfAccess =
        getEnclosingSelfPropertyWrapperAccess(var, /*forProjected=*/false);
    if (enclosingSelfAccess)
      firstWrapperIdx = 1;

    // Perform accesses to the wrappedValues along the composition chain.
    for (unsigned i : range(firstWrapperIdx, lastWrapperIdx)) {
      auto wrapperInfo = var->getAttachedPropertyWrapperTypeInfo(i);
      auto wrappedValue = wrapperInfo.valueVar;

      // Check for availability of wrappedValue.
      if (accessor->getAccessorKind() == AccessorKind::Get ||
          accessor->getAccessorKind() == AccessorKind::Read) {
        if (auto *attr = wrappedValue->getAttrs().getUnavailable(ctx)) {
          diagnoseExplicitUnavailability(
              wrappedValue,
              var->getAttachedPropertyWrappers()[i]->getRangeWithAt(),
              var->getDeclContext(), nullptr);
        }
      }

      underlyingVars.push_back(wrappedValue);
    }
    semantics = AccessSemantics::DirectToStorage;
    selfAccessKind = SelfAccessorKind::Peer;
    break;
  }

  case TargetImpl::WrapperStorage: {
    auto var =
        cast<VarDecl>(accessor->getStorage())->getOriginalWrappedProperty();
    auto *backing = var->getPropertyWrapperBackingProperty();

    // Error recovery.
    if (!backing) {
      auto type = storage->getValueInterfaceType();
      if (isLValue)
        type = LValueType::get(type);
      return new (ctx) ErrorExpr(SourceRange(), type);
    }

    storage = backing;

    enclosingSelfAccess =
        getEnclosingSelfPropertyWrapperAccess(var, /*forProjected=*/true);
    if (!enclosingSelfAccess) {
      underlyingVars.push_back(
        var->getAttachedPropertyWrapperTypeInfo(0).projectedValueVar);
    }
    semantics = AccessSemantics::DirectToStorage;
    selfAccessKind = SelfAccessorKind::Peer;
    break;
  }
  }

  if (!selfDecl) {
    assert(target != TargetImpl::Super);
    auto *storageDRE = new (ctx) DeclRefExpr(storage, DeclNameLoc(),
                                             /*IsImplicit=*/true, semantics);
    auto type = storage->getValueInterfaceType().subst(subs);
    if (isLValue)
      type = LValueType::get(type);
    storageDRE->setType(type);

    return finish(storageDRE);
  }

  bool isMemberLValue = isLValue;
  auto propertyWrapperMutability =
      [&](Decl *decl) -> Optional<std::pair<bool, bool>> {
    auto var = dyn_cast<VarDecl>(decl);
    if (!var)
      return None;
    auto mut = var->getPropertyWrapperMutability();
    if (!mut)
      return None;
    return std::make_pair(mut->Getter == PropertyWrapperMutability::Mutating,
                          mut->Setter == PropertyWrapperMutability::Mutating);
  };

  // If we're accessing a property wrapper, determine if the
  // intermediate access requires an lvalue.
  if (auto mut = propertyWrapperMutability(accessor->getStorage())) {
    isMemberLValue = mut->first;
    if (isLValue)
      isMemberLValue |= mut->second;
  }

  bool isSelfLValue = storage->isGetterMutating();
  if (isMemberLValue)
    isSelfLValue |= storage->isSetterMutating();

  // If we're accessing a property wrapper, determine if
  // the self requires an lvalue.
  if (auto mut = propertyWrapperMutability(storage)) {
    isSelfLValue = mut->first;
    if (isMemberLValue)
      isSelfLValue |= mut->second;
  }

  Expr *selfDRE =
    buildSelfReference(selfDecl, selfAccessKind, isSelfLValue,
                       ctx);
  if (isSelfLValue)
    selfTypeForAccess = LValueType::get(selfTypeForAccess);

  if (!selfDRE->getType()->isEqual(selfTypeForAccess)) {
    assert(selfAccessKind == SelfAccessorKind::Super);
    selfDRE = new (ctx) DerivedToBaseExpr(selfDRE, selfTypeForAccess);
  }

  Expr *lookupExpr;
  ConcreteDeclRef memberRef(storage, subs);
  auto type = storage->getValueInterfaceType().subst(subs);
  if (isMemberLValue)
    type = LValueType::get(type);

  // When we are performing access via a property wrapper's static subscript
  // that accepts the enclosing self along with key paths, form that subscript
  // operation now.
  if (enclosingSelfAccess) {
    Type storageType = storage->getValueInterfaceType().subst(subs);
    // Metatype instance for the wrapper type itself.
    TypeExpr *wrapperMetatype = TypeExpr::createImplicit(storageType, ctx);

    // Key path referring to the property being accessed.
    Expr *propertyKeyPath = new (ctx) KeyPathDotExpr(SourceLoc());
    propertyKeyPath = UnresolvedDotExpr::createImplicit(ctx, propertyKeyPath,
        enclosingSelfAccess->accessedProperty->getFullName());
    propertyKeyPath = new (ctx) KeyPathExpr(
        SourceLoc(), nullptr, propertyKeyPath);

    // Key path referring to the backing storage property.
    Expr *storageKeyPath = new (ctx) KeyPathDotExpr(SourceLoc());
    storageKeyPath = UnresolvedDotExpr::createImplicit(ctx, storageKeyPath,
                                                       storage->getFullName());
    storageKeyPath = new (ctx) KeyPathExpr(
        SourceLoc(), nullptr, storageKeyPath);
    Expr *args[3] = {
      selfDRE,
      propertyKeyPath,
      storageKeyPath
    };

    SubscriptDecl *subscriptDecl = enclosingSelfAccess->subscript;
    lookupExpr = SubscriptExpr::create(
        ctx, wrapperMetatype, SourceLoc(), args,
        subscriptDecl->getFullName().getArgumentNames(), { }, SourceLoc(),
        nullptr, subscriptDecl, /*Implicit=*/true);
    TypeChecker::typeCheckExpression(lookupExpr, accessor);

    // Make sure we produce an lvalue only when desired.
    if (isMemberLValue != lookupExpr->getType()->is<LValueType>()) {
      if (isMemberLValue) {
        // Strip off an extraneous load.
        if (auto load = dyn_cast<LoadExpr>(lookupExpr))
          lookupExpr = load->getSubExpr();
      } else {
        lookupExpr = new (ctx) LoadExpr(
            lookupExpr, lookupExpr->getType()->getRValueType());
      }
    }
  } else if (auto subscript = dyn_cast<SubscriptDecl>(storage)) {
    Expr *indices = buildSubscriptIndexReference(ctx, accessor);
    lookupExpr = SubscriptExpr::create(ctx, selfDRE, indices, memberRef,
                                       /*IsImplicit=*/true, semantics);

    if (selfAccessKind == SelfAccessorKind::Super)
      cast<LookupExpr>(lookupExpr)->setIsSuper(true);

    lookupExpr->setType(type);

  } else {
    lookupExpr = new (ctx) MemberRefExpr(selfDRE, SourceLoc(), memberRef,
                                         DeclNameLoc(), /*IsImplicit=*/true,
                                         semantics);

    if (selfAccessKind == SelfAccessorKind::Super)
      cast<LookupExpr>(lookupExpr)->setIsSuper(true);

    lookupExpr->setType(type);
  }

  return finish(lookupExpr);
}

/// Load the value of VD.  If VD is an @override of another value, we call the
/// superclass getter.  Otherwise, we do a direct load of the value.
static Expr *
createPropertyLoadOrCallSuperclassGetter(AccessorDecl *accessor,
                                         AbstractStorageDecl *storage,
                                         TargetImpl target,
                                         ASTContext &ctx) {
  return buildStorageReference(accessor, storage, target, /*isLValue=*/false,
                               ctx);
}

static ProtocolConformanceRef checkConformanceToNSCopying(VarDecl *var,
                                                          Type type) {
  auto dc = var->getDeclContext();
  auto &ctx = dc->getASTContext();
  auto proto = ctx.getNSCopyingDecl();

  if (proto) {
    if (auto result = TypeChecker::conformsToProtocol(type, proto, dc, None))
      return result;
  }

  ctx.Diags.diagnose(var->getLoc(), diag::nscopying_doesnt_conform);
  return ProtocolConformanceRef::forInvalid();
}

static std::pair<Type, bool> getUnderlyingTypeOfVariable(VarDecl *var) {
  Type type = var->getType()->getReferenceStorageReferent();

  if (Type objectType = type->getOptionalObjectType()) {
    return {objectType, true};
  } else {
    return {type, false};
  }
}

ProtocolConformanceRef TypeChecker::checkConformanceToNSCopying(VarDecl *var) {
  Type type = getUnderlyingTypeOfVariable(var).first;
  return ::checkConformanceToNSCopying(var, type);
}

/// Synthesize the code to store 'Val' to 'VD', given that VD has an @NSCopying
/// attribute on it.  We know that VD is a stored property in a class, so we
/// just need to generate something like "self.property = val.copy(zone: nil)"
/// here.  This does some type checking to validate that the call will succeed.
static Expr *synthesizeCopyWithZoneCall(Expr *Val, VarDecl *VD,
                                        ASTContext &Ctx) {
  // We support @NSCopying on class types (which conform to NSCopying),
  // protocols which conform, and option types thereof.
  auto underlyingTypeAndIsOptional = getUnderlyingTypeOfVariable(VD);
  auto underlyingType = underlyingTypeAndIsOptional.first;
  auto isOptional = underlyingTypeAndIsOptional.second;

  // The element type must conform to NSCopying.  If not, emit an error and just
  // recovery by synthesizing without the copy call.
  auto conformance = checkConformanceToNSCopying(VD, underlyingType);
  if (!conformance)
    return Val;

  //- (id)copyWithZone:(NSZone *)zone;
  DeclName copyWithZoneName(Ctx, Ctx.getIdentifier("copy"), { Ctx.Id_with });
  FuncDecl *copyMethod = nullptr;
  for (auto member : conformance.getRequirement()->getMembers()) {
    if (auto func = dyn_cast<FuncDecl>(member)) {
      if (func->getFullName() == copyWithZoneName) {
        copyMethod = func;
        break;
      }
    }
  }
  assert(copyMethod != nullptr);

  // If we have an optional type, we have to "?" the incoming value to only
  // evaluate the subexpression if the incoming value is non-null.
  if (isOptional) {
    Val = new (Ctx) BindOptionalExpr(Val, SourceLoc(), 0);
    Val->setType(underlyingType);
  }

  SubstitutionMap subs =
      SubstitutionMap::get(copyMethod->getGenericSignature(), {underlyingType},
                           ArrayRef<ProtocolConformanceRef>(conformance));
  ConcreteDeclRef copyMethodRef(copyMethod, subs);
  auto copyMethodType = copyMethod->getInterfaceType()
                           ->castTo<GenericFunctionType>()
                           ->substGenericArgs(subs);
  auto DRE = new (Ctx) DeclRefExpr(copyMethodRef, DeclNameLoc(),
                                   /*IsImplicit=*/true);
  DRE->setType(copyMethodType);

  // Drop the self type
  copyMethodType = copyMethodType->getResult()->castTo<FunctionType>();

  auto DSCE = new (Ctx) DotSyntaxCallExpr(DRE, SourceLoc(), Val);
  DSCE->setImplicit();
  DSCE->setType(copyMethodType);
  DSCE->setThrows(false);

  Expr *Nil = new (Ctx) NilLiteralExpr(SourceLoc(), /*implicit*/true);
  Nil->setType(copyMethodType->getParams()[0].getParameterType());

  auto *Call = CallExpr::createImplicit(Ctx, DSCE, { Nil }, { Ctx.Id_with });
  Call->setType(copyMethodType->getResult());
  Call->setThrows(false);

  TypeLoc ResultTy;
  ResultTy.setType(VD->getType());

  // If we're working with non-optional types, we're forcing the cast.
  if (!isOptional) {
    auto *Cast =
      new (Ctx) ForcedCheckedCastExpr(Call, SourceLoc(), SourceLoc(),
                                      TypeLoc::withoutLoc(underlyingType));
    Cast->setCastKind(CheckedCastKind::ValueCast);
    Cast->setType(underlyingType);
    Cast->setImplicit();

    return Cast;
  }

  // We're working with optional types, so perform a conditional checked
  // downcast.
  auto *Cast =
    new (Ctx) ConditionalCheckedCastExpr(Call, SourceLoc(), SourceLoc(),
                                         TypeLoc::withoutLoc(underlyingType));
  Cast->setCastKind(CheckedCastKind::ValueCast);
  Cast->setType(OptionalType::get(underlyingType));
  Cast->setImplicit();

  // Use OptionalEvaluationExpr to evaluate the "?".
  auto *Result = new (Ctx) OptionalEvaluationExpr(Cast);
  Result->setType(OptionalType::get(underlyingType));

  return Result;
}

/// In a synthesized accessor body, store 'value' to the appropriate element.
///
/// If the property is an override, we call the superclass setter.
/// Otherwise, we do a direct store of the value.
static
void createPropertyStoreOrCallSuperclassSetter(AccessorDecl *accessor,
                                               Expr *value,
                                               AbstractStorageDecl *storage,
                                               TargetImpl target,
                                               SmallVectorImpl<ASTNode> &body,
                                               ASTContext &ctx) {
  // If the storage is an @NSCopying property, then we store the
  // result of a copyWithZone call on the value, not the value itself.
  if (auto property = dyn_cast<VarDecl>(storage)) {
    if (property->getAttrs().hasAttribute<NSCopyingAttr>())
      value = synthesizeCopyWithZoneCall(value, property, ctx);
  }

  // Error recovery.
  if (value->getType()->hasError())
    return;

  Expr *dest = buildStorageReference(accessor, storage, target,
                                     /*isLValue=*/true, ctx);

  // A lazy property setter will store a value of type T into underlying storage
  // of type T?.
  auto destType = dest->getType()->getWithoutSpecifierType();

  // Error recovery.
  if (destType->hasError())
    return;

  if (!destType->isEqual(value->getType())) {
    assert(destType->getOptionalObjectType()->isEqual(value->getType()));
    value = new (ctx) InjectIntoOptionalExpr(value, destType);
  }

  auto *assign = new (ctx) AssignExpr(dest, SourceLoc(), value,
                                      /*IsImplicit=*/true);
  assign->setType(ctx.TheEmptyTupleType);

  body.push_back(assign);
}

LLVM_ATTRIBUTE_UNUSED
static bool isSynthesizedComputedProperty(AbstractStorageDecl *storage) {
  return (storage->getAttrs().hasAttribute<LazyAttr>() ||
          storage->getAttrs().hasAttribute<NSManagedAttr>() ||
          (isa<VarDecl>(storage) &&
           cast<VarDecl>(storage)->hasAttachedPropertyWrapper()));
}

/// Synthesize the body of a trivial getter.  For a non-member vardecl or one
/// which is not an override of a base class property, it performs a direct
/// storage load.  For an override of a base member property, it chains up to
/// super.
static std::pair<BraceStmt *, bool>
synthesizeTrivialGetterBody(AccessorDecl *getter, TargetImpl target,
                            ASTContext &ctx) {
  auto storage = getter->getStorage();
  assert(!isSynthesizedComputedProperty(storage) ||
         target == TargetImpl::Wrapper ||
         target == TargetImpl::WrapperStorage);

  SourceLoc loc = storage->getLoc();

  Expr *result =
    createPropertyLoadOrCallSuperclassGetter(getter, storage, target, ctx);
  ASTNode returnStmt = new (ctx) ReturnStmt(SourceLoc(), result,
                                            /*IsImplicit=*/true);

  return { BraceStmt::create(ctx, loc, returnStmt, loc, true),
           /*isTypeChecked=*/true };
}

/// Synthesize the body of a getter which just directly accesses the
/// underlying storage.
static std::pair<BraceStmt *, bool>
synthesizeTrivialGetterBody(AccessorDecl *getter, ASTContext &ctx) {
  assert(getter->getStorage()->hasStorage());
  return synthesizeTrivialGetterBody(getter, TargetImpl::Storage, ctx);
}

/// Synthesize the body of a getter which just delegates to its superclass
/// implementation.
static std::pair<BraceStmt *, bool>
synthesizeInheritedGetterBody(AccessorDecl *getter, ASTContext &ctx) {
  // This should call the superclass getter.
  return synthesizeTrivialGetterBody(getter, TargetImpl::Super, ctx);
}

/// Synthesize the body of a getter which just delegates to an addressor.
static std::pair<BraceStmt *, bool>
synthesizeAddressedGetterBody(AccessorDecl *getter, ASTContext &ctx) {
  assert(getter->getStorage()->getParsedAccessor(AccessorKind::Address));

  // This should call the addressor.
  return synthesizeTrivialGetterBody(getter, TargetImpl::Implementation, ctx);
}

/// Synthesize the body of a getter which just delegates to a read
/// coroutine accessor.
static std::pair<BraceStmt *, bool>
synthesizeReadCoroutineGetterBody(AccessorDecl *getter, ASTContext &ctx) {
  assert(getter->getStorage()->getParsedAccessor(AccessorKind::Read));

  // This should call the read coroutine.
  return synthesizeTrivialGetterBody(getter, TargetImpl::Implementation, ctx);
}

namespace {
  /// This ASTWalker explores an expression tree looking for expressions (which
  /// are DeclContext's) and changes their parent DeclContext to NewDC.
  class RecontextualizeClosures : public ASTWalker {
    DeclContext *NewDC;
  public:
    RecontextualizeClosures(DeclContext *NewDC) : NewDC(NewDC) {}

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      // If we find a closure, update its declcontext and do *not* walk into it.
      if (auto CE = dyn_cast<AbstractClosureExpr>(E)) {
        CE->setParent(NewDC);
        return { false, E };
      }
      
      if (auto CLE = dyn_cast<CaptureListExpr>(E)) {
        // Make sure to recontextualize any decls in the capture list as well.
        for (auto &CLE : CLE->getCaptureList()) {
          CLE.Var->setDeclContext(NewDC);
          CLE.Init->setDeclContext(NewDC);
        }
      }
      
      // Unlike a closure, a TapExpr is not a DeclContext, so we need to
      // recontextualize its variable and then anything else in its body.
      // FIXME: Might be better to change walkToDeclPre() and walkToStmtPre()
      // below, but I don't know what other effects that might have.
      if (auto TE = dyn_cast<TapExpr>(E)) {
        TE->getVar()->setDeclContext(NewDC);
        for (auto node : TE->getBody()->getElements())
          node.walk(RecontextualizeClosures(NewDC));
      }

      return { true, E };
    }

    /// We don't want to recurse into declarations or statements.
    bool walkToDeclPre(Decl *) override { return false; }
    std::pair<bool, Stmt*> walkToStmtPre(Stmt *S) override { return {false,S}; }
  };
} // end anonymous namespace

/// Synthesize the getter for a lazy property with the specified storage
/// vardecl.
static std::pair<BraceStmt *, bool>
synthesizeLazyGetterBody(AccessorDecl *Get, VarDecl *VD, VarDecl *Storage,
                         ASTContext &Ctx) {
  // The getter checks the optional, storing the initial value in if nil.  The
  // specific pattern we generate is:
  //   get {
  //     if let tmp1 = storage {
  //       return tmp1
  //     }
  //     let tmp2 : Ty = <<initializer expression>>
  //     storage = tmp2
  //     return tmp2
  //   }
  SmallVector<ASTNode, 6> Body;

  // Load the existing storage and store it into the 'tmp1' temporary.
  auto *Tmp1VD = new (Ctx) VarDecl(/*IsStatic*/false, VarDecl::Introducer::Let,
                                   /*IsCaptureList*/false, SourceLoc(),
                                   Ctx.getIdentifier("tmp1"), Get);
  Tmp1VD->setInterfaceType(VD->getValueInterfaceType());
  Tmp1VD->setHasNonPatternBindingInit();
  Tmp1VD->setImplicit();

  auto *Named = new (Ctx) NamedPattern(Tmp1VD, /*implicit*/true);
  Named->setType(Tmp1VD->getType());
  auto *Let = new (Ctx) VarPattern(SourceLoc(), /*let*/true, Named,
                                   /*implict*/true);
  Let->setType(Named->getType());
  auto *Some = new (Ctx) OptionalSomePattern(Let, SourceLoc(),
                                             /*implicit*/true);
  Some->setElementDecl(Ctx.getOptionalSomeDecl());
  Some->setType(OptionalType::get(Let->getType()));

  auto *StoredValueExpr =
    createPropertyLoadOrCallSuperclassGetter(Get, Storage,
                                             TargetImpl::Storage, Ctx);
  SmallVector<StmtConditionElement, 1> Cond;
  Cond.emplace_back(SourceLoc(), Some, StoredValueExpr);

  // Build the early return inside the if.
  auto *Tmp1DRE = new (Ctx) DeclRefExpr(Tmp1VD, DeclNameLoc(), /*Implicit*/true,
                                        AccessSemantics::Ordinary);
  Tmp1DRE->setType(Tmp1VD->getType());
  auto *Return = new (Ctx) ReturnStmt(SourceLoc(), Tmp1DRE,
                                      /*implicit*/true);


  // Build the "if" around the early return.
  Body.push_back(new (Ctx) IfStmt(LabeledStmtInfo(),
                                  SourceLoc(), Ctx.AllocateCopy(Cond), Return,
                                  /*elseloc*/SourceLoc(), /*else*/nullptr,
                                  /*implicit*/ true));


  auto *Tmp2VD = new (Ctx) VarDecl(/*IsStatic*/false, VarDecl::Introducer::Let,
                                   /*IsCaptureList*/false, SourceLoc(),
                                   Ctx.getIdentifier("tmp2"),
                                   Get);
  Tmp2VD->setInterfaceType(VD->getValueInterfaceType());
  Tmp2VD->setImplicit();


  // Take the initializer from the PatternBindingDecl for VD.
  // TODO: This doesn't work with complicated patterns like:
  //   lazy var (a,b) = foo()
  auto PBD = VD->getParentPatternBinding();
  unsigned entryIndex = PBD->getPatternEntryIndexForVarDecl(VD);

  Expr *InitValue;
  if (PBD->getInit(entryIndex)) {
    PBD->setInitializerSubsumed(entryIndex);

    if (!PBD->isInitializerChecked(entryIndex))
      TypeChecker::typeCheckPatternBinding(PBD, entryIndex);

    InitValue = PBD->getInit(entryIndex);
  } else {
    InitValue = new (Ctx) ErrorExpr(SourceRange(), Tmp2VD->getType());
  }

  // Recontextualize any closure declcontexts nested in the initializer to
  // realize that they are in the getter function.
  Get->getImplicitSelfDecl()->setDeclContext(Get);

  InitValue->walk(RecontextualizeClosures(Get));

  // Wrap the initializer in a LazyInitializerExpr to avoid walking it twice.
  auto initType = InitValue->getType();
  InitValue = new (Ctx) LazyInitializerExpr(InitValue);
  InitValue->setType(initType);

  Pattern *Tmp2PBDPattern = new (Ctx) NamedPattern(Tmp2VD, /*implicit*/true);
  Tmp2PBDPattern =
    TypedPattern::createImplicit(Ctx, Tmp2PBDPattern, Tmp2VD->getType());

  auto *Tmp2PBD = PatternBindingDecl::createImplicit(
      Ctx, StaticSpellingKind::None, Tmp2PBDPattern, InitValue, Get,
      /*VarLoc*/ InitValue->getStartLoc());
  Body.push_back(Tmp2PBD);
  Body.push_back(Tmp2VD);

  // Assign tmp2 into storage.
  auto Tmp2DRE = new (Ctx) DeclRefExpr(Tmp2VD, DeclNameLoc(), /*Implicit*/true,
                                       AccessSemantics::DirectToStorage);
  Tmp2DRE->setType(Tmp2VD->getType());
  createPropertyStoreOrCallSuperclassSetter(Get, Tmp2DRE, Storage,
                                            TargetImpl::Storage, Body, Ctx);

  // Return tmp2.
  Tmp2DRE = new (Ctx) DeclRefExpr(Tmp2VD, DeclNameLoc(), /*Implicit*/true,
                                  AccessSemantics::DirectToStorage);
  Tmp2DRE->setType(Tmp2VD->getType());

  Body.push_back(new (Ctx) ReturnStmt(SourceLoc(), Tmp2DRE, /*implicit*/true));

  return { BraceStmt::create(Ctx, VD->getLoc(), Body, VD->getLoc(),
                             /*implicit*/true),
           /*isTypeChecked=*/true };
}

/// Synthesize the body of a getter for a property wrapper, which
/// delegates to the wrapper's "value" property.
static std::pair<BraceStmt *, bool>
synthesizePropertyWrapperGetterBody(AccessorDecl *getter, ASTContext &ctx) {
  return synthesizeTrivialGetterBody(getter, TargetImpl::Wrapper, ctx);
}

static std::pair<BraceStmt *, bool>
synthesizeInvalidAccessor(AccessorDecl *accessor, ASTContext &ctx) {
  auto loc = accessor->getLoc();
  return { BraceStmt::create(ctx, loc, ArrayRef<ASTNode>(), loc, true), true };
}

static std::pair<BraceStmt *, bool>
synthesizeGetterBody(AccessorDecl *getter, ASTContext &ctx) {
  auto storage = getter->getStorage();

  // Synthesize the getter for a lazy property or property wrapper.
  if (auto var = dyn_cast<VarDecl>(storage)) {
    if (var->getAttrs().hasAttribute<LazyAttr>()) {
      auto *storage = var->getLazyStorageProperty();
      return synthesizeLazyGetterBody(getter, var, storage, ctx);
    }

    if (var->hasAttachedPropertyWrapper()) {
      return synthesizePropertyWrapperGetterBody(getter, ctx);
    }

    if (var->getOriginalWrappedProperty(
            PropertyWrapperSynthesizedPropertyKind::StorageWrapper)) {
      return synthesizeTrivialGetterBody(getter, TargetImpl::WrapperStorage,
                                         ctx);
    }
  }

  if (getter->hasForcedStaticDispatch()) {
    return synthesizeTrivialGetterBody(getter, TargetImpl::Ordinary, ctx);
  }

  switch (getter->getStorage()->getReadImpl()) {
  case ReadImplKind::Stored:
    return synthesizeTrivialGetterBody(getter, ctx);

  case ReadImplKind::Get:
    return synthesizeInvalidAccessor(getter, ctx);

  case ReadImplKind::Inherited:
    return synthesizeInheritedGetterBody(getter, ctx);

  case ReadImplKind::Address:
    return synthesizeAddressedGetterBody(getter, ctx);

  case ReadImplKind::Read:
    return synthesizeReadCoroutineGetterBody(getter, ctx);
  }
  llvm_unreachable("bad ReadImplKind");
}

/// Synthesize the body of a setter which just stores to the given storage
/// declaration (which doesn't have to be the storage for the setter).
static std::pair<BraceStmt *, bool>
synthesizeTrivialSetterBodyWithStorage(AccessorDecl *setter,
                                       TargetImpl target,
                                       AbstractStorageDecl *storageToUse,
                                       ASTContext &ctx) {
  SourceLoc loc = setter->getStorage()->getLoc();

  VarDecl *valueParamDecl = setter->getParameters()->get(0);

  auto *valueDRE =
    new (ctx) DeclRefExpr(valueParamDecl, DeclNameLoc(), /*IsImplicit=*/true);
  valueDRE->setType(valueParamDecl->getType());

  SmallVector<ASTNode, 1> setterBody;

  createPropertyStoreOrCallSuperclassSetter(setter, valueDRE, storageToUse,
                                            target, setterBody, ctx);
  return { BraceStmt::create(ctx, loc, setterBody, loc, true),
           /*isTypeChecked=*/true };
}

static std::pair<BraceStmt *, bool>
synthesizeTrivialSetterBody(AccessorDecl *setter, ASTContext &ctx) {
  auto storage = setter->getStorage();
  assert(!isSynthesizedComputedProperty(storage));

  return synthesizeTrivialSetterBodyWithStorage(setter, TargetImpl::Storage,
                                                storage, ctx);
}

/// Synthesize the body of a setter for a property wrapper, which
/// delegates to the wrapper's "value" property.
static std::pair<BraceStmt *, bool>
synthesizePropertyWrapperSetterBody(AccessorDecl *setter, ASTContext &ctx) {
  return synthesizeTrivialSetterBodyWithStorage(setter, TargetImpl::Wrapper,
                                                setter->getStorage(), ctx);
}

/// Synthesize the body of a setter which just delegates to a mutable
/// addressor.
static std::pair<BraceStmt *, bool>
synthesizeMutableAddressSetterBody(AccessorDecl *setter, ASTContext &ctx) {
  // This should call the mutable addressor.
  return synthesizeTrivialSetterBodyWithStorage(setter,
                                                TargetImpl::Implementation,
                                                setter->getStorage(), ctx);
}

/// Synthesize the body of a setter which just delegates to a modify
/// coroutine accessor.
static std::pair<BraceStmt *, bool>
synthesizeModifyCoroutineSetterBody(AccessorDecl *setter, ASTContext &ctx) {
  // This should call the modify coroutine.
  return synthesizeTrivialSetterBodyWithStorage(setter,
                                                TargetImpl::Implementation,
                                                setter->getStorage(), ctx);
}

static Expr *maybeWrapInOutExpr(Expr *expr, ASTContext &ctx) {
  if (auto lvalueType = expr->getType()->getAs<LValueType>()) {
    auto type = lvalueType->getObjectType();
    return new (ctx) InOutExpr(SourceLoc(), expr, type, true);
  }

  return expr;
}

/// Given a VarDecl with a willSet: and/or didSet: specifier, synthesize the
/// setter which calls them.
static std::pair<BraceStmt *, bool>
synthesizeObservedSetterBody(AccessorDecl *Set, TargetImpl target,
                             ASTContext &Ctx) {
  auto VD = cast<VarDecl>(Set->getStorage());

  SourceLoc Loc = VD->getLoc();

  // Start by finding the decls for 'self' and 'value'.
  auto *SelfDecl = Set->getImplicitSelfDecl();
  VarDecl *ValueDecl = Set->getParameters()->get(0);

  bool IsSelfLValue = VD->isSetterMutating();

  SubstitutionMap subs;
  if (auto *genericEnv = Set->getGenericEnvironment())
    subs = genericEnv->getForwardingSubstitutionMap();

  // The setter loads the oldValue, invokes willSet with the incoming value,
  // does a direct store, then invokes didSet with the oldValue.
  SmallVector<ASTNode, 6> SetterBody;

  auto callObserver = [&](AccessorDecl *observer, VarDecl *arg) {
    ConcreteDeclRef ref(observer, subs);
    auto type = observer->getInterfaceType().subst(subs);
    Expr *Callee = new (Ctx) DeclRefExpr(ref, DeclNameLoc(), /*imp*/true);
    Callee->setType(type);
    auto *ValueDRE = new (Ctx) DeclRefExpr(arg, DeclNameLoc(), /*imp*/true);
    ValueDRE->setType(arg->getType());

    if (SelfDecl) {
      auto *SelfDRE = buildSelfReference(SelfDecl, SelfAccessorKind::Peer,
                                         IsSelfLValue, Ctx);
      SelfDRE = maybeWrapInOutExpr(SelfDRE, Ctx);
      auto *DSCE = new (Ctx) DotSyntaxCallExpr(Callee, SourceLoc(), SelfDRE);

      if (auto funcType = type->getAs<FunctionType>())
        type = funcType->getResult();
      DSCE->setType(type);
      DSCE->setThrows(false);
      Callee = DSCE;
    }

    auto *Call = CallExpr::createImplicit(Ctx, Callee, { ValueDRE },
                                          { Identifier() });
    if (auto funcType = type->getAs<FunctionType>())
      type = funcType->getResult();
    Call->setType(type);
    Call->setThrows(false);

    SetterBody.push_back(Call);
  };

  // If there is a didSet, it will take the old value.  Load it into a temporary
  // 'let' so we have it for later.
  // TODO: check the body of didSet to only do this load (which may call the
  // superclass getter) if didSet takes an argument.
  VarDecl *OldValue = nullptr;
  if (VD->getParsedAccessor(AccessorKind::DidSet)) {
    Expr *OldValueExpr
      = buildStorageReference(Set, VD, target, /*isLValue=*/true, Ctx);
    OldValueExpr = new (Ctx) LoadExpr(OldValueExpr, VD->getType());

    OldValue = new (Ctx) VarDecl(/*IsStatic*/false, VarDecl::Introducer::Let,
                                 /*IsCaptureList*/false, SourceLoc(),
                                 Ctx.getIdentifier("tmp"), Set);
    OldValue->setImplicit();
    OldValue->setInterfaceType(VD->getValueInterfaceType());
    auto *tmpPattern = new (Ctx) NamedPattern(OldValue, /*implicit*/ true);
    auto *tmpPBD = PatternBindingDecl::createImplicit(
        Ctx, StaticSpellingKind::None, tmpPattern, OldValueExpr, Set);
    SetterBody.push_back(tmpPBD);
    SetterBody.push_back(OldValue);
  }

  if (auto willSet = VD->getParsedAccessor(AccessorKind::WillSet))
    callObserver(willSet, ValueDecl);
  
  // Create an assignment into the storage or call to superclass setter.
  auto *ValueDRE = new (Ctx) DeclRefExpr(ValueDecl, DeclNameLoc(), true);
  ValueDRE->setType(ValueDecl->getType());
  createPropertyStoreOrCallSuperclassSetter(Set, ValueDRE, VD, target,
                                            SetterBody, Ctx);

  if (auto didSet = VD->getParsedAccessor(AccessorKind::DidSet))
    callObserver(didSet, OldValue);

  return { BraceStmt::create(Ctx, Loc, SetterBody, Loc, true),
           /*isTypeChecked=*/true };
}

static std::pair<BraceStmt *, bool>
synthesizeStoredWithObserversSetterBody(AccessorDecl *setter, ASTContext &ctx) {
  return synthesizeObservedSetterBody(setter, TargetImpl::Storage, ctx);
}

static std::pair<BraceStmt *, bool>
synthesizeInheritedWithObserversSetterBody(AccessorDecl *setter,
                                           ASTContext &ctx) {
  return synthesizeObservedSetterBody(setter, TargetImpl::Super, ctx);
}

static std::pair<BraceStmt *, bool>
synthesizeSetterBody(AccessorDecl *setter, ASTContext &ctx) {
  auto storage = setter->getStorage();

  // Synthesize the setter for a lazy property or property wrapper.
  if (auto var = dyn_cast<VarDecl>(storage)) {
    if (var->getAttrs().hasAttribute<LazyAttr>()) {
      // Lazy property setters write to the underlying storage.
      auto *storage = var->getLazyStorageProperty();
      return synthesizeTrivialSetterBodyWithStorage(setter, TargetImpl::Storage,
                                                    storage, ctx);
    }

    if (var->hasAttachedPropertyWrapper()) {
      if (var->getParsedAccessor(AccessorKind::WillSet) ||
          var->getParsedAccessor(AccessorKind::DidSet)) {
        return synthesizeObservedSetterBody(setter, TargetImpl::Wrapper, ctx);
      }

      return synthesizePropertyWrapperSetterBody(setter, ctx);
    }

    // Synthesize a setter for the storage wrapper property of a property
    // with an attached wrapper.
    if (auto original = var->getOriginalWrappedProperty(
            PropertyWrapperSynthesizedPropertyKind::StorageWrapper)) {
      auto backingVar = original->getPropertyWrapperBackingProperty();
      return synthesizeTrivialSetterBodyWithStorage(setter,
                                                    TargetImpl::WrapperStorage,
                                                    backingVar, ctx);
    }
  }

  switch (storage->getWriteImpl()) {
  case WriteImplKind::Immutable:
    llvm_unreachable("synthesizing setter from immutable storage");

  case WriteImplKind::Stored:
    return synthesizeTrivialSetterBody(setter, ctx);

  case WriteImplKind::StoredWithObservers:
    return synthesizeStoredWithObserversSetterBody(setter, ctx);

  case WriteImplKind::InheritedWithObservers:
    return synthesizeInheritedWithObserversSetterBody(setter, ctx);

  case WriteImplKind::Set:
    return synthesizeInvalidAccessor(setter, ctx);

  case WriteImplKind::MutableAddress:
    return synthesizeMutableAddressSetterBody(setter, ctx);

  case WriteImplKind::Modify:
    return synthesizeModifyCoroutineSetterBody(setter, ctx);
  }
  llvm_unreachable("bad ReadImplKind");
}

static std::pair<BraceStmt *, bool>
synthesizeCoroutineAccessorBody(AccessorDecl *accessor, ASTContext &ctx) {
  assert(accessor->isCoroutine());

  auto storage = accessor->getStorage();
  auto target = (accessor->hasForcedStaticDispatch()
                   ? TargetImpl::Ordinary
                   : TargetImpl::Implementation);

  // If this is a variable with an attached property wrapper, then
  // the accessors need to yield the wrappedValue or projectedValue.
  if (auto var = dyn_cast<VarDecl>(storage)) {
    if (var->hasAttachedPropertyWrapper()) {
      target = TargetImpl::Wrapper;
    }

    if (var->getOriginalWrappedProperty(
            PropertyWrapperSynthesizedPropertyKind::StorageWrapper)) {
      target = TargetImpl::WrapperStorage;
    }
  }

  SourceLoc loc = storage->getLoc();
  SmallVector<ASTNode, 1> body;

  bool isLValue = accessor->getAccessorKind() == AccessorKind::Modify;

  // Build a reference to the storage.
  Expr *ref = buildStorageReference(accessor, storage, target, isLValue, ctx);

  // Wrap it with an `&` marker if this is a modify.
  ref = maybeWrapInOutExpr(ref, ctx);

  // Yield it.
  YieldStmt *yield = YieldStmt::create(ctx, loc, loc, ref, loc, true);
  body.push_back(yield);

  return { BraceStmt::create(ctx, loc, body, loc, true),
           /*isTypeChecked=*/true };
}

/// Synthesize the body of a read coroutine.
static std::pair<BraceStmt *, bool>
synthesizeReadCoroutineBody(AccessorDecl *read, ASTContext &ctx) {
  assert(read->getStorage()->getReadImpl() != ReadImplKind::Read);
  return synthesizeCoroutineAccessorBody(read, ctx);
}

/// Synthesize the body of a modify coroutine.
static std::pair<BraceStmt *, bool>
synthesizeModifyCoroutineBody(AccessorDecl *modify, ASTContext &ctx) {
#ifndef NDEBUG
  auto storage = modify->getStorage();
  auto impl = storage->getReadWriteImpl();
  auto hasWrapper = isa<VarDecl>(storage) &&
                    cast<VarDecl>(storage)->hasAttachedPropertyWrapper();
  assert((hasWrapper || impl != ReadWriteImplKind::Modify) &&
         impl != ReadWriteImplKind::Immutable);
#endif
  return synthesizeCoroutineAccessorBody(modify, ctx);
}

static std::pair<BraceStmt *, bool>
synthesizeAccessorBody(AbstractFunctionDecl *fn, void *) {
  auto *accessor = cast<AccessorDecl>(fn);
  auto &ctx = accessor->getASTContext();

  if (ctx.Stats)
    ctx.Stats->getFrontendCounters().NumAccessorBodiesSynthesized++;

  switch (accessor->getAccessorKind()) {
  case AccessorKind::Get:
    return synthesizeGetterBody(accessor, ctx);

  case AccessorKind::Set:
    return synthesizeSetterBody(accessor, ctx);

  case AccessorKind::Read:
    return synthesizeReadCoroutineBody(accessor, ctx);

  case AccessorKind::Modify:
    return synthesizeModifyCoroutineBody(accessor, ctx);

  case AccessorKind::WillSet:
  case AccessorKind::DidSet:
  case AccessorKind::Address:
  case AccessorKind::MutableAddress:
    break;
  }
  llvm_unreachable("bad synthesized function kind");
}

static void finishImplicitAccessor(AccessorDecl *accessor,
                                   ASTContext &ctx) {
  accessor->setImplicit();

  if (ctx.Stats)
    ctx.Stats->getFrontendCounters().NumAccessorsSynthesized++;

  if (doesAccessorHaveBody(accessor))
    accessor->setBodySynthesizer(&synthesizeAccessorBody);
}

static AccessorDecl *createGetterPrototype(AbstractStorageDecl *storage,
                                           ASTContext &ctx) {
  SourceLoc loc = storage->getLoc();

  ParamDecl *selfDecl = nullptr;
  if (storage->getDeclContext()->isTypeContext()) {
    if (storage->getAttrs().hasAttribute<LazyAttr>()) {
      // For lazy properties, steal the 'self' from the initializer context.
      auto *varDecl = cast<VarDecl>(storage);
      auto *bindingDecl = varDecl->getParentPatternBinding();
      const auto i = bindingDecl->getPatternEntryIndexForVarDecl(varDecl);
      auto *bindingInit = cast<PatternBindingInitializer>(
        bindingDecl->getInitContext(i));

      selfDecl = bindingInit->getImplicitSelfDecl();
    }
  }

  GenericParamList *genericParams = createAccessorGenericParams(storage);

  // Add an index-forwarding clause.
  auto *getterParams = buildIndexForwardingParamList(storage, {}, ctx);

  SourceLoc staticLoc;
  if (storage->isStatic())
    staticLoc = storage->getLoc();

  auto getter = AccessorDecl::create(
      ctx, loc, /*AccessorKeywordLoc*/ loc,
      AccessorKind::Get, storage,
      staticLoc, StaticSpellingKind::None,
      /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
      genericParams,
      getterParams,
      TypeLoc(),
      storage->getDeclContext());

  // If we're stealing the 'self' from a lazy initializer, set it now.
  // Note that we don't re-parent the 'self' declaration to be part of
  // the getter until we synthesize the body of the getter later.
  if (selfDecl)
    *getter->getImplicitSelfDeclStorage() = selfDecl;

  if (storage->isGetterMutating())
    getter->setSelfAccessKind(SelfAccessKind::Mutating);
  else
    getter->setSelfAccessKind(SelfAccessKind::NonMutating);

  if (!storage->requiresOpaqueAccessor(AccessorKind::Get))
    getter->setForcedStaticDispatch(true);

  finishImplicitAccessor(getter, ctx);

  return getter;
}

static AccessorDecl *createSetterPrototype(AbstractStorageDecl *storage,
                                           ASTContext &ctx,
                                           AccessorDecl *getter = nullptr) {
  assert(storage->supportsMutation());

  SourceLoc loc = storage->getLoc();

  bool isMutating = storage->isSetterMutating();

  GenericParamList *genericParams = createAccessorGenericParams(storage);

  // Add a "(value : T, indices...)" argument list.
  auto *param = new (ctx) ParamDecl(SourceLoc(), SourceLoc(),
                                    Identifier(), loc,
                                    ctx.getIdentifier("value"),
                                    storage->getDeclContext());
  param->setSpecifier(ParamSpecifier::Default);
  param->setImplicit();

  auto *params = buildIndexForwardingParamList(storage, param, ctx);

  auto setter = AccessorDecl::create(
      ctx, loc, /*AccessorKeywordLoc*/ SourceLoc(),
      AccessorKind::Set, storage,
      /*StaticLoc=*/SourceLoc(), StaticSpellingKind::None,
      /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
      genericParams, params,
      TypeLoc(),
      storage->getDeclContext());

  if (isMutating)
    setter->setSelfAccessKind(SelfAccessKind::Mutating);
  else
    setter->setSelfAccessKind(SelfAccessKind::NonMutating);

  // All mutable storage requires a setter.
  assert(storage->requiresOpaqueAccessor(AccessorKind::Set));

  finishImplicitAccessor(setter, ctx);

  return setter;
}

static AccessorDecl *
createCoroutineAccessorPrototype(AbstractStorageDecl *storage,
                                 AccessorKind kind,
                                ASTContext &ctx) {
  assert(kind == AccessorKind::Read || kind == AccessorKind::Modify);

  SourceLoc loc = storage->getLoc();

  bool isMutating = storage->isGetterMutating();
  if (kind == AccessorKind::Modify)
    isMutating |= storage->isSetterMutating();

  auto dc = storage->getDeclContext();

  // The forwarding index parameters.
  auto *params = buildIndexForwardingParamList(storage, {}, ctx);

  // Coroutine accessors always return ().
  Type retTy = TupleType::getEmpty(ctx);

  GenericParamList *genericParams = createAccessorGenericParams(storage);

  auto *accessor = AccessorDecl::create(
      ctx, loc, /*AccessorKeywordLoc=*/SourceLoc(),
      kind, storage,
      /*StaticLoc=*/SourceLoc(), StaticSpellingKind::None,
      /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
      genericParams, params, TypeLoc::withoutLoc(retTy), dc);
  
  if (isMutating)
    accessor->setSelfAccessKind(SelfAccessKind::Mutating);
  else
    accessor->setSelfAccessKind(SelfAccessKind::NonMutating);

  // If the storage does not provide this accessor as an opaque accessor,
  // we can't add a dynamically-dispatched method entry for the accessor,
  // so force it to be statically dispatched. ("final" would be inappropriate
  // because the property can still be overridden.)
  if (!storage->requiresOpaqueAccessor(kind))
    accessor->setForcedStaticDispatch(true);

  // Make sure the coroutine is available enough to access
  // the storage (and its getters/setters if it has them).
  SmallVector<const Decl *, 2> asAvailableAs;
  asAvailableAs.push_back(storage);
  if (FuncDecl *getter = storage->getParsedAccessor(AccessorKind::Get)) {
    asAvailableAs.push_back(getter);
  }
  if (kind == AccessorKind::Modify) {
    if (FuncDecl *setter = storage->getParsedAccessor(AccessorKind::Set)) {
      asAvailableAs.push_back(setter);
    }
  }

  AvailabilityInference::applyInferredAvailableAttrs(accessor,
                                                     asAvailableAs, ctx);

  finishImplicitAccessor(accessor, ctx);

  return accessor;
}

static AccessorDecl *
createReadCoroutinePrototype(AbstractStorageDecl *storage,
                             ASTContext &ctx) {
  return createCoroutineAccessorPrototype(storage, AccessorKind::Read, ctx);
}

static AccessorDecl *
createModifyCoroutinePrototype(AbstractStorageDecl *storage,
                               ASTContext &ctx) {
  return createCoroutineAccessorPrototype(storage, AccessorKind::Modify, ctx);
}

llvm::Expected<AccessorDecl *>
SynthesizeAccessorRequest::evaluate(Evaluator &evaluator,
                                    AbstractStorageDecl *storage,
                                    AccessorKind kind) const {
  auto &ctx = storage->getASTContext();

  switch (kind) {
  case AccessorKind::Get:
    return createGetterPrototype(storage, ctx);

  case AccessorKind::Set:
    return createSetterPrototype(storage, ctx);

  case AccessorKind::Read:
    return createReadCoroutinePrototype(storage, ctx);

  case AccessorKind::Modify:
    return createModifyCoroutinePrototype(storage, ctx);

#define OPAQUE_ACCESSOR(ID, KEYWORD)
#define ACCESSOR(ID) \
  case AccessorKind::ID:
#include "swift/AST/AccessorKinds.def"
    llvm_unreachable("not an opaque accessor");
  }
  llvm_unreachable("Unhandled AccessorKind in switch");
}

llvm::Expected<bool>
RequiresOpaqueAccessorsRequest::evaluate(Evaluator &evaluator,
                                         VarDecl *var) const {
  // Nameless vars from interface files should not have any accessors.
  // TODO: Replace this check with a broader check that all storage decls
  //       from interface files have all their accessors up front.
  if (var->getBaseName().empty())
    return false;

  // Computed properties always require opaque accessors.
  if (!var->getImplInfo().isSimpleStored())
    return true;

  // The backing storage for a lazy property does require opaque accessors.
  if (var->isLazyStorageProperty())
    return false;

  auto *dc = var->getDeclContext();

  // Local stored variables don't require opaque accessors.
  if (dc->isLocalContext()) {
    return false;

  } else if (dc->isModuleScopeContext()) {
    // Fixed-layout global variables don't require opaque accessors.
    if (!var->isResilient() && !var->isNativeDynamic())
      return false;

  // Stored properties imported from Clang don't require opaque accessors.
  } else if (auto *structDecl = dyn_cast<StructDecl>(dc)) {
    if (structDecl->hasClangNode())
      return false;
  }

  // Stored properties in SIL mode don't get accessors.
  // But we might need to create opaque accessors for them.
  if (auto sourceFile = dc->getParentSourceFile()) {
    if (sourceFile->Kind == SourceFileKind::SIL) {
      if (!var->getParsedAccessor(AccessorKind::Get))
        return false;
    }
  }

  // Everything else requires opaque accessors.
  return true;
}

llvm::Expected<bool>
RequiresOpaqueModifyCoroutineRequest::evaluate(Evaluator &evaluator,
                                               AbstractStorageDecl *storage) const {
  // Only for mutable storage.
  if (!storage->supportsMutation())
    return false;

  auto *dc = storage->getDeclContext();

  // Local properties don't have an opaque modify coroutine.
  if (dc->isLocalContext())
    return false;

  // Fixed-layout global properties don't have an opaque modify coroutine.
  if (dc->isModuleScopeContext() && !storage->isResilient())
    return false;

  // Imported storage declarations don't have an opaque modify coroutine.
  if (storage->hasClangNode())
    return false;

  // Dynamic storage does not have an opaque modify coroutine.
  if (dc->getSelfClassDecl())
    if (storage->isObjCDynamic())
      return false;

  // Requirements of ObjC protocols don't have an opaque modify coroutine.
  if (auto protoDecl = dyn_cast<ProtocolDecl>(dc))
    if (protoDecl->isObjC())
      return false;

  return true;
}

/// Mark the accessor as transparent if we can.
///
/// If the storage is inside a fixed-layout nominal type, we can mark the
/// accessor as transparent, since in this case we just want it for abstraction
/// purposes (i.e., to make access to the variable uniform and to be able to
/// put the getter in a vtable).
///
/// If the storage is for a global stored property or a stored property of a
/// resilient type, we are synthesizing accessors to present a resilient
/// interface to the storage and they should not be transparent.
llvm::Expected<bool>
IsAccessorTransparentRequest::evaluate(Evaluator &evaluator,
                                       AccessorDecl *accessor) const {
  auto *storage = accessor->getStorage();
  if (storage->isTransparent())
    return true;

  if (accessor->getAttrs().hasAttribute<TransparentAttr>())
    return true;

  if (!accessor->isImplicit())
    return false;

  if (!doesAccessorHaveBody(accessor))
    return false;

  auto *DC = accessor->getDeclContext();
  auto *nominalDecl = DC->getSelfNominalTypeDecl();

  // Global variable accessors are not @_transparent.
  if (!nominalDecl)
    return false;

  // Accessors for resilient properties are not @_transparent.
  if (storage->isResilient())
    return false;

  // Accessors for classes with @objc ancestry are not @_transparent,
  // since they use a field offset variable which is not exported.
  if (auto *classDecl = dyn_cast<ClassDecl>(nominalDecl))
    if (classDecl->checkAncestry(AncestryFlags::ObjC))
      return false;

  // Accessors synthesized on-demand are never transaprent.
  if (accessor->hasForcedStaticDispatch())
    return false;

  if (accessor->getAccessorKind() == AccessorKind::Get ||
      accessor->getAccessorKind() == AccessorKind::Set) {
    // Getters and setters for lazy properties are not @_transparent.
    if (storage->getAttrs().hasAttribute<LazyAttr>())
      return false;

    // Getters/setters for a property with a wrapper are not @_transparent if
    // the backing variable has more-restrictive access than the original
    // property. The same goes for its storage wrapper.
    if (auto var = dyn_cast<VarDecl>(storage)) {
      if (auto backingVar = var->getPropertyWrapperBackingProperty()) {
        if (backingVar->getFormalAccess() < var->getFormalAccess())
          return false;
      }

      if (auto original = var->getOriginalWrappedProperty(
              PropertyWrapperSynthesizedPropertyKind::StorageWrapper)) {
        auto backingVar = original->getPropertyWrapperBackingProperty();
        if (backingVar->getFormalAccess() < var->getFormalAccess())
          return false;
      }
    }
  }

  switch (accessor->getAccessorKind()) {
  case AccessorKind::Get:
    break;

  case AccessorKind::Set:

    switch (storage->getWriteImpl()) {
    case WriteImplKind::Set:
      // Setters for property wrapper are OK, unless there are observers.
      // FIXME: This should be folded into the WriteImplKind below.
      if (auto var = dyn_cast<VarDecl>(storage)) {
        if (var->hasAttachedPropertyWrapper()) {
          if (var->getParsedAccessor(AccessorKind::DidSet) ||
              var->getParsedAccessor(AccessorKind::WillSet))
            return false;

          break;
        } else if (var->getOriginalWrappedProperty(
                     PropertyWrapperSynthesizedPropertyKind::StorageWrapper)) {
          break;
        }
      }

      // Anything else should not have a synthesized setter.
      LLVM_FALLTHROUGH;
    case WriteImplKind::Immutable:
      llvm_unreachable("should not be synthesizing accessor in this case");

    case WriteImplKind::StoredWithObservers:
    case WriteImplKind::InheritedWithObservers:
      // Setters for observed properties are not @_transparent (because the
      // observers are private) and cannot be referenced from a transparent
      // method).
      return false;

    case WriteImplKind::Stored:
    case WriteImplKind::MutableAddress:
    case WriteImplKind::Modify:
      break;
    }
    break;

  case AccessorKind::Read:
  case AccessorKind::Modify:
    break;

  case AccessorKind::WillSet:
  case AccessorKind::DidSet:
  case AccessorKind::Address:
  case AccessorKind::MutableAddress:
    llvm_unreachable("bad synthesized function kind");
  }

  return true;
}

llvm::Expected<VarDecl *>
LazyStoragePropertyRequest::evaluate(Evaluator &evaluator,
                                     VarDecl *VD) const {
  assert(isa<SourceFile>(VD->getDeclContext()->getModuleScopeContext()));
  assert(VD->getAttrs().hasAttribute<LazyAttr>());
  auto &Context = VD->getASTContext();

  // Create the storage property as an optional of VD's type.
  SmallString<64> NameBuf;
  NameBuf += "$__lazy_storage_$_";
  NameBuf += VD->getName().str();
  auto StorageName = Context.getIdentifier(NameBuf);
  auto StorageInterfaceTy = OptionalType::get(VD->getInterfaceType());
  auto StorageTy = OptionalType::get(VD->getType());

  auto *Storage = new (Context) VarDecl(/*IsStatic*/false, VarDecl::Introducer::Var,
                                        /*IsCaptureList*/false, VD->getLoc(),
                                        StorageName,
                                        VD->getDeclContext());
  Storage->setInterfaceType(StorageInterfaceTy);
  Storage->setLazyStorageProperty(true);
  Storage->setUserAccessible(false);

  // The storage is implicit and private.
  Storage->setImplicit();
  Storage->overwriteAccess(AccessLevel::Private);
  Storage->overwriteSetterAccess(AccessLevel::Private);

  addMemberToContextIfNeeded(Storage, VD->getDeclContext(), VD);

  // Create the pattern binding decl for the storage decl.  This will get
  // default initialized to nil.
  Pattern *PBDPattern = new (Context) NamedPattern(Storage, /*implicit*/true);
  PBDPattern->setType(StorageTy);
  PBDPattern = TypedPattern::createImplicit(Context, PBDPattern, StorageTy);
  auto *InitExpr = new (Context) NilLiteralExpr(SourceLoc(), /*Implicit=*/true);
  InitExpr->setType(Storage->getType());

  auto *PBD = PatternBindingDecl::createImplicit(
      Context, StaticSpellingKind::None, PBDPattern, InitExpr,
      VD->getDeclContext(), /*VarLoc*/ VD->getLoc());
  PBD->setInitializerChecked(0);

  addMemberToContextIfNeeded(PBD, VD->getDeclContext(), Storage);

  return Storage;
}

/// Synthesize a computed property `$foo` for a property with an attached
/// wrapper that has a `projectedValue` property.
static VarDecl *synthesizePropertyWrapperStorageWrapperProperty(
    ASTContext &ctx, VarDecl *var, Type wrapperType,
    VarDecl *wrapperVar) {
  // If the original property has a @_projectedValueProperty attribute, use
  // that to find the storage wrapper property.
  if (auto attr = var->getAttrs().getAttribute<ProjectedValuePropertyAttr>()){
    SmallVector<ValueDecl *, 2> declsFound;
    DeclNameRef projectionName(attr->ProjectionPropertyName);
    auto dc = var->getDeclContext();
    if (dc->isTypeContext()) {
      dc->lookupQualified(dc->getSelfNominalTypeDecl(), projectionName,
                          NL_QualifiedDefault, declsFound);
    } else if (dc->isModuleScopeContext()) {
      dc->lookupQualified(dc->getParentModule(), projectionName,
                          NL_QualifiedDefault, declsFound);
    } else {
      llvm_unreachable("Property wrappers don't work in local contexts");
    }

    if (declsFound.size() == 1 && isa<VarDecl>(declsFound.front())) {
      auto property = cast<VarDecl>(declsFound.front());
      property->setOriginalWrappedProperty(var);
      return property;
    }

    ctx.Diags.diagnose(attr->getLocation(),
                       diag::property_wrapper_projection_value_missing,
                       projectionName);
    attr->setInvalid();
  }

  // Compute the name of the storage type.
  SmallString<64> nameBuf;
  nameBuf = "$";
  nameBuf += var->getName().str();
  Identifier name = ctx.getIdentifier(nameBuf);

  // Determine the type of the property.
  Type propertyType = wrapperType->getTypeOfMember(
      var->getModuleContext(), wrapperVar,
      wrapperVar->getValueInterfaceType());

  // Form the property.
  auto dc = var->getDeclContext();
  VarDecl *property = new (ctx) VarDecl(/*IsStatic=*/var->isStatic(),
                                        VarDecl::Introducer::Var,
                                        /*IsCaptureList=*/false,
                                        var->getLoc(),
                                        name, dc);
  property->setInterfaceType(propertyType);
  property->setImplicit();
  property->setOriginalWrappedProperty(var);
  addMemberToContextIfNeeded(property, dc, var);

  // Create the pattern binding declaration for the property.
  Pattern *pbdPattern = new (ctx) NamedPattern(property, /*implicit=*/true);
  pbdPattern->setType(propertyType);
  pbdPattern = TypedPattern::createImplicit(ctx, pbdPattern, propertyType);
  auto pbd = PatternBindingDecl::createImplicit(
      ctx, property->getCorrectStaticSpelling(), pbdPattern,
      /*init*/nullptr, dc, SourceLoc());
  addMemberToContextIfNeeded(pbd, dc, var);
  pbd->setStatic(var->isStatic());

  // Determine the access level for the property.
  property->overwriteAccess(var->getFormalAccess());

  // Determine setter access.
  property->overwriteSetterAccess(var->getSetterFormalAccess());

  // Add the accessors we need.
  bool hasSetter = wrapperVar->isSettable(nullptr) &&
      wrapperVar->isSetterAccessibleFrom(var->getInnermostDeclContext());
  if (hasSetter)
    property->setImplInfo(StorageImplInfo::getMutableComputed());
  else
    property->setImplInfo(StorageImplInfo::getImmutableComputed());

  var->getAttrs().add(
      new (ctx) ProjectedValuePropertyAttr(name, SourceLoc(), SourceRange(),
                                            /*Implicit=*/true));
  return property;
}

static void typeCheckSynthesizedWrapperInitializer(
    PatternBindingDecl *pbd, VarDecl *backingVar, PatternBindingDecl *parentPBD,
    Expr *&initializer) {
  // Figure out the context in which the initializer was written.
  DeclContext *originalDC = parentPBD->getDeclContext();
  if (!originalDC->isLocalContext()) {
    auto initContext =
        cast_or_null<PatternBindingInitializer>(parentPBD->getInitContext(0));
    if (initContext)
      originalDC = initContext;
  }

  // Type-check the initialization.
  TypeChecker::typeCheckExpression(initializer, originalDC);
  const auto i = pbd->getPatternEntryIndexForVarDecl(backingVar);
  if (auto initializerContext =
          dyn_cast_or_null<Initializer>(pbd->getInitContext(i))) {
    TypeChecker::contextualizeInitializer(initializerContext, initializer);
  }
  TypeChecker::checkPropertyWrapperErrorHandling(pbd, initializer);
}

static PropertyWrapperMutability::Value
getGetterMutatingness(VarDecl *var) {
  return var->isGetterMutating()
    ? PropertyWrapperMutability::Mutating
    : PropertyWrapperMutability::Nonmutating;
}

static PropertyWrapperMutability::Value
getSetterMutatingness(VarDecl *var, DeclContext *dc) {
  if (!var->isSettable(nullptr) ||
      !var->isSetterAccessibleFrom(dc))
    return PropertyWrapperMutability::DoesntExist;
  
  return var->isSetterMutating()
    ? PropertyWrapperMutability::Mutating
    : PropertyWrapperMutability::Nonmutating;
}

llvm::Expected<Optional<PropertyWrapperMutability>>
PropertyWrapperMutabilityRequest::evaluate(Evaluator &,
                                           VarDecl *var) const {
  VarDecl *originalVar = var;
  unsigned numWrappers = originalVar->getAttachedPropertyWrappers().size();
  bool isProjectedValue = false;
  if (numWrappers < 1) {
    originalVar = var->getOriginalWrappedProperty(
        PropertyWrapperSynthesizedPropertyKind::StorageWrapper);
    if (!originalVar)
      return None;

    numWrappers = originalVar->getAttachedPropertyWrappers().size();
    isProjectedValue = true;
  }

  // Make sure we don't ignore .swiftinterface files, because those will
  // have the accessors printed
  auto varSourceFile = var->getDeclContext()->getParentSourceFile();
  auto isVarNotInInterfaceFile =
      varSourceFile && varSourceFile->Kind != SourceFileKind::Interface;

  if (var->getParsedAccessor(AccessorKind::Get) && isVarNotInInterfaceFile)
    return None;
  if (var->getParsedAccessor(AccessorKind::Set) && isVarNotInInterfaceFile)
    return None;

  // Figure out which member we're looking through.
  auto varMember = isProjectedValue
    ? &PropertyWrapperTypeInfo::projectedValueVar
    : &PropertyWrapperTypeInfo::valueVar;

  // Start with the traits from the outermost wrapper.
  auto firstWrapper = originalVar->getAttachedPropertyWrapperTypeInfo(0);
  if (firstWrapper.*varMember == nullptr)
    return None;
  
  PropertyWrapperMutability result;
  
  result.Getter = getGetterMutatingness(firstWrapper.*varMember);
  result.Setter = getSetterMutatingness(firstWrapper.*varMember,
                                        var->getInnermostDeclContext());
  
  // Compose the traits of the following wrappers.
  for (unsigned i = 1; i < numWrappers && !isProjectedValue; ++i) {
    assert(var == originalVar);
    auto wrapper = var->getAttachedPropertyWrapperTypeInfo(i);
    if (!wrapper.valueVar)
      return None;
    
    PropertyWrapperMutability nextResult;
    nextResult.Getter =
                    result.composeWith(getGetterMutatingness(wrapper.valueVar));
    // A property must have a getter, so we can't compose a wrapper that
    // exposes a mutating getter wrapped inside a get-only wrapper.
    if (nextResult.Getter == PropertyWrapperMutability::DoesntExist) {
      auto &ctx = var->getASTContext();
      ctx.Diags.diagnose(var->getAttachedPropertyWrappers()[i]->getLocation(),
               diag::property_wrapper_mutating_get_composed_to_get_only,
               var->getAttachedPropertyWrappers()[i]->getTypeLoc(),
               var->getAttachedPropertyWrappers()[i-1]->getTypeLoc());

      return None;
    }
    nextResult.Setter =
              result.composeWith(getSetterMutatingness(wrapper.valueVar,
                                               var->getInnermostDeclContext()));
    result = nextResult;
  }
  assert(result.Getter != PropertyWrapperMutability::DoesntExist
         && "getter must exist");
  return result;
}

llvm::Expected<PropertyWrapperBackingPropertyInfo>
PropertyWrapperBackingPropertyInfoRequest::evaluate(Evaluator &evaluator,
                                                    VarDecl *var) const {
  // Determine the type of the backing property.
  auto wrapperType = var->getPropertyWrapperBackingPropertyType();
  if (!wrapperType || wrapperType->hasError())
    return PropertyWrapperBackingPropertyInfo();

  auto wrapperInfo = var->getAttachedPropertyWrapperTypeInfo(0);
  if (!wrapperInfo)
    return PropertyWrapperBackingPropertyInfo();

  // Compute the name of the storage type.
  ASTContext &ctx = var->getASTContext();
  SmallString<64> nameBuf;
  nameBuf = "_";
  nameBuf += var->getName().str();
  Identifier name = ctx.getIdentifier(nameBuf);

  // Determine the type of the storage.
  auto dc = var->getDeclContext();
  Type storageInterfaceType = wrapperType;
  Type storageType = dc->mapTypeIntoContext(storageInterfaceType);

  // Make sure that the property type matches the value of the
  // wrapper type.
  if (!storageInterfaceType->hasError()) {
    Type expectedPropertyType =
        computeWrappedValueType(var, storageInterfaceType);
    Type propertyType = var->getValueInterfaceType();
    assert(propertyType);
    if (!expectedPropertyType->hasError() &&
        !propertyType->hasError() &&
        !propertyType->isEqual(expectedPropertyType)) {
      var->diagnose(diag::property_wrapper_incompatible_property,
                    propertyType, wrapperType);
      var->setInvalid();
      if (auto nominalWrapper = wrapperType->getAnyNominal()) {
        nominalWrapper->diagnose(diag::property_wrapper_declared_here,
                                 nominalWrapper->getFullName());
      }
    }
  }

  // Create the backing storage property and note it in the cache.
  VarDecl *backingVar = new (ctx) VarDecl(/*IsStatic=*/var->isStatic(),
                                          VarDecl::Introducer::Var,
                                          /*IsCaptureList=*/false,
                                          var->getLoc(),
                                          name, dc);
  backingVar->setInterfaceType(storageInterfaceType);
  backingVar->setImplicit();
  backingVar->setOriginalWrappedProperty(var);

  // The backing storage is 'private'.
  backingVar->overwriteAccess(AccessLevel::Private);
  backingVar->overwriteSetterAccess(AccessLevel::Private);

  addMemberToContextIfNeeded(backingVar, dc, var);

  // Create the pattern binding declaration for the backing property.
  Pattern *pbdPattern = new (ctx) NamedPattern(backingVar, /*implicit=*/true);
  pbdPattern->setType(storageType);
  pbdPattern = TypedPattern::createImplicit(ctx, pbdPattern, storageType);
  auto pbd = PatternBindingDecl::createImplicit(
      ctx, var->getCorrectStaticSpelling(), pbdPattern,
      /*init*/ nullptr, dc, SourceLoc());
  addMemberToContextIfNeeded(pbd, dc, var);
  pbd->setStatic(var->isStatic());

  // Take the initializer from the original property.
  auto parentPBD = var->getParentPatternBinding();
  unsigned patternNumber = parentPBD->getPatternEntryIndexForVarDecl(var);
  
  // Force the default initializer to come into existence, if there is one,
  // and the wrapper doesn't provide its own.
  if (!parentPBD->isInitialized(patternNumber)
      && parentPBD->isDefaultInitializable(patternNumber)
      && !wrapperInfo.defaultInit) {
    auto ty = parentPBD->getPattern(patternNumber)->getType();
    if (auto defaultInit = TypeChecker::buildDefaultInitializer(ty))
      parentPBD->setInit(patternNumber, defaultInit);
  }
  
  if (parentPBD->isInitialized(patternNumber) &&
      !parentPBD->isInitializerChecked(patternNumber)) {
    TypeChecker::typeCheckPatternBinding(parentPBD, patternNumber);
  }

  Expr *originalInitialValue = nullptr;
  if (Expr *init = parentPBD->getInit(patternNumber)) {
    pbd->setInit(0, init);
    pbd->setInitializerChecked(0);
    originalInitialValue = findOriginalPropertyWrapperInitialValue(var, init);
  } else if (!parentPBD->isInitialized(patternNumber) &&
             wrapperInfo.defaultInit) {
    // FIXME: Record this expression somewhere so that DI can perform the
    // initialization itself.
    auto typeExpr = TypeExpr::createImplicit(storageType, ctx);
    Expr *initializer = CallExpr::createImplicit(ctx, typeExpr, {}, { });
    typeCheckSynthesizedWrapperInitializer(pbd, backingVar, parentPBD,
                                           initializer);
    pbd->setInit(0, initializer);
    pbd->setInitializerChecked(0);
  }

  // If there is a projection property (projectedValue) in the wrapper,
  // synthesize a computed property for '$foo'.
  VarDecl *storageVar = nullptr;
  if (wrapperInfo.projectedValueVar) {
    storageVar = synthesizePropertyWrapperStorageWrapperProperty(
        ctx, var, storageInterfaceType, wrapperInfo.projectedValueVar);
  }
  
  // Get the property wrapper information.
  if (!var->allAttachedPropertyWrappersHaveInitialValueInit() &&
      !originalInitialValue) {
    return PropertyWrapperBackingPropertyInfo(
        backingVar, storageVar, nullptr, nullptr, nullptr);
  }

  // Form the initialization of the backing property from a value of the
  // original property's type.
  OpaqueValueExpr *origValue =
      new (ctx) OpaqueValueExpr(var->getSourceRange(), var->getType(),
                                /*isPlaceholder=*/true);
  Expr *initializer = buildPropertyWrapperInitialValueCall(
      var, storageType, origValue,
      /*ignoreAttributeArgs=*/!originalInitialValue);
  typeCheckSynthesizedWrapperInitializer(
      pbd, backingVar, parentPBD, initializer);
  
  return PropertyWrapperBackingPropertyInfo(
      backingVar, storageVar, originalInitialValue, initializer, origValue);
}

/// Given a storage declaration in a protocol, set it up with the right
/// StorageImpl and add the right set of opaque accessors.
static void finishProtocolStorageImplInfo(AbstractStorageDecl *storage,
                                          StorageImplInfo &info) {
  if (auto *var = dyn_cast<VarDecl>(storage)) {
    SourceLoc typeLoc;
    if (auto *repr = var->getTypeReprOrParentPatternTypeRepr())
      typeLoc = repr->getLoc();
    
    if (info.hasStorage()) {
      // Protocols cannot have stored properties.
      if (var->isLet()) {
        var->diagnose(diag::protocol_property_must_be_computed_var)
            .fixItReplace(var->getParentPatternBinding()->getLoc(), "var")
            .fixItInsertAfter(typeLoc, " { get }");
      } else {
        auto diag = var->diagnose(diag::protocol_property_must_be_computed);
        auto braces = var->getBracesRange();

        if (braces.isValid())
          diag.fixItReplace(braces, "{ get <#set#> }");
        else
          diag.fixItInsertAfter(typeLoc, " { get <#set#> }");
      }
    }
  }

  auto protocol = cast<ProtocolDecl>(storage->getDeclContext());
  if (protocol->isObjC()) {
    info = StorageImplInfo::getComputed(info.supportsMutation());
  } else {
    info = StorageImplInfo::getOpaque(info.supportsMutation(),
                                      storage->getOpaqueReadOwnership());
  }
}

/// This emits a diagnostic with a fixit to remove the attribute.
template<typename ...ArgTypes>
void diagnoseAndRemoveAttr(Decl *D, DeclAttribute *attr,
                           ArgTypes &&...Args) {
  auto &ctx = D->getASTContext();
  ctx.Diags.diagnose(attr->getLocation(), std::forward<ArgTypes>(Args)...)
    .fixItRemove(attr->getRangeWithAt());
}

static void finishLazyVariableImplInfo(VarDecl *var,
                                       StorageImplInfo &info) {
  auto *attr = var->getAttrs().getAttribute<LazyAttr>();

  // It cannot currently be used on let's since we don't have a mutability model
  // that supports it.
  if (var->isLet())
    diagnoseAndRemoveAttr(var, attr, diag::lazy_not_on_let);

  // lazy must have an initializer.
  if (!var->getParentInitializer())
    diagnoseAndRemoveAttr(var, attr, diag::lazy_requires_initializer);

  bool invalid = false;

  if (isa<ProtocolDecl>(var->getDeclContext())) {
    diagnoseAndRemoveAttr(var, attr, diag::lazy_not_in_protocol);
    invalid = true;
  }

  // Lazy properties must be written as stored properties in the source.
  if (!info.isSimpleStored()) {
    diagnoseAndRemoveAttr(var, attr,
                          info.hasStorage()
                          ? diag::lazy_not_observable
                          : diag::lazy_not_on_computed);
    invalid = true;
  }

  // The pattern binding must only bind a single variable.
  if (!var->getParentPatternBinding()->getSingleVar())
    diagnoseAndRemoveAttr(var, attr, diag::lazy_requires_single_var);

  if (!invalid)
    info = StorageImplInfo::getMutableComputed();
}

static void finishPropertyWrapperImplInfo(VarDecl *var,
                                          StorageImplInfo &info) {
  auto parentSF = var->getDeclContext()->getParentSourceFile();
  if (!parentSF)
    return;

  // Properties with wrappers must not declare a getter or setter.
  if (!info.hasStorage() && parentSF->Kind != SourceFileKind::Interface) {
    auto &ctx = parentSF->getASTContext();
    for (auto attr : var->getAttrs().getAttributes<CustomAttr>())
      ctx.Diags.diagnose(attr->getLocation(), diag::property_wrapper_computed);

    return;
  }

  bool wrapperSetterIsUsable = false;
  if (var->getParsedAccessor(AccessorKind::Set)) {
    wrapperSetterIsUsable = true;
  } else if (parentSF && parentSF->Kind != SourceFileKind::Interface
             && !var->isLet()) {
    if (auto comp = var->getPropertyWrapperMutability()) {
      wrapperSetterIsUsable =
        comp->Setter != PropertyWrapperMutability::DoesntExist;
    } else {
      wrapperSetterIsUsable = true;
    }
  }

  if (wrapperSetterIsUsable)
    info = StorageImplInfo(ReadImplKind::Get, WriteImplKind::Set,
                           ReadWriteImplKind::Modify);
  else
    info = StorageImplInfo::getImmutableComputed();
}

static void finishNSManagedImplInfo(VarDecl *var,
                                    StorageImplInfo &info) {
  auto *attr = var->getAttrs().getAttribute<NSManagedAttr>();

  if (var->isLet())
    diagnoseAndRemoveAttr(var, attr, diag::attr_NSManaged_let_property);

  SourceFile *parentFile = var->getDeclContext()->getParentSourceFile();

  auto diagnoseNotStored = [&](unsigned kind) {
    // Skip diagnosing @NSManaged declarations in module interfaces. They are
    // properties that are stored, but have specially synthesized observers
    // and we should allow them to have getters and setters in a module
    // interface.
    if (parentFile && parentFile->Kind == SourceFileKind::Interface)
      return;

    diagnoseAndRemoveAttr(var, attr, diag::attr_NSManaged_not_stored, kind);
  };

  // @NSManaged properties must be written as stored.
  if (info.isSimpleStored()) {
    // @NSManaged properties end up being computed; complain if there is
    // an initializer.
    if (var->getParentInitializer()) {
      auto &Diags = var->getASTContext().Diags;
      Diags.diagnose(attr->getLocation(), diag::attr_NSManaged_initial_value)
           .highlight(var->getParentInitializer()->getSourceRange());
    }

    // Otherwise, ok.
    info = StorageImplInfo::getMutableComputed();

  } else if (info.getReadImpl() == ReadImplKind::Address ||
             info.getWriteImpl() == WriteImplKind::MutableAddress) {
    diagnoseNotStored(/*addressed*/ 2);
  } else if (info.getWriteImpl() == WriteImplKind::StoredWithObservers ||
             info.getWriteImpl() == WriteImplKind::InheritedWithObservers) {
    diagnoseNotStored(/*observing*/ 1);
  } else {
    diagnoseNotStored(/*computed*/ 0);
  }
}

static void finishStorageImplInfo(AbstractStorageDecl *storage,
                                  StorageImplInfo &info) {
  auto dc = storage->getDeclContext();

  if (auto var = dyn_cast<VarDecl>(storage)) {
    if (!info.hasStorage()) {
      if (auto *init = var->getParentInitializer()) {
        auto &Diags = var->getASTContext().Diags;
        Diags.diagnose(init->getLoc(), diag::getset_init)
             .highlight(init->getSourceRange());
      }
    }

    if (var->getAttrs().hasAttribute<LazyAttr>()) {
      finishLazyVariableImplInfo(var, info);
    } else if (var->getAttrs().hasAttribute<NSManagedAttr>()) {
      finishNSManagedImplInfo(var, info);
    } else if (var->hasAttachedPropertyWrapper()) {
      finishPropertyWrapperImplInfo(var, info);
    }
  }

  if (isa<ProtocolDecl>(dc))
    finishProtocolStorageImplInfo(storage, info);

  // If we have a stored property in an unsupported context, diagnose
  // and change it to computed to avoid confusing SILGen.

  // Note: Stored properties in protocols are diagnosed in
  // finishProtocolStorageImplInfo().

  if (info.hasStorage() && !storage->isStatic()) {
    if (isa<EnumDecl>(dc)) {
      storage->diagnose(diag::enum_stored_property);
      info = StorageImplInfo::getMutableComputed();
    } else if (isa<ExtensionDecl>(dc) &&
              !storage->getAttrs().getAttribute<DynamicReplacementAttr>()) {
      storage->diagnose(diag::extension_stored_property);

      info = (info.supportsMutation()
              ? StorageImplInfo::getMutableComputed()
              : StorageImplInfo::getImmutableComputed());
    }
  }
}

/// Gets the storage info of the provided storage decl if it has the
/// @_hasStorage attribute and it's not in SIL mode.
///
/// In this case, we say the decl is:
///
/// Read:
///   - Stored, always
/// Write:
///   - Stored, if the decl is a 'var'.
///   - StoredWithObservers, if the decl has a setter
///     - This indicates that the original decl had a 'didSet' and/or 'willSet'
///   - InheritedWithObservers, if the decl has a setter and is an overridde.
///   - Immutable, if the decl is a 'let' or it does not have a setter.
/// ReadWrite:
///   - Stored, if the decl has no accessors listed.
///   - Immutable, if the decl is a 'let' or it does not have a setter.
///   - MaterializeToTemporary, if the decl has a setter.
static StorageImplInfo classifyWithHasStorageAttr(VarDecl *var) {
  WriteImplKind writeImpl;
  ReadWriteImplKind readWriteImpl;

  if (var->getParsedAccessor(AccessorKind::Get) &&
      var->getParsedAccessor(AccessorKind::Set)) {
    // If we see `@_hasStorage var x: T { get set }`, then our property has
    // willSet/didSet observers.
    writeImpl = var->getAttrs().hasAttribute<OverrideAttr>() ?
      WriteImplKind::InheritedWithObservers :
      WriteImplKind::StoredWithObservers;
    readWriteImpl = ReadWriteImplKind::MaterializeToTemporary;
  } else if (var->isLet()) {
    writeImpl = WriteImplKind::Immutable;
    readWriteImpl = ReadWriteImplKind::Immutable;
  } else {
    // Default to stored writes.
    writeImpl = WriteImplKind::Stored;
    readWriteImpl = ReadWriteImplKind::Stored;
  }

  // Always force Stored reads if @_hasStorage is present.
  return StorageImplInfo(ReadImplKind::Stored, writeImpl, readWriteImpl);
}

llvm::Expected<StorageImplInfo>
StorageImplInfoRequest::evaluate(Evaluator &evaluator,
                                 AbstractStorageDecl *storage) const {
  if (auto *param = dyn_cast<ParamDecl>(storage)) {
    return StorageImplInfo::getSimpleStored(
      param->isInOut()
      ? StorageIsMutable
      : StorageIsNotMutable);
  }

  if (auto *var = dyn_cast<VarDecl>(storage)) {
    // Allow the @_hasStorage attribute to override all the accessors we parsed
    // when making the final classification.
    if (var->getAttrs().hasAttribute<HasStorageAttr>()) {
      // The SIL rules for @_hasStorage are slightly different from the non-SIL
      // rules. In SIL mode, @_hasStorage marks that the type is simply stored,
      // and the only thing that determines mutability is the existence of the
      // setter.
      //
      // FIXME: SIL should not be special cased here. The behavior should be
      //        consistent between SIL and non-SIL.
      //        The strategy here should be to keep track of all opaque accessors
      //        along with enough information to access the storage trivially
      //        if allowed. This could be a representational change to
      //        StorageImplInfo such that it keeps a bitset of listed accessors
      //        and dynamically determines the access strategy from that.
      auto *SF = storage->getDeclContext()->getParentSourceFile();
      if (SF && SF->Kind == SourceFileKind::SIL)
        return StorageImplInfo::getSimpleStored(
          var->getParsedAccessor(AccessorKind::Set)
          ? StorageIsMutable
          : StorageIsNotMutable);

      return classifyWithHasStorageAttr(var);
    }
  }

  bool hasWillSet = storage->getParsedAccessor(AccessorKind::WillSet);
  bool hasDidSet = storage->getParsedAccessor(AccessorKind::DidSet);
  bool hasSetter = storage->getParsedAccessor(AccessorKind::Set);
  bool hasModify = storage->getParsedAccessor(AccessorKind::Modify);
  bool hasMutableAddress = storage->getParsedAccessor(AccessorKind::MutableAddress);

  // 'get', 'read', and a non-mutable addressor are all exclusive.
  ReadImplKind readImpl;
  if (storage->getParsedAccessor(AccessorKind::Get)) {
    readImpl = ReadImplKind::Get;
  } else if (storage->getParsedAccessor(AccessorKind::Read)) {
    readImpl = ReadImplKind::Read;
  } else if (storage->getParsedAccessor(AccessorKind::Address)) {
    readImpl = ReadImplKind::Address;

  // If there's a writing accessor of any sort, there must also be a
  // reading accessor.
  } else if (hasSetter || hasModify || hasMutableAddress) {
    readImpl = ReadImplKind::Get;

  // Subscripts always have to have some sort of accessor; they can't be
  // purely stored.
  } else if (isa<SubscriptDecl>(storage)) {
    readImpl = ReadImplKind::Get;

  // Check if we have observers.
  } else if (hasWillSet || hasDidSet) {
    if (storage->getAttrs().hasAttribute<OverrideAttr>() &&
        storage->getDeclContext()->isTypeContext()) {
      readImpl = ReadImplKind::Inherited;
    } else {
      readImpl = ReadImplKind::Stored;
    }

  // Otherwise, it's stored.
  } else {
    readImpl = ReadImplKind::Stored;
  }

  // Prefer using 'set' and 'modify' over a mutable addressor.
  WriteImplKind writeImpl;
  ReadWriteImplKind readWriteImpl;
  if (hasSetter) {
    writeImpl = WriteImplKind::Set;
    if (hasModify) {
      readWriteImpl = ReadWriteImplKind::Modify;
    } else {
      readWriteImpl = ReadWriteImplKind::MaterializeToTemporary;
    }
  } else if (hasModify) {
    writeImpl = WriteImplKind::Modify;
    readWriteImpl = ReadWriteImplKind::Modify;
  } else if (hasMutableAddress) {
    writeImpl = WriteImplKind::MutableAddress;
    readWriteImpl = ReadWriteImplKind::MutableAddress;

  // Check if we have observers.
  } else if (readImpl == ReadImplKind::Inherited) {
    writeImpl = WriteImplKind::InheritedWithObservers;
    readWriteImpl = ReadWriteImplKind::MaterializeToTemporary;

  // Otherwise, it's stored.
  } else if (readImpl == ReadImplKind::Stored &&
             !cast<VarDecl>(storage)->isLet()) {
    if (hasWillSet || hasDidSet) {
      writeImpl = WriteImplKind::StoredWithObservers;
      readWriteImpl = ReadWriteImplKind::MaterializeToTemporary;
    } else {
      writeImpl = WriteImplKind::Stored;
      readWriteImpl = ReadWriteImplKind::Stored;
    }

  // Otherwise, it's immutable.
  } else {
    writeImpl = WriteImplKind::Immutable;
    readWriteImpl = ReadWriteImplKind::Immutable;
  }

  StorageImplInfo info(readImpl, writeImpl, readWriteImpl);
  finishStorageImplInfo(storage, info);

  return info;
}
