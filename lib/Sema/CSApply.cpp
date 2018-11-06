//===--- CSApply.cpp - Constraint Application -----------------------------===//
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
// This file implements application of a solution to a constraint
// system to a particular expression, resulting in a
// fully-type-checked expression.
//
//===----------------------------------------------------------------------===//

#include "ConstraintSystem.h"
#include "CodeSynthesis.h"
#include "CSDiagnostics.h"
#include "MiscDiagnostics.h"
#include "TypeCheckProtocol.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/Basic/StringExtras.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/SaveAndRestore.h"

using namespace swift;
using namespace constraints;

/// \brief Retrieve the fixed type for the given type variable.
Type Solution::getFixedType(TypeVariableType *typeVar) const {
  auto knownBinding = typeBindings.find(typeVar);
  assert(knownBinding != typeBindings.end());
  return knownBinding->second;
}

/// Determine whether the given type is an opened AnyObject.
///
/// This comes up in computeSubstitutions() when accessing
/// members via dynamic lookup.
static bool isOpenedAnyObject(Type type) {
  auto archetype = type->getAs<ArchetypeType>();
  if (!archetype)
    return false;

  auto existential = archetype->getOpenedExistentialType();
  if (!existential)
    return false;

  return existential->isAnyObject();
}

SubstitutionMap Solution::computeSubstitutions(
                               GenericSignature *sig,
                               ConstraintLocatorBuilder locatorBuilder) const {
  if (sig == nullptr)
    return SubstitutionMap();

  // Gather the substitutions from dependent types to concrete types.
  auto locator = getConstraintSystem().getConstraintLocator(locatorBuilder);
  auto openedTypes = OpenedTypes.find(locator);

  // If we have a member reference on an existential, there are no
  // opened types or substitutions.
  if (openedTypes == OpenedTypes.end())
    return SubstitutionMap();

  TypeSubstitutionMap subs;
  for (const auto &opened : openedTypes->second)
    subs[opened.first] = getFixedType(opened.second);

  auto &tc = getConstraintSystem().getTypeChecker();

  auto lookupConformanceFn =
      [&](CanType original, Type replacement, ProtocolDecl *protoType)
          -> Optional<ProtocolConformanceRef> {
    if (replacement->hasError() ||
        isOpenedAnyObject(replacement) ||
        replacement->is<GenericTypeParamType>()) {
      return ProtocolConformanceRef(protoType);
    }

    return tc.conformsToProtocol(replacement, protoType,
                                 getConstraintSystem().DC,
                                 (ConformanceCheckFlags::InExpression|
                                  ConformanceCheckFlags::Used));
  };

  return SubstitutionMap::get(sig,
                              QueryTypeSubstitutionMap{subs},
                              lookupConformanceFn);
}

/// \brief Find a particular named function witness for a type that conforms to
/// the given protocol.
///
/// \param tc The type check we're using.
///
/// \param dc The context in which we need a witness.
///
/// \param type The type whose witness to find.
///
/// \param proto The protocol to which the type conforms.
///
/// \param name The name of the requirement.
///
/// \param diag The diagnostic to emit if the protocol definition doesn't
/// have a requirement with the given name.
///
/// \returns The named witness, or an empty ConcreteDeclRef if no witness
/// could be found.
ConcreteDeclRef findNamedWitnessImpl(
                        TypeChecker &tc, DeclContext *dc, Type type,
                        ProtocolDecl *proto, DeclName name,
                        Diag<> diag,
                        Optional<ProtocolConformanceRef> conformance = None) {
  // Find the named requirement.
  ValueDecl *requirement = nullptr;
  for (auto member : proto->getMembers()) {
    auto d = dyn_cast<ValueDecl>(member);
    if (!d || !d->hasName())
      continue;

    if (d->getFullName().matchesRef(name)) {
      requirement = d;
      break;
    }
  }

  if (!requirement || requirement->isInvalid()) {
    tc.diagnose(proto->getLoc(), diag);
    return nullptr;
  }

  // Find the member used to satisfy the named requirement.
  if (!conformance) {
    conformance = tc.conformsToProtocol(type, proto, dc,
                                        ConformanceCheckFlags::InExpression);
    if (!conformance)
      return nullptr;
  }

  // For a type with dependent conformance, just return the requirement from
  // the protocol. There are no protocol conformance tables.
  if (!conformance->isConcrete()) {
    auto subMap = SubstitutionMap::getProtocolSubstitutions(proto, type,
                                                            *conformance);
    return ConcreteDeclRef(requirement, subMap);
  }

  auto concrete = conformance->getConcrete();
  return concrete->getWitnessDeclRef(requirement, &tc);
}

static bool shouldAccessStorageDirectly(Expr *base, VarDecl *member,
                                        DeclContext *DC) {
  // This only matters for stored properties.
  if (!member->hasStorage())
    return false;

  // ... referenced from constructors and destructors.
  auto *AFD = dyn_cast<AbstractFunctionDecl>(DC);
  if (AFD == nullptr)
    return false;

  if (!isa<ConstructorDecl>(AFD) && !isa<DestructorDecl>(AFD))
    return false;

  // ... via a "self.property" reference.
  auto *DRE = dyn_cast<DeclRefExpr>(base);
  if (DRE == nullptr)
    return false;

  if (AFD->getImplicitSelfDecl() != cast<DeclRefExpr>(base)->getDecl())
    return false;

  // Convenience initializers do not require special handling.
  // FIXME: This is a language change -- for now, keep the old behavior
#if 0
  if (auto *CD = dyn_cast<ConstructorDecl>(AFD))
    if (!CD->isDesignatedInit())
      return false;
#endif

  // Ctor or dtor are for immediate class, not a derived class.
  if (!AFD->getParent()->getDeclaredInterfaceType()->isEqual(
       member->getDeclContext()->getDeclaredInterfaceType()))
    return false;

  // If the storage is resilient, we cannot access it directly at all.
  if (member->isResilient(DC->getParentModule(),
                          DC->getResilienceExpansion()))
    return false;

  return true;
}

/// Return the implicit access kind for a MemberRefExpr with the
/// specified base and member in the specified DeclContext.
static AccessSemantics
getImplicitMemberReferenceAccessSemantics(Expr *base, VarDecl *member,
                                          DeclContext *DC) {
  // Properties that have storage and accessors are frequently accessed through
  // accessors.  However, in the init and destructor methods for the type
  // immediately containing the property, accesses are done direct.
  if (shouldAccessStorageDirectly(base, member, DC)) {
    // Access this directly instead of going through (e.g.) observing or
    // trivial accessors.
    return AccessSemantics::DirectToStorage;
  }
  
  // Check for property behavior initializations.
  if (auto *AFD_DC = dyn_cast<AbstractFunctionDecl>(DC)) {
    if (member->hasBehaviorNeedingInitialization() &&
        // In a ctor.
        isa<ConstructorDecl>(AFD_DC) &&
        
        // Ctor is for immediate class, not a derived class.
        AFD_DC->getParent()->getDeclaredInterfaceType()->isEqual(
          member->getDeclContext()->getDeclaredInterfaceType()) &&
        
        // Is a "self.property" reference.
        isa<DeclRefExpr>(base) &&
        AFD_DC->getImplicitSelfDecl() == cast<DeclRefExpr>(base)->getDecl()) {
      // Do definite initialization analysis to handle this property.
      return AccessSemantics::BehaviorInitialization;
    }
  }

  // Check whether this is a member access on 'self'.
  bool isAccessOnSelf = false;
  if (auto *baseDRE = dyn_cast<DeclRefExpr>(base->getValueProvidingExpr()))
    if (auto *baseVar = dyn_cast<VarDecl>(baseDRE->getDecl()))
      isAccessOnSelf = baseVar->isSelfParameter();

  // If the value is always directly accessed from this context, do it.
  return member->getAccessSemanticsFromContext(DC, isAccessOnSelf);
}

bool ConstraintSystem::isTypeReference(const Expr *E) {
  return E->isTypeReference([&](const Expr *E) -> Type { return getType(E); });
}

bool ConstraintSystem::isStaticallyDerivedMetatype(const Expr *E) {
  return E->isStaticallyDerivedMetatype(
      [&](const Expr *E) -> Type { return getType(E); });
}

Type ConstraintSystem::getInstanceType(const TypeExpr *E) {
  return E->getInstanceType([&](const Expr *E) -> bool { return hasType(E); },
                            [&](const Expr *E) -> Type { return getType(E); });
}

Type ConstraintSystem::getResultType(const AbstractClosureExpr *E) {
  return E->getResultType([&](const Expr *E) -> Type { return getType(E); });
}

static bool buildObjCKeyPathString(KeyPathExpr *E,
                                   llvm::SmallVectorImpl<char> &buf) {
  for (auto &component : E->getComponents()) {
    switch (component.getKind()) {
    case KeyPathExpr::Component::Kind::OptionalChain:
    case KeyPathExpr::Component::Kind::OptionalForce:
    case KeyPathExpr::Component::Kind::OptionalWrap:
      // KVC propagates nulls, so these don't affect the key path string.
      continue;
    case KeyPathExpr::Component::Kind::Identity:
      // The identity component can be elided from the KVC string (unless it's
      // the only component, in which case we use @"self").
      continue;

    case KeyPathExpr::Component::Kind::Property: {
      // Property references must be to @objc properties.
      // TODO: If we added special properties matching KVC operators like '@sum',
      // '@count', etc. those could be mapped too.
      auto property = cast<VarDecl>(component.getDeclRef().getDecl());
      if (!property->isObjC())
        return false;
      if (!buf.empty()) {
        buf.push_back('.');
      }
      auto objcName = property->getObjCPropertyName().str();
      buf.append(objcName.begin(), objcName.end());
      continue;
    }
    case KeyPathExpr::Component::Kind::Subscript: {
      // Subscripts aren't generally represented in KVC.
      // TODO: There are some subscript forms we could map to KVC, such as
      // when indexing a Dictionary or NSDictionary by string, or when applying
      // a mapping subscript operation to Array/Set or NSArray/NSSet.
      return false;
    case KeyPathExpr::Component::Kind::Invalid:
    case KeyPathExpr::Component::Kind::UnresolvedProperty:
    case KeyPathExpr::Component::Kind::UnresolvedSubscript:
      // Don't bother building the key path string if the key path didn't even
      // resolve.
      return false;
    }
    }
  }
  
  // If there are no non-identity components, this is the "self" key.
  if (buf.empty()) {
    auto self = StringRef("self");
    buf.append(self.begin(), self.end());
  }
  
  return true;
}

/// Determine whether the given type refers to a non-final class (or
/// dynamic self of one).
static bool isNonFinalClass(Type type) {
  if (auto dynamicSelf = type->getAs<DynamicSelfType>())
    type = dynamicSelf->getSelfType();

  if (auto classDecl = type->getClassOrBoundGenericClass())
    return !classDecl->isFinal();

  if (auto archetype = type->getAs<ArchetypeType>())
    if (auto super = archetype->getSuperclass())
      return isNonFinalClass(super);

  if (type->isExistentialType())
    return true;

  return false;
}

// Non-required constructors may not be not inherited. Therefore when
// constructing a class object, either the metatype must be statically
// derived (rather than an arbitrary value of metatype type) or the referenced
// constructor must be required.
static bool
diagnoseInvalidDynamicConstructorReferences(ConstraintSystem &cs,
                                            Expr *base,
                                            DeclNameLoc memberRefLoc,
                                            ConstructorDecl *ctorDecl,
                                            bool SuppressDiagnostics) {
  auto &tc = cs.getTypeChecker();
  auto baseTy = cs.getType(base)->getRValueType();
  auto instanceTy = baseTy->getMetatypeInstanceType();

  bool isStaticallyDerived =
    base->isStaticallyDerivedMetatype(
      [&](const Expr *expr) -> Type {
        return cs.getType(expr);
      });

  // FIXME: The "hasClangNode" check here is a complete hack.
  if (isNonFinalClass(instanceTy) &&
      !isStaticallyDerived &&
      !ctorDecl->hasClangNode() &&
      !(ctorDecl->isRequired() ||
        ctorDecl->getDeclContext()->getSelfProtocolDecl())) {
    if (SuppressDiagnostics)
      return false;

    tc.diagnose(memberRefLoc, diag::dynamic_construct_class, instanceTy)
      .highlight(base->getSourceRange());
    auto ctor = cast<ConstructorDecl>(ctorDecl);
    tc.diagnose(ctorDecl, diag::note_nonrequired_initializer,
                ctor->isImplicit(), ctor->getFullName());
  // Constructors cannot be called on a protocol metatype, because there is no
  // metatype to witness it.
  } else if (isa<ConstructorDecl>(ctorDecl) &&
             baseTy->is<MetatypeType>() &&
             instanceTy->isExistentialType()) {
    if (SuppressDiagnostics)
      return false;

    if (isStaticallyDerived) {
      tc.diagnose(memberRefLoc, diag::construct_protocol_by_name, instanceTy)
        .highlight(base->getSourceRange());
    } else {
      tc.diagnose(memberRefLoc, diag::construct_protocol_value, baseTy)
        .highlight(base->getSourceRange());
    }
  }
  return true;
}

/// Form a type checked expression for the index of a @dynamicMemberLookup
/// subscript index expression.  This will have tuple type of (dynamicMember:T).
static Expr *getDMLIndexExpr(StringRef name, Type ty, SourceLoc loc,
                             DeclContext *dc, ConstraintSystem &cs) {
  auto &ctx = cs.TC.Context;
  
  // Build and type check the string literal index value to the specific
  // string type expected by the subscript.
  Expr *nameExpr = new (ctx)
    StringLiteralExpr(name, loc, /*implicit*/true);
  
  
  // Build a tuple so that argument has a label.
  Expr *tuple = TupleExpr::create(ctx, loc, nameExpr, ctx.Id_dynamicMember, loc,
                                  loc, /*hasTrailingClosure*/false,
                                  /*implicit*/true);
  (void)cs.TC.typeCheckExpression(tuple, dc, TypeLoc::withoutLoc(ty),
                                  CTP_CallArgument);
  cs.cacheExprTypes(tuple);
  return tuple;
}


namespace {

  /// \brief Rewrites an expression by applying the solution of a constraint
  /// system to that expression.
  class ExprRewriter : public ExprVisitor<ExprRewriter, Expr *> {
  public:
    ConstraintSystem &cs;
    DeclContext *dc;
    const Solution &solution;
    bool SuppressDiagnostics;

    /// Recognize used conformances from an imported type when we must emit
    /// the witness table.
    ///
    /// This arises in _BridgedStoredNSError, where we wouldn't
    /// otherwise pull in the witness table, causing dynamic casts to
    /// perform incorrectly, and _ErrorCodeProtocol, where we need to
    /// check for _BridgedStoredNSError conformances on the
    /// corresponding ErrorType.
    void checkForImportedUsedConformances(Type toType) {
      cs.getTypeChecker().useBridgedNSErrorConformances(dc, toType);
    }

    /// \brief Coerce the given tuple to another tuple type.
    ///
    /// \param expr The expression we're converting.
    ///
    /// \param fromTuple The tuple type we're converting from, which is the same
    /// as \c expr->getType().
    ///
    /// \param toTuple The tuple type we're converting to.
    ///
    /// \param locator Locator describing where this tuple conversion occurs.
    ///
    /// \param sources The sources of each of the elements to be used in the
    /// resulting tuple, as provided by \c computeTupleShuffle.
    ///
    /// \param variadicArgs The source indices that are mapped to the variadic
    /// parameter of the resulting tuple, as provided by \c computeTupleShuffle.
    ///
    /// \param typeFromPattern Optionally, the caller can specify the pattern
    /// from where the toType is derived, so that we can deliver better fixit.
    Expr *coerceTupleToTuple(Expr *expr, TupleType *fromTuple,
                             TupleType *toTuple,
                             ConstraintLocatorBuilder locator,
                             SmallVectorImpl<int> &sources,
                             SmallVectorImpl<unsigned> &variadicArgs,
                             Optional<Pattern*> typeFromPattern = None);

    /// \brief Coerce a subclass, class-constrained archetype, class-constrained
    /// existential or to a superclass type.
    ///
    /// Also supports metatypes of the above.
    ///
    /// \param expr The expression to be coerced.
    /// \param toType The type to which the expression will be coerced.
    /// \param locator Locator describing where this conversion occurs.
    ///
    /// \return The coerced expression, whose type will be equivalent to
    /// \c toType.
    Expr *coerceSuperclass(Expr *expr, Type toType,
                           ConstraintLocatorBuilder locator);

    /// \brief Coerce the given value to existential type.
    ///
    /// The following conversions are supported:
    /// - concrete to existential
    /// - existential to existential
    /// - concrete metatype to existential metatype
    /// - existential metatype to existential metatype
    ///
    /// \param expr The expression to be coerced.
    /// \param toType The type to which the expression will be coerced.
    /// \param locator Locator describing where this conversion occurs.
    ///
    /// \return The coerced expression, whose type will be equivalent to
    /// \c toType.
    Expr *coerceExistential(Expr *expr, Type toType,
                            ConstraintLocatorBuilder locator);

    /// \brief Coerce an expression of (possibly unchecked) optional
    /// type to have a different (possibly unchecked) optional type.
    Expr *coerceOptionalToOptional(Expr *expr, Type toType,
                                   ConstraintLocatorBuilder locator,
                                   Optional<Pattern*> typeFromPattern = None);

    /// \brief Coerce an expression of implicitly unwrapped optional type to its
    /// underlying value type, in the correct way for an implicit
    /// look-through.
    Expr *coerceImplicitlyUnwrappedOptionalToValue(Expr *expr, Type objTy);

    /// Peephole an array upcast.
    void peepholeArrayUpcast(ArrayExpr *expr, Type toType, bool bridged,
                             Type elementType,
                             ConstraintLocatorBuilder locator);

    /// Peephole a dictionary upcast.
    void peepholeDictionaryUpcast(DictionaryExpr *expr, Type toType,
                                  bool bridged, Type keyType,
                                  Type valueType,
                                  ConstraintLocatorBuilder locator);

    /// Try to peephole the collection upcast, eliminating the need for
    /// a separate collection-upcast expression.
    ///
    /// \returns true if the peephole operation succeeded, in which case
    /// \c expr already subsumes the upcast.
    bool peepholeCollectionUpcast(Expr *expr, Type toType,  bool bridged,
                                  ConstraintLocatorBuilder locator);

    /// \brief Build a collection upcast expression.
    ///
    /// \param bridged Whether this is a bridging conversion, meaning that the
    /// element types themselves are bridged (vs. simply coerced).
    Expr *buildCollectionUpcastExpr(Expr *expr, Type toType,
                                    bool bridged,
                                    ConstraintLocatorBuilder locator);

    /// Build the expression that performs a bridging operation from the
    /// given expression to the given \c toType.
    Expr *buildObjCBridgeExpr(Expr *expr, Type toType,
                              ConstraintLocatorBuilder locator);

    static Type getBaseType(AnyFunctionType *fnType,
                            bool wantsRValueInstanceType = true) {
      auto params = fnType->getParams();
      assert(params.size() == 1);

      const auto &base = params.front();
      if (wantsRValueInstanceType)
        return base.getPlainType()->getMetatypeInstanceType();

      return base.getOldType();
    }

  public:
    /// \brief Build a reference to the given declaration.
    Expr *buildDeclRef(OverloadChoice choice, DeclNameLoc loc, Type openedType,
                       ConstraintLocatorBuilder locator, bool implicit,
                       FunctionRefKind functionRefKind,
                       AccessSemantics semantics) {
      auto *decl = choice.getDecl();

      // Determine the declaration selected for this overloaded reference.
      auto &ctx = cs.getASTContext();
      
      // If this is a member of a nominal type, build a reference to the
      // member with an implied base type.
      if (decl->getDeclContext()->isTypeContext() && isa<FuncDecl>(decl)) {
        assert(cast<FuncDecl>(decl)->isOperator() && "Must be an operator");

        auto openedFnType = openedType->castTo<FunctionType>();
        auto simplifiedFnType
          = simplifyType(openedFnType)->castTo<FunctionType>();
        auto baseTy = getBaseType(simplifiedFnType);

        // Handle operator requirements found in protocols.
        if (auto proto = dyn_cast<ProtocolDecl>(decl->getDeclContext())) {
          // If we don't have an archetype or existential, we have to call the
          // witness.
          // FIXME: This is awful. We should be able to handle this as a call to
          // the protocol requirement with Self == the concrete type, and SILGen
          // (or later) can devirtualize as appropriate.
          if (!baseTy->is<ArchetypeType>() && !baseTy->isAnyExistentialType()) {
            auto &tc = cs.getTypeChecker();
            auto conformance =
              tc.conformsToProtocol(
                        baseTy, proto, cs.DC,
                        (ConformanceCheckFlags::InExpression|
                         ConformanceCheckFlags::Used));
            if (conformance && conformance->isConcrete()) {
              if (auto witness =
                      conformance->getConcrete()->getWitnessDecl(decl, &tc)) {
                // Hack up an AST that we can type-check (independently) to get
                // it into the right form.
                // FIXME: the hop through 'getDecl()' is because
                // SpecializedProtocolConformance doesn't substitute into
                // witnesses' ConcreteDeclRefs.
                Type expectedFnType = simplifiedFnType->getResult();
                Expr *refExpr;
                if (witness->getDeclContext()->isTypeContext()) {
                  Expr *base =
                    TypeExpr::createImplicitHack(loc.getBaseNameLoc(), baseTy,
                                                 ctx);
                  refExpr = new (ctx) MemberRefExpr(base, SourceLoc(), witness,
                                                    loc, /*Implicit=*/true);
                } else {
                  auto declRefExpr =  new (ctx) DeclRefExpr(witness, loc,
                                                            /*Implicit=*/false);
                  declRefExpr->setFunctionRefKind(functionRefKind);
                  refExpr = declRefExpr;
                }

                auto resultTy = tc.typeCheckExpression(
                    refExpr, cs.DC, TypeLoc::withoutLoc(expectedFnType),
                    CTP_CannotFail);
                if (!resultTy)
                  return nullptr;

                cs.cacheExprTypes(refExpr);

                // Remove an outer function-conversion expression. This
                // happens when we end up referring to a witness for a
                // superclass conformance, and 'Self' differs.
                if (auto fnConv = dyn_cast<FunctionConversionExpr>(refExpr))
                  refExpr = fnConv->getSubExpr();

                return forceUnwrapIfExpected(refExpr, choice, locator);
              }
            }
          }
        }

        // Build a reference to the protocol requirement.
        Expr *base =
          TypeExpr::createImplicitHack(loc.getBaseNameLoc(), baseTy, ctx);
        cs.cacheExprTypes(base);

        return buildMemberRef(base, openedType, SourceLoc(), choice, loc,
                              openedFnType->getResult(), locator, locator,
                              implicit, functionRefKind, semantics,
                              /*isDynamic=*/false);
      }

      SubstitutionMap substitutions;

      // Due to a SILGen quirk, unqualified property references do not
      // need substitutions.
      if (!isa<VarDecl>(decl)) {
        substitutions =
          solution.computeSubstitutions(
            decl->getInnermostDeclContext()->getGenericSignatureOfContext(),
            locator);
      }

      auto type = solution.simplifyType(openedType);
      auto declRefExpr =
        new (ctx) DeclRefExpr(ConcreteDeclRef(decl, substitutions),
                              loc, implicit, semantics, type);
      cs.cacheType(declRefExpr);
      declRefExpr->setFunctionRefKind(functionRefKind);
      return forceUnwrapIfExpected(declRefExpr, choice, locator);
    }

    /// Describes an opened existential that has not yet been closed.
    struct OpenedExistential {
      /// The archetype describing this opened existential.
      ArchetypeType *Archetype;

      /// The existential value being opened.
      Expr *ExistentialValue;

      /// The opaque value (of archetype type) stored within the
      /// existential.
      OpaqueValueExpr *OpaqueValue;

      /// The depth of this currently-opened existential. Once the
      /// depth of the expression stack is equal to this value, the
      /// existential can be closed.
      unsigned Depth;
    };

    /// A stack of opened existentials that have not yet been closed.
    /// Ordered by decreasing depth.
    llvm::SmallVector<OpenedExistential, 2> OpenedExistentials;

    /// A stack of expressions being walked, used to compute existential depth.
    llvm::SmallVector<Expr *, 8> ExprStack;

    /// Members which are AbstractFunctionDecls but not FuncDecls cannot
    /// mutate self.
    bool isNonMutatingMember(ValueDecl *member) {
      if (!isa<AbstractFunctionDecl>(member))
        return false;
      return !isa<FuncDecl>(member) || !cast<FuncDecl>(member)->isMutating();
    }

    unsigned getNaturalArgumentCount(ValueDecl *member) {
      if (isa<AbstractFunctionDecl>(member)) {
        // For functions, close the existential once the function
        // has been fully applied.
        return 2;
      } else {
        // For storage, close the existential either when it's
        // accessed (if it's an rvalue only) or when it is loaded or
        // stored (if it's an lvalue).
        assert(isa<AbstractStorageDecl>(member) &&
              "unknown member when opening existential");
        return 1;
      }
    }

    /// If the expression might be a dynamic method call, return the base
    /// value for the call.
    Expr *getBaseExpr(Expr *expr) {
      // Keep going up as long as this expression is the parent's base.
      if (auto unresolvedDot = dyn_cast<UnresolvedDotExpr>(expr)) {
        return unresolvedDot->getBase();
      // Remaining cases should only come up when we're re-typechecking.
      // FIXME: really it would be much better if Sema had stricter phase
      // separation.
      } else if (auto dotSyntax = dyn_cast<DotSyntaxCallExpr>(expr)) {
        return dotSyntax->getArg();
      } else if (auto ctorRef = dyn_cast<ConstructorRefCallExpr>(expr)) {
        return ctorRef->getArg();
      } else if (auto apply = dyn_cast<ApplyExpr>(expr)) {
        return apply->getFn();
      } else if (auto lookupRef = dyn_cast<LookupExpr>(expr)) {
        return lookupRef->getBase();
      } else if (auto load = dyn_cast<LoadExpr>(expr)) {
        return load->getSubExpr();
      } else if (auto inout = dyn_cast<InOutExpr>(expr)) {
        return inout->getSubExpr();
      } else if (auto force = dyn_cast<ForceValueExpr>(expr)) {
        return force->getSubExpr();
      } else {
        return nullptr;
      }
    }

    /// Calculates the nesting depth of the current application.
    unsigned getArgCount(unsigned maxArgCount) {
      unsigned e = ExprStack.size();
      unsigned argCount;

      // Starting from the current expression, count up if the expression is
      // equal to its parent expression's base.
      Expr *prev = ExprStack.back();

      for (argCount = 1; argCount < maxArgCount && argCount < e; argCount++) {
        Expr *result = ExprStack[e - argCount - 1];
        Expr *base = getBaseExpr(result);
        if (base != prev)
          break;
        prev = result;
      }

      // Invalid case -- direct call of a metatype. Has one less argument
      // application because there's no ".init".
      if (isa<ApplyExpr>(ExprStack.back()))
        argCount--;

      return argCount;
    }

    /// Open an existential value into a new, opaque value of
    /// archetype type.
    ///
    /// \param base An expression of existential type whose value will
    /// be opened.
    ///
    /// \param archetype The archetype that describes the opened existential
    /// type.
    ///
    /// \param member The member that is being referenced on the existential
    /// type.
    ///
    /// \returns An OpaqueValueExpr that provides a reference to the value
    /// stored within the expression or its metatype (if the base was a
    /// metatype).
    Expr *openExistentialReference(Expr *base, ArchetypeType *archetype,
                                   ValueDecl *member) {
      assert(archetype && "archetype not already opened?");

      auto &tc = cs.getTypeChecker();

      // Dig out the base type.
      Type baseTy = cs.getType(base);

      // Look through lvalues.
      bool isLValue = false;
      if (auto lvalueTy = baseTy->getAs<LValueType>()) {
        isLValue = true;
        baseTy = lvalueTy->getObjectType();
      }

      // Look through metatypes.
      bool isMetatype = false;
      if (auto metaTy = baseTy->getAs<AnyMetatypeType>()) {
        isMetatype = true;
        baseTy = metaTy->getInstanceType();
      }

      assert(baseTy->isAnyExistentialType() && "Type must be existential");

      // If the base was an lvalue but it will only be treated as an
      // rvalue, turn the base into an rvalue now. This results in
      // better SILGen.
      if (isLValue &&
          (isNonMutatingMember(member) ||
           member->getDeclContext()->getDeclaredInterfaceType()
             ->hasReferenceSemantics())) {
        base = cs.coerceToRValue(base);
        isLValue = false;
      }

      // Determine the number of applications that need to occur before
      // we can close this existential, and record it.
      unsigned maxArgCount = getNaturalArgumentCount(member);
      unsigned depth = ExprStack.size() - getArgCount(maxArgCount);

      // Create the opaque opened value. If we started with a
      // metatype, it's a metatype.
      Type opaqueType = archetype;
      if (isMetatype)
        opaqueType = MetatypeType::get(opaqueType);
      if (isLValue)
        opaqueType = LValueType::get(opaqueType);

      ASTContext &ctx = tc.Context;
      auto archetypeVal = new (ctx) OpaqueValueExpr(base->getLoc(), opaqueType);
      cs.cacheType(archetypeVal);

      // Record the opened existential.
      OpenedExistentials.push_back({archetype, base, archetypeVal, depth});

      return archetypeVal;
    }

    /// Trying to close the active existential, if there is one.
    bool closeExistential(Expr *&result, ConstraintLocatorBuilder locator,
                          bool force=false) {
      if (OpenedExistentials.empty())
        return false;

      auto &record = OpenedExistentials.back();
      assert(record.Depth <= ExprStack.size());

      if (!force && record.Depth < ExprStack.size() - 1)
        return false;

      // If we had a return type of 'Self', erase it.
      ConstraintSystem &innerCS = solution.getConstraintSystem();
      auto &tc = innerCS.getTypeChecker();
      Type resultTy;
      resultTy = cs.getType(result);
      if (resultTy->hasOpenedExistential(record.Archetype)) {
        Type erasedTy = resultTy->eraseOpenedExistential(record.Archetype);
        auto range = result->getSourceRange();
        result = coerceToType(result, erasedTy, locator);
        // FIXME: Implement missing tuple-to-tuple conversion
        if (result == nullptr) {
          result = new (tc.Context) ErrorExpr(range);
          cs.setType(result, erasedTy);
          // The opaque value is no longer reachable in an AST walk as
          // a result of the result above being replaced with an
          // ErrorExpr, but there is code expecting to have a type set
          // on it. Since we no longer have a reachable reference,
          // we'll null this out.
          record.OpaqueValue = nullptr;
        }
      }

      // Form the open-existential expression.
      result = new (tc.Context) OpenExistentialExpr(
                                  record.ExistentialValue,
                                  record.OpaqueValue,
                                  result, cs.getType(result));
      cs.cacheType(result);

      OpenedExistentials.pop_back();
      return true;
    }

    /// \brief Build a new member reference with the given base and member.
    Expr *buildMemberRef(Expr *base, Type openedFullType, SourceLoc dotLoc,
                         OverloadChoice choice, DeclNameLoc memberLoc,
                         Type openedType, ConstraintLocatorBuilder locator,
                         ConstraintLocatorBuilder memberLocator, bool Implicit,
                         FunctionRefKind functionRefKind,
                         AccessSemantics semantics, bool isDynamic) {
      ValueDecl *member = choice.getDecl();

      auto &tc = cs.getTypeChecker();
      auto &context = tc.Context;

      bool isSuper = base->isSuperExpr();

      // The formal type of the 'self' value for the call.
      Type baseTy = cs.getType(base)->getRValueType();

      // Figure out the actual base type, and whether we have an instance of
      // that type or its metatype.
      bool baseIsInstance = true;
      if (auto baseMeta = baseTy->getAs<AnyMetatypeType>()) {
        baseIsInstance = false;
        baseTy = baseMeta->getInstanceType();

        // If the member is a constructor, verify that it can be legally
        // referenced from this base.
        if (auto ctor = dyn_cast<ConstructorDecl>(member)) {
          if (!diagnoseInvalidDynamicConstructorReferences(cs, base, memberLoc,
                                           ctor, SuppressDiagnostics))
            return nullptr;
        }
      }

      // Build a member reference.
      SubstitutionMap substitutions =
        solution.computeSubstitutions(
            member->getInnermostDeclContext()->getGenericSignatureOfContext(),
            memberLocator);
      auto memberRef = ConcreteDeclRef(member, substitutions);

      cs.TC.requestMemberLayout(member);

      auto refTy = solution.simplifyType(openedFullType);

      // If we're referring to the member of a module, it's just a simple
      // reference.
      if (baseTy->is<ModuleType>()) {
        assert(semantics == AccessSemantics::Ordinary &&
               "Direct property access doesn't make sense for this");
        auto ref = new (context) DeclRefExpr(memberRef, memberLoc, Implicit);
        cs.setType(ref, refTy);
        ref->setFunctionRefKind(functionRefKind);
        auto *DSBI = cs.cacheType(new (context) DotSyntaxBaseIgnoredExpr(
            base, dotLoc, ref, cs.getType(ref)));
        return forceUnwrapIfExpected(DSBI, choice, memberLocator);
      }

      // The formal type of the 'self' value for the member's declaration.
      Type containerTy = getBaseType(refTy->castTo<FunctionType>());

      // If we have an opened existential, selfTy and baseTy will both be
      // the same opened existential type.
      Type selfTy = containerTy;

      // If we opened up an existential when referencing this member, update
      // the base accordingly.
      auto knownOpened = solution.OpenedExistentialTypes.find(
                           getConstraintSystem().getConstraintLocator(
                             memberLocator));
      bool openedExistential = false;
      if (knownOpened != solution.OpenedExistentialTypes.end()) {
        base = openExistentialReference(base, knownOpened->second, member);
        baseTy = knownOpened->second;
        selfTy = baseTy;
        openedExistential = true;
      }

      // If this is a method whose result type is dynamic Self, or a
      // construction, replace the result type with the actual object type.
      Type dynamicSelfFnType;
      if (!member->getDeclContext()->getSelfProtocolDecl()) {
        if (auto func = dyn_cast<AbstractFunctionDecl>(member)) {
          if ((isa<FuncDecl>(func) &&
               cast<FuncDecl>(func)->hasDynamicSelf()) ||
              (isa<ConstructorDecl>(func) &&
               containerTy->getClassOrBoundGenericClass())) {
            refTy = refTy->replaceCovariantResultType(containerTy, 2);
            if (!baseTy->isEqual(containerTy)) {
              dynamicSelfFnType = refTy->replaceCovariantResultType(baseTy, 2);
            }
          }
        }
      }

      // References to properties with accessors and storage usually go
      // through the accessors, but sometimes are direct.
      if (auto *VD = dyn_cast<VarDecl>(member)) {
        if (semantics == AccessSemantics::Ordinary)
          semantics = getImplicitMemberReferenceAccessSemantics(base, VD, dc);
      }

      if (baseIsInstance) {
        // Convert the base to the appropriate container type, turning it
        // into an lvalue if required.

        // If the base is already an lvalue with the right base type, we can
        // pass it as an inout qualified type.
        auto selfParamTy = isDynamic ? selfTy : containerTy;

        if (selfTy->isEqual(baseTy))
          if (cs.getType(base)->is<LValueType>())
            selfParamTy = InOutType::get(selfTy);

        base = coerceObjectArgumentToType(
                 base, selfParamTy, member, semantics,
                 locator.withPathElement(ConstraintLocator::MemberRefBase));
      } else {
        // Convert the base to an rvalue of the appropriate metatype.
        base = coerceToType(base,
                            MetatypeType::get(isDynamic ? selfTy : containerTy),
                            locator.withPathElement(
                              ConstraintLocator::MemberRefBase));
        if (!base)
          return nullptr;

        base = cs.coerceToRValue(base);
      }
      assert(base && "Unable to convert base?");

      // Handle dynamic references.
      if (isDynamic || member->getAttrs().hasAttribute<OptionalAttr>()) {
        base = cs.coerceToRValue(base);
        Expr *ref = new (context) DynamicMemberRefExpr(base, dotLoc, memberRef,
                                                       memberLoc);
        ref->setImplicit(Implicit);
        // FIXME: FunctionRefKind

        // Compute the type of the reference.
        Type refType = simplifyType(openedType);

        // If the base was an opened existential, erase the opened
        // existential.
        if (openedExistential &&
            refType->hasOpenedExistential(knownOpened->second)) {
          refType = refType->eraseOpenedExistential(knownOpened->second);
        }

        cs.setType(ref, refType);

        closeExistential(ref, locator, /*force=*/openedExistential);

        // If this attribute was inferred based on deprecated Swift 3 rules,
        // complain.
        if (auto attr = member->getAttrs().getAttribute<ObjCAttr>()) {
          if (attr->isSwift3Inferred() &&
              tc.Context.LangOpts.WarnSwift3ObjCInference
                == Swift3ObjCInferenceWarnings::Minimal) {
            tc.diagnose(memberLoc,
                        diag::expr_dynamic_lookup_swift3_objc_inference,
                        member->getDescriptiveKind(),
                        member->getFullName(),
                        member->getDeclContext()
                          ->getSelfNominalTypeDecl()
                          ->getName());
            tc.diagnose(member, diag::make_decl_objc,
                        member->getDescriptiveKind())
              .fixItInsert(member->getAttributeInsertionLoc(false),
                           "@objc ");
          }
        }

        if (isDynamic) {
          // Rewrite for implicit unwrapping if the solution requires it.
          auto *dynamicLocator =
              cs.getConstraintLocator(memberLocator.withPathElement(
                  ConstraintLocator::DynamicLookupResult));

          if (solution.getDisjunctionChoice(dynamicLocator)) {
            auto *forceValue =
                new (context) ForceValueExpr(ref, ref->getEndLoc());
            auto optTy = cs.getType(forceValue->getSubExpr());
            cs.setType(forceValue, optTy->getOptionalObjectType());
            ref = forceValue;
          }
        }

        // We also need to handle the implicitly unwrap of the result
        // of the called function if that's the type checking solution
        // we ended up with.
        return forceUnwrapIfExpected(
            ref, choice, memberLocator,
            member->getAttrs().hasAttribute<OptionalAttr>());
      }

      // For types and properties, build member references.
      if (isa<TypeDecl>(member) || isa<VarDecl>(member)) {
        assert(!dynamicSelfFnType && "Converted type doesn't make sense here");
        if (!baseIsInstance && member->isInstanceMember()) {
          assert(memberLocator.getBaseLocator() && 
                 cs.UnevaluatedRootExprs.count(
                   memberLocator.getBaseLocator()->getAnchor()) &&
                 "Attempt to reference an instance member of a metatype");
          auto baseInstanceTy = cs.getType(base)
              ->getInOutObjectType()->getMetatypeInstanceType();
          base = new (context) UnevaluatedInstanceExpr(base, baseInstanceTy);
          cs.cacheType(base);
          base->setImplicit();
        }

        auto memberRefExpr
          = new (context) MemberRefExpr(base, dotLoc, memberRef,
                                        memberLoc, Implicit, semantics);
        memberRefExpr->setIsSuper(isSuper);

        // Skip the synthesized 'self' input type of the opened type.
        cs.setType(memberRefExpr, simplifyType(openedType));
        Expr *result = memberRefExpr;
        closeExistential(result, locator);
        return forceUnwrapIfExpected(result, choice, memberLocator);
      }
      
      // Handle all other references.
      auto declRefExpr = new (context) DeclRefExpr(memberRef, memberLoc,
                                                   Implicit, semantics);
      declRefExpr->setFunctionRefKind(functionRefKind);
      cs.setType(declRefExpr, refTy);
      Expr *ref = declRefExpr;

      // If the reference needs to be converted, do so now.
      if (dynamicSelfFnType) {
        ref = new (context) CovariantFunctionConversionExpr(ref,
                                                            dynamicSelfFnType);
        cs.cacheType(ref);
      }

      ApplyExpr *apply;
      if (isa<ConstructorDecl>(member)) {
        // FIXME: Provide type annotation.
        ref = forceUnwrapIfExpected(ref, choice, memberLocator);
        apply = new (context) ConstructorRefCallExpr(ref, base);
      } else if (!baseIsInstance && member->isInstanceMember()) {
        // Reference to an unbound instance method.
        Expr *result = new (context) DotSyntaxBaseIgnoredExpr(base, dotLoc,
                                                              ref,
                                                              cs.getType(ref));
        cs.cacheType(result);
        closeExistential(result, locator, /*force=*/openedExistential);
        return forceUnwrapIfExpected(result, choice, memberLocator);
      } else {
        assert((!baseIsInstance || member->isInstanceMember()) &&
               "can't call a static method on an instance");
        ref = forceUnwrapIfExpected(ref, choice, memberLocator);
        apply = new (context) DotSyntaxCallExpr(ref, dotLoc, base);
        if (Implicit) {
          apply->setImplicit();
        }
      }

      return finishApply(apply, openedType, locator);
    }
    
    /// \brief Describes either a type or the name of a type to be resolved.
    using TypeOrName = llvm::PointerUnion<Identifier, Type>;

    /// \brief Convert the given literal expression via a protocol pair.
    ///
    /// This routine handles the two-step literal conversion process used
    /// by integer, float, character, extended grapheme cluster, and string
    /// literals. The first step uses \c builtinProtocol while the second
    /// step uses \c protocol.
    ///
    /// \param literal The literal expression.
    ///
    /// \param type The literal type. This type conforms to \c protocol,
    /// and may also conform to \c builtinProtocol.
    ///
    /// \param openedType The literal type as it was opened in the type system.
    ///
    /// \param protocol The protocol that describes the literal requirement.
    ///
    /// \param literalType Either the name of the associated type in
    /// \c protocol that describes the argument type of the conversion function
    /// (\c literalFuncName) or the argument type itself.
    ///
    /// \param literalFuncName The name of the conversion function requirement
    /// in \c protocol.
    ///
    /// \param builtinProtocol The "builtin" form of the protocol, which
    /// always takes builtin types and can only be properly implemented
    /// by standard library types. If \c type does not conform to this
    /// protocol, it's literal type will.
    ///
    /// \param builtinLiteralType Either the name of the associated type in
    /// \c builtinProtocol that describes the argument type of the builtin
    /// conversion function (\c builtinLiteralFuncName) or the argument type
    /// itself.
    ///
    /// \param builtinLiteralFuncName The name of the conversion function
    /// requirement in \c builtinProtocol.
    ///
    /// \param isBuiltinArgType Function that determines whether the given
    /// type is acceptable as the argument type for the builtin conversion.
    ///
    /// \param brokenProtocolDiag The diagnostic to emit if the protocol
    /// is broken.
    ///
    /// \param brokenBuiltinProtocolDiag The diagnostic to emit if the builtin
    /// protocol is broken.
    ///
    /// \returns the converted literal expression.
    Expr *convertLiteral(Expr *literal,
                         Type type,
                         Type openedType,
                         ProtocolDecl *protocol,
                         TypeOrName literalType,
                         DeclName literalFuncName,
                         ProtocolDecl *builtinProtocol,
                         TypeOrName builtinLiteralType,
                         DeclName builtinLiteralFuncName,
                         bool (*isBuiltinArgType)(Type),
                         Diag<> brokenProtocolDiag,
                         Diag<> brokenBuiltinProtocolDiag);

    /// \brief Convert the given literal expression via a protocol pair.
    ///
    /// This routine handles the two-step literal conversion process used
    /// by integer, float, character, extended grapheme cluster, and string
    /// literals. The first step uses \c builtinProtocol while the second
    /// step uses \c protocol.
    ///
    /// \param literal The literal expression.
    ///
    /// \param type The literal type. This type conforms to \c protocol,
    /// and may also conform to \c builtinProtocol.
    ///
    /// \param protocol The protocol that describes the literal requirement.
    ///
    /// \param literalType The name of the associated type in \c protocol that
    /// describes the argument type of the conversion function (\c
    /// literalFuncName).
    ///
    /// \param literalFuncName The name of the conversion function requirement
    /// in \c protocol.
    ///
    /// \param builtinProtocol The "builtin" form of the protocol, which
    /// always takes builtin types and can only be properly implemented
    /// by standard library types. If \c type does not conform to this
    /// protocol, it's literal type will.
    ///
    /// \param builtinLiteralFuncName The name of the conversion function
    /// requirement in \c builtinProtocol.
    ///
    /// \param brokenProtocolDiag The diagnostic to emit if the protocol
    /// is broken.
    ///
    /// \param brokenBuiltinProtocolDiag The diagnostic to emit if the builtin
    /// protocol is broken.
    ///
    /// \returns the converted literal expression.
    Expr *convertLiteralInPlace(Expr *literal,
                                Type type,
                                ProtocolDecl *protocol,
                                Identifier literalType,
                                DeclName literalFuncName,
                                ProtocolDecl *builtinProtocol,
                                DeclName builtinLiteralFuncName,
                                Diag<> brokenProtocolDiag,
                                Diag<> brokenBuiltinProtocolDiag);

    /// \brief Finish a function application by performing the appropriate
    /// conversions on the function and argument expressions and setting
    /// the resulting type.
    ///
    /// \param apply The function application to finish type-checking, which
    /// may be a newly-built expression.
    ///
    /// \param openedType The "opened" type this expression had during
    /// type checking, which will be used to specialize the resulting,
    /// type-checked expression appropriately.
    ///
    /// \param locator The locator for the original expression.
    Expr *finishApply(ApplyExpr *apply, Type openedType,
                      ConstraintLocatorBuilder locator);

  private:
    /// \brief Simplify the given type by substituting all occurrences of
    /// type variables for their fixed types.
    Type simplifyType(Type type) {
      return solution.simplifyType(type);
    }

  public:


    /// \brief Coerce a closure expression with a non-Void return type to a
    /// contextual function type with a Void return type.
    ///
    /// This operation cannot fail.
    ///
    /// \param expr The closure expression to coerce.
    ///
    /// \returns The coerced closure expression.
    ///
    ClosureExpr *coerceClosureExprToVoid(ClosureExpr *expr);

    /// \brief Coerce a closure expression with a Never return type to a
    /// contextual function type with some other return type.
    ///
    /// This operation cannot fail.
    ///
    /// \param expr The closure expression to coerce.
    ///
    /// \returns The coerced closure expression.
    ///
    ClosureExpr *coerceClosureExprFromNever(ClosureExpr *expr);
    
    /// \brief Coerce the given expression to the given type.
    ///
    /// This operation cannot fail.
    ///
    /// \param expr The expression to coerce.
    /// \param toType The type to coerce the expression to.
    /// \param locator Locator used to describe where in this expression we are.
    /// \param typeFromPattern Optionally, the caller can specify the pattern
    ///   from where the toType is derived, so that we can deliver better fixit.
    ///
    /// \returns the coerced expression, which will have type \c ToType.
    Expr *coerceToType(Expr *expr, Type toType,
                       ConstraintLocatorBuilder locator,
                       Optional<Pattern*> typeFromPattern = None);
    
    /// \brief Coerce the given expression (which is the argument to a call) to
    /// the given parameter type.
    ///
    /// This operation cannot fail.
    ///
    /// \param arg The argument expression.
    /// \param funcType The function type.
    /// \param apply The ApplyExpr that forms the call.
    /// \param argLabels The argument labels provided for the call.
    /// \param hasTrailingClosure Whether the last argument is a trailing
    /// closure.
    /// \param locator Locator used to describe where in this expression we are.
    ///
    /// \returns the coerced expression, which will have type \c ToType.
    Expr *
    coerceCallArguments(Expr *arg, AnyFunctionType *funcType,
                        ApplyExpr *apply,
                        ArrayRef<Identifier> argLabels,
                        bool hasTrailingClosure,
                        ConstraintLocatorBuilder locator);

    /// \brief Coerce the given object argument (e.g., for the base of a
    /// member expression) to the given type.
    ///
    /// \param expr The expression to coerce.
    ///
    /// \param baseTy The base type
    ///
    /// \param member The member being accessed.
    ///
    /// \param semantics The kind of access we've been asked to perform.
    ///
    /// \param locator Locator used to describe where in this expression we are.
    Expr *coerceObjectArgumentToType(Expr *expr,
                                     Type baseTy, ValueDecl *member,
                                     AccessSemantics semantics,
                                     ConstraintLocatorBuilder locator);

  private:
    /// \brief Build a new subscript.
    ///
    /// \param base The base of the subscript.
    /// \param index The index of the subscript.
    /// \param locator The locator used to refer to the subscript.
    /// \param isImplicit Whether this is an implicit subscript.
    Expr *buildSubscript(Expr *base, Expr *index,
                         ArrayRef<Identifier> argLabels,
                         bool hasTrailingClosure,
                         ConstraintLocatorBuilder locator, bool isImplicit,
                         AccessSemantics semantics,
                         Optional<SelectedOverload> selected = None) {

      // Determine the declaration selected for this subscript operation.
      if (!selected)
        selected = solution.getOverloadChoiceIfAvailable(
                          cs.getConstraintLocator(
                            locator.withPathElement(
                              ConstraintLocator::SubscriptMember)));

      // Handles situation where there was a solution available but it didn't
      // have a proper overload selected from subscript call, might be because
      // solver was allowed to return free or unresolved types, which can
      // happen while running diagnostics on one of the expressions.
      if (!selected.hasValue()) {
        auto &tc = cs.TC;
        auto baseType = cs.getType(base);

        if (auto errorType = baseType->getAs<ErrorType>()) {
          tc.diagnose(base->getLoc(), diag::cannot_subscript_base,
                      errorType->getOriginalType())
              .highlight(base->getSourceRange());
        } else {
          tc.diagnose(base->getLoc(), diag::cannot_subscript_ambiguous_base)
              .highlight(base->getSourceRange());
        }

        return nullptr;
      }

      // Build the new subscript.
      auto newSubscript = buildSubscriptHelper(base, index, argLabels,
                                               *selected, hasTrailingClosure,
                                               locator, isImplicit, semantics);

      if (selected->choice.getKind() == OverloadChoiceKind::DeclViaDynamic) {
        // Rewrite for implicit unwrapping if the solution requires it.
        auto *dynamicLocator = cs.getConstraintLocator(
            locator.withPathElement(ConstraintLocator::SubscriptMember)
                .withPathElement(ConstraintLocator::DynamicLookupResult));

        if (solution.getDisjunctionChoice(dynamicLocator)) {
          auto *forceValue = new (cs.getASTContext())
              ForceValueExpr(newSubscript, newSubscript->getEndLoc());
          auto optTy = cs.getType(forceValue->getSubExpr());
          cs.setType(forceValue, optTy->getOptionalObjectType());
          newSubscript = forceValue;
        }
      }

      if (selected->choice.isDecl()) {
        auto locatorKind = ConstraintLocator::SubscriptMember;
        if (selected->choice.getKind() ==
            OverloadChoiceKind::DynamicMemberLookup)
          locatorKind = ConstraintLocator::Member;

        newSubscript =
            forceUnwrapIfExpected(newSubscript, selected->choice,
                                  locator.withPathElement(locatorKind));
      }

      return newSubscript;
    }

    Expr *buildSubscriptHelper(Expr *base, Expr *index,
                               ArrayRef<Identifier> argLabels,
                               SelectedOverload &selected,
                               bool hasTrailingClosure,
                               ConstraintLocatorBuilder locator,
                               bool isImplicit, AccessSemantics semantics) {
      auto choice = selected.choice;

      // Apply a key path if we have one.
      if (choice.getKind() == OverloadChoiceKind::KeyPathApplication) {
        auto applicationTy =
            simplifyType(selected.openedType)->castTo<FunctionType>();

        index = cs.coerceToRValue(index);
        // The index argument should be (keyPath: KeyPath<Root, Value>).
        // Dig the key path expression out of the arguments.
        auto indexKP = cast<TupleExpr>(index)->getElement(0);
        auto keyPathExprTy = cs.getType(indexKP);
        auto keyPathTy = applicationTy->getParams().front().getOldType();

        Type valueTy;
        Type baseTy;
        bool resultIsLValue;
        
        if (auto nom = keyPathTy->getAs<NominalType>()) {
          // AnyKeyPath is <T> rvalue T -> rvalue Any?
          if (nom->getDecl() == cs.getASTContext().getAnyKeyPathDecl()) {
            valueTy = ProtocolCompositionType::get(cs.getASTContext(), {},
                                                  /*explicit anyobject*/ false);
            valueTy = OptionalType::get(valueTy);
            resultIsLValue = false;
            base = cs.coerceToRValue(base);
            baseTy = cs.getType(base);
            // We don't really want to attempt AnyKeyPath application
            // if we know a more specific key path type is being applied.
            if (!keyPathTy->isEqual(keyPathExprTy)) {
              cs.TC.diagnose(base->getLoc(),
                             diag::expr_smart_keypath_application_type_mismatch,
                             keyPathExprTy,
                             baseTy)
                .highlight(index->getSourceRange());
            }
          } else {
            llvm_unreachable("unknown key path class!");
          }
        } else {
          auto keyPathBGT = keyPathTy->castTo<BoundGenericType>();
          baseTy = keyPathBGT->getGenericArgs()[0];
          
          // Coerce the base to the key path's expected base type.
          if (!baseTy->isEqual(cs.getType(base)->getRValueType()))
            base = coerceToType(base, baseTy, locator);

          if (keyPathBGT->getDecl()
                == cs.getASTContext().getPartialKeyPathDecl()) {
            // PartialKeyPath<T> is rvalue T -> rvalue Any
            valueTy = ProtocolCompositionType::get(cs.getASTContext(), {},
                                                 /*explicit anyobject*/ false);
            resultIsLValue = false;
            base = cs.coerceToRValue(base);
          } else {
            // *KeyPath<T, U> is T -> U, with rvalueness based on mutability
            // of base and keypath
            valueTy = keyPathBGT->getGenericArgs()[1];
        
            // The result may be an lvalue based on the base and key path kind.
            if (keyPathBGT->getDecl() == cs.getASTContext().getKeyPathDecl()) {
              resultIsLValue = false;
              base = cs.coerceToRValue(base);
            } else if (keyPathBGT->getDecl() ==
                         cs.getASTContext().getWritableKeyPathDecl()) {
              resultIsLValue = cs.getType(base)->hasLValueType();
            } else if (keyPathBGT->getDecl() ==
                       cs.getASTContext().getReferenceWritableKeyPathDecl()) {
              resultIsLValue = true;
              base = cs.coerceToRValue(base);
            } else {
              llvm_unreachable("unknown key path class!");
            }
          }
        }
        if (resultIsLValue)
          valueTy = LValueType::get(valueTy);
        
        auto keyPathAp = new (cs.getASTContext())
           KeyPathApplicationExpr(base, index->getStartLoc(), indexKP,
                                  index->getEndLoc(), valueTy,
                                  base->isImplicit() && index->isImplicit());
        cs.setType(keyPathAp, valueTy);
        return keyPathAp;
      }
      
      auto subscript = cast<SubscriptDecl>(choice.getDecl());
      cs.TC.requestMemberLayout(subscript);

      auto &tc = cs.getTypeChecker();
      auto baseTy = cs.getType(base)->getRValueType();

      // Check whether the base is 'super'.
      bool isSuper = base->isSuperExpr();

      // Use the correct kind of locator depending on how this subscript came
      // to be.
      auto locatorKind = ConstraintLocator::SubscriptMember;
      if (choice.getKind() == OverloadChoiceKind::DynamicMemberLookup)
        locatorKind = ConstraintLocator::Member;
      
      // If we opened up an existential when performing the subscript, open
      // the base accordingly.
      auto knownOpened = solution.OpenedExistentialTypes.find(
                           getConstraintSystem().getConstraintLocator(
                             locator.withPathElement(locatorKind)));
      if (knownOpened != solution.OpenedExistentialTypes.end()) {
        base = openExistentialReference(base, knownOpened->second, subscript);
        baseTy = knownOpened->second;
      }
 
      // Figure out the index and result types.
      Type resultTy;
      if (choice.getKind() != OverloadChoiceKind::DynamicMemberLookup) {
        auto subscriptTy = simplifyType(selected.openedType);
        auto *subscriptFnTy = subscriptTy->castTo<FunctionType>();
        resultTy = subscriptFnTy->getResult();

        // Coerce the index argument.
        index = coerceCallArguments(index, subscriptFnTy, nullptr,
                                    argLabels, hasTrailingClosure,
                                    locator.withPathElement(
                                      ConstraintLocator::ApplyArgument));
        if (!index)
          return nullptr;

      } else {
        // If this is a @dynamicMemberLookup, then the type of the selection is
        // actually the property/result type.  That's fine though, and we
        // already have the index type adjusted to the correct type expected by
        // the subscript.
        resultTy = simplifyType(selected.openedType);
      }
      
      auto getType = [&](const Expr *E) -> Type {
        return cs.getType(E);
      };

      // Form the subscript expression.

      // Compute the substitutions used to reference the subscript.
      SubstitutionMap substitutions =
        solution.computeSubstitutions(
          subscript->getInnermostDeclContext()->getGenericSignatureOfContext(),
          locator.withPathElement(locatorKind));
      ConcreteDeclRef subscriptRef(subscript, substitutions);

      // Handle dynamic lookup.
      if (choice.getKind() == OverloadChoiceKind::DeclViaDynamic ||
          subscript->getAttrs().hasAttribute<OptionalAttr>()) {
        base = coerceObjectArgumentToType(base, baseTy, subscript,
                                          AccessSemantics::Ordinary, locator);
        if (!base)
          return nullptr;

        // TODO: diagnose if semantics != AccessSemantics::Ordinary?
        auto subscriptExpr = DynamicSubscriptExpr::create(tc.Context, base,
                                                          index, subscriptRef,
                                                          isImplicit, getType);
        cs.setType(subscriptExpr, resultTy);
        Expr *result = subscriptExpr;
        closeExistential(result, locator);
        return result;
      }

      // Convert the base.
      auto openedFullFnType = selected.openedFullType->castTo<FunctionType>();
      auto openedBaseType =
          getBaseType(openedFullFnType, /*wantsRValue*/ false);
      auto containerTy = solution.simplifyType(openedBaseType);
      base = coerceObjectArgumentToType(
        base, containerTy, subscript, AccessSemantics::Ordinary,
        locator.withPathElement(ConstraintLocator::MemberRefBase));
      if (!base)
        return nullptr;

      // Form the subscript expression.
      auto subscriptExpr = SubscriptExpr::create(
        tc.Context, base, index, subscriptRef, isImplicit, semantics, getType);
      cs.setType(subscriptExpr, resultTy);
      subscriptExpr->setIsSuper(isSuper);

      Expr *result = subscriptExpr;
      closeExistential(result, locator);
      return result;
    }

    /// \brief Build a new reference to another constructor.
    Expr *buildOtherConstructorRef(Type openedFullType,
                                   ConstructorDecl *ctor, Expr *base,
                                   DeclNameLoc loc,
                                   ConstraintLocatorBuilder locator,
                                   bool implicit) {
      auto &tc = cs.getTypeChecker();
      auto &ctx = tc.Context;

      // Compute the concrete reference.
      SubstitutionMap substitutions =
        solution.computeSubstitutions(ctor->getGenericSignature(), locator);

      auto ref = ConcreteDeclRef(ctor, substitutions);

      // The constructor was opened with the allocating type, not the
      // initializer type. Map the former into the latter.
      auto resultTy = solution.simplifyType(openedFullType);

      auto selfTy = getBaseType(resultTy->castTo<FunctionType>());

      // Also replace the result type with the base type, so that calls
      // to constructors defined in a superclass will know to cast the
      // result to the derived type.
      resultTy = resultTy->replaceCovariantResultType(selfTy, 2);

      ParameterTypeFlags flags;
      if (!selfTy->hasReferenceSemantics())
        flags = flags.withInOut(true);

      auto selfParam = AnyFunctionType::Param(selfTy, Identifier(), flags);
      resultTy = FunctionType::get({selfParam},
                                   resultTy->castTo<FunctionType>()->getResult(),
                                   resultTy->castTo<FunctionType>()->getExtInfo());

      // Build the constructor reference.
      Expr *ctorRef = cs.cacheType(
          new (ctx) OtherConstructorDeclRefExpr(ref, loc, implicit, resultTy));

      // Wrap in covariant `Self` return if needed.
      if (selfTy->hasReferenceSemantics()) {
        auto covariantTy = resultTy->replaceCovariantResultType(
          cs.getType(base)->getWithoutSpecifierType(), 2);
        if (!covariantTy->isEqual(resultTy))
          ctorRef = cs.cacheType(
               new (ctx) CovariantFunctionConversionExpr(ctorRef, covariantTy));
      }

      return ctorRef;
    }

    /// Bridge the given value (which is an error type) to NSError.
    Expr *bridgeErrorToObjectiveC(Expr *value) {
      auto &tc = cs.getTypeChecker();

      auto nsErrorDecl = tc.Context.getNSErrorDecl();
      assert(nsErrorDecl && "Missing NSError?");
      Type nsErrorType = nsErrorDecl->getDeclaredInterfaceType();

      auto result = new (tc.Context) BridgeToObjCExpr(value, nsErrorType);
      return cs.cacheType(result);
    }

    /// Bridge the given value to its corresponding Objective-C object
    /// type.
    ///
    /// This routine should only be used for bridging value types.
    ///
    /// \param value The value to be bridged.
    Expr *bridgeToObjectiveC(Expr *value, Type objcType) {
      auto &tc = cs.getTypeChecker();

      auto result = new (tc.Context) BridgeToObjCExpr(value, objcType);
      return cs.cacheType(result);
    }

    /// Bridge the given object from Objective-C to its value type.
    ///
    /// This routine should only be used for bridging value types.
    ///
    /// \param object The object, whose type should already be of the type
    /// that the value type bridges through.
    ///
    /// \param valueType The value type to which we are bridging.
    ///
    /// \param conditional Whether the bridging should be conditional. If false,
    /// uses forced bridging.
    ///
    /// \returns a value of type \c valueType (optional if \c conditional) that
    /// stores the bridged result or (when \c conditional) an empty optional if
    /// conditional bridging fails.
    Expr *bridgeFromObjectiveC(Expr *object, Type valueType, bool conditional) {
      auto &tc = cs.getTypeChecker();

      if (!conditional) {
        auto result = new (tc.Context) BridgeFromObjCExpr(object, valueType);
        return cs.cacheType(result);
      }

      // Find the _BridgedToObjectiveC protocol.
      auto bridgedProto
        = tc.Context.getProtocol(KnownProtocolKind::ObjectiveCBridgeable);

      // Try to find the conformance of the value type to _BridgedToObjectiveC.
      auto bridgedToObjectiveCConformance
        = tc.conformsToProtocol(valueType,
                                bridgedProto,
                                cs.DC,
                                (ConformanceCheckFlags::InExpression|
                                 ConformanceCheckFlags::Used));

      FuncDecl *fn = nullptr;

      if (bridgedToObjectiveCConformance) {
        assert(bridgedToObjectiveCConformance->getConditionalRequirements()
                   .empty() &&
               "cannot conditionally conform to _BridgedToObjectiveC");
        // The conformance to _BridgedToObjectiveC is statically known.
        // Retrieve the bridging operation to be used if a static conformance
        // to _BridgedToObjectiveC can be proven.
        fn = conditional
                 ? tc.Context.getConditionallyBridgeFromObjectiveCBridgeable(&tc)
                 : tc.Context.getForceBridgeFromObjectiveCBridgeable(&tc);
      } else {
        // Retrieve the bridging operation to be used if a static conformance
        // to _BridgedToObjectiveC cannot be proven.
        fn = conditional ? tc.Context.getConditionallyBridgeFromObjectiveC(&tc)
                         : tc.Context.getForceBridgeFromObjectiveC(&tc);
      }

      if (!fn) {
        tc.diagnose(object->getLoc(), diag::missing_bridging_function,
                    conditional);
        return nullptr;
      }

      tc.validateDecl(fn);
      
      // Form a reference to the function. The bridging operations are generic,
      // so we need to form substitutions and compute the resulting type.
      auto genericSig = fn->getGenericSignature();

      auto subMap = SubstitutionMap::get(
        genericSig,
        [&](SubstitutableType *type) -> Type {
          assert(type->isEqual(genericSig->getGenericParams()[0]));
          return valueType;
        },
        [&](CanType origType, Type replacementType, ProtocolDecl *protoType)
          -> ProtocolConformanceRef {
          assert(bridgedToObjectiveCConformance);
          return *bridgedToObjectiveCConformance;
        });

      ConcreteDeclRef fnSpecRef(fn, subMap);

      auto resultType = OptionalType::get(valueType);

      auto result = new (tc.Context) ConditionalBridgeFromObjCExpr(object,
                                                        resultType, fnSpecRef);
      return cs.cacheType(result);
    }

    /// Bridge the given object from Objective-C to its value type.
    ///
    /// This routine should only be used for bridging value types.
    ///
    /// \param object The object, whose type should already be of the type
    /// that the value type bridges through.
    ///
    /// \param valueType The value type to which we are bridging.
    ///
    /// \returns a value of type \c valueType that stores the bridged result.
    Expr *forceBridgeFromObjectiveC(Expr *object, Type valueType) {
      return bridgeFromObjectiveC(object, valueType, false);
    }

    TypeAliasDecl *MaxFloatTypeDecl = nullptr;
    
  public:
    ExprRewriter(ConstraintSystem &cs, const Solution &solution,
                 bool suppressDiagnostics)
        : cs(cs), dc(cs.DC), solution(solution),
          SuppressDiagnostics(suppressDiagnostics) {}

    ConstraintSystem &getConstraintSystem() const { return cs; }

    /// \brief Simplify the expression type and return the expression.
    ///
    /// This routine is used for 'simple' expressions that only need their
    /// types simplified, with no further computation.
    Expr *simplifyExprType(Expr *expr) {
      auto toType = simplifyType(cs.getType(expr));
      cs.setType(expr, toType);
      return expr;
    }

    Expr *visitErrorExpr(ErrorExpr *expr) {
      // Do nothing with error expressions.
      return expr;
    }

    Expr *visitCodeCompletionExpr(CodeCompletionExpr *expr) {
      // Do nothing with code completion expressions.
      auto toType = simplifyType(cs.getType(expr));
      cs.setType(expr, toType);
      return expr;
    }

    Expr *handleIntegerLiteralExpr(LiteralExpr *expr) {
      // If the literal has been assigned a builtin integer type,
      // don't mess with it.
      if (cs.getType(expr)->is<BuiltinIntegerType>())
        return expr;

      auto &tc = cs.getTypeChecker();
      ProtocolDecl *protocol
        = tc.getProtocol(expr->getLoc(),
                         KnownProtocolKind::ExpressibleByIntegerLiteral);
      ProtocolDecl *builtinProtocol
        = tc.getProtocol(expr->getLoc(),
                         KnownProtocolKind::ExpressibleByBuiltinIntegerLiteral);

      // For type-sugar reasons, prefer the spelling of the default literal
      // type.
      auto type = simplifyType(cs.getType(expr));
      if (auto defaultType = tc.getDefaultType(protocol, dc)) {
        if (defaultType->isEqual(type))
          type = defaultType;
      }
      if (auto floatProtocol
            = tc.getProtocol(expr->getLoc(),
                             KnownProtocolKind::ExpressibleByFloatLiteral)) {
        if (auto defaultFloatType = tc.getDefaultType(floatProtocol, dc)) {
          if (defaultFloatType->isEqual(type))
            type = defaultFloatType;
        }
      }

      DeclName initName(tc.Context, DeclBaseName::createConstructor(),
                        { tc.Context.Id_integerLiteral });
      DeclName builtinInitName(tc.Context, DeclBaseName::createConstructor(),
                               { tc.Context.Id_builtinIntegerLiteral });

      return convertLiteral(
               expr,
               type,
               cs.getType(expr),
               protocol,
               tc.Context.Id_IntegerLiteralType,
               initName,
               builtinProtocol,
               // Note that 'MaxIntegerType' is guaranteed to be available.
               // Otherwise it would be caught by CSGen::visitLiteralExpr
               tc.getMaxIntegerType(dc),
               builtinInitName,
               nullptr,
               diag::integer_literal_broken_proto,
               diag::builtin_integer_literal_broken_proto);
    }
    
    Expr *visitNilLiteralExpr(NilLiteralExpr *expr) {
      auto &tc = cs.getTypeChecker();
      auto *protocol = tc.getProtocol(expr->getLoc(),
                                      KnownProtocolKind::ExpressibleByNilLiteral);

      // For type-sugar reasons, prefer the spelling of the default literal
      // type.
      auto type = simplifyType(cs.getType(expr));
      if (auto defaultType = tc.getDefaultType(protocol, dc)) {
        if (defaultType->isEqual(type))
          type = defaultType;
      }

      // By far the most common 'nil' literal is for Optional<T>.none.
      //
      // Emit this case directly instead of calling Optional.init(nilLiteral:),
      // since this generates more efficient SIL.
      if (auto objectType = type->getOptionalObjectType()) {
        auto *nilDecl = tc.Context.getOptionalNoneDecl();
        tc.validateDecl(nilDecl);
        if (!nilDecl->hasInterfaceType())
          return nullptr;

        auto genericSig =
          nilDecl->getDeclContext()->getGenericSignatureOfContext();
        SubstitutionMap subs =
          SubstitutionMap::get(genericSig, llvm::makeArrayRef(objectType),
                               { });
        ConcreteDeclRef concreteDeclRef(nilDecl, subs);

        auto nilType = FunctionType::get(
            {FunctionType::Param(MetatypeType::get(type))}, type);
        auto *nilRefExpr = new (tc.Context) DeclRefExpr(
            concreteDeclRef, DeclNameLoc(expr->getLoc()),
              /*implicit=*/true, AccessSemantics::Ordinary,
            nilType);
        cs.cacheType(nilRefExpr);

        auto *typeExpr = TypeExpr::createImplicitHack(
            expr->getLoc(),
            type,
            tc.Context);
        cs.cacheType(typeExpr);

        auto *callExpr = new (tc.Context) DotSyntaxCallExpr(
            nilRefExpr, expr->getLoc(), typeExpr, type);
        callExpr->setImplicit(true);
        cs.cacheType(callExpr);

        return callExpr;
      }

      DeclName initName(tc.Context, DeclBaseName::createConstructor(),
                        { tc.Context.Id_nilLiteral });
      return convertLiteral(expr, type, cs.getType(expr), protocol,
                            Identifier(), initName,
                            nullptr, Identifier(),
                            Identifier(),
                            [] (Type type) -> bool {
                              return false;
                            },
                            diag::nil_literal_broken_proto,
                            diag::nil_literal_broken_proto);
    }

    
    Expr *visitIntegerLiteralExpr(IntegerLiteralExpr *expr) {
      return handleIntegerLiteralExpr(expr);
    }

    Expr *visitFloatLiteralExpr(FloatLiteralExpr *expr) {
      // If the literal has been assigned a builtin float type,
      // don't mess with it.
      if (cs.getType(expr)->is<BuiltinFloatType>())
        return expr;

      auto &tc = cs.getTypeChecker();
      ProtocolDecl *protocol
        = tc.getProtocol(expr->getLoc(),
                         KnownProtocolKind::ExpressibleByFloatLiteral);
      ProtocolDecl *builtinProtocol
        = tc.getProtocol(expr->getLoc(),
                         KnownProtocolKind::ExpressibleByBuiltinFloatLiteral);

      // For type-sugar reasons, prefer the spelling of the default literal
      // type.
      auto type = simplifyType(cs.getType(expr));
      if (auto defaultType = tc.getDefaultType(protocol, dc)) {
        if (defaultType->isEqual(type))
          type = defaultType;
      }

      // Find the maximum-sized builtin float type.
      // FIXME: Cache name lookup.
      if (!MaxFloatTypeDecl) {
        SmallVector<ValueDecl *, 1> lookupResults;
        tc.getStdlibModule(dc)->lookupValue(/*AccessPath=*/{},
                                            tc.Context.Id_MaxBuiltinFloatType,
                                            NLKind::QualifiedLookup,
                                            lookupResults);
        if (lookupResults.size() == 1)
          MaxFloatTypeDecl = dyn_cast<TypeAliasDecl>(lookupResults.front());
      }
      if (!MaxFloatTypeDecl ||
          !MaxFloatTypeDecl->hasInterfaceType() ||
          !MaxFloatTypeDecl->getDeclaredInterfaceType()->is<BuiltinFloatType>()) {
        tc.diagnose(expr->getLoc(), diag::no_MaxBuiltinFloatType_found);
        return nullptr;
      }
      tc.validateDecl(MaxFloatTypeDecl);
      auto maxType = MaxFloatTypeDecl->getUnderlyingTypeLoc().getType();

      DeclName initName(tc.Context, DeclBaseName::createConstructor(),
                        { tc.Context.Id_floatLiteral });
      DeclName builtinInitName(tc.Context, DeclBaseName::createConstructor(),
                               { tc.Context.Id_builtinFloatLiteral });

      return convertLiteral(
               expr,
               type,
               cs.getType(expr),
               protocol,
               tc.Context.Id_FloatLiteralType,
               initName,
               builtinProtocol,
               maxType,
               builtinInitName,
               nullptr,
               diag::float_literal_broken_proto,
               diag::builtin_float_literal_broken_proto);
    }

    Expr *visitBooleanLiteralExpr(BooleanLiteralExpr *expr) {
      if (cs.getType(expr) && cs.getType(expr)->is<BuiltinIntegerType>())
        return expr;

      auto &tc = cs.getTypeChecker();
      ProtocolDecl *protocol
        = tc.getProtocol(expr->getLoc(),
                         KnownProtocolKind::ExpressibleByBooleanLiteral);
      ProtocolDecl *builtinProtocol
        = tc.getProtocol(expr->getLoc(),
                         KnownProtocolKind::ExpressibleByBuiltinBooleanLiteral);
      if (!protocol || !builtinProtocol)
        return nullptr;

      auto type = simplifyType(cs.getType(expr));
      DeclName initName(tc.Context, DeclBaseName::createConstructor(),
                        { tc.Context.Id_booleanLiteral });
      DeclName builtinInitName(tc.Context, DeclBaseName::createConstructor(),
                               { tc.Context.Id_builtinBooleanLiteral });
      return convertLiteral(
               expr,
               type,
               cs.getType(expr),
               protocol,
               tc.Context.Id_BooleanLiteralType,
               initName,
               builtinProtocol,
               Type(BuiltinIntegerType::get(BuiltinIntegerWidth::fixed(1), 
                                            tc.Context)),
               builtinInitName,
               nullptr,
               diag::boolean_literal_broken_proto,
               diag::builtin_boolean_literal_broken_proto);
    }

    Expr *handleStringLiteralExpr(LiteralExpr *expr) {
      if (cs.getType(expr) && !cs.getType(expr)->hasTypeVariable())
        return expr;
      
      auto stringLiteral = dyn_cast<StringLiteralExpr>(expr);
      auto magicLiteral = dyn_cast<MagicIdentifierLiteralExpr>(expr);
      assert(bool(stringLiteral) != bool(magicLiteral) &&
             "literal must be either a string literal or a magic literal");

      auto type = simplifyType(cs.getType(expr));
      auto &tc = cs.getTypeChecker();

      bool isStringLiteral = true;
      bool isGraphemeClusterLiteral = false;
      ProtocolDecl *protocol = tc.getProtocol(
          expr->getLoc(), KnownProtocolKind::ExpressibleByStringLiteral);

      if (!tc.conformsToProtocol(type, protocol, cs.DC,
                                 ConformanceCheckFlags::InExpression)) {
        // If the type does not conform to ExpressibleByStringLiteral, it should
        // be ExpressibleByExtendedGraphemeClusterLiteral.
        protocol = tc.getProtocol(
            expr->getLoc(),
            KnownProtocolKind::ExpressibleByExtendedGraphemeClusterLiteral);
        isStringLiteral = false;
        isGraphemeClusterLiteral = true;
      }
      if (!tc.conformsToProtocol(type, protocol, cs.DC,
                                 ConformanceCheckFlags::InExpression)) {
        // ... or it should be ExpressibleByUnicodeScalarLiteral.
        protocol = tc.getProtocol(
            expr->getLoc(),
            KnownProtocolKind::ExpressibleByUnicodeScalarLiteral);
        isStringLiteral = false;
        isGraphemeClusterLiteral = false;
      }

      // For type-sugar reasons, prefer the spelling of the default literal
      // type.
      if (auto defaultType = tc.getDefaultType(protocol, dc)) {
        if (defaultType->isEqual(type))
          type = defaultType;
      }

      ProtocolDecl *builtinProtocol;
      Identifier literalType;
      DeclName literalFuncName;
      DeclName builtinLiteralFuncName;
      Diag<> brokenProtocolDiag;
      Diag<> brokenBuiltinProtocolDiag;

      if (isStringLiteral) {
        // If the string contains only ASCII, force a UTF8 representation
        bool forceASCII = stringLiteral != nullptr;
        if (forceASCII) {
          for (auto c: stringLiteral->getValue()) {
            if (c & (1 << 7)) {
              forceASCII = false;
              break;
            }
          }
        }
        
        literalType = tc.Context.Id_StringLiteralType;

        literalFuncName = DeclName(tc.Context, DeclBaseName::createConstructor(),
                                   { tc.Context.Id_stringLiteral });

        // If the string contains non-ASCII and the type can handle
        // UTF-16 string literals, prefer them.
        builtinProtocol = tc.getProtocol(
            expr->getLoc(),
            KnownProtocolKind::ExpressibleByBuiltinUTF16StringLiteral);

        if (!forceASCII && (tc.conformsToProtocol(
                                      type, builtinProtocol, cs.DC,
                                      ConformanceCheckFlags::InExpression))) {
          builtinLiteralFuncName =
              DeclName(tc.Context, DeclBaseName::createConstructor(),
                       {tc.Context.Id_builtinUTF16StringLiteral,
                        tc.Context.getIdentifier("utf16CodeUnitCount")});

          if (stringLiteral)
            stringLiteral->setEncoding(StringLiteralExpr::UTF16);
          else
            magicLiteral->setStringEncoding(StringLiteralExpr::UTF16);
        } else {
          // Otherwise, fall back to UTF-8.
          builtinProtocol = tc.getProtocol(
              expr->getLoc(),
              KnownProtocolKind::ExpressibleByBuiltinStringLiteral);
          builtinLiteralFuncName 
            = DeclName(tc.Context, DeclBaseName::createConstructor(),
                       { tc.Context.Id_builtinStringLiteral,
                         tc.Context.getIdentifier("utf8CodeUnitCount"),
                         tc.Context.getIdentifier("isASCII") });
          if (stringLiteral)
            stringLiteral->setEncoding(StringLiteralExpr::UTF8);
          else
            magicLiteral->setStringEncoding(StringLiteralExpr::UTF8);
        }
        brokenProtocolDiag = diag::string_literal_broken_proto;
        brokenBuiltinProtocolDiag = diag::builtin_string_literal_broken_proto;
      } else if (isGraphemeClusterLiteral) {
        literalType = tc.Context.Id_ExtendedGraphemeClusterLiteralType;
        literalFuncName
          = DeclName(tc.Context, DeclBaseName::createConstructor(),
                     {tc.Context.Id_extendedGraphemeClusterLiteral});
        builtinLiteralFuncName
          = DeclName(tc.Context, DeclBaseName::createConstructor(),
                     { tc.Context.Id_builtinExtendedGraphemeClusterLiteral,
                       tc.Context.getIdentifier("utf8CodeUnitCount"),
                       tc.Context.getIdentifier("isASCII") });

        builtinProtocol = tc.getProtocol(
            expr->getLoc(),
            KnownProtocolKind::ExpressibleByBuiltinExtendedGraphemeClusterLiteral);
        brokenProtocolDiag =
            diag::extended_grapheme_cluster_literal_broken_proto;
        brokenBuiltinProtocolDiag =
            diag::builtin_extended_grapheme_cluster_literal_broken_proto;

        auto *builtinUTF16ExtendedGraphemeClusterProtocol = tc.getProtocol(
            expr->getLoc(),
            KnownProtocolKind::ExpressibleByBuiltinUTF16ExtendedGraphemeClusterLiteral);
        if (tc.conformsToProtocol(type,
                                  builtinUTF16ExtendedGraphemeClusterProtocol,
                                  cs.DC, ConformanceCheckFlags::InExpression)) {
          builtinLiteralFuncName
            = DeclName(tc.Context, DeclBaseName::createConstructor(),
                       { tc.Context.Id_builtinExtendedGraphemeClusterLiteral,
                         tc.Context.getIdentifier("utf16CodeUnitCount") });

          builtinProtocol = builtinUTF16ExtendedGraphemeClusterProtocol;
          brokenBuiltinProtocolDiag =
            diag::builtin_utf16_extended_grapheme_cluster_literal_broken_proto;
          if (stringLiteral)
            stringLiteral->setEncoding(StringLiteralExpr::UTF16);
          else
            magicLiteral->setStringEncoding(StringLiteralExpr::UTF16);
        }
      } else {
        // Otherwise, we should have just one Unicode scalar.
        literalType = tc.Context.Id_UnicodeScalarLiteralType;

        literalFuncName
          = DeclName(tc.Context, DeclBaseName::createConstructor(),
                     {tc.Context.Id_unicodeScalarLiteral});
        builtinLiteralFuncName
          = DeclName(tc.Context, DeclBaseName::createConstructor(),
                     {tc.Context.Id_builtinUnicodeScalarLiteral});

        builtinProtocol = tc.getProtocol(
            expr->getLoc(),
            KnownProtocolKind::ExpressibleByBuiltinUnicodeScalarLiteral);

        brokenProtocolDiag = diag::unicode_scalar_literal_broken_proto;
        brokenBuiltinProtocolDiag =
            diag::builtin_unicode_scalar_literal_broken_proto;

        stringLiteral->setEncoding(StringLiteralExpr::OneUnicodeScalar);
      }

      return convertLiteralInPlace(expr,
                                   type,
                                   protocol,
                                   literalType,
                                   literalFuncName,
                                   builtinProtocol,
                                   builtinLiteralFuncName,
                                   brokenProtocolDiag,
                                   brokenBuiltinProtocolDiag);
    }
    
    Expr *visitStringLiteralExpr(StringLiteralExpr *expr) {
      return handleStringLiteralExpr(expr);
    }

    Expr *
    visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *expr) {
      // Figure out the string type we're converting to.
      auto openedType = cs.getType(expr);
      auto type = simplifyType(openedType);
      cs.setType(expr, type);

      // Find the string interpolation protocol we need.
      auto &tc = cs.getTypeChecker();
      auto interpolationProto
        = tc.getProtocol(expr->getLoc(),
                         KnownProtocolKind::ExpressibleByStringInterpolation);
      assert(interpolationProto && "Missing string interpolation protocol?");

      DeclName name(tc.Context, DeclBaseName::createConstructor(),
                    { tc.Context.Id_stringInterpolation });
      auto member
        = findNamedWitnessImpl(
            tc, dc, type,
            interpolationProto, name,
            diag::interpolation_broken_proto);

      DeclName segmentName(tc.Context, DeclBaseName::createConstructor(),
                           { tc.Context.Id_stringInterpolationSegment });
      auto segmentMember
        = findNamedWitnessImpl(
            tc, dc, type, interpolationProto, segmentName,
            diag::interpolation_broken_proto);
      if (!member ||
          !segmentMember ||
          !isa<ConstructorDecl>(member.getDecl()) ||
          !isa<ConstructorDecl>(segmentMember.getDecl()))
        return nullptr;

      // Build a reference to the init(stringInterpolation:) initializer.
      // FIXME: This location info is bogus.
      auto *typeRef = TypeExpr::createImplicitHack(expr->getStartLoc(), type,
                                                   tc.Context);

      Expr *memberRef =
        new (tc.Context) MemberRefExpr(typeRef,
                                       expr->getStartLoc(),
                                       member.getDecl(),
                                       DeclNameLoc(expr->getStartLoc()),
                                       /*Implicit=*/true);
      cs.cacheSubExprTypes(memberRef);
      cs.setSubExprTypes(memberRef);
      bool failed = tc.typeCheckExpressionShallow(memberRef, cs.DC);
      cs.cacheExprTypes(memberRef);
      assert(!failed && "Could not reference string interpolation witness");
      (void)failed;

      // Create a tuple containing all of the segments.
      SmallVector<Expr *, 4> segments;
      SmallVector<Identifier, 4> names;
      ConstraintLocatorBuilder locatorBuilder(cs.getConstraintLocator(expr));
      auto getType = [&](const Expr *E) -> Type {
        return cs.getType(E);
      };

      for (auto segment : expr->getSegments()) {
        ApplyExpr *apply =
          CallExpr::createImplicit(
            tc.Context, typeRef,
            { segment },
            { tc.Context.Id_stringInterpolationSegment }, getType);
        cs.cacheSubExprTypes(apply);

        Expr *convertedSegment = apply;
        cs.setSubExprTypes(convertedSegment);
        if (tc.typeCheckExpressionShallow(convertedSegment, cs.DC))
          continue;
        cs.cacheExprTypes(convertedSegment);

        segments.push_back(convertedSegment);

        if (names.empty()) {
          names.push_back(tc.Context.Id_stringInterpolation);
        } else {
          names.push_back(Identifier());
        }
      }

      // If all of the segments had errors, bail out.
      if (segments.empty())
        return nullptr;

      // Call the init(stringInterpolation:) initializer with the arguments.
      ApplyExpr *apply = CallExpr::createImplicit(tc.Context, memberRef,
                                                  segments, names, getType);
      cs.cacheExprTypes(apply);
      expr->setSemanticExpr(finishApply(apply, openedType, locatorBuilder));
      return expr;
    }
    
    Expr *visitMagicIdentifierLiteralExpr(MagicIdentifierLiteralExpr *expr) {
      switch (expr->getKind()) {
      case MagicIdentifierLiteralExpr::File:
      case MagicIdentifierLiteralExpr::Function:
        return handleStringLiteralExpr(expr);

      case MagicIdentifierLiteralExpr::Line:
      case MagicIdentifierLiteralExpr::Column:
        return handleIntegerLiteralExpr(expr);

      case MagicIdentifierLiteralExpr::DSOHandle:
        return expr;
      }


      llvm_unreachable("Unhandled MagicIdentifierLiteralExpr in switch.");
    }

    // SWIFT_ENABLE_TENSORFLOW
    Expr *handleReverseAutoDiffExpr(ReverseAutoDiffExpr *expr,
                                    bool preservingOriginalResult) {
      auto gradType = simplifyType(cs.getType(expr));
      auto gradFnType = gradType->getAs<AnyFunctionType>();
      assert(gradFnType &&
             "Gradient expression should've been assigned a function type");
      cs.setType(expr, gradType);
      cs.cacheExprTypes(expr);
      // Verify that differentiation parameters conform to VectorNumeric.
      auto *originalExpr = expr->getOriginalExpr();
      auto originalType = cs.getType(originalExpr)->getAs<AnyFunctionType>();
      assert(originalType && "Original should have function type");
      auto gradParams = gradFnType->getParams();
      assert(gradFnType->getNumParams() == originalType->getNumParams() &&
             "The gradient function should have same number of parameters as "
             "original function");
      SmallVector<Type, 8> diffParamTypes;
      if (expr->getParameters().empty()) {
        for (auto &gradParam : gradParams)
          diffParamTypes.push_back(gradParam.getPlainType());
      } else {
        for (auto &param : expr->getParameters())
          diffParamTypes.push_back(gradParams[param.index].getPlainType());
      }
      return expr;
    }

    Expr *visitGradientExpr(GradientExpr *expr) {
      return handleReverseAutoDiffExpr(expr, /*preservingOriginalResult=*/false);
    }
    
    Expr *visitChainableGradientExpr(ChainableGradientExpr *expr) {
      llvm_unreachable("Unhandled");
    }

    Expr *visitValueAndGradientExpr(ValueAndGradientExpr *expr) {
      return handleReverseAutoDiffExpr(expr, /*preservingOriginalResult=*/true);
    }

    // SWIFT_ENABLE_TENSORFLOW
    Expr *visitAdjointExpr(AdjointExpr *expr) {
      auto locator = cs.getConstraintLocator(expr);
      auto adjointDeclRef = expr->getAdjointFunction();
      auto adjointDecl = cast<FuncDecl>(adjointDeclRef.getDecl());

      // If adjoint is within a type context (it is an instance/static method),
      // use member locator.
      if (adjointDecl->getInnermostTypeContext())
        locator = cs.getConstraintLocator(expr, ConstraintLocator::Member);

      // If adjoint has generic signature, calculate substitutions and update
      // adjoint decl ref with them.
      SubstitutionMap substitutions;
      if (auto genSig = adjointDecl->getGenericSignature()) {
        substitutions = solution.computeSubstitutions(genSig, locator);
        expr->setAdjointFunction(
          ConcreteDeclRef(adjointDecl, substitutions));
      }

      auto selected = solution.getOverloadChoice(locator);
      auto simplifiedType = simplifyType(selected.openedFullType);
      // For static methods, return result of curried method.
      if (adjointDecl->isStatic())
        simplifiedType = simplifiedType->castTo<AnyFunctionType>()->getResult();
      cs.setType(expr, simplifiedType);
      return expr;
    }

    // SWIFT_ENABLE_TENSORFLOW
    Expr *visitTFOp(ObjectLiteralExpr *expr) {
      // All #tfop operands are RValues.
      expr->setArg(cs.coerceToRValue(expr->getArg()));
      return expr;
    }

    Expr *visitObjectLiteralExpr(ObjectLiteralExpr *expr) {
      if (cs.getType(expr) && !cs.getType(expr)->hasTypeVariable())
        return expr;

      auto &ctx = cs.getASTContext();
      auto &tc = cs.getTypeChecker();

      // Figure out the type we're converting to.
      auto openedType = cs.getType(expr);
      auto type = simplifyType(openedType);
      cs.setType(expr, type);

      // SWIFT_ENABLE_TENSORFLOW
      // #tfop() declarations are not like normal object literals, so we use
      // special checking logic.
      if (expr->isTFOp())
        return visitTFOp(expr);

      if (type->is<UnresolvedType>()) return expr;

      Type conformingType = type;
      if (auto baseType = conformingType->getOptionalObjectType()) {
        // The type may be optional due to a failable initializer in the
        // protocol.
        conformingType = baseType;
      }

      // Find the appropriate object literal protocol.
      auto proto = tc.getLiteralProtocol(expr);
      assert(proto && "Missing object literal protocol?");
      auto conformance =
        tc.conformsToProtocol(conformingType, proto, cs.DC,
                              ConformanceCheckFlags::InExpression);
      assert(conformance && "object literal type conforms to protocol");

      Expr *base = TypeExpr::createImplicitHack(expr->getLoc(), conformingType,
                                                ctx);
      cs.cacheExprTypes(base);
        
      SmallVector<Expr *, 4> args;
      if (!isa<TupleExpr>(expr->getArg()))
        return nullptr;
      auto tupleArg = cast<TupleExpr>(expr->getArg());
      for (auto elt : tupleArg->getElements()) {
        cs.setExprTypes(elt);
        args.push_back(elt);
      }
      DeclName constrName(tc.getObjectLiteralConstructorName(expr));

      cs.cacheExprTypes(base);
      cs.setExprTypes(base);

      Expr *semanticExpr = tc.callWitness(base, dc, proto, *conformance,
                                          constrName, args,
                                          diag::object_literal_broken_proto);
      if (semanticExpr)
        cs.cacheExprTypes(semanticExpr);

      expr->setSemanticExpr(semanticExpr);
      return expr;
    }

    // Add a forced unwrap of an expression which either has type Optional<T>
    // or is a function that returns an Optional<T>. The latter turns into a
    // conversion expression that we will hoist above the ApplyExpr
    // that needs to be forced during the process of rewriting the expression.
    //
    // forForcedOptional is used to indicate that we will further need
    // to hoist this result above an explicit force of an optional that is
    // in place for something like an @optional protocol member from
    // Objective C that we might otherwise mistake for the thing we mean to
    // force here.
    Expr *forceUnwrapResult(Expr *expr, bool forForcedOptional =false) {
      auto ty = simplifyType(cs.getType(expr));

      if (forForcedOptional)
        ty = ty->getOptionalObjectType();

      if (auto *fnTy = ty->getAs<AnyFunctionType>()) {
        auto underlyingType = cs.replaceFinalResultTypeWithUnderlying(fnTy);

        auto &ctx = cs.getTypeChecker().Context;
        return cs.cacheType(new (ctx) ImplicitlyUnwrappedFunctionConversionExpr(
            expr, underlyingType));
      } else {
        return coerceImplicitlyUnwrappedOptionalToValue(
            expr, ty->getWithoutSpecifierType()->getOptionalObjectType());
      }
    }

    bool shouldForceUnwrapResult(OverloadChoice choice,
                                 ConstraintLocatorBuilder locator) {
      if (!choice.isImplicitlyUnwrappedValueOrReturnValue())
        return false;

      auto *choiceLocator = cs.getConstraintLocator(locator.withPathElement(
          ConstraintLocator::ImplicitlyUnwrappedDisjunctionChoice));

      return solution.getDisjunctionChoice(choiceLocator);
    }

    Expr *forceUnwrapIfExpected(Expr *expr, OverloadChoice choice,
                                ConstraintLocatorBuilder locator,
                                bool forForcedOptional = false) {
      if (!shouldForceUnwrapResult(choice, locator))
        return expr;

      // Force the expression if required for the solution.
      return forceUnwrapResult(expr, forForcedOptional);
    }

    Expr *visitDeclRefExpr(DeclRefExpr *expr) {
      auto locator = cs.getConstraintLocator(expr);

      // Find the overload choice used for this declaration reference.
      auto selected = solution.getOverloadChoiceIfAvailable(locator);
      if (!selected.hasValue()) {
        auto *varDecl = cast<VarDecl>(expr->getDecl());
        assert(varDecl->getType()->is<UnresolvedType>() &&
               "should only happen for closure arguments in CSDiags");
        cs.setType(expr, varDecl->getType());
        return expr;
      }

      return buildDeclRef(selected->choice, expr->getNameLoc(),
                          selected->openedFullType, locator, expr->isImplicit(),
                          expr->getFunctionRefKind(),
                          expr->getAccessSemantics());
    }

    Expr *visitSuperRefExpr(SuperRefExpr *expr) {
      simplifyExprType(expr);
      return expr;
    }

    Expr *visitTypeExpr(TypeExpr *expr) {
      auto toType = simplifyType(cs.getType(expr->getTypeLoc()));
      expr->getTypeLoc().setType(toType);
      cs.setType(expr, MetatypeType::get(toType));
      return expr;
    }

    Expr *visitOtherConstructorDeclRefExpr(OtherConstructorDeclRefExpr *expr) {
      cs.setType(expr, expr->getDecl()->getInitializerInterfaceType());
      return expr;
    }

    Expr *visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *expr) {
      return simplifyExprType(expr);
    }

    Expr *visitOverloadedDeclRefExpr(OverloadedDeclRefExpr *expr) {
      // Determine the declaration selected for this overloaded reference.
      auto locator = cs.getConstraintLocator(expr);
      auto selected = solution.getOverloadChoice(locator);
      auto choice = selected.choice;

      return buildDeclRef(choice, expr->getNameLoc(), selected.openedFullType,
                          locator, expr->isImplicit(),
                          choice.getFunctionRefKind(),
                          AccessSemantics::Ordinary);
    }

    Expr *visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *expr) {
      // FIXME: We should have generated an overload set from this, in which
      // case we can emit a typo-correction error here but recover well.
      return nullptr;
    }

    Expr *visitUnresolvedSpecializeExpr(UnresolvedSpecializeExpr *expr) {
      // Our specializations should have resolved the subexpr to the right type.
      return expr->getSubExpr();
    }

    Expr *visitMemberRefExpr(MemberRefExpr *expr) {
      auto memberLocator = cs.getConstraintLocator(expr,
                                                   ConstraintLocator::Member);
      auto selected = solution.getOverloadChoice(memberLocator);
      bool isDynamic
        = selected.choice.getKind() == OverloadChoiceKind::DeclViaDynamic;
      return buildMemberRef(
          expr->getBase(), selected.openedFullType, expr->getDotLoc(),
          selected.choice, expr->getNameLoc(), selected.openedType,
          cs.getConstraintLocator(expr), memberLocator, expr->isImplicit(),
          selected.choice.getFunctionRefKind(), expr->getAccessSemantics(),
          isDynamic);
    }

    Expr *visitDynamicMemberRefExpr(DynamicMemberRefExpr *expr) {
      llvm_unreachable("already type-checked?");
    }

    Expr *visitUnresolvedMemberExpr(UnresolvedMemberExpr *expr) {
      // Dig out the type of the base, which will be the result type of this
      // expression.  If constraint solving resolved this to an UnresolvedType,
      // then we're in an ambiguity tolerant mode used for diagnostic
      // generation.  Just leave this as an unresolved member reference.
      Type resultTy = simplifyType(cs.getType(expr));
      if (resultTy->getRValueType()->is<UnresolvedType>()) {
        cs.setType(expr, resultTy);
        return expr;
      }

      Type baseTy = resultTy->getRValueType();
      auto &tc = cs.getTypeChecker();

      // Find the selected member.
      auto memberLocator = cs.getConstraintLocator(
                             expr, ConstraintLocator::UnresolvedMember);
      auto selected = solution.getOverloadChoice(memberLocator);
      
      // If the member came by optional unwrapping, then unwrap the base type.
      if (selected.choice.getKind()
                              == OverloadChoiceKind::DeclViaUnwrappedOptional) {
        baseTy = baseTy->getOptionalObjectType();
        assert(baseTy
               && "got unwrapped optional decl from non-optional base?!");
      }

      // The base expression is simply the metatype of the base type.
      // FIXME: This location info is bogus.
      auto base = TypeExpr::createImplicitHack(expr->getDotLoc(), baseTy,
                                               tc.Context);
      cs.cacheExprTypes(base);

      // Build the member reference.
      bool isDynamic
        = selected.choice.getKind() == OverloadChoiceKind::DeclViaDynamic;
      auto result = buildMemberRef(
          base, selected.openedFullType, expr->getDotLoc(), selected.choice,
          expr->getNameLoc(), selected.openedType,
          cs.getConstraintLocator(expr), memberLocator, expr->isImplicit(),
          selected.choice.getFunctionRefKind(), AccessSemantics::Ordinary,
          isDynamic);
      if (!result)
        return nullptr;

      auto getType = [&](const Expr *E) -> Type {
        return cs.getType(E);
      };

      // If there was an argument, apply it.
      if (auto arg = expr->getArgument()) {
        ApplyExpr *apply = CallExpr::create(
            tc.Context, result, arg, expr->getArgumentLabels(),
            expr->getArgumentLabelLocs(), expr->hasTrailingClosure(),
            /*implicit=*/expr->isImplicit(), Type(), getType);
        result = finishApply(apply, Type(), cs.getConstraintLocator(expr));
      }

      return coerceToType(result, resultTy, cs.getConstraintLocator(expr));
    }
    
  private:
    /// A list of "suspicious" optional injections that come from
    /// forced downcasts.
    SmallVector<InjectIntoOptionalExpr *, 4> SuspiciousOptionalInjections;

  public:
    /// A list of optional injections that have been diagnosed.
    llvm::SmallPtrSet<InjectIntoOptionalExpr *, 4>  DiagnosedOptionalInjections;
  private:
    /// Create a member reference to the given constructor.
    Expr *applyCtorRefExpr(Expr *expr, Expr *base, SourceLoc dotLoc,
                           DeclNameLoc nameLoc, bool implicit,
                           ConstraintLocator *ctorLocator,
                           OverloadChoice choice,
                           FunctionRefKind functionRefKind, Type openedType) {

      auto *ctor = cast<ConstructorDecl>(choice.getDecl());

      // If the subexpression is a metatype, build a direct reference to the
      // constructor.
      if (cs.getType(base)->is<AnyMetatypeType>()) {
        return buildMemberRef(
            base, openedType, dotLoc, choice, nameLoc, cs.getType(expr),
            ConstraintLocatorBuilder(cs.getConstraintLocator(expr)),
            ctorLocator, implicit, functionRefKind, AccessSemantics::Ordinary,
            /*isDynamic=*/false);
      }

      // The subexpression must be either 'self' or 'super'.
      if (!base->isSuperExpr()) {
        // 'super' references have already been fully checked; handle the
        // 'self' case below.
        auto &tc = cs.getTypeChecker();
        bool diagnoseBadInitRef = true;
        auto arg = base->getSemanticsProvidingExpr();
        if (auto dre = dyn_cast<DeclRefExpr>(arg)) {
          if (dre->getDecl()->getFullName() == cs.getASTContext().Id_self) {
            // We have a reference to 'self'.
            diagnoseBadInitRef = false;

            // Special case -- in a protocol extension initializer with a class
            // constrainted Self type, 'self' has archetype type, and only
            // required initializers can be called.
            if (cs.getType(dre)->getRValueType()->is<ArchetypeType>()) {
              if (!diagnoseInvalidDynamicConstructorReferences(cs, base,
                                                               nameLoc,
                                                               ctor,
                                                               SuppressDiagnostics))
                return nullptr;
            }

            // Make sure the reference to 'self' occurs within an initializer.
            if (!dyn_cast_or_null<ConstructorDecl>(
                   cs.DC->getInnermostMethodContext())) {
              if (!SuppressDiagnostics)
                tc.diagnose(dotLoc, diag::init_delegation_outside_initializer);
              return nullptr;
            }
          }
        }

        // If we need to diagnose this as a bad reference to an initializer,
        // do so now.
        if (diagnoseBadInitRef) {
          // Determine whether 'super' would have made sense as a base.
          bool hasSuper = false;
          if (auto func = cs.DC->getInnermostMethodContext()) {
            if (auto classDecl = func->getDeclContext()->getSelfClassDecl()) {
              hasSuper = classDecl->hasSuperclass();
            }
          }

          if (SuppressDiagnostics)
            return nullptr;

          tc.diagnose(dotLoc, diag::bad_init_ref_base, hasSuper);
        }
      }

      // Build a partial application of the delegated initializer.
      Expr *ctorRef = buildOtherConstructorRef(openedType, ctor, base, nameLoc,
                                               ctorLocator, implicit);
      auto *call = new (cs.getASTContext()) DotSyntaxCallExpr(ctorRef, dotLoc,
                                                              base);

      return finishApply(call, cs.getType(expr),
                         ConstraintLocatorBuilder(
                           cs.getConstraintLocator(expr)));
    }

    Expr *applyMemberRefExpr(Expr *expr, Expr *base, SourceLoc dotLoc,
                             DeclNameLoc nameLoc, bool implicit) {
      // If we have a constructor member, handle it as a constructor.
      auto ctorLocator = cs.getConstraintLocator(
                           expr,
                           ConstraintLocator::ConstructorMember);
      if (auto selected = solution.getOverloadChoiceIfAvailable(ctorLocator)) {
        auto choice = selected->choice;
        return applyCtorRefExpr(
            expr, base, dotLoc, nameLoc, implicit, ctorLocator, choice,
            choice.getFunctionRefKind(), selected->openedFullType);
      }

      // Determine the declaration selected for this overloaded reference.
      auto memberLocator = cs.getConstraintLocator(expr,
                                                   ConstraintLocator::Member);
      auto selectedElt = solution.getOverloadChoiceIfAvailable(memberLocator);

      if (!selectedElt) {
        // If constraint solving resolved this to an UnresolvedType, then we're
        // in an ambiguity tolerant mode used for diagnostic generation.  Just
        // leave this as whatever type of member reference it already is.
        Type resultTy = simplifyType(cs.getType(expr));
        cs.setType(expr, resultTy);
        return expr;
      }

      auto selected = *selectedElt;
      if (!selected.choice.getBaseType()) {
        // This is one of the "outer alternatives", meaning the innermost
        // methods didn't work out.
        //
        // The only way to get here is via an UnresolvedDotExpr with outer
        // alternatives.
        auto UDE = cast<UnresolvedDotExpr>(expr);
        cs.diagnoseDeprecatedConditionalConformanceOuterAccess(
            UDE, selected.choice.getDecl());

        return buildDeclRef(selected.choice, nameLoc, selected.openedFullType,
                            memberLocator, implicit,
                            selected.choice.getFunctionRefKind(),
                            AccessSemantics::Ordinary);
      }

      switch (selected.choice.getKind()) {
      case OverloadChoiceKind::DeclViaBridge: {
        base = cs.coerceToRValue(base);

        // Look through an implicitly unwrapped optional.
        auto baseTy = cs.getType(base);
        auto &tc = cs.getTypeChecker();
        auto &ctx = tc.Context;
        auto baseMetaTy = baseTy->getAs<MetatypeType>();
        auto baseInstTy = (baseMetaTy ? baseMetaTy->getInstanceType() : baseTy);
        auto classTy = ctx.getBridgedToObjC(cs.DC, baseInstTy);

        if (baseMetaTy) {
          // FIXME: We're dropping side effects in the base here!
          base = TypeExpr::createImplicitHack(base->getLoc(), classTy,
                                              tc.Context);
          cs.cacheExprTypes(base);
        } else {
          // Bridge the base to its corresponding Objective-C object.
          base = bridgeToObjectiveC(base, classTy);
        }

        // Fall through to build the member reference.
        LLVM_FALLTHROUGH;
      }

      case OverloadChoiceKind::Decl:
      case OverloadChoiceKind::DeclViaUnwrappedOptional:
      case OverloadChoiceKind::DeclViaDynamic: {
        bool isDynamic
          = selected.choice.getKind() == OverloadChoiceKind::DeclViaDynamic;
        return buildMemberRef(base, selected.openedFullType, dotLoc,
                              selected.choice, nameLoc, selected.openedType,
                              cs.getConstraintLocator(expr), memberLocator,
                              implicit, selected.choice.getFunctionRefKind(),
                              AccessSemantics::Ordinary, isDynamic);
      }

      case OverloadChoiceKind::TupleIndex: {
        Type toType = simplifyType(cs.getType(expr));

        auto baseTy = cs.getType(base);
        // If the base type is not a tuple l-value, access to
        // its elements supposed to be r-value as well.
        //
        // This is modeled in constraint system in a way
        // that when member type is resolved by `resolveOverload`
        // it would take r-value type of the element at
        // specified index, but if it's a type variable it
        // could still be bound to l-value later.
        if (!baseTy->is<LValueType>())
          toType = toType->getRValueType();

        // If the result type is an rvalue and the base contains lvalues,
        // need a full tuple coercion to properly load & set access kind
        // on all underlying elements before taking a single element.
        if (!toType->hasLValueType() && baseTy->hasLValueType())
          base = coerceToType(base, baseTy->getRValueType(),
                              cs.getConstraintLocator(base));

        return cs.cacheType(new (cs.getASTContext())
                            TupleElementExpr(base, dotLoc,
                                             selected.choice.getTupleIndex(),
                                             nameLoc.getBaseNameLoc(), toType));
      }

      case OverloadChoiceKind::BaseType:
        return base;

      case OverloadChoiceKind::KeyPathApplication:
        llvm_unreachable("should only happen in a subscript");
          
      case OverloadChoiceKind::DynamicMemberLookup: {
        // Application of a DynamicMemberLookup result turns a member access of
        // x.foo into x[dynamicMember: "foo"].
        auto &ctx = cs.getASTContext();
        auto loc = nameLoc.getStartLoc();
        
        // Figure out the expected type of the string.  We know the
        // openedFullType will be "xType -> indexType -> resultType".  Dig out
        // its index type.
        auto declTy = solution.simplifyType(selected.openedFullType);
        auto subscriptTy = declTy->castTo<FunctionType>()->getResult();
        auto refFnType = subscriptTy->castTo<FunctionType>();
        assert(refFnType->getParams().size() == 1 &&
               "subscript always has one arg");
        auto stringType = refFnType->getParams()[0].getPlainType();
        auto tupleTy = TupleType::get(TupleTypeElt(stringType,
                                                   ctx.Id_dynamicMember), ctx);

        // Build and type check the string literal index value to the specific
        // string type expected by the subscript.
        auto fieldName = selected.choice.getName().getBaseIdentifier().str();
        auto index = getDMLIndexExpr(fieldName, tupleTy, loc, dc, cs);

        // Build and return a subscript that uses this string as the index.
        return buildSubscript(base, index, ctx.Id_dynamicMember,
                              /*trailingClosure*/false,
                              cs.getConstraintLocator(expr),
                              /*isImplicit*/false,
                              AccessSemantics::Ordinary, selected);
      }
      }

      llvm_unreachable("Unhandled OverloadChoiceKind in switch.");
    }
    
  public:
    Expr *visitUnresolvedDotExpr(UnresolvedDotExpr *expr) {
      return applyMemberRefExpr(expr, expr->getBase(), expr->getDotLoc(),
                                expr->getNameLoc(), expr->isImplicit());
    }

    Expr *visitSequenceExpr(SequenceExpr *expr) {
      llvm_unreachable("Expression wasn't parsed?");
    }

    Expr *visitArrowExpr(ArrowExpr *expr) {
      llvm_unreachable("Arrow expr wasn't converted to type?");
    }

    Expr *visitIdentityExpr(IdentityExpr *expr) {
      cs.setType(expr, cs.getType(expr->getSubExpr()));
      return expr;
    }

    Expr *visitAnyTryExpr(AnyTryExpr *expr) {
      cs.setType(expr, cs.getType(expr->getSubExpr()));
      return expr;
    }

    Expr *visitOptionalTryExpr(OptionalTryExpr *expr) {
      return simplifyExprType(expr);
    }

    Expr *visitParenExpr(ParenExpr *expr) {
      auto &ctx = cs.getASTContext();
      auto pty = cs.getType(expr->getSubExpr());
      cs.setType(expr, ParenType::get(ctx, pty->getInOutObjectType(),
                                      ParameterTypeFlags().withInOut(pty->is<InOutType>())));
      return expr;
    }

    Expr *visitTupleExpr(TupleExpr *expr) {
      return simplifyExprType(expr);
    }

    Expr *visitSubscriptExpr(SubscriptExpr *expr) {
      return buildSubscript(expr->getBase(), expr->getIndex(),
                            expr->getArgumentLabels(),
                            expr->hasTrailingClosure(),
                            cs.getConstraintLocator(expr),
                            expr->isImplicit(),
                            expr->getAccessSemantics());
    }

    /// "Finish" an array expression by filling in the semantic expression.
    ArrayExpr *finishArrayExpr(ArrayExpr *expr) {
      Type arrayTy = cs.getType(expr);
      auto &tc = cs.getTypeChecker();

      ProtocolDecl *arrayProto
        = tc.getProtocol(expr->getLoc(),
                         KnownProtocolKind::ExpressibleByArrayLiteral);
      assert(arrayProto && "type-checked array literal w/o protocol?!");

      auto conformance =
        tc.conformsToProtocol(arrayTy, arrayProto, cs.DC,
                              ConformanceCheckFlags::InExpression);
      assert(conformance && "Type does not conform to protocol?");

      // Call the witness that builds the array literal.
      // FIXME: callWitness() may end up re-doing some work we already did
      // to convert the array literal elements to the element type. It would
      // be nicer to re-use them.

      // FIXME: This location info is bogus.
      Expr *typeRef = TypeExpr::createImplicitHack(expr->getLoc(), arrayTy,
                                                   tc.Context);
      cs.cacheExprTypes(typeRef);

      DeclName name(tc.Context, DeclBaseName::createConstructor(),
                    { tc.Context.Id_arrayLiteral });

      // Coerce the array elements to be rvalues, so that other type-checker
      // code that attempts to peephole the AST doesn't have to re-load the
      // elements (and break the invariant that lvalue nodes only get their
      // access kind set once).
      for (auto &element : expr->getElements()) {
        element = cs.coerceToRValue(element);
      }

      // Restructure the argument to provide the appropriate labels in the
      // tuple.
      SmallVector<TupleTypeElt, 4> typeElements;
      SmallVector<Identifier, 4> names;
      bool first = true;
      for (auto elt : expr->getElements()) {
        if (first) {
          typeElements.push_back(TupleTypeElt(cs.getType(elt),
                                              tc.Context.Id_arrayLiteral));
          names.push_back(tc.Context.Id_arrayLiteral);

          first = false;
          continue;
        }

        typeElements.push_back(cs.getType(elt));
        names.push_back(Identifier());
      }

      Type argType = TupleType::get(typeElements, tc.Context);
      assert(isa<TupleType>(argType.getPointer()));

      Expr *arg =
        TupleExpr::create(tc.Context, SourceLoc(),
                          expr->getElements(),
                          names,
                          { },
                          SourceLoc(), /*HasTrailingClosure=*/false,
                          /*Implicit=*/true,
                          argType);

      cs.cacheExprTypes(arg);

      cs.setExprTypes(typeRef);
      cs.setExprTypes(arg);

      Expr *result = tc.callWitness(typeRef, dc, arrayProto, *conformance,
                                    name, arg, diag::array_protocol_broken);
      if (!result)
        return nullptr;

      cs.cacheExprTypes(result);

      expr->setSemanticExpr(result);
      return expr;
    }

    Expr *visitArrayExpr(ArrayExpr *expr) {
      Type openedType = cs.getType(expr);
      Type arrayTy = simplifyType(openedType);
      cs.setType(expr, arrayTy);
      if (!finishArrayExpr(expr)) return nullptr;

      // If the array element type was defaulted, note that in the expression.
      if (solution.DefaultedConstraints.count(cs.getConstraintLocator(expr)))
        expr->setIsTypeDefaulted();

      return expr;
    }

    /// "Finish" a dictionary expression by filling in the semantic expression.
    DictionaryExpr *finishDictionaryExpr(DictionaryExpr *expr) {
      Type dictionaryTy = cs.getType(expr);

      auto &tc = cs.getTypeChecker();
      ProtocolDecl *dictionaryProto
        = tc.getProtocol(expr->getLoc(),
                         KnownProtocolKind::ExpressibleByDictionaryLiteral);

      auto conformance =
        tc.conformsToProtocol(dictionaryTy, dictionaryProto, cs.DC,
                              ConformanceCheckFlags::InExpression);
      if (!conformance)
        return nullptr;

      // Call the witness that builds the dictionary literal.
      // FIXME: callWitness() may end up re-doing some work we already did
      // to convert the dictionary literal elements to the (key, value) tuple.
      // It would be nicer to re-use them.
      // FIXME: Cache the name.
      // FIXME: This location info is bogus.
      Expr *typeRef = TypeExpr::createImplicitHack(expr->getLoc(), dictionaryTy,
                                                   tc.Context);
      cs.cacheExprTypes(typeRef);

      DeclName name(tc.Context, DeclBaseName::createConstructor(),
                    { tc.Context.Id_dictionaryLiteral });

      // Restructure the argument to provide the appropriate labels in the
      // tuple.
      SmallVector<TupleTypeElt, 4> typeElements;
      SmallVector<Identifier, 4> names;
      bool first = true;
      for (auto elt : expr->getElements()) {
        if (first) {
          typeElements.push_back(TupleTypeElt(cs.getType(elt),
                                              tc.Context.Id_dictionaryLiteral));
          names.push_back(tc.Context.Id_dictionaryLiteral);

          first = false;
          continue;
        }

        typeElements.push_back(cs.getType(elt));
        names.push_back(Identifier());
      }

      Type argType = TupleType::get(typeElements, tc.Context);
      assert(isa<TupleType>(argType.getPointer()));

      Expr *arg =
            TupleExpr::create(tc.Context, expr->getLBracketLoc(),
                              expr->getElements(),
                              names,
                              { },
                              expr->getRBracketLoc(),
                              /*HasTrailingClosure=*/false,
                              /*Implicit=*/true,
                              argType);

      cs.cacheExprTypes(arg);

      cs.setExprTypes(typeRef);
      cs.setExprTypes(arg);

      Expr *result = tc.callWitness(typeRef, dc, dictionaryProto,
                                    *conformance, name, arg,
                                    diag::dictionary_protocol_broken);
      if (!result)
        return nullptr;

      cs.cacheExprTypes(result);

      expr->setSemanticExpr(result);
      return expr;
    }

    Expr *visitDictionaryExpr(DictionaryExpr *expr) {
      Type openedType = cs.getType(expr);
      Type dictionaryTy = simplifyType(openedType);
      cs.setType(expr, dictionaryTy);
      if (!finishDictionaryExpr(expr)) return nullptr;

      // If the dictionary key or value type was defaulted, note that in the
      // expression.
      if (solution.DefaultedConstraints.count(cs.getConstraintLocator(expr)))
        expr->setIsTypeDefaulted();

      return expr;
    }

    Expr *visitDynamicSubscriptExpr(DynamicSubscriptExpr *expr) {
      return buildSubscript(expr->getBase(), expr->getIndex(),
                            expr->getArgumentLabels(),
                            expr->hasTrailingClosure(),
                            cs.getConstraintLocator(expr),
                            expr->isImplicit(), AccessSemantics::Ordinary);
    }

    Expr *visitTupleElementExpr(TupleElementExpr *expr) {
      simplifyExprType(expr);
      return expr;
    }

    Expr *visitCaptureListExpr(CaptureListExpr *expr) {
      // The type of the capture list is the type of the closure contained
      // inside it.
      cs.setType(expr, cs.getType(expr->getClosureBody()));
      return expr;
    }

    Expr *visitClosureExpr(ClosureExpr *expr) {
      llvm_unreachable("Handled by the walker directly");
    }

    Expr *visitAutoClosureExpr(AutoClosureExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Expr *visitInOutExpr(InOutExpr *expr) {
      auto objectTy = cs.getType(expr->getSubExpr())->getRValueType();

      // The type is simply inout of whatever the lvalue's object type was.
      cs.setType(expr, InOutType::get(objectTy));
      return expr;
    }

    Expr *visitVarargExpansionExpr(VarargExpansionExpr *expr) {
      simplifyExprType(expr);

      auto elementTy = cs.getType(expr);
      auto arrayTy =
        cs.getTypeChecker().getArraySliceType(expr->getLoc(), elementTy);
      if (!arrayTy) return expr;

      expr->setSubExpr(coerceToType(expr->getSubExpr(), arrayTy,
                                    cs.getConstraintLocator(expr)));
      return expr;
    }

    Expr *visitDynamicTypeExpr(DynamicTypeExpr *expr) {
      Expr *base = expr->getBase();
      base = cs.coerceToRValue(base);
      expr->setBase(base);

      return simplifyExprType(expr);
    }

    Expr *visitOpaqueValueExpr(OpaqueValueExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Expr *visitApplyExpr(ApplyExpr *expr) {
      return finishApply(expr, cs.getType(expr),
                         ConstraintLocatorBuilder(
                           cs.getConstraintLocator(expr)));
    }

    Expr *visitRebindSelfInConstructorExpr(RebindSelfInConstructorExpr *expr) {
      // A non-failable initializer cannot delegate to a failable
      // initializer.
      Expr *unwrappedSubExpr = expr->getSubExpr()->getSemanticsProvidingExpr();
      Type valueTy = cs.getType(unwrappedSubExpr)->getOptionalObjectType();
      auto inCtor = cast<ConstructorDecl>(cs.DC->getInnermostMethodContext());
      if (valueTy && inCtor->getFailability() == OTK_None) {
        bool isChaining;
        auto *otherCtorRef = expr->getCalledConstructor(isChaining);
        ConstructorDecl *ctor = otherCtorRef->getDecl();
        assert(ctor);

        // If the initializer we're calling is not declared as
        // checked, it's an error.
        bool isError =
            !ctor->getAttrs().hasAttribute<ImplicitlyUnwrappedOptionalAttr>();

        // If we're suppressing diagnostics, just fail.
        if (isError && SuppressDiagnostics)
          return nullptr;

        auto &tc = cs.getTypeChecker();
        auto &ctx = tc.Context;

        if (isError) {
          if (auto *optTry = dyn_cast<OptionalTryExpr>(unwrappedSubExpr)) {
            tc.diagnose(optTry->getTryLoc(),
                        diag::delegate_chain_nonoptional_to_optional_try,
                        isChaining);
            tc.diagnose(optTry->getTryLoc(), diag::init_delegate_force_try)
              .fixItReplace({optTry->getTryLoc(), optTry->getQuestionLoc()},
                            "try!");
            tc.diagnose(inCtor->getLoc(), diag::init_propagate_failure)
              .fixItInsertAfter(inCtor->getLoc(), "?");
          } else {
            // Give the user the option of adding '!' or making the enclosing
            // initializer failable.
            tc.diagnose(otherCtorRef->getLoc(),
                        diag::delegate_chain_nonoptional_to_optional,
                        isChaining, ctor->getFullName());
            tc.diagnose(otherCtorRef->getLoc(), diag::init_force_unwrap)
              .fixItInsertAfter(expr->getEndLoc(), "!");
            tc.diagnose(inCtor->getLoc(), diag::init_propagate_failure)
              .fixItInsertAfter(inCtor->getLoc(), "?");
          }
        }

        // Recover by injecting the force operation (the first option).
        Expr *newSub = new (ctx) ForceValueExpr(expr->getSubExpr(),
                                                expr->getEndLoc());
        cs.setType(newSub, valueTy);
        newSub->setImplicit();
        expr->setSubExpr(newSub);
      }

      return expr;
    }

    Expr *visitIfExpr(IfExpr *expr) {
      auto resultTy = simplifyType(cs.getType(expr));
      cs.setType(expr, resultTy);

      // Convert the condition to a logic value.
      auto cond
        = solution.convertBooleanTypeToBuiltinI1(expr->getCondExpr(),
                                                 cs.getConstraintLocator(expr));
      expr->setCondExpr(cond);

      // Coerce the then/else branches to the common type.
      expr->setThenExpr(coerceToType(expr->getThenExpr(), resultTy,
                               cs.getConstraintLocator(expr->getThenExpr())));
      expr->setElseExpr(coerceToType(expr->getElseExpr(), resultTy,
                                 cs.getConstraintLocator(expr->getElseExpr())));

      return expr;
    }
    
    Expr *visitImplicitConversionExpr(ImplicitConversionExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Expr *visitIsExpr(IsExpr *expr) {
      // Turn the subexpression into an rvalue.
      auto &tc = cs.getTypeChecker();
      auto toType = simplifyType(cs.getType(expr->getCastTypeLoc()));
      auto sub = cs.coerceToRValue(expr->getSubExpr());

      checkForImportedUsedConformances(toType);
      expr->setSubExpr(sub);

      // Set the type we checked against.
      expr->getCastTypeLoc().setType(toType);
      auto fromType = cs.getType(sub);
      auto castContextKind =
          SuppressDiagnostics ? CheckedCastContextKind::None
                              : CheckedCastContextKind::IsExpr;
      auto castKind = tc.typeCheckCheckedCast(
                        fromType, toType, castContextKind, cs.DC,
                        expr->getLoc(), sub,
                        expr->getCastTypeLoc().getSourceRange());

      switch (castKind) {
      case CheckedCastKind::Unresolved:
        expr->setCastKind(CheckedCastKind::ValueCast);
        break;
          
      case CheckedCastKind::Coercion:
      case CheckedCastKind::BridgingCoercion:
        // Check is trivially true.
        tc.diagnose(expr->getLoc(), diag::isa_is_always_true, "is");
        expr->setCastKind(castKind);
        break;
      case CheckedCastKind::ValueCast:
        // Check the cast target is a non-foreign type
        if (auto cls = toType->getAs<ClassType>()) {
          if (cls->getDecl()->getForeignClassKind() ==
                ClassDecl::ForeignKind::CFType) {
            tc.diagnose(expr->getLoc(), diag::isa_is_foreign_check, toType);
          }
        }
        expr->setCastKind(castKind);
        break;
      case CheckedCastKind::ArrayDowncast:
      case CheckedCastKind::DictionaryDowncast:
      case CheckedCastKind::SetDowncast:
        // Valid checks.
        expr->setCastKind(castKind);
        break;
      }

      // SIL-generation magically turns this into a Bool; make sure it can.
      if (!cs.getASTContext().getGetBoolDecl(&cs.getTypeChecker())) {
        tc.diagnose(expr->getLoc(), diag::bool_intrinsics_not_found);
        // Continue anyway.
      }

      // Dig through the optionals in the from/to types.
      SmallVector<Type, 2> fromOptionals;
      fromType->lookThroughAllOptionalTypes(fromOptionals);
      SmallVector<Type, 2> toOptionals;
      toType->lookThroughAllOptionalTypes(toOptionals);

      // If we have an imbalance of optionals or a collection
      // downcast, handle this as a checked cast followed by a
      // a 'hasValue' check.
      if (fromOptionals.size() != toOptionals.size() ||
          castKind == CheckedCastKind::ArrayDowncast ||
          castKind == CheckedCastKind::DictionaryDowncast ||
          castKind == CheckedCastKind::SetDowncast) {
        auto toOptType = OptionalType::get(toType);
        ConditionalCheckedCastExpr *cast
          = new (tc.Context) ConditionalCheckedCastExpr(
                               sub, expr->getLoc(), SourceLoc(),
                               TypeLoc::withoutLoc(toType));
        cs.setType(cast, toOptType);
        cs.setType(cast->getCastTypeLoc(), toType);
        if (expr->isImplicit())
          cast->setImplicit();

        // Type-check this conditional case.
        Expr *result = handleConditionalCheckedCastExpr(cast, true);
        if (!result)
          return nullptr;

        // Extract a Bool from the resulting expression.
        return solution.convertOptionalToBool(result,
                                              cs.getConstraintLocator(expr));
      }

      return expr;
    }

    /// The kind of cast we're working with for handling optional bindings.
    enum class OptionalBindingsCastKind {
      /// An explicit bridging conversion, spelled "as".
      Bridged,
      /// A forced cast, spelled "as!".
      Forced,
      /// A conditional cast, spelled "as?".
      Conditional,
    };

    /// Handle optional operands and results in an explicit cast.
    Expr *handleOptionalBindingsForCast(ExplicitCastExpr *cast, 
                                        Type finalResultType,
                                        OptionalBindingsCastKind castKind) {
      return handleOptionalBindings(cast->getSubExpr(), finalResultType,
                                    castKind,
        [&](Expr *sub, Type resultType) -> Expr* {

        // Complain about conditional casts to CF class types; they can't
        // actually be conditionally checked.
        if (castKind == OptionalBindingsCastKind::Conditional) {
          Type destValueType = resultType->getOptionalObjectType();
          auto destObjectType = destValueType;
          if (auto metaTy = destObjectType->getAs<MetatypeType>())
            destObjectType = metaTy->getInstanceType();
          if (auto destClass = destObjectType->getClassOrBoundGenericClass()) {
            if (destClass->getForeignClassKind() ==
                  ClassDecl::ForeignKind::CFType) {
              if (SuppressDiagnostics)
                return nullptr;

              auto &tc = cs.getTypeChecker();
              tc.diagnose(cast->getLoc(), diag::conditional_downcast_foreign,
                          destValueType);
              ConcreteDeclRef refDecl = sub->getReferencedDecl();
              if (refDecl) {
                tc.diagnose(cast->getLoc(), diag::note_explicitly_compare_cftypeid,
                            refDecl.getDecl()->getBaseName(), destValueType);
              }
            }
          }
        }

        // Set the expression as the sub-expression of the cast, then
        // use the cast as the inner operation.
        cast->setSubExpr(sub);
        cs.setType(cast, resultType);
        return cast;
      });
    }

    /// A helper function to build an operation.  The inner result type
    /// is the expected type of the operation; it will be a non-optional
    /// type unless the castKind is Conditional.
    using OperationBuilderRef =
      llvm::function_ref<Expr*(Expr *subExpr, Type innerResultType)>;

    /// Handle optional operands and results in an explicit cast.
    Expr *handleOptionalBindings(Expr *subExpr, Type finalResultType,
                                 OptionalBindingsCastKind castKind,
                                 OperationBuilderRef buildInnerOperation) {
      auto &tc = cs.getTypeChecker();

      unsigned destExtraOptionals;
      bool forceExtraSourceOptionals;
      switch (castKind) {
      case OptionalBindingsCastKind::Bridged:
        destExtraOptionals = 0;
        forceExtraSourceOptionals = true;
        break;

      case OptionalBindingsCastKind::Forced:
        destExtraOptionals = 0;
        forceExtraSourceOptionals = true;
        break;

      case OptionalBindingsCastKind::Conditional:
        destExtraOptionals = 1;
        forceExtraSourceOptionals = false;
        break;
      }

      // FIXME: some of this work needs to be delayed until runtime to
      // properly account for archetypes dynamically being optional
      // types.  For example, if we're casting T to NSView?, that
      // should succeed if T=NSObject? and its value is actually nil.
      Type srcType = cs.getType(subExpr);

      SmallVector<Type, 4> srcOptionals;
      srcType = srcType->lookThroughAllOptionalTypes(srcOptionals);

      SmallVector<Type, 4> destOptionals;
      auto destValueType
        = finalResultType->lookThroughAllOptionalTypes(destOptionals);

      auto isBridgeToAnyObject =
        castKind == OptionalBindingsCastKind::Bridged &&
        destValueType->isAnyObject();

      // If the destination value type is 'AnyObject' when performing a
      // bridging operation, or if the destination value type could dynamically
      // be an optional type, leave any extra optionals on the source in place.
      // Only apply the latter condition in Swift 5 mode to best preserve
      // compatibility with Swift 4.1's casting behaviour.
      if (isBridgeToAnyObject || (tc.Context.isSwiftVersionAtLeast(5) &&
                                  destValueType->canDynamicallyBeOptionalType(
                                      /*includeExistential*/ false))) {
        auto destOptionalsCount = destOptionals.size() - destExtraOptionals;
        if (srcOptionals.size() > destOptionalsCount) {
          srcType = srcOptionals[destOptionalsCount];
          srcOptionals.erase(srcOptionals.begin() + destOptionalsCount,
                             srcOptionals.end());
        }
      }

      // When performing a bridging operation, if the destination type
      // is more optional than the source, we'll add extra optional injections
      // at the end.
      SmallVector<Type, 4> destOptionalInjections;
      if (castKind == OptionalBindingsCastKind::Bridged &&
          destOptionals.size() > srcOptionals.size()) {
        // Remove the extra optionals from destOptionals, but keep them around
        // separately so we can perform the injections on the final result of
        // the cast.
        auto cutPoint = destOptionals.end() - srcOptionals.size();
        destOptionalInjections.append(destOptionals.begin(), cutPoint);
        destOptionals.erase(destOptionals.begin(), cutPoint);

        finalResultType = destOptionals.empty() ? destValueType
                                                : destOptionals.front();
      }

      // Local function to add the optional injections to final result.
      auto addFinalOptionalInjections = [&](Expr *result) {
        for (auto destType : reversed(destOptionalInjections)) {
          result =
            cs.cacheType(new (tc.Context) InjectIntoOptionalExpr(result,
                                                                 destType));
        }

        return result;
      };

      // There's nothing special to do if the operand isn't optional
      // and we don't need any bridging.
      if (srcOptionals.empty()) {
        Expr *result = buildInnerOperation(subExpr, finalResultType);
        if (!result) return nullptr;
        return addFinalOptionalInjections(result);
      }

      // The result type (without the final optional) is a subtype of
      // the operand type, so it will never have a higher depth.
      assert(destOptionals.size() - destExtraOptionals <= srcOptionals.size());

      // The outermost N levels of optionals on the operand must all
      // be present or the cast fails.  The innermost M levels of
      // optionals on the operand are reflected in the requested
      // destination type, so we should map these nils into the result.
      unsigned numRequiredOptionals =
        srcOptionals.size() - (destOptionals.size() - destExtraOptionals);

      // The number of OptionalEvaluationExprs between the point of the
      // inner cast and the enclosing OptionalEvaluationExpr (exclusive)
      // which represents failure for the entire operation.
      unsigned failureDepth = destOptionals.size() - destExtraOptionals;

      // Drill down on the operand until it's non-optional.
      SourceLoc fakeQuestionLoc = subExpr->getEndLoc();
      for (unsigned i : indices(srcOptionals)) {
        Type valueType =
          (i + 1 == srcOptionals.size() ? srcType : srcOptionals[i+1]);

        // As we move into the range of mapped optionals, start
        // lowering the depth.
        unsigned depth = failureDepth;
        if (i >= numRequiredOptionals) {
          depth -= (i - numRequiredOptionals) + 1;
        } else if (forceExtraSourceOptionals) {
          // For a forced cast, force the required optionals.
          subExpr = new (tc.Context) ForceValueExpr(subExpr, fakeQuestionLoc);
          cs.setType(subExpr, valueType);
          subExpr->setImplicit(true);
          continue;
        }

        subExpr =
          cs.cacheType(new (tc.Context) BindOptionalExpr(subExpr,
                                                         fakeQuestionLoc,
                                                         depth, valueType));
        subExpr->setImplicit(true);
      }

      // If this is a conditional cast, the result type will always
      // have at least one level of optional, which should become the
      // type of the checked-cast expression.
      Expr *result;
      if (castKind == OptionalBindingsCastKind::Conditional) {
        assert(!destOptionals.empty() &&
               "result of checked cast is not an optional type");
        result = buildInnerOperation(subExpr, destOptionals.back());
      } else {
        result = buildInnerOperation(subExpr, destValueType);
      }
      if (!result) return nullptr;

      // If we're casting to an optional type, we need to capture the
      // final M bindings.

      if (destOptionals.size() > destExtraOptionals) {
        if (castKind == OptionalBindingsCastKind::Conditional) {
          // If the innermost cast fails, the entire expression fails.  To
          // get this behavior, we have to bind and then re-inject the result.
          // (SILGen should know how to peephole this.)
          result =
            cs.cacheType(new (tc.Context) BindOptionalExpr(result,
                                                           result->getEndLoc(),
                                                           failureDepth,
                                                           destValueType));
          result->setImplicit(true);
        }

        for (unsigned i = destOptionals.size(); i != 0; --i) {
          Type destType = destOptionals[i-1];
          result =
            cs.cacheType(new (tc.Context) InjectIntoOptionalExpr(result,
                                                                 destType));
          result =
            cs.cacheType(new (tc.Context) OptionalEvaluationExpr(result,
                                                                 destType));
        }

      // Otherwise, we just need to capture the failure-depth binding.
      } else if (!forceExtraSourceOptionals) {
        result =
          cs.cacheType(new (tc.Context) OptionalEvaluationExpr(result,
                                                              finalResultType));
      }

      return addFinalOptionalInjections(result);
    }

    bool hasForcedOptionalResult(ExplicitCastExpr *expr) {
      auto *TR = expr->getCastTypeLoc().getTypeRepr();
      if (TR && TR->getKind() == TypeReprKind::ImplicitlyUnwrappedOptional) {
        auto *locator = cs.getConstraintLocator(
            expr, ConstraintLocator::ImplicitlyUnwrappedDisjunctionChoice);
        return solution.getDisjunctionChoice(locator);
      }
      return false;
    }

    Expr *visitCoerceExpr(CoerceExpr *expr) {
      // If we need to insert a force-unwrap for coercions of the form
      // 'as T!', do so now.
      if (hasForcedOptionalResult(expr)) {
        auto *coerced = visitCoerceExpr(expr, None);
        if (!coerced)
          return nullptr;

        return coerceImplicitlyUnwrappedOptionalToValue(
            coerced, cs.getType(coerced)->getOptionalObjectType());
      }

      return visitCoerceExpr(expr, None);
    }

    Expr *visitCoerceExpr(CoerceExpr *expr, Optional<unsigned> choice) {
      // Simplify the type we're casting to.
      auto toType = simplifyType(cs.getType(expr->getCastTypeLoc()));
      expr->getCastTypeLoc().setType(toType);
      checkForImportedUsedConformances(toType);

      auto &tc = cs.getTypeChecker();

      // If this is a literal that got converted into constructor call
      // lets put proper source information in place.
      if (expr->isLiteralInit()) {
        auto *literalInit = expr->getSubExpr();
        if (auto *call = dyn_cast<CallExpr>(literalInit)) {
          call->getFn()->forEachChildExpr([&](Expr *subExpr) -> Expr * {
            auto *TE = dyn_cast<TypeExpr>(subExpr);
            if (!TE)
              return subExpr;

            auto type = TE->getInstanceType(
                [&](const Expr *expr) { return cs.hasType(expr); },
                [&](const Expr *expr) { return cs.getType(expr); });

            assert(!type->hasError());

            if (!type->isEqual(toType))
              return subExpr;

            return cs.cacheType(new (tc.Context)
                                    TypeExpr(expr->getCastTypeLoc()));
          });
        }

        literalInit->setImplicit(false);

        // Keep the coercion around, because it contains the source range
        // for the original constructor call.
        return expr;
      }

      // Turn the subexpression into an rvalue.
      auto rvalueSub = cs.coerceToRValue(expr->getSubExpr());
      expr->setSubExpr(rvalueSub);

      // If we weren't explicitly told by the caller which disjunction choice,
      // get it from the solution to determine whether we've picked a coercion
      // or a bridging conversion.
      auto *locator = cs.getConstraintLocator(expr);

      if (!choice) {
        choice = solution.getDisjunctionChoice(locator);
      }

      // Handle the coercion/bridging of the underlying subexpression, where
      // optionality has been removed.
      if (*choice == 0) {
        // Convert the subexpression.
        Expr *sub = expr->getSubExpr();

        cs.setExprTypes(sub);
        if (tc.convertToType(sub, toType, cs.DC))
          return nullptr;
        cs.cacheExprTypes(sub);

        expr->setSubExpr(sub);
        cs.setType(expr, toType);
        return expr;
      }

      // Bridging conversion.
      assert(*choice == 1 && "should be bridging");

      // Handle optional bindings.
      Expr *sub = handleOptionalBindings(expr->getSubExpr(), toType,
                                         OptionalBindingsCastKind::Bridged,
                                         [&](Expr *sub, Type toInstanceType) {
        return buildObjCBridgeExpr(sub, toInstanceType, locator);
      });

      if (!sub) return nullptr;
      expr->setSubExpr(sub);
      cs.setType(expr, toType);
      return expr;
    }

    // Rewrite ForcedCheckedCastExpr based on what the solver computed.
    Expr *visitForcedCheckedCastExpr(ForcedCheckedCastExpr *expr) {
      // Simplify the type we're casting to.
      auto toType = simplifyType(cs.getType(expr->getCastTypeLoc()));
      if (hasForcedOptionalResult(expr))
        toType = toType->getOptionalObjectType();

      expr->getCastTypeLoc().setType(toType);
      checkForImportedUsedConformances(toType);

      // The subexpression is always an rvalue.
      auto &tc = cs.getTypeChecker();
      auto sub = cs.coerceToRValue(expr->getSubExpr());
      expr->setSubExpr(sub);

      auto castContextKind =
          SuppressDiagnostics ? CheckedCastContextKind::None
                              : CheckedCastContextKind::ForcedCast;

      auto fromType = cs.getType(sub);
      auto castKind = tc.typeCheckCheckedCast(
                        fromType, toType, castContextKind, cs.DC,
                        expr->getLoc(), sub,
                        expr->getCastTypeLoc().getSourceRange());
      switch (castKind) {
        /// Invalid cast.
      case CheckedCastKind::Unresolved:
        return nullptr;
      case CheckedCastKind::Coercion:
      case CheckedCastKind::BridgingCoercion: {
        if (SuppressDiagnostics)
          return nullptr;

        if (cs.getType(sub)->isEqual(toType)) {
          tc.diagnose(expr->getLoc(), diag::forced_downcast_noop, toType)
            .fixItRemove(SourceRange(expr->getLoc(),
                                 expr->getCastTypeLoc().getSourceRange().End));

        } else {
          tc.diagnose(expr->getLoc(), diag::forced_downcast_coercion,
                      cs.getType(sub), toType)
            .fixItReplace(SourceRange(expr->getLoc(), expr->getExclaimLoc()),
                          "as");
        }

        // Transmute the checked cast into a coercion expression.
        auto *result = new (tc.Context) CoerceExpr(sub, expr->getLoc(),
                                                   expr->getCastTypeLoc());
        cs.setType(result, toType);
        cs.setType(result->getCastTypeLoc(), toType);
        unsigned disjunctionChoice =
          (castKind == CheckedCastKind::Coercion ? 0 : 1);
        return visitCoerceExpr(result, disjunctionChoice);
      }

      // Valid casts.
      case CheckedCastKind::ArrayDowncast:
      case CheckedCastKind::DictionaryDowncast:
      case CheckedCastKind::SetDowncast:
      case CheckedCastKind::ValueCast:
        expr->setCastKind(castKind);
        break;
      }
      
      return handleOptionalBindingsForCast(expr, simplifyType(cs.getType(expr)),
                                           OptionalBindingsCastKind::Forced);
    }

    Expr *visitConditionalCheckedCastExpr(ConditionalCheckedCastExpr *expr) {
      // If we need to insert a force-unwrap for coercions of the form
      // 'as! T!', do so now.
      if (hasForcedOptionalResult(expr)) {
        auto *coerced = handleConditionalCheckedCastExpr(expr);
        if (!coerced)
          return nullptr;

        return coerceImplicitlyUnwrappedOptionalToValue(
            coerced, cs.getType(coerced)->getOptionalObjectType());
      }

      return handleConditionalCheckedCastExpr(expr);
    }

    Expr *handleConditionalCheckedCastExpr(ConditionalCheckedCastExpr *expr,
                                           bool isInsideIsExpr = false) {
      // Simplify the type we're casting to.
      auto toType = simplifyType(cs.getType(expr->getCastTypeLoc()));
      checkForImportedUsedConformances(toType);
      expr->getCastTypeLoc().setType(toType);

      // The subexpression is always an rvalue.
      auto &tc = cs.getTypeChecker();
      auto sub = cs.coerceToRValue(expr->getSubExpr());
      expr->setSubExpr(sub);


      auto castContextKind =
          (SuppressDiagnostics || isInsideIsExpr)
            ? CheckedCastContextKind::None
            : CheckedCastContextKind::ConditionalCast;

      auto fromType = cs.getType(sub);
      auto castKind = tc.typeCheckCheckedCast(
                        fromType, toType, castContextKind, cs.DC,
                        expr->getLoc(), sub,
                        expr->getCastTypeLoc().getSourceRange());
      switch (castKind) {
        /// Invalid cast.
      case CheckedCastKind::Unresolved:
        expr->setCastKind(CheckedCastKind::ValueCast);
        break;

      case CheckedCastKind::Coercion:
      case CheckedCastKind::BridgingCoercion: {
        if (SuppressDiagnostics)
          return nullptr;

        tc.diagnose(expr->getLoc(), diag::conditional_downcast_coercion,
                    cs.getType(sub), toType);

        // Transmute the checked cast into a coercion expression.
        auto *coerce = new (tc.Context) CoerceExpr(sub, expr->getLoc(),
                                                   expr->getCastTypeLoc());
        cs.setType(coerce, toType);
        cs.setType(coerce->getCastTypeLoc(), toType);
        unsigned disjunctionChoice =
          (castKind == CheckedCastKind::Coercion ? 0 : 1);
        Expr *result = visitCoerceExpr(coerce, disjunctionChoice);
        if (!result)
          return nullptr;

        // Wrap the result in an optional. Mark the optional injection as
        // explicit, because the user did in fact write the '?' as part of
        // 'as?', even though it wasn't necessary.
        result = new (tc.Context) InjectIntoOptionalExpr(
                                                     result,
                                                     OptionalType::get(toType));
        result->setImplicit(false);
        return cs.cacheType(result);
      }

      // Valid casts.
      case CheckedCastKind::ArrayDowncast:
      case CheckedCastKind::DictionaryDowncast:
      case CheckedCastKind::SetDowncast:
      case CheckedCastKind::ValueCast:
        expr->setCastKind(castKind);
        break;
      }
      
      return handleOptionalBindingsForCast(expr, simplifyType(cs.getType(expr)),
                                         OptionalBindingsCastKind::Conditional);
    }

    Expr *visitAssignExpr(AssignExpr *expr) {
      // Convert the source to the simplified destination type.
      auto destTy = simplifyType(cs.getType(expr->getDest()));
      auto locator =
        ConstraintLocatorBuilder(cs.getConstraintLocator(expr->getSrc()));
      Expr *src = coerceToType(expr->getSrc(), destTy->getRValueType(), locator);
      if (!src)
        return nullptr;

      expr->setSrc(src);

      if (!SuppressDiagnostics) {
        // If we're performing an assignment to a weak or unowned variable from
        // a constructor call, emit a warning that the instance will be
        // immediately deallocated.
        diagnoseUnownedImmediateDeallocation(cs.getTypeChecker(), expr);
      }
      return expr;
    }
    
    Expr *visitDiscardAssignmentExpr(DiscardAssignmentExpr *expr) {
      return simplifyExprType(expr);
    }
    
    Expr *visitUnresolvedPatternExpr(UnresolvedPatternExpr *expr) {
      // If we end up here, we should have diagnosed somewhere else
      // already.
      Expr *simplified = simplifyExprType(expr);
      if (!SuppressDiagnostics
          && !cs.getType(simplified)->is<UnresolvedType>()) {
        cs.TC.diagnose(simplified->getLoc(), diag::pattern_in_expr,
                       expr->getSubPattern()->getKind());
      }
      return simplified;
    }
    
    Expr *visitBindOptionalExpr(BindOptionalExpr *expr) {
      return simplifyExprType(expr);
    }

    Expr *visitOptionalEvaluationExpr(OptionalEvaluationExpr *expr) {
      Type optType = simplifyType(cs.getType(expr));

      // If this is an optional chain that isn't chaining anything, and if the
      // subexpression is already optional (not IUO), then this is a noop:
      // reject it.  This avoids confusion of the model (where the programmer
      // thought it was doing something) and keeps pointless ?'s out of the
      // code.
      if (!SuppressDiagnostics)
        if (auto *Bind = dyn_cast<BindOptionalExpr>(
                        expr->getSubExpr()->getSemanticsProvidingExpr())) {
          if (cs.getType(Bind->getSubExpr())->isEqual(optType))
            cs.TC.diagnose(expr->getLoc(), diag::optional_chain_noop,
                           optType).fixItRemove(Bind->getQuestionLoc());
          else
            cs.TC.diagnose(expr->getLoc(), diag::optional_chain_isnt_chaining);
        }

      Expr *subExpr = coerceToType(expr->getSubExpr(), optType,
                                   cs.getConstraintLocator(expr));
      if (!subExpr) return nullptr;

      expr->setSubExpr(subExpr);
      cs.setType(expr, optType);
      return expr;
    }

    Expr *visitForceValueExpr(ForceValueExpr *expr) {
      // Check to see if we are forcing an
      // ImplicitlyUnwrappedFunctionConversionExpr.  This can happen
      // in cases where we had a ForceValueExpr of an optional for a
      // declaration for a function whose result type we need to
      // implicitly force after applying. We need to hoist the function
      // conversion above the ForceValueExpr, so that we may ultimately
      // hoist it above the ApplyExpr where we will eventually rewrite the
      // function conversion into a force of the result.
      Expr *replacement = expr;
      if (auto fnConv =
          dyn_cast<ImplicitlyUnwrappedFunctionConversionExpr>(expr->getSubExpr())) {
        auto fnConvSubExpr = fnConv->getSubExpr();
        auto fnConvSubObjTy =
          cs.getType(fnConvSubExpr)->getOptionalObjectType();
        cs.setType(expr, fnConvSubObjTy);
        expr->setSubExpr(fnConvSubExpr);
        fnConv->setSubExpr(expr);
        replacement = fnConv;
      }

      Type valueType = simplifyType(cs.getType(expr));
      cs.setType(expr, valueType);

      // Coerce the object type, if necessary.
      auto subExpr = expr->getSubExpr();
      if (auto objectTy = cs.getType(subExpr)->getOptionalObjectType()) {
        if (objectTy && !objectTy->isEqual(valueType)) {
          auto coercedSubExpr = coerceToType(subExpr,
                                             OptionalType::get(valueType),
                                             cs.getConstraintLocator(subExpr));
          
          expr->setSubExpr(coercedSubExpr);
        }
      }
      
      return replacement;
    }

    Expr *visitOpenExistentialExpr(OpenExistentialExpr *expr) {
      llvm_unreachable("Already type-checked");
    }
    
    Expr *visitMakeTemporarilyEscapableExpr(MakeTemporarilyEscapableExpr *expr){
      llvm_unreachable("Already type-checked");
    }

    Expr *visitKeyPathApplicationExpr(KeyPathApplicationExpr *expr){
      // This should already be type-checked, but we may have had to re-
      // check it for failure diagnosis.
      return simplifyExprType(expr);
    }
    
    Expr *visitEnumIsCaseExpr(EnumIsCaseExpr *expr) {
      // Should already be type-checked.
      return simplifyExprType(expr);
    }

    Expr *visitLazyInitializerExpr(LazyInitializerExpr *expr) {
      simplifyExprType(expr);
      assert(expr->getType()->isEqual(expr->getSubExpr()->getType()));
      return expr;
    }
    
    Expr *visitEditorPlaceholderExpr(EditorPlaceholderExpr *E) {
      simplifyExprType(E);
      auto valueType = cs.getType(E);

      auto &tc = cs.getTypeChecker();
      auto &ctx = tc.Context;
      // Synthesize a call to _undefined() of appropriate type.
      FuncDecl *undefinedDecl = ctx.getUndefinedDecl(&tc);
      if (!undefinedDecl) {
        tc.diagnose(E->getLoc(), diag::missing_undefined_runtime);
        return nullptr;
      }
      DeclRefExpr *fnRef = new (ctx) DeclRefExpr(undefinedDecl, DeclNameLoc(),
                                                 /*Implicit=*/true);
      fnRef->setFunctionRefKind(FunctionRefKind::SingleApply);

      StringRef msg = "attempt to evaluate editor placeholder";
      Expr *argExpr = new (ctx) StringLiteralExpr(msg, E->getLoc(),
                                                  /*implicit*/true);

      Expr *callExpr = CallExpr::createImplicit(ctx, fnRef, { argExpr },
                                                { Identifier() });

      auto resultTy = tc.typeCheckExpression(
          callExpr, cs.DC, TypeLoc::withoutLoc(valueType), CTP_CannotFail);
      assert(resultTy && "Conversion cannot fail!");
      (void)resultTy;

      cs.cacheExprTypes(callExpr);
      E->setSemanticExpr(callExpr);
      return E;
    }

    Expr *visitObjCSelectorExpr(ObjCSelectorExpr *E) {
      // Dig out the reference to a declaration.
      Expr *subExpr = E->getSubExpr();
      ValueDecl *foundDecl = nullptr;
      while (subExpr) {
        // Declaration reference.
        if (auto declRef = dyn_cast<DeclRefExpr>(subExpr)) {
          foundDecl = declRef->getDecl();
          break;
        }

        // Constructor reference.
        if (auto ctorRef = dyn_cast<OtherConstructorDeclRefExpr>(subExpr)) {
          foundDecl = ctorRef->getDecl();
          break;
        }

        // Member reference.
        if (auto memberRef = dyn_cast<MemberRefExpr>(subExpr)) {
          foundDecl = memberRef->getMember().getDecl();
          break;
        }

        // Dynamic member reference.
        if (auto dynMemberRef = dyn_cast<DynamicMemberRefExpr>(subExpr)) {
          foundDecl = dynMemberRef->getMember().getDecl();
          break;
        }

        // Look through parentheses.
        if (auto paren = dyn_cast<ParenExpr>(subExpr)) {
          subExpr = paren->getSubExpr();
          continue;
        }

        // Look through "a.b" to "b".
        if (auto dotSyntax = dyn_cast<DotSyntaxBaseIgnoredExpr>(subExpr)) {
          subExpr = dotSyntax->getRHS();
          continue;
        }

        // Look through self-rebind expression.
        if (auto rebindSelf = dyn_cast<RebindSelfInConstructorExpr>(subExpr)) {
          subExpr = rebindSelf->getSubExpr();
          continue;
        }

        // Look through optional binding within the monadic "?".
        if (auto bind = dyn_cast<BindOptionalExpr>(subExpr)) {
          subExpr = bind->getSubExpr();
          continue;
        }

        // Look through optional evaluation of the monadic "?".
        if (auto optEval = dyn_cast<OptionalEvaluationExpr>(subExpr)) {
          subExpr = optEval->getSubExpr();
          continue;
        }

        // Look through an implicit force-value.
        if (auto force = dyn_cast<ForceValueExpr>(subExpr)) {
          subExpr = force->getSubExpr();
          continue;
        }

        // Look through implicit open-existential operations.
        if (auto open = dyn_cast<OpenExistentialExpr>(subExpr)) {
          if (open->isImplicit()) {
            subExpr = open->getSubExpr();
            continue;
          }
          break;
        }

        // Look to the referenced member in a self-application.
        if (auto selfApply = dyn_cast<SelfApplyExpr>(subExpr)) {
          subExpr = selfApply->getFn();
          continue;
        }

        // Look through implicit conversions.
        if (auto conversion = dyn_cast<ImplicitConversionExpr>(subExpr)) {
          subExpr = conversion->getSubExpr();
          continue;
        }

        // Look through explicit coercions.
        if (auto coercion = dyn_cast<CoerceExpr>(subExpr)) {
          subExpr = coercion->getSubExpr();
          continue;
        }

        break;
      }

      if (!subExpr) return nullptr;

      // If we didn't find any declaration at all, we're stuck.
      auto &tc = cs.getTypeChecker();
      if (!foundDecl) {
        tc.diagnose(E->getLoc(), diag::expr_selector_no_declaration)
          .highlight(subExpr->getSourceRange());
        return E;
      }

      // Check whether we found an entity that #selector could refer to.
      // If we found a method or initializer, check it.
      AbstractFunctionDecl *method = nullptr;
      if (auto func = dyn_cast<AbstractFunctionDecl>(foundDecl)) {
        // Methods and initializers.

        // If this isn't a method, complain.
        if (!func->getDeclContext()->isTypeContext()) {
          tc.diagnose(E->getLoc(), diag::expr_selector_not_method,
                      func->getDeclContext()->isModuleScopeContext(),
                      func->getFullName())
            .highlight(subExpr->getSourceRange());
          tc.diagnose(func, diag::decl_declared_here, func->getFullName());
          return E;
        }

        // Check that we requested a method.
        switch (E->getSelectorKind()) {
        case ObjCSelectorExpr::Method:
          break;

        case ObjCSelectorExpr::Getter:
        case ObjCSelectorExpr::Setter:
          // Complain that we cannot ask for the getter or setter of a
          // method.
          tc.diagnose(E->getModifierLoc(),
                      diag::expr_selector_expected_property,
                      E->getSelectorKind() == ObjCSelectorExpr::Setter,
                      foundDecl->getDescriptiveKind(),
                      foundDecl->getFullName())
            .fixItRemoveChars(E->getModifierLoc(),
                              E->getSubExpr()->getStartLoc());

          // Update the AST to reflect the fix.
          E->overrideObjCSelectorKind(ObjCSelectorExpr::Method, SourceLoc());
          break;
        }

        // Note the method we're referring to.
        method = func;
      } else if (auto var = dyn_cast<VarDecl>(foundDecl)) {
        // Properties.
        maybeAddAccessorsToStorage(tc, var);

        // If this isn't a property on a type, complain.
        if (!var->getDeclContext()->isTypeContext()) {
          tc.diagnose(E->getLoc(), diag::expr_selector_not_property,
                      isa<ParamDecl>(var), var->getFullName())
            .highlight(subExpr->getSourceRange());
          tc.diagnose(var, diag::decl_declared_here, var->getFullName());
          return E;
        }

        // Check that we requested a property getter or setter.
        switch (E->getSelectorKind()) {
        case ObjCSelectorExpr::Method: {
          bool isSettable = var->isSettable(cs.DC) &&
            var->isSetterAccessibleFrom(cs.DC);
          auto primaryDiag =
            tc.diagnose(E->getLoc(), diag::expr_selector_expected_method,
                        isSettable, var->getFullName());
          primaryDiag.highlight(subExpr->getSourceRange());

          // The point at which we will insert the modifier.
          SourceLoc modifierLoc = E->getSubExpr()->getStartLoc();

          // If the property is settable, we don't know whether the
          // user wanted the getter or setter. Provide notes for each.
          if (isSettable) {
            // Flush the primary diagnostic. We have notes to add.
            primaryDiag.flush();

            // Add notes for the getter and setter, respectively.
            tc.diagnose(modifierLoc, diag::expr_selector_add_modifier,
                        false, var->getFullName())
              .fixItInsert(modifierLoc, "getter: ");
            tc.diagnose(modifierLoc, diag::expr_selector_add_modifier,
                        true, var->getFullName())
              .fixItInsert(modifierLoc, "setter: ");

            // Bail out now. We don't know what the user wanted, so
            // don't fill in the details.
            return E;
          }

          // The property is non-settable, so add "getter:".
          primaryDiag.fixItInsert(modifierLoc, "getter: ");
          E->overrideObjCSelectorKind(ObjCSelectorExpr::Getter, modifierLoc);
          method = var->getGetter();
          break;
        }

        case ObjCSelectorExpr::Getter:
          method = var->getGetter();
          break;

        case ObjCSelectorExpr::Setter:
          // Make sure we actually have a setter.
          if (!var->isSettable(cs.DC)) {
            tc.diagnose(E->getLoc(), diag::expr_selector_property_not_settable,
                        var->getDescriptiveKind(), var->getFullName());
            tc.diagnose(var, diag::decl_declared_here, var->getFullName());
            return E;
          }

          // Make sure the setter is accessible.
          if (!var->isSetterAccessibleFrom(cs.DC)) {
            tc.diagnose(E->getLoc(),
                        diag::expr_selector_property_setter_inaccessible,
                        var->getDescriptiveKind(), var->getFullName());
            tc.diagnose(var, diag::decl_declared_here, var->getFullName());
            return E;
          }

          method = var->getSetter();
          break;
        }
      } else {
        // Cannot reference with #selector.
        tc.diagnose(E->getLoc(), diag::expr_selector_no_declaration)
          .highlight(subExpr->getSourceRange());
        tc.diagnose(foundDecl, diag::decl_declared_here,
                    foundDecl->getFullName());
        return E;
      }
      assert(method && "Didn't find a method?");

      // The declaration we found must be exposed to Objective-C.
      tc.validateDecl(method);
      if (!method->isObjC()) {
        tc.diagnose(E->getLoc(), diag::expr_selector_not_objc,
                    foundDecl->getDescriptiveKind(), foundDecl->getFullName())
          .highlight(subExpr->getSourceRange());
        tc.diagnose(foundDecl, diag::make_decl_objc,
                    foundDecl->getDescriptiveKind())
          .fixItInsert(foundDecl->getAttributeInsertionLoc(false),
                       "@objc ");
        return E;
      } else if (auto attr = foundDecl->getAttrs().getAttribute<ObjCAttr>()) {
        // If this attribute was inferred based on deprecated Swift 3 rules,
        // complain.
        if (attr->isSwift3Inferred() &&
            tc.Context.LangOpts.WarnSwift3ObjCInference
              == Swift3ObjCInferenceWarnings::Minimal) {
          tc.diagnose(E->getLoc(), diag::expr_selector_swift3_objc_inference,
                      foundDecl->getDescriptiveKind(), foundDecl->getFullName(),
                      foundDecl->getDeclContext()
                        ->getSelfNominalTypeDecl()
                        ->getName())
            .highlight(subExpr->getSourceRange());
          tc.diagnose(foundDecl, diag::make_decl_objc,
                      foundDecl->getDescriptiveKind())
            .fixItInsert(foundDecl->getAttributeInsertionLoc(false),
                         "@objc ");
        }
      }

      // Note which method we're referencing.
      E->setMethod(method);
      return E;
    }
    
  private:
    // Key path components we need to
    SmallVector<std::pair<KeyPathExpr *, unsigned>, 4>
      KeyPathSubscriptComponents;
  public:
    Expr *visitKeyPathExpr(KeyPathExpr *E) {
      if (E->isObjC()) {
        cs.setType(E, cs.getType(E->getObjCStringLiteralExpr()));
        return E;
      }

      simplifyExprType(E);
      
      if (cs.getType(E)->hasError())
        return E;

      // If a component is already resolved, then all of them should be
      // resolved, and we can let the expression be. This might happen when
      // re-checking a failed system for diagnostics.
      if (!E->getComponents().empty()
          && E->getComponents().front().isResolved()) {
        assert([&]{
          for (auto &c : E->getComponents())
            if (!c.isResolved())
              return false;
          return true;
        }());
        return E;
      }

      SmallVector<KeyPathExpr::Component, 4> resolvedComponents;

      // Resolve each of the components.
      bool didOptionalChain = false;
      auto keyPathTy = cs.getType(E)->castTo<BoundGenericType>();
      Type baseTy = keyPathTy->getGenericArgs()[0];
      Type leafTy = keyPathTy->getGenericArgs()[1];
      
      for (unsigned i : indices(E->getComponents())) {
        auto &origComponent = E->getMutableComponents()[i];
        
        // If there were unresolved types, we may end up with a null base for
        // following components.
        if (!baseTy) {
          resolvedComponents.push_back(origComponent);
          continue;
        }
        
        auto getObjectType = [](Type optionalTy) -> Type {
          Type objectTy;
          if (auto lvalue = optionalTy->getAs<LValueType>()) {
            objectTy = lvalue->getObjectType()->getOptionalObjectType();
            if (optionalTy->hasUnresolvedType() && !objectTy) {
              objectTy = optionalTy;
            }
            objectTy = LValueType::get(objectTy);
          } else {
            objectTy = optionalTy->getOptionalObjectType();
            if (optionalTy->hasUnresolvedType() && !objectTy) {
              objectTy = optionalTy;
            }
          }
          assert(objectTy);
          return objectTy;
        };

        auto kind = origComponent.getKind();
        Optional<SelectedOverload> foundDecl;

        auto locator =
          cs.getConstraintLocator(E,
                        ConstraintLocator::PathElement::getKeyPathComponent(i));
        if (kind == KeyPathExpr::Component::Kind::UnresolvedSubscript) {
          locator =
            cs.getConstraintLocator(locator,
                                    ConstraintLocator::SubscriptMember);
        }

        // If this is an unresolved link, make sure we resolved it.
        if (kind == KeyPathExpr::Component::Kind::UnresolvedProperty ||
            kind == KeyPathExpr::Component::Kind::UnresolvedSubscript) {
          foundDecl = solution.getOverloadChoiceIfAvailable(locator);
          // Leave the component unresolved if the overload was not resolved.
          if (foundDecl) {
            // If this was a @dynamicMemberLookup property, then we actually
            // form a subscript reference, so switch the kind.
            if (foundDecl->choice.getKind()
                    == OverloadChoiceKind::DynamicMemberLookup) {
              kind = KeyPathExpr::Component::Kind::UnresolvedSubscript;
            }
          }
        }

        KeyPathExpr::Component component;
        switch (kind) {
        case KeyPathExpr::Component::Kind::UnresolvedProperty: {
          // If we couldn't resolve the component, leave it alone.
          if (!foundDecl) {
            component = origComponent;
            break;
          }

          auto property = foundDecl->choice.getDecl();
          
          // Key paths can only refer to properties currently.
          if (!isa<VarDecl>(property)) {
            cs.TC.diagnose(origComponent.getLoc(),
                           diag::expr_keypath_not_property,
                           property->getDescriptiveKind(),
                           property->getFullName());
          } else {
            // Key paths don't work with mutating-get properties.
            auto varDecl = cast<VarDecl>(property);
            if (varDecl->isGetterMutating()) {
              cs.TC.diagnose(origComponent.getLoc(),
                             diag::expr_keypath_mutating_getter,
                             property->getFullName());
            }
            
            // Key paths don't currently support static members.
            if (varDecl->isStatic()) {
              cs.TC.diagnose(origComponent.getLoc(),
                             diag::expr_keypath_static_member,
                             property->getFullName());
            }
          }
          
          cs.TC.requestMemberLayout(property);

          auto dc = property->getInnermostDeclContext();

          // Compute substitutions to refer to the member.
          SubstitutionMap subs =
            solution.computeSubstitutions(dc->getGenericSignatureOfContext(),
                                          locator);

          auto resolvedTy = foundDecl->openedType;
          resolvedTy = simplifyType(resolvedTy);
          
          auto ref = ConcreteDeclRef(property, subs);

          component = KeyPathExpr::Component::forProperty(ref,
                                                       resolvedTy,
                                                       origComponent.getLoc());

          baseTy = component.getComponentType();
          resolvedComponents.push_back(component);

          if (shouldForceUnwrapResult(foundDecl->choice, locator)) {
            auto objectTy = getObjectType(baseTy);
            auto loc = origComponent.getLoc();
            component = KeyPathExpr::Component::forOptionalForce(objectTy, loc);
            baseTy = component.getComponentType();
            resolvedComponents.push_back(component);
          }
          break;
        }
        case KeyPathExpr::Component::Kind::UnresolvedSubscript: {
          // Leave the component unresolved if the overload was not resolved.
          if (!foundDecl) {
            component = origComponent;
            break;
          }
          
          auto subscript = cast<SubscriptDecl>(foundDecl->choice.getDecl());
          if (subscript->isGetterMutating()) {
            cs.TC.diagnose(origComponent.getLoc(),
                           diag::expr_keypath_mutating_getter,
                           subscript->getFullName());
          }

          cs.TC.requestMemberLayout(subscript);

          auto dc = subscript->getInnermostDeclContext();

          // FIXME: It's not correct to turn the subscript's parameter list
          // into a single type here. Further down we pass it to coerceToType(),
          // but the rules for argument list conversions are different than
          // tuple conversions, and coerceToType() doesn't handle varargs or
          // default arguments.
          auto indexType = AnyFunctionType::composeInput(
            cs.TC.Context,
            subscript->getInterfaceType()
              ->castTo<AnyFunctionType>()
              ->getParams(),
            /*canonicalVararg=*/false);

          SubstitutionMap subs;
          if (auto sig = dc->getGenericSignatureOfContext()) {
            // Compute substitutions to refer to the member.
            subs = solution.computeSubstitutions(sig, locator);
            indexType = indexType.subst(subs);
          }

          // If this is a @dynamicMemberLookup reference to resolve a property
          // through the subscript(dynamicMember:) member, restore the
          // openedType and origComponent to its full reference as if the user
          // wrote out the subscript manually.
          if (foundDecl->choice.getKind()
                                  == OverloadChoiceKind::DynamicMemberLookup) {
            foundDecl->openedType = foundDecl->openedFullType
                  ->castTo<AnyFunctionType>()->getResult();

            auto &ctx = cs.TC.Context;
            auto loc = origComponent.getLoc();
            auto fieldName =
                foundDecl->choice.getName().getBaseIdentifier().str();
            auto index = getDMLIndexExpr(fieldName, indexType, loc, dc, cs);
            
            origComponent = KeyPathExpr::Component::
              forUnresolvedSubscript(ctx, loc, index, {}, loc, loc,
                                     /*trailingClosure*/nullptr);
            cs.setType(origComponent.getIndexExpr(), index->getType());
          }
          
          auto resolvedTy = foundDecl->openedType->castTo<AnyFunctionType>()
            ->getResult();
          
          resolvedTy = simplifyType(resolvedTy);
          
          auto ref = ConcreteDeclRef(subscript, subs);
          
          // Coerce the indices to the type the subscript expects.
          auto indexExpr = coerceToType(origComponent.getIndexExpr(),
                                        indexType,
                                        locator);
          
          component = KeyPathExpr::Component
            ::forSubscriptWithPrebuiltIndexExpr(ref, indexExpr,
                                            origComponent.getSubscriptLabels(),
                                            resolvedTy,
                                            origComponent.getLoc(),
                                            {});
          // Save a reference to the component so we can do a post-pass to check
          // the Hashable conformance of the indexes.
          KeyPathSubscriptComponents.push_back({E, resolvedComponents.size()});

          baseTy = component.getComponentType();
          resolvedComponents.push_back(component);

          if (shouldForceUnwrapResult(foundDecl->choice, locator)) {
            auto objectTy = getObjectType(baseTy);
            auto loc = origComponent.getLoc();
            component = KeyPathExpr::Component::forOptionalForce(objectTy, loc);
            baseTy = component.getComponentType();
            resolvedComponents.push_back(component);
          }
          break;
        }
        case KeyPathExpr::Component::Kind::OptionalChain: {
          didOptionalChain = true;
          // Chaining always forces the element to be an rvalue.
          auto objectTy =
              baseTy->getWithoutSpecifierType()->getOptionalObjectType();
          if (baseTy->hasUnresolvedType() && !objectTy) {
            objectTy = baseTy;
          }
          assert(objectTy);
          
          auto loc = origComponent.getLoc();
          component = KeyPathExpr::Component::forOptionalChain(objectTy, loc);

          baseTy = component.getComponentType();
          resolvedComponents.push_back(component);
          break;
        }
        case KeyPathExpr::Component::Kind::OptionalForce: {
          auto objectTy = getObjectType(baseTy);
          auto loc = origComponent.getLoc();
          component = KeyPathExpr::Component::forOptionalForce(objectTy, loc);
          baseTy = component.getComponentType();
          resolvedComponents.push_back(component);
          break;
        }
        case KeyPathExpr::Component::Kind::Invalid:
          component = origComponent;
          component.setComponentType(leafTy);

          baseTy = component.getComponentType();
          resolvedComponents.push_back(component);
          break;
        case KeyPathExpr::Component::Kind::Identity:
          component = origComponent;
          component.setComponentType(baseTy);
          resolvedComponents.push_back(component);
          break;
        case KeyPathExpr::Component::Kind::Property:
        case KeyPathExpr::Component::Kind::Subscript:
        case KeyPathExpr::Component::Kind::OptionalWrap:
          llvm_unreachable("already resolved");
        }
      }
      
      // Wrap a non-optional result if there was chaining involved.
      if (didOptionalChain &&
          baseTy &&
          !baseTy->hasUnresolvedType() &&
          !baseTy->getWithoutSpecifierType()->isEqual(leafTy)) {
        assert(leafTy->getOptionalObjectType()->isEqual(
            baseTy->getWithoutSpecifierType()));
        auto component = KeyPathExpr::Component::forOptionalWrap(leafTy);
        resolvedComponents.push_back(component);
        baseTy = leafTy;
      }
      E->resolveComponents(cs.getASTContext(), resolvedComponents);
      
      // See whether there's an equivalent ObjC key path string we can produce
      // for interop purposes.
      if (cs.getASTContext().LangOpts.EnableObjCInterop) {
        SmallString<64> compatStringBuf;
        if (buildObjCKeyPathString(E, compatStringBuf)) {
          auto stringCopy =
            cs.getASTContext().AllocateCopy<char>(compatStringBuf.begin(),
                                                  compatStringBuf.end());
          auto stringExpr = new (cs.getASTContext()) StringLiteralExpr(
                                 StringRef(stringCopy, compatStringBuf.size()),
                                 SourceRange(),
                                 /*implicit*/ true);
          cs.setType(stringExpr, cs.getType(E));
          E->setObjCStringLiteralExpr(stringExpr);
        }
      }
      
      // The final component type ought to line up with the leaf type of the
      // key path.
      assert(!baseTy || baseTy->hasUnresolvedType()
             || baseTy->getWithoutSpecifierType()->isEqual(leafTy));
      return E;
    }

    Expr *visitKeyPathDotExpr(KeyPathDotExpr *E) {
      llvm_unreachable("found KeyPathDotExpr in CSApply");
    }

    // SWIFT_ENABLE_TENSORFLOW
    Expr *visitPoundAssertExpr(PoundAssertExpr *E) {
      // Convert the condition to a logic value.
      auto condition = solution.convertBooleanTypeToBuiltinI1(
          E->getCondition(), cs.getConstraintLocator(E));
      // TODO(marcrasi): Once we pull in commit 754e8e27, `condition` can no
      // longer be null so we don't need this check.
      if (!condition) {
        cs.setType(E->getCondition(), ErrorType::get(cs.getType(E)));
      } else {
        E->setCondition(condition);
      }

      // CSGen already set this expression's type to Void.
      return E;
    }

    /// Interface for ExprWalker
    void walkToExprPre(Expr *expr) {
      ExprStack.push_back(expr);
    }

    Expr *walkToExprPost(Expr *expr) {
      Expr *result = visit(expr);

      // Mark any _ObjectiveCBridgeable conformances as 'used'.
      if (result) {
        useObjectiveCBridgeableConformances(cs.DC, cs.getType(result));
      }

      assert(expr == ExprStack.back());
      ExprStack.pop_back();

      return result;
    }

    void finalize(Expr *&result) {
      assert(ExprStack.empty());
      assert(OpenedExistentials.empty());

      auto &tc = cs.getTypeChecker();

      // Look at all of the suspicious optional injections
      for (auto injection : SuspiciousOptionalInjections) {
        // If we already diagnosed this injection, we're done.
        if (DiagnosedOptionalInjections.count(injection)) {
          continue;
        }

        auto *cast = findForcedDowncast(tc.Context, injection->getSubExpr());
        if (!cast)
          continue;

        if (isa<ParenExpr>(injection->getSubExpr()))
          continue;

        tc.diagnose(injection->getLoc(), diag::inject_forced_downcast,
                    cs.getType(injection->getSubExpr())->getRValueType());
        auto exclaimLoc = cast->getExclaimLoc();
        tc.diagnose(exclaimLoc, diag::forced_to_conditional_downcast,
                    cs.getType(injection)->getOptionalObjectType())
            .fixItReplace(exclaimLoc, "?");
        tc.diagnose(cast->getStartLoc(), diag::silence_inject_forced_downcast)
          .fixItInsert(cast->getStartLoc(), "(")
          .fixItInsertAfter(cast->getEndLoc(), ")");
      }
      
      // Look at key path subscript components to verify that they're hashable.
      for (auto componentRef : KeyPathSubscriptComponents) {
        auto &component = componentRef.first
                                  ->getMutableComponents()[componentRef.second];
        // We need to be able to hash the captured index values in order for
        // KeyPath itself to be hashable, so check that all of the subscript
        // index components are hashable and collect their conformances here.
        SmallVector<ProtocolConformanceRef, 2> hashables;
        bool allIndexesHashable = true;
        ArrayRef<TupleTypeElt> indexTypes;
        TupleTypeElt singleIndexTypeBuf;
        if (auto tup = cs.getType(component.getIndexExpr())
                                               ->getAs<TupleType>()) {
          indexTypes = tup->getElements();
        } else {
          singleIndexTypeBuf = cs.getType(component.getIndexExpr());
          indexTypes = singleIndexTypeBuf;
        }
      
        auto hashable =
          cs.getASTContext().getProtocol(KnownProtocolKind::Hashable);
        auto equatable =
          cs.getASTContext().getProtocol(KnownProtocolKind::Equatable);
        for (auto indexType : indexTypes) {
          auto conformance =
            cs.TC.conformsToProtocol(indexType.getType(), hashable,
                                     cs.DC,
                                     (ConformanceCheckFlags::Used|
                                      ConformanceCheckFlags::InExpression));
          if (!conformance) {
            cs.TC.diagnose(component.getIndexExpr()->getLoc(),
                           diag::expr_keypath_subscript_index_not_hashable,
                           indexType.getType());
            allIndexesHashable = false;
            continue;
          }
          hashables.push_back(*conformance);
          
          // FIXME: Hashable implies Equatable, but we need to make sure the
          // Equatable conformance is forced into existence during type checking
          // so that it's available for SILGen.
          auto eqConformance =
            cs.TC.conformsToProtocol(indexType.getType(), equatable,
                                     cs.DC,
                                     (ConformanceCheckFlags::Used|
                                      ConformanceCheckFlags::InExpression));
          assert(eqConformance.hasValue());
          (void)eqConformance;
        }

        if (allIndexesHashable) {
          component.setSubscriptIndexHashableConformances(hashables);
        }
      }

      // Set the final types on the expression.
      cs.setExprTypes(result);
    }

    /// Diagnose an optional injection that is probably not what the
    /// user wanted, because it comes from a forced downcast.
    void diagnoseOptionalInjection(InjectIntoOptionalExpr *injection) {
      // Check whether we have a forced downcast.
      auto &tc = cs.getTypeChecker();
      auto *cast = findForcedDowncast(tc.Context, injection->getSubExpr());
      if (!cast)
        return;
      
      SuspiciousOptionalInjections.push_back(injection);
    }
  };
} // end anonymous namespace


/// Resolve a locator to the specific declaration it references, if possible.
///
/// \param cs The constraint system in which the locator will be resolved.
///
/// \param locator The locator to resolve.
///
/// \param findOvlChoice A function that searches for the overload choice
/// associated with the given locator, or an empty optional if there is no such
/// overload.
///
/// \returns the decl to which the locator resolved.
///
static ConcreteDeclRef resolveLocatorToDecl(
    ConstraintSystem &cs, ConstraintLocator *locator,
    llvm::function_ref<Optional<SelectedOverload>(ConstraintLocator *)>
        findOvlChoice,
    llvm::function_ref<ConcreteDeclRef(ValueDecl *decl, Type openedType,
                                       ConstraintLocator *declLocator)>
        getConcreteDeclRef) {
  assert(locator && "Null locator");
  if (!locator->getAnchor())
    return ConcreteDeclRef();

  auto anchor = locator->getAnchor();
  // Unwrap any specializations, constructor calls, implicit conversions, and
  // '.'s.
  // FIXME: This is brittle.
  do {
    if (auto specialize = dyn_cast<UnresolvedSpecializeExpr>(anchor)) {
      anchor = specialize->getSubExpr();
      continue;
    }

    if (auto implicit = dyn_cast<ImplicitConversionExpr>(anchor)) {
      anchor = implicit->getSubExpr();
      continue;
    }

    if (auto identity = dyn_cast<IdentityExpr>(anchor)) {
      anchor = identity->getSubExpr();
      continue;
    }

    if (auto tryExpr = dyn_cast<AnyTryExpr>(anchor)) {
      if (isa<OptionalTryExpr>(tryExpr))
        break;

      anchor = tryExpr->getSubExpr();
      continue;
    }

    if (auto selfApply = dyn_cast<SelfApplyExpr>(anchor)) {
      anchor = selfApply->getFn();
      continue;
    }

    if (auto dotSyntax = dyn_cast<DotSyntaxBaseIgnoredExpr>(anchor)) {
      anchor = dotSyntax->getRHS();
      continue;
    }

    if (auto *OEE = dyn_cast<OpenExistentialExpr>(anchor)) {
      anchor = OEE->getSubExpr();
      continue;
    }

    break;
  } while (true);
  
  // Simple case: direct reference to a declaration.
  if (auto dre = dyn_cast<DeclRefExpr>(anchor))
    return dre->getDeclRef();
    
  // Simple case: direct reference to a declaration.
  if (auto mre = dyn_cast<MemberRefExpr>(anchor))
    return mre->getMember();
  
  if (auto ctorRef = dyn_cast<OtherConstructorDeclRefExpr>(anchor))
    return ctorRef->getDeclRef();

  if (isa<OverloadedDeclRefExpr>(anchor) ||
      isa<UnresolvedDeclRefExpr>(anchor)) {
    // Overloaded and unresolved cases: find the resolved overload.
    auto anchorLocator = cs.getConstraintLocator(anchor);
    if (auto selected = findOvlChoice(anchorLocator)) {
      if (selected->choice.isDecl())
        return getConcreteDeclRef(selected->choice.getDecl(),
                                  selected->openedType,
                                  anchorLocator);
    }
  }
  
  if (isa<UnresolvedMemberExpr>(anchor)) {
    // Unresolved member: find the resolved overload.
    auto anchorLocator = cs.getConstraintLocator(anchor,
                           ConstraintLocator::UnresolvedMember);
    if (auto selected = findOvlChoice(anchorLocator)) {
      if (selected->choice.isDecl())
        return getConcreteDeclRef(selected->choice.getDecl(),
                                  selected->openedType,
                                  anchorLocator);
    }
  }

  if (isa<UnresolvedDotExpr>(anchor)) {
    // Unresolved member: find the resolved overload.
    auto anchorLocator = cs.getConstraintLocator(anchor,
                                                 ConstraintLocator::Member);
    if (auto selected = findOvlChoice(anchorLocator)) {
      if (selected->choice.isDecl())
        return getConcreteDeclRef(selected->choice.getDecl(),
                                  selected->openedType,
                                  anchorLocator);
    }
  }

  if (auto subscript = dyn_cast<SubscriptExpr>(anchor)) {
    // Subscript expressions may have a declaration. If so, use it.
    if (subscript->hasDecl())
      return subscript->getDecl();

    // Otherwise, find the resolved overload.
    auto anchorLocator =
      cs.getConstraintLocator(anchor, ConstraintLocator::SubscriptMember);
    if (auto selected = findOvlChoice(anchorLocator)) {
      if (selected->choice.isDecl())
        return getConcreteDeclRef(selected->choice.getDecl(),
                                  selected->openedType,
                                  anchorLocator);
    }
  }

  if (auto subscript = dyn_cast<DynamicSubscriptExpr>(anchor)) {
    // Dynamic subscripts are always resolved.
    return subscript->getMember();
  }

  return ConcreteDeclRef();
}

ConcreteDeclRef Solution::resolveLocatorToDecl(
                  ConstraintLocator *locator) const {
  auto &cs = getConstraintSystem();

  // Simplify the locator.
  SourceRange range;
  locator = simplifyLocator(cs, locator, range);

  // If we didn't map down to a specific expression, we can't handle a default
  // argument.
  if (!locator->getAnchor() || !locator->getPath().empty())
    return nullptr;

  if (auto resolved
        = ::resolveLocatorToDecl(cs, locator,
            [&](ConstraintLocator *locator) -> Optional<SelectedOverload> {
              auto known = overloadChoices.find(locator);
              if (known == overloadChoices.end()) {
                return None;
              }

              return known->second;
            },
            [&](ValueDecl *decl, Type openedType, ConstraintLocator *locator)
                  -> ConcreteDeclRef {
              SubstitutionMap subs =
                computeSubstitutions(
                  decl->getInnermostDeclContext()
                      ->getGenericSignatureOfContext(),
                locator);
              return ConcreteDeclRef(decl, subs);
            })) {
    return resolved;
  }

  return ConcreteDeclRef();
}

/// \brief Given a constraint locator, find the declaration reference
/// to the callee, it is a call to a declaration.
static ConcreteDeclRef
findCalleeDeclRef(ConstraintSystem &cs, const Solution &solution,
                  ConstraintLocator *locator) {
  if (locator->getPath().empty() || !locator->getAnchor())
    return nullptr;

  // If the locator points to a function application, find the function itself.
  if (locator->getPath().back().getKind() == ConstraintLocator::ApplyArgument) {
    assert(locator->getPath().back().getNewSummaryFlags() == 0 &&
           "ApplyArgument adds no flags");
    SmallVector<LocatorPathElt, 4> newPath;
    newPath.append(locator->getPath().begin(), locator->getPath().end()-1);

    unsigned newFlags = locator->getSummaryFlags();
    newPath.push_back(ConstraintLocator::ApplyFunction);

    assert(newPath.back().getNewSummaryFlags() == 0 &&
           "added element that changes the flags?");
    locator = cs.getConstraintLocator(locator->getAnchor(), newPath, newFlags);
  }

  return solution.resolveLocatorToDecl(locator);
}

static bool
shouldApplyAddingLabelFixit(TuplePattern *tuplePattern, TupleType *fromTuple,
                            TupleType *toTuple,
                std::vector<std::pair<SourceLoc, std::string>> &locInsertPairs) {
  std::vector<TuplePattern*> patternParts;
  std::vector<TupleType*> fromParts;
  std::vector<TupleType*> toParts;
  patternParts.push_back(tuplePattern);
  fromParts.push_back(fromTuple);
  toParts.push_back(toTuple);
  while (!patternParts.empty()) {
    TuplePattern *curPattern = patternParts.back();
    TupleType *curFrom = fromParts.back();
    TupleType *curTo = toParts.back();
    patternParts.pop_back();
    fromParts.pop_back();
    toParts.pop_back();
    unsigned n = curPattern->getElements().size();
    if (curFrom->getElements().size() != n ||
        curTo->getElements().size() != n)
      return false;
    for (unsigned i = 0; i < n; i++) {
      Pattern* subPat = curPattern->getElement(i).getPattern();
      const TupleTypeElt &subFrom = curFrom->getElement(i);
      const TupleTypeElt &subTo = curTo->getElement(i);
      if ((subFrom.getType()->getKind() == TypeKind::Tuple) ^
          (subTo.getType()->getKind() == TypeKind::Tuple))
        return false;
      auto addLabelFunc = [&]() {
        if (subFrom.getName().empty() && !subTo.getName().empty()) {
          llvm::SmallString<8> Name;
          Name.append(subTo.getName().str());
          Name.append(": ");
          locInsertPairs.push_back({subPat->getStartLoc(), Name.str()});
        }
      };
      if (auto subFromTuple = subFrom.getType()->getAs<TupleType>()) {
        fromParts.push_back(subFromTuple);
        toParts.push_back(subTo.getType()->getAs<TupleType>());
        patternParts.push_back(static_cast<TuplePattern*>(subPat));
        addLabelFunc();
      } else if (subFrom.getType()->isEqual(subTo.getType())) {
        addLabelFunc();
      } else
        return false;
    }
  }
  return true;
}

/// Produce the caller-side default argument for this default argument, or
/// null if the default argument will be provided by the callee.
static std::pair<Expr *, DefaultArgumentKind>
getCallerDefaultArg(ConstraintSystem &cs, DeclContext *dc,
                    SourceLoc loc, ConcreteDeclRef &owner,
                    unsigned index) {
  auto &tc = cs.getTypeChecker();

  auto defArg = getDefaultArgumentInfo(cast<ValueDecl>(owner.getDecl()), index);
  Expr *init = nullptr;
  switch (defArg.first) {
  case DefaultArgumentKind::None:
    llvm_unreachable("No default argument here?");

  case DefaultArgumentKind::Normal:
    return {nullptr, defArg.first};

  case DefaultArgumentKind::Inherited:
    // Update the owner to reflect inheritance here.
    owner = owner.getOverriddenDecl();
    return getCallerDefaultArg(cs, dc, loc, owner, index);

  case DefaultArgumentKind::Column:
    init = new (tc.Context) MagicIdentifierLiteralExpr(
                              MagicIdentifierLiteralExpr::Column, loc,
                              /*implicit=*/true);
    break;

  case DefaultArgumentKind::File:
    init = new (tc.Context) MagicIdentifierLiteralExpr(
                              MagicIdentifierLiteralExpr::File, loc,
                              /*implicit=*/true);
    break;
    
  case DefaultArgumentKind::Line:
    init = new (tc.Context) MagicIdentifierLiteralExpr(
                              MagicIdentifierLiteralExpr::Line, loc,
                              /*implicit=*/true);
    break;
      
  case DefaultArgumentKind::Function:
    init = new (tc.Context) MagicIdentifierLiteralExpr(
                              MagicIdentifierLiteralExpr::Function, loc,
                              /*implicit=*/true);
    break;

  case DefaultArgumentKind::DSOHandle:
    init = new (tc.Context) MagicIdentifierLiteralExpr(
                              MagicIdentifierLiteralExpr::DSOHandle, loc,
                              /*implicit=*/true);
    break;

  case DefaultArgumentKind::NilLiteral:
    init = new (tc.Context) NilLiteralExpr(loc, /*Implicit=*/true);
    break;

  case DefaultArgumentKind::EmptyArray:
    init = ArrayExpr::create(tc.Context, loc, {}, {}, loc);
    init->setImplicit();
    break;

  case DefaultArgumentKind::EmptyDictionary:
    init = DictionaryExpr::create(tc.Context, loc, {}, {}, loc);
    init->setImplicit();
    break;
  }

  // Convert the literal to the appropriate type.
  auto defArgType = owner.getDecl()->getDeclContext()
                       ->mapTypeIntoContext(defArg.second);
  auto resultTy = tc.typeCheckExpression(
      init, dc, TypeLoc::withoutLoc(defArgType), CTP_CannotFail);
  assert(resultTy && "Conversion cannot fail");
  (void)resultTy;

  cs.cacheExprTypes(init);

  return {init, defArg.first};
}

static Expr *lookThroughIdentityExprs(Expr *expr) {
  while (true) {
    if (auto ident = dyn_cast<IdentityExpr>(expr)) {
      expr = ident->getSubExpr();
    } else if (auto anyTry = dyn_cast<AnyTryExpr>(expr)) {
      if (isa<OptionalTryExpr>(anyTry))
        return expr;
      expr = anyTry->getSubExpr();
    } else {
      return expr;
    }
  }
}

/// Rebuild the ParenTypes for the given expression, whose underlying expression
/// should be set to the given type.  This has to apply to exactly the same
/// levels of sugar that were stripped off by lookThroughIdentityExprs.
static Type rebuildIdentityExprs(ConstraintSystem &cs, Expr *expr, Type type) {
  ASTContext &ctx = cs.getASTContext();
  if (auto paren = dyn_cast<ParenExpr>(expr)) {
    type = rebuildIdentityExprs(cs, paren->getSubExpr(), type);
    cs.setType(paren, ParenType::get(ctx, type->getInOutObjectType(),
                                     ParameterTypeFlags().withInOut(type->is<InOutType>())));
    return cs.getType(paren);
  }

  if (auto ident = dyn_cast<IdentityExpr>(expr)) {
    type = rebuildIdentityExprs(cs, ident->getSubExpr(), type);
    cs.setType(ident, type);
    return cs.getType(ident);
  }

  if (auto ident = dyn_cast<AnyTryExpr>(expr)) {
    if (isa<OptionalTryExpr>(ident))
      return type;

    type = rebuildIdentityExprs(cs, ident->getSubExpr(), type);
    cs.setType(ident, type);
    return cs.getType(ident);
  }

  return type;
}

Expr *ExprRewriter::coerceTupleToTuple(Expr *expr, TupleType *fromTuple,
                                       TupleType *toTuple,
                                       ConstraintLocatorBuilder locator,
                                       SmallVectorImpl<int> &sources,
                                       SmallVectorImpl<unsigned> &variadicArgs,
                                       Optional<Pattern*> typeFromPattern){
  auto &tc = cs.getTypeChecker();

  // Capture the tuple expression, if there is one.
  Expr *innerExpr = lookThroughIdentityExprs(expr);
  auto *fromTupleExpr = dyn_cast<TupleExpr>(innerExpr);

  /// Check each of the tuple elements in the destination.
  bool anythingShuffled = false;
  SmallVector<TupleTypeElt, 4> toSugarFields;
  SmallVector<TupleTypeElt, 4> fromTupleExprFields(
                                 fromTuple->getElements().size());

  for (unsigned i = 0, n = toTuple->getNumElements(); i != n; ++i) {
    assert(sources[i] != TupleShuffleExpr::DefaultInitialize &&
           sources[i] != TupleShuffleExpr::Variadic);

    const auto &toElt = toTuple->getElement(i);
    auto toEltType = toElt.getType();

    // If the source and destination index are different, we'll be shuffling.
    if ((unsigned)sources[i] != i) {
      anythingShuffled = true;
    }

    // We're matching one element to another. If the types already
    // match, there's nothing to do.
    const auto &fromElt = fromTuple->getElement(sources[i]);
    auto fromEltType = fromElt.getType();
    if (fromEltType->isEqual(toEltType)) {
      // Get the sugared type directly from the tuple expression, if there
      // is one.
      if (fromTupleExpr)
        fromEltType = cs.getType(fromTupleExpr->getElement(sources[i]));

      toSugarFields.push_back(toElt.getWithType(fromEltType));
      fromTupleExprFields[sources[i]] = fromElt;
      continue;
    }

    // We need to convert the source element to the destination type.
    if (!fromTupleExpr) {
      // FIXME: Lame! We can't express this in the AST.
      auto anchorExpr = locator.getBaseLocator()->getAnchor();
      InFlightDiagnostic diag = tc.diagnose(anchorExpr->getLoc(),
                                        diag::tuple_conversion_not_expressible,
                                            fromTuple, toTuple);
      if (typeFromPattern) {
        std::vector<std::pair<SourceLoc, std::string>> locInsertPairs;
        auto *tupleP = dyn_cast<TuplePattern>(typeFromPattern.getValue());
        if (tupleP && shouldApplyAddingLabelFixit(tupleP, toTuple, fromTuple,
                                                  locInsertPairs)) {
          for (auto &Pair : locInsertPairs) {
            diag.fixItInsert(Pair.first, Pair.second);
          }
        }
      }
      return nullptr;
    }

    // Actually convert the source element.
    auto convertedElt
      = coerceToType(fromTupleExpr->getElement(sources[i]), toEltType,
                     locator.withPathElement(
                       LocatorPathElt::getTupleElement(sources[i])));
    if (!convertedElt)
      return nullptr;

    fromTupleExpr->setElement(sources[i], convertedElt);

    // Record the sugared field name.
    toSugarFields.push_back(toElt.getWithType(cs.getType(convertedElt)));
    fromTupleExprFields[sources[i]] =
      fromElt.getWithType(cs.getType(convertedElt));
  }

  // Compute the updated 'from' tuple type, since we may have
  // performed some conversions in place.
  Type fromTupleType = TupleType::get(fromTupleExprFields, tc.Context);
  if (fromTupleExpr) {
    cs.setType(fromTupleExpr, fromTupleType);

    // Update the types of parentheses around the tuple expression.
    rebuildIdentityExprs(cs, expr, fromTupleType);
  }

  // Compute the re-sugared tuple type.
  Type toSugarType = TupleType::get(toSugarFields, tc.Context);

  // If we don't have to shuffle anything, we're done.
  if (!anythingShuffled && fromTupleExpr) {
    cs.setType(fromTupleExpr, toSugarType);

    // Update the types of parentheses around the tuple expression.
    rebuildIdentityExprs(cs, expr, toSugarType);

    return expr;
  }
  
  // Create the tuple shuffle.
  return
    cs.cacheType(TupleShuffleExpr::create(tc.Context,
                                          expr, sources,
                                          TupleShuffleExpr::TupleToTuple,
                                          ConcreteDeclRef(), {}, Type(), {},
                                          toSugarType));
}

static Type getMetatypeSuperclass(Type t, TypeChecker &tc) {
  if (auto *metaTy = t->getAs<MetatypeType>())
    return MetatypeType::get(getMetatypeSuperclass(
                               metaTy->getInstanceType(),
                               tc));

  if (auto *metaTy = t->getAs<ExistentialMetatypeType>())
    return ExistentialMetatypeType::get(getMetatypeSuperclass(
                                          metaTy->getInstanceType(),
                                          tc));

  return t->getSuperclass();
}

Expr *ExprRewriter::coerceSuperclass(Expr *expr, Type toType,
                                     ConstraintLocatorBuilder locator) {
  auto &tc = cs.getTypeChecker();

  auto fromType = cs.getType(expr);

  auto fromInstanceType = fromType;
  auto toInstanceType = toType;

  while (fromInstanceType->is<AnyMetatypeType>() &&
         toInstanceType->is<MetatypeType>()) {
    fromInstanceType = fromInstanceType->castTo<AnyMetatypeType>()
      ->getInstanceType();
    toInstanceType = toInstanceType->castTo<MetatypeType>()
      ->getInstanceType();
  }

  if (fromInstanceType->is<ArchetypeType>()) {
    // Coercion from archetype to its (concrete) superclass.
    auto superclass = getMetatypeSuperclass(fromType, tc);

    expr =
      cs.cacheType(
        new (tc.Context) ArchetypeToSuperExpr(expr, superclass));

    if (!superclass->isEqual(toType))
      return coerceSuperclass(expr, toType, locator);

    return expr;

  }

  if (fromInstanceType->isExistentialType()) {
    // Coercion from superclass-constrained existential to its
    // concrete superclass.
    auto fromArchetype = ArchetypeType::getAnyOpened(fromType);

    auto *archetypeVal =
      cs.cacheType(
        new (tc.Context) OpaqueValueExpr(expr->getLoc(),
                                         fromArchetype));

    auto *result = coerceSuperclass(archetypeVal, toType, locator);

    return cs.cacheType(
      new (tc.Context) OpenExistentialExpr(expr, archetypeVal, result,
                                           toType));
  }

  // Coercion from subclass to superclass.
  if (toType->is<MetatypeType>()) {
    return cs.cacheType(
      new (tc.Context) MetatypeConversionExpr(expr, toType));
  }

  return cs.cacheType(
    new (tc.Context) DerivedToBaseExpr(expr, toType));
}

/// Collect the conformances for all the protocols of an existential type.
/// If the source type is also existential, we don't want to check conformance
/// because most protocols do not conform to themselves -- however we still
/// allow the conversion here, except the ErasureExpr ends up with trivial
/// conformances.
static ArrayRef<ProtocolConformanceRef>
collectExistentialConformances(TypeChecker &tc, Type fromType, Type toType,
                               DeclContext *DC) {
  auto layout = toType->getExistentialLayout();

  SmallVector<ProtocolConformanceRef, 4> conformances;
  for (auto proto : layout.getProtocols()) {
    conformances.push_back(
      *tc.containsProtocol(fromType, proto->getDecl(), DC,
                           (ConformanceCheckFlags::InExpression|
                            ConformanceCheckFlags::Used)));
  }

  return tc.Context.AllocateCopy(conformances);
}

Expr *ExprRewriter::coerceExistential(Expr *expr, Type toType,
                                      ConstraintLocatorBuilder locator) {
  auto &tc = solution.getConstraintSystem().getTypeChecker();
  Type fromType = cs.getType(expr);
  Type fromInstanceType = fromType;
  Type toInstanceType = toType;

  // Look through metatypes
  while ((fromInstanceType->is<UnresolvedType>() ||
          fromInstanceType->is<AnyMetatypeType>()) &&
         toInstanceType->is<ExistentialMetatypeType>()) {
    if (!fromInstanceType->is<UnresolvedType>())
      fromInstanceType = fromInstanceType->castTo<AnyMetatypeType>()->getInstanceType();
    toInstanceType = toInstanceType->castTo<ExistentialMetatypeType>()->getInstanceType();
  }

  ASTContext &ctx = tc.Context;

  auto conformances =
    collectExistentialConformances(tc, fromInstanceType, toInstanceType, cs.DC);

  // For existential-to-existential coercions, open the source existential.
  if (fromType->isAnyExistentialType()) {
    fromType = ArchetypeType::getAnyOpened(fromType);
    
    auto *archetypeVal =
      cs.cacheType(
          new (ctx) OpaqueValueExpr(expr->getLoc(),
                                    fromType));
    
    auto *result = cs.cacheType(ErasureExpr::create(ctx, archetypeVal, toType,
                                                    conformances));
    return cs.cacheType(
        new (ctx) OpenExistentialExpr(expr, archetypeVal, result,
                                      cs.getType(result)));
  }

  // Load tuples with lvalue elements.
  if (auto tupleType = fromType->getAs<TupleType>()) {
    if (tupleType->hasLValueType()) {
      auto toTuple = tupleType->getRValueType()->castTo<TupleType>();
      SmallVector<int, 4> sources;
      SmallVector<unsigned, 4> variadicArgs;
      bool failed = computeTupleShuffle(tupleType, toTuple,
                                        sources, variadicArgs);
      assert(!failed && "Couldn't convert tuple to tuple?");
      (void)failed;

      coerceTupleToTuple(expr, tupleType, toTuple, locator, sources,
                         variadicArgs);
    }
  }

  return cs.cacheType(ErasureExpr::create(ctx, expr, toType, conformances));
}

/// Given that the given expression is an implicit conversion added
/// to the target by coerceToType, find out how many OptionalEvaluationExprs
/// it includes and the target.
static unsigned getOptionalEvaluationDepth(Expr *expr, Expr *target) {
  unsigned depth = 0;
  while (true) {
    // Look through sugar expressions.
    expr = expr->getSemanticsProvidingExpr();

    // If we find the target expression, we're done.
    if (expr == target) return depth;

    // If we see an optional evaluation, the depth goes up.
    if (auto optEval = dyn_cast<OptionalEvaluationExpr>(expr)) {
      depth++;
      expr = optEval->getSubExpr();

    // We have to handle any other expressions that can be introduced by
    // coerceToType.
    } else if (auto bind = dyn_cast<BindOptionalExpr>(expr)) {
      expr = bind->getSubExpr();
    } else if (auto force = dyn_cast<ForceValueExpr>(expr)) {
      expr = force->getSubExpr();
    } else if (auto open = dyn_cast<OpenExistentialExpr>(expr)) {
      depth += getOptionalEvaluationDepth(open->getSubExpr(),
                                          open->getOpaqueValue());
      expr = open->getExistentialValue();

    // Otherwise, look through implicit conversions.
    } else {
      expr = cast<ImplicitConversionExpr>(expr)->getSubExpr();
    }
  }
}

Expr *ExprRewriter::coerceOptionalToOptional(Expr *expr, Type toType,
                                             ConstraintLocatorBuilder locator,
                                          Optional<Pattern*> typeFromPattern) {
  auto &tc = cs.getTypeChecker();
  Type fromType = cs.getType(expr);
  
  tc.requireOptionalIntrinsics(expr->getLoc());

  SmallVector<Type, 4> fromOptionals;
  (void)fromType->lookThroughAllOptionalTypes(fromOptionals);

  SmallVector<Type, 4> toOptionals;
  (void)toType->lookThroughAllOptionalTypes(toOptionals);

  assert(!toOptionals.empty());
  assert(!fromOptionals.empty());

  // If we are adding optionals but the types are equivalent up to the common
  // depth, peephole the optional-to-optional conversion into a series of nested
  // injections.
  auto toDepth = toOptionals.size();
  auto fromDepth = fromOptionals.size();
  if (toDepth > fromDepth &&
      toOptionals[toOptionals.size() - fromDepth]->isEqual(fromType)) {
    auto diff = toDepth - fromDepth;
    while (diff--) {
      Type type = toOptionals[diff];
      expr = cs.cacheType(new (tc.Context) InjectIntoOptionalExpr(expr, type));
      diagnoseOptionalInjection(cast<InjectIntoOptionalExpr>(expr));
    }

    return expr;
  }

  Type fromValueType = fromType->getOptionalObjectType();
  Type toValueType = toType->getOptionalObjectType();

  // The depth we use here will get patched after we apply the coercion.
  auto bindOptional =
    new (tc.Context) BindOptionalExpr(expr, expr->getSourceRange().End,
                                      /*depth*/ 0, fromValueType);

  expr = cs.cacheType(bindOptional);
  expr->setImplicit(true);
  expr = coerceToType(expr, toValueType, locator, typeFromPattern);
  if (!expr) return nullptr;

  unsigned depth = getOptionalEvaluationDepth(expr, bindOptional);
  bindOptional->setDepth(depth);
      
  expr = cs.cacheType(new (tc.Context) InjectIntoOptionalExpr(expr, toType));
      
  expr = cs.cacheType(new (tc.Context) OptionalEvaluationExpr(expr, toType));
  expr->setImplicit(true);
  return expr;
}

Expr *ExprRewriter::coerceImplicitlyUnwrappedOptionalToValue(Expr *expr, Type objTy) {
  auto optTy = cs.getType(expr);
  // Coerce to an r-value.
  if (optTy->is<LValueType>())
    objTy = LValueType::get(objTy);

  expr = new (cs.getTypeChecker().Context) ForceValueExpr(expr,
                                                          expr->getEndLoc(),
                                                          /* forcedIUO=*/ true);
  cs.setType(expr, objTy);
  expr->setImplicit();
  return expr;
}

/// Determine whether the given expression is a reference to an
/// unbound instance member of a type.
static bool isReferenceToMetatypeMember(ConstraintSystem &cs, Expr *expr) {
  expr = expr->getSemanticsProvidingExpr();
  if (auto dotIgnored = dyn_cast<DotSyntaxBaseIgnoredExpr>(expr))
    return cs.getType(dotIgnored->getLHS())->is<AnyMetatypeType>();
  if (auto dotSyntax = dyn_cast<DotSyntaxCallExpr>(expr))
    return cs.getType(dotSyntax->getBase())->is<AnyMetatypeType>();
  return false;
}

static bool hasCurriedSelf(ConstraintSystem &cs, ConcreteDeclRef callee,
                           ApplyExpr *apply) {
  // If we do not have a callee, return false.
  if (!callee) {
    return false;
  }

  // Only calls to members of types can have curried 'self'.
  auto calleeDecl = callee.getDecl();
  if (!calleeDecl->getDeclContext()->isTypeContext()) {
    return false;
  }

  // Would have `self`, if we're not applying it.
  if (auto *call = dyn_cast<CallExpr>(apply)) {
    if (!calleeDecl->isInstanceMember() ||
        !isReferenceToMetatypeMember(cs, call->getDirectCallee())) {
      return true;
    }
    return false;
  }

  // Operators have curried self.
  if (isa<PrefixUnaryExpr>(apply) || isa<PostfixUnaryExpr>(apply) ||
      isa<BinaryExpr>(apply)) {
    return true;
  }

  // Otherwise, we have a normal application.
  return false;
}

Expr *ExprRewriter::coerceCallArguments(
    Expr *arg, AnyFunctionType *funcType,
    ApplyExpr *apply,
    ArrayRef<Identifier> argLabels,
    bool hasTrailingClosure,
    ConstraintLocatorBuilder locator) {
  auto &tc = getConstraintSystem().getTypeChecker();
  auto params = funcType->getParams();

  // Local function to produce a locator to refer to the given parameter.
  auto getArgLocator = [&](unsigned argIdx, unsigned paramIdx)
                         -> ConstraintLocatorBuilder {
    return locator.withPathElement(
             LocatorPathElt::getApplyArgToParam(argIdx, paramIdx));
  };

  bool matchCanFail =
      llvm::any_of(params, [](const AnyFunctionType::Param &param) {
        return param.getPlainType()->hasUnresolvedType();
      });

  // Find the callee declaration.
  ConcreteDeclRef callee =
    findCalleeDeclRef(cs, solution, cs.getConstraintLocator(locator));

  // Determine whether this application has curried self.
  bool skipCurriedSelf = apply ? hasCurriedSelf(cs, callee, apply) : false;

  // Determine the parameter bindings.
  SmallBitVector defaultMap
    = computeDefaultMap(params, callee.getDecl(), skipCurriedSelf);

  SmallVector<AnyFunctionType::Param, 8> args;
  AnyFunctionType::decomposeInput(cs.getType(arg), args);

  // Quickly test if any further fix-ups for the argument types are necessary.
  if (AnyFunctionType::equalParams(args, params))
    return arg;

  // Apply labels to arguments.
  AnyFunctionType::relabelParams(args, argLabels);

  MatchCallArgumentListener listener;
  SmallVector<ParamBinding, 4> parameterBindings;
  bool failed = constraints::matchCallArguments(args, params,
                                                defaultMap,
                                                hasTrailingClosure,
                                                /*allowFixes=*/false, listener,
                                                parameterBindings);

  assert((matchCanFail || !failed) && "Call arguments did not match up?");
  (void)failed;
  (void)matchCanFail;

  // We should either have parentheses or a tuple.
  auto *argTuple = dyn_cast<TupleExpr>(arg);
  auto *argParen = dyn_cast<ParenExpr>(arg);
  // FIXME: Eventually, we want to enforce that we have either argTuple or
  // argParen here.

  // Local function to extract the ith argument expression, which papers
  // over some of the weirdness with tuples vs. parentheses.
  auto getArg = [&](unsigned i) -> Expr * {
    if (argTuple)
      return argTuple->getElement(i);
    assert(i == 0 && "Scalar only has a single argument");

    if (argParen)
      return argParen->getSubExpr();

    return arg;
  };

  // Local function to extract the ith argument label, which papers over some
  // of the weirdness with tuples vs. parentheses.
  auto getArgLabel = [&](unsigned i) -> Identifier {
    if (argTuple)
      return argTuple->getElementName(i);

    assert(i == 0 && "Scalar only has a single argument");
    return Identifier();
  };

  SmallVector<TupleTypeElt, 4> toSugarFields;
  SmallVector<TupleTypeElt, 4> fromTupleExprFields(
                                 argTuple? argTuple->getNumElements() : 1);
  SmallVector<Expr*, 4> fromTupleExpr(argTuple? argTuple->getNumElements() : 1);
  SmallVector<unsigned, 4> variadicArgs;
  SmallVector<Expr *, 2> callerDefaultArgs;
  Type sliceType = nullptr;
  SmallVector<int, 4> sources;
  for (unsigned paramIdx = 0, numParams = parameterBindings.size();
       paramIdx != numParams; ++paramIdx) {
    // Extract the parameter.
    const auto &param = params[paramIdx];

    // Handle variadic parameters.
    if (param.isVariadic()) {
      // Find the appropriate injection function.
      if (tc.requireArrayLiteralIntrinsics(arg->getStartLoc()))
        return nullptr;

      // Record this parameter.
      auto paramBaseType = param.getOldType();
      assert(sliceType.isNull() && "Multiple variadic parameters?");
      sliceType = tc.getArraySliceType(arg->getLoc(), paramBaseType);
      toSugarFields.push_back(
          TupleTypeElt(sliceType, param.getLabel(), param.getParameterFlags()));
      sources.push_back(TupleShuffleExpr::Variadic);

      // Convert the arguments.
      for (auto argIdx : parameterBindings[paramIdx]) {
        auto arg = getArg(argIdx);
        auto argType = cs.getType(arg);
        variadicArgs.push_back(argIdx);

        // If the argument type exactly matches, this just works.
        if (argType->isEqual(paramBaseType)) {
          fromTupleExprFields[argIdx] = TupleTypeElt(argType,
                                                     getArgLabel(argIdx));
          fromTupleExpr[argIdx] = arg;
          continue;
        }

        // Convert the argument.
        auto convertedArg = coerceToType(arg, paramBaseType,
                                         getArgLocator(argIdx, paramIdx));
        if (!convertedArg)
          return nullptr;

        // Add the converted argument.
        fromTupleExpr[argIdx] = convertedArg;
        fromTupleExprFields[argIdx] = TupleTypeElt(cs.getType(convertedArg),
                                                   getArgLabel(argIdx));
      }

      continue;
    }

    // If we are using a default argument, handle it now.
    if (parameterBindings[paramIdx].empty()) {
      // Create a caller-side default argument, if we need one.
      Expr *defArg;
      DefaultArgumentKind defArgKind;
      std::tie(defArg, defArgKind) = getCallerDefaultArg(cs, dc, arg->getLoc(),
                                                         callee, paramIdx);

      // Note that we'll be doing a shuffle involving default arguments.
      toSugarFields.push_back(TupleTypeElt(
                                param.isVariadic()
                                  ? tc.getArraySliceType(arg->getLoc(),
                                                         param.getOldType())
                                  : param.getOldType(),
                                param.getLabel(),
                                param.getParameterFlags()));

      if (defArg) {
        callerDefaultArgs.push_back(defArg);
        sources.push_back(TupleShuffleExpr::CallerDefaultInitialize);
      } else {
        sources.push_back(TupleShuffleExpr::DefaultInitialize);
      }
      continue;
    }

    // Extract the argument used to initialize this parameter.
    assert(parameterBindings[paramIdx].size() == 1);
    unsigned argIdx = parameterBindings[paramIdx].front();
    auto arg = getArg(argIdx);
    auto argType = cs.getType(arg);

    // If the argument and parameter indices differ, or if the names differ,
    // this is a shuffle.
    sources.push_back(argIdx);

    // If the types exactly match, this is easy.
    auto paramType = param.getOldType();
    if (argType->isEqual(paramType)) {
      toSugarFields.push_back(
          TupleTypeElt(param.getPlainType(), getArgLabel(argIdx),
                       param.getParameterFlags()));
      fromTupleExprFields[argIdx] =
          TupleTypeElt(param.getPlainType(), getArgLabel(argIdx),
                       param.getParameterFlags());
      fromTupleExpr[argIdx] = arg;
      continue;
    }

    // Convert the argument.
    auto convertedArg = coerceToType(arg, paramType,
                                     getArgLocator(argIdx, paramIdx));
    if (!convertedArg)
      return nullptr;

    // Add the converted argument.
    fromTupleExpr[argIdx] = convertedArg;
    fromTupleExprFields[argIdx] = TupleTypeElt(
        cs.getType(convertedArg)->getInOutObjectType(),
        getArgLabel(argIdx), param.getParameterFlags());
    toSugarFields.push_back(
        TupleTypeElt(argType->getInOutObjectType(), param.getLabel(),
                     param.getParameterFlags()));
  }

  Type argTupleType = TupleType::get(fromTupleExprFields, tc.Context);

  // Compute a new 'arg', from the bits we have.  We have three cases: the
  // scalar case, the paren case, and the tuple literal case.
  if (!argTuple && !argParen) {
    assert(fromTupleExpr.size() == 1 && fromTupleExpr[0]);
    arg = fromTupleExpr[0];
  } else if (argParen) {
    // If the element changed, rebuild a new ParenExpr.
    assert(fromTupleExpr.size() == 1 && fromTupleExpr[0]);
    if (fromTupleExpr[0] != argParen->getSubExpr()) {
      bool argParenImplicit = argParen->isImplicit();
      argParen =
        cs.cacheType(new (tc.Context)
                     ParenExpr(argParen->getLParenLoc(),
                               fromTupleExpr[0],
                               argParen->getRParenLoc(),
                               argParen->hasTrailingClosure(),
                               argTupleType));
      if (argParenImplicit) {
        argParen->setImplicit();
      }
      arg = argParen;
    } else {
      // coerceToType may have updated the element type of the ParenExpr in
      // place.  If so, propagate the type out to the ParenExpr as well.
      cs.setType(argParen, argTupleType);
    }
  } else {
    assert(argTuple);

    bool anyChanged = false;
    for (unsigned i = 0, e = argTuple->getNumElements(); i != e; ++i)
      if (fromTupleExpr[i] != argTuple->getElement(i)) {
        anyChanged = true;
        break;
      }

    // If anything about the TupleExpr changed, rebuild a new one.
    Type argTupleType = TupleType::get(fromTupleExprFields, tc.Context);
    assert(isa<TupleType>(argTupleType.getPointer()));

    if (anyChanged || !cs.getType(argTuple)->isEqual(argTupleType)) {
      auto EltNames = argTuple->getElementNames();
      auto EltNameLocs = argTuple->getElementNameLocs();
      argTuple =
        cs.cacheType(TupleExpr::create(tc.Context, argTuple->getLParenLoc(),
                                       fromTupleExpr, EltNames, EltNameLocs,
                                       argTuple->getRParenLoc(),
                                       argTuple->hasTrailingClosure(),
                                       argTuple->isImplicit(),
                                       argTupleType));
      arg = argTuple;
    }
  }

  // If we don't have to shuffle anything, we're done.
  args.clear();
  AnyFunctionType::decomposeInput(cs.getType(arg), args);
  if (AnyFunctionType::equalParams(args, params))
    return arg;

  auto paramType = AnyFunctionType::composeInput(tc.Context, params,
                                                 /*canonicalVararg=*/false);

  // If we came from a scalar, create a scalar-to-tuple conversion.
  TupleShuffleExpr::TypeImpact typeImpact;
  if (argTuple == nullptr) {
    // Deal with a difference in only scalar ownership.
    if (auto paramParenTy = dyn_cast<ParenType>(paramType.getPointer())) {
      assert(paramParenTy->getUnderlyingType()
              ->isEqual(cs.getType(arg)));
      auto argParen = new (tc.Context) ParenExpr(arg->getStartLoc(),
                                                 arg, arg->getEndLoc(), false,
                                                 paramParenTy);
      argParen->setImplicit();
      return cs.cacheType(argParen);
    }

    typeImpact = TupleShuffleExpr::ScalarToTuple;
    assert(isa<TupleType>(paramType.getPointer()));
  } else if (isa<TupleType>(paramType.getPointer())) {
    typeImpact = TupleShuffleExpr::TupleToTuple;
  } else {
    typeImpact = TupleShuffleExpr::TupleToScalar;
    assert(isa<ParenType>(paramType.getPointer()));
  }

  // Create the tuple shuffle.
  return cs.cacheType(TupleShuffleExpr::create(tc.Context, arg, sources,
                                               typeImpact, callee, variadicArgs,
                                               sliceType, callerDefaultArgs,
                                               paramType));
}

static ClosureExpr *getClosureLiteralExpr(Expr *expr) {
  expr = expr->getSemanticsProvidingExpr();

  if (auto *captureList = dyn_cast<CaptureListExpr>(expr))
    return captureList->getClosureBody();

  if (auto *closure = dyn_cast<ClosureExpr>(expr))
    return closure;

  return nullptr;
}

/// If the expression is an explicit closure expression (potentially wrapped in
/// IdentityExprs), change the type of the closure and identities to the
/// specified type and return true.  Otherwise, return false with no effect.
static bool applyTypeToClosureExpr(ConstraintSystem &cs,
                                   Expr *expr, Type toType) {
  // Look through identity expressions, like parens.
  if (auto IE = dyn_cast<IdentityExpr>(expr)) {
    if (!applyTypeToClosureExpr(cs, IE->getSubExpr(), toType)) return false;
    cs.setType(IE, toType);
    return true;
  }

  // Look through capture lists.
  if (auto CLE = dyn_cast<CaptureListExpr>(expr)) {
    if (!applyTypeToClosureExpr(cs, CLE->getClosureBody(), toType)) return false;
    cs.setType(CLE, toType);
    return true;
  }

  // If we found an explicit ClosureExpr, update its type.
  if (auto CE = dyn_cast<ClosureExpr>(expr)) {
    cs.setType(CE, toType);

    // If this is not a single-expression closure, write the type into the
    // ClosureExpr directly here, since the visitor won't.
    if (!CE->hasSingleExpressionBody())
      CE->setType(toType);

    return true;
  }

  // Otherwise fail.
  return false;
}

ClosureExpr *ExprRewriter::coerceClosureExprToVoid(ClosureExpr *closureExpr) {
  auto &tc = cs.getTypeChecker();

  // Re-write the single-expression closure to return '()'
  assert(closureExpr->hasSingleExpressionBody());

  // A single-expression body contains a single return statement
  // prior to this transformation.
  auto member = closureExpr->getBody()->getElement(0);
 
  if (member.is<Stmt *>()) {
    auto returnStmt = cast<ReturnStmt>(member.get<Stmt *>());
    auto singleExpr = returnStmt->getResult();
    auto voidExpr =
      cs.cacheType(
          TupleExpr::createEmpty(tc.Context,
                                 singleExpr->getStartLoc(),
                                 singleExpr->getEndLoc(),
                                 /*implicit*/true));
    returnStmt->setResult(voidExpr);

    // For l-value types, reset to the object type. This might not be strictly
    // necessary any more, but it's probably still a good idea.
    if (cs.getType(singleExpr)->is<LValueType>())
      cs.setType(singleExpr,
                 cs.getType(singleExpr)->getWithoutSpecifierType());

    cs.setExprTypes(singleExpr);
    tc.checkIgnoredExpr(singleExpr);

    SmallVector<ASTNode, 2> elements;
    elements.push_back(singleExpr);
    elements.push_back(returnStmt);
    
    auto braceStmt = BraceStmt::create(tc.Context,
                                       closureExpr->getStartLoc(),
                                       elements,
                                       closureExpr->getEndLoc(),
                                       /*implicit*/true);
    
    closureExpr->setImplicit();
    closureExpr->setBody(braceStmt, /*isSingleExpression*/true);
  }
  
  // Finally, compute the proper type for the closure.
  auto fnType = cs.getType(closureExpr)->getAs<FunctionType>();
  auto newClosureType = FunctionType::get(
      fnType->getParams(), tc.Context.TheEmptyTupleType, fnType->getExtInfo());
  cs.setType(closureExpr, newClosureType);
  return closureExpr;
}

ClosureExpr *ExprRewriter::coerceClosureExprFromNever(ClosureExpr *closureExpr) {
  auto &tc = cs.getTypeChecker();

  // Re-write the single-expression closure to drop the 'return'.
  assert(closureExpr->hasSingleExpressionBody());

  // A single-expression body contains a single return statement
  // prior to this transformation.
  auto member = closureExpr->getBody()->getElement(0);

  if (member.is<Stmt *>()) {
    auto returnStmt = cast<ReturnStmt>(member.get<Stmt *>());
    auto singleExpr = returnStmt->getResult();

    cs.setExprTypes(singleExpr);
    tc.checkIgnoredExpr(singleExpr);

    SmallVector<ASTNode, 1> elements;
    elements.push_back(singleExpr);

    auto braceStmt = BraceStmt::create(tc.Context,
                                       closureExpr->getStartLoc(),
                                       elements,
                                       closureExpr->getEndLoc(),
                                       /*implicit*/true);

    closureExpr->setImplicit();
    closureExpr->setBody(braceStmt, /*isSingleExpression*/true);
  }

  return closureExpr;
}

// Look through sugar and DotSyntaxBaseIgnoredExprs.
static Expr *
getSemanticExprForDeclOrMemberRef(Expr *expr) {
  auto semanticExpr = expr->getSemanticsProvidingExpr();
  while (auto ignoredBase = dyn_cast<DotSyntaxBaseIgnoredExpr>(semanticExpr)){
    semanticExpr = ignoredBase->getRHS()->getSemanticsProvidingExpr();
  }
  return semanticExpr;
}

static void
maybeDiagnoseUnsupportedFunctionConversion(ConstraintSystem &cs, Expr *expr,
                                           AnyFunctionType *toType) {
  auto &tc = cs.getTypeChecker();
  Type fromType = cs.getType(expr);
  auto fromFnType = fromType->getAs<AnyFunctionType>();

  // SWIFT_ENABLE_TENSORFLOW
  // If function types have different representations and one of them is
  // @convention(tensorflow), reject it.
  auto toTypeRepr = toType->getRepresentation();
  auto fromTypeRepr = fromFnType->getRepresentation();
  if (toTypeRepr != fromTypeRepr &&
      (toTypeRepr == AnyFunctionType::Representation::TensorFlow ||
       fromTypeRepr == AnyFunctionType::Representation::TensorFlow)) {
    tc.diagnose(expr->getLoc(),
                diag::invalid_tensorflow_fn_conversion);
    return;
  }

  // Conversions to C function pointer type are limited. Since a C function
  // pointer captures no context, we can only do the necessary thunking or
  // codegen if the original function is a direct reference to a global function
  // or context-free closure or local function.
  if (toType->getRepresentation()
       == AnyFunctionType::Representation::CFunctionPointer) {
    // Can convert from an ABI-compatible C function pointer.
    if (fromFnType
        && fromFnType->getRepresentation()
            == AnyFunctionType::Representation::CFunctionPointer)
      return;
    
    // Can convert a decl ref to a global or local function that doesn't
    // capture context. Look through ignored bases too.
    // TODO: Look through static method applications to the type.
    auto semanticExpr = getSemanticExprForDeclOrMemberRef(expr);
    auto maybeDiagnoseFunctionRef = [&](FuncDecl *fn) {
      // TODO: We could allow static (or class final) functions too by
      // "capturing" the metatype in a thunk.
      if (fn->getDeclContext()->isTypeContext()) {
        tc.diagnose(expr->getLoc(),
                    diag::c_function_pointer_from_method);
      } else if (fn->getGenericParams()) {
        tc.diagnose(expr->getLoc(),
                    diag::c_function_pointer_from_generic_function);
      } else {
        tc.maybeDiagnoseCaptures(expr, fn);
      }
    };
    
    if (auto declRef = dyn_cast<DeclRefExpr>(semanticExpr)) {
      if (auto fn = dyn_cast<FuncDecl>(declRef->getDecl())) {
        return maybeDiagnoseFunctionRef(fn);
      }
    }
    
    if (auto memberRef = dyn_cast<MemberRefExpr>(semanticExpr)) {
      if (auto fn = dyn_cast<FuncDecl>(memberRef->getMember().getDecl())) {
        return maybeDiagnoseFunctionRef(fn);
      }
    }

    // Unwrap closures with explicit capture lists.
    if (auto capture = dyn_cast<CaptureListExpr>(semanticExpr))
      semanticExpr = capture->getClosureBody();
    
    // Can convert a literal closure that doesn't capture context.
    if (auto closure = dyn_cast<ClosureExpr>(semanticExpr)) {
      tc.maybeDiagnoseCaptures(expr, closure);
      return;
    }
    
    tc.diagnose(expr->getLoc(),
                diag::invalid_c_function_pointer_conversion_expr);
  }
}

/// Build the conversion of an element in a collection upcast.
static Expr *buildElementConversion(ExprRewriter &rewriter,
                                    SourceLoc srcLoc,
                                    Type srcType,
                                    Type destType,
                                    bool bridged,
                                    ConstraintLocatorBuilder locator,
                                    Expr *element) {
  auto &cs = rewriter.getConstraintSystem();

  auto &tc = rewriter.getConstraintSystem().getTypeChecker();
  if (bridged &&
      tc.typeCheckCheckedCast(srcType, destType,
                              CheckedCastContextKind::None, cs.DC,
                              SourceLoc(), nullptr, SourceRange())
        != CheckedCastKind::Coercion) {
    if (auto conversion =
          rewriter.buildObjCBridgeExpr(element, destType, locator))
        return conversion;
  }

  return rewriter.coerceToType(element, destType, locator);
}

static CollectionUpcastConversionExpr::ConversionPair
buildOpaqueElementConversion(ExprRewriter &rewriter,
                             SourceLoc srcLoc,
                             Type srcCollectionType,
                             Type destCollectionType,
                             bool bridged,
                             ConstraintLocatorBuilder locator,
                             unsigned typeArgIndex) {
  // We don't need this stuff unless we've got generalized casts.
  Type srcType = srcCollectionType->castTo<BoundGenericType>()
                                  ->getGenericArgs()[typeArgIndex];
  Type destType = destCollectionType->castTo<BoundGenericType>()
                                    ->getGenericArgs()[typeArgIndex];

  // Build the conversion.
  auto &cs = rewriter.getConstraintSystem();
  ASTContext &ctx = cs.getASTContext();
  auto opaque =
    rewriter.cs.cacheType(new (ctx) OpaqueValueExpr(srcLoc, srcType));

  Expr *conversion =
    buildElementConversion(rewriter, srcLoc, srcType, destType, bridged,
                           locator.withPathElement(
                             ConstraintLocator::PathElement::getGenericArgument(
                                typeArgIndex)),
                           opaque);

  return { opaque, conversion };
}

void ExprRewriter::peepholeArrayUpcast(ArrayExpr *expr, Type toType,
                                       bool bridged, Type elementType,
                                       ConstraintLocatorBuilder locator) {
  // Update the type of the array literal.
  cs.setType(expr, toType);
  // FIXME: finish{Array,Dictionary}Expr invoke cacheExprTypes after forming
  // the semantic expression for the dictionary literal, which will undo the
  // type we set here if this dictionary literal is nested unless we update
  // the expr type as well.
  expr->setType(toType);

  // Convert the elements.
  ConstraintLocatorBuilder innerLocator =
    locator.withPathElement(
                        ConstraintLocator::PathElement::getGenericArgument(0));
  for (auto &element : expr->getElements()) {
    if (auto newElement = buildElementConversion(*this, expr->getLoc(),
                                                 cs.getType(element),
                                                 elementType,
                                                 bridged, innerLocator,
                                                 element)) {
      element = newElement;
    }
  }

  (void)finishArrayExpr(expr);
}

void ExprRewriter::peepholeDictionaryUpcast(DictionaryExpr *expr,
                                            Type toType, bool bridged,
                                            Type keyType, Type valueType,
                                            ConstraintLocatorBuilder locator) {
  // Update the type of the dictionary literal.
  cs.setType(expr, toType);
  // FIXME: finish{Array,Dictionary}Expr invoke cacheExprTypes after forming
  // the semantic expression for the dictionary literal, which will undo the
  // type we set here if this dictionary literal is nested unless we update
  // the expr type as well.
  expr->setType(toType);

  ConstraintLocatorBuilder valueLocator =
    locator.withPathElement(
      ConstraintLocator::PathElement::getGenericArgument(1));

  // Convert the elements.
  TupleTypeElt tupleTypeElts[2] = { keyType, valueType };
  auto tupleType = TupleType::get(tupleTypeElts, cs.getASTContext());
  for (auto element : expr->getElements()) {
    if (auto tuple = dyn_cast<TupleExpr>(element)) {
      auto key = tuple->getElement(0);
      if (auto newKey = buildElementConversion(*this, expr->getLoc(),
                                               cs.getType(key), keyType,
                                               bridged, valueLocator, key))
        tuple->setElement(0, newKey);

      auto value = tuple->getElement(1);
      if (auto newValue = buildElementConversion(*this, expr->getLoc(),
                                                 cs.getType(value), valueType,
                                                 bridged, valueLocator,
                                                 value)) {
        tuple->setElement(1, newValue);
      }

      cs.setType(tuple, tupleType);
      // FIXME: finish{Array,Dictionary}Expr invoke cacheExprTypes after forming
      // the semantic expression for the dictionary literal, which will undo the
      // type we set here if this dictionary literal is nested unless we update
      // the expr type as well.
      tuple->setType(tupleType);
    }
  }

  (void)finishDictionaryExpr(expr);
}

bool ExprRewriter::peepholeCollectionUpcast(Expr *expr, Type toType,
                                            bool bridged,
                                            ConstraintLocatorBuilder locator) {
  // Recur into parenthesized expressions.
  if (auto paren = dyn_cast<ParenExpr>(expr)) {
    // If we can't peephole the subexpression, we're done.
    if (!peepholeCollectionUpcast(paren->getSubExpr(), toType, bridged,
                                  locator))
      return false;

    // Update the type of this expression.
    auto parenTy = ParenType::get(cs.getASTContext(),
                                  cs.getType(paren->getSubExpr()));
    cs.setType(paren, parenTy);
    // FIXME: finish{Array,Dictionary}Expr invoke cacheExprTypes after forming
    // the semantic expression for the dictionary literal, which will undo the
    // type we set here if this dictionary literal is nested unless we update
    // the expr type as well.
    paren->setType(parenTy);
    return true;
  }

  // Array literals.
  if (auto arrayLiteral = dyn_cast<ArrayExpr>(expr)) {
    if (Optional<Type> elementType = ConstraintSystem::isArrayType(toType)) {
      peepholeArrayUpcast(arrayLiteral, toType, bridged, *elementType, locator);
      return true;
    }

    if (Optional<Type> elementType = ConstraintSystem::isSetType(toType)) {
      peepholeArrayUpcast(arrayLiteral, toType, bridged, *elementType, locator);
      return true;
    }

    return false;
  }

  // Dictionary literals.
  if (auto dictLiteral = dyn_cast<DictionaryExpr>(expr)) {
    if (auto elementType = ConstraintSystem::isDictionaryType(toType)) {
      peepholeDictionaryUpcast(dictLiteral, toType, bridged,
                               elementType->first, elementType->second,
                               locator);
      return true;
    }

    return false;
  }

  return false;
}

Expr *ExprRewriter::buildCollectionUpcastExpr(
                                            Expr *expr, Type toType,
                                            bool bridged,
                                            ConstraintLocatorBuilder locator) {
  if (peepholeCollectionUpcast(expr, toType, bridged, locator))
    return expr;

  ASTContext &ctx = cs.getASTContext();
  // Build the first value conversion.
  auto conv =
    buildOpaqueElementConversion(*this, expr->getLoc(), cs.getType(expr),
                                 toType, bridged, locator, 0);

  // For single-parameter collections, form the upcast.
  if (ConstraintSystem::isArrayType(toType) ||
      ConstraintSystem::isSetType(toType)) {
    return cs.cacheType(
              new (ctx) CollectionUpcastConversionExpr(expr, toType, {}, conv));
  }

  assert(ConstraintSystem::isDictionaryType(toType) &&
         "Unhandled collection upcast");

  // Build the second value conversion.
  auto conv2 =
    buildOpaqueElementConversion(*this, expr->getLoc(), cs.getType(expr),
                                 toType, bridged, locator, 1);

  return cs.cacheType(
           new (ctx) CollectionUpcastConversionExpr(expr, toType, conv, conv2));

}

Expr *ExprRewriter::buildObjCBridgeExpr(Expr *expr, Type toType,
                                        ConstraintLocatorBuilder locator) {
  auto &tc = cs.getTypeChecker();

  Type fromType = cs.getType(expr);

  // Bridged collection casts always succeed, so we treat them as
  // collection "upcasts".
  if ((ConstraintSystem::isArrayType(fromType) &&
       ConstraintSystem::isArrayType(toType)) ||
      (ConstraintSystem::isDictionaryType(fromType) &&
       ConstraintSystem::isDictionaryType(toType)) ||
      (ConstraintSystem::isSetType(fromType) &&
       ConstraintSystem::isSetType(toType))) {
    return buildCollectionUpcastExpr(expr, toType, /*bridged=*/true, locator);
  }

  // Bridging from a Swift type to an Objective-C class type.
  if (toType->isAnyObject() ||
      (fromType->getRValueType()->isPotentiallyBridgedValueType() &&
       (toType->isBridgeableObjectType() || toType->isExistentialType()))) {
    // Bridging to Objective-C.
    Expr *objcExpr = bridgeToObjectiveC(expr, toType);
    if (!objcExpr)
      return nullptr;

    // We might have a coercion of a Swift type to a CF type toll-free
    // bridged to Objective-C.
    //
    // FIXME: Ideally we would instead have already recorded a restriction
    // when solving the constraint, and we wouldn't need to duplicate this
    // part of coerceToType() here.
    if (auto foreignClass = toType->getClassOrBoundGenericClass()) {
      if (foreignClass->getForeignClassKind() ==
            ClassDecl::ForeignKind::CFType) {
        return cs.cacheType(
          new (tc.Context) ForeignObjectConversionExpr(objcExpr, toType));
      }
    }

    return coerceToType(objcExpr, toType, locator);
  }

  // Bridging from an Objective-C class type to a Swift type.
  return forceBridgeFromObjectiveC(expr, toType);
}

static Expr *addImplicitLoadExpr(ConstraintSystem &cs, Expr *expr) {
  auto &tc = cs.getTypeChecker();
  return tc.addImplicitLoadExpr(
      expr, [&cs](Expr *expr) { return cs.getType(expr); },
      [&cs](Expr *expr, Type type) { cs.setType(expr, type); });
}

Expr *ExprRewriter::coerceToType(Expr *expr, Type toType,
                                 ConstraintLocatorBuilder locator,
                                 Optional<Pattern*> typeFromPattern) {
  auto &tc = cs.getTypeChecker();

  // The type we're converting from.
  Type fromType = cs.getType(expr);

  // If the types are already equivalent, we don't have to do anything.
  if (fromType->isEqual(toType))
    return expr;

  // If the solver recorded what we should do here, just do it immediately.
  auto knownRestriction = solution.ConstraintRestrictions.find(
                            { fromType->getCanonicalType(),
                              toType->getCanonicalType() });
  if (knownRestriction != solution.ConstraintRestrictions.end()) {
    switch (knownRestriction->second) {

    case ConversionRestrictionKind::TupleToTuple:
    case ConversionRestrictionKind::LValueToRValue:
        // Restrictions that don't need to be recorded.
        // Should match recordRestriction() in CSSimplify
        break;

    case ConversionRestrictionKind::DeepEquality: {
      if (toType->hasUnresolvedType())
        break;

      // HACK: Fix problem related to Swift 4 mode (with assertions),
      // since Swift 4 mode allows passing arguments with extra parens
      // to parameters which don't expect them, it should be supported
      // by "deep equality" type - Optional<T> e.g.
      // ```swift
      // func foo(_: (() -> Void)?) {}
      // func bar() -> ((()) -> Void)? { return nil }
      // foo(bar) // This expression should compile in Swift 3 mode
      // ```
      //
      // See also: https://bugs.swift.org/browse/SR-6796
      if (cs.getASTContext().isSwiftVersionAtLeast(4) &&
          !cs.getASTContext().isSwiftVersionAtLeast(5)) {
        auto obj1 = fromType->getOptionalObjectType();
        auto obj2 = toType->getOptionalObjectType();

        if (obj1 && obj2) {
          auto *fn1 = obj1->getAs<AnyFunctionType>();
          auto *fn2 = obj2->getAs<AnyFunctionType>();

          if (fn1 && fn2) {
            auto params1 = fn1->getParams();
            auto params2 = fn2->getParams();

            // This handles situations like argument: (()), parameter: ().
            if (params1.size() == 1 && params2.empty()) {
              auto tupleTy = params1.front().getOldType()->getAs<TupleType>();
              if (tupleTy && tupleTy->getNumElements() == 0)
                break;
            }
          }
        }
      }

      llvm_unreachable("Should be handled above");
    }

    case ConversionRestrictionKind::Superclass:
    case ConversionRestrictionKind::ExistentialMetatypeToMetatype:
      return coerceSuperclass(expr, toType, locator);

    case ConversionRestrictionKind::Existential:
    case ConversionRestrictionKind::MetatypeToExistentialMetatype:
      return coerceExistential(expr, toType, locator);

    case ConversionRestrictionKind::ClassMetatypeToAnyObject: {
      assert(tc.getLangOpts().EnableObjCInterop
             && "metatypes can only be cast to objects w/ objc runtime!");
      return cs.cacheType(
          new (tc.Context) ClassMetatypeToObjectExpr(expr, toType));
    }
    case ConversionRestrictionKind::ExistentialMetatypeToAnyObject: {
      assert(tc.getLangOpts().EnableObjCInterop
             && "metatypes can only be cast to objects w/ objc runtime!");
      return cs.cacheType(
          new (tc.Context) ExistentialMetatypeToObjectExpr(expr, toType));
    }
    case ConversionRestrictionKind::ProtocolMetatypeToProtocolClass: {
      return cs.cacheType(
          new (tc.Context) ProtocolMetatypeToObjectExpr(expr, toType));
    }
        
    case ConversionRestrictionKind::ValueToOptional: {
      auto toGenericType = toType->castTo<BoundGenericType>();
      assert(toGenericType->getDecl()->isOptionalDecl());
      tc.requireOptionalIntrinsics(expr->getLoc());

      Type valueType = toGenericType->getGenericArgs()[0];
      expr = coerceToType(expr, valueType, locator);
      if (!expr) return nullptr;

      auto *result =
        cs.cacheType(new (tc.Context) InjectIntoOptionalExpr(expr, toType));
      diagnoseOptionalInjection(result);
      return result;
    }

    case ConversionRestrictionKind::OptionalToOptional:
      return coerceOptionalToOptional(expr, toType, locator, typeFromPattern);

    case ConversionRestrictionKind::ArrayUpcast: {
      // Build the value conversion.
      return buildCollectionUpcastExpr(expr, toType, /*bridged=*/false,
                                       locator);
    }

    case ConversionRestrictionKind::HashableToAnyHashable: {
      // We want to check conformance on the rvalue, as that's what has
      // the Hashable conformance
      expr = cs.coerceToRValue(expr);

      // Find the conformance of the source type to Hashable.
      auto hashable = tc.Context.getProtocol(KnownProtocolKind::Hashable);
      auto conformance =
        tc.conformsToProtocol(
                        cs.getType(expr), hashable, cs.DC,
                        (ConformanceCheckFlags::InExpression |
                         ConformanceCheckFlags::Used));
      assert(conformance && "must conform to Hashable");

      return cs.cacheType(
          new (tc.Context) AnyHashableErasureExpr(expr, toType, *conformance));
    }

    case ConversionRestrictionKind::DictionaryUpcast: {
      // Build the value conversion.
      return buildCollectionUpcastExpr(expr, toType, /*bridged=*/false,
                                       locator);
    }

    case ConversionRestrictionKind::SetUpcast: {
      // Build the value conversion.
      return buildCollectionUpcastExpr(expr, toType, /*bridged=*/false, locator);
    }

    case ConversionRestrictionKind::InoutToPointer: {
      bool isOptional = false;
      Type unwrappedTy = toType;
      if (Type unwrapped = toType->getOptionalObjectType()) {
        isOptional = true;
        unwrappedTy = unwrapped;
      }
      PointerTypeKind pointerKind;
      auto toEltType = unwrappedTy->getAnyPointerElementType(pointerKind);
      assert(toEltType && "not a pointer type?"); (void) toEltType;

      tc.requirePointerArgumentIntrinsics(expr->getLoc());
      Expr *result =
          cs.cacheType(new (tc.Context) InOutToPointerExpr(expr, unwrappedTy));
      if (isOptional)
        result = cs.cacheType(new (tc.Context)
                                  InjectIntoOptionalExpr(result, toType));
      return result;
    }
    
    case ConversionRestrictionKind::ArrayToPointer: {
      bool isOptional = false;
      Type unwrappedTy = toType;
      if (Type unwrapped = toType->getOptionalObjectType()) {
        isOptional = true;
        unwrappedTy = unwrapped;
      }

      tc.requirePointerArgumentIntrinsics(expr->getLoc());
      Expr *result =
          cs.cacheType(new (tc.Context) ArrayToPointerExpr(expr, unwrappedTy));
      if (isOptional)
        result = cs.cacheType(new (tc.Context)
                                  InjectIntoOptionalExpr(result, toType));
      return result;
    }
    
    case ConversionRestrictionKind::StringToPointer: {
      bool isOptional = false;
      Type unwrappedTy = toType;
      if (Type unwrapped = toType->getOptionalObjectType()) {
        isOptional = true;
        unwrappedTy = unwrapped;
      }

      tc.requirePointerArgumentIntrinsics(expr->getLoc());
      Expr *result =
          cs.cacheType(new (tc.Context) StringToPointerExpr(expr, unwrappedTy));
      if (isOptional)
        result = cs.cacheType(new (tc.Context)
                                  InjectIntoOptionalExpr(result, toType));
      return result;
    }
    
    case ConversionRestrictionKind::PointerToPointer: {
      tc.requirePointerArgumentIntrinsics(expr->getLoc());
      Type unwrappedToTy = toType->getOptionalObjectType();

      // Optional to optional.
      if (Type unwrappedFromTy = cs.getType(expr)->getOptionalObjectType()) {
        assert(unwrappedToTy && "converting optional to non-optional");
        Expr *boundOptional = cs.cacheType(
            new (tc.Context) BindOptionalExpr(expr, SourceLoc(),
                                              /*depth*/ 0, unwrappedFromTy));
        Expr *converted = cs.cacheType(new (tc.Context) PointerToPointerExpr(
            boundOptional, unwrappedToTy));
        Expr *rewrapped = cs.cacheType(
            new (tc.Context) InjectIntoOptionalExpr(converted, toType));
        return cs.cacheType(new (tc.Context)
                                OptionalEvaluationExpr(rewrapped, toType));
      }

      // Non-optional to optional.
      if (unwrappedToTy) {
        Expr *converted = cs.cacheType(
            new (tc.Context) PointerToPointerExpr(expr, unwrappedToTy));
        return cs.cacheType(new (tc.Context)
                                InjectIntoOptionalExpr(converted, toType));
      }

      // Non-optional to non-optional.
      return cs.cacheType(new (tc.Context) PointerToPointerExpr(expr, toType));
    }

    case ConversionRestrictionKind::CFTollFreeBridgeToObjC: {
      auto foreignClass = fromType->getClassOrBoundGenericClass();
      auto objcType = foreignClass->getAttrs().getAttribute<ObjCBridgedAttr>()
                        ->getObjCClass()->getDeclaredInterfaceType();
      auto asObjCClass = cs.cacheType(
          new (tc.Context) ForeignObjectConversionExpr(expr, objcType));
      return coerceToType(asObjCClass, toType, locator);
    }

    case ConversionRestrictionKind::ObjCTollFreeBridgeToCF: {
      auto foreignClass = toType->getClassOrBoundGenericClass();
      auto objcType = foreignClass->getAttrs().getAttribute<ObjCBridgedAttr>()
                        ->getObjCClass()->getDeclaredInterfaceType();
      Expr *result = coerceToType(expr, objcType, locator);
      if (!result)
        return nullptr;

      return cs.cacheType(new (tc.Context)
                              ForeignObjectConversionExpr(result, toType));
    }
    }
  }

  // Coercions from an lvalue: load or perform implicit address-of. We perform
  // these coercions first because they are often the first step in a multi-step
  // coercion.
  if (auto fromLValue = fromType->getAs<LValueType>()) {
    if (auto *toIO = toType->getAs<InOutType>()) {
      // In an 'inout' operator like "i += 1", the operand is converted from
      // an implicit lvalue to an inout argument.
      assert(toIO->getObjectType()->isEqual(fromLValue->getObjectType()));
      return cs.cacheType(new (tc.Context)
                              InOutExpr(expr->getStartLoc(), expr,
                                        toIO->getObjectType(),
                                        /*isImplicit*/ true));
    }

    return coerceToType(addImplicitLoadExpr(cs, expr), toType, locator);
  }

  // Coercions to tuple type.
  if (auto toTuple = toType->getAs<TupleType>()) {
    // Coerce from a tuple to a tuple.
    if (auto fromTuple = fromType->getAs<TupleType>()) {
      SmallVector<int, 4> sources;
      SmallVector<unsigned, 4> variadicArgs;
      if (!computeTupleShuffle(fromTuple, toTuple, sources, variadicArgs)) {
        return coerceTupleToTuple(expr, fromTuple, toTuple,
                                  locator, sources, variadicArgs);
      }
    }
  }

  // Coercion from a subclass to a superclass.
  //
  // FIXME: Can we rig things up so that we always have a Superclass
  // conversion restriction in this case?
  if (fromType->mayHaveSuperclass() &&
      toType->getClassOrBoundGenericClass()) {
    for (auto fromSuperClass = fromType->getSuperclass();
         fromSuperClass;
         fromSuperClass = fromSuperClass->getSuperclass()) {
      if (fromSuperClass->isEqual(toType)) {
        return coerceSuperclass(expr, toType, locator);
      }
    }
  }

  // Coercions to function type.
  if (auto toFunc = toType->getAs<FunctionType>()) {
    // Default argument generator must return escaping functions. Therefore, we
    // leave an explicit escape to noescape cast here such that SILGen can skip
    // the cast and emit a code for the escaping function.
    bool isInDefaultArgumentContext = false;
    if (auto initalizerCtx = dyn_cast<Initializer>(cs.DC))
      isInDefaultArgumentContext = (initalizerCtx->getInitializerKind() ==
                                    InitializerKind::DefaultArgument);
    auto toEI = toFunc->getExtInfo();

    auto fromFunc = fromType->getAs<FunctionType>();

    // Coercion to an autoclosure type produces an implicit closure.
    // The constraint solver only performs this conversion when the source
    // type is not an autoclosure function type.  That's a weird rule in
    // some rules, but it's easy to follow here.  Really we just shouldn't
    // represent autoclosures as a bit on function types.
    // FIXME: The type checker is more lenient, and allows @autoclosures to
    // be subtypes of non-@autoclosures, which is bogus.
    if (toFunc->isAutoClosure() &&
        (!fromFunc || !fromFunc->isAutoClosure())) {
      // The function type without @noescape if we are in the default argument
      // context.
      auto newToFuncType = toFunc;

      // Remove the noescape attribute so that we can apply a separate function
      // conversion instruction if we are in a default argument context.
      if (isInDefaultArgumentContext && toEI.isNoEscape())
        newToFuncType = toFunc->withExtInfo(toEI.withNoEscape(false))
                            ->castTo<FunctionType>();

      // Convert the value to the expected result type of the function.
      expr = coerceToType(
          expr, toFunc->getResult(),
          locator.withPathElement(ConstraintLocator::AutoclosureResult));

      // We'll set discriminator values on all the autoclosures in a
      // later pass.
      auto discriminator = AutoClosureExpr::InvalidDiscriminator;
      auto closure = cs.cacheType(new (tc.Context) AutoClosureExpr(
          expr, newToFuncType, discriminator, dc));
      closure->setParameterList(ParameterList::createEmpty(tc.Context));

      // Compute the capture list, now that we have analyzed the expression.
      tc.ClosuresWithUncomputedCaptures.push_back(closure);

      // Apply the noescape conversion.
      if (!newToFuncType->isEqual(toFunc)) {
        assert(isInDefaultArgumentContext);
        assert(newToFuncType
                   ->withExtInfo(newToFuncType->getExtInfo().withNoEscape(true))
                   ->isEqual(toFunc));
        return cs.cacheType(new (tc.Context)
                                FunctionConversionExpr(closure, toFunc));
      }

      return closure;
    }

    // Coercion from one function type to another, this produces a
    // FunctionConversionExpr in its full generality.
    if (fromFunc) {
      // SWIFT_ENABLE_TENSORFLOW
      // If we have a ClosureExpr, then we can safely propagate tensorflow
      // convention to the closure expression.
      // NOTE: we also need to check if the closure captures any values.
      // However, capture information is not available at this point. Therefore,
      // the check currently happens in the SILGen phase.
      auto fromEI = fromFunc->getExtInfo();
      if (toEI.getRepresentation() ==
              AnyFunctionType::Representation::TensorFlow &&
          fromEI.getRepresentation() !=
              AnyFunctionType::Representation::TensorFlow) {
        auto newFromFuncType = fromFunc->withExtInfo(fromEI.withRepresentation(
            AnyFunctionType::Representation::TensorFlow));
        if (applyTypeToClosureExpr(cs, expr, newFromFuncType)) {
          fromFunc = newFromFuncType->castTo<FunctionType>();
        }
      }
      // If we have a ClosureExpr, then we can safely propagate the 'no escape'
      // bit to the closure without invalidating prior analysis.
      fromEI = fromFunc->getExtInfo();
      if (toEI.isNoEscape() && !fromEI.isNoEscape()) {
        auto newFromFuncType = fromFunc->withExtInfo(fromEI.withNoEscape());
        if (!isInDefaultArgumentContext &&
            applyTypeToClosureExpr(cs, expr, newFromFuncType)) {
          fromFunc = newFromFuncType->castTo<FunctionType>();
          // Propagating the 'no escape' bit might have satisfied the entire
          // conversion.  If so, we're done, otherwise keep converting.
          if (fromFunc->isEqual(toType))
            return expr;
        } else if (isInDefaultArgumentContext) {
          // First apply the conversion *without* noescape attribute.
          if (!newFromFuncType->isEqual(toType)) {
            auto escapingToFuncTy =
                toFunc->withExtInfo(toEI.withNoEscape(false));
            maybeDiagnoseUnsupportedFunctionConversion(cs, expr, toFunc);
            expr = cs.cacheType(new (tc.Context) FunctionConversionExpr(
                expr, escapingToFuncTy));
          }
          // Apply an explict function conversion *only* for the escape to
          // noescape conversion. This conversion will be stripped by the
          // default argument generator. (We can't return a @noescape function)
          auto newExpr = cs.cacheType(new (tc.Context)
                                          FunctionConversionExpr(expr, toFunc));
          return newExpr;
        }
      }

      maybeDiagnoseUnsupportedFunctionConversion(cs, expr, toFunc);

      return cs.cacheType(new (tc.Context)
                              FunctionConversionExpr(expr, toType));
    }
  }

  // Coercions from metadata to objects.
  if (auto fromMeta = fromType->getAs<AnyMetatypeType>()) {
    if (toType->isAnyObject()) {
      assert(cs.getASTContext().LangOpts.EnableObjCInterop
             && "metatype-to-object conversion requires objc interop");
      if (fromMeta->is<MetatypeType>()) {
        assert(fromMeta->getInstanceType()->mayHaveSuperclass()
               && "metatype-to-object input should be a class metatype");
        return cs.cacheType(
          new (tc.Context) ClassMetatypeToObjectExpr(expr, toType));
      }
      
      if (fromMeta->is<ExistentialMetatypeType>()) {
        assert(fromMeta->getInstanceType()->getCanonicalType()
                       ->getExistentialLayout().requiresClass()
               && "metatype-to-object input should be a class metatype");
        return cs.cacheType(
          new (tc.Context) ExistentialMetatypeToObjectExpr(expr, toType));
      }
      
      llvm_unreachable("unhandled metatype kind");
    }
    
    if (auto toClass = toType->getClassOrBoundGenericClass()) {
      if (toClass->getName() == cs.getASTContext().Id_Protocol
          && toClass->getModuleContext()->getName()
              == cs.getASTContext().Id_ObjectiveC) {
        assert(cs.getASTContext().LangOpts.EnableObjCInterop
               && "metatype-to-object conversion requires objc interop");
        assert(fromMeta->is<MetatypeType>()
               && fromMeta->getInstanceType()->is<ProtocolType>()
               && "protocol-metatype-to-Protocol only works for single "
                  "protocols");
        return cs.cacheType(
          new (tc.Context) ProtocolMetatypeToObjectExpr(expr, toType));
      }
    }
  }

  // Coercions from a type to an existential type.
  if (toType->isAnyExistentialType()) {
    return coerceExistential(expr, toType, locator);
  }

  if (toType->getOptionalObjectType() &&
      cs.getType(expr)->getOptionalObjectType()) {
    return coerceOptionalToOptional(expr, toType, locator, typeFromPattern);
  }

  // Coercion to Optional<T>.
  if (auto toGenericType = toType->getAs<BoundGenericType>()) {
    if (toGenericType->getDecl()->isOptionalDecl()) {
      tc.requireOptionalIntrinsics(expr->getLoc());

      Type valueType = toGenericType->getGenericArgs()[0];
      expr = coerceToType(expr, valueType, locator);
      if (!expr) return nullptr;

      auto *result =
          cs.cacheType(new (tc.Context) InjectIntoOptionalExpr(expr, toType));
      diagnoseOptionalInjection(result);
      return result;
    }
  }

  // Coercion from one metatype to another.
  if (fromType->is<MetatypeType>() &&
      toType->is<MetatypeType>()) {
    auto toMeta = toType->castTo<MetatypeType>();
    return cs.cacheType(new (tc.Context) MetatypeConversionExpr(expr, toMeta));
  }

  // Unresolved types come up in diagnostics for lvalue and inout types.
  if (fromType->hasUnresolvedType() || toType->hasUnresolvedType())
    return cs.cacheType(new (tc.Context)
                            UnresolvedTypeConversionExpr(expr, toType));

  llvm_unreachable("Unhandled coercion");
}

/// Adjust the given type to become the self type when referring to
/// the given member.
static Type adjustSelfTypeForMember(Type baseTy, ValueDecl *member,
                                    AccessSemantics semantics,
                                    DeclContext *UseDC) {
  auto baseObjectTy = baseTy->getWithoutSpecifierType();

  if (isa<ConstructorDecl>(member))
    return baseObjectTy;

  if (auto func = dyn_cast<FuncDecl>(member)) {
    // If 'self' is an inout type, turn the base type into an lvalue
    // type with the same qualifiers.
    if (func->isMutating())
      return InOutType::get(baseObjectTy);

    // Otherwise, return the rvalue type.
    return baseObjectTy;
  }

  // If the base of the access is mutable, then we may be invoking a getter or
  // setter that requires the base to be mutable.
  auto *SD = cast<AbstractStorageDecl>(member);
  bool isSettableFromHere = SD->isSettable(UseDC)
    && (!UseDC->getASTContext().LangOpts.EnableAccessControl
        || SD->isSetterAccessibleFrom(UseDC));

  // If neither the property's getter nor its setter are mutating, the base
  // can be an rvalue.
  if (!SD->isGetterMutating()
      && (!isSettableFromHere || !SD->isSetterMutating()))
    return baseObjectTy;

  // If we're calling an accessor, keep the base as an inout type, because the
  // getter may be mutating.
  auto strategy = SD->getAccessStrategy(semantics,
                                        isSettableFromHere
                                          ? AccessKind::ReadWrite
                                          : AccessKind::Read,
                                        UseDC);
  if (baseTy->is<InOutType>() && strategy.getKind() != AccessStrategy::Storage)
    return InOutType::get(baseObjectTy);

  // Accesses to non-function members in value types are done through an @lvalue
  // type.
  if (baseTy->is<InOutType>())
    return LValueType::get(baseObjectTy);
  
  // Accesses to members in values of reference type (classes, metatypes) are
  // always done through a the reference to self.  Accesses to value types with
  // a non-mutable self are also done through the base type.
  return baseTy;
}

Expr *
ExprRewriter::coerceObjectArgumentToType(Expr *expr,
                                         Type baseTy, ValueDecl *member,
                                         AccessSemantics semantics,
                                         ConstraintLocatorBuilder locator) {
  Type toType = adjustSelfTypeForMember(baseTy, member, semantics, dc);

  // If our expression already has the right type, we're done.
  Type fromType = cs.getType(expr);
  if (fromType->isEqual(toType))
    return expr;

  // If we're coercing to an rvalue type, just do it.
  auto toInOutTy = toType->getAs<InOutType>();
  if (!toInOutTy)
    return coerceToType(expr, toType, locator);

  assert(fromType->is<LValueType>() && "Can only convert lvalues to inout");

  auto &ctx = cs.getTypeChecker().Context;

  // Use InOutExpr to convert it to an explicit inout argument for the
  // receiver.
  return cs.cacheType(new (ctx) InOutExpr(expr->getStartLoc(), expr, 
                                          toInOutTy->getInOutObjectType(),
                                          /*isImplicit*/ true));
}

Expr *ExprRewriter::convertLiteral(Expr *literal,
                                   Type type,
                                   Type openedType,
                                   ProtocolDecl *protocol,
                                   TypeOrName literalType,
                                   DeclName literalFuncName,
                                   ProtocolDecl *builtinProtocol,
                                   TypeOrName builtinLiteralType,
                                   DeclName builtinLiteralFuncName,
                                   bool (*isBuiltinArgType)(Type),
                                   Diag<> brokenProtocolDiag,
                                   Diag<> brokenBuiltinProtocolDiag) {
  auto &tc = cs.getTypeChecker();

  auto getType = [&](const Expr *E) -> Type {
    return cs.getType(E);
  };

  auto setType = [&](Expr *E, Type Ty) {
    cs.setType(E, Ty);
  };

  // If coercing a literal to an unresolved type, we don't try to look up the
  // witness members, just do it.
  if (type->is<UnresolvedType>()) {
    // Instead of updating the literal expr in place, allocate a new node.  This
    // avoids issues where Builtin types end up on expr nodes and pollute
    // diagnostics.
    literal = cast<LiteralExpr>(literal)->shallowClone(tc.Context, setType,
                                                       getType);

    // The literal expression has this type.
    cs.setType(literal, type);
    return literal;
  }
  
  // Check whether this literal type conforms to the builtin protocol.
  Optional<ProtocolConformanceRef> builtinConformance;
  if (builtinProtocol &&
      (builtinConformance =
         tc.conformsToProtocol(
                      type, builtinProtocol, cs.DC,
                      (ConformanceCheckFlags::InExpression)))) {

    // Find the builtin argument type we'll use.
    Type argType;
    if (builtinLiteralType.is<Type>())
      argType = builtinLiteralType.get<Type>();
    else
      argType = tc.getWitnessType(type, builtinProtocol,
                                  *builtinConformance,
                                  builtinLiteralType.get<Identifier>(),
                                  brokenBuiltinProtocolDiag);

    if (!argType)
      return nullptr;

    // Make sure it's of an appropriate builtin type.
    if (isBuiltinArgType && !isBuiltinArgType(argType)) {
      tc.diagnose(builtinProtocol->getLoc(), brokenBuiltinProtocolDiag);
      return nullptr;
    }

    // Instead of updating the literal expr in place, allocate a new node.  This
    // avoids issues where Builtin types end up on expr nodes and pollute
    // diagnostics.
    literal = cast<LiteralExpr>(literal)->shallowClone(tc.Context, setType,
                                                       getType);

    // The literal expression has this type.
    cs.setType(literal, argType);

    // Call the builtin conversion operation.
    // FIXME: Bogus location info.
    Expr *base =
      TypeExpr::createImplicitHack(literal->getLoc(), type, tc.Context);

    cs.cacheExprTypes(base);
    cs.setExprTypes(base);
    cs.setExprTypes(literal);
    SmallVector<Expr *, 1> arguments = { literal };

    Expr *result = tc.callWitness(base, dc,
                                  builtinProtocol, *builtinConformance,
                                  builtinLiteralFuncName,
                                  arguments,
                                  brokenBuiltinProtocolDiag);
    if (result)
      cs.cacheExprTypes(result);

    return result;
  }

  // This literal type must conform to the (non-builtin) protocol.
  assert(protocol && "requirements should have stopped recursion");
  auto conformance = tc.conformsToProtocol(type, protocol, cs.DC,
                                           ConformanceCheckFlags::InExpression);
  assert(conformance && "must conform to literal protocol");

  // Figure out the (non-builtin) argument type if there is one.
  Type argType;
  if (literalType.is<Identifier>() &&
      literalType.get<Identifier>().empty()) {
    // If there is no argument to the constructor function, then just pass in
    // the empty tuple.
    literal =
      cs.cacheType(
          TupleExpr::createEmpty(tc.Context, literal->getLoc(),
                                 literal->getLoc(),
                                 /*implicit*/!literal->getLoc().isValid()));
  } else {
    // Otherwise, figure out the type of the constructor function and coerce to
    // it.
    if (literalType.is<Type>())
      argType = literalType.get<Type>();
    else
      argType = tc.getWitnessType(type, protocol, *conformance,
                                  literalType.get<Identifier>(),
                                  brokenProtocolDiag);
    if (!argType)
      return nullptr;

    // Convert the literal to the non-builtin argument type via the
    // builtin protocol, first.
    // FIXME: Do we need an opened type here?
    literal = convertLiteral(literal, argType, argType, nullptr, Identifier(),
                             Identifier(), builtinProtocol,
                             builtinLiteralType, builtinLiteralFuncName,
                             isBuiltinArgType, brokenProtocolDiag,
                             brokenBuiltinProtocolDiag);
    if (!literal)
      return nullptr;
  }

  // Convert the resulting expression to the final literal type.
  // FIXME: Bogus location info.
  Expr *base = TypeExpr::createImplicitHack(literal->getLoc(), type,
                                            tc.Context);
  cs.cacheExprTypes(base);
  cs.setExprTypes(base);
  cs.setExprTypes(literal);

  literal = tc.callWitness(base, dc,
                           protocol, *conformance, literalFuncName,
                           literal, brokenProtocolDiag);
  if (literal)
    cs.cacheExprTypes(literal);

  return literal;
}

Expr *ExprRewriter::convertLiteralInPlace(Expr *literal,
                                          Type type,
                                          ProtocolDecl *protocol,
                                          Identifier literalType,
                                          DeclName literalFuncName,
                                          ProtocolDecl *builtinProtocol,
                                          DeclName builtinLiteralFuncName,
                                          Diag<> brokenProtocolDiag,
                                          Diag<> brokenBuiltinProtocolDiag) {
  auto &tc = cs.getTypeChecker();

  auto getType = [&](const Expr *E) -> Type {
    return cs.getType(E);
  };

  auto setType = [&](Expr *E, Type Ty) {
    cs.setType(E, Ty);
  };

  // If coercing a literal to an unresolved type, we don't try to look up the
  // witness members, just do it.
  if (type->is<UnresolvedType>()) {
    // Instead of updating the literal expr in place, allocate a new node.  This
    // avoids issues where Builtin types end up on expr nodes and pollute
    // diagnostics.
    literal = cast<LiteralExpr>(literal)->shallowClone(tc.Context, setType,
                                                       getType);

    // The literal expression has this type.
    cs.setType(literal, type);
    return literal;
  }

  // Check whether this literal type conforms to the builtin protocol. If so,
  // initialize via the builtin protocol.
  Optional<ProtocolConformanceRef> builtinConformance;
  if (builtinProtocol &&
      (builtinConformance =
         tc.conformsToProtocol(type, builtinProtocol, cs.DC,
                               ConformanceCheckFlags::InExpression))) {

    // Find the witness that we'll use to initialize the type via a builtin
    // literal.
    auto witness = findNamedWitnessImpl(
                     tc, dc, type->getRValueType(), builtinProtocol,
                     builtinLiteralFuncName, brokenBuiltinProtocolDiag,
                     *builtinConformance);
    if (!witness || !isa<AbstractFunctionDecl>(witness.getDecl()))
      return nullptr;

    // Form a reference to the builtin conversion function.

    // Set the builtin initializer.
    if (auto stringLiteral = dyn_cast<StringLiteralExpr>(literal))
      stringLiteral->setBuiltinInitializer(witness);
    else {
      cast<MagicIdentifierLiteralExpr>(literal)
        ->setBuiltinInitializer(witness);
    }

    // The literal expression has this type.
    cs.setType(literal, type);

    return literal;
  }

  // This literal type must conform to the (non-builtin) protocol.
  assert(protocol && "requirements should have stopped recursion");
  auto conformance = tc.conformsToProtocol(type, protocol, cs.DC,
                                           ConformanceCheckFlags::InExpression);
  assert(conformance && "must conform to literal protocol");

  // Dig out the literal type and perform a builtin literal conversion to it.
  if (!literalType.empty()) {
    // Extract the literal type.
    Type builtinLiteralType = tc.getWitnessType(type, protocol, *conformance,
                                                literalType,
                                                brokenProtocolDiag);
    if (!builtinLiteralType)
      return nullptr;

    // Perform the builtin conversion.
    if (!convertLiteralInPlace(literal, builtinLiteralType, nullptr,
                               Identifier(), DeclName(), builtinProtocol,
                               builtinLiteralFuncName, brokenProtocolDiag,
                               brokenBuiltinProtocolDiag))
      return nullptr;
  }

  // Find the witness that we'll use to initialize the literal value.
  auto witness = findNamedWitnessImpl(
                   tc, dc, type->getRValueType(), protocol,
                   literalFuncName, brokenProtocolDiag,
                   conformance);
  if (!witness || !isa<AbstractFunctionDecl>(witness.getDecl()))
    return nullptr;

  // Set the initializer.
  if (auto stringLiteral = dyn_cast<StringLiteralExpr>(literal))
    stringLiteral->setInitializer(witness);
  else
    cast<MagicIdentifierLiteralExpr>(literal)->setInitializer(witness);

  // The literal expression has this type.
  cs.setType(literal, type);

  return literal;
}

Expr *ExprRewriter::finishApply(ApplyExpr *apply, Type openedType,
                                ConstraintLocatorBuilder locator) {
  TypeChecker &tc = cs.getTypeChecker();
  
  auto fn = apply->getFn();

  bool hasTrailingClosure =
    isa<CallExpr>(apply) && cast<CallExpr>(apply)->hasTrailingClosure();

  auto finishApplyOfDeclWithSpecialTypeCheckingSemantics
    = [&](ApplyExpr *apply,
          ValueDecl *decl,
          Type openedType) -> Expr* {
      switch (cs.TC.getDeclTypeCheckingSemantics(decl)) {
      case DeclTypeCheckingSemantics::TypeOf: {
        // Resolve into a DynamicTypeExpr.
        auto arg = apply->getArg();

        SmallVector<Identifier, 2> argLabelsScratch;

        auto fnType = cs.getType(fn)->getAs<FunctionType>();
        arg = coerceCallArguments(arg, fnType,
                                  apply,
                                  apply->getArgumentLabels(argLabelsScratch),
                                  hasTrailingClosure,
                                  locator.withPathElement(
                                    ConstraintLocator::ApplyArgument));
        if (!arg) {
          return nullptr;
        }

        if (auto shuffle = dyn_cast<TupleShuffleExpr>(arg))
          arg = shuffle->getSubExpr();

        if (auto tuple = dyn_cast<TupleExpr>(arg))
          arg = tuple->getElements()[0];

        auto replacement = new (tc.Context)
          DynamicTypeExpr(apply->getFn()->getLoc(),
                          apply->getArg()->getStartLoc(),
                          arg,
                          apply->getArg()->getEndLoc(),
                          Type());
        cs.setType(replacement, simplifyType(openedType));
        return replacement;
      }
      
      case DeclTypeCheckingSemantics::WithoutActuallyEscaping: {
        // Resolve into a MakeTemporarilyEscapableExpr.
        auto arg = cast<TupleExpr>(apply->getArg());
        assert(arg->getNumElements() == 2 && "should have two arguments");
        auto nonescaping = arg->getElements()[0];
        auto body = arg->getElements()[1];
        auto bodyTy = cs.getType(body)->getWithoutSpecifierType();
        auto bodyFnTy = bodyTy->castTo<FunctionType>();
        auto escapableParams = bodyFnTy->getParams();
        auto resultType = bodyFnTy->getResult();
        
        // The body is immediately called, so is obviously noescape.
        bodyFnTy = cast<FunctionType>(
          bodyFnTy->withExtInfo(bodyFnTy->getExtInfo().withNoEscape()));
        body = coerceToType(body, bodyFnTy, locator);
        assert(body && "can't make nonescaping?!");
        
        auto escapable = new (tc.Context)
          OpaqueValueExpr(apply->getFn()->getLoc(), Type());
        cs.setType(escapable, escapableParams[0].getOldType());

        auto getType = [&](const Expr *E) -> Type {
          return cs.getType(E);
        };

        auto callSubExpr = CallExpr::createImplicit(tc.Context, body,
                                                    {escapable}, {}, getType);
        cs.cacheSubExprTypes(callSubExpr);
        cs.setType(callSubExpr->getArg(),
                   AnyFunctionType::composeInput(tc.Context,
                                                 escapableParams, false));
        cs.setType(callSubExpr, resultType);
        
        auto replacement = new (tc.Context)
          MakeTemporarilyEscapableExpr(apply->getFn()->getLoc(),
                                       apply->getArg()->getStartLoc(),
                                       nonescaping,
                                       callSubExpr,
                                       apply->getArg()->getEndLoc(),
                                       escapable,
                                       apply);
        cs.setType(replacement, resultType);
        return replacement;
      }
      
      case DeclTypeCheckingSemantics::OpenExistential: {
        // Resolve into an OpenExistentialExpr.
        auto arg = cast<TupleExpr>(apply->getArg());
        assert(arg->getNumElements() == 2 && "should have two arguments");

        auto existential = cs.coerceToRValue(arg->getElements()[0]);
        auto body = cs.coerceToRValue(arg->getElements()[1]);

        auto bodyFnTy = cs.getType(body)->castTo<FunctionType>();
        auto openedTy = getBaseType(bodyFnTy, /*wantsRValue*/ false);
        auto resultTy = bodyFnTy->getResult();

        // The body is immediately called, so is obviously noescape.
        bodyFnTy = cast<FunctionType>(
          bodyFnTy->withExtInfo(bodyFnTy->getExtInfo().withNoEscape()));
        body = coerceToType(body, bodyFnTy, locator);
        assert(body && "can't make nonescaping?!");

        auto openedInstanceTy = openedTy;
        auto existentialInstanceTy = cs.getType(existential);
        if (auto metaTy = openedTy->getAs<MetatypeType>()) {
          openedInstanceTy = metaTy->getInstanceType();
          existentialInstanceTy = existentialInstanceTy
            ->castTo<ExistentialMetatypeType>()
            ->getInstanceType();
        }
        assert(openedInstanceTy->castTo<ArchetypeType>()
                   ->getOpenedExistentialType()
                   ->isEqual(existentialInstanceTy));

        auto opaqueValue = new (tc.Context)
          OpaqueValueExpr(apply->getLoc(), openedTy);
        cs.setType(opaqueValue, openedTy);
        
        auto getType = [&](const Expr *E) -> Type {
          return cs.getType(E);
        };

        auto callSubExpr = CallExpr::createImplicit(tc.Context, body, {opaqueValue}, {}, getType);
        cs.cacheSubExprTypes(callSubExpr);
        cs.setType(callSubExpr, resultTy);
        
        auto replacement = new (tc.Context)
          OpenExistentialExpr(existential, opaqueValue, callSubExpr,
                              resultTy);
        cs.setType(replacement, resultTy);
        return replacement;
      }

      case DeclTypeCheckingSemantics::Normal:
        return nullptr;
      }

      llvm_unreachable("Unhandled DeclTypeCheckingSemantics in switch.");
    };

  // The function is always an rvalue.
  fn = cs.coerceToRValue(fn);

  // Resolve applications of decls with special semantics.
  if (auto declRef =
      dyn_cast<DeclRefExpr>(getSemanticExprForDeclOrMemberRef(fn))) {
    if (auto special =
        finishApplyOfDeclWithSpecialTypeCheckingSemantics(apply,
                                                          declRef->getDecl(),
                                                          openedType)) {
      return special;
    }
  }
  

  bool unwrapResult = false;
  if (auto *IUOFnTy = dyn_cast<ImplicitlyUnwrappedFunctionConversionExpr>(fn)) {
    unwrapResult = true;
    fn = IUOFnTy->getSubExpr();
  }

  // If we're applying a function that resulted from a covariant
  // function conversion, strip off that conversion.
  // FIXME: It would be nicer if we could build the ASTs properly in the
  // first shot.
  Type covariantResultType;
  if (auto covariant = dyn_cast<CovariantFunctionConversionExpr>(fn)) {
    // Strip off one layer of application from the covariant result.
    covariantResultType
      = cs.getType(covariant)->castTo<AnyFunctionType>()->getResult();
   
    // Use the subexpression as the function.
    fn = covariant->getSubExpr();
  }

  // An immediate application of a closure literal is always noescape.
  if (getClosureLiteralExpr(fn)) {
    if (auto fnTy = cs.getType(fn)->getAs<FunctionType>()) {
      fnTy = cast<FunctionType>(
        fnTy->withExtInfo(fnTy->getExtInfo().withNoEscape()));
      fn = coerceToType(fn, fnTy, locator);
    }
  }

  apply->setFn(fn);

  // Check whether the argument is 'super'.
  bool isSuper = apply->getArg()->isSuperExpr();

  // For function application, convert the argument to the input type of
  // the function.
  SmallVector<Identifier, 2> argLabelsScratch;
  if (auto fnType = cs.getType(fn)->getAs<FunctionType>()) {
    auto origArg = apply->getArg();
    Expr *arg = coerceCallArguments(origArg, fnType,
                                    apply,
                                    apply->getArgumentLabels(argLabelsScratch),
                                    hasTrailingClosure,
                                    locator.withPathElement(
                                      ConstraintLocator::ApplyArgument));
    if (!arg) {
      return nullptr;
    }

    apply->setArg(arg);
    cs.setType(apply, fnType->getResult());
    apply->setIsSuper(isSuper);

    // We need the layout of nominal types returned from a function call.
    if (auto nominalResult = fnType->getResult()->getAnyNominal()) {
      tc.requestNominalLayout(nominalResult);
    }

    cs.setExprTypes(apply);
    Expr *result = tc.substituteInputSugarTypeForResult(apply);
    cs.cacheExprTypes(result);

    // If we have a covariant result type, perform the conversion now.
    if (covariantResultType) {
      if (covariantResultType->is<FunctionType>())
        result = cs.cacheType(new (tc.Context) CovariantFunctionConversionExpr(
            result, covariantResultType));
      else
        result = cs.cacheType(new (tc.Context) CovariantReturnConversionExpr(
            result, covariantResultType));
    }

    // Try closing the existential, if there is one.
    closeExistential(result, locator);

    // Extract all arguments.
    auto *CEA = arg;
    if (auto *TSE = dyn_cast<TupleShuffleExpr>(CEA))
      CEA = TSE->getSubExpr();
    // The argument is either a ParenExpr or TupleExpr.
    ArrayRef<Expr *> arguments;

    SmallVector<Expr *, 1> Scratch;
    if (auto *TE = dyn_cast<TupleExpr>(CEA))
      arguments = TE->getElements();
    else if (auto *PE = dyn_cast<ParenExpr>(CEA)) {
      Scratch.push_back(PE->getSubExpr());
      arguments = makeArrayRef(Scratch);
    }
    else {
      Scratch.push_back(apply->getArg());
      arguments = makeArrayRef(Scratch);
    }

    for (auto arg: arguments) {
      bool isNoEscape = false;
      while (1) {
        if (auto AFT = cs.getType(arg)->getAs<AnyFunctionType>()) {
          isNoEscape = isNoEscape || AFT->isNoEscape();
        }

        if (auto conv = dyn_cast<ImplicitConversionExpr>(arg))
          arg = conv->getSubExpr();
        else if (auto *PE = dyn_cast<ParenExpr>(arg))
          arg = PE->getSubExpr();
        else
          break;
      }
      if (!isNoEscape) {
        if (auto DRE = dyn_cast<DeclRefExpr>(arg))
          if (auto FD = dyn_cast<FuncDecl>(DRE->getDecl())) {
            tc.addEscapingFunctionAsArgument(FD, apply);
          }
      }
    }

    if (unwrapResult)
      return forceUnwrapResult(result);

    return result;
  }

  // FIXME: handle unwrapping everywhere else
  assert(!unwrapResult);

  // If this is an UnresolvedType in the system, preserve it.
  if (cs.getType(fn)->is<UnresolvedType>()) {
    cs.setType(apply, cs.getType(fn));
    return apply;
  }

  // SWIFT_ENABLE_TENSORFLOW
  // Handle @dynamicCallable applications.
  auto isDynamicCallable = [&](Expr *base) {
    auto type = cs.getType(base);
    return type && cs.DynamicCallableCache.count(type->getCanonicalType());
  };

  if (isDynamicCallable(fn)) {
    auto &ctx = tc.Context;
    auto methods = cs.DynamicCallableCache[cs.getType(fn)->getCanonicalType()];

    if (methods.isValid()) {
      TupleExpr *arg = dyn_cast<TupleExpr>(apply->getArg());
      if (auto parenExpr = dyn_cast<ParenExpr>(apply->getArg())) {
        arg = TupleExpr::createImplicit(ctx, parenExpr->getSubExpr(), {});
      }

      // Determine whether to call the positional arguments method or the
      // keyword arguments method.
      bool useKwargsMethod = methods.argumentsMethod == nullptr;
      if (!useKwargsMethod) {
        for (auto name : arg->getElementNames()) {
          if (!name.empty()) {
            useKwargsMethod = true;
            break;
          }
        }
      }

      auto method = useKwargsMethod
        ? methods.keywordArgumentsMethod
        : methods.argumentsMethod;
      assert(method && "Dynamic call method should exist");

      auto memberType =
        cs.getTypeOfMemberReference(cs.getType(fn), method, cs.DC,
                                    /*isDynamicResult*/ false,
                                    FunctionRefKind::DoubleApply,
                                    locator).second;
      auto methodType = memberType->castTo<AnyFunctionType>();

      // Construct expression referencing the `dynamicallyCall` method.
      Expr *member =
        new (ctx) MemberRefExpr(fn, fn->getEndLoc(), ConcreteDeclRef(method),
                                DeclNameLoc(method->getNameLoc()),
                                /*Implicit*/ true);

      // Construct argument to the method (either an array or dictionary
      // expression).
      Expr *argument = nullptr;
      if (!useKwargsMethod) {
        argument = ArrayExpr::create(ctx, SourceLoc(), arg->getElements(),
                                     {}, SourceLoc());
      } else {
        SmallVector<Identifier, 4> names;
        SmallVector<Expr *, 4> dictElements;
        for (unsigned i = 0, n = arg->getNumElements(); i < n; i++) {
          Expr *labelExpr =
            new (ctx) StringLiteralExpr(arg->getElementName(i).get(),
                                        arg->getElementNameLoc(i),
                                        /*Implicit*/ true);
          Expr *pair =
            TupleExpr::createImplicit(ctx, { labelExpr, arg->getElement(i) },
                                      {});
          dictElements.push_back(pair);
        }
        argument = DictionaryExpr::create(ctx, SourceLoc(), dictElements, {},
                                          SourceLoc());
      }
      argument->setImplicit();

      // Construct call to the `dynamicallyCall` method.
      auto argumentName = methodType->getParams()[0].getLabel();
      Expr *result = CallExpr::createImplicit(ctx, member, argument,
                                              { argumentName });
      tc.typeCheckExpression(result, dc);
      cs.cacheExprTypes(result);

      return result;
    }
  } // end @dynamicCallable handling

  // We have a type constructor.
  auto metaTy = cs.getType(fn)->castTo<AnyMetatypeType>();
  auto ty = metaTy->getInstanceType();

  if (!cs.isTypeReference(fn)) {
    bool isExistentialType = false;
    // If this is an attempt to initialize existential type.
    if (auto metaType = cs.getType(fn)->getAs<MetatypeType>()) {
      auto instanceType = metaType->getInstanceType();
      isExistentialType = instanceType->isExistentialType();
    }

    if (!isExistentialType) {
      // If the metatype value isn't a type expression,
      // the user should reference '.init' explicitly, for clarity.
      cs.TC
          .diagnose(apply->getArg()->getStartLoc(),
                    diag::missing_init_on_metatype_initialization)
          .fixItInsert(apply->getArg()->getStartLoc(), ".init");
    }
  }

  // If we're "constructing" a tuple type, it's simply a conversion.
  if (auto tupleTy = ty->getAs<TupleType>()) {
    // FIXME: Need an AST to represent this properly.
    return coerceToType(apply->getArg(), tupleTy, locator);
  }

  // We're constructing a value of nominal type. Look for the constructor or
  // enum element to use.
  auto ctorLocator = cs.getConstraintLocator(
                 locator.withPathElement(ConstraintLocator::ApplyFunction)
                        .withPathElement(ConstraintLocator::ConstructorMember));
  auto selected = solution.getOverloadChoiceIfAvailable(ctorLocator);
  if (!selected) {
    assert(ty->hasError() || ty->hasUnresolvedType());
    cs.setType(apply, ty);
    return apply;
  }

  assert(ty->getNominalOrBoundGenericNominal() || ty->is<DynamicSelfType>() ||
         ty->isExistentialType() || ty->is<ArchetypeType>());

  // We have the constructor.
  auto choice = selected->choice;
  
  // Consider the constructor decl reference expr 'implicit', but the
  // constructor call expr itself has the apply's 'implicitness'.
  bool isDynamic = choice.getKind() == OverloadChoiceKind::DeclViaDynamic;
  Expr *declRef = buildMemberRef(fn, selected->openedFullType,
                                 /*dotLoc=*/SourceLoc(), choice,
                                 DeclNameLoc(fn->getEndLoc()),
                                 selected->openedType, locator, ctorLocator,
                                 /*Implicit=*/true, choice.getFunctionRefKind(),
                                 AccessSemantics::Ordinary, isDynamic);
  if (!declRef)
    return nullptr;
  declRef->setImplicit(apply->isImplicit());
  apply->setFn(declRef);

  // Tail-recur to actually call the constructor.
  return finishApply(apply, openedType, locator);
}


// Return the precedence-yielding parent of 'expr', along with the index of
// 'expr' as the child of that parent. The precedence-yielding parent is the
// nearest ancestor of 'expr' which imposes a minimum precedence on 'expr'.
// Right now that just means skipping over TupleExpr instances that only exist
// to hold arguments to binary operators.
static std::pair<Expr *, unsigned> getPrecedenceParentAndIndex(Expr *expr,
                                                               Expr *rootExpr)
{
  auto parentMap = rootExpr->getParentMap();
  auto it = parentMap.find(expr);
  if (it == parentMap.end()) {
    return { nullptr, 0 };
  }
  Expr *parent = it->second;

  // Handle all cases where the answer isn't just going to be { parent, 0 }.
  if (auto tuple = dyn_cast<TupleExpr>(parent)) {
    // Get index of expression in tuple.
    auto tupleElems = tuple->getElements();
    auto elemIt = std::find(tupleElems.begin(), tupleElems.end(), expr);
    assert(elemIt != tupleElems.end() && "expr not found in parent TupleExpr");
    unsigned index = elemIt - tupleElems.begin();

    it = parentMap.find(parent);
    if (it != parentMap.end()) {
      Expr *gparent = it->second;

      // Was this tuple just constructed for a binop?
      if (isa<BinaryExpr>(gparent)) {
        return { gparent, index };
      }
    }

    // Must be a tuple literal, function arg list, collection, etc.
    return { parent, index };
  } else if (auto ifExpr = dyn_cast<IfExpr>(parent)) {
    unsigned index;
    if (expr == ifExpr->getCondExpr()) {
      index = 0;
    } else if (expr == ifExpr->getThenExpr()) {
      index = 1;
    } else if (expr == ifExpr->getElseExpr()) {
      index = 2;
    } else {
      llvm_unreachable("expr not found in parent IfExpr");
    }
    return { ifExpr, index };
  } else if (auto assignExpr = dyn_cast<AssignExpr>(parent)) {
    unsigned index;
    if (expr == assignExpr->getSrc()) {
      index = 0;
    } else if (expr == assignExpr->getDest()) {
      index = 1;
    } else {
      llvm_unreachable("expr not found in parent AssignExpr");
    }
    return { assignExpr, index };
  }

  return { parent, 0 };
}

/// Return true if, when replacing "<expr>" with "<expr> op <something>",
/// parentheses must be added around "<expr>" to allow the new operator
/// to bind correctly.
bool swift::exprNeedsParensInsideFollowingOperator(
    TypeChecker &TC, DeclContext *DC, Expr *expr,
    PrecedenceGroupDecl *followingPG) {
  if (expr->isInfixOperator()) {
    auto exprPG = TC.lookupPrecedenceGroupForInfixOperator(DC, expr);
    if (!exprPG) return true;

    return TC.Context.associateInfixOperators(exprPG, followingPG)
             != Associativity::Left;
  }

  // We want to parenthesize a 'try?' on the LHS, but we don't care about
  // capturing the new operator inside a 'try' or 'try!'.
  if (isa<OptionalTryExpr>(expr))
    return true;

  return false;
}

/// Return true if, when replacing "<expr>" with "<expr> op <something>"
/// within the given root expression, parentheses must be added around
/// the new operator to prevent it from binding incorrectly in the
/// surrounding context.
bool swift::exprNeedsParensOutsideFollowingOperator(
    TypeChecker &TC, DeclContext *DC, Expr *expr, Expr *rootExpr,
    PrecedenceGroupDecl *followingPG) {
  Expr *parent;
  unsigned index;
  std::tie(parent, index) = getPrecedenceParentAndIndex(expr, rootExpr);
  if (!parent || isa<TupleExpr>(parent) || isa<ParenExpr>(parent)) {
    return false;
  }

  if (parent->isInfixOperator()) {
    auto parentPG = TC.lookupPrecedenceGroupForInfixOperator(DC, parent);
    if (!parentPG) return true;

    // If the index is 0, this is on the LHS of the parent.
    if (index == 0) {
      return TC.Context.associateInfixOperators(followingPG, parentPG)
               != Associativity::Left;
    } else {
      return TC.Context.associateInfixOperators(parentPG, followingPG)
               != Associativity::Right;
    }
  }

  return true;
}

bool swift::exprNeedsParensBeforeAddingNilCoalescing(TypeChecker &TC,
                                                     DeclContext *DC,
                                                     Expr *expr) {
  auto asPG =
    TC.lookupPrecedenceGroup(DC, DC->getASTContext().Id_NilCoalescingPrecedence,
                             SourceLoc());
  if (!asPG) return true;
  return exprNeedsParensInsideFollowingOperator(TC, DC, expr, asPG);
}

bool swift::exprNeedsParensAfterAddingNilCoalescing(TypeChecker &TC,
                                                    DeclContext *DC,
                                                    Expr *expr,
                                                    Expr *rootExpr) {
  auto asPG =
    TC.lookupPrecedenceGroup(DC, DC->getASTContext().Id_NilCoalescingPrecedence,
                             SourceLoc());
  if (!asPG) return true;
  return exprNeedsParensOutsideFollowingOperator(TC, DC, expr, rootExpr, asPG);
}

namespace {
  class ExprWalker : public ASTWalker {
    ExprRewriter &Rewriter;
    SmallVector<ClosureExpr *, 4> ClosuresToTypeCheck;

  public:
    ExprWalker(ExprRewriter &Rewriter) : Rewriter(Rewriter) { }

    const SmallVectorImpl<ClosureExpr *> &getClosuresToTypeCheck() const {
      return ClosuresToTypeCheck;
    }

    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      // For closures, update the parameter types and check the body.
      if (auto closure = dyn_cast<ClosureExpr>(expr)) {
        Rewriter.simplifyExprType(expr);
        auto &cs = Rewriter.getConstraintSystem();
        auto &tc = cs.getTypeChecker();

        // Coerce the pattern, in case we resolved something.
        auto fnType = cs.getType(closure)->castTo<FunctionType>();
        auto *params = closure->getParameters();
        if (tc.coerceParameterListToType(params, closure, fnType))
          return { false, nullptr };

        // Require layout of dependent types that could be used to materialize
        // metadata types/conformances during IRGen.
        tc.requestRequiredNominalTypeLayoutForParameters(params);

        // If this is a single-expression closure, convert the expression
        // in the body to the result type of the closure.
        if (closure->hasSingleExpressionBody()) {
          // Enter the context of the closure when type-checking the body.
          llvm::SaveAndRestore<DeclContext *> savedDC(Rewriter.dc, closure);
          Expr *body = closure->getSingleExpressionBody()->walk(*this);
          if (!body)
            return { false, nullptr };
          
          if (body != closure->getSingleExpressionBody())
            closure->setSingleExpressionBody(body);
          
          if (body) {
            // A single-expression closure with a non-Void expression type
            // coerces to a Void-returning function type.
            if (fnType->getResult()->isVoid() && !cs.getType(body)->isVoid()) {
              closure = Rewriter.coerceClosureExprToVoid(closure);
            // A single-expression closure with a Never expression type
            // coerces to any other function type.
            } else if (cs.getType(body)->isUninhabited()) {
              closure = Rewriter.coerceClosureExprFromNever(closure);
            } else {
            
              body = Rewriter.coerceToType(body,
                                           fnType->getResult(),
                                           cs.getConstraintLocator(
                                             closure,
                                             ConstraintLocator::ClosureResult));
              if (!body)
                return { false, nullptr };

              closure->setSingleExpressionBody(body);
            }
          }
        } else {
          // For other closures, type-check the body once we've finished with
          // the expression.
          cs.setExprTypes(closure);
          ClosuresToTypeCheck.push_back(closure);
        }

        tc.ClosuresWithUncomputedCaptures.push_back(closure);

        return { false, closure };
      }

      Rewriter.walkToExprPre(expr);
      return { true, expr };
    }

    Expr *walkToExprPost(Expr *expr) override {
      Expr *result = Rewriter.walkToExprPost(expr);
      auto &cs = Rewriter.getConstraintSystem();
      if (result && cs.hasType(result))
        Rewriter.checkForImportedUsedConformances(cs.getType(result));
      return result;
    }

    /// \brief Ignore statements.
    std::pair<bool, Stmt *> walkToStmtPre(Stmt *stmt) override {
      return { false, stmt };
    }

    /// \brief Ignore declarations.
    bool walkToDeclPre(Decl *decl) override { return false; }
  };
} // end anonymous namespace

Expr *ConstraintSystem::coerceToRValue(Expr *expr) {
  auto &tc = getTypeChecker();
  return tc.coerceToRValue(expr,
                           [&](Expr *expr) {
                             return getType(expr);
                           },
                           [&](Expr *expr, Type type) {
                             setType(expr, type);
                           });
}

/// Emit the fixes computed as part of the solution, returning true if we were
/// able to emit an error message, or false if none of the fixits worked out.
bool ConstraintSystem::applySolutionFixes(Expr *E, const Solution &solution) {
  llvm::SmallDenseMap<Expr *, SmallVector<const ConstraintFix *, 4>>
      fixesPerExpr;

  applySolution(solution);

  for (auto *fix : solution.Fixes)
    fixesPerExpr[fix->getAnchor()].push_back(fix);

  auto diagnoseExprFailures = [&](Expr *expr) -> bool {
    auto fixes = fixesPerExpr.find(expr);
    if (fixes == fixesPerExpr.end())
      return false;

    bool diagnosed = false;
    for (const auto *fix : fixes->second)
      diagnosed |= fix->diagnose(E);
    return diagnosed;
  };

  bool diagnosed = false;
  E->forEachChildExpr([&](Expr *subExpr) -> Expr * {
    // Diagnose root expression at the end to
    // preserve ordering.
    if (subExpr != E)
      diagnosed |= diagnoseExprFailures(subExpr);
    return subExpr;
  });

  diagnosed |= diagnoseExprFailures(E);
  return diagnosed;
}

/// \brief Apply a given solution to the expression, producing a fully
/// type-checked expression.
Expr *ConstraintSystem::applySolution(Solution &solution, Expr *expr,
                                      Type convertType,
                                      bool discardedExpr,
                                      bool skipClosures) {
  // If any fixes needed to be applied to arrive at this solution, resolve
  // them to specific expressions.
  if (!solution.Fixes.empty()) {
    if (shouldSuppressDiagnostics())
      return nullptr;

    // If we can diagnose the problem with the fixits that we've pre-assumed,
    // do so now.
    if (applySolutionFixes(expr, solution))
      return nullptr;

    // If we didn't manage to diagnose anything well, so fall back to
    // diagnosing mining the system to construct a reasonable error message.
    diagnoseFailureForExpr(expr);
    return nullptr;
  }

  // Mark any normal conformances used in this solution as "used".
  for (auto &e : solution.Conformances)
    TC.markConformanceUsed(e.second, DC);

  ExprRewriter rewriter(*this, solution, shouldSuppressDiagnostics());
  ExprWalker walker(rewriter);

  // Apply the solution to the expression.
  auto result = expr->walk(walker);
  if (!result)
    return nullptr;

  // If we're re-typechecking an expression for diagnostics, don't
  // visit closures that have non-single expression bodies.
  if (!skipClosures) {
    auto &tc = getTypeChecker();
    bool hadError = false;
    for (auto *closure : walker.getClosuresToTypeCheck())
      hadError |= tc.typeCheckClosureBody(closure);
    
    // If any of the closures failed to type check, bail.
    if (hadError)
      return nullptr;
  }
  
  
  // If we're supposed to convert the expression to some particular type,
  // do so now.
  if (convertType) {
    result = rewriter.coerceToType(result, convertType,
                                   getConstraintLocator(expr));
    if (!result)
      return nullptr;
  } else if (getType(result)->hasLValueType() && !discardedExpr) {
    // We referenced an lvalue. Load it.
    result = rewriter.coerceToType(result, getType(result)->getRValueType(),
                                   getConstraintLocator(expr));
  }

  if (result)
    rewriter.finalize(result);

  return result;
}

Expr *ConstraintSystem::applySolutionShallow(const Solution &solution,
                                             Expr *expr,
                                             bool suppressDiagnostics) {
  ExprRewriter rewriter(*this, solution, suppressDiagnostics);
  rewriter.walkToExprPre(expr);
  Expr *result = rewriter.walkToExprPost(expr);
  if (result)
    rewriter.finalize(result);
  return result;
}

Expr *Solution::coerceToType(Expr *expr, Type toType,
                             ConstraintLocator *locator,
                             bool ignoreTopLevelInjection,
                             Optional<Pattern*> typeFromPattern) const {
  auto &cs = getConstraintSystem();
  ExprRewriter rewriter(cs, *this, /*suppressDiagnostics=*/false);
  Expr *result = rewriter.coerceToType(expr, toType, locator, typeFromPattern);
  if (!result)
    return nullptr;

  // If we were asked to ignore top-level optional injections, mark
  // the top-level injection (if any) as "diagnosed".
  if (ignoreTopLevelInjection) {
    if (auto injection = dyn_cast<InjectIntoOptionalExpr>(
                           result->getSemanticsProvidingExpr())) {
      rewriter.DiagnosedOptionalInjections.insert(injection);
    }
  }

  rewriter.finalize(result);
  return result;
}

// Determine whether this is a variadic witness.
static bool isVariadicWitness(AbstractFunctionDecl *afd) {
  for (auto param : *afd->getParameters())
    if (param->isVariadic())
      return true;

  return false;
}

static bool argumentNamesMatch(Type argTy, ArrayRef<Identifier> names) {
  auto tupleType = argTy->getAs<TupleType>();
  if (!tupleType)
    return names.size() == 1 && names[0].empty();

  if (tupleType->getNumElements() != names.size())
    return false;

  for (unsigned i = 0, n = tupleType->getNumElements(); i != n; ++i) {
    if (tupleType->getElement(i).getName() != names[i])
      return false;
  }

  return true;
}

Expr *TypeChecker::callWitness(Expr *base, DeclContext *dc,
                               ProtocolDecl *protocol,
                               ProtocolConformanceRef conformance,
                               DeclName name,
                               MutableArrayRef<Expr *> arguments,
                               Diag<> brokenProtocolDiag) {
  // Construct an empty constraint system and solution.
  ConstraintSystem cs(*this, dc, ConstraintSystemOptions());

  for (auto *arg : arguments)
    cs.cacheType(arg);

  // Find the witness we need to use.
  auto type = base->getType();
  assert(!type->hasTypeVariable() &&
         "Building call to witness with unresolved base type!");

  if (auto metaType = type->getAs<AnyMetatypeType>())
    type = metaType->getInstanceType();
  
  auto witness = findNamedWitnessImpl(
                   *this, dc, type->getRValueType(), protocol,
                   name, brokenProtocolDiag);
  if (!witness || !isa<AbstractFunctionDecl>(witness.getDecl()))
    return nullptr;

  auto *witnessFn = cast<AbstractFunctionDecl>(witness.getDecl());

  // Form a syntactic expression that describes the reference to the
  // witness.
  // FIXME: Egregious hack.
  auto unresolvedDot = new (Context) UnresolvedDotExpr(
                                       base, SourceLoc(),
                                       witness.getDecl()->getFullName(),
                                       DeclNameLoc(base->getEndLoc()),
                                       /*Implicit=*/true);
  unresolvedDot->setFunctionRefKind(FunctionRefKind::SingleApply);
  auto dotLocator = cs.getConstraintLocator(unresolvedDot);

  // Form a reference to the witness itself.
  Type openedFullType, openedType;
  std::tie(openedFullType, openedType)
    = cs.getTypeOfMemberReference(base->getType(), witness.getDecl(), dc,
                                  /*isDynamicResult=*/false,
                                  FunctionRefKind::DoubleApply,
                                  dotLocator);

  auto getType = [&](const Expr *E) -> Type {
    return cs.getType(E);
  };

  // Form the call argument.
  // FIXME: Standardize all callers to always provide all argument names,
  // rather than hack around this.
  CallExpr *call;
  auto argLabels = witness.getDecl()->getFullName().getArgumentNames();
  if (arguments.size() == 1 &&
      (isVariadicWitness(witnessFn) ||
       argumentNamesMatch(cs.getType(arguments[0]), argLabels))) {
    call = CallExpr::create(Context, unresolvedDot, arguments[0], {}, {},
                            /*hasTrailingClosure=*/false,
                            /*implicit=*/true, Type(), getType);
  } else {
    // The tuple should have the source range enclosing its arguments unless
    // they are invalid or there are no arguments.
    SourceLoc TupleStartLoc = base->getStartLoc();
    SourceLoc TupleEndLoc = base->getEndLoc();
    if (!arguments.empty()) {
      SourceLoc AltStartLoc = arguments.front()->getStartLoc();
      SourceLoc AltEndLoc = arguments.back()->getEndLoc();
      if (AltStartLoc.isValid() && AltEndLoc.isValid()) {
        TupleStartLoc = AltStartLoc;
        TupleEndLoc = AltEndLoc;
      }
    }

    call = CallExpr::create(Context, unresolvedDot, TupleStartLoc, arguments,
                            argLabels, {}, TupleEndLoc,
                            /*trailingClosure=*/nullptr,
                            /*implicit=*/true, getType);
  }

  cs.cacheSubExprTypes(call);

  // Extract the arguments.
  SmallVector<AnyFunctionType::Param, 8> args;
  AnyFunctionType::decomposeInput(cs.getType(call->getArg()), args);

  // Add the conversion from the argument to the function parameter type.
  auto openedFuncType = openedType->castTo<FunctionType>();
  ::matchCallArguments(
      cs, args, openedFuncType->getParams(),
      cs.getConstraintLocator(call, ConstraintLocator::ApplyArgument));

  // Solve the system.
  SmallVector<Solution, 1> solutions;
  
  cs.cacheExprTypes(call);

  // If the system failed to produce a solution, post any available diagnostics.
  if (cs.solve(call, solutions) || solutions.size() != 1) {
    cs.salvage(solutions, base);
    return nullptr;
  }

  Solution &solution = solutions.front();
  ExprRewriter rewriter(cs, solution,
                        /*suppressDiagnostics=*/false);

  auto choice =
      OverloadChoice(openedFullType, witnessFn, FunctionRefKind::SingleApply);
  auto memberRef = rewriter.buildMemberRef(
      base, openedFullType, base->getStartLoc(), choice,
      DeclNameLoc(base->getEndLoc()), openedType, dotLocator, dotLocator,
      /*Implicit=*/true, FunctionRefKind::SingleApply,
      AccessSemantics::Ordinary,
      /*isDynamic=*/false);
  call->setFn(memberRef);

  // Call the witness.
  Expr *result = rewriter.finishApply(call, openedType,
                                      cs.getConstraintLocator(call));
  if (!result)
    return nullptr;

  rewriter.finalize(result);
  return result;
}

Expr *
Solution::convertBooleanTypeToBuiltinI1(Expr *expr,
                                        ConstraintLocator *locator) const {
  auto &cs = getConstraintSystem();

  // Load lvalues here.
  expr = cs.coerceToRValue(expr);

  auto &tc = cs.getTypeChecker();
  auto &ctx = tc.Context;

  auto type = cs.getType(expr);

  // We allow UnresolvedType <c $T for all $T, so we might end up here
  // in diagnostics. Just bail out.
  if (type->is<UnresolvedType>())
    return expr;

  // Look for the builtin name. If we don't have it, we need to call the
  // general name via the witness table.
  NameLookupOptions lookupOptions = defaultMemberLookupOptions;
  if (isa<AbstractFunctionDecl>(cs.DC))
    lookupOptions |= NameLookupFlags::KnownPrivate;
  auto members = tc.lookupMember(cs.DC, type,
                                 tc.Context.Id_getBuiltinLogicValue,
                                 lookupOptions);

  // Find the builtin method.
  if (members.size() != 1) {
    tc.diagnose(expr->getLoc(), diag::broken_bool);
    return expr;
  }
  auto *builtinMethod = dyn_cast<FuncDecl>(members[0].getValueDecl());
  if (!builtinMethod) {
    tc.diagnose(expr->getLoc(), diag::broken_bool);
    return expr;
  }

  // The method is not generic, so there are no substitutions.
  tc.validateDeclForNameLookup(builtinMethod);
  auto builtinMethodType = builtinMethod->getInterfaceType()
    ->castTo<FunctionType>();

  // Form an unbound reference to the builtin method.
  auto *declRef = new (ctx) DeclRefExpr(builtinMethod,
                                        DeclNameLoc(expr->getLoc()),
                                        /*Implicit=*/true);
  declRef->setFunctionRefKind(FunctionRefKind::DoubleApply);
  cs.setType(declRef, builtinMethodType);

  auto getType = [&](const Expr *E) -> Type {
    return cs.getType(E);
  };

  // Apply 'self' to get the method value.
  auto *methodRef = new (ctx) DotSyntaxCallExpr(declRef,
                                                SourceLoc(),
                                                expr);
  cs.setType(methodRef, builtinMethodType->getResult());

  // Apply the empty argument list to get the final result.
  auto *result = CallExpr::createImplicit(ctx, methodRef,
                                          { }, { }, getType);
  cs.setType(result, builtinMethodType->getResult()
                  ->castTo<FunctionType>()->getResult());
  cs.setType(result->getArg(), ctx.TheEmptyTupleType);

  if (!cs.getType(result)->isBuiltinIntegerType(1)) {
    tc.diagnose(expr->getLoc(), diag::broken_bool);
    return result;
  }

  return result;
}

Expr *Solution::convertOptionalToBool(Expr *expr,
                                      ConstraintLocator *locator) const {
  auto &cs = getConstraintSystem();
  auto &tc = cs.getTypeChecker();
  tc.requireOptionalIntrinsics(expr->getLoc());

  // Match the optional value against its `Some` case.
  auto &ctx = tc.Context;
  auto isSomeExpr = new (ctx) EnumIsCaseExpr(expr, ctx.getOptionalSomeDecl());
  cs.setType(isSomeExpr, tc.lookupBoolType(cs.DC));
  return isSomeExpr;
}
