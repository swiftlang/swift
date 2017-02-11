//===--- CSApply.cpp - Constraint Application -----------------------------===//
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
// This file implements application of a solution to a constraint
// system to a particular expression, resulting in a
// fully-type-checked expression.
//
//===----------------------------------------------------------------------===//

#include "ConstraintSystem.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/Basic/StringExtras.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/SaveAndRestore.h"

using namespace swift;
using namespace constraints;

/// \brief Get a substitution corresponding to the type witness.
/// Inspired by ProtocolConformance::getTypeWitnessByName.
const Substitution *
getTypeWitnessByName(ProtocolConformance *conformance,
                     Identifier name,
                     LazyResolver *resolver) {
  // Find the named requirement.
  AssociatedTypeDecl *assocType = nullptr;
  auto members = conformance->getProtocol()->lookupDirect(name);
  for (auto member : members) {
    assocType = dyn_cast<AssociatedTypeDecl>(member);
    if (assocType)
      break;
  }

  if (!assocType)
    return nullptr;

  assert(conformance && "Missing conformance information");
  return &conformance->getTypeWitness(assocType, resolver);
}


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

  SmallVector<ProtocolDecl *, 2> protocols;
  existential->isExistentialType(protocols);
  return protocols.size() == 1 &&
         protocols[0]->isSpecificProtocol(KnownProtocolKind::AnyObject);
}

Type Solution::computeSubstitutions(
       GenericSignature *sig,
       Type openedType,
       ConstraintLocator *locator,
       SmallVectorImpl<Substitution> &result) const {
  auto &tc = getConstraintSystem().getTypeChecker();

  // Produce the concrete form of the opened type.
  Type type = simplifyType(openedType);

  // Gather the substitutions from dependent types to concrete types.
  auto openedTypes = OpenedTypes.find(locator);
  if (openedTypes == OpenedTypes.end())
    return type;
  TypeSubstitutionMap subs;
  for (const auto &opened : openedTypes->second) {
    subs[opened.first->castTo<GenericTypeParamType>()] =
      getFixedType(opened.second);
  }

  auto lookupConformanceFn =
      [&](CanType original, Type replacement, ProtocolType *protoType)
          -> Optional<ProtocolConformanceRef> {
    if (replacement->hasError() ||
        isOpenedAnyObject(replacement) ||
        replacement->is<GenericTypeParamType>()) {
      return ProtocolConformanceRef(protoType->getDecl());
    }

    return tc.conformsToProtocol(replacement,
                                 protoType->getDecl(),
                                 getConstraintSystem().DC,
                                 (ConformanceCheckFlags::InExpression|
                                  ConformanceCheckFlags::Used));
  };

  sig->getSubstitutions(subs, lookupConformanceFn, result);
  return type;
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
/// \returns The named witness, or nullptr if no witness could be found.
template <typename DeclTy>
static DeclTy *findNamedWitnessImpl(
                        TypeChecker &tc, DeclContext *dc, Type type,
                        ProtocolDecl *proto, DeclName name,
                        Diag<> diag,
                        Optional<ProtocolConformanceRef> conformance = None) {
  // Find the named requirement.
  DeclTy *requirement = nullptr;
  for (auto member : proto->getMembers()) {
    auto d = dyn_cast<DeclTy>(member);
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
  if (!conformance->isConcrete())
    return requirement;
  auto concrete = conformance->getConcrete();
  // FIXME: Dropping substitutions here.
  return cast_or_null<DeclTy>(concrete->getWitness(requirement, &tc).getDecl());
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
  if (!member->hasFixedLayout(DC->getParentModule(),
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

  // If the value is always directly accessed from this context, do it.
  return member->getAccessSemanticsFromContext(DC);
}

void ConstraintSystem::propagateLValueAccessKind(Expr *E,
                                                 AccessKind accessKind,
                                                 bool allowOverwrite) {
  E->propagateLValueAccessKind(accessKind,
                               [&](Expr *E) -> Type {
                                 return getType(E);
                               },
                               allowOverwrite);
}

bool ConstraintSystem::isTypeReference(Expr *E) {
  return E->isTypeReference([&](const Expr *E) -> Type { return getType(E); });
}

bool ConstraintSystem::isStaticallyDerivedMetatype(Expr *E) {
  return E->isStaticallyDerivedMetatype(
      [&](const Expr *E) -> Type { return getType(E); });
}

Type ConstraintSystem::getInstanceType(TypeExpr *E) {
  return E->getInstanceType([&](const Expr *E) -> bool { return hasType(E); },
                            [&](const Expr *E) -> Type { return getType(E); });
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
    bool SkipClosures;

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

    /// \brief Coerce the given scalar value to the given tuple type.
    ///
    /// \param expr The expression to be coerced.
    /// \param toTuple The tuple type to which the expression will be coerced.
    /// \param toScalarIdx The index of the scalar field within the tuple type
    /// \c toType.
    /// \param locator Locator describing where this conversion occurs.
    ///
    /// \returns The coerced expression, whose type will be equivalent to
    /// \c toTuple.
    Expr *coerceScalarToTuple(Expr *expr, TupleType *toTuple,
                              int toScalarIdx,
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
    Expr *coerceImplicitlyUnwrappedOptionalToValue(Expr *expr, Type objTy,
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

  public:
    /// \brief Build a reference to the given declaration.
    Expr *buildDeclRef(ValueDecl *decl, DeclNameLoc loc, Type openedType,
                       ConstraintLocatorBuilder locator,
                       bool specialized, bool implicit,
                       FunctionRefKind functionRefKind,
                       AccessSemantics semantics) {
      // Determine the declaration selected for this overloaded reference.
      auto &ctx = cs.getASTContext();
      
      // If this is a member of a nominal type, build a reference to the
      // member with an implied base type.
      if (decl->getDeclContext()->isTypeContext() && isa<FuncDecl>(decl)) {
        assert(cast<FuncDecl>(decl)->isOperator() && "Must be an operator");

        auto openedFnType = openedType->castTo<FunctionType>();
        auto simplifiedFnType
          = simplifyType(openedFnType)->castTo<FunctionType>();
        auto baseTy = simplifiedFnType->getInput()->getRValueInstanceType();

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
              tc.conformsToProtocol(baseTy, proto, cs.DC,
                                    (ConformanceCheckFlags::InExpression|
                                         ConformanceCheckFlags::Used));
            if (conformance && conformance->isConcrete()) {
              if (auto witnessRef =
                    conformance->getConcrete()->getWitness(decl, &tc)) {
                // Hack up an AST that we can type-check (independently) to get
                // it into the right form.
                // FIXME: the hop through 'getDecl()' is because
                // SpecializedProtocolConformance doesn't substitute into
                // witnesses' ConcreteDeclRefs.
                Type expectedFnType = simplifiedFnType->getResult();
                Expr *refExpr;
                ValueDecl *witness = witnessRef.getDecl();
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

                if (tc.typeCheckExpression(refExpr, cs.DC,
                                           TypeLoc::withoutLoc(expectedFnType),
                                           CTP_CannotFail))
                  return nullptr;

                cs.cacheExprTypes(refExpr);

               // Remove an outer function-conversion expression. This
               // happens when we end up referring to a witness for a
               // superclass conformance, and 'Self' differs.
               if (auto fnConv = dyn_cast<FunctionConversionExpr>(refExpr))
                 refExpr = fnConv->getSubExpr();

                return refExpr;
              }
            }
          }
        }

        // Build a reference to the protocol requirement.
        Expr *base =
          TypeExpr::createImplicitHack(loc.getBaseNameLoc(), baseTy, ctx);
        cs.cacheExprTypes(base);

        return buildMemberRef(base, openedType, SourceLoc(), decl,
                              loc, openedFnType->getResult(),
                              locator, locator, implicit, functionRefKind,
                              semantics, /*isDynamic=*/false);
      }

      // If this is a declaration with generic function type, build a
      // specialized reference to it.
      if (auto genericFn
            = decl->getInterfaceType()->getAs<GenericFunctionType>()) {
        SmallVector<Substitution, 4> substitutions;
        auto type = solution.computeSubstitutions(
                      genericFn->getGenericSignature(), openedType,
                      getConstraintSystem().getConstraintLocator(locator),
                      substitutions);
        auto declRefExpr =
          new (ctx) DeclRefExpr(ConcreteDeclRef(ctx, decl, substitutions),
                                loc, implicit, semantics, type);
        cs.cacheType(declRefExpr);
        declRefExpr->setFunctionRefKind(functionRefKind);
        return declRefExpr;
      }

      auto type = simplifyType(openedType);
        
      // If we've ended up trying to assign an inout type here, it means we're
      // missing an ampersand in front of the ref.
      if (auto inoutType = type->getAs<InOutType>()) {
        auto &tc = cs.getTypeChecker();
        tc.diagnose(loc.getBaseNameLoc(), diag::missing_address_of,
                    inoutType->getInOutObjectType())
          .fixItInsert(loc.getBaseNameLoc(), "&");
        return nullptr;
      }
        
      auto declRefExpr = new (ctx) DeclRefExpr(decl, loc, implicit, semantics,
                                               type);
      cs.cacheType(declRefExpr);
      declRefExpr->setFunctionRefKind(functionRefKind);
      return declRefExpr;
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
      if (auto func = dyn_cast<AbstractFunctionDecl>(member)) {
        // For functions, close the existential once the function
        // has been fully applied.
        return func->getNumParameterLists();
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
      } else if (auto memberRef = dyn_cast<MemberRefExpr>(expr)) {
        return memberRef->getBase();
      } else if (auto dynMemberRef = dyn_cast<DynamicMemberRefExpr>(expr)) {
        return dynMemberRef->getBase();
      } else if (auto subscriptRef = dyn_cast<SubscriptExpr>(expr)) {
        return subscriptRef->getBase();
      } else if (auto dynSubscriptRef = dyn_cast<DynamicSubscriptExpr>(expr)) {
        return dynSubscriptRef->getBase();
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
           isMetatype || baseTy->isClassExistentialType())) {
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
        }
      }

      // If the opaque value has an l-value access kind, then
      // the OpenExistentialExpr isn't making a derived l-value, which
      // means this is our only chance to propagate the l-value access kind
      // down to the original existential value.  Otherwise, propagateLVAK
      // will handle this.
      if (record.OpaqueValue->hasLValueAccessKind())
        cs.propagateLValueAccessKind(record.ExistentialValue,
                                  record.OpaqueValue->getLValueAccessKind());

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
                         ValueDecl *member, DeclNameLoc memberLoc,
                         Type openedType, ConstraintLocatorBuilder locator,
                         ConstraintLocatorBuilder memberLocator,
                         bool Implicit, FunctionRefKind functionRefKind,
                         AccessSemantics semantics, bool isDynamic) {
      auto &tc = cs.getTypeChecker();
      auto &context = tc.Context;

      bool isSuper = base->isSuperExpr();

      Type baseTy = cs.getType(base)->getRValueType();

      // Explicit member accesses are permitted to implicitly look
      // through ImplicitlyUnwrappedOptional<T>.
      if (!Implicit) {
        if (auto objTy = cs.lookThroughImplicitlyUnwrappedOptionalType(baseTy)) {
          base = coerceImplicitlyUnwrappedOptionalToValue(base, objTy, locator);
          baseTy = objTy;
        }
      }

      // Figure out the actual base type, and whether we have an instance of
      // that type or its metatype.
      bool baseIsInstance = true;
      if (auto baseMeta = baseTy->getAs<AnyMetatypeType>()) {
        baseIsInstance = false;
        baseTy = baseMeta->getInstanceType();
        // If the member is a constructor, verify that it can be legally
        // referenced from this base.
        if (auto ctor = dyn_cast<ConstructorDecl>(member)) {
          cs.setExprTypes(base);
          if (!tc.diagnoseInvalidDynamicConstructorReferences(base, memberLoc,
                                           baseMeta, ctor, SuppressDiagnostics))
            return nullptr;
        }
      }

      // Produce a reference to the member and the type produced by the
      // reference itself.
      ConcreteDeclRef memberRef;
      Type refTy;
      if (auto *sig = member->getInnermostDeclContext()
              ->getGenericSignatureOfContext()) {
        // We require substitutions. Figure out what they are.

        // Build a reference to the generic member.
        SmallVector<Substitution, 4> substitutions;
        refTy = solution.computeSubstitutions(
                  sig,
                  openedFullType,
                  getConstraintSystem().getConstraintLocator(memberLocator),
                  substitutions);

        memberRef = ConcreteDeclRef(context, member, substitutions);
      } else {
        // No substitutions required; the declaration reference is simple.
        memberRef = member;
        refTy = openedFullType;
      }

      // If we're referring to the member of a module, it's just a simple
      // reference.
      if (baseTy->is<ModuleType>()) {
        assert(semantics == AccessSemantics::Ordinary &&
               "Direct property access doesn't make sense for this");
        auto ref = new (context) DeclRefExpr(memberRef, memberLoc, Implicit);
        cs.setType(ref, refTy);
        ref->setFunctionRefKind(functionRefKind);
        return cs.cacheType(new (context)
                                DotSyntaxBaseIgnoredExpr(base, dotLoc, ref,
                                                         cs.getType(ref)));
      }

      // Produce a reference to the type of the container the member
      // resides in.
      Type containerTy =
          openedFullType->castTo<FunctionType>()
              ->getInput()->getRValueInstanceType();
      containerTy = solution.simplifyType(containerTy);

      // If we opened up an existential when referencing this member, update
      // the base accordingly.
      auto knownOpened = solution.OpenedExistentialTypes.find(
                           getConstraintSystem().getConstraintLocator(
                             memberLocator));
      bool openedExistential = false;
      if (knownOpened != solution.OpenedExistentialTypes.end()) {
        base = openExistentialReference(base, knownOpened->second, member);
        baseTy = knownOpened->second;
        containerTy = baseTy;
        openedExistential = true;
      }

      // If this is a method whose result type is dynamic Self, or a
      // construction, replace the result type with the actual object type.
      Type dynamicSelfFnType;
      if (!member->getDeclContext()->getAsProtocolOrProtocolExtensionContext()) {
        if (auto func = dyn_cast<AbstractFunctionDecl>(member)) {
          if ((isa<FuncDecl>(func) &&
               cast<FuncDecl>(func)->hasDynamicSelf()) ||
              (isa<ConstructorDecl>(func) &&
               containerTy->getClassOrBoundGenericClass())) {
            refTy = refTy->replaceCovariantResultType(
                containerTy, func->getNumParameterLists());
            if (!baseTy->isEqual(containerTy)) {
              dynamicSelfFnType = refTy->replaceCovariantResultType(
                  baseTy, func->getNumParameterLists());
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
        Type selfTy = containerTy;
        if (selfTy->isEqual(baseTy))
          if (cs.getType(base)->is<LValueType>())
            selfTy = InOutType::get(selfTy);
        base = coerceObjectArgumentToType(
                 base, selfTy, member, semantics,
                 locator.withPathElement(ConstraintLocator::MemberRefBase));
      } else {
        // Convert the base to an rvalue of the appropriate metatype.
        base = coerceToType(base,
                            MetatypeType::get(containerTy),
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
        if (!base) return nullptr;
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

        return ref;
      }

      // For types and properties, build member references.
      if (isa<TypeDecl>(member) || isa<VarDecl>(member)) {
        assert(!dynamicSelfFnType && "Converted type doesn't make sense here");
        if (!baseIsInstance && member->isInstanceMember()) {
          assert(memberLocator.getBaseLocator() && 
                 cs.UnevaluatedRootExprs.count(
                   memberLocator.getBaseLocator()->getAnchor()) &&
                 "Attempt to reference an instance member of a metatype");
          auto baseInstanceTy = cs.getType(base)->getRValueInstanceType();
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
        return result;
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
        apply = new (context) ConstructorRefCallExpr(ref, base);
      } else if (!baseIsInstance && member->isInstanceMember()) {
        // Reference to an unbound instance method.
        Expr *result = new (context) DotSyntaxBaseIgnoredExpr(base, dotLoc,
                                                              ref,
                                                              cs.getType(ref));
        cs.cacheType(result);
        closeExistential(result, locator, /*force=*/openedExistential);
        return result;
      } else {
        assert((!baseIsInstance || member->isInstanceMember()) &&
               "can't call a static method on an instance");
        apply = new (context) DotSyntaxCallExpr(ref, dotLoc, base);
        if (Implicit) {
          apply->setImplicit();
        }
      }

      return finishApply(apply, openedType, locator);
    }
    
    /// \brief Describes either a type or the name of a type to be resolved.
    typedef llvm::PointerUnion<Identifier, Type> TypeOrName;

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
    /// \brief Retrieve the overload choice associated with the given
    /// locator.
    SelectedOverload getOverloadChoice(ConstraintLocator *locator) {
      return *getOverloadChoiceIfAvailable(locator);
    }

    /// \brief Retrieve the overload choice associated with the given
    /// locator.
    Optional<SelectedOverload>
    getOverloadChoiceIfAvailable(ConstraintLocator *locator) {
      auto known = solution.overloadChoices.find(locator);
      if (known != solution.overloadChoices.end())
        return known->second;

      return None;
    }

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

    using LevelTy = llvm::PointerEmbeddedInt<unsigned, 2>;

    /// \brief Coerce the given expression (which is the argument to a call) to
    /// the given parameter type.
    ///
    /// This operation cannot fail.
    ///
    /// \param arg The argument expression.
    /// \param paramType The parameter type.
    /// \param applyOrLevel For function applications, the ApplyExpr that forms
    /// the call. Otherwise, a specific level describing which parameter level
    /// we're applying.
    /// \param argLabels The argument labels provided for the call.
    /// \param hasTrailingClosure Whether the last argument is a trailing
    /// closure.
    /// \param locator Locator used to describe where in this expression we are.
    ///
    /// \returns the coerced expression, which will have type \c ToType.
    Expr *
    coerceCallArguments(Expr *arg, Type paramType,
                        llvm::PointerUnion<ApplyExpr *, LevelTy> applyOrLevel,
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
                         ConstraintLocatorBuilder locator,
                         bool isImplicit, AccessSemantics semantics) {
      // Determine the declaration selected for this subscript operation.
      auto selected = getOverloadChoiceIfAvailable(
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

      auto choice = selected->choice;
      auto subscript = cast<SubscriptDecl>(choice.getDecl());

      auto &tc = cs.getTypeChecker();
      auto baseTy = cs.getType(base)->getRValueType();

      // Check whether the base is 'super'.
      bool isSuper = base->isSuperExpr();

      // Handle accesses that implicitly look through ImplicitlyUnwrappedOptional<T>.
      if (auto objTy = cs.lookThroughImplicitlyUnwrappedOptionalType(baseTy)) {
        base = coerceImplicitlyUnwrappedOptionalToValue(base, objTy, locator);
        baseTy = cs.getType(base);
      }

      // Figure out the index and result types.
      auto containerTy
        = subscript->getDeclContext()->getDeclaredTypeOfContext();
      auto subscriptTy = simplifyType(selected->openedType);
      auto indexTy = subscriptTy->castTo<AnyFunctionType>()->getInput();
      auto resultTy = subscriptTy->castTo<AnyFunctionType>()->getResult();

      // If we opened up an existential when performing the subscript, open
      // the base accordingly.
      auto knownOpened = solution.OpenedExistentialTypes.find(
                           getConstraintSystem().getConstraintLocator(
                             locator.withPathElement(
                               ConstraintLocator::SubscriptMember)));
      if (knownOpened != solution.OpenedExistentialTypes.end()) {
        base = openExistentialReference(base, knownOpened->second, subscript);
        baseTy = knownOpened->second;
        containerTy = baseTy;
      }

      // Coerce the index argument.
      index = coerceCallArguments(
          index, indexTy, LevelTy(1), argLabels,
          hasTrailingClosure,
          locator.withPathElement(ConstraintLocator::SubscriptIndex));
      if (!index)
        return nullptr;

      auto getType = [&](const Expr *E) -> Type {
        return cs.getType(E);
      };

      // Form the subscript expression.

      // Handle dynamic lookup.
      if (choice.getKind() == OverloadChoiceKind::DeclViaDynamic ||
          subscript->getAttrs().hasAttribute<OptionalAttr>()) {
        base = coerceObjectArgumentToType(base, baseTy, subscript,
                                          AccessSemantics::Ordinary, locator);
        if (!base)
          return nullptr;

        // TODO: diagnose if semantics != AccessSemantics::Ordinary?
        auto subscriptExpr = DynamicSubscriptExpr::create(tc.Context, base,
                                                          index, subscript,
                                                          isImplicit, getType);
        cs.setType(subscriptExpr, resultTy);
        Expr *result = subscriptExpr;
        closeExistential(result, locator);
        return result;
      }

      // Handle subscripting of generics.
      auto *dc = subscript->getInnermostDeclContext();
      if (auto *sig = dc->getGenericSignatureOfContext()) {
        // Compute the substitutions used to reference the subscript.
        SmallVector<Substitution, 4> substitutions;
        solution.computeSubstitutions(
          sig,
          selected->openedFullType,
          getConstraintSystem().getConstraintLocator(
            locator.withPathElement(ConstraintLocator::SubscriptMember)),
          substitutions);

        // Convert the base.
        auto openedFullFnType = selected->openedFullType->castTo<FunctionType>();
        auto openedBaseType = openedFullFnType->getInput();
        containerTy = solution.simplifyType(openedBaseType);
        base = coerceObjectArgumentToType(
            base, containerTy, subscript, AccessSemantics::Ordinary,
            locator.withPathElement(ConstraintLocator::MemberRefBase));
        if (!base)
          return nullptr;

        // Form the generic subscript expression.
        auto subscriptExpr = SubscriptExpr::create(
            tc.Context, base, index,
            ConcreteDeclRef(tc.Context, subscript, substitutions), isImplicit,
            semantics, getType);
        cs.setType(subscriptExpr, resultTy);
        subscriptExpr->setIsSuper(isSuper);

        Expr *result = subscriptExpr;
        closeExistential(result, locator);
        return result;
      }

      Type selfTy = containerTy;
      if (selfTy->isEqual(baseTy) && !selfTy->hasReferenceSemantics())
        if (cs.getType(base)->is<LValueType>())
          selfTy = InOutType::get(selfTy);

      // Coerce the base to the container type.
      base = coerceObjectArgumentToType(base, selfTy, subscript,
                                        AccessSemantics::Ordinary, locator);
      if (!base)
        return nullptr;

      // Form a normal subscript.
      auto *subscriptExpr = SubscriptExpr::create(
          tc.Context, base, index, subscript, isImplicit, semantics, getType);
      cs.setType(subscriptExpr, resultTy);
      subscriptExpr->setIsSuper(isSuper);
      Expr *result = subscriptExpr;
      closeExistential(result, locator);
      return result;
    }

    /// \brief Build a new reference to another constructor.
    Expr *buildOtherConstructorRef(Type openedFullType,
                                   ConstructorDecl *ctor, DeclNameLoc loc,
                                   ConstraintLocatorBuilder locator,
                                   bool implicit) {
      auto &tc = cs.getTypeChecker();
      auto &ctx = tc.Context;

      // Compute the concrete reference.
      ConcreteDeclRef ref;
      Type resultTy;
      if (auto *sig = ctor->getGenericSignature()) {
        // Compute the reference to the generic constructor.
        SmallVector<Substitution, 4> substitutions;
        resultTy = solution.computeSubstitutions(
                     sig,
                     openedFullType,
                     getConstraintSystem().getConstraintLocator(locator),
                     substitutions);

        ref = ConcreteDeclRef(ctx, ctor, substitutions);
      } else {
        Type containerTy = ctor->getDeclContext()->getDeclaredTypeOfContext();
        resultTy = openedFullType->replaceCovariantResultType(
                     containerTy, ctor->getNumParameterLists());
        ref = ConcreteDeclRef(ctor);
      }

      // The constructor was opened with the allocating type, not the
      // initializer type. Map the former into the latter.
      auto resultFnTy = resultTy->castTo<FunctionType>();
      auto selfTy = resultFnTy->getInput()->getRValueInstanceType();
      if (!selfTy->hasReferenceSemantics())
        selfTy = InOutType::get(selfTy);
      
      resultTy = FunctionType::get(selfTy, resultFnTy->getResult(),
                                   resultFnTy->getExtInfo());

      // Build the constructor reference.
      return cs.cacheType(
          new (ctx) OtherConstructorDeclRefExpr(ref, loc, implicit, resultTy));
    }

    /// Bridge the given value (which is an error type) to NSError.
    Expr *bridgeErrorToObjectiveC(Expr *value) {
      auto &tc = cs.getTypeChecker();

      // Use _bridgeErrorToNSError to convert to an NSError.
      auto fn = tc.Context.getBridgeErrorToNSError(&tc);
      SourceLoc loc = value->getLoc();
      if (!fn) {
        tc.diagnose(loc, diag::missing_nserror_bridging_function);
        return nullptr;
      }
      tc.validateDecl(fn);

      ConcreteDeclRef fnDeclRef(fn);
      auto fnRef = new (tc.Context) DeclRefExpr(fnDeclRef, DeclNameLoc(loc),
                                                /*Implicit=*/true);
      cs.setType(fnRef, fn->getInterfaceType());
      fnRef->setFunctionRefKind(FunctionRefKind::SingleApply);

      auto getType = [&](const Expr *E) -> Type {
        return cs.getType(E);
      };

      Expr *call = CallExpr::createImplicit(tc.Context, fnRef, { value }, { },
                                            getType);
      cs.cacheSubExprTypes(call);
      cs.setSubExprTypes(call);
      if (tc.typeCheckExpressionShallow(call, dc))
        return nullptr;

      cs.cacheExprTypes(call);

      // The return type of _bridgeErrorToNSError is formally
      // 'AnyObject' to avoid stdlib-to-Foundation dependencies, but it's
      // really NSError.  Abuse CovariantReturnConversionExpr to fix this.
      auto nsErrorDecl = tc.Context.getNSErrorDecl();
      assert(nsErrorDecl && "Missing NSError?");
      Type nsErrorType = nsErrorDecl->getDeclaredInterfaceType();
      return cs.cacheType(new (tc.Context)
                              CovariantReturnConversionExpr(call, nsErrorType));
    }

    /// Bridge the given value to its corresponding Objective-C object
    /// type.
    ///
    /// This routine should only be used for bridging value types.
    ///
    /// \param value The value to be bridged.
    Expr *bridgeToObjectiveC(Expr *value) {
      // TODO: It would be nicer to represent this as a special AST node
      // instead of inlining the bridging calls in the AST. Doing so would
      // simplify the AST and make it easier for SILGen to peephole bridging
      // conversions.
      auto &tc = cs.getTypeChecker();

      // Find the _ObjectiveCBridgeable protocol.
      auto bridgedProto
        = tc.Context.getProtocol(KnownProtocolKind::ObjectiveCBridgeable);

      // Find the conformance of the value type to _ObjectiveCBridgeable.
      Type valueType = cs.getType(value)->getRValueType();
      if (auto conformance =
            tc.conformsToProtocol(valueType, bridgedProto, cs.DC,
                                  (ConformanceCheckFlags::InExpression|
                                   ConformanceCheckFlags::Used))) {
        cs.setExprTypes(value);

        // Form the call.
        auto result =
            tc.callWitness(value, cs.DC, bridgedProto, *conformance,
                           tc.Context.Id_bridgeToObjectiveC,
                           { }, diag::broken_bridged_to_objc_protocol);

        if (result)
          cs.cacheExprTypes(result);

        return result;
      }
      
      // If there is an Error conformance, try bridging as an error.
      if (tc.isConvertibleTo(valueType,
                             tc.Context.getProtocol(KnownProtocolKind::Error)
                               ->getDeclaredType(),
                             cs.DC))
        return bridgeErrorToObjectiveC(value);

      // Fall back to universal bridging.
      auto bridgeAnything = tc.Context.getBridgeAnythingToObjectiveC(&tc);
      value = cs.coerceToRValue(value);
      auto valueSub = Substitution(valueType, {});
      auto bridgeAnythingRef = ConcreteDeclRef(tc.Context, bridgeAnything,
                                               valueSub);
      auto valueParenTy = ParenType::get(tc.Context, cs.getType(value));
      auto bridgeTy = tc.Context.getProtocol(KnownProtocolKind::AnyObject)
        ->getDeclaredType();
      auto bridgeFnTy = FunctionType::get(valueParenTy, bridgeTy);
      // FIXME: Wrap in ParenExpr to prevent bogus "tuple passed as argument"
      // errors.
      auto valueParen = new (tc.Context) ParenExpr(SourceLoc(),
                                                   value,
                                                   SourceLoc(),
                                                   /*trailing closure*/ false,
                                                   valueParenTy);

      valueParen->setImplicit();
      auto fnRef = new (tc.Context) DeclRefExpr(bridgeAnythingRef,
                                                DeclNameLoc(),
                                                /*implicit*/ true,
                                                AccessSemantics::Ordinary,
                                                bridgeFnTy);

      auto getType = [&](const Expr *E) -> Type {
        return cs.getType(E);
      };

      fnRef->setFunctionRefKind(FunctionRefKind::SingleApply);
      Expr *call = CallExpr::createImplicit(tc.Context, fnRef, valueParen,
                                            { Identifier() }, getType);
      cs.setType(call, bridgeTy);
      cs.cacheExprTypes(call);
      return call;
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

      auto subMap = genericSig->getSubstitutionMap(
        [&](SubstitutableType *type) -> Type {
          assert(type->isEqual(genericSig->getGenericParams()[0]));
          return valueType;
        },
        [&](CanType origType, Type replacementType, ProtocolType *protoType)
          -> ProtocolConformanceRef {
          assert(bridgedToObjectiveCConformance);
          return *bridgedToObjectiveCConformance;
        });

      SmallVector<Substitution, 1> subs;
      genericSig->getSubstitutions(subMap, subs);

      ConcreteDeclRef fnSpecRef(tc.Context, fn, subs);
      auto fnRef = new (tc.Context) DeclRefExpr(fnSpecRef,
                                                DeclNameLoc(
                                                  object->getStartLoc()),
                                                /*Implicit=*/true);
      fnRef->setFunctionRefKind(FunctionRefKind::SingleApply);

      cs.setType(fnRef,
                 fn->getInterfaceType()->castTo<GenericFunctionType>()
                   ->substGenericArgs(subMap));

      // Form the arguments.
      Expr *args[2] = {
        object,
        new (tc.Context) DotSelfExpr(
                             TypeExpr::createImplicitHack(object->getLoc(),
                                                          valueType,
                                                          tc.Context),
                             object->getLoc(), object->getLoc(),
                             MetatypeType::get(valueType))
      };
      args[1]->setImplicit();

      // Form the call and type-check it.
      auto getType = [&](const Expr *E) -> Type {
        return cs.getType(E);
      };

      Expr *call = CallExpr::createImplicit(tc.Context, fnRef, args,
                                            { Identifier(), Identifier() },
                                            getType);
      cs.cacheSubExprTypes(call);
      cs.setSubExprTypes(call);

      if (tc.typeCheckExpressionShallow(call, dc))
        return nullptr;

      cs.cacheExprTypes(call);
      return call;
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

    TypeAliasDecl *MaxIntegerTypeDecl = nullptr;
    TypeAliasDecl *MaxFloatTypeDecl = nullptr;
    
  public:
    ExprRewriter(ConstraintSystem &cs, const Solution &solution,
                 bool suppressDiagnostics, bool skipClosures)
      : cs(cs), dc(cs.DC), solution(solution), 
        SuppressDiagnostics(suppressDiagnostics),
        SkipClosures(skipClosures) { }

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

      // Find the maximum-sized builtin integer type.
      
      if (!MaxIntegerTypeDecl) {
        SmallVector<ValueDecl *, 1> lookupResults;
        tc.getStdlibModule(dc)->lookupValue(/*AccessPath=*/{},
                                            tc.Context.Id_MaxBuiltinIntegerType,
                                            NLKind::QualifiedLookup,
                                            lookupResults);
        if (lookupResults.size() == 1) {
          MaxIntegerTypeDecl = dyn_cast<TypeAliasDecl>(lookupResults.front());
          tc.validateDecl(MaxIntegerTypeDecl);
        }
      }
      if (!MaxIntegerTypeDecl ||
          !MaxIntegerTypeDecl->hasInterfaceType() ||
          !MaxIntegerTypeDecl->getDeclaredInterfaceType()->is<BuiltinIntegerType>()) {
        tc.diagnose(expr->getLoc(), diag::no_MaxBuiltinIntegerType_found);
        return nullptr;
      }
      tc.validateDecl(MaxIntegerTypeDecl);
      auto maxType = MaxIntegerTypeDecl->getUnderlyingTypeLoc().getType();

      DeclName initName(tc.Context, tc.Context.Id_init,
                        { tc.Context.Id_integerLiteral });
      DeclName builtinInitName(tc.Context, tc.Context.Id_init,
                               { tc.Context.Id_builtinIntegerLiteral });

      return convertLiteral(
               expr,
               type,
               cs.getType(expr),
               protocol,
               tc.Context.Id_IntegerLiteralType,
               initName,
               builtinProtocol,
               maxType,
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

      DeclName initName(tc.Context, tc.Context.Id_init,
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

      DeclName initName(tc.Context, tc.Context.Id_init,
                        { tc.Context.Id_floatLiteral });
      DeclName builtinInitName(tc.Context, tc.Context.Id_init,
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
      DeclName initName(tc.Context, tc.Context.Id_init,
                        { tc.Context.Id_booleanLiteral });
      DeclName builtinInitName(tc.Context, tc.Context.Id_init,
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

        literalFuncName = DeclName(tc.Context, tc.Context.Id_init,
                                   { tc.Context.Id_stringLiteral });

        // If the string contains non-ASCII and the type can handle
        // UTF-16 string literals, prefer them.
        builtinProtocol = tc.getProtocol(
            expr->getLoc(),
            KnownProtocolKind::ExpressibleByBuiltinUTF16StringLiteral);
        if (!forceASCII &&
            tc.conformsToProtocol(type, builtinProtocol, cs.DC,
                                  ConformanceCheckFlags::InExpression)) {
          builtinLiteralFuncName 
            = DeclName(tc.Context, tc.Context.Id_init,
                       { tc.Context.Id_builtinUTF16StringLiteral,
                         tc.Context.getIdentifier("utf16CodeUnitCount") });

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
            = DeclName(tc.Context, tc.Context.Id_init,
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
          = DeclName(tc.Context, tc.Context.Id_init,
                     {tc.Context.Id_extendedGraphemeClusterLiteral});
        builtinLiteralFuncName
          = DeclName(tc.Context, tc.Context.Id_init,
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
      } else {
        // Otherwise, we should have just one Unicode scalar.
        literalType = tc.Context.Id_UnicodeScalarLiteralType;

        literalFuncName
          = DeclName(tc.Context, tc.Context.Id_init,
                     {tc.Context.Id_unicodeScalarLiteral});
        builtinLiteralFuncName
          = DeclName(tc.Context, tc.Context.Id_init,
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

      DeclName name(tc.Context, tc.Context.Id_init,
                    { tc.Context.Id_stringInterpolation });
      auto member
        = findNamedWitnessImpl<ConstructorDecl>(
            tc, dc, type,
            interpolationProto, name,
            diag::interpolation_broken_proto);

      DeclName segmentName(tc.Context, tc.Context.Id_init,
                           { tc.Context.Id_stringInterpolationSegment });
      auto segmentMember
        = findNamedWitnessImpl<ConstructorDecl>(
            tc, dc, type, interpolationProto, segmentName,
            diag::interpolation_broken_proto);
      if (!member || !segmentMember)
        return nullptr;

      // Build a reference to the init(stringInterpolation:) initializer.
      // FIXME: This location info is bogus.
      auto *typeRef = TypeExpr::createImplicitHack(expr->getStartLoc(), type,
                                                   tc.Context);

      Expr *memberRef =
        new (tc.Context) MemberRefExpr(typeRef,
                                       expr->getStartLoc(),
                                       member,
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

    Expr *visitObjectLiteralExpr(ObjectLiteralExpr *expr) {
      if (cs.getType(expr) && !cs.getType(expr)->hasTypeVariable())
        return expr;

      auto &ctx = cs.getASTContext();
      auto &tc = cs.getTypeChecker();

      // Figure out the type we're converting to.
      auto openedType = cs.getType(expr);
      auto type = simplifyType(openedType);
      cs.setType(expr, type);

      if (type->is<UnresolvedType>()) return expr;

      Type conformingType = type;
      if (auto baseType = conformingType->getAnyOptionalObjectType()) {
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

    Expr *visitDeclRefExpr(DeclRefExpr *expr) {
      auto locator = cs.getConstraintLocator(expr);

      // Find the overload choice used for this declaration reference.
      auto selected = getOverloadChoiceIfAvailable(locator);
      if (!selected.hasValue()) {
        auto *varDecl = cast<VarDecl>(expr->getDecl());
        assert(varDecl->getType()->is<UnresolvedType>() &&
               "should only happen for closure arguments in CSDiags");
        cs.setType(expr, varDecl->getType());
        return expr;
      }
      
      auto choice = selected->choice;
      auto decl = choice.getDecl();

      // FIXME: Cannibalize the existing DeclRefExpr rather than allocating a
      // new one?
      return buildDeclRef(decl, expr->getNameLoc(), selected->openedFullType,
                          locator, expr->isSpecialized(),
                          expr->isImplicit(),
                          expr->getFunctionRefKind(),
                          expr->getAccessSemantics());
    }

    Expr *visitSuperRefExpr(SuperRefExpr *expr) {
      simplifyExprType(expr);
      return expr;
    }

    Expr *visitTypeExpr(TypeExpr *expr) {
      auto toType = simplifyType(expr->getTypeLoc().getType());
      expr->getTypeLoc().setType(toType, /*validated=*/true);
      cs.setType(expr, MetatypeType::get(toType));
      
      return expr;
    }

    Expr *visitOtherConstructorDeclRefExpr(OtherConstructorDeclRefExpr *expr) {
      assert(!expr->getDeclRef().isSpecialized());
      cs.setType(expr, expr->getDecl()->getInitializerInterfaceType());
      return expr;
    }

    Expr *visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *expr) {
      return simplifyExprType(expr);
    }

    Expr *visitOverloadedDeclRefExpr(OverloadedDeclRefExpr *expr) {
      // Determine the declaration selected for this overloaded reference.
      auto locator = cs.getConstraintLocator(expr);
      auto selected = getOverloadChoice(locator);
      auto choice = selected.choice;
      auto decl = choice.getDecl();

      return buildDeclRef(decl, expr->getNameLoc(), selected.openedFullType,
                          locator, expr->isSpecialized(), expr->isImplicit(),
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
      if (auto DRE = dyn_cast<DeclRefExpr>(expr->getSubExpr())) {
        assert(DRE->getGenericArgs().empty() ||
            DRE->getGenericArgs().size() == expr->getUnresolvedParams().size());
        if (DRE->getGenericArgs().empty()) {
          SmallVector<TypeRepr *, 8> GenArgs;
          for (auto TL : expr->getUnresolvedParams())
            GenArgs.push_back(TL.getTypeRepr());
          DRE->setGenericArgs(GenArgs);
        }
      }
      return expr->getSubExpr();
    }

    Expr *visitMemberRefExpr(MemberRefExpr *expr) {
      auto memberLocator = cs.getConstraintLocator(expr,
                                                   ConstraintLocator::Member);
      auto selected = getOverloadChoice(memberLocator);
      bool isDynamic
        = selected.choice.getKind() == OverloadChoiceKind::DeclViaDynamic;
      return buildMemberRef(expr->getBase(),
                            selected.openedFullType,
                            expr->getDotLoc(),
                            selected.choice.getDecl(), expr->getNameLoc(),
                            selected.openedType,
                            cs.getConstraintLocator(expr),
                            memberLocator,
                            expr->isImplicit(),
                            selected.choice.getFunctionRefKind(),
                            expr->getAccessSemantics(),
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
      auto selected = getOverloadChoice(memberLocator);
      auto member = selected.choice.getDecl();
      
      // If the member came by optional unwrapping, then unwrap the base type.
      if (selected.choice.getKind()
                              == OverloadChoiceKind::DeclViaUnwrappedOptional) {
        baseTy = baseTy->getAnyOptionalObjectType();
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
      auto result = buildMemberRef(base,
                                   selected.openedFullType,
                                   expr->getDotLoc(), member, 
                                   expr->getNameLoc(),
                                   selected.openedType,
                                   cs.getConstraintLocator(expr),
                                   memberLocator,
                                   expr->isImplicit(),
                                   selected.choice.getFunctionRefKind(),
                                   AccessSemantics::Ordinary,
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
            /*implicit=*/false, Type(), getType);
        result = finishApply(apply, Type(), cs.getConstraintLocator(expr));
      }

      result = coerceToType(result, resultTy, cs.getConstraintLocator(expr));
      return result;
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
                           ConstructorDecl *ctor,
                           FunctionRefKind functionRefKind,
                           Type openedType) {
      // If the subexpression is a metatype, build a direct reference to the
      // constructor.
      if (cs.getType(base)->is<AnyMetatypeType>()) {
        return buildMemberRef(base, openedType, dotLoc, ctor, nameLoc,
                              cs.getType(expr),
                              ConstraintLocatorBuilder(
                                cs.getConstraintLocator(expr)),
                              ctorLocator,
                              implicit,
                              functionRefKind,
                              AccessSemantics::Ordinary,
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
          if (dre->getDecl()->getName() == cs.getASTContext().Id_self) {
            // We have a reference to 'self'.
            diagnoseBadInitRef = false;

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
            if (auto nominalType
                       = func->getDeclContext()->getDeclaredTypeOfContext()) {
              if (auto classDecl = nominalType->getClassOrBoundGenericClass()) {
                hasSuper = classDecl->hasSuperclass();
              }
            }
          }

          if (SuppressDiagnostics)
            return nullptr;

          tc.diagnose(dotLoc, diag::bad_init_ref_base, hasSuper);
        }
      }

      // Build a partial application of the delegated initializer.
      Expr *ctorRef = buildOtherConstructorRef(openedType, ctor, nameLoc,
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
      if (auto selected = getOverloadChoiceIfAvailable(ctorLocator)) {
        auto choice = selected->choice;
        auto *ctor = cast<ConstructorDecl>(choice.getDecl());
        return applyCtorRefExpr(expr, base, dotLoc, nameLoc, implicit,
                                ctorLocator, ctor, choice.getFunctionRefKind(),
                                selected->openedFullType);
      }

      // Determine the declaration selected for this overloaded reference.
      auto memberLocator = cs.getConstraintLocator(expr,
                                                   ConstraintLocator::Member);
      auto selectedElt = getOverloadChoiceIfAvailable(memberLocator);

      if (!selectedElt) {
        // If constraint solving resolved this to an UnresolvedType, then we're
        // in an ambiguity tolerant mode used for diagnostic generation.  Just
        // leave this as whatever type of member reference it already is.
        Type resultTy = simplifyType(cs.getType(expr));
        cs.setType(expr, resultTy);
        return expr;
      }

      auto selected = *selectedElt;
      switch (selected.choice.getKind()) {
      case OverloadChoiceKind::DeclViaBridge: {
        // Look through an implicitly unwrapped optional.
        auto baseTy = cs.getType(base)->getRValueType();
        if (auto objTy = cs.lookThroughImplicitlyUnwrappedOptionalType(baseTy)){
          base = coerceImplicitlyUnwrappedOptionalToValue(base, objTy,
                                         cs.getConstraintLocator(base));

          baseTy = cs.getType(base)->getRValueType();
        }

        if (auto baseMetaTy = baseTy->getAs<MetatypeType>()) {
          auto &tc = cs.getTypeChecker();
          auto &ctx = tc.Context;
          auto classTy = ctx.getBridgedToObjC(cs.DC,
                                              baseMetaTy->getInstanceType());
          
          // FIXME: We're dropping side effects in the base here!
          base = TypeExpr::createImplicitHack(base->getLoc(), classTy,
                                              tc.Context);
          cs.cacheExprTypes(base);
        } else {
          // Bridge the base to its corresponding Objective-C object.
          base = bridgeToObjectiveC(base);
        }

        // Fall through to build the member reference.
        LLVM_FALLTHROUGH;
      }

      case OverloadChoiceKind::Decl:
      case OverloadChoiceKind::DeclViaUnwrappedOptional:
      case OverloadChoiceKind::DeclViaDynamic: {
        bool isDynamic
          = selected.choice.getKind() == OverloadChoiceKind::DeclViaDynamic;
        auto member = buildMemberRef(base,
                                     selected.openedFullType,
                                     dotLoc,
                                     selected.choice.getDecl(),
                                     nameLoc,
                                     selected.openedType,
                                     cs.getConstraintLocator(expr),
                                     memberLocator,
                                     implicit,
                                     selected.choice.getFunctionRefKind(),
                                     AccessSemantics::Ordinary,
                                     isDynamic);

        return member;
      }

      case OverloadChoiceKind::TupleIndex: {
        auto baseTy = cs.getType(base)->getRValueType();
        if (auto objTy = cs.lookThroughImplicitlyUnwrappedOptionalType(baseTy)){
          base = coerceImplicitlyUnwrappedOptionalToValue(base, objTy,
                                         cs.getConstraintLocator(base));
        }

        Type toType = simplifyType(cs.getType(expr));
        
        // If the result type is an rvalue and the base contains lvalues, need a full
        // tuple coercion to properly load & set access kind on all underlying elements
        // before taking a single element.
        baseTy = cs.getType(base);
        if (!toType->isLValueType() && baseTy->isLValueType())
          base = coerceToType(base, baseTy->getRValueType(), cs.getConstraintLocator(base));

        return cs.cacheType(new (cs.getASTContext())
                            TupleElementExpr(base, dotLoc,
                                             selected.choice.getTupleIndex(),
                                             nameLoc.getBaseNameLoc(), toType));
      }

      case OverloadChoiceKind::BaseType: {
        // FIXME: Losing ".0" sugar here.
        return base;
      }

      case OverloadChoiceKind::TypeDecl:
        llvm_unreachable("Nonsensical overload choice");
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
      cs.setType(expr, ParenType::get(ctx, cs.getType(expr->getSubExpr())));
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

    Expr *visitArrayExpr(ArrayExpr *expr) {
      Type openedType = cs.getType(expr);
      Type arrayTy = simplifyType(openedType);
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

      DeclName name(tc.Context, tc.Context.Id_init,
                    { tc.Context.Id_arrayLiteral });

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

      cs.cacheExprTypes(typeRef);
      cs.setExprTypes(typeRef);

      Expr *result = tc.callWitness(typeRef, dc, arrayProto, *conformance,
                                    name, arg, diag::array_protocol_broken);
      if (!result)
        return nullptr;

      cs.cacheExprTypes(result);

      expr->setSemanticExpr(result);
      cs.setType(expr, arrayTy);

      // If the array element type was defaulted, note that in the expression.
      if (solution.DefaultedConstraints.count(cs.getConstraintLocator(expr)))
        expr->setIsTypeDefaulted();

      return expr;
    }

    Expr *visitDictionaryExpr(DictionaryExpr *expr) {
      Type openedType = cs.getType(expr);
      Type dictionaryTy = simplifyType(openedType);
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

      DeclName name(tc.Context, tc.Context.Id_init,
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
                              /*Implicit=*/false,
                              argType);

      cs.cacheExprTypes(typeRef);
      cs.setExprTypes(typeRef);

      Expr *result = tc.callWitness(typeRef, dc, dictionaryProto,
                                    *conformance, name, arg,
                                    diag::dictionary_protocol_broken);
      if (!result)
        return nullptr;

      cs.cacheExprTypes(result);

      expr->setSemanticExpr(result);
      cs.setType(expr, dictionaryTy);

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
      // Handle accesses that implicitly look through ImplicitlyUnwrappedOptional<T>.
      auto base = expr->getBase();
      auto baseTy = cs.getType(base)->getRValueType();
      if (auto objTy = cs.lookThroughImplicitlyUnwrappedOptionalType(baseTy)) {
        base = coerceImplicitlyUnwrappedOptionalToValue(base, objTy,
                                              cs.getConstraintLocator(base));
        expr->setBase(base);
      }

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
      // The default assumption is that inouts are read-write.  It's easier
      // to do this unconditionally here and then overwrite in the exception
      // case (when we turn the inout into an UnsafePointer) than to try to
      // discover that we're in that case right now.
      if (!cs.getType(expr->getSubExpr())->is<UnresolvedType>())
        cs.propagateLValueAccessKind(expr->getSubExpr(), AccessKind::ReadWrite);
      auto objectTy = cs.getType(expr->getSubExpr())->getRValueType();

      // The type is simply inout of whatever the lvalue's object type was.
      cs.setType(expr, InOutType::get(objectTy));
      return expr;
    }

    Expr *visitDynamicTypeExpr(DynamicTypeExpr *expr) {
      Expr *base = expr->getBase();
      base = cs.coerceToRValue(base);
      if (!base) return nullptr;
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
      OptionalTypeKind calledOTK;
      Expr *unwrappedSubExpr = expr->getSubExpr()->getSemanticsProvidingExpr();
      Type valueTy
        = cs.getType(unwrappedSubExpr)->getAnyOptionalObjectType(calledOTK);
      auto inCtor = cast<ConstructorDecl>(cs.DC->getInnermostMethodContext());
      if (calledOTK != OTK_None && inCtor->getFailability() == OTK_None) {
        bool isError = (calledOTK == OTK_Optional);

        // If we're suppressing diagnostics, just fail.
        if (isError && SuppressDiagnostics)
          return nullptr;

        bool isChaining;
        auto *otherCtorRef = expr->getCalledConstructor(isChaining);

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
            ConstructorDecl *ctor = otherCtorRef->getDecl();
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
      if (!cond) {
        cs.setType(expr->getCondExpr(), ErrorType::get(resultTy));
      } else {
        expr->setCondExpr(cond);
      }

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
      auto toType = simplifyType(expr->getCastTypeLoc().getType());
      auto sub = cs.coerceToRValue(expr->getSubExpr());
      if (!sub)
        return nullptr;
      
      checkForImportedUsedConformances(toType);
      expr->setSubExpr(sub);

      // Set the type we checked against.
      expr->getCastTypeLoc().setType(toType, /*validated=*/true);
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
      case CheckedCastKind::Swift3BridgingDowncast:
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
      fromType->lookThroughAllAnyOptionalTypes(fromOptionals);
      SmallVector<Type, 2> toOptionals;
      toType->lookThroughAllAnyOptionalTypes(toOptionals);

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
        if (expr->isImplicit())
          cast->setImplicit();

        // Type-check this conditional case.
        Expr *result = visitConditionalCheckedCastExpr(cast, true);
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
    Expr *handleOptionalBindings(ExplicitCastExpr *cast, 
                                 Type finalResultType,
                                 OptionalBindingsCastKind castKind) {
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
      Expr *subExpr = cast->getSubExpr();
      Type srcType = cs.getType(subExpr);

      SmallVector<Type, 4> srcOptionals;
      srcType = srcType->lookThroughAllAnyOptionalTypes(srcOptionals);

      SmallVector<Type, 4> destOptionals;
      auto destValueType
        = finalResultType->lookThroughAllAnyOptionalTypes(destOptionals);

      // When performing a bridging operation, if the destination value type
      // is 'AnyObject', leave any extra optionals on the source in place.
      if (castKind == OptionalBindingsCastKind::Bridged &&
          srcOptionals.size() > destOptionals.size() &&
          destValueType->isAnyObject()) {
        srcType = srcOptionals[destOptionals.size()];
        srcOptionals.erase(srcOptionals.begin() + destOptionals.size(),
                           srcOptionals.end());
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

      // Complain about conditional casts to foreign class types; they can't
      // actually be conditionally checked.
      if (castKind == OptionalBindingsCastKind::Conditional) {
        auto destObjectType = destValueType;
        if (auto metaTy = destObjectType->getAs<MetatypeType>())
          destObjectType = metaTy->getInstanceType();
        if (auto destClass = destObjectType->getClassOrBoundGenericClass()) {
          if (destClass->getForeignClassKind() ==
                ClassDecl::ForeignKind::CFType) {
            if (SuppressDiagnostics)
              return nullptr;

            tc.diagnose(cast->getLoc(), diag::conditional_downcast_foreign,
                        destValueType);
          }
        }
      }

      // There's nothing special to do if the operand isn't optional
      // and we don't need any bridging.
      if (srcOptionals.empty()) {
        cs.setType(cast, finalResultType);
        return addFinalOptionalInjections(cast);
      }

      // If this is a conditional cast, the result type will always
      // have at least one level of optional, which should become the
      // type of the checked-cast expression.
      if (castKind == OptionalBindingsCastKind::Conditional) {
        assert(!destOptionals.empty() &&
               "result of checked cast is not an optional type");
        cs.setType(cast, destOptionals.back());
      } else {
        cs.setType(cast, destValueType);
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
      cast->setSubExpr(subExpr);

      // If we're casting to an optional type, we need to capture the
      // final M bindings.
      Expr *result = cast;

      if (destOptionals.size() > destExtraOptionals) {
        if (castKind == OptionalBindingsCastKind::Conditional) {
          // If the innermost cast fails, the entire expression fails.  To
          // get this behavior, we have to bind and then re-inject the result.
          // (SILGen should know how to peephole this.)
          result =
            cs.cacheType(new (tc.Context) BindOptionalExpr(result,
                                                           cast->getEndLoc(),
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

    Expr *visitCoerceExpr(CoerceExpr *expr) {
      return visitCoerceExpr(expr, None);
    }

    Expr *visitCoerceExpr(CoerceExpr *expr, Optional<unsigned> choice) {
      // Simplify the type we're casting to.
      auto toType = simplifyType(expr->getCastTypeLoc().getType());
      expr->getCastTypeLoc().setType(toType, /*validated=*/true);
      checkForImportedUsedConformances(toType);

      auto &tc = cs.getTypeChecker();

      // Turn the subexpression into an rvalue.
      if (auto rvalueSub = cs.coerceToRValue(expr->getSubExpr()))
        expr->setSubExpr(rvalueSub);
      else
        return nullptr;

      // If we weren't explicitly told by the caller which disjunction choice,
      // get it from the solution to determine whether we've picked a coercion
      // or a bridging conversion.
      auto locator = cs.getConstraintLocator(expr);
      if (!choice) {
        if (tc.Context.LangOpts.EnableObjCInterop)
          choice = solution.getDisjunctionChoice(locator);
        else
          choice = 0;
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
      Expr *result = handleOptionalBindings(expr, toType,
                                            OptionalBindingsCastKind::Bridged);
      if (!result)
        return nullptr;

      Expr *sub = expr->getSubExpr();
      Type toInstanceType = toType->lookThroughAllAnyOptionalTypes();
      
      // Warn about NSNumber and NSValue bridging coercions we accepted in
      // Swift 3 but which can fail at runtime.
      if (tc.Context.LangOpts.isSwiftVersion3()
          && tc.typeCheckCheckedCast(cs.getType(sub), toInstanceType,
                                     CheckedCastContextKind::None,
                                     dc, SourceLoc(), sub, SourceRange())
               == CheckedCastKind::Swift3BridgingDowncast) {
        tc.diagnose(expr->getLoc(),
                    diag::missing_forced_downcast_swift3_compat_warning,
                    cs.getType(sub), toInstanceType)
          .fixItReplace(expr->getAsLoc(), "as!");
      }
      
      sub = buildObjCBridgeExpr(sub, toInstanceType, locator);
      if (!sub) return nullptr;
      expr->setSubExpr(sub);
      return result;
    }

    Expr *visitForcedCheckedCastExpr(ForcedCheckedCastExpr *expr) {
      // Simplify the type we're casting to.
      auto toType = simplifyType(expr->getCastTypeLoc().getType());
      expr->getCastTypeLoc().setType(toType, /*validated=*/true);
      checkForImportedUsedConformances(toType);

      // The subexpression is always an rvalue.
      auto &tc = cs.getTypeChecker();
      auto sub = cs.coerceToRValue(expr->getSubExpr());
      if (!sub)
        return nullptr;
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
        unsigned disjunctionChoice =
          (castKind == CheckedCastKind::Coercion ? 0 : 1);
        return visitCoerceExpr(result, disjunctionChoice);
      }

      // Valid casts.
      case CheckedCastKind::Swift3BridgingDowncast:
      case CheckedCastKind::ArrayDowncast:
      case CheckedCastKind::DictionaryDowncast:
      case CheckedCastKind::SetDowncast:
      case CheckedCastKind::ValueCast:
        expr->setCastKind(castKind);
        break;
      }
      
      return handleOptionalBindings(expr, simplifyType(cs.getType(expr)),
                                    OptionalBindingsCastKind::Forced);
    }

    Expr *visitConditionalCheckedCastExpr(ConditionalCheckedCastExpr *expr,
                                          bool isInsideIsExpr = false) {
      // Simplify the type we're casting to.
      auto toType = simplifyType(expr->getCastTypeLoc().getType());
      checkForImportedUsedConformances(toType);
      expr->getCastTypeLoc().setType(toType, /*validated=*/true);

      // The subexpression is always an rvalue.
      auto &tc = cs.getTypeChecker();
      auto sub = cs.coerceToRValue(expr->getSubExpr());
      if (!sub)
        return nullptr;
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
      case CheckedCastKind::Swift3BridgingDowncast:
      case CheckedCastKind::ArrayDowncast:
      case CheckedCastKind::DictionaryDowncast:
      case CheckedCastKind::SetDowncast:
      case CheckedCastKind::ValueCast:
        expr->setCastKind(castKind);
        break;
      }
      
      return handleOptionalBindings(expr, simplifyType(cs.getType(expr)),
                                    OptionalBindingsCastKind::Conditional);
    }

    Expr *visitAssignExpr(AssignExpr *expr) {
      // Compute the type to which the source must be converted to allow
      // assignment to the destination.
      //
      // FIXME: This is also computed when the constraint system is set up.
      auto destTy = cs.computeAssignDestType(expr->getDest(), expr->getLoc());
      if (!destTy)
        return nullptr;
      cs.propagateLValueAccessKind(expr->getDest(), AccessKind::Write);

      // Convert the source to the simplified destination type.
      auto locator =
        ConstraintLocatorBuilder(cs.getConstraintLocator(expr->getSrc()));
      Expr *src = coerceToType(expr->getSrc(), destTy, locator);
      if (!src)
        return nullptr;
      expr->setSrc(src);
      return expr;
    }
    
    Expr *visitDiscardAssignmentExpr(DiscardAssignmentExpr *expr) {
      return simplifyExprType(expr);
    }
    
    Expr *visitUnresolvedPatternExpr(UnresolvedPatternExpr *expr) {
      // If we end up here, we should have diagnosed somewhere else
      // already.
      if (!SuppressDiagnostics) {
        cs.TC.diagnose(expr->getLoc(), diag::pattern_in_expr,
                       expr->getSubPattern()->getKind());
      }
      return expr;
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
      Type valueType = simplifyType(cs.getType(expr));
      cs.setType(expr, valueType);
      
      // Coerce the object type, if necessary.
      auto subExpr = expr->getSubExpr();
      if (auto objectTy = cs.getType(subExpr)->getAnyOptionalObjectType()) {
        if (objectTy && !objectTy->isEqual(valueType)) {
          auto coercedSubExpr = coerceToType(subExpr,
                                             OptionalType::get(valueType),
                                             cs.getConstraintLocator(subExpr));
          
          expr->setSubExpr(coercedSubExpr);
        }
      }
      
      return expr;
    }

    Expr *visitOpenExistentialExpr(OpenExistentialExpr *expr) {
      llvm_unreachable("Already type-checked");
    }
    
    Expr *visitMakeTemporarilyEscapableExpr(MakeTemporarilyEscapableExpr *expr){
      llvm_unreachable("Already type-checked");
    }
    
    Expr *visitEnumIsCaseExpr(EnumIsCaseExpr *expr) {
      // Should already be type-checked.
      return simplifyExprType(expr);
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

      auto getType = [&](const Expr *E) -> Type {
        return cs.getType(E);
      };

      Expr *callExpr = CallExpr::createImplicit(ctx, fnRef, { argExpr },
                                                { Identifier() }, getType);

      bool invalid = tc.typeCheckExpression(callExpr, cs.DC,
                                            TypeLoc::withoutLoc(valueType),
                                            CTP_CannotFail);
      cs.cacheExprTypes(callExpr);

      (void) invalid;
      assert(!invalid && "conversion cannot fail");

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
      if (!method->isObjC()) {
        tc.diagnose(E->getLoc(), diag::expr_selector_not_objc,
                    foundDecl->getDescriptiveKind(), foundDecl->getFullName())
          .highlight(subExpr->getSourceRange());
        tc.diagnose(foundDecl, diag::make_decl_objc,
                    foundDecl->getDescriptiveKind())
          .fixItInsert(foundDecl->getAttributeInsertionLoc(false),
                       "@objc ");
        return E;
      }

      // Note which method we're referencing.
      E->setMethod(method);
      return E;
    }

    Expr *visitObjCKeyPathExpr(ObjCKeyPathExpr *E) {
      if (auto semanticE = E->getSemanticExpr())
        cs.setType(E, cs.getType(semanticE));

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
        auto &tc = cs.getTypeChecker();
        tc.useObjectiveCBridgeableConformances(cs.DC, cs.getType(result));
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
                    cs.getType(injection)->getAnyOptionalObjectType())
          .fixItReplace(exclaimLoc, "?");
        tc.diagnose(cast->getStartLoc(), diag::silence_inject_forced_downcast)
          .fixItInsert(cast->getStartLoc(), "(")
          .fixItInsertAfter(cast->getEndLoc(), ")");
      }

      // Set the final types on the expression.
      cs.setExprTypes(result);
    }

    /// Diagnose an optional injection that is probably not what the
    /// user wanted, because it comes from a forced downcast.
    void diagnoseOptionalInjection(InjectIntoOptionalExpr *injection) {
      // Don't diagnose when we're injecting into 
      auto toOptionalType = cs.getType(injection);
      if (toOptionalType->getImplicitlyUnwrappedOptionalObjectType())
        return;

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
static ConcreteDeclRef
resolveLocatorToDecl(ConstraintSystem &cs, ConstraintLocator *locator,
   std::function<Optional<SelectedOverload>(ConstraintLocator *)> findOvlChoice,
   std::function<ConcreteDeclRef (ValueDecl *decl,
                                  Type openedType,
                                  ConstraintLocator *declLocator)>
     getConcreteDeclRef)
{
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
              if (auto *sig = decl->getInnermostDeclContext()
                      ->getGenericSignatureOfContext()) {
                SmallVector<Substitution, 4> subs;
                computeSubstitutions(
                  sig, openedType, locator, subs);
                return ConcreteDeclRef(cs.getASTContext(), decl, subs);
              }
              
              return decl;
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
  bool isSubscript =
    locator->getPath().back().getKind() == ConstraintLocator::SubscriptIndex;
  if (locator->getPath().back().getKind() == ConstraintLocator::ApplyArgument ||
      isSubscript) {
    assert(locator->getPath().back().getNewSummaryFlags() == 0 &&
           "ApplyArgument/SubscriptIndex adds no flags");
    SmallVector<LocatorPathElt, 4> newPath;
    newPath.append(locator->getPath().begin(), locator->getPath().end()-1);

    unsigned newFlags = locator->getSummaryFlags();

    if (isSubscript) {
      newPath.push_back(ConstraintLocator::SubscriptMember);
    } else {
      newPath.push_back(ConstraintLocator::ApplyFunction);
    }
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
  auto ownerFn = cast<AbstractFunctionDecl>(owner.getDecl());
  auto defArg = ownerFn->getDefaultArg(index);
  Expr *init = nullptr;
  switch (defArg.first) {
  case DefaultArgumentKind::None:
    llvm_unreachable("No default argument here?");

  case DefaultArgumentKind::Normal:
    return {nullptr, defArg.first};

  case DefaultArgumentKind::Inherited:
    // Update the owner to reflect inheritance here.
    owner = owner.getOverriddenDecl(tc.Context, &tc);
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

  case DefaultArgumentKind::Nil:
    init = new (tc.Context) NilLiteralExpr(loc, /*Implicit=*/true);
    break;

  case DefaultArgumentKind::EmptyArray:
    init = ArrayExpr::create(tc.Context, loc, {}, {}, loc);
    init->setImplicit();
    break;

  case DefaultArgumentKind::EmptyDictionary:
    init = DictionaryExpr::create(tc.Context, loc, {}, loc);
    init->setImplicit();
    break;
  }

  // Convert the literal to the appropriate type.
  auto defArgType = ownerFn->mapTypeIntoContext(defArg.second);
  bool invalid = tc.typeCheckExpression(init, dc, 
                                        TypeLoc::withoutLoc(defArgType),
                                        CTP_CannotFail);
  cs.cacheExprTypes(init);

  assert(!invalid && "conversion cannot fail");
  (void)invalid;

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
    cs.setType(paren, ParenType::get(ctx, type));
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
  TupleExpr *fromTupleExpr = dyn_cast<TupleExpr>(innerExpr);

  /// Check each of the tuple elements in the destination.
  bool hasVariadic = false;
  unsigned variadicParamIdx = toTuple->getNumElements();
  bool anythingShuffled = false;
  bool hasInits = false;
  SmallVector<TupleTypeElt, 4> toSugarFields;
  SmallVector<TupleTypeElt, 4> fromTupleExprFields(
                                 fromTuple->getElements().size());
  SmallVector<Expr *, 2> callerDefaultArgs;
  ConcreteDeclRef callee =
    findCalleeDeclRef(cs, solution, cs.getConstraintLocator(locator));

  for (unsigned i = 0, n = toTuple->getNumElements(); i != n; ++i) {
    const auto &toElt = toTuple->getElement(i);
    auto toEltType = toElt.getType();

    // If we're default-initializing this member, there's nothing to do.
    if (sources[i] == TupleShuffleExpr::DefaultInitialize) {
      anythingShuffled = true;
      hasInits = true;
      toSugarFields.push_back(toElt);

      // Create a caller-side default argument, if we need one.
      if (auto defArg = getCallerDefaultArg(cs, dc, expr->getLoc(),
                                            callee, i).first) {
        callerDefaultArgs.push_back(defArg);
        sources[i] = TupleShuffleExpr::CallerDefaultInitialize;
      }
      continue;
    }

    // If this is the variadic argument, note it.
    if (sources[i] == TupleShuffleExpr::Variadic) {
      assert(!hasVariadic && "two variadic parameters?");
      toSugarFields.push_back(toElt);
      hasVariadic = true;
      variadicParamIdx = i;
      anythingShuffled = true;
      continue;
    }

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
        TuplePattern *tupleP = dyn_cast<TuplePattern>(typeFromPattern.getValue());
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

  // Convert all of the variadic arguments to the destination type.
  ArraySliceType *arrayType = nullptr;
  if (hasVariadic) {
    Type toEltType = toTuple->getElements()[variadicParamIdx].getVarargBaseTy();
    for (int fromFieldIdx : variadicArgs) {
      const auto &fromElt = fromTuple->getElement(fromFieldIdx);
      Type fromEltType = fromElt.getType();

      // If the source and destination types match, there's nothing to do.
      if (toEltType->isEqual(fromEltType)) {
        fromTupleExprFields[fromFieldIdx] = fromElt;
        continue;
      }

      // We need to convert the source element to the destination type.
      if (!fromTupleExpr) {
        // FIXME: Lame! We can't express this in the AST.
        tc.diagnose(expr->getLoc(),
                    diag::tuple_conversion_not_expressible,
                    fromTuple, toTuple);
        return nullptr;
      }

      // Actually convert the source element.
      auto convertedElt = coerceToType(
                            fromTupleExpr->getElement(fromFieldIdx),
                            toEltType,
                            locator.withPathElement(
                              LocatorPathElt::getTupleElement(fromFieldIdx)));
      if (!convertedElt)
        return nullptr;

      fromTupleExpr->setElement(fromFieldIdx, convertedElt);

      fromTupleExprFields[fromFieldIdx] =
        fromElt.getWithType(cs.getType(convertedElt));
    }

    // Find the appropriate injection function.
    if (tc.requireArrayLiteralIntrinsics(expr->getStartLoc()))
      return nullptr;
    arrayType = cast<ArraySliceType>(
          toTuple->getElements()[variadicParamIdx].getType().getPointer());
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
  Type toSugarType = hasInits? toTuple
                             : TupleType::get(toSugarFields, tc.Context);

  // If we don't have to shuffle anything, we're done.
  if (!anythingShuffled && fromTupleExpr) {
    cs.setType(fromTupleExpr, toSugarType);

    // Update the types of parentheses around the tuple expression.
    rebuildIdentityExprs(cs, expr, toSugarType);

    return expr;
  }
  
  // Create the tuple shuffle.
  ArrayRef<int> mapping = tc.Context.AllocateCopy(sources);
  auto callerDefaultArgsCopy = tc.Context.AllocateCopy(callerDefaultArgs);
  auto shuffle =
    cs.cacheType(new (tc.Context) TupleShuffleExpr(
                     expr, mapping,
                     TupleShuffleExpr::SourceIsTuple,
                     callee,
                     tc.Context.AllocateCopy(variadicArgs),
                     callerDefaultArgsCopy,
                     toSugarType));
  shuffle->setVarargsArrayType(arrayType);
  return shuffle;
}



Expr *ExprRewriter::coerceScalarToTuple(Expr *expr, TupleType *toTuple,
                                        int toScalarIdx,
                                        ConstraintLocatorBuilder locator) {
  auto &tc = solution.getConstraintSystem().getTypeChecker();

  // If the destination type is variadic, compute the injection function to use.
  Type arrayType = nullptr;
  const auto &lastField = toTuple->getElements().back();

  if (lastField.isVararg()) {
    // Find the appropriate injection function.
    arrayType = cast<ArraySliceType>(lastField.getType().getPointer());
    if (tc.requireArrayLiteralIntrinsics(expr->getStartLoc()))
      return nullptr;
  }

  // If we're initializing the varargs list, use its base type.
  const auto &field = toTuple->getElement(toScalarIdx);
  Type toScalarType;
  if (field.isVararg())
    toScalarType = field.getVarargBaseTy();
  else
    toScalarType = field.getType();

  // Coerce the expression to the scalar type.
  expr = coerceToType(expr, toScalarType,
                      locator.withPathElement(
                        ConstraintLocator::ScalarToTuple));
  if (!expr)
    return nullptr;

  // Preserve the sugar of the scalar field.
  // FIXME: This doesn't work if the type has default values because they fail
  // to canonicalize.
  SmallVector<TupleTypeElt, 4> sugarFields;
  bool hasInit = false;
  int i = 0;
  for (auto &field : toTuple->getElements()) {
    if (i == toScalarIdx) {
      if (field.isVararg()) {
        assert(cs.getType(expr)->isEqual(field.getVarargBaseTy()) &&
               "scalar field is not equivalent to dest vararg field?!");

        sugarFields.push_back(field);
      }
      else {
        assert(cs.getType(expr)->isEqual(field.getType()) &&
               "scalar field is not equivalent to dest tuple field?!");
        sugarFields.push_back(field.getWithType(cs.getType(expr)));
      }

      // Record the
    } else {
      sugarFields.push_back(field);
    }
    ++i;
  }

  // Compute the elements of the resulting tuple.
  SmallVector<int, 4> elements;
  SmallVector<unsigned, 1> variadicArgs;
  SmallVector<Expr*, 4> callerDefaultArgs;
  ConcreteDeclRef callee =
    findCalleeDeclRef(cs, solution, cs.getConstraintLocator(locator));

  i = 0;
  for (auto &field : toTuple->getElements()) {
    if (field.isVararg()) {
      elements.push_back(TupleShuffleExpr::Variadic);
      if (i == toScalarIdx) {
        variadicArgs.push_back(i);
        ++i;
        continue;
      }
    }

    // This is the scalar field, so act like we're shuffling the 0th element.
    assert(i == toScalarIdx);
    elements.push_back(0);
    ++i;
  }

  Type destSugarTy = hasInit? toTuple
                            : TupleType::get(sugarFields, tc.Context);

  return cs.cacheType(
      new (tc.Context) TupleShuffleExpr(expr,
                                        tc.Context.AllocateCopy(elements),
                                        TupleShuffleExpr::SourceIsScalar,
                                        callee,
                                        tc.Context.AllocateCopy(variadicArgs),
                                     tc.Context.AllocateCopy(callerDefaultArgs),
                                        destSugarTy));
}

/// Collect the conformances for all the protocols of an existential type.
/// If the source type is also existential, we don't want to check conformance
/// because most protocols do not conform to themselves -- however we still
/// allow the conversion here, except the ErasureExpr ends up with trivial
/// conformances.
static ArrayRef<ProtocolConformanceRef>
collectExistentialConformances(TypeChecker &tc, Type fromType, Type toType,
                               DeclContext *DC) {
  SmallVector<ProtocolDecl *, 4> protocols;
  toType->getAnyExistentialTypeProtocols(protocols);

  SmallVector<ProtocolConformanceRef, 4> conformances;
  for (auto proto : protocols) {
    conformances.push_back(
      *tc.containsProtocol(fromType, proto, DC,
                           (ConformanceCheckFlags::InExpression|
                            ConformanceCheckFlags::Used)));
  }

  return tc.Context.AllocateCopy(conformances);
}

Expr *ExprRewriter::coerceExistential(Expr *expr, Type toType,
                                      ConstraintLocatorBuilder locator) {
  auto &tc = solution.getConstraintSystem().getTypeChecker();
  Type fromType = cs.getType(expr);

  // Handle existential coercions that implicitly look through ImplicitlyUnwrappedOptional<T>.
  if (auto ty = cs.lookThroughImplicitlyUnwrappedOptionalType(fromType)) {
    auto unwrappedExpr
      = coerceImplicitlyUnwrappedOptionalToValue(expr, ty, locator);
    
    auto unwrappedFromType = cs.getType(unwrappedExpr);
    assert(!unwrappedFromType->is<AnyMetatypeType>());

    // FIXME: Hack. We shouldn't try to coerce existential when there is no
    // existential upcast to perform.
    if (unwrappedFromType->isEqual(toType))
      return unwrappedExpr;
  }

  Type fromInstanceType = fromType;
  Type toInstanceType = toType;

  // Look through metatypes
  while (fromInstanceType->is<AnyMetatypeType>() &&
         toInstanceType->is<ExistentialMetatypeType>()) {
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
    
    auto *result =
      cs.cacheType(new (ctx) ErasureExpr(archetypeVal, toType, conformances));
    return cs.cacheType(
        new (ctx) OpenExistentialExpr(expr, archetypeVal, result,
                                      cs.getType(result)));
  }

  // If the type we are trying to coerce is a tuple, let's look through
  // its elements to see if there are any LValue types present, such requires
  // load or address-of operation first before proceeding with erasure.
  if (auto tupleType = fromType->getAs<TupleType>()) {
    bool coerceToRValue = false;
    for (auto element : tupleType->getElements()) {
      if (element.getType()->is<LValueType>()) {
        coerceToRValue = true;
        break;
      }
    }

    // Tuple has one or more LValue types associated with it,
    // which requires coercion to RValue, let's perform it here by creating
    // new tuple type with LValue(s) stripped off and coercing
    // expression to that type, which would do required transformation.
    if (coerceToRValue) {
      SmallVector<TupleTypeElt, 2> elements;
      for (auto &element : tupleType->getElements())
        elements.push_back(TupleTypeElt(element.getType()->getRValueType(),
                                        element.getName(),
                                        element.getParameterFlags()));

      // New type is guaranteed to be a tuple because source type is one.
      auto toTuple = TupleType::get(elements, ctx)->castTo<TupleType>();
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

  return cs.cacheType(new (ctx) ErasureExpr(expr, toType, conformances));
}

Expr *ExprRewriter::coerceOptionalToOptional(Expr *expr, Type toType,
                                             ConstraintLocatorBuilder locator,
                                          Optional<Pattern*> typeFromPattern) {
  auto &tc = cs.getTypeChecker();
  Type fromType = cs.getType(expr);
  
  tc.requireOptionalIntrinsics(expr->getLoc());

  SmallVector<Type, 4> fromOptionals;
  (void)fromType->lookThroughAllAnyOptionalTypes(fromOptionals);

  SmallVector<Type, 4> toOptionals;
  (void)toType->lookThroughAllAnyOptionalTypes(toOptionals);

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

  Type fromValueType = fromType->getAnyOptionalObjectType();
  Type toValueType = toType->getAnyOptionalObjectType();

  expr =
    cs.cacheType(new (tc.Context) BindOptionalExpr(expr,
                                                   expr->getSourceRange().End,
                                                   /*depth*/ 0, fromValueType));
  expr->setImplicit(true);
  expr = coerceToType(expr, toValueType, locator, typeFromPattern);
  if (!expr) return nullptr;
      
  expr = cs.cacheType(new (tc.Context) InjectIntoOptionalExpr(expr, toType));
      
  expr = cs.cacheType(new (tc.Context) OptionalEvaluationExpr(expr, toType));
  expr->setImplicit(true);
  return expr;
}

Expr *ExprRewriter::coerceImplicitlyUnwrappedOptionalToValue(Expr *expr, Type objTy,
                                            ConstraintLocatorBuilder locator) {
  auto optTy = cs.getType(expr);
  // Coerce to an r-value.
  if (optTy->is<LValueType>())
    objTy = LValueType::get(objTy);

  expr = new (cs.getTypeChecker().Context) ForceValueExpr(expr,
                                                          expr->getEndLoc());
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

Expr *ExprRewriter::coerceCallArguments(
    Expr *arg, Type paramType,
    llvm::PointerUnion<ApplyExpr *, LevelTy> applyOrLevel,
    ArrayRef<Identifier> argLabels,
    bool hasTrailingClosure,
    ConstraintLocatorBuilder locator) {

  // Local function to produce a locator to refer to the ith element of the
  // argument tuple.
  auto getArgLocator = [&](unsigned argIdx, unsigned paramIdx)
                         -> ConstraintLocatorBuilder {
    return locator.withPathElement(
             LocatorPathElt::getApplyArgToParam(argIdx, paramIdx));
  };

  bool matchCanFail = false;

  if (paramType->hasUnresolvedType())
    matchCanFail = true;

  // If you value your sanity, ignore the body of this 'if' statement.
  if (cs.getASTContext().isSwiftVersion3()) {
    // Total hack: In Swift 3 mode, we can end up with an arity mismatch due to
    // loss of ParenType sugar.
    if (isa<TupleExpr>(arg))
      if (auto *parenType = dyn_cast<ParenType>(paramType.getPointer()))
        if (isa<TupleType>(parenType->getUnderlyingType().getPointer()))
          paramType = parenType->getUnderlyingType();

    // Total hack: In Swift 3 mode, argument labels are ignored when calling
    // function type with a single Any parameter.
    if (paramType->isAny()) {
      if (auto tupleArgType = dyn_cast<TupleType>(cs.getType(arg).getPointer())) {
        if (tupleArgType->getNumElements() == 1) {
          matchCanFail = true;
        }
      }
    }
  }

  bool allParamsMatch = cs.getType(arg)->isEqual(paramType);

  // Find the callee declaration.
  ConcreteDeclRef callee =
    findCalleeDeclRef(cs, solution, cs.getConstraintLocator(locator));

  // Determine the level,
  unsigned level = 0;
  if (applyOrLevel.is<LevelTy>()) {
    // Level specified by caller.
    level = applyOrLevel.get<LevelTy>();
  } else if (callee) {
    // Determine the level based on the application itself.
    auto apply = applyOrLevel.get<ApplyExpr *>();

    // Only calls to members of types can have level > 0.
    auto calleeDecl = callee.getDecl();
    if (calleeDecl->getDeclContext()->isTypeContext()) {
      // Level 1 if we're not applying "self".
      if (auto call = dyn_cast<CallExpr>(apply)) {
        if (!calleeDecl->isInstanceMember() ||
            !isReferenceToMetatypeMember(cs, call->getDirectCallee()))
          level = 1;
      } else if (isa<PrefixUnaryExpr>(apply) ||
                 isa<PostfixUnaryExpr>(apply) ||
                 isa<BinaryExpr>(apply)) {
        level = 1;
      }
    }
  }

  // Determine the parameter bindings.
  auto params = decomposeParamType(paramType, callee.getDecl(), level);
  auto args = decomposeArgType(cs.getType(arg), argLabels);

  // Quickly test if any further fix-ups for the argument types are necessary.
  // FIXME: This hack is only necessary to work around some problems we have
  // for inferring the type of an unresolved member reference expression in
  // an optional context. We should seek a more holistic fix for this.
  if (allParamsMatch &&
      (params.size() == args.size())) {
    if (auto argTuple = dyn_cast<TupleExpr>(arg)) {
      auto argElts = argTuple->getElements();
    
      for (size_t i = 0; i < params.size(); i++) {
        if (auto dotExpr = dyn_cast<DotSyntaxCallExpr>(argElts[i])) {
          auto paramTy = params[i].Ty->getLValueOrInOutObjectType();
          auto argTy = cs.getType(dotExpr)->getLValueOrInOutObjectType();
          if (!paramTy->isEqual(argTy)) {
            allParamsMatch = false;
            break;
          }
        }
      }
    }
  }
  
  if (allParamsMatch)
    return arg;
  
  MatchCallArgumentListener listener;
  
  SmallVector<ParamBinding, 4> parameterBindings;
  bool failed = constraints::matchCallArguments(args, params,
                                                hasTrailingClosure,
                                                /*allowFixes=*/false, listener,
                                                parameterBindings);

  assert((matchCanFail || !failed) && "Call arguments did not match up?");
  (void)failed;

  // We should either have parentheses or a tuple.
  TupleExpr *argTuple = dyn_cast<TupleExpr>(arg);
  ParenExpr *argParen = dyn_cast<ParenExpr>(arg);
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

  auto &tc = getConstraintSystem().getTypeChecker();
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
      auto paramBaseType = param.Ty;
      assert(sliceType.isNull() && "Multiple variadic parameters?");
      sliceType = tc.getArraySliceType(arg->getLoc(), paramBaseType);
      toSugarFields.push_back(
          TupleTypeElt(sliceType, param.Label, param.parameterFlags));
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
                                                         param.Ty)
                                  : param.Ty,
                                param.Label,
                                param.parameterFlags));

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
    auto paramType = param.Ty;
    if (argType->isEqual(paramType)) {
      toSugarFields.push_back(
          TupleTypeElt(argType, getArgLabel(argIdx), param.parameterFlags));
      fromTupleExprFields[argIdx] =
          TupleTypeElt(paramType, getArgLabel(argIdx), param.parameterFlags);
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
        cs.getType(convertedArg), getArgLabel(argIdx), param.parameterFlags);
    toSugarFields.push_back(
        TupleTypeElt(argType, param.Label, param.parameterFlags));
  }

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
                               cs.getType(fromTupleExpr[0])));
      if (argParenImplicit) {
        argParen->setImplicit();
      }
      arg = argParen;
    } else {
      // coerceToType may have updated the element type of the ParenExpr in
      // place.  If so, propagate the type out to the ParenExpr as well.
      cs.setType(argParen, cs.getType(fromTupleExpr[0]));
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
  if (cs.getType(arg)->isEqual(paramType))
    return arg;

  // If we came from a scalar, create a scalar-to-tuple conversion.
  auto isSourceScalar = TupleShuffleExpr::SourceIsScalar_t(argTuple == nullptr);

  // Create the tuple shuffle.
  ArrayRef<int> mapping = tc.Context.AllocateCopy(sources);
  auto callerDefaultArgsCopy = tc.Context.AllocateCopy(callerDefaultArgs);
  auto *shuffle =
    cs.cacheType(new (tc.Context) TupleShuffleExpr(
                     arg, mapping,
                     isSourceScalar,
                     callee,
                     tc.Context.AllocateCopy(variadicArgs),
                     callerDefaultArgsCopy,
                     paramType));
  shuffle->setVarargsArrayType(sliceType);
  return shuffle;
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

  // If we found an explicit ClosureExpr, update its type.
  if (auto CE = dyn_cast<ClosureExpr>(expr)) {
    cs.setType(CE, toType);
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
    if (cs.getType(singleExpr)->getAs<LValueType>())
      cs.setType(singleExpr,
                 cs.getType(singleExpr)->getLValueOrInOutObjectType());

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
  Type inputType = fnType->getInput();
  auto newClosureType = FunctionType::get(inputType,
                                          tc.Context.TheEmptyTupleType,
                                          fnType->getExtInfo());
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

static CollectionUpcastConversionExpr::ConversionPair
buildElementConversion(ExprRewriter &rewriter,
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
  Expr *conversion = nullptr;

  auto &tc = rewriter.getConstraintSystem().getTypeChecker();
  if (bridged &&
      tc.typeCheckCheckedCast(srcType, destType,
                              CheckedCastContextKind::None, cs.DC,
                              SourceLoc(), nullptr, SourceRange())
        != CheckedCastKind::Coercion) {
    conversion = rewriter.buildObjCBridgeExpr(opaque, destType,
           locator.withPathElement(
             ConstraintLocator::PathElement::getGenericArgument(typeArgIndex)));
  }

  if (!conversion) {
    conversion = rewriter.coerceToType(opaque, destType,
           locator.withPathElement(
             ConstraintLocator::PathElement::getGenericArgument(typeArgIndex)));
  }

  return { opaque, conversion };
}

Expr *ExprRewriter::buildCollectionUpcastExpr(Expr *expr, Type toType,
                                              bool bridged,
                                              ConstraintLocatorBuilder locator) {
  ASTContext &ctx = cs.getASTContext();
  // Build the first value conversion.
  auto conv = buildElementConversion(*this, expr->getLoc(), cs.getType(expr),
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
  auto conv2 = buildElementConversion(*this, expr->getLoc(), cs.getType(expr),
                                      toType, bridged, locator, 1);

  return cs.cacheType(
           new (ctx) CollectionUpcastConversionExpr(expr, toType, conv, conv2));

}

Expr *ExprRewriter::buildObjCBridgeExpr(Expr *expr, Type toType,
                                        ConstraintLocatorBuilder locator) {
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
    Expr *objcExpr = bridgeToObjectiveC(expr);
    if (!objcExpr)
      return nullptr;

    return coerceToType(objcExpr, toType, locator);
  }

  // Bridging from an Objective-C class type to a Swift type.
  return forceBridgeFromObjectiveC(expr, toType);
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
    case ConversionRestrictionKind::TupleToTuple: {
      auto fromTuple = fromType->castTo<TupleType>();
      auto toTuple = toType->castTo<TupleType>();
      SmallVector<int, 4> sources;
      SmallVector<unsigned, 4> variadicArgs;
      bool failed = computeTupleShuffle(fromTuple, toTuple,
                                        sources, variadicArgs);
      assert(!failed && "Couldn't convert tuple to tuple?");
      (void)failed;
      return coerceTupleToTuple(expr, fromTuple, toTuple, locator, sources,
                                variadicArgs, typeFromPattern);
    }

    case ConversionRestrictionKind::ScalarToTuple: {
      auto toTuple = toType->castTo<TupleType>();
      return coerceScalarToTuple(expr, toTuple,
                                 toTuple->getElementForScalarInit(), locator);
    }

    case ConversionRestrictionKind::DeepEquality:
      assert(toType->hasUnresolvedType() && "Should have handled this above");
      break;

    case ConversionRestrictionKind::Superclass: {
      // Coercion from archetype to its (concrete) superclass.
      if (auto fromArchetype = fromType->getAs<ArchetypeType>()) {
        expr =
          cs.cacheType(
              new (tc.Context) ArchetypeToSuperExpr(expr,
                                               fromArchetype->getSuperclass()));

        // If we are done succeeded, use the coerced result.
        if (cs.getType(expr)->isEqual(toType)) {
          return expr;
        }

        fromType = cs.getType(expr);
      }
      
      // Coercion from subclass to superclass.
      return cs.cacheType(new (tc.Context) DerivedToBaseExpr(expr, toType));
    }

    case ConversionRestrictionKind::LValueToRValue: {
      if (toType->is<TupleType>() || fromType->is<TupleType>())
        break;
      
      // Load from the lvalue.
      cs.propagateLValueAccessKind(expr, AccessKind::Read);
      expr = cs.cacheType(
          new (tc.Context) LoadExpr(expr, fromType->getRValueType()));

      // Coerce the result.
      return coerceToType(expr, toType, locator);
    }

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
      assert(toGenericType->getDecl()->classifyAsOptionalType());
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

    case ConversionRestrictionKind::ForceUnchecked: {
      auto valueTy = fromType->getImplicitlyUnwrappedOptionalObjectType();
      assert(valueTy);
      expr = coerceImplicitlyUnwrappedOptionalToValue(expr, valueTy, locator);
      return coerceToType(expr, toType, locator);
    }

    case ConversionRestrictionKind::ArrayUpcast: {
      // Look through implicitly unwrapped optionals.
      if (auto objTy= cs.lookThroughImplicitlyUnwrappedOptionalType(fromType)) {
        expr = coerceImplicitlyUnwrappedOptionalToValue(expr, objTy, locator);
      }

      // Build the value conversion.
      return buildCollectionUpcastExpr(expr, toType, /*bridged=*/false,
                                       locator);
    }

    case ConversionRestrictionKind::HashableToAnyHashable: {
      // Look through implicitly unwrapped optionals.
      if (auto objTy
          = cs.lookThroughImplicitlyUnwrappedOptionalType(cs.getType(expr))) {
        expr = coerceImplicitlyUnwrappedOptionalToValue(expr, objTy, locator);
      }

      // We want to check conformance on the rvalue, as that's what has
      // the Hashable conformance
      expr = cs.coerceToRValue(expr);

      // Find the conformance of the source type to Hashable.
      auto hashable = tc.Context.getProtocol(KnownProtocolKind::Hashable);
      auto conformance =
        tc.conformsToProtocol(cs.getType(expr), hashable, cs.DC,
                              (ConformanceCheckFlags::InExpression |
                               ConformanceCheckFlags::Used));
      assert(conformance && "must conform to Hashable");
      return cs.cacheType(
          new (tc.Context) AnyHashableErasureExpr(expr, toType, *conformance));
    }

    case ConversionRestrictionKind::DictionaryUpcast: {
      // Look through implicitly unwrapped optionals.
      if (auto objTy
          = cs.lookThroughImplicitlyUnwrappedOptionalType(cs.getType(expr))) {
        expr = coerceImplicitlyUnwrappedOptionalToValue(expr, objTy, locator);
      }

      // Build the value conversion.
      return buildCollectionUpcastExpr(expr, toType, /*bridged=*/false,
                                       locator);
    }

    case ConversionRestrictionKind::SetUpcast: {
      // Look through implicitly unwrapped optionals.
      if (auto objTy
          = cs.lookThroughImplicitlyUnwrappedOptionalType(cs.getType(expr))) {
        expr = coerceImplicitlyUnwrappedOptionalToValue(expr, objTy, locator);
      }

      // Build the value conversion.
      return buildCollectionUpcastExpr(expr, toType, /*bridged=*/false, locator);
    }

    case ConversionRestrictionKind::InoutToPointer: {
      OptionalTypeKind optionalKind;
      Type unwrappedTy = toType;
      if (Type unwrapped = toType->getAnyOptionalObjectType(optionalKind))
        unwrappedTy = unwrapped;
      PointerTypeKind pointerKind;
      auto toEltType = unwrappedTy->getAnyPointerElementType(pointerKind);
      assert(toEltType && "not a pointer type?"); (void) toEltType;
      if (pointerKind == PTK_UnsafePointer) {
        // Overwrite the l-value access kind to be read-only if we're
        // converting to a non-mutable pointer type.
        auto *E = cast<InOutExpr>(expr->getValueProvidingExpr())->getSubExpr();
        cs.propagateLValueAccessKind(E,
                                     AccessKind::Read, /*overwrite*/ true);
      }

      tc.requirePointerArgumentIntrinsics(expr->getLoc());
      Expr *result =
          cs.cacheType(new (tc.Context) InOutToPointerExpr(expr, unwrappedTy));
      if (optionalKind != OTK_None)
        result = cs.cacheType(new (tc.Context)
                                  InjectIntoOptionalExpr(result, toType));
      return result;
    }
    
    case ConversionRestrictionKind::ArrayToPointer: {
      OptionalTypeKind optionalKind;
      Type unwrappedTy = toType;
      if (Type unwrapped = toType->getAnyOptionalObjectType(optionalKind))
        unwrappedTy = unwrapped;

      tc.requirePointerArgumentIntrinsics(expr->getLoc());
      Expr *result =
          cs.cacheType(new (tc.Context) ArrayToPointerExpr(expr, unwrappedTy));
      if (optionalKind != OTK_None)
        result = cs.cacheType(new (tc.Context)
                                  InjectIntoOptionalExpr(result, toType));
      return result;
    }
    
    case ConversionRestrictionKind::StringToPointer: {
      OptionalTypeKind optionalKind;
      Type unwrappedTy = toType;
      if (Type unwrapped = toType->getAnyOptionalObjectType(optionalKind))
        unwrappedTy = unwrapped;

      tc.requirePointerArgumentIntrinsics(expr->getLoc());
      Expr *result =
          cs.cacheType(new (tc.Context) StringToPointerExpr(expr, unwrappedTy));
      if (optionalKind != OTK_None)
        result = cs.cacheType(new (tc.Context)
                                  InjectIntoOptionalExpr(result, toType));
      return result;
    }
    
    case ConversionRestrictionKind::PointerToPointer: {
      tc.requirePointerArgumentIntrinsics(expr->getLoc());
      Type unwrappedToTy = toType->getAnyOptionalObjectType();

      // Optional to optional.
      if (Type unwrappedFromTy = cs.getType(expr)->getAnyOptionalObjectType()) {
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
      (void)toIO;
      // In an 'inout' operator like "++i", the operand is converted from
      // an implicit lvalue to an inout argument.
      assert(toIO->getObjectType()->isEqual(fromLValue->getObjectType()));
      cs.propagateLValueAccessKind(expr, AccessKind::ReadWrite);
      return cs.cacheType(new (tc.Context)
                              InOutExpr(expr->getStartLoc(), expr, toType,
                                        /*isImplicit*/ true));
    }

    // If we're actually turning this into an lvalue tuple element, don't
    // load.
    bool performLoad = true;
    if (auto toTuple = toType->getAs<TupleType>()) {
      int scalarIdx = toTuple->getElementForScalarInit();
      if (scalarIdx >= 0 &&
          toTuple->getElementType(scalarIdx)->is<InOutType>())
        performLoad = false;
    }

    if (performLoad) {
      // Load from the lvalue.
      cs.propagateLValueAccessKind(expr, AccessKind::Read);
      expr = cs.cacheType(new (tc.Context)
                              LoadExpr(expr, fromLValue->getObjectType()));

      // Coerce the result.
      return coerceToType(expr, toType, locator);
    }
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

    // Coerce scalar to tuple.
    int toScalarIdx = toTuple->getElementForScalarInit();
    if (toScalarIdx != -1) {
      return coerceScalarToTuple(expr, toTuple, toScalarIdx, locator);
    }
  }

  // Coercion from a subclass to a superclass.
  if (fromType->mayHaveSuperclass() &&
      toType->getClassOrBoundGenericClass()) {
    for (auto fromSuperClass = tc.getSuperClassOf(fromType);
         fromSuperClass;
         fromSuperClass = tc.getSuperClassOf(fromSuperClass)) {
      if (fromSuperClass->isEqual(toType)) {
        
        // Coercion from archetype to its (concrete) superclass.
        if (auto fromArchetype = fromType->getAs<ArchetypeType>()) {
          expr = cs.cacheType(new (tc.Context) ArchetypeToSuperExpr(
              expr, fromArchetype->getSuperclass()));

          // If we succeeded, use the coerced result.
          if (cs.getType(expr)->isEqual(toType))
            return expr;
        }

        // Coercion from subclass to superclass.
        return cs.cacheType(new (tc.Context) DerivedToBaseExpr(expr, toType));
      }
    }
  }

  // Coercions to function type.
  if (auto toFunc = toType->getAs<FunctionType>()) {
    // Coercion to an autoclosure type produces an implicit closure.
    // FIXME: The type checker is more lenient, and allows @autoclosures to
    // be subtypes of non-@autoclosures, which is bogus.
    if (toFunc->isAutoClosure()) {
      // Convert the value to the expected result type of the function.
      expr = coerceToType(expr, toFunc->getResult(),
                          locator.withPathElement(ConstraintLocator::Load));

      // We'll set discriminator values on all the autoclosures in a
      // later pass.
      auto discriminator = AutoClosureExpr::InvalidDiscriminator;
      auto closure = cs.cacheType(
          new (tc.Context) AutoClosureExpr(expr, toType, discriminator, dc));
      closure->setParameterList(ParameterList::createEmpty(tc.Context));

      // Compute the capture list, now that we have analyzed the expression.
      tc.ClosuresWithUncomputedCaptures.push_back(closure);

      return closure;
    }

    // Coercion from one function type to another, this produces a
    // FunctionConversionExpr in its full generality.
    if (auto fromFunc = fromType->getAs<FunctionType>()) {
      // If toType is a NoEscape or NoReturn function type and the expression is
      // a ClosureExpr, propagate these bits onto the ClosureExpr.  Do not
      // *remove* any bits that are already on the closure though.
      // Note that in this case, we do not want to propagate the 'throws' bit
      // to the closure type, as the closure has already been analyzed for
      // throwing subexpressions. We also don't want to change the convention
      // of the original closure.
      auto fromEI = fromFunc->getExtInfo(), toEI = toFunc->getExtInfo();
      if (toEI.isNoEscape() && !fromEI.isNoEscape()) {
        swift::AnyFunctionType::ExtInfo newEI(fromEI.getRepresentation(),
                                        toEI.isAutoClosure(),
                                        toEI.isNoEscape() | fromEI.isNoEscape(),
                                        toEI.throws() & fromEI.throws());
        auto newToType = FunctionType::get(fromFunc->getInput(),
                                           fromFunc->getResult(), newEI);
        if (applyTypeToClosureExpr(cs, expr, newToType)) {
          fromFunc = newToType;
          // Propagating the bits in might have satisfied the entire
          // conversion.  If so, we're done, otherwise keep converting.
          if (fromFunc->isEqual(toType))
            return expr;
        }
      }

      maybeDiagnoseUnsupportedFunctionConversion(cs, expr, toFunc);

      return cs.cacheType(new (tc.Context)
                              FunctionConversionExpr(expr, toType));
    }
  }

  // Coercions from a type to an existential type.
  if (toType->isAnyExistentialType()) {
    return coerceExistential(expr, toType, locator);
  }

  if (toType->getAnyOptionalObjectType() &&
      cs.getType(expr)->getAnyOptionalObjectType()) {
    return coerceOptionalToOptional(expr, toType, locator, typeFromPattern);
  }

  // Coercion to Optional<T>.
  if (auto toGenericType = toType->getAs<BoundGenericType>()) {
    if (toGenericType->getDecl()->classifyAsOptionalType()) {
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

  // Look through ImplicitlyUnwrappedOptional<T> before coercing expression.
  if (auto ty = cs.lookThroughImplicitlyUnwrappedOptionalType(fromType)) {
    expr = coerceImplicitlyUnwrappedOptionalToValue(expr, ty, locator);
    return coerceToType(expr, toType, locator);
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
  auto baseObjectTy = baseTy->getLValueOrInOutObjectType();
  if (auto func = dyn_cast<AbstractFunctionDecl>(member)) {
    // If 'self' is an inout type, turn the base type into an lvalue
    // type with the same qualifiers.
    auto selfTy = func->getInterfaceType()->getAs<AnyFunctionType>()->getInput();
    if (selfTy->is<InOutType>()) {
      // Unless we're looking at a nonmutating existential member.  In which
      // case, the member will be modeled as an inout but ExistentialMemberRef
      // and ArchetypeMemberRef want to take the base as an rvalue.
      if (auto *fd = dyn_cast<FuncDecl>(func))
        if (!fd->isMutating() && baseObjectTy->is<ArchetypeType>())
          return baseObjectTy;
      
      return InOutType::get(baseObjectTy);
    }

    // Otherwise, return the rvalue type.
    return baseObjectTy;
  }

  // If the base of the access is mutable, then we may be invoking a getter or
  // setter that requires the base to be mutable.
  if (auto *SD = dyn_cast<AbstractStorageDecl>(member)) {
    bool isSettableFromHere = SD->isSettable(UseDC)
      && (!UseDC->getASTContext().LangOpts.EnableAccessControl
          || SD->isSetterAccessibleFrom(UseDC));

    // If neither the property's getter nor its setter are mutating, the base
    // can be an rvalue.
    if (!SD->isGetterMutating()
        && (!isSettableFromHere || SD->isSetterNonMutating()))
      return baseObjectTy;

    // If we're calling an accessor, keep the base as an inout type, because the
    // getter may be mutating.
    if (SD->hasAccessorFunctions() && baseTy->is<InOutType>() &&
        semantics != AccessSemantics::DirectToStorage)
      return InOutType::get(baseObjectTy);
  }

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
  if (!toType->is<InOutType>())
    return coerceToType(expr, toType, locator);

  assert(fromType->is<LValueType>() && "Can only convert lvalues to inout");

  auto &ctx = cs.getTypeChecker().Context;

  // Use InOutExpr to convert it to an explicit inout argument for the
  // receiver.
  cs.propagateLValueAccessKind(expr, AccessKind::ReadWrite);
  return cs.cacheType(new (ctx) InOutExpr(expr->getStartLoc(), expr, toType,
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

  // If coercing a literal to an unresolved type, we don't try to look up the
  // witness members, just do it.
  if (type->is<UnresolvedType>()) {
    // Instead of updating the literal expr in place, allocate a new node.  This
    // avoids issues where Builtin types end up on expr nodes and pollute
    // diagnostics.
    literal = cast<LiteralExpr>(literal)->shallowClone(tc.Context, getType);

    // The literal expression has this type.
    cs.setType(literal, type);
    return literal;
  }
  
  // Check whether this literal type conforms to the builtin protocol.
  Optional<ProtocolConformanceRef> builtinConformance;
  if (builtinProtocol &&
      (builtinConformance =
         tc.conformsToProtocol(type, builtinProtocol, cs.DC,
                               ConformanceCheckFlags::InExpression))) {
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
    literal = cast<LiteralExpr>(literal)->shallowClone(tc.Context, getType);

    // The literal expression has this type.
    cs.setType(literal, argType);

    // Call the builtin conversion operation.
    // FIXME: Bogus location info.
    Expr *base =
      TypeExpr::createImplicitHack(literal->getLoc(), type, tc.Context);

    cs.cacheExprTypes(base);
    cs.setExprTypes(base);
    cs.setExprTypes(literal);

    Expr *result = tc.callWitness(base, dc,
                                  builtinProtocol, *builtinConformance,
                                  builtinLiteralFuncName,
                                  literal,
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

    // If the argument type is in error, we're done.
    if (argType->hasError())
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

  // If coercing a literal to an unresolved type, we don't try to look up the
  // witness members, just do it.
  if (type->is<UnresolvedType>()) {
    // Instead of updating the literal expr in place, allocate a new node.  This
    // avoids issues where Builtin types end up on expr nodes and pollute
    // diagnostics.
    literal = cast<LiteralExpr>(literal)->shallowClone(tc.Context, getType);

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
    auto witness = findNamedWitnessImpl<AbstractFunctionDecl>(
                     tc, dc, type->getRValueType(), builtinProtocol,
                     builtinLiteralFuncName, brokenBuiltinProtocolDiag,
                     *builtinConformance);
    if (!witness)
      return nullptr;

    // Form a reference to the builtin conversion function.
    // FIXME: Bogus location info.
    Expr *base = TypeExpr::createImplicitHack(literal->getLoc(), type,
                                              tc.Context);

    Expr *unresolvedDot = new (tc.Context) UnresolvedDotExpr(
                                             base, SourceLoc(),
                                             witness->getFullName(),
                                             DeclNameLoc(base->getEndLoc()),
                                             /*Implicit=*/true);
    (void)tc.typeCheckExpression(unresolvedDot, dc);

    cs.cacheExprTypes(unresolvedDot);

    ConcreteDeclRef builtinRef = unresolvedDot->getReferencedDecl();
    if (!builtinRef) {
      tc.diagnose(base->getLoc(), brokenBuiltinProtocolDiag);
      return nullptr;
    }

    // Set the builtin initializer.
    if (auto stringLiteral = dyn_cast<StringLiteralExpr>(literal))
      stringLiteral->setBuiltinInitializer(builtinRef);
    else {
      cast<MagicIdentifierLiteralExpr>(literal)
        ->setBuiltinInitializer(builtinRef);
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
  auto witness = findNamedWitnessImpl<AbstractFunctionDecl>(
                   tc, dc, type->getRValueType(), protocol,
                   literalFuncName, brokenProtocolDiag,
                   conformance);
  if (!witness)
    return nullptr;

  // Form a reference to the conversion function.
  // FIXME: Bogus location info.
  Expr *base = TypeExpr::createImplicitHack(literal->getLoc(), type,
                                            tc.Context);

  Expr *unresolvedDot = new (tc.Context) UnresolvedDotExpr(
                                           base, SourceLoc(),
                                           witness->getFullName(),
                                           DeclNameLoc(base->getEndLoc()),
                                           /*Implicit=*/true);
  (void)tc.typeCheckExpression(unresolvedDot, dc);
  cs.cacheExprTypes(unresolvedDot);

  ConcreteDeclRef ref = unresolvedDot->getReferencedDecl();
  if (!ref) {
    tc.diagnose(base->getLoc(), brokenProtocolDiag);
    return nullptr;
  }

  // Set the initializer.
  if (auto stringLiteral = dyn_cast<StringLiteralExpr>(literal))
    stringLiteral->setInitializer(ref);
  else
    cast<MagicIdentifierLiteralExpr>(literal)->setInitializer(ref);

  // The literal expression has this type.
  cs.setType(literal, type);

  return literal;
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

  return false;
}

// Non-required constructors may not be not inherited. Therefore when
// constructing a class object, either the metatype must be statically
// derived (rather than an arbitrary value of metatype type) or the referenced
// constructor must be required.
bool
TypeChecker::diagnoseInvalidDynamicConstructorReferences(Expr *base,
                                                     DeclNameLoc memberRefLoc,
                                                     AnyMetatypeType *metaTy,
                                                     ConstructorDecl *ctorDecl,
                                                     bool SuppressDiagnostics) {
  auto ty = metaTy->getInstanceType();
  
  // FIXME: The "hasClangNode" check here is a complete hack.
  if (isNonFinalClass(ty) &&
      !base->isStaticallyDerivedMetatype() &&
      !ctorDecl->hasClangNode() &&
      !(ctorDecl->isRequired() ||
        ctorDecl->getDeclContext()->getAsProtocolOrProtocolExtensionContext())) {
    if (SuppressDiagnostics)
      return false;

    diagnose(memberRefLoc, diag::dynamic_construct_class, ty)
      .highlight(base->getSourceRange());
    auto ctor = cast<ConstructorDecl>(ctorDecl);
    diagnose(ctorDecl, diag::note_nonrequired_initializer,
             ctor->isImplicit(), ctor->getFullName());
  // Constructors cannot be called on a protocol metatype, because there is no
  // metatype to witness it.
  } else if (isa<ConstructorDecl>(ctorDecl) &&
             isa<MetatypeType>(metaTy) &&
             ty->isExistentialType()) {
    if (SuppressDiagnostics)
      return false;

    if (base->isStaticallyDerivedMetatype()) {
      diagnose(memberRefLoc, diag::construct_protocol_by_name, ty)
        .highlight(base->getSourceRange());
    } else {
      diagnose(memberRefLoc, diag::construct_protocol_value, metaTy)
        .highlight(base->getSourceRange());
    }
  }
  return true;
}

Expr *ExprRewriter::finishApply(ApplyExpr *apply, Type openedType,
                                ConstraintLocatorBuilder locator) {
  TypeChecker &tc = cs.getTypeChecker();
  
  auto fn = apply->getFn();

  auto finishApplyOfDeclWithSpecialTypeCheckingSemantics
    = [&](ApplyExpr *apply,
          ValueDecl *decl,
          Type openedType) -> Expr* {
      switch (cs.TC.getDeclTypeCheckingSemantics(decl)) {
      case DeclTypeCheckingSemantics::TypeOf: {
        // Resolve into a DynamicTypeExpr.
        auto arg = apply->getArg();
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
        // Resolve into a MakeTemporarilyEscapingExpr.
        auto arg = cast<TupleExpr>(apply->getArg());
        assert(arg->getNumElements() == 2 && "should have two arguments");
        auto nonescaping = arg->getElements()[0];
        auto body = arg->getElements()[1];
        auto bodyFnTy = cs.getType(body)->castTo<FunctionType>();
        auto escapableType = bodyFnTy->getInput();
        auto resultType = bodyFnTy->getResult();
        
        // The body is immediately called, so is obviously noescape.
        bodyFnTy = cast<FunctionType>(
          bodyFnTy->withExtInfo(bodyFnTy->getExtInfo().withNoEscape()));
        body = coerceToType(body, bodyFnTy, locator);
        assert(body && "can't make nonescaping?!");
        
        auto escapable = new (tc.Context)
          OpaqueValueExpr(apply->getFn()->getLoc(), Type());
        cs.setType(escapable, escapableType);

        auto getType = [&](const Expr *E) -> Type {
          return cs.getType(E);
        };

        auto callSubExpr = CallExpr::create(tc.Context, body, escapable, {}, {},
                                            /*trailing closure*/ false,
                                            /*implicit*/ true, Type(), getType);
        cs.setType(callSubExpr, resultType);
        
        auto replacement = new (tc.Context)
          MakeTemporarilyEscapableExpr(apply->getFn()->getLoc(),
                                       apply->getArg()->getStartLoc(),
                                       nonescaping,
                                       callSubExpr,
                                       apply->getArg()->getEndLoc(),
                                       escapable);
        cs.setType(replacement, resultType);
        return replacement;
      }
      
      case DeclTypeCheckingSemantics::Normal:
        return nullptr;
      }

      llvm_unreachable("Unhandled DeclTypeCheckingSemantics in switch.");
    };

  // The function is always an rvalue.
  fn = cs.coerceToRValue(fn);
  assert(fn && "Rvalue conversion failed?");
  if (!fn)
    return nullptr;
  
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
  
  // Handle applications that look through ImplicitlyUnwrappedOptional<T>.
  if (auto fnTy = cs.lookThroughImplicitlyUnwrappedOptionalType(cs.getType(fn)))
    fn = coerceImplicitlyUnwrappedOptionalToValue(fn, fnTy, locator);

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
  
  apply->setFn(fn);

  // Check whether the argument is 'super'.
  bool isSuper = apply->getArg()->isSuperExpr();

  // For function application, convert the argument to the input type of
  // the function.
  SmallVector<Identifier, 2> argLabelsScratch;
  if (auto fnType = cs.getType(fn)->getAs<FunctionType>()) {
    auto origArg = apply->getArg();
    bool hasTrailingClosure =
      isa<CallExpr>(apply) && cast<CallExpr>(apply)->hasTrailingClosure();
    Expr *arg = coerceCallArguments(origArg, fnType->getInput(),
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

    cs.setExprTypes(apply);
    Expr *result = tc.substituteInputSugarTypeForResult(apply);
    cs.cacheExprTypes(result);

    // Try closing the existential, if there is one.
    closeExistential(result, locator);

    // If we have a covariant result type, perform the conversion now.
    if (covariantResultType) {
      if (covariantResultType->is<FunctionType>())
        result = cs.cacheType(new (tc.Context) CovariantFunctionConversionExpr(
            result, covariantResultType));
      else
        result = cs.cacheType(new (tc.Context) CovariantReturnConversionExpr(
            result, covariantResultType));
    }

    // Extract all arguments.
    auto *CEA = arg;
    if (auto *TSE = dyn_cast<TupleShuffleExpr>(CEA))
      CEA = TSE->getSubExpr();
    // The argument is either a ParenExpr or TupleExpr.
    ArrayRef<Expr *> arguments;
    ArrayRef<TypeBase *> types;

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
    return result;
  }

  // If this is an UnresolvedType in the system, preserve it.
  if (cs.getType(fn)->is<UnresolvedType>()) {
    cs.setType(apply, cs.getType(fn));
    return apply;
  }

  // We have a type constructor.
  auto metaTy = cs.getType(fn)->castTo<AnyMetatypeType>();
  auto ty = metaTy->getInstanceType();

  // If this is an UnresolvedType in the system, preserve it.
  if (ty->is<UnresolvedType>()) {
    cs.setType(apply, ty);
    return apply;
  }

  // If the metatype value isn't a type expression, the user should reference
  // '.init' explicitly, for clarity.
  if (!cs.isTypeReference(fn)) {
    cs.TC.diagnose(apply->getArg()->getStartLoc(),
                   diag::missing_init_on_metatype_initialization)
      .fixItInsert(apply->getArg()->getStartLoc(), ".init");
  }

  // If we're "constructing" a tuple type, it's simply a conversion.
  if (auto tupleTy = ty->getAs<TupleType>()) {
    // FIXME: Need an AST to represent this properly.
    return coerceToType(apply->getArg(), tupleTy, locator);
  }

  // We're constructing a value of nominal type. Look for the constructor or
  // enum element to use.
  assert(ty->getNominalOrBoundGenericNominal() || ty->is<DynamicSelfType>() ||
         ty->isExistentialType() || ty->is<ArchetypeType>());
  auto ctorLocator = cs.getConstraintLocator(
                 locator.withPathElement(ConstraintLocator::ApplyFunction)
                        .withPathElement(ConstraintLocator::ConstructorMember));
  auto selected = getOverloadChoiceIfAvailable(ctorLocator);

  // We have the constructor.
  auto choice = selected->choice;
  auto decl = choice.getDecl();
  
  // Consider the constructor decl reference expr 'implicit', but the
  // constructor call expr itself has the apply's 'implicitness'.
  bool isDynamic = choice.getKind() == OverloadChoiceKind::DeclViaDynamic;
  Expr *declRef = buildMemberRef(fn,
                                 selected->openedFullType,
                                 /*dotLoc=*/SourceLoc(),
                                 decl, DeclNameLoc(fn->getEndLoc()),
                                 selected->openedType,
                                 locator,
                                 ctorLocator,
                                 /*Implicit=*/true,
                                 choice.getFunctionRefKind(),
                                 AccessSemantics::Ordinary,
                                 isDynamic);
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
static bool exprNeedsParensInsideFollowingOperator(TypeChecker &TC,
                                                   DeclContext *DC, Expr *expr,
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
static bool exprNeedsParensOutsideFollowingOperator(TypeChecker &TC,
                                                    DeclContext *DC, Expr *expr,
                                                    Expr *rootExpr,
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

// Return true if, when replacing "<expr>" with "<expr> as T", parentheses need
// to be added around <expr> first in order to maintain the correct precedence.
static bool exprNeedsParensBeforeAddingAs(TypeChecker &TC, DeclContext *DC,
                                          Expr *expr) {
  auto asPG =
    TC.lookupPrecedenceGroup(DC, DC->getASTContext().Id_CastingPrecedence,
                             SourceLoc());
  if (!asPG) return true;
  return exprNeedsParensInsideFollowingOperator(TC, DC, expr, asPG);
}

// Return true if, when replacing "<expr>" with "<expr> as T", parentheses need
// to be added around the new expression in order to maintain the correct
// precedence.
static bool exprNeedsParensAfterAddingAs(TypeChecker &TC, DeclContext *DC,
                                         Expr *expr, Expr *rootExpr) {
  auto asPG =
    TC.lookupPrecedenceGroup(DC, DC->getASTContext().Id_CastingPrecedence,
                             SourceLoc());
  if (!asPG) return true;
  return exprNeedsParensOutsideFollowingOperator(TC, DC, expr, rootExpr, asPG);
}

static bool exprNeedsParensInsidePostfixOperator(TypeChecker &TC,
                                                 DeclContext *DC,
                                                 Expr *expr) {
  // Prefix and infix operators will bind outside of a postfix operator.
  // Postfix operators will get token-merged with a new postfix operator.
  return (isa<PrefixUnaryExpr>(expr) ||
          isa<PostfixUnaryExpr>(expr) ||
          isa<OptionalEvaluationExpr>(expr) ||
          expr->isInfixOperator());
}

namespace {
  class ExprWalker : public ASTWalker {
    ExprRewriter &Rewriter;
    SmallVector<ClosureExpr *, 4> closuresToTypeCheck;

  public:
    ExprWalker(ExprRewriter &Rewriter) : Rewriter(Rewriter) { }

    ~ExprWalker() override {
      // If we're re-typechecking an expression for diagnostics, don't
      // visit closures that have non-single expression bodies.
      if (Rewriter.SkipClosures)
        return;

      auto &cs = Rewriter.getConstraintSystem();
      auto &tc = cs.getTypeChecker();
      for (auto *closure : closuresToTypeCheck)
        tc.typeCheckClosureBody(closure);
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
            } else if (!fnType->getResult()->isUninhabited() &&
                       cs.getType(body)->isUninhabited()) {
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
          closuresToTypeCheck.push_back(closure);
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
  // Can't load from an inout value.
  if (auto *iot = getType(expr)->getAs<InOutType>()) {
    // Emit a fixit if we can find the & expression that turned this into an
    // inout.
    auto &tc = getTypeChecker();
    if (auto addrOf =
        dyn_cast<InOutExpr>(expr->getSemanticsProvidingExpr())) {
      tc.diagnose(expr->getLoc(), diag::load_of_explicit_lvalue,
                  iot->getObjectType())
        .fixItRemove(SourceRange(addrOf->getLoc()));
      return coerceToRValue(addrOf->getSubExpr());
    } else {
      tc.diagnose(expr->getLoc(), diag::load_of_explicit_lvalue,
                  iot->getObjectType());
      return expr;
    }
  }

  // If we already have an rvalue, we're done, otherwise emit a load.
  if (auto lvalueTy = getType(expr)->getAs<LValueType>()) {
    propagateLValueAccessKind(expr, AccessKind::Read);
    return cacheType(new (getASTContext())
                         LoadExpr(expr, lvalueTy->getObjectType()));
  }

  return expr;
}


/// Emit the fixes computed as part of the solution, returning true if we were
/// able to emit an error message, or false if none of the fixits worked out.
bool ConstraintSystem::applySolutionFixes(Expr *E, const Solution &solution) {
  bool diagnosed = false;
  for (unsigned i = 0, e = solution.Fixes.size(); i != e; ++i)
    diagnosed |= applySolutionFix(E, solution, i);

  return diagnosed;
}




/// \brief Apply the specified Fix # to this solution, producing a fixit hint
/// diagnostic for it and returning true.  If the fixit hint turned out to be
/// bogus, this returns false and doesn't emit anything.
bool ConstraintSystem::applySolutionFix(Expr *expr,
                                        const Solution &solution,
                                        unsigned fixNo) {
  auto &fix = solution.Fixes[fixNo];
  
  // Some fixes need more information from the locator.
  ConstraintLocator *locator = fix.second;

  // In the case of us having applied a type member constraint against a
  // synthesized type variable during diagnostic generation, we may not have
  // a valid locator.
  if (!locator)
    return false;

  // Resolve the locator to a specific expression.
  SourceRange range;
  bool isSubscriptMember =
    (!locator->getPath().empty() &&
     locator->getPath().back().getKind() == ConstraintLocator::SubscriptMember);
  ConstraintLocator *resolved = simplifyLocator(*this, locator, range);

  // If we didn't manage to resolve directly to an expression, we don't
  // have a great diagnostic to give, so bail.
  if (!resolved || !resolved->getAnchor() ||
      !resolved->getPath().empty())
    return false;
  
  Expr *affected = resolved->getAnchor();

  // FIXME: Work around an odd locator representation that doesn't separate the
  // base of a subscript member from the member access.
  if (isSubscriptMember) {
    if (auto subscript = dyn_cast<SubscriptExpr>(affected))
      affected = subscript->getBase();
  }
    
  switch (fix.first.getKind()) {
  case FixKind::None:
    llvm_unreachable("no-fix marker should never make it into solution");

  case FixKind::ForceOptional: {
    const Expr *unwrapped = affected->getValueProvidingExpr();
    auto type = solution.simplifyType(getType(affected))
                  ->getRValueObjectType();

    if (auto tryExpr = dyn_cast<OptionalTryExpr>(unwrapped)) {
      TC.diagnose(tryExpr->getTryLoc(), diag::missing_unwrap_optional_try,
                  type)
        .fixItReplace({tryExpr->getTryLoc(), tryExpr->getQuestionLoc()},
                      "try!");

    } else {
      auto diag = TC.diagnose(affected->getLoc(),
                              diag::missing_unwrap_optional, type);
      bool parensNeeded =
        exprNeedsParensInsidePostfixOperator(TC, DC, affected);

      if (parensNeeded) {
        diag.fixItInsert(affected->getStartLoc(), "(")
            .fixItInsertAfter(affected->getEndLoc(), ")!");
      } else {
        diag.fixItInsertAfter(affected->getEndLoc(), "!");
      }
    }
    return true;
  }
          
  case FixKind::OptionalChaining: {
    auto type = solution.simplifyType(getType(affected))
                ->getRValueObjectType();
    auto diag = TC.diagnose(affected->getLoc(),
                            diag::missing_unwrap_optional, type);
    diag.fixItInsertAfter(affected->getEndLoc(), "?");
    return true;
  }

  case FixKind::ForceDowncast: {
    auto fromType = solution.simplifyType(getType(affected))
                      ->getRValueObjectType();
    Type toType = solution.simplifyType(fix.first.getTypeArgument(*this));
    bool useAs = TC.isExplicitlyConvertibleTo(fromType, toType, DC);
    bool useAsBang = !useAs && TC.checkedCastMaySucceed(fromType, toType,
                                                        DC);
    if (!useAs && !useAsBang)
      return false;

    bool needsParensInside = exprNeedsParensBeforeAddingAs(TC, DC, affected);
    bool needsParensOutside = exprNeedsParensAfterAddingAs(TC, DC, affected,
                                                           expr);
    llvm::SmallString<2> insertBefore;
    llvm::SmallString<32> insertAfter;
    if (needsParensOutside) {
      insertBefore += "(";
    }
    if (needsParensInside) {
      insertBefore += "(";
      insertAfter += ")";
    }
    insertAfter += useAs ? " as " : " as! ";
    insertAfter += toType.getString();
    if (needsParensOutside)
      insertAfter += ")";
    
    auto diagID = useAs ? diag::missing_explicit_conversion
                        : diag::missing_forced_downcast;
    auto diag = TC.diagnose(affected->getLoc(), diagID, fromType, toType);
    if (!insertBefore.empty()) {
      diag.fixItInsert(affected->getStartLoc(), insertBefore);
    }
    diag.fixItInsertAfter(affected->getEndLoc(), insertAfter);
    return true;
  }

  case FixKind::AddressOf: {
    auto type = solution.simplifyType(getType(affected))
                  ->getRValueObjectType();
    TC.diagnose(affected->getLoc(), diag::missing_address_of, type)
      .fixItInsert(affected->getStartLoc(), "&");
    return true;
  }

  case FixKind::CoerceToCheckedCast: {
    if (auto *coerceExpr = dyn_cast<CoerceExpr>(locator->getAnchor())) {
      Expr *subExpr = coerceExpr->getSubExpr();
      auto fromType =
        solution.simplifyType(getType(subExpr))->getRValueType();
      auto toType =
        solution.simplifyType(coerceExpr->getCastTypeLoc().getType());
      auto castKind = TC.typeCheckCheckedCast(
                        fromType, toType, CheckedCastContextKind::None, DC,
                        coerceExpr->getLoc(), subExpr,
                        coerceExpr->getCastTypeLoc().getSourceRange());
      
      switch (castKind) {
      // Invalid cast.
      case CheckedCastKind::Unresolved:
        // Fix didn't work, let diagnoseFailureForExpr handle this.
        return false;
      case CheckedCastKind::Coercion:
      case CheckedCastKind::BridgingCoercion:
        llvm_unreachable("Coercions handled in other disjunction branch");

      // Valid casts.
      case CheckedCastKind::Swift3BridgingDowncast: {
        // Swift 3 accepted coercions from NSNumber and NSValue to Swift
        // value types, even though there are multiple Swift types that
        // bridge to those classes, and the bridging operation back into Swift
        // is type-checked. For compatibility, downgrade to a warning.
        assert(TC.Context.LangOpts.isSwiftVersion3()
               && "should only appear in Swift 3 compat mode");

        TC.diagnose(coerceExpr->getLoc(),
                    diag::missing_forced_downcast_swift3_compat_warning,
                    fromType, toType)
          .highlight(coerceExpr->getSourceRange())
          .fixItReplace(coerceExpr->getLoc(), "as!");

        // This is just a warning, so allow the expression to type-check.
        return false;
      }

      case CheckedCastKind::ArrayDowncast:
      case CheckedCastKind::DictionaryDowncast:
      case CheckedCastKind::SetDowncast:
      case CheckedCastKind::ValueCast:
        TC.diagnose(coerceExpr->getLoc(), diag::missing_forced_downcast,
                    fromType, toType)
          .highlight(coerceExpr->getSourceRange())
          .fixItReplace(coerceExpr->getLoc(), "as!");
        return true;
      }
    }
    return false;
  }
  }

  // FIXME: It would be really nice to emit a follow-up note showing where
  // we got the other type information from, e.g., the parameter we're
  // initializing.
  return false;
}


/// \brief Apply a given solution to the expression, producing a fully
/// type-checked expression.
Expr *ConstraintSystem::applySolution(Solution &solution, Expr *expr,
                                      Type convertType,
                                      bool discardedExpr,
                                      bool suppressDiagnostics,
                                      bool skipClosures) {
  // If any fixes needed to be applied to arrive at this solution, resolve
  // them to specific expressions.
  if (!solution.Fixes.empty()) {
    // If we can diagnose the problem with the fixits that we've pre-assumed,
    // do so now.
    if (applySolutionFixes(expr, solution))
      return nullptr;

    // If we didn't manage to diagnose anything well, so fall back to
    // diagnosing mining the system to construct a reasonable error message.
    diagnoseFailureForExpr(expr);
    return nullptr;
  }

  ExprRewriter rewriter(*this, solution, suppressDiagnostics, skipClosures);
  ExprWalker walker(rewriter);

  // Apply the solution to the expression.
  auto result = expr->walk(walker);
  if (!result)
    return nullptr;

  // If we're supposed to convert the expression to some particular type,
  // do so now.
  if (convertType) {
    result = rewriter.coerceToType(result, convertType,
                                   getConstraintLocator(expr));
    if (!result)
      return nullptr;
  } else if (getType(result)->isLValueType() && !discardedExpr) {
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
  ExprRewriter rewriter(*this, solution, suppressDiagnostics,
                        /*skipClosures=*/false);
  rewriter.walkToExprPre(expr);
  Expr *result = rewriter.walkToExprPost(expr);
  if (result)
    rewriter.finalize(result);
  return result;
}

Expr *Solution::coerceToType(Expr *expr, Type toType,
                             ConstraintLocator *locator,
                             bool ignoreTopLevelInjection,
                             bool skipClosures,
                             Optional<Pattern*> typeFromPattern) const {
  auto &cs = getConstraintSystem();
  ExprRewriter rewriter(cs, *this,
                        /*suppressDiagnostics=*/false,
                        /*skipClosures=*/skipClosures);
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
  unsigned index = 0;
  if (afd->getImplicitSelfDecl())
    ++index;

  for (auto param : *afd->getParameterList(index))
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

  // Find the witness we need to use.
  auto type = base->getType();
  assert(!type->hasTypeVariable() &&
         "Building call to witness with unresolved base type!");

  if (auto metaType = type->getAs<AnyMetatypeType>())
    type = metaType->getInstanceType();
  
  auto witness = findNamedWitnessImpl<AbstractFunctionDecl>(
                   *this, dc, type->getRValueType(), protocol,
                   name, brokenProtocolDiag);
  if (!witness)
    return nullptr;

  // Form a syntactic expression that describes the reference to the
  // witness.
  // FIXME: Egregious hack.
  auto unresolvedDot = new (Context) UnresolvedDotExpr(
                                       base, SourceLoc(),
                                       witness->getFullName(),
                                       DeclNameLoc(base->getEndLoc()),
                                       /*Implicit=*/true);
  unresolvedDot->setFunctionRefKind(FunctionRefKind::SingleApply);
  auto dotLocator = cs.getConstraintLocator(unresolvedDot);

  // Form a reference to the witness itself.
  Type openedFullType, openedType;
  std::tie(openedFullType, openedType)
    = cs.getTypeOfMemberReference(base->getType(), witness, dc,
                                  /*isTypeReference=*/false,
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
  auto argLabels = witness->getFullName().getArgumentNames();
  if (arguments.size() == 1 &&
      (isVariadicWitness(witness) ||
       argumentNamesMatch(arguments[0]->getType(), argLabels))) {
    call = CallExpr::create(Context, unresolvedDot, arguments[0], {}, {},
                            /*hasTrailingClosure=*/false,
                            /*implicit=*/true, Type(), getType);
  } else {
    // The tuple should have the source range enclosing its arguments unless
    // they are invalid or there are no arguments.
    SourceLoc TupleStartLoc = base->getStartLoc();
    SourceLoc TupleEndLoc = base->getEndLoc();
    if (arguments.size() > 0) {
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

  // Add the conversion from the argument to the function parameter type.
  cs.addConstraint(ConstraintKind::ArgumentTupleConversion,
                   call->getArg()->getType(),
                   openedType->castTo<FunctionType>()->getInput(),
                   cs.getConstraintLocator(call,
                                           ConstraintLocator::ApplyArgument));

  // Solve the system.
  SmallVector<Solution, 1> solutions;
  
  cs.cacheExprTypes(call);

  // If the system failed to produce a solution, post any available diagnostics.
  if (cs.solve(solutions) || solutions.size() != 1) {
    cs.salvage(solutions, base);
    return nullptr;
  }

  Solution &solution = solutions.front();
  ExprRewriter rewriter(cs, solution,
                        /*suppressDiagnostics=*/false,
                        /*skipClosures=*/false);

  auto memberRef = rewriter.buildMemberRef(base, openedFullType,
                                           base->getStartLoc(),
                                           witness,
                                           DeclNameLoc(base->getEndLoc()),
                                           openedType, dotLocator, dotLocator,
                                           /*Implicit=*/true,
                                           FunctionRefKind::SingleApply,
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
Solution::convertBooleanTypeToBuiltinI1(Expr *expr, ConstraintLocator *locator) const {
  // FIXME: Cache name.
  auto &cs = getConstraintSystem();
  auto &tc = cs.getTypeChecker();
  auto &ctx = tc.Context;
  auto type = cs.getType(expr)->getRValueType();

  // Member accesses are permitted to implicitly look through
  // ImplicitlyUnwrappedOptional<T>.
  if (auto objTy = cs.lookThroughImplicitlyUnwrappedOptionalType(type)) {
    expr = new (ctx) ForceValueExpr(expr, expr->getEndLoc());
    cs.setType(expr, objTy);
    expr->setImplicit();
    type = objTy;
  }

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
    return nullptr;
  }
  FuncDecl *builtinMethod = dyn_cast<FuncDecl>(members[0].Decl);
  if (!builtinMethod) {
    tc.diagnose(expr->getLoc(), diag::broken_bool);
    return nullptr;
  }

  // Form a reference to the builtin method.
  Expr *memberRef = new (ctx) MemberRefExpr(expr, SourceLoc(),
                                            builtinMethod,
                                            DeclNameLoc(expr->getLoc()),
                                            /*Implicit=*/true);
  cs.cacheSubExprTypes(memberRef);
  cs.setSubExprTypes(memberRef);
  bool failed = tc.typeCheckExpressionShallow(memberRef, cs.DC);
  cs.cacheExprTypes(memberRef);
  assert(!failed && "Could not reference witness?");
  (void)failed;

  // Call the builtin method.
  auto getType = [&](const Expr *E) -> Type {
    return cs.getType(E);
  };

  expr = CallExpr::createImplicit(ctx, memberRef, { }, { }, getType);
  cs.cacheSubExprTypes(expr);
  cs.setSubExprTypes(expr);
  failed = tc.typeCheckExpressionShallow(expr, cs.DC);
  cs.cacheExprTypes(expr);
  assert(!failed && "Could not call witness?");
  (void)failed;

  if (expr && !cs.getType(expr)->isBuiltinIntegerType(1)) {
    tc.diagnose(expr->getLoc(), diag::broken_bool);
    return nullptr;
  }

  return expr;
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
