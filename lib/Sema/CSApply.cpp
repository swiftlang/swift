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

/// Retrieve the fixed type for the given type variable.
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
  auto archetype = type->getAs<OpenedArchetypeType>();
  if (!archetype)
    return false;

  return archetype->getOpenedExistentialType()->isAnyObject();
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

  auto lookupConformanceFn =
      [&](CanType original, Type replacement, ProtocolDecl *protoType)
          -> Optional<ProtocolConformanceRef> {
    if (replacement->hasError() ||
        isOpenedAnyObject(replacement) ||
        replacement->is<GenericTypeParamType>()) {
      return ProtocolConformanceRef(protoType);
    }

    return TypeChecker::conformsToProtocol(replacement, protoType,
                                           getConstraintSystem().DC,
                                           ConformanceCheckFlags::InExpression);
  };

  return SubstitutionMap::get(sig,
                              QueryTypeSubstitutionMap{subs},
                              lookupConformanceFn);
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

  // Check whether this is a member access on 'self'.
  bool isAccessOnSelf = false;
  if (auto *baseDRE = dyn_cast<DeclRefExpr>(base->getValueProvidingExpr()))
    if (auto *baseVar = dyn_cast<VarDecl>(baseDRE->getDecl()))
      isAccessOnSelf = baseVar->isSelfParameter();

  // If the value is always directly accessed from this context, do it.
  return member->getAccessSemanticsFromContext(DC, isAccessOnSelf);
}

/// This extends functionality of `Expr::isTypeReference` with
/// support for `UnresolvedDotExpr` and `UnresolvedMemberExpr`.
/// This method could be used on not yet fully type-checked AST.
bool ConstraintSystem::isTypeReference(const Expr *E) {
  return E->isTypeReference(
      [&](const Expr *E) -> Type { return simplifyType(getType(E)); },
      [&](const Expr *E) -> Decl * {
        if (auto *UDE = dyn_cast<UnresolvedDotExpr>(E)) {
          return findResolvedMemberRef(
              getConstraintLocator(UDE, ConstraintLocator::Member));
        }

        if (auto *UME = dyn_cast<UnresolvedMemberExpr>(E)) {
          return findResolvedMemberRef(
              getConstraintLocator(UME, ConstraintLocator::UnresolvedMember));
        }

        if (isa<OverloadSetRefExpr>(E))
          return findResolvedMemberRef(
              getConstraintLocator(const_cast<Expr *>(E)));

        return nullptr;
      });
}

bool ConstraintSystem::isStaticallyDerivedMetatype(const Expr *E) {
  return E->isStaticallyDerivedMetatype(
      [&](const Expr *E) -> Type { return simplifyType(getType(E)); },
      [&](const Expr *E) -> bool { return isTypeReference(E); });
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
    case KeyPathExpr::Component::Kind::TupleElement:
    case KeyPathExpr::Component::Kind::Subscript:
      // Subscripts and tuples aren't generally represented in KVC.
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
  
  // If there are no non-identity components, this is the "self" key.
  if (buf.empty()) {
    auto self = StringRef("self");
    buf.append(self.begin(), self.end());
  }
  
  return true;
}

/// Form a type checked expression for the index of a @dynamicMemberLookup
/// subscript index parameter.
/// The index expression will have a tuple type of `(dynamicMember: T)`.
static Expr *buildDynamicMemberLookupIndexExpr(StringRef name, SourceLoc loc,
                                               DeclContext *dc,
                                               ConstraintSystem &cs) {
  auto &ctx = cs.TC.Context;

  auto *stringDecl = ctx.getStringDecl();
  auto stringType = stringDecl->getDeclaredType();

  // Build and type check the string literal index value to the specific
  // string type expected by the subscript.
  auto *nameExpr = new (ctx) StringLiteralExpr(name, loc, /*implicit*/true);
  nameExpr->setBuiltinInitializer(ctx.getStringBuiltinInitDecl(stringDecl));
  nameExpr->setType(stringType);

  cs.cacheExprTypes(nameExpr);
  return nameExpr;
}

namespace {

  /// Rewrites an expression by applying the solution of a constraint
  /// system to that expression.
  class ExprRewriter : public ExprVisitor<ExprRewriter, Expr *> {
  public:
    ConstraintSystem &cs;
    DeclContext *dc;
    const Solution &solution;
    bool SuppressDiagnostics;

    /// Coerce the given tuple to another tuple type.
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
    Expr *coerceTupleToTuple(Expr *expr, TupleType *fromTuple,
                             TupleType *toTuple,
                             ConstraintLocatorBuilder locator,
                             ArrayRef<unsigned> sources);

    /// Coerce a subclass, class-constrained archetype, class-constrained
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

    /// Coerce the given value to existential type.
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

    /// Coerce an expression of (possibly unchecked) optional
    /// type to have a different (possibly unchecked) optional type.
    Expr *coerceOptionalToOptional(Expr *expr, Type toType,
                                   ConstraintLocatorBuilder locator,
                                   Optional<Pattern*> typeFromPattern = None);

    /// Coerce an expression of implicitly unwrapped optional type to its
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

    /// Build a collection upcast expression.
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
    /// Build a reference to the given declaration.
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
              TypeChecker::conformsToProtocol(
                        baseTy, proto, cs.DC,
                        ConformanceCheckFlags::InExpression);
            if (conformance && conformance->isConcrete()) {
              if (auto witness =
                      conformance->getConcrete()->getWitnessDecl(decl)) {
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

      auto type = solution.simplifyType(openedType);

      if (isa<TypeDecl>(decl) && !isa<ModuleDecl>(decl)) {
        auto typeExpr = TypeExpr::createImplicitHack(
          loc.getBaseNameLoc(), type->getMetatypeInstanceType(),
          ctx);
        cs.cacheType(typeExpr);
        return typeExpr;
      }

      auto substitutions =
          solution.computeSubstitutions(
            decl->getInnermostDeclContext()->getGenericSignatureOfContext(),
            locator);
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
      OpenedArchetypeType *Archetype;

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
    Expr *openExistentialReference(Expr *base, OpenedArchetypeType *archetype,
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
      auto archetypeVal =
          new (ctx) OpaqueValueExpr(base->getSourceRange(), opaqueType);
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

    /// Build a new member reference with the given base and member.
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
      bool isExistentialMetatype = false;
      if (auto baseMeta = baseTy->getAs<AnyMetatypeType>()) {
        baseIsInstance = false;
        isExistentialMetatype = baseMeta->is<ExistentialMetatypeType>();
        baseTy = baseMeta->getInstanceType();
      }

      // Build a member reference.
      SubstitutionMap substitutions =
        solution.computeSubstitutions(
            member->getInnermostDeclContext()->getGenericSignatureOfContext(),
            memberLocator);
      auto memberRef = ConcreteDeclRef(member, substitutions);

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

      // If we're referring to a member type, it's just a type
      // reference.
      if (isa<TypeDecl>(member)) {
        Type refType = simplifyType(openedType);
        auto ref =
            TypeExpr::createImplicitHack(memberLoc.getBaseNameLoc(),
                                         refType, context);
        cs.setType(ref, refType);
        auto *result = new (context) DotSyntaxBaseIgnoredExpr(
            base, dotLoc, ref, refType);
        cs.setType(result, refType);
        return result;
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
          if (func->hasDynamicSelfResult() &&
              !baseTy->getOptionalObjectType()) {
            refTy = refTy->replaceCovariantResultType(containerTy, 2);
            if (!baseTy->isEqual(containerTy)) {
              dynamicSelfFnType = refTy->replaceCovariantResultType(baseTy, 2);
            }
          }
        } else if (auto *decl = dyn_cast<VarDecl>(member)) {
          if (decl->getValueInterfaceType()->hasDynamicSelfType()) {
            refTy = refTy->replaceCovariantResultType(containerTy, 1);
            if (!baseTy->isEqual(containerTy)) {
              dynamicSelfFnType = refTy->replaceCovariantResultType(baseTy, 1);
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
        if (!isExistentialMetatype || openedExistential) {
          // Convert the base to an rvalue of the appropriate metatype.
          base = coerceToType(base,
                              MetatypeType::get(
                                isDynamic ? selfTy : containerTy),
                              locator.withPathElement(
                                ConstraintLocator::MemberRefBase));
        }

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

      // For properties, build member references.
      if (isa<VarDecl>(member)) {
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
        if (dynamicSelfFnType) {
          result = new (context) CovariantReturnConversionExpr(result,
                                                            dynamicSelfFnType);
          cs.cacheType(result);
          cs.setType(result, simplifyType(openedType));
        }
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
    
    /// Describes either a type or the name of a type to be resolved.
    using TypeOrName = llvm::PointerUnion<Identifier, Type>;

    /// Convert the given literal expression via a protocol pair.
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

    /// Finish a function application by performing the appropriate
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

    // Resolve `@dynamicCallable` applications.
    Expr *finishApplyDynamicCallable(ApplyExpr *apply,
                                     SelectedOverload selected,
                                     FuncDecl *method,
                                     AnyFunctionType *methodType,
                                     ConstraintLocatorBuilder applyFunctionLoc);

  private:
    /// Simplify the given type by substituting all occurrences of
    /// type variables for their fixed types.
    Type simplifyType(Type type) {
      return solution.simplifyType(type);
    }

  public:


    /// Coerce a closure expression with a non-Void return type to a
    /// contextual function type with a Void return type.
    ///
    /// This operation cannot fail.
    ///
    /// \param expr The closure expression to coerce.
    ///
    /// \returns The coerced closure expression.
    ///
    ClosureExpr *coerceClosureExprToVoid(ClosureExpr *expr);

    /// Coerce a closure expression with a Never return type to a
    /// contextual function type with some other return type.
    ///
    /// This operation cannot fail.
    ///
    /// \param expr The closure expression to coerce.
    ///
    /// \returns The coerced closure expression.
    ///
    ClosureExpr *coerceClosureExprFromNever(ClosureExpr *expr);
    
    /// Coerce the given expression to the given type.
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
    
    /// Coerce the given expression (which is the argument to a call) to
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

    /// Coerce the given object argument (e.g., for the base of a
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
    /// Build a new subscript.
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

        if (selected->choice.getKind() ==
                OverloadChoiceKind::KeyPathDynamicMemberLookup &&
            !isa<SubscriptExpr>(locator.getAnchor()))
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

          // Coerce the index to the key path's type
          indexKP = coerceToType(indexKP, keyPathTy, locator);

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

      auto &tc = cs.getTypeChecker();
      auto baseTy = cs.getType(base)->getRValueType();
      
      bool baseIsInstance = true;
      if (auto baseMeta = baseTy->getAs<AnyMetatypeType>()) {
        baseIsInstance = false;
        baseTy = baseMeta->getInstanceType();
      }

      // Check whether the base is 'super'.
      bool isSuper = base->isSuperExpr();

      // Use the correct locator kind based on the subscript kind.
      auto locatorKind = ConstraintLocator::SubscriptMember;
      if (choice.getKind() == OverloadChoiceKind::DynamicMemberLookup)
        locatorKind = ConstraintLocator::Member;

      if (choice.getKind() == OverloadChoiceKind::KeyPathDynamicMemberLookup) {
        locatorKind = isa<SubscriptExpr>(locator.getAnchor())
                          ? ConstraintLocator::SubscriptMember
                          : ConstraintLocator::Member;
      }

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
      if (choice.getKind() != OverloadChoiceKind::DynamicMemberLookup &&
          choice.getKind() != OverloadChoiceKind::KeyPathDynamicMemberLookup) {
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
      
      if (baseIsInstance) {
        base = coerceObjectArgumentToType(
          base, containerTy, subscript, AccessSemantics::Ordinary,
          locator.withPathElement(ConstraintLocator::MemberRefBase));
      } else {
        base = coerceToType(base,
                            MetatypeType::get(containerTy),
                            locator.withPathElement(
                              ConstraintLocator::MemberRefBase));
        
        if (!base)
          return nullptr;
        
        base = cs.coerceToRValue(base);
      }
      if (!base)
        return nullptr;

      // Form the subscript expression.
      auto subscriptExpr = SubscriptExpr::create(
        tc.Context, base, index, subscriptRef, isImplicit, semantics, getType);
      cs.setType(subscriptExpr, resultTy);
      subscriptExpr->setIsSuper(isSuper);

      Expr *result = subscriptExpr;
      closeExistential(result, locator);

      if (subscript->getElementInterfaceType()->hasDynamicSelfType()) {
        auto dynamicSelfFnType =
          openedFullFnType->replaceCovariantResultType(baseTy, 2);
        result = new (tc.Context) CovariantReturnConversionExpr(result,
                                                            dynamicSelfFnType);
        cs.cacheType(result);
        cs.setType(result, simplifyType(baseTy));
      }

      return result;
    }

    /// Build a new reference to another constructor.
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

    /// Build an implicit argument for keypath based dynamic lookup,
    /// which consists of KeyPath expression and a single component.
    ///
    /// \param keyPathTy The type of the keypath argument.
    /// \param dotLoc The location of the '.' preceding member name.
    /// \param memberLoc The locator to be associated with new argument.
    Expr *buildKeyPathDynamicMemberIndexExpr(BoundGenericType *keyPathTy,
                                             SourceLoc dotLoc,
                                             ConstraintLocator *memberLoc) {
      auto &ctx = cs.getASTContext();
      auto *anchor = memberLoc->getAnchor();

      KeyPathExpr::Component component;

      // Let's create a KeyPath expression and fill in "parsed path"
      // after component is built.
      auto *keyPath = new (ctx) KeyPathExpr(/*backslashLoc=*/dotLoc,
                                            /*parsedRoot=*/nullptr,
                                            /*parsedPath=*/anchor,
                                            /*isImplicit=*/true);
      // Type of the keypath expression we are forming is known
      // in advance, so let's set it right away.
      keyPath->setType(keyPathTy);
      cs.cacheType(keyPath);

      auto *componentLoc = cs.getConstraintLocator(
          memberLoc,
          LocatorPathElt::KeyPathDynamicMember(keyPathTy->getAnyNominal()));
      auto overload = solution.getOverloadChoice(componentLoc);

      // Looks like there is a chain of implicit `subscript(dynamicMember:)`
      // calls necessary to resolve a member reference.
      if (overload.choice.getKind() ==
          OverloadChoiceKind::KeyPathDynamicMemberLookup) {
        keyPath->resolveComponents(ctx,
                                   buildKeyPathSubscriptComponent(
                                       overload, dotLoc, /*indexExpr=*/nullptr,
                                       ctx.Id_dynamicMember, componentLoc));
        return keyPath;
      }

      // We can't reuse existing expression because type-check
      // based diagnostics could hold the reference to original AST.
      Expr *componentExpr = nullptr;
      auto *dotExpr = new (ctx) KeyPathDotExpr(dotLoc);

      // Determines whether this index is built to be used for
      // one of the existing keypath components e.g. `\Lens<[Int]>.count`
      // instead of a regular expression e.g. `lens[0]`.
      bool forKeyPathComponent = false;
      // Looks like keypath dynamic member lookup was used inside
      // of a keypath expression e.g. `\Lens<[Int]>.count` where
      // `count` is referenced using dynamic lookup.
      if (auto *KPE = dyn_cast<KeyPathExpr>(anchor)) {
        auto kpElt = memberLoc->findFirst<LocatorPathElt::KeyPathComponent>();
        assert(kpElt && "no keypath component node");
        auto &origComponent = KPE->getComponents()[kpElt->getIndex()];

        using ComponentKind = KeyPathExpr::Component::Kind;
        if (origComponent.getKind() == ComponentKind::UnresolvedProperty) {
          anchor = new (ctx) UnresolvedDotExpr(
              dotExpr, dotLoc, origComponent.getUnresolvedDeclName(),
              DeclNameLoc(origComponent.getLoc()),
              /*Implicit=*/true);
        } else if (origComponent.getKind() ==
                   ComponentKind::UnresolvedSubscript) {
          anchor = SubscriptExpr::create(
              ctx, dotExpr, origComponent.getIndexExpr(), ConcreteDeclRef(),
              /*implicit=*/true, AccessSemantics::Ordinary,
              [&](const Expr *expr) { return simplifyType(cs.getType(expr)); });
        } else {
          return nullptr;
        }

        anchor->setType(simplifyType(overload.openedType));
        cs.cacheType(anchor);
        forKeyPathComponent = true;
      }

      if (auto *UDE = dyn_cast<UnresolvedDotExpr>(anchor)) {
        componentExpr =
            forKeyPathComponent
                ? UDE
                : new (ctx) UnresolvedDotExpr(dotExpr, dotLoc, UDE->getName(),
                                              UDE->getNameLoc(),
                                              /*Implicit=*/true);

        component = buildKeyPathPropertyComponent(overload, UDE->getLoc(),
                                                  componentLoc);
      } else if (auto *SE = dyn_cast<SubscriptExpr>(anchor)) {
        componentExpr = SE;
        // If this is not for a keypath component, we have to copy
        // original subscript expression because expression based
        // diagnostics might have a reference to it, so it couldn't
        // be modified.
        if (!forKeyPathComponent) {
          SmallVector<Expr *, 4> arguments;
          if (auto *TE = dyn_cast<TupleExpr>(SE->getIndex())) {
            auto args = TE->getElements();
            arguments.append(args.begin(), args.end());
          } else {
            arguments.push_back(SE->getIndex()->getSemanticsProvidingExpr());
          }

          Expr *trailingClosure = nullptr;
          if (SE->hasTrailingClosure())
            trailingClosure = arguments.back();

          componentExpr = SubscriptExpr::create(
              ctx, dotExpr, SE->getStartLoc(), arguments,
              SE->getArgumentLabels(), SE->getArgumentLabelLocs(),
              SE->getEndLoc(), trailingClosure,
              SE->hasDecl() ? SE->getDecl() : ConcreteDeclRef(),
              /*implicit=*/true, SE->getAccessSemantics());
        }

        component = buildKeyPathSubscriptComponent(
            overload, SE->getLoc(), SE->getIndex(), SE->getArgumentLabels(),
            componentLoc);
      } else {
        return nullptr;
      }

      assert(componentExpr);
      Type ty = simplifyType(cs.getType(anchor));
      componentExpr->setType(ty);
      cs.cacheType(componentExpr);

      cs.setType(keyPath, 0, ty);

      keyPath->setParsedPath(componentExpr);
      keyPath->resolveComponents(ctx, {component});
      return keyPath;
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
        = TypeChecker::conformsToProtocol(valueType,
                                          bridgedProto,
                                          cs.DC,
                                          ConformanceCheckFlags::InExpression);

      FuncDecl *fn = nullptr;

      if (bridgedToObjectiveCConformance) {
        assert(bridgedToObjectiveCConformance->getConditionalRequirements()
                   .empty() &&
               "cannot conditionally conform to _BridgedToObjectiveC");
        // The conformance to _BridgedToObjectiveC is statically known.
        // Retrieve the bridging operation to be used if a static conformance
        // to _BridgedToObjectiveC can be proven.
        fn = conditional
                 ? tc.Context.getConditionallyBridgeFromObjectiveCBridgeable()
                 : tc.Context.getForceBridgeFromObjectiveCBridgeable();
      } else {
        // Retrieve the bridging operation to be used if a static conformance
        // to _BridgedToObjectiveC cannot be proven.
        fn = conditional ? tc.Context.getConditionallyBridgeFromObjectiveC()
                         : tc.Context.getForceBridgeFromObjectiveC();
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
    
  public:
    ExprRewriter(ConstraintSystem &cs, const Solution &solution,
                 bool suppressDiagnostics)
        : cs(cs), dc(cs.DC), solution(solution),
          SuppressDiagnostics(suppressDiagnostics) {}

    ConstraintSystem &getConstraintSystem() const { return cs; }

    /// Simplify the expression type and return the expression.
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
      if (cs.getType(expr)->is<AnyBuiltinIntegerType>())
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

      auto *result = convertLiteralInPlace(
          expr, type, protocol, tc.Context.Id_IntegerLiteralType, initName,
          builtinProtocol, builtinInitName, diag::integer_literal_broken_proto,
          diag::builtin_integer_literal_broken_proto);
      if (result) {
        // TODO: It seems that callers expect this to have types assigned...
        result->setType(cs.getType(result));
      }
      return result;
    }
    
    Expr *visitNilLiteralExpr(NilLiteralExpr *expr) {
      auto type = simplifyType(cs.getType(expr));

      // By far the most common 'nil' literal is for Optional<T>.none.
      // We don't have to look up the witness in this case since SILGen
      // knows how to lower it directly.
      if (auto objectType = type->getOptionalObjectType()) {
        cs.setType(expr, type);
        return expr;
      }

      auto &tc = cs.getTypeChecker();
      auto *protocol = tc.getProtocol(expr->getLoc(),
                                      KnownProtocolKind::ExpressibleByNilLiteral);

      // For type-sugar reasons, prefer the spelling of the default literal
      // type.
      if (auto defaultType = tc.getDefaultType(protocol, dc)) {
        if (defaultType->isEqual(type))
          type = defaultType;
      }

      DeclName initName(tc.Context, DeclBaseName::createConstructor(),
                        { tc.Context.Id_nilLiteral });
      return convertLiteralInPlace(expr, type, protocol,
                                   Identifier(), initName,
                                   nullptr,
                                   Identifier(),
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

      // Get the _MaxBuiltinFloatType decl, or look for it if it's not cached.
      auto maxFloatTypeDecl = tc.Context.get_MaxBuiltinFloatTypeDecl();

      if (!maxFloatTypeDecl ||
          !maxFloatTypeDecl->hasInterfaceType() ||
          !maxFloatTypeDecl->getDeclaredInterfaceType()->is<BuiltinFloatType>()) {
        tc.diagnose(expr->getLoc(), diag::no_MaxBuiltinFloatType_found);
        return nullptr;
      }

      tc.validateDecl(maxFloatTypeDecl);
      auto maxType = maxFloatTypeDecl->getUnderlyingType();

      DeclName initName(tc.Context, DeclBaseName::createConstructor(),
                        { tc.Context.Id_floatLiteral });
      DeclName builtinInitName(tc.Context, DeclBaseName::createConstructor(),
                               { tc.Context.Id_builtinFloatLiteral });

      expr->setBuiltinType(maxType);
      return convertLiteralInPlace(
          expr, type, protocol, tc.Context.Id_FloatLiteralType, initName,
          builtinProtocol, builtinInitName, diag::float_literal_broken_proto,
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
      return convertLiteralInPlace(
               expr,
               type,
               protocol,
               tc.Context.Id_BooleanLiteralType,
               initName,
               builtinProtocol,
               builtinInitName,
               diag::boolean_literal_broken_proto,
               diag::builtin_boolean_literal_broken_proto);
    }

    Expr *handleStringLiteralExpr(LiteralExpr *expr) {
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

      if (!TypeChecker::conformsToProtocol(type, protocol, cs.DC,
                                           ConformanceCheckFlags::InExpression)) {
        // If the type does not conform to ExpressibleByStringLiteral, it should
        // be ExpressibleByExtendedGraphemeClusterLiteral.
        protocol = tc.getProtocol(
            expr->getLoc(),
            KnownProtocolKind::ExpressibleByExtendedGraphemeClusterLiteral);
        isStringLiteral = false;
        isGraphemeClusterLiteral = true;
      }
      if (!TypeChecker::conformsToProtocol(type, protocol, cs.DC,
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
        literalType = tc.Context.Id_StringLiteralType;

        literalFuncName = DeclName(tc.Context, DeclBaseName::createConstructor(),
                                   { tc.Context.Id_stringLiteral });

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

      auto &tc = cs.getTypeChecker();
      auto loc = expr->getStartLoc();

      auto fetchProtocolInitWitness =
        [&](KnownProtocolKind protocolKind, Type type,
            ArrayRef<Identifier> argLabels) -> ConcreteDeclRef {

        auto proto = tc.getProtocol(loc, protocolKind);
        assert(proto && "Missing string interpolation protocol?");

        auto conformance =
          TypeChecker::conformsToProtocol(type, proto, cs.DC,
                                          ConformanceCheckFlags::InExpression);
        assert(conformance && "string interpolation type conforms to protocol");

        DeclName constrName(tc.Context, DeclBaseName::createConstructor(), argLabels);

        ConcreteDeclRef witness =
            conformance->getWitnessByName(type->getRValueType(), constrName);
        if (!witness || !isa<AbstractFunctionDecl>(witness.getDecl()))
          return nullptr;
        return witness;
      };

      auto *interpolationProto = 
        tc.getProtocol(expr->getLoc(),
             KnownProtocolKind::ExpressibleByStringInterpolation);
      auto associatedTypeDecl = interpolationProto->getAssociatedType(
          tc.Context.Id_StringInterpolation);
      if (associatedTypeDecl == nullptr) {
        tc.diagnose(expr->getStartLoc(), diag::interpolation_broken_proto);
        return nullptr;
      }
      auto interpolationType =
        simplifyType(DependentMemberType::get(openedType, associatedTypeDecl));

      // Fetch needed witnesses.
      ConcreteDeclRef builderInit = fetchProtocolInitWitness(
          KnownProtocolKind::StringInterpolationProtocol, interpolationType,
          { tc.Context.Id_literalCapacity, tc.Context.Id_interpolationCount });
      if (!builderInit) return nullptr;
      expr->setBuilderInit(builderInit);

      ConcreteDeclRef resultInit = fetchProtocolInitWitness(
          KnownProtocolKind::ExpressibleByStringInterpolation, type,
          { tc.Context.Id_stringInterpolation });
      if (!resultInit) return nullptr;
      expr->setResultInit(resultInit);

      // Make the integer literals for the parameters.
      auto buildExprFromUnsigned = [&](unsigned value) {
        LiteralExpr *expr =
            IntegerLiteralExpr::createFromUnsigned(tc.Context, value);
        cs.setType(expr, tc.getIntType(cs.DC));
        return handleIntegerLiteralExpr(expr);
      };

      expr->setLiteralCapacityExpr(
          buildExprFromUnsigned(expr->getLiteralCapacity()));
      expr->setInterpolationCountExpr(
          buildExprFromUnsigned(expr->getInterpolationCount()));

      // This OpaqueValueExpr represents the result of builderInit above in
      // silgen.
      OpaqueValueExpr *interpolationExpr = new (tc.Context)
          OpaqueValueExpr(expr->getSourceRange(), interpolationType);
      cs.setType(interpolationExpr, interpolationType);
      expr->setInterpolationExpr(interpolationExpr);

      auto appendingExpr = expr->getAppendingExpr();
      appendingExpr->setSubExpr(interpolationExpr);

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

      auto &tc = cs.getTypeChecker();

      // Figure out the type we're converting to.
      auto openedType = cs.getType(expr);
      auto type = simplifyType(openedType);
      cs.setType(expr, type);

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
        TypeChecker::conformsToProtocol(conformingType, proto, cs.DC,
                                        ConformanceCheckFlags::InExpression);
      assert(conformance && "object literal type conforms to protocol");

      DeclName constrName(tc.getObjectLiteralConstructorName(expr));

      ConcreteDeclRef witness =
        conformance->getWitnessByName(conformingType->getRValueType(),
                                      constrName);
      if (!witness || !isa<AbstractFunctionDecl>(witness.getDecl()))
        return nullptr;
      expr->setInitializer(witness);
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

        // FIXME: Application could fail, because some of the solutions
        // are not expressible in AST (yet?), like certain tuple-to-tuple
        // conversions. Better solution here would be not to form solutions
        // which couldn't be applied by e.g. detecting situations like that
        // and inserting fixes early.
        if (!result)
          return nullptr;
      }

      // Check for ambiguous member if the base is an Optional
      if (baseTy->getOptionalObjectType()) {
        diagnoseAmbiguousNominalMember(baseTy, result);
      }

      return coerceToType(result, resultTy, cs.getConstraintLocator(expr));
    }
    
    /// Diagnose if the base type is optional, we're referring to a nominal
    /// type member via the dot syntax and the member name matches
    /// Optional<T>.{member name}
    void diagnoseAmbiguousNominalMember(Type baseTy, Expr *result) {
      if (auto baseTyUnwrapped = baseTy->lookThroughAllOptionalTypes()) {
        // Return if the base type doesn't have a nominal type decl
        if (!baseTyUnwrapped->getNominalOrBoundGenericNominal()) {
          return;
        }
        
        // Otherwise, continue digging
        if (auto DSCE = dyn_cast<DotSyntaxCallExpr>(result)) {
          auto calledValue = DSCE->getCalledValue();
          auto isOptional = false;
          Identifier memberName;
          
          // Try cast the assigned value to an enum case
          //
          // This will always succeed if the base is Optional<T> & the
          // assigned case comes from Optional<T>
          if (auto EED = dyn_cast<EnumElementDecl>(calledValue)) {
            isOptional = EED->getParentEnum()->isOptionalDecl();
            memberName = EED->getBaseName().getIdentifier();
          }
          
          // Return if the enum case doesn't come from Optional<T>
          if (!isOptional) {
            return;
          }
          
          // Look up the enum case in the unwrapped type to check if it exists
          // as a member
          auto baseTyNominalDecl = baseTyUnwrapped
                                   ->getNominalOrBoundGenericNominal();
          auto &tc = cs.getTypeChecker();
          auto results = tc.lookupMember(baseTyNominalDecl->getModuleContext(),
                                         baseTyUnwrapped,
                                         memberName,
                                         defaultMemberLookupOptions);
          
          // Lookup didn't find anything, so return
          if (results.empty()) {
            return;
          }
          
          if (auto member = results.front().getValueDecl()) {
            // Lookup returned a member that is an instance member,
            // so return
            if (member->isInstanceMember()) {
              return;
            }
            
            // Return if the member is an enum case w/ assoc values, as we only
            // care (for now) about cases with no assoc values (like none)
            if (auto EED = dyn_cast<EnumElementDecl>(member)) {
              if (EED->hasAssociatedValues()) {
                return;
              }
            }
            
            // Emit a diagnostic with some fixits
            auto baseTyName = baseTy->getCanonicalType().getString();
            auto baseTyUnwrappedName = baseTyUnwrapped->getString();
            auto loc = DSCE->getLoc();
            auto startLoc = DSCE->getStartLoc();
            
            tc.diagnose(loc, swift::diag::optional_ambiguous_case_ref,
                        baseTyName, baseTyUnwrappedName, memberName.str());
            
            tc.diagnose(loc, swift::diag::optional_fixit_ambiguous_case_ref)
              .fixItInsert(startLoc, "Optional");
            tc.diagnose(loc, swift::diag::type_fixit_optional_ambiguous_case_ref,
                        baseTyUnwrappedName, memberName.str())
              .fixItInsert(startLoc, baseTyUnwrappedName);
          }
        }
      }
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

      case OverloadChoiceKind::DynamicMemberLookup:
      case OverloadChoiceKind::KeyPathDynamicMemberLookup: {
        return buildDynamicMemberLookupRef(
            expr, base, dotLoc, nameLoc.getStartLoc(), selected, memberLocator);
      }
      }

      llvm_unreachable("Unhandled OverloadChoiceKind in switch.");
    }

    Expr *buildDynamicMemberLookupRef(Expr *expr, Expr *base, SourceLoc dotLoc,
                                      SourceLoc nameLoc,
                                      const SelectedOverload &overload,
                                      ConstraintLocator *memberLocator) {
      // Application of a DynamicMemberLookup result turns
      // a member access of `x.foo` into x[dynamicMember: "foo"], or
      // x[dynamicMember: KeyPath<T, U>]
      auto &ctx = cs.getASTContext();

      // Figure out the expected type of the lookup parameter. We know the
      // openedFullType will be "xType -> indexType -> resultType".  Dig out
      // its index type.
      auto paramTy = getTypeOfDynamicMemberIndex(overload);

      Expr *argExpr = nullptr;
      if (overload.choice.getKind() ==
          OverloadChoiceKind::DynamicMemberLookup) {
        // Build and type check the string literal index value to the specific
        // string type expected by the subscript.
        auto fieldName = overload.choice.getName().getBaseIdentifier().str();
        argExpr = buildDynamicMemberLookupIndexExpr(fieldName, nameLoc, dc, cs);
      } else {
        argExpr = buildKeyPathDynamicMemberIndexExpr(
            paramTy->castTo<BoundGenericType>(), dotLoc, memberLocator);
      }

      if (!argExpr)
        return nullptr;

      // Build a tuple so that the argument has a label.
      auto tupleTy =
          TupleType::get(TupleTypeElt(paramTy, ctx.Id_dynamicMember), ctx);

      Expr *index =
          TupleExpr::createImplicit(ctx, argExpr, ctx.Id_dynamicMember);
      index->setType(tupleTy);
      cs.cacheType(index);

      // Build and return a subscript that uses this string as the index.
      return buildSubscript(
          base, index, ctx.Id_dynamicMember,
          /*trailingClosure*/ false, cs.getConstraintLocator(expr),
          /*isImplicit*/ true, AccessSemantics::Ordinary, overload);
    }

    Type getTypeOfDynamicMemberIndex(const SelectedOverload &overload) {
      assert(overload.choice.getKind() ==
                 OverloadChoiceKind::DynamicMemberLookup ||
             overload.choice.getKind() ==
                 OverloadChoiceKind::KeyPathDynamicMemberLookup);

      auto declTy = solution.simplifyType(overload.openedFullType);
      auto subscriptTy = declTy->castTo<FunctionType>()->getResult();
      auto refFnType = subscriptTy->castTo<FunctionType>();
      assert(refFnType->getParams().size() == 1 &&
             "subscript always has one arg");
      return refFnType->getParams()[0].getPlainType();
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
      // Prior to Swift 5, 'try?' simply wraps the type of its sub-expression
      // in an Optional, regardless of the sub-expression type.
      //
      // In Swift 5+, the type of a 'try?' expression of static type T is:
      //  - Equal to T if T is optional
      //  - Equal to T? if T is not optional
      //
      // The result is that in Swift 5, 'try?' avoids producing nested optionals.
      
      if (!cs.getTypeChecker().getLangOpts().isSwiftVersionAtLeast(5)) {
        // Nothing to do for Swift 4 and earlier!
        return simplifyExprType(expr);
      }
      
      Type exprType = simplifyType(cs.getType(expr));

      auto subExpr = coerceToType(expr->getSubExpr(), exprType,
                                  cs.getConstraintLocator(expr));
      if (!subExpr) return nullptr;
      expr->setSubExpr(subExpr);

      cs.setType(expr, exprType);
      return expr;
    }

    Expr *visitParenExpr(ParenExpr *expr) {
      return simplifyExprType(expr);
    }

    Expr *visitTupleExpr(TupleExpr *expr) {
      return simplifyExprType(expr);
    }

    Expr *visitSubscriptExpr(SubscriptExpr *expr) {
      auto *memberLocator =
          cs.getConstraintLocator(expr, ConstraintLocator::SubscriptMember);
      auto overload = solution.getOverloadChoiceIfAvailable(memberLocator);

      if (overload && overload->choice.getKind() ==
                          OverloadChoiceKind::KeyPathDynamicMemberLookup) {
        return buildDynamicMemberLookupRef(
            expr, expr->getBase(), expr->getIndex()->getStartLoc(), SourceLoc(),
            *overload, memberLocator);
      }

      return buildSubscript(
          expr->getBase(), expr->getIndex(), expr->getArgumentLabels(),
          expr->hasTrailingClosure(), cs.getConstraintLocator(expr),
          expr->isImplicit(), expr->getAccessSemantics(), overload);
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
        TypeChecker::conformsToProtocol(arrayTy, arrayProto, cs.DC,
                                        ConformanceCheckFlags::InExpression);
      assert(conformance && "Type does not conform to protocol?");

      DeclName name(tc.Context, DeclBaseName::createConstructor(),
                    { tc.Context.Id_arrayLiteral });
      ConcreteDeclRef witness =
        conformance->getWitnessByName(arrayTy->getRValueType(), name);
      if (!witness || !isa<AbstractFunctionDecl>(witness.getDecl()))
        return nullptr;
      expr->setInitializer(witness);

      auto elementType = expr->getElementType();

      for (auto &element : expr->getElements()) {
        element = coerceToType(element, elementType,
                               cs.getConstraintLocator(element));
      }

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
        TypeChecker::conformsToProtocol(dictionaryTy, dictionaryProto, cs.DC,
                                        ConformanceCheckFlags::InExpression);
      if (!conformance)
        return nullptr;

      DeclName name(tc.Context, DeclBaseName::createConstructor(),
                    { tc.Context.Id_dictionaryLiteral });
      ConcreteDeclRef witness =
        conformance->getWitnessByName(dictionaryTy->getRValueType(), name);
      if (!witness || !isa<AbstractFunctionDecl>(witness.getDecl()))
        return nullptr;
      expr->setInitializer(witness);

      auto elementType = expr->getElementType();
      for (auto &element : expr->getElements()) {
        element = coerceToType(element, elementType,
                               cs.getConstraintLocator(element));
      }

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

      auto arrayTy = cs.getType(expr);
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
      assert(expr->isPlaceholder() && "Already type-checked");
      return expr;
    }

    Expr *visitDefaultArgumentExpr(DefaultArgumentExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Expr *visitCallerDefaultArgumentExpr(CallerDefaultArgumentExpr *expr) {
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
      if (valueTy && !inCtor->isFailable()) {
        bool isChaining;
        auto *otherCtorRef = expr->getCalledConstructor(isChaining);
        ConstructorDecl *ctor = otherCtorRef->getDecl();
        assert(ctor);

        // If the initializer we're calling is not declared as
        // checked, it's an error.
        bool isError = !ctor->isImplicitlyUnwrappedOptional();

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

      auto cond = cs.coerceToRValue(expr->getCondExpr());
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
      if (!tc.Context.getBoolBuiltinInitDecl()) {
        tc.diagnose(expr->getLoc(), diag::broken_bool);
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
        tc.requireOptionalIntrinsics(expr->getLoc());

        // Match the optional value against its `Some` case.
        auto &ctx = tc.Context;
        auto *someDecl = tc.Context.getOptionalSomeDecl();
        auto isSomeExpr = new (tc.Context) EnumIsCaseExpr(result, someDecl);
        auto boolDecl = ctx.getBoolDecl();

        if (!boolDecl) {
          tc.diagnose(SourceLoc(), diag::broken_bool);
        }

        cs.setType(isSomeExpr, boolDecl ? boolDecl->getDeclaredType() : Type());
        return isSomeExpr;
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

        if (auto *literal = dyn_cast<NumberLiteralExpr>(literalInit)) {
          literal->setExplicitConversion();
        } else {
          literalInit->setImplicit(false);
        }

        cs.setType(expr, toType);
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
      llvm_unreachable("Already type-checked");
    }
    
    Expr *visitEditorPlaceholderExpr(EditorPlaceholderExpr *E) {
      simplifyExprType(E);
      auto valueType = cs.getType(E);

      // TODO(diagnostics): Once all of the diagnostics are moved to
      // new diagnostics framework this check could be eliminated.
      //
      // Only way for this to happen is CSDiag try to re-typecheck
      // sub-expression which contains this placeholder with
      // `AllowUnresolvedTypeVariables` flag set.
      //
      // A better solution could be to replace placeholders with this
      // implicit call early on and type-check that call together with
      // the rest of the constraint system.
      if (valueType->hasUnresolvedType())
        return nullptr;

      auto &tc = cs.getTypeChecker();
      auto &ctx = tc.Context;
      // Synthesize a call to _undefined() of appropriate type.
      FuncDecl *undefinedDecl = ctx.getUndefined();
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
          method = var->getOpaqueAccessor(AccessorKind::Get);
          break;
        }

        case ObjCSelectorExpr::Getter:
          method = var->getOpaqueAccessor(AccessorKind::Get);
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

          method = var->getOpaqueAccessor(AccessorKind::Set);
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
        // If the method declaration lies in a protocol and we're providing
        // a default implementation of the method through a protocol extension
        // and using it as a selector, then bail out as adding @objc to the
        // protocol might not be the right thing to do and could lead to
        // problems.
        if (auto protocolDecl = dyn_cast<ProtocolDecl>(foundDecl->getDeclContext())) {
          tc.diagnose(E->getLoc(), diag::expr_selector_cannot_be_used,
                      foundDecl->getBaseName(), protocolDecl->getFullName());
          return E;
        }
        
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
      bool isFunctionType = false;
      Type baseTy, leafTy;
      Type exprType = cs.getType(E);
      if (auto fnTy = exprType->getAs<FunctionType>()) {
        baseTy = fnTy->getParams()[0].getPlainType();
        leafTy = fnTy->getResult();
        isFunctionType = true;
      } else {
        auto keyPathTy = exprType->castTo<BoundGenericType>();
        baseTy = keyPathTy->getGenericArgs()[0];
        leafTy = keyPathTy->getGenericArgs()[1];
      }

      // Updates the constraint system with the type of the last resolved
      // component. We do it this way because we sometimes insert new
      // components.
      auto updateCSWithResolvedComponent = [&]() {
        cs.setType(E, resolvedComponents.size() - 1, baseTy);
      };

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

        auto locator = cs.getConstraintLocator(
            E, LocatorPathElt::KeyPathComponent(i));

        // Adjust the locator such that it includes any additional elements to
        // point to the component's callee, e.g a SubscriptMember for a
        // subscript component.
        locator = cs.getCalleeLocator(locator);

        bool isDynamicMember = false;
        // If this is an unresolved link, make sure we resolved it.
        if (kind == KeyPathExpr::Component::Kind::UnresolvedProperty ||
            kind == KeyPathExpr::Component::Kind::UnresolvedSubscript) {
          foundDecl = solution.getOverloadChoiceIfAvailable(locator);
          // Leave the component unresolved if the overload was not resolved.
          if (foundDecl) {
            isDynamicMember =
                foundDecl->choice.getKind() ==
                    OverloadChoiceKind::DynamicMemberLookup ||
                foundDecl->choice.getKind() ==
                    OverloadChoiceKind::KeyPathDynamicMemberLookup;

            // If this was a @dynamicMemberLookup property, then we actually
            // form a subscript reference, so switch the kind.
            if (isDynamicMember) {
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

          component = buildKeyPathPropertyComponent(
              *foundDecl, origComponent.getLoc(), locator);

          baseTy = component.getComponentType();
          resolvedComponents.push_back(component);

          if (shouldForceUnwrapResult(foundDecl->choice, locator)) {
            updateCSWithResolvedComponent();
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

          ArrayRef<Identifier> subscriptLabels;
          if (!isDynamicMember)
            subscriptLabels = origComponent.getSubscriptLabels();

          component = buildKeyPathSubscriptComponent(
              *foundDecl, origComponent.getLoc(), origComponent.getIndexExpr(),
              subscriptLabels, locator);

          baseTy = component.getComponentType();
          resolvedComponents.push_back(component);

          if (shouldForceUnwrapResult(foundDecl->choice, locator)) {
            updateCSWithResolvedComponent();
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
        case KeyPathExpr::Component::Kind::TupleElement:
          llvm_unreachable("already resolved");
        }

        // By now, "baseTy" is the result type of this component.
        updateCSWithResolvedComponent();
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
        updateCSWithResolvedComponent();
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

      if (!isFunctionType)
        return E;

      // If we've gotten here, the user has used key path literal syntax to form
      // a closure. The type checker has given E a function type to indicate
      // this; we're going to change E's type to KeyPath<baseTy, leafTy> and
      // then wrap it in a larger closure expression with the appropriate type.

      // baseTy has been overwritten by the loop above; restore it.
      baseTy = exprType->getAs<FunctionType>()->getParams()[0].getPlainType();

      // Compute KeyPath<baseTy, leafTy> and set E's type back to it.
      auto kpDecl = cs.getASTContext().getKeyPathDecl();
      auto keyPathTy =
          BoundGenericType::get(kpDecl, nullptr, { baseTy, leafTy });
      E->setType(keyPathTy);
      cs.cacheType(E);

      // To ensure side effects of the key path expression (mainly indices in
      // subscripts) are only evaluated once, we construct an outer closure,
      // which is immediately evaluated, and an inner closure, which it returns.
      // The result looks like this:
      //
      //     return "{ $kp$ in { $0[keyPath: $kp$] } }( \(E) )"

      auto &ctx = cs.getASTContext();
      auto discriminator = AutoClosureExpr::InvalidDiscriminator;

      // The inner closure.
      //
      //     let closure = "{ $0[keyPath: $kp$] }"
      auto closureTy =
          FunctionType::get({ FunctionType::Param(baseTy) }, leafTy);
      auto closure = new (ctx)
          AutoClosureExpr(E, leafTy, discriminator, cs.DC);
      auto param = new (ctx) ParamDecl(
          ParamDecl::Specifier::Default, SourceLoc(),
          /*argument label*/ SourceLoc(), Identifier(),
          /*parameter name*/ SourceLoc(), ctx.getIdentifier("$0"), closure);
      param->setType(baseTy);
      param->setInterfaceType(baseTy->mapTypeOutOfContext());

      // The outer closure.
      //
      //    let outerClosure = "{ $kp$ in \(closure) }"
      auto outerClosureTy =
          FunctionType::get({ FunctionType::Param(keyPathTy) }, closureTy);
      auto outerClosure = new (ctx)
          AutoClosureExpr(closure, closureTy, discriminator, cs.DC);
      auto outerParam =
          new (ctx) ParamDecl(ParamDecl::Specifier::Default, SourceLoc(),
                              /*argument label*/ SourceLoc(), Identifier(),
                              /*parameter name*/ SourceLoc(),
                              ctx.getIdentifier("$kp$"), outerClosure);
      outerParam->setType(keyPathTy);
      outerParam->setInterfaceType(keyPathTy->mapTypeOutOfContext());

      // let paramRef = "$0"
      auto *paramRef = new (ctx)
          DeclRefExpr(param, DeclNameLoc(E->getLoc()), /*Implicit=*/true);
      paramRef->setType(baseTy);
      cs.cacheType(paramRef);

      // let outerParamRef = "$kp$"
      auto outerParamRef = new (ctx)
          DeclRefExpr(outerParam, DeclNameLoc(E->getLoc()), /*Implicit=*/true);
      outerParamRef->setType(keyPathTy);
      cs.cacheType(outerParamRef);

      // let application = "\(paramRef)[keyPath: \(outerParamRef)]"
      auto *application = new (ctx)
          KeyPathApplicationExpr(paramRef,
                                 E->getStartLoc(), outerParamRef, E->getEndLoc(),
                                 leafTy, /*implicit=*/true);
      cs.cacheType(application);

      // Finish up the inner closure.
      closure->setParameterList(ParameterList::create(ctx, {param}));
      closure->setBody(application);
      closure->setType(closureTy);
      cs.cacheType(closure);

      // Finish up the outer closure.
      outerClosure->setParameterList(ParameterList::create(ctx, {outerParam}));
      outerClosure->setBody(closure);
      outerClosure->setType(outerClosureTy);
      cs.cacheType(outerClosure);

      // let outerApply = "\( outerClosure )( \(E) )"
      auto outerApply = CallExpr::createImplicit(ctx, outerClosure, {E}, {});
      outerApply->setType(closureTy);
      cs.cacheExprTypes(outerApply);

      return coerceToType(outerApply, exprType, cs.getConstraintLocator(E));
    }

    KeyPathExpr::Component
    buildKeyPathPropertyComponent(const SelectedOverload &overload,
                                  SourceLoc componentLoc,
                                  ConstraintLocator *locator) {
      if (auto *property = overload.choice.getDeclOrNull()) {
        // Key paths can only refer to properties currently.
        auto varDecl = cast<VarDecl>(property);
        // Key paths don't work with mutating-get properties.
        assert(!varDecl->isGetterMutating());
        // Key paths don't currently support static members.
        // There is a fix which diagnoses such situation already.
        assert(!varDecl->isStatic());

        auto dc = property->getInnermostDeclContext();

        // Compute substitutions to refer to the member.
        SubstitutionMap subs = solution.computeSubstitutions(
            dc->getGenericSignatureOfContext(), locator);

        auto resolvedTy = overload.openedType;
        resolvedTy = simplifyType(resolvedTy);

        auto ref = ConcreteDeclRef(property, subs);

        return KeyPathExpr::Component::forProperty(ref, resolvedTy,
                                                   componentLoc);
      }

      auto fieldIndex = overload.choice.getTupleIndex();
      auto resolvedTy = overload.openedType;
      resolvedTy = simplifyType(resolvedTy);

      return KeyPathExpr::Component::forTupleElement(fieldIndex, resolvedTy,
                                                     componentLoc);
    }

    KeyPathExpr::Component buildKeyPathSubscriptComponent(
        SelectedOverload &overload, SourceLoc componentLoc, Expr *indexExpr,
        ArrayRef<Identifier> labels, ConstraintLocator *locator) {
      auto subscript = cast<SubscriptDecl>(overload.choice.getDecl());
      assert(!subscript->isGetterMutating());

      auto dc = subscript->getInnermostDeclContext();

      auto indexType = AnyFunctionType::composeInput(
          cs.TC.Context,
          subscript->getInterfaceType()->castTo<AnyFunctionType>()->getParams(),
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
      bool forDynamicLookup =
          overload.choice.getKind() ==
              OverloadChoiceKind::DynamicMemberLookup ||
          overload.choice.getKind() ==
              OverloadChoiceKind::KeyPathDynamicMemberLookup;

      if (forDynamicLookup) {
        overload.openedType =
            overload.openedFullType->castTo<AnyFunctionType>()->getResult();

        labels = cs.getASTContext().Id_dynamicMember;

        if (overload.choice.getKind() ==
            OverloadChoiceKind::KeyPathDynamicMemberLookup) {
          auto indexType = getTypeOfDynamicMemberIndex(overload);
          indexExpr = buildKeyPathDynamicMemberIndexExpr(
              indexType->castTo<BoundGenericType>(), componentLoc, locator);
        } else {
          auto fieldName = overload.choice.getName().getBaseIdentifier().str();
          indexExpr = buildDynamicMemberLookupIndexExpr(fieldName, componentLoc,
                                                        dc, cs);
        }
      }

      auto subscriptType =
          simplifyType(overload.openedType)->castTo<AnyFunctionType>();
      auto resolvedTy = subscriptType->getResult();
      auto ref = ConcreteDeclRef(subscript, subs);

      // Coerce the indices to the type the subscript expects.
      auto *newIndexExpr =
          coerceCallArguments(indexExpr, subscriptType,
                              /*applyExpr*/ nullptr, labels,
                              /*hasTrailingClosure*/ false, locator);

      auto component = KeyPathExpr::Component::forSubscriptWithPrebuiltIndexExpr(
          ref, newIndexExpr, labels, resolvedTy, componentLoc, {});

      // We need to be able to hash the captured index values in order for
      // KeyPath itself to be hashable, so check that all of the subscript
      // index components are hashable and collect their conformances here.
      SmallVector<ProtocolConformanceRef, 4> conformances;

      auto hashable =
          cs.getASTContext().getProtocol(KnownProtocolKind::Hashable);

      auto fnType = overload.openedType->castTo<FunctionType>();
      for (const auto &param : fnType->getParams()) {
        auto indexType = simplifyType(param.getPlainType());
        // Index type conformance to Hashable protocol has been
        // verified by the solver, we just need to get it again
        // with all of the generic parameters resolved.
        auto hashableConformance =
          TypeChecker::conformsToProtocol(indexType, hashable, cs.DC,
                                          ConformanceCheckFlags::InExpression);
        assert(hashableConformance.hasValue());

        conformances.push_back(*hashableConformance);
      }

      component.setSubscriptIndexHashableConformances(conformances);
      return component;
    }

    Expr *visitKeyPathDotExpr(KeyPathDotExpr *E) {
      llvm_unreachable("found KeyPathDotExpr in CSApply");
    }

    Expr *visitOneWayExpr(OneWayExpr *E) {
      auto type = simplifyType(cs.getType(E));
      return coerceToType(E->getSubExpr(), type, cs.getConstraintLocator(E));
    }

    Expr *visitTapExpr(TapExpr *E) {
      auto type = simplifyType(cs.getType(E));

      E->getVar()->setType(type);
      E->getVar()->setInterfaceType(type->mapTypeOutOfContext());

      cs.setType(E, type);
      E->setType(type);

      return E;
    }

    /// Interface for ExprWalker
    void walkToExprPre(Expr *expr) {
      ExprStack.push_back(expr);
    }

    Expr *walkToExprPost(Expr *expr) {
      Expr *result = visit(expr);

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
      if (auto *decl = selected->choice.getDeclOrNull())
        return getConcreteDeclRef(decl, selected->openedType, anchorLocator);
    }
  }
  
  if (isa<UnresolvedMemberExpr>(anchor)) {
    // Unresolved member: find the resolved overload.
    auto anchorLocator = cs.getConstraintLocator(anchor,
                           ConstraintLocator::UnresolvedMember);
    if (auto selected = findOvlChoice(anchorLocator)) {
      if (auto *decl = selected->choice.getDeclOrNull())
        return getConcreteDeclRef(decl, selected->openedType, anchorLocator);
    }
  }

  if (isa<UnresolvedDotExpr>(anchor)) {
    // Unresolved member: find the resolved overload.
    auto anchorLocator = cs.getConstraintLocator(anchor,
                                                 ConstraintLocator::Member);
    if (auto selected = findOvlChoice(anchorLocator)) {
      if (auto *decl = selected->choice.getDeclOrNull())
        return getConcreteDeclRef(decl, selected->openedType, anchorLocator);
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
      if (auto *decl = selected->choice.getDeclOrNull())
        return getConcreteDeclRef(decl, selected->openedType, anchorLocator);
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

/// Given a constraint locator, find the declaration reference
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

/// Produce the caller-side default argument for this default argument, or
/// null if the default argument will be provided by the callee.
static std::pair<Expr *, DefaultArgumentKind>
getCallerDefaultArg(ConstraintSystem &cs, DeclContext *dc,
                    SourceLoc loc, ConcreteDeclRef &owner,
                    unsigned index) {
  auto &tc = cs.getTypeChecker();

  const auto *param = getParameterAt(cast<ValueDecl>(owner.getDecl()), index);
  Expr *init = nullptr;
  switch (param->getDefaultArgumentKind()) {
  case DefaultArgumentKind::None:
    llvm_unreachable("No default argument here?");

  case DefaultArgumentKind::StoredProperty:
  case DefaultArgumentKind::Normal:
    return {nullptr, param->getDefaultArgumentKind()};

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
  auto defArgType = owner.getDecl()->getDeclContext()->mapTypeIntoContext(
      param->getInterfaceType());
  auto resultTy =
      tc.typeCheckParameterDefault(init, dc, defArgType,
                                   /*isAutoClosure=*/param->isAutoClosure(),
                                   /*canFail=*/false);
  assert(resultTy && "Conversion cannot fail");
  (void)resultTy;

  cs.cacheExprTypes(init);

  return {init, param->getDefaultArgumentKind()};
}

static bool canPeepholeTupleConversion(Expr *expr,
                                       ArrayRef<unsigned> sources) {
  if (!isa<TupleExpr>(expr))
    return false;

  for (unsigned i = 0, e = sources.size(); i != e; ++i) {
    if (sources[i] != i)
      return false;
  }

  return true;
}

Expr *ExprRewriter::coerceTupleToTuple(Expr *expr,
                                       TupleType *fromTuple,
                                       TupleType *toTuple,
                                       ConstraintLocatorBuilder locator,
                                       ArrayRef<unsigned> sources) {
  auto &tc = cs.getTypeChecker();

  // If the input expression is a tuple expression, we can convert it in-place.
  if (canPeepholeTupleConversion(expr, sources)) {
    auto *tupleExpr = cast<TupleExpr>(expr);

    for (unsigned i = 0, e = tupleExpr->getNumElements(); i != e; ++i) {
      auto *fromElt = tupleExpr->getElement(i);

      // Actually convert the source element.
      auto toEltType = toTuple->getElementType(i);

      auto *toElt
        = coerceToType(fromElt, toEltType,
                      locator.withPathElement(
                        LocatorPathElt::TupleElement(i)));
      if (!toElt)
        return nullptr;

      tupleExpr->setElement(i, toElt);
    }

    tupleExpr->setType(toTuple);
    cs.cacheType(tupleExpr);

    return tupleExpr;
  }

  // Build a list of OpaqueValueExprs that matches the structure
  // of expr's type.
  //
  // Each OpaqueValueExpr's type is equal to the type of the
  // corresponding element of fromTuple.
  SmallVector<OpaqueValueExpr *, 4> destructured;
  for (unsigned i = 0, e = sources.size(); i != e; ++i) {
    auto fromEltType = fromTuple->getElementType(i);
    auto *opaqueElt =
        new (tc.Context) OpaqueValueExpr(expr->getSourceRange(), fromEltType);
    cs.cacheType(opaqueElt);
    destructured.push_back(opaqueElt);
  }

  // Convert each OpaqueValueExpr to the correct type.
  SmallVector<Expr *, 4> converted;
  SmallVector<Identifier, 4> labels;
  SmallVector<TupleTypeElt, 4> convertedElts;

  for (unsigned i = 0, e = sources.size(); i != e; ++i) {
    unsigned source = sources[i];
    auto *fromElt = destructured[source];

    // Actually convert the source element.
    auto toEltType = toTuple->getElementType(i);
    auto toLabel = toTuple->getElement(i).getName();

    auto *toElt
      = coerceToType(fromElt, toEltType,
                     locator.withPathElement(
                       LocatorPathElt::TupleElement(source)));
    if (!toElt)
      return nullptr;

    converted.push_back(toElt);
    labels.push_back(toLabel);
    convertedElts.emplace_back(toEltType, toLabel, ParameterTypeFlags());
  }

  // Create the result tuple, written in terms of the destructured
  // OpaqueValueExprs.
  auto *result = TupleExpr::createImplicit(tc.Context, converted, labels);
  result->setType(TupleType::get(convertedElts, tc.Context));
  cs.cacheType(result);

  // Create the tuple conversion.
  return
    cs.cacheType(DestructureTupleExpr::create(tc.Context,
                                              destructured, expr, result,
                                              toTuple));
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
    auto fromArchetype = OpenedArchetypeType::getAny(fromType);

    auto *archetypeVal = cs.cacheType(new (tc.Context) OpaqueValueExpr(
        expr->getSourceRange(), fromArchetype));

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
                           ConformanceCheckFlags::InExpression));
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
    fromType = OpenedArchetypeType::getAny(fromType);

    auto *archetypeVal = cs.cacheType(
        new (ctx) OpaqueValueExpr(expr->getSourceRange(), fromType));

    auto *result = cs.cacheType(ErasureExpr::create(ctx, archetypeVal, toType,
                                                    conformances));
    return cs.cacheType(
        new (ctx) OpenExistentialExpr(expr, archetypeVal, result,
                                      cs.getType(result)));
  }

  // Load tuples with lvalue elements.
  if (auto tupleType = fromType->getAs<TupleType>()) {
    if (tupleType->hasLValueType()) {
      expr = cs.coerceToRValue(expr);
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
  auto getArgLocator =
      [&](unsigned argIdx, unsigned paramIdx,
          ParameterTypeFlags flags) -> ConstraintLocatorBuilder {
    return locator.withPathElement(
        LocatorPathElt::ApplyArgToParam(argIdx, paramIdx, flags));
  };

  bool matchCanFail =
      llvm::any_of(params, [](const AnyFunctionType::Param &param) {
        return param.getPlainType()->hasUnresolvedType();
      });

  // Find the callee declaration.
  ConcreteDeclRef callee =
    findCalleeDeclRef(cs, solution, cs.getConstraintLocator(locator));

  // Determine whether this application has curried self.
  bool skipCurriedSelf = apply ? hasCurriedSelf(cs, callee, apply) : true;
  // Determine the parameter bindings.
  ParameterListInfo paramInfo(params, callee.getDecl(), skipCurriedSelf);

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
                                                paramInfo,
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

  SourceLoc lParenLoc, rParenLoc;
  if (argTuple) {
    lParenLoc = argTuple->getLParenLoc();
    rParenLoc = argTuple->getRParenLoc();
  } else if (argParen) {
    lParenLoc = argParen->getLParenLoc();
    rParenLoc = argParen->getRParenLoc();
  }

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

  auto getLabelLoc = [&](unsigned i) -> SourceLoc {
    if (argTuple)
      return argTuple->getElementNameLoc(i);

    assert(i == 0 && "Scalar only has a single argument");
    return SourceLoc();
  };

  SmallVector<Expr *, 4> newArgs;
  SmallVector<Identifier, 4> newLabels;
  SmallVector<SourceLoc, 4> newLabelLocs;
  SmallVector<AnyFunctionType::Param, 4> newParams;

  for (unsigned paramIdx = 0, numParams = parameterBindings.size();
       paramIdx != numParams; ++paramIdx) {
    // Extract the parameter.
    const auto &param = params[paramIdx];
    newLabels.push_back(param.getLabel());

    // Handle variadic parameters.
    if (param.isVariadic()) {
      assert(!param.isInOut());

      SmallVector<Expr *, 4> variadicArgs;

      // The first argument of this vararg parameter may have had a label;
      // save its location.
      auto &varargIndices = parameterBindings[paramIdx];
      if (!varargIndices.empty())
        newLabelLocs.push_back(getLabelLoc(varargIndices[0]));
      else
        newLabelLocs.push_back(SourceLoc());

      // Convert the arguments.
      for (auto argIdx : varargIndices) {
        auto arg = getArg(argIdx);
        auto argType = cs.getType(arg);

        // If the argument type exactly matches, this just works.
        if (argType->isEqual(param.getPlainType())) {
          variadicArgs.push_back(arg);
          continue;
        }

        // Convert the argument.
        auto convertedArg = coerceToType(
            arg, param.getPlainType(),
            getArgLocator(argIdx, paramIdx, param.getParameterFlags()));
        if (!convertedArg)
          return nullptr;

        // Add the converted argument.
        variadicArgs.push_back(convertedArg);
      }

      SourceLoc start, end;
      if (!variadicArgs.empty()) {
        start = variadicArgs.front()->getStartLoc();
        end = variadicArgs.back()->getEndLoc();
      }

      // Collect them into an ArrayExpr.
      auto *arrayExpr = ArrayExpr::create(tc.Context,
                                          start,
                                          variadicArgs,
                                          {}, end,
                                          param.getParameterType());
      arrayExpr->setImplicit();
      cs.cacheType(arrayExpr);

      // Wrap the ArrayExpr in a VarargExpansionExpr.
      auto *varargExpansionExpr =
        new (tc.Context) VarargExpansionExpr(arrayExpr,
                                             /*implicit=*/true,
                                             arrayExpr->getType());
      cs.cacheType(varargExpansionExpr);

      newArgs.push_back(varargExpansionExpr);
      newParams.push_back(param);
      continue;
    }

    // Handle default arguments.
    if (parameterBindings[paramIdx].empty()) {
      Expr *defArg;
      DefaultArgumentKind defArgKind;
      std::tie(defArg, defArgKind) = getCallerDefaultArg(cs, dc, arg->getLoc(),
                                                         callee, paramIdx);

      // If we have a caller-side default argument, just add the magic literal
      // expression to our argument list.
      if (defArg) {
        defArg =
            new (tc.Context) CallerDefaultArgumentExpr(defArg,
                                                       arg->getStartLoc(),
                                                       param.getParameterType());

      // Otherwise, create a call of the default argument generator.
      } else {
        defArg =
            new (tc.Context) DefaultArgumentExpr(callee, paramIdx,
                                                 arg->getStartLoc(),
                                                 param.getParameterType());
      }

      cs.cacheType(defArg);
      newArgs.push_back(defArg);
      newParams.push_back(param);
      newLabelLocs.push_back(SourceLoc());
      continue;
    }

    // Otherwise, we have a plain old ordinary argument.

    // Extract the argument used to initialize this parameter.
    assert(parameterBindings[paramIdx].size() == 1);
    unsigned argIdx = parameterBindings[paramIdx].front();
    auto arg = getArg(argIdx);
    auto argType = cs.getType(arg);

    // Save the original label location.
    newLabelLocs.push_back(getLabelLoc(argIdx));

    // If the types exactly match, this is easy.
    auto paramType = param.getOldType();
    if (argType->isEqual(paramType)) {
      newArgs.push_back(arg);
      newParams.push_back(param);
      continue;
    }

    Expr *convertedArg = nullptr;
    auto argRequiresAutoClosureExpr = [&](const AnyFunctionType::Param &param,
                                          Type argType) {
      if (!param.isAutoClosure())
        return false;

      // Since it was allowed to pass function types to @autoclosure
      // parameters in Swift versions < 5, it has to be handled as
      // a regular function coversion by `coerceToType`.
      if (isAutoClosureArgument(arg)) {
        // In Swift >= 5 mode we only allow `@autoclosure` arguments
        // to be used by value if parameter would return a function
        // type (it just needs to get wrapped into autoclosure expr),
        // otherwise argument must always form a call.
        return cs.getASTContext().isSwiftVersionAtLeast(5);
      }

      return true;
    };

    if (argRequiresAutoClosureExpr(param, argType)) {
      assert(!param.isInOut());

      // If parameter is an autoclosure, we need to make sure that:
      //   - argument type is coerced to parameter result type
      //   - impilict autoclosure is created to wrap argument expression
      //   - new types are propagated to constraint system
      auto *closureType = param.getPlainType()->castTo<FunctionType>();

      arg = coerceToType(
          arg, closureType->getResult(),
          locator.withPathElement(ConstraintLocator::AutoclosureResult));

      convertedArg = cs.TC.buildAutoClosureExpr(dc, arg, closureType);
      cs.cacheExprTypes(convertedArg);
    } else {
      convertedArg = coerceToType(
          arg, paramType,
          getArgLocator(argIdx, paramIdx, param.getParameterFlags()));
    }

    if (!convertedArg)
      return nullptr;

    newArgs.push_back(convertedArg);

    // Make an effort to preserve the sugared type of the argument in the
    // case where there was no conversion, instead of using the parameter
    // type.
    newParams.emplace_back(cs.getType(convertedArg)->getInOutObjectType(),
                           param.getLabel(),
                           param.getParameterFlags());
  }

  assert(newArgs.size() == newParams.size());
  assert(newArgs.size() == newLabels.size());
  assert(newArgs.size() == newLabelLocs.size());

  // This is silly. SILGen gets confused if a 'self' parameter is wrapped
  // in a ParenExpr sometimes.
  if (!argTuple && !argParen &&
      (params[0].getValueOwnership() == ValueOwnership::Default ||
       params[0].getValueOwnership() == ValueOwnership::InOut)) {
    assert(newArgs.size() == 1);
    assert(!hasTrailingClosure);
    return newArgs[0];
  }

  // Rebuild the argument list, sharing as much structure as possible.
  auto paramType = AnyFunctionType::composeInput(tc.Context, newParams,
                                                 /*canonicalVararg=*/false);
  if (isa<ParenType>(paramType.getPointer())) {
    if (argParen) {
      // We already had a ParenExpr, so replace it's sub-expression.
      argParen->setSubExpr(newArgs[0]);
    } else {
      arg = new (tc.Context) ParenExpr(lParenLoc,
                                       newArgs[0],
                                       rParenLoc,
                                       hasTrailingClosure);
      arg->setImplicit();
    }
  } else {
    assert(isa<TupleType>(paramType.getPointer()));

    if (argTuple && newArgs.size() == argTuple->getNumElements()) {
      // The existing TupleExpr has the right number of elements,
      // replace them.
      for (unsigned i = 0, e = newArgs.size(); i != e; ++i) {
        argTuple->setElement(i, newArgs[i]);
      }
    } else {
      // Build a new TupleExpr, re-using source location information.
      arg = TupleExpr::create(tc.Context, lParenLoc,
                              newArgs, newLabels, newLabelLocs,
                              rParenLoc,
                              hasTrailingClosure,
                              /*implicit=*/true);
    }
  }

  arg->setType(paramType);
  return cs.cacheType(arg);
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
    if (auto closure = dyn_cast<ClosureExpr>(semanticExpr))
      return;
    
    tc.diagnose(expr->getLoc(),
                diag::invalid_c_function_pointer_conversion_expr);
  }
}

/// Build the conversion of an element in a collection upcast.
static Expr *buildElementConversion(ExprRewriter &rewriter,
                                    SourceRange srcRange, Type srcType,
                                    Type destType, bool bridged,
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
buildOpaqueElementConversion(ExprRewriter &rewriter, SourceRange srcRange,
                             Type srcCollectionType, Type destCollectionType,
                             bool bridged, ConstraintLocatorBuilder locator,
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
      rewriter.cs.cacheType(new (ctx) OpaqueValueExpr(srcRange, srcType));

  Expr *conversion = buildElementConversion(
      rewriter, srcRange, srcType, destType, bridged,
      locator.withPathElement(LocatorPathElt::GenericArgument(typeArgIndex)),
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
    locator.withPathElement(LocatorPathElt::GenericArgument(0));
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
    locator.withPathElement(LocatorPathElt::GenericArgument(1));

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
        TypeChecker::conformsToProtocol(
                        cs.getType(expr), hashable, cs.DC,
                        ConformanceCheckFlags::InExpression);
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

  // Handle "from specific" coercions before "catch all" coercions.
  auto desugaredFromType = fromType->getDesugaredType();
  switch (desugaredFromType->getKind()) {
  // Coercions from an lvalue: load or perform implicit address-of. We perform
  // these coercions first because they are often the first step in a multi-step
  // coercion.
  case TypeKind::LValue: {
    auto fromLValue = cast<LValueType>(desugaredFromType);
    auto toIO = toType->getAs<InOutType>();
    if (!toIO)
      return coerceToType(addImplicitLoadExpr(cs, expr), toType, locator);

    // In an 'inout' operator like "i += 1", the operand is converted from
    // an implicit lvalue to an inout argument.
    assert(toIO->getObjectType()->isEqual(fromLValue->getObjectType()));
    return cs.cacheType(new (tc.Context) InOutExpr(expr->getStartLoc(), expr,
                                                   toIO->getObjectType(),
                                                   /*isImplicit*/ true));
  }

  // Coerce from a tuple to a tuple.
  case TypeKind::Tuple: {
    auto fromTuple = cast<TupleType>(desugaredFromType);
    auto toTuple = toType->getAs<TupleType>();
    if (!toTuple)
      break;

    if (fromTuple->hasLValueType() && !toTuple->hasLValueType())
      return coerceToType(cs.coerceToRValue(expr), toType, locator);

    SmallVector<unsigned, 4> sources;
    if (!computeTupleShuffle(fromTuple, toTuple, sources)) {
      return coerceTupleToTuple(expr, fromTuple, toTuple,
                                locator, sources);
    }
    break;
  }

  case TypeKind::PrimaryArchetype:
  case TypeKind::OpenedArchetype:
  case TypeKind::NestedArchetype:
  case TypeKind::OpaqueTypeArchetype:
    if (!cast<ArchetypeType>(desugaredFromType)->requiresClass())
      break;
    LLVM_FALLTHROUGH;

  // Coercion from a subclass to a superclass.
  //
  // FIXME: Can we rig things up so that we always have a Superclass
  // conversion restriction in this case?
  case TypeKind::DynamicSelf:
  case TypeKind::BoundGenericClass:
  case TypeKind::Class: {
    if (!toType->getClassOrBoundGenericClass())
      break;
    for (auto fromSuperClass = fromType->getSuperclass();
         fromSuperClass;
         fromSuperClass = fromSuperClass->getSuperclass()) {
      if (fromSuperClass->isEqual(toType)) {
        return coerceSuperclass(expr, toType, locator);
      }
    }
    break;
  }

  // Coercion from one function type to another, this produces a
  // FunctionConversionExpr in its full generality.
  case TypeKind::Function: {
    auto fromFunc = cast<FunctionType>(desugaredFromType);
    auto toFunc = toType->getAs<FunctionType>();
    if (!toFunc)
      break;

    // Default argument generator must return escaping functions. Therefore, we
    // leave an explicit escape to noescape cast here such that SILGen can skip
    // the cast and emit a code for the escaping function.
    bool isInDefaultArgumentContext = false;
    if (auto initalizerCtx = dyn_cast<Initializer>(cs.DC))
      isInDefaultArgumentContext = (initalizerCtx->getInitializerKind() ==
                                    InitializerKind::DefaultArgument);
    auto toEI = toFunc->getExtInfo();
    assert(toType->is<FunctionType>());
    // If we have a ClosureExpr, then we can safely propagate the 'no escape'
    // bit to the closure without invalidating prior analysis.
    auto fromEI = fromFunc->getExtInfo();
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

  // Coercions from one metatype to another.
  case TypeKind::Metatype: {
    if (auto toMeta = toType->getAs<MetatypeType>())
      return cs.cacheType(new(tc.Context) MetatypeConversionExpr(expr, toMeta));
    LLVM_FALLTHROUGH;
  }
  // Coercions from metatype to objects.
  case TypeKind::ExistentialMetatype: {
    auto fromMeta = cast<AnyMetatypeType>(desugaredFromType);
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

    break;
  }

#define SUGARED_TYPE(Name, Parent) case TypeKind::Name:
#define BUILTIN_TYPE(Name, Parent) case TypeKind::Name:
#define UNCHECKED_TYPE(Name, Parent) case TypeKind::Name:
#define ARTIFICIAL_TYPE(Name, Parent) case TypeKind::Name:
#define TYPE(Name, Parent)
#include "swift/AST/TypeNodes.def"
  case TypeKind::Error:
  case TypeKind::InOut:
  case TypeKind::Module:
  case TypeKind::Enum:
  case TypeKind::Struct:
  case TypeKind::Protocol:
  case TypeKind::ProtocolComposition:
  case TypeKind::BoundGenericEnum:
  case TypeKind::BoundGenericStruct:
  case TypeKind::GenericFunction:
  case TypeKind::GenericTypeParam:
  case TypeKind::DependentMember:
    break;
  }

  // "Catch all" coercions.
  auto desugaredToType = toType->getDesugaredType();
  switch (desugaredToType->getKind()) {
  // Coercions from a type to an existential type.
  case TypeKind::ExistentialMetatype:
  case TypeKind::ProtocolComposition:
  case TypeKind::Protocol:
    return coerceExistential(expr, toType, locator);

  // Coercion to Optional<T>.
  case TypeKind::BoundGenericEnum: {
    auto toGenericType = cast<BoundGenericEnumType>(desugaredToType);
    if (!toGenericType->getDecl()->isOptionalDecl())
      break;
    tc.requireOptionalIntrinsics(expr->getLoc());

    if (cs.getType(expr)->getOptionalObjectType())
      return coerceOptionalToOptional(expr, toType, locator, typeFromPattern);

    Type valueType = toGenericType->getGenericArgs()[0];
    expr = coerceToType(expr, valueType, locator);
    if (!expr) return nullptr;

    auto *result =
        cs.cacheType(new (tc.Context) InjectIntoOptionalExpr(expr, toType));
    diagnoseOptionalInjection(result);
    return result;
  }

#define SUGARED_TYPE(Name, Parent) case TypeKind::Name:
#define BUILTIN_TYPE(Name, Parent) case TypeKind::Name:
#define UNCHECKED_TYPE(Name, Parent) case TypeKind::Name:
#define ARTIFICIAL_TYPE(Name, Parent) case TypeKind::Name:
#define TYPE(Name, Parent)
#include "swift/AST/TypeNodes.def"
  case TypeKind::Error:
  case TypeKind::Module:
  case TypeKind::Tuple:
  case TypeKind::Enum:
  case TypeKind::Struct:
  case TypeKind::Class:
  case TypeKind::BoundGenericClass:
  case TypeKind::BoundGenericStruct:
  case TypeKind::Metatype:
  case TypeKind::DynamicSelf:
  case TypeKind::PrimaryArchetype:
  case TypeKind::OpenedArchetype:
  case TypeKind::NestedArchetype:
  case TypeKind::OpaqueTypeArchetype:
  case TypeKind::GenericTypeParam:
  case TypeKind::DependentMember:
  case TypeKind::Function:
  case TypeKind::GenericFunction:
  case TypeKind::LValue:
  case TypeKind::InOut:
    break;
  }

  // Unresolved types come up in diagnostics for lvalue and inout types.
  if (fromType->hasUnresolvedType() || toType->hasUnresolvedType())
    return cs.cacheType(new (tc.Context)
                            UnresolvedTypeConversionExpr(expr, toType));

  // Use an opaque type to abstract a value of the underlying concrete type.
  if (toType->getAs<OpaqueTypeArchetypeType>()) {
    return cs.cacheType(new (tc.Context)
                        UnderlyingToOpaqueExpr(expr, toType));
  }
  
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
  bool isSettableFromHere =
      SD->isSettable(UseDC) && SD->isSetterAccessibleFrom(UseDC);

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
                                        UseDC->getParentModule(),
                                        UseDC->getResilienceExpansion());
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

Expr *ExprRewriter::convertLiteralInPlace(Expr *literal,
                                          Type type,
                                          ProtocolDecl *protocol,
                                          Identifier literalType,
                                          DeclName literalFuncName,
                                          ProtocolDecl *builtinProtocol,
                                          DeclName builtinLiteralFuncName,
                                          Diag<> brokenProtocolDiag,
                                          Diag<> brokenBuiltinProtocolDiag) {
  // If coercing a literal to an unresolved type, we don't try to look up the
  // witness members, just do it.
  if (type->is<UnresolvedType>()) {
    cs.setType(literal, type);
    return literal;
  }

  // Check whether this literal type conforms to the builtin protocol. If so,
  // initialize via the builtin protocol.
  Optional<ProtocolConformanceRef> builtinConformance;
  if (builtinProtocol &&
      (builtinConformance =
         TypeChecker::conformsToProtocol(type, builtinProtocol, cs.DC,
                                         ConformanceCheckFlags::InExpression))) {

    // Find the witness that we'll use to initialize the type via a builtin
    // literal.
    auto witness = builtinConformance->getWitnessByName(type->getRValueType(),
                                                        builtinLiteralFuncName);
    if (!witness || !isa<AbstractFunctionDecl>(witness.getDecl()))
      return nullptr;

    // Form a reference to the builtin conversion function.

    // Set the builtin initializer.
    if (auto stringLiteral = dyn_cast<StringLiteralExpr>(literal))
      stringLiteral->setBuiltinInitializer(witness);
    else if (auto booleanLiteral = dyn_cast<BooleanLiteralExpr>(literal))
      booleanLiteral->setBuiltinInitializer(witness);
    else if (auto numberLiteral = dyn_cast<NumberLiteralExpr>(literal))
      numberLiteral->setBuiltinInitializer(witness);
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
  auto conformance = TypeChecker::conformsToProtocol(type, protocol, cs.DC,
                                           ConformanceCheckFlags::InExpression);
  assert(conformance && "must conform to literal protocol");

  // Dig out the literal type and perform a builtin literal conversion to it.
  if (!literalType.empty()) {
    // Extract the literal type.
    Type builtinLiteralType =
        conformance->getTypeWitnessByName(type, literalType);
    if (builtinLiteralType->hasError())
      return nullptr;

    // Perform the builtin conversion.
    if (!convertLiteralInPlace(literal, builtinLiteralType, nullptr,
                               Identifier(), DeclName(), builtinProtocol,
                               builtinLiteralFuncName, brokenProtocolDiag,
                               brokenBuiltinProtocolDiag))
      return nullptr;
  }

  // Find the witness that we'll use to initialize the literal value.
  auto witness = conformance->getWitnessByName(type->getRValueType(),
                                               literalFuncName);
  if (!witness || !isa<AbstractFunctionDecl>(witness.getDecl()))
    return nullptr;

  // Set the initializer.
  if (auto nilLiteral = dyn_cast<NilLiteralExpr>(literal))
    nilLiteral->setInitializer(witness);
  else if (auto stringLiteral = dyn_cast<StringLiteralExpr>(literal))
    stringLiteral->setInitializer(witness);
  else if (auto booleanLiteral = dyn_cast<BooleanLiteralExpr>(literal))
    booleanLiteral->setInitializer(witness);
  else if (auto numberLiteral = dyn_cast<NumberLiteralExpr>(literal))
    numberLiteral->setInitializer(witness);
  else
    cast<MagicIdentifierLiteralExpr>(literal)->setInitializer(witness);

  // The literal expression has this type.
  cs.setType(literal, type);

  return literal;
}

// Returns true if the given method and method type are a valid
// `@dynamicCallable` required `func dynamicallyCall` method.
static bool isValidDynamicCallableMethod(FuncDecl *method,
                                         AnyFunctionType *methodType) {
  auto &ctx = method->getASTContext();
  if (method->getName() != ctx.Id_dynamicallyCall)
    return false;
  if (methodType->getParams().size() != 1)
    return false;
  auto argumentLabel = methodType->getParams()[0].getLabel();
  if (argumentLabel != ctx.Id_withArguments &&
      argumentLabel != ctx.Id_withKeywordArguments)
    return false;
  return true;
}

// Resolve `callAsFunction` method applications.
static Expr *finishApplyCallAsFunctionMethod(
    ExprRewriter &rewriter, ApplyExpr *apply, SelectedOverload selected,
    AnyFunctionType *openedMethodType,
    ConstraintLocatorBuilder applyFunctionLoc) {
  auto &cs = rewriter.cs;
  auto *fn = apply->getFn();
  auto choice = selected.choice;
  // Create direct reference to `callAsFunction` method.
  bool isDynamic = choice.getKind() == OverloadChoiceKind::DeclViaDynamic;
  auto *declRef = rewriter.buildMemberRef(
      fn, selected.openedFullType, /*dotLoc*/ SourceLoc(), choice,
      DeclNameLoc(fn->getEndLoc()), selected.openedType, applyFunctionLoc,
      applyFunctionLoc, /*implicit*/ true, choice.getFunctionRefKind(),
      AccessSemantics::Ordinary, isDynamic);
  if (!declRef)
    return nullptr;
  declRef->setImplicit(apply->isImplicit());
  apply->setFn(declRef);
  // Coerce argument to input type of the `callAsFunction` method.
  SmallVector<Identifier, 2> argLabelsScratch;
  auto *arg = rewriter.coerceCallArguments(
      apply->getArg(), openedMethodType, apply,
      apply->getArgumentLabels(argLabelsScratch), apply->hasTrailingClosure(),
      applyFunctionLoc);
  if (!arg)
    return nullptr;
  apply->setArg(arg);
  cs.setType(apply, openedMethodType->getResult());
  cs.cacheExprTypes(apply);
  return apply;
}

// Resolve `@dynamicCallable` applications.
Expr *
ExprRewriter::finishApplyDynamicCallable(ApplyExpr *apply,
                                         SelectedOverload selected,
                                         FuncDecl *method,
                                         AnyFunctionType *methodType,
                                         ConstraintLocatorBuilder loc) {
  auto &ctx = cs.getASTContext();
  auto *fn = apply->getFn();

  TupleExpr *arg = dyn_cast<TupleExpr>(apply->getArg());
  if (auto parenExpr = dyn_cast<ParenExpr>(apply->getArg()))
    arg = TupleExpr::createImplicit(ctx, parenExpr->getSubExpr(), {});

  // Get resolved `dynamicallyCall` method and verify it.
  assert(isValidDynamicCallableMethod(method, methodType));
  auto params = methodType->getParams();
  auto argumentType = params[0].getParameterType();

  // Determine which method was resolved: a `withArguments` method or a
  // `withKeywordArguments` method.
  auto argumentLabel = methodType->getParams()[0].getLabel();
  bool useKwargsMethod = argumentLabel == ctx.Id_withKeywordArguments;

  // Construct expression referencing the `dynamicallyCall` method.
  bool isDynamic =
      selected.choice.getKind() == OverloadChoiceKind::DeclViaDynamic;
  auto member = buildMemberRef(fn, selected.openedFullType,
                               SourceLoc(), selected.choice,
                               DeclNameLoc(method->getNameLoc()),
                               selected.openedType, loc, loc, /*implicit*/ true,
                               selected.choice.getFunctionRefKind(),
                               AccessSemantics::Ordinary, isDynamic);

  // Construct argument to the method (either an array or dictionary
  // expression).
  Expr *argument = nullptr;
  if (!useKwargsMethod) {
    argument = ArrayExpr::create(ctx, SourceLoc(), arg->getElements(),
                                 {}, SourceLoc());
    cs.setType(argument, argumentType);
    finishArrayExpr(cast<ArrayExpr>(argument));
  } else {
    auto dictLitProto =
        ctx.getProtocol(KnownProtocolKind::ExpressibleByDictionaryLiteral);
    auto conformance =
        cs.TC.conformsToProtocol(argumentType, dictLitProto, cs.DC,
                                 ConformanceCheckFlags::InExpression);
    auto keyType = conformance->getTypeWitnessByName(argumentType, ctx.Id_Key);
    auto valueType = conformance->getTypeWitnessByName(argumentType,
                                                       ctx.Id_Value);
    SmallVector<Identifier, 4> names;
    SmallVector<Expr *, 4> dictElements;
    for (unsigned i = 0, n = arg->getNumElements(); i < n; i++) {
      Expr *labelExpr =
        new (ctx) StringLiteralExpr(arg->getElementName(i).get(),
                                    arg->getElementNameLoc(i),
                                    /*Implicit*/ true);
      cs.setType(labelExpr, keyType);
      handleStringLiteralExpr(cast<LiteralExpr>(labelExpr));

      Expr *valueExpr = coerceToType(arg->getElement(i), valueType, loc);
      if (!valueExpr)
        return nullptr;
      Expr *pair = TupleExpr::createImplicit(ctx, {labelExpr, valueExpr}, {});
      auto eltTypes = { TupleTypeElt(keyType), TupleTypeElt(valueType) };
      cs.setType(pair, TupleType::get(eltTypes, ctx));
      dictElements.push_back(pair);
    }
    argument = DictionaryExpr::create(ctx, SourceLoc(), dictElements, {},
                                      SourceLoc());
    cs.setType(argument, argumentType);
    finishDictionaryExpr(cast<DictionaryExpr>(argument));
  }
  argument->setImplicit();

  // Construct call to the `dynamicallyCall` method.
  auto result = CallExpr::createImplicit(ctx, member, argument,
                                         { argumentLabel });
  cs.setType(result->getArg(), AnyFunctionType::composeInput(ctx, params,
                                                             false));
  cs.setType(result, methodType->getResult());
  cs.cacheExprTypes(result);
  return result;
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
            OpaqueValueExpr(apply->getFn()->getSourceRange(), Type());
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
        assert(openedInstanceTy->castTo<OpenedArchetypeType>()
                   ->getOpenedExistentialType()
                   ->isEqual(existentialInstanceTy));

        auto opaqueValue =
            new (tc.Context) OpaqueValueExpr(apply->getSourceRange(), openedTy);
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

  // Resolve `callAsFunction` and `@dynamicCallable` applications.
  auto applyFunctionLoc =
      locator.withPathElement(ConstraintLocator::ApplyFunction);
  if (auto selected = solution.getOverloadChoiceIfAvailable(
          cs.getConstraintLocator(applyFunctionLoc))) {
    auto *method = dyn_cast<FuncDecl>(selected->choice.getDecl());
    auto methodType =
        simplifyType(selected->openedType)->getAs<AnyFunctionType>();
    if (method && methodType) {
      if (method->isCallAsFunctionMethod())
        return finishApplyCallAsFunctionMethod(
            *this, apply, *selected, methodType, applyFunctionLoc);
      if (methodType && isValidDynamicCallableMethod(method, methodType))
        return finishApplyDynamicCallable(
            apply, *selected, method, methodType, applyFunctionLoc);
    }
  }

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

  // We have a type constructor.
  auto metaTy = cs.getType(fn)->castTo<AnyMetatypeType>();
  auto ty = metaTy->getInstanceType();

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
                                 /*Implicit=*/true,
                                 choice.getFunctionRefKind(),
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
  if (!parent || isa<TupleExpr>(parent)) {
    return false;
  }

  if (auto parenExp = dyn_cast<ParenExpr>(parent))
    if (!parenExp->isImplicit())
      return false;

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
    SmallVector<std::pair<TapExpr *, DeclContext *>, 4> TapsToTypeCheck;

  public:
    ExprWalker(ExprRewriter &Rewriter) : Rewriter(Rewriter) { }

    const SmallVectorImpl<ClosureExpr *> &getClosuresToTypeCheck() const {
      return ClosuresToTypeCheck;
    }

    const SmallVectorImpl<std::pair<TapExpr *, DeclContext *>> &getTapsToTypeCheck() const {
      return TapsToTypeCheck;
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
        tc.coerceParameterListToType(params, closure, fnType);

        // If this closure had a function builder applied, rewrite it to a
        // closure with a single expression body containing the builder
        // invocations.
        auto builder =
            Rewriter.solution.builderTransformedClosures.find(closure);
        if (builder != Rewriter.solution.builderTransformedClosures.end()) {
          auto singleExpr = builder->second.second;
          auto returnStmt = new (tc.Context) ReturnStmt(
             singleExpr->getStartLoc(), singleExpr, /*implicit=*/true);
          auto braceStmt = BraceStmt::create(
              tc.Context, returnStmt->getStartLoc(), ASTNode(returnStmt),
              returnStmt->getEndLoc(), /*implicit=*/true);
          closure->setBody(braceStmt, /*isSingleExpression=*/true);
        }

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

        // Don't try to register captures if constraint system is used to
        // produce diagnostics for one of the sub-expressions.
        if (!cs.Options.contains(
                ConstraintSystemFlags::SubExpressionDiagnostics))
          tc.ClosuresWithUncomputedCaptures.push_back(closure);

        return { false, closure };
      }

      if (auto tap = dyn_cast_or_null<TapExpr>(expr)) {
        // We remember the DeclContext because the code to handle
        // single-expression-body closures above changes it.
        TapsToTypeCheck.push_back(std::make_pair(tap, Rewriter.dc));
      }

      Rewriter.walkToExprPre(expr);
      return { true, expr };
    }

    Expr *walkToExprPost(Expr *expr) override {
      return Rewriter.walkToExprPost(expr);
    }

    /// Ignore statements.
    std::pair<bool, Stmt *> walkToStmtPre(Stmt *stmt) override {
      return { false, stmt };
    }

    /// Ignore declarations.
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
  // First transfer all of the deduced information back
  // to the constraint system.
  applySolution(solution);

  class DiagnosticWalker : public ASTWalker {
    Expr *root;
    const Solution &solution;
    llvm::SmallDenseMap<Expr *, SmallVector<ConstraintFix *, 4>> fixesPerExpr;

    /// Determines whether any error have been diagnosed while
    /// trying to apply fixes associated with a given solution.
    bool DiagnosedAnyErrors = false;

  public:
    DiagnosticWalker(Expr *expr, const Solution &solution)
        : root(expr), solution(solution) {
      for (auto *fix : solution.Fixes)
        fixesPerExpr[fix->getAnchor()].push_back(fix);
    }

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      // Diagnose root expression last.
      if (E == root)
        return {true, E};

      if (auto *closure = dyn_cast<ClosureExpr>(E)) {
        auto result = solution.builderTransformedClosures.find(closure);
        if (result != solution.builderTransformedClosures.end()) {
          auto *transformedExpr = result->second.second;
          // Since this closure has been transformed into something
          // else let's look inside transformed expression instead.
          transformedExpr->walk(*this);
          return {false, E};
        }
      }

      diagnose(E);
      return {true, E};
    }

    Expr *walkToExprPost(Expr *E) override {
      if (E == root)
        diagnose(E);
      return E;
    }

    std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
      return {true, S};
    }

    bool hadErrors() const { return DiagnosedAnyErrors; }

  private:
    void diagnose(Expr *E) {
      auto fixes = fixesPerExpr.find(E);
      if (fixes == fixesPerExpr.end())
        return;

      for (const auto *fix : fixes->second) {
        auto diagnosed = fix->diagnose(root);
        if (fix->isWarning()) {
          assert(diagnosed && "warnings should always be diagnosed");
          (void)diagnosed;
        } else {
          DiagnosedAnyErrors |= diagnosed;
        }
      }
    }
  };

  DiagnosticWalker diagnostics(E, solution);
  E->walk(diagnostics);
  return diagnostics.hadErrors();
}

/// Apply a given solution to the expression, producing a fully
/// type-checked expression.
Expr *ConstraintSystem::applySolution(Solution &solution, Expr *expr,
                                      Type convertType,
                                      bool discardedExpr,
                                      bool skipClosures) {
  // Add the node types back.
  for (auto &nodeType : solution.addedNodeTypes) {
    setType(nodeType.first, nodeType.second);
  }

  // If any fixes needed to be applied to arrive at this solution, resolve
  // them to specific expressions.
  if (!solution.Fixes.empty()) {
    if (shouldSuppressDiagnostics())
      return nullptr;

    bool diagnosedErrorsViaFixes = applySolutionFixes(expr, solution);
    // If all of the available fixes would result in a warning,
    // we can go ahead and apply this solution to AST.
    if (!llvm::all_of(solution.Fixes, [](const ConstraintFix *fix) {
          return fix->isWarning();
        })) {
      // If we already diagnosed any errors via fixes, that's it.
      if (diagnosedErrorsViaFixes)
        return nullptr;

      // If we didn't manage to diagnose anything well, so fall back to
      // diagnosing mining the system to construct a reasonable error message.
      diagnoseFailureForExpr(expr);
      return nullptr;
    }
  }

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

    // Tap expressions too; they should or should not be
    // type-checked under the same conditions as closure bodies.
    for (auto tuple : walker.getTapsToTypeCheck()) {
      auto tap = std::get<0>(tuple);
      auto tapDC = std::get<1>(tuple);
      hadError |= tc.typeCheckTapBody(tap, tapDC);
    }

    // If any of them failed to type check, bail.
    if (hadError)
      return nullptr;
  }

  // We are supposed to use contextual type only if it is present and
  // this expression doesn't represent the implicit return of the single
  // expression function which got deduced to be `Never`.
  auto shouldCoerceToContextualType = [&]() {
    return convertType && !(getType(result)->isUninhabited() &&
                            getContextualTypePurpose() == CTP_ReturnSingleExpr);
  };

  // If we're supposed to convert the expression to some particular type,
  // do so now.
  if (shouldCoerceToContextualType()) {
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
