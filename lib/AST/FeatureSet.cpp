//===--- FeatureSet.cpp - Language feature support --------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "FeatureSet.h"

#include "swift/AST/ASTWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/GenericParamList.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/ProtocolConformance.h"
#include "clang/AST/DeclObjC.h"
#include "swift/Basic/Assertions.h"

using namespace swift;

/// Does the interface of this declaration use a type for which the
/// given predicate returns true?
static bool usesTypeMatching(const Decl *decl,
                             llvm::function_ref<bool(Type)> fn) {
  if (auto value = dyn_cast<ValueDecl>(decl)) {
    if (Type type = value->getInterfaceType()) {
      return type.findIf(fn);
    }
  }

  return false;
}

// ----------------------------------------------------------------------------
// MARK: - Standard Features
// ----------------------------------------------------------------------------

/// Functions to determine which features a particular declaration uses. The
/// usesFeatureNNN functions correspond to the features in Features.def.

#define BASELINE_LANGUAGE_FEATURE(FeatureName, SENumber, Description)          \
  static bool usesFeature##FeatureName(Decl *decl) { return false; }
#define LANGUAGE_FEATURE(FeatureName, SENumber, Description)
#include "swift/Basic/Features.def"

#define UNINTERESTING_FEATURE(FeatureName)                                     \
  static bool usesFeature##FeatureName(Decl *decl) { return false; }

// ----------------------------------------------------------------------------
// MARK: - Upcoming Features
// ----------------------------------------------------------------------------

UNINTERESTING_FEATURE(ConciseMagicFile)
UNINTERESTING_FEATURE(ForwardTrailingClosures)
UNINTERESTING_FEATURE(StrictConcurrency)
UNINTERESTING_FEATURE(BareSlashRegexLiterals)
UNINTERESTING_FEATURE(DeprecateApplicationMain)
UNINTERESTING_FEATURE(ImportObjcForwardDeclarations)
UNINTERESTING_FEATURE(DisableOutwardActorInference)
UNINTERESTING_FEATURE(InternalImportsByDefault)
UNINTERESTING_FEATURE(IsolatedDefaultValues)
UNINTERESTING_FEATURE(GlobalConcurrency)
UNINTERESTING_FEATURE(FullTypedThrows)
UNINTERESTING_FEATURE(ExistentialAny)
UNINTERESTING_FEATURE(InferSendableFromCaptures)
UNINTERESTING_FEATURE(ImplicitOpenExistentials)
UNINTERESTING_FEATURE(MemberImportVisibility)

// ----------------------------------------------------------------------------
// MARK: - Experimental Features
// ----------------------------------------------------------------------------

UNINTERESTING_FEATURE(StaticAssert)
UNINTERESTING_FEATURE(NamedOpaqueTypes)
UNINTERESTING_FEATURE(FlowSensitiveConcurrencyCaptures)
UNINTERESTING_FEATURE(CodeItemMacros)
UNINTERESTING_FEATURE(PreambleMacros)
UNINTERESTING_FEATURE(TupleConformances)
UNINTERESTING_FEATURE(SymbolLinkageMarkers)
UNINTERESTING_FEATURE(LazyImmediate)
UNINTERESTING_FEATURE(MoveOnlyClasses)
UNINTERESTING_FEATURE(NoImplicitCopy)
UNINTERESTING_FEATURE(OldOwnershipOperatorSpellings)
UNINTERESTING_FEATURE(MoveOnlyEnumDeinits)
UNINTERESTING_FEATURE(MoveOnlyTuples)
UNINTERESTING_FEATURE(MoveOnlyPartialReinitialization)
UNINTERESTING_FEATURE(AccessLevelOnImport)
UNINTERESTING_FEATURE(AllowNonResilientAccessInPackage)
UNINTERESTING_FEATURE(ClientBypassResilientAccessInPackage)
UNINTERESTING_FEATURE(LayoutStringValueWitnesses)
UNINTERESTING_FEATURE(LayoutStringValueWitnessesInstantiation)
UNINTERESTING_FEATURE(DifferentiableProgramming)
UNINTERESTING_FEATURE(ForwardModeDifferentiation)
UNINTERESTING_FEATURE(AdditiveArithmeticDerivedConformances)
UNINTERESTING_FEATURE(SendableCompletionHandlers)
UNINTERESTING_FEATURE(OpaqueTypeErasure)
UNINTERESTING_FEATURE(PackageCMO)
UNINTERESTING_FEATURE(ParserRoundTrip)
UNINTERESTING_FEATURE(ParserValidation)
UNINTERESTING_FEATURE(UnqualifiedLookupValidation)
UNINTERESTING_FEATURE(ImplicitSome)
UNINTERESTING_FEATURE(ParserASTGen)
UNINTERESTING_FEATURE(BuiltinMacros)
UNINTERESTING_FEATURE(GenerateBindingsForThrowingFunctionsInCXX)
UNINTERESTING_FEATURE(ReferenceBindings)
UNINTERESTING_FEATURE(BuiltinModule)
UNINTERESTING_FEATURE(RegionBasedIsolation)
UNINTERESTING_FEATURE(PlaygroundExtendedCallbacks)
UNINTERESTING_FEATURE(ThenStatements)
UNINTERESTING_FEATURE(DoExpressions)
UNINTERESTING_FEATURE(ImplicitLastExprResults)
UNINTERESTING_FEATURE(RawLayout)
UNINTERESTING_FEATURE(Embedded)
UNINTERESTING_FEATURE(Volatile)
UNINTERESTING_FEATURE(SuppressedAssociatedTypes)
UNINTERESTING_FEATURE(StructLetDestructuring)
UNINTERESTING_FEATURE(MacrosOnImports)
UNINTERESTING_FEATURE(NonisolatedNonsendingByDefault)
UNINTERESTING_FEATURE(KeyPathWithMethodMembers)
UNINTERESTING_FEATURE(ImportMacroAliases)
UNINTERESTING_FEATURE(NoExplicitNonIsolated)

// TODO: Return true for inlinable function bodies with module selectors in them
UNINTERESTING_FEATURE(ModuleSelector)

static bool usesFeatureInlineArrayTypeSugar(Decl *D) {
  return usesTypeMatching(D, [&](Type ty) {
    return isa<InlineArrayType>(ty.getPointer());
  });
}

UNINTERESTING_FEATURE(StaticExclusiveOnly)
UNINTERESTING_FEATURE(ExtractConstantsFromMembers)
UNINTERESTING_FEATURE(GroupActorErrors)
UNINTERESTING_FEATURE(SameElementRequirements)
UNINTERESTING_FEATURE(SendingArgsAndResults)

static bool findUnderscoredLifetimeAttr(Decl *decl) {
  auto hasUnderscoredLifetimeAttr = [](Decl *decl) {
    if (!decl->getAttrs().hasAttribute<LifetimeAttr>()) {
      return false;
    }
    // Since we ban mixing @lifetime and @_lifetime on the same decl, checking
    // any one LifetimeAttr on the decl is sufficient.
    // FIXME: Implement the ban.
    return decl->getAttrs().getAttribute<LifetimeAttr>()->isUnderscored();
  };

  switch (decl->getKind()) {
  case DeclKind::Var: {
    auto *var = cast<VarDecl>(decl);
    return llvm::any_of(var->getAllAccessors(), hasUnderscoredLifetimeAttr);
  }
  default:
    return hasUnderscoredLifetimeAttr(decl);
  }
}

static bool usesFeatureLifetimeDependence(Decl *decl) {
  if (decl->getAttrs().hasAttribute<LifetimeAttr>()) {
    if (findUnderscoredLifetimeAttr(decl)) {
      // Experimental feature Lifetimes will guard the decl.
      return false;
    }
    return true;
  }

  if (auto *afd = dyn_cast<AbstractFunctionDecl>(decl)) {
    return afd->getInterfaceType()
      ->getAs<AnyFunctionType>()
      ->hasLifetimeDependencies();
  }
  if (auto *varDecl = dyn_cast<VarDecl>(decl)) {
    return !varDecl->getTypeInContext()->isEscapable();
  }
  return false;
}

static bool usesFeatureLifetimes(Decl *decl) {
  return findUnderscoredLifetimeAttr(decl);
}

static bool usesFeatureInoutLifetimeDependence(Decl *decl) {
  auto hasInoutLifetimeDependence = [](Decl *decl) {
    for (auto attr : decl->getAttrs().getAttributes<LifetimeAttr>()) {
      for (auto source : attr->getLifetimeEntry()->getSources()) {
        if (source.getParsedLifetimeDependenceKind() ==
            ParsedLifetimeDependenceKind::Inout) {
          return true;
        }
      }
    }
    return false;
  };

  switch (decl->getKind()) {
  case DeclKind::Var: {
    auto *var = cast<VarDecl>(decl);
    return llvm::any_of(var->getAllAccessors(), hasInoutLifetimeDependence);
  }
  default:
    return hasInoutLifetimeDependence(decl);
  }
}

static bool usesFeatureLifetimeDependenceMutableAccessors(Decl *decl) {
  if (!isa<VarDecl>(decl)) {
    return false;
  }
  auto var = cast<VarDecl>(decl);
  return var->isGetterMutating() && !var->getTypeInContext()->isEscapable();
}

static bool usesFeatureNonescapableAccessorOnTrivial(Decl *decl) {
  if (!isa<VarDecl>(decl)) {
    return false;
  }
  auto var = cast<VarDecl>(decl);
  if (!var->hasParsedAccessors()) {
    return false;
  }
  // Check for properties that are both non-Copyable and non-Escapable
  // (MutableSpan).
  if (var->getTypeInContext()->isNoncopyable()
      && !var->getTypeInContext()->isEscapable()) {
    auto selfTy = var->getDeclContext()->getSelfTypeInContext();
    // Consider 'self' trivial if it is BitwiseCopyable and Escapable
    // (UnsafeMutableBufferPointer).
    return selfTy->isBitwiseCopyable() && selfTy->isEscapable();
  }
  return false;
}

UNINTERESTING_FEATURE(DynamicActorIsolation)
UNINTERESTING_FEATURE(NonfrozenEnumExhaustivity)
UNINTERESTING_FEATURE(ClosureIsolation)
UNINTERESTING_FEATURE(Extern)
UNINTERESTING_FEATURE(ConsumeSelfInDeinit)

static bool usesFeatureAddressableParameters(Decl *d) {
  if (d->getAttrs().hasAttribute<AddressableSelfAttr>()) {
    return true;
  }

  auto fd = dyn_cast<AbstractFunctionDecl>(d);
  if (!fd) {
    return false;
  }
  
  for (auto pd : *fd->getParameters()) {
    if (pd->isAddressable()) {
      return true;
    }
  }
  return false;
}

static bool usesFeatureAddressableTypes(Decl *d) {
  if (d->getAttrs().hasAttribute<AddressableForDependenciesAttr>()) {
    return true;
  }
  
  return false;
}

UNINTERESTING_FEATURE(AddressableInterop)
UNINTERESTING_FEATURE(IsolatedAny2)
UNINTERESTING_FEATURE(GlobalActorIsolatedTypesUsability)
UNINTERESTING_FEATURE(ObjCImplementation)
UNINTERESTING_FEATURE(ObjCImplementationWithResilientStorage)
UNINTERESTING_FEATURE(CImplementation)
UNINTERESTING_FEATURE(Sensitive)
UNINTERESTING_FEATURE(DebugDescriptionMacro)
UNINTERESTING_FEATURE(ReinitializeConsumeInMultiBlockDefer)
UNINTERESTING_FEATURE(SE427NoInferenceOnExtension)
UNINTERESTING_FEATURE(TrailingComma)
UNINTERESTING_FEATURE(RawIdentifiers)
UNINTERESTING_FEATURE(InferIsolatedConformances)

static ABIAttr *getABIAttr(Decl *decl) {
  if (auto pbd = dyn_cast<PatternBindingDecl>(decl))
    for (auto i : range(pbd->getNumPatternEntries()))
      if (auto anchorVar = pbd->getAnchoringVarDecl(i))
        return getABIAttr(anchorVar);
  // FIXME: EnumCaseDecl/EnumElementDecl

  return decl->getAttrs().getAttribute<ABIAttr>();
}

static bool usesFeatureABIAttributeSE0479(Decl *decl) {
  return getABIAttr(decl) != nullptr;
}

static bool usesFeatureIsolatedConformances(Decl *decl) { 
  // FIXME: Check conformances associated with this decl?
  return false;
}

static bool usesFeatureConcurrencySyntaxSugar(Decl *decl) {
  return false;
}

static bool usesFeatureCompileTimeValues(Decl *decl) {
  return decl->getAttrs().hasAttribute<ConstValAttr>() ||
         decl->getAttrs().hasAttribute<ConstInitializedAttr>();
}

static bool usesFeatureCompileTimeValuesPreview(Decl *decl) {
  return false;
}

static bool usesFeatureClosureBodyMacro(Decl *decl) {
  return false;
}

static bool usesFeatureCDecl(Decl *decl) {
  auto attr = decl->getAttrs().getAttribute<CDeclAttr>();
  return attr && !attr->Underscored;
}

UNINTERESTING_FEATURE(StrictMemorySafety)
UNINTERESTING_FEATURE(SafeInteropWrappers)
UNINTERESTING_FEATURE(AssumeResilientCxxTypes)
UNINTERESTING_FEATURE(ImportNonPublicCxxMembers)
UNINTERESTING_FEATURE(SuppressCXXForeignReferenceTypeInitializers)
UNINTERESTING_FEATURE(WarnUnannotatedReturnOfCxxFrt)
UNINTERESTING_FEATURE(CoroutineAccessorsUnwindOnCallerError)
UNINTERESTING_FEATURE(AllowRuntimeSymbolDeclarations)

static bool usesFeatureCoroutineAccessors(Decl *decl) {
  auto accessorDeclUsesFeatureCoroutineAccessors = [](AccessorDecl *accessor) {
    return requiresFeatureCoroutineAccessors(accessor->getAccessorKind());
  };
  switch (decl->getKind()) {
  case DeclKind::Var: {
    auto *var = cast<VarDecl>(decl);
    return llvm::any_of(var->getAllAccessors(),
                        accessorDeclUsesFeatureCoroutineAccessors);
  }
  case DeclKind::Accessor: {
    auto *accessor = cast<AccessorDecl>(decl);
    return accessorDeclUsesFeatureCoroutineAccessors(accessor);
  }
  default:
    return false;
  }
}

UNINTERESTING_FEATURE(GeneralizedIsSameMetaTypeBuiltin)

static bool usesFeatureCustomAvailability(Decl *decl) {
  for (auto attr : decl->getSemanticAvailableAttrs()) {
    if (attr.getDomain().isCustom())
      return true;
  }
  return false;
}

static bool usesFeatureAsyncExecutionBehaviorAttributes(Decl *decl) {
  // Explicit `@concurrent` attribute on the declaration.
  if (decl->getAttrs().hasAttribute<ConcurrentAttr>())
    return true;

  // Explicit `nonisolated(nonsending)` attribute on the declaration.
  if (auto *nonisolated = decl->getAttrs().getAttribute<NonisolatedAttr>()) {
    if (nonisolated->isNonSending())
      return true;
  }

  auto hasCallerIsolatedAttr = [](TypeRepr *R) {
    if (!R)
      return false;

    return R->findIf([](TypeRepr *repr) {
      if (isa<CallerIsolatedTypeRepr>(repr))
        return true;

      // We don't check for @concurrent here because it's
      // not printed in type positions since it indicates
      // old "nonisolated" state.

      return false;
    });
  };

  auto *VD = dyn_cast<ValueDecl>(decl);
  if (!VD)
    return false;

  // The declaration is going to be printed with `nonisolated(nonsending)`
  // attribute.
  if (getActorIsolation(VD).isCallerIsolationInheriting())
    return true;

  // Check if any parameters that have `nonisolated(nonsending)` attribute.
  if (auto *PL = VD->getParameterList()) {
    if (llvm::any_of(*PL, [&](const ParamDecl *P) {
          return hasCallerIsolatedAttr(P->getTypeRepr());
        }))
      return true;
  }

  // Check if result type has explicit `nonisolated(nonsending)` attribute.
  if (hasCallerIsolatedAttr(VD->getResultTypeRepr()))
    return true;

  return false;
}

static bool usesFeatureNonexhaustiveAttribute(Decl *decl) {
  return decl->getAttrs().hasAttribute<NonexhaustiveAttr>();
}

static bool usesFeatureAlwaysInheritActorContext(Decl *decl) {
  auto *VD = dyn_cast<ValueDecl>(decl);
  if (!VD)
    return false;

  if (auto *PL = VD->getParameterList()) {
    return llvm::any_of(*PL, [&](const ParamDecl *P) {
      auto *attr = P->getAttrs().getAttribute<InheritActorContextAttr>();
      return attr && attr->isAlways();
    });
  }

  return false;
}

static bool usesFeatureDefaultIsolationPerFile(Decl *D) {
  return isa<UsingDecl>(D);
}

UNINTERESTING_FEATURE(BuiltinSelect)
UNINTERESTING_FEATURE(BuiltinInterleave)
UNINTERESTING_FEATURE(BuiltinVectorsExternC)
UNINTERESTING_FEATURE(AddressOfProperty2)
UNINTERESTING_FEATURE(ImmutableWeakCaptures)

// ----------------------------------------------------------------------------
// MARK: - FeatureSet
// ----------------------------------------------------------------------------

void FeatureSet::collectRequiredFeature(Feature feature,
                                        InsertOrRemove operation) {
  required.insertOrRemove(feature, operation == Insert);
}

void FeatureSet::collectSuppressibleFeature(Feature feature,
                                            InsertOrRemove operation) {
  suppressible.insertOrRemove(Feature::getNumFeatures() - size_t(feature),
                              operation == Insert);
}

static bool hasFeatureSuppressionAttribute(Decl *decl, StringRef featureName,
                                           bool inverted) {
  auto attr = decl->getAttrs().getAttribute<AllowFeatureSuppressionAttr>();
  if (!attr)
    return false;

  if (attr->getInverted() != inverted)
    return false;

  for (auto suppressedFeature : attr->getSuppressedFeatures()) {
    if (suppressedFeature.is(featureName))
      return true;
  }

  return false;
}

// These functions are only used when there suppressible language features
// defined, so suppress warnings about them being unused to avoid spam when
// there are none.
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused"
static bool disallowFeatureSuppression(StringRef featureName, Decl *decl) {
  return hasFeatureSuppressionAttribute(decl, featureName, true);
}

static bool allowFeatureSuppression(StringRef featureName, Decl *decl) {
  return hasFeatureSuppressionAttribute(decl, featureName, false);
}
#pragma clang diagnostic pop

/// Go through all the features used by the given declaration and
/// either add or remove them to this set.
void FeatureSet::collectFeaturesUsed(Decl *decl, InsertOrRemove operation) {
  // Count feature usage in an ABI decl as feature usage by the API, not itself,
  // since we can't use `#if` inside an @abi attribute.
  Decl *abiDecl = nullptr;
  if (auto abiAttr = getABIAttr(decl)) {
    abiDecl = abiAttr->abiDecl;
  }

#define CHECK(Function) (Function(decl) || (abiDecl && Function(abiDecl)))
#define CHECK_ARG(Function, Arg) (Function(Arg, decl) || (abiDecl && Function(Arg, abiDecl)))

  // Go through each of the features, checking whether the
  // declaration uses that feature.
#define LANGUAGE_FEATURE(FeatureName, SENumber, Description)                   \
  if (CHECK(usesFeature##FeatureName))                                         \
    collectRequiredFeature(Feature::FeatureName, operation);
#define SUPPRESSIBLE_LANGUAGE_FEATURE(FeatureName, SENumber, Description)      \
  if (CHECK(usesFeature##FeatureName)) {                                       \
    if (CHECK_ARG(disallowFeatureSuppression, #FeatureName))                   \
      collectRequiredFeature(Feature::FeatureName, operation);                 \
    else                                                                       \
      collectSuppressibleFeature(Feature::FeatureName, operation);             \
  }
#define CONDITIONALLY_SUPPRESSIBLE_LANGUAGE_FEATURE(FeatureName, SENumber, Description)      \
  if (CHECK(usesFeature##FeatureName)) {                                       \
    if (CHECK_ARG(allowFeatureSuppression, #FeatureName))                      \
      collectSuppressibleFeature(Feature::FeatureName, operation);             \
    else                                                                       \
      collectRequiredFeature(Feature::FeatureName, operation);                 \
  }
#include "swift/Basic/Features.def"
#undef CHECK
#undef CHECK_ARG
}

FeatureSet swift::getUniqueFeaturesUsed(Decl *decl) {
  // Add all the features used by this declaration.
  FeatureSet features;
  features.collectFeaturesUsed(decl, FeatureSet::Insert);

  // Remove all the features used by all enclosing declarations.
  Decl *enclosingDecl = decl;
  while (!features.empty()) {
    // If we were in an @abi attribute, collect from the API counterpart.
    auto abiRole = ABIRoleInfo(enclosingDecl);
    if (!abiRole.providesAPI() && abiRole.getCounterpart())
      enclosingDecl = abiRole.getCounterpart();
    // Find the next outermost enclosing declaration.
    else if (auto accessor = dyn_cast<AccessorDecl>(enclosingDecl))
      enclosingDecl = accessor->getStorage();
    else
      enclosingDecl = enclosingDecl->getDeclContext()->getAsDecl();
    if (!enclosingDecl)
      break;

    features.collectFeaturesUsed(enclosingDecl, FeatureSet::Remove);
  }

  return features;
}
