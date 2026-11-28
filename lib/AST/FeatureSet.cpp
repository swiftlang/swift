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

#include "swift/AST/ASTPrinter.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/AvailabilityQuery.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DeclExportabilityVisitor.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/GenericParamList.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/TypeDeclFinder.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/STLExtras.h"

using namespace swift;

/// Calls \p callback for each type in each requirement provided by
/// \p source.
///
/// @returns true after short-circuiting if the callback returned true for
///          any of the types
static bool
forAllRequirementTypes(WhereClauseOwner &&source,
                       llvm::function_ref<bool(Type, TypeRepr *)> callback) {
  return std::move(source).visitRequirements(
      TypeResolutionStage::Interface,
      [&](const Requirement &req, RequirementRepr *reqRepr) {
        switch (req.getKind()) {
        case RequirementKind::SameShape:
        case RequirementKind::Conformance:
        case RequirementKind::SameType:
        case RequirementKind::Superclass:
          if (callback(req.getFirstType(),
                       RequirementRepr::getFirstTypeRepr(reqRepr)))
            return true;

          if (callback(req.getSecondType(),
                       RequirementRepr::getSecondTypeRepr(reqRepr)))
            return true;

          break;

        case RequirementKind::Layout:
          if (callback(req.getFirstType(),
                       RequirementRepr::getFirstTypeRepr(reqRepr)))
            return true;
          break;
        }
        return false;
      });
}

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

// Does the where-clause use a type for which the given predicate returns true?
static bool usesTypeMatching(WhereClauseOwner &&source,
                             llvm::function_ref<bool(Type)> fn) {
  return forAllRequirementTypes(
      std::move(source),
      [&](Type ty, TypeRepr *_unused_) { return ty.findIf(fn); });
}

/// Determines whether \p root either uses \p feature directly, or needs to be
/// dropped from a module interface because some other declaration it depends
/// on uses \p feature directly. Works only for features supported by
/// \c IndirectFeatureUseCheckRequest::usesFeatureDirectly() .
///
/// This helper accounts for the fact that a protocol declaration is unsafe to
/// use if one of its requirements has been dropped and a frozen type is unsafe
/// to use if part of its storage has been dropped. It also accounts for
/// uses of types that have been dropped.
static bool usesFeatureIndirectly(Decl *root, Feature feature) {
  IndirectFeatureUseCheckRequest rootReq{feature, root};
  return evaluateOrFatal(root->getASTContext().evaluator, rootReq);
}

static bool isExposedToClang(const Decl *decl) {
  if (auto *VD = dyn_cast<ValueDecl>(decl))
    if (VD->isObjC())
      return true;
  if (auto *AFD = dyn_cast<AbstractFunctionDecl>(decl))
    if (AFD->getCDeclKind().has_value())
      return true;
  return false;
}

bool IndirectFeatureUseCheckRequest::usesFeatureDirectly(Feature feature,
                                                          Decl *decl) const {
  switch (feature) {
  case Feature::ObjCAttrAllowsCArrays:
    return isExposedToClang(decl) && usesTypeMatching(decl, [](Type type) {
      return type->isStructurallyCArray(/*assumingFeatureAllowed=*/true);
    });

  default:
    ABORT([&](auto &out) {
      out << "IndirectFeatureUseCheckRequest for unsupported feature: "
          << feature.getName() << "\n";
    });
  }
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
UNINTERESTING_FEATURE(NoncopyableCasting)
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
UNINTERESTING_FEATURE(DeprecateCompatMemberwiseInit)
UNINTERESTING_FEATURE(ReferenceBindings)
UNINTERESTING_FEATURE(BuiltinModule)
UNINTERESTING_FEATURE(RegionBasedIsolation)
UNINTERESTING_FEATURE(PlaygroundExtendedCallbacks)
UNINTERESTING_FEATURE(ThenStatements)
UNINTERESTING_FEATURE(DoExpressions)
UNINTERESTING_FEATURE(ForExpressions)
UNINTERESTING_FEATURE(ImplicitLastExprResults)
UNINTERESTING_FEATURE(RawLayout)
UNINTERESTING_FEATURE(Embedded)
UNINTERESTING_FEATURE(Volatile)
UNINTERESTING_FEATURE(SuppressedAssociatedTypes)
UNINTERESTING_FEATURE(SuppressedAssociatedTypesWithDefaults)
UNINTERESTING_FEATURE(SourceWarningControl)
UNINTERESTING_FEATURE(StructLetDestructuring)
UNINTERESTING_FEATURE(MacrosOnImports)
UNINTERESTING_FEATURE(NonisolatedNonsendingByDefault)
UNINTERESTING_FEATURE(KeyPathWithMethodMembers)
UNINTERESTING_FEATURE(ImportMacroAliases)
UNINTERESTING_FEATURE(NoExplicitNonIsolated)
UNINTERESTING_FEATURE(EmbeddedDynamicExclusivity)
UNINTERESTING_FEATURE(TypedAllocation)
UNINTERESTING_FEATURE(BuiltinAllocRawTyped)
UNINTERESTING_FEATURE(BuiltinTypedAllocationID)
UNINTERESTING_FEATURE(MutateAndConsumeInDeinit)
UNINTERESTING_FEATURE(Cells)

bool swift::usesFeatureCChar32IsUInt32(const Decl *decl) {
  auto *TAD = dyn_cast<TypeAliasDecl>(decl);
  if (!TAD || !TAD->getName().is("CChar32"))
    return false;
  if (!TAD->getDeclContext()->isModuleScopeContext() ||
      !TAD->getModuleContext()->isStdlibModule())
    return false;
  auto underlying = TAD->getUnderlyingType();
  return underlying && !underlying->isUnicodeScalar();
}

static bool usesFeatureSubscriptParametersWithOwnership(Decl *decl) {
  auto *SD = dyn_cast<SubscriptDecl>(decl);
  if (!SD)
    return false;

  return llvm::any_of(*SD->getIndices(), [](const ParamDecl *index) {
    return index->getSpecifier() != ParamSpecifier::Default;
  });
}

static bool usesFeatureUnderscoreOwned(Decl *D) {
  return D->getAttrs().hasAttribute<OwnedAttr>();
}

// TODO: Return true for inlinable function bodies with module selectors in them
UNINTERESTING_FEATURE(ModuleSelector)

static bool usesFeatureInlineArrayTypeSugar(Decl *D) {
  return usesTypeMatching(D, [&](Type ty) {
    return isa<InlineArrayType>(ty.getPointer());
  });
}

UNINTERESTING_FEATURE(StaticExclusiveOnly)
UNINTERESTING_FEATURE(ManualOwnership)
UNINTERESTING_FEATURE(ExtractConstantsFromMembers)
UNINTERESTING_FEATURE(SameElementRequirements)
UNINTERESTING_FEATURE(SendingArgsAndResults)
UNINTERESTING_FEATURE(CheckImplementationOnly)
UNINTERESTING_FEATURE(CheckImplementationOnlyStrict)
UNINTERESTING_FEATURE(SerializeAbstractTypeLayoutForHiddenTypes)
UNINTERESTING_FEATURE(EnforceSPIOperatorGroup)

static bool usesFeatureCAttribute(Decl *decl) {
  for (auto attr : decl->getAttrs()) {
    if (auto cdeclAttr = dyn_cast<CDeclAttr>(attr))
      if (!cdeclAttr->Underscored)
        return true;
  }

  return false;
}

static bool usesFeatureTargetAttribute(Decl *decl) {
  return decl->getAttrs().hasAttribute<TargetAttr>();
}

static bool usesFeatureTailAllocatedAttribute(Decl *decl) {
  return decl->getAttrs().hasAttribute<TailAllocatedAttr>();
}

static bool usesFeatureExtendedSpecializedAttribute(Decl *decl) {
  for (auto *attr : decl->getAttrs().getAttributes<SpecializedAttr>())
    // Exported functions and types, and non-exported types.
    if (attr->isExported() || isa<NominalTypeDecl>(decl))
      return true;
  return false;
}

static bool findLifetimeAttr(Decl *decl, bool findUnderscored) {
  auto hasLifetimeAttr = [&](Decl *decl) {
    if (!decl->getAttrs().hasAttribute<LifetimeAttr>()) {
      return false;
    }
    // Since we ban mixing @lifetime and @_lifetime on the same decl, checking
    // any one LifetimeAttr on the decl is sufficient.
    // FIXME: Implement the ban.
    if (findUnderscored) {
      return decl->getAttrs().getAttribute<LifetimeAttr>()->isUnderscored();
    }
    return !decl->getAttrs().getAttribute<LifetimeAttr>()->isUnderscored();
  };

  // A property or subscript may carry the attribute on its accessors.
  if (auto *storage = dyn_cast<AbstractStorageDecl>(decl))
    return hasLifetimeAttr(storage) ||
           llvm::any_of(storage->getAllAccessors(), hasLifetimeAttr);
  return hasLifetimeAttr(decl);
}

static bool usesFeatureLifetimeDependence(Decl *decl) {
  if (findLifetimeAttr(decl, /*findUnderscored*/ false)) {
    return true;
  }

  // Guard inferred lifetime dependencies with LifetimeDependence if it was
  // enabled.
  if (!decl->getASTContext().LangOpts.hasFeature(Feature::LifetimeDependence)) {
    return false;
  }

  // Check for inferred lifetime dependencies
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
  if (findLifetimeAttr(decl, /*findUnderscored*/ true)) {
    return true;
  }

  // Guard inferred lifetime dependencies with Lifetimes if it was enabled.
  if (!decl->getASTContext().LangOpts.hasFeature(Feature::Lifetimes)) {
    return false;
  }

  // Check for inferred lifetime dependencies
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

static PreInverseGenericsAttr *getPreInverseGenericsExcept(Decl *decl) {
  if (auto pbd = dyn_cast<PatternBindingDecl>(decl))
    for (auto i : range(pbd->getNumPatternEntries()))
      if (auto anchorVar = pbd->getAnchoringVarDecl(i))
        return getPreInverseGenericsExcept(anchorVar);

  return decl->getAttrs().getAttribute<PreInverseGenericsAttr>();
}

static bool usesFeaturePreInverseGenericsExcept(Decl *decl) {
  if (auto *attr = getPreInverseGenericsExcept(decl))
    return attr->hasExcept(decl);
  return false;
}

static bool hasLifetimeDependencies(Type type) {
  if (auto *aft = type->getAs<AnyFunctionType>()) {
    return aft->hasExplicitLifetimeDependencies();
  }
  return false;
}

/// Search for any types within decl with lifetime dependencies. Ignore
/// lifetimes on AbstractFunctionDecl decls, since those are supported by the
/// Lifetimes feature, not ClosureLifetimes.
static bool findClosureLifetimes(Decl *decl) {
  // Search for any Decl that uses a type with lifetime dependencies, possibly
  // restricting this to explicit dependencies.
  class ClosureLifetimesWalker : public ASTWalker {

  public:
    bool useFound = false;

    PreWalkAction walkToDeclPre(Decl *D) override {
      if (auto *afd = dyn_cast<AbstractFunctionDecl>(D)) {
        // Check the parameters and result, but not the AFD itself, since
        // lifetimes on AFDs are supported by the Lifetimes feature.
        Type resultType =
            afd->getInterfaceType()->getAs<AnyFunctionType>()->getResult();
        useFound = resultType.findIf(hasLifetimeDependencies);
        return Action::StopIf(useFound);
      }

      // Check for lifetime dependence info on param and type decls.
      if (isa<ParamDecl>(D) || isa<TypeDecl>(D)) {
        useFound = usesTypeMatching(D, hasLifetimeDependencies);
        return Action::StopIf(useFound);
      }

      // Any other Decl kinds are irrelevant.
      return Action::SkipChildren();
    }
  };

  ClosureLifetimesWalker walker;
  decl->walk(walker);
  return walker.useFound;
}

static bool usesFeatureClosureLifetimes(Decl *decl) {
  // This will find function types with lifetimes & closures with lifetimes,
  // since it walks the AST rooted at decl, checking types.
  return findClosureLifetimes(decl);
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

static bool usesFeatureBackDeployFrozenValueTypes(Decl *d) {
  return (isa<StructDecl>(d) || isa<EnumDecl>(d)) &&
         d->getAttrs().hasAttribute<BackDeployedAttr>();
}

static bool usesFeatureAddressableTypes(Decl *d) {
  if (d->getAttrs().hasAttribute<AddressableForDependenciesAttr>()) {
    return true;
  }

  return false;
}

UNINTERESTING_FEATURE(IsolatedAny2)
UNINTERESTING_FEATURE(GlobalActorIsolatedTypesUsability)
UNINTERESTING_FEATURE(ObjCImplementation)
UNINTERESTING_FEATURE(ObjCImplementationWithResilientStorage)
UNINTERESTING_FEATURE(Sensitive)
UNINTERESTING_FEATURE(DebugDescriptionMacro)
UNINTERESTING_FEATURE(ReinitializeConsumeInMultiBlockDefer)
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

static bool usesFeatureIsolatedConformances(Decl *decl) {
  // FIXME: Check conformances associated with this decl?
  return false;
}

UNINTERESTING_FEATURE(ConcurrencySyntaxSugar)

static bool usesFeatureCompileTimeValues(Decl *decl) {
  return decl->getAttrs().hasAttribute<ConstValAttr>() ||
         decl->getAttrs().hasAttribute<ConstInitializedAttr>();
}

UNINTERESTING_FEATURE(ClosureBodyMacro)
UNINTERESTING_FEATURE(BuiltinConcurrencyStackNesting)
UNINTERESTING_FEATURE(BuiltinTaskCancellationShield)
UNINTERESTING_FEATURE(BuiltinTaskCancellationScope)
UNINTERESTING_FEATURE(BuiltinTaskDeadline)
UNINTERESTING_FEATURE(BuiltinCancellationHandlerWithReason)
UNINTERESTING_FEATURE(BuiltinCreateSynchronousDiscardingTask)
UNINTERESTING_FEATURE(BuiltinAddTaskLocalValue)
UNINTERESTING_FEATURE(BuiltinContinuationNonCopyableSuccess)
UNINTERESTING_FEATURE(BuiltinSplitContinuations)
UNINTERESTING_FEATURE(CompileTimeValuesPreview)
UNINTERESTING_FEATURE(LiteralExpressions)
UNINTERESTING_FEATURE(StrictMemorySafety)
UNINTERESTING_FEATURE(LibraryEvolution)
UNINTERESTING_FEATURE(SafeInteropWrappers)
UNINTERESTING_FEATURE(SafeInteropWrappersNullAsEmptySpan)
UNINTERESTING_FEATURE(AssumeResilientCxxTypes)
UNINTERESTING_FEATURE(ImportNonPublicCxxMembers)
UNINTERESTING_FEATURE(ImportCxxMembersLazily)
UNINTERESTING_FEATURE(ImportUnsafeCxxMethodsAsAlwaysUnsafe)
UNINTERESTING_FEATURE(ForeignReferenceTypeInheritance)
UNINTERESTING_FEATURE(ForeignReferenceTypeSubclassing)
UNINTERESTING_FEATURE(CxxImplementation)
UNINTERESTING_FEATURE(GenerateBindingsForNoncopyableTypesInCXX)
UNINTERESTING_FEATURE(CoroutineAccessorsUnwindOnCallerError)
UNINTERESTING_FEATURE(AllowRuntimeSymbolDeclarations)
UNINTERESTING_FEATURE(DistributedActorResignRemoteID)

// TODO: Detect scope arguments anywhere in the declaration's interface type,
// which are printed even when elided, and explicit scope parameters.
static bool usesFeatureScopeRestrictions(Decl *decl) { return false; }

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
  case DeclKind::Subscript: {
    auto *subscript = cast<SubscriptDecl>(decl);
    return llvm::any_of(subscript->getAllAccessors(),
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

static bool usesFeatureCoroutineFunctions(Decl *decl) {
  if (auto *FD = dyn_cast<FuncDecl>(decl))
    return FD->isCoroutine() && !isa<AccessorDecl>(FD);
  
  return false;
}

UNINTERESTING_FEATURE(GeneralizedIsSameMetaTypeBuiltin)
UNINTERESTING_FEATURE(CustomAvailability)
UNINTERESTING_FEATURE(BuiltinMarkDependence)
UNINTERESTING_FEATURE(BuiltinGepProjection)

static bool usesFeatureAsyncExecutionBehaviorAttributes(Decl *decl) {
  // Explicit `@concurrent` attribute on the declaration.
  if (decl->getAttrs().hasAttribute<ConcurrentAttr>())
    return true;

  // Explicit `nonisolated(nonsending)` attribute on the declaration.
  if (auto *nonisolated = decl->getAttrs().getAttribute<NonisolatedAttr>()) {
    if (nonisolated->isNonSending())
      return true;
  }

  auto hasNonisolatedNonsendingAttr = [](TypeRepr *R) {
    if (!R)
      return false;

    return R->findIf([](TypeRepr *repr) {
      if (isa<NonisolatedNonsendingTypeRepr>(repr))
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
  if (getActorIsolation(VD).isNonisolatedNonsending())
    return true;

  // Check if any parameters that have `nonisolated(nonsending)` attribute.
  if (auto *PL = VD->getParameterList()) {
    if (llvm::any_of(*PL, [&](const ParamDecl *P) {
          return hasNonisolatedNonsendingAttr(P->getTypeRepr());
        }))
      return true;
  }

  // Check if result type has explicit `nonisolated(nonsending)` attribute.
  if (hasNonisolatedNonsendingAttr(VD->getResultTypeRepr()))
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

UNINTERESTING_FEATURE(BuiltinSelect)
UNINTERESTING_FEATURE(BuiltinInterleave)
UNINTERESTING_FEATURE(BuiltinVectorsExternC)
UNINTERESTING_FEATURE(AddressOfProperty2)
UNINTERESTING_FEATURE(ImmutableWeakCaptures)
UNINTERESTING_FEATURE(BorrowingForLoop)

static bool usesFeatureBorrowAndMutateAccessors(Decl *decl) {
  auto accessorDeclUsesFeatureBorrowAndMutateAccessors =
      [](AccessorDecl *accessor) {
        return requiresFeatureBorrowAndMutateAccessors(
            accessor->getAccessorKind());
      };
  switch (decl->getKind()) {
  case DeclKind::Var: {
    auto *var = cast<VarDecl>(decl);
    return llvm::any_of(var->getAllAccessors(),
                        accessorDeclUsesFeatureBorrowAndMutateAccessors);
  }
  case DeclKind::Accessor: {
    auto *accessor = cast<AccessorDecl>(decl);
    return accessorDeclUsesFeatureBorrowAndMutateAccessors(accessor);
  }
  default:
    return false;
  }
}

static bool usesFeatureInlineAlways(Decl *decl) {
  if (auto *inlineAttr = decl->getAttrs().getAttribute<InlineAttr>()) {
    return inlineAttr->getKind() == InlineKind::Always;
  }
  return false;
}

static bool usesFeatureAlwaysUnsafeAttribute(Decl *decl) {
  if (auto *unsafeAttr = decl->getAttrs().getAttribute<UnsafeAttr>()) {
    return unsafeAttr->isAlways();
  }
  return false;
}

static bool usesFeatureAvailabilityOSVersionQueryKind(Decl *decl) {
  for (auto *attr : decl->getAttrs().getAttributes<SemanticsAttr>()) {
    if (parseOSVersionQuerySemanticsAttr(attr->Value))
      return true;
  }
  return false;
}

/// Whether \p sig suppresses Deinitable for a type that \p parentSig, the
/// signature of the enclosing context, doesn't already suppress it for.
static bool hasOwnDeinitableInverse(GenericSignature sig,
                                    GenericSignature parentSig) {
  auto getSubjects = [](GenericSignature sig) {
    SmallVector<CanType, 2> subjects;
    if (!sig)
      return subjects;
    SmallVector<Requirement, 2> reqs;
    SmallVector<InverseRequirement, 2> inverses;
    sig->getRequirementsWithInverses(reqs, inverses);
    for (auto inverse : inverses)
      if (inverse.getKind() == InvertibleProtocolKind::Deinitable)
        subjects.push_back(inverse.subject->getCanonicalType());
    return subjects;
  };

  auto subjects = getSubjects(sig);
  if (subjects.empty())
    return false;

  auto parentSubjects = getSubjects(parentSig);
  return llvm::any_of(subjects, [&](CanType subject) {
    return !llvm::is_contained(parentSubjects, subject);
  });
}

/// Whether the declaration suppresses `Deinitable` in its inheritance clause,
/// its generic signature, its opaque result type, or the associated types of
/// its requirement signature, or extends a type that does.
static bool suppressesDeinitable(Decl *decl) {
  // Only the standard library and modules with NondeinitableTypes can
  // suppress Deinitable.
  if (!decl->getModuleContext()->isStdlibModule() &&
      !decl->getASTContext().LangOpts.hasFeature(Feature::NondeinitableTypes))
    return false;

  // An extension of a type that suppresses Deinitable. An unconstrained
  // extension of a type with a `~Deinitable` generic parameter has a signature
  // in which the parameter is Deinitable.
  if (auto *ext = dyn_cast<ExtensionDecl>(decl)) {
    auto *nominal = ext->getExtendedNominal();
    if (nominal && !isa<ProtocolDecl>(nominal) && suppressesDeinitable(nominal))
      return true;
  }

  // A struct or enum that suppresses Deinitable.
  auto *nominal = dyn_cast<NominalTypeDecl>(decl);
  if (nominal && !isa<ProtocolDecl>(nominal)) {
    InvertibleProtocolSet inverses;
    bool anyObject = false;
    (void)getDirectlyInheritedNominalTypeDecls(nominal, inverses, anyObject);
    if (inverses.contains(InvertibleProtocolKind::Deinitable))
      return true;
  }

  // A generic signature that suppresses Deinitable. Inverses that are
  // inherited from the enclosing context, like those of the `Self` parameter
  // of a protocol requirement, belong to the enclosing declaration. The
  // inverses of a protocol's own `Self` come from its inheritance clause.
  auto *genericContext = decl->getAsGenericContext();
  if (genericContext && !isa<ProtocolDecl>(decl)) {
    if (hasOwnDeinitableInverse(
            genericContext->getGenericSignature(),
            decl->getDeclContext()->getGenericSignatureOfContext()))
      return true;
  }

  // An opaque result type that suppresses Deinitable.
  if (auto *value = dyn_cast<ValueDecl>(decl)) {
    if (auto *opaque = value->getOpaqueResultTypeDecl()) {
      if (hasOwnDeinitableInverse(
              opaque->getOpaqueInterfaceGenericSignature(),
              value->getInnermostDeclContext()->getGenericSignatureOfContext()))
        return true;
    }
  }

  // A protocol with an associated type that suppresses Deinitable.
  if (auto *proto = dyn_cast<ProtocolDecl>(decl)) {
    SmallVector<Requirement, 2> reqs;
    SmallVector<InverseRequirement, 2> inverses;
    proto->getRequirementSignature().getRequirementsWithInverses(proto, reqs,
                                                                 inverses);
    auto selfType = proto->getSelfInterfaceType();
    for (auto inverse : inverses) {
      if (inverse.getKind() == InvertibleProtocolKind::Deinitable &&
          !inverse.subject->isEqual(selfType))
        return true;
    }
  }

  return false;
}

static bool usesFeatureDeinitableProtocol(Decl *decl) {
  auto *proto = dyn_cast<ProtocolDecl>(decl);
  if (!proto)
    return suppressesDeinitable(decl);

  if (proto->isSpecificProtocol(KnownProtocolKind::Deinitable))
    return true;

  // The printer never prints Sendable's inverses.
  if (proto->isSpecificProtocol(KnownProtocolKind::Sendable))
    return false;

  if (suppressesDeinitable(proto))
    return true;

  // Copyable inherits Deinitable, and a protocol may suppress it.
  InheritedTypes inherited(proto);
  return llvm::any_of(inherited.getIndices(), [&](unsigned i) {
    auto type = inherited.getResolvedType(i);
    if (!type)
      return false;
    if (auto *inheritedProto = type->getAs<ProtocolType>())
      if (inheritedProto->getDecl()->isSpecificProtocol(
              KnownProtocolKind::Deinitable))
        return true;
    if (auto *pct = type->getCanonicalType()->getAs<ProtocolCompositionType>())
      return pct->getInverses().contains(InvertibleProtocolKind::Deinitable);
    return false;
  });
}

UNINTERESTING_FEATURE(SwiftRuntimeAvailability)
UNINTERESTING_FEATURE(StandaloneSwiftAvailability)

static bool usesFeatureTildeSendable(Decl *decl) {
  auto *TD = dyn_cast<TypeDecl>(decl);
  if (!TD)
    return false;

  return llvm::any_of(
      TD->getInherited().getEntries(), [&decl](const auto &entry) {
        if (!entry.isSuppressed())
          return false;

        auto T = entry.getType();
        if (!T)
          return false;

        auto &C = decl->getASTContext();
        return C.getProtocol(KnownProtocolKind::Sendable) == T->getAnyNominal();
      });
}

static bool usesFeatureReparenting(Decl *decl) {
  auto reparentableProto = [](Type ty) {
    if (!ty)
      return false;

    if (auto protoTy = ty->getAs<ProtocolType>()) {
      if (protoTy->getDecl()->getAttrs().hasAttribute<ReparentableAttr>())
        return true;
    }
    return false;
  };

  // Search the decl or extension for mentions of a reparentable protocol.
  if (isa<ValueDecl>(decl)) {
    if (usesTypeMatching(decl, reparentableProto))
      return true;
  } else if (auto *gc = decl->getAsGenericContext()) {
    bool foundInWhereClause = usesTypeMatching(
        WhereClauseOwner(const_cast<GenericContext *>(gc)), reparentableProto);

    if (foundInWhereClause)
      return true;
  }

  // Check the inheritance clause for mentions of a reparentable protocol.
  InheritedTypes inherited(decl);
  for (auto const &entry : inherited.getEntries()) {
    if (entry.isReparented())
      return true;

    if (Type ty = entry.getType())
      if (ty.findIf(reparentableProto))
        return true;
  }

  // Check if this decl itself is a reparentable protocol (of extension of).
  if (auto ext = dyn_cast<ExtensionDecl>(decl)) {
    if (auto *nominal = ext->getExtendedNominal())
      decl = nominal;
  }

  if (auto proto = dyn_cast<ProtocolDecl>(decl)) {
    if (proto->getAttrs().hasAttribute<ReparentableAttr>())
      return true;
  }

  return false;
}

UNINTERESTING_FEATURE(StrictAccessControl)
UNINTERESTING_FEATURE(BorrowingSequence)

UNINTERESTING_FEATURE(DeriveConformancesViaMacros)

static bool usesFeatureBorrowInout(Decl *decl) {
  auto &ctx = decl->getASTContext();

  if (auto ext = dyn_cast<ExtensionDecl>(decl)) {
    decl = ext->getExtendedNominal();
  }

  return decl == ctx.getRefDecl() || decl == ctx.getMutableRefDecl();
}

UNINTERESTING_FEATURE(BuiltinDereferenceable)

static bool usesFeatureOnceFunctions(Decl *D) {
  auto &ctx = D->getASTContext();
  if (!ctx.LangOpts.hasFeature(Feature::OnceFunctions))
    return false;

  std::function<bool(Type)> hasCalled = [](Type T) {
    if (auto F = dyn_cast<AnyFunctionType>(T.getPointer()))
      return F->hasCalledAtMostOnceSemantics();
    return false;
  };

  if (auto *TA = dyn_cast<TypeAliasDecl>(D)) {
    if (auto type = TA->getUnderlyingType()) {
      return type.findIf(hasCalled);
    }
    return false;
  }

  if (auto *PBD = dyn_cast<PatternBindingDecl>(D)) {
    for (unsigned i = 0, n = PBD->getNumPatternEntries(); i != n; ++i) {
      auto P = PBD->getCheckedPattern(i);
      if (!P)
        continue;

      bool usesCalled = false;
      P->forEachVariable([&usesCalled](auto *V) {
        usesCalled |= usesFeatureOnceFunctions(V);
      });

      if (usesCalled)
        return true;
    }

    return false;
  }

  return usesTypeMatching(D, hasCalled);
}

UNINTERESTING_FEATURE(BuiltinExtendVectorLanes)
UNINTERESTING_FEATURE(BuiltinRelaxedFP)

static bool usesFeatureNondeinitableTypes(Decl *decl) {
  auto &ctx = decl->getASTContext();
  if (!ctx.LangOpts.hasFeature(Feature::NondeinitableTypes))
    return false;

  return suppressesDeinitable(decl);
}

UNINTERESTING_FEATURE(ModernImportedCArrays)
UNINTERESTING_FEATURE(ModernImportedCArraysOnly)

static bool usesFeatureObjCAttrAllowsCArrays(Decl *decl) {
  return usesFeatureIndirectly(decl, Feature::ObjCAttrAllowsCArrays);
}

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
  // Compilers without the Deinitable protocol must not see it at all. Only
  // protocols can be printed without their references to it.
  if (featureName == "DeinitableProtocol") {
    auto *proto = dyn_cast<ProtocolDecl>(decl);
    if (!proto || proto->isSpecificProtocol(KnownProtocolKind::Deinitable))
      return true;
  }

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

// ----------------------------------------------------------------------------
// MARK: - Machinery for usesFeatureIndirectly()
// ----------------------------------------------------------------------------

bool IndirectFeatureUseCheckRequest::evaluate(
    Evaluator &evaluator, Feature feature, Decl *root) const {
  if (!root || root->hasClangNode())
    return false;

  // Although the user only asked about `root`, we cannot evaluate `root`
  // without evaluating many other nodes. Moreover, there can be cycles in the
  // graph, so we cannot simply recurse to evaluate them. We will therefore
  // gather all the nodes we need to evaluate, compute the feature check for
  // each one, and manually cache all those results so we don't repeat work.

  /// All of the nodes whose feature use is unknown and will need to be
  /// evaluated and cached.
  llvm::SmallSetVector<Decl *, 4> nodesToEvaluate;

  /// All of the nodes which we have determined use `feature`, including those
  /// evaluated during previous requests and cached.
  llvm::SmallPtrSet<Decl *, 8> nodesMarkedAsUser;

  /// Given a node, returns all the nodes that reference it (and that
  /// therefore must be marked as users if it is).
  llvm::DenseMap<Decl *, SmallVector<Decl *, 2>> nodesReferencingNode;

  // Find the declarations reachable from `root`, populating `nodesToEvaluate`
  // with those which have no cache entry and `nodesMarkedAsUser` with those
  // whose cache entry says they use the feature. Also build
  // `nodesReferencingNode` in the process.
  gatherNodesToEvaluate(feature, root, nodesToEvaluate, nodesReferencingNode,
                        nodesMarkedAsUser);

  // Add each node from `nodesToEvaluate` that uses the feature (directly or
  // indirectly) to `nodesMarkedAsUser`.
  evaluateAllNodes(feature, nodesToEvaluate, nodesReferencingNode,
                   nodesMarkedAsUser);

  // Cache everything *but* `root` that we just computed.
  for (auto *node : nodesToEvaluate)
    if (node != root)
      setCachedEvaluation(feature, node, nodesMarkedAsUser.count(node));

  // Return the evaluation for `root`.
  return nodesMarkedAsUser.count(root);
}

void IndirectFeatureUseCheckRequest::gatherNodesToEvaluate(
    Feature feature, Decl *root,
    llvm::SmallSetVector<Decl *, 4> &nodesToEvaluate,
    IndirectFeatureUseCheckRequest::NodesReferencingMap &nodesReferencingNode,
    llvm::SmallPtrSetImpl<Decl *> &nodesMarkedAsUser) const {
  auto &ctx = root->getASTContext();
  SmallVector<Decl *, 8> worklist{root};

  while (!worklist.empty()) {
    auto *node = worklist.pop_back_val();

    // If we've cached a result for this node in a previous request, pull in the
    // previous result instead of re-evaluating it.
    if (auto known = getCachedEvaluation(feature, node)) {
      if (*known)
        nodesMarkedAsUser.insert(node);
      continue;
    }

    // Schedule for evaluation, unless we already did.
    if (!nodesToEvaluate.insert(node))
      continue;

    // Record that `node` references each of its dependencies.
    auto deps = evaluateOrDefault(
        ctx.evaluator, ModuleInterfaceDependenciesRequest{node}, {});
    for (auto *dep : deps)
      nodesReferencingNode[dep].push_back(node);

    // Schedule their own dependencies to be gathered.
    worklist.append(deps.begin(), deps.end());
  }
}

void IndirectFeatureUseCheckRequest::evaluateAllNodes(
    Feature feature, const llvm::SmallSetVector<Decl *, 4> &nodesToEvaluate,
    const IndirectFeatureUseCheckRequest::NodesReferencingMap
        &nodesReferencingNode,
    llvm::SmallPtrSetImpl<Decl *> &nodesMarkedAsUser) const {
  // Mark nodes which *directly* use the feature.
  for (auto *node : nodesToEvaluate)
    if (usesFeatureDirectly(feature, node))
      nodesMarkedAsUser.insert(node);

  // Starting from the nodes we've identified as users so far, mark every node
  // that references them as a user.
  SmallVector<Decl *, 4> worklist(nodesMarkedAsUser.begin(),
                                  nodesMarkedAsUser.end());
  while (!worklist.empty()) {
    auto *node = worklist.pop_back_val();

    auto referencers = nodesReferencingNode.find(node);
    if (referencers == nodesReferencingNode.end())
      continue;
    for (auto *referencer : referencers->second)
      if (nodesMarkedAsUser.insert(referencer).second)
        worklist.push_back(referencer);
  }
}

std::optional<bool>
IndirectFeatureUseCheckRequest::getCachedEvaluation(Feature feature,
                                                    Decl *decl) const {
  auto &evaluator = decl->getASTContext().evaluator;
  IndirectFeatureUseCheckRequest declReq{feature, decl};

  if (!evaluator.hasCachedResult(declReq))
    return std::nullopt;

  return evaluateOrFatal(evaluator, declReq);
}

void IndirectFeatureUseCheckRequest::setCachedEvaluation(Feature feature,
                                                         Decl *decl,
                                                         bool result) const {
  auto &evaluator = decl->getASTContext().evaluator;
  IndirectFeatureUseCheckRequest declReq{feature, decl};
  evaluator.cacheOutput(declReq, std::move(result));
}

ArrayRef<Decl *>
ModuleInterfaceDependenciesRequest::evaluate(Evaluator &evaluator,
                                             Decl *decl) const {
  if (decl->hasClangNode())
    return {};

  class Collector : public DeclVisitor<Collector>, public TypeDeclFinder {
    SmallVector<Decl *, 8> dependencies;
    llvm::SmallPtrSet<const Decl *, 8> seen;

    /// Can \p decl be seen in the interface of its module?
    static bool isPrinted(const Decl *decl) {
      // FIXME: Share more logic with the real source of truth
      //        (`ShouldPrintForModuleInterface` in ASTPrinter.cpp)

      if (auto ed = dyn_cast_or_null<ExtensionDecl>(decl))
        return isPrinted(ed->getExtendedNominal());
      if (auto vd = dyn_cast_or_null<ValueDecl>(decl))
        return DeclExportabilityVisitor().visit(vd) ||
                   isPrintedInModuleInterfaceDespiteAccessLevel(vd);

      return true;
    }

    void addDependency(const Decl *decl) {
      // Clang declarations aren't printed in module interfaces.
      if (!decl || decl->hasClangNode() || !seen.insert(decl).second)
        return;

      if (isPrinted(decl))
        dependencies.push_back(const_cast<Decl *>(decl));
      else
        // A declaration that isn't part of the interface can't be dropped from
        // it, but whatever it inherits from can still be.
        addInheritedDependencies(decl);
    }

    /// Adds the nominal types that \p type is made of.
    void addTypeDependencies(Type type) {
      if (!type.isNull())
        type.walk(*this);
    }

    /// Adds the types that the where clause \p owner spells out mention.
    void addWhereClauseDependencies(WhereClauseOwner &&owner) {
      (void)usesTypeMatching(std::move(owner), [&](Type type) {
        addTypeDependencies(type);
        return false;
      });
    }

    /// Adds the types named by \p decl's custom attributes.
    void addCustomAttributeDependencies(const Decl *decl) {
      for (auto *attr : decl->getAttrs().getAttributes<CustomAttr>()) {
        Type type = decl->getResolvedCustomAttrType(
                              const_cast<CustomAttr *>(attr));
        if (!type)
          continue;

        addDependency(type->getAnyNominal());
        addTypeDependencies(type);
      }
    }

    /// Adds the types that the requirements of \p opaque's interface signature
    /// (and so its constraints) mention.
    void addOpaqueTypeDependencies(const OpaqueTypeDecl *opaque) {
      for (const auto &req :
               opaque->getOpaqueInterfaceGenericSignature().getRequirements()) {
        addTypeDependencies(req.getFirstType());
        if (req.getKind() != RequirementKind::Layout)
          addTypeDependencies(req.getSecondType());
      }
    }

    /// Adds the types in the inheritance clause of \p decl.
    void addInheritedDependencies(const Decl *decl) {
      InheritedTypes inherited(decl);
      for (auto i : inherited.getIndices())
        addTypeDependencies(inherited.getResolvedType(i));
    }

    /// Adds the types that the generic requirements \p gc spells out mention.
    ///
    /// You only need to call this on decls whose generic parameters do \em not
    /// appear in their interface type (e.g. generic classes, but not generic
    /// functions).
    void addGenericRequirementDependencies(const GenericContext *gc) {
      if (auto *params = gc->getGenericParams())
        for (auto *param : *params)
          addInheritedDependencies(param);

      addWhereClauseDependencies(WhereClauseOwner(
                                     const_cast<GenericContext *>(gc)));
    }

    /// If \p ntd is non-resilient, adds the members that \p affectsLayout says
    /// determine its layout.
    void addFragileMemberDependencies(
        NominalTypeDecl *ntd, llvm::function_ref<bool(Decl *)> affectsLayout) {
      if (ntd->isFormallyResilient())
        return;
      for (auto *member : ntd->getABIMembers())
        if (affectsLayout(member))
          addDependency(member);
    }

  public:
    ArrayRef<Decl *> collect(const Decl *decl) {
      addInheritedDependencies(decl);

      if (isPrinted(decl))
        visit(const_cast<Decl *>(decl));

      return dependencies;
    }

    // TypeDeclFinder:
    Action visitNominalType(NominalType *ty) override {
      addDependency(ty->getDecl());
      return TypeWalker::Action::Continue;
    }

    Action visitTypeAliasType(TypeAliasType *ty) override {
      addDependency(ty->getDecl());
      return TypeWalker::Action::Continue;
    }

    // DeclVisitor:
    void visitDecl(Decl *decl) {}

    void visitExtensionDecl(ExtensionDecl *ext) {
      visitDecl(ext);
      addGenericRequirementDependencies(ext);

      // Drop an extension if the type it extends is dropped.
      addDependency(ext->getExtendedNominal());
    }

    void visitPatternBindingDecl(PatternBindingDecl *pbd) {
      visitDecl(pbd);

      // A pattern binding is printed as a unit, so it's dropped if any of its
      // variables are.
      // FIXME: ASTPrinter could refine the scope of feature checks.
      for (auto i : range(pbd->getNumPatternEntries()))
        pbd->getPattern(i)->forEachVariable(
            [&](VarDecl *var) { addDependency(var); });
    }

    void visitValueDecl(ValueDecl *VD) {
      visitDecl(VD);

      // Drop a decl whose interface type includes a dropped decl.
      addTypeDependencies(VD->getInterfaceType());

      // The interface type doesn't show an opaque result type's constraints.
      if (auto *opaque = VD->getOpaqueResultTypeDecl())
        addOpaqueTypeDependencies(opaque);

      addCustomAttributeDependencies(VD);
    }

    void visitAbstractFunctionDecl(AbstractFunctionDecl *AFD) {
      visitValueDecl(AFD);

      // Parameters' custom attributes are printed with the function.
      for (auto *param : *AFD->getParameters())
        addCustomAttributeDependencies(param);
    }

    void visitSubscriptDecl(SubscriptDecl *SD) {
      visitValueDecl(SD);

      // Parameters' custom attributes are printed with the function.
      for (auto *param : *SD->getIndices())
        addCustomAttributeDependencies(param);
    }

    void visitAssociatedTypeDecl(AssociatedTypeDecl *ATD) {
      visitValueDecl(ATD);

      // An associated type's default and where clause are printed with it.
      addTypeDependencies(ATD->getDefaultDefinitionType());
      addWhereClauseDependencies(WhereClauseOwner(ATD));
    }

    void visitGenericTypeDecl(GenericTypeDecl *gtd) {
      visitTypeDecl(gtd);
      addGenericRequirementDependencies(gtd);
    }

    void visitTypeAliasDecl(TypeAliasDecl *tad) {
      visitGenericTypeDecl(tad);

      // The underlying type is printed in addition to the interface type.
      addTypeDependencies(tad->getUnderlyingType());
    }

    void visitEnumDecl(EnumDecl *ed) {
      visitNominalTypeDecl(ed);

      // Drop a frozen enum with a dropped case (prevents using the wrong memory
      // layout).
      addFragileMemberDependencies(ed, [](Decl *member) {
        return isa<EnumElementDecl>(member);
      });
    }

    void visitStructDecl(StructDecl *sd) {
      visitNominalTypeDecl(sd);

      // Drop a frozen struct with a dropped stored property (prevents using the
      // wrong memory layout).
      addFragileMemberDependencies(sd, [](Decl *member) {
        if (auto var = dyn_cast<VarDecl>(member))
          return var->hasStorage() && !var->isStatic();
        return false;
      });
    }

    void visitClassDecl(ClassDecl *cd) {
      visitNominalTypeDecl(cd);

      // Drop a frozen class with a dropped stored property (prevents using the
      // wrong memory layout).
      addFragileMemberDependencies(cd, [](Decl *member) {
        if (auto var = dyn_cast<VarDecl>(member))
          return var->hasStorage() && !var->isStatic();
        return false;
      });
    }

    void visitProtocolDecl(ProtocolDecl *pd) {
      visitNominalTypeDecl(pd);

      // Drop a protocol with a dropped requirement (prevents bad conformances).
      for (auto *req : pd->getProtocolRequirements())
        addDependency(req);
    }

    void visitEnumCaseDecl(EnumCaseDecl *cd) {
      visitDecl(cd);

      // A case decl is printed as a unit, so it's dropped if any element is.
      // FIXME: ASTPrinter could refine the scope of feature checks.
      for (auto *element : cd->getElements())
        addDependency(element);
    }
  };

  return decl->getASTContext().AllocateCopy(Collector().collect(decl));
}
