//===--- FeatureSet.cpp - Language feature support --------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "FeatureSet.h"

#include "swift/AST/Decl.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/GenericParamList.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "clang/AST/DeclObjC.h"
#include "swift/Basic/Assertions.h"

using namespace swift;

/// Does the interface of this declaration use a type for which the
/// given predicate returns true?
static bool usesTypeMatching(Decl *decl, llvm::function_ref<bool(Type)> fn) {
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

static bool usesFeatureSpecializeAttributeWithAvailability(Decl *decl) {
  if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
    for (auto specialize : func->getAttrs().getAttributes<SpecializeAttr>()) {
      if (!specialize->getAvailableAttrs().empty())
        return true;
    }
  }
  return false;
}

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
UNINTERESTING_FEATURE(OneWayClosureParameters)
UNINTERESTING_FEATURE(LayoutPrespecialization)
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
UNINTERESTING_FEATURE(ParserDiagnostics)
UNINTERESTING_FEATURE(ImplicitSome)
UNINTERESTING_FEATURE(ParserASTGen)
UNINTERESTING_FEATURE(BuiltinMacros)
UNINTERESTING_FEATURE(ImportSymbolicCXXDecls)
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
UNINTERESTING_FEATURE(NonescapableTypes)
UNINTERESTING_FEATURE(StaticExclusiveOnly)
UNINTERESTING_FEATURE(ExtractConstantsFromMembers)
UNINTERESTING_FEATURE(FixedArrays)
UNINTERESTING_FEATURE(GroupActorErrors)
UNINTERESTING_FEATURE(SameElementRequirements)

static bool usesFeatureSendingArgsAndResults(Decl *decl) {
  auto isFunctionTypeWithSending = [](Type type) {
      auto fnType = type->getAs<AnyFunctionType>();
      if (!fnType)
        return false;

      if (fnType->hasExtInfo() && fnType->hasSendingResult())
        return true;

      return llvm::any_of(fnType->getParams(),
                          [](AnyFunctionType::Param param) {
                            return param.getParameterFlags().isSending();
                          });
  };
  auto declUsesFunctionTypesThatUseSending = [&](Decl *decl) {
    return usesTypeMatching(decl, isFunctionTypeWithSending);
  };

  if (auto *pd = dyn_cast<ParamDecl>(decl)) {
    if (pd->isSending()) {
      return true;
    }

    if (declUsesFunctionTypesThatUseSending(pd))
      return true;
  }

  if (auto *fDecl = dyn_cast<AbstractFunctionDecl>(decl)) {
    // First check for param decl results.
    if (llvm::any_of(fDecl->getParameters()->getArray(), [](ParamDecl *pd) {
          return usesFeatureSendingArgsAndResults(pd);
        }))
      return true;
    if (declUsesFunctionTypesThatUseSending(decl))
      return true;
  }

  // Check if we have a pattern binding decl for a function that has sending
  // parameters and results.
  if (auto *pbd = dyn_cast<PatternBindingDecl>(decl)) {
    for (auto index : range(pbd->getNumPatternEntries())) {
      auto *pattern = pbd->getPattern(index);
      if (pattern->hasType() && isFunctionTypeWithSending(pattern->getType()))
        return true;
    }
  }

  return false;
}

UNINTERESTING_FEATURE(DynamicActorIsolation)
UNINTERESTING_FEATURE(NonfrozenEnumExhaustivity)
UNINTERESTING_FEATURE(ClosureIsolation)
UNINTERESTING_FEATURE(Extern)
UNINTERESTING_FEATURE(ConsumeSelfInDeinit)

static bool usesFeatureBitwiseCopyable2(Decl *decl) {
  if (!decl->getModuleContext()->isStdlibModule()) {
    return false;
  }
  if (auto *proto = dyn_cast<ProtocolDecl>(decl)) {
    return proto->getNameStr() == "BitwiseCopyable";
  }
  if (auto *typealias = dyn_cast<TypeAliasDecl>(decl)) {
    return typealias->getNameStr() == "_BitwiseCopyable";
  }
  return false;
}

static bool usesFeatureIsolatedAny(Decl *decl) {
  return usesTypeMatching(decl, [](Type type) {
    if (auto fnType = type->getAs<AnyFunctionType>()) {
      return fnType->getIsolation().isErased();
    }
    return false;
  });
}

UNINTERESTING_FEATURE(MemberImportVisibility)
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

static bool usesFeatureAllowUnsafeAttribute(Decl *decl) {
  return decl->getAttrs().hasAttribute<UnsafeAttr>();
}

UNINTERESTING_FEATURE(WarnUnsafe)

static bool usesFeatureValueGenerics(Decl *decl) {
  auto genericContext = decl->getAsGenericContext();

  if (!genericContext || !genericContext->getGenericParams())
    return false;

  for (auto param : genericContext->getGenericParams()->getParams()) {
    if (param->isValue())
      return true;

    continue;
  }

  return false;
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
  suppressible.insertOrRemove(numFeatures() - size_t(feature),
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

static bool disallowFeatureSuppression(StringRef featureName, Decl *decl) {
  return hasFeatureSuppressionAttribute(decl, featureName, true);
}

static bool allowFeatureSuppression(StringRef featureName, Decl *decl) {
  return hasFeatureSuppressionAttribute(decl, featureName, false);
}

/// Go through all the features used by the given declaration and
/// either add or remove them to this set.
void FeatureSet::collectFeaturesUsed(Decl *decl, InsertOrRemove operation) {
  // Go through each of the features, checking whether the
  // declaration uses that feature.
#define LANGUAGE_FEATURE(FeatureName, SENumber, Description)                   \
  if (usesFeature##FeatureName(decl))                                          \
    collectRequiredFeature(Feature::FeatureName, operation);
#define SUPPRESSIBLE_LANGUAGE_FEATURE(FeatureName, SENumber, Description)      \
  if (usesFeature##FeatureName(decl)) {                                        \
    if (disallowFeatureSuppression(#FeatureName, decl))                        \
      collectRequiredFeature(Feature::FeatureName, operation);                 \
    else                                                                       \
      collectSuppressibleFeature(Feature::FeatureName, operation);             \
  }
#define CONDITIONALLY_SUPPRESSIBLE_LANGUAGE_FEATURE(FeatureName, SENumber, Description)      \
  if (usesFeature##FeatureName(decl)) {                                        \
    if (allowFeatureSuppression(#FeatureName, decl))                           \
      collectSuppressibleFeature(Feature::FeatureName, operation);             \
    else                                                                       \
      collectRequiredFeature(Feature::FeatureName, operation);                 \
  }
#include "swift/Basic/Features.def"
}

FeatureSet swift::getUniqueFeaturesUsed(Decl *decl) {
  // Add all the features used by this declaration.
  FeatureSet features;
  features.collectFeaturesUsed(decl, FeatureSet::Insert);

  // Remove all the features used by all enclosing declarations.
  Decl *enclosingDecl = decl;
  while (!features.empty()) {
    // Find the next outermost enclosing declaration.
    if (auto accessor = dyn_cast<AccessorDecl>(enclosingDecl))
      enclosingDecl = accessor->getStorage();
    else
      enclosingDecl = enclosingDecl->getDeclContext()->getAsDecl();
    if (!enclosingDecl)
      break;

    features.collectFeaturesUsed(enclosingDecl, FeatureSet::Remove);
  }

  return features;
}
