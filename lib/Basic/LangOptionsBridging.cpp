//===--- Bridging/LangOptsBridging.cpp ------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTBridging.h"

#include "swift/Basic/LangOptions.h"

using namespace swift;

bool BridgedLangOptions_hasFeature(BridgedLangOptions cLangOpts,
                                   BridgedFeature feature) {
  return cLangOpts.unbridged().hasFeature((Feature)feature);
}


unsigned BridgedLangOptions::getTargetPointerBitWidth() const {
  return unbridged().Target.isArch64Bit()   ? 64
         : unbridged().Target.isArch32Bit() ? 32
         : unbridged().Target.isArch16Bit() ? 16
                                            : 0;
}

BridgedEndianness BridgedLangOptions::getTargetEndianness() const {
  return unbridged().Target.isLittleEndian() ? EndianLittle : EndianBig;
}

bool BridgedLangOptions::getAttachCommentsToDecls() const {
  return unbridged().AttachCommentsToDecls;
}

/// Convert an array of numbers into a form we can use in Swift.
namespace {
template <typename Arr>
SwiftInt convertArray(const Arr &array, SwiftInt **cElements) {
  SwiftInt numElements = array.size();
  *cElements = (SwiftInt *)malloc(sizeof(SwiftInt) * numElements);
  for (SwiftInt i = 0; i != numElements; ++i)
    (*cElements)[i] = array[i];
  return numElements;
}
} // namespace

void deallocateIntBuffer(SwiftInt *_Nullable cComponents) { free(cComponents); }

SwiftInt
BridgedLangOptions_getLanguageVersion(BridgedLangOptions cLangOpts,
                                      SwiftInt **cComponents) {
  auto theVersion = cLangOpts.unbridged().EffectiveLanguageVersion;
  return convertArray(theVersion, cComponents);
}

SwiftInt
BridgedLangOptions_getCompilerVersion(BridgedLangOptions cLangOpts,
                                      SwiftInt **cComponents) {
  auto theVersion = version::Version::getCurrentLanguageVersion();
  return convertArray(theVersion, cComponents);
}

SwiftInt BridgedLangOptions_getTargetAtomicBitWidths(
    BridgedLangOptions cLangOpts, SwiftInt *_Nullable *_Nonnull cElements) {
  return convertArray(cLangOpts.unbridged().getAtomicBitWidthValues(),
                      cElements);
}

namespace {

/// Describe behaviors that should prevent an attribute from being shown.
///
/// This is DeclAttrBehaviors, but with irrelevent values set to zero.
enum DeclAttrBehaviorsNotShown : uint64_t {
  /// Whether this attribute is only valid when concurrency is enabled.
  ConcurrencyOnly = 0,

  /// True if multiple instances of this attribute are allowed on a single
  /// declaration.
  AllowMultipleAttributes = 0,

  /// True if this is a decl modifier - i.e., that it should not be spelled
  /// with an @.
  DeclModifier = 1ull << 2,

  /// True if this is a long attribute that should be printed on its own line.
  ///
  /// Currently has no effect on DeclModifier attributes.
  LongAttribute = 0,

  /// True if this shouldn't be serialized.
  NotSerialized = 0,

  /// True if this attribute is only valid when parsing a .sil file.
  SILOnly = 1ull << 5,

  /// The attribute should be reported by parser as unknown.
  RejectByParser = 1ull << 6,

  /// Whether client code cannot use the attribute. Hides it in code completion.
  UserInaccessible = 1ull << 7,

  /// Whether adding this attribute can break API
  APIBreakingToAdd = 0,

  /// Whether removing this attribute can break API
  APIBreakingToRemove = 0,

  /// Whether adding this attribute can break ABI
  ABIBreakingToAdd = 0,

  /// Whether removing this attribute can break ABI
  ABIBreakingToRemove = 0,

  /// The opposite of APIBreakingToAdd
  APIStableToAdd = 0,

  /// The opposite of APIBreakingToRemove
  APIStableToRemove = 0,

  /// The opposite of ABIBreakingToAdd
  ABIStableToAdd = 0,

  /// The opposite of ABIBreakingToRemove
  ABIStableToRemove = 0,

  /// Attribute should not be used in an \c \@abi attribute. Use for
  /// attributes which cannot affect mangled names, even indirectly, and
  /// which either don't affect ABI or where ABI-only declarations get their
  /// behavior from their API counterpart.
  ForbiddenInABIAttr = 0,

  /// Attribute can be used without restrictions in an \c \@abi attribute.
  /// Use for attributes which affect mangled names but otherwise don't alter
  /// the ABI, or ones where the \c ABIDeclChecker manually implements
  /// special checking logic (e.g. because several different attributes
  /// contribute to the same aspect of ABI in some complicated way).
  UnconstrainedInABIAttr = 0,

  /// Attribute can be used in an \c \@abi attribute, but must match
  /// equivalent on API decl. Use for attributes which affect both mangled
  /// names and other parts of the ABI such that the declaration can only be
  /// valid if they match.
  EquivalentInABIAttr = 0,

  /// Use for attributes which are \em only valid on declarations that cannot
  /// have an \c @abi attribute, such as \c ImportDecl .
  UnreachableInABIAttr = 0,
};

}

void BridgedLangOptions_enumerateBuildConfigurationEntries(
    BridgedLangOptions cLangOpts,
    void * _Nonnull callbackContext,
    void (* _Nonnull callback)(
        BridgedLangOptions cLangOpts, void * _Nonnull callbackContext,
        BuildConfigurationKey key, BridgedStringRef value)) {
  const LangOptions &langOpts = cLangOpts.unbridged();

  // Enumerate custom conditions.
  for (const auto &customCondition: langOpts.getCustomConditionalCompilationFlags()) {
    callback(cLangOpts, callbackContext, BCKCustomCondition,
             StringRef(customCondition));
  }

  // Enumerate features that are enabled.
#define LANGUAGE_FEATURE(FeatureName, SENumber, Description)                   \
  if (langOpts.hasFeature(Feature::FeatureName))                               \
    callback(cLangOpts, callbackContext, BCKFeature, StringRef(#FeatureName));
#include "swift/Basic/Features.def"

  // Enumerate attributes that are available.
#define DECL_ATTR(SPELLING, CLASS, REQUIREMENTS, BEHAVIORS, CODE)              \
  if ((BEHAVIORS) == 0) \
    callback(cLangOpts, callbackContext, BCKAttribute, StringRef(#SPELLING));
#include "swift/AST/DeclAttr.def"

#define SIL_TYPE_ATTR(X, C)
#define TYPE_ATTR(SPELLING, CLASS)                                             \
  callback(cLangOpts, callbackContext, BCKAttribute, StringRef(#SPELLING));
#include "swift/AST/TypeAttr.def"

  // Deal with all of the target platform/architecture information.
  for (const auto &[kind, value] : langOpts.getPlatformConditionValues()) {
    switch (kind) {
      case PlatformConditionKind::OS:
        callback(cLangOpts, callbackContext, BCKTargetOSName, StringRef(value));

        // Special case that macOS is an alias of OSX.
        if (value == "OSX") {
          callback(cLangOpts, callbackContext, BCKTargetOSName,
                   StringRef("macOS"));
        }
        break;

      case PlatformConditionKind::Arch:
        callback(cLangOpts, callbackContext, BCKTargetArchitecture, StringRef(value));
        break;

      case PlatformConditionKind::Runtime:
        callback(cLangOpts, callbackContext, BCKTargetRuntime, StringRef(value));
        break;

      case PlatformConditionKind::TargetEnvironment:
        callback(cLangOpts, callbackContext, BCKTargetEnvironment,
                 StringRef(value));

        // When compiling for iOS we consider "macCatalyst" to be a
        // synonym of "macabi". This enables the use of
        // #if targetEnvironment(macCatalyst) as a compilation
        // condition for macCatalyst.
        if (value == "macabi" && langOpts.Target.isiOS()) {
          callback(cLangOpts, callbackContext, BCKTargetEnvironment,
                   StringRef("macCatalyst"));
        }
        break;

      case PlatformConditionKind::PtrAuth:
        callback(cLangOpts, callbackContext, BCKTargetPointerAuthenticationScheme,
                 StringRef(value));
        break;

      case PlatformConditionKind::Endianness:
      case PlatformConditionKind::PointerBitWidth:
      case PlatformConditionKind::CanImport:
      case PlatformConditionKind::HasAtomicBitWidth:
        // Handled separately.
        break;
    }
  }

  // Object file format.
  llvm::Triple triple(langOpts.Target.getTriple());
  switch (triple.getObjectFormat()) {
    case llvm::Triple::ObjectFormatType::COFF:
      callback(cLangOpts, callbackContext, BCKTargetObjectFileFormat,
               StringRef("COFF"));
      break;
    case llvm::Triple::ObjectFormatType::ELF:
      callback(cLangOpts, callbackContext, BCKTargetObjectFileFormat,
               StringRef("ELF"));
      break;
    case llvm::Triple::ObjectFormatType::MachO:
      callback(cLangOpts, callbackContext, BCKTargetObjectFileFormat,
               StringRef("MachO"));
      break;
    case llvm::Triple::ObjectFormatType::SPIRV:
      callback(cLangOpts, callbackContext, BCKTargetObjectFileFormat,
               StringRef("SPIRV"));
      break;
    case llvm::Triple::ObjectFormatType::Wasm:
      callback(cLangOpts, callbackContext, BCKTargetObjectFileFormat,
               StringRef("Wasm"));
      break;
    default:
      // Ignore others.
      break;
  }
}
