//===--- SILLinkage.h - Defines the SILLinkage type -------------*- C++ -*-===//
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

#ifndef SWIFT_SIL_SILLINKAGE_H
#define SWIFT_SIL_SILLINKAGE_H

#include "llvm/Support/ErrorHandling.h"

// FIXME: Remove after fixmeWitnessHasLinkageThatNeedsToBePublic is removed.
#include "swift/SIL/SILDeclRef.h"

namespace swift {

class ValueDecl;

/// Linkage for a SIL object.  This concept combines the notions
/// of symbol linkage and visibility.
///
/// Note that a Swift module is not the same thing as a SILModule.
/// A SILModule is just a collection of objects.
///
/// Semantic equivalence does not imply exact operational equivalence.
/// For example, a function definition might be semantically
/// equivalent to a second definition which uses a parameter that the
/// first does not, perhaps by reading a value out of it (and then
/// ignoring the result) or retaining it (and then releasing it
/// later).
enum class SILLinkage : uint8_t {
  /// This object definition is visible to multiple Swift modules (and
  /// thus potentially across linkage-unit boundaries).  There are no
  /// other object definitions with this name in the program.
  ///
  /// Public functions must be definitions, i.e. must have a body, except the
  /// body is emitted by clang.
  Public,

  /// This is a special linkage used for symbols which are treated
  /// as public for the purposes of SIL serialization and optimization,
  /// but do not have public entry points in the generated binary.
  ///
  /// This linkage is used for @_alwaysEmitIntoClient functions.
  ///
  /// There is no external variant of this linkage, because from other
  /// translation units in the same module, this behaves identically
  /// to the HiddenExternal linkage.
  ///
  /// When deserialized, such declarations receive Shared linkage.
  ///
  /// PublicNonABI functions must be definitions.
  PublicNonABI,

  /// Same as \c Public, except the definition is visible within a package
  /// of modules.
  Package,

  /// Similar to \c PublicNonABI, this definition is used for symbols treated
  /// as package but do not have package entry points in the generated binary.
  /// It's used for default argument expressions and `@_alwaysEmitIntoClient`.
  /// When deserialized, this will become \c Shared linkage.
  PackageNonABI,

  /// This object definition is visible only to the current Swift
  /// module (and thus should not be visible across linkage-unit
  /// boundaries).  There are no other object definitions with this
  /// name in the module.
  ///
  /// Hidden functions must be definitions, i.e. must have a body, except the
  /// body is emitted by clang.
  Hidden,

  /// This object definition is visible only within a single Swift
  /// module.  There may be other object definitions with this name in
  /// the module; those definitions are all guaranteed to be
  /// semantically equivalent to this one.
  ///
  /// This linkage is used e.g. for thunks and for specialized functions.
  ///
  /// Shared functions must be definitions, i.e. must have a body, except the
  /// body is emitted by clang.
  Shared,

  /// This object definition is visible only within a single Swift
  /// file.
  ///
  /// Private functions must be definitions, i.e. must have a body, except the
  /// body is emitted by clang.
  Private,

  /// A Public definition with the same name as this object will be
  /// available to the current Swift module at runtime.  If this
  /// object is a definition, it is semantically equivalent to that
  /// definition.
  PublicExternal,

  /// Similar to \c PublicExternal.
  /// Used to reference a \c Package definition in a different module
  /// within a package.
  PackageExternal,

  /// A Public or Hidden definition with the same name as this object
  /// will be defined by the current Swift module at runtime.
  ///
  /// This linkage is only used for non-whole-module compilations to refer to
  /// functions in other files of the same module.
  HiddenExternal,

  /// The default linkage for a definition.
  DefaultForDefinition = Public,

  /// The default linkage for an external declaration.
  DefaultForDeclaration = PublicExternal,
};

enum {
  /// The number of bits required to store a SILLinkage value.
  NumSILLinkageBits = 4
};

/// Related to linkage: flag if a function, global variable, vtable or witness
/// table is serialized.
///
/// Used, e.g. for @inlinable functions.
///
/// This flag serves for two purposes:
/// * Imposes restrictions for optimizations. For example, non-serialized functions
///   cannot be inlined into serialized functions, because that could expose
///   internal types/functions to client modules.
/// * Tells the serializer which functions (and tables) need to be serialized:
///   - all public functions with the IsSerialized flag and
///   - all IsSerialized shared functions which are referenced from such functions.
///
/// After the swiftmodule file is written, the IsSerialized flag is cleared from
/// all functions. This means that optimizations after the serialization point
/// are not limited anymore regarding serialized functions.
enum SerializedKind_t : uint8_t {

  /// The function is not inlinable and will not be serialized.
  IsNotSerialized,

  /// The function (or table) will be serialized.
  ///
  /// This flag is only valid for Public, PublicNonABI, PublicExternal,
  /// HiddenExternal and Shared functions.
  /// Functions with external linkage (PublicExternal, HiddenExternal) will not
  /// be serialized, because they are available in a different module (from
  /// which they were de-serialized).
  ///
  /// Functions with Shared linkage will only be serialized if they are
  /// referenced from another serialized function (or table).
  ///
  /// This flag is removed from all functions after the serialization point in
  /// the optimizer pipeline.
  IsSerialized,

  /// This flag is valid for all linkages applicable to IsSerialized as well as
  /// Package, PackageNonABI, and PackageExternal, if package-wide
  /// serialization is enabled with Package-CMO optimization.
  ///
  /// The [serialized_for_package] attribute is used to indicate that a function
  /// is serialized because of Package CMO, which allows loadable types in a
  /// serialized function in a resiliently built module, which is otherwise illegal.
  /// It's also used to determine during SIL deserialization whether loadable
  /// types in a serialized function can be allowed in the client module that
  /// imports the module built with Package CMO. If the client contains a [serialized]
  /// function due to `@inlinable`, funtions with [serialized_for_package] from
  /// the imported module are not allowed being inlined into the client function,
  /// which is the correct behavior.
  IsSerializedForPackage
};

/// The scope in which a subclassable class can be subclassed.
enum class SubclassScope : uint8_t {
  /// This class can be subclassed in other modules.
  External,

  /// This class can only be subclassed in this module.
  Internal,

  /// This class is resilient so even public methods cannot be directly
  /// referenced from outside the module.
  Resilient,

  /// There is no class to subclass, or it is final.
  NotApplicable,
};

/// Strip external from public_external, hidden_external. Otherwise just return
/// the linkage.
inline SILLinkage stripExternalFromLinkage(SILLinkage linkage) {
  switch (linkage) {
  case SILLinkage::PublicExternal:
    return SILLinkage::Public;
  case SILLinkage::PackageExternal:
    return SILLinkage::Package;
  case SILLinkage::HiddenExternal:
    return SILLinkage::Hidden;
  case SILLinkage::Public:
  case SILLinkage::PublicNonABI:
  case SILLinkage::Package:
  case SILLinkage::PackageNonABI:
  case SILLinkage::Hidden:
  case SILLinkage::Shared:
  case SILLinkage::Private:
    return linkage;
  }
  llvm_unreachable("Unhandled SILLinkage in switch.");
}

/// Add the 'external' attribute to \p linkage.
inline SILLinkage addExternalToLinkage(SILLinkage linkage) {
  switch (linkage) {
  case SILLinkage::Public:
    return SILLinkage::PublicExternal;
  case SILLinkage::Package:
    return SILLinkage::PackageExternal;
  case SILLinkage::PublicNonABI:
  case SILLinkage::PackageNonABI:
    // An external reference to a public or package non-ABI function is only valid
    // if the function was emitted in another translation unit of the
    // same Swift module, so we treat it as hidden here.
    return SILLinkage::HiddenExternal;
  case SILLinkage::Hidden:
    return SILLinkage::HiddenExternal;
  case SILLinkage::Shared:
  case SILLinkage::Private:
  case SILLinkage::PublicExternal:
  case SILLinkage::PackageExternal:
  case SILLinkage::HiddenExternal:
    return linkage;
  }

  llvm_unreachable("Unhandled SILLinkage in switch.");
}

/// Return whether the linkage indicates that an object has a
/// definition outside the current SILModule.
inline bool isAvailableExternally(SILLinkage linkage) {
  switch (linkage) {
  case SILLinkage::Public:
  case SILLinkage::PublicNonABI:
  case SILLinkage::Package:
  case SILLinkage::PackageNonABI:
  case SILLinkage::Hidden:
  case SILLinkage::Shared:
  case SILLinkage::Private:
    return false;
  case SILLinkage::PublicExternal:
  case SILLinkage::PackageExternal:
  case SILLinkage::HiddenExternal:
    return true;
  }
  llvm_unreachable("Unhandled SILLinkage in switch.");
}

/// Return whether the given linkage indicates that an object's
/// definition might be required outside the current SILModule.
/// If \p is true then we are in whole-module compilation.
inline bool isPossiblyUsedExternally(SILLinkage linkage, bool wholeModule) {
  switch (linkage) {
  case SILLinkage::Public:
  case SILLinkage::PublicNonABI:
  case SILLinkage::Package:
  case SILLinkage::PackageNonABI:
    return true;
  case SILLinkage::Hidden:
    return !wholeModule;
  case SILLinkage::Shared:
  case SILLinkage::Private:
  case SILLinkage::PublicExternal:
  case SILLinkage::PackageExternal:
  case SILLinkage::HiddenExternal:
    return false;
  }
  llvm_unreachable("Unhandled SILLinkage in switch.");
}

SILLinkage getDeclSILLinkage(const ValueDecl *decl);

inline bool hasPublicVisibility(SILLinkage linkage) {
  switch (linkage) {
  case SILLinkage::Public:
  case SILLinkage::PublicExternal:
  case SILLinkage::PublicNonABI:
    return true;
  case SILLinkage::Package:
  case SILLinkage::PackageExternal:
  case SILLinkage::PackageNonABI:
  case SILLinkage::Hidden:
  case SILLinkage::Shared:
  case SILLinkage::Private:
  case SILLinkage::HiddenExternal:
    return false;
  }

  llvm_unreachable("Unhandled SILLinkage in switch.");
}

/// Opt in package linkage for visibility in case Package CMO is enabled.
/// Used in SIL verification and other checks that determine inlinability to
/// accomodate for the optimization.
inline bool hasPublicOrPackageVisibility(SILLinkage linkage, bool includePackage) {
    switch (linkage) {
    case SILLinkage::Public:
    case SILLinkage::PublicExternal:
    case SILLinkage::PublicNonABI:
        return true;
    case SILLinkage::Package:
    case SILLinkage::PackageExternal:
    case SILLinkage::PackageNonABI:
        return includePackage;
    case SILLinkage::Hidden:
    case SILLinkage::Shared:
    case SILLinkage::Private:
    case SILLinkage::HiddenExternal:
        return false;
    }

    llvm_unreachable("Unhandled SILLinkage in switch.");
}

inline bool hasSharedVisibility(SILLinkage linkage) {
  switch (linkage) {
  case SILLinkage::Shared:
    return true;
  case SILLinkage::Public:
  case SILLinkage::PublicExternal:
  case SILLinkage::PublicNonABI:
  case SILLinkage::Package:
  case SILLinkage::PackageExternal:
  case SILLinkage::PackageNonABI:
  case SILLinkage::Hidden:
  case SILLinkage::HiddenExternal:
  case SILLinkage::Private:
    return false;
  }

  llvm_unreachable("Unhandled SILLinkage in switch.");
}

inline bool hasPrivateVisibility(SILLinkage linkage) {
  switch (linkage) {
  case SILLinkage::Private:
    return true;
  case SILLinkage::Public:
  case SILLinkage::PublicExternal:
  case SILLinkage::PublicNonABI:
  case SILLinkage::Package:
  case SILLinkage::PackageExternal:
  case SILLinkage::PackageNonABI:
  case SILLinkage::Hidden:
  case SILLinkage::HiddenExternal:
  case SILLinkage::Shared:
    return false;
  }

  llvm_unreachable("Unhandled SILLinkage in switch.");
}

inline SILLinkage effectiveLinkageForClassMember(SILLinkage linkage,
                                                 SubclassScope scope) {
  switch (scope) {
  case SubclassScope::External:
    switch (linkage) {
      case SILLinkage::Hidden:
      case SILLinkage::Private:
        return SILLinkage::Public;
      case SILLinkage::HiddenExternal:
        return SILLinkage::PublicExternal;
      case SILLinkage::Public:
      case SILLinkage::PublicNonABI:
      case SILLinkage::Package:
      case SILLinkage::PackageNonABI:
      case SILLinkage::PublicExternal:
      case SILLinkage::PackageExternal:
      case SILLinkage::Shared:
        break;
    }
    break;
  case SubclassScope::Internal:
    if (linkage == SILLinkage::Private)
      return SILLinkage::Hidden;
    break;

  case SubclassScope::Resilient:
    if (isAvailableExternally(linkage))
      return SILLinkage::HiddenExternal;
    return SILLinkage::Hidden;

  case SubclassScope::NotApplicable:
    break;
  }
  return linkage;
}

// FIXME: This should not be necessary, but it looks like visibility rules for
// extension members are slightly bogus, and so some protocol witness thunks
// need to be public.
//
// We allow a 'public' member of an extension to witness a public
// protocol requirement, even if the extended type is not public;
// then SILGen gives the member private linkage, ignoring the more
// visible access level it was given in the AST.
//
// Despite the FIXME above, this is still used to determine the linkage
// for witness thunks. In case package serialization is enabled, we need
// to take the package linkage into account so we can set a proper final
// linkage to the thunks in the witness table with a package linkage.
inline bool fixmeWitnessHasLinkageThatNeedsToBePublic(SILDeclRef witness,
                                                      bool isPackageVisible) {
  auto witnessLinkage = witness.getLinkage(ForDefinition);
  return !hasPublicOrPackageVisibility(witnessLinkage, isPackageVisible) &&
         (!hasSharedVisibility(witnessLinkage) || !witness.isSerialized());
}

} // end swift namespace

#endif
