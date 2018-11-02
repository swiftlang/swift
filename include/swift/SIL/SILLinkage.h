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
  Public,

  /// This is a special linkage used for symbols which are treated
  /// as public for the purposes of SIL serialization and optimization,
  /// but do not have public entry points in the generated binary.
  ///
  /// There is no external variant of this linkage, because from other
  /// translation units in the same module, this behaves identically
  /// to the HiddenExternal linkage.
  ///
  /// When deserialized, such declarations receive Shared linkage.
  PublicNonABI,

  /// This object definition is visible only to the current Swift
  /// module (and thus should not be visible across linkage-unit
  /// boundaries).  There are no other object definitions with this
  /// name in the module.
  Hidden,

  /// This object definition is visible only within a single Swift
  /// module.  There may be other object definitions with this name in
  /// the module; those definitions are all guaranteed to be
  /// semantically equivalent to this one.
  Shared,

  /// This object definition is visible only within a single Swift
  /// file.
  Private,

  /// A Public definition with the same name as this object will be
  /// available to the current Swift module at runtime.  If this
  /// object is a definition, it is semantically equivalent to that
  /// definition.
  PublicExternal,

  /// A Public or Hidden definition with the same name as this object
  /// will be defined by the current Swift module at runtime.  If this
  /// object is a definition, it is semantically equivalent to that
  /// definition.
  HiddenExternal,

  /// This Shared definition was imported from another module. It is not
  /// necessary to serialize it since it can be deserialized from the original
  /// module. Besides that caveat this should be treated exactly the same as
  /// shared.
  SharedExternal,

  /// The same as SharedExternal, except that the definition is private in the
  /// other module. This can only occur if an inlined fragile function from
  /// another module references a private definition in the other module.
  PrivateExternal,

  /// The default linkage for a definition.
  DefaultForDefinition = Public,

  /// The default linkage for an external declaration.
  DefaultForDeclaration = PublicExternal,
};

enum {
  /// The number of bits required to store a SILLinkage value.
  NumSILLinkageBits = 4
};

/// Related to linkage: flag if a function or global variable is serialized,
/// either unconditionally, or if referenced from another serialized function.
enum IsSerialized_t : unsigned char {
  // Never serialized.
  IsNotSerialized,
  // Serialized if referenced from another serialized function.
  IsSerializable,
  // Always serialized.
  IsSerialized
};

/// The scope in which a subclassable class can be subclassed.
enum class SubclassScope : uint8_t {
  /// This class can be subclassed in other modules.
  External,

  /// This class can only be subclassed in this module.
  Internal,

  /// There is no class to subclass, or it is final.
  NotApplicable,
};

/// Strip external from public_external, hidden_external. Otherwise just return
/// the linkage.
inline SILLinkage stripExternalFromLinkage(SILLinkage linkage) {
  if (linkage == SILLinkage::PublicExternal)
    return SILLinkage::Public;
  if (linkage == SILLinkage::HiddenExternal)
    return SILLinkage::Hidden;
  if (linkage == SILLinkage::SharedExternal)
    return SILLinkage::Shared;
  if (linkage == SILLinkage::PrivateExternal)
    return SILLinkage::Private;
  return linkage;
}

/// Add the 'external' attribute to \p linkage.
inline SILLinkage addExternalToLinkage(SILLinkage linkage) {
  switch (linkage) {
  case SILLinkage::Public:
    return SILLinkage::PublicExternal;
  case SILLinkage::PublicNonABI:
    // An external reference to a public non-ABI function is only valid
    // if the function was emitted in another translation unit of the
    // same Swift module, so we treat it as hidden here.
    return SILLinkage::HiddenExternal;
  case SILLinkage::Shared:
    return SILLinkage::SharedExternal;
  case SILLinkage::Hidden:
    return SILLinkage::HiddenExternal;
  case SILLinkage::Private:
    return SILLinkage::PrivateExternal;
  case SILLinkage::PublicExternal:
  case SILLinkage::SharedExternal:
  case SILLinkage::PrivateExternal:
  case SILLinkage::HiddenExternal:
    return linkage;
  }

  llvm_unreachable("Unhandled SILLinkage in switch.");
}

/// Return whether the linkage indicates that an object has a
/// definition outside the current SILModule.
inline bool isAvailableExternally(SILLinkage linkage) {
  return linkage >= SILLinkage::PublicExternal;
}

/// Return whether the given linkage indicates that an object's
/// definition might be required outside the current SILModule.
/// If \p is true then we are in whole-module compilation.
inline bool isPossiblyUsedExternally(SILLinkage linkage, bool wholeModule) {
  if (wholeModule) {
    return linkage <= SILLinkage::PublicNonABI;
  }
  return linkage <= SILLinkage::Hidden;
}

SILLinkage getDeclSILLinkage(const ValueDecl *decl);

inline bool hasPublicVisibility(SILLinkage linkage) {
  switch (linkage) {
  case SILLinkage::Public:
  case SILLinkage::PublicExternal:
  case SILLinkage::PublicNonABI:
    return true;
  case SILLinkage::Hidden:
  case SILLinkage::Shared:
  case SILLinkage::SharedExternal:
  case SILLinkage::Private:
  case SILLinkage::PrivateExternal:
  case SILLinkage::HiddenExternal:
    return false;
  }

  llvm_unreachable("Unhandled SILLinkage in switch.");
}

inline bool hasSharedVisibility(SILLinkage linkage) {
  switch (linkage) {
  case SILLinkage::Shared:
  case SILLinkage::SharedExternal:
    return true;
  case SILLinkage::Public:
  case SILLinkage::PublicExternal:
  case SILLinkage::PublicNonABI:
  case SILLinkage::Hidden:
  case SILLinkage::HiddenExternal:
  case SILLinkage::Private:
  case SILLinkage::PrivateExternal:
    return false;
  }

  llvm_unreachable("Unhandled SILLinkage in switch.");
}

inline bool hasPrivateVisibility(SILLinkage linkage) {
  switch (linkage) {
  case SILLinkage::Private:
  case SILLinkage::PrivateExternal:
    return true;
  case SILLinkage::Public:
  case SILLinkage::PublicExternal:
  case SILLinkage::PublicNonABI:
  case SILLinkage::Hidden:
  case SILLinkage::HiddenExternal:
  case SILLinkage::Shared:
  case SILLinkage::SharedExternal:
    return false;
  }

  llvm_unreachable("Unhandled SILLinkage in switch.");
}

inline SILLinkage effectiveLinkageForClassMember(SILLinkage linkage,
                                                 SubclassScope scope) {
  switch (scope) {
  case SubclassScope::External:
    if (linkage == SILLinkage::Private || linkage == SILLinkage::Hidden)
      return SILLinkage::Public;
    if (linkage == SILLinkage::PrivateExternal ||
        linkage == SILLinkage::HiddenExternal)
      return SILLinkage::PublicExternal;
    break;

  case SubclassScope::Internal:
    if (linkage == SILLinkage::Private)
      return SILLinkage::Hidden;
    break;

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
inline bool
fixmeWitnessHasLinkageThatNeedsToBePublic(SILLinkage witnessLinkage) {
  return !hasPublicVisibility(witnessLinkage) &&
         !hasSharedVisibility(witnessLinkage);
}

} // end swift namespace

#endif
