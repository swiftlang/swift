//===--- SILLinkage.h - Defines the SILLinkage type -------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILLINKAGE_H
#define SWIFT_SIL_SILLINKAGE_H

namespace swift {

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
enum class SILLinkage : unsigned char {
  /// This object definition is visible to multiple Swift modules (and
  /// thus potentially across linkage-unit boundaries).  There are no
  /// other object definitions with this name in the program.
  Public,

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

  /// The default linkage for a definition.
  DefaultForDefinition = Public,

  /// The default linkage for an external declaration.
  DefaultForDeclaration = PublicExternal,
};

enum {
  /// The number of bits required to store a SILLinkage value.
  NumSILLinkageBits = 3
};

/// Return whether the linkage indicates that an object has a
/// definition outside the current SILModule.
inline bool isAvailableExternally(SILLinkage linkage) {
  return linkage >= SILLinkage::PublicExternal;
}

/// Return whether the given linkage indicates that an object's
/// definition might be required outside the current SILModule.
inline bool isPossiblyUsedExternally(SILLinkage linkage) {
  return linkage <= SILLinkage::Hidden;
}

} // end swift namespace

#endif
