//===--- LookupKinds.h - Swift name-lookup enums ----------------*- C++ -*-===//
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
// This file defines enums relating to name lookup.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_LOOKUPKINDS_H
#define SWIFT_LOOKUPKINDS_H

/// `LookupKinds.h` is imported into Swift. Be *very* careful with what you
/// include here and keep these includes minimal!
/// If you don't need to import a header into Swift, include it in a
/// `#ifdef NOT_COMPILED_WITH_SWIFT_PURE_BRIDGING_MODE`
/// block below instead.
///
/// See include guidelines and caveats in `BasicBridging.h`.

#include "swift/Basic/OptionSet.h"

namespace swift {

/// NLKind - This is a specifier for the kind of name lookup being performed
/// by various query methods.
enum class NLKind {
  UnqualifiedLookup,
  QualifiedLookup
};

void simple_display(llvm::raw_ostream &out, NLKind kind);

/// Constants used to customize name lookup.
enum class NLFlags : unsigned {
  /// Consider declarations within protocols to which the context type conforms.
  ProtocolMembers = 1 << 0,

  /// Remove non-visible declarations from the set of results.
  RemoveNonVisible = 1 << 1,

  /// Remove overridden declarations from the set of results.
  RemoveOverridden = 1 << 2,

  /// Remove associated type declarations from the set of results. This is used
  /// by conformance checking for resolving type witnesses.
  RemoveAssociatedTypes = 1 << 3,

  /// Don't check access when doing lookup into a type.
  ///
  /// When performing lookup into a module, this option only applies to
  /// declarations in the same module the lookup is coming from.
  IgnoreAccessControl = 1 << 4,

  /// This lookup should only return type declarations.
  OnlyTypes = 1 << 5,

  /// Include synonyms declared with @_implements()
  IncludeAttributeImplements = 1 << 6,

  // Include @usableFromInline and @inlinable
  IncludeUsableFromInline = 1 << 7,

  /// Exclude names introduced by macro expansions in the top-level module.
  ExcludeMacroExpansions = 1 << 8,

  /// This lookup should only return macro declarations.
  OnlyMacros = 1 << 9,

  /// Include members that would otherwise be filtered out because they come
  /// from a module that has not been imported.
  IgnoreMissingImports = 1 << 10,

  /// If @abi attributes are present, return the decls representing the ABI,
  /// not the API.
  ABIProviding = 1 << 11,

  /// The default set of options used for qualified name lookup.
  ///
  /// FIXME: Eventually, add ProtocolMembers to this, once all of the
  /// callers can handle it.
  QualifiedDefault = RemoveNonVisible | RemoveOverridden,

  /// The default set of options used for unqualified name lookup.
  UnqualifiedDefault = RemoveNonVisible | RemoveOverridden
};

/// A set of `NLFlags` options for name lookup.
using NLOptions = OptionSet<NLFlags>;

inline bool operator==(NLOptions lhs, NLOptions rhs) {
  return lhs.containsOnly(rhs);
}

#ifdef NOT_COMPILED_WITH_SWIFT_PURE_BRIDGING_MODE

inline llvm::hash_code hash_value(NLOptions options) {
  return llvm::hash_value(options.toRaw());
}

void simple_display(llvm::raw_ostream &out, NLOptions options);

#endif // #ifdef NOT_COMPILED_WITH_SWIFT_PURE_BRIDGING_MODE

/// Flags affecting module-level lookup.
enum class ModuleLookupFlags : unsigned {
  /// Exclude names introduced by macro expansions in the top-level module.
  ExcludeMacroExpansions = 1 << 0,
  /// If @abi attributes are present, return the decls representing the ABI,
  /// not the API.
  ABIProviding = 1 << 1,
  /// The lookup is qualified by a module selector which has specified this
  /// module explicitly.
  HasModuleSelector = 1 << 2,
};

} // end namespace swift

#endif
