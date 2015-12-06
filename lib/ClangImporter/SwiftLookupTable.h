//===--- SwiftLookupTable.h - Swift Lookup Table ----------------*- C++ -*-===//
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
//
// This file implements support for Swift name lookup tables stored in Clang
// modules.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_CLANGIMPORTER_SWIFTLOOKUPTABLE_H
#define SWIFT_CLANGIMPORTER_SWIFTLOOKUPTABLE_H

#include "swift/Basic/LLVM.h"
#include "swift/AST/Identifier.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/TinyPtrVector.h"
#include <utility>

namespace clang {
class NamedDecl;
class DeclContext;
}

namespace swift {

/// A lookup table that maps Swift names to the set of Clang
/// declarations with that particular name.
///
/// The names of C entities can undergo significant transformations
/// when they are mapped into Swift, which makes Clang's name lookup
/// mechanisms useless when searching for the Swift name of
/// entities. This lookup table provides efficient access to the C
/// entities based on their Swift names, and is used by the Clang
/// importer to satisfy the Swift compiler's queries.
class SwiftLookupTable {
public:
  /// The kind of context in which a name occurs.
  enum class ContextKind : uint8_t {
    /// A translation unit.
    TranslationUnit = 0,
    /// A tag declaration (struct, enum, union, C++ class).
    Tag,
    /// An Objective-C class.
    ObjCClass,
    /// An Objective-C protocol.
    ObjCProtocol,
  };

  /// An entry in the table of C entities indexed by full Swift name.
  struct FullTableEntry {
    /// The context in which the entities with the given name occur, e.g.,
    /// a class, struct, translation unit, etc.
    /// is always the canonical DeclContext for the entity.
    std::pair<ContextKind, StringRef> Context;

    /// The set of Clang declarations with this name and in this
    /// context.
    llvm::TinyPtrVector<clang::NamedDecl *> Decls;
  };

private:
  /// A table mapping from the full name of Swift entities to all of
  /// the C entities that have that name, in all contexts.
  llvm::DenseMap<DeclName, SmallVector<FullTableEntry, 2>> FullNameTable;

  /// A table mapping from the base name of a Swift name to all of the
  /// full Swift names based on that identifier.
  llvm::DenseMap<Identifier, SmallVector<DeclName, 2>> BaseNameTable;

public:
  /// Translate a Clang DeclContext into a context kind and name.
  llvm::Optional<std::pair<ContextKind, StringRef>>
  translateContext(clang::DeclContext *context);

  /// Add an entry to the lookup table.
  ///
  /// \param name The Swift name of the entry.
  /// \param decl The Clang declaration to add.
  /// \param effectiveContext The effective context in which name lookup occurs.
  void addEntry(DeclName name, clang::NamedDecl *decl,
                clang::DeclContext *effectiveContext);

  /// Lookup the set of declarations with the given base name.
  ///
  /// \param baseName The base name to search for. All results will
  /// have this base name.
  ///
  /// \param context The context in which the resulting set of
  /// declarations should reside. This may be null to indicate that
  /// all results from all contexts should be produced.
  ArrayRef<clang::NamedDecl *>
  lookup(Identifier baseName,
         clang::DeclContext *context,
         SmallVectorImpl<clang::NamedDecl *> &scratch);

  /// Lookup the set of declarations with the given full name.
  ///
  /// \param name The full name to search for. All results will have
  /// this full name.
  ///
  /// \param context The context in which the resulting set of
  /// declarations should reside. This may be null to indicate that
  /// all results from all contexts should be produced.
  ArrayRef<clang::NamedDecl *>
  lookup(DeclName name,
         clang::DeclContext *context,
         SmallVectorImpl<clang::NamedDecl *> &scratch);

  /// Dump the internal representation of this lookup table.
  void dump() const;
};

}

#endif // SWIFT_CLANGIMPORTER_SWIFTLOOKUPTABLE_H
