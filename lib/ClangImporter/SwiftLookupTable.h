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
#include "clang/Serialization/ModuleFileExtension.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/TinyPtrVector.h"
#include <functional>
#include <utility>

namespace llvm {
class BitstreamWriter;
}

namespace clang {
class NamedDecl;
class DeclContext;
}

namespace swift {

class SwiftLookupTableReader;
class SwiftLookupTableWriter;

/// Lookup table major version number.
///
const uint16_t SWIFT_LOOKUP_TABLE_VERSION_MAJOR = 1;

/// Lookup table major version number.
///
/// When the format changes IN ANY WAY, this number should be incremented.
const uint16_t SWIFT_LOOKUP_TABLE_VERSION_MINOR = 0;

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

  /// Determine whether the given context requires a name to disambiguate.
  static bool contextRequiresName(ContextKind kind);

  /// An entry in the table of C entities indexed by full Swift name.
  struct FullTableEntry {
    /// The context in which the entities with the given name occur, e.g.,
    /// a class, struct, translation unit, etc.
    /// is always the canonical DeclContext for the entity.
    std::pair<ContextKind, StringRef> Context;

    /// The set of Clang declarations with this name and in this
    /// context.
    ///
    /// The low bit indicates whether we have a clang::serialization::DeclID or
    /// not. If set, the upper 64 bits are the DeclID; if unset, the whole
    /// value can be cast to a Decl*.
    llvm::SmallVector<uint64_t, 2> Decls;
  };

private:
  /// A table mapping from the base name of Swift entities to all of
  /// the C entities that have that name, in all contexts.
  llvm::DenseMap<StringRef, SmallVector<FullTableEntry, 2>> LookupTable;

  /// The reader responsible for lazily loading the contents of this table.
  SwiftLookupTableReader *Reader;

  friend class SwiftLookupTableReader;
  friend class SwiftLookupTableWriter;

public:
  explicit SwiftLookupTable(SwiftLookupTableReader *reader) : Reader(reader) { }

  /// Maps a stored declaration entry to an actual Clang declaration.
  clang::NamedDecl *mapStoredDecl(uint64_t &entry);

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
  /// \param searchContext The context in which the resulting set of
  /// declarations should reside. This may be null to indicate that
  /// all results from all contexts should be produced.
  SmallVector<clang::NamedDecl *, 4>
  lookup(StringRef baseName, clang::DeclContext *searchContext);

  /// Deserialize all entries.
  void deserializeAll();

  /// Dump the internal representation of this lookup table.
  void dump() const;
};

/// Module file extension writer for the Swift lookup tables.
class SwiftLookupTableWriter : public clang::ModuleFileExtensionWriter {
  clang::ASTWriter &Writer;
  std::function<void(clang::Sema &, SwiftLookupTable &)> PopulateTable;

public:
  SwiftLookupTableWriter(
    clang::ModuleFileExtension *extension,
    std::function<void(clang::Sema &, SwiftLookupTable &)> populateTable,
    clang::ASTWriter &writer)
      : ModuleFileExtensionWriter(extension), Writer(writer),
        PopulateTable(std::move(populateTable)) { }

  void writeExtensionContents(clang::Sema &sema,
                              llvm::BitstreamWriter &stream) override;
};

/// Module file extension reader for the Swift lookup tables.
class SwiftLookupTableReader : public clang::ModuleFileExtensionReader {
  clang::ASTReader &Reader;
  clang::serialization::ModuleFile &ModuleFile;
  std::function<void()> OnRemove;

  void *SerializedTable;

  SwiftLookupTableReader(clang::ModuleFileExtension *extension,
                         clang::ASTReader &reader,
                         clang::serialization::ModuleFile &moduleFile,
                         std::function<void()> onRemove,
                         void *serializedTable)
    : ModuleFileExtensionReader(extension), Reader(reader),
      ModuleFile(moduleFile), OnRemove(onRemove),
      SerializedTable(serializedTable) { }

public:
  /// Create a new lookup table reader for the given AST reader and stream
  /// position.
  static std::unique_ptr<SwiftLookupTableReader>
  create(clang::ModuleFileExtension *extension, clang::ASTReader &reader,
         clang::serialization::ModuleFile &moduleFile,
         std::function<void()> onRemove,
         const llvm::BitstreamCursor &stream);

  ~SwiftLookupTableReader();

  /// Retrieve the AST reader associated with this lookup table reader.
  clang::ASTReader &getASTReader() const { return Reader; }

  /// Retrieve the module file associated with this lookup table reader.
  clang::serialization::ModuleFile &getModuleFile() { return ModuleFile; }

  /// Retrieve the set of base names that are stored in the on-disk hash table.
  SmallVector<StringRef, 4> getBaseNames();

  /// Retrieve the set of entries associated with the given base name.
  ///
  /// \returns true if we found anything, false otherwise.
  bool lookup(StringRef baseName,
              SmallVectorImpl<SwiftLookupTable::FullTableEntry> &entries);
};

}

#endif // SWIFT_CLANGIMPORTER_SWIFTLOOKUPTABLE_H
