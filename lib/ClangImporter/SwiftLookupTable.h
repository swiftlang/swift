//===--- SwiftLookupTable.h - Swift Lookup Table ----------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
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
#include "clang/Serialization/ASTBitCodes.h"
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
class MacroInfo;
class ObjCCategoryDecl;
}

namespace swift {

class SwiftLookupTableReader;
class SwiftLookupTableWriter;

/// Lookup table major version number.
///
const uint16_t SWIFT_LOOKUP_TABLE_VERSION_MAJOR = 1;

/// Lookup table minor version number.
///
/// When the format changes IN ANY WAY, this number should be incremented.
const uint16_t SWIFT_LOOKUP_TABLE_VERSION_MINOR = 2;

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

  /// A single entry referencing either a named declaration or a macro.
  typedef llvm::PointerUnion<clang::NamedDecl *, clang::MacroInfo *> SingleEntry;

  /// An entry in the table of C entities indexed by full Swift name.
  struct FullTableEntry {
    /// The context in which the entities with the given name occur, e.g.,
    /// a class, struct, translation unit, etc.
    /// is always the canonical DeclContext for the entity.
    std::pair<ContextKind, StringRef> Context;

    /// The set of Clang declarations and macros with this name and in
    /// this context.
    ///
    /// The low bit indicates whether we have a declaration or macro
    /// (declaration = unset, macro = set) and the second lowest bit
    /// indicates whether we have a serialization ID (set = DeclID or
    /// MacroID, as appropriate) vs. a pointer (unset,
    /// clang::NamedDecl * or clang::MacroInfo *). In the ID case, the
    /// upper N-2 bits are the ID value; in the pointer case, the
    /// lower two bits will always be clear due to the alignment of
    /// the Clang pointers.
    llvm::SmallVector<uintptr_t, 2> DeclsOrMacros;
  };

  /// Whether the given entry is a macro entry.
  static bool isMacroEntry(uintptr_t entry) { return entry & 0x01; }

  /// Whether the given entry is a declaration entry.
  static bool isDeclEntry(uintptr_t entry) { return !isMacroEntry(entry); }

  /// Whether the given entry is a serialization ID.
  static bool isSerializationIDEntry(uintptr_t entry) { return (entry & 0x02); }

  /// Whether the given entry is an AST node.
  static bool isASTNodeEntry(uintptr_t entry) {
    return !isSerializationIDEntry(entry);
  }

  /// Retrieve the serialization ID for an entry.
  static uint32_t getSerializationID(uintptr_t entry) {
    assert(isSerializationIDEntry(entry) && "Not a serialization entry");
    return entry >> 2;
  }

  /// Retrieve the pointer for an entry.
  static void *getPointerFromEntry(uintptr_t entry) {
    assert(isASTNodeEntry(entry) && "Not an AST node entry");
    const uintptr_t mask = ~static_cast<uintptr_t>(0x03);
    return reinterpret_cast<void *>(entry & mask);
  }

  /// Encode a Clang named declaration as an entry in the table.
  static uintptr_t encodeEntry(clang::NamedDecl *decl) {
    auto bits = reinterpret_cast<uintptr_t>(decl);
    assert((bits & 0x03) == 0 && "low bits set?");
    return bits;
  }

  // Encode a Clang macro as an entry in the table.
  static uintptr_t encodeEntry(clang::MacroInfo *macro) {
    auto bits = reinterpret_cast<uintptr_t>(macro);
    assert((bits & 0x03) == 0 && "low bits set?");
    return bits | 0x01;
  }

  /// Encode a declaration ID as an entry in the table.
  static uintptr_t encodeDeclID(clang::serialization::DeclID id) {
    auto upper = static_cast<uintptr_t>(id) << 2;
    assert(upper >> 2 == id);
    return upper | 0x02;
  }

  /// Encode a macro ID as an entry in the table.
  static uintptr_t encodeMacroID(clang::serialization::MacroID id) {
    auto upper = static_cast<uintptr_t>(id) << 2;
    assert(upper >> 2 == id);
    return upper | 0x02 | 0x01;
  }

private:
  /// A table mapping from the base name of Swift entities to all of
  /// the C entities that have that name, in all contexts.
  llvm::DenseMap<StringRef, SmallVector<FullTableEntry, 2>> LookupTable;

  /// The list of Objective-C categories and extensions.
  llvm::SmallVector<clang::ObjCCategoryDecl *, 4> Categories;

  /// The reader responsible for lazily loading the contents of this table.
  SwiftLookupTableReader *Reader;

  friend class SwiftLookupTableReader;
  friend class SwiftLookupTableWriter;

  /// Find or create the table entry for the given base name. 
  llvm::DenseMap<StringRef, SmallVector<FullTableEntry, 2>>::iterator
  findOrCreate(StringRef baseName);

public:
  explicit SwiftLookupTable(SwiftLookupTableReader *reader) : Reader(reader) { }

  /// Maps a stored declaration entry to an actual Clang declaration.
  clang::NamedDecl *mapStoredDecl(uintptr_t &entry);

  /// Maps a stored macro entry to an actual Clang macro.
  clang::MacroInfo *mapStoredMacro(uintptr_t &entry);

  /// Maps a stored entry to an actual Clang AST node.
  SingleEntry mapStored(uintptr_t &entry);

  /// Translate a Clang DeclContext into a context kind and name.
  llvm::Optional<std::pair<ContextKind, StringRef>>
  translateContext(clang::DeclContext *context);

  /// Add an entry to the lookup table.
  ///
  /// \param name The Swift name of the entry.
  /// \param newEntry The Clang declaration or macro.
  /// \param effectiveContext The effective context in which name lookup occurs.
  void addEntry(DeclName name, SingleEntry newEntry,
                clang::DeclContext *effectiveContext);

  /// Add an Objective-C category or extension to the table.
  void addCategory(clang::ObjCCategoryDecl *category);

  /// Lookup the set of entities with the given base name.
  ///
  /// \param baseName The base name to search for. All results will
  /// have this base name.
  ///
  /// \param searchContext The context in which the resulting set of
  /// entities should reside. This may be null to indicate that
  /// all results from all contexts should be produced.
  SmallVector<SingleEntry, 4>
  lookup(StringRef baseName, clang::DeclContext *searchContext);

  /// Retrieve the set of base names that are stored in the lookup table.
  SmallVector<StringRef, 4> allBaseNames();

  /// Lookup Objective-C members with the given base name, regardless
  /// of context.
  SmallVector<clang::NamedDecl *, 4> lookupObjCMembers(StringRef baseName);

  /// Retrieve the set of Objective-C categories and extensions.
  ArrayRef<clang::ObjCCategoryDecl *> categories();

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
  ArrayRef<clang::serialization::DeclID> Categories;

  SwiftLookupTableReader(clang::ModuleFileExtension *extension,
                         clang::ASTReader &reader,
                         clang::serialization::ModuleFile &moduleFile,
                         std::function<void()> onRemove,
                         void *serializedTable,
                         ArrayRef<clang::serialization::DeclID> categories)
    : ModuleFileExtensionReader(extension), Reader(reader),
      ModuleFile(moduleFile), OnRemove(onRemove),
      SerializedTable(serializedTable), Categories(categories) { }

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

  /// Retrieve the declaration IDs of the categories.
  ArrayRef<clang::serialization::DeclID> categories() const {
    return Categories;
  }
};

}

#endif // SWIFT_CLANGIMPORTER_SWIFTLOOKUPTABLE_H
