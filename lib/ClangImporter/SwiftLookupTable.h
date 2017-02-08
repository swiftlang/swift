//===--- SwiftLookupTable.h - Swift Lookup Table ----------------*- C++ -*-===//
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
// This file implements support for Swift name lookup tables stored in Clang
// modules.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_CLANGIMPORTER_SWIFTLOOKUPTABLE_H
#define SWIFT_CLANGIMPORTER_SWIFTLOOKUPTABLE_H

#include "swift/Basic/LLVM.h"
#include "swift/AST/Identifier.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
#include "clang/Serialization/ASTBitCodes.h"
#include "clang/Serialization/ModuleFileExtension.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Support/Compiler.h"
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
class TypedefNameDecl;
}

namespace swift {

/// The context into which a Clang declaration will be imported.
///
/// When the context into which a declaration will be imported matches
/// a Clang declaration context (the common case), the result will be
/// expressed as a declaration context. Otherwise, if the Clang type
/// is not itself a declaration context (for example, a typedef that
/// comes into Swift as a strong type), the Clang type declaration
/// will be provided. Finally, if the context is known only via its
/// Swift name, this will be recorded as
class EffectiveClangContext {
public:
  enum Kind {
    DeclContext,
    TypedefContext,
    UnresolvedContext,
  };

private:
  union {
    const clang::DeclContext *DC;
    const clang::TypedefNameDecl *Typedef;
    struct {
      const char *Data;
    } Unresolved;
  };
  Kind TheKind;
  unsigned UnresolvedLength;
  
public:
  EffectiveClangContext() : TheKind(DeclContext) {
    DC = nullptr;
  }

  EffectiveClangContext(const clang::DeclContext *dc) : TheKind(DeclContext) {
    assert(dc != nullptr && "use null constructor instead");
    if (auto tagDecl = dyn_cast<clang::TagDecl>(dc)) {
      DC = tagDecl->getCanonicalDecl();
    } else if (auto oiDecl = dyn_cast<clang::ObjCInterfaceDecl>(dc)) {
      DC = oiDecl->getCanonicalDecl();
    } else if (auto opDecl = dyn_cast<clang::ObjCProtocolDecl>(dc)) {
      DC = opDecl->getCanonicalDecl();
    } else if (auto omDecl = dyn_cast<clang::ObjCMethodDecl>(dc)) {
      DC = omDecl->getCanonicalDecl();
    } else if (auto fDecl = dyn_cast<clang::FunctionDecl>(dc)) {
      DC = fDecl->getCanonicalDecl();
    } else {
      assert(isa<clang::TranslationUnitDecl>(dc) ||
             isa<clang::ObjCContainerDecl>(dc) &&
                 "No other kinds of effective Clang contexts");
      DC = dc;
    }
  }

  EffectiveClangContext(const clang::TypedefNameDecl *typedefName)
    : TheKind(TypedefContext)
  {
    Typedef = typedefName->getCanonicalDecl();
  }

  EffectiveClangContext(StringRef unresolved) : TheKind(UnresolvedContext) {
    Unresolved.Data = unresolved.data();
    UnresolvedLength = unresolved.size();
  }

  /// Determine whether this effective Clang context was set.
  explicit operator bool() const {
    return getKind() != DeclContext || DC != nullptr;
  }

  /// Determine the kind of effective Clang context.
  Kind getKind() const { return TheKind; }

  /// Retrieve the declaration context.
  const clang::DeclContext *getAsDeclContext() const {
    return getKind() == DeclContext ? DC : nullptr;
  }

  /// Retrieve the typedef declaration.
  const clang::TypedefNameDecl *getTypedefName() const {
    assert(getKind() == TypedefContext);
    return Typedef;
  }

  /// Retrieve the unresolved context name.
  StringRef getUnresolvedName() const {
    assert(getKind() == UnresolvedContext);
    return StringRef(Unresolved.Data, UnresolvedLength);
  }
};

#if LLVM_PTR_SIZE == 4
static_assert(sizeof(EffectiveClangContext) <= 4 * sizeof(void *),
              "should fit in four pointers");
#else
static_assert(sizeof(EffectiveClangContext) <= 2 * sizeof(void *),
              "should fit in a couple pointers");
#endif

class SwiftLookupTableReader;
class SwiftLookupTableWriter;

/// Lookup table major version number.
///
const uint16_t SWIFT_LOOKUP_TABLE_VERSION_MAJOR = 1;

/// Lookup table minor version number.
///
/// When the format changes IN ANY WAY, this number should be incremented.
const uint16_t SWIFT_LOOKUP_TABLE_VERSION_MINOR = 14; // Swift 2 names

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
  ///
  /// Note that values 0 and 1 are reserved for empty and tombstone
  /// keys.
  enum class ContextKind : uint8_t {
    /// A translation unit.
    TranslationUnit = 2,
    /// A tag declaration (struct, enum, union, C++ class).
    Tag,
    /// An Objective-C class.
    ObjCClass,
    /// An Objective-C protocol.
    ObjCProtocol,
    /// A typedef that produces a strong type in Swift.
    Typedef,
  };

  /// Determine whether the given context requires a name to disambiguate.
  static bool contextRequiresName(ContextKind kind);

  /// A single entry referencing either a named declaration or a macro.
  typedef llvm::PointerUnion<clang::NamedDecl *, clang::MacroInfo *>
    SingleEntry;

  /// A stored version of the context of an entity, which is Clang
  /// ASTContext-independent.
  typedef std::pair<ContextKind, StringRef> StoredContext;

  /// An entry in the table of C entities indexed by full Swift name.
  struct FullTableEntry {
    /// The context in which the entities with the given name occur, e.g.,
    /// a class, struct, translation unit, etc.
    /// is always the canonical DeclContext for the entity.
    StoredContext Context;

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

  /// A mapping from stored contexts to the set of global declarations that
  /// are mapped to members within that context.
  ///
  /// The values use the same representation as
  /// FullTableEntry::DeclsOrMacros.
  llvm::DenseMap<StoredContext, SmallVector<uintptr_t, 2>> GlobalsAsMembers;

  /// The reader responsible for lazily loading the contents of this table.
  SwiftLookupTableReader *Reader;

  /// Entries whose effective contexts could not be resolved, and
  /// therefore will need to be added later.
  SmallVector<std::tuple<DeclName, SingleEntry, EffectiveClangContext>, 4>
    UnresolvedEntries;

  friend class SwiftLookupTableReader;
  friend class SwiftLookupTableWriter;

  /// Find or create the table entry for the given base name. 
  llvm::DenseMap<StringRef, SmallVector<FullTableEntry, 2>>::iterator
  findOrCreate(StringRef baseName);

  /// Add the given entry to the list of entries, if it's not already
  /// present.
  ///
  /// \returns true if the entry was added, false otherwise.
  bool addLocalEntry(SingleEntry newEntry, SmallVectorImpl<uintptr_t> &entries,
                     const clang::Preprocessor *PP);

public:
  explicit SwiftLookupTable(SwiftLookupTableReader *reader) : Reader(reader) { }

  /// Maps a stored declaration entry to an actual Clang declaration.
  clang::NamedDecl *mapStoredDecl(uintptr_t &entry);

  /// Maps a stored macro entry to an actual Clang macro.
  clang::MacroInfo *mapStoredMacro(uintptr_t &entry);

  /// Maps a stored entry to an actual Clang AST node.
  SingleEntry mapStored(uintptr_t &entry);

  /// Translate a Clang DeclContext into a context kind and name.
  static llvm::Optional<StoredContext> translateDeclContext(
                                         const clang::DeclContext *dc);

  /// Translate a Clang effective context into a context kind and name.
  llvm::Optional<StoredContext> translateContext(EffectiveClangContext context);

  /// Add an entry to the lookup table.
  ///
  /// \param name The Swift name of the entry.
  /// \param newEntry The Clang declaration or macro.
  /// \param effectiveContext The effective context in which name lookup occurs.
  void addEntry(DeclName name, SingleEntry newEntry,
                EffectiveClangContext effectiveContext,
                const clang::Preprocessor *PP = nullptr);

  /// Add an Objective-C category or extension to the table.
  void addCategory(clang::ObjCCategoryDecl *category);

  /// Resolve any unresolved entries.
  ///
  /// \param unresolved Will be populated with the list of entries
  /// that could not be resolved.
  ///
  /// \returns true if any remaining entries could not be resolved,
  /// and false otherwise.
  bool resolveUnresolvedEntries(SmallVectorImpl<SingleEntry> &unresolved);

private:
  /// Lookup the set of entities with the given base name.
  ///
  /// \param baseName The base name to search for. All results will
  /// have this base name.
  ///
  /// \param searchContext The context in which the resulting set of
  /// entities should reside. This may be None to indicate that
  /// all results from all contexts should be produced.
  SmallVector<SingleEntry, 4>
  lookup(StringRef baseName, llvm::Optional<StoredContext> searchContext);

  /// Retrieve the set of global declarations that are going to be
  /// imported as members into the given context.
  SmallVector<SingleEntry, 4> lookupGlobalsAsMembers(StoredContext context);

public:
  /// Lookup an unresolved context name and resolve it to a Clang
  /// named declaration.
  clang::NamedDecl *resolveContext(StringRef unresolvedName);

  /// Lookup the set of entities with the given base name.
  ///
  /// \param baseName The base name to search for. All results will
  /// have this base name.
  ///
  /// \param searchContext The context in which the resulting set of
  /// entities should reside. This may be None to indicate that
  /// all results from all contexts should be produced.
  SmallVector<SingleEntry, 4>
  lookup(StringRef baseName, EffectiveClangContext searchContext);

  /// Retrieve the set of base names that are stored in the lookup table.
  SmallVector<StringRef, 4> allBaseNames();

  /// Lookup Objective-C members with the given base name, regardless
  /// of context.
  SmallVector<clang::NamedDecl *, 4> lookupObjCMembers(StringRef baseName);

  /// Retrieve the set of Objective-C categories and extensions.
  ArrayRef<clang::ObjCCategoryDecl *> categories();

  /// Retrieve the set of global declarations that are going to be
  /// imported as members into the given context.
  SmallVector<SingleEntry, 4>
  lookupGlobalsAsMembers(EffectiveClangContext context);

  /// Retrieve the set of global declarations that are going to be
  /// imported as members.
  SmallVector<SingleEntry, 4> allGlobalsAsMembers();

  /// Deserialize all entries.
  void deserializeAll();

  /// Dump the internal representation of this lookup table.
  void dump() const;
};

namespace importer {
class NameImporter;

/// Add the given named declaration as an entry to the given Swift name
/// lookup table, including any of its child entries.
void addEntryToLookupTable(SwiftLookupTable &table, clang::NamedDecl *named,
                           NameImporter &);

/// Add the macros from the given Clang preprocessor to the given
/// Swift name lookup table.
void addMacrosToLookupTable(SwiftLookupTable &table, NameImporter &);

/// Finalize a lookup table, handling any as-yet-unresolved entries
/// and emitting diagnostics if necessary.
void finalizeLookupTable(SwiftLookupTable &table, NameImporter &);
}
}

namespace llvm {

template <> struct DenseMapInfo<swift::SwiftLookupTable::ContextKind> {
  typedef swift::SwiftLookupTable::ContextKind ContextKind;
  static ContextKind getEmptyKey() {
    return static_cast<ContextKind>(0);
  }
  static ContextKind getTombstoneKey() {
    return static_cast<ContextKind>(1);
  }
  static unsigned getHashValue(ContextKind kind) {
    return static_cast<unsigned>(kind);
  }
  static bool isEqual(ContextKind lhs, ContextKind rhs) {
    return lhs == rhs;
  }
};

}

#endif // SWIFT_CLANGIMPORTER_SWIFTLOOKUPTABLE_H
