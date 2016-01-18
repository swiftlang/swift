//===--- SwiftLookupTable.cpp - Swift Lookup Table ------------------------===//
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
#include "SwiftLookupTable.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Basic/Version.h"
#include "clang/AST/DeclObjC.h"
#include "clang/Serialization/ASTBitCodes.h"
#include "clang/Serialization/ASTReader.h"
#include "clang/Serialization/ASTWriter.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Bitcode/BitstreamReader.h"
#include "llvm/Bitcode/BitstreamWriter.h"
#include "llvm/Bitcode/RecordLayout.h"
#include "llvm/Support/OnDiskHashTable.h"

using namespace swift;
using namespace llvm::support;

/// Determine whether the new declarations matches an existing declaration.
static bool matchesExistingDecl(clang::Decl *decl, clang::Decl *existingDecl) {
  // If the canonical declarations are equivalent, we have a match.
  if (decl->getCanonicalDecl() == existingDecl->getCanonicalDecl()) {
    return true;
  }

  return false;
}

bool SwiftLookupTable::contextRequiresName(ContextKind kind) {
  switch (kind) {
  case ContextKind::ObjCClass:
  case ContextKind::ObjCProtocol:
  case ContextKind::Tag:
    return true;

  case ContextKind::TranslationUnit:
    return false;
  }
}

Optional<std::pair<SwiftLookupTable::ContextKind, StringRef>>
SwiftLookupTable::translateContext(clang::DeclContext *context) {
  // Translation unit context.
  if (context->isTranslationUnit())
    return std::make_pair(ContextKind::TranslationUnit, StringRef());

  // Tag declaration context.
  if (auto tag = dyn_cast<clang::TagDecl>(context)) {
    if (tag->getIdentifier())
      return std::make_pair(ContextKind::Tag, tag->getName());
    if (auto typedefDecl = tag->getTypedefNameForAnonDecl())
      return std::make_pair(ContextKind::Tag, typedefDecl->getName());
    return None;
  }

  // Objective-C class context.
  if (auto objcClass = dyn_cast<clang::ObjCInterfaceDecl>(context))
    return std::make_pair(ContextKind::ObjCClass, objcClass->getName());

  // Objective-C protocol context.
  if (auto objcProtocol = dyn_cast<clang::ObjCProtocolDecl>(context))
    return std::make_pair(ContextKind::ObjCProtocol, objcProtocol->getName());

  return None;
}

void SwiftLookupTable::addCategory(clang::ObjCCategoryDecl *category) {
  assert(!Reader && "Cannot modify a lookup table stored on disk");

  // Add the category.
  Categories.push_back(category);
}

void SwiftLookupTable::addEntry(DeclName name, SingleEntry newEntry,
                                clang::DeclContext *effectiveContext) {
  assert(!Reader && "Cannot modify a lookup table stored on disk");

  // Translate the context.
  auto contextOpt = translateContext(effectiveContext);
  if (!contextOpt) return;
  auto context = *contextOpt;

  // Find the list of entries for this base name.
  auto &entries = LookupTable[name.getBaseName().str()];
  auto decl = newEntry.dyn_cast<clang::NamedDecl *>();
  auto macro = newEntry.dyn_cast<clang::MacroInfo *>();
  for (auto &entry : entries) {
    if (entry.Context == context) {
      // We have entries for this context.

      // Check whether this entry matches any existing entry.
      for (auto &existingEntry : entry.DeclsOrMacros) {
        // If it matches an existing declaration, there's nothing to do.
        if (decl && isDeclEntry(existingEntry) && 
            matchesExistingDecl(decl, mapStoredDecl(existingEntry)))
          return;

        // If it matches an existing macro, overwrite the existing entry.
        if (macro && isMacroEntry(existingEntry)) {
          existingEntry = encodeEntry(macro);
          return;
        }
      }

      // Add an entry to this context.
      if (decl)
        entry.DeclsOrMacros.push_back(encodeEntry(decl));
      else
        entry.DeclsOrMacros.push_back(encodeEntry(macro));
      return;
    }
  }

  // This is a new context for this name. Add it.
  FullTableEntry entry;
  entry.Context = context;
  if (decl)
    entry.DeclsOrMacros.push_back(encodeEntry(decl));
  else
    entry.DeclsOrMacros.push_back(encodeEntry(macro));
  entries.push_back(entry);
}


auto SwiftLookupTable::findOrCreate(StringRef baseName) 
  -> llvm::DenseMap<StringRef, SmallVector<FullTableEntry, 2>>::iterator {
  // If there is no base name, there is nothing to find.
  if (baseName.empty()) return LookupTable.end();

  // Find entries for this base name.
  auto known = LookupTable.find(baseName);

  // If we found something, we're done.
  if (known != LookupTable.end()) return known;
  
  // If there's no reader, we've found all there is to find.
  if (!Reader) return known;

  // Add an entry to the table so we don't look again.
  known = LookupTable.insert({ baseName, { } }).first;

  // Lookup this base name in the module file.
  (void)Reader->lookup(baseName, known->second);

  return known;
}

SmallVector<SwiftLookupTable::SingleEntry, 4>
SwiftLookupTable::lookup(StringRef baseName,
                         clang::DeclContext *searchContext) {
  SmallVector<SwiftLookupTable::SingleEntry, 4> result;

  // Find the lookup table entry for this base name.
  auto known = findOrCreate(baseName);
  if (known == LookupTable.end()) return result;

  // Translate context.
  Optional<std::pair<SwiftLookupTable::ContextKind, StringRef>> context;
  if (searchContext) {
    context = translateContext(searchContext);
    if (!context) return result;
  }

  // Walk each of the entries.
  for (auto &entry : known->second) {
    // If we're looking in a particular context and it doesn't match the
    // entry context, we're done.
    if (context && *context != entry.Context) continue;

    // Map each of the declarations.
    for (auto &stored : entry.DeclsOrMacros)
      result.push_back(mapStored(stored));
  }

  return result;
}

SmallVector<StringRef, 4> SwiftLookupTable::allBaseNames() {
  // If we have a reader, enumerate its base names.
  if (Reader) return Reader->getBaseNames();

  // Otherwise, walk the lookup table.
  SmallVector<StringRef, 4> result;
  for (const auto &entry : LookupTable) {
    result.push_back(entry.first);
  }
  return result;
}

SmallVector<clang::NamedDecl *, 4>
SwiftLookupTable::lookupObjCMembers(StringRef baseName) {
  SmallVector<clang::NamedDecl *, 4> result;

  // Find the lookup table entry for this base name.
  auto known = findOrCreate(baseName);
  if (known == LookupTable.end()) return result;

  // Walk each of the entries.
  for (auto &entry : known->second) {
    // If we're looking in a particular context and it doesn't match the
    // entry context, we're done.
    switch (entry.Context.first) {
    case ContextKind::TranslationUnit:
    case ContextKind::Tag:
      continue;

    case ContextKind::ObjCClass:
    case ContextKind::ObjCProtocol:
      break;
    }

    // Map each of the declarations.
    for (auto &stored : entry.DeclsOrMacros) {
      assert(isDeclEntry(stored) && "Not a declaration?");
      result.push_back(mapStoredDecl(stored));
    }
  }

  return result;
}

ArrayRef<clang::ObjCCategoryDecl *> SwiftLookupTable::categories() {
  if (!Categories.empty() || !Reader) return Categories;

  // Map categories known to the reader.
  for (auto declID : Reader->categories()) {
    auto category =
      cast_or_null<clang::ObjCCategoryDecl>(
        Reader->getASTReader().GetLocalDecl(Reader->getModuleFile(), declID));
    if (category)
      Categories.push_back(category);

  }

  return Categories;
}

static void printName(clang::NamedDecl *named, llvm::raw_ostream &out) {
  // If there is a name, print it.
  if (!named->getDeclName().isEmpty()) {
    // If we have an Objective-C method, print the class name along
    // with '+'/'-'.
    if (auto objcMethod = dyn_cast<clang::ObjCMethodDecl>(named)) {
      out << (objcMethod->isInstanceMethod() ? '-' : '+') << '[';
      if (auto classDecl = objcMethod->getClassInterface()) {
        classDecl->printName(out);
        out << ' ';
      } else if (auto proto = dyn_cast<clang::ObjCProtocolDecl>(
                                objcMethod->getDeclContext())) {
        proto->printName(out);
        out << ' ';
      }
      named->printName(out);
      out << ']';
      return;
    }

    // If we have an Objective-C property, print the class name along
    // with the property name.
    if (auto objcProperty = dyn_cast<clang::ObjCPropertyDecl>(named)) {
      auto dc = objcProperty->getDeclContext();
      if (auto classDecl = dyn_cast<clang::ObjCInterfaceDecl>(dc)) {
        classDecl->printName(out);
        out << '.';
      } else if (auto categoryDecl = dyn_cast<clang::ObjCCategoryDecl>(dc)) {
        categoryDecl->getClassInterface()->printName(out);
        out << '.';
      } else if (auto proto = dyn_cast<clang::ObjCProtocolDecl>(dc)) {
        proto->printName(out);
        out << '.';
      }
      named->printName(out);
      return;
    }

    named->printName(out);
    return;
  }

  // If this is an anonymous tag declaration with a typedef name, use that.
  if (auto tag = dyn_cast<clang::TagDecl>(named)) {
    if (auto typedefName = tag->getTypedefNameForAnonDecl()) {
      printName(typedefName, out);
      return;
    }
  }
}

void SwiftLookupTable::deserializeAll() {
  if (!Reader) return;

  for (auto baseName : Reader->getBaseNames()) {
    (void)lookup(baseName, nullptr);
  }

  (void)categories();
}

void SwiftLookupTable::dump() const {
  // Dump the base name -> full table entry mappings.
  SmallVector<StringRef, 4> baseNames;
  for (const auto &entry : LookupTable) {
    baseNames.push_back(entry.first);
  }
  llvm::array_pod_sort(baseNames.begin(), baseNames.end());
  llvm::errs() << "Base name -> entry mappings:\n";
  for (auto baseName : baseNames) {
    llvm::errs() << "  " << baseName << ":\n";
    const auto &entries = LookupTable.find(baseName)->second;
    for (const auto &entry : entries) {
      llvm::errs() << "    ";
      switch (entry.Context.first) {
      case ContextKind::TranslationUnit:
        llvm::errs() << "TU";
        break;

      case ContextKind::Tag:
      case ContextKind::ObjCClass:
      case ContextKind::ObjCProtocol:
        llvm::errs() << entry.Context.second;
      }
      llvm::errs() << ": ";

      interleave(entry.DeclsOrMacros.begin(), entry.DeclsOrMacros.end(),
                 [this](uintptr_t entry) {
                   if (isSerializationIDEntry(entry)) {
                     llvm::errs() << (isMacroEntry(entry) ? "macro" : "decl")
                                  << " ID #" << getSerializationID(entry);
                   } else if (isMacroEntry(entry)) {
                     llvm::errs() << "Macro";
                   } else {
                     auto decl = const_cast<SwiftLookupTable *>(this)
                                   ->mapStoredDecl(entry);
                     printName(decl, llvm::errs());
                   }
                 },
                 [] {
                   llvm::errs() << ", ";
                 });
      llvm::errs() << "\n";
    }
  }

  if (!Categories.empty()) {
    llvm::errs() << "Categories: ";
    interleave(Categories.begin(), Categories.end(),
               [](clang::ObjCCategoryDecl *category) {
                 llvm::errs() << category->getClassInterface()->getName()
                              << "(" << category->getName() << ")";
               },
               [] {
                 llvm::errs() << ", ";
               });
    llvm::errs() << "\n";
  } else if (Reader && !Reader->categories().empty()) {
    llvm::errs() << "Categories: ";
    interleave(Reader->categories().begin(), Reader->categories().end(),
               [](clang::serialization::DeclID declID) {
                 llvm::errs() << "decl ID #" << declID;
               },
               [] {
                 llvm::errs() << ", ";
               });
    llvm::errs() << "\n";
  }
}

// ---------------------------------------------------------------------------
// Serialization
// ---------------------------------------------------------------------------
using llvm::Fixnum;
using llvm::BCArray;
using llvm::BCBlob;
using llvm::BCFixed;
using llvm::BCGenericRecordLayout;
using llvm::BCRecordLayout;
using llvm::BCVBR;

namespace {
  enum RecordTypes {
    /// Record that contains the mapping from base names to entities with that
    /// name.
    BASE_NAME_TO_ENTITIES_RECORD_ID
      = clang::serialization::FIRST_EXTENSION_RECORD_ID,

    /// Record that contains the list of Objective-C category/extension IDs.
    CATEGORIES_RECORD_ID
  };

  using BaseNameToEntitiesTableRecordLayout
    = BCRecordLayout<BASE_NAME_TO_ENTITIES_RECORD_ID, BCVBR<16>, BCBlob>;

  using CategoriesRecordLayout
    = llvm::BCRecordLayout<CATEGORIES_RECORD_ID, BCBlob>;

  /// Trait used to write the on-disk hash table for the base name -> entities
  /// mapping.
  class BaseNameToEntitiesTableWriterInfo {
    SwiftLookupTable &Table;
    clang::ASTWriter &Writer;

  public:
    using key_type = StringRef;
    using key_type_ref = key_type;
    using data_type = SmallVector<SwiftLookupTable::FullTableEntry, 2>;
    using data_type_ref = data_type &;
    using hash_value_type = uint32_t;
    using offset_type = unsigned;

    BaseNameToEntitiesTableWriterInfo(SwiftLookupTable &table,
                                      clang::ASTWriter &writer)
      : Table(table), Writer(writer)
    {
    }

    hash_value_type ComputeHash(key_type_ref key) {
      return llvm::HashString(key);
    }

    std::pair<unsigned, unsigned> EmitKeyDataLength(raw_ostream &out,
                                                    key_type_ref key,
                                                    data_type_ref data) {
      // The length of the key.
      uint32_t keyLength = key.size();

      // # of entries
      uint32_t dataLength = sizeof(uint16_t);

      // Storage per entry.
      for (const auto &entry : data) {
        // Context info.
        dataLength += 1;
        if (SwiftLookupTable::contextRequiresName(entry.Context.first)) {
          dataLength += sizeof(uint16_t) + entry.Context.second.size();
        }

        // # of entries.
        dataLength += sizeof(uint16_t);

        // Actual entries.
        dataLength += (sizeof(clang::serialization::DeclID) *
                       entry.DeclsOrMacros.size());
      }

      endian::Writer<little> writer(out);
      writer.write<uint16_t>(keyLength);
      writer.write<uint16_t>(dataLength);
      return { keyLength, dataLength };
    }

    void EmitKey(raw_ostream &out, key_type_ref key, unsigned len) {
      out << key;
    }

    void EmitData(raw_ostream &out, key_type_ref key, data_type_ref data,
                  unsigned len) {
      endian::Writer<little> writer(out);

      // # of entries
      writer.write<uint16_t>(data.size());

      for (auto &fullEntry : data) {
        // Context.
        writer.write<uint8_t>(static_cast<uint8_t>(fullEntry.Context.first));
        if (SwiftLookupTable::contextRequiresName(fullEntry.Context.first)) {
          writer.write<uint16_t>(fullEntry.Context.second.size());
          out << fullEntry.Context.second;
        }

        // # of entries.
        writer.write<uint16_t>(fullEntry.DeclsOrMacros.size());

        // Write the declarations and macros.
        for (auto &entry : fullEntry.DeclsOrMacros) {
          uint32_t id;
          if (SwiftLookupTable::isDeclEntry(entry)) {
            auto decl = Table.mapStoredDecl(entry);
            id = (Writer.getDeclID(decl) << 2) | 0x02;
          } else {
            auto macro = Table.mapStoredMacro(entry);
            id = (Writer.getMacroID(macro) << 2) | 0x02 | 0x01;
          }
          writer.write<uint32_t>(id);
        }
      }
    }
  };
}

void SwiftLookupTableWriter::writeExtensionContents(
       clang::Sema &sema,
       llvm::BitstreamWriter &stream) {
  // Populate the lookup table.
  SwiftLookupTable table(nullptr);
  PopulateTable(sema, table);

  SmallVector<uint64_t, 64> ScratchRecord;

  // First, gather the sorted list of base names.
  SmallVector<StringRef, 2> baseNames;
  for (const auto &entry : table.LookupTable)
    baseNames.push_back(entry.first);
  llvm::array_pod_sort(baseNames.begin(), baseNames.end());

  // Form the mapping from base names to entities with their context.
  {
    llvm::SmallString<4096> hashTableBlob;
    uint32_t tableOffset;
    {
      llvm::OnDiskChainedHashTableGenerator<BaseNameToEntitiesTableWriterInfo>
        generator;
      BaseNameToEntitiesTableWriterInfo info(table, Writer);
      for (auto baseName : baseNames)
        generator.insert(baseName, table.LookupTable[baseName], info);

      llvm::raw_svector_ostream blobStream(hashTableBlob);
      // Make sure that no bucket is at offset 0
      endian::Writer<little>(blobStream).write<uint32_t>(0);
      tableOffset = generator.Emit(blobStream, info);
    }

    BaseNameToEntitiesTableRecordLayout layout(stream);
    layout.emit(ScratchRecord, tableOffset, hashTableBlob);
  }

  // Write the categories, if there are any.
  if (!table.Categories.empty()) {
    SmallVector<clang::serialization::DeclID, 4> categoryIDs;
    for (auto category : table.Categories) {
      categoryIDs.push_back(Writer.getDeclID(category));
    }

    StringRef blob(reinterpret_cast<const char *>(categoryIDs.data()),
                   categoryIDs.size() * sizeof(clang::serialization::DeclID));
    CategoriesRecordLayout layout(stream);
    layout.emit(ScratchRecord, blob);
  }
}

namespace {
  /// Used to deserialize the on-disk base name -> entities table.
  class BaseNameToEntitiesTableReaderInfo {
  public:
    using internal_key_type = StringRef;
    using external_key_type = internal_key_type;
    using data_type = SmallVector<SwiftLookupTable::FullTableEntry, 2>;
    using hash_value_type = uint32_t;
    using offset_type = unsigned;

    internal_key_type GetInternalKey(external_key_type key) {
      return key;
    }

    external_key_type GetExternalKey(internal_key_type key) {
      return key;
    }

    hash_value_type ComputeHash(internal_key_type key) {
      return llvm::HashString(key);
    }

    static bool EqualKey(internal_key_type lhs, internal_key_type rhs) {
      return lhs == rhs;
    }

    static std::pair<unsigned, unsigned>
    ReadKeyDataLength(const uint8_t *&data) {
      unsigned keyLength = endian::readNext<uint16_t, little, unaligned>(data);
      unsigned dataLength = endian::readNext<uint16_t, little, unaligned>(data);
      return { keyLength, dataLength };
    }

    static internal_key_type ReadKey(const uint8_t *data, unsigned length) {
      return StringRef((const char *)data, length);
    }

    static data_type ReadData(internal_key_type key, const uint8_t *data,
                              unsigned length) {
      data_type result;

      // # of entries.
      unsigned numEntries = endian::readNext<uint16_t, little, unaligned>(data);
      result.reserve(numEntries);

      // Read all of the entries.
      while (numEntries--) {
        SwiftLookupTable::FullTableEntry entry;

        // Read the context.
        entry.Context.first =
          static_cast<SwiftLookupTable::ContextKind>(
            endian::readNext<uint8_t, little, unaligned>(data));
        if (SwiftLookupTable::contextRequiresName(entry.Context.first)) {
          uint16_t length = endian::readNext<uint16_t, little, unaligned>(data);
          entry.Context.second = StringRef((const char *)data, length);
          data += length;
        }

        // Read the declarations and macros.
        unsigned numDeclsOrMacros =
          endian::readNext<uint16_t, little, unaligned>(data);
        while (numDeclsOrMacros--) {
          auto id = endian::readNext<uint32_t, little, unaligned>(data);
          entry.DeclsOrMacros.push_back(id);
        }

        result.push_back(entry);
      }

      return result;
    }
  };

}

namespace swift {
  using SerializedBaseNameToEntitiesTable =
    llvm::OnDiskIterableChainedHashTable<BaseNameToEntitiesTableReaderInfo>;
}

clang::NamedDecl *SwiftLookupTable::mapStoredDecl(uintptr_t &entry) {
  assert(isDeclEntry(entry) && "Not a declaration entry");

  // If we have an AST node here, just cast it.
  if (isASTNodeEntry(entry)) {
    return static_cast<clang::NamedDecl *>(getPointerFromEntry(entry));
  }

  // Otherwise, resolve the declaration.
  assert(Reader && "Cannot resolve the declaration without a reader");
  clang::serialization::DeclID declID = getSerializationID(entry);
  auto decl = cast_or_null<clang::NamedDecl>(
                Reader->getASTReader().GetLocalDecl(Reader->getModuleFile(),
                                                    declID));

  // Update the entry now that we've resolved the declaration.
  entry = encodeEntry(decl);
  return decl;
}

clang::MacroInfo *SwiftLookupTable::mapStoredMacro(uintptr_t &entry) {
  assert(isMacroEntry(entry) && "Not a macro entry");

  // If we have an AST node here, just cast it.
  if (isASTNodeEntry(entry)) {
    return static_cast<clang::MacroInfo *>(getPointerFromEntry(entry));
  }

  // Otherwise, resolve the macro.
  assert(Reader && "Cannot resolve the macro without a reader");
  clang::serialization::MacroID macroID = getSerializationID(entry);
  auto macro = cast_or_null<clang::MacroInfo>(
                Reader->getASTReader().getMacro(
                  Reader->getASTReader().getGlobalMacroID(
                    Reader->getModuleFile(),
                    macroID)));

  // Update the entry now that we've resolved the macro.
  entry = encodeEntry(macro);
  return macro;
}

SwiftLookupTable::SingleEntry SwiftLookupTable::mapStored(uintptr_t &entry) {
  if (isDeclEntry(entry))
    return mapStoredDecl(entry);

  return mapStoredMacro(entry);
}

SwiftLookupTableReader::~SwiftLookupTableReader() {
  OnRemove();
  delete static_cast<SerializedBaseNameToEntitiesTable *>(SerializedTable);
}

std::unique_ptr<SwiftLookupTableReader>
SwiftLookupTableReader::create(clang::ModuleFileExtension *extension,
                               clang::ASTReader &reader,
                               clang::serialization::ModuleFile &moduleFile,
                               std::function<void()> onRemove,
                               const llvm::BitstreamCursor &stream)
{
  // Look for the base name -> entities table record.
  SmallVector<uint64_t, 64> scratch;
  auto cursor = stream;
  auto next = cursor.advance();
  std::unique_ptr<SerializedBaseNameToEntitiesTable> serializedTable;
  ArrayRef<clang::serialization::DeclID> categories;
  while (next.Kind != llvm::BitstreamEntry::EndBlock) {
    if (next.Kind == llvm::BitstreamEntry::Error)
      return nullptr;

    if (next.Kind == llvm::BitstreamEntry::SubBlock) {
      // Unknown sub-block, possibly for use by a future version of the
      // API notes format.
      if (cursor.SkipBlock())
        return nullptr;
      
      next = cursor.advance();
      continue;
    }

    scratch.clear();
    StringRef blobData;
    unsigned kind = cursor.readRecord(next.ID, scratch, &blobData);
    switch (kind) {
    case BASE_NAME_TO_ENTITIES_RECORD_ID: {
      // Already saw base name -> entities table.
      if (serializedTable)
        return nullptr;

      uint32_t tableOffset;
      BaseNameToEntitiesTableRecordLayout::readRecord(scratch, tableOffset);
      auto base = reinterpret_cast<const uint8_t *>(blobData.data());

      serializedTable.reset(
        SerializedBaseNameToEntitiesTable::Create(base + tableOffset,
                                                  base + sizeof(uint32_t),
                                                  base));
      break;
    }

    case CATEGORIES_RECORD_ID: {
      // Already saw categories; input is malformed.
      if (!categories.empty()) return nullptr;

      auto start =
        reinterpret_cast<const clang::serialization::DeclID *>(blobData.data());
      unsigned numElements
        = blobData.size() / sizeof(clang::serialization::DeclID);
      categories = llvm::makeArrayRef(start, numElements);
      break;
    }

    default:
      // Unknown record, possibly for use by a future version of the
      // module format.
      break;
    }

    next = cursor.advance();
  }

  if (!serializedTable) return nullptr;

  // Create the reader.
  return std::unique_ptr<SwiftLookupTableReader>(
           new SwiftLookupTableReader(extension, reader, moduleFile, onRemove,
                                      serializedTable.release(), categories));

}

SmallVector<StringRef, 4> SwiftLookupTableReader::getBaseNames() {
  auto table = static_cast<SerializedBaseNameToEntitiesTable*>(SerializedTable);
  SmallVector<StringRef, 4> results;
  for (auto key : table->keys()) {
    results.push_back(key);
  }
  return results;
}

/// Retrieve the set of entries associated with the given base name.
///
/// \returns true if we found anything, false otherwise.
bool SwiftLookupTableReader::lookup(
       StringRef baseName,
       SmallVectorImpl<SwiftLookupTable::FullTableEntry> &entries) {
  auto table = static_cast<SerializedBaseNameToEntitiesTable*>(SerializedTable);

  // Look for an entry with this base name.
  auto known = table->find(baseName);
  if (known == table->end()) return false;

  // Grab the results.
  entries = std::move(*known);
  return true;
}

