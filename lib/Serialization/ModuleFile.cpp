//===--- ModuleFile.cpp - Loading a serialized module ---------------------===//
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

#include "swift/Serialization/ModuleFile.h"
#include "swift/Serialization/ModuleFormat.h"
#include "swift/Subsystems.h"
#include "swift/AST/AST.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Basic/Range.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/Serialization/BCReadingExtras.h"
#include "swift/Serialization/SerializedModuleLoader.h"

#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/OnDiskHashTable.h"
#include "llvm/Support/PrettyStackTrace.h"

using namespace swift;
using namespace swift::serialization;
using namespace llvm::support;

static bool checkModuleSignature(llvm::BitstreamCursor &cursor) {
  for (unsigned char byte : MODULE_SIGNATURE)
    if (cursor.AtEndOfStream() || cursor.Read(8) != byte)
      return false;
  return true;
}

static bool checkModuleDocSignature(llvm::BitstreamCursor &cursor) {
  for (unsigned char byte : MODULE_DOC_SIGNATURE)
    if (cursor.AtEndOfStream() || cursor.Read(8) != byte)
      return false;
  return true;
}

static bool enterTopLevelModuleBlock(llvm::BitstreamCursor &cursor,
                                     unsigned ID,
                                     bool shouldReadBlockInfo = true) {
  auto next = cursor.advance();

  if (next.Kind != llvm::BitstreamEntry::SubBlock)
    return false;

  if (next.ID == llvm::bitc::BLOCKINFO_BLOCK_ID) {
    if (shouldReadBlockInfo) {
      if (!cursor.ReadBlockInfoBlock())
        return false;
    } else {
      if (cursor.SkipBlock())
        return false;
    }
    return enterTopLevelModuleBlock(cursor, ID, false);
  }

  if (next.ID != ID)
    return false;

  cursor.EnterSubBlock(ID);
  return true;
}

/// Populate \p extendedInfo with the data from the options block.
///
/// Returns true on success.
static bool readOptionsBlock(llvm::BitstreamCursor &cursor,
                             SmallVectorImpl<uint64_t> &scratch,
                             ExtendedValidationInfo &extendedInfo) {
  while (!cursor.AtEndOfStream()) {
    auto entry = cursor.advance();
    if (entry.Kind == llvm::BitstreamEntry::EndBlock)
      break;

    if (entry.Kind == llvm::BitstreamEntry::Error)
      return false;

    if (entry.Kind == llvm::BitstreamEntry::SubBlock) {
      // Unknown metadata sub-block, possibly for use by a future version of
      // the module format.
      if (cursor.SkipBlock())
        return false;
      continue;
    }

    scratch.clear();
    StringRef blobData;
    unsigned kind = cursor.readRecord(entry.ID, scratch, &blobData);
    switch (kind) {
    case options_block::SDK_PATH:
      extendedInfo.setSDKPath(blobData);
      break;
    case options_block::XCC:
      extendedInfo.addExtraClangImporterOption(blobData);
      break;
    case options_block::IS_SIB:
      bool IsSIB;
      options_block::IsSIBLayout::readRecord(scratch, IsSIB);
      extendedInfo.setIsSIB(IsSIB);
      break;
    case options_block::IS_TESTABLE:
      extendedInfo.setIsTestable(true);
      break;
    case options_block::RESILIENCE_STRATEGY:
      unsigned Strategy;
      options_block::ResilienceStrategyLayout::readRecord(scratch, Strategy);
      extendedInfo.setResilienceStrategy(ResilienceStrategy(Strategy));
      break;
    default:
      // Unknown options record, possibly for use by a future version of the
      // module format.
      break;
    }
  }

  return true;
}

static ValidationInfo
validateControlBlock(llvm::BitstreamCursor &cursor,
                     SmallVectorImpl<uint64_t> &scratch,
                     ExtendedValidationInfo *extendedInfo) {
  // The control block is malformed until we've at least read a major version
  // number.
  ValidationInfo result;
  bool versionSeen = false;

  while (!cursor.AtEndOfStream()) {
    auto entry = cursor.advance();
    if (entry.Kind == llvm::BitstreamEntry::EndBlock)
      break;

    if (entry.Kind == llvm::BitstreamEntry::Error) {
      result.status = Status::Malformed;
      return result;
    }

    if (entry.Kind == llvm::BitstreamEntry::SubBlock) {
      if (entry.ID == OPTIONS_BLOCK_ID && extendedInfo) {
        cursor.EnterSubBlock(OPTIONS_BLOCK_ID);
        if (!readOptionsBlock(cursor, scratch, *extendedInfo)) {
          result.status = Status::Malformed;
          return result;
        }
      } else {
        // Unknown metadata sub-block, possibly for use by a future version of
        // the module format.
        if (cursor.SkipBlock()) {
          result.status = Status::Malformed;
          return result;
        }
      }
      continue;
    }

    scratch.clear();
    StringRef blobData;
    unsigned kind = cursor.readRecord(entry.ID, scratch, &blobData);
    switch (kind) {
    case control_block::METADATA: {
      if (versionSeen) {
        result.status = Status::Malformed;
        break;
      }

      uint16_t versionMajor = scratch[0];
      if (versionMajor > VERSION_MAJOR)
        result.status = Status::FormatTooNew;
      else if (versionMajor < VERSION_MAJOR)
        result.status = Status::FormatTooOld;
      else
        result.status = Status::Valid;

      // Major version 0 does not have stable minor versions.
      if (versionMajor == 0) {
        uint16_t versionMinor = scratch[1];
        if (versionMinor != VERSION_MINOR) {
          if (versionMinor < VERSION_MINOR)
            result.status = Status::FormatTooOld;
          else
            result.status = Status::FormatTooNew;
        }
      }

      // This field was added later; be resilient against its absence.
      if (scratch.size() > 2) {
        result.shortVersion = blobData.slice(0, scratch[2]);
      }

      versionSeen = true;
      break;
    }
    case control_block::MODULE_NAME:
      result.name = blobData;
      break;
    case control_block::TARGET:
      result.targetTriple = blobData;
      break;
    default:
      // Unknown metadata record, possibly for use by a future version of the
      // module format.
      break;
    }
  }

  return result;
}

bool serialization::isSerializedAST(StringRef data) {
  StringRef signatureStr(reinterpret_cast<const char *>(MODULE_SIGNATURE),
                         llvm::array_lengthof(MODULE_SIGNATURE));
  return data.startswith(signatureStr);
}

ValidationInfo serialization::validateSerializedAST(
    StringRef data,
    ExtendedValidationInfo *extendedInfo) {
  ValidationInfo result;

  // Check 32-bit alignment.
  if (data.size() % 4 != 0 ||
      reinterpret_cast<uintptr_t>(data.data()) % 4 != 0)
    return result;

  llvm::BitstreamCursor cursor(data);
  SmallVector<uint64_t, 32> scratch;

  if (!checkModuleSignature(cursor) ||
      !enterTopLevelModuleBlock(cursor, MODULE_BLOCK_ID, false))
    return result;

  llvm::BitstreamEntry topLevelEntry;

  while (!cursor.AtEndOfStream()) {
    topLevelEntry = cursor.advance(AF_DontPopBlockAtEnd);
    if (topLevelEntry.Kind != llvm::BitstreamEntry::SubBlock)
      break;

    if (topLevelEntry.ID == CONTROL_BLOCK_ID) {
      cursor.EnterSubBlock(CONTROL_BLOCK_ID);
      result = validateControlBlock(cursor, scratch, extendedInfo);
      if (result.status == Status::Malformed)
        return result;
    } else {
      if (cursor.SkipBlock()) {
        result.status = Status::Malformed;
        return result;
      }
    }
  }

  if (topLevelEntry.Kind == llvm::BitstreamEntry::EndBlock) {
    cursor.ReadBlockEnd();
    assert(cursor.GetCurrentBitNo() % CHAR_BIT == 0);
    result.bytes = cursor.GetCurrentBitNo() / CHAR_BIT;
  } else {
    result.status = Status::Malformed;
  }

  return result;
}

std::string ModuleFile::Dependency::getPrettyPrintedPath() const {
  StringRef pathWithoutScope = RawPath;
  if (isScoped()) {
    size_t splitPoint = pathWithoutScope.find_last_of('\0');
    pathWithoutScope = pathWithoutScope.slice(0, splitPoint);
  }
  std::string output = pathWithoutScope.str();
  std::replace(output.begin(), output.end(), '\0', '.');
  return output;
}

namespace {
  class PrettyModuleFileDeserialization : public llvm::PrettyStackTraceEntry {
    const ModuleFile &File;
  public:
    explicit PrettyModuleFileDeserialization(const ModuleFile &file)
        : File(file) {}

    void print(raw_ostream &os) const override {
      os << "While reading from " << File.getModuleFilename() << "\n";
    }
  };
} // end anonymous namespace

/// Used to deserialize entries in the on-disk decl hash table.
class ModuleFile::DeclTableInfo {
public:
  using internal_key_type = std::pair<DeclBaseName::Kind, StringRef>;
  using external_key_type = DeclBaseName;
  using data_type = SmallVector<std::pair<uint8_t, DeclID>, 8>;
  using hash_value_type = uint32_t;
  using offset_type = unsigned;

  internal_key_type GetInternalKey(external_key_type ID) {
    if (ID.getKind() == DeclBaseName::Kind::Normal) {
      return {DeclBaseName::Kind::Normal, ID.getIdentifier().str()};
    } else {
      return {ID.getKind(), StringRef()};
    }
  }

  hash_value_type ComputeHash(internal_key_type key) {
    if (key.first == DeclBaseName::Kind::Normal) {
      return llvm::HashString(key.second);
    } else {
      return (hash_value_type)key.first;
    }
  }

  static bool EqualKey(internal_key_type lhs, internal_key_type rhs) {
    return lhs == rhs;
  }

  static std::pair<unsigned, unsigned> ReadKeyDataLength(const uint8_t *&data) {
    unsigned keyLength = endian::readNext<uint16_t, little, unaligned>(data);
    unsigned dataLength = endian::readNext<uint16_t, little, unaligned>(data);
    return { keyLength, dataLength };
  }

  static internal_key_type ReadKey(const uint8_t *data, unsigned length) {
    uint8_t kind = endian::readNext<uint8_t, little, unaligned>(data);
    switch (kind) {
    case (uint8_t)DeclBaseName::Kind::Normal: {
      StringRef str(reinterpret_cast<const char *>(data),
                    length - sizeof(uint8_t));
      return {DeclBaseName::Kind::Normal, str};
    }
    case (uint8_t)DeclBaseName::Kind::Subscript:
      return {DeclBaseName::Kind::Subscript, StringRef()};
    default:
      llvm_unreachable("Unknown kind for DeclBaseName");
    }
  }

  static data_type ReadData(internal_key_type key, const uint8_t *data,
                            unsigned length) {
    data_type result;
    while (length > 0) {
      uint8_t kind = *data++;
      DeclID offset = endian::readNext<uint32_t, little, unaligned>(data);
      result.push_back({ kind, offset });
      length -= 5;
    }

    return result;
  }
};

/// Used to deserialize entries in the on-disk decl hash table.
class ModuleFile::ExtensionTableInfo {
  ModuleFile &File;
public:
  using internal_key_type = StringRef;
  using external_key_type = Identifier;
  using data_type = SmallVector<std::pair<StringRef, DeclID>, 8>;
  using hash_value_type = uint32_t;
  using offset_type = unsigned;

  internal_key_type GetInternalKey(external_key_type ID) {
    return ID.str();
  }

  hash_value_type ComputeHash(internal_key_type key) {
    return llvm::HashString(key);
  }

  static bool EqualKey(internal_key_type lhs, internal_key_type rhs) {
    return lhs == rhs;
  }

  static std::pair<unsigned, unsigned> ReadKeyDataLength(const uint8_t *&data) {
    unsigned keyLength = endian::readNext<uint16_t, little, unaligned>(data);
    unsigned dataLength = endian::readNext<uint16_t, little, unaligned>(data);
    return { keyLength, dataLength };
  }

  static internal_key_type ReadKey(const uint8_t *data, unsigned length) {
    return StringRef(reinterpret_cast<const char *>(data), length);
  }

  data_type ReadData(internal_key_type key, const uint8_t *data,
                     unsigned length) {
    data_type result;
    const uint8_t *limit = data + length;
    while (data < limit) {
      DeclID offset = endian::readNext<uint32_t, little, unaligned>(data);

      int32_t nameIDOrLength =
          endian::readNext<int32_t, little, unaligned>(data);
      StringRef moduleNameOrMangledBase;
      if (nameIDOrLength < 0) {
        const ModuleDecl *module = File.getModule(-nameIDOrLength);
        moduleNameOrMangledBase = module->getName().str();
      } else {
        moduleNameOrMangledBase =
            StringRef(reinterpret_cast<const char *>(data), nameIDOrLength);
        data += nameIDOrLength;
      }

      result.push_back({ moduleNameOrMangledBase, offset });
    }

    return result;
  }

  explicit ExtensionTableInfo(ModuleFile &file) : File(file) {}
};

/// Used to deserialize entries in the on-disk decl hash table.
class ModuleFile::LocalDeclTableInfo {
public:
  using internal_key_type = StringRef;
  using external_key_type = internal_key_type;
  using data_type = std::pair<DeclID, unsigned>; // ID, local discriminator
  using hash_value_type = uint32_t;
  using offset_type = unsigned;

  internal_key_type GetInternalKey(external_key_type ID) {
    return ID;
  }

  hash_value_type ComputeHash(internal_key_type key) {
    return llvm::HashString(key);
  }

  static bool EqualKey(internal_key_type lhs, internal_key_type rhs) {
    return lhs == rhs;
  }

  static std::pair<unsigned, unsigned> ReadKeyDataLength(const uint8_t *&data) {
    unsigned keyLength = endian::readNext<uint16_t, little, unaligned>(data);
    return { keyLength, sizeof(uint32_t) + sizeof(unsigned) };
  }

  static internal_key_type ReadKey(const uint8_t *data, unsigned length) {
    return StringRef(reinterpret_cast<const char *>(data), length);
  }

  static data_type ReadData(internal_key_type key, const uint8_t *data,
                            unsigned length) {
    auto declID = endian::readNext<uint32_t, little, unaligned>(data);
    auto discriminator = endian::readNext<unsigned, little, unaligned>(data);
    return { declID, discriminator };
  }
};

class ModuleFile::NestedTypeDeclsTableInfo {
public:
  using internal_key_type = StringRef;
  using external_key_type = Identifier;
  using data_type = SmallVector<std::pair<DeclID, DeclID>, 4>;
  using hash_value_type = uint32_t;
  using offset_type = unsigned;

  internal_key_type GetInternalKey(external_key_type ID) {
    return ID.str();
  }

  hash_value_type ComputeHash(internal_key_type key) {
    return llvm::HashString(key);
  }

  static bool EqualKey(internal_key_type lhs, internal_key_type rhs) {
    return lhs == rhs;
  }

  static std::pair<unsigned, unsigned> ReadKeyDataLength(const uint8_t *&data) {
    unsigned keyLength = endian::readNext<uint16_t, little, unaligned>(data);
    unsigned dataLength = endian::readNext<uint16_t, little, unaligned>(data);
    return { keyLength, dataLength };
  }

  static internal_key_type ReadKey(const uint8_t *data, unsigned length) {
    return StringRef(reinterpret_cast<const char *>(data), length);
  }

  static data_type ReadData(internal_key_type key, const uint8_t *data,
                            unsigned length) {
    data_type result;
    while (length > 0) {
      DeclID parentID = endian::readNext<uint32_t, little, unaligned>(data);
      DeclID childID = endian::readNext<uint32_t, little, unaligned>(data);
      result.push_back({ parentID, childID });
      length -= sizeof(uint32_t) * 2;
    }

    return result;
  }
};

std::unique_ptr<ModuleFile::SerializedDeclTable>
ModuleFile::readDeclTable(ArrayRef<uint64_t> fields, StringRef blobData) {
  uint32_t tableOffset;
  index_block::DeclListLayout::readRecord(fields, tableOffset);
  auto base = reinterpret_cast<const uint8_t *>(blobData.data());

  using OwnedTable = std::unique_ptr<SerializedDeclTable>;
  return OwnedTable(SerializedDeclTable::Create(base + tableOffset,
                                                base + sizeof(uint32_t), base));
}

std::unique_ptr<ModuleFile::SerializedExtensionTable>
ModuleFile::readExtensionTable(ArrayRef<uint64_t> fields, StringRef blobData) {
  uint32_t tableOffset;
  index_block::DeclListLayout::readRecord(fields, tableOffset);
  auto base = reinterpret_cast<const uint8_t *>(blobData.data());

  using OwnedTable = std::unique_ptr<SerializedExtensionTable>;
  return OwnedTable(SerializedExtensionTable::Create(base + tableOffset,
    base + sizeof(uint32_t), base, ExtensionTableInfo(*this)));
}

std::unique_ptr<ModuleFile::SerializedLocalDeclTable>
ModuleFile::readLocalDeclTable(ArrayRef<uint64_t> fields, StringRef blobData) {
  uint32_t tableOffset;
  index_block::DeclListLayout::readRecord(fields, tableOffset);
  auto base = reinterpret_cast<const uint8_t *>(blobData.data());

  using OwnedTable = std::unique_ptr<SerializedLocalDeclTable>;
  return OwnedTable(SerializedLocalDeclTable::Create(base + tableOffset,
    base + sizeof(uint32_t), base));
}

std::unique_ptr<ModuleFile::SerializedNestedTypeDeclsTable>
ModuleFile::readNestedTypeDeclsTable(ArrayRef<uint64_t> fields,
                                     StringRef blobData) {
  uint32_t tableOffset;
  index_block::NestedTypeDeclsLayout::readRecord(fields, tableOffset);
  auto base = reinterpret_cast<const uint8_t *>(blobData.data());

  using OwnedTable = std::unique_ptr<SerializedNestedTypeDeclsTable>;
  return OwnedTable(SerializedNestedTypeDeclsTable::Create(base + tableOffset,
      base + sizeof(uint32_t), base));
}

/// Used to deserialize entries in the on-disk Objective-C method table.
class ModuleFile::ObjCMethodTableInfo {
public:
  using internal_key_type = std::string;
  using external_key_type = ObjCSelector;
  using data_type = SmallVector<std::tuple<TypeID, bool, DeclID>, 8>;
  using hash_value_type = uint32_t;
  using offset_type = unsigned;

  internal_key_type GetInternalKey(external_key_type ID) {
    llvm::SmallString<32> scratch;
    return ID.getString(scratch).str();
  }

  hash_value_type ComputeHash(internal_key_type key) {
    return llvm::HashString(key);
  }

  static bool EqualKey(internal_key_type lhs, internal_key_type rhs) {
    return lhs == rhs;
  }

  static std::pair<unsigned, unsigned> ReadKeyDataLength(const uint8_t *&data) {
    unsigned keyLength = endian::readNext<uint16_t, little, unaligned>(data);
    unsigned dataLength = endian::readNext<uint16_t, little, unaligned>(data);
    return { keyLength, dataLength };
  }

  static internal_key_type ReadKey(const uint8_t *data, unsigned length) {
    return std::string(reinterpret_cast<const char *>(data), length);
  }

  static data_type ReadData(internal_key_type key, const uint8_t *data,
                            unsigned length) {
    const constexpr auto recordSize = sizeof(uint32_t) + 1 + sizeof(uint32_t);
    assert(length % recordSize == 0 && "invalid length");
    data_type result;
    while (length > 0) {
      TypeID typeID = endian::readNext<uint32_t, little, unaligned>(data);
      bool isInstanceMethod = *data++ != 0;
      DeclID methodID = endian::readNext<uint32_t, little, unaligned>(data);
      result.push_back(std::make_tuple(typeID, isInstanceMethod, methodID));
      length -= recordSize;
    }

    return result;
  }
};

std::unique_ptr<ModuleFile::SerializedObjCMethodTable>
ModuleFile::readObjCMethodTable(ArrayRef<uint64_t> fields, StringRef blobData) {
  uint32_t tableOffset;
  index_block::ObjCMethodTableLayout::readRecord(fields, tableOffset);
  auto base = reinterpret_cast<const uint8_t *>(blobData.data());

  using OwnedTable = std::unique_ptr<SerializedObjCMethodTable>;
  return OwnedTable(
           SerializedObjCMethodTable::Create(base + tableOffset,
                                             base + sizeof(uint32_t), base));
}

bool ModuleFile::readIndexBlock(llvm::BitstreamCursor &cursor) {
  cursor.EnterSubBlock(INDEX_BLOCK_ID);

  SmallVector<uint64_t, 4> scratch;
  StringRef blobData;

  while (!cursor.AtEndOfStream()) {
    auto entry = cursor.advance();
    switch (entry.Kind) {
    case llvm::BitstreamEntry::EndBlock:
      return true;

    case llvm::BitstreamEntry::Error:
      return false;

    case llvm::BitstreamEntry::SubBlock:
      // Unknown sub-block, which this version of the compiler won't use.
      if (cursor.SkipBlock())
        return false;
      break;

    case llvm::BitstreamEntry::Record:
      scratch.clear();
      blobData = {};
      unsigned kind = cursor.readRecord(entry.ID, scratch, &blobData);

      switch (kind) {
      case index_block::DECL_OFFSETS:
        assert(blobData.empty());
        Decls.assign(scratch.begin(), scratch.end());
        break;
      case index_block::DECL_CONTEXT_OFFSETS:
        assert(blobData.empty());
        DeclContexts.assign(scratch.begin(), scratch.end());
        break;
      case index_block::TYPE_OFFSETS:
        assert(blobData.empty());
        Types.assign(scratch.begin(), scratch.end());
        break;
      case index_block::IDENTIFIER_OFFSETS:
        assert(blobData.empty());
        Identifiers.assign(scratch.begin(), scratch.end());
        break;
      case index_block::TOP_LEVEL_DECLS:
        TopLevelDecls = readDeclTable(scratch, blobData);
        break;
      case index_block::OPERATORS:
        OperatorDecls = readDeclTable(scratch, blobData);
        break;
      case index_block::PRECEDENCE_GROUPS:
        PrecedenceGroupDecls = readDeclTable(scratch, blobData);
        break;
      case index_block::EXTENSIONS:
        ExtensionDecls = readExtensionTable(scratch, blobData);
        break;
      case index_block::CLASS_MEMBERS:
        ClassMembersByName = readDeclTable(scratch, blobData);
        break;
      case index_block::OPERATOR_METHODS:
        OperatorMethodDecls = readDeclTable(scratch, blobData);
        break;
      case index_block::OBJC_METHODS:
        ObjCMethods = readObjCMethodTable(scratch, blobData);
        break;
      case index_block::ENTRY_POINT:
        assert(blobData.empty());
        setEntryPointClassID(scratch.front());
        break;
      case index_block::LOCAL_TYPE_DECLS:
        LocalTypeDecls = readLocalDeclTable(scratch, blobData);
        break;
      case index_block::NESTED_TYPE_DECLS:
        NestedTypeDecls = readNestedTypeDeclsTable(scratch, blobData);
        break;
      case index_block::LOCAL_DECL_CONTEXT_OFFSETS:
        assert(blobData.empty());
        LocalDeclContexts.assign(scratch.begin(), scratch.end());
        break;
      case index_block::GENERIC_ENVIRONMENT_OFFSETS:
        assert(blobData.empty());
        GenericEnvironments.assign(scratch.begin(), scratch.end());
        break;
      case index_block::NORMAL_CONFORMANCE_OFFSETS:
        assert(blobData.empty());
        NormalConformances.assign(scratch.begin(), scratch.end());
        break;
      case index_block::SIL_LAYOUT_OFFSETS:
        assert(blobData.empty());
        SILLayouts.assign(scratch.begin(), scratch.end());
        break;

      default:
        // Unknown index kind, which this version of the compiler won't use.
        break;
      }
      break;
    }
  }

  return false;
}

class ModuleFile::DeclCommentTableInfo {
  ModuleFile &F;

public:
  using internal_key_type = StringRef;
  using external_key_type = StringRef;
  using data_type = CommentInfo;
  using hash_value_type = uint32_t;
  using offset_type = unsigned;

  DeclCommentTableInfo(ModuleFile &F) : F(F) {}

  internal_key_type GetInternalKey(external_key_type key) {
    return key;
  }

  hash_value_type ComputeHash(internal_key_type key) {
    assert(!key.empty());
    return llvm::HashString(key);
  }

  static bool EqualKey(internal_key_type lhs, internal_key_type rhs) {
    return lhs == rhs;
  }

  static std::pair<unsigned, unsigned> ReadKeyDataLength(const uint8_t *&data) {
    unsigned keyLength = endian::readNext<uint32_t, little, unaligned>(data);
    unsigned dataLength = endian::readNext<uint32_t, little, unaligned>(data);
    return { keyLength, dataLength };
  }

  static internal_key_type ReadKey(const uint8_t *data, unsigned length) {
    return StringRef(reinterpret_cast<const char *>(data), length);
  }

  data_type ReadData(internal_key_type key, const uint8_t *data,
                     unsigned length) {
    data_type result;

    {
      unsigned BriefSize = endian::readNext<uint32_t, little, unaligned>(data);
      result.Brief = StringRef(reinterpret_cast<const char *>(data), BriefSize);
      data += BriefSize;
    }

    unsigned NumComments = endian::readNext<uint32_t, little, unaligned>(data);
    MutableArrayRef<SingleRawComment> Comments =
        F.getContext().AllocateUninitialized<SingleRawComment>(NumComments);

    for (unsigned i = 0; i != NumComments; ++i) {
      unsigned StartColumn =
          endian::readNext<uint32_t, little, unaligned>(data);
      unsigned RawSize = endian::readNext<uint32_t, little, unaligned>(data);
      auto RawText = StringRef(reinterpret_cast<const char *>(data), RawSize);
      data += RawSize;

      new (&Comments[i]) SingleRawComment(RawText, StartColumn);
    }
    result.Raw = RawComment(Comments);
    result.Group = endian::readNext<uint32_t, little, unaligned>(data);
    result.SourceOrder = endian::readNext<uint32_t, little, unaligned>(data);
    return result;
  }
};

std::unique_ptr<ModuleFile::SerializedDeclCommentTable>
ModuleFile::readDeclCommentTable(ArrayRef<uint64_t> fields,
                                 StringRef blobData) {
  uint32_t tableOffset;
  index_block::DeclListLayout::readRecord(fields, tableOffset);
  auto base = reinterpret_cast<const uint8_t *>(blobData.data());

  return std::unique_ptr<SerializedDeclCommentTable>(
    SerializedDeclCommentTable::Create(base + tableOffset,
                                       base + sizeof(uint32_t), base,
                                       DeclCommentTableInfo(*this)));
}

std::unique_ptr<ModuleFile::GroupNameTable>
ModuleFile::readGroupTable(ArrayRef<uint64_t> Fields, StringRef BlobData) {
  std::unique_ptr<ModuleFile::GroupNameTable> pMap(
    new ModuleFile::GroupNameTable);
  auto Data = reinterpret_cast<const uint8_t *>(BlobData.data());
  unsigned GroupCount = endian::readNext<uint32_t, little, unaligned>(Data);
  for (unsigned I = 0; I < GroupCount; I++) {
    auto RawSize = endian::readNext<uint32_t, little, unaligned>(Data);
    auto RawText = StringRef(reinterpret_cast<const char *>(Data), RawSize);
    Data += RawSize;
    (*pMap)[I] = RawText;
  }
  return pMap;
}

bool ModuleFile::readCommentBlock(llvm::BitstreamCursor &cursor) {
  cursor.EnterSubBlock(COMMENT_BLOCK_ID);

  SmallVector<uint64_t, 4> scratch;
  StringRef blobData;

  while (!cursor.AtEndOfStream()) {
    auto entry = cursor.advance();
    switch (entry.Kind) {
    case llvm::BitstreamEntry::EndBlock:
      return true;

    case llvm::BitstreamEntry::Error:
      return false;

    case llvm::BitstreamEntry::SubBlock:
      // Unknown sub-block, which this version of the compiler won't use.
      if (cursor.SkipBlock())
        return false;
      break;

    case llvm::BitstreamEntry::Record:
      scratch.clear();
      unsigned kind = cursor.readRecord(entry.ID, scratch, &blobData);

      switch (kind) {
      case comment_block::DECL_COMMENTS:
        DeclCommentTable = readDeclCommentTable(scratch, blobData);
        break;
      case comment_block::GROUP_NAMES:
        GroupNamesMap = readGroupTable(scratch, blobData);
        break;
      default:
        // Unknown index kind, which this version of the compiler won't use.
        break;
      }
      break;
    }
  }

  return false;
}

static Optional<swift::LibraryKind> getActualLibraryKind(unsigned rawKind) {
  auto stableKind = static_cast<serialization::LibraryKind>(rawKind);
  if (stableKind != rawKind)
    return None;

  switch (stableKind) {
  case serialization::LibraryKind::Library:
    return swift::LibraryKind::Library;
  case serialization::LibraryKind::Framework:
    return swift::LibraryKind::Framework;
  }

  // If there's a new case value in the module file, ignore it.
  return None;
}

static bool areCompatibleArchitectures(const llvm::Triple &moduleTarget,
                                       const llvm::Triple &ctxTarget) {
  if (moduleTarget.getArch() == ctxTarget.getArch())
    return true;

  // Special case: ARM and Thumb are compatible.
  const llvm::Triple::ArchType moduleArch = moduleTarget.getArch();
  const llvm::Triple::ArchType ctxArch = ctxTarget.getArch();
  if ((moduleArch == llvm::Triple::arm && ctxArch == llvm::Triple::thumb) ||
      (moduleArch == llvm::Triple::thumb && ctxArch == llvm::Triple::arm))
    return true;
  if ((moduleArch == llvm::Triple::armeb && ctxArch == llvm::Triple::thumbeb) ||
      (moduleArch == llvm::Triple::thumbeb && ctxArch == llvm::Triple::armeb))
    return true;

  return false;
}

static bool areCompatibleOSs(const llvm::Triple &moduleTarget,
                             const llvm::Triple &ctxTarget) {
  if (moduleTarget.getOS() == ctxTarget.getOS())
    return true;

  // Special case: macOS and Darwin are compatible.
  const llvm::Triple::OSType moduleOS = moduleTarget.getOS();
  const llvm::Triple::OSType ctxOS = ctxTarget.getOS();
  if ((moduleOS == llvm::Triple::Darwin && ctxOS == llvm::Triple::MacOSX) ||
      (moduleOS == llvm::Triple::MacOSX && ctxOS == llvm::Triple::Darwin))
    return true;

  return false;
}

static bool isTargetTooNew(const llvm::Triple &moduleTarget,
                           const llvm::Triple &ctxTarget) {
  unsigned major, minor, micro;

  if (moduleTarget.isMacOSX()) {
    moduleTarget.getMacOSXVersion(major, minor, micro);
    return ctxTarget.isMacOSXVersionLT(major, minor, micro);
  }

  moduleTarget.getOSVersion(major, minor, micro);
  return ctxTarget.isOSVersionLT(major, minor, micro);
}

ModuleFile::ModuleFile(
    std::unique_ptr<llvm::MemoryBuffer> moduleInputBuffer,
    std::unique_ptr<llvm::MemoryBuffer> moduleDocInputBuffer,
    bool isFramework, serialization::ValidationInfo &info,
    serialization::ExtendedValidationInfo *extInfo)
    : ModuleInputBuffer(std::move(moduleInputBuffer)),
      ModuleDocInputBuffer(std::move(moduleDocInputBuffer)),
      DeserializedTypeCallback([](Type ty) {}) {
  assert(getStatus() == Status::Valid);
  Bits.IsFramework = isFramework;

  PrettyModuleFileDeserialization stackEntry(*this);

  llvm::BitstreamCursor cursor{ModuleInputBuffer->getMemBufferRef()};

  if (!checkModuleSignature(cursor) ||
      !enterTopLevelModuleBlock(cursor, MODULE_BLOCK_ID)) {
    error();
    return;
  }

  // Future-proofing: make sure we validate the control block before we try to
  // read any other blocks.
  bool hasValidControlBlock = false;
  SmallVector<uint64_t, 64> scratch;

  llvm::BitstreamEntry topLevelEntry;

  while (!cursor.AtEndOfStream()) {
    topLevelEntry = cursor.advance(AF_DontPopBlockAtEnd);
    if (topLevelEntry.Kind != llvm::BitstreamEntry::SubBlock)
      break;

    switch (topLevelEntry.ID) {
    case CONTROL_BLOCK_ID: {
      cursor.EnterSubBlock(CONTROL_BLOCK_ID);

      info = validateControlBlock(cursor, scratch, extInfo);
      if (info.status != Status::Valid) {
        error(info.status);
        return;
      }
      Name = info.name;
      TargetTriple = info.targetTriple;

      hasValidControlBlock = true;
      break;
    }

    case INPUT_BLOCK_ID: {
      if (!hasValidControlBlock) {
        error();
        return;
      }

      cursor.EnterSubBlock(INPUT_BLOCK_ID);
      bool seenFlags = false;

      auto next = cursor.advance();
      while (next.Kind == llvm::BitstreamEntry::Record) {
        scratch.clear();
        StringRef blobData;
        unsigned kind = cursor.readRecord(next.ID, scratch, &blobData);
        switch (kind) {
        case input_block::IMPORTED_MODULE: {
          bool exported, scoped;
          input_block::ImportedModuleLayout::readRecord(scratch,
                                                        exported, scoped);
          Dependencies.push_back({blobData, exported, scoped});
          break;
        }
        case input_block::LINK_LIBRARY: {
          uint8_t rawKind;
          bool shouldForceLink;
          input_block::LinkLibraryLayout::readRecord(scratch, rawKind,
                                                     shouldForceLink);
          if (auto libKind = getActualLibraryKind(rawKind))
            LinkLibraries.push_back({blobData, *libKind, shouldForceLink});
          // else ignore the dependency...it'll show up as a linker error.
          break;
        }
        case input_block::IMPORTED_HEADER: {
          assert(!importedHeaderInfo.fileSize && "only one header allowed");
          bool exported;
          input_block::ImportedHeaderLayout::readRecord(scratch,
            exported, importedHeaderInfo.fileSize,
            importedHeaderInfo.fileModTime);
          Dependencies.push_back(Dependency::forHeader(blobData, exported));
          break;
        }
        case input_block::IMPORTED_HEADER_CONTENTS: {
          assert(Dependencies.back().isHeader() && "must follow header record");
          assert(importedHeaderInfo.contents.empty() &&
                 "contents seen already");
          importedHeaderInfo.contents = blobData;
          break;
        }
        case input_block::MODULE_FLAGS: {
          assert(!seenFlags && "only one flags record allowed");
          seenFlags = true;
          bool hasUnderlyingModule;
          input_block::ModuleFlagsLayout::readRecord(scratch,
                                                     hasUnderlyingModule);
          Bits.HasUnderlyingModule = hasUnderlyingModule;
          break;
        }
        case input_block::SEARCH_PATH: {
          bool isFramework;
          bool isSystem;
          input_block::SearchPathLayout::readRecord(scratch, isFramework,
                                                    isSystem);
          SearchPaths.push_back({blobData, isFramework, isSystem});
          break;
        }
        default:
          // Unknown input kind, possibly for use by a future version of the
          // module format.
          // FIXME: Should we warn about this?
          break;
        }

        next = cursor.advance();
      }

      if (next.Kind != llvm::BitstreamEntry::EndBlock)
        error();

      break;
    }

    case DECLS_AND_TYPES_BLOCK_ID: {
      if (!hasValidControlBlock) {
        error();
        return;
      }

      // The decls-and-types block is lazily loaded. Save the cursor and load
      // any abbrev records at the start of the block.
      DeclTypeCursor = cursor;
      DeclTypeCursor.EnterSubBlock(DECLS_AND_TYPES_BLOCK_ID);
      if (DeclTypeCursor.advance().Kind == llvm::BitstreamEntry::Error)
        error();

      // With the main cursor, skip over the block and continue.
      if (cursor.SkipBlock()) {
        error();
        return;
      }
      break;
    }

    case IDENTIFIER_DATA_BLOCK_ID: {
      if (!hasValidControlBlock) {
        error();
        return;
      }

      cursor.EnterSubBlock(IDENTIFIER_DATA_BLOCK_ID);

      auto next = cursor.advanceSkippingSubblocks();
      while (next.Kind == llvm::BitstreamEntry::Record) {
        scratch.clear();
        StringRef blobData;
        unsigned kind = cursor.readRecord(next.ID, scratch, &blobData);

        switch (kind) {
        case identifier_block::IDENTIFIER_DATA:
          assert(scratch.empty());
          IdentifierData = blobData;
          break;
        default:
          // Unknown identifier data, which this version of the compiler won't
          // use.
          break;
        }

        next = cursor.advanceSkippingSubblocks();
      }

      if (next.Kind != llvm::BitstreamEntry::EndBlock) {
        error();
        return;
      }

      break;
    }

    case INDEX_BLOCK_ID: {
      if (!hasValidControlBlock || !readIndexBlock(cursor)) {
        error();
        return;
      }
      break;
    }

    case SIL_INDEX_BLOCK_ID: {
      // Save the cursor.
      SILIndexCursor = cursor;
      SILIndexCursor.EnterSubBlock(SIL_INDEX_BLOCK_ID);

      // With the main cursor, skip over the block and continue.
      if (cursor.SkipBlock()) {
        error();
        return;
      }
      break;
    }

    case SIL_BLOCK_ID: {
      // Save the cursor.
      SILCursor = cursor;
      SILCursor.EnterSubBlock(SIL_BLOCK_ID);

      // With the main cursor, skip over the block and continue.
      if (cursor.SkipBlock()) {
        error();
        return;
      }
      break;
    }

    default:
      // Unknown top-level block, possibly for use by a future version of the
      // module format.
      if (cursor.SkipBlock()) {
        error();
        return;
      }
      break;
    }
  }

  if (topLevelEntry.Kind != llvm::BitstreamEntry::EndBlock) {
    error();
    return;
  }

  if (!this->ModuleDocInputBuffer)
    return;

  llvm::BitstreamCursor docCursor{ModuleDocInputBuffer->getMemBufferRef()};
  if (!checkModuleDocSignature(docCursor) ||
      !enterTopLevelModuleBlock(docCursor, MODULE_DOC_BLOCK_ID)) {
    error(Status::MalformedDocumentation);
    return;
  }

  while (!docCursor.AtEndOfStream()) {
    topLevelEntry = docCursor.advance(AF_DontPopBlockAtEnd);
    if (topLevelEntry.Kind != llvm::BitstreamEntry::SubBlock)
      break;

    switch (topLevelEntry.ID) {
    case COMMENT_BLOCK_ID: {
      if (!hasValidControlBlock || !readCommentBlock(docCursor)) {
        error(Status::MalformedDocumentation);
        return;
      }
      break;
    }

    default:
      // Unknown top-level block, possibly for use by a future version of the
      // module format.
      if (docCursor.SkipBlock()) {
        error(Status::MalformedDocumentation);
        return;
      }
      break;
    }
  }

  if (topLevelEntry.Kind != llvm::BitstreamEntry::EndBlock) {
    error(Status::MalformedDocumentation);
    return;
  }
}

Status ModuleFile::associateWithFileContext(FileUnit *file,
                                            SourceLoc diagLoc) {
  PrettyModuleFileDeserialization stackEntry(*this);

  assert(getStatus() == Status::Valid && "invalid module file");
  assert(!FileContext && "already associated with an AST module");
  FileContext = file;

  if (file->getParentModule()->getName().str() != Name)
    return error(Status::NameMismatch);

  ASTContext &ctx = getContext();

  llvm::Triple moduleTarget(llvm::Triple::normalize(TargetTriple));
  if (!areCompatibleArchitectures(moduleTarget, ctx.LangOpts.Target) ||
      !areCompatibleOSs(moduleTarget, ctx.LangOpts.Target)) {
    return error(Status::TargetIncompatible);
  }
  if (ctx.LangOpts.EnableTargetOSChecking &&
      isTargetTooNew(moduleTarget, ctx.LangOpts.Target)) {
    return error(Status::TargetTooNew);
  }

  for (const auto &searchPath : SearchPaths)
    ctx.addSearchPath(searchPath.Path, searchPath.IsFramework,
                      searchPath.IsSystem);

  auto clangImporter = static_cast<ClangImporter *>(ctx.getClangModuleLoader());

  bool missingDependency = false;
  for (auto &dependency : Dependencies) {
    assert(!dependency.isLoaded() && "already loaded?");

    if (dependency.isHeader()) {
      // The path may be empty if the file being loaded is a partial AST,
      // and the current compiler invocation is a merge-modules step.
      if (!dependency.RawPath.empty()) {
        bool hadError =
            clangImporter->importHeader(dependency.RawPath,
                                        file->getParentModule(),
                                        importedHeaderInfo.fileSize,
                                        importedHeaderInfo.fileModTime,
                                        importedHeaderInfo.contents,
                                        diagLoc);
        if (hadError)
          return error(Status::FailedToLoadBridgingHeader);
      }
      ModuleDecl *importedHeaderModule = clangImporter->getImportedHeaderModule();
      dependency.Import = { {}, importedHeaderModule };
      continue;
    }

    StringRef modulePathStr = dependency.RawPath;
    StringRef scopePath;
    if (dependency.isScoped()) {
      auto splitPoint = modulePathStr.find_last_of('\0');
      assert(splitPoint != StringRef::npos);
      scopePath = modulePathStr.substr(splitPoint+1);
      modulePathStr = modulePathStr.slice(0, splitPoint);
    }

    SmallVector<Identifier, 4> modulePath;
    while (!modulePathStr.empty()) {
      StringRef nextComponent;
      std::tie(nextComponent, modulePathStr) = modulePathStr.split('\0');
      modulePath.push_back(ctx.getIdentifier(nextComponent));
      assert(!modulePath.back().empty() &&
             "invalid module name (submodules not yet supported)");
    }
    auto module = getModule(modulePath);
    if (!module || module->failedToLoad()) {
      // If we're missing the module we're shadowing, treat that specially.
      if (modulePath.size() == 1 &&
          modulePath.front() == file->getParentModule()->getName()) {
        return error(Status::MissingShadowedModule);
      }

      // Otherwise, continue trying to load dependencies, so that we can list
      // everything that's missing.
      missingDependency = true;
      continue;
    }

    // This is for backwards-compatibility with modules that still rely on the
    // "HasUnderlyingModule" flag.
    if (Bits.HasUnderlyingModule && module == ShadowedModule)
      dependency.forceExported();

    if (scopePath.empty()) {
      dependency.Import = { {}, module };
    } else {
      auto scopeID = ctx.getIdentifier(scopePath);
      assert(!scopeID.empty() &&
             "invalid decl name (non-top-level decls not supported)");
      std::pair<Identifier, SourceLoc> accessPathElem(scopeID, SourceLoc());
      dependency.Import = {ctx.AllocateCopy(llvm::makeArrayRef(accessPathElem)),
                           module};
    }
  }

  if (missingDependency) {
    return error(Status::MissingDependency);
  }

  if (Bits.HasEntryPoint) {
    FileContext->getParentModule()->registerEntryPointFile(FileContext,
                                                           SourceLoc(),
                                                           None);
  }

  return getStatus();
}

ModuleFile::~ModuleFile() = default;

void ModuleFile::lookupValue(DeclName name,
                             SmallVectorImpl<ValueDecl*> &results) {
  PrettyModuleFileDeserialization stackEntry(*this);

  if (TopLevelDecls) {
    // Find top-level declarations with the given name.
    // FIXME: As a bit of a hack, do lookup by the simple name, then filter
    // compound decls, to avoid having to completely redo how modules are
    // serialized.
    auto iter = TopLevelDecls->find(name.getBaseName());
    if (iter != TopLevelDecls->end()) {
      if (name.isSimpleName()) {
        for (auto item : *iter) {
          auto VD = cast<ValueDecl>(getDecl(item.second));
          results.push_back(VD);
        }
      } else {
        for (auto item : *iter) {
          auto VD = cast<ValueDecl>(getDecl(item.second));
          if (VD->getFullName().matchesRef(name))
            results.push_back(VD);
        }
      }
    }
  }

  // If the name is an operator name, also look for operator methods.
  if (name.isOperator() && OperatorMethodDecls) {
    auto iter = OperatorMethodDecls->find(name.getBaseName());
    if (iter != OperatorMethodDecls->end()) {
      for (auto item : *iter) {
        auto VD = cast<ValueDecl>(getDecl(item.second));
        results.push_back(VD);
      }
    }
  }
}

TypeDecl *ModuleFile::lookupLocalType(StringRef MangledName) {
  PrettyModuleFileDeserialization stackEntry(*this);

  if (!LocalTypeDecls)
    return nullptr;

  auto iter = LocalTypeDecls->find(MangledName);
  if (iter == LocalTypeDecls->end())
    return nullptr;

  return cast<TypeDecl>(getDecl((*iter).first));
}

TypeDecl *ModuleFile::lookupNestedType(Identifier name,
                                       const ValueDecl *parent) {
  PrettyModuleFileDeserialization stackEntry(*this);

  if (!NestedTypeDecls)
    return nullptr;

  auto iter = NestedTypeDecls->find(name);
  if (iter == NestedTypeDecls->end())
    return nullptr;

  auto data = *iter;
  for (std::pair<DeclID, DeclID> entry : data) {
    assert(entry.first);
    auto declOrOffset = Decls[entry.first - 1];
    if (!declOrOffset.isComplete())
      continue;

    Decl *decl = declOrOffset;
    if (decl != parent)
      continue;
    return cast<TypeDecl>(getDecl(entry.second));
  }

  return nullptr;
}

OperatorDecl *ModuleFile::lookupOperator(Identifier name, DeclKind fixity) {
  PrettyModuleFileDeserialization stackEntry(*this);

  if (!OperatorDecls)
    return nullptr;

  auto iter = OperatorDecls->find(name);
  if (iter == OperatorDecls->end())
    return nullptr;

  for (auto item : *iter) {
    if (getStableFixity(fixity) == item.first)
      return cast<OperatorDecl>(getDecl(item.second));
  }

  // FIXME: operators re-exported from other modules?

  return nullptr;
}

PrecedenceGroupDecl *ModuleFile::lookupPrecedenceGroup(Identifier name) {
  PrettyModuleFileDeserialization stackEntry(*this);

  if (!PrecedenceGroupDecls)
    return nullptr;

  auto iter = PrecedenceGroupDecls->find(name);
  if (iter == PrecedenceGroupDecls->end())
    return nullptr;

  auto data = *iter;
  assert(data.size() == 1);
  return cast<PrecedenceGroupDecl>(getDecl(data[0].second));
}

void ModuleFile::getImportedModules(
    SmallVectorImpl<ModuleDecl::ImportedModule> &results,
    ModuleDecl::ImportFilter filter) {
  PrettyModuleFileDeserialization stackEntry(*this);

  for (auto &dep : Dependencies) {
    if (filter != ModuleDecl::ImportFilter::All &&
        (filter == ModuleDecl::ImportFilter::Public) ^ dep.isExported())
      continue;
    assert(dep.isLoaded());
    results.push_back(dep.Import);
  }
}

void ModuleFile::getImportDecls(SmallVectorImpl<Decl *> &Results) {
  if (!Bits.ComputedImportDecls) {
    ASTContext &Ctx = getContext();
    for (auto &Dep : Dependencies) {
      // FIXME: We need a better way to show headers, since they usually /are/
      // re-exported. This isn't likely to come up much, though.
      if (Dep.isHeader())
        continue;

      StringRef ModulePathStr = Dep.RawPath;
      StringRef ScopePath;
      if (Dep.isScoped())
        std::tie(ModulePathStr, ScopePath) = ModulePathStr.rsplit('\0');

      SmallVector<std::pair<swift::Identifier, swift::SourceLoc>, 1> AccessPath;
      while (!ModulePathStr.empty()) {
        StringRef NextComponent;
        std::tie(NextComponent, ModulePathStr) = ModulePathStr.split('\0');
        AccessPath.push_back({Ctx.getIdentifier(NextComponent), SourceLoc()});
      }

      if (AccessPath.size() == 1 && AccessPath[0].first == Ctx.StdlibModuleName)
        continue;

      ModuleDecl *M = Ctx.getModule(AccessPath);

      auto Kind = ImportKind::Module;
      if (!ScopePath.empty()) {
        auto ScopeID = Ctx.getIdentifier(ScopePath);
        assert(!ScopeID.empty() &&
               "invalid decl name (non-top-level decls not supported)");

        if (!M) {
          // The dependency module could not be loaded.  Just make a guess
          // about the import kind, we cannot do better.
          Kind = ImportKind::Func;
        } else {
          // Lookup the decl in the top-level module.
          ModuleDecl *TopLevelModule = M;
          if (AccessPath.size() > 1)
            TopLevelModule = Ctx.getLoadedModule(AccessPath.front().first);

          SmallVector<ValueDecl *, 8> Decls;
          TopLevelModule->lookupQualified(
              ModuleType::get(TopLevelModule), ScopeID,
              NL_QualifiedDefault | NL_KnownNoDependency, nullptr, Decls);
          Optional<ImportKind> FoundKind = ImportDecl::findBestImportKind(Decls);
          assert(FoundKind.hasValue() &&
                 "deserialized imports should not be ambiguous");
          Kind = *FoundKind;
        }

        AccessPath.push_back({ ScopeID, SourceLoc() });
      }

      auto *ID = ImportDecl::create(Ctx, FileContext, SourceLoc(), Kind,
                                    SourceLoc(), AccessPath);
      ID->setModule(M);
      if (Dep.isExported())
        ID->getAttrs().add(
            new (Ctx) ExportedAttr(/*IsImplicit=*/false));
      ImportDecls.push_back(ID);
    }
    Bits.ComputedImportDecls = true;
  }
  Results.append(ImportDecls.begin(), ImportDecls.end());
}

void ModuleFile::lookupVisibleDecls(ModuleDecl::AccessPathTy accessPath,
                                    VisibleDeclConsumer &consumer,
                                    NLKind lookupKind) {
  PrettyModuleFileDeserialization stackEntry(*this);
  assert(accessPath.size() <= 1 && "can only refer to top-level decls");

  if (!TopLevelDecls)
    return;

  if (!accessPath.empty()) {
    auto iter = TopLevelDecls->find(accessPath.front().first);
    if (iter == TopLevelDecls->end())
      return;

    for (auto item : *iter)
      consumer.foundDecl(cast<ValueDecl>(getDecl(item.second)),
                         DeclVisibilityKind::VisibleAtTopLevel);
    return;
  }

  for (auto entry : TopLevelDecls->data()) {
    for (auto item : entry)
      consumer.foundDecl(cast<ValueDecl>(getDecl(item.second)),
                         DeclVisibilityKind::VisibleAtTopLevel);
  }
}

void ModuleFile::loadExtensions(NominalTypeDecl *nominal) {
  PrettyModuleFileDeserialization stackEntry(*this);
  if (!ExtensionDecls)
    return;

  auto iter = ExtensionDecls->find(nominal->getName());
  if (iter == ExtensionDecls->end())
    return;

  if (nominal->hasAccessibility() &&
      nominal->getEffectiveAccess() < Accessibility::Internal) {
    if (nominal->getModuleScopeContext() != getFile())
      return;
  }

  if (nominal->getParent()->isModuleScopeContext()) {
    Identifier moduleName = nominal->getParentModule()->getName();
    for (auto item : *iter) {
      if (item.first == moduleName.str())
        (void)getDecl(item.second);
    }
  } else {
    std::string mangledName =
        NewMangling::ASTMangler().mangleNominalType(nominal);
    for (auto item : *iter) {
      if (item.first == mangledName)
        (void)getDecl(item.second);
    }
  }
}

void ModuleFile::loadObjCMethods(
       ClassDecl *classDecl,
       ObjCSelector selector,
       bool isInstanceMethod,
       llvm::TinyPtrVector<AbstractFunctionDecl *> &methods) {
  // If we don't have an Objective-C method table, there's nothing to do.
  if (!ObjCMethods)
    return;

  // Look for all methods in the module file with this selector.
  auto known = ObjCMethods->find(selector);
  if (known == ObjCMethods->end()) {
    return;
  }

  auto results = *known;
  for (const auto &result : results) {
    // If the method is the wrong kind (instance vs. class), skip it.
    if (isInstanceMethod != std::get<1>(result))
      continue;

    // If the method isn't defined in the requested class, skip it.
    Type type = getType(std::get<0>(result));
    if (type->getClassOrBoundGenericClass() != classDecl)
      continue;

    // Deserialize the method and add it to the list.
    if (auto func = dyn_cast_or_null<AbstractFunctionDecl>(
                      getDecl(std::get<2>(result)))) {
      methods.push_back(func);
    }
  }
}

void ModuleFile::lookupClassMember(ModuleDecl::AccessPathTy accessPath,
                                   DeclName name,
                                   SmallVectorImpl<ValueDecl*> &results) {
  PrettyModuleFileDeserialization stackEntry(*this);
  assert(accessPath.size() <= 1 && "can only refer to top-level decls");

  if (!ClassMembersByName)
    return;

  auto iter = ClassMembersByName->find(name.getBaseName());
  if (iter == ClassMembersByName->end())
    return;

  if (!accessPath.empty()) {
    // As a hack to avoid completely redoing how the module is indexed, we take
    // the simple-name-based lookup then filter by the compound name if we have
    // one.
    if (name.isSimpleName()) {
      for (auto item : *iter) {
        auto vd = cast<ValueDecl>(getDecl(item.second));
        auto dc = vd->getDeclContext();
        while (!dc->getParent()->isModuleScopeContext())
          dc = dc->getParent();
        if (auto nominal = dc->getAsNominalTypeOrNominalTypeExtensionContext())
          if (nominal->getName() == accessPath.front().first)
            results.push_back(vd);
      }
    } else {
      for (auto item : *iter) {
        auto vd = cast<ValueDecl>(getDecl(item.second));
        if (!vd->getFullName().matchesRef(name))
          continue;
        
        auto dc = vd->getDeclContext();
        while (!dc->getParent()->isModuleScopeContext())
          dc = dc->getParent();
        if (auto nominal = dc->getAsNominalTypeOrNominalTypeExtensionContext())
          if (nominal->getName() == accessPath.front().first)
            results.push_back(vd);
      }
    }
    return;
  }

  for (auto item : *iter) {
    auto vd = cast<ValueDecl>(getDecl(item.second));
    results.push_back(vd);
  }
}

void ModuleFile::lookupClassMembers(ModuleDecl::AccessPathTy accessPath,
                                    VisibleDeclConsumer &consumer) {
  PrettyModuleFileDeserialization stackEntry(*this);
  assert(accessPath.size() <= 1 && "can only refer to top-level decls");

  if (!ClassMembersByName)
    return;

  if (!accessPath.empty()) {
    for (const auto &list : ClassMembersByName->data()) {
      for (auto item : list) {
        auto vd = cast<ValueDecl>(getDecl(item.second));
        auto dc = vd->getDeclContext();
        while (!dc->getParent()->isModuleScopeContext())
          dc = dc->getParent();
        if (auto nominal = dc->getAsNominalTypeOrNominalTypeExtensionContext())
          if (nominal->getName() == accessPath.front().first)
            consumer.foundDecl(vd, DeclVisibilityKind::DynamicLookup);
      }
    }
    return;
  }

  for (const auto &list : ClassMembersByName->data()) {
    for (auto item : list)
      consumer.foundDecl(cast<ValueDecl>(getDecl(item.second)),
                         DeclVisibilityKind::DynamicLookup);
  }
}

void ModuleFile::lookupObjCMethods(
       ObjCSelector selector,
       SmallVectorImpl<AbstractFunctionDecl *> &results) {
  // If we don't have an Objective-C method table, there's nothing to do.
  if (!ObjCMethods) return;

  // Look for all methods in the module file with this selector.
  auto known = ObjCMethods->find(selector);
  if (known == ObjCMethods->end()) return;

  auto found = *known;
  for (const auto &result : found) {
    // Deserialize the method and add it to the list.
    if (auto func = dyn_cast_or_null<AbstractFunctionDecl>(
                      getDecl(std::get<2>(result))))
      results.push_back(func);
  }
}

void
ModuleFile::collectLinkLibraries(ModuleDecl::LinkLibraryCallback callback) const {
  for (auto &lib : LinkLibraries)
    callback(lib);
  if (Bits.IsFramework)
    callback(LinkLibrary(Name, LibraryKind::Framework));
}

void ModuleFile::getTopLevelDecls(SmallVectorImpl<Decl *> &results) {
  PrettyModuleFileDeserialization stackEntry(*this);
  if (PrecedenceGroupDecls) {
    for (auto entry : PrecedenceGroupDecls->data()) {
      for (auto item : entry)
        results.push_back(getDecl(item.second));
    }
  }

  if (OperatorDecls) {
    for (auto entry : OperatorDecls->data()) {
      for (auto item : entry)
        results.push_back(getDecl(item.second));
    }
  }

  if (TopLevelDecls) {
    for (auto entry : TopLevelDecls->data()) {
      for (auto item : entry)
        results.push_back(getDecl(item.second));
    }
  }

  if (ExtensionDecls) {
    for (auto entry : ExtensionDecls->data()) {
      for (auto item : entry)
        results.push_back(getDecl(item.second));
    }
  }
}

void
ModuleFile::getLocalTypeDecls(SmallVectorImpl<TypeDecl *> &results) {
  PrettyModuleFileDeserialization stackEntry(*this);
  if (!LocalTypeDecls)
    return;

  for (auto entry : LocalTypeDecls->data()) {
    auto DeclID = entry.first;
    auto TD = cast<TypeDecl>(getDecl(DeclID));
    TD->setLocalDiscriminator(entry.second);
    results.push_back(TD);
  }
}

void ModuleFile::getDisplayDecls(SmallVectorImpl<Decl *> &results) {
  if (ShadowedModule)
    ShadowedModule->getDisplayDecls(results);

  PrettyModuleFileDeserialization stackEntry(*this);
  getImportDecls(results);
  getTopLevelDecls(results);
}

Optional<CommentInfo> ModuleFile::getCommentForDecl(const Decl *D) const {
  assert(D);

  // Keep these as assertions instead of early exits to ensure that we are not
  // doing extra work.  These cases should be handled by clients of this API.
  assert(!D->hasClangNode() &&
         "cannot find comments for Clang decls in Swift modules");
  assert(D->getDeclContext()->getModuleScopeContext() == FileContext &&
         "Decl is from a different serialized file");

  if (!DeclCommentTable)
    return None;

  if (D->isImplicit())
    return None;

  if (auto *ED = dyn_cast<ExtensionDecl>(D)) {
    // Compute the USR.
    llvm::SmallString<128> USRBuffer;
    {
      llvm::raw_svector_ostream OS(USRBuffer);
      if (ide::printExtensionUSR(ED, OS))
        return None;
    }
     return getCommentForDeclByUSR(USRBuffer.str());
  }

  auto *VD = dyn_cast<ValueDecl>(D);
  if (!VD)
    return None;

  // Compute the USR.
  llvm::SmallString<128> USRBuffer;
  {
    llvm::raw_svector_ostream OS(USRBuffer);
    if (ide::printDeclUSR(VD, OS))
      return None;
  }

  return getCommentForDeclByUSR(USRBuffer.str());
}

const static StringRef Separator = "/";

Optional<StringRef> ModuleFile::getGroupNameById(unsigned Id) const {
  if (!GroupNamesMap || GroupNamesMap->count(Id) == 0)
    return None;
  auto Original = (*GroupNamesMap)[Id];
  if (Original.empty())
    return None;
  auto SepPos = Original.find_last_of(Separator);
  assert(SepPos != StringRef::npos && "Cannot find Separator.");
  return StringRef(Original.data(), SepPos);
}

Optional<StringRef> ModuleFile::getSourceFileNameById(unsigned Id) const {
  if (!GroupNamesMap || GroupNamesMap->count(Id) == 0)
    return None;
  auto Original = (*GroupNamesMap)[Id];
  if (Original.empty())
    return None;
  auto SepPos = Original.find_last_of(Separator);
  assert(SepPos != StringRef::npos && "Cannot find Separator.");
  auto Start = Original.data() + SepPos + 1;
  auto Len = Original.size() - SepPos - 1;
  return StringRef(Start, Len);
}

Optional<StringRef> ModuleFile::getGroupNameForDecl(const Decl *D) const {
  auto Triple = getCommentForDecl(D);
  if (!Triple.hasValue()) {
    return None;
  }
  return getGroupNameById(Triple.getValue().Group);
}


Optional<StringRef>
ModuleFile::getSourceFileNameForDecl(const Decl *D) const {
  auto Triple = getCommentForDecl(D);
  if (!Triple.hasValue()) {
    return None;
  }
  return getSourceFileNameById(Triple.getValue().Group);
}

Optional<unsigned>
ModuleFile::getSourceOrderForDecl(const Decl *D) const {
  auto Triple = getCommentForDecl(D);
  if (!Triple.hasValue()) {
    return None;
  }
  return Triple.getValue().SourceOrder;
}

void ModuleFile::collectAllGroups(std::vector<StringRef> &Names) const {
  if (!GroupNamesMap)
    return;
  for (auto It = GroupNamesMap->begin(); It != GroupNamesMap->end(); ++ It) {
    StringRef FullGroupName = It->getSecond();
    if (FullGroupName.empty())
      continue;
    auto Sep = FullGroupName.find_last_of(Separator);
    assert(Sep != StringRef::npos);
    auto Group = FullGroupName.substr(0, Sep);
    auto Found = std::find(Names.begin(), Names.end(), Group);
    if (Found != Names.end())
      continue;
    Names.push_back(Group);
  }
}

Optional<CommentInfo>
ModuleFile::getCommentForDeclByUSR(StringRef USR) const {
  if (!DeclCommentTable)
    return None;

  auto I = DeclCommentTable->find(USR);
  if (I == DeclCommentTable->end())
    return None;

  return *I;
}

Optional<StringRef>
ModuleFile::getGroupNameByUSR(StringRef USR) const {
  if (auto Comment = getCommentForDeclByUSR(USR)) {
    return getGroupNameById(Comment.getValue().Group);
  }
  return None;
}

Identifier ModuleFile::getDiscriminatorForPrivateValue(const ValueDecl *D) {
  Identifier discriminator = PrivateDiscriminatorsByValue.lookup(D);
  assert(!discriminator.empty() && "no discriminator found for decl");
  return discriminator;
}

void ModuleFile::verify() const {
#ifndef NDEBUG
  const auto &Context = getContext();
  for (const Serialized<Decl*> &next : Decls)
    if (next.isComplete() && swift::shouldVerify(next, Context))
      swift::verify(next);
#endif
}

bool SerializedASTFile::hasEntryPoint() const {
  return File.Bits.HasEntryPoint;
}

ClassDecl *SerializedASTFile::getMainClass() const {
  assert(hasEntryPoint());
  return cast_or_null<ClassDecl>(File.getDecl(File.Bits.EntryPointDeclID));
}
