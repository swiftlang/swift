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

#include "ModuleFile.h"
#include "BCReadingExtras.h"
#include "DeserializationErrors.h"
#include "DocFormat.h"
#include "SourceInfoFormat.h"
#include "ModuleFormat.h"
#include "swift/Serialization/SerializationOptions.h"
#include "swift/Subsystems.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Basic/Range.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/Chrono.h"
#include "llvm/Support/DJB.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/OnDiskHashTable.h"

using namespace swift;
using namespace swift::serialization;
using namespace llvm::support;
using llvm::Expected;

static_assert(IsTriviallyDestructible<SerializedASTFile>::value,
              "SerializedASTFiles are BumpPtrAllocated; d'tors are not called");

static bool checkModuleSignature(llvm::BitstreamCursor &cursor,
                                 ArrayRef<unsigned char> signature) {
  for (unsigned char byte : signature) {
    if (cursor.AtEndOfStream())
      return false;
    if (Expected<llvm::SimpleBitstreamCursor::word_t> maybeRead =
            cursor.Read(8)) {
      if (maybeRead.get() != byte)
        return false;
    } else {
      consumeError(maybeRead.takeError());
      return false;
    }
  }
  return true;
}

static bool enterTopLevelModuleBlock(llvm::BitstreamCursor &cursor,
                                     unsigned ID,
                                     bool shouldReadBlockInfo = true) {
  Expected<llvm::BitstreamEntry> maybeNext = cursor.advance();
  if (!maybeNext) {
    // FIXME this drops the error on the floor.
    consumeError(maybeNext.takeError());
    return false;
  }
  llvm::BitstreamEntry next = maybeNext.get();

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

  if (llvm::Error Err = cursor.EnterSubBlock(ID)) {
    // FIXME this drops the error on the floor.
    consumeError(std::move(Err));
    return false;
  }

  return true;
}

/// Populate \p extendedInfo with the data from the options block.
///
/// Returns true on success.
static bool readOptionsBlock(llvm::BitstreamCursor &cursor,
                             SmallVectorImpl<uint64_t> &scratch,
                             ExtendedValidationInfo &extendedInfo) {
  while (!cursor.AtEndOfStream()) {
    Expected<llvm::BitstreamEntry> maybeEntry = cursor.advance();
    if (!maybeEntry) {
      // FIXME this drops the error on the floor.
      consumeError(maybeEntry.takeError());
      return false;
    }
    llvm::BitstreamEntry entry = maybeEntry.get();
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
    Expected<unsigned> maybeKind =
        cursor.readRecord(entry.ID, scratch, &blobData);
    if (!maybeKind) {
      // FIXME this drops the error on the floor.
      consumeError(maybeKind.takeError());
      return false;
    }
    unsigned kind = maybeKind.get();
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
    case options_block::ARE_PRIVATE_IMPORTS_ENABLED:
      extendedInfo.setPrivateImportsEnabled(true);
      break;
    case options_block::IS_IMPLICIT_DYNAMIC_ENABLED:
      extendedInfo.setImplicitDynamicEnabled(true);
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
                     std::pair<uint16_t, uint16_t> expectedVersion,
                     ExtendedValidationInfo *extendedInfo) {
  // The control block is malformed until we've at least read a major version
  // number.
  ValidationInfo result;
  bool versionSeen = false;

  while (!cursor.AtEndOfStream()) {
    Expected<llvm::BitstreamEntry> maybeEntry = cursor.advance();
    if (!maybeEntry) {
      // FIXME this drops the error on the floor.
      consumeError(maybeEntry.takeError());
      result.status = Status::Malformed;
      return result;
    }
    llvm::BitstreamEntry entry = maybeEntry.get();
    if (entry.Kind == llvm::BitstreamEntry::EndBlock)
      break;

    if (entry.Kind == llvm::BitstreamEntry::Error) {
      result.status = Status::Malformed;
      return result;
    }

    if (entry.Kind == llvm::BitstreamEntry::SubBlock) {
      if (entry.ID == OPTIONS_BLOCK_ID && extendedInfo) {
        if (llvm::Error Err = cursor.EnterSubBlock(OPTIONS_BLOCK_ID)) {
          // FIXME this drops the error on the floor.
          consumeError(std::move(Err));
          result.status = Status::Malformed;
          return result;
        }
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
    Expected<unsigned> maybeKind =
        cursor.readRecord(entry.ID, scratch, &blobData);
    if (!maybeKind) {
      // FIXME this drops the error on the floor.
      consumeError(maybeKind.takeError());
      result.status = Status::Malformed;
      return result;
    }
    unsigned kind = maybeKind.get();
    switch (kind) {
    case control_block::METADATA: {
      if (versionSeen) {
        result.status = Status::Malformed;
        break;
      }

      uint16_t versionMajor = scratch[0];
      if (versionMajor > expectedVersion.first)
        result.status = Status::FormatTooNew;
      else if (versionMajor < expectedVersion.first)
        result.status = Status::FormatTooOld;
      else
        result.status = Status::Valid;

      // Major version 0 does not have stable minor versions.
      if (versionMajor == 0) {
        uint16_t versionMinor = scratch[1];
        if (versionMinor != expectedVersion.second) {
          if (versionMinor < expectedVersion.second)
            result.status = Status::FormatTooOld;
          else
            result.status = Status::FormatTooNew;
        }
      }

      // These fields were added later; be resilient against their absence.
      switch (scratch.size()) {
      default:
        // Add new cases here, in descending order.
      case 4:
        if (scratch[3] != 0) {
          result.compatibilityVersion =
            version::Version(blobData.substr(scratch[2]+1, scratch[3]),
                             SourceLoc(), nullptr);
        }
        LLVM_FALLTHROUGH;
      case 3:
        result.shortVersion = blobData.slice(0, scratch[2]);
        LLVM_FALLTHROUGH;
      case 2:
      case 1:
      case 0:
        break;
      }

      result.miscVersion = blobData;
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

static bool validateInputBlock(
    llvm::BitstreamCursor &cursor, SmallVectorImpl<uint64_t> &scratch,
    SmallVectorImpl<SerializationOptions::FileDependency> &dependencies) {
  SmallVector<StringRef, 4> dependencyDirectories;
  SmallString<256> dependencyFullPathBuffer;

  while (!cursor.AtEndOfStream()) {
    Expected<llvm::BitstreamEntry> maybeEntry = cursor.advance();
    if (!maybeEntry) {
      // FIXME this drops the error on the floor.
      consumeError(maybeEntry.takeError());
      return true;
    }
    llvm::BitstreamEntry entry = maybeEntry.get();
    if (entry.Kind == llvm::BitstreamEntry::EndBlock)
      break;

    if (entry.Kind == llvm::BitstreamEntry::Error)
      return true;

    scratch.clear();
    StringRef blobData;
    Expected<unsigned> maybeKind =
        cursor.readRecord(entry.ID, scratch, &blobData);
    if (!maybeKind) {
      // FIXME this drops the error on the floor.
      consumeError(maybeKind.takeError());
      return true;
    }
    unsigned kind = maybeKind.get();
    switch (kind) {
    case input_block::FILE_DEPENDENCY: {
      bool isHashBased = scratch[2] != 0;
      bool isSDKRelative = scratch[3] != 0;

      StringRef path = blobData;
      size_t directoryIndex = scratch[4];
      if (directoryIndex != 0) {
        if (directoryIndex > dependencyDirectories.size())
          return true;
        dependencyFullPathBuffer = dependencyDirectories[directoryIndex-1];
        llvm::sys::path::append(dependencyFullPathBuffer, blobData);
        path = dependencyFullPathBuffer;
      }

      if (isHashBased) {
        dependencies.push_back(
          SerializationOptions::FileDependency::hashBased(
            path, isSDKRelative, scratch[0], scratch[1]));
      } else {
        dependencies.push_back(
          SerializationOptions::FileDependency::modTimeBased(
            path, isSDKRelative, scratch[0], scratch[1]));
      }
      break;
    }
    case input_block::DEPENDENCY_DIRECTORY:
      dependencyDirectories.push_back(blobData);
      break;
    default:
      // Unknown metadata record, possibly for use by a future version of the
      // module format.
      break;
    }
  }
  return false;
}


bool serialization::isSerializedAST(StringRef data) {
  StringRef signatureStr(reinterpret_cast<const char *>(SWIFTMODULE_SIGNATURE),
                         llvm::array_lengthof(SWIFTMODULE_SIGNATURE));
  return data.startswith(signatureStr);
}

ValidationInfo serialization::validateSerializedAST(
    StringRef data,
    ExtendedValidationInfo *extendedInfo,
    SmallVectorImpl<SerializationOptions::FileDependency> *dependencies) {
  ValidationInfo result;

  // Check 32-bit alignment.
  if (data.size() % SWIFTMODULE_ALIGNMENT != 0 ||
      reinterpret_cast<uintptr_t>(data.data()) % SWIFTMODULE_ALIGNMENT != 0)
    return result;

  llvm::BitstreamCursor cursor(data);
  SmallVector<uint64_t, 32> scratch;

  if (!checkModuleSignature(cursor, SWIFTMODULE_SIGNATURE) ||
      !enterTopLevelModuleBlock(cursor, MODULE_BLOCK_ID, false))
    return result;

  llvm::BitstreamEntry topLevelEntry;

  while (!cursor.AtEndOfStream()) {
    Expected<llvm::BitstreamEntry> maybeEntry =
        cursor.advance(AF_DontPopBlockAtEnd);
    if (!maybeEntry) {
      // FIXME this drops the error on the floor.
      consumeError(maybeEntry.takeError());
      result.status = Status::Malformed;
      return result;
    }
    topLevelEntry = maybeEntry.get();
    if (topLevelEntry.Kind != llvm::BitstreamEntry::SubBlock)
      break;

    if (topLevelEntry.ID == CONTROL_BLOCK_ID) {
      if (llvm::Error Err = cursor.EnterSubBlock(CONTROL_BLOCK_ID)) {
        // FIXME this drops the error on the floor.
        consumeError(std::move(Err));
        result.status = Status::Malformed;
        return result;
      }
      result = validateControlBlock(cursor, scratch,
                                    {SWIFTMODULE_VERSION_MAJOR,
                                     SWIFTMODULE_VERSION_MINOR},
                                    extendedInfo);
      if (result.status == Status::Malformed)
        return result;
    } else if (dependencies &&
               result.status == Status::Valid &&
               topLevelEntry.ID == INPUT_BLOCK_ID) {
      if (llvm::Error Err = cursor.EnterSubBlock(INPUT_BLOCK_ID)) {
        // FIXME this drops the error on the floor.
        consumeError(std::move(Err));
        result.status = Status::Malformed;
        return result;
      }
      if (validateInputBlock(cursor, scratch, *dependencies)) {
        result.status = Status::Malformed;
        return result;
      }
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
      return llvm::djbHash(key.second, SWIFTMODULE_HASH_SEED);
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
    case static_cast<uint8_t>(DeclNameKind::Normal): {
      StringRef str(reinterpret_cast<const char *>(data),
                    length - sizeof(uint8_t));
      return {DeclBaseName::Kind::Normal, str};
    }
    case static_cast<uint8_t>(DeclNameKind::Subscript):
      return {DeclBaseName::Kind::Subscript, StringRef()};
    case static_cast<uint8_t>(DeclNameKind::Destructor):
      return {DeclBaseName::Kind::Destructor, StringRef()};
    default:
      llvm_unreachable("Unknown DeclNameKind");
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
    return llvm::djbHash(key, SWIFTMODULE_HASH_SEED);
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
        if (module)
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
  using data_type = DeclID;
  using hash_value_type = uint32_t;
  using offset_type = unsigned;

  internal_key_type GetInternalKey(external_key_type ID) {
    return ID;
  }

  external_key_type GetExternalKey(internal_key_type ID) {
    return ID;
  }

  hash_value_type ComputeHash(internal_key_type key) {
    return llvm::djbHash(key, SWIFTMODULE_HASH_SEED);
  }

  static bool EqualKey(internal_key_type lhs, internal_key_type rhs) {
    return lhs == rhs;
  }

  static std::pair<unsigned, unsigned> ReadKeyDataLength(const uint8_t *&data) {
    unsigned keyLength = endian::readNext<uint16_t, little, unaligned>(data);
    return { keyLength, sizeof(uint32_t) };
  }

  static internal_key_type ReadKey(const uint8_t *data, unsigned length) {
    return StringRef(reinterpret_cast<const char *>(data), length);
  }

  static data_type ReadData(internal_key_type key, const uint8_t *data,
                            unsigned length) {
    return endian::readNext<uint32_t, little, unaligned>(data);
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
    return llvm::djbHash(key, SWIFTMODULE_HASH_SEED);
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

// Indexing the members of all Decls (well, NominalTypeDecls anyway) is
// accomplished by a 2-level hashtable scheme. The outer table here maps from
// DeclBaseNames to serialization::BitOffsets of sub-tables. The sub-tables --
// SerializedDeclMembersTables, one table per name -- map from the enclosing
// (NominalTypeDecl) DeclID to a vector of DeclIDs of members of the nominal
// with the name. See DeclMembersTableInfo below.
class ModuleFile::DeclMemberNamesTableInfo {
public:
  using internal_key_type = std::pair<DeclBaseName::Kind, StringRef>;
  using external_key_type = DeclBaseName;
  using data_type = serialization::BitOffset;
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
      return llvm::djbHash(key.second, SWIFTMODULE_HASH_SEED);
    } else {
      return (hash_value_type)key.first;
    }
  }

  static bool EqualKey(internal_key_type lhs, internal_key_type rhs) {
    return lhs == rhs;
  }

  static std::pair<unsigned, unsigned> ReadKeyDataLength(const uint8_t *&data) {
    unsigned keyLength = endian::readNext<uint16_t, little, unaligned>(data);
    return { keyLength, sizeof(uint32_t) };
  }

  static internal_key_type ReadKey(const uint8_t *data, unsigned length) {
    uint8_t kind = endian::readNext<uint8_t, little, unaligned>(data);
    switch (kind) {
    case static_cast<uint8_t>(DeclNameKind::Normal): {
      StringRef str(reinterpret_cast<const char *>(data),
                    length - sizeof(uint8_t));
      return {DeclBaseName::Kind::Normal, str};
    }
    case static_cast<uint8_t>(DeclNameKind::Subscript):
      return {DeclBaseName::Kind::Subscript, StringRef()};
    case static_cast<uint8_t>(DeclNameKind::Destructor):
      return {DeclBaseName::Kind::Destructor, StringRef()};
    case static_cast<uint8_t>(DeclNameKind::Constructor):
      return {DeclBaseName::Kind::Constructor, StringRef()};
    default:
      llvm_unreachable("Unknown DeclNameKind");
    }
  }

  static data_type ReadData(internal_key_type key, const uint8_t *data,
                            unsigned length) {
    assert(length == sizeof(uint32_t));
    return endian::readNext<uint32_t, little, unaligned>(data);
  }
};

// Second half of the 2-level member name lookup scheme, see
// DeclMemberNamesTableInfo above. There is one of these tables for each member
// DeclBaseName N in the module, and it maps from enclosing DeclIDs (say: each
// NominalTypeDecl T that has members named N) to the set of N-named members of
// T. In other words, there are no names in this table: the names are one level
// up, this table just maps { Owner-DeclID => [Member-DeclID, ...] }.
class ModuleFile::DeclMembersTableInfo {
public:
  using internal_key_type = uint32_t;
  using external_key_type = DeclID;
  using data_type = SmallVector<DeclID, 2>;
  using hash_value_type = uint32_t;
  using offset_type = unsigned;

  internal_key_type GetInternalKey(external_key_type ID) {
    return ID;
  }

  hash_value_type ComputeHash(internal_key_type key) {
    return llvm::hash_value(key);
  }

  static bool EqualKey(internal_key_type lhs, internal_key_type rhs) {
    return lhs == rhs;
  }

  static std::pair<unsigned, unsigned> ReadKeyDataLength(const uint8_t *&data) {
    unsigned dataLength = endian::readNext<uint16_t, little, unaligned>(data);
    return { sizeof(uint32_t), dataLength };
  }

  static internal_key_type ReadKey(const uint8_t *data, unsigned length) {
    return endian::readNext<uint32_t, little, unaligned>(data);
  }

  static data_type ReadData(internal_key_type key, const uint8_t *data,
                            unsigned length) {
    data_type result;
    while (length > 0) {
      DeclID declID = endian::readNext<uint32_t, little, unaligned>(data);
      result.push_back(declID);
      length -= sizeof(uint32_t);
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

std::unique_ptr<ModuleFile::SerializedDeclMemberNamesTable>
ModuleFile::readDeclMemberNamesTable(ArrayRef<uint64_t> fields,
                                     StringRef blobData) {
  uint32_t tableOffset;
  index_block::DeclMemberNamesLayout::readRecord(fields, tableOffset);
  auto base = reinterpret_cast<const uint8_t *>(blobData.data());

  using OwnedTable = std::unique_ptr<SerializedDeclMemberNamesTable>;
  return OwnedTable(SerializedDeclMemberNamesTable::Create(base + tableOffset,
      base + sizeof(uint32_t), base));
}

std::unique_ptr<ModuleFile::SerializedDeclMembersTable>
ModuleFile::readDeclMembersTable(ArrayRef<uint64_t> fields,
                                     StringRef blobData) {
  uint32_t tableOffset;
  decl_member_tables_block::DeclMembersLayout::readRecord(fields,
                                                          tableOffset);
  auto base = reinterpret_cast<const uint8_t *>(blobData.data());

  using OwnedTable = std::unique_ptr<SerializedDeclMembersTable>;
  return OwnedTable(SerializedDeclMembersTable::Create(base + tableOffset,
      base + sizeof(uint32_t), base));
}

/// Used to deserialize entries in the on-disk Objective-C method table.
class ModuleFile::ObjCMethodTableInfo {
public:
  using internal_key_type = std::string;
  using external_key_type = ObjCSelector;
  using data_type = SmallVector<std::tuple<std::string, bool, DeclID>, 8>;
  using hash_value_type = uint32_t;
  using offset_type = unsigned;

  internal_key_type GetInternalKey(external_key_type ID) {
    llvm::SmallString<32> scratch;
    return ID.getString(scratch).str();
  }

  hash_value_type ComputeHash(internal_key_type key) {
    return llvm::djbHash(key, SWIFTMODULE_HASH_SEED);
  }

  static bool EqualKey(internal_key_type lhs, internal_key_type rhs) {
    return lhs == rhs;
  }

  static std::pair<unsigned, unsigned> ReadKeyDataLength(const uint8_t *&data) {
    unsigned keyLength = endian::readNext<uint16_t, little, unaligned>(data);
    unsigned dataLength = endian::readNext<uint32_t, little, unaligned>(data);
    return { keyLength, dataLength };
  }

  static internal_key_type ReadKey(const uint8_t *data, unsigned length) {
    return std::string(reinterpret_cast<const char *>(data), length);
  }

  static data_type ReadData(internal_key_type key, const uint8_t *data,
                            unsigned length) {
    const constexpr auto recordSize = sizeof(uint32_t) + 1 + sizeof(uint32_t);
    data_type result;
    while (length > 0) {
      unsigned ownerLen = endian::readNext<uint32_t, little, unaligned>(data);
      bool isInstanceMethod = *data++ != 0;
      DeclID methodID = endian::readNext<uint32_t, little, unaligned>(data);
      std::string ownerName((const char *)data, ownerLen);
      result.push_back(
        std::make_tuple(std::move(ownerName), isInstanceMethod, methodID));
      data += ownerLen;
      length -= (recordSize + ownerLen);
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

/// Used to deserialize entries in the on-disk derivative function configuration
/// table.
class ModuleFile::DerivativeFunctionConfigTableInfo {
public:
  using internal_key_type = StringRef;
  using external_key_type = internal_key_type;
  using data_type = SmallVector<std::pair<std::string, GenericSignatureID>, 8>;
  using hash_value_type = uint32_t;
  using offset_type = unsigned;

  external_key_type GetExternalKey(internal_key_type ID) { return ID; }

  internal_key_type GetInternalKey(external_key_type ID) { return ID; }

  hash_value_type ComputeHash(internal_key_type key) {
    return llvm::djbHash(key, SWIFTMODULE_HASH_SEED);
  }

  static bool EqualKey(internal_key_type lhs, internal_key_type rhs) {
    return lhs == rhs;
  }

  static std::pair<unsigned, unsigned> ReadKeyDataLength(const uint8_t *&data) {
    unsigned keyLength = endian::readNext<uint16_t, little, unaligned>(data);
    unsigned dataLength = endian::readNext<uint16_t, little, unaligned>(data);
    return {keyLength, dataLength};
  }

  static internal_key_type ReadKey(const uint8_t *data, unsigned length) {
    return StringRef(reinterpret_cast<const char *>(data), length);
  }

  static data_type ReadData(internal_key_type key, const uint8_t *data,
                            unsigned length) {
    data_type result;
    const uint8_t *limit = data + length;
    while (data < limit) {
      DeclID genSigId = endian::readNext<uint32_t, little, unaligned>(data);
      int32_t nameLength = endian::readNext<int32_t, little, unaligned>(data);
      StringRef mangledName(reinterpret_cast<const char *>(data), nameLength);
      data += nameLength;
      result.push_back({mangledName, genSigId});
    }
    return result;
  }
};

std::unique_ptr<ModuleFile::SerializedDerivativeFunctionConfigTable>
ModuleFile::readDerivativeFunctionConfigTable(ArrayRef<uint64_t> fields,
                                              StringRef blobData) {
  uint32_t tableOffset;
  index_block::DerivativeFunctionConfigTableLayout::readRecord(fields,
                                                               tableOffset);
  auto base = reinterpret_cast<const uint8_t *>(blobData.data());

  using OwnedTable = std::unique_ptr<SerializedDerivativeFunctionConfigTable>;
  return OwnedTable(SerializedDerivativeFunctionConfigTable::Create(
      base + tableOffset, base + sizeof(uint32_t), base));
}

bool ModuleFile::readIndexBlock(llvm::BitstreamCursor &cursor) {
  if (llvm::Error Err = cursor.EnterSubBlock(INDEX_BLOCK_ID)) {
    // FIXME this drops the error on the floor.
    consumeError(std::move(Err));
    return false;
  }

  SmallVector<uint64_t, 4> scratch;
  StringRef blobData;

  while (!cursor.AtEndOfStream()) {
    Expected<llvm::BitstreamEntry> maybeEntry = cursor.advance();
    if (!maybeEntry) {
      // FIXME this drops the error on the floor.
      consumeError(maybeEntry.takeError());
      return false;
    }
    llvm::BitstreamEntry entry = maybeEntry.get();
    switch (entry.Kind) {
    case llvm::BitstreamEntry::EndBlock:
      return true;

    case llvm::BitstreamEntry::Error:
      return false;

    case llvm::BitstreamEntry::SubBlock:
      if (entry.ID == DECL_MEMBER_TABLES_BLOCK_ID) {
        DeclMemberTablesCursor = cursor;
        if (llvm::Error Err = DeclMemberTablesCursor.EnterSubBlock(
                DECL_MEMBER_TABLES_BLOCK_ID)) {
          // FIXME this drops the error on the floor.
          consumeError(std::move(Err));
          return false;
        }
        llvm::BitstreamEntry subentry;
        do {
          // Scan forward, to load the cursor with any abbrevs we'll need while
          // seeking inside this block later.
          Expected<llvm::BitstreamEntry> maybeEntry =
              DeclMemberTablesCursor.advance(
                  llvm::BitstreamCursor::AF_DontPopBlockAtEnd);
          if (!maybeEntry) {
            // FIXME this drops the error on the floor.
            consumeError(maybeEntry.takeError());
            return false;
          }
          subentry = maybeEntry.get();
        } while (!DeclMemberTablesCursor.AtEndOfStream() &&
                 subentry.Kind != llvm::BitstreamEntry::Record &&
                 subentry.Kind != llvm::BitstreamEntry::EndBlock);
      }
      if (cursor.SkipBlock())
        return false;
      break;

    case llvm::BitstreamEntry::Record:
      scratch.clear();
      blobData = {};
      Expected<unsigned> maybeKind =
          cursor.readRecord(entry.ID, scratch, &blobData);
      if (!maybeKind) {
        // FIXME this drops the error on the floor.
        consumeError(maybeKind.takeError());
        return false;
      }
      unsigned kind = maybeKind.get();

      switch (kind) {
      case index_block::DECL_OFFSETS:
        assert(blobData.empty());
        allocateBuffer(Decls, scratch);
        break;
      case index_block::TYPE_OFFSETS:
        assert(blobData.empty());
        allocateBuffer(Types, scratch);
        break;
      case index_block::CLANG_TYPE_OFFSETS:
        assert(blobData.empty());
        allocateBuffer(ClangTypes, scratch);
        break;
      case index_block::IDENTIFIER_OFFSETS:
        assert(blobData.empty());
        allocateBuffer(Identifiers, scratch);
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
      case index_block::CLASS_MEMBERS_FOR_DYNAMIC_LOOKUP:
        ClassMembersForDynamicLookup = readDeclTable(scratch, blobData);
        break;
      case index_block::OPERATOR_METHODS:
        OperatorMethodDecls = readDeclTable(scratch, blobData);
        break;
      case index_block::OBJC_METHODS:
        ObjCMethods = readObjCMethodTable(scratch, blobData);
        break;
      case index_block::DERIVATIVE_FUNCTION_CONFIGURATIONS:
        DerivativeFunctionConfigurations =
            readDerivativeFunctionConfigTable(scratch, blobData);
        break;
      case index_block::ENTRY_POINT:
        assert(blobData.empty());
        setEntryPointClassID(scratch.front());
        break;
      case index_block::ORDERED_TOP_LEVEL_DECLS:
        allocateBuffer(OrderedTopLevelDecls, scratch);
        break;
      case index_block::LOCAL_TYPE_DECLS:
        LocalTypeDecls = readLocalDeclTable(scratch, blobData);
        break;
      case index_block::OPAQUE_RETURN_TYPE_DECLS:
        OpaqueReturnTypeDecls = readLocalDeclTable(scratch, blobData);
        break;
      case index_block::NESTED_TYPE_DECLS:
        NestedTypeDecls = readNestedTypeDeclsTable(scratch, blobData);
        break;
      case index_block::DECL_MEMBER_NAMES:
        DeclMemberNames = readDeclMemberNamesTable(scratch, blobData);
        break;
      case index_block::LOCAL_DECL_CONTEXT_OFFSETS:
        assert(blobData.empty());
        allocateBuffer(LocalDeclContexts, scratch);
        break;
      case index_block::GENERIC_SIGNATURE_OFFSETS:
        assert(blobData.empty());
        allocateBuffer(GenericSignatures, scratch);
        break;
      case index_block::SUBSTITUTION_MAP_OFFSETS:
        assert(blobData.empty());
        allocateBuffer(SubstitutionMaps, scratch);
        break;
      case index_block::NORMAL_CONFORMANCE_OFFSETS:
        assert(blobData.empty());
        allocateBuffer(NormalConformances, scratch);
        break;
      case index_block::SIL_LAYOUT_OFFSETS:
        assert(blobData.empty());
        allocateBuffer(SILLayouts, scratch);
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
    return llvm::djbHash(key, SWIFTDOC_HASH_SEED_5_1);
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
  if (fields.empty() || blobData.empty())
    return nullptr;
  uint32_t tableOffset = static_cast<uint32_t>(fields.front());
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
  for (unsigned I = 0; I < GroupCount; ++I) {
    auto RawSize = endian::readNext<uint32_t, little, unaligned>(Data);
    auto RawText = StringRef(reinterpret_cast<const char *>(Data), RawSize);
    Data += RawSize;
    (*pMap)[I] = RawText;
  }
  return pMap;
}

bool ModuleFile::readCommentBlock(llvm::BitstreamCursor &cursor) {
  if (llvm::Error Err = cursor.EnterSubBlock(COMMENT_BLOCK_ID)) {
    // FIXME this drops the error on the floor.
    consumeError(std::move(Err));
    return false;
  }

  SmallVector<uint64_t, 4> scratch;
  StringRef blobData;

  while (!cursor.AtEndOfStream()) {
    Expected<llvm::BitstreamEntry> maybeEntry = cursor.advance();
    if (!maybeEntry) {
      // FIXME this drops the error on the floor.
      consumeError(maybeEntry.takeError());
      return false;
    }
    llvm::BitstreamEntry entry = maybeEntry.get();
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
      Expected<unsigned> maybeKind =
          cursor.readRecord(entry.ID, scratch, &blobData);
      if (!maybeKind) {
        // FIXME this drops the error on the floor.
        consumeError(maybeKind.takeError());
        return false;
      }
      unsigned kind = maybeKind.get();

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

static Optional<ModuleDecl::ImportFilterKind>
getActualImportControl(unsigned rawValue) {
  // We switch on the raw value rather than the enum in order to handle future
  // values.
  switch (rawValue) {
  case static_cast<unsigned>(serialization::ImportControl::Normal):
    return ModuleDecl::ImportFilterKind::Private;
  case static_cast<unsigned>(serialization::ImportControl::Exported):
    return ModuleDecl::ImportFilterKind::Public;
  case static_cast<unsigned>(serialization::ImportControl::ImplementationOnly):
    return ModuleDecl::ImportFilterKind::ImplementationOnly;
  default:
    return None;
  }
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
  if ((!moduleTarget.hasEnvironment() && ctxTarget.isSimulatorEnvironment()) ||
      (!ctxTarget.hasEnvironment() && moduleTarget.isSimulatorEnvironment()))
    return false;

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

bool ModuleFile::readModuleDocIfPresent() {
  if (!this->ModuleDocInputBuffer)
    return true;

  llvm::BitstreamCursor docCursor{ModuleDocInputBuffer->getMemBufferRef()};
  if (!checkModuleSignature(docCursor, SWIFTDOC_SIGNATURE) ||
      !enterTopLevelModuleBlock(docCursor, MODULE_DOC_BLOCK_ID)) {
    return false;
  }

  SmallVector<uint64_t, 64> scratch;
  llvm::BitstreamEntry topLevelEntry;

  bool hasValidControlBlock = false;
  ValidationInfo info;

  while (!docCursor.AtEndOfStream()) {
    Expected<llvm::BitstreamEntry> maybeEntry =
        docCursor.advance(AF_DontPopBlockAtEnd);
    if (!maybeEntry) {
      // FIXME this drops the error on the floor.
      consumeError(maybeEntry.takeError());
      return false;
    }
    topLevelEntry = maybeEntry.get();
    if (topLevelEntry.Kind != llvm::BitstreamEntry::SubBlock)
      break;

    switch (topLevelEntry.ID) {
    case CONTROL_BLOCK_ID: {
      if (llvm::Error Err = docCursor.EnterSubBlock(CONTROL_BLOCK_ID)) {
        // FIXME this drops the error on the floor.
        consumeError(std::move(Err));
        return false;
      }

      info = validateControlBlock(docCursor, scratch,
                                  {SWIFTDOC_VERSION_MAJOR,
                                   SWIFTDOC_VERSION_MINOR},
                                  /*extendedInfo*/nullptr);
      if (info.status != Status::Valid)
        return false;
      // Check that the swiftdoc is actually for this module.
      if (info.name != Name)
        return false;
      hasValidControlBlock = true;
      break;
    }

    case COMMENT_BLOCK_ID: {
      if (!hasValidControlBlock || !readCommentBlock(docCursor))
        return false;
      break;
    }

    default:
      // Unknown top-level block, possibly for use by a future version of the
      // module format.
      if (docCursor.SkipBlock())
        return false;
      break;
    }
  }

  if (topLevelEntry.Kind != llvm::BitstreamEntry::EndBlock)
    return false;

  return true;
}

class ModuleFile::DeclUSRTableInfo {
public:
  using internal_key_type = StringRef;
  using external_key_type = StringRef;
  using data_type = uint32_t;
  using hash_value_type = uint32_t;
  using offset_type = unsigned;

  internal_key_type GetInternalKey(external_key_type key) { return key; }

  hash_value_type ComputeHash(internal_key_type key) {
    assert(!key.empty());
    return llvm::djbHash(key, SWIFTSOURCEINFO_HASH_SEED);
  }

  static bool EqualKey(internal_key_type lhs, internal_key_type rhs) {
    return lhs == rhs;
  }

  static std::pair<unsigned, unsigned> ReadKeyDataLength(const uint8_t *&data) {
    unsigned keyLength = endian::readNext<uint32_t, little, unaligned>(data);
    unsigned dataLength = 4;
    return { keyLength, dataLength };
  }

  static internal_key_type ReadKey(const uint8_t *data, unsigned length) {
    return StringRef(reinterpret_cast<const char*>(data), length);
  }

  data_type ReadData(internal_key_type key, const uint8_t *data, unsigned length) {
    assert(length == 4);
    return endian::readNext<uint32_t, little, unaligned>(data);
  }
};

std::unique_ptr<ModuleFile::SerializedDeclUSRTable>
ModuleFile::readDeclUSRsTable(ArrayRef<uint64_t> fields, StringRef blobData) {
  if (fields.empty() || blobData.empty())
    return nullptr;
  uint32_t tableOffset = static_cast<uint32_t>(fields.front());
  auto base = reinterpret_cast<const uint8_t *>(blobData.data());
  return std::unique_ptr<SerializedDeclUSRTable>(
    SerializedDeclUSRTable::Create(base + tableOffset, base + sizeof(uint32_t),
                                   base));
}

bool ModuleFile::readDeclLocsBlock(llvm::BitstreamCursor &cursor) {
  if (llvm::Error Err = cursor.EnterSubBlock(CONTROL_BLOCK_ID)) {
    consumeError(std::move(Err));
    return false;
  }

  SmallVector<uint64_t, 4> scratch;
  StringRef blobData;

  while (!cursor.AtEndOfStream()) {
    Expected<llvm::BitstreamEntry> entry = cursor.advance();
    if (!entry) {
      consumeError(entry.takeError());
      return false;
    }
    switch (entry->Kind) {
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
      Expected<unsigned> kind =
          cursor.readRecord(entry->ID, scratch, &blobData);
      if (!kind) {
        consumeError(kind.takeError());
        return false;
      }
      switch (*kind) {
      case decl_locs_block::BASIC_DECL_LOCS:
        BasicDeclLocsData = blobData;
        break;
      case decl_locs_block::TEXT_DATA:
        SourceLocsTextData = blobData;
        break;
      case decl_locs_block::DECL_USRS:
        DeclUSRsTable = readDeclUSRsTable(scratch, blobData);
        break;
      case decl_locs_block::DOC_RANGES:
        DocRangesData = blobData;
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

bool ModuleFile::readModuleSourceInfoIfPresent() {
  if (!this->ModuleSourceInfoInputBuffer)
    return true;

  llvm::BitstreamCursor infoCursor{ModuleSourceInfoInputBuffer->getMemBufferRef()};
  if (!checkModuleSignature(infoCursor, SWIFTSOURCEINFO_SIGNATURE) ||
      !enterTopLevelModuleBlock(infoCursor, MODULE_SOURCEINFO_BLOCK_ID)) {
    return false;
  }

  SmallVector<uint64_t, 64> scratch;

  bool hasValidControlBlock = false;
  ValidationInfo info;
  unsigned kind = llvm::BitstreamEntry::Error;

  while (!infoCursor.AtEndOfStream()) {
    Expected<llvm::BitstreamEntry> topLevelEntry =
        infoCursor.advance(AF_DontPopBlockAtEnd);
    if (!topLevelEntry) {
      consumeError(topLevelEntry.takeError());
      return false;
    }
    kind = topLevelEntry->Kind;
    if (kind != llvm::BitstreamEntry::SubBlock)
      break;

    switch (topLevelEntry->ID) {
    case CONTROL_BLOCK_ID: {
      if (llvm::Error Err = infoCursor.EnterSubBlock(CONTROL_BLOCK_ID)) {
        consumeError(std::move(Err));
        return false;
      }
      info = validateControlBlock(infoCursor, scratch,
                                  {SWIFTSOURCEINFO_VERSION_MAJOR,
                                   SWIFTSOURCEINFO_VERSION_MINOR},
                                  /*extendedInfo*/nullptr);
      if (info.status != Status::Valid)
        return false;
      // Check that the swiftsourceinfo is actually for this module.
      if (info.name != Name)
        return false;
      hasValidControlBlock = true;
      break;
    }

    case DECL_LOCS_BLOCK_ID: {
      if (!hasValidControlBlock || !readDeclLocsBlock(infoCursor))
        return false;
      break;
    }

    default:
      // Unknown top-level block, possibly for use by a future version of the
      // module format.
      if (infoCursor.SkipBlock())
        return false;
      break;
    }
  }

  if (kind != llvm::BitstreamEntry::EndBlock)
    return false;

  return true;
}

ModuleFile::ModuleFile(
    std::unique_ptr<llvm::MemoryBuffer> moduleInputBuffer,
    std::unique_ptr<llvm::MemoryBuffer> moduleDocInputBuffer,
    std::unique_ptr<llvm::MemoryBuffer> moduleSourceInfoInputBuffer,
    bool isFramework, serialization::ValidationInfo &info,
    serialization::ExtendedValidationInfo *extInfo)
    : ModuleInputBuffer(std::move(moduleInputBuffer)),
      ModuleDocInputBuffer(std::move(moduleDocInputBuffer)),
      ModuleSourceInfoInputBuffer(std::move(moduleSourceInfoInputBuffer)),
      DeserializedTypeCallback([](Type ty) {}) {
  assert(!hasError());
  Bits.IsFramework = isFramework;

  PrettyStackTraceModuleFile stackEntry(*this);

  llvm::BitstreamCursor cursor{ModuleInputBuffer->getMemBufferRef()};

  if (!checkModuleSignature(cursor, SWIFTMODULE_SIGNATURE) ||
      !enterTopLevelModuleBlock(cursor, MODULE_BLOCK_ID)) {
    info.status = error(Status::Malformed);
    return;
  }

  // Future-proofing: make sure we validate the control block before we try to
  // read any other blocks.
  bool hasValidControlBlock = false;
  SmallVector<uint64_t, 64> scratch;

  llvm::BitstreamEntry topLevelEntry;

  while (!cursor.AtEndOfStream()) {
    Expected<llvm::BitstreamEntry> maybeEntry =
        cursor.advance(AF_DontPopBlockAtEnd);
    if (!maybeEntry) {
      // FIXME this drops the error diagnostic on the floor.
      consumeError(maybeEntry.takeError());
      info.status = error(Status::Malformed);
      return;
    }
    topLevelEntry = maybeEntry.get();
    if (topLevelEntry.Kind != llvm::BitstreamEntry::SubBlock)
      break;

    switch (topLevelEntry.ID) {
    case CONTROL_BLOCK_ID: {
      if (llvm::Error Err = cursor.EnterSubBlock(CONTROL_BLOCK_ID)) {
        // FIXME this drops the error on the floor.
        consumeError(std::move(Err));
        info.status = error(Status::Malformed);
        return;
      }

      info = validateControlBlock(cursor, scratch,
                                  {SWIFTMODULE_VERSION_MAJOR,
                                   SWIFTMODULE_VERSION_MINOR},
                                  extInfo);
      if (info.status != Status::Valid) {
        error(info.status);
        return;
      }
      Name = info.name;
      TargetTriple = info.targetTriple;
      CompatibilityVersion = info.compatibilityVersion;
      IsSIB = extInfo->isSIB();
      MiscVersion = info.miscVersion;

      hasValidControlBlock = true;
      break;
    }

    case INPUT_BLOCK_ID: {
      if (!hasValidControlBlock) {
        info.status = error(Status::Malformed);
        return;
      }

      if (llvm::Error Err = cursor.EnterSubBlock(INPUT_BLOCK_ID)) {
        // FIXME this drops the error on the floor.
        consumeError(std::move(Err));
        info.status = error(Status::Malformed);
        return;
      }

      Expected<llvm::BitstreamEntry> maybeNext = cursor.advance();
      if (!maybeNext) {
        // FIXME this drops the error on the floor.
        consumeError(maybeNext.takeError());
        info.status = error(Status::Malformed);
        return;
      }
      llvm::BitstreamEntry next = maybeNext.get();
      while (next.Kind == llvm::BitstreamEntry::Record) {
        scratch.clear();
        StringRef blobData;
        Expected<unsigned> maybeKind =
            cursor.readRecord(next.ID, scratch, &blobData);
        if (!maybeKind) {
          // FIXME this drops the error on the floor.
          consumeError(maybeKind.takeError());
          info.status = error(Status::Malformed);
          return;
        }
        unsigned kind = maybeKind.get();
        switch (kind) {
        case input_block::IMPORTED_MODULE: {
          unsigned rawImportControl;
          bool scoped;
          bool hasSPI;
          input_block::ImportedModuleLayout::readRecord(scratch,
                                                        rawImportControl,
                                                        scoped, hasSPI);
          auto importKind = getActualImportControl(rawImportControl);
          if (!importKind) {
            // We don't know how to import this dependency.
            info.status = error(Status::Malformed);
            return;
          }

          StringRef spiBlob;
          if (hasSPI) {
            scratch.clear();

            llvm::BitstreamEntry entry =
                fatalIfUnexpected(cursor.advance(AF_DontPopBlockAtEnd));
            unsigned recordID =
                fatalIfUnexpected(cursor.readRecord(entry.ID, scratch, &spiBlob));
            assert(recordID == input_block::IMPORTED_MODULE_SPIS);
            input_block::ImportedModuleLayoutSPI::readRecord(scratch);
            (void) recordID;
          } else {
            spiBlob = StringRef();
          }

          Dependencies.push_back({blobData, spiBlob, importKind.getValue(), scoped});
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
        case input_block::SEARCH_PATH: {
          bool isFramework;
          bool isSystem;
          input_block::SearchPathLayout::readRecord(scratch, isFramework,
                                                    isSystem);
          SearchPaths.push_back({blobData, isFramework, isSystem});
          break;
        }
        case input_block::MODULE_INTERFACE_PATH: {
          ModuleInterfacePath = blobData;
          break;
        }
        default:
          // Unknown input kind, possibly for use by a future version of the
          // module format.
          // FIXME: Should we warn about this?
          break;
        }

        maybeNext = cursor.advance();
        if (!maybeNext) {
          // FIXME this drops the error on the floor.
          consumeError(maybeNext.takeError());
          info.status = error(Status::Malformed);
          return;
        }
        next = maybeNext.get();
      }

      if (next.Kind != llvm::BitstreamEntry::EndBlock)
        info.status = error(Status::Malformed);

      break;
    }

    case DECLS_AND_TYPES_BLOCK_ID: {
      if (!hasValidControlBlock) {
        info.status = error(Status::Malformed);
        return;
      }

      // The decls-and-types block is lazily loaded. Save the cursor and load
      // any abbrev records at the start of the block.
      DeclTypeCursor = cursor;
      if (llvm::Error Err =
              DeclTypeCursor.EnterSubBlock(DECLS_AND_TYPES_BLOCK_ID)) {
        // FIXME this drops the error on the floor.
        consumeError(std::move(Err));
        info.status = error(Status::Malformed);
        return;
      }

      Expected<llvm::BitstreamEntry> maybeCursor = DeclTypeCursor.advance();
      if (!maybeCursor) {
        // FIXME this drops the error on the floor.
        consumeError(maybeCursor.takeError());
        info.status = error(Status::Malformed);
        return;
      }
      if (maybeCursor.get().Kind == llvm::BitstreamEntry::Error)
        info.status = error(Status::Malformed);

      // With the main cursor, skip over the block and continue.
      if (cursor.SkipBlock()) {
        info.status = error(Status::Malformed);
        return;
      }
      break;
    }

    case IDENTIFIER_DATA_BLOCK_ID: {
      if (!hasValidControlBlock) {
        info.status = error(Status::Malformed);
        return;
      }

      if (llvm::Error Err = cursor.EnterSubBlock(IDENTIFIER_DATA_BLOCK_ID)) {
        // FIXME this drops the error on the floor.
        consumeError(std::move(Err));
        info.status = error(Status::Malformed);
        return;
      }

      Expected<llvm::BitstreamEntry> maybeNext =
          cursor.advanceSkippingSubblocks();
      if (!maybeNext) {
        // FIXME this drops the error on the floor.
        consumeError(maybeNext.takeError());
        info.status = error(Status::Malformed);
        return;
      }
      llvm::BitstreamEntry next = maybeNext.get();
      while (next.Kind == llvm::BitstreamEntry::Record) {
        scratch.clear();
        StringRef blobData;
        Expected<unsigned> maybeKind =
            cursor.readRecord(next.ID, scratch, &blobData);
        if (!maybeKind) {
          // FIXME this drops the error on the floor.
          consumeError(maybeKind.takeError());
          info.status = error(Status::Malformed);
          return;
        }
        unsigned kind = maybeKind.get();

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

        maybeNext = cursor.advanceSkippingSubblocks();
        if (!maybeNext) {
          // FIXME this drops the error on the floor.
          consumeError(maybeNext.takeError());
          info.status = error(Status::Malformed);
          return;
        }
        next = maybeNext.get();
      }

      if (next.Kind != llvm::BitstreamEntry::EndBlock) {
        info.status = error(Status::Malformed);
        return;
      }

      break;
    }

    case INDEX_BLOCK_ID: {
      if (!hasValidControlBlock || !readIndexBlock(cursor)) {
        info.status = error(Status::Malformed);
        return;
      }
      break;
    }

    case SIL_INDEX_BLOCK_ID: {
      // Save the cursor.
      SILIndexCursor = cursor;
      if (llvm::Error Err = SILIndexCursor.EnterSubBlock(SIL_INDEX_BLOCK_ID)) {
        // FIXME this drops the error on the floor.
        consumeError(std::move(Err));
        info.status = error(Status::Malformed);
        return;
      }

      // With the main cursor, skip over the block and continue.
      if (cursor.SkipBlock()) {
        info.status = error(Status::Malformed);
        return;
      }
      break;
    }

    case SIL_BLOCK_ID: {
      // Save the cursor.
      SILCursor = cursor;
      if (llvm::Error Err = SILCursor.EnterSubBlock(SIL_BLOCK_ID)) {
        // FIXME this drops the error on the floor.
        consumeError(std::move(Err));
        info.status = error(Status::Malformed);
        return;
      }

      // With the main cursor, skip over the block and continue.
      if (cursor.SkipBlock()) {
        info.status = error(Status::Malformed);
        return;
      }
      break;
    }

    default:
      // Unknown top-level block, possibly for use by a future version of the
      // module format.
      if (cursor.SkipBlock()) {
        info.status = error(Status::Malformed);
        return;
      }
      break;
    }
  }

  if (topLevelEntry.Kind != llvm::BitstreamEntry::EndBlock) {
    info.status = error(Status::Malformed);
    return;
  }
  // Read source info file.
  readModuleSourceInfoIfPresent();
  if (!readModuleDocIfPresent()) {
    info.status = error(Status::MalformedDocumentation);
    return;
  }
}

Status ModuleFile::associateWithFileContext(FileUnit *file,
                                            SourceLoc diagLoc,
                                            bool treatAsPartialModule) {
  PrettyStackTraceModuleFile stackEntry(*this);

  assert(!hasError() && "error already detected; should not call this");
  assert(!FileContext && "already associated with an AST module");
  FileContext = file;

  ModuleDecl *M = file->getParentModule();
  if (M->getName().str() != Name)
    return error(Status::NameMismatch);

  ASTContext &ctx = getContext();

  llvm::Triple moduleTarget(llvm::Triple::normalize(TargetTriple));
  if (!areCompatibleArchitectures(moduleTarget, ctx.LangOpts.Target) ||
      !areCompatibleOSs(moduleTarget, ctx.LangOpts.Target)) {
    return error(Status::TargetIncompatible);
  }
  if (ctx.LangOpts.EnableTargetOSChecking &&
      !M->isResilient() &&
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

    if (dependency.isImplementationOnly() &&
        !(treatAsPartialModule || ctx.LangOpts.DebuggerSupport)) {
      // When building normally (and not merging partial modules), we don't
      // want to bring in the implementation-only module, because that might
      // change the set of visible declarations. However, when debugging we
      // want to allow getting at the internals of this module when possible,
      // and so we'll try to reference the implementation-only module if it's
      // available.
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
    auto module = getModule(modulePath, /*allowLoading*/true);
    if (!module || module->failedToLoad()) {
      // If we're missing the module we're an overlay for, treat that specially.
      if (modulePath.size() == 1 &&
          modulePath.front() == file->getParentModule()->getName()) {
        return error(Status::MissingUnderlyingModule);
      }

      // Otherwise, continue trying to load dependencies, so that we can list
      // everything that's missing.
      if (!(dependency.isImplementationOnly() && ctx.LangOpts.DebuggerSupport))
        missingDependency = true;
      continue;
    }

    if (scopePath.empty()) {
      dependency.Import = { {}, module };
    } else {
      auto scopeID = ctx.getIdentifier(scopePath);
      assert(!scopeID.empty() &&
             "invalid decl name (non-top-level decls not supported)");
      Located<Identifier> accessPathElem = { scopeID, SourceLoc() };
      dependency.Import = {ctx.AllocateCopy(llvm::makeArrayRef(accessPathElem)),
                           module};
    }

    // SPI
    StringRef spisStr = dependency.RawSPIs;
    while (!spisStr.empty()) {
      StringRef nextComponent;
      std::tie(nextComponent, spisStr) = spisStr.split('\0');
      dependency.spiGroups.push_back(ctx.getIdentifier(nextComponent));
    }

    if (!module->hasResolvedImports()) {
      // Notice that we check this condition /after/ recording the module that
      // caused the problem. Clients need to be able to track down what the
      // cycle was.
      return error(Status::CircularDependency);
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

  return Status::Valid;
}

std::unique_ptr<llvm::MemoryBuffer> ModuleFile::takeBufferForDiagnostics() {
  assert(hasError());

  // Today, the only buffer that might have diagnostics in them is the input
  // buffer, and even then only if it has imported module contents.
  if (!importedHeaderInfo.contents.empty())
    return std::move(ModuleInputBuffer);

  return nullptr;
}

ModuleFile::~ModuleFile() { }

void ModuleFile::lookupValue(DeclName name,
                             SmallVectorImpl<ValueDecl*> &results) {
  PrettyStackTraceModuleFile stackEntry(*this);

  if (TopLevelDecls) {
    // Find top-level declarations with the given name.
    // FIXME: As a bit of a hack, do lookup by the simple name, then filter
    // compound decls, to avoid having to completely redo how modules are
    // serialized.
    auto iter = TopLevelDecls->find(name.getBaseName());
    if (iter != TopLevelDecls->end()) {
      for (auto item : *iter) {
        Expected<Decl *> declOrError = getDeclChecked(item.second);
        if (!declOrError) {
          if (!getContext().LangOpts.EnableDeserializationRecovery)
            fatal(declOrError.takeError());
          llvm::consumeError(declOrError.takeError());
          continue;
        }
        auto VD = cast<ValueDecl>(declOrError.get());
        if (name.isSimpleName() || VD->getName().matchesRef(name))
          results.push_back(VD);
      }
    }
  }

  // If the name is an operator name, also look for operator methods.
  if (name.isOperator() && OperatorMethodDecls) {
    auto iter = OperatorMethodDecls->find(name.getBaseName());
    if (iter != OperatorMethodDecls->end()) {
      for (auto item : *iter) {
        Expected<Decl *> declOrError = getDeclChecked(item.second);
        if (!declOrError) {
          if (!getContext().LangOpts.EnableDeserializationRecovery)
            fatal(declOrError.takeError());
          llvm::consumeError(declOrError.takeError());
          continue;
        }
        auto VD = cast<ValueDecl>(declOrError.get());
        results.push_back(VD);
      }
    }
  }
}

TypeDecl *ModuleFile::lookupLocalType(StringRef MangledName) {
  PrettyStackTraceModuleFile stackEntry(*this);

  if (!LocalTypeDecls)
    return nullptr;

  auto iter = LocalTypeDecls->find(MangledName);
  if (iter == LocalTypeDecls->end())
    return nullptr;

  return cast<TypeDecl>(getDecl(*iter));
}

OpaqueTypeDecl *ModuleFile::lookupOpaqueResultType(StringRef MangledName) {
  PrettyStackTraceModuleFile stackEntry(*this);

  if (!OpaqueReturnTypeDecls)
    return nullptr;
  
  auto iter = OpaqueReturnTypeDecls->find(MangledName);
  if (iter == OpaqueReturnTypeDecls->end())
    return nullptr;
  
  return cast<OpaqueTypeDecl>(getDecl(*iter));
}

TypeDecl *ModuleFile::lookupNestedType(Identifier name,
                                       const NominalTypeDecl *parent) {
  PrettyStackTraceModuleFile stackEntry(*this);

  if (NestedTypeDecls) {
    auto iter = NestedTypeDecls->find(name);
    if (iter != NestedTypeDecls->end()) {
      for (std::pair<DeclID, DeclID> entry : *iter) {
        assert(entry.first);
        auto declOrOffset = Decls[entry.first - 1];
        if (!declOrOffset.isComplete())
          continue;

        Decl *decl = declOrOffset;
        if (decl != parent)
          continue;
        return cast<TypeDecl>(getDecl(entry.second));
      }
    }
  }

  if (!UnderlyingModule)
    return nullptr;

  for (FileUnit *file : UnderlyingModule->getFiles())
    if (auto *nestedType = file->lookupNestedType(name, parent))
      return nestedType;

  return nullptr;
}

OperatorDecl *ModuleFile::lookupOperator(Identifier name,
                                         OperatorFixity fixity) {
  PrettyStackTraceModuleFile stackEntry(*this);

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
  PrettyStackTraceModuleFile stackEntry(*this);

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
  PrettyStackTraceModuleFile stackEntry(*this);

  for (auto &dep : Dependencies) {
    if (dep.isExported()) {
      if (!filter.contains(ModuleDecl::ImportFilterKind::Public))
        continue;

    } else if (dep.isImplementationOnly()) {
      if (!filter.contains(ModuleDecl::ImportFilterKind::ImplementationOnly))
        continue;
      if (!dep.isLoaded()) {
        // Pretend we didn't have this import if we weren't originally asked to
        // load it.
        continue;
      }

    } else {
      if (!filter.contains(ModuleDecl::ImportFilterKind::Private))
        continue;
    }

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

      SmallVector<Located<swift::Identifier>, 1> AccessPath;
      while (!ModulePathStr.empty()) {
        StringRef NextComponent;
        std::tie(NextComponent, ModulePathStr) = ModulePathStr.split('\0');
        AccessPath.push_back({Ctx.getIdentifier(NextComponent), SourceLoc()});
      }

      if (AccessPath.size() == 1 && AccessPath[0].Item == Ctx.StdlibModuleName)
        continue;

      ModuleDecl *M = Ctx.getLoadedModule(AccessPath);

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
            TopLevelModule = Ctx.getLoadedModule(AccessPath.front().Item);

          SmallVector<ValueDecl *, 8> Decls;
          TopLevelModule->lookupQualified(
              TopLevelModule, DeclNameRef(ScopeID),
              NL_QualifiedDefault | NL_KnownNoDependency, Decls);
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
  PrettyStackTraceModuleFile stackEntry(*this);
  assert(accessPath.size() <= 1 && "can only refer to top-level decls");

  if (!TopLevelDecls)
    return;

  auto tryImport = [this, &consumer](DeclID ID) {
    Expected<Decl *> declOrError = getDeclChecked(ID);
    if (!declOrError) {
      if (!getContext().LangOpts.EnableDeserializationRecovery)
        fatal(declOrError.takeError());
      llvm::consumeError(declOrError.takeError());
      return;
    }
    consumer.foundDecl(cast<ValueDecl>(declOrError.get()),
                       DeclVisibilityKind::VisibleAtTopLevel);
  };

  if (!accessPath.empty()) {
    auto iter = TopLevelDecls->find(accessPath.front().Item);
    if (iter == TopLevelDecls->end())
      return;

    for (auto item : *iter)
      tryImport(item.second);

    return;
  }

  for (auto entry : TopLevelDecls->data()) {
    for (auto item : entry)
      tryImport(item.second);
  }
}

void ModuleFile::loadExtensions(NominalTypeDecl *nominal) {
  PrettyStackTraceModuleFile stackEntry(*this);
  if (!ExtensionDecls)
    return;

  auto iter = ExtensionDecls->find(nominal->getName());
  if (iter == ExtensionDecls->end())
    return;

  if (nominal->getEffectiveAccess() < AccessLevel::Internal) {
    if (nominal->getModuleScopeContext() != getFile())
      return;
  }

  if (nominal->getParent()->isModuleScopeContext()) {
    auto parentFile = cast<FileUnit>(nominal->getParent());
    StringRef moduleName = parentFile->getExportedModuleName();

    for (auto item : *iter) {
      if (item.first != moduleName)
        continue;
      Expected<Decl *> declOrError = getDeclChecked(item.second);
      if (!declOrError) {
        if (!getContext().LangOpts.EnableDeserializationRecovery)
          fatal(declOrError.takeError());
        llvm::consumeError(declOrError.takeError());
      }
    }
  } else {
    std::string mangledName =
        Mangle::ASTMangler().mangleNominalType(nominal);
    for (auto item : *iter) {
      if (item.first != mangledName)
        continue;
      Expected<Decl *> declOrError = getDeclChecked(item.second);
      if (!declOrError) {
        if (!getContext().LangOpts.EnableDeserializationRecovery)
          fatal(declOrError.takeError());
        llvm::consumeError(declOrError.takeError());
      }
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

  std::string ownerName = Mangle::ASTMangler().mangleNominalType(classDecl);
  auto results = *known;
  for (const auto &result : results) {
    // If the method is the wrong kind (instance vs. class), skip it.
    if (isInstanceMethod != std::get<1>(result))
      continue;

    // If the method isn't defined in the requested class, skip it.
    if (std::get<0>(result) != ownerName)
      continue;

    // Deserialize the method and add it to the list.
    if (auto func = dyn_cast_or_null<AbstractFunctionDecl>(
                      getDecl(std::get<2>(result)))) {
      methods.push_back(func);
    }
  }
}

void ModuleFile::loadDerivativeFunctionConfigurations(
    AbstractFunctionDecl *originalAFD,
    llvm::SetVector<AutoDiffConfig> &results) {
  if (!DerivativeFunctionConfigurations)
    return;
  auto &ctx = originalAFD->getASTContext();
  Mangle::ASTMangler Mangler;
  auto mangledName = Mangler.mangleDeclAsUSR(originalAFD, "");
  auto configs = DerivativeFunctionConfigurations->find(mangledName);
  if (configs == DerivativeFunctionConfigurations->end())
    return;
  for (auto entry : *configs) {
    auto *parameterIndices = IndexSubset::getFromString(ctx, entry.first);
    auto derivativeGenSigOrError = getGenericSignatureChecked(entry.second);
    if (!derivativeGenSigOrError) {
      if (!getContext().LangOpts.EnableDeserializationRecovery)
        fatal(derivativeGenSigOrError.takeError());
      llvm::consumeError(derivativeGenSigOrError.takeError());
    }
    auto derivativeGenSig = derivativeGenSigOrError.get();
    // NOTE(TF-1038): Result indices are currently unsupported in derivative
    // registration attributes. In the meantime, always use `{0}` (wrt the
    // first and only result).
    auto resultIndices = IndexSubset::get(ctx, 1, {0});
    results.insert({parameterIndices, resultIndices, derivativeGenSig});
  }
}

TinyPtrVector<ValueDecl *>
ModuleFile::loadNamedMembers(const IterableDeclContext *IDC, DeclBaseName N,
                             uint64_t contextData) {
  PrettyStackTraceDecl trace("loading members for", IDC->getDecl());

  assert(IDC->wasDeserialized());
  assert(DeclMemberNames);

  TinyPtrVector<ValueDecl *> results;
  auto i = DeclMemberNames->find(N);
  if (i == DeclMemberNames->end())
    return results;

  BitOffset subTableOffset = *i;
  std::unique_ptr<SerializedDeclMembersTable> &subTable =
    DeclMembersTables[subTableOffset];
  if (!subTable) {
    BCOffsetRAII restoreOffset(DeclMemberTablesCursor);
    fatalIfNotSuccess(DeclMemberTablesCursor.JumpToBit(subTableOffset));
    llvm::BitstreamEntry entry =
        fatalIfUnexpected(DeclMemberTablesCursor.advance());
    if (entry.Kind != llvm::BitstreamEntry::Record) {
      fatal();
      return results;
    }
    SmallVector<uint64_t, 64> scratch;
    StringRef blobData;
    unsigned kind = fatalIfUnexpected(
        DeclMemberTablesCursor.readRecord(entry.ID, scratch, &blobData));
    assert(kind == decl_member_tables_block::DECL_MEMBERS);
    (void)kind;
    subTable = readDeclMembersTable(scratch, blobData);
  }

  assert(subTable);
  auto j = subTable->find(IDC->getDeclID());
  if (j != subTable->end()) {
    for (DeclID d : *j) {
      Expected<Decl *> mem = getDeclChecked(d);
      if (mem) {
        assert(mem.get() && "unchecked error deserializing named member");
        if (auto MVD = dyn_cast<ValueDecl>(mem.get())) {
          results.push_back(MVD);
        }
      } else {
        if (!getContext().LangOpts.EnableDeserializationRecovery)
          fatal(mem.takeError());
        consumeError(mem.takeError());
      }
    }
  }
  return results;
}

void ModuleFile::lookupClassMember(ModuleDecl::AccessPathTy accessPath,
                                   DeclName name,
                                   SmallVectorImpl<ValueDecl*> &results) {
  PrettyStackTraceModuleFile stackEntry(*this);
  assert(accessPath.size() <= 1 && "can only refer to top-level decls");

  if (!ClassMembersForDynamicLookup)
    return;

  auto iter = ClassMembersForDynamicLookup->find(name.getBaseName());
  if (iter == ClassMembersForDynamicLookup->end())
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
        if (auto nominal = dc->getSelfNominalTypeDecl())
          if (nominal->getName() == accessPath.front().Item)
            results.push_back(vd);
      }
    } else {
      for (auto item : *iter) {
        auto vd = cast<ValueDecl>(getDecl(item.second));
        if (!vd->getName().matchesRef(name))
          continue;
        
        auto dc = vd->getDeclContext();
        while (!dc->getParent()->isModuleScopeContext())
          dc = dc->getParent();
        if (auto nominal = dc->getSelfNominalTypeDecl())
          if (nominal->getName() == accessPath.front().Item)
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
  PrettyStackTraceModuleFile stackEntry(*this);
  assert(accessPath.size() <= 1 && "can only refer to top-level decls");

  if (!ClassMembersForDynamicLookup)
    return;

  if (!accessPath.empty()) {
    for (const auto &list : ClassMembersForDynamicLookup->data()) {
      for (auto item : list) {
        auto vd = cast<ValueDecl>(getDecl(item.second));
        auto dc = vd->getDeclContext();
        while (!dc->getParent()->isModuleScopeContext())
          dc = dc->getParent();
        if (auto nominal = dc->getSelfNominalTypeDecl())
          if (nominal->getName() == accessPath.front().Item)
            consumer.foundDecl(vd, DeclVisibilityKind::DynamicLookup,
                               DynamicLookupInfo::AnyObject);
      }
    }
    return;
  }

  for (const auto &list : ClassMembersForDynamicLookup->data()) {
    for (auto item : list)
      consumer.foundDecl(cast<ValueDecl>(getDecl(item.second)),
                         DeclVisibilityKind::DynamicLookup,
                         DynamicLookupInfo::AnyObject);
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

void ModuleFile::lookupImportedSPIGroups(const ModuleDecl *importedModule,
                                    SmallVectorImpl<Identifier> &spiGroups) const {
  for (auto &dep : Dependencies) {
    auto depSpis = dep.spiGroups;
    if (dep.Import.second == importedModule &&
        !depSpis.empty()) {
      spiGroups.append(depSpis.begin(), depSpis.end());
    }
  }
}

void
ModuleFile::collectLinkLibraries(ModuleDecl::LinkLibraryCallback callback) const {
  for (auto &lib : LinkLibraries)
    callback(lib);
  if (Bits.IsFramework)
    callback(LinkLibrary(Name, LibraryKind::Framework));
}

void ModuleFile::getTopLevelDecls(
       SmallVectorImpl<Decl *> &results,
       llvm::function_ref<bool(DeclAttributes)> matchAttributes) {
  PrettyStackTraceModuleFile stackEntry(*this);
  for (DeclID entry : OrderedTopLevelDecls) {
    Expected<Decl *> declOrError = getDeclChecked(entry, matchAttributes);
    if (!declOrError) {
      if (declOrError.errorIsA<DeclAttributesDidNotMatch>()) {
        // Decl rejected by matchAttributes, ignore it.
        assert(matchAttributes);
        consumeError(declOrError.takeError());
        continue;
      }

      if (!getContext().LangOpts.EnableDeserializationRecovery)
        fatal(declOrError.takeError());
      consumeError(declOrError.takeError());
      continue;
    }
    results.push_back(declOrError.get());
  }
}

void ModuleFile::getOperatorDecls(SmallVectorImpl<OperatorDecl *> &results) {
  PrettyStackTraceModuleFile stackEntry(*this);
  if (!OperatorDecls)
    return;

  for (auto entry : OperatorDecls->data()) {
    for (auto item : entry)
      results.push_back(cast<OperatorDecl>(getDecl(item.second)));
  }
}

void ModuleFile::getPrecedenceGroups(
       SmallVectorImpl<PrecedenceGroupDecl*> &results) {
  PrettyStackTraceModuleFile stackEntry(*this);
  if (PrecedenceGroupDecls) {
    for (auto entry : PrecedenceGroupDecls->data()) {
      for (auto item : entry)
        results.push_back(cast<PrecedenceGroupDecl>(getDecl(item.second)));
    }
  }
}

void
ModuleFile::getLocalTypeDecls(SmallVectorImpl<TypeDecl *> &results) {
  PrettyStackTraceModuleFile stackEntry(*this);
  if (!LocalTypeDecls)
    return;

  for (auto DeclID : LocalTypeDecls->data()) {
    auto TD = cast<TypeDecl>(getDecl(DeclID));
    results.push_back(TD);
  }
}

void
ModuleFile::getOpaqueReturnTypeDecls(SmallVectorImpl<OpaqueTypeDecl *> &results)
{
  PrettyStackTraceModuleFile stackEntry(*this);
  if (!OpaqueReturnTypeDecls)
    return;

  for (auto DeclID : OpaqueReturnTypeDecls->data()) {
    auto TD = cast<OpaqueTypeDecl>(getDecl(DeclID));
    results.push_back(TD);
  }
}

void ModuleFile::getDisplayDecls(SmallVectorImpl<Decl *> &results) {
  if (UnderlyingModule)
    UnderlyingModule->getDisplayDecls(results);

  PrettyStackTraceModuleFile stackEntry(*this);
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
  // Compute the USR.
  llvm::SmallString<128> USRBuffer;
  llvm::raw_svector_ostream OS(USRBuffer);
  if (ide::printDeclUSR(D, OS))
    return None;

  return getCommentForDeclByUSR(USRBuffer.str());
}

Optional<BasicDeclLocs>
ModuleFile::getBasicDeclLocsForDecl(const Decl *D) const {
  assert(D);

  // Keep these as assertions instead of early exits to ensure that we are not
  // doing extra work.  These cases should be handled by clients of this API.
  assert(!D->hasClangNode() &&
         "cannot find comments for Clang decls in Swift modules");
  assert(D->getDeclContext()->getModuleScopeContext() == FileContext &&
         "Decl is from a different serialized file");
  if (!DeclUSRsTable)
    return None;
  // Future compilers may not provide BasicDeclLocsData anymore.
  if (BasicDeclLocsData.empty())
    return None;
  if (D->isImplicit())
    return None;
  // Compute the USR.
  llvm::SmallString<128> USRBuffer;
  llvm::raw_svector_ostream OS(USRBuffer);
  if (ide::printDeclUSR(D, OS))
    return None;

  auto It = DeclUSRsTable->find(OS.str());
  if (It == DeclUSRsTable->end())
    return None;
  auto UsrId = *It;
  uint32_t NumSize = 4;
  // Size of BasicDeclLocs in the buffer.
  // FilePathOffset + LocNum * LineColumn
  uint32_t LineColumnCount = 3;
  uint32_t RecordSize =
    NumSize + // Offset into source filename blob
    NumSize + // Offset into doc ranges blob
    NumSize * 2 * LineColumnCount; // Line/column of: Loc, StartLoc, EndLoc
  uint32_t RecordOffset = RecordSize * UsrId;
  assert(RecordOffset < BasicDeclLocsData.size());
  assert(BasicDeclLocsData.size() % RecordSize == 0);
  BasicDeclLocs Result;
  auto *Record = BasicDeclLocsData.data() + RecordOffset;
  auto ReadNext = [&Record]() {
    return endian::readNext<uint32_t, little, unaligned>(Record);
  };

  auto FilePath = SourceLocsTextData.substr(ReadNext());
  size_t TerminatorOffset = FilePath.find('\0');
  assert(TerminatorOffset != StringRef::npos && "unterminated string data");
  Result.SourceFilePath = FilePath.slice(0, TerminatorOffset);

  const auto DocRangesOffset = ReadNext();
  if (DocRangesOffset) {
    assert(!DocRangesData.empty());
    const auto *Data = DocRangesData.data() + DocRangesOffset;
    const auto NumLocs = endian::readNext<uint32_t, little, unaligned>(Data);
    assert(NumLocs);

    for (uint32_t i = 0; i < NumLocs; ++i) {
      LineColumn LC;
      LC.Line = endian::readNext<uint32_t, little, unaligned>(Data);
      LC.Column = endian::readNext<uint32_t, little, unaligned>(Data);
      auto Length = endian::readNext<uint32_t, little, unaligned>(Data);
      Result.DocRanges.push_back(std::make_pair(LC, Length));
    }
  }

#define READ_FIELD(X)                                                         \
Result.X.Line = ReadNext();                                                   \
Result.X.Column = ReadNext();
  READ_FIELD(Loc)
  READ_FIELD(StartLoc)
  READ_FIELD(EndLoc)
#undef READ_FIELD
  return Result;
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

bool SerializedASTFile::getAllGenericSignatures(
                       SmallVectorImpl<GenericSignature> &genericSignatures) {
  genericSignatures.clear();
  for (unsigned index : indices(File.GenericSignatures)) {
    if (auto genericSig = File.getGenericSignature(index + 1))
      genericSignatures.push_back(genericSig);
  }

  return true;
}

Decl *SerializedASTFile::getMainDecl() const {
  assert(hasEntryPoint());
  return File.getDecl(File.Bits.EntryPointDeclID);
}

const version::Version &SerializedASTFile::getLanguageVersionBuiltWith() const {
  return File.CompatibilityVersion;
}

StringRef SerializedASTFile::getModuleDefiningPath() const {
  StringRef moduleFilename = getFilename();
  StringRef parentDir = llvm::sys::path::parent_path(moduleFilename);

  if (llvm::sys::path::extension(parentDir) == ".swiftmodule")
    return parentDir;

  return moduleFilename;
}
