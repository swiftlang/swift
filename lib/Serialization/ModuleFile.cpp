//===--- ModuleFile.cpp - Loading a serialized module -----------*- C++ -*-===//
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

#include "swift/Serialization/ModuleFile.h"
#include "swift/Serialization/ModuleFormat.h"
#include "swift/Subsystems.h"
#include "swift/AST/AST.h"
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
      if (cursor.ReadBlockInfoBlock())
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
  auto next = cursor.advance();
  while (next.Kind != llvm::BitstreamEntry::EndBlock) {
    if (next.Kind == llvm::BitstreamEntry::Error)
      return false;

    if (next.Kind == llvm::BitstreamEntry::SubBlock) {
      // Unknown metadata sub-block, possibly for use by a future version of
      // the module format.
      if (cursor.SkipBlock())
        return false;
      next = cursor.advance();
      continue;
    }

    scratch.clear();
    StringRef blobData;
    unsigned kind = cursor.readRecord(next.ID, scratch, &blobData);
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
    default:
      // Unknown options record, possibly for use by a future version of the
      // module format.
      break;
    }

    next = cursor.advance();
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

  auto next = cursor.advance();
  while (next.Kind != llvm::BitstreamEntry::EndBlock) {
    if (next.Kind == llvm::BitstreamEntry::Error) {
      result.status = Status::Malformed;
      return result;
    }

    if (next.Kind == llvm::BitstreamEntry::SubBlock) {
      if (next.ID == OPTIONS_BLOCK_ID && extendedInfo) {
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
      next = cursor.advance();
      continue;
    }

    scratch.clear();
    StringRef blobData;
    unsigned kind = cursor.readRecord(next.ID, scratch, &blobData);
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

    next = cursor.advance();
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

  llvm::BitstreamReader reader(reinterpret_cast<const uint8_t *>(data.begin()),
                               reinterpret_cast<const uint8_t *>(data.end()));
  llvm::BitstreamCursor cursor(reader);
  SmallVector<uint64_t, 32> scratch;

  if (!checkModuleSignature(cursor) ||
      !enterTopLevelModuleBlock(cursor, MODULE_BLOCK_ID, false))
    return result;

  auto topLevelEntry = cursor.advance();
  while (topLevelEntry.Kind == llvm::BitstreamEntry::SubBlock) {
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

    topLevelEntry = cursor.advance(AF_DontPopBlockAtEnd);
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

    virtual void print(raw_ostream &os) const override {
      os << "While reading from " << File.getModuleFilename() << "\n";
    }
  };
} // end anonymous namespace

/// Used to deserialize entries in the on-disk decl hash table.
class ModuleFile::DeclTableInfo {
public:
  using internal_key_type = StringRef;
  using external_key_type = Identifier;
  using data_type = SmallVector<std::pair<uint8_t, DeclID>, 8>;
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
      uint8_t kind = *data++;
      DeclID offset = endian::readNext<uint32_t, little, unaligned>(data);
      result.push_back({ kind, offset });
      length -= 5;
    }

    return result;
  }
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
    return { keyLength, sizeof(DeclID) + sizeof(unsigned) };
  }

  static internal_key_type ReadKey(const uint8_t *data, unsigned length) {
    return StringRef(reinterpret_cast<const char *>(data), length);
  }

  static data_type ReadData(internal_key_type key, const uint8_t *data,
                            unsigned length) {
    auto declID = endian::readNext<DeclID, little, unaligned>(data);
    auto discriminator = endian::readNext<unsigned, little, unaligned>(data);
    return { declID, discriminator };
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

std::unique_ptr<ModuleFile::SerializedLocalDeclTable>
ModuleFile::readLocalDeclTable(ArrayRef<uint64_t> fields, StringRef blobData) {
  uint32_t tableOffset;
  index_block::DeclListLayout::readRecord(fields, tableOffset);
  auto base = reinterpret_cast<const uint8_t *>(blobData.data());

  using OwnedTable = std::unique_ptr<SerializedLocalDeclTable>;
  return OwnedTable(SerializedLocalDeclTable::Create(base + tableOffset,
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
    data_type result;
    while (length > 0) {
      TypeID typeID = endian::readNext<uint32_t, little, unaligned>(data);
      bool isInstanceMethod = *data++ != 0;
      DeclID methodID = endian::readNext<uint32_t, little, unaligned>(data);
      result.push_back(std::make_tuple(typeID, isInstanceMethod, methodID));
      length -= sizeof(TypeID) + 1 + sizeof(DeclID);
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

  while (true) {
    auto next = cursor.advance();
    switch (next.Kind) {
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
      unsigned kind = cursor.readRecord(next.ID, scratch, &blobData);

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
      case index_block::EXTENSIONS:
        ExtensionDecls = readDeclTable(scratch, blobData);
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
      case index_block::LOCAL_DECL_CONTEXT_OFFSETS:
        assert(blobData.empty());
        LocalDeclContexts.assign(scratch.begin(), scratch.end());
        break;
      case index_block::NORMAL_CONFORMANCE_OFFSETS:
        assert(blobData.empty());
        NormalConformances.assign(scratch.begin(), scratch.end());
        break;

      default:
        // Unknown index kind, which this version of the compiler won't use.
        break;
      }
      break;
    }
  }
}

class ModuleFile::DeclCommentTableInfo {
  ModuleFile &F;

public:
  using internal_key_type = StringRef;
  using external_key_type = StringRef;
  using data_type = BriefAndRawComment;
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

bool ModuleFile::readCommentBlock(llvm::BitstreamCursor &cursor) {
  cursor.EnterSubBlock(COMMENT_BLOCK_ID);

  SmallVector<uint64_t, 4> scratch;
  StringRef blobData;

  while (true) {
    auto next = cursor.advance();
    switch (next.Kind) {
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
      unsigned kind = cursor.readRecord(next.ID, scratch, &blobData);

      switch (kind) {
      case comment_block::DECL_COMMENTS:
        DeclCommentTable = readDeclCommentTable(scratch, blobData);
        break;
      default:
        // Unknown index kind, which this version of the compiler won't use.
        break;
      }
      break;
    }
  }
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

static const uint8_t *getStartBytePtr(llvm::MemoryBuffer *buffer) {
  if (!buffer)
    return nullptr;
  return reinterpret_cast<const uint8_t *>(buffer->getBufferStart());
}

static const uint8_t *getEndBytePtr(llvm::MemoryBuffer *buffer) {
  if (!buffer)
    return nullptr;
  return reinterpret_cast<const uint8_t *>(buffer->getBufferEnd());
}

static bool areCompatibleArchitectures(const llvm::Triple &moduleTarget,
                                       const llvm::Triple &ctxTarget) {
  if (moduleTarget.getArch() == ctxTarget.getArch())
    return true;

  auto archPair = std::minmax(moduleTarget.getArch(), ctxTarget.getArch());
  if (archPair == std::minmax(llvm::Triple::arm, llvm::Triple::thumb))
    return true;
  if (archPair == std::minmax(llvm::Triple::armeb, llvm::Triple::thumbeb))
    return true;

  return false;
}

static bool areCompatibleOSs(const llvm::Triple &moduleTarget,
                             const llvm::Triple &ctxTarget) {
  if (moduleTarget.getOS() == ctxTarget.getOS())
    return true;

  auto osPair = std::minmax(moduleTarget.getOS(), ctxTarget.getOS());
  if (osPair == std::minmax(llvm::Triple::Darwin, llvm::Triple::MacOSX))
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
    bool isFramework,
    serialization::ExtendedValidationInfo *extInfo)
    : ModuleInputBuffer(std::move(moduleInputBuffer)),
      ModuleDocInputBuffer(std::move(moduleDocInputBuffer)),
      ModuleInputReader(getStartBytePtr(this->ModuleInputBuffer.get()),
                        getEndBytePtr(this->ModuleInputBuffer.get())),
      ModuleDocInputReader(getStartBytePtr(this->ModuleDocInputBuffer.get()),
                           getEndBytePtr(this->ModuleDocInputBuffer.get())) {
  assert(getStatus() == Status::Valid);
  Bits.IsFramework = isFramework;

  PrettyModuleFileDeserialization stackEntry(*this);

  llvm::BitstreamCursor cursor{ModuleInputReader};

  if (!checkModuleSignature(cursor) ||
      !enterTopLevelModuleBlock(cursor, MODULE_BLOCK_ID)) {
    error();
    return;
  }

  // Future-proofing: make sure we validate the control block before we try to
  // read any other blocks.
  bool hasValidControlBlock = false;
  SmallVector<uint64_t, 64> scratch;

  auto topLevelEntry = cursor.advance();
  while (topLevelEntry.Kind == llvm::BitstreamEntry::SubBlock) {
    switch (topLevelEntry.ID) {
    case CONTROL_BLOCK_ID: {
      cursor.EnterSubBlock(CONTROL_BLOCK_ID);

      auto info = validateControlBlock(cursor, scratch, extInfo);
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
          input_block::SearchPathLayout::readRecord(scratch, isFramework);
          SearchPaths.push_back({blobData, isFramework});
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

    topLevelEntry = cursor.advance(AF_DontPopBlockAtEnd);
  }

  if (topLevelEntry.Kind != llvm::BitstreamEntry::EndBlock) {
    error();
    return;
  }

  if (!this->ModuleDocInputBuffer)
    return;

  llvm::BitstreamCursor docCursor{ModuleDocInputReader};
  if (!checkModuleDocSignature(docCursor) ||
      !enterTopLevelModuleBlock(docCursor, MODULE_DOC_BLOCK_ID)) {
    error(Status::MalformedDocumentation);
    return;
  }

  topLevelEntry = docCursor.advance();
  while (topLevelEntry.Kind == llvm::BitstreamEntry::SubBlock) {
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

    topLevelEntry = docCursor.advance(AF_DontPopBlockAtEnd);
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

  if (file->getParentModule()->Name.str() != Name)
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

  auto clangImporter = static_cast<ClangImporter *>(ctx.getClangModuleLoader());

  for (const auto &searchPathPair : SearchPaths) {
    if (searchPathPair.second) {
      ctx.SearchPathOpts.FrameworkSearchPaths.push_back(searchPathPair.first);
    } else {
      ctx.SearchPathOpts.ImportSearchPaths.push_back(searchPathPair.first);
    }
    clangImporter->addSearchPath(searchPathPair.first, searchPathPair.second);
  }

  bool missingDependency = false;
  for (auto &dependency : Dependencies) {
    assert(!dependency.isLoaded() && "already loaded?");

    if (dependency.isHeader()) {
      // The path may be empty if the file being loaded is a partial AST,
      // and the current compiler invocation is a merge-modules step.
      if (!dependency.RawPath.empty()) {
        clangImporter->importHeader(dependency.RawPath, file->getParentModule(),
                                    importedHeaderInfo.fileSize,
                                    importedHeaderInfo.fileModTime,
                                    importedHeaderInfo.contents, diagLoc);
      }
      Module *importedHeaderModule = clangImporter->getImportedHeaderModule();
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
    if (!module) {
      // If we're missing the module we're shadowing, treat that specially.
      if (modulePath.size() == 1 &&
          modulePath.front() == file->getParentModule()->Name) {
        return error(Status::MissingShadowedModule);
      }

      // Otherwise, continue trying to load dependencies, so that we can list
      // everything that's missing.
      missingDependency = true;
      continue;
    }

    if (scopePath.empty()) {
      dependency.Import = { {}, module };
    } else {
      auto scopeID = ctx.getIdentifier(scopePath);
      assert(!scopeID.empty() &&
             "invalid decl name (non-top-level decls not supported)");
      auto path = Module::AccessPathTy({scopeID, SourceLoc()});
      dependency.Import = { ctx.AllocateCopy(path), module };
    }
  }

  if (missingDependency) {
    return error(Status::MissingDependency);
  }

  if (Bits.HasUnderlyingModule)
    (void)getModule(FileContext->getParentModule()->Name);

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

void ModuleFile::getImportedModules(
    SmallVectorImpl<Module::ImportedModule> &results,
    Module::ImportFilter filter) {
  PrettyModuleFileDeserialization stackEntry(*this);
  bool includeShadowedModule = (filter != Module::ImportFilter::Private &&
                                ShadowedModule && Bits.HasUnderlyingModule);

  for (auto &dep : Dependencies) {
    if (filter != Module::ImportFilter::All &&
        (filter == Module::ImportFilter::Public) ^ dep.isExported())
      continue;
    assert(dep.isLoaded());
    results.push_back(dep.Import);

    // FIXME: Do we want a way to limit re-exports?
    if (includeShadowedModule && dep.Import.first.empty() &&
        dep.Import.second == ShadowedModule)
      includeShadowedModule = false;
  }

  if (includeShadowedModule)
    results.push_back({ {}, ShadowedModule });
}

void ModuleFile::getImportDecls(SmallVectorImpl<Decl *> &Results) {
  if (!Bits.ComputedImportDecls) {
    ASTContext &Ctx = getContext();
    for (auto &Dep : Dependencies) {
      // FIXME: We need a better way to show headers, since they usually /are/
      // re-exported. This isn't likely to come up much, though.
      if (Dep.isHeader())
        continue;

      StringRef ModulePath, ScopePath;
      std::tie(ModulePath, ScopePath) = Dep.RawPath.split('\0');

      auto ModuleID = Ctx.getIdentifier(ModulePath);
      assert(!ModuleID.empty() &&
             "invalid module name (submodules not yet supported)");

      if (ModuleID == Ctx.StdlibModuleName)
        continue;

      SmallVector<std::pair<swift::Identifier, swift::SourceLoc>, 1>
          AccessPath;
      AccessPath.push_back({ ModuleID, SourceLoc() });
      Module *M = Ctx.getModule(AccessPath);

      auto Kind = ImportKind::Module;
      if (!ScopePath.empty()) {
        auto ScopeID = Ctx.getIdentifier(ScopePath);
        assert(!ScopeID.empty() &&
               "invalid decl name (non-top-level decls not supported)");

        if (!M) {
          // The dependency module could not be loaded.  Just make a guess
          // about the import kind, we can not do better.
          Kind = ImportKind::Func;
        } else {
          SmallVector<ValueDecl *, 8> Decls;
          M->lookupQualified(ModuleType::get(M), ScopeID,
                             NL_QualifiedDefault | NL_KnownNoDependency,
                             nullptr, Decls);
          Optional<ImportKind> FoundKind = ImportDecl::findBestImportKind(Decls);
          assert(FoundKind.hasValue() &&
                 "deserialized imports should not be ambigous");
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

void ModuleFile::lookupVisibleDecls(Module::AccessPathTy accessPath,
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

  for (auto item : *iter) {
    if (item.first == getKindForTable(nominal))
      (void)getDecl(item.second);
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

void ModuleFile::lookupClassMember(Module::AccessPathTy accessPath,
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
        if (auto nominal = dc->getDeclaredTypeInContext()->getAnyNominal())
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
        if (auto nominal = dc->getDeclaredTypeInContext()->getAnyNominal())
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

void ModuleFile::lookupClassMembers(Module::AccessPathTy accessPath,
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
        if (auto nominal = dc->getDeclaredTypeInContext()->getAnyNominal())
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

void
ModuleFile::collectLinkLibraries(Module::LinkLibraryCallback callback) const {
  for (auto &lib : LinkLibraries)
    callback(lib);
  if (Bits.IsFramework)
    callback(LinkLibrary(Name, LibraryKind::Framework));
}

void ModuleFile::getTopLevelDecls(SmallVectorImpl<Decl *> &results) {
  PrettyModuleFileDeserialization stackEntry(*this);
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

Optional<BriefAndRawComment> ModuleFile::getCommentForDecl(const Decl *D) {
  assert(D);

  // Keep these as assertions instead of early exits to ensure that we are not
  // doing extra work.  These cases should be handled by clients of this API.
  assert(!D->hasClangNode() &&
         "can not find comments for Clang decls in Swift modules");
  assert(D->getDeclContext()->getModuleScopeContext() == FileContext &&
         "Decl is from a different serialized file");

  if (!DeclCommentTable)
    return None;

  if (D->isImplicit())
    return None;

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

Optional<BriefAndRawComment> ModuleFile::getCommentForDeclByUSR(StringRef USR) {
  if (!DeclCommentTable)
    return None;

  auto I = DeclCommentTable->find(USR);
  if (I == DeclCommentTable->end())
    return None;

  return *I;
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
