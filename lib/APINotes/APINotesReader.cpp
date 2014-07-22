//===--- APINotesReader.cpp - Side Car Reader --------------------*- C++ -*-===//
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
// This file implements the \c APINotesReader class that reads source
// API notes data providing additional information about source code as
// a separate input, such as the non-nil/nilable annotations for
// method parameters.
//
//===----------------------------------------------------------------------===//
#include "swift/APINotes/APINotesReader.h"
#include "APINotesFormat.h"
#include "llvm/Bitcode/BitstreamReader.h"
#include "llvm/Support/EndianStream.h"
#include "llvm/Support/OnDiskHashTable.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/StringExtras.h"

using namespace swift;
using namespace api_notes;
using namespace llvm::support;

namespace {
  /// Read serialized CommonEntityInfo.
  void readCommonEntityInfo(const uint8_t *&data, CommonEntityInfo &info) {
    info.Unavailable = *data++;

    unsigned msgLength = endian::readNext<uint16_t, little, unaligned>(data);
    info.UnavailableMsg
      = std::string(reinterpret_cast<const char *>(data),
                    reinterpret_cast<const char *>(data) + msgLength);
    data += msgLength;
  }

  /// Used to deserialize the on-disk identifier table.
  class IdentifierTableInfo {
  public:
    using internal_key_type = StringRef;
    using external_key_type = StringRef;
    using data_type = IdentifierID;
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
      return StringRef(reinterpret_cast<const char *>(data), length);
    }
    
    static data_type ReadData(internal_key_type key, const uint8_t *data,
                              unsigned length) {
      return endian::readNext<uint32_t, little, unaligned>(data);
    }
  };

  /// Used to deserialize the on-disk Objective-C class table.
  class ObjCContextTableInfo {
  public:
    // identifier ID, is-protocol
    using internal_key_type = std::pair<unsigned, char>;
    using external_key_type = internal_key_type;
    using data_type = std::pair<unsigned, ObjCContextInfo>;
    using hash_value_type = size_t;
    using offset_type = unsigned;

    internal_key_type GetInternalKey(external_key_type key) {
      return key;
    }

    external_key_type GetExternalKey(internal_key_type key) {
      return key;
    }

    hash_value_type ComputeHash(internal_key_type key) {
      return static_cast<size_t>(llvm::hash_value(key));
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
      auto nameID
        = endian::readNext<IdentifierID, little, unaligned>(data);
      auto isProtocol = endian::readNext<uint8_t, little, unaligned>(data);
      return { nameID, isProtocol };
    }
    
    static data_type ReadData(internal_key_type key, const uint8_t *data,
                              unsigned length) {
      data_type result;
      result.first = endian::readNext<StoredContextID, little, unaligned>(data);
      readCommonEntityInfo(data, result.second);
      if (*data++) {
        result.second.setDefaultNullability(static_cast<NullableKind>(*data));
      }
      ++data;
      result.second.setHasDesignatedInits(*data++);
      return result;
    }
  };

  /// Used to deserialize the on-disk Objective-C class table.
  class ObjCPropertyTableInfo {
  public:
    // (context ID, name ID)
    using internal_key_type = std::pair<unsigned, unsigned>; 
    using external_key_type = internal_key_type;
    using data_type = ObjCPropertyInfo;
    using hash_value_type = size_t;
    using offset_type = unsigned;

    internal_key_type GetInternalKey(external_key_type key) {
      return key;
    }

    external_key_type GetExternalKey(internal_key_type key) {
      return key;
    }

    hash_value_type ComputeHash(internal_key_type key) {
      return static_cast<size_t>(llvm::hash_value(key));
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
      auto classID = endian::readNext<IdentifierID, little, unaligned>(data);
      auto nameID = endian::readNext<IdentifierID, little, unaligned>(data);
      return { classID, nameID };
    }
    
    static data_type ReadData(internal_key_type key, const uint8_t *data,
                              unsigned length) {
      ObjCPropertyInfo info;
      readCommonEntityInfo(data, info);
      if (*data++) {
        info.setNullabilityAudited(static_cast<NullableKind>(*data));
      }
      ++data;

      return info;
    }
  };

  /// Used to deserialize the on-disk Objective-C method table.
  class ObjCMethodTableInfo {
  public:
    // (class ID, selector ID, is-instance)
    using internal_key_type = std::tuple<unsigned, unsigned, char>; 
    using external_key_type = internal_key_type;
    using data_type = ObjCMethodInfo;
    using hash_value_type = size_t;
    using offset_type = unsigned;

    internal_key_type GetInternalKey(external_key_type key) {
      return key;
    }

    external_key_type GetExternalKey(internal_key_type key) {
      return key;
    }

    hash_value_type ComputeHash(internal_key_type key) {
      return llvm::hash_combine(std::get<0>(key), 
                                std::get<1>(key), 
                                std::get<2>(key));
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
      auto classID = endian::readNext<IdentifierID, little, unaligned>(data);
      auto selectorID = endian::readNext<SelectorID, little, unaligned>(data);
      auto isInstance = endian::readNext<uint8_t, little, unaligned>(data);
      return { classID, selectorID, isInstance };
    }
    
    static data_type ReadData(internal_key_type key, const uint8_t *data,
                              unsigned length) {
      ObjCMethodInfo info;
      readCommonEntityInfo(data, info);
      info.DesignatedInit = endian::readNext<uint8_t, little, unaligned>(data);
      info.FactoryAsInit = endian::readNext<uint8_t, little, unaligned>(data);
      info.NullabilityAudited
        = endian::readNext<uint8_t, little, unaligned>(data);
      info.NumAdjustedNullable
        = endian::readNext<uint8_t, little, unaligned>(data);
      info.NullabilityPayload
        = endian::readNext<uint64_t, little, unaligned>(data);
      return info;
    }
  };

  /// Used to deserialize the on-disk Objective-C selector table.
  class ObjCSelectorTableInfo {
  public:
    using internal_key_type = StoredObjCSelector; 
    using external_key_type = internal_key_type;
    using data_type = SelectorID;
    using hash_value_type = unsigned;
    using offset_type = unsigned;

    internal_key_type GetInternalKey(external_key_type key) {
      return key;
    }

    external_key_type GetExternalKey(internal_key_type key) {
      return key;
    }

    hash_value_type ComputeHash(internal_key_type key) {
      return llvm::DenseMapInfo<StoredObjCSelector>::getHashValue(key);
    }
    
    static bool EqualKey(internal_key_type lhs, internal_key_type rhs) {
      return llvm::DenseMapInfo<StoredObjCSelector>::isEqual(lhs, rhs);
    }
    
    static std::pair<unsigned, unsigned> 
    ReadKeyDataLength(const uint8_t *&data) {
      unsigned keyLength = endian::readNext<uint16_t, little, unaligned>(data);
      unsigned dataLength = endian::readNext<uint16_t, little, unaligned>(data);
      return { keyLength, dataLength };
    }
    
    static internal_key_type ReadKey(const uint8_t *data, unsigned length) {
      internal_key_type key;
      key.NumPieces = endian::readNext<uint16_t, little, unaligned>(data);
      unsigned numIdents = (length - sizeof(uint16_t)) / sizeof(IdentifierID);
      for (unsigned i = 0; i != numIdents; ++i) {
        key.Identifiers.push_back(
          endian::readNext<IdentifierID, little, unaligned>(data));
      }
      return key;
    }
    
    static data_type ReadData(internal_key_type key, const uint8_t *data,
                              unsigned length) {
      return endian::readNext<SelectorID, little, unaligned>(data);
    }
  };
} // end anonymous namespace

class APINotesReader::Implementation {
public:
  /// The input buffer for the API notes data.
  std::unique_ptr<llvm::MemoryBuffer> InputBuffer;

  /// The reader attached to \c InputBuffer.
  llvm::BitstreamReader InputReader;

  using SerializedIdentifierTable =
      llvm::OnDiskIterableChainedHashTable<IdentifierTableInfo>;

  /// The identifier table.
  std::unique_ptr<SerializedIdentifierTable> IdentifierTable;

  using SerializedObjCContextTable =
      llvm::OnDiskIterableChainedHashTable<ObjCContextTableInfo>;

  /// The Objective-C context table.
  std::unique_ptr<SerializedObjCContextTable> ObjCContextTable;

  using SerializedObjCPropertyTable =
      llvm::OnDiskIterableChainedHashTable<ObjCPropertyTableInfo>;

  /// The Objective-C property table.
  std::unique_ptr<SerializedObjCPropertyTable> ObjCPropertyTable;

  using SerializedObjCMethodTable =
      llvm::OnDiskIterableChainedHashTable<ObjCMethodTableInfo>;

  /// The Objective-C method table.
  std::unique_ptr<SerializedObjCMethodTable> ObjCMethodTable;

  using SerializedObjCSelectorTable =
      llvm::OnDiskIterableChainedHashTable<ObjCSelectorTableInfo>;

  /// The Objective-C selector table.
  std::unique_ptr<SerializedObjCSelectorTable> ObjCSelectorTable;

  /// Retrieve the identifier ID for the given string, or an empty
  /// optional if the string is unknown.
  Optional<IdentifierID> getIdentifier(StringRef str);

  /// Retrieve the selector ID for the given selector, or an empty
  /// optional if the string is unknown.
  Optional<SelectorID> getSelector(ObjCSelectorRef selector);

  bool readControlBlock(llvm::BitstreamCursor &cursor, 
                        SmallVectorImpl<uint64_t> &scratch);
  bool readIdentifierBlock(llvm::BitstreamCursor &cursor,
                           SmallVectorImpl<uint64_t> &scratch);
  bool readObjCContextBlock(llvm::BitstreamCursor &cursor,
                            SmallVectorImpl<uint64_t> &scratch);
  bool readObjCPropertyBlock(llvm::BitstreamCursor &cursor, 
                             SmallVectorImpl<uint64_t> &scratch);
  bool readObjCMethodBlock(llvm::BitstreamCursor &cursor, 
                             SmallVectorImpl<uint64_t> &scratch);
  bool readObjCSelectorBlock(llvm::BitstreamCursor &cursor, 
                             SmallVectorImpl<uint64_t> &scratch);
};

Optional<IdentifierID> APINotesReader::Implementation::getIdentifier(
                         StringRef str) {
  if (!IdentifierTable)
    return Nothing;

  if (str.empty())
    return 0;

  auto known = IdentifierTable->find(str);
  if (known == IdentifierTable->end())
    return Nothing;

  return *known;
}

Optional<SelectorID> APINotesReader::Implementation::getSelector(
                       ObjCSelectorRef selector) {
  if (!ObjCSelectorTable || !IdentifierTable)
    return Nothing;

  // Translate the identifiers.
  StoredObjCSelector key;
  key.NumPieces = selector.NumPieces;
  for (auto ident : selector.Identifiers) {
    if (auto identID = getIdentifier(ident)) {
      key.Identifiers.push_back(*identID);
    } else {
      return Nothing;
    }
  }

  auto known = ObjCSelectorTable->find(key);
  if (known == ObjCSelectorTable->end())
    return Nothing;

  return *known;

}

bool APINotesReader::Implementation::readControlBlock(
       llvm::BitstreamCursor &cursor,
       SmallVectorImpl<uint64_t> &scratch) {
  if (cursor.EnterSubBlock(CONTROL_BLOCK_ID))
    return true;

  bool sawMetadata = false;
  
  auto next = cursor.advance();
  while (next.Kind != llvm::BitstreamEntry::EndBlock) {
    if (next.Kind == llvm::BitstreamEntry::Error)
      return true;

    if (next.Kind == llvm::BitstreamEntry::SubBlock) {
      // Unknown metadata sub-block, possibly for use by a future version of the
      // API notes format.
      if (cursor.SkipBlock())
        return true;
      
      next = cursor.advance();
      continue;
    }

    scratch.clear();
    StringRef blobData;
    unsigned kind = cursor.readRecord(next.ID, scratch, &blobData);
    switch (kind) {
    case control_block::METADATA:
      // Already saw metadata.
      if (sawMetadata)
        return true;

      if (scratch[0] != VERSION_MAJOR || scratch[1] != VERSION_MINOR)
        return true;

      sawMetadata = true;
      break;

    default:
      // Unknown metadata record, possibly for use by a future version of the
      // module format.
      break;
    }

    next = cursor.advance();
  }

  return !sawMetadata;
}

bool APINotesReader::Implementation::readIdentifierBlock(
       llvm::BitstreamCursor &cursor,
       SmallVectorImpl<uint64_t> &scratch) {
  if (cursor.EnterSubBlock(IDENTIFIER_BLOCK_ID))
    return true;

  auto next = cursor.advance();
  while (next.Kind != llvm::BitstreamEntry::EndBlock) {
    if (next.Kind == llvm::BitstreamEntry::Error)
      return true;

    if (next.Kind == llvm::BitstreamEntry::SubBlock) {
      // Unknown sub-block, possibly for use by a future version of the
      // API notes format.
      if (cursor.SkipBlock())
        return true;
      
      next = cursor.advance();
      continue;
    }

    scratch.clear();
    StringRef blobData;
    unsigned kind = cursor.readRecord(next.ID, scratch, &blobData);
    switch (kind) {
    case identifier_block::IDENTIFIER_DATA: {
      // Already saw identifier table.
      if (IdentifierTable)
        return true;

      uint32_t tableOffset;
      identifier_block::IdentifierDataLayout::readRecord(scratch, tableOffset);
      auto base = reinterpret_cast<const uint8_t *>(blobData.data());

      IdentifierTable.reset(
        SerializedIdentifierTable::Create(base + tableOffset,
                                          base + sizeof(uint32_t),
                                          base));
      break;
    }

    default:
      // Unknown record, possibly for use by a future version of the
      // module format.
      break;
    }

    next = cursor.advance();
  }

  return false;
}

bool APINotesReader::Implementation::readObjCContextBlock(
       llvm::BitstreamCursor &cursor,
       SmallVectorImpl<uint64_t> &scratch) {
  if (cursor.EnterSubBlock(OBJC_CONTEXT_BLOCK_ID))
    return true;

  auto next = cursor.advance();
  while (next.Kind != llvm::BitstreamEntry::EndBlock) {
    if (next.Kind == llvm::BitstreamEntry::Error)
      return true;

    if (next.Kind == llvm::BitstreamEntry::SubBlock) {
      // Unknown sub-block, possibly for use by a future version of the
      // API notes format.
      if (cursor.SkipBlock())
        return true;
      
      next = cursor.advance();
      continue;
    }

    scratch.clear();
    StringRef blobData;
    unsigned kind = cursor.readRecord(next.ID, scratch, &blobData);
    switch (kind) {
    case objc_context_block::OBJC_CONTEXT_DATA: {
      // Already saw Objective-C class table.
      if (ObjCContextTable)
        return true;

      uint32_t tableOffset;
      objc_context_block::ObjCContextDataLayout::readRecord(scratch, tableOffset);
      auto base = reinterpret_cast<const uint8_t *>(blobData.data());

      ObjCContextTable.reset(
        SerializedObjCContextTable::Create(base + tableOffset,
                                         base + sizeof(uint32_t),
                                         base));
      break;
    }

    default:
      // Unknown record, possibly for use by a future version of the
      // module format.
      break;
    }

    next = cursor.advance();
  }

  return false;
}

bool APINotesReader::Implementation::readObjCPropertyBlock(
       llvm::BitstreamCursor &cursor, 
       SmallVectorImpl<uint64_t> &scratch) {
  if (cursor.EnterSubBlock(OBJC_PROPERTY_BLOCK_ID))
    return true;

  auto next = cursor.advance();
  while (next.Kind != llvm::BitstreamEntry::EndBlock) {
    if (next.Kind == llvm::BitstreamEntry::Error)
      return true;

    if (next.Kind == llvm::BitstreamEntry::SubBlock) {
      // Unknown sub-block, possibly for use by a future version of the
      // API notes format.
      if (cursor.SkipBlock())
        return true;
      
      next = cursor.advance();
      continue;
    }

    scratch.clear();
    StringRef blobData;
    unsigned kind = cursor.readRecord(next.ID, scratch, &blobData);
    switch (kind) {
    case objc_property_block::OBJC_PROPERTY_DATA: {
      // Already saw Objective-C property table.
      if (ObjCPropertyTable)
        return true;

      uint32_t tableOffset;
      objc_property_block::ObjCPropertyDataLayout::readRecord(scratch, 
                                                              tableOffset);
      auto base = reinterpret_cast<const uint8_t *>(blobData.data());

      ObjCPropertyTable.reset(
        SerializedObjCPropertyTable::Create(base + tableOffset,
                                            base + sizeof(uint32_t),
                                            base));
      break;
    }

    default:
      // Unknown record, possibly for use by a future version of the
      // module format.
      break;
    }

    next = cursor.advance();
  }

  return false;
}

bool APINotesReader::Implementation::readObjCMethodBlock(
       llvm::BitstreamCursor &cursor, 
       SmallVectorImpl<uint64_t> &scratch) {
  if (cursor.EnterSubBlock(OBJC_METHOD_BLOCK_ID))
    return true;

  auto next = cursor.advance();
  while (next.Kind != llvm::BitstreamEntry::EndBlock) {
    if (next.Kind == llvm::BitstreamEntry::Error)
      return true;

    if (next.Kind == llvm::BitstreamEntry::SubBlock) {
      // Unknown sub-block, possibly for use by a future version of the
      // API notes format.
      if (cursor.SkipBlock())
        return true;
      
      next = cursor.advance();
      continue;
    }

    scratch.clear();
    StringRef blobData;
    unsigned kind = cursor.readRecord(next.ID, scratch, &blobData);
    switch (kind) {
    case objc_method_block::OBJC_METHOD_DATA: {
      // Already saw Objective-C method table.
      if (ObjCMethodTable)
        return true;

      uint32_t tableOffset;
      objc_method_block::ObjCMethodDataLayout::readRecord(scratch, tableOffset);
      auto base = reinterpret_cast<const uint8_t *>(blobData.data());

      ObjCMethodTable.reset(
        SerializedObjCMethodTable::Create(base + tableOffset,
                                          base + sizeof(uint32_t),
                                          base));
      break;
    }

    default:
      // Unknown record, possibly for use by a future version of the
      // module format.
      break;
    }

    next = cursor.advance();
  }

  return false;
}

bool APINotesReader::Implementation::readObjCSelectorBlock(
       llvm::BitstreamCursor &cursor, 
       SmallVectorImpl<uint64_t> &scratch) {
  if (cursor.EnterSubBlock(OBJC_SELECTOR_BLOCK_ID))
    return true;

  auto next = cursor.advance();
  while (next.Kind != llvm::BitstreamEntry::EndBlock) {
    if (next.Kind == llvm::BitstreamEntry::Error)
      return true;

    if (next.Kind == llvm::BitstreamEntry::SubBlock) {
      // Unknown sub-block, possibly for use by a future version of the
      // API notes format.
      if (cursor.SkipBlock())
        return true;
      
      next = cursor.advance();
      continue;
    }

    scratch.clear();
    StringRef blobData;
    unsigned kind = cursor.readRecord(next.ID, scratch, &blobData);
    switch (kind) {
    case objc_selector_block::OBJC_SELECTOR_DATA: {
      // Already saw Objective-C selector table.
      if (ObjCSelectorTable)
        return true;

      uint32_t tableOffset;
      objc_selector_block::ObjCSelectorDataLayout::readRecord(scratch, 
                                                              tableOffset);
      auto base = reinterpret_cast<const uint8_t *>(blobData.data());

      ObjCSelectorTable.reset(
        SerializedObjCSelectorTable::Create(base + tableOffset,
                                          base + sizeof(uint32_t),
                                          base));
      break;
    }

    default:
      // Unknown record, possibly for use by a future version of the
      // module format.
      break;
    }

    next = cursor.advance();
  }

  return false;
}

APINotesReader::APINotesReader(std::unique_ptr<llvm::MemoryBuffer> inputBuffer, 
                             bool &failed) 
  : Impl(*new Implementation)
{
  failed = false;

  // Initialize the input buffer.
  Impl.InputBuffer = std::move(inputBuffer);
  Impl.InputReader.init(
    reinterpret_cast<const uint8_t *>(Impl.InputBuffer->getBufferStart()), 
    reinterpret_cast<const uint8_t *>(Impl.InputBuffer->getBufferEnd()));
  llvm::BitstreamCursor cursor(Impl.InputReader);

  // Validate signature.
  for (auto byte : API_NOTES_SIGNATURE) {
    if (cursor.AtEndOfStream() || cursor.Read(8) != byte) {
      failed = true;
      return;
    }
  }

  // Look at all of the blocks.
  bool hasValidControlBlock = false;
  SmallVector<uint64_t, 64> scratch;
  auto topLevelEntry = cursor.advance();
  while (topLevelEntry.Kind == llvm::BitstreamEntry::SubBlock) {
    switch (topLevelEntry.ID) {
    case llvm::bitc::BLOCKINFO_BLOCK_ID:
      if (cursor.ReadBlockInfoBlock()) {
        failed = true;
        break;
      }
      break;

    case CONTROL_BLOCK_ID:
      // Only allow a single control block.
      if (hasValidControlBlock || Impl.readControlBlock(cursor, scratch)) {
        failed = true;
        return;
      }

      hasValidControlBlock = true;
      break;

    case IDENTIFIER_BLOCK_ID:
      if (!hasValidControlBlock || Impl.readIdentifierBlock(cursor, scratch)) {
        failed = true;
        return;
      }
      break;

    case OBJC_CONTEXT_BLOCK_ID:
      if (!hasValidControlBlock || Impl.readObjCContextBlock(cursor, scratch)) {
        failed = true;
        return;
      }

      break;

    case OBJC_PROPERTY_BLOCK_ID:
      if (!hasValidControlBlock || 
          Impl.readObjCPropertyBlock(cursor, scratch)) {
        failed = true;
        return;
      }
      break;

    case OBJC_METHOD_BLOCK_ID:
      if (!hasValidControlBlock || Impl.readObjCMethodBlock(cursor, scratch)) {
        failed = true;
        return;
      }
      break;

    case OBJC_SELECTOR_BLOCK_ID:
      if (!hasValidControlBlock || 
          Impl.readObjCSelectorBlock(cursor, scratch)) {
        failed = true;
        return;
      }
      break;

    default:
      // Unknown top-level block, possibly for use by a future version of the
      // module format.
      if (cursor.SkipBlock()) {
        failed = true;
        return;
      }
      break;
    }

    topLevelEntry = cursor.advance(llvm::BitstreamCursor::AF_DontPopBlockAtEnd);
  }

  if (topLevelEntry.Kind != llvm::BitstreamEntry::EndBlock) {
    failed = true;
    return;
  }
}

APINotesReader::~APINotesReader() {
}

std::unique_ptr<APINotesReader> 
APINotesReader::get(std::unique_ptr<llvm::MemoryBuffer> inputBuffer) {
  bool failed = false;
  std::unique_ptr<APINotesReader> 
    reader(new APINotesReader(std::move(inputBuffer), failed));
  if (failed)
    return nullptr;

  return std::move(reader);
}

auto APINotesReader::lookupObjCClass(StringRef name)
       -> Optional<std::pair<ContextID, ObjCContextInfo>> {
  if (!Impl.ObjCContextTable)
    return Nothing;

  Optional<IdentifierID> classID = Impl.getIdentifier(name);
  if (!classID)
    return Nothing;

  auto known = Impl.ObjCContextTable->find({*classID, '\0'});
  if (known == Impl.ObjCContextTable->end())
    return Nothing;

  auto result = *known;
  return { ContextID(result.first), result.second };
}

auto APINotesReader::lookupObjCProtocol(StringRef name)
       -> Optional<std::pair<ContextID, ObjCContextInfo>> {
  if (!Impl.ObjCContextTable)
    return Nothing;

  Optional<IdentifierID> classID = Impl.getIdentifier(name);
  if (!classID)
    return Nothing;

  auto known = Impl.ObjCContextTable->find({*classID, '\1'});
  if (known == Impl.ObjCContextTable->end())
    return Nothing;

  auto result = *known;
  return { ContextID(result.first), result.second };
}

Optional<ObjCPropertyInfo> APINotesReader::lookupObjCProperty(
                             ContextID contextID,
                             StringRef name) {
  if (!Impl.ObjCPropertyTable)
    return Nothing;

  Optional<IdentifierID> propertyID = Impl.getIdentifier(name);
  if (!propertyID)
    return Nothing;

  auto known = Impl.ObjCPropertyTable->find({contextID.Value, *propertyID});
  if (known == Impl.ObjCPropertyTable->end())
    return Nothing;

  return *known;
}

Optional<ObjCMethodInfo> APINotesReader::lookupObjCMethod(
                           ContextID contextID,
                           ObjCSelectorRef selector,
                           bool isInstanceMethod) {
  if (!Impl.ObjCMethodTable)
    return Nothing;

  Optional<SelectorID> selectorID = Impl.getSelector(selector);
  if (!selectorID)
    return Nothing;

  auto known = Impl.ObjCMethodTable->find({contextID.Value, *selectorID,
                                           isInstanceMethod});
  if (known == Impl.ObjCMethodTable->end())
    return Nothing;

  return *known;
}

APINotesReader::Visitor::~Visitor() { }

void APINotesReader::Visitor::visitObjCClass(ContextID contextID,
                                             StringRef name,
                                             const ObjCContextInfo &info) { }

void APINotesReader::Visitor::visitObjCProtocol(ContextID contextID,
                                                StringRef name,
                                                const ObjCContextInfo &info) { }

void APINotesReader::Visitor::visitObjCMethod(ContextID contextID,
                                              StringRef selector,
                                              bool isInstanceMethod,
                                              const ObjCMethodInfo &info) { }

void APINotesReader::Visitor::visitObjCProperty(ContextID contextID,
                                                StringRef name,
                                                const ObjCPropertyInfo &info) { }

void APINotesReader::visit(Visitor &visitor) {
  // FIXME: All of these iterations would be significantly more efficient if we
  // could get the keys and data together, but OnDiskIterableHashTable doesn't
  // support that.

  // Build an identifier ID -> string mapping, which we'll need when visiting
  // any of the tables.
  llvm::DenseMap<unsigned, StringRef> identifiers;
  if (Impl.IdentifierTable) {
    for (auto key : Impl.IdentifierTable->keys()) {
      unsigned ID = *Impl.IdentifierTable->find(key);
      assert(identifiers.count(ID) == 0);
      identifiers[ID] = key;
    }
  }

  // Visit classes and protocols.
  if (Impl.ObjCContextTable) {
    for (auto key : Impl.ObjCContextTable->keys()) {
      auto name = identifiers[key.first];
      auto info = *Impl.ObjCContextTable->find(key);

      if (key.second)
        visitor.visitObjCProtocol(ContextID(info.first), name, info.second);
      else
        visitor.visitObjCClass(ContextID(info.first), name, info.second);
    }
  }

  // Build a selector ID -> stored Objective-C selector mapping, which we need
  // when visiting the method tables.
  llvm::DenseMap<unsigned, std::string> selectors;
  if (Impl.ObjCSelectorTable) {
    for (auto key : Impl.ObjCSelectorTable->keys()) {
      std::string selector;
      if (key.NumPieces == 0)
        selector = identifiers[key.Identifiers[0]];
      else {
        for (auto identID : key.Identifiers) {
          selector += identifiers[identID];
          selector += ':';
        }
      }

      unsigned selectorID = *Impl.ObjCSelectorTable->find(key);
      selectors[selectorID] = selector;
    }
  }

  // Visit methods.
  if (Impl.ObjCMethodTable) {
    for (auto key : Impl.ObjCMethodTable->keys()) {
      ContextID contextID(std::get<0>(key));
      const auto &selector = selectors[std::get<1>(key)];
      auto info = *Impl.ObjCMethodTable->find(key);
      visitor.visitObjCMethod(contextID, selector, std::get<2>(key), info);
    }
  }

  // Visit properties.
  if (Impl.ObjCPropertyTable) {
    for (auto key : Impl.ObjCPropertyTable->keys()) {
      ContextID contextID(key.first);
      auto name = identifiers[key.second];
      auto info = *Impl.ObjCPropertyTable->find(key);
      visitor.visitObjCProperty(contextID, name, info);
    }
  }
}

