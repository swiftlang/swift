//===--- SideCarWriter.cpp - Side Car Writer --------------------*- C++ -*-===//
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
// This file implements the \c SideCarWriter class that writes out
// source side-car data providing additional information about source
// code as a separate input, such as the non-nil/nilable annotations
// for method parameters.
//
//===----------------------------------------------------------------------===//
#include "swift/APINotes/SideCarWriter.h"
#include "SideCarFormat.h"
#include "swift/Basic/DenseMapInfo.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/EndianStream.h"
#include "llvm/Support/OnDiskHashTable.h"
#include "llvm/Support/raw_ostream.h"
#include <tuple>
#include <vector>
using namespace swift;
using namespace side_car;
using namespace llvm::support;

class SideCarWriter::Implementation {
  /// Mapping from strings to identifier IDs.
  llvm::StringMap<IdentifierID> IdentifierIDs;

  /// Mapping from selectors to selector ID.
  llvm::DenseMap<StoredObjCSelector, SelectorID> SelectorIDs;

  /// Scratch space for bitstream writing.
  SmallVector<uint64_t, 64> ScratchRecord;

public:
  /// Information about Objective-C classes.
  ///
  /// Indexed by the class ID and provides information
  /// describing the class within that module.
  llvm::DenseMap<unsigned, ObjCClassInfo> ObjCClasses;

  /// Information about Objective-C properties.
  ///
  /// Indexed by the class ID and property name.
  llvm::DenseMap<std::pair<unsigned, unsigned>, ObjCPropertyInfo> 
    ObjCProperties;

  /// Information about Objective-C methods.
  ///
  /// Indexed by the class ID, selector ID, and Boolean (stored as a
  /// char) indicating whether this is a class or instance method.
  llvm::DenseMap<std::tuple<unsigned, unsigned, char>, ObjCMethodInfo> 
    ObjCMethods;

  /// Retrieve the ID for the given identifier.
  IdentifierID getIdentifier(StringRef identifier) {
    if (identifier.empty())
      return 0;

    auto known = IdentifierIDs.find(identifier);
    if (known != IdentifierIDs.end())
      return known->second;

    // Add to the identifier table.
    known = IdentifierIDs.insert({identifier, IdentifierIDs.size() + 1}).first;
    return known->second;
  }

  /// Retrieve the ID for the given selector.
  SelectorID getSelector(ObjCSelectorRef selectorRef) {
    // Translate the selector reference into a stored selector.
    StoredObjCSelector selector;
    selector.NumPieces = selectorRef.NumPieces;
    selector.Identifiers.reserve(selectorRef.Identifiers.size());
    for (auto piece : selectorRef.Identifiers) {
      selector.Identifiers.push_back(getIdentifier(piece));
    }

    // Look for the stored selector.
    auto known = SelectorIDs.find(selector);
    if (known != SelectorIDs.end())
      return known->second;

    // Add to the selector table.
    known = SelectorIDs.insert({selector, SelectorIDs.size()}).first;
    return known->second;
  }

  void writeToStream(llvm::raw_ostream &os);

private:
  void writeBlockInfoBlock(llvm::BitstreamWriter &writer);
  void writeControlBlock(llvm::BitstreamWriter &writer);
  void writeIdentifierBlock(llvm::BitstreamWriter &writer);
  void writeObjCClassBlock(llvm::BitstreamWriter &writer);
  void writeObjCPropertyBlock(llvm::BitstreamWriter &writer);
  void writeObjCMethodBlock(llvm::BitstreamWriter &writer);
  void writeObjCSelectorBlock(llvm::BitstreamWriter &writer);
};

/// Record the name of a block.
static void emitBlockID(llvm::BitstreamWriter &out, unsigned ID,
                        StringRef name,
                        SmallVectorImpl<unsigned char> &nameBuffer) {
  SmallVector<unsigned, 1> idBuffer;
  idBuffer.push_back(ID);
  out.EmitRecord(llvm::bitc::BLOCKINFO_CODE_SETBID, idBuffer);

  // Emit the block name if present.
  if (name.empty())
    return;
  nameBuffer.resize(name.size());
  memcpy(nameBuffer.data(), name.data(), name.size());
  out.EmitRecord(llvm::bitc::BLOCKINFO_CODE_BLOCKNAME, nameBuffer);
}

/// Record the name of a record within a block.
static void emitRecordID(llvm::BitstreamWriter &out, unsigned ID,
                         StringRef name,
                         SmallVectorImpl<unsigned char> &nameBuffer) {
  assert(ID < 256 && "can't fit record ID in next to name");
  nameBuffer.resize(name.size()+1);
  nameBuffer[0] = ID;
  memcpy(nameBuffer.data()+1, name.data(), name.size());
  out.EmitRecord(llvm::bitc::BLOCKINFO_CODE_SETRECORDNAME, nameBuffer);
}

void SideCarWriter::Implementation::writeBlockInfoBlock(
       llvm::BitstreamWriter &writer) {
  BCBlockRAII restoreBlock(writer, llvm::bitc::BLOCKINFO_BLOCK_ID, 2);  

  SmallVector<unsigned char, 64> nameBuffer;
#define BLOCK(X) emitBlockID(writer, X ## _ID, #X, nameBuffer)
#define BLOCK_RECORD(K, X) emitRecordID(writer, K::X, #X, nameBuffer)

  BLOCK(CONTROL_BLOCK);
  BLOCK_RECORD(control_block, METADATA);

  BLOCK(IDENTIFIER_BLOCK);
  BLOCK_RECORD(identifier_block, IDENTIFIER_DATA);

  BLOCK(OBJC_CLASS_BLOCK);
  BLOCK_RECORD(objc_class_block, OBJC_CLASS_DATA);

  BLOCK(OBJC_PROPERTY_BLOCK);
  BLOCK_RECORD(objc_property_block, OBJC_PROPERTY_DATA);

  BLOCK(OBJC_METHOD_BLOCK);
  BLOCK_RECORD(objc_method_block, OBJC_METHOD_DATA);

  BLOCK(OBJC_SELECTOR_BLOCK);
  BLOCK_RECORD(objc_selector_block, OBJC_SELECTOR_DATA);

#undef BLOCK
#undef BLOCK_RECORD
}

void SideCarWriter::Implementation::writeControlBlock(llvm::BitstreamWriter &writer) {
  BCBlockRAII restoreBlock(writer, CONTROL_BLOCK_ID, 3);
  control_block::MetadataLayout metadata(writer);
  metadata.emit(ScratchRecord, VERSION_MAJOR, VERSION_MINOR);
}

namespace {
  /// Used to serialize the on-disk identifier table.
  class IdentifierTableInfo {
  public:
    using key_type = StringRef;
    using key_type_ref = key_type;
    using data_type = IdentifierID;
    using data_type_ref = const data_type &;
    using hash_value_type = uint32_t;
    using offset_type = unsigned;

    hash_value_type ComputeHash(key_type_ref key) {
      return llvm::HashString(key);
    }

    std::pair<unsigned, unsigned> EmitKeyDataLength(raw_ostream &out,
                                                    key_type_ref key,
                                                    data_type_ref data) {
      uint32_t keyLength = key.size();
      uint32_t dataLength = sizeof(uint32_t);
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
      static_assert(sizeof(IdentifierID) <= 4, "DeclID too large");
      endian::Writer<little> writer(out);
      writer.write<uint32_t>(data);
    }
  };
} // end anonymous namespace

void SideCarWriter::Implementation::writeIdentifierBlock(
       llvm::BitstreamWriter &writer) {
  BCBlockRAII restoreBlock(writer, IDENTIFIER_BLOCK_ID, 3);

  if (IdentifierIDs.empty())
    return;

  llvm::SmallString<4096> hashTableBlob;
  uint32_t tableOffset;
  {
    llvm::OnDiskChainedHashTableGenerator<IdentifierTableInfo> generator;
    for (auto &entry : IdentifierIDs)
      generator.insert(entry.first(), entry.second);

    llvm::raw_svector_ostream blobStream(hashTableBlob);
    // Make sure that no bucket is at offset 0
    endian::Writer<little>(blobStream).write<uint32_t>(0);
    tableOffset = generator.Emit(blobStream);
  }

  identifier_block::IdentifierDataLayout layout(writer);
  layout.emit(ScratchRecord, tableOffset, hashTableBlob);
}

namespace {
  /// Used to serialize the on-disk Objective-C class table.
  class ObjCClassTableInfo {
  public:
    using key_type = unsigned; // class ID
    using key_type_ref = key_type;
    using data_type = ObjCClassInfo;
    using data_type_ref = const data_type &;
    using hash_value_type = size_t;
    using offset_type = unsigned;

    hash_value_type ComputeHash(key_type_ref key) {
      return static_cast<size_t>(llvm::hash_value(key));
    }

    std::pair<unsigned, unsigned> EmitKeyDataLength(raw_ostream &out,
                                                    key_type_ref key,
                                                    data_type_ref data) {
      uint32_t keyLength = sizeof(IdentifierID);
      uint32_t dataLength = 2;
      endian::Writer<little> writer(out);
      writer.write<uint16_t>(keyLength);
      writer.write<uint16_t>(dataLength);
      return { keyLength, dataLength };
    }

    void EmitKey(raw_ostream &out, key_type_ref key, unsigned len) {
      endian::Writer<little> writer(out);
      writer.write<IdentifierID>(key);
    }

    void EmitData(raw_ostream &out, key_type_ref key, data_type_ref data,
                  unsigned len) {
      // FIXME: Inefficient representation.
      uint8_t bytes[2] = { 0, 0 };
      if (auto nullable = data.getDefaultNullability()) {
        bytes[0] = 1;
        bytes[1] = static_cast<uint8_t>(*nullable);
      } else {
        // Nothing to do.
      }
      out.write(reinterpret_cast<const char *>(bytes), 2);
    }
  };
} // end anonymous namespace

void SideCarWriter::Implementation::writeObjCClassBlock(
       llvm::BitstreamWriter &writer) {
  BCBlockRAII restoreBlock(writer, OBJC_CLASS_BLOCK_ID, 3);

  if (ObjCClasses.empty())
    return;  

  llvm::SmallString<4096> hashTableBlob;
  uint32_t tableOffset;
  {
    llvm::OnDiskChainedHashTableGenerator<ObjCClassTableInfo> generator;
    for (auto &entry : ObjCClasses)
      generator.insert(entry.first, entry.second);

    llvm::raw_svector_ostream blobStream(hashTableBlob);
    // Make sure that no bucket is at offset 0
    endian::Writer<little>(blobStream).write<uint32_t>(0);
    tableOffset = generator.Emit(blobStream);
  }

  objc_class_block::ObjCClassDataLayout layout(writer);
  layout.emit(ScratchRecord, tableOffset, hashTableBlob);
}

namespace {
  /// Used to serialize the on-disk Objective-C property table.
  class ObjCPropertyTableInfo {
  public:
    using key_type = std::pair<unsigned, unsigned>; // (class ID, name ID)
    using key_type_ref = key_type;
    using data_type = ObjCPropertyInfo;
    using data_type_ref = const data_type &;
    using hash_value_type = size_t;
    using offset_type = unsigned;

    hash_value_type ComputeHash(key_type_ref key) {
      return static_cast<size_t>(llvm::hash_value(key));
    }

    std::pair<unsigned, unsigned> EmitKeyDataLength(raw_ostream &out,
                                                    key_type_ref key,
                                                    data_type_ref data) {
      uint32_t keyLength = sizeof(IdentifierID) + sizeof(IdentifierID);
      uint32_t dataLength = 2;
      endian::Writer<little> writer(out);
      writer.write<uint16_t>(keyLength);
      writer.write<uint16_t>(dataLength);
      return { keyLength, dataLength };
    }

    void EmitKey(raw_ostream &out, key_type_ref key, unsigned len) {
      endian::Writer<little> writer(out);
      writer.write<IdentifierID>(key.first);
      writer.write<IdentifierID>(key.second);
    }

    void EmitData(raw_ostream &out, key_type_ref key, data_type_ref data,
                  unsigned len) {
      // FIXME: Terrible solution. This needs abstraction.
      uint8_t bytes[2] = { 0, 0 };
      if (auto nullable = data.getNullability()) {
        bytes[0] = 1;
        bytes[1] = static_cast<uint8_t>(*nullable);
      } else {
        // Nothing to do.
      }
      
      out.write(reinterpret_cast<const char *>(bytes), 2);
    }
  };
} // end anonymous namespace

void SideCarWriter::Implementation::writeObjCPropertyBlock(
       llvm::BitstreamWriter &writer) {
  BCBlockRAII restoreBlock(writer, OBJC_PROPERTY_BLOCK_ID, 3);

  if (ObjCProperties.empty())
    return;  

  llvm::SmallString<4096> hashTableBlob;
  uint32_t tableOffset;
  {
    llvm::OnDiskChainedHashTableGenerator<ObjCPropertyTableInfo> generator;
    for (auto &entry : ObjCProperties)
      generator.insert(entry.first, entry.second);

    llvm::raw_svector_ostream blobStream(hashTableBlob);
    // Make sure that no bucket is at offset 0
    endian::Writer<little>(blobStream).write<uint32_t>(0);
    tableOffset = generator.Emit(blobStream);
  }

  objc_property_block::ObjCPropertyDataLayout layout(writer);
  layout.emit(ScratchRecord, tableOffset, hashTableBlob);
}

namespace {
  /// Used to serialize the on-disk Objective-C method table.
  class ObjCMethodTableInfo {
  public:
    // (class ID, selector ID, is-instance)
    using key_type = std::tuple<unsigned, unsigned, char>; 
    using key_type_ref = key_type;
    using data_type = ObjCMethodInfo;
    using data_type_ref = const data_type &;
    using hash_value_type = size_t;
    using offset_type = unsigned;

    hash_value_type ComputeHash(key_type_ref key) {
      return llvm::hash_combine(std::get<0>(key), 
                                std::get<1>(key), 
                                std::get<2>(key));
    }

    std::pair<unsigned, unsigned> EmitKeyDataLength(raw_ostream &out,
                                                    key_type_ref key,
                                                    data_type_ref data) {
      uint32_t keyLength = sizeof(IdentifierID) + sizeof(SelectorID) + 1;
      uint32_t dataLength = 5 + sizeof(uint64_t) + data.UnavailableMsg.size(); 
      endian::Writer<little> writer(out);
      writer.write<uint16_t>(keyLength);
      writer.write<uint16_t>(dataLength);
      return { keyLength, dataLength };
    }

    void EmitKey(raw_ostream &out, key_type_ref key, unsigned len) {
      endian::Writer<little> writer(out);
      writer.write<IdentifierID>(std::get<0>(key));
      writer.write<SelectorID>(std::get<1>(key));
      writer.write<uint8_t>(std::get<2>(key));
    }

    void EmitData(raw_ostream &out, key_type_ref key, data_type_ref data,
                  unsigned len) {
      endian::Writer<little> writer(out);

      // FIXME: Inefficient representation
      writer.write<uint8_t>(data.DesignatedInit);
      writer.write<uint8_t>(data.FactoryAsInit);
      writer.write<uint8_t>(data.Unavailable);
      writer.write<uint8_t>(data.NullabilityAudited);
      writer.write<uint8_t>(data.NumAdjustedNullable);
      writer.write<uint64_t>(data.NullabilityPayload);
      out.write(data.UnavailableMsg.c_str(), data.UnavailableMsg.size());
    }
  };
} // end anonymous namespace

void SideCarWriter::Implementation::writeObjCMethodBlock(
       llvm::BitstreamWriter &writer) {
  BCBlockRAII restoreBlock(writer, OBJC_METHOD_BLOCK_ID, 3);

  if (ObjCMethods.empty())
    return;  

  llvm::SmallString<4096> hashTableBlob;
  uint32_t tableOffset;
  {
    llvm::OnDiskChainedHashTableGenerator<ObjCMethodTableInfo> generator;
    for (auto &entry : ObjCMethods) {
      generator.insert(entry.first, entry.second);
    }

    llvm::raw_svector_ostream blobStream(hashTableBlob);
    // Make sure that no bucket is at offset 0
    endian::Writer<little>(blobStream).write<uint32_t>(0);
    tableOffset = generator.Emit(blobStream);
  }

  objc_method_block::ObjCMethodDataLayout layout(writer);
  layout.emit(ScratchRecord, tableOffset, hashTableBlob);
}

namespace {
  /// Used to serialize the on-disk Objective-C selector table.
  class ObjCSelectorTableInfo {
  public:
    using key_type = StoredObjCSelector;
    using key_type_ref = const key_type &;
    using data_type = SelectorID;
    using data_type_ref = data_type;
    using hash_value_type = unsigned;
    using offset_type = unsigned;

    hash_value_type ComputeHash(key_type_ref key) {
      return llvm::DenseMapInfo<StoredObjCSelector>::getHashValue(key);
    }

    std::pair<unsigned, unsigned> EmitKeyDataLength(raw_ostream &out,
                                                    key_type_ref key,
                                                    data_type_ref data) {
      uint32_t keyLength = sizeof(uint16_t) 
                         + sizeof(IdentifierID) * key.Identifiers.size();
      uint32_t dataLength = sizeof(SelectorID);
      endian::Writer<little> writer(out);
      writer.write<uint16_t>(keyLength);
      writer.write<uint16_t>(dataLength);
      return { keyLength, dataLength };
    }

    void EmitKey(raw_ostream &out, key_type_ref key, unsigned len) {
      endian::Writer<little> writer(out);
      writer.write<uint16_t>(key.NumPieces);
      for (auto piece : key.Identifiers) {
        writer.write<IdentifierID>(piece);
      }
    }

    void EmitData(raw_ostream &out, key_type_ref key, data_type_ref data,
                  unsigned len) {
      endian::Writer<little> writer(out);
      writer.write<SelectorID>(data);
    }
  };
} // end anonymous namespace

void SideCarWriter::Implementation::writeObjCSelectorBlock(
       llvm::BitstreamWriter &writer) {
  BCBlockRAII restoreBlock(writer, OBJC_SELECTOR_BLOCK_ID, 3);

  if (SelectorIDs.empty())
    return;  

  llvm::SmallString<4096> hashTableBlob;
  uint32_t tableOffset;
  {
    llvm::OnDiskChainedHashTableGenerator<ObjCSelectorTableInfo> generator;
    for (auto &entry : SelectorIDs)
      generator.insert(entry.first, entry.second);

    llvm::raw_svector_ostream blobStream(hashTableBlob);
    // Make sure that no bucket is at offset 0
    endian::Writer<little>(blobStream).write<uint32_t>(0);
    tableOffset = generator.Emit(blobStream);
  }

  objc_selector_block::ObjCSelectorDataLayout layout(writer);
  layout.emit(ScratchRecord, tableOffset, hashTableBlob);
}

void SideCarWriter::Implementation::writeToStream(llvm::raw_ostream &os) {
  // Write the side car file into a buffer.
  SmallVector<char, 0> buffer;
  {
    llvm::BitstreamWriter writer(buffer);

    // Emit the signature.
    for (unsigned char byte : SIDE_CAR_SIGNATURE)
      writer.Emit(byte, 8);

    // Emit the blocks.
    writeBlockInfoBlock(writer);
    writeControlBlock(writer);
    writeIdentifierBlock(writer);
    writeObjCClassBlock(writer);
    writeObjCPropertyBlock(writer);
    writeObjCMethodBlock(writer);
    writeObjCSelectorBlock(writer);
  }

  // Write the buffer to the stream.
  os.write(buffer.data(), buffer.size());
  os.flush();
}

SideCarWriter::SideCarWriter()
  : Impl(*new Implementation)
{
}

SideCarWriter::~SideCarWriter() {
  delete &Impl;
}


void SideCarWriter::writeToStream(raw_ostream &os) {
  Impl.writeToStream(os);
}

void SideCarWriter::addObjCClass(StringRef name, const ObjCClassInfo &info) {
  IdentifierID classID = Impl.getIdentifier(name);
  assert(!Impl.ObjCClasses.count(classID));
  Impl.ObjCClasses[classID] = info;
}

void SideCarWriter::addObjCProperty(StringRef className, StringRef name,
                                    const ObjCPropertyInfo &info) {
  IdentifierID classID = Impl.getIdentifier(className);
  IdentifierID nameID = Impl.getIdentifier(name);
  assert(!Impl.ObjCProperties.count({classID, nameID}));
  Impl.ObjCProperties[{classID, nameID}] = info;
}

void SideCarWriter::addObjCMethod(StringRef className, 
                                  ObjCSelectorRef selector, 
                                  bool isInstanceMethod, 
                                  const ObjCMethodInfo &info) {
  IdentifierID classID = Impl.getIdentifier(className);
  SelectorID selectorID = Impl.getSelector(selector);
  assert(!Impl.ObjCMethods.count({classID, selectorID, isInstanceMethod}));
  Impl.ObjCMethods[{classID, selectorID, isInstanceMethod}] = info;
}


