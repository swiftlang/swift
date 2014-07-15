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
#include "swift/SideCar/SideCarWriter.h"
#include "SideCarFormat.h"
#include "llvm/ADT/DenseMap.h"
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

  /// Mapping from names to class ID.
  llvm::StringMap<ClassID> ClassIDs;

  /// Mapping from names to module ID.
  llvm::StringMap<ModuleID> ModuleIDs;

  /// Mapping from selectors to selector ID.
  llvm::StringMap<SelectorID> SelectorIDs;

  /// Scratch space for bitstream writing.
  SmallVector<uint64_t, 64> ScratchRecord;

public:
  /// Information about Objective-C classes.
  ///
  /// Indexed by the class ID, and provides a sequence of (module ID,
  /// class info) tuples with information about the class scoped to a particular
  /// module.
  llvm::DenseMap<
    unsigned,
    std::vector<std::tuple<ModuleID, ObjCClassInfo>>> ObjCClasses;

  /// Information about Objective-C properties.
  ///
  /// Indexed by the class ID and property name.
  llvm::DenseMap<
    std::pair<unsigned, unsigned>,
    std::vector<std::tuple<ModuleID, ObjCPropertyInfo>>> ObjCProperties;

  /// Information about Objective-C methods.
  ///
  /// Indexed by the class ID and selector ID.
  llvm::DenseMap<
    std::pair<unsigned, unsigned>,
    std::vector<std::tuple<bool, ModuleID, ObjCMethodInfo>>> ObjCMethods;

  /// Retrieve the ID for the given identifier.
  IdentifierID getIdentifier(StringRef identifier) {
    auto known = IdentifierIDs.find(identifier);
    if (known != IdentifierIDs.end())
      return known->second;

    // Add to the identifier table.
    known = IdentifierIDs.insert({identifier, IdentifierIDs.size()}).first;
    return known->second;
  }

  /// Retrieve the ID for the given class.
  ClassID getClass(StringRef name) {
    auto known = ClassIDs.find(name);
    if (known != ClassIDs.end())
      return known->second;

    // Add to the class table.
    known = ClassIDs.insert({name, ClassIDs.size()}).first;
    return known->second;
  }

  /// Retrieve the ID for the given module.
  ModuleID getModule(StringRef name) {
    auto known = ModuleIDs.find(name);
    if (known != ModuleIDs.end())
      return known->second;

    // Add to the module table.
    known = ModuleIDs.insert({name, ModuleIDs.size()}).first;
    return known->second;
  }

  /// Retrieve the ID for the given selector.
  SelectorID getSelector(StringRef name) {
    auto known = SelectorIDs.find(name);
    if (known != SelectorIDs.end())
      return known->second;

    // Add to the selector table.
    known = SelectorIDs.insert({name, SelectorIDs.size()}).first;
    return known->second;
  }

  void writeToStream(llvm::raw_ostream &os);

private:
  void writeBlockInfoBlock(llvm::BitstreamWriter &writer);
  void writeControlBlock(llvm::BitstreamWriter &writer);
  void writeIdentifierBlock(llvm::BitstreamWriter &writer);
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

void SideCarWriter::Implementation::writeBlockInfoBlock(llvm::BitstreamWriter &writer){
  BCBlockRAII restoreBlock(writer, llvm::bitc::BLOCKINFO_BLOCK_ID, 2);  

  SmallVector<unsigned char, 64> nameBuffer;
#define BLOCK(X) emitBlockID(writer, X ## _ID, #X, nameBuffer)
#define BLOCK_RECORD(K, X) emitRecordID(writer, K::X, #X, nameBuffer)

  BLOCK(SIDE_CAR_BLOCK);

  BLOCK(CONTROL_BLOCK);
  BLOCK_RECORD(control_block, METADATA);

  BLOCK(IDENTIFIER_BLOCK);
  BLOCK_RECORD(identifier_block, IDENTIFIER_DATA);
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

void SideCarWriter::Implementation::writeIdentifierBlock(llvm::BitstreamWriter &writer){
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

void SideCarWriter::Implementation::writeToStream(llvm::raw_ostream &os) {
  // Write the side car file into a buffer.
  SmallVector<char, 0> buffer;
  {
    llvm::BitstreamWriter writer(buffer);

    // Emit the signature.
    for (unsigned char byte : SIDE_CAR_SIGNATURE)
      writer.Emit(byte, 8);

    BCBlockRAII sideCarBlock(writer, SIDE_CAR_BLOCK_ID, 2);

    // Emit the blocks.
    writeBlockInfoBlock(writer);
    writeControlBlock(writer);
    writeIdentifierBlock(writer);
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

void SideCarWriter::addObjCClass(StringRef moduleName, StringRef name,
                                 const ObjCClassInfo &info) {
  ModuleID moduleID = Impl.getModule(moduleName);
  ClassID classID = Impl.getClass(name);
  Impl.ObjCClasses[classID].push_back({moduleID, info});
}

void SideCarWriter::addObjCProperty(StringRef moduleName, StringRef className,
                                    StringRef name,
                                    const ObjCPropertyInfo &info) {
  ModuleID moduleID = Impl.getModule(moduleName);
  ClassID classID = Impl.getClass(className);
  IdentifierID nameID = Impl.getIdentifier(name);
  Impl.ObjCProperties[{classID, nameID}].push_back({moduleID, info});
}

void SideCarWriter::addObjCMethod(StringRef moduleName, StringRef className,
                                  StringRef selector, bool isInstanceMethod,
                                  const ObjCMethodInfo &info) {
  ModuleID moduleID = Impl.getModule(moduleName);
  ClassID classID = Impl.getClass(className);
  SelectorID selectorID = Impl.getSelector(selector);
  Impl.ObjCMethods[{classID, selectorID}]
    .push_back({isInstanceMethod, moduleID, info});
}


