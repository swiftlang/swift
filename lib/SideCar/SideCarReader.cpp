//===--- SideCarReader.cpp - Side Car Reader --------------------*- C++ -*-===//
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
// This file implements the \c SideCarReader class that reads source
// side-car data providing additional information about source code as
// a separate input, such as the non-nil/nilable annotations for
// method parameters.
//
//===----------------------------------------------------------------------===//
#include "swift/SideCar/SideCarReader.h"
#include "SideCarFormat.h"
#include "llvm/Bitcode/BitstreamReader.h"

using namespace swift;
using namespace side_car;

class SideCarReader::Implementation {
public:
  /// The input buffer for the side car data.
  std::unique_ptr<llvm::MemoryBuffer> InputBuffer;

  /// The reader attached to \c InputBuffer.
  llvm::BitstreamReader InputReader;

  bool readControlBlock(llvm::BitstreamCursor &cursor);
  bool readIdentifierBlock(llvm::BitstreamCursor &cursor);
  bool readObjCClassBlock(llvm::BitstreamCursor &cursor);
  bool readObjCPropertyBlock(llvm::BitstreamCursor &cursor);
  bool readObjCMethodBlock(llvm::BitstreamCursor &cursor);
  bool readObjCSelectorBlock(llvm::BitstreamCursor &cursor);
};

bool SideCarReader::Implementation::readControlBlock(
       llvm::BitstreamCursor &cursor) {
  return cursor.SkipBlock();
}

bool SideCarReader::Implementation::readIdentifierBlock(
       llvm::BitstreamCursor &cursor) {
  return cursor.SkipBlock();
}

bool SideCarReader::Implementation::readObjCClassBlock(
       llvm::BitstreamCursor &cursor) {
  return cursor.SkipBlock();
}

bool SideCarReader::Implementation::readObjCPropertyBlock(
       llvm::BitstreamCursor &cursor) {
  return cursor.SkipBlock();
}

bool SideCarReader::Implementation::readObjCMethodBlock(
       llvm::BitstreamCursor &cursor) {
  return cursor.SkipBlock();
}

bool SideCarReader::Implementation::readObjCSelectorBlock(
       llvm::BitstreamCursor &cursor) {
  return cursor.SkipBlock();
}

SideCarReader::SideCarReader(std::unique_ptr<llvm::MemoryBuffer> inputBuffer, 
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
  for (auto byte : SIDE_CAR_SIGNATURE) {
    if (cursor.AtEndOfStream() || cursor.Read(8) != byte) {
      failed = true;
      return;
    }
  }

  // 
  bool hasValidControlBlock = false;

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
      if (hasValidControlBlock || Impl.readControlBlock(cursor)) {
        failed = true;
        return;
      }

      hasValidControlBlock = true;
      break;

    case IDENTIFIER_BLOCK_ID:
      if (!hasValidControlBlock || Impl.readIdentifierBlock(cursor)) {
        failed = true;
        return;
      }
      break;

    case OBJC_CLASS_BLOCK_ID:
      if (!hasValidControlBlock || Impl.readObjCClassBlock(cursor)) {
        failed = true;
        return;
      }

      break;

    case OBJC_PROPERTY_BLOCK_ID:
      if (Impl.readObjCPropertyBlock(cursor)) {
        failed = true;
        return;
      }
      break;

    case OBJC_METHOD_BLOCK_ID:
      if (Impl.readObjCMethodBlock(cursor)) {
        failed = true;
        return;
      }
      break;

    case OBJC_SELECTOR_BLOCK_ID:
      if (Impl.readObjCSelectorBlock(cursor)) {
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
}

SideCarReader::~SideCarReader() {
}

std::unique_ptr<SideCarReader> 
SideCarReader::get(std::unique_ptr<llvm::MemoryBuffer> inputBuffer) {
  bool failed = false;
  std::unique_ptr<SideCarReader> 
    reader(new SideCarReader(std::move(inputBuffer), failed));
  if (failed)
    return nullptr;

  return std::move(reader);
}

