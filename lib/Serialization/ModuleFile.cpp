//===--- ModuleFile.cpp - Loading a serialized module -----------*- c++ -*-===//
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

#include "ModuleFile.h"
#include "ModuleFormat.h"
#include "swift/AST/AST.h"
#include "llvm/Support/MemoryBuffer.h"

using namespace swift;
using namespace swift::serialization;

static ModuleStatus
validateControlBlock(llvm::BitstreamCursor &cursor,
                     llvm::SmallVectorImpl<uint64_t> &scratch) {
  // The control block is malformed until we've at least read a major version
  // number.
  ModuleStatus result = ModuleStatus::Malformed;

  auto next = cursor.advance();
  while (next.Kind != llvm::BitstreamEntry::EndBlock) {
    if (next.Kind == llvm::BitstreamEntry::Error)
      return ModuleStatus::Malformed;

    if (next.Kind == llvm::BitstreamEntry::SubBlock) {
      // Unknown metadata sub-block, possibly for use by a future version of the
      // module format.
      if (cursor.SkipBlock())
        return ModuleStatus::Malformed;
      next = cursor.advance();
      continue;
    }

    scratch.clear();
    StringRef blobData;
    unsigned kind = cursor.readRecord(next.ID, scratch, &blobData);
    switch (kind) {
    case control_block::METADATA: {
      uint16_t versionMajor = scratch[0];
      if (versionMajor > VERSION_MAJOR)
        return ModuleStatus::FormatTooNew;
      result = ModuleStatus::Valid;
      break;
    }
    default:
      // Unknown metadata record, possibly for use by a future version of the
      // module format.
      break;
    }

    next = cursor.advance();
  }

  return result;
}

const Decl *ModuleFile::getDecl(DeclID DID) {
  if (DID == 0)
    return nullptr;

  assert(DID <= Decls.size() && "invalid decl ID");
  auto &declOrOffset = Decls[DID-1];

  if (declOrOffset.isPointer())
    return declOrOffset.getPointer();

  DeclTypeCursor.JumpToBit(declOrOffset.getInt());
  auto entry = DeclTypeCursor.advance();

  if (entry.Kind != llvm::BitstreamEntry::Record) {
    // We don't know how to serialize decls represented by sub-blocks.
    error();
    return nullptr;
  }

  SmallVector<uint64_t, 64> scratch;
  StringRef blobData;
  unsigned recordID = DeclTypeCursor.readRecord(entry.ID, scratch, &blobData);

  switch (recordID) {
  case decls_block::TYPE_ALIAS_DECL: {
    TypeID underlyingTypeID;
    bool isGeneric;
    bool isImplicit;
    ArrayRef<uint64_t> inheritedIDs;

    decls_block::TypeAliasLayout::readRecord(scratch, underlyingTypeID,
                                             isGeneric, isImplicit,
                                             inheritedIDs);
    assert(inheritedIDs.empty() && "can't handle inherited IDs yet");

    // Deserialize the name.
    // FIXME: Move this to an identifier table instead of a trailing record.
    entry = DeclTypeCursor.advance();
    if (entry.Kind != llvm::BitstreamEntry::Record) {
      error();
      return nullptr;
    }
    SmallVector<uint64_t, 0> empty;
    recordID = DeclTypeCursor.readRecord(entry.ID, empty, &blobData);
    if (recordID != decls_block::NAME_HACK) {
      error();
      return nullptr;
    }
    assert(empty.empty() && "no data in NAME_HACK records");
    assert(!blobData.empty() && "missing name in NAME_HACK record");

    ASTContext &ctx = ModuleContext->Ctx;
    TypeLoc underlyingType = TypeLoc::withoutLoc(getType(underlyingTypeID));
    declOrOffset = new (ctx) TypeAliasDecl(SourceLoc(),
                                           ctx.getIdentifier(blobData),
                                           SourceLoc(),
                                           underlyingType,
                                           ModuleContext,
                                           {});
    break;
  }
    
  default:
    // We don't know how to deserialize this kind of decl.
    error();
    return nullptr;
  }

  return declOrOffset.getPointer();
}

Type ModuleFile::getType(TypeID TID) {
  if (TID == 0)
    return Type();

  assert(TID <= Types.size() && "invalid decl ID");
  auto &typeOrOffset = Types[TID-1];

  if (typeOrOffset.isPointer())
    return typeOrOffset.getPointer();

  DeclTypeCursor.JumpToBit(typeOrOffset.getInt());
  auto entry = DeclTypeCursor.advance();

  if (entry.Kind != llvm::BitstreamEntry::Record) {
    // We don't know how to serialize types represented by sub-blocks.
    error();
    return nullptr;
  }

  SmallVector<uint64_t, 64> scratch;
  StringRef blobData;
  unsigned recordID = DeclTypeCursor.readRecord(entry.ID, scratch, &blobData);

  switch (recordID) {
  case decls_block::BUILTIN_TYPE: {
    assert(!blobData.empty() && "missing name in BUILTIN_TYPE record");

    SmallVector<ValueDecl *, 1> lookupResult;
    ASTContext &ctx = ModuleContext->Ctx;
    ctx.TheBuiltinModule->lookupValue({}, ctx.getIdentifier(blobData),
                                      NLKind::QualifiedLookup, lookupResult);
    if (lookupResult.empty()) {
      // This builtin is not supported.
      error();
      return nullptr;
    }
    assert(lookupResult.size() == 1 && "multiple types for the same name");
    typeOrOffset = cast<TypeDecl>(lookupResult.front())->getDeclaredType();
    break;
  }

  case decls_block::NAME_ALIAS_TYPE: {
    DeclID underlyingID;
    decls_block::NameAliasTypeLayout::readRecord(scratch, underlyingID);
    auto alias = dyn_cast_or_null<TypeAliasDecl>(getDecl(underlyingID));
    if (!alias) {
      error();
      return nullptr;
    }
    typeOrOffset = alias->getDeclaredType();
    break;
  }

  default:
    // We don't know how to deserialize this kind of type.
    error();
    return nullptr;
  }
  
  return typeOrOffset.getPointer();
}

ModuleFile::ModuleFile(llvm::OwningPtr<llvm::MemoryBuffer> &&input)
  : ModuleContext(nullptr),
    InputFile(std::move(input)),
    InputReader(reinterpret_cast<const uint8_t *>(InputFile->getBufferStart()),
                reinterpret_cast<const uint8_t *>(InputFile->getBufferEnd())),
    Status(ModuleStatus::Valid) {
  llvm::BitstreamCursor cursor{InputReader};

  for (unsigned char byte : SIGNATURE) {
    if (cursor.AtEndOfStream() || cursor.Read(8) != byte)
      return error();
  }

  // Future-proofing: make sure we validate the control block before we try to
  // read any other blocks.
  bool hasValidControlBlock = false;
  SmallVector<uint64_t, 64> scratch;

  auto topLevelEntry = cursor.advance();
  while (topLevelEntry.Kind == llvm::BitstreamEntry::SubBlock) {
    switch (topLevelEntry.ID) {
    case llvm::bitc::BLOCKINFO_BLOCK_ID:
      if (cursor.ReadBlockInfoBlock())
        return error();
      break;

    case CONTROL_BLOCK_ID: {
      cursor.EnterSubBlock(CONTROL_BLOCK_ID);

      ModuleStatus err = validateControlBlock(cursor, scratch);
      if (err != ModuleStatus::Valid)
        return error(err);

      hasValidControlBlock = true;
      break;
    }

    case INPUT_BLOCK_ID: {
      if (!hasValidControlBlock)
        return error();

      cursor.EnterSubBlock(INPUT_BLOCK_ID);

      auto next = cursor.advance();
      while (next.Kind == llvm::BitstreamEntry::Record) {
        scratch.clear();
        StringRef blobData;
        unsigned kind = cursor.readRecord(next.ID, scratch, &blobData);
        switch (kind) {
        case input_block::SOURCE_FILE:
          assert(scratch.empty());
          SourcePaths.push_back(blobData);
          break;
        default:
          // Unknown input kind, possibly for use by a future version of the
          // module format.
          // FIXME: Should we warn about this?
          break;
        }

        next = cursor.advance();
      }

      if (next.Kind != llvm::BitstreamEntry::EndBlock)
        return error();

      break;
    }

    case DECLS_AND_TYPES_BLOCK_ID: {
      if (!hasValidControlBlock)
        return error();

      // The decls-and-types block is lazily loaded. Save the cursor and load
      // any abbrev records at the start of the block.
      DeclTypeCursor = cursor;
      DeclTypeCursor.EnterSubBlock(DECLS_AND_TYPES_BLOCK_ID);
      if (DeclTypeCursor.advance().Kind == llvm::BitstreamEntry::Error)
        return error();

      // With the main cursor, skip over the block and continue.
      if (cursor.SkipBlock())
        return error();
      break;
    }

    case INDEX_BLOCK_ID: {
      if (!hasValidControlBlock)
        return error();

      cursor.EnterSubBlock(INDEX_BLOCK_ID);

      auto next = cursor.advanceSkippingSubblocks();
      while (next.Kind == llvm::BitstreamEntry::Record) {
        scratch.clear();
        StringRef blobData;
        unsigned kind = cursor.readRecord(next.ID, scratch, &blobData);

        switch (kind) {
        case index_block::DECL_OFFSETS:
          assert(blobData.empty());
          // FIXME: Use proper BCRecordLayout for this.
          Decls.assign(scratch.begin(), scratch.end());
          break;
        case index_block::TYPE_OFFSETS:
          assert(blobData.empty());
          // FIXME: Use proper BCRecordLayout for this.
          Types.assign(scratch.begin(), scratch.end());
          break;
        default:
          // Unknown index kind, which this version of the compiler won't use.
          break;
        }

        next = cursor.advanceSkippingSubblocks();
      }

      if (next.Kind != llvm::BitstreamEntry::EndBlock)
        return error();

      break;
    }

    case FALL_BACK_TO_TRANSLATION_UNIT_ID:
      // This is a bring-up hack and will eventually go away.
      Status = ModuleStatus::FallBackToTranslationUnit;
      break;

    default:
      // Unknown top-level block, possibly for use by a future version of the
      // module format.
      if (cursor.SkipBlock())
        return error();
      break;
    }
    
    topLevelEntry = cursor.advance(llvm::BitstreamCursor::AF_DontPopBlockAtEnd);
  }
  
  if (topLevelEntry.Kind != llvm::BitstreamEntry::EndBlock)
    return error();
}
