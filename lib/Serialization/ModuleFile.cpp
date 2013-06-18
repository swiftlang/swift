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

Identifier ModuleFile::getIdentifier(IdentifierID IID) {
  if (IID == 0)
    return Identifier();

  assert(IID <= Identifiers.size() && "invalid identifier ID");
  auto identRecord = Identifiers[IID-1];

  if (identRecord.Offset == 0)
    return identRecord.Ident;

  assert(!IdentifierData.empty() && "no identifier data in module");

  StringRef rawStrPtr = IdentifierData.substr(identRecord.Offset);
  size_t terminatorOffset = rawStrPtr.find('\0');
  assert(terminatorOffset != StringRef::npos &&
         "unterminated identifier string data");

  return ModuleContext->Ctx.getIdentifier(rawStrPtr.slice(0, terminatorOffset));
}

DeclContext *ModuleFile::getDeclContext(DeclID DID) {
  if (DID == 0)
    return ModuleContext;

  Decl *D = getDecl(DID);

  if (auto ND = dyn_cast<NominalTypeDecl>(D))
    return ND;
  if (auto ED = dyn_cast<ExtensionDecl>(D))
    return ED;
  if (auto CD = dyn_cast<ConstructorDecl>(D))
    return CD;
  if (auto DD = dyn_cast<DestructorDecl>(D))
    return DD;
  if (auto FD = dyn_cast<FuncDecl>(D))
    return FD->getBody();

  llvm_unreachable("unknown DeclContext kind");
}

Decl *ModuleFile::getDecl(DeclID DID) {
  if (DID == 0)
    return nullptr;

  assert(DID <= Decls.size() && "invalid decl ID");
  auto &declOrOffset = Decls[DID-1];

  if (declOrOffset.is<Decl *>())
    return declOrOffset.get<Decl *>();

  DeclTypeCursor.JumpToBit(declOrOffset.get<BitOffset>());
  auto entry = DeclTypeCursor.advance();

  if (entry.Kind != llvm::BitstreamEntry::Record) {
    // We don't know how to serialize decls represented by sub-blocks.
    error();
    return nullptr;
  }

  ASTContext &ctx = ModuleContext->Ctx;

  SmallVector<uint64_t, 64> scratch;
  StringRef blobData;
  unsigned recordID = DeclTypeCursor.readRecord(entry.ID, scratch, &blobData);

  switch (recordID) {
  case decls_block::TYPE_ALIAS_DECL: {
    IdentifierID nameID;
    DeclID contextID;
    TypeID underlyingTypeID;
    bool isGeneric;
    bool isImplicit;
    ArrayRef<uint64_t> inheritedIDs;

    decls_block::TypeAliasLayout::readRecord(scratch, nameID, contextID,
                                             underlyingTypeID,
                                             isGeneric, isImplicit,
                                             inheritedIDs);

    auto inherited =
      MutableArrayRef<TypeLoc>(ctx.Allocate<TypeLoc>(inheritedIDs.size()),
                               inheritedIDs.size());

    TypeLoc *nextInheritedType = inherited.data();
    for (TypeID TID : inheritedIDs) {
      auto type = getType(TID);
      new (nextInheritedType) TypeLoc(TypeLoc::withoutLoc(type));
      ++nextInheritedType;
    }

    TypeLoc underlyingType = TypeLoc::withoutLoc(getType(underlyingTypeID));
    auto alias = new (ctx) TypeAliasDecl(SourceLoc(),
                                         getIdentifier(nameID),
                                         SourceLoc(),
                                         underlyingType,
                                         getDeclContext(contextID),
                                         inherited);
    declOrOffset = alias;

    if (isImplicit)
      alias->setImplicit();
    if (isGeneric)
      alias->setGenericParameter();
    break;
  }

  case decls_block::STRUCT_DECL: {
    IdentifierID nameID;
    DeclID contextID;
    bool isImplicit;
    ArrayRef<uint64_t> inheritedIDs;

    decls_block::StructLayout::readRecord(scratch, nameID, contextID,
                                          isImplicit, inheritedIDs);

    auto inherited =
      MutableArrayRef<TypeLoc>(ctx.Allocate<TypeLoc>(inheritedIDs.size()),
                               inheritedIDs.size());

    TypeLoc *nextInheritedType = inherited.data();
    for (TypeID TID : inheritedIDs) {
      auto type = getType(TID);
      new (nextInheritedType) TypeLoc(TypeLoc::withoutLoc(type));
      ++nextInheritedType;
    }

    ArrayRef<uint64_t> memberIDs;

    auto entry = DeclTypeCursor.advance();
    if (entry.Kind != llvm::BitstreamEntry::Record)
      return {};

    StringRef blobData;
    SmallVector<uint64_t, 16> memberIDBuffer;
    unsigned recordID = DeclTypeCursor.readRecord(entry.ID, memberIDBuffer,
                                                  &blobData);
    if (recordID != decls_block::DECL_CONTEXT) {
      error();
      return nullptr;
    }
    decls_block::DeclContextLayout::readRecord(memberIDBuffer, memberIDs);

    auto theStruct = new (ctx) StructDecl(SourceLoc(), getIdentifier(nameID),
                                          SourceLoc(), inherited,
                                          /*generic params=*/nullptr,
                                          getDeclContext(contextID));
    declOrOffset = theStruct;

    if (isImplicit)
      theStruct->setImplicit();

    if (!memberIDs.empty()) {
      MutableArrayRef<Decl *> members(ctx.Allocate<Decl *>(memberIDs.size()),
                                      memberIDs.size());
      auto nextMember = members.begin();
      for (auto rawID : memberIDBuffer) {
        *nextMember = getDecl(rawID);
        ++nextMember;
      }

      theStruct->setMembers(members, SourceRange());
    }
    break;
  }

  case decls_block::CONSTRUCTOR_DECL: {
    DeclID parentID;
    bool isImplicit;
    DeclID implicitThisID;

    decls_block::ConstructorLayout::readRecord(scratch, parentID, isImplicit,
                                               implicitThisID);
    auto thisDecl = cast<VarDecl>(getDecl(implicitThisID));
    auto parent = cast<NominalTypeDecl>(getDeclContext(parentID));

    auto emptyArgs = TuplePattern::create(ctx, SourceLoc(), {}, SourceLoc());
    auto ctor = new (ctx) ConstructorDecl(ctx.getIdentifier("constructor"),
                                          SourceLoc(), emptyArgs, thisDecl,
                                          /*generic params=*/nullptr,
                                          parent);
    declOrOffset = ctor;

    // FIXME: Actually serialize the type instead of reconstructing it here.
    ctor->setType(FunctionType::get(MetaTypeType::get(thisDecl->getType(),
                                                      ctx),
                                    FunctionType::get(ctx.TheEmptyTupleType,
                                                      thisDecl->getType(),
                                                      ctx),
                                    ctx));

    if (isImplicit)
      ctor->setImplicit();
    break;
  }

  case decls_block::VAR_DECL: {
    IdentifierID nameID;
    DeclID contextID;
    bool isImplicit, isNeverLValue;
    TypeID typeID;
    DeclID getterID, setterID;
    DeclID overriddenID;

    decls_block::VarLayout::readRecord(scratch, nameID, contextID, isImplicit,
                                       isNeverLValue, typeID, getterID,
                                       setterID, overriddenID);

    auto var = new (ctx) VarDecl(SourceLoc(), getIdentifier(nameID),
                                 getType(typeID), nullptr);
    declOrOffset = var;

    // Explicitly set the DeclContext /after/ creating the VarDecl.
    // Sometimes the context has to reference this decl.
    var->setDeclContext(getDeclContext(contextID));

    var->setNeverUsedAsLValue(isNeverLValue);
    if (isImplicit)
      var->setImplicit();

    if (getterID || setterID) {
      var->setProperty(ctx, SourceLoc(),
                       cast_or_null<FuncDecl>(getDecl(getterID)),
                       cast_or_null<FuncDecl>(getDecl(setterID)),
                       SourceLoc());
    }

    var->setOverriddenDecl(cast_or_null<VarDecl>(getDecl(overriddenID)));
    break;
  }

  default:
    // We don't know how to deserialize this kind of decl.
    error();
    return nullptr;
  }

  return declOrOffset.get<Decl *>();
}

Type ModuleFile::getType(TypeID TID) {
  if (TID == 0)
    return Type();

  assert(TID <= Types.size() && "invalid decl ID");
  auto &typeOrOffset = Types[TID-1];

  if (typeOrOffset.is<Type>())
    return typeOrOffset.get<Type>();

  DeclTypeCursor.JumpToBit(typeOrOffset.get<BitOffset>());
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

  case decls_block::STRUCT_TYPE: {
    DeclID structID;
    TypeID parentID;
    decls_block::StructTypeLayout::readRecord(scratch, structID, parentID);
    typeOrOffset = StructType::get(cast<StructDecl>(getDecl(structID)),
                                   getType(parentID), ModuleContext->Ctx);
    break;
  }

  default:
    // We don't know how to deserialize this kind of type.
    error();
    return nullptr;
  }
  
  return typeOrOffset.get<Type>();
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

    case IDENTIFIER_DATA_BLOCK_ID: {
      if (!hasValidControlBlock)
        return error();

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

      if (next.Kind != llvm::BitstreamEntry::EndBlock)
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
          Decls.assign(scratch.begin(), scratch.end());
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
          assert(blobData.empty());
          RawTopLevelIDs.assign(scratch.begin(), scratch.end());
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

void ModuleFile::buildTopLevelDeclMap() {
  // FIXME: be more lazy about deserialization by encoding this some other way.
  for (DeclID ID : RawTopLevelIDs) {
    auto value = cast<ValueDecl>(getDecl(ID));
    TopLevelIDs[value->getName()] = ID;
  }

  RawTopLevelIDs.clear();
}

void ModuleFile::lookupValue(Identifier name,
                             SmallVectorImpl<ValueDecl*> &results) {
  if (!RawTopLevelIDs.empty())
    buildTopLevelDeclMap();

  if (DeclID ID = TopLevelIDs.lookup(name))
    results.push_back(cast<ValueDecl>(getDecl(ID)));
}
