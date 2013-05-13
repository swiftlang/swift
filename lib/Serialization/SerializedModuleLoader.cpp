//===--- SerializedModuleLoader.cpp - Import Swift modules ------*- c++ -*-===//
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

#include "swift/Serialization/SerializedModuleLoader.h"
#include "ModuleFormat.h"
#include "swift/Subsystems.h"
#include "swift/AST/AST.h"
#include "swift/AST/Component.h"
#include "swift/AST/Diagnostics.h"
#include "llvm/ADT/OwningPtr.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Bitcode/BitstreamReader.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/system_error.h"

using namespace swift;

namespace {
class SerializedModule : public LoadedModule {
public:
  SerializedModule(ASTContext &ctx, SerializedModuleLoader &owner,
                   Identifier name, Component *comp)
    : LoadedModule(DeclContextKind::SerializedModule, name, comp, ctx, owner) {}

  static bool classof(const DeclContext *DC) {
    return DC->getContextKind() == DeclContextKind::SerializedModule;
  }
};

typedef std::pair<Identifier, SourceLoc> AccessPathElem;
}

// FIXME: Copied from SourceLoader. Not bothering to fix until we decide that
// the source loader search path should be the same as the module loader search
// path.
static llvm::error_code findModule(ASTContext &ctx, AccessPathElem moduleID,
                                   llvm::OwningPtr<llvm::MemoryBuffer> &buffer){
  llvm::SmallString<64> moduleFilename(moduleID.first.str());
  moduleFilename += ".sm";

  llvm::SmallString<128> inputFilename;

  // First, search in the directory corresponding to the import location.
  // FIXME: This screams for a proper FileManager abstraction.
  llvm::SMLoc rawLoc = moduleID.second.Value;
  int currentBufferID = ctx.SourceMgr.FindBufferContainingLoc(rawLoc);
  if (currentBufferID >= 0) {
    const llvm::MemoryBuffer *importingBuffer
      = ctx.SourceMgr.getBufferInfo(currentBufferID).Buffer;
    StringRef currentDirectory
      = llvm::sys::path::parent_path(importingBuffer->getBufferIdentifier());
    if (!currentDirectory.empty()) {
      inputFilename = currentDirectory;
      llvm::sys::path::append(inputFilename, moduleFilename.str());
      llvm::error_code err = llvm::MemoryBuffer::getFile(inputFilename, buffer);
      if (!err)
        return err;
    }
  }

  // Second, search in the current directory.
  llvm::error_code err = llvm::MemoryBuffer::getFile(moduleFilename, buffer);
  if (!err)
    return err;

  // If we fail, search each import search path.
  for (auto Path : ctx.ImportSearchPaths) {
    inputFilename = Path;
    llvm::sys::path::append(inputFilename, moduleFilename.str());
    err = llvm::MemoryBuffer::getFile(inputFilename, buffer);
    if (!err)
      return err;
  }

  return err;
}

static Module *makeTU(ASTContext &ctx, AccessPathElem moduleID,
                      ArrayRef<StringRef> inputPaths) {
  Component *comp = new (ctx.Allocate<Component>(1)) Component();
  TranslationUnit *TU = new (ctx) TranslationUnit(moduleID.first, comp, ctx,
                                                  /*IsMainModule=*/false,
                                                  /*IsReplModule=*/false);

  ctx.LoadedModules[moduleID.first.str()] = TU;

  std::vector<unsigned> BufferIDs;
  for (auto &path : inputPaths) {
    // Open the input file.
    llvm::OwningPtr<llvm::MemoryBuffer> InputFile;
    if (llvm::MemoryBuffer::getFileOrSTDIN(path, InputFile))
      return nullptr;

    // Transfer ownership of the MemoryBuffer to the SourceMgr.
    // FIXME: include location
    llvm::SMLoc rawLoc = moduleID.second.Value;
    BufferIDs.push_back(ctx.SourceMgr.AddNewSourceBuffer(InputFile.take(),
                                                         rawLoc));
  }

  for (auto &BufferID : BufferIDs) {
    unsigned BufferOffset = 0;
    const llvm::MemoryBuffer *Buffer =
      ctx.SourceMgr.getMemoryBuffer(BufferID);
    do {
      parseIntoTranslationUnit(TU, BufferID, &BufferOffset, 0, /*SIL=*/nullptr);
    } while (BufferOffset != Buffer->getBufferSize());
  }

  performNameBinding(TU);
  performTypeChecking(TU);

  return TU;
}


Module *SerializedModuleLoader::error(AccessPathElem moduleID) {
  Ctx.Diags.diagnose(moduleID.second, diag::serialization_malformed_module);

  // Return a dummy module to avoid future errors.
  auto comp = new (Ctx.Allocate<Component>(1)) Component();
  auto module = new (Ctx) SerializedModule(Ctx, *this, moduleID.first, comp);

  Ctx.LoadedModules[moduleID.first.str()] = module;
  return module;
}

Module *SerializedModuleLoader::loadModule(SourceLoc importLoc,
                                           Module::AccessPathTy path) {
  using namespace serialization;

  // FIXME: Swift submodules?
  if (path.size() > 1)
    return nullptr;

  auto moduleID = path[0];

  llvm::OwningPtr<llvm::MemoryBuffer> inputFile;
  if (llvm::error_code err = findModule(Ctx, moduleID, inputFile)) {
    if (err.value() != llvm::errc::no_such_file_or_directory) {
      Ctx.Diags.diagnose(moduleID.second, diag::sema_opening_import,
                         moduleID.first.str(), err.message());
    }

    return nullptr;
  }

  assert(inputFile);
  auto start =
    reinterpret_cast<const unsigned char *>(inputFile->getBufferStart());
  auto end =
    reinterpret_cast<const unsigned char *>(inputFile->getBufferEnd());
  llvm::BitstreamReader reader(start, end);
  llvm::BitstreamCursor cursor(reader);

  for (unsigned char byte : SIGNATURE) {
    if (cursor.AtEndOfStream() || cursor.Read(8) != byte)
      return error(moduleID);
  }

  // Future-proofing: make sure we validate the control block before we try to
  // read any other blocks.
  bool hasValidControlBlock = false;

  // FIXME: Hack to get the interesting case up and running.
  SmallVector<uint64_t, 64> scratch;
  SmallVector<StringRef, 4> inputFilePaths;

  auto topLevelEntry = cursor.advance();
  while (topLevelEntry.Kind == llvm::BitstreamEntry::SubBlock) {
    switch (topLevelEntry.ID) {
    case llvm::bitc::BLOCKINFO_BLOCK_ID:
      if (cursor.ReadBlockInfoBlock())
        return error(moduleID);
      break;

    case CONTROL_BLOCK_ID:
      // FIXME: Actually validate the control block.
      if (cursor.SkipBlock())
        return error(moduleID);
      hasValidControlBlock = true;
      break;

    case INPUT_BLOCK_ID: {
      cursor.EnterSubBlock(INPUT_BLOCK_ID);
      // FIXME: Hack to get the interesting case up and running.
      auto next = cursor.advance();
      while (next.Kind == llvm::BitstreamEntry::Record) {
        scratch.clear();
        StringRef blobData;
        unsigned kind = cursor.readRecord(next.ID, scratch, &blobData);
        switch (kind) {
        case input_block::SOURCE_FILE:
          assert(scratch.empty());
          inputFilePaths.push_back(blobData);
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
        return error(moduleID);

      break;
    }

    default:
      // Unknown top-level block, possibly for use by a future version of the
      // module format.
      if (cursor.SkipBlock())
        return error(moduleID);
      break;
    }

    topLevelEntry = cursor.advance(llvm::BitstreamCursor::AF_DontPopBlockAtEnd);
  }

  if (topLevelEntry.Kind != llvm::BitstreamEntry::EndBlock)
    return error(moduleID);

  // FIXME: At least /pretend/ to be a LoadedModule.
  auto module = makeTU(Ctx, moduleID, inputFilePaths);
  return module;
}
