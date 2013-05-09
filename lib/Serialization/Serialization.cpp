//===--- Serialization.cpp - Read and write Swift modules -----------------===//
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

#include "swift/Serialization/Serialization.h"
#include "swift/AST/AST.h"
#include "swift/AST/Diagnostics.h"
#include "swift/Serialization/BCRecordLayout.h"
#include "swift/Subsystems.h"
#include "llvm/Bitcode/BitstreamWriter.h"
#include "llvm/Config/Config.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace swift::serialization;

namespace {
  typedef ArrayRef<unsigned> FileBufferIDs;

  class Serializer {
    SmallVector<char, 0> Buffer;
    llvm::BitstreamWriter Out;

    /// A reusable buffer for emitting records.
    SmallVector<uint64_t, 64> ScratchRecord;

    /// Writes the BLOCKINFO block.
    void writeBlockInfoBlock();

    /// Writes the Swift module file header, BLOCKINFO block, and
    /// non-TU-specific metadata.
    void writeHeader();

    /// Writes the input file paths.
    void writeInputFiles(llvm::SourceMgr &sourceMgr, FileBufferIDs inputFiles);

    /// Top-level entry point for serializing a translation unit module.
    void writeTranslationUnit(const TranslationUnit *TU);

  public:
    Serializer() : Out(Buffer) {}

    /// Serialize a translation unit to the given stream.
    void writeToStream(raw_ostream &os, const TranslationUnit *TU,
                       FileBufferIDs inputFiles);
  };
} // end anonymous namespace


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

void Serializer::writeBlockInfoBlock() {
  BCBlockRAII restoreBlock(Out, llvm::bitc::BLOCKINFO_BLOCK_ID, 2);

  SmallVector<unsigned char, 64> nameBuffer;
#define BLOCK(X) emitBlockID(Out, X ## _ID, #X, nameBuffer)
#define RECORD(X) emitRecordID(Out, X, #X, nameBuffer)

  BLOCK(CONTROL_BLOCK);
  RECORD(METADATA);

  BLOCK(INPUT_BLOCK);
  RECORD(SOURCE_FILE);

#undef BLOCK
#undef RECORD
}

void Serializer::writeHeader() {
  // Swift module file type: 'SMod'.
  Out.Emit((unsigned)'S', 8);
  Out.Emit((unsigned)'M', 8);
  Out.Emit((unsigned)'o', 8);
  Out.Emit((unsigned)'d', 8);

  writeBlockInfoBlock();

  {
    BCBlockRAII restoreBlock(Out, CONTROL_BLOCK_ID, 3);

    BCRecordLayout<
      BCLiteral<METADATA>, // ID
      BCFixed<16>, // Module format major version
      BCFixed<16>, // Module format minor version
      BCBlob // misc. version information
    > Metadata(Out);

    // FIXME: put a real version in here.
#ifdef LLVM_VERSION_INFO
# define EXTRA_VERSION_STRING PACKAGE_STRING LLVM_VERSION_INFO
#else
# define EXTRA_VERSION_STRING PACKAGE_STRING
#endif
    Metadata.emit(ScratchRecord,
                  METADATA, VERSION_MAJOR, VERSION_MINOR, EXTRA_VERSION_STRING);
#undef EXTRA_VERSION_STRING
  }
}

void Serializer::writeInputFiles(llvm::SourceMgr &sourceMgr,
                                 FileBufferIDs inputFiles) {
  BCBlockRAII restoreBlock(Out, INPUT_BLOCK_ID, 3);

  BCRecordLayout<
    BCLiteral<SOURCE_FILE>, // ID
    BCBlob // path
  > SourceFile(Out);

  for (auto bufferID : inputFiles) {
    // FIXME: We could really use a real FileManager here.
    auto buffer = sourceMgr.getMemoryBuffer(bufferID);
    llvm::SmallString<128> path(buffer->getBufferIdentifier());

    llvm::error_code err;
    err = llvm::sys::fs::make_absolute(path);
    if (err)
      continue;
    
    SourceFile.emit(ScratchRecord, SOURCE_FILE, path);
  }
}

void Serializer::writeTranslationUnit(const TranslationUnit *TU) {
  (void)TU;
}

void Serializer::writeToStream(raw_ostream &os, const TranslationUnit *TU,
                               FileBufferIDs inputFiles){
  writeHeader();
  writeInputFiles(TU->Ctx.SourceMgr, inputFiles);
  writeTranslationUnit(TU);

  os.write(Buffer.data(), Buffer.size());
  os.flush();
  Buffer.clear();
}

void swift::serialize(const TranslationUnit *TU, const char *outputPath,
                      FileBufferIDs inputFiles) {
  std::string errorInfo;
  llvm::raw_fd_ostream out(outputPath, errorInfo,
                           llvm::raw_fd_ostream::F_Binary);

  if (out.has_error() || !errorInfo.empty()) {
    TU->Ctx.Diags.diagnose(SourceLoc(), diag::error_opening_output, outputPath,
                           errorInfo);
    out.clear_error();
    return;
  }

  Serializer S;
  S.writeToStream(out, TU, inputFiles);
}
