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
#include "swift/Subsystems.h"
#include "llvm/Bitcode/BitstreamWriter.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

namespace {
  class Serializer {
    SmallVector<char, 0> Buffer;
    llvm::BitstreamWriter Out;

    /// Writes the Swift module file header and BLOCKINFO block.
    void writeHeader();

    /// Top-level entry point for serializing a translation unit module.
    void writeTranslationUnit(const TranslationUnit *TU);

  public:
    Serializer() : Out(Buffer) {}

    /// Serialize a translation unit to the given stream.
    void writeToStream(const TranslationUnit *TU, raw_ostream &os);
  };
} // end anonymous namespace

void Serializer::writeHeader() {
  // Swift module file type: 'SMod'.
  Out.Emit((unsigned)'S', 8);
  Out.Emit((unsigned)'M', 8);
  Out.Emit((unsigned)'o', 8);
  Out.Emit((unsigned)'d', 8);

  Out.EnterSubblock(llvm::bitc::BLOCKINFO_BLOCK_ID, 3);
  Out.ExitBlock();
}

void Serializer::writeTranslationUnit(const TranslationUnit *TU) {
  (void)TU;
}

void Serializer::writeToStream(const TranslationUnit *TU, raw_ostream &os) {
  writeHeader();
  writeTranslationUnit(TU);

  os.write(Buffer.data(), Buffer.size());
  os.flush();
  Buffer.clear();
}

void swift::serialize(const TranslationUnit *TU, const char *outputPath) {
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
  S.writeToStream(TU, out);
}
