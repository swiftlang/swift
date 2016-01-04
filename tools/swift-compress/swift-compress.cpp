//===--- swift-compress.cpp - Swift compression tool  ---------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Swift compression tool.
//
//===----------------------------------------------------------------------===//

#include "swift/ABI/Compression.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Regex.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/raw_ostream.h"

#include <iostream>
#include <string>

using EncodingKind = swift::Compress::EncodingKind;

static llvm::cl::opt<bool>
DecompressMode("decompress", llvm::cl::desc("Decompress the input."));
// Decompress only using the (CBC code book compression) dictionary
// compression.
static llvm::cl::opt<bool>
CBCOnly("cbc-only", llvm::cl::desc("Only use CBC compression."));
// Decompress only using the (CBC code book compression) dictionary
// compression.
static llvm::cl::opt<bool>
HuffOnly("huff-only", llvm::cl::desc("Only use variable length encoding."));

static std::string Compress(const std::string &In, bool Compress) {
  // Only inspect Swift mangled names.
  if (In.substr(0, 2) != "_T") return In;

  auto Prefix = std::string("_T");
  auto Payload = In.substr(2, std::string::npos);

  if (CBCOnly) {
    return Prefix + (Compress ?
                     swift::Compress::EncodeCBCString(Payload) :
                     swift::Compress::DecodeCBCString(Payload));
  }

  if (HuffOnly) {
    if (Compress) {
      llvm::APInt num = EncodeStringAsNumber(Payload, EncodingKind::Variable);
      return Prefix + DecodeStringFromNumber(num, EncodingKind::Fixed);
    } else {
      llvm::APInt num = EncodeStringAsNumber(Payload, EncodingKind::Fixed);
      return Prefix + DecodeStringFromNumber(num, EncodingKind::Variable);
    }
  }

  return Prefix + (Compress ?
                   swift::Compress::CompressName(Payload) :
                   swift::Compress::DecompressName(Payload));
}
static llvm::StringRef substrBefore(llvm::StringRef whole,
                                    llvm::StringRef part) {
  return whole.slice(0, part.data() - whole.data());
}

static llvm::StringRef substrAfter(llvm::StringRef whole,
                                   llvm::StringRef part) {
  return whole.substr((part.data() - whole.data()) + part.size());
}

int main(int argc, char **argv) {
  llvm::cl::ParseCommandLineOptions(argc, argv);

  if (HuffOnly && CBCOnly) {
    llvm::errs() << "Can't select two compression mode at once." << '\n';
    return EXIT_FAILURE;
  }

  auto input = llvm::MemoryBuffer::getSTDIN();

  if (!input) {
    llvm::errs() << input.getError().message() << '\n';
    return EXIT_FAILURE;
  }

  llvm::StringRef inputContents = input.get()->getBuffer();

  llvm::Regex maybeSymbol("_T[_a-zA-Z0-9$]+");
  llvm::SmallVector<llvm::StringRef, 1> matches;
  while (maybeSymbol.match(inputContents, &matches)) {
    llvm::outs() << substrBefore(inputContents, matches.front());
    llvm::outs() << Compress(matches.front().str(), !DecompressMode);
    inputContents = substrAfter(inputContents, matches.front());
  }
  llvm::outs() << inputContents;

  return EXIT_SUCCESS;
}
