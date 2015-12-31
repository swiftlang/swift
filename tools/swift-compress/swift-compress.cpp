//===-- swift-compress.cpp - Swift compressiom tool  ----------------------===//
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

static llvm::cl::opt<bool>
DecompressMode("decompress",
llvm::cl::desc("Decompress the incoming strings."));

static std::string Compress(const std::string &In, bool Compress) {
    // Only inspect Swift mangled names.
    if (In.substr(0, 2) != "_T") return In;

    return std::string("_T") + (Compress ?
     swift::Compress::CompressName(In.substr(2, std::string::npos)) :
     swift::Compress::DecompressName(In.substr(2, std::string::npos)));
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
