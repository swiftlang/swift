//===--- Replacement.cpp ----------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Migrator/Replacement.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace swift::migrator;

bool Replacement::emitRemap(StringRef OutFilename,
                            llvm::ArrayRef<Replacement> Replacements) {
  if (OutFilename == "-") {
    printReplacements(llvm::outs(), llvm::makeArrayRef(Replacements));
    return false;
  } else {
    std::error_code Error;
    llvm::raw_fd_ostream FileOS(OutFilename, Error, llvm::sys::fs::F_Text);
    if (FileOS.has_error()) {
      return true;
    }

    printReplacements(FileOS, Replacements);
    FileOS.flush();
    return FileOS.has_error();
  }
}

void Replacement::printReplacements(llvm::raw_ostream &OS,
                                    llvm::ArrayRef<Replacement> Replacements) {
  OS << "[\n";
  for (auto R = Replacements.begin(); R != Replacements.end(); ++R) {
    if (R != Replacements.begin()) {
      OS << ",\n";
    }
    R->printJSON(OS);
  }
  OS << "\n]";
}
