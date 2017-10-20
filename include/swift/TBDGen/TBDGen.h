//===--- TBDGen.h - Public interface to TBDGen ------------------*- C++ -*-===//
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
#ifndef SWIFT_IRGEN_TBDGEN_H
#define SWIFT_IRGEN_TBDGEN_H

#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSet.h"

namespace llvm {
class raw_ostream;
}

namespace swift {
class FileUnit;
class ModuleDecl;

void enumeratePublicSymbols(FileUnit *module, llvm::StringSet<> &symbols,
                            bool hasMultipleIRGenThreads);
void enumeratePublicSymbols(ModuleDecl *module, llvm::StringSet<> &symbols,
                            bool hasMultipleIRGenThreads);

void writeTBDFile(ModuleDecl *M, llvm::raw_ostream &os,
                  bool hasMultipleIRGenThreads,
                  llvm::StringRef installName);

} // end namespace swift

#endif
