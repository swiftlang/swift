//===--- TBD.h -- generates and validates TBD files -------------*- C++ -*-===//
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

#ifndef SWIFT_FRONTENDTOOL_TBD_H
#define SWIFT_FRONTENDTOOL_TBD_H

#include "swift/Frontend/FrontendOptions.h"

namespace llvm {
class StringRef;
class Module;
}
namespace swift {
class ModuleDecl;
class FileUnit;
class FrontendOptions;

bool writeTBD(ModuleDecl *M, bool hasMultipleIGMs, StringRef OutputFilename,
              llvm::StringRef installName);
bool inputFileKindCanHaveTBDValidated(InputFileKind kind);
bool validateTBD(ModuleDecl *M, llvm::Module &IRModule, bool hasMultipleIGMs,
                 bool diagnoseExtraSymbolsInTBD);
bool validateTBD(FileUnit *M, llvm::Module &IRModule, bool hasMultipleIGMs,
                 bool diagnoseExtraSymbolsInTBD);
}

#endif
