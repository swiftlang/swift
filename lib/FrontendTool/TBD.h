//===--- Tbd.h -- generates and validates TBD files -----------------------===//
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

namespace llvm {
class StringRef;
class Module;
}
namespace swift {
class ModuleDecl;
class FileUnit;

bool writeTBD(ModuleDecl *M, llvm::StringRef OutputFilename);
bool validateTBD(ModuleDecl *M, llvm::Module &IRModule);
bool validateTBD(FileUnit *M, llvm::Module &IRModule);
}

#endif
