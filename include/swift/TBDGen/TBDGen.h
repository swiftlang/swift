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
#include "swift/Basic/Version.h"

namespace llvm {
class raw_ostream;
}

namespace swift {
class FileUnit;
class ModuleDecl;

/// \brief The current ABI version of Swift, as tapi labels it.
const uint8_t TAPI_SWIFT_ABI_VERSION = 5;

/// \brief Options for controlling the exact set of symbols included in the TBD
/// output.
struct TBDGenOptions {
  /// \brief Whether this compilation has multiple IRGen instances.
  bool HasMultipleIGMs;
  /// \brief The install-name used for the compilation.
  llvm::StringRef InstallName;
  /// \brief The module link name (for force loading).
  llvm::StringRef ModuleLinkName;
  /// \brief The current dylib version.
  version::Version CurrentVersion;
  /// \brief The dylib compatibility version.
  version::Version CompatibilityVersion;
};

void enumeratePublicSymbols(FileUnit *module, llvm::StringSet<> &symbols,
                            TBDGenOptions &opts);
void enumeratePublicSymbols(ModuleDecl *module, llvm::StringSet<> &symbols,
                            TBDGenOptions &opts);

void writeTBDFile(ModuleDecl *M, llvm::raw_ostream &os, TBDGenOptions &opts);

} // end namespace swift

#endif
