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

/// The current ABI version of Swift, as tapi labels it.
const uint8_t TAPI_SWIFT_ABI_VERSION = 5;

/// Options for controlling the exact set of symbols included in the TBD
/// output.
struct TBDGenOptions {
  /// Whether this compilation has multiple IRGen instances.
  bool HasMultipleIGMs;

  /// The install_name to use in the TBD file.
  std::string InstallName;

  /// The module link name (for force loading).
  std::string ModuleLinkName;

  /// The current project version to use in the generated TBD file. Defaults
  /// to 1, which matches the default if the DYLIB_CURRENT_VERSION build setting
  /// is not set.
  version::Version CurrentVersion = {1, 0, 0};

  /// The dylib compatibility-version to use in the generated TBD file. Defaults
  /// to 1, which matches the default if the DYLIB_COMPATIBILITY_VERSION build
  /// setting is not set.
  version::Version CompatibilityVersion = {1, 0, 0};
};

void enumeratePublicSymbols(FileUnit *module, llvm::StringSet<> &symbols,
                            const TBDGenOptions &opts);
void enumeratePublicSymbols(ModuleDecl *module, llvm::StringSet<> &symbols,
                            const TBDGenOptions &opts);

void writeTBDFile(ModuleDecl *M, llvm::raw_ostream &os,
                  const TBDGenOptions &opts);

} // end namespace swift

#endif
