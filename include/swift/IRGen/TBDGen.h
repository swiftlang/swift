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

#include "swift/Basic/Version.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/StringSet.h"
#include <vector>

namespace llvm {
class raw_ostream;
}

namespace swift {
class FileUnit;
class ModuleDecl;
class TBDGenDescriptor;

/// Options for controlling the exact set of symbols included in the TBD
/// output.
struct TBDGenOptions {
  /// Whether this compilation has multiple IRGen instances.
  bool HasMultipleIGMs = false;

  /// Whether this compilation is producing a TBD for InstallAPI.
  bool IsInstallAPI = false;

  /// Only collect linker directive symbols.
  bool LinkerDirectivesOnly = false;

  /// Whether to include only symbols with public or package linkage.
  bool PublicOrPackageSymbolsOnly = true;

  /// Whether LLVM IR Virtual Function Elimination is enabled.
  bool VirtualFunctionElimination = false;

  /// Whether LLVM IR Witness Method Elimination is enabled.
  bool WitnessMethodElimination = false;

  /// Whether resilient protocols should be emitted fragile.
  bool FragileResilientProtocols = false;

  /// The install_name to use in the TBD file.
  std::string InstallName;

  /// The module link name (for force loading).
  std::string ModuleLinkName;

  /// The current project version to use in the generated TBD file. Defaults
  /// to empty string if not provided.
  std::string CurrentVersion;

  /// The dylib compatibility-version to use in the generated TBD file. Defaults
  /// to empty string if not provided.
  std::string CompatibilityVersion;

  /// The path to a Json file indicating the module name to install-name map
  /// used by @_originallyDefinedIn
  std::string ModuleInstallNameMapPath;

  /// For these modules, TBD gen should embed their symbols in the emitted tbd
  /// file.
  /// Typically, these modules are static linked libraries. Thus their symbols
  /// are embedded in the current dylib.
  std::vector<std::string> embedSymbolsFromModules;

  friend bool operator==(const TBDGenOptions &lhs, const TBDGenOptions &rhs) {
    return lhs.HasMultipleIGMs == rhs.HasMultipleIGMs &&
           lhs.IsInstallAPI == rhs.IsInstallAPI &&
           lhs.LinkerDirectivesOnly == rhs.LinkerDirectivesOnly &&
           lhs.PublicOrPackageSymbolsOnly == rhs.PublicOrPackageSymbolsOnly &&
           lhs.VirtualFunctionElimination == rhs.VirtualFunctionElimination &&
           lhs.WitnessMethodElimination == rhs.WitnessMethodElimination &&
           lhs.FragileResilientProtocols == rhs.FragileResilientProtocols &&
           lhs.InstallName == rhs.InstallName &&
           lhs.ModuleLinkName == rhs.ModuleLinkName &&
           lhs.CurrentVersion == rhs.CurrentVersion &&
           lhs.CompatibilityVersion == rhs.CompatibilityVersion &&
           lhs.ModuleInstallNameMapPath == rhs.ModuleInstallNameMapPath &&
           lhs.embedSymbolsFromModules == rhs.embedSymbolsFromModules;
  }

  friend bool operator!=(const TBDGenOptions &lhs, const TBDGenOptions &rhs) {
    return !(lhs == rhs);
  }

  friend llvm::hash_code hash_value(const TBDGenOptions &opts) {
    using namespace llvm;
    return hash_combine(
        opts.HasMultipleIGMs, opts.IsInstallAPI, opts.LinkerDirectivesOnly,
        opts.PublicOrPackageSymbolsOnly, opts.VirtualFunctionElimination,
        opts.WitnessMethodElimination, opts.FragileResilientProtocols,
        opts.InstallName, opts.ModuleLinkName,
        opts.CurrentVersion, opts.CompatibilityVersion,
        opts.ModuleInstallNameMapPath,
        hash_combine_range(opts.embedSymbolsFromModules.begin(),
                           opts.embedSymbolsFromModules.end()));
  }
};

std::vector<std::string> getPublicSymbols(TBDGenDescriptor desc);

void writeTBDFile(ModuleDecl *M, llvm::raw_ostream &os,
                  const TBDGenOptions &opts);

void writeAPIJSONFile(ModuleDecl *M, llvm::raw_ostream &os, bool PrettyPrint);

} // end namespace swift

#endif
