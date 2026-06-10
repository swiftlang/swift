//===--- DiagnosticGroups.h - Diagnostic Groups -----------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file defines the diagnostic groups enumaration, group graph
//  and auxilary functions.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DIAGNOSTICGROUPS_H
#define SWIFT_DIAGNOSTICGROUPS_H

#include "swift/Basic/OptionSet.h"
#include "llvm/ADT/ArrayRef.h"
#include <array>
#include <string_view>
#include <unordered_map>

namespace swift {
enum class DiagnosticGroupOptions {
  /// No options.
  none = 0,

  /// The diagnostic warnings belonging to this group should be ignored by default,
  /// but will be re-enabled by various warning options (-Wwarning, -Werror).
  DefaultIgnoreWarnings = 1 << 0,

  /// The documentation file for this diagnostic group is distributed in the toolchain
  /// directly.
  ToolchainLocalDocumentation = 1 << 1,
};

enum class DiagID : uint32_t;

enum class DiagGroupID : uint32_t {
#define GROUP(Name, Options, DocsFile) Name,
#include "swift/AST/DiagnosticGroups.def"
};

constexpr const auto DiagGroupsCount = [] {
  size_t count = 0;
#define GROUP(Name, Options, DocsFile) ++count;
#include "DiagnosticGroups.def"
  return count;
}();

struct DiagGroupInfo {
  DiagGroupID id;
  std::string_view name;
  std::string_view documentationFile;
  llvm::ArrayRef<DiagGroupID> supergroups;
  llvm::ArrayRef<DiagGroupID> subgroups;
  llvm::ArrayRef<DiagID> diagnostics;
  bool defaultIgnoreWarnings : 1;
  bool toolchainLocalDocumentation : 1;

  constexpr DiagGroupInfo(DiagGroupID groupID, std::string_view name,
                          std::string_view documentationFile,
                          llvm::ArrayRef<DiagGroupID> supergroups,
                          llvm::ArrayRef<DiagGroupID> subgroups,
                          llvm::ArrayRef<DiagID> diagnostics,
                          bool defaultIgnoreWarnings,
                          bool toolchainLocalDocumentation)
  : id(groupID), name(name), documentationFile(documentationFile),
    supergroups(supergroups), subgroups(subgroups), diagnostics(diagnostics),
    defaultIgnoreWarnings(defaultIgnoreWarnings),
    toolchainLocalDocumentation(toolchainLocalDocumentation) {}

  constexpr DiagGroupInfo(DiagGroupID groupID, std::string_view name,
                          OptionSet<DiagnosticGroupOptions> options,
                          std::string_view documentationFile,
                          llvm::ArrayRef<DiagGroupID> supergroups,
                          llvm::ArrayRef<DiagGroupID> subgroups,
                          llvm::ArrayRef<DiagID> diagnostics)
  : DiagGroupInfo(groupID, name, documentationFile, supergroups,
                  subgroups, diagnostics,
                  options.contains(DiagnosticGroupOptions::DefaultIgnoreWarnings),
                  options.contains(DiagnosticGroupOptions::ToolchainLocalDocumentation)) {}

  void traverseDepthFirst(
      llvm::function_ref<void(const DiagGroupInfo &)> func) const;
};

// Add OptionSet aliases so that the '|' operator in the `DiagnosticGroups.def`
// file does the right thing.
static constexpr OptionSet<DiagnosticGroupOptions> none = DiagnosticGroupOptions::none;
static constexpr OptionSet<DiagnosticGroupOptions> DefaultIgnoreWarnings = DiagnosticGroupOptions::DefaultIgnoreWarnings;
static constexpr OptionSet<DiagnosticGroupOptions> ToolchainLocalDocumentation = DiagnosticGroupOptions::ToolchainLocalDocumentation;
extern const std::array<DiagGroupInfo, DiagGroupsCount> diagnosticGroupsInfo;
const DiagGroupInfo &getDiagGroupInfoByID(DiagGroupID id);
std::optional<DiagGroupID> getDiagGroupIDByName(std::string_view name);

} // end namespace swift

#endif /* SWIFT_DIAGNOSTICGROUPS_H */
