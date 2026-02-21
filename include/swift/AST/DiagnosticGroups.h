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

#include "llvm/ADT/ArrayRef.h"
#include <array>
#include <string_view>
#include <unordered_map>

namespace swift {
enum class DiagnosticGroupOptions {
  /// No options.
  none,

  /// The diagnostic warnings belonging to this group should be ignored by default,
  /// but will be re-enabled by various warning options (-Wwarning, -Werror).
  DefaultIgnoreWarnings,
};

enum class DiagID : uint32_t;

enum class DiagGroupID : uint32_t {
#define GROUP(Name, Option, DocsFile) Name,
#include "swift/AST/DiagnosticGroups.def"
};

constexpr const auto DiagGroupsCount = [] {
  size_t count = 0;
#define GROUP(Name, Option, DocsFile) ++count;
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

  constexpr DiagGroupInfo(DiagGroupID groupID, std::string_view name,
                          std::string_view documentationFile,
                          llvm::ArrayRef<DiagGroupID> supergroups,
                          llvm::ArrayRef<DiagGroupID> subgroups,
                          llvm::ArrayRef<DiagID> diagnostics,
                          bool defaultIgnoreWarnings)
  : id(groupID), name(name), documentationFile(documentationFile),
    supergroups(supergroups), subgroups(subgroups), diagnostics(diagnostics),
    defaultIgnoreWarnings(defaultIgnoreWarnings) {}

  constexpr DiagGroupInfo(DiagGroupID groupID, std::string_view name,
                          std::string_view documentationFile,
                          DiagnosticGroupOptions opts,
                          llvm::ArrayRef<DiagGroupID> supergroups,
                          llvm::ArrayRef<DiagGroupID> subgroups,
                          llvm::ArrayRef<DiagID> diagnostics)
  : DiagGroupInfo(groupID, name, documentationFile, supergroups,
                  subgroups, diagnostics,
                  opts == DiagnosticGroupOptions::DefaultIgnoreWarnings) {}

  void traverseDepthFirst(
      llvm::function_ref<void(const DiagGroupInfo &)> func) const;
};

extern const std::array<DiagGroupInfo, DiagGroupsCount> diagnosticGroupsInfo;
const DiagGroupInfo &getDiagGroupInfoByID(DiagGroupID id);
std::optional<DiagGroupID> getDiagGroupIDByName(std::string_view name);

} // end namespace swift

#endif /* SWIFT_DIAGNOSTICGROUPS_H */
