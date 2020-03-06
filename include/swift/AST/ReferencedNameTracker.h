//===--- ReferencedNameTracker.h - Records looked-up names ------*- C++ -*-===//
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

#ifndef SWIFT_REFERENCEDNAMETRACKER_H
#define SWIFT_REFERENCEDNAMETRACKER_H

#include "swift/AST/Identifier.h"
#include "swift/Basic/ReferenceDependencyKeys.h"
#include "llvm/ADT/DenseMap.h"

#include <unordered_set>

namespace swift {

class NominalTypeDecl;
class DependencyTracker;

class ReferencedNameTracker {
public:
  using EnumerateUsedDecl = function_ref<void(
      fine_grained_dependencies::NodeKind kind, StringRef context,
      StringRef name, bool isCascadingUse)>;

private:
#define TRACKED_SET(KIND, NAME)                                                \
private:                                                                       \
  llvm::DenseMap<KIND, bool> NAME##s;                                          \
                                                                               \
public:                                                                        \
  void add##NAME(KIND new##NAME, bool isCascadingUse) {                        \
    NAME##s[new##NAME] |= isCascadingUse;                                      \
  }                                                                            \
  /* make private once ReferenceDependencies.cpp is gone */                    \
  const decltype(NAME##s) &get##NAME##s() const { return NAME##s; }

  TRACKED_SET(DeclBaseName, TopLevelName)
  TRACKED_SET(DeclBaseName, DynamicLookupName)

  using MemberPair = std::pair<const NominalTypeDecl *, DeclBaseName>;
  TRACKED_SET(MemberPair, UsedMember)

#undef TRACKED_SET
public:
  // Pushing the DependencyTracker through unifies external dependency
  // enumeration.
  void enumerateAllUses(bool includeIntrafileDeps,
                        const DependencyTracker &depTracker,
                        EnumerateUsedDecl enumerateUsedDecl) const;

private:
  template <fine_grained_dependencies::NodeKind kind>
  void enumerateSimpleUses(llvm::DenseMap<DeclBaseName, bool> cascadesByName,
                           EnumerateUsedDecl enumerateUsedDecl) const;

  void enumerateExternalUses(const DependencyTracker &,
                             EnumerateUsedDecl enumerateUsedDecl) const;

  void enumerateCompoundUses(bool includeIntrafileDeps,
                             EnumerateUsedDecl enumerateUsedDecl) const;

  std::unordered_set<std::string>
  computeHoldersOfCascadingMembers(bool includeIntrafileDeps) const;

  void enumerateNominalUses(
      bool includeIntrafileDeps,
      const std::unordered_set<std::string> &&holdersOfCascadingMembers,
      EnumerateUsedDecl enumerateUsedDecl) const;

  void enumerateMemberUses(EnumerateUsedDecl enumerateUsedDecl) const;
};

} // end namespace swift

#endif // LLVM_SWIFT_REFERENCEDNAMETRACKER_H
