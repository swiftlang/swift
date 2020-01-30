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
#include "swift/Basic/NullablePtr.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {

class Decl;
class NominalTypeDecl;

class ReferencedNameTracker {

  /// Internally, use pairs and tuples for ease of using \c DenseMap
  /// The second element, if not null, points to the \c Decl that uses the first
  /// element.
  using NameAndUse = std::pair<DeclBaseName, const Decl *>;

  /// Also for members, contains the holder, member name, and (if not null) a
  /// pointer to the use.
  using HolderMemberAndUse =
      std::tuple<const NominalTypeDecl *, DeclBaseName, const Decl *>;

  llvm::DenseMap<NameAndUse, bool> TopLevelNames;
  llvm::DenseMap<NameAndUse, bool> DynamicLookupNames;
  llvm::DenseMap<HolderMemberAndUse, bool> UsedMembers;

public:
  /// Record a top level use.
  /// \param name The name being used
  /// \param isCascadingUse Whether the dependency from use to def transitively affects
  /// uses of the use.
  /// \param use The declaration using the name, if known.
  void addTopLevelName(DeclBaseName name, bool isCascadingUse,
                       NullablePtr<const Decl> use = nullptr) {
    TopLevelNames[NameAndUse{name, use.getPtrOrNull()}] |= isCascadingUse;
  }

  /// Record a dynamically-looked-up use.
  /// \param name The name being used
  /// \param isCascadingUse Whether the dependency from use to def transitively affects
  /// uses of the use.
  /// \param use The declaration using the name, if known.
  void addDynamicLookupName(DeclBaseName name, bool isCascadingUse,
                            NullablePtr<const Decl> use = nullptr) {
    DynamicLookupNames[NameAndUse{name, use.getPtrOrNull()}] |= isCascadingUse;
  }

  /// Record a use of a member.
  /// \param holder The holder of the member being used.
  /// \param member The name of the member being used.
  /// \param isCascadingUse Whether the dependency from use to def transitively affects
  /// uses of the use.
  /// \param use The declaration using the name, if known.
  void addUsedMember(const NominalTypeDecl *holder, DeclBaseName member,
                     bool isCascadingUse,
                     NullablePtr<const Decl> use = nullptr) {
    UsedMembers[HolderMemberAndUse{holder, member, use.getPtrOrNull()}] |=
        isCascadingUse;
  }

  /// Invoke  \c fn for each depended-upon top level name, passing in the name,
  /// whether this use is cascading, and if known, the declaration using the
  /// name.
  void forEachTopLevelName(
      function_ref<void(const DeclBaseName &, bool, NullablePtr<const Decl>)>
          fn) const {
    for (auto &entry : TopLevelNames)
      fn(entry.first.first, entry.second,
         NullablePtr<const Decl>(entry.first.second));
  }

  /// Invoke  \c fn for each depended-upon dynamically looked-up name, passing
  /// in the name, whether this use is cascading, and if known, the declaration
  /// using the name.
  void forEachDynamicLookupName(
      function_ref<void(const DeclBaseName &, bool, NullablePtr<const Decl>)>
          fn) const {
    for (auto &entry : DynamicLookupNames)
      fn(entry.first.first, entry.second,
         NullablePtr<const Decl>(entry.first.second));
  }

  /// Invoke  \c fn for each depended-upon member, passing in the holder, the
  /// member name, whether this use is cascading, and if known, the declaration
  /// using the name.
  void forEachUsedMember(
      function_ref<void(const NominalTypeDecl *, const DeclBaseName &, bool,
                        NullablePtr<const Decl>)>
          fn) const {
    for (auto &entry : UsedMembers)
      fn(std::get<0>(entry.first), std::get<1>(entry.first), entry.second,
         NullablePtr<const Decl>(std::get<2>(entry.first)));
  }

  size_t getNumReferencedTopLevelNames() const { return TopLevelNames.size(); }
  size_t getNumReferencedDynamicNames() const {
    return DynamicLookupNames.size();
  }
  size_t getNumReferencedMemberNames() const { return UsedMembers.size(); }
};

} // end namespace swift

#endif // LLVM_SWIFT_REFERENCEDNAMETRACKER_H
