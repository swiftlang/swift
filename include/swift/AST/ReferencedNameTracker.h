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

public:
  using MemberPair = std::pair<const NominalTypeDecl *, DeclBaseName>;
private:
  llvm::DenseMap<DeclBaseName, bool> TopLevelNames;
  llvm::DenseMap<DeclBaseName, bool> DynamicLookupNames;
  llvm::DenseMap<MemberPair, bool> UsedMembers;

public:
  const decltype(TopLevelNames) &getTopLevelNames() const {
    return TopLevelNames;
  }
  const decltype(DynamicLookupNames) &getDynamicLookupNames() const {
    return DynamicLookupNames;
  }
  const decltype(UsedMembers) &getUsedMembers() const {
    return UsedMembers;
  }

  void addTopLevelName(DeclBaseName name, bool isCascadingUse) {
    TopLevelNames[name] |= isCascadingUse;
  }
  void addDynamicLookupName(DeclBaseName name, bool isCascadingUse) {
    DynamicLookupNames[name] |= isCascadingUse;
  }
  void addUsedMember(MemberPair member, bool isCascadingUse) {
    UsedMembers[member] |= isCascadingUse;
  }

};

} // end namespace swift

#endif // LLVM_SWIFT_REFERENCEDNAMETRACKER_H
