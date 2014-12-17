//===--- ReferencedNameTracker.h - Records looked-up names ------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFERENCEDNAMETRACKER_H
#define SWIFT_REFERENCEDNAMETRACKER_H

#include "swift/AST/Identifier.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {

class NominalTypeDecl;

class ReferencedNameTracker {
  llvm::DenseMap<Identifier, bool> TopLevelNames;
  llvm::DenseMap<const NominalTypeDecl *, bool> UsedNominals;
public:
  void addTopLevelName(Identifier name, bool isCascadingUse) {
    TopLevelNames[name] |= isCascadingUse;
  }

  const llvm::DenseMap<Identifier, bool> &getTopLevelNames() const {
    return TopLevelNames;
  }

  void addUsedNominal(const NominalTypeDecl *nominal, bool isCascadingUse) {
    UsedNominals[nominal] |= isCascadingUse;
  }

  const llvm::DenseMap<const NominalTypeDecl *, bool> &getUsedNominals() const {
    return UsedNominals;
  }
};

} // end namespace swift

#endif // LLVM_SWIFT_REFERENCEDNAMETRACKER_H
