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
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/SmallPtrSet.h"

namespace swift {

class ReferencedNameTracker {
  SmallPtrSet<Identifier, 1> TopLevelNames;
public:
  void addTopLevelName(Identifier name) {
    TopLevelNames.insert(name);
  }

  const SmallPtrSetImpl<Identifier> &getTopLevelNames() const {
    return TopLevelNames;
  }
};

} // end namespace swift

#endif // LLVM_SWIFT_REFERENCEDNAMETRACKER_H
