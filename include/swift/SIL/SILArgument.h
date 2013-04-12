//===--- SILArgument.h - SIL BasicBlock Argument Representation -*- C++ -*-===//
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

#ifndef SWIFT_SIL_SILARGUMENT_H
#define SWIFT_SIL_SILARGUMENT_H

#include "swift/SIL/SILValue.h"

namespace swift {
  class SILBasicBlock;

class SILArgument : public ValueBase {
  void operator=(const SILArgument &) = delete;
  void operator delete(void *Ptr, size_t) = delete;

  SILBasicBlock *ParentBB;
public:
  explicit SILArgument(SILType Ty, SILBasicBlock *ParentBB);

  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

  SILBasicBlock *getParent() { return ParentBB; }
  const SILBasicBlock *getParent() const { return ParentBB; }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::SILArgument;
  }
};

} // end swift namespace

#endif

