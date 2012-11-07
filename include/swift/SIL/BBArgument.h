//===--- BBArgument.h - SIL BasicBlock Argument Representation --*- C++ -*-===//
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

#ifndef SWIFT_SIL_BBARGUMENT_H
#define SWIFT_SIL_BBARGUMENT_H

#include "swift/SIL/Value.h"

namespace swift {
  class BasicBlock;

class BBArgument : public Value {
  void operator=(const BBArgument &) = delete;
  void operator delete(void *Ptr, size_t) = delete;

  BasicBlock *ParentBB;
public:
  explicit BBArgument(Type Ty, BasicBlock *ParentBB);
  
  BasicBlock *getParent() { return ParentBB; }
  const BasicBlock *getParent() const { return ParentBB; }

  static bool classof(const Value *I) {
    return I->getKind() == ValueKind::BBArgument;
  }
};

} // end swift namespace

#endif

