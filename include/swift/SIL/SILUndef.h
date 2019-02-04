//===--- SILUndef.h - SIL Undef Value Representation ------------*- C++ -*-===//
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

#ifndef SWIFT_SIL_UNDEF_H
#define SWIFT_SIL_UNDEF_H

#include "swift/Basic/Compiler.h"
#include "swift/SIL/SILValue.h"

namespace swift {

class SILArgument;
class SILInstruction;
class SILModule;

class SILUndef : public ValueBase {
  ValueOwnershipKind ownershipKind;

  SILUndef(SILType type, SILModule &m);

public:
  void operator=(const SILArgument &) = delete;
  void operator delete(void *, size_t) SWIFT_DELETE_OPERATOR_DELETED;

  static SILUndef *get(SILType ty, SILModule &m);
  static SILUndef *get(SILType ty, SILModule *m) { return get(ty, *m); }

  template <class OwnerTy>
  static SILUndef *getSentinelValue(SILType type, SILModule &m, OwnerTy owner) {
    return new (*owner) SILUndef(type, m);
  }

  ValueOwnershipKind getOwnershipKind() const { return ownershipKind; }

  static bool classof(const SILArgument *) = delete;
  static bool classof(const SILInstruction *) = delete;
  static bool classof(const SILNode *node) {
    return node->getKind() == SILNodeKind::SILUndef;
  }
};

} // end swift namespace

#endif

