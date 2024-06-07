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
  /// A back pointer to the function that this SILUndef is uniqued by.
  SILFunction *parent;

  SILUndef(SILFunction *parent, SILType type);

public:
  void operator=(const SILArgument &) = delete;
  void operator delete(void *, size_t) = delete;

  /// Return a SILUndef with the same type as the passed in value.
  static SILUndef *get(SILValue value) {
    return SILUndef::get(value->getFunction(), value->getType());
  }

  static SILUndef *get(SILFunction *f, SILType ty);
  static SILUndef *get(SILFunction &f, SILType ty) {
    return SILUndef::get(&f, ty);
  }

  /// This is an API only used by SILSSAUpdater... please do not use it anywhere
  /// else.
  template <class OwnerTy>
  static SILUndef *getSentinelValue(SILFunction *fn, OwnerTy owner,
                                    SILType type) {
    // Ownership kind isn't used here, the value just needs to have a unique
    // address.
    return new (*owner) SILUndef(fn, type);
  }

  SILFunction *getParent() const { return parent; }
  ValueOwnershipKind getOwnershipKind() const { return OwnershipKind::None; }

  static bool classof(const SILArgument *) = delete;
  static bool classof(const SILInstruction *) = delete;
  static bool classof(SILNodePointer node) {
    return node->getKind() == SILNodeKind::SILUndef;
  }
};

} // end swift namespace

#endif

