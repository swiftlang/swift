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
  class SILModule;

class SILUndef : public ValueBase {
  void operator=(const SILArgument &) = delete;
  void operator delete(void *Ptr, size_t) SWIFT_DELETE_OPERATOR_DELETED

  SILUndef(SILType Ty) : ValueBase(ValueKind::SILUndef, Ty) {}
public:

  static SILUndef *get(SILType Ty, SILModule *M);
  static SILUndef *get(SILType Ty, SILModule &M) { return get(Ty, &M); }

  template<class OwnerTy>
  static SILUndef *getSentinelValue(SILType Ty, OwnerTy Owner) { return new (*Owner) SILUndef(Ty); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::SILUndef;
  }
};

} // end swift namespace

#endif

