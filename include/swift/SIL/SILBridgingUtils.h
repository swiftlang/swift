//===--- SILBridgingUtils.h - utilities for swift bridging ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILBRIDGINGUTILS_H
#define SWIFT_SIL_SILBRIDGINGUTILS_H

#include "swift/SIL/SILBridging.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILGlobalVariable.h"
#include "swift/SIL/SILVTable.h"
#include "swift/SIL/SILWitnessTable.h"
#include "swift/SIL/SILDefaultWitnessTable.h"
#include "llvm/ADT/StringRef.h"

#include <string>

namespace swift {

inline SILNode *castToSILNode(BridgedNode node) {
  return static_cast<SILNode *>(node.obj);
}

template <class I = SILInstruction> I *castToInst(BridgedInstruction inst) {
  return cast<I>(static_cast<SILNode *>(inst.obj)->castToInstruction());
}

template <class I = SILInstruction> I *castToInst(OptionalBridgedInstruction inst) {
  if (!inst.obj)
    return nullptr;
  return cast<I>(static_cast<SILNode *>(inst.obj)->castToInstruction());
}

template <class A = SILArgument> A *castToArgument(BridgedArgument argument) {
  return cast<A>(static_cast<SILArgument *>(argument.obj));
}

ArrayRef<SILValue> getSILValues(BridgedValueArray values,
                                SmallVectorImpl<SILValue> &storage);

} // namespace swift

#endif

