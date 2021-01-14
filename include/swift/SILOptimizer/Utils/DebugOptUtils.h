//===--- DebugOptUtils.h --------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// Debug Info related utilities that rely on code in SILOptimizer/ and thus can
/// not be in include/swift/SIL/DebugUtils.h.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_DEBUGOPTUTILS_H
#define SWIFT_SILOPTIMIZER_DEBUGOPTUTILS_H

#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"

namespace swift {

/// Deletes all of the debug instructions that use \p value.
inline void deleteAllDebugUses(SILValue value, InstModCallbacks &callbacks) {
  for (auto ui = value->use_begin(), ue = value->use_end(); ui != ue;) {
    auto *inst = ui->getUser();
    ++ui;
    if (inst->isDebugInstruction()) {
      callbacks.deleteInst(inst);
    }
  }
}

/// Deletes all of the debug uses of any result of \p inst.
inline void deleteAllDebugUses(SILInstruction *inst,
                               InstModCallbacks &callbacks) {
  for (SILValue v : inst->getResults()) {
    deleteAllDebugUses(v, callbacks);
  }
}

} // namespace swift

#endif
