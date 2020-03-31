//===--- SILInstructionWorklist.cpp ---------------------------------------===//
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

#define DEBUG_TYPE "sil-instruction-worklist"
#include "swift/SIL/SILInstructionWorklist.h"

using namespace swift;

void SILInstructionWorklistBase::withDebugStream(
    std::function<void(llvm::raw_ostream &stream, const char *loggingName)>
        perform) {
#ifndef NDEBUG
  LLVM_DEBUG(perform(llvm::dbgs(), loggingName));
#endif
}
