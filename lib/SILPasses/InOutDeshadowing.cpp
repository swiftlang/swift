//===--- InOutDeshadowing.cpp - Remove non-escaping inout shadows ---------===//
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
//
// SILGen produces shadow variables for "inout" arguments to provide proper
// semantics for when
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "inout-deshadowing"
#include "swift/Subsystems.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
using namespace swift;

STATISTIC(NumShadowsRemoved, "Number of inout shadow variables removed");

//===----------------------------------------------------------------------===//
//                          alloc_stack Promotion
//===----------------------------------------------------------------------===//

/// processInOutValue - Walk the use-def list of the inout argument to find uses
/// of it.  We should only have stores and loads of the argument itself.  The
/// load should be a copy into a stack value, which is the shadow we're trying
/// to eliminate.
static bool processInOutValue(SILArgument *InOutArg) {
  assert(InOutArg->getType().isAddress() &&
         "inout arguments should always be addresses");


  return false;
}

//===----------------------------------------------------------------------===//
//                          Top Level Driver
//===----------------------------------------------------------------------===//

void swift::performInOutDeshadowing(SILModule *M) {
  DEBUG(llvm::errs() << "*** inout Deshadowing\n");

  for (auto &Fn : *M) {
    if (Fn.empty()) continue;
    SILBasicBlock &EntryBlock = Fn.front();

    // For each function, find any inout arguments and try to optimize each of
    // them.
    SILFunctionTypeInfo *FTI = Fn.getFunctionTypeInfo();
    
    for (unsigned arg = 0, e = FTI->getInputTypes().size(); arg != e; ++arg) {
      if (!FTI->isInOutArgument(arg)) continue;

      if (processInOutValue(EntryBlock.getBBArgs()[arg]))
        ++NumShadowsRemoved;
    }
  }
}


