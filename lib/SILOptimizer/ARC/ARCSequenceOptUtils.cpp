//===--- ARCSequenceOptUtils.cpp - ARCSequenceOpt utilities ------------===//
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

#include "ARCSequenceOptUtils.h"

using namespace swift;
namespace swift {
bool isARCSignificantTerminator(TermInst *TI) {
  switch (TI->getTermKind()) {
  case TermKind::UnreachableInst:
  // br is a forwarding use for its arguments. It cannot in of itself extend
  // the lifetime of an object (just like a phi-node) cannot.
  case TermKind::BranchInst:
  // A cond_br is a forwarding use for its non-operand arguments in a similar
  // way to br. Its operand must be an i1 that has a different lifetime from any
  // ref counted object.
  case TermKind::CondBranchInst:
    return false;
  // Be conservative for now. These actually perform some sort of operation
  // against the operand or can use the value in some way.
  case TermKind::ThrowInst:
  case TermKind::ThrowAddrInst:
  case TermKind::ReturnInst:
  case TermKind::UnwindInst:
  case TermKind::YieldInst:
  case TermKind::AwaitAsyncContinuationInst:
  case TermKind::TryApplyInst:
  case TermKind::SwitchValueInst:
  case TermKind::SwitchEnumInst:
  case TermKind::SwitchEnumAddrInst:
  case TermKind::DynamicMethodBranchInst:
  case TermKind::CheckedCastBranchInst:
  case TermKind::CheckedCastAddrBranchInst:
    return true;
  }

  llvm_unreachable("Unhandled TermKind in switch.");
}

} // end namespace 'swift'
