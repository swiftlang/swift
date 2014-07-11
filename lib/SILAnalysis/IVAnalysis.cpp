//===----------------- IVAnalysis.cpp - SIL IV Analysis -------*- C++ -*---===//
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

#include "swift/SILAnalysis/IVAnalysis.h"
#include "swift/SIL/PatternMatch.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILValue.h"

using namespace swift;
using namespace swift::PatternMatch;

// For now, we'll consider only the simplest induction variables:
// - The last element in the cycle must be a SILArgument.
// - No other element in the cycle is a SILArgument, which guarantees
//   that increments are unconditional (although they may come after)
//   a loop exit.
// - Only a single increment by a literal.
//
// In other words many valid things that could be considered induction
// variables are disallowed at this point.
bool IVInfo::isInductionSequence(llvm::SmallVectorImpl<ValueBase *> &SCC) {
  if (!isa<SILArgument>(SCC[SCC.size()-1]))
    return false;

  bool FoundApply = false;
  for (unsigned long i = 0, e = SCC.size() - 2; i != e; ++i) {
    if (isa<SILArgument>(SCC[i]))
      return false;

    auto *I = cast<SILInstruction>(SCC[i]);
    switch (I->getKind()) {
    case ValueKind::ApplyInst: {
      if (FoundApply)
        return false;

      auto *AI = cast<ApplyInst>(I);
      SILValue L, R;
      if (!match(AI, m_ApplyInst(BuiltinValueKind::SAddOver, m_SILValue(L),
                                 m_SILValue(R))))
        return false;

      if (match(L, m_IntegerLiteralInst()))
        std::swap(L, R);

      if (!match(R, m_IntegerLiteralInst()))
        return false;

      FoundApply = true;
      break;
    }

    case ValueKind::TupleExtractInst:
      break;

    default:
      return false;
    }
  }

  return true;
}

void IVInfo::visit(SCCType &SCC) {
  assert(SCC.size() && "SCCs should have an element!!");

  // Ignore SCCs of size 1 for now. Some of these are derived IVs
  // like i+1 or i*4, which we will eventually want to handle.
  if (SCC.size() == 1)
    return;

  if (!isInductionSequence(SCC))
    return;

  auto Header = cast<SILArgument>(SCC[SCC.size() - 1]);

  for (auto V : SCC)
    InductionVariableMap[V] = Header;
}

IVAnalysis::~IVAnalysis() {
  for (auto I = IVInfos.begin(), E = IVInfos.end(); I != E; ++I)
    delete I->second;
}
