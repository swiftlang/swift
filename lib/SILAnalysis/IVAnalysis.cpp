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

static bool inSCC(ValueBase *Value, IVInfo::SCCType &SCC) {
  return std::find(SCC.begin(), SCC.end(), Value) != SCC.end();
}

// For now, we'll consider only the simplest induction variables:
// - Exactly one element in the cycle must be a SILArgument.
// - Only a single increment by a literal.
//
// In other words many valid things that could be considered induction
// variables are disallowed at this point.
SILArgument *IVInfo::isInductionSequence(SCCType &SCC) {
  // Ignore SCCs of size 1 for now. Some of these are derived IVs
  // like i+1 or i*4, which we will eventually want to handle.
  if (SCC.size() == 1)
    return nullptr;

  ApplyInst *FoundApply = nullptr;
  SILArgument *FoundArgument = nullptr;
  IntegerLiteralInst *IncValue = nullptr;
  for (unsigned long i = 0, e = SCC.size(); i != e; ++i) {
    if (auto IV = dyn_cast<SILArgument>(SCC[i])) {
      if (FoundArgument)
        return nullptr;

      FoundArgument = IV;
      continue;
    }

    auto *I = cast<SILInstruction>(SCC[i]);
    switch (I->getKind()) {
    case ValueKind::ApplyInst: {
      if (FoundApply)
        return nullptr;

      FoundApply = cast<ApplyInst>(I);

      SILValue L, R;
      if (!match(FoundApply, m_ApplyInst(BuiltinValueKind::SAddOver,
                                         m_SILValue(L), m_SILValue(R))))
        return nullptr;

      if (match(L, m_IntegerLiteralInst(IncValue)))
        std::swap(L, R);

      if (!match(R, m_IntegerLiteralInst(IncValue)))
        return nullptr;
      break;
    }

    case ValueKind::TupleExtractInst: {
      auto *TEI = cast<TupleExtractInst>(I);
      if (!inSCC(TEI->getOperand().getDef(), SCC))
        return nullptr;
      break;
    }

    default:
      return nullptr;
    }
  }
  if (!FoundApply || !FoundArgument || !IncValue)
    return nullptr;

  InductionInfoMap[FoundArgument] = IVDesc(FoundApply, IncValue);
  return FoundArgument;
}

void IVInfo::visit(SCCType &SCC) {
  assert(SCC.size() && "SCCs should have an element!!");

  SILArgument *IV;
  if (!(IV = isInductionSequence(SCC)))
    return;

  for (auto V : SCC)
    InductionVariableMap[V] = IV;
}

IVAnalysis::~IVAnalysis() {
  for (auto I = IVInfos.begin(), E = IVInfos.end(); I != E; ++I)
    delete I->second;
}
