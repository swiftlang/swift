//===--- SILCoverageMap.cpp - Defines the SILCoverageMap class ------------===//
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
// This file defines the SILCoverageMap class, which is used to relay coverage
// mapping information from the AST to lower layers of the compiler.
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILCoverageMap.h"
#include "swift/SIL/SILModule.h"

using namespace swift;

using llvm::coverage::CounterExpression;

SILCoverageMap *
SILCoverageMap::create(SILModule &M, StringRef Name, uint64_t Hash,
                       ArrayRef<MappedRegion> MappedRegions,
                       ArrayRef<CounterExpression> Expressions) {
  void *Buf = M.allocate(sizeof(SILCoverageMap), alignof(SILCoverageMap));
  SILCoverageMap *CM = ::new (Buf) SILCoverageMap(Hash);

  // Store a copy of the name so that we own the lifetime.
  char *AllocatedName = (char *)M.allocate(Name.size(), alignof(char));
  memcpy(AllocatedName, Name.data(), Name.size());
  CM->Name = StringRef(AllocatedName, Name.size());

  // Since we have two arrays, we need to manually tail allocate each of them,
  // rather than relying on the flexible array trick.
  size_t MappedRegionsSize = sizeof(MappedRegion) * MappedRegions.size();
  CM->MappedRegions =
      (MappedRegion *)M.allocate(MappedRegionsSize, alignof(MappedRegion));
  CM->NumMappedRegions = MappedRegions.size();
  memcpy(CM->MappedRegions, MappedRegions.begin(), MappedRegionsSize);

  size_t ExpressionsSize = sizeof(CounterExpression) * Expressions.size();
  CM->Expressions = (CounterExpression *)M.allocate(ExpressionsSize,
                                                    alignof(CounterExpression));
  CM->NumExpressions = Expressions.size();
  memcpy(CM->Expressions, Expressions.begin(), ExpressionsSize);

  M.coverageMaps.push_back(CM);
  return CM;
}

SILCoverageMap::SILCoverageMap(uint64_t Hash) : Hash(Hash) {}

SILCoverageMap::~SILCoverageMap() {}

namespace {
struct Printer {
  const llvm::coverage::Counter &C;
  ArrayRef<llvm::coverage::CounterExpression> Exprs;
  Printer(const llvm::coverage::Counter &C,
          ArrayRef<llvm::coverage::CounterExpression> Exprs)
      : C(C), Exprs(Exprs) {}

  void print(raw_ostream &OS) const {
    // TODO: This format's nice and human readable, but does it fit well with
    // SIL's relatively simple structure?
    if (C.isZero())
      OS << "zero";
    else if (C.isExpression()) {
      assert(C.getExpressionID() < Exprs.size() && "expression out of range");
      const auto &E = Exprs[C.getExpressionID()];
      OS << '(' << Printer(E.LHS, Exprs)
         << (E.Kind == CounterExpression::Add ? " + " : " - ")
         << Printer(E.RHS, Exprs) << ')';
    } else
      OS << C.getCounterID();
  }

  friend raw_ostream &operator<<(raw_ostream &OS, const Printer &P) {
    P.print(OS);
    return OS;
  }
};
}

void SILCoverageMap::printCounter(llvm::raw_ostream &OS,
                                  llvm::coverage::Counter C) const {
  OS << Printer(C, getExpressions());
}
