//===--- SILCoverageMap.cpp - Defines the SILCoverageMap class ------------===//
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
SILCoverageMap::create(SILModule &M, StringRef Filename, StringRef Name,
                       bool External, uint64_t Hash,
                       ArrayRef<MappedRegion> MappedRegions,
                       ArrayRef<CounterExpression> Expressions) {
  auto *Buf = M.allocate<SILCoverageMap>(1);
  SILCoverageMap *CM = ::new (Buf) SILCoverageMap(Hash, External);

  // Store a copy of the names so that we own the lifetime.
  CM->Filename = M.allocateCopy(Filename);
  CM->Name = M.allocateCopy(Name);

  // Since we have two arrays, we need to manually tail allocate each of them,
  // rather than relying on the flexible array trick.
  CM->MappedRegions = M.allocateCopy(MappedRegions);
  CM->Expressions = M.allocateCopy(Expressions);

  M.coverageMaps.push_back(CM);
  return CM;
}

SILCoverageMap::SILCoverageMap(uint64_t Hash, bool External)
    : External(External), Hash(Hash) {}

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
} // end anonymous namespace

void SILCoverageMap::printCounter(llvm::raw_ostream &OS,
                                  llvm::coverage::Counter C) const {
  OS << Printer(C, getExpressions());
}
