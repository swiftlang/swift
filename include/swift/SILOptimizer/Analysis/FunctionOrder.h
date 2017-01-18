//===--- FunctionOrder.h - Utilities for function ordering  -----*- C++ -*-===//
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

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_FUNCTIONORDER_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_FUNCTIONORDER_H

#include "swift/SILOptimizer/Analysis/BasicCalleeAnalysis.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/TinyPtrVector.h"

namespace swift {

class BasicCalleeAnalysis;
class SILFunction;
class SILModule;

class BottomUpFunctionOrder {
public:
  typedef TinyPtrVector<SILFunction *> SCC;

private:
  SILModule &M;
  llvm::SmallVector<SCC, 32> TheSCCs;
  llvm::SmallVector<SILFunction *, 32> TheFunctions;

  // The callee analysis we use to determine the callees at each call site.
  BasicCalleeAnalysis *BCA;

  unsigned NextDFSNum;
  llvm::DenseMap<SILFunction *, unsigned> DFSNum;
  llvm::DenseMap<SILFunction *, unsigned> MinDFSNum;
  llvm::SmallSetVector<SILFunction *, 4> DFSStack;

public:
  BottomUpFunctionOrder(SILModule &M, BasicCalleeAnalysis *BCA)
      : M(M), BCA(BCA), NextDFSNum(0) {}

  /// Get the SCCs in bottom-up order.
  ArrayRef<SCC> getSCCs() {
    if (!TheSCCs.empty())
      return TheSCCs;

    FindSCCs(M);
    return TheSCCs;
  }

  /// Get a flattened view of all functions in all the SCCs in
  /// bottom-up order
  ArrayRef<SILFunction *> getFunctions() {
    if (!TheFunctions.empty())
      return TheFunctions;

    for (auto SCC : getSCCs())
      for (auto *F : SCC)
        TheFunctions.push_back(F);

    return TheFunctions;
  }

private:
  void DFS(SILFunction *F);
  void FindSCCs(SILModule &M);
};

} // end namespace swift

#endif
