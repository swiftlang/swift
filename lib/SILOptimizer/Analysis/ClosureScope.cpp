//===--- ClosureScope.cpp - Closure Scope Analysis ------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
// Implementation of ClosureScopeAnalysis.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "closure-scope"

#include "swift/SILOptimizer/Analysis/ClosureScope.h"

namespace swift {

class ClosureScopeData {
  using IndexRange = ClosureScopeAnalysis::IndexRange;
  using IndexLookupFunc = ClosureScopeAnalysis::IndexLookupFunc;
  using ScopeRange = ClosureScopeAnalysis::ScopeRange;

private:
  // Map an index to each SILFunction with a closure scope.
  std::vector<SILFunction *> indexedScopes;

  // Map each SILFunction with a closure scope to an index.
  llvm::DenseMap<SILFunction *, int> scopeToIndexMap;

  // A list of all indices for the SILFunctions that partially apply this
  // closure. The indices index into the `indexedScopes` vector. If the indexed
  // scope is nullptr, then that function has been deleted.
  using ClosureScopes = llvm::SmallVector<int, 1>;

  // Map each closure to its parent scopes.
  llvm::DenseMap<SILFunction *, ClosureScopes> closureToScopesMap;

public:
  void reset() {
    indexedScopes.clear();
    scopeToIndexMap.clear();
    closureToScopesMap.clear();
  }

  void erase(SILFunction *F) {
    // If this function is a mapped closure scope, remove it, leaving a nullptr
    // sentinel.
    auto indexPos = scopeToIndexMap.find(F);
    if (indexPos != scopeToIndexMap.end()) {
      indexedScopes[indexPos->second] = nullptr;
      scopeToIndexMap.erase(F);
    }
    // If this function is a closure, remove it.
    closureToScopesMap.erase(F);
  }

  // Record all closure scopes in this module.
  void compute(SILModule *M);

  bool isClosureScope(SILFunction *F) { return scopeToIndexMap.count(F); }

  // Return a range of scopes for the given closure. The elements of the
  // returned range have type `SILFunction *` and are non-null. Return an empty
  // range for a SILFunction that is not a closure or is a dead closure.
  ScopeRange getClosureScopes(SILFunction *ClosureF) {
    IndexRange indexRange(nullptr, nullptr);
    auto closureScopesPos = closureToScopesMap.find(ClosureF);
    if (closureScopesPos != closureToScopesMap.end()) {
      auto &indexedScopes = closureScopesPos->second;
      indexRange = IndexRange(indexedScopes.begin(), indexedScopes.end());
    }
    return makeOptionalTransformRange(indexRange,
                                      IndexLookupFunc(indexedScopes));
  }

  void recordScope(PartialApplyInst *PAI) {
    // Only track scopes of non-escaping closures.
    auto closureTy = PAI->getCallee()->getType().castTo<SILFunctionType>();
    if (!isNonEscapingClosure(closureTy))
      return;

    auto closureFunc = PAI->getCalleeFunction();
    assert(closureFunc && "non-escaping closure needs a direct partial_apply.");

    auto scopeFunc = PAI->getFunction();
    int scopeIdx = lookupScopeIndex(scopeFunc);

    auto &indices = closureToScopesMap[closureFunc];
    if (std::find(indices.begin(), indices.end(), scopeIdx) != indices.end())
      return;

    indices.push_back(scopeIdx);
  }

protected:
  int lookupScopeIndex(SILFunction *scopeFunc) {
    auto indexPos = scopeToIndexMap.find(scopeFunc);
    if (indexPos != scopeToIndexMap.end())
      return indexPos->second;

    int scopeIdx = indexedScopes.size();
    scopeToIndexMap[scopeFunc] = scopeIdx;
    indexedScopes.push_back(scopeFunc);
    return scopeIdx;
  }
};

void ClosureScopeData::compute(SILModule *M) {
  for (auto &F : *M) {
    for (auto &BB : F) {
      for (auto &I : BB) {
        if (auto *PAI = dyn_cast<PartialApplyInst>(&I)) {
          recordScope(PAI);
        }
      }
    }
  }
}

ClosureScopeAnalysis::ClosureScopeAnalysis(SILModule *M)
    : SILAnalysis(AnalysisKind::ClosureScope), M(M), scopeData(nullptr) {}

ClosureScopeAnalysis::~ClosureScopeAnalysis() = default;

bool ClosureScopeAnalysis::isClosureScope(SILFunction *scopeFunc) {
  return getOrComputeScopeData()->isClosureScope(scopeFunc);
}

ClosureScopeAnalysis::ScopeRange
ClosureScopeAnalysis::getClosureScopes(SILFunction *closureFunc) {
  return getOrComputeScopeData()->getClosureScopes(closureFunc);
}

void ClosureScopeAnalysis::invalidate() {
  if (scopeData) scopeData->reset();
}

void ClosureScopeAnalysis::notifyDeleteFunction(SILFunction *F) {
  if (scopeData) scopeData->erase(F);
}

ClosureScopeData *ClosureScopeAnalysis::getOrComputeScopeData() {
  if (!scopeData) {
    scopeData = llvm::make_unique<ClosureScopeData>();
    scopeData->compute(M);
  }
  return scopeData.get();
}

SILAnalysis *createClosureScopeAnalysis(SILModule *M) {
  return new ClosureScopeAnalysis(M);
}

} // namespace swift
