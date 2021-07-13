//===--- DAGNodeWorklist.h --------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_BASIC_DAGNODEWORKLIST_H
#define SWIFT_BASIC_DAGNODEWORKLIST_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"

/// Worklist of pointer-like things that have an invalid default value. This not
/// only avoids duplicates in the worklist, but also avoids revisiting
/// already-popped nodes. This makes it suitable for DAG traversal. This can
/// also be used within hybrid worklist/recursive traversal by recording the
/// size of the worklist at each level of recursion.
///
/// The primary API has two methods: intialize() and pop(). Others are provided
/// for flexibility.
template <typename T, unsigned SmallSize> struct DAGNodeWorklist {
  llvm::SmallPtrSet<T, SmallSize> nodeVisited;
  llvm::SmallVector<T, SmallSize> nodeVector;

  DAGNodeWorklist() = default;

  DAGNodeWorklist(const DAGNodeWorklist &) = delete;

  void initialize(T t) {
    clear();
    insert(t);
  }

  template <typename R> void initializeRange(R &&range) {
    clear();
    nodeVisited.insert(range.begin(), range.end());
    nodeVector.append(range.begin(), range.end());
  }

  T pop() { return empty() ? T() : nodeVector.pop_back_val(); }

  bool empty() const { return nodeVector.empty(); }

  unsigned size() const { return nodeVector.size(); }

  void clear() {
    nodeVector.clear();
    nodeVisited.clear();
  }

  void insert(T t) {
    if (nodeVisited.insert(t).second)
      nodeVector.push_back(t);
  }
};

#endif // SWIFT_BASIC_DAGNODEWORKLIST_H
