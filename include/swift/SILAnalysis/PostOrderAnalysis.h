//===--- PostOrderAnalysis.h - SIL POT and RPOT Analysis -------*- C++ -*--===//
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

#ifndef SWIFT_SILANALYSIS_POSTORDERANALYSIS_H
#define SWIFT_SILANALYSIS_POSTORDERANALYSIS_H

#include "swift/SILAnalysis/Analysis.h"
#include "swift/Basic/Range.h"
#include "swift/SIL/CFG.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILFunction.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/DenseMap.h"
#include <vector>

namespace swift {

class SILBasicBlock;
class SILFunction;

struct PostOrderInfo {
  std::vector<SILBasicBlock *> PostOrder;

  PostOrderInfo(SILFunction *F) {
    std::copy(po_begin(F), po_end(F), std::back_inserter(PostOrder));
  }
};

/// This class is a simple wrapper around the POT iterator provided by LLVM. It
/// lazily re-evaluates the post order when it is invalidated so that we do not
/// reform the post order over and over again (it can be expensive).
class PostOrderAnalysis : public FunctionAnalysisBase<PostOrderInfo> {
protected:
  virtual PostOrderInfo *newFunctionAnalysis(SILFunction *F) override {
    return new PostOrderInfo(F);
  }

  virtual bool shouldInvalidate(SILAnalysis::PreserveKind K) override {
    return !(K & PreserveKind::Branches);
  }

public:
  PostOrderAnalysis()
      : FunctionAnalysisBase<PostOrderInfo>(AnalysisKind::PostOrder) {}

  // This is a cache and shouldn't be copied around.
  PostOrderAnalysis(const PostOrderAnalysis &) = delete;
  PostOrderAnalysis &operator=(const PostOrderAnalysis &) = delete;

  using iterator = std::vector<SILBasicBlock *>::iterator;
  using reverse_iterator = std::vector<SILBasicBlock *>::reverse_iterator;

  using range = Range<iterator>;
  using reverse_range = Range<reverse_iterator>;

  Range<iterator> getPostOrder(SILFunction *F) {
    auto *Info = get(F);
    return Range<iterator>(Info->PostOrder.begin(), Info->PostOrder.end());
  }

  Range<reverse_iterator> getReversePostOrder(SILFunction *F) {
    auto *Info = get(F);
    return reversed(Info->PostOrder);
  }

  // Return the size of the post order for \p F.
  unsigned size(SILFunction *F) const {
    auto R = const_cast<PostOrderAnalysis *>(this)->getPostOrder(F);
    // This is cheap since we know that we are using vector iterators.
    return std::distance(R.begin(), R.end());
  }

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::PostOrder;
  }
};

} // end namespace swift

#endif
