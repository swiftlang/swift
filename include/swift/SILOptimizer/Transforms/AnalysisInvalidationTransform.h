//===--- AnalysisInvalidationTransform.h ----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// This file defines a transform that can be used to explicitly invalidate an
/// analysis in the pass pipeline. It is intended to be used in a situation
/// where passes are reusing state from an analysis and one wants to explicitly
/// placed into the pass pipeline that it is expected that the analysis be
/// invalidated after the series of passes complete. If we were to just place
/// the invalidation in the last run of the passes, it is possible for a
/// programmer later to add another pass to the end of the pipeline. This would
/// then force recomputation and may prevent invalidation from happening. So by
/// doing this, it is clear to someone adding a new pass that this needs to
/// happen last.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_ANALYSISINVALIDATIONTRANSFORM_H
#define SWIFT_SILOPTIMIZER_UTILS_ANALYSISINVALIDATIONTRANSFORM_H

#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"

namespace swift {

template <typename AnalysisTy>
struct AnalysisInvalidationTransform : public SILFunctionTransform {
  void run() override {
    getAnalysis<AnalysisTy>()->invalidate();
  }
};

} // namespace swift

#endif
