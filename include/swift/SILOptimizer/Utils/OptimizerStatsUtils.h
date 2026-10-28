//===--- OptimizerStatsUtils.h - Utils for collecting stats  --*- C++ ---*-===//
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

#ifndef SWIFT_OPTIMIZER_STATS_UTILS_H
#define SWIFT_OPTIMIZER_STATS_UTILS_H

#include "swift/Basic/LLVM.h"

namespace swift {
class SILFunction;
class SILModule;
class SILTransform;
class SILPassManager;

/// Updates SILModule stats before executing the transform \p Transform.
///
/// \param M SILModule to be processed
/// \param Transform the SIL transformation that was just executed
/// \param PM the PassManager being used
void updateSILModuleStatsBeforeTransform(SILModule &M, SILTransform *Transform,
                                         SILPassManager &PM, int PassNumber);

/// Updates SILModule stats after finishing executing the
/// transform \p Transform.
///
/// \param M SILModule to be processed
/// \param Transform the SIL transformation that was just executed
/// \param PM the PassManager being used
void updateSILModuleStatsAfterTransform(SILModule &M, SILTransform *Transform,
                                        SILPassManager &PM, int PassNumber,
                                        int Duration);

/// Updates SILModule stats before executing a new subpass of \p Transform.
/// Only called when -sil-stats-subpass is enabled.
///
/// \param F the function the subpass is running on
/// \param Label identifies the subpass within the transform, typically the
///              name of the instruction being transformed
/// \param Transform the SIL transformation the subpass belongs to
/// \param PM the PassManager being used
/// \param PassNumber the pass number of the transformation
/// \param SubpassNumber the number of subpasses the transform already ran
void updateSILModuleStatsBeforeSubpass(SILFunction *F, StringRef Label,
                                       SILTransform *Transform,
                                       SILPassManager &PM, int PassNumber,
                                       unsigned SubpassNumber);

} // end namespace swift

#endif
