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

/// Notify the SILModule stats tracking, that \p Derived was created from
/// \p Original (by specialization).  This allows lost-variable tracking to
/// compare the derived function's variables against the original's.
void notifySILModuleStatsOfFunctionDerivedFrom(SILPassManager &PM,
                                               SILFunction *Derived,
                                               SILFunction *Original);

} // end namespace swift

#endif
