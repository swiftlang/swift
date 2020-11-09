//===--- ARCSequenceOptUtils.h - ARCSequenceOpts utilities ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// Utilities used by the ARCSequenceOpts for analyzing and transforming
/// SILInstructions.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_ARC_ARCSEQUENCEOPTUTILS_H
#define SWIFT_SILOPTIMIZER_ARC_ARCSEQUENCEOPTUTILS_H

#include "swift/SIL/SILInstruction.h"

namespace swift {
bool isARCSignificantTerminator(TermInst *TI);
} // end namespace swift

#endif
